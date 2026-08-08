//! conwaybg — a GPU-accelerated Conway's Game of Life field, rendered as a grid of
//! thin, glossy, bevelled 3D tiles under a near-orthographic top-down camera.
//!
//! Built for <https://thomasgilray.com> as a full-page fixed background canvas.
//! It targets WebGPU where available and falls back to WebGL2, both via `wgpu`.
//!
//! The simulation runs on the CPU — the grid is only ~1-2k cells — while the GPU
//! draws one instanced bevelled-prism mesh per cell and evaluates the per-tile
//! "pop" spring in the vertex shader. That keeps uploads down to roughly two small
//! buffer writes per second no matter the frame rate.
//!
//! Everything above the `web` module is platform-independent, so `src/bin/preview.rs`
//! can render the same scene headlessly to PNG.
//!
//! Copyright (c) Thomas Gilray. MIT licensed.

use bytemuck::{Pod, Zeroable};

// ---------------------------------------------------------------------------
// Tunables
// ---------------------------------------------------------------------------

/// Grid pitch in CSS pixels. Everything below is expressed relative to it, so this
/// is the one knob that scales the whole field.
pub const CELL_PX: f32 = 76.0;
/// Fraction of the pitch a tile occupies; the remainder is the gap between tiles.
const TILE_FILL: f32 = 0.88;
/// Tile thickness in CSS pixels — thin, but with real depth.
const THICK: f32 = 18.2;
/// Radius of the (2D) rounded corners.
const CORNER_R: f32 = 9.9;
/// Radius of the rolled-over top edge. This is what catches the glints.
const BEVEL: f32 = 4.3;
/// How much higher a live tile rests than a dead one.
const RISE: f32 = 9.1;
/// How far toward the viewer a tile snaps when it is switched on.
const POP: f32 = 25.3;

/// Damped-spring return: amplitude envelope exp(-DECAY * t)…
const SPRING_DECAY: f32 = 11.0;
/// …times cos(SPRING_OMEGA * t). ~0.23s period, so it overshoots once and clicks home.
const SPRING_OMEGA: f32 = 27.0;

/// Seconds a tile takes to appear. Switching on is the snappy half of the animation:
/// it springs forward and clicks into place.
const BIRTH_FADE: f32 = 0.18;
/// Seconds a tile takes to disappear. Dying is the quiet half — no spring at all, just
/// a slower settle down and out. Must stay under the shortest simulation step; the
/// step loop holds a tile in the solid pass until this long has elapsed.
const DEATH_FADE: f32 = 0.55;
/// When a birth replaces an orthogonally adjacent death, the old tile travels one
/// cell into the new position over the same span the ordinary death would have used.
const TILE_SLIDE_SECS: f32 = DEATH_FADE;

/// A tile that survives a generation gets a little celebratory spin about its own
/// vertical axis this often — but only the coloured ones, so it stays an accent
/// rather than the whole field twitching.
const SPIN_CHANCE: f32 = 0.5;
/// How long one spin takes. Must stay under the shortest step so spins never overlap.
const SPIN_SECS: f32 = 0.5;
/// Hop that rides along with the spin, peaking as the tile turns through edge-on.
const SPIN_LIFT: f32 = 16.0;

/// Coverage of a settled "off" tile. At 0 they vanish completely and the ghost pass
/// is skipped outright; raise it to leave a faint glass lattice behind instead.
const GHOST_ALPHA: f32 = 0.0;
/// Specular boost applied as a tile fades out, so glints linger for a moment on the
/// way down rather than snapping off with the body.
const GHOST_SPEC: f32 = 3.0;

/// Soft halo drawn around each live tile: an even bloom in the tile's own colour,
/// radiating equally in all directions. Deliberately not a cast shadow — an offset
/// darkening reads as a drop shadow rather than as light, and under a top-down camera
/// a physically-placed shadow would sit hidden underneath the tile anyway.
/// Extent of the halo quad beyond the tile edge; the bloom is windowed to zero here.
const HALO_PAD: f32 = 69.0;
/// Falloff of the bright core, in pixels.
const GLOW_RADIUS: f32 = 16.1;
/// The wide haze is the same falloff stretched by this much.
const GLOW_HAZE: f32 = 2.5;
/// Keep the atmospheric colour but let the distant walkers and fog remain visible
/// through overlapping blooms. This is about 38% below the original 0.55 coverage.
const GLOW_STRENGTH: f32 = 0.34;

/// Simulation extends this many cells beyond the visible viewport on every side.
pub const MARGIN: usize = 3;
/// Depth of the band injections are drawn from: the margin plus the outermost
/// visible row/column, so an injection reaches *at most* onto the edge of the screen.
const BAND: usize = MARGIN + 1;

/// One 8s rhythm: 5.5s of ordinary stepping, then 2.5s of fast-forward running
/// 40% quicker. Expressed as rates rather than a step count per phase, so the split
/// isn't forced onto whole numbers of generations.
const CYCLE: f64 = 8.0;
/// The fast-forward phase is switched off: the whole cycle now runs at one pace.
/// Give this less than CYCLE to bring the second gear back.
const SLOW_SECS: f64 = 8.0;
const FAST_SECS: f64 = CYCLE - SLOW_SECS;
/// The two phases are set independently rather than as a ratio, because they have
/// been pulled apart in opposite directions. Deliberately chosen so `CYCLE_GENS`
/// lands off a whole number: at well under a generation a second there are only about
/// four and a half of them in a cycle, so if every cycle held the same integer count
/// the slow/fast split would quantize identically each time instead of averaging out.
const SLOW_HZ: f64 = 0.266;
const FAST_HZ: f64 = 0.91;
/// Generations per cycle. Not an integer, and it must not be.
const CYCLE_GENS: f64 = SLOW_SECS * SLOW_HZ + FAST_SECS * FAST_HZ;

/// Edge injections per second of simulation time. Scaled alongside the step rate so
/// each generation takes on about the same amount of new material regardless of pace.
const INJECT_HZ: f64 = 0.483;
/// Probability an injection is a glider rather than a random scatter.
const GLIDER_CHANCE: f32 = 0.25;
/// Live density of a scatter injection.
const SCATTER_DENSITY: f32 = 0.45;
/// Live density of the initial field.
const SEED_DENSITY: f32 = 0.22;

/// Vertical field of view. Small enough to read as orthographic, wide enough that
/// tiles away from centre show a sliver of their side wall.
const FOV_Y: f32 = 32.0 * std::f32::consts::PI / 180.0;

/// Mesh resolution.
const CORNER_SEGS: usize = 7;
const BEVEL_SEGS: usize = 5;

/// Colours, as sRGB hex. `#e8e8e8` page background is the clear colour.
pub const BG: u32 = 0xe8e8e8;
/// An "off" tile — barely tinted, and drawn at GHOST_ALPHA coverage on top.
const C_DEAD: u32 = 0xe9e9e9;
/// The live colours and their share of the field, keyed off a hash of the cell's
/// coordinates so a tile's colour is fixed for the life of the grid. The common grey
/// and the first two accents are the palette the original DOM version used.
const C_LIVE: u32 = 0xcccccc;
const C_GREEN: u32 = 0x589864;
const C_TEAL: u32 = 0x3e9993;
/// A pale sky blue, well up toward white but still crisply blue, and carrying far
/// less green than the other two accents.
const C_BLUE: u32 = 0x86bdec;
/// Hash cuts: 72% common grey, 12% green, 8% teal, 8% blue.
const GREEN_CUT: f32 = 0.72;
const TEAL_CUT: f32 = 0.84;
const BLUE_CUT: f32 = 0.92;

/// Longest frame we will integrate in one go, for an ordinary slow frame.
const MAX_FRAME_DT: f64 = 0.25;
/// A gap longer than this means the page was not being drawn at all — tab hidden,
/// bfcache, machine asleep. Resume where we left off rather than replaying it.
const RESUME_GAP: f64 = 0.5;
/// Backstop only; the dt clamp above should keep this out of reach.
const MAX_STEPS_PER_FRAME: i64 = 4;

/// Generations run before the first frame is ever shown. A fresh random soup collapses
/// violently over its first several generations, and watching that play out on arrival
/// reads as the page frantically catching up on something.
const WARMUP_GENS: u32 = 12;

const DEPTH_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Depth24Plus;

// ---------------------------------------------------------------------------
// Small utilities
// ---------------------------------------------------------------------------

/// xorshift64* — deterministic, tiny, and plenty random for scattering cells.
pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Self {
        Rng(seed | 1)
    }
    pub fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.0 = x;
        x.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }
    fn f32(&mut self) -> f32 {
        (self.next_u64() >> 40) as f32 / (1u32 << 24) as f32
    }
    fn below(&mut self, n: usize) -> usize {
        if n == 0 {
            0
        } else {
            (self.next_u64() % n as u64) as usize
        }
    }
}

/// sRGB hex -> linear RGB, so the lighting maths happens in a linear space.
fn srgb_hex_to_linear(hex: u32) -> [f32; 3] {
    let f = |b: u32| {
        let c = b as f32 / 255.0;
        if c <= 0.04045 {
            c / 12.92
        } else {
            ((c + 0.055) / 1.055).powf(2.4)
        }
    };
    [f((hex >> 16) & 0xff), f((hex >> 8) & 0xff), f(hex & 0xff)]
}

// Row-major 4x4 helpers. Uploaded transposed, since WGSL reads column-major.
type Mat4 = [[f32; 4]; 4];

fn mat_mul(a: &Mat4, b: &Mat4) -> Mat4 {
    let mut o = [[0.0f32; 4]; 4];
    for r in 0..4 {
        for c in 0..4 {
            o[r][c] = (0..4).map(|k| a[r][k] * b[k][c]).sum();
        }
    }
    o
}

fn perspective_rh(fovy: f32, aspect: f32, near: f32, far: f32) -> Mat4 {
    let f = 1.0 / (fovy * 0.5).tan();
    [
        [f / aspect, 0.0, 0.0, 0.0],
        [0.0, f, 0.0, 0.0],
        [0.0, 0.0, far / (near - far), near * far / (near - far)],
        [0.0, 0.0, -1.0, 0.0],
    ]
}

/// Camera sits at (0, 0, d) looking down -Z with +Y up: a pure translation.
fn view_from_height(d: f32) -> Mat4 {
    [
        [1.0, 0.0, 0.0, 0.0],
        [0.0, 1.0, 0.0, 0.0],
        [0.0, 0.0, 1.0, -d],
        [0.0, 0.0, 0.0, 1.0],
    ]
}

fn to_cols(m: &Mat4) -> [[f32; 4]; 4] {
    let mut o = [[0.0f32; 4]; 4];
    for r in 0..4 {
        for c in 0..4 {
            o[c][r] = m[r][c];
        }
    }
    o
}

fn pad(c: [f32; 3]) -> [f32; 4] {
    [c[0], c[1], c[2], 1.0]
}

// ---------------------------------------------------------------------------
// Tile mesh: a rounded-rectangle prism with a rolled top edge
// ---------------------------------------------------------------------------

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct Vertex {
    pos: [f32; 3],
    nrm: [f32; 3],
}

/// Builds one tile: a flat top face, a quarter-round bevel rolling down and out to
/// the silhouette, a straight side wall, and a flat underside. The underside is never
/// visible from a camera sitting above the plane, but a tile tumbling toward the
/// viewer turns through it, so it has to be there.
fn build_tile_mesh() -> (Vec<Vertex>, Vec<u16>) {
    let half = CELL_PX * TILE_FILL * 0.5;
    let rc = CORNER_R.min(half - 0.5);

    // Silhouette: a closed CCW polyline of points with outward XY normals.
    let mut outline: Vec<([f32; 2], [f32; 2])> = Vec::new();
    let centers = [
        [half - rc, half - rc],
        [-(half - rc), half - rc],
        [-(half - rc), -(half - rc)],
        [half - rc, -(half - rc)],
    ];
    for (k, c) in centers.iter().enumerate() {
        let a0 = k as f32 * std::f32::consts::FRAC_PI_2;
        for i in 0..=CORNER_SEGS {
            let a = a0 + std::f32::consts::FRAC_PI_2 * (i as f32 / CORNER_SEGS as f32);
            let (s, co) = a.sin_cos();
            outline.push(([c[0] + rc * co, c[1] + rc * s], [co, s]));
        }
    }
    let n_out = outline.len();

    let mut verts: Vec<Vertex> = Vec::new();
    let mut idx: Vec<u16> = Vec::new();

    // The profile is mirrored about the tile's mid-plane, so the two faces are alike.
    // That is what lets a tile tumble a *half* turn and land exactly as it started:
    // with a bevel on the top only, 180 degrees would leave it lying on its back.
    // Rings run top face -> top bevel -> bottom bevel -> bottom face.
    let push_ring = |verts: &mut Vec<Vertex>, inset: f32, z: f32, nxy: f32, nz: f32| {
        for (p, n) in &outline {
            let (nx, ny) = (n[0] * nxy, n[1] * nxy);
            let len = (nx * nx + ny * ny + nz * nz).sqrt().max(1e-6);
            verts.push(Vertex {
                pos: [p[0] - inset * n[0], p[1] - inset * n[1], z],
                nrm: [nx / len, ny / len, nz / len],
            });
        }
    };
    for ring in 0..=(2 * BEVEL_SEGS + 1) {
        // First half rolls the top edge down and out, second half rolls the bottom
        // edge back in; the straight side wall is the seam between them.
        let (top, i) = if ring <= BEVEL_SEGS {
            (true, ring)
        } else {
            (false, 2 * BEVEL_SEGS + 1 - ring)
        };
        let theta = (i as f32 / BEVEL_SEGS as f32) * std::f32::consts::FRAC_PI_2;
        let (sin, cos) = theta.sin_cos();
        let inset = BEVEL * (1.0 - sin);
        let depth = BEVEL * (1.0 - cos);
        let (z, nz) = if top {
            (THICK - depth, cos)
        } else {
            (depth, -cos)
        };
        push_ring(&mut verts, inset, z, sin, nz);
    }

    let rings = 2 * BEVEL_SEGS + 2;
    let last_ring = ((rings - 1) * n_out) as u16;

    // Flat cap on each face, as a fan from a centre vertex.
    let top_c = verts.len() as u16;
    verts.push(Vertex {
        pos: [0.0, 0.0, THICK],
        nrm: [0.0, 0.0, 1.0],
    });
    let bot_c = verts.len() as u16;
    verts.push(Vertex {
        pos: [0.0, 0.0, 0.0],
        nrm: [0.0, 0.0, -1.0],
    });
    for i in 0..n_out {
        let a = i as u16;
        let b = ((i + 1) % n_out) as u16;
        idx.extend_from_slice(&[top_c, a, b]);
        // Wound the other way so it faces down.
        idx.extend_from_slice(&[bot_c, last_ring + b, last_ring + a]);
    }

    // Bevel and wall quads between consecutive rings.
    for ring in 0..(rings - 1) {
        let lo = (ring * n_out) as u16;
        let hi = ((ring + 1) * n_out) as u16;
        for i in 0..n_out {
            let i0 = i as u16;
            let i1 = ((i + 1) % n_out) as u16;
            idx.extend_from_slice(&[lo + i0, hi + i0, hi + i1]);
            idx.extend_from_slice(&[lo + i0, hi + i1, lo + i1]);
        }
    }

    (verts, idx)
}

// ---------------------------------------------------------------------------
// Props — free-standing models a critter can carry, as opposed to grid tiles
// ---------------------------------------------------------------------------

/// A vertex of a prop model. Unlike tiles, props carry their own colour, since one
/// model is several materials at once.
#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
pub struct PropVertex {
    pos: [f32; 3],
    nrm: [f32; 3],
    /// Linear RGB.
    col: [f32; 3],
    /// Texture coordinate. Ordinary meshes leave it at zero; sprite billboards use it.
    uv: [f32; 2],
}

/// One placed instance of a prop model. Padded out to 16-byte rows so each attribute
/// sits on a natural offset; the vertex layout names those offsets explicitly.
#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable, Default)]
pub struct Prop {
    /// World position of the model origin, in the same units as everything else.
    pos: [f32; 3],
    /// Coverage. Solid models pass 1.
    alpha: f32,
    /// Per-axis, so one unit rod model can serve as every limb of a figure: scaled
    /// along its length without also getting thicker.
    scale: [f32; 3],
    _pad0: f32,
    /// Roll about the model's own +X axis, pitch about +Y, yaw about +Z. Applied in
    /// that order, which for something flying nose-first along +X reads as bank,
    /// climb and turn.
    rot: [f32; 3],
    _pad1: f32,
    /// Multiplies the model's own vertex colours, so one mesh can be recoloured per
    /// instance. Pass white to keep the model as authored.
    tint: [f32; 3],
    _pad2: f32,
}

impl Prop {
    /// A solid, fully opaque instance wearing the model's own colours.
    pub fn new(pos: [f32; 3], scale: f32, rot: [f32; 3]) -> Prop {
        Prop::stretched(pos, [scale; 3], rot)
    }

    pub fn stretched(pos: [f32; 3], scale: [f32; 3], rot: [f32; 3]) -> Prop {
        Prop {
            pos,
            alpha: 1.0,
            scale,
            _pad0: 0.0,
            rot,
            _pad1: 0.0,
            tint: [1.0, 1.0, 1.0],
            _pad2: 0.0,
        }
    }

    pub fn tinted(mut self, tint: [f32; 3], alpha: f32) -> Prop {
        self.tint = tint;
        self.alpha = alpha;
        self
    }
}

/// The prop models. Both live in one pair of buffers; each is drawn from its own index
/// range, which is why they are a small fixed set rather than a registry.
pub const MODEL_ROCKET: usize = 0;
pub const MODEL_EMBER: usize = 1;
/// A unit-length bar along +X, one unit thick, lying in the XY plane. Every limb and
/// stroke of a stick figure is this one model under a different stretch and turn.
pub const MODEL_ROD: usize = 2;
/// A unit-radius filled circle in the XY plane.
pub const MODEL_DISC: usize = 3;
/// Six independently pivoted cells from the photographic leg atlas. Keeping them
/// ahead of the body in model order makes their attachment ends disappear under fur.
pub const MODEL_BEE_LEG_FIRST: usize = 4;
pub const MODEL_BEE_LEG_LAST: usize = MODEL_BEE_LEG_FIRST + 5;
/// Textured top-down body.
pub const MODEL_BEE_BODY: usize = 10;
/// Translucent bee wings are a separate late-drawn disc model, so they veil the
/// thorax like real wings instead of disappearing behind the body cutout.
pub const MODEL_BEE_WING: usize = 11;
/// Oversized carved mask replacing the walker's former disc head.
pub const MODEL_TIKI_MASK: usize = 12;
/// Alternate oversized astronaut helmet, selected once when a walker arrives.
pub const MODEL_ASTRONAUT_HELMET: usize = 13;
/// Fierce traditional Noh theatre mask replacing the first Japanese-mask draft.
pub const MODEL_NOH_MASK: usize = 14;
/// Mischievous carved monkey mask.
pub const MODEL_MONKEY_MASK: usize = 15;
/// One split comedy/tragedy Greek theatre mask.
pub const MODEL_COMEDY_TRAGEDY_MASK: usize = 16;
/// Tux-inspired giant in the experimental distant background layer.
pub const MODEL_PENGUIN: usize = 17;
/// Independently pivoted thigh and shin layers for the enlarged Godzilla/kaiju.
/// Lower legs draw first, then thighs, then the body, hiding both knee and hip seams.
pub const MODEL_GODZILLA_REAR_UPPER: usize = 18;
pub const MODEL_GODZILLA_REAR_LOWER: usize = 19;
pub const MODEL_GODZILLA_FRONT_UPPER: usize = 20;
pub const MODEL_GODZILLA_FRONT_LOWER: usize = 21;
/// Distal 40% of the tail, independently pitched away from the camera.
pub const MODEL_GODZILLA_TAIL: usize = 22;
pub const MODEL_GODZILLA_BODY: usize = 23;
/// Procedural marshmallow sailor. One shared ellipsoid supplies every articulated
/// limb puff; torso and head remain separate so the face can turn a little farther
/// toward the camera than the shoulders.
pub const MODEL_MARSHMALLOW_PUFF: usize = 24;
pub const MODEL_MARSHMALLOW_BODY: usize = 25;
pub const MODEL_MARSHMALLOW_HEAD: usize = 26;
/// Eight generated mist cells that drift between the distant visitor and everything
/// in the ordinary scene.
pub const MODEL_FOG_FIRST: usize = 27;
pub const MODEL_FOG_LAST: usize = MODEL_FOG_FIRST + 7;
/// Solid, code-built flying saucer.
pub const MODEL_UFO: usize = 35;
/// A real tile mesh detached from Conway and carried away by the saucer.
pub const MODEL_CAPTURED_TILE: usize = 36;
/// The translucent tractor-beam cone. Kept last so it blends over the solids.
pub const MODEL_UFO_BEAM: usize = 37;
/// The tile mimic is a tiny clockwork crab whose carapace is an ordinary tile.
/// Its articulated pieces remain separate so the six legs, eyes, claws, and key can
/// each carry their own small secondary motion.
pub const MODEL_MIMIC_BODY: usize = 38;
pub const MODEL_MIMIC_SEGMENT: usize = 39;
pub const MODEL_MIMIC_EYE: usize = 40;
pub const MODEL_MIMIC_CLAW: usize = 41;
pub const MODEL_MIMIC_KEY: usize = 42;
pub const MODEL_COUNT: usize = 43;

/// Which models are drawn flat and unlit — blended over the scene without writing
/// depth. Embers are their own light source; line art wants to stay the colour it is
/// asked for rather than picking up highlights.
const fn model_unlit() -> [bool; MODEL_COUNT] {
    let mut result = [true; MODEL_COUNT];
    result[MODEL_ROCKET] = false;
    result[MODEL_UFO] = false;
    result[MODEL_CAPTURED_TILE] = false;
    result[MODEL_MARSHMALLOW_PUFF] = false;
    result[MODEL_MARSHMALLOW_BODY] = false;
    result[MODEL_MARSHMALLOW_HEAD] = false;
    result[MODEL_MIMIC_BODY] = false;
    result[MODEL_MIMIC_SEGMENT] = false;
    result[MODEL_MIMIC_EYE] = false;
    result[MODEL_MIMIC_CLAW] = false;
    result[MODEL_MIMIC_KEY] = false;
    result
}
const MODEL_UNLIT: [bool; MODEL_COUNT] = model_unlit();

/// Where critters drop their prop instances, grouped by model so each group can be
/// drawn from one range.
pub struct PropSink {
    models: [Vec<Prop>; MODEL_COUNT],
}

impl Default for PropSink {
    fn default() -> Self {
        PropSink {
            models: std::array::from_fn(|_| Vec::new()),
        }
    }
}

impl PropSink {
    pub fn push(&mut self, model: usize, prop: Prop) {
        self.models[model].push(prop);
    }
    pub fn clear(&mut self) {
        for m in &mut self.models {
            m.clear();
        }
    }
    pub fn group(&self, model: usize) -> &[Prop] {
        &self.models[model]
    }
    pub fn total(&self) -> usize {
        self.models.iter().map(|m| m.len()).sum()
    }
    /// Scale the coverage of every visitor for the pause fade. The fog is exempt:
    /// it is weather rather than a visitor, and it stays through a pause.
    pub fn scale_visitor_alpha(&mut self, factor: f32) {
        for (model, props) in self.models.iter_mut().enumerate() {
            if (MODEL_FOG_FIRST..=MODEL_FOG_LAST).contains(&model) {
                continue;
            }
            for prop in props {
                prop.alpha *= factor;
            }
        }
    }
}

/// Packs every prop model into one vertex/index buffer pair and reports where each
/// model's indices begin.
///
/// The indices are made **absolute** into the shared vertex buffer rather than left
/// relative with a `base_vertex` on the draw call, because WebGL2 has no base-vertex
/// indexed draw — that is a desktop-GL extension. Passing one silently works on a
/// native GL context and then kills the module in a browser.
/// The meshes, in model-index order. The single place that mapping lives, so adding a
/// model cannot leave the combiner and anything checking it disagreeing.
fn prop_model_meshes() -> [(Vec<PropVertex>, Vec<u16>); MODEL_COUNT] {
    [
        build_rocket(ROCKET_LENGTH),
        build_ember(),
        build_rod(),
        build_disc(),
        build_bee_leg_quad(0),
        build_bee_leg_quad(1),
        build_bee_leg_quad(2),
        build_bee_leg_quad(3),
        build_bee_leg_quad(4),
        build_bee_leg_quad(5),
        build_bee_body_quad(),
        build_disc(),
        build_tiki_mask_quad(),
        build_astronaut_helmet_quad(),
        build_noh_mask_quad(),
        build_monkey_mask_quad(),
        build_comedy_tragedy_mask_quad(),
        build_penguin_quad(),
        build_godzilla_leg_quad(0),
        build_godzilla_leg_quad(1),
        build_godzilla_leg_quad(2),
        build_godzilla_leg_quad(3),
        build_godzilla_tail_quad(),
        build_godzilla_body_quad(),
        build_marshmallow_puff(),
        build_marshmallow_body(),
        build_marshmallow_head(),
        build_fog_blob_quad(0),
        build_fog_blob_quad(1),
        build_fog_blob_quad(2),
        build_fog_blob_quad(3),
        build_fog_blob_quad(4),
        build_fog_blob_quad(5),
        build_fog_blob_quad(6),
        build_fog_blob_quad(7),
        build_ufo(),
        build_captured_tile(),
        build_ufo_beam(),
        build_mimic_body(),
        build_mimic_segment(),
        build_mimic_eye(),
        build_mimic_claw(),
        build_mimic_key(),
    ]
}

fn combine_prop_models() -> (Vec<PropVertex>, Vec<u16>, [(u32, u32); MODEL_COUNT]) {
    let mut verts: Vec<PropVertex> = Vec::new();
    let mut indices: Vec<u16> = Vec::new();
    let mut ranges = [(0u32, 0u32); MODEL_COUNT];
    for (slot, (mv, mi)) in prop_model_meshes().into_iter().enumerate() {
        let base = verts.len();
        assert!(
            base + mv.len() < u16::MAX as usize,
            "prop models outgrew a u16 index buffer"
        );
        ranges[slot] = (indices.len() as u32, mi.len() as u32);
        verts.extend_from_slice(&mv);
        indices.extend(mi.iter().map(|i| i + base as u16));
    }
    (verts, indices, ranges)
}

/// A flat bar from the origin along +X, one unit long and one unit thick, facing +Z.
/// Anchored at its start rather than its centre so a limb can be placed at its joint.
pub fn build_rod() -> (Vec<PropVertex>, Vec<u16>) {
    let white = [1.0f32, 1.0, 1.0];
    let n = [0.0f32, 0.0, 1.0];
    let v: Vec<PropVertex> = [
        [0.0f32, -0.5, 0.0],
        [1.0, -0.5, 0.0],
        [1.0, 0.5, 0.0],
        [0.0, 0.5, 0.0],
    ]
    .into_iter()
    .map(|pos| PropVertex {
        pos,
        nrm: n,
        col: white,
        uv: [0.0; 2],
    })
    .collect();
    (v, vec![0, 1, 2, 0, 2, 3])
}

/// A unit-radius filled circle in the XY plane, facing +Z.
pub fn build_disc() -> (Vec<PropVertex>, Vec<u16>) {
    const SEGS: usize = 22;
    let white = [1.0f32, 1.0, 1.0];
    let n = [0.0f32, 0.0, 1.0];
    let mut v = vec![PropVertex {
        pos: [0.0, 0.0, 0.0],
        nrm: n,
        col: white,
        uv: [0.0; 2],
    }];
    let mut idx = Vec::new();
    for i in 0..SEGS {
        let a = i as f32 / SEGS as f32 * std::f32::consts::TAU;
        let (sa, ca) = a.sin_cos();
        v.push(PropVertex {
            pos: [ca, sa, 0.0],
            nrm: n,
            col: white,
            uv: [0.0; 2],
        });
        idx.extend_from_slice(&[0, 1 + i as u16, 1 + ((i + 1) % SEGS) as u16]);
    }
    (v, idx)
}

/// A one-unit-wide textured billboard. The source image's aspect is carried in the
/// mesh so callers can use one uniform scale for both dimensions.
fn build_textured_quad(height_over_width: f32) -> (Vec<PropVertex>, Vec<u16>) {
    let half_h = 0.5 * height_over_width;
    let n = [0.0f32, 0.0, 1.0];
    let white = [1.0f32, 1.0, 1.0];
    let v = vec![
        PropVertex {
            pos: [-0.5, -half_h, 0.0],
            nrm: n,
            col: white,
            uv: [0.0, 1.0],
        },
        PropVertex {
            pos: [0.5, -half_h, 0.0],
            nrm: n,
            col: white,
            uv: [1.0, 1.0],
        },
        PropVertex {
            pos: [0.5, half_h, 0.0],
            nrm: n,
            col: white,
            uv: [1.0, 0.0],
        },
        PropVertex {
            pos: [-0.5, half_h, 0.0],
            nrm: n,
            col: white,
            uv: [0.0, 0.0],
        },
    ];
    (v, vec![0, 1, 2, 0, 2, 3])
}

const BEE_LEG_CELL_W: f32 = 112.0;
const BEE_LEG_CELL_H: f32 = 88.0;
const BEE_LEG_ATLAS_W: f32 = BEE_LEG_CELL_W * 3.0;
const BEE_LEG_ATLAS_H: f32 = BEE_LEG_CELL_H * 2.0;
/// Pivot locations inside the six atlas cells, in cell pixels. They sit just inside
/// the body silhouette, so the overlaid thorax hides every cropped attachment seam.
const BEE_LEG_CELL_PIVOTS: [[f32; 2]; 6] = [
    [75.0, 66.0],
    [35.0, 67.0],
    [22.0, 69.0],
    [73.0, 24.0],
    [33.0, 23.0],
    [18.0, 26.0],
];

/// One atlas cell, with its local origin at the leg's body attachment rather than at
/// the quad centre. Rotating the instance therefore moves only that leg.
pub fn build_bee_leg_quad(slot: usize) -> (Vec<PropVertex>, Vec<u16>) {
    assert!(slot < 6);
    let col = slot % 3;
    let row = slot / 3;
    let [pivot_x, pivot_y] = BEE_LEG_CELL_PIVOTS[slot];
    let left = -pivot_x / BEE_LEG_CELL_W;
    let right = (BEE_LEG_CELL_W - pivot_x) / BEE_LEG_CELL_W;
    let top = pivot_y / BEE_LEG_CELL_W;
    let bottom = (pivot_y - BEE_LEG_CELL_H) / BEE_LEG_CELL_W;
    // Inset half a texel so linear filtering cannot borrow a neighbouring leg cell.
    let u0 = (col as f32 * BEE_LEG_CELL_W + 0.5) / BEE_LEG_ATLAS_W;
    let u1 = ((col + 1) as f32 * BEE_LEG_CELL_W - 0.5) / BEE_LEG_ATLAS_W;
    let v0 = (row as f32 * BEE_LEG_CELL_H + 0.5) / BEE_LEG_ATLAS_H;
    let v1 = ((row + 1) as f32 * BEE_LEG_CELL_H - 0.5) / BEE_LEG_ATLAS_H;
    let n = [0.0f32, 0.0, 1.0];
    let col = [1.0f32, 1.0, 1.0];
    let v = vec![
        PropVertex {
            pos: [left, bottom, 0.0],
            nrm: n,
            col,
            uv: [u0, v1],
        },
        PropVertex {
            pos: [right, bottom, 0.0],
            nrm: n,
            col,
            uv: [u1, v1],
        },
        PropVertex {
            pos: [right, top, 0.0],
            nrm: n,
            col,
            uv: [u1, v0],
        },
        PropVertex {
            pos: [left, top, 0.0],
            nrm: n,
            col,
            uv: [u0, v0],
        },
    ];
    (v, vec![0, 1, 2, 0, 2, 3])
}

/// The top-down source faces left, so yaw zero is already its usual travel direction.
pub fn build_bee_body_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(269.0 / 420.0)
}

pub fn build_tiki_mask_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(512.0 / 223.0)
}

pub fn build_astronaut_helmet_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(1.0)
}

pub fn build_noh_mask_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(384.0 / 290.0)
}

pub fn build_monkey_mask_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(373.0 / 384.0)
}

pub fn build_comedy_tragedy_mask_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(384.0 / 293.0)
}

pub fn build_penguin_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(384.0 / 305.0)
}

const GODZILLA_SOURCE_W: f32 = 384.0;
const GODZILLA_SOURCE_H: f32 = 266.0;
const GODZILLA_LEG_ATLAS_W: f32 = GODZILLA_SOURCE_W * 4.0;
/// Slots are rear thigh, rear shin, front thigh, front shin. Hip and knee pivots sit
/// inside the overlaps retained by the layer above them.
const GODZILLA_LEG_PIVOTS: [[f32; 2]; 4] = [
    [184.0, 132.0],
    [158.0, 199.0],
    [234.0, 135.0],
    [248.0, 211.0],
];
/// The distal tail layer overlaps the body by eight source pixels around this narrow
/// joint. Pitching around the overlap keeps the cut registered at rest and hides the
/// seam under the body while the tip recedes.
const GODZILLA_TAIL_PIVOT: [f32; 2] = [86.0, 218.0];

pub fn build_godzilla_body_quad() -> (Vec<PropVertex>, Vec<u16>) {
    build_textured_quad(266.0 / 384.0)
}

/// Full-canvas tail layer anchored at its cut rather than the source-image centre.
/// The transparent canvas preserves exact registration with the body at pitch zero.
pub fn build_godzilla_tail_quad() -> (Vec<PropVertex>, Vec<u16>) {
    let [pivot_x, pivot_y] = GODZILLA_TAIL_PIVOT;
    let left = -pivot_x / GODZILLA_SOURCE_W;
    let right = (GODZILLA_SOURCE_W - pivot_x) / GODZILLA_SOURCE_W;
    let top = pivot_y / GODZILLA_SOURCE_W;
    let bottom = (pivot_y - GODZILLA_SOURCE_H) / GODZILLA_SOURCE_W;
    let n = [0.0f32, 0.0, 1.0];
    let col = [1.0f32, 1.0, 1.0];
    let v = vec![
        PropVertex {
            pos: [left, bottom, 0.0],
            nrm: n,
            col,
            uv: [0.0, 1.0],
        },
        PropVertex {
            pos: [right, bottom, 0.0],
            nrm: n,
            col,
            uv: [1.0, 1.0],
        },
        PropVertex {
            pos: [right, top, 0.0],
            nrm: n,
            col,
            uv: [1.0, 0.0],
        },
        PropVertex {
            pos: [left, top, 0.0],
            nrm: n,
            col,
            uv: [0.0, 0.0],
        },
    ];
    (v, vec![0, 1, 2, 0, 2, 3])
}

/// One full-canvas Godzilla leg cell, anchored at its hip rather than at the image
/// centre. The generous transparent canvas preserves the source registration.
pub fn build_godzilla_leg_quad(slot: usize) -> (Vec<PropVertex>, Vec<u16>) {
    assert!(slot < 4);
    let [pivot_x, pivot_y] = GODZILLA_LEG_PIVOTS[slot];
    let left = -pivot_x / GODZILLA_SOURCE_W;
    let right = (GODZILLA_SOURCE_W - pivot_x) / GODZILLA_SOURCE_W;
    let top = pivot_y / GODZILLA_SOURCE_W;
    let bottom = (pivot_y - GODZILLA_SOURCE_H) / GODZILLA_SOURCE_W;
    let u0 = (slot as f32 * GODZILLA_SOURCE_W + 0.5) / GODZILLA_LEG_ATLAS_W;
    let u1 = ((slot + 1) as f32 * GODZILLA_SOURCE_W - 0.5) / GODZILLA_LEG_ATLAS_W;
    let n = [0.0f32, 0.0, 1.0];
    let col = [1.0f32, 1.0, 1.0];
    let v = vec![
        PropVertex {
            pos: [left, bottom, 0.0],
            nrm: n,
            col,
            uv: [u0, 1.0],
        },
        PropVertex {
            pos: [right, bottom, 0.0],
            nrm: n,
            col,
            uv: [u1, 1.0],
        },
        PropVertex {
            pos: [right, top, 0.0],
            nrm: n,
            col,
            uv: [u1, 0.0],
        },
        PropVertex {
            pos: [left, top, 0.0],
            nrm: n,
            col,
            uv: [u0, 0.0],
        },
    ];
    (v, vec![0, 1, 2, 0, 2, 3])
}

const FOG_ATLAS_COLS: usize = 4;
const FOG_ATLAS_ROWS: usize = 2;

/// One square cell of the generated 4×2 fog atlas. Half-texel insets keep filtering
/// from borrowing the neighbouring cloud at the very soft transparent edges.
pub fn build_fog_blob_quad(slot: usize) -> (Vec<PropVertex>, Vec<u16>) {
    assert!(slot < FOG_ATLAS_COLS * FOG_ATLAS_ROWS);
    let atlas_col = slot % FOG_ATLAS_COLS;
    let atlas_row = slot / FOG_ATLAS_COLS;
    let u0 = (atlas_col as f32 + 0.5 / 256.0) / FOG_ATLAS_COLS as f32;
    let u1 = (atlas_col as f32 + 1.0 - 0.5 / 256.0) / FOG_ATLAS_COLS as f32;
    let v0 = (atlas_row as f32 + 0.5 / 256.0) / FOG_ATLAS_ROWS as f32;
    let v1 = (atlas_row as f32 + 1.0 - 0.5 / 256.0) / FOG_ATLAS_ROWS as f32;
    let n = [0.0f32, 0.0, 1.0];
    let col = [1.0f32, 1.0, 1.0];
    let v = vec![
        PropVertex {
            pos: [-0.5, -0.5, 0.0],
            nrm: n,
            col,
            uv: [u0, v1],
        },
        PropVertex {
            pos: [0.5, -0.5, 0.0],
            nrm: n,
            col,
            uv: [u1, v1],
        },
        PropVertex {
            pos: [0.5, 0.5, 0.0],
            nrm: n,
            col,
            uv: [u1, v0],
        },
        PropVertex {
            pos: [-0.5, 0.5, 0.0],
            nrm: n,
            col,
            uv: [u0, v0],
        },
    ];
    (v, vec![0, 1, 2, 0, 2, 3])
}

/// Adds a coloured superellipsoid to a procedural prop mesh. Exponents below one
/// square the silhouette while retaining pillowy rounded edges; one is an ordinary
/// ellipsoid. The modest tessellation is softened by lighting and the distant blur.
fn append_superellipsoid(
    verts: &mut Vec<PropVertex>,
    indices: &mut Vec<u16>,
    center: [f32; 3],
    radii: [f32; 3],
    exponent: f32,
    rot_z: f32,
    col: [f32; 3],
) {
    const LATS: usize = 12;
    const LONS: usize = 20;
    let base = verts.len() as u16;
    let signed_pow = |v: f32| v.signum() * v.abs().powf(exponent);
    let (sr, cr) = rot_z.sin_cos();
    for lat in 0..=LATS {
        let latitude =
            -std::f32::consts::FRAC_PI_2 + lat as f32 / LATS as f32 * std::f32::consts::PI;
        let (slat, clat) = latitude.sin_cos();
        for lon in 0..LONS {
            let longitude = lon as f32 / LONS as f32 * std::f32::consts::TAU;
            let (slon, clon) = longitude.sin_cos();
            let p = [
                radii[0] * signed_pow(clat) * signed_pow(clon),
                radii[1] * signed_pow(slat),
                radii[2] * signed_pow(clat) * signed_pow(slon),
            ];
            // The ellipsoid gradient is a stable, soft approximation to the exact
            // superquadric normal at the rounded corners.
            let n0 = [
                p[0] / (radii[0] * radii[0]).max(1e-6),
                p[1] / (radii[1] * radii[1]).max(1e-6),
                p[2] / (radii[2] * radii[2]).max(1e-6),
            ];
            let nl = (n0[0] * n0[0] + n0[1] * n0[1] + n0[2] * n0[2])
                .sqrt()
                .max(1e-6);
            let n0 = [n0[0] / nl, n0[1] / nl, n0[2] / nl];
            verts.push(PropVertex {
                pos: [
                    center[0] + p[0] * cr - p[1] * sr,
                    center[1] + p[0] * sr + p[1] * cr,
                    center[2] + p[2],
                ],
                nrm: [n0[0] * cr - n0[1] * sr, n0[0] * sr + n0[1] * cr, n0[2]],
                col,
                uv: [0.0; 2],
            });
        }
    }
    for lat in 0..LATS {
        for lon in 0..LONS {
            let a = base + (lat * LONS + lon) as u16;
            let b = base + (lat * LONS + (lon + 1) % LONS) as u16;
            let c = base + ((lat + 1) * LONS + lon) as u16;
            let d = base + ((lat + 1) * LONS + (lon + 1) % LONS) as u16;
            indices.extend_from_slice(&[a, b, c, b, d, c]);
        }
    }
}

/// A slightly thick triangular cloth panel facing the camera. Side faces matter when
/// the sailor turns away, so the neckerchief remains an object rather than a decal.
fn append_triangle_prism(
    verts: &mut Vec<PropVertex>,
    indices: &mut Vec<u16>,
    points: [[f32; 2]; 3],
    z: f32,
    thickness: f32,
    col: [f32; 3],
) {
    let base = verts.len() as u16;
    for (depth, normal) in [
        (z - thickness * 0.5, [0.0, 0.0, -1.0]),
        (z + thickness * 0.5, [0.0, 0.0, 1.0]),
    ] {
        for p in points {
            verts.push(PropVertex {
                pos: [p[0], p[1], depth],
                nrm: normal,
                col,
                uv: [0.0; 2],
            });
        }
    }
    indices.extend_from_slice(&[base, base + 2, base + 1, base + 3, base + 4, base + 5]);
    for edge in 0u16..3 {
        let next = (edge + 1) % 3;
        let a = base + edge;
        let b = base + next;
        let c = base + 3 + edge;
        let d = base + 3 + next;
        indices.extend_from_slice(&[a, b, c, b, d, c]);
    }
}

/// Shared unit marshmallow puff used for arms, hands, thighs, shins and feet.
pub fn build_marshmallow_puff() -> (Vec<PropVertex>, Vec<u16>) {
    let mut verts = Vec::new();
    let mut indices = Vec::new();
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0; 3],
        [1.0; 3],
        0.78,
        0.0,
        // Pure white vanishes against the light-gray page after the 33% distant
        // composite and 45% foreground fog. This cool shadow-white still reads as
        // marshmallow foam while retaining the articulated silhouette.
        srgb_hex_to_linear(0xa8b6b3),
    );
    (verts, indices)
}

/// Big pillowy torso with a dimensional sailor collar and bright neckerchief.
/// Coordinates are fractions of the complete character height, measured upward from
/// the soles, so every decorative feature follows the root's genuine 3D turn.
pub fn build_marshmallow_body() -> (Vec<PropVertex>, Vec<u16>) {
    let white = srgb_hex_to_linear(0xa8b6b3);
    let navy = srgb_hex_to_linear(0x071722);
    let stripe = srgb_hex_to_linear(0xeaf2ed);
    let red = srgb_hex_to_linear(0xd12a3c);
    let mut verts = Vec::new();
    let mut indices = Vec::new();
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.505, 0.0],
        [0.275, 0.255, 0.215],
        0.72,
        0.0,
        white,
    );

    // Broad navy bib and its pale inset V remain legible through the gray fog.
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.657, 0.214],
        [0.250, 0.070, 0.018],
        0.64,
        0.0,
        navy,
    );
    for (center, angle) in [
        ([-0.070, 0.657, 0.236], -0.47),
        ([0.070, 0.657, 0.236], 0.47),
    ] {
        append_superellipsoid(
            &mut verts,
            &mut indices,
            center,
            [0.009, 0.085, 0.010],
            0.82,
            angle,
            stripe,
        );
    }

    // Knot plus two asymmetric tails: recognisable in front view without textures.
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.595, 0.254],
        [0.039, 0.032, 0.027],
        0.72,
        0.0,
        red,
    );
    append_triangle_prism(
        &mut verts,
        &mut indices,
        [[-0.025, 0.582], [-0.005, 0.572], [-0.066, 0.468]],
        0.250,
        0.020,
        red,
    );
    append_triangle_prism(
        &mut verts,
        &mut indices,
        [[0.008, 0.573], [0.030, 0.583], [0.055, 0.480]],
        0.251,
        0.020,
        red,
    );
    (verts, indices)
}

/// A rounded-square marshmallow head with face and a jaunty sailor cap. It is
/// authored around its own neck pivot so animation can let the eyes lead the turn.
pub fn build_marshmallow_head() -> (Vec<PropVertex>, Vec<u16>) {
    let skin = srgb_hex_to_linear(0xa8b6b3);
    let cap_white = srgb_hex_to_linear(0xf7f8f5);
    let navy = srgb_hex_to_linear(0x071722);
    let black = srgb_hex_to_linear(0x111820);
    let mouth_red = srgb_hex_to_linear(0x9f2638);
    let cheek = srgb_hex_to_linear(0xb7dce2);
    let mut verts = Vec::new();
    let mut indices = Vec::new();

    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.0, 0.0],
        [0.168, 0.137, 0.150],
        0.55,
        0.0,
        skin,
    );
    for x in [-0.060f32, 0.060] {
        append_superellipsoid(
            &mut verts,
            &mut indices,
            [x, 0.025, 0.151],
            [0.026, 0.041, 0.016],
            0.72,
            0.0,
            black,
        );
    }
    for x in [-0.105f32, 0.105] {
        append_superellipsoid(
            &mut verts,
            &mut indices,
            [x, -0.031, 0.143],
            [0.031, 0.019, 0.008],
            0.78,
            0.0,
            cheek,
        );
    }
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, -0.054, 0.153],
        [0.072, 0.042, 0.016],
        0.70,
        0.0,
        black,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, -0.069, 0.163],
        [0.036, 0.014, 0.008],
        0.82,
        0.0,
        mouth_red,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, -0.006, 0.161],
        [0.025, 0.021, 0.020],
        0.75,
        0.0,
        skin,
    );

    // The cap is a stack of squashed superellipsoids, tilted like the references.
    // Anchor the brim over the head and let the upper pieces follow that tilt;
    // shifting the entire stack made the hat look detached during camera turns.
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.142, -0.005],
        [0.205, 0.023, 0.155],
        0.70,
        -0.13,
        cap_white,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [-0.0045, 0.176, -0.006],
        [0.147, 0.041, 0.126],
        0.64,
        -0.13,
        navy,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [-0.010, 0.217, -0.008],
        [0.137, 0.050, 0.116],
        0.64,
        -0.13,
        cap_white,
    );
    (verts, indices)
}

/// A compact silver saucer with a blue glass dome, built as a surface of revolution
/// about world +Z. Dimensions are authored directly in scene pixels so an instance at
/// scale one is a little wider than a tile.
pub fn build_ufo() -> (Vec<PropVertex>, Vec<u16>) {
    const RADIAL: usize = 32;
    let dark = srgb_hex_to_linear(0x27323b);
    let lower = srgb_hex_to_linear(0x77858d);
    let rim = srgb_hex_to_linear(0xdce3e5);
    let upper = srgb_hex_to_linear(0xaebbc0);
    let glass_base = srgb_hex_to_linear(0x238fab);
    let glass = srgb_hex_to_linear(0x6bdcf2);
    // (radius, z, colour), traced from the lower centre over the rim to the dome.
    let profile = [
        (4.0f32, -12.0f32, dark),
        (34.0, -10.0, lower),
        (58.0, -3.0, rim),
        (62.0, 1.5, rim),
        (45.0, 7.0, upper),
        (36.0, 9.0, upper),
        // Duplicate the base ring so the cyan canopy starts at a hard material seam
        // instead of blending gradually into the silver wing.
        (32.0, 10.0, dark),
        (32.0, 10.0, glass_base),
        (29.0, 17.0, glass_base),
        (24.0, 26.0, glass),
        (14.0, 34.0, glass),
        (2.0, 38.0, glass),
    ];
    let mut verts = Vec::with_capacity(profile.len() * RADIAL + 600);
    let mut indices = Vec::new();
    for (row, &(radius, z, col)) in profile.iter().enumerate() {
        let prev = profile[row.saturating_sub(1)];
        let next = profile[(row + 1).min(profile.len() - 1)];
        let dr = next.0 - prev.0;
        let dz = next.1 - prev.1;
        let nl = (dz * dz + dr * dr).sqrt().max(1e-6);
        for k in 0..RADIAL {
            let a = k as f32 / RADIAL as f32 * std::f32::consts::TAU;
            let (sa, ca) = a.sin_cos();
            verts.push(PropVertex {
                pos: [radius * ca, radius * sa, z],
                nrm: [dz / nl * ca, dz / nl * sa, -dr / nl],
                col,
                uv: [0.0; 2],
            });
        }
    }
    for row in 0..profile.len() - 1 {
        if profile[row].0 == profile[row + 1].0 && profile[row].1 == profile[row + 1].1 {
            continue;
        }
        for k in 0..RADIAL {
            let a = (row * RADIAL + k) as u16;
            let b = (row * RADIAL + (k + 1) % RADIAL) as u16;
            let c = ((row + 1) * RADIAL + k) as u16;
            let d = ((row + 1) * RADIAL + (k + 1) % RADIAL) as u16;
            indices.extend_from_slice(&[a, b, c, b, d, c]);
        }
    }

    // Eight warm glass portholes set into dark bezels around the silver upper ring.
    // Small flattened spheres remain circular from above but protrude clearly when
    // the saucer twists into profile.
    let mut append_porthole = |center: [f32; 3], radius_xy: f32, radius_z: f32, col: [f32; 3]| {
        const LATS: usize = 5;
        const LONS: usize = 10;
        let base = verts.len() as u16;
        for lat in 0..=LATS {
            let theta = lat as f32 / LATS as f32 * std::f32::consts::PI;
            let (st, ct) = theta.sin_cos();
            for lon in 0..LONS {
                let phi = lon as f32 / LONS as f32 * std::f32::consts::TAU;
                let (sp, cp) = phi.sin_cos();
                verts.push(PropVertex {
                    pos: [
                        center[0] + radius_xy * st * cp,
                        center[1] + radius_xy * st * sp,
                        center[2] + radius_z * ct,
                    ],
                    nrm: [st * cp, st * sp, ct],
                    col,
                    uv: [0.0; 2],
                });
            }
        }
        for lat in 0..LATS {
            for lon in 0..LONS {
                let a = base + (lat * LONS + lon) as u16;
                let b = base + (lat * LONS + (lon + 1) % LONS) as u16;
                let c = base + ((lat + 1) * LONS + lon) as u16;
                let d = base + ((lat + 1) * LONS + (lon + 1) % LONS) as u16;
                indices.extend_from_slice(&[a, b, c, b, d, c]);
            }
        }
    };
    let bezel = srgb_hex_to_linear(0x243640);
    let window = srgb_hex_to_linear(0xffc84f);
    for port in 0..8 {
        let a = std::f32::consts::PI / 8.0 + port as f32 * std::f32::consts::TAU / 8.0;
        let (sa, ca) = a.sin_cos();
        let center = [47.0 * ca, 47.0 * sa, 8.0];
        append_porthole(center, 5.0, 3.2, bezel);
        append_porthole([center[0], center[1], center[2] + 2.0], 3.35, 2.6, window);
    }
    (verts, indices)
}

/// The same rounded prism used by the field, recentered so a carried tile spins about
/// its middle instead of orbiting around its lower face.
pub fn build_captured_tile() -> (Vec<PropVertex>, Vec<u16>) {
    let (verts, indices) = build_tile_mesh();
    let white = [1.0f32, 1.0, 1.0];
    (
        verts
            .into_iter()
            .map(|v| PropVertex {
                pos: [v.pos[0], v.pos[1], v.pos[2] - THICK * 0.5],
                nrm: v.nrm,
                col: white,
                uv: [0.0; 2],
            })
            .collect(),
        indices,
    )
}

/// A filled unit-height cone. Layered instances stretch it between the moving tile
/// and saucer to make a broad translucent tractor beam with a bright inner core.
pub fn build_ufo_beam() -> (Vec<PropVertex>, Vec<u16>) {
    const SEGS: usize = 32;
    let mut verts = Vec::with_capacity(SEGS * 2 + 1);
    let mut indices = Vec::with_capacity(SEGS * 9);
    let white = [1.0f32, 1.0, 1.0];
    for z in [0.0f32, 1.0] {
        let radius = if z < 0.5 { 1.0 } else { 0.22 };
        for k in 0..SEGS {
            let a = k as f32 / SEGS as f32 * std::f32::consts::TAU;
            let (sa, ca) = a.sin_cos();
            let n = [ca, sa, 0.78];
            let nl = (n[0] * n[0] + n[1] * n[1] + n[2] * n[2]).sqrt();
            verts.push(PropVertex {
                pos: [radius * ca, radius * sa, z],
                nrm: [n[0] / nl, n[1] / nl, n[2] / nl],
                col: white,
                uv: [0.0; 2],
            });
        }
    }
    for k in 0..SEGS {
        let a = k as u16;
        let b = ((k + 1) % SEGS) as u16;
        let c = (SEGS + k) as u16;
        let d = (SEGS + (k + 1) % SEGS) as u16;
        indices.extend_from_slice(&[a, b, c, b, d, c]);
    }
    // A cap makes the footprint read as a solid pool of light rather than just a
    // yellow ring when viewed from directly above.
    let center = verts.len() as u16;
    verts.push(PropVertex {
        pos: [0.0, 0.0, 0.0],
        nrm: [0.0, 0.0, -1.0],
        col: white,
        uv: [0.0; 2],
    });
    for k in 0..SEGS {
        indices.extend_from_slice(&[center, ((k + 1) % SEGS) as u16, k as u16]);
    }
    (verts, indices)
}

/// Petrol-blue underbody of the tile mimic. The broad copper side cheeks remain
/// visible beneath its oversized tile shell and make the little mechanism legible
/// against both grey and coloured cells.
pub fn build_mimic_body() -> (Vec<PropVertex>, Vec<u16>) {
    let shell_shadow = srgb_hex_to_linear(0x132f35);
    let body = srgb_hex_to_linear(0x1f5961);
    let copper = srgb_hex_to_linear(0xb76f3f);
    let rivet = srgb_hex_to_linear(0xf2c36e);
    let mut verts = Vec::new();
    let mut indices = Vec::new();
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.0, 0.0],
        [24.0, 18.0, 7.4],
        0.70,
        0.0,
        shell_shadow,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [5.0, 0.0, 5.2],
        [18.0, 14.5, 4.8],
        0.74,
        0.0,
        body,
    );
    for side in [-1.0f32, 1.0] {
        append_superellipsoid(
            &mut verts,
            &mut indices,
            [-1.5, side * 17.2, 1.0],
            [13.0, 3.1, 4.0],
            0.72,
            -0.04 * side,
            copper,
        );
        for x in [-8.0f32, 7.0] {
            append_superellipsoid(
                &mut verts,
                &mut indices,
                [x, side * 19.9, 1.8],
                [1.8, 1.4, 1.8],
                0.88,
                0.0,
                rivet,
            );
        }
    }
    (verts, indices)
}

/// A rounded unit segment running from the origin to +X. Non-uniform instance
/// scaling turns it into every leg bone, eye stalk, and claw wrist.
pub fn build_mimic_segment() -> (Vec<PropVertex>, Vec<u16>) {
    let mut verts = Vec::new();
    let mut indices = Vec::new();
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.5, 0.0, 0.0],
        [0.5, 0.5, 0.5],
        0.82,
        0.0,
        [1.0; 3],
    );
    (verts, indices)
}

/// One swivelling ivory eye, authored looking down local +X. A raised petrol pupil
/// and pinprick highlight keep its gaze readable even at the creature's small scale.
pub fn build_mimic_eye() -> (Vec<PropVertex>, Vec<u16>) {
    let ivory = srgb_hex_to_linear(0xf4ead2);
    let pupil = srgb_hex_to_linear(0x102b31);
    let glint = srgb_hex_to_linear(0xffffff);
    let mut verts = Vec::new();
    let mut indices = Vec::new();
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0; 3],
        [1.0, 1.0, 1.0],
        0.88,
        0.0,
        ivory,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.86, 0.0, 0.08],
        [0.30, 0.42, 0.42],
        0.86,
        0.0,
        pupil,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [1.10, 0.11, 0.23],
        [0.075, 0.085, 0.085],
        0.90,
        0.0,
        glint,
    );
    (verts, indices)
}

/// A deliberately oversized two-finger pincer. Its permanent little gap survives
/// the top-down view, while whole-claw rotation supplies tapping and clacking.
pub fn build_mimic_claw() -> (Vec<PropVertex>, Vec<u16>) {
    let palm = srgb_hex_to_linear(0xb76f3f);
    let tip = srgb_hex_to_linear(0xe0a45c);
    let dark = srgb_hex_to_linear(0x5f352b);
    let mut verts = Vec::new();
    let mut indices = Vec::new();
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.35, 0.0, 0.0],
        [0.42, 0.34, 0.28],
        0.72,
        0.0,
        palm,
    );
    for side in [-1.0f32, 1.0] {
        append_superellipsoid(
            &mut verts,
            &mut indices,
            [0.92, side * 0.25, 0.02],
            [0.64, 0.15, 0.16],
            0.76,
            side * 0.25,
            tip,
        );
        append_superellipsoid(
            &mut verts,
            &mut indices,
            [1.40, side * 0.40, 0.01],
            [0.20, 0.12, 0.13],
            0.78,
            side * 0.38,
            dark,
        );
    }
    (verts, indices)
}

/// Wind-up key rooted at z=0. It sits on the carried tile and counter-rotates against
/// the gait, an intentionally unnecessary mechanism that gives the mimic personality.
pub fn build_mimic_key() -> (Vec<PropVertex>, Vec<u16>) {
    let brass = srgb_hex_to_linear(0xe0a84e);
    let shade = srgb_hex_to_linear(0x76512b);
    let mut verts = Vec::new();
    let mut indices = Vec::new();
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.0, 4.2],
        [2.0, 2.0, 5.0],
        0.80,
        0.0,
        shade,
    );
    append_superellipsoid(
        &mut verts,
        &mut indices,
        [0.0, 0.0, 9.0],
        [11.0, 2.1, 2.1],
        0.76,
        0.0,
        brass,
    );
    for x in [-10.5f32, 10.5] {
        append_superellipsoid(
            &mut verts,
            &mut indices,
            [x, 0.0, 9.0],
            [3.6, 5.5, 2.3],
            0.72,
            0.0,
            brass,
        );
    }
    (verts, indices)
}

/// A small four-sided pyramid, apex along +Z, sized about one unit so the per-instance
/// scale sets its real size. Vertex colours are left white: embers are tinted per
/// instance, since each one burns its own shade.
pub fn build_ember() -> (Vec<PropVertex>, Vec<u16>) {
    let apex = [0.0f32, 0.0, 1.0];
    let base = [
        [-0.62f32, -0.62, -0.3],
        [0.62, -0.62, -0.3],
        [0.62, 0.62, -0.3],
        [-0.62, 0.62, -0.3],
    ];
    let mut v = Vec::new();
    let mut idx = Vec::new();
    let white = [1.0f32, 1.0, 1.0];

    // Flat-shaded faces, so each triangle carries its own normal.
    let mut face = |a: [f32; 3], b: [f32; 3], c: [f32; 3]| {
        let u = [b[0] - a[0], b[1] - a[1], b[2] - a[2]];
        let w = [c[0] - a[0], c[1] - a[1], c[2] - a[2]];
        let n = [
            u[1] * w[2] - u[2] * w[1],
            u[2] * w[0] - u[0] * w[2],
            u[0] * w[1] - u[1] * w[0],
        ];
        let l = (n[0] * n[0] + n[1] * n[1] + n[2] * n[2]).sqrt().max(1e-6);
        let n = [n[0] / l, n[1] / l, n[2] / l];
        for pos in [a, b, c] {
            idx.push(v.len() as u16);
            v.push(PropVertex {
                pos,
                nrm: n,
                col: white,
                uv: [0.0; 2],
            });
        }
    };
    for i in 0..4 {
        face(base[i], base[(i + 1) % 4], apex);
    }
    face(base[0], base[2], base[1]);
    face(base[0], base[3], base[2]);
    (v, idx)
}

/// Nose-to-tail length of the rocket model, in the same pixel units as everything
/// else. Roughly a cell and a half, so it reads as a visitor rather than as scenery.
pub const ROCKET_LENGTH: f32 = 128.0 * 0.80;
/// A relaxed full axial turn over nine seconds shows all three fins without making the
/// rocket read as a drill bit.
const ROCKET_AXIAL_ROLL_RATE: f32 = std::f32::consts::TAU / 9.0;

/// Cartoon rocket palette, as sRGB hex.
const ROCKET_SHELL: u32 = 0xf6f4f5;
const ROCKET_RED: u32 = 0xe8283b;
const ROCKET_NOZZLE: u32 = 0x3f8fd2;
const ROCKET_RIM: u32 = 0x2c2a44;
const ROCKET_RING: u32 = 0x2f7ed0;
const ROCKET_GLASS: u32 = 0xcbe4f7;

/// Radius of the hull at `u` along its axis, 0 at the tail and 1 at the nose tip.
///
/// One expression covers the whole silhouette: a sine lobe sampled over a window that
/// stops short of zero at the tail, so the body starts out already wide, swells to a
/// belly just past the middle and closes to a point at the nose. Raising it to a
/// fractional power fattens the shoulders, which is what makes it read as a stubby
/// cartoon rocket rather than a missile.
fn rocket_radius(u: f32) -> f32 {
    // The clamp on the sine is load-bearing: at u = 1 the argument is exactly PI, and
    // f32 puts sin(PI) a hair *below* zero, so raising it to a fractional power there
    // yields NaN. The rasterizer quietly drops the resulting triangles, which shows up
    // only as a nose that never quite comes to a point.
    (std::f32::consts::PI * (0.08 + 0.92 * u.clamp(0.0, 1.0)))
        .sin()
        .max(0.0)
        .powf(0.55)
}

/// Builds a prototypical cartoon rocket, nose pointing along +X, centred on the
/// origin, `length` long: white ogive hull with a red nose cone, two portholes 180
/// degrees apart around the hull, three swept fins and a nozzle at the tail.
///
/// Everything is a surface of revolution or an extruded blade, so the whole model
/// falls out of a handful of parameters rather than any authored geometry.
pub fn build_rocket(length: f32) -> (Vec<PropVertex>, Vec<u16>) {
    /// Where the red cone begins, as a fraction along the hull.
    const NOSE: f32 = 0.72;
    /// Radial segments around the hull.
    const RADIAL: usize = 24;
    /// Samples along the hull.
    const AXIAL: usize = 34;
    const FINS: usize = 3;

    let max_r = length * 0.21;
    let x_of = |u: f32| (u - 0.42) * length;
    let r_of = |u: f32| rocket_radius(u) * max_r;

    let mut v: Vec<PropVertex> = Vec::new();
    let mut idx: Vec<u16> = Vec::new();
    let shell = srgb_hex_to_linear(ROCKET_SHELL);
    let red = srgb_hex_to_linear(ROCKET_RED);

    // ---- hull: a surface of revolution about +X ----------------------------
    // The seam where white meets red is sampled twice, so the colour changes across
    // an edge instead of blending over the shoulder.
    let mut us: Vec<(f32, [f32; 3])> = Vec::new();
    for i in 0..=AXIAL {
        let u = i as f32 / AXIAL as f32;
        let past = u >= NOSE;
        if i > 0 && past && us.last().map(|(pu, _)| *pu < NOSE).unwrap_or(false) {
            us.push((NOSE, shell));
            us.push((NOSE, red));
        }
        us.push((u, if past { red } else { shell }));
    }

    let ring0 = v.len();
    for (u, col) in &us {
        // Slope of the profile gives the correct normal tilt along the axis.
        let d = 0.004f32;
        let dr = (r_of((u + d).min(1.0)) - r_of((u - d).max(0.0))) / ((2.0 * d) * length);
        for k in 0..RADIAL {
            let a = k as f32 / RADIAL as f32 * std::f32::consts::TAU;
            let (sa, ca) = a.sin_cos();
            let r = r_of(*u);
            let n = [-dr, ca, sa];
            let len = (n[0] * n[0] + n[1] * n[1] + n[2] * n[2]).sqrt().max(1e-6);
            v.push(PropVertex {
                pos: [x_of(*u), r * ca, r * sa],
                nrm: [n[0] / len, n[1] / len, n[2] / len],
                col: *col,
                uv: [0.0; 2],
            });
        }
    }
    for row in 0..us.len() - 1 {
        // A duplicated seam row has zero height; skip it rather than emit slivers.
        if us[row].0 == us[row + 1].0 {
            continue;
        }
        for k in 0..RADIAL {
            let k1 = (k + 1) % RADIAL;
            let a = (ring0 + row * RADIAL + k) as u16;
            let b = (ring0 + row * RADIAL + k1) as u16;
            let c = (ring0 + (row + 1) * RADIAL + k) as u16;
            let d = (ring0 + (row + 1) * RADIAL + k1) as u16;
            idx.extend_from_slice(&[a, c, d, a, d, b]);
        }
    }

    // ---- tail cap and nozzle ------------------------------------------------
    let nozzle = srgb_hex_to_linear(ROCKET_NOZZLE);
    let tail_r = r_of(0.0) * 0.92;
    let tail_x = x_of(0.0);
    let bell_x = tail_x - length * 0.07;
    let bell_r = tail_r * 0.86;
    let base = v.len();
    for k in 0..RADIAL {
        let a = k as f32 / RADIAL as f32 * std::f32::consts::TAU;
        let (sa, ca) = a.sin_cos();
        v.push(PropVertex {
            pos: [tail_x, tail_r * ca, tail_r * sa],
            nrm: [0.0, ca, sa],
            col: nozzle,
            uv: [0.0; 2],
        });
        v.push(PropVertex {
            pos: [bell_x, bell_r * ca, bell_r * sa],
            nrm: [0.0, ca, sa],
            col: nozzle,
            uv: [0.0; 2],
        });
    }
    let bell_c = v.len() as u16;
    v.push(PropVertex {
        pos: [bell_x, 0.0, 0.0],
        nrm: [-1.0, 0.0, 0.0],
        col: nozzle,
        uv: [0.0; 2],
    });
    for k in 0..RADIAL {
        let k1 = (k + 1) % RADIAL;
        let (a, b) = ((base + k * 2) as u16, (base + k * 2 + 1) as u16);
        let (c, d) = ((base + k1 * 2) as u16, (base + k1 * 2 + 1) as u16);
        idx.extend_from_slice(&[a, b, d, a, d, c]);
        idx.extend_from_slice(&[bell_c, d, b]);
    }

    // ---- fins: swept blades, extruded from a curve pair ---------------------
    let fin_t = length * 0.023;
    for f in 0..FINS {
        // Rolled so the trio reads the way the reference does: one fin toward the
        // camera and the other two out to either side of the silhouette. Starting the
        // set at zero instead would stack both of the others below the hull, since a
        // top-down view flattens the roll plane onto one screen axis.
        let a = std::f32::consts::FRAC_PI_2 + f as f32 * std::f32::consts::TAU / FINS as f32;
        let (sa, ca) = a.sin_cos();
        let rad = [0.0f32, ca, sa];
        let tan = [0.0f32, -sa, ca];

        const STEPS: usize = 12;
        let start = v.len();
        for i in 0..=STEPS {
            let s = i as f32 / STEPS as f32;
            // Root creeps back along the hull; the outer edge is a quadratic Bezier
            // bowing out and back past the tail, which gives the swept claw.
            let u_root = 0.28 * (1.0 - s);
            let inner = [x_of(u_root), r_of(u_root) * 0.96];
            // Bows well clear of the hull early, then hooks back in to a tip behind
            // the tail. That outward bulge is what gives the fin its cartoon heft
            // instead of leaving it a thin swept triangle.
            let (p0, p1, p2) = (
                [x_of(0.28), r_of(0.28) * 0.96],
                [x_of(0.06), max_r * 1.92],
                [tail_x - length * 0.10, max_r * 1.18],
            );
            let w = 1.0 - s;
            let outer = [
                w * w * p0[0] + 2.0 * w * s * p1[0] + s * s * p2[0],
                w * w * p0[1] + 2.0 * w * s * p1[1] + s * s * p2[1],
            ];
            for (pt, side) in [(inner, 0usize), (outer, 1usize)] {
                let _ = side;
                for sgn in [-1.0f32, 1.0] {
                    v.push(PropVertex {
                        pos: [
                            pt[0] + tan[0] * fin_t * sgn,
                            pt[1] * rad[1] + tan[1] * fin_t * sgn,
                            pt[1] * rad[2] + tan[2] * fin_t * sgn,
                        ],
                        nrm: [tan[0] * sgn, tan[1] * sgn, tan[2] * sgn],
                        col: red,
                        uv: [0.0; 2],
                    });
                }
            }
        }
        // Four vertices per step: inner-, inner+, outer-, outer+.
        for i in 0..STEPS {
            let q = (start + i * 4) as u16;
            let n = (start + (i + 1) * 4) as u16;
            // The two flat faces.
            idx.extend_from_slice(&[q, q + 2, n + 2, q, n + 2, n]);
            idx.extend_from_slice(&[q + 1, n + 3, q + 3, q + 1, n + 1, n + 3]);
            // Outer rim, joining the two faces.
            idx.extend_from_slice(&[q + 2, q + 3, n + 3, q + 2, n + 3, n + 2]);
        }
    }

    // ---- portholes: concentric discs on opposite sides of the hull ----------
    // The matching windows are exactly half a turn apart. As the rocket rolls, one
    // disappears around the flank just as the other comes into view.
    let win_u = 0.50;
    let win_z = r_of(win_u) - length * 0.012;
    let win_x = x_of(win_u);
    let rings = [
        (max_r * 0.62, length * 0.000, srgb_hex_to_linear(ROCKET_RIM)),
        (
            max_r * 0.50,
            length * 0.016,
            srgb_hex_to_linear(ROCKET_RING),
        ),
        (
            max_r * 0.38,
            length * 0.028,
            srgb_hex_to_linear(ROCKET_GLASS),
        ),
    ];
    for side in [1.0f32, -1.0] {
        for &(r, lift, col) in &rings {
            let z = side * (win_z + lift);
            let c = v.len() as u16;
            v.push(PropVertex {
                pos: [win_x, 0.0, z],
                nrm: [0.0, 0.0, side],
                col,
                uv: [0.0; 2],
            });
            let rim = v.len();
            for k in 0..RADIAL {
                let a = k as f32 / RADIAL as f32 * std::f32::consts::TAU;
                let (sa, ca) = a.sin_cos();
                v.push(PropVertex {
                    pos: [win_x + r * ca, r * sa, z],
                    nrm: [0.0, 0.0, side],
                    col,
                    uv: [0.0; 2],
                });
                v.push(PropVertex {
                    pos: [win_x + r * ca, r * sa, z - side * length * 0.03],
                    nrm: [ca, sa, 0.0],
                    col,
                    uv: [0.0; 2],
                });
            }
            for k in 0..RADIAL {
                let k1 = (k + 1) % RADIAL;
                let a = (rim + k * 2) as u16;
                let b = (rim + k1 * 2) as u16;
                let ai = (rim + k * 2 + 1) as u16;
                let bi = (rim + k1 * 2 + 1) as u16;
                if side > 0.0 {
                    idx.extend_from_slice(&[c, a, b]);
                    idx.extend_from_slice(&[a, ai, bi, a, bi, b]);
                } else {
                    idx.extend_from_slice(&[c, b, a]);
                    idx.extend_from_slice(&[a, bi, ai, a, b, bi]);
                }
            }
        }
    }

    debug_assert!(
        v.len() < u16::MAX as usize,
        "rocket outgrew a u16 index buffer"
    );
    (v, idx)
}

/// Unit quad for the halo pass, expanded to the tile footprint plus HALO_PAD in the
/// vertex shader. Wound CCW seen from +Z.
fn build_halo_quad() -> (Vec<[f32; 2]>, Vec<u16>) {
    (
        vec![[-1.0, -1.0], [1.0, -1.0], [1.0, 1.0], [-1.0, 1.0]],
        vec![0, 1, 2, 0, 2, 3],
    )
}

// ---------------------------------------------------------------------------
// GPU data
// ---------------------------------------------------------------------------

/// One tile. The grid coordinate travels with the instance rather than coming from
/// `instance_index`, because the buffer gets partitioned into solid and ghost runs
/// (see `Sim::repack`) and so is no longer in grid order.
#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable, Default)]
pub struct Instance {
    /// Destination grid x, y.
    cell: [f32; 2],
    /// alive, previously-alive (each 0.0 or 1.0)
    state: [f32; 2],
    /// shader-clock time of the last change
    t: f32,
    /// shader-clock time the last spin started, and which way it turns. The tile
    /// tumbles toward the viewer about a horizontal axis, half a turn; its profile is
    /// mirrored about its mid-plane, so that lands it exactly as it started.
    spin: [f32; 2],
    /// Which colour to wear: `PALETTE_FROM_CELL` to take it from the cell hash like a
    /// grid tile, or an index into the live palette. Critters set this, since they
    /// move between cells and should not change colour as they go.
    palette: f32,
    /// Grid coordinate this tile slides from. Equal to `cell` for every ordinary tile.
    slide_from: [f32; 2],
}

/// Sentinel for `Instance::palette`: colour comes from the cell hash.
pub const PALETTE_FROM_CELL: f32 = -1.0;
/// Palette indices a critter can ask for.
pub const PALETTE_GREY: f32 = 0.0;
pub const PALETTE_GREEN: f32 = 1.0;
pub const PALETTE_TEAL: f32 = 2.0;
pub const PALETTE_BLUE: f32 = 3.0;

impl Instance {
    /// A settled tile at an arbitrary grid coordinate. Fractional coordinates are
    /// fine — the position is just a coordinate, so a critter can sit between cells.
    pub fn tile(cell: [f32; 2], palette: f32) -> Instance {
        Instance {
            cell,
            state: [1.0, 1.0],
            t: -99.0,
            spin: [-99.0, 0.0],
            palette,
            slide_from: cell,
        }
    }

    /// The same, mid-tumble: `started` is a shader-clock time and `toward` is +1 or -1.
    pub fn spinning(mut self, started: f32, toward: f32) -> Instance {
        self.spin = [started, toward];
        self
    }
}

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
pub struct Globals {
    view_proj: [[f32; 4]; 4],
    cam: [f32; 4],
    /// xyz = key light position, w = intensity
    key: [f32; 4],
    /// xyz = fill direction (surface -> light), w = intensity
    fill: [f32; 4],
    /// time, rise, pop, thickness
    p0: [f32; 4],
    /// cols, rows, cell pitch, encode-sRGB flag
    p1: [f32; 4],
    /// sky ambient rgb, w = ground bounce fraction
    amb: [f32; 4],
    /// green cut, teal cut, ghost alpha, ghost specular boost
    p2: [f32; 4],
    /// blue cut, unused, unused, unused
    p5: [f32; 4],
    /// halo pad, glow core radius, glow strength, haze stretch
    p3: [f32; 4],
    /// tile half-extent, corner radius, unused, unused
    p4: [f32; 4],
    c_dead: [f32; 4],
    c_live: [f32; 4],
    c_green: [f32; 4],
    c_teal: [f32; 4],
    c_blue: [f32; 4],
}

/// Builds the frame uniforms. `view_h` is the viewport height in CSS pixels: the
/// camera is placed so one world unit equals one CSS pixel at z = 0.
pub fn globals_for(
    cols: usize,
    rows: usize,
    aspect: f32,
    view_h: f32,
    clock: f32,
    light_t: f32,
    encode_srgb: bool,
) -> Globals {
    let dist = (view_h * 0.5) / (FOV_Y * 0.5).tan();
    let half_w = cols as f32 * CELL_PX * 0.5;
    let half_h = rows as f32 * CELL_PX * 0.5;
    let near = (dist * 0.5).max(1.0);
    let far = (dist * dist + half_w * half_w + half_h * half_h).sqrt() + 200.0;
    let vp = mat_mul(
        &perspective_rh(FOV_Y, aspect, near, far),
        &view_from_height(dist),
    );

    // The key light drifts on a slow lissajous so a soft glint crosses the field.
    let kx = half_w * (0.70 * (light_t * 0.103).sin() + 0.28 * (light_t * 0.037).sin());
    let ky = half_h * (0.62 * (light_t * 0.081).cos() + 0.24 * (light_t * 0.029).cos());

    let fill_dir = {
        let d = [-0.34f32, 0.42, 0.84];
        let l = (d[0] * d[0] + d[1] * d[1] + d[2] * d[2]).sqrt();
        [d[0] / l, d[1] / l, d[2] / l, 0.12]
    };

    Globals {
        view_proj: to_cols(&vp),
        cam: [0.0, 0.0, dist, 0.0],
        key: [kx, ky, 1150.0, 0.40],
        fill: fill_dir,
        p0: [clock, RISE, POP, THICK],
        p1: [
            cols as f32,
            rows as f32,
            CELL_PX,
            if encode_srgb { 1.0 } else { 0.0 },
        ],
        amb: [0.50, 0.50, 0.52, 0.78],
        p2: [GREEN_CUT, TEAL_CUT, GHOST_ALPHA, GHOST_SPEC],
        p5: [BLUE_CUT, 0.0, 0.0, 0.0],
        p3: [HALO_PAD, GLOW_RADIUS, GLOW_STRENGTH, GLOW_HAZE],
        p4: [CELL_PX * TILE_FILL * 0.5, CORNER_R, 0.0, 0.0],
        c_dead: pad(srgb_hex_to_linear(C_DEAD)),
        c_live: pad(srgb_hex_to_linear(C_LIVE)),
        c_green: pad(srgb_hex_to_linear(C_GREEN)),
        c_teal: pad(srgb_hex_to_linear(C_TEAL)),
        c_blue: pad(srgb_hex_to_linear(C_BLUE)),
    }
}

// ---------------------------------------------------------------------------
// Grid simulation — Conway's Game of Life
//
// Kept deliberately separate from, and unaware of, the visualisation layer below.
// It runs ahead of what is on screen and hands out read-only views, so anything in
// the visualisation can see not just the present board but the next few generations
// and plan against them.
// ---------------------------------------------------------------------------

/// How many generations past the present are kept computed. Anything in the
/// visualisation layer can read the board at 0..=LOOKAHEAD generations from now.
pub const LOOKAHEAD: usize = 4;
/// Boards held resident: the present plus the lookahead.
const BOARDS: usize = LOOKAHEAD + 1;

pub struct Life {
    pub cols: usize,
    pub rows: usize,
    /// Ring of `BOARDS` generations. `head` indexes the present; successive entries
    /// are successive futures. Once computed a board never changes, which is what
    /// makes it safe to plan against.
    boards: Vec<Vec<bool>>,
    head: usize,
    scratch: Vec<bool>,
    nb: Vec<u8>,
    rng: Rng,
}

impl Life {
    pub fn new(cols: usize, rows: usize, rng_seed: u64) -> Self {
        let n = cols * rows;
        let mut rng = Rng::new(rng_seed);
        let seed: Vec<bool> = (0..n).map(|_| rng.f32() < SEED_DENSITY).collect();
        let mut life = Life {
            cols,
            rows,
            boards: vec![seed; BOARDS],
            head: 0,
            scratch: vec![false; n],
            nb: vec![0; n],
            rng,
        };
        // A fresh random soup collapses violently over its first several generations.
        // Burning those off here means a visitor arrives at a field that is already
        // alive rather than one mid-implosion — and it fills the lookahead on the way.
        for _ in 0..(WARMUP_GENS as usize + BOARDS) {
            life.advance(1);
        }
        life
    }

    fn slot(&self, ahead: usize) -> usize {
        (self.head + ahead.min(LOOKAHEAD)) % BOARDS
    }

    /// The board as it will be `ahead` generations from now, clamped to the lookahead.
    pub fn board(&self, ahead: usize) -> &[bool] {
        &self.boards[self.slot(ahead)]
    }

    /// Retire the present generation and compute one more into the far end of the
    /// ring, so the lookahead stays full.
    pub fn advance(&mut self, injections: u32) {
        let (w, h) = (self.cols as isize, self.rows as isize);
        let last = self.slot(LOOKAHEAD);

        // Neighbour counts of the furthest board we hold. Out of world is always off.
        {
            let src = &self.boards[last];
            for y in 0..h {
                for x in 0..w {
                    let mut n = 0u8;
                    for dy in -1isize..=1 {
                        for dx in -1isize..=1 {
                            if dx == 0 && dy == 0 {
                                continue;
                            }
                            let (nx, ny) = (x + dx, y + dy);
                            if nx >= 0 && nx < w && ny >= 0 && ny < h && src[(ny * w + nx) as usize]
                            {
                                n += 1;
                            }
                        }
                    }
                    self.nb[(y * w + x) as usize] = n;
                }
            }
            for i in 0..src.len() {
                let n = self.nb[i];
                self.scratch[i] = n == 3 || (n == 2 && src[i]);
            }
        }
        for _ in 0..injections {
            self.inject();
        }

        // The slot the present is vacating becomes the new far future.
        let dest = self.head;
        self.boards[dest].copy_from_slice(&self.scratch);
        self.head = (self.head + 1) % BOARDS;
    }

    /// Pick a 6x4 (or 4x6) box lying in the border band — three cells of margin plus
    /// the outermost visible row/column — and either scatter it or launch a glider
    /// inward from it. Applied to the generation currently being computed.
    fn inject(&mut self) {
        let (c, r) = (self.cols, self.rows);
        if c < BAND + 6 || r < BAND + 6 {
            return;
        }
        // 0 = top, 1 = bottom, 2 = left, 3 = right
        let edge = self.rng.below(4);
        let (x0, y0, bw, bh) = match edge {
            0 => (self.rng.below(c - 6 + 1), 0, 6, BAND),
            1 => (self.rng.below(c - 6 + 1), r - BAND, 6, BAND),
            2 => (0, self.rng.below(r - 6 + 1), BAND, 6),
            _ => (c - BAND, self.rng.below(r - 6 + 1), BAND, 6),
        };

        if self.rng.f32() < GLIDER_CHANCE {
            for y in y0..y0 + bh {
                for x in x0..x0 + bw {
                    self.scratch[y * c + x] = false;
                }
            }
            // Head roughly toward the middle of the screen: pick freely between the
            // two diagonals that point inward from this edge.
            let flip = self.rng.f32() < 0.5;
            let pat = glider_for(edge, flip);
            let gx = x0 + (bw - 3) / 2;
            let gy = y0 + (bh - 3) / 2;
            for (ry, bits) in pat.iter().enumerate() {
                for rx in 0..3usize {
                    if bits & (0b100 >> rx) != 0 {
                        self.scratch[(gy + ry) * c + gx + rx] = true;
                    }
                }
            }
        } else {
            for y in y0..y0 + bh {
                for x in x0..x0 + bw {
                    self.scratch[y * c + x] = self.rng.f32() < SCATTER_DENSITY;
                }
            }
        }
    }

    pub fn view(&self) -> LifeView<'_> {
        LifeView { life: self }
    }
}

/// Read-only window onto the board as it is now and as it will be for the next few
/// generations. This is the only way the visualisation layer sees the grid: the
/// future is already computed and cannot be influenced from here, so a critter can
/// plan against it freely and know it will come true.
#[derive(Clone, Copy)]
pub struct LifeView<'a> {
    life: &'a Life,
}

impl LifeView<'_> {
    pub fn cols(&self) -> usize {
        self.life.cols
    }
    pub fn rows(&self) -> usize {
        self.life.rows
    }
    /// Furthest generation that can be asked about.
    pub fn lookahead(&self) -> usize {
        LOOKAHEAD
    }

    /// Whether a cell is alive `ahead` generations from now. Outside the world is
    /// always off, and `ahead` past the lookahead is clamped to the furthest board.
    pub fn alive(&self, x: isize, y: isize, ahead: usize) -> bool {
        if x < 0 || y < 0 || x >= self.life.cols as isize || y >= self.life.rows as isize {
            return false;
        }
        self.life.board(ahead)[y as usize * self.life.cols + x as usize]
    }

    /// Live neighbours of a cell `ahead` generations from now.
    pub fn neighbours(&self, x: isize, y: isize, ahead: usize) -> u32 {
        let mut n = 0;
        for dy in -1isize..=1 {
            for dx in -1isize..=1 {
                if (dx != 0 || dy != 0) && self.alive(x + dx, y + dy, ahead) {
                    n += 1;
                }
            }
        }
        n
    }

    /// How many generations from now, up to the lookahead, until this cell changes.
    /// `None` if it holds steady for as far as we can see — handy for a critter
    /// picking somewhere safe to land.
    pub fn changes_in(&self, x: isize, y: isize) -> Option<usize> {
        let now = self.alive(x, y, 0);
        (1..=LOOKAHEAD).find(|&i| self.alive(x, y, i) != now)
    }

    /// Centre of a cell in world units, so something can be placed relative to the
    /// grid. Fractional coordinates are fine.
    pub fn cell_center(&self, x: f32, y: f32) -> [f32; 2] {
        [
            (x - (self.life.cols as f32 - 1.0) * 0.5) * CELL_PX,
            ((self.life.rows as f32 - 1.0) * 0.5 - y) * CELL_PX,
        ]
    }
}

// ---------------------------------------------------------------------------
// Critters — things that live in the visualisation layer rather than the grid
// ---------------------------------------------------------------------------

/// What a critter is handed each frame.
pub struct CritterCtx<'a> {
    /// The board now and for the next few generations. Already decided; plan freely.
    pub life: LifeView<'a>,
    /// Seconds since the previous frame.
    pub dt: f32,
    /// Shader clock. Instance timestamps are expressed against this.
    pub now: f32,
    /// How far the visualisation is between the present generation and the next,
    /// 0 at the moment a generation lands and approaching 1 just before the next.
    pub phase: f32,
    /// Seconds a generation currently lasts, for pacing a move across cells.
    pub gen_secs: f32,
    /// Per-cell visual spin state for the current frame. It is absent in lightweight
    /// callers that do not own a visualisation; critters then treat every tile as still.
    pub spinning: Option<&'a [bool]>,
}

impl CritterCtx<'_> {
    fn tile_spinning(&self, col: isize, row: isize) -> bool {
        if col < 0
            || row < 0
            || col >= self.life.cols() as isize
            || row >= self.life.rows() as isize
        {
            return false;
        }
        self.spinning
            .and_then(|cells| cells.get(row as usize * self.life.cols() + col as usize))
            .copied()
            .unwrap_or(false)
    }
}

/// Something that lives on top of the grid: it can read the board and the near future
/// but never change them, and it draws itself as tiles, as prop models, or both.
///
/// The grid decides what the field does; a critter decides only what it does about it.
pub trait Critter {
    /// Advance by `ctx.dt`. Return false to be removed from the scene.
    fn update(&mut self, ctx: &CritterCtx) -> bool;

    /// Append any tiles this critter draws. They land in the solid pass, so they
    /// occlude and are occluded by the grid's own tiles normally.
    fn draw(&self, _ctx: &CritterCtx, _out: &mut Vec<Instance>) {}

    /// Append any prop models this critter places, grouped by model.
    fn props(&self, _ctx: &CritterCtx, _out: &mut PropSink) {}

    /// A dying field tile claimed by this critter. The visualisation suppresses that
    /// tile's ordinary fade on the death boundary so the critter can replace it with
    /// a freely moving copy in the very same frame.
    fn claimed_cell(&self) -> Option<(usize, usize)> {
        None
    }

    /// A one-line summary, for probing behaviour in tests.
    #[cfg(test)]
    fn debug_state(&self) -> Option<String> {
        None
    }
}

// ---------------------------------------------------------------------------

/// A critter is drawn every sixteen seconds, starting sixteen seconds in. The first
/// scheduled visitor is the tile mimic so its longer performance is discoverable without
/// a lucky reload; subsequent draws give it two slots and each established kind one. If a
/// planned visitor has no safe route on this board, the rocket is the graceful fallback.
/// The distant background walkers are independent of this schedule.
const FIRST_CRITTER: f64 = 16.0;
const CRITTER_EVERY: f64 = 16.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CritterKind {
    Rocket,
    Walker,
    Bee,
    Ufo,
    Mimic,
}

fn random_critter_kind(rng: &mut Rng) -> CritterKind {
    match rng.below(6) {
        0 => CritterKind::Rocket,
        1 => CritterKind::Walker,
        2 => CritterKind::Bee,
        3 => CritterKind::Ufo,
        _ => CritterKind::Mimic,
    }
}

fn scheduled_critter_kind(rng: &mut Rng, critters_sent: u32) -> CritterKind {
    if critters_sent == 0 {
        CritterKind::Mimic
    } else {
        random_critter_kind(rng)
    }
}

// ---------------------------------------------------------------------------
// UFO — plans against Conway, then steals one tile on its exact death boundary
// ---------------------------------------------------------------------------

const UFO_HOVER_Z: f32 = 158.0;
const UFO_HOVER_SCREEN_LIFT: f32 = 66.0;
const UFO_START_Z: f32 = 430.0;
const UFO_HOVER_ITERS: f32 = 0.34;
const UFO_PULL_SECS: f32 = 1.05;
const UFO_ESCAPE_SECS: f32 = 3.15;
/// The tractor beam stays locked to the cargo for the entire getaway, fading only
/// as the saucer and tile finish leaving together.
const UFO_BEAM_SECS: f32 = UFO_PULL_SECS + UFO_ESCAPE_SECS;
const UFO_TILE_TOW_Z: f32 = 93.0;
const UFO_TOP_TILT: f32 = 0.08;
const UFO_PROFILE_TILT: f32 = 1.18;

fn ufo_target_candidates(view: &LifeView, phase: f32) -> Vec<(usize, usize)> {
    // At an exact generation boundary, a board(2) death is exactly two iterations
    // away and falls outside x < 2, so use a board(1) death. At every fractional
    // phase, board(2) is continuously less than two and at least one iteration away.
    let death_ahead = if phase <= f32::EPSILON { 1 } else { 2 };
    let x0 = MARGIN.min(view.cols());
    let y0 = MARGIN.min(view.rows());
    let x1 = view.cols().saturating_sub(MARGIN);
    let y1 = view.rows().saturating_sub(MARGIN);
    let mut cells = Vec::new();
    for y in y0..y1 {
        for x in x0..x1 {
            let (x, y) = (x as isize, y as isize);
            // Ordinarily it lives through the next integer generation and dies on
            // the second; the exact-boundary case dies on the next. Two further dead
            // boards rule out a tile about to blink straight back on.
            let alive_until_death = (0..death_ahead).all(|ahead| view.alive(x, y, ahead));
            let dead_after_death =
                (death_ahead..=death_ahead + 2).all(|ahead| !view.alive(x, y, ahead));
            if alive_until_death && dead_after_death {
                cells.push((x as usize, y as usize));
            }
        }
    }
    cells
}

struct Ufo {
    target: (usize, usize),
    target_world: [f32; 2],
    origin: [f32; 3],
    start_iterations: f32,
    t: f32,
    capture_age: Option<f32>,
    tile_tint: [f32; 3],
    /// Half arrive edge-on and flatten over the target; half do the inverse.
    profile_at_capture: bool,
    tilt_sign: f32,
}

impl Ufo {
    fn new(view: &LifeView, phase: f32, rng: &mut Rng) -> Option<Ufo> {
        let phase = phase.clamp(0.0, 1.0);
        let candidates = ufo_target_candidates(view, phase);
        let target = *candidates.get(rng.below(candidates.len()))?;
        let death_ahead = view
            .changes_in(target.0 as isize, target.1 as isize)
            .expect("UFO candidates always have a planned death");
        let target_world = view.cell_center(target.0 as f32, target.1 as f32);
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let half_h = view.rows() as f32 * CELL_PX * 0.5;
        let visible_left = -half_w + MARGIN as f32 * CELL_PX;
        let visible_right = half_w - MARGIN as f32 * CELL_PX;
        let visible_top = half_h - MARGIN as f32 * CELL_PX;
        let edge_pad = 155.0;
        let origin = match rng.below(3) {
            // Straight down from above, with enough lateral offset for the path to
            // read as a swoop rather than a scale-only arrival.
            0 => [
                (target_world[0] + (rng.f32() - 0.5) * 420.0).clamp(visible_left, visible_right),
                visible_top + edge_pad,
                UFO_START_Z,
            ],
            1 => [
                visible_left - edge_pad,
                visible_top + (rng.f32() - 0.5) * CELL_PX * 2.0,
                UFO_START_Z,
            ],
            _ => [
                visible_right + edge_pad,
                visible_top + (rng.f32() - 0.5) * CELL_PX * 2.0,
                UFO_START_Z,
            ],
        };
        let tile_tint = match cell_category(target.0 as isize, target.1 as isize) {
            1 => srgb_hex_to_linear(C_GREEN),
            2 => srgb_hex_to_linear(C_TEAL),
            3 => srgb_hex_to_linear(C_BLUE),
            _ => srgb_hex_to_linear(C_LIVE),
        };
        Some(Ufo {
            target,
            target_world,
            origin,
            start_iterations: death_ahead as f32 - phase,
            t: 0.0,
            capture_age: None,
            tile_tint,
            profile_at_capture: rng.f32() < 0.5,
            tilt_sign: if rng.f32() < 0.5 { 1.0 } else { -1.0 },
        })
    }

    fn smooth(p: f32) -> f32 {
        let p = p.clamp(0.0, 1.0);
        p * p * (3.0 - 2.0 * p)
    }

    fn lerp3(a: [f32; 3], b: [f32; 3], p: f32) -> [f32; 3] {
        [
            a[0] + (b[0] - a[0]) * p,
            a[1] + (b[1] - a[1]) * p,
            a[2] + (b[2] - a[2]) * p,
        ]
    }

    fn hover_position(&self) -> [f32; 3] {
        [
            self.target_world[0],
            self.target_world[1] + UFO_HOVER_SCREEN_LIFT,
            UFO_HOVER_Z + (self.t * 2.7).sin() * 4.0,
        ]
    }

    fn profile_amount(&self, pos: [f32; 3]) -> f32 {
        let hover_mix = Self::smooth((UFO_START_Z - pos[2]) / (UFO_START_Z - UFO_HOVER_Z));
        if self.profile_at_capture {
            hover_mix
        } else {
            1.0 - hover_mix
        }
    }

    fn tilt_at(&self, pos: [f32; 3]) -> f32 {
        let profile = Self::smooth(self.profile_amount(pos));
        self.tilt_sign * (UFO_TOP_TILT + (UFO_PROFILE_TILT - UFO_TOP_TILT) * profile)
    }

    /// Rotation and length that carry the beam model's local +Z axis from `from`
    /// toward `to`, including the lateral tow that develops during the pull.
    fn beam_pose(from: [f32; 3], to: [f32; 3]) -> ([f32; 3], f32) {
        let d = [to[0] - from[0], to[1] - from[1], to[2] - from[2]];
        let horizontal = d[0].hypot(d[1]);
        let length = horizontal.hypot(d[2]).max(1.0);
        let tilt = horizontal.atan2(d[2]);
        let yaw = d[0].atan2(-d[1]);
        ([tilt, 0.0, yaw], length)
    }

    fn tow_offset(&self) -> [f32; 2] {
        let hover = self.hover_position();
        let dx = self.origin[0] - hover[0];
        let dy = self.origin[1] - hover[1];
        let len = dx.hypot(dy).max(1.0);
        // Departure heads toward `origin`, so the cargo trails on the opposite side
        // where a top-down camera can see it instead of directly beneath the hull.
        [-dx / len * 96.0, -dy / len * 96.0]
    }

    fn position(&self, ctx: &CritterCtx) -> [f32; 3] {
        let hover = self.hover_position();
        if let Some(age) = self.capture_age {
            if age <= UFO_PULL_SECS {
                return hover;
            }
            let p = Self::smooth((age - UFO_PULL_SECS) / UFO_ESCAPE_SECS);
            return Self::lerp3(hover, self.origin, p);
        }

        let integer_remaining = ctx
            .life
            .changes_in(self.target.0 as isize, self.target.1 as isize)
            .unwrap_or(2) as f32;
        let remaining = (integer_remaining - ctx.phase).max(0.0);
        let travel = (self.start_iterations - UFO_HOVER_ITERS).max(0.01);
        let p = Self::smooth((self.start_iterations - remaining) / travel);
        Self::lerp3(self.origin, hover, p)
    }

    fn captured_tile_position(&self, ctx: &CritterCtx, age: f32) -> [f32; 3] {
        let tow = self.tow_offset();
        // This is the single cargo anchor used on both sides of the pull/escape
        // boundary. Keeping it relative to the live saucer position includes the
        // hover bob during the pull and the flight path during escape without
        // changing coordinate systems between the two phases.
        let ufo = self.position(ctx);
        let tow_position = [
            ufo[0] + tow[0],
            ufo[1] + tow[1],
            ufo[2] - (UFO_HOVER_Z - UFO_TILE_TOW_Z),
        ];
        if age <= UFO_PULL_SECS {
            let p = Self::smooth(age / UFO_PULL_SECS);
            return Self::lerp3(
                [
                    self.target_world[0],
                    self.target_world[1],
                    RISE + THICK * 0.5,
                ],
                tow_position,
                p,
            );
        }
        tow_position
    }
}

impl Critter for Ufo {
    fn update(&mut self, ctx: &CritterCtx) -> bool {
        self.t += ctx.dt;
        if let Some(age) = self.capture_age.as_mut() {
            *age += ctx.dt;
            return *age < UFO_PULL_SECS + UFO_ESCAPE_SECS;
        }
        if !ctx
            .life
            .alive(self.target.0 as isize, self.target.1 as isize, 0)
        {
            self.capture_age = Some(0.0);
        }
        true
    }

    fn props(&self, ctx: &CritterCtx, out: &mut PropSink) {
        let pos = self.position(ctx);
        let profile = self.profile_amount(pos);
        let bank = if self.capture_age.is_none() {
            ((self.origin[0] - self.target_world[0]) * 0.0009).clamp(-0.23, 0.23)
                * (1.0 - ((pos[2] - UFO_HOVER_Z) / (UFO_START_Z - UFO_HOVER_Z)).abs())
                * (1.0 - profile)
        } else {
            0.0
        };
        out.push(
            MODEL_UFO,
            Prop::new(
                pos,
                1.0,
                [
                    self.tilt_at(pos) + bank,
                    (self.t * 1.15).cos() * 0.035 * (1.0 - profile),
                    self.t * 0.20,
                ],
            ),
        );

        if let Some(age) = self.capture_age {
            let tile_pos = self.captured_tile_position(ctx, age);
            if age < UFO_BEAM_SECS {
                let beam_from = [tile_pos[0], tile_pos[1], tile_pos[2] - THICK * 0.30];
                let beam_to = [pos[0], pos[1], pos[2] - 13.0];
                let (beam_rot, beam_len) = Self::beam_pose(beam_from, beam_to);
                let p = age / UFO_BEAM_SECS;
                // Hold through the pull and nearly the whole escape. Because both
                // endpoints are recomputed above, the cone keeps shining directly
                // between the flying saucer and its spinning cargo as they depart.
                let fade = 1.0 - Self::smooth(((p - 0.88) / 0.12).clamp(0.0, 1.0));
                let pulse = 0.92 + 0.08 * (self.t * 19.0).sin();
                for (radius, color, alpha) in [
                    (78.0, 0xffdc42, 0.14),
                    (56.0, 0xffe85f, 0.20),
                    (31.0, 0xfff3a0, 0.18),
                ] {
                    out.push(
                        MODEL_UFO_BEAM,
                        Prop::stretched(beam_from, [radius, radius, beam_len], beam_rot)
                            .tinted(srgb_hex_to_linear(color), alpha * fade * pulse),
                    );
                }
            }
            out.push(
                MODEL_CAPTURED_TILE,
                Prop::new(tile_pos, 1.0, [age * 2.35, age * 1.75, age * 0.68])
                    .tinted(self.tile_tint, 1.0),
            );
        }
    }

    fn claimed_cell(&self) -> Option<(usize, usize)> {
        Some(self.target)
    }

    #[cfg(test)]
    fn debug_state(&self) -> Option<String> {
        Some(format!(
            "Ufo target=({}, {}) captured={}",
            self.target.0,
            self.target.1,
            self.capture_age.is_some()
        ))
    }
}

// ---------------------------------------------------------------------------
// Tile mimic — a clockwork crab hiding beneath an ordinary Conway tile
// ---------------------------------------------------------------------------

const MIMIC_HATCH_SECS: f32 = 1.38;
const MIMIC_STEP_SECS: f32 = 0.86;
const MIMIC_INSPECT_SECS: f32 = 2.35;
const MIMIC_ALIGN_SECS: f32 = 0.46;
const MIMIC_HIDE_SECS: f32 = 0.82;
const MIMIC_EYE_LINGER_SECS: f32 = 0.90;
const MIMIC_SAFE_TRANSITIONS: usize = 4;
const MIMIC_BODY_Z: f32 = RISE + THICK + 13.0;
const MIMIC_SHELL_SCALE: f32 = 0.76;

struct MimicPose {
    root: [f32; 3],
    yaw: f32,
    body_alpha: f32,
    eye_alpha: f32,
    eye_blink: f32,
    shell_lift: f32,
    shell_alpha: f32,
    shell_mix: f32,
    sample_flash: f32,
    shell_tilt: [f32; 2],
    walk_phase: f32,
    stride: f32,
    probe: f32,
    inspect: f32,
}

struct Mimic {
    route: Vec<(usize, usize)>,
    world: Vec<[f32; 2]>,
    age: f32,
    start_tint: [f32; 3],
    end_tint: [f32; 3],
    personality: f32,
}

impl Mimic {
    fn stable_for(view: &LifeView, x: isize, y: isize, generations: usize) -> bool {
        (0..=generations).all(|ahead| view.alive(x, y, ahead))
    }

    fn safe_foothold(view: &LifeView, x: isize, y: isize) -> bool {
        Self::stable_for(view, x, y, MIMIC_SAFE_TRANSITIONS)
    }

    fn tint(cell: (usize, usize)) -> [f32; 3] {
        match cell_category(cell.0 as isize, cell.1 as isize) {
            1 => srgb_hex_to_linear(C_GREEN),
            2 => srgb_hex_to_linear(C_TEAL),
            3 => srgb_hex_to_linear(C_BLUE),
            _ => srgb_hex_to_linear(C_LIVE),
        }
    }

    fn plan(view: &LifeView, rng: &mut Rng) -> Option<Vec<(usize, usize)>> {
        // The legs and claws extend almost one cell beyond the shell. Two more cells
        // than the simulation margin keeps the entire performance inside the viewport
        // instead of choosing a technically visible tile that clips half the animal.
        let creature_margin = MARGIN + 2;
        let x0 = creature_margin.min(view.cols());
        let y0 = creature_margin.min(view.rows());
        let x1 = view.cols().saturating_sub(creature_margin);
        let y1 = view.rows().saturating_sub(creature_margin);
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let flank = if half_w > 560.0 { 300.0 } else { 0.0 };
        let mut starts = Vec::new();
        for y in y0..y1 {
            for x in x0..x1 {
                let world = view.cell_center(x as f32, y as f32);
                if world[0].abs() >= flank && Self::safe_foothold(view, x as isize, y as isize) {
                    starts.push((x, y));
                }
            }
        }
        if starts.is_empty() {
            return None;
        }

        // Ordinary neighbours dominate, diagonals make turns less grid-bound, and
        // the occasional two-cell reach gives the long legs a theatrical gap to vault.
        const OFFSETS: [(isize, isize); 12] = [
            (-1, 0),
            (1, 0),
            (0, -1),
            (0, 1),
            (-1, -1),
            (1, -1),
            (-1, 1),
            (1, 1),
            (-2, 0),
            (2, 0),
            (0, -2),
            (0, 2),
        ];
        let mut best = Vec::new();
        let mut best_score = i32::MIN;
        for _ in 0..420 {
            let wanted = 4 + rng.below(3);
            let start = starts[rng.below(starts.len())];
            let mut route = vec![start];
            while route.len() < wanted {
                let (x, y) = *route.last().unwrap();
                let rotation = rng.below(OFFSETS.len());
                let mut choices = Vec::new();
                for step in 0..OFFSETS.len() {
                    let (dx, dy) = OFFSETS[(rotation + step) % OFFSETS.len()];
                    let (nx, ny) = (x as isize + dx, y as isize + dy);
                    if nx < x0 as isize
                        || ny < y0 as isize
                        || nx >= x1 as isize
                        || ny >= y1 as isize
                        || route
                            .iter()
                            .filter(|&&cell| cell == (nx as usize, ny as usize))
                            .count()
                            >= 2
                        || !Self::safe_foothold(view, nx, ny)
                    {
                        continue;
                    }
                    choices.push((nx as usize, ny as usize));
                }
                if choices.is_empty() {
                    break;
                }
                // On the last hop, strongly prefer a coloured tile: that gives the
                // camouflage-sampling finale an observable payoff.
                if route.len() + 1 == wanted {
                    let coloured: Vec<_> = choices
                        .iter()
                        .copied()
                        .filter(|&(x, y)| is_colored(x, y))
                        .collect();
                    if !coloured.is_empty() {
                        choices = coloured;
                    }
                }
                route.push(choices[rng.below(choices.len())]);
            }
            if route.len() < 3 {
                continue;
            }
            let home = *route.last().unwrap();
            if !Self::safe_foothold(view, home.0 as isize, home.1 as isize) {
                continue;
            }
            let mut score = route.len() as i32 * 20;
            let unique = route
                .iter()
                .enumerate()
                .filter(|(index, cell)| !route[..*index].contains(cell))
                .count();
            score += unique as i32 * 9;
            if !is_colored(route[0].0, route[0].1) {
                score += 12;
            }
            if is_colored(route.last().unwrap().0, route.last().unwrap().1) {
                score += 52;
            }
            for segment in route.windows(2) {
                let dx = segment[1].0.abs_diff(segment[0].0);
                let dy = segment[1].1.abs_diff(segment[0].1);
                if dx + dy > 1 {
                    score += 4;
                }
            }
            for bend in route.windows(3) {
                let a = (
                    bend[1].0 as isize - bend[0].0 as isize,
                    bend[1].1 as isize - bend[0].1 as isize,
                );
                let b = (
                    bend[2].0 as isize - bend[1].0 as isize,
                    bend[2].1 as isize - bend[1].1 as isize,
                );
                if a != b {
                    score += 5;
                }
            }
            if score > best_score {
                best_score = score;
                best = route;
            }
        }
        if best.len() >= 3 {
            Some(best)
        } else {
            // A sparse board may have isolated long-lived tiles but no honest nearby
            // route between them. Let the cautious mimic inspect and re-hide on one
            // safe foothold instead of inventing a huge leap or falling back to a
            // different critter.
            let coloured: Vec<_> = starts
                .iter()
                .copied()
                .filter(|&(x, y)| is_colored(x, y))
                .collect();
            let pool = if coloured.is_empty() {
                &starts
            } else {
                &coloured
            };
            Some(vec![pool[rng.below(pool.len())]])
        }
    }

    fn new(view: &LifeView, rng: &mut Rng) -> Option<Mimic> {
        let route = Self::plan(view, rng)?;
        let world = route
            .iter()
            .map(|&(x, y)| view.cell_center(x as f32, y as f32))
            .collect();
        Some(Mimic {
            start_tint: Self::tint(route[0]),
            end_tint: Self::tint(*route.last().unwrap()),
            route,
            world,
            age: 0.0,
            personality: rng.f32() * std::f32::consts::TAU,
        })
    }

    fn travel_secs(&self) -> f32 {
        self.route.len().saturating_sub(1) as f32 * MIMIC_STEP_SECS
    }

    fn total_secs(&self) -> f32 {
        MIMIC_HATCH_SECS
            + self.travel_secs()
            + MIMIC_INSPECT_SECS
            + MIMIC_ALIGN_SECS
            + MIMIC_HIDE_SECS
            + MIMIC_EYE_LINGER_SECS
    }

    fn angle(a: [f32; 2], b: [f32; 2]) -> f32 {
        (b[1] - a[1]).atan2(b[0] - a[0])
    }

    fn angle_lerp(a: f32, b: f32, p: f32) -> f32 {
        let delta =
            (b - a + std::f32::consts::PI).rem_euclid(std::f32::consts::TAU) - std::f32::consts::PI;
        a + delta * Self::smooth(p)
    }

    fn smooth(p: f32) -> f32 {
        let p = p.clamp(0.0, 1.0);
        p * p * (3.0 - 2.0 * p)
    }

    fn smoother(p: f32) -> f32 {
        let p = p.clamp(0.0, 1.0);
        p * p * p * (p * (p * 6.0 - 15.0) + 10.0)
    }

    fn orthogonal_yaw(yaw: f32) -> f32 {
        let quarter_turn = std::f32::consts::FRAC_PI_2;
        (yaw / quarter_turn).round() * quarter_turn
    }

    fn pose(&self) -> MimicPose {
        let first_yaw = self
            .world
            .get(1)
            .map(|&next| Self::angle(self.world[0], next))
            .unwrap_or(0.0);
        let last = self.world.len() - 1;
        let last_yaw = if last > 0 {
            Self::angle(self.world[last - 1], self.world[last])
        } else {
            first_yaw
        };
        let mut pose = MimicPose {
            root: [self.world[0][0], self.world[0][1], MIMIC_BODY_Z],
            yaw: first_yaw,
            body_alpha: 1.0,
            eye_alpha: 1.0,
            eye_blink: 0.0,
            shell_lift: 1.0,
            shell_alpha: 1.0,
            shell_mix: 0.0,
            sample_flash: 0.0,
            shell_tilt: [0.0; 2],
            walk_phase: 0.0,
            stride: 0.0,
            probe: 0.0,
            inspect: 0.0,
        };

        if self.age < MIMIC_HATCH_SECS {
            let lift = Self::smooth((self.age - 0.12) / 0.92);
            let reveal = Self::smooth((self.age - 0.26) / 0.62);
            let tremble = (self.age * 38.0 + self.personality).sin() * (1.0 - lift);
            pose.root[2] = RISE + THICK - 5.0 + (MIMIC_BODY_Z - (RISE + THICK - 5.0)) * reveal;
            pose.body_alpha = reveal;
            pose.eye_alpha = reveal;
            pose.shell_lift = lift;
            pose.shell_alpha = Self::smooth(self.age / 0.14);
            pose.shell_tilt = [0.022 * tremble, -0.018 * tremble];
            pose.probe = Self::smooth((self.age - 0.70) / 0.52) * 0.28;
            return pose;
        }

        let travel_age = self.age - MIMIC_HATCH_SECS;
        if travel_age < self.travel_secs() {
            let segment = (travel_age / MIMIC_STEP_SECS).floor() as usize;
            let local = (travel_age / MIMIC_STEP_SECS).fract();
            let motion = Self::smooth((local - 0.07) / 0.86);
            let a = self.world[segment];
            let b = self.world[segment + 1];
            pose.root[0] = a[0] + (b[0] - a[0]) * motion;
            pose.root[1] = a[1] + (b[1] - a[1]) * motion;
            let distance_cells = (b[0] - a[0]).hypot(b[1] - a[1]) / CELL_PX;
            let vault = 2.4 + (distance_cells - 1.05).max(0.0) * 10.0;
            pose.root[2] += (std::f32::consts::PI * motion).sin() * vault;
            let current_yaw = Self::angle(a, b);
            let previous_yaw = if segment == 0 {
                current_yaw
            } else {
                Self::angle(self.world[segment - 1], a)
            };
            pose.yaw = Self::angle_lerp(previous_yaw, current_yaw, local / 0.27);
            pose.walk_phase = (segment as f32 + motion) * std::f32::consts::TAU + self.personality;
            pose.stride = (std::f32::consts::PI * local).sin().max(0.0);
            pose.probe = if local < 0.28 {
                (std::f32::consts::PI * local / 0.28).sin()
            } else {
                0.0
            };
            pose.shell_tilt = [
                0.048 * pose.walk_phase.sin() * pose.stride,
                0.060 * (pose.walk_phase + 0.8).sin() * pose.stride,
            ];
            return pose;
        }

        pose.root[0] = self.world[last][0];
        pose.root[1] = self.world[last][1];
        pose.yaw = last_yaw;
        let inspect_age = travel_age - self.travel_secs();
        if inspect_age < MIMIC_INSPECT_SECS {
            let q = (inspect_age / MIMIC_INSPECT_SECS).clamp(0.0, 1.0);
            let settle = Self::smooth(q / 0.18);
            let taps = (q * std::f32::consts::TAU * 3.0).sin().max(0.0);
            pose.inspect = settle;
            pose.probe =
                taps * Self::smooth((q - 0.05) / 0.15) * (1.0 - Self::smooth((q - 0.82) / 0.14));
            pose.shell_mix = Self::smooth((q - 0.22) / 0.55);
            pose.sample_flash = Self::smooth((q - 0.13) / 0.16)
                * (1.0 - Self::smooth((q - 0.76) / 0.15))
                * (0.74 + 0.26 * (q * std::f32::consts::TAU * 4.0).sin());
            pose.root[2] += (q * std::f32::consts::TAU * 3.0).sin() * 1.2 * (1.0 - q);
            pose.shell_tilt = [
                -0.10 * settle + 0.035 * taps,
                0.025 * (q * std::f32::consts::TAU * 2.0 + self.personality).sin(),
            ];
            return pose;
        }

        // Square up before folding. The shell may turn while the creature is still
        // plainly itself, but once camouflage begins its yaw is locked to the tile
        // grid so the new tile never corkscrews into place.
        let grid_yaw = Self::orthogonal_yaw(last_yaw);
        let finale_age = inspect_age - MIMIC_INSPECT_SECS;
        if finale_age < MIMIC_ALIGN_SECS {
            let align = Self::smoother(finale_age / MIMIC_ALIGN_SECS);
            pose.yaw = Self::angle_lerp(last_yaw, grid_yaw, align);
            pose.inspect = 1.0;
            pose.shell_mix = 1.0;
            pose.shell_tilt = [-0.10 * (1.0 - align), 0.0];
            return pose;
        }

        pose.yaw = grid_yaw;
        pose.inspect = 1.0;
        pose.shell_mix = 1.0;
        let hide_age = finale_age - MIMIC_ALIGN_SECS;
        let hide = (hide_age / MIMIC_HIDE_SECS).clamp(0.0, 1.0);
        let folded = Self::smoother(hide);
        pose.body_alpha = 1.0 - folded;
        pose.shell_lift = 1.0 - folded;
        pose.root[2] -= folded * 13.0;
        pose.probe = 0.0;
        pose.shell_tilt = [-0.10 * (1.0 - folded), 0.0];
        pose.shell_alpha = 1.0 - Self::smoother((hide - 0.82) / 0.18);
        if hide_age >= MIMIC_HIDE_SECS {
            let linger = ((hide_age - MIMIC_HIDE_SECS) / MIMIC_EYE_LINGER_SECS).clamp(0.0, 1.0);
            // Hold the stare, blink once at dead centre, then retain a short beat
            // before fading. Keeping opacity solid through the blink makes the squash
            // unmistakable instead of reading as part of the disappearance.
            let blink_distance = ((linger - 0.50) / 0.12).abs();
            pose.eye_blink = Self::smoother(1.0 - blink_distance);
            pose.eye_alpha = 1.0 - Self::smoother((linger - 0.78) / 0.22);
        }
        pose
    }

    fn local(root: [f32; 3], yaw: f32, p: [f32; 3]) -> [f32; 3] {
        let (s, c) = yaw.sin_cos();
        [
            root[0] + p[0] * c - p[1] * s,
            root[1] + p[0] * s + p[1] * c,
            root[2] + p[2],
        ]
    }

    fn segment(a: [f32; 3], b: [f32; 3], width: f32, tint: [f32; 3], alpha: f32) -> Prop {
        let d = [b[0] - a[0], b[1] - a[1], b[2] - a[2]];
        let len = d[0].hypot(d[1]).hypot(d[2]).max(0.01);
        let pitch = -(d[2] / len).clamp(-1.0, 1.0).asin();
        let yaw = d[1].atan2(d[0]);
        Prop::stretched(a, [len, width, width], [0.0, pitch, yaw]).tinted(tint, alpha)
    }

    fn eyes_at(&self, pose: &MimicPose, out: &mut PropSink, stalk_alpha: f32, eye_alpha: f32) {
        let stalk = srgb_hex_to_linear(0x315057);
        for eye in 0..2 {
            let side = if eye == 0 { -1.0 } else { 1.0 };
            let base = Self::local(pose.root, pose.yaw, [14.5, side * 8.0, 3.0]);
            let eye_local = [
                24.0 + pose.probe * 3.0,
                side * (10.0 + pose.inspect * 2.0),
                24.0 + 2.2 * (self.age * 3.1 + eye as f32).sin(),
            ];
            let eye_pos = Self::local(pose.root, pose.yaw, eye_local);
            if stalk_alpha > 0.001 {
                out.push(
                    MODEL_MIMIC_SEGMENT,
                    Self::segment(base, eye_pos, 2.1, stalk, stalk_alpha),
                );
            }
            if eye_alpha > 0.001 {
                let glance = side * 0.10
                    + 0.13 * (self.age * 1.8 + self.personality + eye as f32 * 0.7).sin();
                let blink_height = 1.0 - pose.eye_blink * 0.88;
                let blink_width = 1.0 + pose.eye_blink * 0.10;
                out.push(
                    MODEL_MIMIC_EYE,
                    Prop::stretched(
                        eye_pos,
                        [5.8 * blink_width, 5.8 * blink_height, 5.8 * blink_height],
                        [0.0, -0.82, pose.yaw + glance],
                    )
                    .tinted([1.0; 3], eye_alpha),
                );
            }
        }
    }

    fn props_at(&self, pose: &MimicPose, out: &mut PropSink) {
        let brass = srgb_hex_to_linear(0xc58a42);
        let copper = srgb_hex_to_linear(0xa85e3c);
        let foot_dark = srgb_hex_to_linear(0x3c3430);
        let settled_tint = Ufo::lerp3(self.start_tint, self.end_tint, pose.shell_mix);
        let tint = Ufo::lerp3(
            settled_tint,
            srgb_hex_to_linear(0x8ed8c5),
            pose.sample_flash * 0.42,
        );
        let shell_scale = 1.0 + (MIMIC_SHELL_SCALE - 1.0) * pose.shell_lift;
        let hidden_shell_z = RISE + THICK * 0.5 + 0.35;
        let active_shell_z = pose.root[2] + 17.0;
        let shell_z = hidden_shell_z + (active_shell_z - hidden_shell_z) * pose.shell_lift;
        let grid_yaw = Self::orthogonal_yaw(pose.yaw);
        let shell_yaw = Self::angle_lerp(grid_yaw, pose.yaw, pose.shell_lift);
        let shell_pos = Self::local(
            pose.root,
            pose.yaw,
            [-8.0 * pose.shell_lift, 0.0, shell_z - pose.root[2]],
        );
        out.push(
            MODEL_CAPTURED_TILE,
            Prop::new(
                shell_pos,
                shell_scale,
                [pose.shell_tilt[0], pose.shell_tilt[1], shell_yaw],
            )
            .tinted(tint, pose.shell_alpha),
        );

        if pose.body_alpha <= 0.001 {
            self.eyes_at(pose, out, 0.0, pose.eye_alpha);
            return;
        }
        // Solid articulated props write depth. A long partial-alpha reveal therefore
        // layers translucent limbs over one another; make material coverage snap in
        // quickly while `body_alpha` continues to unfold the actual geometry.
        let coverage = Self::smooth((pose.body_alpha - 0.04) / 0.22);
        out.push(
            MODEL_MIMIC_BODY,
            Prop::new(pose.root, 1.05, [0.0, 0.0, pose.yaw]).tinted([1.0; 3], coverage),
        );

        // Six legs use an alternating tripod gait. Long hops stretch the same joint
        // hierarchy naturally because feet remain near the surface while the body
        // vaults, rather than scaling the entire creature like a sprite.
        for side_i in 0..2 {
            let side = if side_i == 0 { -1.0 } else { 1.0 };
            for leg in 0..3 {
                let anchor_x = -15.0 + leg as f32 * 15.0;
                let tripod = if (leg + side_i) % 2 == 0 {
                    0.0
                } else {
                    std::f32::consts::PI
                };
                let wave = (pose.walk_phase + tripod).sin() * pose.stride;
                let lift = (pose.walk_phase + tripod).cos().max(0.0) * pose.stride;
                let spread = 17.0 + 27.0 * pose.body_alpha;
                let hip = Self::local(pose.root, pose.yaw, [anchor_x, side * 15.0, -1.5]);
                let knee = Self::local(
                    pose.root,
                    pose.yaw,
                    [
                        anchor_x + wave * 4.5,
                        side * (spread - 9.0),
                        -6.0 + lift * 3.0,
                    ],
                );
                let foot = Self::local(
                    pose.root,
                    pose.yaw,
                    [
                        anchor_x + wave * 10.5,
                        side * spread,
                        RISE + THICK + 1.4 - pose.root[2] + lift * 5.5,
                    ],
                );
                let colour = if (leg + side_i) % 2 == 0 {
                    brass
                } else {
                    copper
                };
                out.push(
                    MODEL_MIMIC_SEGMENT,
                    Self::segment(hip, knee, 4.2, colour, coverage),
                );
                out.push(
                    MODEL_MIMIC_SEGMENT,
                    Self::segment(knee, foot, 3.7, colour, coverage),
                );
                let toe = Self::local(foot, pose.yaw, [7.5 + wave * 1.5, side * 1.5, 0.0]);
                out.push(
                    MODEL_MIMIC_SEGMENT,
                    Self::segment(foot, toe, 2.8, foot_dark, coverage),
                );
            }
        }

        // Eyes rise ahead of the shell and lead turns by a few degrees. Their pupils
        // pitch toward the camera, so the gaze survives the near-top-down view. Unlike
        // their stalks, the eyeballs keep peeking out after the rest has folded away.
        self.eyes_at(pose, out, coverage, pose.eye_alpha);

        // Both claws test the next landing before the body commits. At the final
        // coloured tile they tap out of phase, as though comparing paint samples.
        for claw in 0..2 {
            let side = if claw == 0 { -1.0 } else { 1.0 };
            let wrist = Self::local(pose.root, pose.yaw, [18.0, side * 13.0, 0.5]);
            let reach = 29.0 + pose.probe * (13.0 + claw as f32 * 2.0);
            let tip = Self::local(
                pose.root,
                pose.yaw,
                [
                    reach,
                    side * (15.0 + pose.probe * 2.0),
                    -1.0 - pose.probe * 2.0,
                ],
            );
            out.push(
                MODEL_MIMIC_SEGMENT,
                Self::segment(wrist, tip, 3.2, copper, coverage),
            );
            let clack = side * (0.10 + 0.12 * pose.probe);
            out.push(
                MODEL_MIMIC_CLAW,
                Prop::new(tip, 6.6, [0.0, -0.10 * pose.probe, pose.yaw + clack])
                    .tinted([1.0; 3], coverage),
            );
        }

        let key_base = [
            shell_pos[0],
            shell_pos[1],
            shell_pos[2] + THICK * shell_scale * 0.5,
        ];
        let key_spin = pose.yaw - self.age * (2.8 + 0.35 * self.personality.sin());
        out.push(
            MODEL_MIMIC_KEY,
            Prop::new(
                key_base,
                0.82,
                [pose.shell_tilt[0], pose.shell_tilt[1], key_spin],
            )
            .tinted([1.0; 3], coverage * pose.shell_alpha),
        );
    }
}

impl Critter for Mimic {
    fn update(&mut self, ctx: &CritterCtx) -> bool {
        self.age += ctx.dt;
        self.age < self.total_secs()
    }

    fn props(&self, _ctx: &CritterCtx, out: &mut PropSink) {
        let pose = self.pose();
        self.props_at(&pose, out);
    }

    #[cfg(test)]
    fn debug_state(&self) -> Option<String> {
        Some(format!(
            "Mimic route={} age={:.2} endpoint={:?}",
            self.route.len(),
            self.age,
            self.route.last()
        ))
    }
}

/// Peak crossing speed, as seconds the trip would take if it never eased off.
const ROCKET_CROSS_SECS: f32 = 9.4;
/// Margin beyond the field it enters and leaves through.
const ROCKET_MARGIN: f32 = 200.0;
/// Seconds per headwind cycle.
const HEADWIND_SECS: f32 = 3.1;
/// The headwind is two things added together, and it has to be, because one curve
/// cannot do both jobs.
///
/// `SWELL` is the gentle sine: the cruise easing off and picking back up, which is the
/// part you actually read as wind. `GUST` is a very narrow spike at the top of that
/// swell which briefly tips the rocket backward.
///
/// Trying to get the reversal out of the swell alone does not work. Any smooth maximum
/// is *flat* near its peak, so a single curve whose peak barely crosses into reverse
/// leaves the speed sitting at nearly zero for a few tenths of a second — the rocket
/// hangs in the air, which reads as the animation having frozen rather than as weather.
/// Separating them lets the swell stay shallow enough never to approach zero, while the
/// spike is steep enough to cross zero and come back almost at once. Both ends are
/// pinned: `rocket_never_hovers` and `headwind_bites_and_briefly_reverses`.
const HEADWIND_SWELL: f32 = 0.26;
const HEADWIND_GUST: f32 = 0.88;
const HEADWIND_SHARPNESS: i32 = 26;
/// Cruising heights: well clear in front of the tiles, and down behind them where the
/// live ones hide it and the gaps do not. Pulled far apart on purpose — under a
/// near-orthographic camera depth is cheap to lose, and this spread is what makes the
/// rocket visibly swell as it comes forward rather than merely stop being occluded.
const ROCKET_FRONT_Z: f32 = 250.0;
const ROCKET_BEHIND_Z: f32 = -120.0;
/// Fastest it climbs or dives. The model's pitch is derived from this, so it doubles as
/// how steeply the rocket noses over when changing sides: at a rate comparable to its
/// forward speed the turn is around forty-five degrees, which is what makes the
/// manoeuvre read as a turn toward the viewer instead of a slide in depth.
const ROCKET_Z_RATE: f32 = 340.0;
const ROCKET_PITCH_MAX: f32 = 0.85;
/// Past this fraction of the way to the right it stays out front, so every crossing
/// ends with the rocket clearly ahead of the field on its way off screen.
const ROCKET_EXIT_FRONT: f32 = 0.45;
/// How far it drifts toward and away from the viewer while holding a side.
const ROCKET_Z_WEAVE: f32 = 16.0;
/// Least time it will hold a side before considering the other. The floor underneath
/// matters: a dive taken because it was overdue rather than because it found cover has
/// nothing ahead to hold it down, so without a floor it surfaces again almost at once
/// and the weave barely registers.
const ROCKET_DWELL_FRONT: f32 = 1.7;
const ROCKET_DWELL_BEHIND: f32 = 1.3;
/// And a cap, so a long run of cover cannot keep it under the tiles indefinitely.
const ROCKET_MAX_BEHIND: f32 = 4.0;
/// After this long out front it will take any window going, whether or not it has found
/// a row worth diving under. Without it a flight over a locally empty stretch of board
/// simply never weaves at all.
const ROCKET_DIVE_OVERDUE: f32 = 3.0;
/// Generations a crossing window has to stay clear before it will commit.
///
/// One is enough, and more is actively harmful. A generation lasts several seconds while
/// crossing the tile plane takes about one, so a single generation already guarantees the
/// corridor stays open far longer than the manoeuvre needs — while every extra generation
/// demanded makes windows rarer and leaves the rocket refusing to change sides at all.
const ROCKET_WINDOW_GENS: usize = 1;
/// Cells of corridor a crossing needs. Changing sides takes about a second, in which the
/// rocket covers some four cells, so a shorter check can leave it still inside the tile
/// plane when the next tile arrives on top of it.
const ROCKET_CROSS_CELLS: isize = 5;
/// The least it will accept when it has been waiting too long to come up: enough to get
/// the hull and fins through, rather than the nothing at all it used to settle for.
const ROCKET_MIN_CELLS: isize = 3;
/// The band further ahead that it judges by when deciding *whether* to dive: cells this
/// far along its path, which is roughly where it will be once it is through.
/// Deliberately starting beyond the crossing corridor: the corridor has to be *clear* and
/// the scout band has to have *cover*, so overlapping them sets the two requirements
/// against each other and the rocket ends up diving only where there is nothing to dive
/// under.
const ROCKET_SCOUT_FROM: isize = 6;
const ROCKET_SCOUT_TO: isize = 13;
/// The field is sparse — a few percent live — so there is no useful threshold of "lots
/// of tiles ahead" to wait for. It dives at any opportunity and stays under while there
/// is anything at all left to pass beneath.
///
/// Sparseness is also why it steers. Left on a fixed line, a dive swept about eighteen
/// cell-widths of a six-percent field and so passed beneath roughly one tile — measured
/// at two percent of its time under the plane, which is nothing. Picking the best row
/// within reach before going down is what turns the manoeuvre into something you can
/// actually see.
const ROCKET_STEER_ROWS: isize = 3;
/// Stiffness of the drift onto a new lane, as a critically damped spring. Second order
/// rather than a simple ease, because a first-order ease steps its *velocity* the moment
/// the target moves — and the model's heading is taken from that velocity, so the nose
/// would flick. Accelerating instead gives a path whose velocity is continuous, which
/// makes the attitude smooth without having to filter it afterwards.
const ROCKET_LANE_STIFFNESS: f32 = 1.15;
/// Caps on the attitude the path can ask for, and how much bank comes with a turn.
const ROCKET_YAW_MAX: f32 = 0.5;
const ROCKET_BANK: f32 = 1.5;

/// Embers per rocket. They are a ring buffer: whichever one is oldest gets reused.
const EMBERS: usize = 34;
/// Sparks are deliberately short-lived: the plume length is this times how fast the
/// rocket is pulling away from them, so it is the knob for how far the flame trails.
const EMBER_LIFE: f32 = 0.285;

/// One spark of exhaust.
#[derive(Clone, Copy, Default)]
struct Ember {
    pos: [f32; 3],
    vel: [f32; 3],
    rot: [f32; 3],
    spin: [f32; 3],
    age: f32,
    life: f32,
    size: f32,
    /// 0 for the orange end of the flame, 1 for the yellow.
    heat: f32,
}

/// A rocket that flies in from off the left, crosses the field and leaves to the
/// right, fighting periodic headwinds and threading in front of and behind the tiles
/// wherever the board leaves it a gap. Reading the future is what makes the second of
/// those possible: it only commits to crossing the tile plane through a window it
/// knows will still be open when it gets there.
pub struct Rocket {
    t: f32,
    /// Current world position, integrated rather than interpolated, because the
    /// headwind makes the speed vary and occasionally reverse.
    x: f32,
    to_x: f32,
    /// Peak forward speed, in world units per second.
    peak: f32,
    /// Current lane, and the lane it is easing toward. It re-aims before a dive so the
    /// dive has something to pass under.
    lane_y: f32,
    lane_target: f32,
    /// Where it actually is vertically, and how fast that is changing. Tracked rather
    /// than differentiated in closed form because the drift onto a new lane contributes
    /// as much as the bob does — deriving the heading from the bob alone left the rocket
    /// sliding bodily up or down while still pointing dead ahead.
    y: f32,
    vy: f32,
    /// Rate the lane itself is drifting, kept so the drift can be second order.
    lane_vy: f32,
    /// How far from the middle it is willing to stray while re-aiming.
    lane_limit: f32,
    bob: f32,
    /// True when it is flying under the tile plane.
    behind: bool,
    /// Height above the tile plane, and the rate it is changing at. Kept as a velocity
    /// rather than eased straight to a target so the climb has a definite steepness for
    /// the model to point along.
    z: f32,
    vz: f32,
    /// Beyond this x it commits to the front for the rest of the crossing.
    exit_front_x: f32,
    /// Edge of the visible area; past it, nobody can see it cheat.
    vis_edge: f32,
    /// A row it means to slide over to, held until it is safely below the tiles.
    ///
    /// Ordering matters here, and both orderings are wrong except one. Crossing the plane
    /// while drifting toward cover means crossing into the very tiles it is aiming for —
    /// that is how it ended up ploughing through blocks. Waiting to arrive before crossing
    /// is no better: it is then sitting in a row full of tiles, so no corridor is ever
    /// clear and it never goes down at all. It has to cross first, through clear air in the
    /// row it is already in, and only reposition once it is underneath.
    lane_after_dive: Option<isize>,
    since_side: f32,
    embers: Vec<Ember>,
    next_ember: usize,
    spawn_debt: f32,
    seed: f32,
    rng: Rng,
}

impl Rocket {
    pub fn new(view: &LifeView, rng: &mut Rng) -> Rocket {
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let half_h = view.rows() as f32 * CELL_PX * 0.5;
        let from_x = -half_w - ROCKET_MARGIN;
        let to_x = half_w + ROCKET_MARGIN;
        let mut me = Rocket {
            t: 0.0,
            x: from_x,
            to_x,
            peak: (to_x - from_x) / ROCKET_CROSS_SECS,
            // Somewhere in the middle band, never right along an edge.
            lane_y: (rng.f32() - 0.5) * half_h * 0.9,
            lane_target: 0.0,
            y: 0.0,
            vy: 0.0,
            lane_vy: 0.0,
            lane_limit: half_h * 0.88,
            bob: 9.0 + rng.f32() * 9.0,
            // Enters tucked in behind the field, so the first thing it does on screen is
            // find a gap and climb out through it.
            behind: true,
            z: ROCKET_BEHIND_Z,
            vz: 0.0,
            exit_front_x: half_w * ROCKET_EXIT_FRONT,
            vis_edge: half_w - MARGIN as f32 * CELL_PX,
            lane_after_dive: None,
            since_side: 0.0,
            embers: vec![Ember::default(); EMBERS],
            next_ember: 0,
            spawn_debt: 0.0,
            seed: rng.f32() * 10.0,
            rng: Rng::new(rng.next_u64()),
        };
        me.lane_target = me.lane_y;
        me.y = me.cruise_y();
        me
    }

    /// Forward speed right now: a shallow sine swell with a narrow gust spiking at the
    /// top of it. See the constants for why it is two terms rather than one.
    fn speed(&self) -> f32 {
        let phase = std::f32::consts::TAU * self.t / HEADWIND_SECS + self.seed;
        // 0 where the going is easiest, 1 at the height of the wind.
        let swell = 0.5 - 0.5 * phase.cos();
        let gust = swell.powi(HEADWIND_SHARPNESS);
        self.peak * (1.0 - HEADWIND_SWELL * swell - HEADWIND_GUST * gust)
    }

    fn cruise_y(&self) -> f32 {
        self.lane_y + (self.t * 1.7 + self.seed).sin() * self.bob
    }

    /// World y of the centre of a grid row.
    fn row_to_y(view: &LifeView, row: isize) -> f32 {
        ((view.rows() as f32 - 1.0) * 0.5 - row as f32) * CELL_PX
    }

    /// Position, and the roll, pitch, and yaw that follow from the path. The slow
    /// rotation around local +X shows off the complete three-fin model as it flies.
    ///
    /// The heading is measured against the *cruise* speed rather than the instantaneous
    /// one. Using the latter puts a singularity exactly where the gust drives the speed
    /// through zero: the ratio blows up, `atan` saturates near a right angle, and the
    /// roll — being a multiple of it — carries the model past upside down and back
    /// several times a crossing. A momentary headwind should not change where the nose
    /// points anyway; only the shape of the path does.
    fn pose(&self) -> ([f32; 3], [f32; 3]) {
        // Heading follows the path it is actually on, both terms of it.
        let yaw = (self.vy / self.peak)
            .atan()
            .clamp(-ROCKET_YAW_MAX, ROCKET_YAW_MAX);
        let roll = self.seed + self.t * ROCKET_AXIAL_ROLL_RATE - yaw * ROCKET_BANK
            + (self.t * 1.1 + self.seed).sin() * 0.08;
        // Nose along the actual path through depth. Pitch is positive toward -Z, so
        // climbing toward the viewer wants a negative one.
        let pitch = (-(self.vz / self.peak))
            .atan()
            .clamp(-ROCKET_PITCH_MAX, ROCKET_PITCH_MAX);
        ([self.x, self.y, self.z], [roll, pitch, yaw])
    }

    /// The cell the rocket is over, `cells` cells further along its path.
    fn cell_ahead(&self, view: &LifeView, cells: isize) -> (isize, isize) {
        let x = (self.x / CELL_PX + (view.cols() as f32 - 1.0) * 0.5).round() as isize + cells;
        let y = ((view.rows() as f32 - 1.0) * 0.5 - self.cruise_y() / CELL_PX).round() as isize;
        (x, y)
    }

    /// Is the corridor it would cross the tile plane through clear, and does it stay
    /// clear for `gens` generations?
    ///
    /// The rows either side are always included: the fins reach most of a cell out from
    /// the hull and the bob carries it further still, so checking only the centre row let
    /// it clip tiles above and below on the way through.
    fn corridor_clear(&self, view: &LifeView, cells: isize, gens: usize) -> bool {
        let (cx, cy) = self.cell_ahead(view, 1);
        !(0..cells).any(|step| {
            (-1isize..=1).any(|dy| (0..=gens).any(|gen| view.alive(cx + step, cy + dy, gen)))
        })
    }

    /// Room to dive: the full corridor, held open for a couple of generations.
    fn window_ahead(&self, view: &LifeView) -> bool {
        self.corridor_clear(view, ROCKET_CROSS_CELLS, ROCKET_WINDOW_GENS)
    }

    /// Live cells ahead in a given row band, a generation out — about when it arrives.
    fn cover_in_row(&self, view: &LifeView, row: isize) -> u32 {
        let (cx, _) = self.cell_ahead(view, 0);
        let mut n = 0;
        for step in ROCKET_SCOUT_FROM..ROCKET_SCOUT_TO {
            for dy in -1isize..=1 {
                if view.alive(cx + step, row + dy, 1) {
                    n += 1;
                }
            }
        }
        n
    }

    /// The row within reach with the most to fly under, and how much that is.
    fn best_lane(&self, view: &LifeView) -> (isize, u32) {
        let (_, cy) = self.cell_ahead(view, 0);
        (-ROCKET_STEER_ROWS..=ROCKET_STEER_ROWS)
            .map(|d| (cy + d, self.cover_in_row(view, cy + d)))
            // Nearer rows win ties, so it never swerves further than it has to.
            .max_by_key(|(row, n)| (*n, -(row - cy).abs()))
            .unwrap_or((cy, 0))
    }

    /// Room to climb back out. Deliberately laxer than the check for going down: rising
    /// only has to clear its own row, and only for as long as the climb takes.
    ///
    /// Using the same test for both directions was a trap. The rocket dives into a lane
    /// picked *because* it has tiles in it, so the strict three-row window is rarely open
    /// down there — and with the time limit gated behind it, the rocket could not get
    /// out and spent three quarters of the crossing underneath.

    /// How much tile there is to fly under, further along than the crossing window —
    /// judged a generation out, which is about when it would arrive.
    ///
    /// This is what makes the weave visible at all. Gating the dive only on a clear
    /// window meant it went behind exactly where there was nothing to go behind, and
    /// surfaced again before reaching anything: measured over a whole crossing, not one
    /// tile ever passed over it. Diving *toward* cover is the point.
    fn cover_ahead(&self, view: &LifeView) -> u32 {
        let (cx, cy) = self.cell_ahead(view, 0);
        let mut n = 0;
        for step in ROCKET_SCOUT_FROM..ROCKET_SCOUT_TO {
            for dy in -1isize..=1 {
                if view.alive(cx + step, cy + dy, 1) {
                    n += 1;
                }
            }
        }
        n
    }

    fn spawn_ember(&mut self, tail: [f32; 3], back: [f32; 3]) {
        let e = &mut self.embers[self.next_ember];
        self.next_ember = (self.next_ember + 1) % EMBERS;
        let jitter = |rng: &mut Rng, s: f32| (rng.f32() - 0.5) * s;
        let spread = 150.0;
        *e = Ember {
            pos: [
                tail[0] + jitter(&mut self.rng, 6.0),
                tail[1] + jitter(&mut self.rng, 6.0),
                tail[2] + jitter(&mut self.rng, 6.0),
            ],
            vel: [
                back[0] * spread + jitter(&mut self.rng, 74.0),
                back[1] * spread + jitter(&mut self.rng, 74.0),
                back[2] * spread + jitter(&mut self.rng, 62.0),
            ],
            rot: [
                self.rng.f32() * 6.28,
                self.rng.f32() * 6.28,
                self.rng.f32() * 6.28,
            ],
            spin: [
                jitter(&mut self.rng, 5.0),
                jitter(&mut self.rng, 5.0),
                jitter(&mut self.rng, 5.0),
            ],
            age: 0.0,
            life: EMBER_LIFE * (0.7 + self.rng.f32() * 0.6),
            size: 6.0 + self.rng.f32() * 7.0,
            heat: self.rng.f32(),
        };
    }
}

impl Critter for Rocket {
    fn update(&mut self, ctx: &CritterCtx) -> bool {
        self.t += ctx.dt;
        self.since_side += ctx.dt;
        self.x += self.speed() * ctx.dt;

        // Change sides through a gap the board will hold open. It dives at any chance it
        // gets, and comes back out once there is nothing left ahead to pass beneath — or
        // once it has been under long enough regardless.
        let dwell = if self.behind {
            ROCKET_DWELL_BEHIND
        } else {
            ROCKET_DWELL_FRONT
        };
        // Depth changes read best out toward the sides, where the camera sees more of
        // them, so it is quicker to commit to one there.
        let dwell = if self.x.abs() > self.exit_front_x {
            dwell * 0.55
        } else {
            dwell
        };
        if self.behind {
            // Come up once there is nothing left ahead to hide under, or once it has been
            // down long enough — and past the limit, take the gap whether or not one is
            // strictly there, since emerging is the part worth seeing.
            let overdue = self.since_side > ROCKET_MAX_BEHIND;
            let spent = self.cover_ahead(&ctx.life) == 0;
            let leaving = self.x > self.exit_front_x;
            // Never cross the plane through a tile where it can be seen doing it. When
            // it has waited too long it will take a narrower gap, and only once it is
            // past the edge of the frame will it come up through anything at all.
            let clear = self.corridor_clear(&ctx.life, ROCKET_CROSS_CELLS, 1);
            let squeeze = self.corridor_clear(&ctx.life, ROCKET_MIN_CELLS, 1);
            let unseen = self.x.abs() > self.vis_edge;
            if self.since_side > dwell
                && ((spent && clear) || ((overdue || leaving) && squeeze) || unseen)
            {
                self.behind = false;
                self.since_side = 0.0;
                // Hold the lane steady through the climb, for the same reason as the dive.
                self.lane_target = self.lane_y;
                self.lane_after_dive = None;
            }
        } else if self.x < self.exit_front_x
            && self.since_side > dwell
            && self.window_ahead(&ctx.life)
        {
            // Straight down through the clear corridor it is already lined up with, and
            // only note where the cover is — the slide over to it happens underneath.
            let (row, cover) = self.best_lane(&ctx.life);
            if cover > 0 || self.since_side > ROCKET_DIVE_OVERDUE {
                self.behind = true;
                self.since_side = 0.0;
                self.lane_after_dive = (cover > 0).then_some(row);
            }
        }

        // Once it is genuinely below the tiles, slide over to whatever it spotted. Nothing
        // to hit down here, so the drift is free.
        if self.behind && self.z < -RISE {
            if let Some(row) = self.lane_after_dive.take() {
                self.lane_target =
                    Self::row_to_y(&ctx.life, row).clamp(-self.lane_limit, self.lane_limit);
            }
        }

        // Drift onto the chosen lane as a critically damped spring, so the lane's own
        // velocity starts from zero and stays continuous however the target jumps.
        let k = ROCKET_LANE_STIFFNESS;
        let accel = (self.lane_target - self.lane_y) * k * k - 2.0 * k * self.lane_vy;
        self.lane_vy += accel * ctx.dt;
        self.lane_y += self.lane_vy * ctx.dt;
        let y = self.cruise_y();
        // Low-passed, not the raw difference. Attitude is derived from this, and a
        // finite difference over one frame turns any step in position — the first frame
        // after spawning, a resumed clock — into an instantaneous flick of the whole
        // model. Smoothing it means the nose can only ever swing at a believable rate.
        let raw = if ctx.dt > 1e-5 {
            (y - self.y) / ctx.dt
        } else {
            self.vy
        };
        self.vy += (raw - self.vy) * (1.0 - (-12.0 * ctx.dt).exp());
        self.y = y;

        // Ease toward the side it is on, and drift gently toward and away from the
        // viewer on top of that.
        let weave = (self.t * 0.55 + self.seed).sin() * ROCKET_Z_WEAVE;
        let target = if self.behind {
            ROCKET_BEHIND_Z
        } else {
            ROCKET_FRONT_Z
        } + weave;
        // Fly the change of depth rather than easing into it: aim for a climb rate,
        // approach that rate quickly but not instantly, and integrate. That gives the
        // path a definite steepness, which is what the model's pitch is taken from.
        let want_vz = ((target - self.z) * 2.8).clamp(-ROCKET_Z_RATE, ROCKET_Z_RATE);
        // Eased into rather than applied at once: the pitch is an arctangent of this, and
        // arctan is steepest at zero, so how fast the rocket may *start* nosing over is
        // set here rather than by the climb rate itself.
        self.vz += (want_vz - self.vz) * (1.0 - (-3.5 * ctx.dt).exp());
        self.z += self.vz * ctx.dt;

        // Exhaust, thrown from the tail back along the rocket's own axis.
        let (pos, rot) = self.pose();
        let back = rot_x_axis(rot, -1.0);
        let tail = [
            pos[0] + back[0] * ROCKET_LENGTH * 0.46,
            pos[1] + back[1] * ROCKET_LENGTH * 0.46,
            pos[2] + back[2] * ROCKET_LENGTH * 0.46,
        ];
        // More sparks when it is working hard, fewer when a gust has it stalled.
        let throttle = (self.speed() / self.peak).clamp(0.0, 1.0);
        self.spawn_debt += ctx.dt * (21.0 + 60.0 * throttle);
        while self.spawn_debt >= 1.0 {
            self.spawn_debt -= 1.0;
            self.spawn_ember(tail, back);
        }

        for e in &mut self.embers {
            if e.life <= 0.0 {
                continue;
            }
            e.age += ctx.dt;
            if e.age >= e.life {
                // Retire the slot rather than leaving it to age on forever. It is not
                // drawn either way, but the ring only comes back around to reuse it
                // when the spawn rate lets it, which during a gust can be a while.
                e.life = 0.0;
                continue;
            }
            for k in 0..3 {
                e.pos[k] += e.vel[k] * ctx.dt;
                e.rot[k] += e.spin[k] * ctx.dt;
            }
            // Sparks lose their push almost at once, so the plume stays short.
            let drag = (-2.8 * ctx.dt).exp();
            for v in &mut e.vel {
                *v *= drag;
            }
        }

        self.x < self.to_x
    }

    fn props(&self, _ctx: &CritterCtx, out: &mut PropSink) {
        let (pos, rot) = self.pose();
        out.push(MODEL_ROCKET, Prop::new(pos, 1.0, rot));

        let orange = srgb_hex_to_linear(0xff8a1e);
        let yellow = srgb_hex_to_linear(0xffdc4a);
        for e in &self.embers {
            if e.life <= 0.0 {
                continue;
            }
            let p = e.age / e.life;
            // Snaps alight, then fades away over the rest of its life.
            let rise = (p / 0.14).min(1.0);
            let fall = ((1.0 - p) / 0.86).max(0.0).powf(0.75);
            let mut tint = [0.0f32; 3];
            for k in 0..3 {
                tint[k] = orange[k] + (yellow[k] - orange[k]) * e.heat;
            }
            out.push(
                MODEL_EMBER,
                Prop::new(e.pos, e.size, e.rot).tinted(tint, 0.8 * rise * fall),
            );
        }
    }
}

// ---------------------------------------------------------------------------
// The bumblebee — a textured visitor with procedural wings
// ---------------------------------------------------------------------------

const BEE_SIZE: f32 = 0.80;
const BEE_BODY_W: f32 = 70.0 * BEE_SIZE;
const BEE_BODY_H: f32 = BEE_BODY_W * 269.0 / 420.0;
const BEE_Z: f32 = 48.0;
const BEE_APPROACH_SPEED: f32 = 104.0;
const BEE_SCUTTLE_RADIUS: f32 = CELL_PX * TILE_FILL * 0.24;
const BEE_KICK_SECS: f32 = 0.34;
const BEE_KICK_GROW: f32 = 0.70;
const BEE_KICK_LOOM_PORTION: f32 = 0.58;
const BEE_KICK_AWAY: f32 = 44.0;
const BEE_WING_SIZE: f32 = 1.10;
/// Attachments in the original 420x269 body coordinate system, in atlas order.
const BEE_LEG_BODY_PIVOTS: [[f32; 2]; 6] = [
    [122.0, 61.0],
    [181.0, 58.0],
    [270.0, 77.0],
    [126.0, 210.0],
    [183.0, 213.0],
    [271.0, 210.0],
];

#[derive(Clone, Copy, Debug, PartialEq)]
enum BeeAct {
    Flying,
    Approach {
        col: isize,
        row: isize,
        x: f32,
        y: f32,
    },
    Landed {
        col: isize,
        row: isize,
    },
    Kicked {
        since: f32,
        origin: [f32; 2],
        away: [f32; 2],
    },
}

/// A top-down bee enters low on the right, bumbles generally left and upward, and
/// plots a course between coloured tile faces. The walker treats tile edges as ground
/// in a perpendicular side-view world, so their two performances never collide. The
/// body is a generated cutout; wings remain geometry so flight blur, random angles, and
/// still crisp landing poses do not require sprite sheets.
pub struct Bee {
    x: f32,
    y: f32,
    vx: f32,
    vy: f32,
    want_vx: f32,
    want_vy: f32,
    yaw: f32,
    t: f32,
    next_turn: f32,
    next_land_check: f32,
    wing_phase: f32,
    next_twitch: f32,
    scuttle_goal: [f32; 2],
    scuttle_speed: f32,
    next_scuttle: f32,
    vis_left: f32,
    vis_top: f32,
    vis_bottom: f32,
    act: BeeAct,
    rng: Rng,
}

impl Bee {
    pub fn new(view: &LifeView, rng: &mut Rng) -> Bee {
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let half_h = view.rows() as f32 * CELL_PX * 0.5;
        let margin = MARGIN as f32 * CELL_PX;
        let vis_right = half_w - margin;
        let vis_top = half_h - margin;
        let vis_bottom = -vis_top;
        let height = vis_top - vis_bottom;
        let y = vis_bottom + height * (0.12 + rng.f32() * 0.24);
        let vx = -(92.0 + rng.f32() * 44.0);
        let vy = 20.0 + rng.f32() * 34.0;
        Bee {
            x: vis_right + BEE_BODY_W * 0.65,
            y,
            vx,
            vy,
            want_vx: vx,
            want_vy: vy,
            yaw: 0.0,
            t: 0.0,
            next_turn: 0.35 + rng.f32() * 0.45,
            next_land_check: 0.9 + rng.f32() * 0.9,
            wing_phase: rng.f32() * std::f32::consts::TAU,
            next_twitch: 0.0,
            scuttle_goal: [0.0; 2],
            scuttle_speed: 0.0,
            next_scuttle: 0.0,
            vis_left: -vis_right,
            vis_top,
            vis_bottom,
            act: BeeAct::Flying,
            rng: Rng::new(rng.next_u64()),
        }
    }

    fn tile_face(view: &LifeView, col: isize, row: isize) -> [f32; 2] {
        view.cell_center(col as f32, row as f32)
    }

    fn landing_is_live(view: &LifeView, col: isize, row: isize) -> bool {
        col >= 0
            && row >= 0
            && col < view.cols() as isize
            && row < view.rows() as isize
            && view.alive(col, row, 0)
            && is_colored(col as usize, row as usize)
    }

    /// Plot the next leg toward a stable coloured face up-and-left. The bee treats those
    /// accents as flowers and ignores the neutral tiles entirely. It need not have an
    /// exposed screen-top edge: this is the orthogonal square face itself.
    fn course_ahead(&mut self, view: &LifeView) -> Option<(isize, isize, f32, f32)> {
        let mut choices = Vec::new();
        for col in MARGIN as isize..(view.cols() - MARGIN) as isize {
            for row in MARGIN as isize..(view.rows() - MARGIN) as isize {
                let stable = is_colored(col as usize, row as usize)
                    && (0..=1).all(|g| view.alive(col, row, g));
                if !stable {
                    continue;
                }
                let [x, y] = Self::tile_face(view, col, row);
                let dx = x - self.x;
                let dy = y - self.y;
                if !(-CELL_PX * 7.2..=-CELL_PX * 0.30).contains(&dx)
                    || !(-CELL_PX * 1.15..=CELL_PX * 4.8).contains(&dy)
                {
                    continue;
                }
                let distance = (dx * dx + dy * dy).sqrt();
                // Downward detours lose strongly; a little randomness keeps two visits
                // to the same field from tracing exactly the same flower chain.
                let score = distance + (-dy).max(0.0) * 2.4 + self.rng.f32() * CELL_PX * 0.65;
                choices.push((score, col, row, x, y));
            }
        }
        choices
            .into_iter()
            .min_by(|a, b| a.0.partial_cmp(&b.0).unwrap())
            .map(|(_, col, row, x, y)| (col, row, x, y))
    }

    fn choose_scuttle(&mut self, view: &LifeView, col: isize, row: isize) {
        let centre = Self::tile_face(view, col, row);
        let angle = self.rng.f32() * std::f32::consts::TAU;
        let radius = BEE_SCUTTLE_RADIUS * (0.25 + self.rng.f32() * 0.75);
        self.scuttle_goal = [
            centre[0] + angle.cos() * radius,
            centre[1] + angle.sin() * radius,
        ];
        self.scuttle_speed = 22.0 + self.rng.f32() * 34.0;
        self.next_scuttle = self.t + 0.22 + self.rng.f32() * 0.50;
    }

    fn kick_off(&mut self) {
        let angle = self.rng.f32() * std::f32::consts::TAU;
        self.act = BeeAct::Kicked {
            since: self.t,
            origin: [self.x, self.y],
            away: [angle.cos(), angle.sin()],
        };
        self.vx = angle.cos() * BEE_KICK_AWAY / BEE_KICK_SECS;
        self.vy = angle.sin() * BEE_KICK_AWAY / BEE_KICK_SECS;
    }

    fn startled_take_off(&mut self) {
        self.act = BeeAct::Flying;
        self.want_vx = -(105.0 + self.rng.f32() * 45.0);
        self.want_vy = 78.0 + self.rng.f32() * 42.0;
        self.vx = self.want_vx;
        self.vy = self.want_vy;
        self.next_turn = self.t + 0.32 + self.rng.f32() * 0.38;
        self.next_land_check = self.t + 0.75;
    }

    fn recover_from_kick(&mut self) {
        self.act = BeeAct::Flying;
        self.want_vx = -(105.0 + self.rng.f32() * 45.0);
        self.want_vy = 28.0 + self.rng.f32() * 40.0;
        // Preserve the direction of the shove for a few frames, then the normal flight
        // steering bends it back toward the next flower without a velocity cut.
        self.next_turn = self.t + 0.20;
        self.next_land_check = self.t + 0.08;
    }

    fn body_y(&self) -> f32 {
        match self.act {
            BeeAct::Flying | BeeAct::Approach { .. } => self.y + (self.t * 8.5).sin() * 1.4,
            BeeAct::Landed { .. } | BeeAct::Kicked { .. } => self.y,
        }
    }

    fn presentation_scale(&self) -> f32 {
        match self.act {
            BeeAct::Kicked { since, .. } => {
                let p = ((self.t - since) / BEE_KICK_SECS).clamp(0.0, 1.0);
                let loom = if p < BEE_KICK_LOOM_PORTION {
                    (std::f32::consts::PI * p / BEE_KICK_LOOM_PORTION).sin()
                } else {
                    0.0
                };
                1.0 + BEE_KICK_GROW * loom
            }
            _ => 1.0,
        }
    }

    fn leg_wiggle(&self, leg: usize) -> f32 {
        let side = if leg < 3 { 1.0 } else { -1.0 };
        let phase = leg as f32 * 1.37;
        match self.act {
            BeeAct::Landed { .. } => {
                side * ((self.t * 22.0 + phase).sin() * 0.23
                    + (self.t * 47.0 + phase * 0.7).sin() * 0.055)
            }
            BeeAct::Kicked { .. } => side * (self.t * 29.0 + phase).sin() * 0.34,
            BeeAct::Flying | BeeAct::Approach { .. } => {
                side * (self.t * 10.0 + phase).sin() * 0.055
            }
        }
    }

    fn legs(&self, y: f32, scale: f32, out: &mut PropSink) {
        let pixel = BEE_BODY_W * scale / 420.0;
        let leg_scale = BEE_BODY_W * scale * BEE_LEG_CELL_W / 420.0;
        let (sy, cy) = self.yaw.sin_cos();
        for (leg, [px, py]) in BEE_LEG_BODY_PIVOTS.into_iter().enumerate() {
            let local = [(px - 210.0) * pixel, (134.5 - py) * pixel];
            let pivot = [
                self.x + local[0] * cy - local[1] * sy,
                y + local[0] * sy + local[1] * cy,
                BEE_Z - 1.4,
            ];
            out.push(
                MODEL_BEE_LEG_FIRST + leg,
                Prop::new(
                    pivot,
                    leg_scale,
                    [0.0, 0.0, self.yaw + self.leg_wiggle(leg)],
                ),
            );
        }
    }

    fn wing(
        out: &mut PropSink,
        root: [f32; 2],
        body_yaw: f32,
        rel: f32,
        length: f32,
        width: f32,
        alpha: f32,
        z: f32,
    ) {
        let length = length * BEE_WING_SIZE;
        let width = width * BEE_WING_SIZE;
        let a = body_yaw + rel;
        let (s, c) = a.sin_cos();
        let centre = [root[0] + c * length * 0.48, root[1] + s * length * 0.48, z];
        let tint = srgb_hex_to_linear(0xa8bec9);
        out.push(
            MODEL_BEE_WING,
            Prop::stretched(centre, [length * 0.58, width, 1.0], [0.0, 0.0, a]).tinted(tint, alpha),
        );
    }
}

impl Critter for Bee {
    fn update(&mut self, ctx: &CritterCtx) -> bool {
        self.t += ctx.dt;

        match self.act {
            BeeAct::Flying => {
                if self.t >= self.next_turn {
                    self.want_vx = -(82.0 + self.rng.f32() * 72.0);
                    self.want_vy = 8.0 + self.rng.f32() * 74.0;
                    self.next_turn = self.t + 0.45 + self.rng.f32() * 0.85;
                }
                let ease = 1.0 - (-2.8 * ctx.dt).exp();
                self.vx += (self.want_vx - self.vx) * ease;
                self.vy += (self.want_vy - self.vy) * ease;
                self.x += self.vx * ctx.dt;
                self.y += self.vy * ctx.dt;

                if self.t >= self.next_land_check {
                    self.next_land_check = self.t + 0.35 + self.rng.f32() * 0.45;
                    if let Some((col, row, x, y)) = self.course_ahead(&ctx.life) {
                        self.act = BeeAct::Approach { col, row, x, y };
                    }
                }
            }
            BeeAct::Approach { col, row, x, y } => {
                if !Self::landing_is_live(&ctx.life, col, row) || ctx.tile_spinning(col, row) {
                    self.startled_take_off();
                } else {
                    let dx = x - self.x;
                    let dy = y - self.y;
                    let distance = (dx * dx + dy * dy).sqrt();
                    if distance < 2.2 {
                        self.x = x;
                        self.y = y;
                        self.vx = 0.0;
                        self.vy = 0.0;
                        self.yaw = 0.0;
                        self.next_twitch = self.t + 0.7 + self.rng.f32() * 0.7;
                        self.act = BeeAct::Landed { col, row };
                        self.choose_scuttle(&ctx.life, col, row);
                    } else {
                        let speed = (distance * 2.7).clamp(18.0, BEE_APPROACH_SPEED);
                        let target_vx = dx / distance * speed;
                        let target_vy = dy / distance * speed;
                        let ease = 1.0 - (-6.0 * ctx.dt).exp();
                        self.vx += (target_vx - self.vx) * ease;
                        self.vy += (target_vy - self.vy) * ease;
                        self.x += self.vx * ctx.dt;
                        self.y += self.vy * ctx.dt;
                    }
                }
            }
            BeeAct::Landed { col, row } => {
                if !Self::landing_is_live(&ctx.life, col, row) {
                    self.startled_take_off();
                } else if ctx.tile_spinning(col, row) {
                    self.kick_off();
                } else {
                    if self.t >= self.next_twitch {
                        self.wing_phase = (self.rng.f32() - 0.5) * 0.55;
                        self.next_twitch = self.t + 0.75 + self.rng.f32() * 0.65;
                    }
                    let dx = self.scuttle_goal[0] - self.x;
                    let dy = self.scuttle_goal[1] - self.y;
                    let distance = (dx * dx + dy * dy).sqrt();
                    if self.t >= self.next_scuttle || distance < 0.8 {
                        self.choose_scuttle(&ctx.life, col, row);
                    } else {
                        // A clipped sine makes a stop-go gait: quick little darts broken
                        // by near-pauses and slight heading shakes, not a smooth orbit.
                        let pulse = (self.t * 31.0 + self.wing_phase * 7.0).sin().max(0.0);
                        let step =
                            (self.scuttle_speed * ctx.dt * (0.16 + 0.84 * pulse)).min(distance);
                        self.x += dx / distance * step;
                        self.y += dy / distance * step;
                        let want =
                            dy.atan2(dx) - std::f32::consts::PI + (self.t * 23.0).sin() * 0.07;
                        let delta = (want - self.yaw + std::f32::consts::PI)
                            .rem_euclid(std::f32::consts::TAU)
                            - std::f32::consts::PI;
                        self.yaw += delta * (1.0 - (-15.0 * ctx.dt).exp());
                    }
                }
            }
            BeeAct::Kicked {
                since,
                origin,
                away,
            } => {
                let p = ((self.t - since) / BEE_KICK_SECS).clamp(0.0, 1.0);
                let old = [self.x, self.y];
                let travel = BEE_KICK_AWAY * (1.0 - (1.0 - p).powi(2));
                let normal = [-away[1], away[0]];
                let wobble = (p * std::f32::consts::TAU).sin() * 3.5;
                self.x = origin[0] + away[0] * travel + normal[0] * wobble;
                self.y = origin[1] + away[1] * travel + normal[1] * wobble;
                if ctx.dt > 1e-5 {
                    self.vx = (self.x - old[0]) / ctx.dt;
                    self.vy = (self.y - old[1]) / ctx.dt;
                }
                if p >= 1.0 {
                    self.recover_from_kick();
                }
            }
        }

        if !matches!(self.act, BeeAct::Landed { .. }) {
            // A fresh sample every frame is the irregular blur rather than a clockwork
            // sine wave. The props method fans several translucent exposures around it.
            self.wing_phase = self.rng.f32() * std::f32::consts::TAU;
            if self.vx.abs() + self.vy.abs() > 1.0 {
                let travel = self.vy.atan2(self.vx);
                let want = travel - std::f32::consts::PI;
                let delta = (want - self.yaw + std::f32::consts::PI)
                    .rem_euclid(std::f32::consts::TAU)
                    - std::f32::consts::PI;
                self.yaw += delta * (1.0 - (-7.0 * ctx.dt).exp());
            }
        }

        self.x > self.vis_left - BEE_BODY_W
            && self.y < self.vis_top + BEE_BODY_H
            && self.y > self.vis_bottom - BEE_BODY_H
    }

    fn props(&self, _ctx: &CritterCtx, out: &mut PropSink) {
        let y = self.body_y();
        let scale = self.presentation_scale();
        let (sy, cy) = self.yaw.sin_cos();
        self.legs(y, scale, out);
        // Both wings meet at the dorsal thorax, slightly headward in bee-local space.
        let root = [
            self.x - cy * 7.0 * BEE_SIZE * scale,
            y - sy * 7.0 * BEE_SIZE * scale,
        ];
        if matches!(self.act, BeeAct::Landed { .. }) {
            // In dorsal view the crisp pair opens to opposite sides of the body. Two
            // nested ellipses give each wing a translucent rim; the angle changes only
            // on the roughly one-second twitch.
            for side in [-1.0f32, 1.0] {
                let rel = side * (0.82 + self.wing_phase * 0.22);
                Self::wing(
                    out,
                    root,
                    self.yaw,
                    rel,
                    38.0 * BEE_SIZE,
                    6.4 * BEE_SIZE,
                    0.30,
                    BEE_Z - 0.8,
                );
                Self::wing(
                    out,
                    root,
                    self.yaw,
                    rel,
                    34.0 * BEE_SIZE,
                    4.5 * BEE_SIZE,
                    0.48,
                    BEE_Z - 0.6,
                );
            }
        } else {
            // Four broad exposures on either side make the randomized top-down flight
            // blur. This model is drawn after the photographic body, veiling the thorax
            // beneath the beating wings instead of leaving the whole bee unobscured.
            for side in [-1.0f32, 1.0] {
                for trail in 0..4 {
                    let flutter = (self.wing_phase + trail as f32 * 1.17).sin() * 0.45;
                    let rel = side * (0.72 + flutter);
                    Self::wing(
                        out,
                        root,
                        self.yaw,
                        rel,
                        42.0 * BEE_SIZE * scale,
                        10.5 * BEE_SIZE * scale,
                        0.13,
                        BEE_Z - 1.0,
                    );
                }
            }
        }
        out.push(
            MODEL_BEE_BODY,
            Prop::stretched(
                [self.x, y, BEE_Z],
                [BEE_BODY_W * scale, BEE_BODY_W * scale, 1.0],
                [0.0, 0.0, self.yaw],
            ),
        );
    }

    #[cfg(test)]
    fn debug_state(&self) -> Option<String> {
        Some(format!("{:?} x={:.1} y={:.1}", self.act, self.x, self.y))
    }
}

// ---------------------------------------------------------------------------
// Distant layer — one colossal silhouette crossing behind the tile field at a time
// ---------------------------------------------------------------------------

const DISTANT_TUX_W: f32 = 602.0;
const DISTANT_TUX_H: f32 = DISTANT_TUX_W * 384.0 / 305.0;
const DISTANT_GODZILLA_W: f32 = 790.0 * 1.40;
const DISTANT_GODZILLA_H: f32 = DISTANT_GODZILLA_W * 266.0 / 384.0;
/// The marshmallow sailor is authored in height-normalized coordinates. His arms
/// bring the side-view silhouette to roughly 70% of this height.
const DISTANT_MARSHMALLOW_H: f32 = 820.0;
const DISTANT_MARSHMALLOW_W: f32 = DISTANT_MARSHMALLOW_H * 0.72;
const DISTANT_WALK_SPEED: f32 = 34.0;
const DISTANT_TUX_MOTION_SCALE: f32 = DISTANT_TUX_W / 70.0 * 0.64;
const DISTANT_TUX_TIME_SCALE: f32 = 0.58;
/// Godzilla and the marshmallow sailor composite at 33%, while Tux's compensating
/// source alpha preserves his separately tuned effective 28% through the same layer.
const DISTANT_LAYER_OPACITY: f32 = 0.33;
const DISTANT_TUX_ALPHA: f32 = 0.28 / DISTANT_LAYER_OPACITY;
const DISTANT_GODZILLA_ALPHA: f32 = 1.0;
const DISTANT_MARSHMALLOW_ALPHA: f32 = 1.0;
/// World-space expansion for the back-face silhouette shell. Post-process blur turns
/// this narrow cool-gray rim into a soft edge cue instead of a cartoon ink stroke.
const DISTANT_MARSHMALLOW_OUTLINE_PX: f32 = 19.0;
const DISTANT_TUX_BASE_LIFT: f32 = 110.0;
const DISTANT_GODZILLA_BASE_LIFT: f32 = 92.0;
const DISTANT_MARSHMALLOW_BASE_LIFT: f32 = 96.0;
const DISTANT_PAUSE_MIN: f32 = 7.0;
const DISTANT_PAUSE_SPAN: f32 = 10.0;
/// The wide articulated stride covers about 75px per step at this scale. A 1.1 rad/s
/// cycle lets the body travel approximately that far between alternating footfalls,
/// instead of the legs completing two steps over one step's worth of translation.
const GODZILLA_STEP_RATE: f32 = 1.10;
const GODZILLA_TAIL_FLICK_MIN: f32 = 4.0;
const GODZILLA_TAIL_FLICK_SPAN: f32 = 6.0;
const GODZILLA_TAIL_AWAY_SECS: f32 = 0.338;
const GODZILLA_TAIL_HOLD_SECS: f32 = 0.130;
const GODZILLA_TAIL_RETURN_SECS: f32 = 0.169;
const GODZILLA_TAIL_FLICK_SECS: f32 =
    GODZILLA_TAIL_AWAY_SECS + GODZILLA_TAIL_HOLD_SECS + GODZILLA_TAIL_RETURN_SECS;
/// Gaussian sample spacing in the half-resolution background layer. The two-pixel
/// kernel radius and 2x upscale spread the fog well beyond the silhouette without
/// ever filtering against the edge of Tux's own image quad.
const DISTANT_TUX_BLUR_LAYER_PX: f32 = 10.0;
const PENGUIN_CYCLE: f32 = 5.2;
const PENGUIN_WADDLE_SECS: f32 = 2.55;
const PENGUIN_HOP_SECS: f32 = 1.05;
const PENGUIN_HOP_HEIGHT: f32 = 38.0;
const MARSHMALLOW_STEP_RATE: f32 = 0.78;
const MARSHMALLOW_LOOK_CYCLE: f32 = 17.0;
const MARSHMALLOW_HEAD_Y: f32 = 0.805;

#[derive(Clone, Copy, Debug)]
struct SpritePose {
    y: f32,
    x_jitter: f32,
    rot: f32,
    scale_x: f32,
    scale_y: f32,
    speed: f32,
}

#[derive(Clone, Copy, Debug)]
struct GodzillaLegPose {
    upper: [f32; 2],
    lower: [f32; 2],
}

#[derive(Clone, Copy, Debug)]
struct MarshmallowPose {
    bob: f32,
    roll: f32,
    depth: f32,
    speed: f32,
    body_front: f32,
    head_front: f32,
    upper_leg: [f32; 2],
    lower_leg: [f32; 2],
    arm: [f32; 2],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DistantKind {
    Tux,
    Godzilla,
    Marshmallow,
}

struct DistantCrossing {
    kind: DistantKind,
    t: f32,
    x: f32,
    to_x: f32,
    direction: f32,
    ground_y: f32,
    seed: f32,
    tail_flick_started: Option<f32>,
    tail_next_flick_at: f32,
}

struct DistantBackground {
    crossing: Option<DistantCrossing>,
    cooldown: f32,
    rng: Rng,
    next_kind: DistantKind,
    /// The quieter page modes stretch the rest between crossings.
    pause_scale: f32,
    /// Cleared for pages that want the fog with no visitors at all.
    enabled: bool,
}

impl DistantBackground {
    fn new(view: &LifeView, seed: u64) -> DistantBackground {
        let mut background = DistantBackground {
            crossing: None,
            cooldown: 0.0,
            rng: Rng::new(seed),
            // Lead with the new visitor while his look and movement are being
            // refined. The one-shot cycle continues Marshmallow -> Tux -> Godzilla,
            // so the established giants remain in the background rotation.
            next_kind: DistantKind::Marshmallow,
            pause_scale: 1.0,
            enabled: true,
        };
        background.start_crossing(view);
        background
    }

    fn rest(&mut self) -> f32 {
        (DISTANT_PAUSE_MIN + self.rng.f32() * DISTANT_PAUSE_SPAN) * self.pause_scale
    }

    /// Drop any crossing in progress without spending that visitor's turn, and rest
    /// a full pause before the next one.
    fn clear_crossing(&mut self) {
        if let Some(crossing) = self.crossing.take() {
            self.next_kind = crossing.kind;
        }
        self.cooldown = self.rest();
    }

    /// Stretch the gaps between crossings, and trade the crossing `new` opened with
    /// for a first full pause.
    fn calm(&mut self, pause_scale: f32) {
        self.pause_scale = pause_scale;
        self.clear_crossing();
    }

    fn disable(&mut self) {
        self.enabled = false;
        self.crossing = None;
    }

    fn start_crossing(&mut self, view: &LifeView) {
        let kind = self.next_kind;
        // Cycle after a random initial pick. All three visitors therefore get seen,
        // while the single Option structurally prevents overlap.
        self.next_kind = match kind {
            DistantKind::Tux => DistantKind::Godzilla,
            DistantKind::Godzilla => DistantKind::Marshmallow,
            DistantKind::Marshmallow => DistantKind::Tux,
        };
        let (width, height, lift) = match kind {
            DistantKind::Tux => (DISTANT_TUX_W, DISTANT_TUX_H, DISTANT_TUX_BASE_LIFT),
            DistantKind::Godzilla => (
                DISTANT_GODZILLA_W,
                DISTANT_GODZILLA_H,
                DISTANT_GODZILLA_BASE_LIFT,
            ),
            DistantKind::Marshmallow => (
                DISTANT_MARSHMALLOW_W,
                DISTANT_MARSHMALLOW_H,
                DISTANT_MARSHMALLOW_BASE_LIFT,
            ),
        };
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let half_h = view.rows() as f32 * CELL_PX * 0.5;
        let visible_left = -half_w + MARGIN as f32 * CELL_PX;
        let visible_right = half_w - MARGIN as f32 * CELL_PX;
        let visible_bottom = -half_h + MARGIN as f32 * CELL_PX;
        let pad = width * 0.52 + DISTANT_TUX_BLUR_LAYER_PX * 4.0;
        let direction = if self.rng.f32() < 0.5 { 1.0 } else { -1.0 };
        let (from_x, to_x) = if direction > 0.0 {
            (visible_left - pad, visible_right + pad)
        } else {
            (visible_right + pad, visible_left - pad)
        };
        self.crossing = Some(DistantCrossing {
            kind,
            t: 0.0,
            // Every individual starts fully outside, traverses once, and is retired.
            x: from_x,
            to_x,
            direction,
            ground_y: visible_bottom + height * 0.5 + lift + self.rng.f32() * CELL_PX * 0.38,
            seed: self.rng.f32() * std::f32::consts::TAU,
            tail_flick_started: None,
            tail_next_flick_at: GODZILLA_TAIL_FLICK_MIN + self.rng.f32() * GODZILLA_TAIL_FLICK_SPAN,
        });
    }

    fn tux_pose_at(t: f32, seed: f32) -> SpritePose {
        let phase = t.rem_euclid(PENGUIN_CYCLE);
        if phase < PENGUIN_WADDLE_SECS {
            // Four short, alternating steps. The whole silhouette tips about its feet
            // and lifts on each step, which reads as a waddle even from a single image.
            let p = phase / PENGUIN_WADDLE_SECS;
            let step = (p * std::f32::consts::TAU * 2.0 + seed).sin();
            let lift = step.abs();
            SpritePose {
                y: 2.8 * lift,
                x_jitter: 0.0,
                rot: 0.075 * step,
                scale_x: 1.0 - 0.018 * lift,
                scale_y: 1.0 + 0.018 * lift,
                speed: 0.67 + 0.18 * lift,
            }
        } else if phase < PENGUIN_WADDLE_SECS + PENGUIN_HOP_SECS {
            // A complete parabola, not a leap offscreen: its last frame returns to the
            // exact same baseline before the quick scoot begins.
            let p = (phase - PENGUIN_WADDLE_SECS) / PENGUIN_HOP_SECS;
            let arc = 4.0 * p * (1.0 - p);
            SpritePose {
                y: PENGUIN_HOP_HEIGHT * arc,
                x_jitter: 0.0,
                rot: -0.11 * (std::f32::consts::PI * p).sin(),
                scale_x: 1.065 - 0.065 * arc,
                scale_y: 0.94 + 0.06 * arc,
                speed: 1.22,
            }
        } else {
            // A low, fast scoot after landing. Scaling eases back to neutral at both
            // ends, so the next waddle begins without a one-frame pop.
            let p = (phase - PENGUIN_WADDLE_SECS - PENGUIN_HOP_SECS)
                / (PENGUIN_CYCLE - PENGUIN_WADDLE_SECS - PENGUIN_HOP_SECS);
            let crouch = (std::f32::consts::PI * p).sin();
            SpritePose {
                y: 0.0,
                x_jitter: 1.3 * (p * std::f32::consts::TAU * 3.0).sin() * crouch,
                rot: -0.10 * crouch,
                scale_x: 1.0 + 0.08 * crouch,
                scale_y: 1.0 - 0.08 * crouch,
                speed: 1.05 + 0.38 * crouch,
            }
        }
    }

    fn godzilla_pose_at(t: f32, seed: f32) -> SpritePose {
        // Heavy alternating footfalls with a small delayed body sway. The wider leg
        // arc carries the visible stride at a cadence matched to horizontal travel.
        // Forward speed peaks while the planted leg sweeps backward, then eases at
        // each exchange where both feet momentarily reverse direction.
        let phase = t * GODZILLA_STEP_RATE + seed;
        let step = phase.sin();
        let impact = step.abs().powf(3.2);
        let planted_drive = phase.cos().abs().powf(0.85);
        SpritePose {
            y: 1.45 * impact,
            x_jitter: 0.35 * (t * GODZILLA_STEP_RATE * 0.5 + seed * 0.4).sin(),
            rot: 0.0145 * step + 0.0045 * (t * 0.53 + seed).sin(),
            scale_x: 1.0 + 0.004 * impact,
            scale_y: 1.0 - 0.004 * impact,
            speed: 0.60 + 0.28 * planted_drive,
        }
    }

    fn godzilla_depth_at(t: f32, seed: f32) -> f32 {
        // A full front/back body rock takes two footfalls. Real perspective makes the
        // complete rig grow and shrink subtly instead of faking the effect with a
        // flat scale pulse.
        -220.0 + 38.0 * (t * GODZILLA_STEP_RATE * 0.5 + seed * 0.5).sin()
    }

    fn godzilla_leg_angles_at(t: f32, seed: f32) -> GodzillaLegPose {
        let phase = t * GODZILLA_STEP_RATE + seed;
        // Flatten the ends of the sine slightly so each foot appears planted for a
        // beat. The front leg lands a little after the rear leg leaves, and their
        // unequal harmonics keep the gait from looking like a mirrored pendulum.
        let planted = |p: f32| (p.sin() * 1.35).tanh() / 1.35f32.tanh();
        let upper = [
            0.112 * planted(phase) + 0.011 * (phase * 2.0 - 0.45).sin(),
            0.101 * planted(phase + std::f32::consts::PI + 0.16)
                + 0.009 * (phase * 2.0 + 0.80).sin(),
        ];
        let rear_lift = phase.sin().max(0.0).powf(1.35);
        let front_lift = (phase + std::f32::consts::PI + 0.16)
            .sin()
            .max(0.0)
            .powf(1.35);
        GodzillaLegPose {
            upper,
            // Counter-rotation keeps a planted shin comparatively vertical; the
            // extra positive bend arrives only while that foot is being carried.
            lower: [
                -upper[0] * 0.70 + 0.205 * rear_lift,
                -upper[1] * 0.68 + 0.182 * front_lift,
            ],
        }
    }

    fn marshmallow_pose_at(t: f32, seed: f32) -> MarshmallowPose {
        let phase = t * MARSHMALLOW_STEP_RATE + seed;
        let planted = |p: f32| (p.sin() * 1.28).tanh() / 1.28f32.tanh();
        let step = phase.sin();
        let impact = phase.cos().abs().powf(3.0);
        let rear_lift = phase.sin().max(0.0).powf(1.25);
        let front_lift = (phase + std::f32::consts::PI).sin().max(0.0).powf(1.25);
        let upper_leg = [
            0.30 * planted(phase),
            0.30 * planted(phase + std::f32::consts::PI),
        ];
        let smooth = |p: f32| {
            let p = p.clamp(0.0, 1.0);
            p * p * (3.0 - 2.0 * p)
        };
        // Roughly once per crossing interval he eases his shoulders toward us,
        // holds long enough for the neckerchief and expression to register, then
        // returns to profile without interrupting the walk.
        let look_phase = (t + seed * 0.83).rem_euclid(MARSHMALLOW_LOOK_CYCLE);
        let look = smooth((look_phase - 4.8) / 1.35) * (1.0 - smooth((look_phase - 9.0) / 1.45));
        MarshmallowPose {
            bob: 0.0065 * impact,
            roll: 0.024 * step + 0.006 * (phase * 0.5).sin(),
            depth: -220.0 + 26.0 * (phase * 0.5 + seed * 0.35).sin(),
            // Horizontal travel peaks while one planted leg swings behind him and
            // eases at foot exchange, keeping the huge feet from skating.
            speed: 0.54 + 0.25 * phase.cos().abs().powf(0.82),
            body_front: 0.70 * look,
            head_front: 0.90 * look,
            upper_leg,
            lower_leg: [
                -0.43 * upper_leg[0] + 0.25 * rear_lift,
                -0.43 * upper_leg[1] + 0.25 * front_lift,
            ],
            arm: [
                -0.27 * planted(phase),
                -0.27 * planted(phase + std::f32::consts::PI),
            ],
        }
    }

    fn marshmallow_foot_reach(upper_angle: f32) -> f32 {
        // The previous signum-based toe offset jumped from -0.025 to +0.025 at
        // mid-stride: more than 40 world pixels at giant scale. Carry the foot
        // smoothly through the exchange instead.
        0.055 + 0.025 * (upper_angle / 0.30).clamp(-1.0, 1.0)
    }

    fn tail_flick_amount_at(crossing: &DistantCrossing) -> f32 {
        let Some(started) = crossing.tail_flick_started else {
            return 0.0;
        };
        let age = (crossing.t - started).max(0.0);
        let smooth = |p: f32| {
            let p = p.clamp(0.0, 1.0);
            p * p * (3.0 - 2.0 * p)
        };
        if age < GODZILLA_TAIL_AWAY_SECS {
            smooth(age / GODZILLA_TAIL_AWAY_SECS)
        } else if age < GODZILLA_TAIL_AWAY_SECS + GODZILLA_TAIL_HOLD_SECS {
            1.0
        } else if age < GODZILLA_TAIL_FLICK_SECS {
            1.0 - smooth(
                (age - GODZILLA_TAIL_AWAY_SECS - GODZILLA_TAIL_HOLD_SECS)
                    / GODZILLA_TAIL_RETURN_SECS,
            )
        } else {
            0.0
        }
    }

    fn update(&mut self, ctx: &CritterCtx) {
        if let Some(crossing) = self.crossing.as_mut() {
            let pose = match crossing.kind {
                DistantKind::Tux => {
                    Self::tux_pose_at(crossing.t * DISTANT_TUX_TIME_SCALE, crossing.seed)
                }
                DistantKind::Godzilla => Self::godzilla_pose_at(crossing.t, crossing.seed),
                DistantKind::Marshmallow => {
                    let pose = Self::marshmallow_pose_at(crossing.t, crossing.seed);
                    SpritePose {
                        y: pose.bob,
                        x_jitter: 0.0,
                        rot: pose.roll,
                        scale_x: 1.0,
                        scale_y: 1.0,
                        speed: pose.speed,
                    }
                }
            };
            crossing.x += crossing.direction * DISTANT_WALK_SPEED * pose.speed * ctx.dt;
            crossing.t += ctx.dt;
            if crossing.kind == DistantKind::Godzilla {
                match crossing.tail_flick_started {
                    Some(started) if crossing.t - started >= GODZILLA_TAIL_FLICK_SECS => {
                        crossing.tail_flick_started = None;
                        crossing.tail_next_flick_at = crossing.t
                            + GODZILLA_TAIL_FLICK_MIN
                            + self.rng.f32() * GODZILLA_TAIL_FLICK_SPAN;
                    }
                    None if crossing.t >= crossing.tail_next_flick_at => {
                        crossing.tail_flick_started = Some(crossing.t);
                    }
                    _ => {}
                }
            }
            let finished = if crossing.direction > 0.0 {
                crossing.x >= crossing.to_x
            } else {
                crossing.x <= crossing.to_x
            };
            if finished {
                self.crossing = None;
                self.cooldown = self.rest();
            }
        } else {
            self.cooldown -= ctx.dt;
            if self.cooldown <= 0.0 && self.enabled {
                self.start_crossing(&ctx.life);
            }
        }
    }

    fn props(&self, out: &mut PropSink) {
        let Some(crossing) = self.crossing.as_ref() else {
            return;
        };
        match crossing.kind {
            DistantKind::Tux => {
                let pose = Self::tux_pose_at(crossing.t * DISTANT_TUX_TIME_SCALE, crossing.seed);
                let scaled_h = DISTANT_TUX_H * pose.scale_y;
                // Compensating by half the height change pins the feet to the baseline
                // through the squash and stretch.
                let y = crossing.ground_y
                    + pose.y * DISTANT_TUX_MOTION_SCALE
                    + (scaled_h - DISTANT_TUX_H) * 0.5;
                out.push(
                    MODEL_PENGUIN,
                    Prop::stretched(
                        [
                            crossing.x + pose.x_jitter * DISTANT_TUX_MOTION_SCALE,
                            y,
                            -220.0,
                        ],
                        [
                            DISTANT_TUX_W * pose.scale_x * crossing.direction,
                            DISTANT_TUX_W * pose.scale_y,
                            1.0,
                        ],
                        [0.0, 0.0, pose.rot * crossing.direction],
                    )
                    .tinted([1.0, 1.0, 1.0], DISTANT_TUX_ALPHA),
                );
            }
            DistantKind::Godzilla => {
                let pose = Self::godzilla_pose_at(crossing.t, crossing.seed);
                let motion_scale = DISTANT_GODZILLA_W / 85.0;
                let scaled_h = DISTANT_GODZILLA_H * pose.scale_y;
                let body_rot = pose.rot * crossing.direction;
                let body_pos = [
                    crossing.x + pose.x_jitter * motion_scale,
                    crossing.ground_y
                        + pose.y * motion_scale
                        + (scaled_h - DISTANT_GODZILLA_H) * 0.5,
                    Self::godzilla_depth_at(crossing.t, crossing.seed),
                ];
                let scale = [
                    DISTANT_GODZILLA_W * pose.scale_x * crossing.direction,
                    DISTANT_GODZILLA_W * pose.scale_y,
                    1.0,
                ];
                let (sin_body, cos_body) = body_rot.sin_cos();
                let legs = Self::godzilla_leg_angles_at(crossing.t, crossing.seed);

                // Build a two-joint hierarchy for each leg. The hip follows the
                // swaying body, while the knee follows the rotated thigh; the shin
                // then receives its own delayed flex around that moving knee.
                for leg in 0..2 {
                    let upper_slot = leg * 2;
                    let lower_slot = upper_slot + 1;
                    let upper_model = [MODEL_GODZILLA_REAR_UPPER, MODEL_GODZILLA_FRONT_UPPER][leg];
                    let lower_model = [MODEL_GODZILLA_REAR_LOWER, MODEL_GODZILLA_FRONT_LOWER][leg];
                    let hip = GODZILLA_LEG_PIVOTS[upper_slot];
                    let knee = GODZILLA_LEG_PIVOTS[lower_slot];
                    let hip_x = (hip[0] - GODZILLA_SOURCE_W * 0.5) / GODZILLA_SOURCE_W * scale[0];
                    let hip_y = (GODZILLA_SOURCE_H * 0.5 - hip[1]) / GODZILLA_SOURCE_W * scale[1];
                    let hip_pos = [
                        body_pos[0] + hip_x * cos_body - hip_y * sin_body,
                        body_pos[1] + hip_x * sin_body + hip_y * cos_body,
                        body_pos[2],
                    ];
                    let upper_rot = body_rot + legs.upper[leg] * crossing.direction;
                    let (sin_upper, cos_upper) = upper_rot.sin_cos();
                    let knee_x = (knee[0] - hip[0]) / GODZILLA_SOURCE_W * scale[0];
                    let knee_y = (hip[1] - knee[1]) / GODZILLA_SOURCE_W * scale[1];
                    let knee_pos = [
                        hip_pos[0] + knee_x * cos_upper - knee_y * sin_upper,
                        hip_pos[1] + knee_x * sin_upper + knee_y * cos_upper,
                        body_pos[2],
                    ];
                    let lower_rot = upper_rot + legs.lower[leg] * crossing.direction;

                    out.push(
                        lower_model,
                        Prop::stretched(knee_pos, scale, [0.0, 0.0, lower_rot])
                            .tinted([1.0, 1.0, 1.0], DISTANT_GODZILLA_ALPHA),
                    );
                    out.push(
                        upper_model,
                        Prop::stretched(hip_pos, scale, [0.0, 0.0, upper_rot])
                            .tinted([1.0, 1.0, 1.0], DISTANT_GODZILLA_ALPHA),
                    );
                }

                // The tail tip is anchored at the source cut and pitched around Y.
                // Because the local tail extends opposite the travel direction, this
                // signed pitch always sends its distal pixels away along -Z while the
                // joint itself remains pinned. A simultaneous fade sells the brief
                // disappearance into the distant fog.
                let tail_x = (GODZILLA_TAIL_PIVOT[0] - GODZILLA_SOURCE_W * 0.5) / GODZILLA_SOURCE_W
                    * scale[0];
                let tail_y = (GODZILLA_SOURCE_H * 0.5 - GODZILLA_TAIL_PIVOT[1]) / GODZILLA_SOURCE_W
                    * scale[1];
                let tail_pos = [
                    body_pos[0] + tail_x * cos_body - tail_y * sin_body,
                    body_pos[1] + tail_x * sin_body + tail_y * cos_body,
                    body_pos[2],
                ];
                let tail_away = Self::tail_flick_amount_at(crossing);
                let tail_pitch = -0.78 * tail_away * crossing.direction;
                let tail_alpha = DISTANT_GODZILLA_ALPHA * (1.0 - 0.68 * tail_away);
                out.push(
                    MODEL_GODZILLA_TAIL,
                    Prop::stretched(tail_pos, scale, [0.0, tail_pitch, body_rot])
                        .tinted([1.0, 1.0, 1.0], tail_alpha),
                );
                out.push(
                    MODEL_GODZILLA_BODY,
                    Prop::stretched(body_pos, scale, [0.0, 0.0, body_rot])
                        .tinted([1.0, 1.0, 1.0], DISTANT_GODZILLA_ALPHA),
                );
            }
            DistantKind::Marshmallow => {
                let pose = Self::marshmallow_pose_at(crossing.t, crossing.seed);
                let root_yaw =
                    crossing.direction * std::f32::consts::FRAC_PI_2 * (1.0 - pose.body_front);
                let head_yaw =
                    crossing.direction * std::f32::consts::FRAC_PI_2 * (1.0 - pose.head_front);
                let scale = DISTANT_MARSHMALLOW_H;
                let root = [
                    crossing.x,
                    crossing.ground_y - DISTANT_MARSHMALLOW_H * 0.5 + pose.bob * scale,
                    pose.depth,
                ];
                let (sy, cy) = root_yaw.sin_cos();
                let world_point = |local: [f32; 3]| {
                    [
                        root[0] + scale * (local[0] * cy + local[2] * sy),
                        root[1] + scale * local[1],
                        root[2] + scale * (-local[0] * sy + local[2] * cy),
                    ]
                };
                let alpha = DISTANT_MARSHMALLOW_ALPHA;

                // Legs are two-joint chains swinging in the local front/back plane.
                // After the root's Y turn that plane becomes horizontal screen
                // travel, giving a true side-view stride rather than flat leg rolls.
                for leg in 0..2 {
                    let side = if leg == 0 { -1.0 } else { 1.0 };
                    let hip = [0.112 * side, 0.335, 0.0];
                    let upper_angle = pose.upper_leg[leg];
                    let upper_len = 0.165;
                    let knee = [
                        hip[0],
                        hip[1] - upper_len * upper_angle.cos(),
                        hip[2] - upper_len * upper_angle.sin(),
                    ];
                    let lower_angle = upper_angle + pose.lower_leg[leg];
                    let lower_len = 0.145;
                    let ankle = [
                        knee[0],
                        knee[1] - lower_len * lower_angle.cos(),
                        knee[2] - lower_len * lower_angle.sin(),
                    ];
                    for (center, radii, angle) in [
                        (
                            [hip[0], (hip[1] + knee[1]) * 0.5, (hip[2] + knee[2]) * 0.5],
                            [0.092, upper_len * 0.57, 0.088],
                            upper_angle,
                        ),
                        (
                            [
                                knee[0],
                                (knee[1] + ankle[1]) * 0.5,
                                (knee[2] + ankle[2]) * 0.5,
                            ],
                            [0.087, lower_len * 0.57, 0.083],
                            lower_angle,
                        ),
                    ] {
                        out.push(
                            MODEL_MARSHMALLOW_PUFF,
                            Prop::stretched(
                                world_point(center),
                                [radii[0] * scale, radii[1] * scale, radii[2] * scale],
                                [angle, root_yaw, pose.roll * crossing.direction],
                            )
                            .tinted([1.0, 1.0, 1.0], alpha),
                        );
                    }
                    let foot_center = [
                        ankle[0],
                        ankle[1] - 0.008,
                        ankle[2] + Self::marshmallow_foot_reach(upper_angle),
                    ];
                    out.push(
                        MODEL_MARSHMALLOW_PUFF,
                        Prop::stretched(
                            world_point(foot_center),
                            [0.105 * scale, 0.060 * scale, 0.135 * scale],
                            [0.10, root_yaw, pose.roll * crossing.direction],
                        )
                        .tinted([1.0, 1.0, 1.0], alpha),
                    );
                }

                // Counter-swinging arms use the same puff mesh, with spherical hands
                // that stay visibly separate from the belly during the camera turn.
                for arm in 0..2 {
                    let side = if arm == 0 { -1.0 } else { 1.0 };
                    let shoulder = [0.272 * side, 0.665, 0.0];
                    let upper_angle = pose.arm[arm];
                    let upper_len = 0.165;
                    let elbow = [
                        shoulder[0],
                        shoulder[1] - upper_len * upper_angle.cos(),
                        shoulder[2] - upper_len * upper_angle.sin(),
                    ];
                    let fore_angle = upper_angle * 0.48 - 0.10;
                    let fore_len = 0.145;
                    let hand = [
                        elbow[0],
                        elbow[1] - fore_len * fore_angle.cos(),
                        elbow[2] - fore_len * fore_angle.sin(),
                    ];
                    for (center, radii, angle) in [
                        (
                            [
                                shoulder[0],
                                (shoulder[1] + elbow[1]) * 0.5,
                                (shoulder[2] + elbow[2]) * 0.5,
                            ],
                            [0.078, upper_len * 0.56, 0.076],
                            upper_angle,
                        ),
                        (
                            [
                                elbow[0],
                                (elbow[1] + hand[1]) * 0.5,
                                (elbow[2] + hand[2]) * 0.5,
                            ],
                            [0.070, fore_len * 0.56, 0.068],
                            fore_angle,
                        ),
                    ] {
                        out.push(
                            MODEL_MARSHMALLOW_PUFF,
                            Prop::stretched(
                                world_point(center),
                                [radii[0] * scale, radii[1] * scale, radii[2] * scale],
                                [angle, root_yaw, pose.roll * crossing.direction],
                            )
                            .tinted([1.0, 1.0, 1.0], alpha),
                        );
                    }
                    out.push(
                        MODEL_MARSHMALLOW_PUFF,
                        Prop::stretched(
                            world_point(hand),
                            [0.076 * scale, 0.076 * scale, 0.072 * scale],
                            [0.0, root_yaw, pose.roll * crossing.direction],
                        )
                        .tinted([1.0, 1.0, 1.0], alpha),
                    );
                }

                out.push(
                    MODEL_MARSHMALLOW_BODY,
                    Prop::new(root, scale, [0.0, root_yaw, pose.roll * crossing.direction])
                        .tinted([1.0, 1.0, 1.0], alpha),
                );
                let head_anchor = world_point([0.0, MARSHMALLOW_HEAD_Y, 0.0]);
                out.push(
                    MODEL_MARSHMALLOW_HEAD,
                    Prop::new(
                        head_anchor,
                        scale,
                        [0.0, head_yaw, pose.roll * crossing.direction * 0.55],
                    )
                    .tinted([1.0, 1.0, 1.0], alpha),
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Foreground fog — page-coloured mist between the distant giant and the scene
// ---------------------------------------------------------------------------

const FOG_ATLAS_CELL_COUNT: usize = 8;
/// A dense field reuses the eight atlas silhouettes with independent positions,
/// scales, headings, and breathing phases.
const FOG_BLOB_COUNT: usize = 60;
const FOG_SCALE: f32 = 1.30;
const FOG_ALPHA: f32 = 0.45;
const FOG_Z: f32 = -190.0;

#[derive(Clone, Copy, Debug)]
struct FogBlob {
    pos: [f32; 2],
    velocity: [f32; 2],
    size: [f32; 2],
    phase: f32,
    phase_rate: f32,
    rotation: f32,
}

struct FogLayer {
    blobs: Vec<FogBlob>,
    half_w: f32,
    half_h: f32,
}

impl FogLayer {
    fn new(view: &LifeView, seed: u64) -> FogLayer {
        let mut rng = Rng::new(seed);
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let half_h = view.rows() as f32 * CELL_PX * 0.5;
        let mut blobs = Vec::with_capacity(FOG_BLOB_COUNT);
        for _ in 0..FOG_BLOB_COUNT {
            let heading = rng.f32() * std::f32::consts::TAU;
            let speed = 2.8 + rng.f32() * 6.2;
            let width = (330.0 + rng.f32() * 360.0) * FOG_SCALE;
            blobs.push(FogBlob {
                pos: [
                    -half_w + rng.f32() * half_w * 2.0,
                    -half_h + rng.f32() * half_h * 2.0,
                ],
                velocity: [heading.cos() * speed, heading.sin() * speed],
                size: [width, width * (0.52 + rng.f32() * 0.38)],
                phase: rng.f32() * std::f32::consts::TAU,
                phase_rate: 0.08 + rng.f32() * 0.13,
                rotation: (rng.f32() - 0.5) * 0.24,
            });
        }
        FogLayer {
            blobs,
            half_w,
            half_h,
        }
    }

    fn update(&mut self, dt: f32) {
        for blob in &mut self.blobs {
            blob.phase += blob.phase_rate * dt;
            blob.pos[0] += (blob.velocity[0] + 1.7 * (blob.phase * 0.73).sin()) * dt;
            blob.pos[1] += (blob.velocity[1] + 1.3 * (blob.phase * 0.57).cos()) * dt;
            let pad = blob.size[0].max(blob.size[1]) * 0.56;
            if blob.pos[0] < -self.half_w - pad {
                blob.pos[0] = self.half_w + pad;
            } else if blob.pos[0] > self.half_w + pad {
                blob.pos[0] = -self.half_w - pad;
            }
            if blob.pos[1] < -self.half_h - pad {
                blob.pos[1] = self.half_h + pad;
            } else if blob.pos[1] > self.half_h + pad {
                blob.pos[1] = -self.half_h - pad;
            }
        }
    }

    fn props(&self, out: &mut PropSink) {
        let page_gray = srgb_hex_to_linear(BG);
        for (slot, blob) in self.blobs.iter().enumerate() {
            let breathe = 1.0 + 0.035 * blob.phase.sin();
            let position = [
                blob.pos[0] + 9.0 * (blob.phase * 0.41).sin(),
                blob.pos[1] + 7.0 * (blob.phase * 0.37).cos(),
                FOG_Z,
            ];
            out.push(
                MODEL_FOG_FIRST + slot % FOG_ATLAS_CELL_COUNT,
                Prop::stretched(
                    position,
                    [blob.size[0] * breathe, blob.size[1] * (2.0 - breathe), 1.0],
                    [0.0, 0.0, blob.rotation + 0.025 * (blob.phase * 0.31).sin()],
                )
                .tinted(page_gray, FOG_ALPHA),
            );
        }
    }
}

// ---------------------------------------------------------------------------
// The walker — a stick figure that drops onto the field and stands on it
// ---------------------------------------------------------------------------

/// Overall standing height, a little more than a tile is wide.
const WALKER_H: f32 = 82.0;
const WALKER_LINE: f32 = 3.4;
/// Segment lengths, as fractions of the standing height.
const WALKER_TORSO: f32 = 0.32 * WALKER_H;
const WALKER_SHOULDER: f32 = 0.095 * WALKER_H;
const WALKER_UPPER_ARM: f32 = 0.185 * WALKER_H;
const WALKER_FOREARM: f32 = 0.17 * WALKER_H;
const WALKER_THIGH: f32 = 0.205 * WALKER_H;
const WALKER_SHIN: f32 = 0.205 * WALKER_H;
/// The mask is intentionally absurdly larger than the old head. Its bottom overlaps
/// the upper torso, hiding any neck and making it feel worn rather than balanced there.
const WALKER_MASK_W: f32 = 34.0 * 0.80 * 1.10;
const WALKER_MASK_H: f32 = WALKER_MASK_W * (512.0 / 223.0) * 0.90;
const WALKER_MASK_CHEST_OVERLAP: f32 = WALKER_TORSO * 0.30;
const WALKER_HELMET_W: f32 = 46.2;
const WALKER_HELMET_H: f32 = WALKER_HELMET_W;
const WALKER_HELMET_CHEST_OVERLAP: f32 = WALKER_TORSO * 0.25;
const WALKER_HELMET_LIFT: f32 = WALKER_HELMET_H * 0.05;
const WALKER_NOH_W: f32 = 40.0;
const WALKER_NOH_H: f32 = WALKER_NOH_W * (384.0 / 290.0) * 0.95;
const WALKER_NOH_CHEST_OVERLAP: f32 = WALKER_TORSO * 0.28;
const WALKER_MONKEY_W: f32 = 44.0;
const WALKER_MONKEY_H: f32 = WALKER_MONKEY_W * (373.0 / 384.0);
const WALKER_MONKEY_CHEST_OVERLAP: f32 = WALKER_TORSO * 0.25;
const WALKER_GREEK_W: f32 = 42.0;
const WALKER_GREEK_H: f32 = WALKER_GREEK_W * (384.0 / 293.0) * 0.95;
const WALKER_GREEK_CHEST_OVERLAP: f32 = WALKER_TORSO * 0.28;
const WALKER_MASKED_H: f32 =
    WALKER_THIGH + WALKER_SHIN + WALKER_TORSO + WALKER_MASK_H - WALKER_MASK_CHEST_OVERLAP;
/// Ledge physics still belong to the slim side-view body beneath the costume. Letting
/// the decorative mask widen this clearance would erase otherwise natural catches.
const WALKER_GRAB_BODY_CLEARANCE: f32 = 0.105 * WALKER_H;

const WALKER_GRAVITY: f32 = 1450.0;
/// Just clear of the tile tops, so he reads as standing on them.
const WALKER_Z: f32 = 34.0;
/// The page's own content sits down the middle of the screen, so he keeps to the flanks.
/// Half-width of the strip to leave alone, in pixels; on a window too narrow to have any
/// flank left he falls back to wherever he can be seen at all.
const WALKER_KEEP_CLEAR: f32 = 470.0;
/// How long the deep part of a landing is held before he starts to straighten.
const WALKER_ABSORB: f32 = 0.09;
/// How fast the pose chases its target, per activity. Impact is nearly instant; standing
/// back up is unhurried, which is what gives the limbs their follow-through.
const WALKER_EASE_FALL: f32 = 9.0;
const WALKER_EASE_ABSORB: f32 = 26.0;
const WALKER_EASE_RECOVER: f32 = 7.5;
const WALKER_EASE_RUN: f32 = 16.0;

/// Fall far enough and the gesture goes fully wide. Shorter drops get a milder version,
/// so a step down does not look like a plummet.
const WALKER_DRAMA_DROP: f32 = 3.2 * CELL_PX;

const WALKER_RUN_SPEED: f32 = 168.0;
const WALKER_RUN_CADENCE: f32 = 11.0;
/// A short corrective step is deliberately slower than a run. It is used when a tile
/// grows into his standing space, and is visible motion rather than a grid snap.
const WALKER_STEP_SPEED: f32 = 118.0;
/// Upward kick of a real jump, and of the hopeless little hop he manages when boxed in.
const WALKER_JUMP_V: f32 = 600.0;
const WALKER_HOP_V: f32 = 320.0;
/// Beats spent standing still before picking something to do, and how long each of the
/// stuck behaviours runs for.
const WALKER_DECIDE: f32 = 0.5;
const WALKER_SHOVE_SECS: f32 = 1.45;
/// Not every suitable jump becomes a ledge routine, but catches are decided before
/// take-off. The horizontal tolerances are intentionally tight: reaching the right
/// height is not enough if the jump did not also carry his body naturally to the face.
const WALKER_PLAN_GRAB_CHANCE: f32 = 0.62;
const WALKER_GRAB_X_TOLERANCE: f32 = 4.0;
const WALKER_GRAB_MIN_SPEED: f32 = WALKER_RUN_SPEED * 0.68;
const WALKER_GRAB_MAX_SPEED: f32 = WALKER_RUN_SPEED * 1.18;
/// A foot-first jump may vary the carried speed more widely than a ledge catch. At
/// equal height these bounds naturally cover landings one to three cells away.
const WALKER_LAND_MIN_SPEED: f32 = 72.0;
const WALKER_LAND_MAX_SPEED: f32 = 240.0;
/// Some hangs end in a deliberate throw. The upward kick is just under a running jump,
/// leaving room to redirect away from the held face without looking superhuman.
const WALKER_HANG_THROW_CHANCE: f32 = 0.48;
const WALKER_THROW_V: f32 = 560.0;
const WALKER_THROW_MIN_SPEED: f32 = 72.0;
const WALKER_THROW_MAX_SPEED: f32 = 238.0;
/// A foot may land a little off-centre, then visibly settle during the impact. Wider
/// corner contacts are allowed to keep falling instead of becoming awkward perches.
const WALKER_LAND_HALF_WIDTH: f32 = CELL_PX * 0.27;
const WALKER_SETTLE_SPEED: f32 = 86.0;
const WALKER_CROUCH_SECS: f32 = 0.24;
/// Cells of runway he will look along before committing to a run.
const WALKER_MAX_RUNWAY: isize = 5;
/// Generations ahead a cell has to keep its state before he will plan around it. One is
/// already several seconds of wall time — far longer than any of his actions — and asking
/// for more only makes him refuse to do anything.
const WALKER_PLAN_GENS: usize = 1;

/// Every joint angle, measured from straight down and positive toward +X, so zero is a
/// limb hanging vertically. Index 0 and 1 are the figure's two sides.
///
/// Arms hang off the torso and so are measured relative to `lean`; legs are measured from
/// vertical and ignore it. That split matters: with the legs following the lean he could
/// not bend over without his feet swinging out from under him, which rules out any pose
/// where he braces and pushes.
#[derive(Clone, Copy)]
struct Pose {
    lean: f32,
    shoulder: [f32; 2],
    elbow: [f32; 2],
    hip: [f32; 2],
    knee: [f32; 2],
}

impl Pose {
    fn standing() -> Pose {
        Pose {
            lean: 0.0,
            shoulder: [0.20, -0.20],
            elbow: [0.18, -0.18],
            hip: [0.06, -0.06],
            knee: [0.0, 0.0],
        }
    }

    /// Arms up and out, thighs apart, shins tucked back under. `spread` runs 0 to 1 with
    /// how far he has dropped, so a long fall throws the whole gesture wide open.
    fn falling(spread: f32) -> Pose {
        let s = spread.clamp(0.0, 1.0);
        let m = |a: f32, b: f32| a + (b - a) * s;
        Pose {
            lean: m(0.04, 0.10),
            shoulder: [m(1.85, 2.75), m(-1.85, -2.75)],
            elbow: [m(-0.35, -0.75), m(0.35, 0.75)],
            // Deliberately asymmetric: a perfectly mirrored fall looks mechanical.
            hip: [m(0.35, 0.85), m(-0.22, -0.62)],
            knee: [m(-0.55, -1.05), m(0.40, 0.85)],
        }
    }

    /// A jump is a controlled fall: tucked rather than flung.
    fn airborne_tuck() -> Pose {
        Pose {
            lean: 0.10,
            shoulder: [1.55, -1.35],
            elbow: [-0.75, 0.55],
            hip: [0.75, 0.30],
            knee: [-1.0, -0.75],
        }
    }

    /// Deep absorbing squat: thighs splayed wide with the shins left vertical. The body
    /// drops on its own, because the hip rides on however far the legs happen to reach.
    fn landed() -> Pose {
        Pose {
            lean: 0.20,
            shoulder: [1.35, -1.35],
            elbow: [0.55, -0.55],
            hip: [1.25, -1.25],
            knee: [-1.25, 1.25],
        }
    }

    /// Braced against the tile to his right, both arms out into it, back leg driving.
    fn shoving(effort: f32) -> Pose {
        Pose {
            lean: 0.42 + effort * 0.10,
            shoulder: [1.45 + effort * 0.14, 1.25 + effort * 0.14],
            elbow: [0.10, 0.22],
            hip: [0.62, -0.70],
            knee: [-0.42, 0.30],
        }
    }

    /// Bent right over with everything he has into it, and moving nothing at all. The
    /// front foot planted, the back leg driving out behind, arms locked straight into the
    /// tile — all of it and no result, which is the joke.
    fn straining() -> Pose {
        Pose {
            lean: 1.15,
            // Relative to a torso already pitched over, so these come out just below
            // horizontal: shoving into the face of the tile rather than down at it.
            shoulder: [0.60, 0.48],
            elbow: [0.05, 0.12],
            hip: [0.25, -0.85],
            knee: [0.40, 0.10],
        }
    }

    /// Dangling from a ledge by both hands, arms overhead in a narrow V.
    fn hanging() -> Pose {
        Pose {
            lean: 0.0,
            shoulder: [2.95, -2.95],
            elbow: [-0.25, 0.25],
            hip: [0.10, -0.12],
            knee: [0.25, -0.20],
        }
    }

    /// Arms remain locked to the ledge while the torso and legs swing out to preload a
    /// throw. `amount` rises only over the last beat before release.
    fn hanging_throw(away: f32, amount: f32) -> Pose {
        let k = amount.clamp(0.0, 1.0);
        let mut p = Pose::hanging();
        p.lean = 0.24 * k;
        p.hip = [0.10 + 0.78 * k, -0.12 + 0.36 * k];
        p.knee = [0.25 - 1.12 * k, -0.20 - 0.52 * k];
        p.facing(away)
    }

    /// One stride of a run. `p` is the cycle phase; the legs alternate, the arms
    /// counter-swing, and the knee folds hardest as its leg swings through.
    fn running(p: f32) -> Pose {
        let pi = std::f32::consts::PI;
        Pose {
            lean: 0.22,
            shoulder: [0.55 * (p + pi).sin(), 0.55 * p.sin() - 0.10],
            elbow: [-0.95, 0.95],
            hip: [0.68 * p.sin(), 0.68 * (p + pi).sin()],
            knee: [
                -0.55 - 0.45 * (p + 1.1).sin(),
                -0.55 - 0.45 * (p + 1.1 + pi).sin(),
            ],
        }
    }

    /// The same pose facing the other way. Mirroring by negating every angle and swapping
    /// the two sides means each pose only has to be authored once, for +X.
    fn mirrored(self) -> Pose {
        Pose {
            lean: -self.lean,
            shoulder: [-self.shoulder[1], -self.shoulder[0]],
            elbow: [-self.elbow[1], -self.elbow[0]],
            hip: [-self.hip[1], -self.hip[0]],
            knee: [-self.knee[1], -self.knee[0]],
        }
    }

    fn facing(self, dir: f32) -> Pose {
        if dir < 0.0 {
            self.mirrored()
        } else {
            self
        }
    }

    /// Exponential approach, which is what makes the limbs move nicely: every joint
    /// eases rather than tracking, so they arrive slightly apart and the fast joints
    /// lead the slow ones instead of the whole body snapping between keyframes.
    fn approach(&mut self, to: &Pose, k: f32) {
        let f = |a: &mut f32, b: f32| *a += (b - *a) * k;
        f(&mut self.lean, to.lean);
        for i in 0..2 {
            f(&mut self.shoulder[i], to.shoulder[i]);
            f(&mut self.elbow[i], to.elbow[i]);
            f(&mut self.hip[i], to.hip[i]);
            f(&mut self.knee[i], to.knee[i]);
        }
    }
}

/// Unit vector pointing down-ish: zero is straight down, positive tilts toward +X.
fn down(a: f32) -> [f32; 2] {
    let (s, c) = a.sin_cos();
    [s, -c]
}

/// Unit vector pointing up-ish, same convention.
fn up(a: f32) -> [f32; 2] {
    let (s, c) = a.sin_cos();
    [s, c]
}

/// What the walker is doing. Each variant exists because there is an animation for it —
/// the point of reading the board ahead is to only ever put him into one of these.
#[derive(Clone, Copy, PartialEq, Debug)]
enum Act {
    /// Off the ground, whether dropped or jumped. `vx` carries any run speed with him.
    Airborne,
    /// Absorbing an impact.
    Landing(f32),
    /// Stood still, sizing up the board.
    Deciding(f32),
    /// Boxed in: a crouch and a hopeless little hop that gets him nowhere.
    Hop(f32),
    /// A standing leap toward a reachable landing when there is no useful runway.
    Leap { dir: f32, t: f32 },
    /// Boxed in: braced against a neighbour, shoving.
    Shove { dir: f32, t: f32 },
    /// A visible one-cell correction onto a clear, supported centre.
    Sidestep { dir: f32, to_x: f32 },
    /// Running along a surface toward a jump.
    Run { dir: f32, t: f32, take_off: f32 },
    /// Hanging by his hands from the top edge of a tile, until it stops being there.
    Hang { col: isize, row: isize },
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Headgear {
    Tiki,
    Astronaut,
    Noh,
    Monkey,
    ComedyTragedy,
}

fn random_headgear(rng: &mut Rng) -> Headgear {
    match rng.below(5) {
        0 => Headgear::Tiki,
        1 => Headgear::Astronaut,
        2 => Headgear::Noh,
        3 => Headgear::Monkey,
        _ => Headgear::ComedyTragedy,
    }
}

/// A ledge catch selected at take-off whose ballistic path has already been checked.
///
/// `hip_x` is where the body will be when the raised hands descend through `ledge_y`.
/// The catch is abandoned unless both crossings still coincide within a few pixels, so
/// this is permission to reach, never permission to teleport.
#[derive(Clone, Copy, Debug)]
struct GrabPlan {
    col: isize,
    row: isize,
    hip_x: f32,
    ledge_y: f32,
}

/// A stable exposed tile selected before a jump, reached only by choosing a natural
/// launch speed. The ordinary falling collision still has to complete the landing:
/// this is a destination, not permission to snap onto it.
#[derive(Clone, Copy, Debug)]
struct LandingPlan {
    col: isize,
    row: isize,
    vx: f32,
    score: f32,
}

/// A stick figure that falls into the field, lands on whatever tile is beneath him, and
/// then picks something to do about where he has ended up.
///
/// Destinations and jump arcs are checked against both the present board and its known
/// next generation. Short runs use the present surface and are re-checked every frame,
/// so a generation landing mid-stride causes an immediate re-plan at the new tile face.
/// He only ever commits to situations there is an animation for.
pub struct Walker {
    /// Column he is over, and his world x.
    col: isize,
    x: f32,
    vx: f32,
    /// The hip is the body the physics acts on; the feet follow from the pose, and the
    /// legs' reach is what sets how high the hip sits when he is standing.
    hip_y: f32,
    vy: f32,
    /// Height he last left the ground at, for judging how dramatic a fall is.
    fell_from: f32,
    /// Edges of the *visible* area. He is only ever put somewhere he can be seen, and
    /// once any part of him drops below the bottom he is committed to falling out.
    vis_bottom: f32,
    edge: f32,
    /// Set once his feet pass below the screen: from then on nothing can catch him.
    doomed: bool,
    /// A catch planned at take-off. Ordinary drops and accidental knocks never get one.
    grab_plan: Option<GrabPlan>,
    /// The intended foot landing for a voluntary jump. Physics may still miss it if the
    /// board changes after take-off, but whenever this is present the original arc was
    /// checked all the way to a visible, stable tile.
    landing_plan: Option<LandingPlan>,
    /// A hard squeeze can send him behind the floor for a beat. During that interval the
    /// tile that displaced him cannot immediately re-catch him and start a hop loop.
    no_land_until: f32,
    /// Absolute walker time for a voluntary throw, or infinity when this hang waits for
    /// the ledge to fade.
    hang_release_at: f32,
    /// Which boxed-in routine was last used: false for hop, true for shove. Alternating
    /// after the first weighted choice keeps either idle from repeating indefinitely.
    last_stuck: Option<bool>,
    /// Chosen once at arrival; it changes only the costume, never the physics.
    headgear: Headgear,
    act: Act,
    pose: Pose,
    /// Row he is standing on, watched so he can fall when it goes away.
    support: Option<isize>,
    /// Which way he is facing, for the poses that are not symmetric.
    facing: f32,
    t: f32,
    seed: f32,
    rng: Rng,
}

impl Walker {
    /// Half the width he takes up with his arms out, for keeping him clear of the edges.
    fn reach() -> f32 {
        WALKER_H * 0.42
    }

    /// Picks a column he can drop into *and be seen doing it*, or gives up if the board
    /// does not currently offer one.
    ///
    /// Three things have to hold, and each of them was a way he looked wrong before:
    /// something has to be there to catch him, or he simply falls through the field; the
    /// catch has to be far enough below the top edge that he is fully on screen by the
    /// time he arrives, rather than stopping half out of shot; and it has to be above the
    /// bottom edge, or he lands with his legs off the screen.
    pub fn new(view: &LifeView, rng: &mut Rng) -> Option<Walker> {
        let half_h = view.rows() as f32 * CELL_PX * 0.5;
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let margin = MARGIN as f32 * CELL_PX;
        let vis_top = half_h - margin;
        let vis_bottom = -vis_top;
        let vis_right = half_w - margin;
        let half_tile = CELL_PX * TILE_FILL * 0.5;

        // Both flanks are fair game; the middle is only used if there is no flank left.
        let mut flanks: Vec<(isize, f32)> = Vec::new();
        let mut middle: Vec<(isize, f32)> = Vec::new();
        for col in MARGIN as isize..(view.cols() - MARGIN) as isize {
            let x = view.cell_center(col as f32, 0.0)[0];
            // Fully within the frame, arms and all.
            if x - Self::reach() < -vis_right || x + Self::reach() > vis_right {
                continue;
            }

            // The first thing that would stop him, which is the topmost live cell — not
            // the topmost *durable* one, since anything alive will catch him.
            let blocker = (0..view.rows() as isize).find(|&row| view.alive(col, row, 0));
            let Some(row) = blocker else { continue };

            // It has to still be there when he arrives, and for a moment after.
            if !(0..=WALKER_PLAN_GENS).all(|g| view.alive(col, row, g)) {
                continue;
            }

            let surface = view.cell_center(col as f32, row as f32)[1] + half_tile;
            let clear_of_top = surface <= vis_top - WALKER_MASKED_H;
            let clear_of_bottom = surface >= vis_bottom + WALKER_H * 0.15;
            if clear_of_top && clear_of_bottom {
                if x.abs() >= WALKER_KEEP_CLEAR {
                    flanks.push((col, x));
                } else {
                    middle.push((col, x));
                }
            }
        }
        let options = if flanks.is_empty() { middle } else { flanks };
        if options.is_empty() {
            return None;
        }

        let (col, x) = options[rng.below(options.len())];
        let headgear = random_headgear(rng);
        // Entering with his feet level with the top edge means the whole drop is on
        // screen, and the check above guarantees he has room to finish it.
        let hip_y = vis_top + WALKER_H * 0.55;
        Some(Walker {
            col,
            x,
            vx: 0.0,
            hip_y,
            vy: 0.0,
            fell_from: hip_y,
            vis_bottom,
            edge: half_w,
            doomed: false,
            grab_plan: None,
            landing_plan: None,
            no_land_until: 0.0,
            hang_release_at: f32::INFINITY,
            last_stuck: None,
            headgear,
            act: Act::Airborne,
            pose: Pose::falling(0.0),
            support: None,
            facing: 1.0,
            t: 0.0,
            seed: rng.f32() * 10.0,
            rng: Rng::new(rng.next_u64()),
        })
    }

    /// How far below the hip the lower foot reaches in the current pose. This is what
    /// makes the landing squash work: a deeper squat simply reaches less far, so the
    /// body sinks and rises without any of it being animated separately.
    fn leg_reach(&self) -> f32 {
        (0..2)
            .map(|i| {
                let thigh = self.pose.hip[i];
                let shin = thigh + self.pose.knee[i];
                WALKER_THIGH * thigh.cos() + WALKER_SHIN * shin.cos()
            })
            .fold(f32::MIN, f32::max)
    }

    fn foot_y(&self) -> f32 {
        self.hip_y - self.leg_reach()
    }

    /// Vertical distance from the hip up to the higher hand. The mirror of `leg_reach`,
    /// and it does the same job for hanging that that one does for standing: the hip rides
    /// wherever the arms happen to reach, so easing into the hanging pose settles him onto
    /// the ledge instead of needing the height animated.
    fn hand_rise_for(pose: &Pose) -> f32 {
        let lean = pose.lean;
        let torso = WALKER_TORSO * lean.cos();
        (0..2)
            .map(|i| {
                let sh = lean + pose.shoulder[i];
                let fore = sh + pose.elbow[i];
                torso - WALKER_UPPER_ARM * sh.cos() - WALKER_FOREARM * fore.cos()
            })
            .fold(f32::MIN, f32::max)
    }

    fn hand_rise(&self) -> f32 {
        Self::hand_rise_for(&self.pose)
    }

    fn cell_x(view: &LifeView, col: isize) -> f32 {
        view.cell_center(col as f32, 0.0)[0]
    }

    /// A landing owns a cell only near its middle. Merely rounding to the nearest column
    /// accepted positions almost half a pitch away and left him balanced on corners.
    fn centered_over(view: &LifeView, col: isize, x: f32) -> bool {
        (x - Self::cell_x(view, col)).abs() <= WALKER_LAND_HALF_WIDTH
    }

    /// Find a stable exposed ledge whose face and height coincide with a natural
    /// descending jump. The only adjustment is a modest choice of carried run speed;
    /// position and height are never corrected after take-off.
    fn plan_grab(&mut self, view: &LifeView, dir: f32) -> Option<(GrabPlan, f32)> {
        if self.rng.f32() >= WALKER_PLAN_GRAB_CHANCE {
            return None;
        }

        let half_tile = CELL_PX * TILE_FILL * 0.5;
        let hand_rise = Self::hand_rise_for(&Pose::hanging());
        let mut best: Option<(f32, GrabPlan, f32)> = None;

        for c in 0..view.cols() as isize {
            let tile_x = Self::cell_x(view, c);
            let along = (tile_x - self.x) * dir;
            if !(CELL_PX * 0.7..=CELL_PX * 6.2).contains(&along) {
                continue;
            }

            // Hang just outside the near face. In the final V-shaped arm pose the near
            // hand then reaches comfortably onto the top without the torso entering it.
            let hip_x = tile_x - dir * (half_tile + WALKER_GRAB_BODY_CLEARANCE * 1.05);
            let dx = hip_x - self.x;
            if dx * dir <= 0.0 {
                continue;
            }

            for row in 0..view.rows() as isize {
                let top = view.cell_center(c as f32, row as f32)[1] + half_tile;
                let body_col = c - dir as isize;
                let stable_ledge = (0..=WALKER_PLAN_GENS)
                    .all(|g| view.alive(c, row, g) && !view.alive(c, row - 1, g));
                let clear_body = (0..=WALKER_PLAN_GENS).all(|g| !view.alive(body_col, row, g));
                if top < self.vis_bottom || !stable_ledge || !clear_body {
                    continue;
                }

                // Solve y0 + jump_v*t - gravity*t^2/2 = top - raised hands, taking
                // the later root: the descending half of the jump.
                let target_hip_y = top - hand_rise;
                let dy = target_hip_y - self.hip_y;
                let disc = WALKER_JUMP_V * WALKER_JUMP_V - 2.0 * WALKER_GRAVITY * dy;
                if disc <= 0.0 {
                    continue;
                }
                let catch_t = (WALKER_JUMP_V + disc.sqrt()) / WALKER_GRAVITY;
                if !(0.42..=1.45).contains(&catch_t) {
                    continue;
                }
                let vx = dx / catch_t;
                if !(WALKER_GRAB_MIN_SPEED..=WALKER_GRAB_MAX_SPEED).contains(&vx.abs()) {
                    continue;
                }

                // Prefer the least altered running speed, then the nearer performance.
                let score = (vx.abs() - WALKER_RUN_SPEED).abs() + catch_t * 2.0;
                let plan = GrabPlan {
                    col: c,
                    row,
                    hip_x,
                    ledge_y: top,
                };
                if best.is_none_or(|(old, _, _)| score < old) {
                    best = Some((score, plan, vx));
                }
            }
        }
        best.map(|(_, plan, vx)| (plan, vx))
    }

    /// Check the whole jump rather than just its destination. Tiles that are alive now
    /// or arrive next generation count as solid; this keeps a well-planned landing from
    /// taking him through the face of a block that is currently fading in.
    fn landing_arc_clear(
        &self,
        view: &LifeView,
        launch_x: f32,
        launch_y: f32,
        vx: f32,
        landing_t: f32,
    ) -> bool {
        const SAMPLES: usize = 24;
        let half_tile = CELL_PX * TILE_FILL * 0.5;
        let landing_reach = (WALKER_THIGH + WALKER_SHIN) * 0.94;
        let body_above_hip = WALKER_MASKED_H - landing_reach;
        let body_half_w = WALKER_GRAB_BODY_CLEARANCE;

        // Skip the exact launch and contact instants: touching the top of the launch
        // floor and the destination floor is the intended motion, not an obstruction.
        for sample in 1..SAMPLES {
            let t = landing_t * sample as f32 / SAMPLES as f32;
            let x = launch_x + vx * t;
            let hip_y = launch_y + WALKER_JUMP_V * t - WALKER_GRAVITY * t * t * 0.5;
            let foot_y = hip_y - landing_reach;
            let crown_y = hip_y + body_above_hip;

            for col in 0..view.cols() as isize {
                let tile_x = Self::cell_x(view, col);
                if tile_x + half_tile < x - body_half_w || tile_x - half_tile > x + body_half_w {
                    continue;
                }
                for row in 0..view.rows() as isize {
                    if !self.blocked_soon(view, col, row) {
                        continue;
                    }
                    let tile_y = view.cell_center(col as f32, row as f32)[1];
                    let tile_bottom = tile_y - half_tile;
                    let tile_top = tile_y + half_tile;
                    if tile_top > foot_y + 1.0 && tile_bottom < crown_y {
                        return false;
                    }
                }
            }
        }
        true
    }

    /// Find an exposed stable tile whose centre a natural descending jump can meet.
    /// Unlike a ledge catch this is the common case: whenever such a tile exists, the
    /// walker aims his feet at it before considering an untargeted exit jump.
    fn plan_landing(
        &self,
        view: &LifeView,
        dir: f32,
        launch_x: f32,
        launch_y: f32,
    ) -> Option<LandingPlan> {
        let half_tile = CELL_PX * TILE_FILL * 0.5;
        let landing_reach = (WALKER_THIGH + WALKER_SHIN) * 0.94;
        let mut best: Option<LandingPlan> = None;

        for col in MARGIN as isize..(view.cols() - MARGIN) as isize {
            let target_x = Self::cell_x(view, col);
            let dx = target_x - launch_x;
            let along = dx * dir;
            if !(CELL_PX * 0.70..=CELL_PX * 6.2).contains(&along) {
                continue;
            }

            for row in MARGIN as isize..(view.rows() - MARGIN) as isize {
                let stable_floor = (0..=WALKER_PLAN_GENS)
                    .all(|g| view.alive(col, row, g) && !view.alive(col, row - 1, g));
                if !stable_floor {
                    continue;
                }

                let top = view.cell_center(col as f32, row as f32)[1] + half_tile;
                if top < self.vis_bottom {
                    continue;
                }
                let target_hip_y = top + landing_reach;
                let dy = target_hip_y - launch_y;
                let disc = WALKER_JUMP_V * WALKER_JUMP_V - 2.0 * WALKER_GRAVITY * dy;
                if disc <= 0.0 {
                    continue;
                }
                let landing_t = (WALKER_JUMP_V + disc.sqrt()) / WALKER_GRAVITY;
                if !(0.38..=1.45).contains(&landing_t) {
                    continue;
                }
                let vx = dx / landing_t;
                if vx * dir <= 0.0
                    || !(WALKER_LAND_MIN_SPEED..=WALKER_LAND_MAX_SPEED).contains(&vx.abs())
                    || !self.landing_arc_clear(view, launch_x, launch_y, vx, landing_t)
                {
                    continue;
                }

                // Ordinary running pace looks best. Near-equal arcs prefer the nearer
                // landing and a smaller vertical change, both of which read more clearly.
                let score = (vx.abs() - WALKER_RUN_SPEED).abs() + along * 0.012 + dy.abs() * 0.008;
                let plan = LandingPlan {
                    col,
                    row,
                    vx,
                    score,
                };
                if best.is_none_or(|old| plan.score < old.score) {
                    best = Some(plan);
                }
            }
        }
        best
    }

    fn surface_of(&self, view: &LifeView, row: isize) -> f32 {
        view.cell_center(self.col as f32, row as f32)[1] + CELL_PX * TILE_FILL * 0.5
    }

    fn col_of(view: &LifeView, x: f32) -> isize {
        (x / CELL_PX + (view.cols() as f32 - 1.0) * 0.5).round() as isize
    }

    /// A cell that is there now and stays put long enough to stand on.
    fn solid_soon(&self, view: &LifeView, col: isize, row: isize) -> bool {
        (0..=WALKER_PLAN_GENS).all(|g| view.alive(col, row, g))
    }

    /// A jump-arc cell is blocked if it is solid now or will be in the next known
    /// generation. That prevents a future birth from intersecting a committed arc; once
    /// its fade begins it is also present in generation zero and handled as a normal wall.
    fn blocked_soon(&self, view: &LifeView, col: isize, row: isize) -> bool {
        (0..=WALKER_PLAN_GENS).any(|g| view.alive(col, row, g))
    }

    /// The first live tile his feet would meet on the way down: the highest one in his
    /// column whose surface is already below them.
    ///
    /// This answers "what is under me", not "have I landed" — the two are different, and
    /// conflating them teleported him onto the ground the instant he spawned. Taking it
    /// from where his feet were *before* a step also means a fast fall cannot tunnel
    /// through a tile, since the topmost candidate is the first one he would reach.
    fn ground_below(&self, view: &LifeView, foot_y: f32) -> Option<(isize, f32)> {
        // Rows count downward, so ascending rows walk down the screen.
        (0..view.rows() as isize).find_map(|row| {
            let top = self.surface_of(view, row);
            (top <= foot_y && view.alive(self.col, row, 0)).then_some((row, top))
        })
    }

    /// Is there a tile beside him at body height, walling him in that direction?
    fn walled(&self, view: &LifeView, dir: isize) -> bool {
        let row = self.support.unwrap_or(0);
        // Rows count downward, so one row *up* is one less.
        view.alive(self.col + dir, row - 1, 0)
    }

    /// How many cells he could run in a direction: floor underfoot and head room above
    /// it, for as far as he cares to look.
    ///
    /// Judged on the board as it stands, not a generation out. A flat stretch of Life is
    /// almost never stable, so demanding the whole runway survive a generation left him
    /// with nowhere to go and reduced him to hopping in place forever. A fast generation
    /// can still land during a run, which is why `update` re-checks the next wall every
    /// frame. Lookahead remains strict for the take-off floor and jump destination.
    fn runway(&self, view: &LifeView, dir: isize) -> isize {
        let row = self.support.unwrap_or(0);
        (1..=WALKER_MAX_RUNWAY)
            .take_while(|i| {
                let c = self.col + dir * i;
                view.alive(c, row, 0) && !view.alive(c, row - 1, 0)
            })
            .count() as isize
    }

    /// The nearest body-height wall in a run direction, expressed as the last safe hip
    /// position before his body reaches its face.
    fn run_wall_limit(&self, view: &LifeView, dir: f32) -> Option<f32> {
        let row = self.support? - 1;
        let col = self.col + dir as isize;
        if !view.alive(col, row, 0) {
            return None;
        }
        let face = Self::cell_x(view, col)
            - dir * (CELL_PX * TILE_FILL * 0.5 + WALKER_GRAB_BODY_CLEARANCE);
        Some(face)
    }

    /// Pick something to do, from the handful of things he knows how to do.
    fn decide(&mut self, view: &LifeView) {
        let (left, right) = (self.runway(view, -1), self.runway(view, 1));
        let boxed = self.walled(view, -1) && self.walled(view, 1);

        if !boxed {
            // First look for a running jump with a real destination. A shorter approach
            // that ends on another block beats a longer run whose only outcome is leaving
            // the screen.
            let run_landing = [(-1.0, left), (1.0, right)]
                .into_iter()
                .filter(|(_, cells)| *cells > 0)
                .filter_map(|(dir, mut cells)| {
                    let d = dir as isize;
                    let row = self.support.unwrap_or(0);
                    while cells > 1 && !self.solid_soon(view, self.col + d * cells, row) {
                        cells -= 1;
                    }
                    let take_off = self.x + dir * cells as f32 * CELL_PX;
                    self.plan_landing(view, dir, take_off, self.hip_y)
                        .map(|plan| (plan.score, dir, take_off))
                })
                .min_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

            if let Some((_, dir, take_off)) = run_landing {
                self.facing = dir;
                self.act = Act::Run {
                    dir,
                    t: 0.0,
                    take_off,
                };
                return;
            }

            // A small isolated perch has no runway, but it can still offer a perfectly
            // good leap. Use a visible crouch and spring directly toward that landing.
            let standing_landing = [-1.0, 1.0]
                .into_iter()
                .filter_map(|dir| {
                    self.plan_landing(view, dir, self.x, self.hip_y)
                        .map(|plan| (plan.score, dir))
                })
                .min_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
            if let Some((_, dir)) = standing_landing {
                self.facing = dir;
                self.act = Act::Leap { dir, t: 0.0 };
                return;
            }
        }

        if boxed || (left == 0 && right == 0) {
            // Nowhere to go. The first boxed choice strongly favours the readable shove;
            // after that the two routines alternate, rather than rolling the same idle
            // three or four times in succession.
            let shove = boxed
                && match self.last_stuck {
                    Some(was_shove) => !was_shove,
                    None => self.rng.f32() < 0.78,
                };
            self.last_stuck = Some(shove);
            if shove {
                let dir = if self.walled(view, 1) && self.rng.f32() < 0.7 {
                    1.0
                } else {
                    -1.0
                };
                self.facing = dir;
                self.act = Act::Shove { dir, t: 0.0 };
            } else {
                self.act = Act::Hop(0.0);
            }
            return;
        }

        // Head the roomier way, and toss a coin when it is a tie.
        let dir = if right > left || (right == left && self.rng.f32() < 0.5) {
            1.0
        } else {
            -1.0
        };
        let mut cells = if dir > 0.0 { right } else { left };
        // Take off from ground that will still be there when he gets to it; if the last
        // cell is on its way out, leave from the one before instead.
        let d = if dir > 0.0 { 1 } else { -1 };
        let row = self.support.unwrap_or(0);
        while cells > 1 && !self.solid_soon(view, self.col + d * cells, row) {
            cells -= 1;
        }
        self.facing = dir;
        self.act = Act::Run {
            dir,
            t: 0.0,
            take_off: self.x + dir * cells as f32 * CELL_PX,
        };
    }

    fn leave_ground(&mut self, vy: f32, vx: f32) {
        self.fell_from = self.hip_y;
        self.support = None;
        self.act = Act::Airborne;
        self.vy = vy;
        self.vx = vx;
        self.grab_plan = None;
        self.landing_plan = None;
        self.hang_release_at = f32::INFINITY;
    }

    /// Commit to the kind of hang this catch will become. Waiting until contact to roll
    /// this choice keeps the ballistic catch itself deterministic: only the performance
    /// after the hands are safely on the ledge varies.
    fn begin_hang(&mut self, col: isize, row: isize) {
        self.act = Act::Hang { col, row };
        self.hang_release_at = if self.rng.f32() < WALKER_HANG_THROW_CHANCE {
            self.t + 1.05 + self.rng.f32() * 2.15
        } else {
            f32::INFINITY
        };
    }

    /// Find a stable exposed tile away from the held face that the descending throw arc
    /// can meet near its centre. As with a ledge catch, this changes velocity at launch
    /// but never changes position later, so a successful landing follows the visible arc.
    fn plan_hang_throw(&self, view: &LifeView, away: f32) -> Option<f32> {
        let half_tile = CELL_PX * TILE_FILL * 0.5;
        // The falling pose reaches almost this far below the hip. The actual collision
        // still uses the animated feet; this estimate only selects a natural launch.
        let landing_reach = (WALKER_THIGH + WALKER_SHIN) * 0.94;
        let mut best: Option<(f32, f32)> = None;

        for col in MARGIN as isize..(view.cols() - MARGIN) as isize {
            let target_x = Self::cell_x(view, col);
            let dx = target_x - self.x;
            let along = dx * away;
            if !(CELL_PX * 0.45..=CELL_PX * 6.2).contains(&along) {
                continue;
            }

            for row in MARGIN as isize..(view.rows() - MARGIN) as isize {
                let stable_floor = (0..=WALKER_PLAN_GENS)
                    .all(|g| view.alive(col, row, g) && !view.alive(col, row - 1, g));
                if !stable_floor {
                    continue;
                }

                let top = view.cell_center(col as f32, row as f32)[1] + half_tile;
                let target_hip_y = top + landing_reach;
                let dy = target_hip_y - self.hip_y;
                let disc = WALKER_THROW_V * WALKER_THROW_V - 2.0 * WALKER_GRAVITY * dy;
                if disc <= 0.0 {
                    continue;
                }
                let landing_t = (WALKER_THROW_V + disc.sqrt()) / WALKER_GRAVITY;
                if !(0.38..=1.35).contains(&landing_t) {
                    continue;
                }
                let vx = dx / landing_t;
                if vx * away <= 0.0
                    || !(WALKER_THROW_MIN_SPEED..=WALKER_THROW_MAX_SPEED).contains(&vx.abs())
                {
                    continue;
                }

                // Prefer an ordinary running pace, with a slight preference for the
                // nearer place when two arcs require essentially the same kick.
                let score = (vx.abs() - WALKER_RUN_SPEED * 0.82).abs() + along * 0.015;
                if best.is_none_or(|(old, _)| score < old) {
                    best = Some((score, vx));
                }
            }
        }

        best.map(|(_, vx)| vx)
    }

    fn throw_from_hang(&mut self, view: &LifeView) {
        let away = -self.facing;
        let vx = self
            .plan_hang_throw(view, away)
            .unwrap_or(away * WALKER_RUN_SPEED * 0.72);
        self.facing = away;
        self.leave_ground(WALKER_THROW_V, vx);
    }

    /// A newly born tile has occupied the body cell. Step into a supported clear neighbour
    /// when one exists; otherwise be knocked sideways into a clear drop. Only a completely
    /// sealed squeeze uses the brief behind-the-floor fall-through escape.
    fn dislodge(&mut self, view: &LifeView, row: isize) {
        self.grab_plan = None;
        self.landing_plan = None;
        let first = if self.rng.f32() < 0.5 { -1isize } else { 1 };
        let dirs = [first, -first];

        // Prefer a real step that keeps his feet on the same surface.
        for dir in dirs {
            let col = self.col + dir;
            if view.alive(col, row, 0) && !view.alive(col, row - 1, 0) {
                self.facing = dir as f32;
                self.vx = 0.0;
                self.act = Act::Sidestep {
                    dir: dir as f32,
                    to_x: Self::cell_x(view, col),
                };
                return;
            }
        }

        // If there is no floor next door, a sideways fall reads as the block displacing
        // him. There is no upward kick, so it cannot turn into the repeated-hop artifact.
        for dir in dirs {
            let col = self.col + dir;
            if !view.alive(col, row - 1, 0) {
                self.facing = dir as f32;
                self.fell_from = self.hip_y;
                self.support = None;
                self.act = Act::Airborne;
                self.vy = self.vy.min(0.0);
                self.vx = dir as f32 * WALKER_STEP_SPEED;
                return;
            }
        }

        // Fully sealed. Let the tile carry him behind the plane and down for long enough
        // to clear this floor instead of landing on it again every frame.
        self.fell_from = self.hip_y;
        self.support = None;
        self.act = Act::Airborne;
        self.vy = 0.0;
        self.vx = 0.0;
        self.no_land_until = self.t + 0.46;
    }

    /// Target pose and how hard to chase it, for whatever he is doing.
    fn target(&self) -> (Pose, f32) {
        match self.act {
            Act::Airborne => {
                if self.vy > 0.0 {
                    // On the way up out of a jump: tucked, under control.
                    (Pose::airborne_tuck().facing(self.facing), WALKER_EASE_FALL)
                } else if self.grab_plan.is_some() {
                    // This reach began at the apex, well before contact. Carrying the
                    // same target through Hang keeps the arms continuous at the catch.
                    (Pose::hanging(), 10.5)
                } else {
                    // The further he has dropped, the wider the gesture opens.
                    let drop = (self.fell_from - self.hip_y).max(0.0);
                    let mut p = Pose::falling(drop / WALKER_DRAMA_DROP);
                    let w = (self.t * 8.0 + self.seed).sin() * 0.16;
                    p.shoulder[0] += w;
                    p.shoulder[1] += w * 0.8;
                    p.knee[0] -= w * 0.5;
                    p.knee[1] += w * 0.5;
                    (p, WALKER_EASE_FALL)
                }
            }
            Act::Landing(t) if t < WALKER_ABSORB => (Pose::landed(), WALKER_EASE_ABSORB),
            Act::Landing(_) => (Pose::standing(), WALKER_EASE_RECOVER),
            Act::Deciding(_) => {
                let mut p = Pose::standing();
                // Breathing, so standing still is not completely static.
                let b = (self.t * 1.5 + self.seed).sin() * 0.03;
                p.shoulder[0] += b;
                p.shoulder[1] -= b;
                (p, 4.0)
            }
            Act::Hop(t) | Act::Leap { t, .. } => {
                // Gather, then spring — the spring itself is a jump, handled elsewhere.
                let k = (t / WALKER_CROUCH_SECS).clamp(0.0, 1.0);
                let mut p = Pose::landed();
                p.shoulder = [0.55, -0.55];
                p.elbow = [-0.30, 0.30];
                let _ = k;
                (p, WALKER_EASE_ABSORB * 0.6)
            }
            Act::Shove { dir, t } => {
                // Brace up against it, put everything in, then give it up.
                if t < 0.30 {
                    (Pose::shoving(0.0).facing(dir), 9.0)
                } else if t < WALKER_SHOVE_SECS - 0.35 {
                    let mut p = Pose::straining();
                    // A shudder, so the effort reads as effort and not as a held pose.
                    let shake = (t * 21.0).sin() * 0.045;
                    p.lean += shake;
                    p.shoulder[0] += shake;
                    p.shoulder[1] -= shake;
                    p.knee[1] += shake * 2.0;
                    (p.facing(dir), 14.0)
                } else {
                    (Pose::shoving(0.2).facing(dir), 7.0)
                }
            }
            Act::Sidestep { dir, .. } => (
                Pose::running(self.t * WALKER_RUN_CADENCE * 0.65).facing(dir),
                WALKER_EASE_RUN,
            ),
            Act::Run { dir, t, .. } => (
                Pose::running(t * WALKER_RUN_CADENCE).facing(dir),
                WALKER_EASE_RUN,
            ),
            Act::Hang { .. } => {
                let remaining = self.hang_release_at - self.t;
                if remaining.is_finite() && remaining < 0.44 {
                    let amount = 1.0 - (remaining / 0.44).clamp(0.0, 1.0);
                    (Pose::hanging_throw(-self.facing, amount), WALKER_EASE_RUN)
                } else {
                    let mut p = Pose::hanging();
                    // A slow swing, so he is not a coat on a hook.
                    let sway = (self.t * 2.1 + self.seed).sin() * 0.11;
                    p.hip[0] += sway;
                    p.hip[1] += sway;
                    p.knee[0] -= sway * 0.5;
                    (p, 7.0)
                }
            }
        }
    }

    /// Emits the figure: one rod per limb segment and the oversized mask over the upper
    /// torso. The mask is closer to the camera than the strokes, so it really replaces
    /// the head and covers the neck/chest lines underneath.
    fn draw_figure(&self, out: &mut PropSink) {
        let ink = [0.0f32, 0.0, 0.0];
        let mut rod = |from: [f32; 2], dir: [f32; 2], len: f32| {
            out.push(
                MODEL_ROD,
                Prop::stretched(
                    [from[0], from[1], WALKER_Z],
                    [len, WALKER_LINE, 1.0],
                    [0.0, 0.0, dir[1].atan2(dir[0])],
                )
                .tinted(ink, 1.0),
            );
        };

        let hip = [self.x, self.hip_y];
        let lean = self.pose.lean;
        let torso_dir = up(lean);
        let shoulder = [
            hip[0] + torso_dir[0] * WALKER_TORSO,
            hip[1] + torso_dir[1] * WALKER_TORSO,
        ];
        rod(hip, torso_dir, WALKER_TORSO);

        // Shoulder bar, across the torso and centred on it.
        let across = [lean.cos(), -lean.sin()];
        rod(
            [
                shoulder[0] - across[0] * WALKER_SHOULDER,
                shoulder[1] - across[1] * WALKER_SHOULDER,
            ],
            across,
            WALKER_SHOULDER * 2.0,
        );

        for i in 0..2 {
            let side = if i == 0 { 1.0 } else { -1.0 };
            let joint = [
                shoulder[0] + across[0] * WALKER_SHOULDER * side,
                shoulder[1] + across[1] * WALKER_SHOULDER * side,
            ];
            let upper = down(lean + self.pose.shoulder[i]);
            rod(joint, upper, WALKER_UPPER_ARM);
            let elbow = [
                joint[0] + upper[0] * WALKER_UPPER_ARM,
                joint[1] + upper[1] * WALKER_UPPER_ARM,
            ];
            rod(
                elbow,
                down(lean + self.pose.shoulder[i] + self.pose.elbow[i]),
                WALKER_FOREARM,
            );

            let thigh = down(self.pose.hip[i]);
            rod(hip, thigh, WALKER_THIGH);
            let knee = [
                hip[0] + thigh[0] * WALKER_THIGH,
                hip[1] + thigh[1] * WALKER_THIGH,
            ];
            rod(
                knee,
                down(self.pose.hip[i] + self.pose.knee[i]),
                WALKER_SHIN,
            );
        }

        let (model, width, height, scale_y, overlap, lift) = match self.headgear {
            Headgear::Tiki => (
                MODEL_TIKI_MASK,
                WALKER_MASK_W,
                WALKER_MASK_H,
                WALKER_MASK_W * 0.90,
                WALKER_MASK_CHEST_OVERLAP,
                0.0,
            ),
            Headgear::Astronaut => (
                MODEL_ASTRONAUT_HELMET,
                WALKER_HELMET_W,
                WALKER_HELMET_H,
                WALKER_HELMET_H,
                WALKER_HELMET_CHEST_OVERLAP,
                WALKER_HELMET_LIFT,
            ),
            Headgear::Noh => (
                MODEL_NOH_MASK,
                WALKER_NOH_W,
                WALKER_NOH_H,
                WALKER_NOH_W * 0.95,
                WALKER_NOH_CHEST_OVERLAP,
                0.0,
            ),
            Headgear::Monkey => (
                MODEL_MONKEY_MASK,
                WALKER_MONKEY_W,
                WALKER_MONKEY_H,
                WALKER_MONKEY_W,
                WALKER_MONKEY_CHEST_OVERLAP,
                0.0,
            ),
            Headgear::ComedyTragedy => (
                MODEL_COMEDY_TRAGEDY_MASK,
                WALKER_GREEK_W,
                WALKER_GREEK_H,
                WALKER_GREEK_W * 0.95,
                WALKER_GREEK_CHEST_OVERLAP,
                0.0,
            ),
        };
        let head_from_shoulder = height * 0.5 - overlap + lift;
        let head = [
            shoulder[0] + torso_dir[0] * head_from_shoulder,
            shoulder[1] + torso_dir[1] * head_from_shoulder,
        ];
        out.push(
            model,
            Prop::stretched(
                [head[0], head[1], WALKER_Z + 1.0],
                [width, scale_y, 1.0],
                [0.0, 0.0, -lean],
            ),
        );
    }
}

impl Critter for Walker {
    fn update(&mut self, ctx: &CritterCtx) -> bool {
        self.t += ctx.dt;

        // Advance whatever he is doing, and hand off when it finishes.
        match self.act {
            Act::Landing(ref mut t) => {
                *t += ctx.dt;
                if *t > WALKER_ABSORB * 6.0 {
                    self.act = Act::Deciding(0.0);
                }
            }
            Act::Deciding(ref mut t) => {
                *t += ctx.dt;
                if *t > WALKER_DECIDE {
                    self.decide(&ctx.life);
                }
            }
            Act::Hop(ref mut t) => {
                *t += ctx.dt;
                if *t > WALKER_CROUCH_SECS {
                    // Straight up, no sideways push: he lands right back where he was.
                    self.leave_ground(WALKER_HOP_V, 0.0);
                }
            }
            Act::Leap { dir, t } => {
                let t = t + ctx.dt;
                if t > WALKER_CROUCH_SECS {
                    // Re-plan after the crouch so a target changed during the wind-up is
                    // never treated as guaranteed. If it vanished, simply reconsider.
                    if let Some(plan) = self.plan_landing(&ctx.life, dir, self.x, self.hip_y) {
                        self.leave_ground(WALKER_JUMP_V, plan.vx);
                        self.landing_plan = Some(plan);
                    } else {
                        self.act = Act::Deciding(0.0);
                    }
                } else {
                    self.act = Act::Leap { dir, t };
                }
            }
            Act::Shove { dir, ref mut t } => {
                *t += ctx.dt;
                // Straighten up before the tile can go, so the gag lands: he strains, gives
                // up, and only then does it vanish of its own accord.
                let target = self.support.unwrap_or(0) - 1;
                let doomed = !ctx.life.alive(self.col + dir as isize, target, 1);
                let left_in_gen = (1.0 - ctx.phase) * ctx.gen_secs;
                if *t > WALKER_SHOVE_SECS || (doomed && left_in_gen < 0.5) {
                    self.act = Act::Deciding(0.0);
                }
            }
            Act::Sidestep { dir, to_x } => {
                let step = WALKER_STEP_SPEED * ctx.dt;
                let left = (to_x - self.x).abs();
                if left <= step {
                    self.x = to_x;
                    self.col = Self::col_of(&ctx.life, self.x);
                    self.act = Act::Deciding(0.0);
                } else {
                    self.x += dir * step;
                    self.col = Self::col_of(&ctx.life, self.x);
                }
            }
            Act::Run { dir, t, take_off } => {
                let t = t + ctx.dt;
                let proposed = self.x + dir * WALKER_RUN_SPEED * ctx.dt;

                // Re-check every frame. A generation can land halfway through the run,
                // and its newborn tile is already solid even while the renderer is only
                // beginning to fade it in.
                if let Some(limit) = self.run_wall_limit(&ctx.life, dir) {
                    if (proposed - limit) * dir >= 0.0 {
                        self.x = limit;
                        self.col = Self::col_of(&ctx.life, self.x);
                        self.vx = 0.0;
                        self.act = Act::Deciding(0.0);
                    } else {
                        self.x = proposed;
                        self.col = Self::col_of(&ctx.life, self.x);
                        self.act = Act::Run { dir, t, take_off };
                    }
                } else {
                    self.x = proposed;
                    self.col = Self::col_of(&ctx.life, self.x);
                    if (self.x - take_off) * dir >= 0.0 {
                        // Foot landings are deliberate whenever the board offers one.
                        // Only without one do we try the occasional ledge performance or
                        // preserve the old untargeted exit jump.
                        if let Some(plan) = self.plan_landing(&ctx.life, dir, self.x, self.hip_y) {
                            self.leave_ground(WALKER_JUMP_V, plan.vx);
                            self.landing_plan = Some(plan);
                        } else {
                            let planned = self.plan_grab(&ctx.life, dir);
                            let run = planned.map(|(_, vx)| vx).unwrap_or(WALKER_RUN_SPEED * dir);
                            self.leave_ground(WALKER_JUMP_V, run);
                            self.grab_plan = planned.map(|(plan, _)| plan);
                        }
                    } else {
                        self.act = Act::Run { dir, t, take_off };
                    }
                }
            }
            Act::Hang { col, row } => {
                // Down he goes the moment the ledge stops being a ledge.
                if !ctx.life.alive(col, row, 0) || ctx.life.alive(col, row - 1, 0) {
                    self.support = None;
                    self.act = Act::Airborne;
                    self.fell_from = self.hip_y;
                    self.vy = 0.0;
                    self.vx = 0.0;
                    self.grab_plan = None;
                    self.landing_plan = None;
                    self.hang_release_at = f32::INFINITY;
                } else if self.t >= self.hang_release_at {
                    self.throw_from_hang(&ctx.life);
                }
            }
            Act::Airborne => {}
        }

        // Lose your footing the moment the tile under you stops existing — or the moment
        // one grows in the space you are standing in, which shoves you off your perch.
        if let Some(row) = self.support {
            let gone = !ctx.life.alive(self.col, row, 0);
            let occupied = ctx.life.alive(self.col, row - 1, 0);
            if gone {
                let carry = match self.act {
                    Act::Run { dir, .. } => dir * WALKER_RUN_SPEED,
                    Act::Sidestep { dir, .. } => dir * WALKER_STEP_SPEED,
                    _ => self.vx,
                };
                self.fell_from = self.hip_y;
                self.support = None;
                self.act = Act::Airborne;
                self.vy = self.vy.min(0.0);
                self.vx = carry;
                self.grab_plan = None;
                self.landing_plan = None;
            } else if occupied && !matches!(self.act, Act::Sidestep { .. }) {
                self.dislodge(&ctx.life, row);
            }
        }

        // A planned ledge can still change before contact. Cancelling the reach is smooth
        // because all poses use the same eased joints; it merely returns to falling.
        if let Some(plan) = self.grab_plan {
            let body_col = plan.col - self.facing as isize;
            let invalid = !ctx.life.alive(plan.col, plan.row, 0)
                || ctx.life.alive(plan.col, plan.row - 1, 0)
                || ctx.life.alive(body_col, plan.row, 0);
            if invalid {
                self.grab_plan = None;
            }
        }

        // The target is advisory after take-off. If it changes, discard the expectation
        // and let the ordinary falling collision find anything that remains below.
        if let Some(plan) = self.landing_plan {
            if !ctx.life.alive(plan.col, plan.row, 0) || ctx.life.alive(plan.col, plan.row - 1, 0) {
                self.landing_plan = None;
            }
        }

        let (target, rate) = self.target();
        self.pose.approach(&target, 1.0 - (-rate * ctx.dt).exp());

        // Hanging is its own kind of support: the hip rides on where his hands are, the
        // same way it rides on his feet when he is stood up.
        if let Act::Hang { col, row } = self.act {
            let half_tile = CELL_PX * TILE_FILL * 0.5;
            let ledge = ctx.life.cell_center(col as f32, row as f32)[1] + half_tile;
            self.hip_y = ledge - self.hand_rise();
            self.vy = 0.0;
            self.vx = 0.0;
            return self.hip_y > self.vis_bottom - WALKER_H * 3.0;
        }

        match self.support {
            // Standing: the hip rides on whatever the legs currently reach, so the pose
            // recovering out of its squat is what lifts him back up.
            Some(row) => {
                // Landings may be modestly off-centre, but settle by visible translation
                // while the impact pose recovers. Running and emergency steps own their
                // horizontal movement and are left alone here.
                if !matches!(self.act, Act::Run { .. } | Act::Sidestep { .. }) {
                    let centre = Self::cell_x(&ctx.life, self.col);
                    let dx = centre - self.x;
                    let max_step = WALKER_SETTLE_SPEED * ctx.dt;
                    self.x += dx.clamp(-max_step, max_step);
                }
                let floor = self.surface_of(&ctx.life, row);
                self.hip_y = floor + self.leg_reach();
                self.vy = 0.0;
            }
            None => {
                let was_x = self.x;
                let was_hip = self.hip_y;
                self.x += self.vx * ctx.dt;
                self.col = Self::col_of(&ctx.life, self.x);
                // Where the feet were, then where they are: he lands when they cross the
                // surface of whatever was beneath them, not merely when something exists
                // down there somewhere.
                let was = self.foot_y();
                self.vy -= WALKER_GRAVITY * ctx.dt;
                self.hip_y += self.vy * ctx.dt;
                // Once any part of him is below the screen he is committed: nothing
                // catches him after that, so he never comes to rest with his legs out of
                // shot.
                if self.foot_y() < self.vis_bottom {
                    self.doomed = true;
                }

                // Only a pre-planned jump may catch. Resolve at the sub-frame crossing,
                // which is at most a couple of pixels back along the path and avoids both
                // lateral grid snapping and a one-frame vertical discontinuity.
                if self.vy < 0.0 && !self.doomed {
                    if let Some(plan) = self.grab_plan {
                        let hands_was = was_hip + self.hand_rise();
                        let hands_now = self.hip_y + self.hand_rise();
                        if hands_was >= plan.ledge_y && hands_now <= plan.ledge_y {
                            let span = (hands_was - hands_now).max(0.001);
                            let u = ((hands_was - plan.ledge_y) / span).clamp(0.0, 1.0);
                            let catch_x = was_x + (self.x - was_x) * u;
                            if (catch_x - plan.hip_x).abs() <= WALKER_GRAB_X_TOLERANCE {
                                self.x = catch_x;
                                self.col = Self::col_of(&ctx.life, self.x);
                                self.hip_y = plan.ledge_y - self.hand_rise();
                                self.vx = 0.0;
                                self.vy = 0.0;
                                self.facing = (plan.col - self.col) as f32;
                                self.begin_hang(plan.col, plan.row);
                            }
                            // Crossing once is the only opportunity. A horizontal miss is
                            // a real miss and continues down unchanged.
                            self.grab_plan = None;
                        }
                    }
                }

                if self.vy < 0.0
                    && !self.doomed
                    && self.t >= self.no_land_until
                    && !matches!(self.act, Act::Hang { .. })
                {
                    if let Some((row, top)) = self.ground_below(&ctx.life, was) {
                        if self.foot_y() <= top
                            && top >= self.vis_bottom
                            && Self::centered_over(&ctx.life, self.col, self.x)
                        {
                            self.support = Some(row);
                            self.act = Act::Landing(0.0);
                            self.hip_y = top + self.leg_reach();
                            self.vy = 0.0;
                            self.vx = 0.0;
                            self.grab_plan = None;
                            self.landing_plan = None;
                        }
                    }
                }
            }
        }

        // Gone once he has dropped clean off the bottom, or left sideways.
        self.hip_y > self.vis_bottom - WALKER_H * 3.0 && self.x.abs() < self.edge + CELL_PX * 2.0
    }

    fn props(&self, _ctx: &CritterCtx, out: &mut PropSink) {
        self.draw_figure(out);
    }

    #[cfg(test)]
    fn debug_state(&self) -> Option<String> {
        Some(format!(
            "{:?} col={} support={:?}",
            self.act, self.col, self.support
        ))
    }
}

/// The model's own +X axis after its rotation, scaled by `s`. Mirrors `rot_xyz` in the
/// shader, which is what keeps the exhaust attached to the tail as the rocket banks.
fn rot_x_axis(r: [f32; 3], s: f32) -> [f32; 3] {
    // Roll is a rotation about +X and so leaves it alone; only pitch and yaw carry it.
    let (sy, cy) = r[1].sin_cos();
    let (sz, cz) = r[2].sin_cos();
    let p = [cy, 0.0, -sy];
    [
        (p[0] * cz - p[1] * sz) * s,
        (p[0] * sz + p[1] * cz) * s,
        p[2] * s,
    ]
}

// ---------------------------------------------------------------------------
// Visualisation — tiles reacting to the grid, plus whatever else lives up here
// ---------------------------------------------------------------------------

pub struct Viz {
    cols: usize,
    /// The generation the tiles currently represent.
    shown: Vec<bool>,
    /// Per-cell tile state, in grid order.
    inst: Vec<Instance>,
    /// Tiles reordered into a solid run then a ghost run, with any critter instances
    /// spliced in after the solid tiles. Translucent tiles have to be drawn last.
    upload: Vec<Instance>,
    /// Length of the solid run at the head of `upload`.
    solid: usize,
    /// Experimental scenery layer: at most one giant crosses behind the grid.
    distant: DistantBackground,
    /// Page-coloured drifting mist in front of the distant giant, but behind every
    /// ordinary scene element.
    fog: FogLayer,
    critters: Vec<Box<dyn Critter>>,
    /// Prop models the critters placed this frame.
    props: PropSink,
    /// Pause-fade multiplier over every visitor's props; fog is exempt.
    visitor_alpha: f32,
    rng: Rng,
}

impl Viz {
    pub fn new(life: &Life, rng_seed: u64) -> Self {
        let (cols, rows) = (life.cols, life.rows);
        let present = life.board(0);
        let distant = DistantBackground::new(&life.view(), rng_seed ^ 0x7475_782d_666f_6721);
        let fog = FogLayer::new(&life.view(), rng_seed ^ 0x666f_672d_6472_6966);
        let inst: Vec<Instance> = (0..cols * rows)
            .map(|i| {
                let a = present[i] as u8 as f32;
                Instance {
                    cell: [(i % cols) as f32, (i / cols) as f32],
                    state: [a, a],
                    // Start settled: timestamps far in the past.
                    t: -99.0,
                    spin: [-99.0, 0.0],
                    palette: PALETTE_FROM_CELL,
                    slide_from: [(i % cols) as f32, (i / cols) as f32],
                }
            })
            .collect();
        let mut viz = Viz {
            cols,
            shown: present.to_vec(),
            upload: inst.clone(),
            inst,
            solid: 0,
            distant,
            fog,
            critters: Vec::new(),
            props: PropSink::default(),
            visitor_alpha: 1.0,
            rng: Rng::new(rng_seed),
        };
        viz.repack(&[]);
        viz
    }

    pub fn add_critter(&mut self, c: Box<dyn Critter>) {
        self.critters.push(c);
    }

    pub fn set_visitor_alpha(&mut self, alpha: f32) {
        self.visitor_alpha = alpha;
    }

    /// Drop every visitor — foreground critters and any distant crossing — so a
    /// user pause resumes onto a clean field.
    pub fn clear_critters(&mut self) {
        self.critters.clear();
        self.distant.clear_crossing();
    }

    pub fn calm_distant(&mut self, pause_scale: f32) {
        self.distant.calm(pause_scale);
    }

    pub fn disable_distant(&mut self) {
        self.distant.disable();
    }

    pub fn critter_count(&self) -> usize {
        self.critters.len()
    }

    /// Instances the renderer should draw, and how many of them are solid.
    pub fn draw_list(&self) -> (&[Instance], u32) {
        (&self.upload, self.solid as u32)
    }

    /// Whether a cell is currently drawn alive, read back out of the instance data
    /// rather than the internal arrays so checks cover the whole path.
    pub fn drawn(&self, x: usize, y: usize) -> bool {
        self.inst[y * self.cols + x].state[0] > 0.5
    }

    /// Partition into "draw solid" and "draw as a translucent ghost", splicing the
    /// critters in between. A tile counts as solid while it is on and for as long as
    /// a fade off it might still be running.
    fn repack(&mut self, critters: &[Instance]) {
        self.upload.clear();
        self.upload.reserve(self.inst.len() + critters.len());
        for inst in &self.inst {
            if inst.state[0] > 0.5 || inst.state[1] > 0.5 {
                self.upload.push(*inst);
            }
        }
        self.upload.extend_from_slice(critters);
        self.solid = self.upload.len();
        for inst in &self.inst {
            if inst.state[0] <= 0.5 && inst.state[1] <= 0.5 {
                self.upload.push(*inst);
            }
        }
    }

    /// A generation has landed: start the tiles animating toward it.
    pub fn on_generation(&mut self, life: &LifeView, now: f32) {
        let present = life.life.board(0);
        let claimed: Vec<(usize, usize)> = self
            .critters
            .iter()
            .filter_map(|critter| critter.claimed_cell())
            .collect();

        // Match births to adjacent deaths before touching instance state. A dying
        // cell and a newborn may each participate in at most one move. A stable
        // per-cell direction rotation avoids a permanent left/up bias without
        // consuming the RNG used by unrelated survivor-spin behavior.
        let mut claimed_death = vec![false; self.inst.len()];
        for &(x, y) in &claimed {
            if x < self.cols && y * self.cols + x < claimed_death.len() {
                claimed_death[y * self.cols + x] = true;
            }
        }
        let mut death_taken = claimed_death.clone();
        let mut slide_source = vec![None; self.inst.len()];
        let directions = [(-1isize, 0isize), (1, 0), (0, -1), (0, 1)];
        for i in 0..self.inst.len() {
            if self.shown[i] || !present[i] {
                continue;
            }
            let (x, y) = ((i % self.cols) as isize, (i / self.cols) as isize);
            let first = (cell_hash(x as u32, y as u32) * directions.len() as f32) as usize;
            for step in 0..directions.len() {
                let (dx, dy) = directions[(first + step) % directions.len()];
                let (nx, ny) = (x + dx, y + dy);
                if nx < 0
                    || ny < 0
                    || nx >= self.cols as isize
                    || ny >= present.len() as isize / self.cols as isize
                {
                    continue;
                }
                let source = ny as usize * self.cols + nx as usize;
                if self.shown[source] && !present[source] && !death_taken[source] {
                    slide_source[i] = Some(source);
                    death_taken[source] = true;
                    break;
                }
            }
        }

        for i in 0..self.inst.len() {
            let (alive, was) = (present[i], self.shown[i]);
            let cell = [(i % self.cols) as f32, (i / self.cols) as f32];
            self.inst[i].slide_from = cell;
            if alive != was {
                let x = i % self.cols;
                let y = i / self.cols;
                // A UFO replaces its target with a free prop in this exact frame. Do
                // not also run the ordinary half-second death fade underneath it.
                let abducted = was && !alive && claimed.contains(&(x, y));
                let moved_away = was && !alive && death_taken[i];
                self.inst[i].state = if let Some(source) = slide_source[i] {
                    self.inst[i].slide_from =
                        [(source % self.cols) as f32, (source / self.cols) as f32];
                    // One continuously solid tile replaces the ordinary death and
                    // birth fades, beginning at the dying cell's exact position.
                    [1.0, 1.0]
                } else if abducted || moved_away {
                    [0.0, 0.0]
                } else {
                    [alive as u8 as f32, was as u8 as f32]
                };
                self.inst[i].t = now;
            } else if alive && is_colored(i % self.cols, i / self.cols) {
                // Survived another generation, and it is one of the coloured ones.
                if self.rng.f32() < SPIN_CHANCE {
                    let toward = if self.rng.f32() < 0.5 { 1.0 } else { -1.0 };
                    self.inst[i].spin = [now, toward];
                }
            }
        }
        self.shown.copy_from_slice(present);
    }

    /// Retire transitions whose fade has finished, so a tile that is no longer visible
    /// stops being drawn.
    ///
    /// This has to happen every frame rather than once a generation. `repack` keeps a
    /// tile in the solid, depth-writing run while it is "on or was just on", and a fade
    /// out takes DEATH_FADE — but a generation now lasts several seconds. Retiring on
    /// the generation boundary therefore left fully transparent tiles writing depth for
    /// seconds at a time, and an invisible occluder eclipses whatever is behind it.
    /// That is what made the rocket vanish behind tiles that were not there.
    fn retire_finished(&mut self, now: f32) {
        for inst in &mut self.inst {
            if inst.state[0] != inst.state[1] && now - inst.t >= DEATH_FADE {
                inst.state[1] = inst.state[0];
            }
        }
    }

    /// Per-frame: run the critters and rebuild the upload buffers.
    pub fn update(&mut self, ctx: &CritterCtx) {
        self.retire_finished(ctx.now);
        let spinning: Vec<bool> = self
            .inst
            .iter()
            .map(|inst| {
                let age = ctx.now - inst.spin[0];
                inst.spin[1] != 0.0 && (0.0..SPIN_SECS).contains(&age)
            })
            .collect();
        let critter_ctx = CritterCtx {
            life: ctx.life,
            dt: ctx.dt,
            now: ctx.now,
            phase: ctx.phase,
            gen_secs: ctx.gen_secs,
            spinning: Some(&spinning),
        };
        let mut critters = std::mem::take(&mut self.critters);
        critters.retain_mut(|c| c.update(&critter_ctx));

        let mut emitted = Vec::new();
        self.props.clear();
        self.distant.update(&critter_ctx);
        self.distant.props(&mut self.props);
        self.fog.update(critter_ctx.dt);
        self.fog.props(&mut self.props);
        for c in &critters {
            c.draw(&critter_ctx, &mut emitted);
            c.props(&critter_ctx, &mut self.props);
        }
        if self.visitor_alpha != 1.0 {
            self.props.scale_visitor_alpha(self.visitor_alpha);
        }
        self.critters = critters;
        self.repack(&emitted);
    }

    #[cfg(test)]
    fn walker_probe(&self) -> Option<String> {
        self.critters.iter().find_map(|c| c.debug_state())
    }

    /// Prop models placed by the critters this frame.
    pub fn props(&self) -> &PropSink {
        &self.props
    }

    /// Re-base timestamps so the shader clock stays small and f32-precise.
    fn rebase(&mut self, delta: f32) {
        for inst in &mut self.inst {
            inst.t = (inst.t - delta).max(-9.0);
            inst.spin[0] = (inst.spin[0] - delta).max(-9.0);
        }
    }
}

/// Rust mirror of the shader's `hash_cell`. Used to decide which tiles are eligible
/// to spin, and to test the colour split.
fn cell_hash(x: u32, y: u32) -> f32 {
    let mut h = x
        .wrapping_mul(374_761_393)
        .wrapping_add(y.wrapping_mul(668_265_263));
    h = (h ^ (h >> 13)).wrapping_mul(1_274_126_177);
    h ^= h >> 16;
    h as f32 / 4_294_967_296.0
}

/// Which colour a cell wears. Mirrored by `live_color` in the shader.
///
/// Green and teal fall straight out of the hash and may sit next to each other, but a
/// blue has to stand alone: it only keeps its colour if none of its eight neighbours
/// is coloured at all. That is a filter on an already-chosen colour rather than a
/// reassignment, so a rejected blue goes grey rather than displacing another accent.
fn cell_category(x: isize, y: isize) -> u8 {
    let base = |x: isize, y: isize| {
        if x < 0 || y < 0 {
            return 0;
        }
        let h = cell_hash(x as u32, y as u32);
        if h > BLUE_CUT {
            3
        } else if h > TEAL_CUT {
            2
        } else if h > GREEN_CUT {
            1
        } else {
            0
        }
    };
    let me = base(x, y);
    if me != 3 {
        return me;
    }
    for dy in -1isize..=1 {
        for dx in -1isize..=1 {
            if (dx != 0 || dy != 0) && base(x + dx, y + dy) != 0 {
                return 0;
            }
        }
    }
    3
}

/// Whether a cell wears one of the accent colours rather than the common grey.
fn is_colored(x: usize, y: usize) -> bool {
    cell_category(x as isize, y as isize) != 0
}

/// A 3x3 glider as three rows of three bits (MSB leftmost), oriented so it travels
/// inward from `edge` (0 top, 1 bottom, 2 left, 3 right). Each edge has two diagonals
/// heading onto the screen; `flip` chooses between them.
fn glider_for(edge: usize, flip: bool) -> [u8; 3] {
    const SE: [u8; 3] = [0b010, 0b001, 0b111];
    const SW: [u8; 3] = [0b010, 0b100, 0b111];
    const NE: [u8; 3] = [0b111, 0b001, 0b010];
    const NW: [u8; 3] = [0b111, 0b100, 0b010];
    match (edge, flip) {
        (0, false) => SE,
        (0, true) => SW,
        (1, false) => NE,
        (1, true) => NW,
        (2, false) => SE,
        (2, true) => NE,
        (_, false) => SW,
        (_, true) => NW,
    }
}

/// Which generation a given point on the simulation clock falls in. Monotonic and
/// continuous across cycle boundaries, so the counter never stalls or jumps back.
pub fn generation_at(t: f64) -> i64 {
    if FAST_SECS <= 0.0 {
        return (t * SLOW_HZ).floor() as i64;
    }
    let cycles = (t / CYCLE).floor();
    let u = t - cycles * CYCLE;
    let within = if u < SLOW_SECS {
        u * SLOW_HZ
    } else {
        SLOW_SECS * SLOW_HZ + (u - SLOW_SECS) * FAST_HZ
    };
    (cycles * CYCLE_GENS + within).floor() as i64
}

pub fn grid_dims(css_w: f64, css_h: f64) -> (usize, usize) {
    let cols = (css_w / CELL_PX as f64).ceil() as usize + 2 * MARGIN;
    let rows = (css_h / CELL_PX as f64).ceil() as usize + 2 * MARGIN;
    (cols.max(BAND + 6), rows.max(BAND + 6))
}

// ---------------------------------------------------------------------------
// Shader
// ---------------------------------------------------------------------------

const SHADER: &str = r#"
struct Globals {
  view_proj : mat4x4<f32>,
  cam       : vec4<f32>,
  key       : vec4<f32>,
  fill      : vec4<f32>,
  p0        : vec4<f32>,   // time, rise, pop, thickness
  p1        : vec4<f32>,   // cols, rows, cell, encode_srgb
  amb       : vec4<f32>,   // sky rgb, ground bounce
  p2        : vec4<f32>,   // green cut, teal cut, ghost alpha, ghost spec boost
  p5        : vec4<f32>,   // blue cut
  p3        : vec4<f32>,   // halo pad, glow radius, glow strength, shadow strength
  p4        : vec4<f32>,   // half extent, corner radius, shadow softness, max offset
  c_dead    : vec4<f32>,
  c_live    : vec4<f32>,
  c_green   : vec4<f32>,
  c_teal    : vec4<f32>,
  c_blue    : vec4<f32>,
};

@group(0) @binding(0) var<uniform> g : Globals;
@group(1) @binding(0) var prop_tex : texture_2d<f32>;
@group(1) @binding(1) var prop_sampler : sampler;

const SPRING_DECAY : f32 = SPRING_DECAY_LIT;
const SPRING_OMEGA : f32 = SPRING_OMEGA_LIT;
const BIRTH_FADE   : f32 = BIRTH_FADE_LIT;
const DEATH_FADE   : f32 = DEATH_FADE_LIT;
const TILE_SLIDE_SECS : f32 = TILE_SLIDE_SECS_LIT;
const SPIN_SECS    : f32 = SPIN_SECS_LIT;
const SPIN_LIFT    : f32 = SPIN_LIFT_LIT;
const TAU          : f32 = 6.28318531;
const PI           : f32 = 3.14159265;

fn hash11(x : f32) -> f32 {
  var p = fract(x * 0.1031);
  p = p * (p + 33.33);
  p = p * (p + p);
  return fract(p);
}

/// Stable per-cell hash: a tile's colour is fixed for the life of the grid. Integer
/// mixing rather than the usual fract() tricks, which bias badly on small grids and
/// visibly skew the colour split. Mirrored by `cell_hash` in Rust and tested there.
fn hash_cell(c : vec2<f32>) -> f32 {
  var h : u32 = u32(c.x) * 374761393u + u32(c.y) * 668265263u;
  h = (h ^ (h >> 13u)) * 1274126177u;
  h = h ^ (h >> 16u);
  return f32(h) * (1.0 / 4294967296.0);
}

/// Which of the three live colours this cell wears, fixed by its coordinates.
/// Explicit palette entry, for anything that moves between cells and so cannot take
/// its colour from where it happens to be standing.
fn palette_color(i : f32) -> vec3<f32> {
  if (i >= 2.5) { return g.c_blue.rgb; }
  if (i >= 1.5) { return g.c_teal.rgb; }
  if (i >= 0.5) { return g.c_green.rgb; }
  return g.c_live.rgb;
}

/// Which palette entry a cell's hash lands on, before the isolation rule.
fn base_category(cell : vec2<f32>) -> f32 {
  let h = hash_cell(cell);
  if (h > g.p5.x) { return 3.0; }
  if (h > g.p2.y) { return 2.0; }
  if (h > g.p2.x) { return 1.0; }
  return 0.0;
}

/// Mirrors `cell_category` in Rust. Green and teal may sit next to each other, but a
/// blue only keeps its colour if none of its eight neighbours is coloured at all —
/// eight extra hashes, and only for the 8% of cells that come up blue to begin with.
fn live_color(cell : vec2<f32>, pal : f32) -> vec3<f32> {
  if (pal >= 0.0) { return palette_color(pal); }
  var cat = base_category(cell);
  if (cat > 2.5) {
    for (var dy = -1.0; dy <= 1.0; dy += 1.0) {
      for (var dx = -1.0; dx <= 1.0; dx += 1.0) {
        if ((dx != 0.0 || dy != 0.0)
            && cell.x + dx >= 0.0 && cell.y + dy >= 0.0
            && base_category(cell + vec2<f32>(dx, dy)) > 0.5) {
          cat = 0.0;
        }
      }
    }
  }
  return palette_color(cat);
}

/// Where a tile is in its transition, shared by the tile and halo passes so the two
/// never disagree about a tile's height or how far along its fade it is.
struct Anim {
  on     : f32,   // 0 = off, 1 = on, smoothly between during a change
  spring : f32,   // +1 the instant it flips, ringing down to 0
  dz     : f32,   // height above the floor plane
  jitter : f32,
  angle  : f32,   // rotation about the tile's own vertical axis
};

/// Away fast, then hard onto the mark. No overshoot: the tile has to arrive flush
/// with the grid, and a wobble at the end reads as slop rather than as bounce.
fn ease_out_quart(p : f32) -> f32 {
  let q = 1.0 - p;
  return 1.0 - q * q * q * q;
}

fn tile_anim(state : vec2<f32>, t : f32, spin : vec2<f32>, cell : vec2<f32>) -> Anim {
  let seed = cell.x + cell.y * g.p1.x;
  let r1 = hash11(seed + 0.5);
  let r2 = hash11(seed * 1.7 + 11.3);

  // Switching on and switching off are deliberately asymmetric: a tile springs into
  // existence, but simply fades away. `birth` is 1 for off->on, 0 for on->off, and 0
  // for a tile that has settled either way.
  let birth = clamp(state.x - state.y, 0.0, 1.0);
  let tau = max(g.p0.x - t, 0.0);
  let w   = SPRING_OMEGA * (0.94 + 0.12 * r1);

  var a : Anim;
  a.spring = exp(-SPRING_DECAY * tau) * cos(w * tau) * birth;
  let fade  = mix(DEATH_FADE, BIRTH_FADE, birth);
  let blend = smoothstep(0.0, 1.0, clamp(tau / fade, 0.0, 1.0));
  a.on = mix(state.y, state.x, blend);

  // A surviving tile occasionally tumbles toward the viewer: half a turn about the
  // screen-vertical axis, so one edge swings forward and the other drops away and you
  // see it edge-on halfway through. The tile's two faces are identical, so it lands
  // flush. The hop is keyed to the eased rotation rather than to time, so it peaks
  // exactly as the tile goes side-on.
  let sp = clamp(max(g.p0.x - spin.x, 0.0) / SPIN_SECS, 0.0, 1.0);
  let er = ease_out_quart(sp);
  a.angle = spin.y * PI * er;
  let hop = SPIN_LIFT * sin(PI * er);

  a.dz = g.p0.y * a.on + g.p0.z * a.spring + hop + (r2 - 0.5) * 0.8;
  a.jitter = 0.97 + 0.06 * r2;
  return a;
}

/// Turn about the screen-vertical axis, which is world +Y here. Rotating about Z
/// would spin the tile in the plane of the screen like a record; this tips it toward
/// the camera instead, which is what shows off the thickness.
fn spin_y(v : vec3<f32>, angle : f32) -> vec3<f32> {
  let c = cos(angle);
  let s = sin(angle);
  return vec3<f32>(v.x * c + v.z * s, v.y, -v.x * s + v.z * c);
}

fn cell_origin(cell : vec2<f32>) -> vec2<f32> {
  return vec2<f32>(
    (cell.x - (g.p1.x - 1.0) * 0.5) * g.p1.z,
    ((g.p1.y - 1.0) * 0.5 - cell.y) * g.p1.z,
  );
}

/// A paired birth owns one continuously solid tile whose destination is `cell` and
/// whose starting coordinate is `slide_from`. Ordinary instances store the same
/// coordinate in both fields and therefore stay exactly where they always did.
fn tile_slide_progress(t : f32, cell : vec2<f32>, slide_from : vec2<f32>) -> f32 {
  if (distance(cell, slide_from) < 0.1) { return 1.0; }
  let age = max(g.p0.x - t, 0.0);
  return smoothstep(0.0, 1.0, clamp(age / TILE_SLIDE_SECS, 0.0, 1.0));
}

fn tile_animated_cell(t : f32, cell : vec2<f32>, slide_from : vec2<f32>) -> vec2<f32> {
  return mix(slide_from, cell, tile_slide_progress(t, cell, slide_from));
}

fn tile_animated_color(
  t : f32,
  cell : vec2<f32>,
  slide_from : vec2<f32>,
  pal : f32,
) -> vec3<f32> {
  let p = tile_slide_progress(t, cell, slide_from);
  return mix(live_color(slide_from, pal), live_color(cell, pal), p);
}

fn lin_to_srgb(c : vec3<f32>) -> vec3<f32> {
  let lo = c * 12.92;
  let hi = 1.055 * pow(max(c, vec3<f32>(0.0)), vec3<f32>(1.0 / 2.4)) - 0.055;
  return select(hi, lo, c <= vec3<f32>(0.0031308));
}

// ---------------------------------------------------------------------------
// Props: free-standing models carried by critters
// ---------------------------------------------------------------------------

struct PIn {
  @location(0) pos   : vec3<f32>,
  @location(1) nrm   : vec3<f32>,
  @location(2) col   : vec3<f32>,
  @location(3) ipos  : vec3<f32>,
  @location(4) alpha : f32,
  @location(5) scale : vec3<f32>,
  @location(6) rot   : vec3<f32>,
  @location(7) tint  : vec3<f32>,
  @location(8) uv    : vec2<f32>,
};

struct POut {
  @builtin(position) clip : vec4<f32>,
  @location(0) wpos  : vec3<f32>,
  @location(1) nrm   : vec3<f32>,
  @location(2) col   : vec3<f32>,
  @location(3) alpha : f32,
  @location(4) uv    : vec2<f32>,
};

/// Roll about +X, then pitch about +Y, then yaw about +Z. For a model built nose-first
/// along +X that reads as bank, climb and turn, in that order.
fn rot_xyz(v : vec3<f32>, r : vec3<f32>) -> vec3<f32> {
  var p = v;
  let cx = cos(r.x); let sx = sin(r.x);
  p = vec3<f32>(p.x, p.y * cx - p.z * sx, p.y * sx + p.z * cx);
  let cy = cos(r.y); let sy = sin(r.y);
  p = vec3<f32>(p.x * cy + p.z * sy, p.y, -p.x * sy + p.z * cy);
  let cz = cos(r.z); let sz = sin(r.z);
  return vec3<f32>(p.x * cz - p.y * sz, p.x * sz + p.y * cz, p.z);
}

@vertex
fn vs_prop(in : PIn) -> POut {
  let wp = rot_xyz(in.pos * in.scale, in.rot) + in.ipos;
  var out : POut;
  out.clip = g.view_proj * vec4<f32>(wp, 1.0);
  out.wpos = wp;
  // Non-uniform scale skews normals, so undo it before rotating.
  out.nrm = rot_xyz(normalize(in.nrm / max(in.scale, vec3<f32>(1e-4))), in.rot);
  out.col   = in.col * in.tint;
  out.alpha = in.alpha;
  out.uv    = in.uv;
  return out;
}

/// Expanded back-face shell for the distant marshmallow sailor. The outline is
/// generated from the fully posed 3D surface, so arms, feet and the camera turn all
/// retain a silhouette even where pale foam meets pale fog.
@vertex
fn vs_distant_outline(in : PIn) -> POut {
  let n = rot_xyz(normalize(in.nrm / max(in.scale, vec3<f32>(1e-4))), in.rot);
  let wp = rot_xyz(in.pos * in.scale, in.rot) + in.ipos
         + n * DISTANT_MARSHMALLOW_OUTLINE_PX_LIT;
  var out : POut;
  out.clip = g.view_proj * vec4<f32>(wp, 1.0);
  out.wpos = wp;
  out.nrm = n;
  out.col = vec3<f32>(0.0);
  out.alpha = in.alpha;
  out.uv = in.uv;
  return out;
}

@fragment
fn fs_distant_outline(in : POut) -> @location(0) vec4<f32> {
  let alpha = in.alpha * 0.90;
  var rim = vec3<f32>(0.055, 0.082, 0.090);
  if (g.p1.w > 0.5) { rim = lin_to_srgb(rim); }
  return vec4<f32>(rim * alpha, alpha);
}

@fragment
fn fs_prop(in : POut) -> @location(0) vec4<f32> {
  let lit = light_surface(normalize(in.nrm), in.wpos, in.col);
  let surface = lit.body + lit.spec;
  var o = surface / (1.0 + max(surface - vec3<f32>(1.0), vec3<f32>(0.0)));
  o = clamp(o, vec3<f32>(0.0), vec3<f32>(1.0));
  if (g.p1.w > 0.5) { o = lin_to_srgb(o); }
  // Premultiplied after encoding, matching the tiles.
  return vec4<f32>(o * in.alpha, in.alpha);
}

/// Flat and unlit: a hot core, or a stroke of line art, rather than a lit surface. Just
/// enough normal-dependent shading to keep facets readable.
@fragment
fn fs_unlit(in : POut) -> @location(0) vec4<f32> {
  let n = normalize(in.nrm);
  let v = normalize(g.cam.xyz - in.wpos);
  let facing = 0.72 + 0.28 * max(dot(n, v), 0.0);
  var o = clamp(in.col * facing, vec3<f32>(0.0), vec3<f32>(1.0));
  if (g.p1.w > 0.5) { o = lin_to_srgb(o); }
  return vec4<f32>(o * in.alpha, in.alpha);
}

/// Generated sprite art is authored in sRGB and uploaded through an sRGB texture, so
/// the sampler hands us linear colour. Preserve its detail, then encode only when the
/// surface itself is a raw non-sRGB target.
@fragment
fn fs_textured(in : POut) -> @location(0) vec4<f32> {
  let texel = textureSample(prop_tex, prop_sampler, in.uv);
  let alpha = texel.a * in.alpha;
  if (alpha < 0.01) { discard; }
  var o = clamp(texel.rgb * in.col, vec3<f32>(0.0), vec3<f32>(1.0));
  if (g.p1.w > 0.5) { o = lin_to_srgb(o); }
  return vec4<f32>(o * alpha, alpha);
}

/// Tux is first rasterized into a canvas-sized layer after all of his animation
/// transforms have been applied. This source pass is intentionally sharp; fog is a
/// post-process on the completed layer, not a filter trapped inside the sprite quad.
@fragment
fn fs_distant_source(in : POut) -> @location(0) vec4<f32> {
  let texel = textureSample(prop_tex, prop_sampler, in.uv);
  let alpha = texel.a * in.alpha;
  if (alpha < 0.01) { discard; }
  var o = clamp(texel.rgb * in.col, vec3<f32>(0.0), vec3<f32>(1.0));
  if (g.p1.w > 0.5) { o = lin_to_srgb(o); }
  return vec4<f32>(o * alpha, alpha);
}

struct DistantLayerOut {
  @builtin(position) clip : vec4<f32>,
  @location(0) uv : vec2<f32>,
};

/// One oversized triangle covers the output. Its UVs read the half-resolution
/// offscreen layer in screen space, so filtering continues through transparent pixels
/// around the animated silhouette instead of stopping at a moving rectangle.
@vertex
fn vs_distant_composite(@builtin(vertex_index) vertex_index : u32) -> DistantLayerOut {
  var positions = array<vec2<f32>, 3>(
    vec2<f32>(-1.0, -1.0),
    vec2<f32>( 3.0, -1.0),
    vec2<f32>(-1.0,  3.0),
  );
  let p = positions[vertex_index];
  var out : DistantLayerOut;
  out.clip = vec4<f32>(p, 0.0, 1.0);
  out.uv = p * vec2<f32>(0.5, -0.5) + vec2<f32>(0.5);
  return out;
}

/// Blur the already-animated full-canvas layer, then let the sampler's linear
/// filtering upscale it. The source and destination use the same colour format, so
/// these premultiplied values can pass straight through on both raw and sRGB targets.
@fragment
fn fs_distant_composite(in : DistantLayerOut) -> @location(0) vec4<f32> {
  let dims = vec2<f32>(textureDimensions(prop_tex));
  let step = vec2<f32>(DISTANT_TUX_BLUR_LAYER_PX_LIT) / dims;
  var premul = vec4<f32>(0.0);
  var total = 0.0;
  for (var iy = -2; iy <= 2; iy += 1) {
    for (var ix = -2; ix <= 2; ix += 1) {
      let d = vec2<f32>(f32(ix), f32(iy));
      let weight = exp(-0.5 * dot(d, d));
      let sample = textureSample(prop_tex, prop_sampler, in.uv + d * step);
      // The source target already contains premultiplied colour. Multiplying its RGB
      // by alpha again would square per-visitor opacity before the blur.
      premul += sample * weight;
      total += weight;
    }
  }
  premul /= total;
  let alpha = premul.a * DISTANT_LAYER_OPACITY_LIT;
  if (alpha < 0.002) { discard; }
  return vec4<f32>(premul.rgb * DISTANT_LAYER_OPACITY_LIT, alpha);
}

// ---------------------------------------------------------------------------
// Halo: an even colour bloom radiating from each live tile
// ---------------------------------------------------------------------------

struct HIn {
  @location(0) corner : vec2<f32>,
  @location(2) cell   : vec2<f32>,
  @location(3) state  : vec2<f32>,
  @location(4) t      : f32,
  @location(5) spin   : vec2<f32>,
  @location(6) pal    : f32,
  @location(7) slide_from : vec2<f32>,
};

struct HOut {
  @builtin(position) clip : vec4<f32>,
  @location(0) local : vec2<f32>,  // pixels from the tile centre, in the halo's plane
  @location(1) tint  : vec3<f32>,
  @location(2) on    : f32,
};

@vertex
fn vs_halo(in : HIn) -> HOut {
  let a   = tile_anim(in.state, in.t, in.spin, in.cell);
  let o   = cell_origin(tile_animated_cell(in.t, in.cell, in.slide_from));
  let ext = g.p4.x + g.p3.x;
  let local = in.corner * ext;

  var out : HOut;
  // Just above the floor, under the tiles, and centred on the cell — the bloom is
  // light in the air around the tile, so it does not track the tile's own height.
  out.clip = g.view_proj * vec4<f32>(o.x + local.x, o.y + local.y, 1.0, 1.0);
  out.local = local;

  // Neon: the tile's hue taken up to full brightness, not a pale mix of its colour.
  // The halo washes the page toward this rather than adding to it — on a light
  // background an additive glow can only brighten toward white, so a teal tile would
  // get a white aura. But washing toward the colour itself would *darken* the page,
  // since #589864 and #3e9993 are both darker than #e8e8e8. Normalising the hue and then
  // pulling white toward it gives something brighter than the page that still carries
  // the colour. Grey tiles normalise to white and simply glow white.
  let c = tile_animated_color(in.t, in.cell, in.slide_from, in.pal);
  let peak = max(max(c.r, c.g), c.b);
  var tint = mix(vec3<f32>(1.0), c / max(peak, 1e-3), 0.35);
  if (g.p1.w > 0.5) { tint = lin_to_srgb(tint); }
  out.tint = tint;

  // Flare a touch as the tile rides forward, so the pop reads in the halo too.
  out.on = clamp(a.on, 0.0, 1.0) * (1.0 + 0.22 * max(a.spring, 0.0));
  return out;
}

fn sd_round_box(p : vec2<f32>, b : f32, r : f32) -> f32 {
  let q = abs(p) - vec2<f32>(b - r);
  return length(max(q, vec2<f32>(0.0))) + min(max(q.x, q.y), 0.0) - r;
}

@fragment
fn fs_halo(in : HOut) -> @location(0) vec4<f32> {
  // Distance outward from the tile's own silhouette, so the bloom hugs the rounded
  // rectangle evenly on every side instead of pooling at the corners.
  let d = max(sd_round_box(in.local, g.p4.x, g.p4.y), 0.0);

  // Two falloffs stacked: a bright core right at the edge and a much wider haze that
  // spreads into the page. Windowed to reach exactly zero at the quad's rim so the
  // halo never shows its own rectangular edge.
  let core = exp(-d / g.p3.y);
  let haze = exp(-d / (g.p3.y * g.p3.w));
  let edge = 1.0 - smoothstep(g.p3.x * 0.45, g.p3.x, d);
  let gl   = (core * 0.5 + haze * 0.5) * edge * g.p3.z * in.on;

  // Premultiplied: the page is lerped toward the tint by the coverage.
  return vec4<f32>(in.tint * gl, clamp(gl, 0.0, 1.0));
}

// ---------------------------------------------------------------------------
// Tiles
// ---------------------------------------------------------------------------

struct VIn {
  @location(0) pos   : vec3<f32>,
  @location(1) nrm   : vec3<f32>,
  @location(2) cell  : vec2<f32>,
  @location(3) state : vec2<f32>,
  @location(4) t     : f32,
  @location(5) spin  : vec2<f32>,
  @location(6) pal   : f32,
  @location(7) slide_from : vec2<f32>,
};

struct VOut {
  @builtin(position) clip : vec4<f32>,
  @location(0) wpos  : vec3<f32>,
  @location(1) nrm   : vec3<f32>,
  @location(2) col   : vec3<f32>,
  @location(3) extra : vec4<f32>,  // local height (AO), jitter, pop lift, on-ness
};

@vertex
fn vs(in : VIn) -> VOut {
  let a = tile_anim(in.state, in.t, in.spin, in.cell);
  let o = cell_origin(tile_animated_cell(in.t, in.cell, in.slide_from));

  // A rigid turn about the tile's own mid-plane, so it pivots in place rather than
  // swinging around its base. The normals come along too, or the lighting would stay
  // stuck to the grid while the geometry moves.
  let mid = g.p0.w * 0.5;
  let rp = spin_y(vec3<f32>(in.pos.x, in.pos.y, in.pos.z - mid), a.angle);
  let rn = spin_y(in.nrm, a.angle);

  var out : VOut;
  let wp = vec3<f32>(rp.x + o.x, rp.y + o.y, rp.z + mid + a.dz);
  out.wpos = wp;
  out.nrm  = rn;
  out.clip = g.view_proj * vec4<f32>(wp, 1.0);
  out.col  = mix(
    g.c_dead.rgb,
    tile_animated_color(in.t, in.cell, in.slide_from, in.pal),
    a.on,
  );
  out.extra = vec4<f32>(in.pos.z / g.p0.w, a.jitter, clamp(a.spring, -0.7, 1.0), a.on);
  return out;
}

fn d_ggx(ndh : f32, a : f32) -> f32 {
  let a2 = a * a;
  let d  = ndh * ndh * (a2 - 1.0) + 1.0;
  return a2 / max(PI * d * d, 1e-6);
}

fn v_smith(ndl : f32, ndv : f32, a : f32) -> f32 {
  let k  = a * 0.5;
  let gv = ndv * (1.0 - k) + k;
  let gl = ndl * (1.0 - k) + k;
  return 0.25 / max(gv * gl, 1e-5);
}

/// Diffuse body and specular kept apart, because a tile fading out thins its body
/// while still reflecting like glass. Shared by the tiles and by prop models so both
/// sit under exactly the same light.
struct Lit {
  body : vec3<f32>,
  spec : vec3<f32>,
};

fn light_surface(n : vec3<f32>, wpos : vec3<f32>, albedo : vec3<f32>) -> Lit {
  let v = normalize(g.cam.xyz - wpos);

  // Broad hemispheric ambient does most of the work — the field stays evenly lit.
  let hemi = mix(g.amb.rgb * g.amb.w, g.amb.rgb, n.z * 0.5 + 0.5);

  // One key light drifting high above the plane gives direction and a moving sheen.
  let ld    = g.key.xyz - wpos;
  let dist  = length(ld);
  let l     = ld / max(dist, 1e-4);
  let atten = 1.0 / (1.0 + pow(dist / 2400.0, 2.0));
  let ndl   = max(dot(n, l), 0.0);
  let keyc  = vec3<f32>(1.0, 0.985, 0.95) * g.key.w * atten;

  let ndf   = max(dot(n, g.fill.xyz), 0.0);
  let fillc = vec3<f32>(0.90, 0.95, 1.0) * g.fill.w;

  // Two specular lobes. Roughness opens up on faces pointing at the camera and
  // tightens toward grazing ones, so a surface carries a broad satin sheen while the
  // sharper glints ride its rolled edges. Neither is tight enough to alias against
  // the facets of the mesh.
  let tilt = 1.0 - abs(n.z);
  let h    = normalize(l + v);
  let ndh  = max(dot(n, h), 0.0);
  let ndv  = max(dot(n, v), 1e-4);
  let f0   = 0.055;
  let fres = f0 + (1.0 - f0) * pow(1.0 - max(dot(h, v), 0.0), 5.0);
  let rb    = mix(0.58, 0.34, tilt);
  let rt    = mix(0.28, 0.11, tilt);
  let broad = d_ggx(ndh, rb) * v_smith(ndl, ndv, rb) * ndl;
  let tight = d_ggx(ndh, rt) * v_smith(ndl, ndv, rt) * ndl;

  var out : Lit;
  out.body = albedo * (hemi + keyc * ndl + fillc * ndf);
  out.spec = (broad * 0.90 + tight * 0.85) * fres * keyc * 13.0;
  // Grazing sky reflection lifts rolled edges out of the flat faces.
  out.spec += pow(1.0 - min(dot(n, v), 1.0), 4.0) * 0.05 * g.amb.rgb;
  return out;
}

@fragment
fn fs(in : VOut) -> @location(0) vec4<f32> {
  let n = normalize(in.nrm);
  let on = in.extra.w;

  // Throw the fragment away entirely rather than drawing it at zero coverage. The
  // solid pass writes depth, so an invisible tile would still occlude everything
  // behind it — a hole in the scene with nothing in it. `discard` skips the depth
  // write too, which is the whole point.
  if (mix(g.p2.z, 1.0, on) < 0.01) {
    discard;
  }

  // Sides darken toward their base: a cheap stand-in for contact occlusion. The floor
  // is kept fairly high because the underside sits at zero too, and a tile tumbling
  // through shows it — too dark there and the flip reads as a shading fault.
  let ao = mix(0.82, 1.0, smoothstep(0.0, 0.9, in.extra.x));

  // A tile riding forward catches more light and sheds its neighbours' occlusion;
  // under a near-orthographic camera this reads far better than the ~2% of extra
  // scale the lift alone buys, and it is what turns the pops into visible ripples.
  let lift = 1.0 + 0.075 * in.extra.z;

  let lit  = light_surface(n, in.wpos, in.col);
  let body = lit.body * ao * in.extra.y * lift;
  let spec = lit.spec;

  // A fading tile turns to glass: the body thins out but it keeps reflecting, so
  // boost the specular as coverage drops instead of letting it snap off.
  let alpha = mix(g.p2.z, 1.0, on);
  let surface = body + spec * mix(g.p2.w, 1.0, on);

  // Only compress what would clip, so the base tones stay as authored.
  var o = surface / (1.0 + max(surface - vec3<f32>(1.0), vec3<f32>(0.0)));
  o = clamp(o, vec3<f32>(0.0), vec3<f32>(1.0));
  if (g.p1.w > 0.5) { o = lin_to_srgb(o); }

  // Premultiplied *after* encoding, so the One / OneMinusSrcAlpha blend is a plain
  // lerp in the same space the surface is stored in.
  return vec4<f32>(o * alpha, alpha);
}
"#;

fn shader_source() -> String {
    SHADER
        .replace("SPRING_DECAY_LIT", &format!("{:?}", SPRING_DECAY))
        .replace("SPRING_OMEGA_LIT", &format!("{:?}", SPRING_OMEGA))
        .replace("BIRTH_FADE_LIT", &format!("{:?}", BIRTH_FADE))
        .replace("DEATH_FADE_LIT", &format!("{:?}", DEATH_FADE))
        .replace("TILE_SLIDE_SECS_LIT", &format!("{:?}", TILE_SLIDE_SECS))
        .replace("SPIN_SECS_LIT", &format!("{:?}", SPIN_SECS))
        .replace("SPIN_LIFT_LIT", &format!("{:?}", SPIN_LIFT))
        .replace(
            "DISTANT_TUX_BLUR_LAYER_PX_LIT",
            &format!("{:?}", DISTANT_TUX_BLUR_LAYER_PX),
        )
        .replace(
            "DISTANT_LAYER_OPACITY_LIT",
            &format!("{:?}", DISTANT_LAYER_OPACITY),
        )
        .replace(
            "DISTANT_MARSHMALLOW_OUTLINE_PX_LIT",
            &format!("{:?}", DISTANT_MARSHMALLOW_OUTLINE_PX),
        )
}

/// Decode generated sprite art once while constructing the scene. PNG decoding stays
/// out of the frame loop, and `include_bytes` makes every asset available to native
/// previews, WebGPU, and the WebGL2 fallback.
fn sprite_pixels(bytes: &[u8], label: &str) -> (Vec<u8>, u32, u32) {
    let decoder = png::Decoder::new(std::io::Cursor::new(bytes));
    let mut reader = decoder
        .read_info()
        .unwrap_or_else(|_| panic!("decode {label} header"));
    let mut pixels = vec![0; reader.output_buffer_size()];
    let info = reader
        .next_frame(&mut pixels)
        .unwrap_or_else(|_| panic!("decode {label} pixels"));
    assert_eq!(info.bit_depth, png::BitDepth::Eight);
    assert_eq!(info.color_type, png::ColorType::Rgba);
    pixels.truncate(info.buffer_size());
    (pixels, info.width, info.height)
}

fn bee_body_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/bumblebee-body.png"),
        "bumblebee body",
    )
}

fn bee_legs_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/bumblebee-legs.png"),
        "bumblebee legs",
    )
}

fn tiki_mask_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/tiki-warrior-mask.png"),
        "tiki mask",
    )
}

fn astronaut_helmet_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/astronaut-helmet.png"),
        "astronaut helmet",
    )
}

fn noh_mask_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/noh-obeshimi-mask.png"),
        "Noh mask",
    )
}

fn monkey_mask_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(include_bytes!("../../img/monkey-mask.png"), "monkey mask")
}

fn comedy_tragedy_mask_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/comedy-tragedy-mask.png"),
        "comedy/tragedy mask",
    )
}

fn penguin_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(include_bytes!("../../img/penguin-draft.png"), "distant Tux")
}

fn godzilla_body_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/tiny-kaiju-body-tailcut.png"),
        "distant Godzilla body",
    )
}

fn godzilla_tail_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/tiny-kaiju-tail-tip.png"),
        "articulated distant Godzilla tail",
    )
}

fn godzilla_legs_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/tiny-kaiju-leg-parts.png"),
        "articulated distant Godzilla legs",
    )
}

fn fog_blob_pixels() -> (Vec<u8>, u32, u32) {
    sprite_pixels(
        include_bytes!("../../img/fog-blobs-atlas.png"),
        "foreground fog blobs",
    )
}

fn sprite_bind_group(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    layout: &wgpu::BindGroupLayout,
    label: &str,
    pixels: &[u8],
    width: u32,
    height: u32,
) -> wgpu::BindGroup {
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some(label),
        size: wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8UnormSrgb,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    });
    queue.write_texture(
        wgpu::TexelCopyTextureInfo {
            texture: &texture,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        pixels,
        wgpu::TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: Some(width * 4),
            rows_per_image: Some(height),
        },
        wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
    );
    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        label: Some(label),
        address_mode_u: wgpu::AddressMode::ClampToEdge,
        address_mode_v: wgpu::AddressMode::ClampToEdge,
        address_mode_w: wgpu::AddressMode::ClampToEdge,
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });
    device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some(label),
        layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(&sampler),
            },
        ],
    })
}

/// A full-screen scratch layer at half device resolution. Tux is drawn here after his
/// pose is known, leaving transparent room on every side for the subsequent Gaussian
/// blur. Linear sampling performs the final 2x upscale during compositing.
fn make_distant_layer(
    device: &wgpu::Device,
    format: wgpu::TextureFormat,
    layout: &wgpu::BindGroupLayout,
    width: u32,
    height: u32,
) -> (wgpu::TextureView, wgpu::BindGroup, wgpu::TextureView) {
    let size = wgpu::Extent3d {
        width: (width.max(1) + 1) / 2,
        height: (height.max(1) + 1) / 2,
        depth_or_array_layers: 1,
    };
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("distant-layer"),
        size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        label: Some("distant-layer"),
        address_mode_u: wgpu::AddressMode::ClampToEdge,
        address_mode_v: wgpu::AddressMode::ClampToEdge,
        address_mode_w: wgpu::AddressMode::ClampToEdge,
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });
    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("distant-layer"),
        layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(&sampler),
            },
        ],
    });
    let depth = device
        .create_texture(&wgpu::TextureDescriptor {
            label: Some("distant-layer-depth"),
            size,
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: DEPTH_FORMAT,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        })
        .create_view(&wgpu::TextureViewDescriptor::default());
    (view, bind_group, depth)
}

// ---------------------------------------------------------------------------
// Scene: everything needed to draw the field into any colour target
// ---------------------------------------------------------------------------

pub struct Scene {
    /// Solid tiles: depth-writing, drawn first.
    pipeline: wgpu::RenderPipeline,
    /// Translucent tiles: same shader, but they must not write depth or they would
    /// hide the live tiles behind them.
    pipeline_ghost: wgpu::RenderPipeline,
    /// Soft shadow and bloom on the floor, drawn under everything else.
    pipeline_halo: wgpu::RenderPipeline,
    quad_v: wgpu::Buffer,
    quad_i: wgpu::Buffer,
    /// Prop models carried by critters. Solid ones write depth; embers blend over
    /// everything without writing any.
    pipeline_prop: wgpu::RenderPipeline,
    pipeline_unlit: wgpu::RenderPipeline,
    pipeline_textured: wgpu::RenderPipeline,
    pipeline_distant_source: wgpu::RenderPipeline,
    pipeline_distant_outline: wgpu::RenderPipeline,
    pipeline_distant_prop: wgpu::RenderPipeline,
    pipeline_distant_composite: wgpu::RenderPipeline,
    texture_bgl: wgpu::BindGroupLayout,
    distant_layer: wgpu::TextureView,
    distant_layer_bind_group: wgpu::BindGroup,
    distant_depth: wgpu::TextureView,
    bee_bind_group: wgpu::BindGroup,
    bee_legs_bind_group: wgpu::BindGroup,
    tiki_bind_group: wgpu::BindGroup,
    astronaut_bind_group: wgpu::BindGroup,
    noh_bind_group: wgpu::BindGroup,
    monkey_bind_group: wgpu::BindGroup,
    comedy_tragedy_bind_group: wgpu::BindGroup,
    penguin_bind_group: wgpu::BindGroup,
    godzilla_body_bind_group: wgpu::BindGroup,
    godzilla_tail_bind_group: wgpu::BindGroup,
    godzilla_legs_bind_group: wgpu::BindGroup,
    fog_bind_group: wgpu::BindGroup,
    prop_v: wgpu::Buffer,
    prop_i: wgpu::Buffer,
    /// Where each model's indices start in the shared buffer, and how many. The
    /// indices are absolute, so draws never need a base vertex.
    prop_ranges: [(u32, u32); MODEL_COUNT],
    props: wgpu::Buffer,
    prop_cap: usize,
    /// Instances of each model in the prop buffer this frame, as (offset, count).
    prop_groups: [(u32, u32); MODEL_COUNT],
    bind_group: wgpu::BindGroup,
    globals: wgpu::Buffer,
    vbuf: wgpu::Buffer,
    ibuf: wgpu::Buffer,
    index_count: u32,
    instances: wgpu::Buffer,
    instance_cap: usize,
    depth: wgpu::TextureView,
    msaa: Option<wgpu::TextureView>,
    format: wgpu::TextureFormat,
    samples: u32,
    pub encode_srgb: bool,
    clear: wgpu::Color,
}

impl Scene {
    /// Picks 4x MSAA when the adapter reports support for it on `format`.
    pub fn preferred_samples(adapter: &wgpu::Adapter, format: wgpu::TextureFormat) -> u32 {
        if adapter
            .get_texture_format_features(format)
            .flags
            .sample_count_supported(4)
        {
            4
        } else {
            1
        }
    }

    pub fn new(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        format: wgpu::TextureFormat,
        samples: u32,
        width: u32,
        height: u32,
    ) -> Scene {
        let encode_srgb = !format.is_srgb();

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("tiles"),
            source: wgpu::ShaderSource::Wgsl(shader_source().into()),
        });

        let globals = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("globals"),
            size: std::mem::size_of::<Globals>() as u64,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let bgl = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX_FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }],
        });
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &bgl,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: globals.as_entire_binding(),
            }],
        });
        let layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[Some(&bgl)],
            immediate_size: 0,
        });

        let texture_bgl = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("sprite-texture-layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        view_dimension: wgpu::TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });
        let texture_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("sprite-pipeline-layout"),
            bind_group_layouts: &[Some(&bgl), Some(&texture_bgl)],
            immediate_size: 0,
        });
        let (bee_pixels, bee_width, bee_height) = bee_body_pixels();
        let bee_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "bumblebee-body",
            &bee_pixels,
            bee_width,
            bee_height,
        );
        let (bee_legs_pixels, bee_legs_width, bee_legs_height) = bee_legs_pixels();
        let bee_legs_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "bumblebee-legs",
            &bee_legs_pixels,
            bee_legs_width,
            bee_legs_height,
        );
        let (tiki_pixels, tiki_width, tiki_height) = tiki_mask_pixels();
        let tiki_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "tiki-warrior-mask",
            &tiki_pixels,
            tiki_width,
            tiki_height,
        );
        let (astronaut_pixels, astronaut_width, astronaut_height) = astronaut_helmet_pixels();
        let astronaut_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "astronaut-helmet",
            &astronaut_pixels,
            astronaut_width,
            astronaut_height,
        );
        let (noh_pixels, noh_width, noh_height) = noh_mask_pixels();
        let noh_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "noh-obeshimi-mask",
            &noh_pixels,
            noh_width,
            noh_height,
        );
        let (monkey_pixels, monkey_width, monkey_height) = monkey_mask_pixels();
        let monkey_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "monkey-mask",
            &monkey_pixels,
            monkey_width,
            monkey_height,
        );
        let (comedy_tragedy_pixels, comedy_tragedy_width, comedy_tragedy_height) =
            comedy_tragedy_mask_pixels();
        let comedy_tragedy_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "comedy-tragedy-mask",
            &comedy_tragedy_pixels,
            comedy_tragedy_width,
            comedy_tragedy_height,
        );
        let (penguin_pixels, penguin_width, penguin_height) = penguin_pixels();
        let penguin_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "distant-tux",
            &penguin_pixels,
            penguin_width,
            penguin_height,
        );
        let (godzilla_body_pixels, godzilla_body_width, godzilla_body_height) =
            godzilla_body_pixels();
        let godzilla_body_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "distant-godzilla-body",
            &godzilla_body_pixels,
            godzilla_body_width,
            godzilla_body_height,
        );
        let (godzilla_tail_pixels, godzilla_tail_width, godzilla_tail_height) =
            godzilla_tail_pixels();
        let godzilla_tail_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "distant-godzilla-tail",
            &godzilla_tail_pixels,
            godzilla_tail_width,
            godzilla_tail_height,
        );
        let (godzilla_legs_pixels, godzilla_legs_width, godzilla_legs_height) =
            godzilla_legs_pixels();
        let godzilla_legs_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "distant-godzilla-legs",
            &godzilla_legs_pixels,
            godzilla_legs_width,
            godzilla_legs_height,
        );
        let (fog_pixels, fog_width, fog_height) = fog_blob_pixels();
        let fog_bind_group = sprite_bind_group(
            device,
            queue,
            &texture_bgl,
            "temporary-blue-foreground-fog-atlas",
            &fog_pixels,
            fog_width,
            fog_height,
        );

        let (verts, indices) = build_tile_mesh();
        let vbuf = init_buffer(
            device,
            queue,
            bytemuck::cast_slice(&verts),
            wgpu::BufferUsages::VERTEX,
            "tile-verts",
        );
        let ibuf = init_buffer(
            device,
            queue,
            bytemuck::cast_slice(&indices),
            wgpu::BufferUsages::INDEX,
            "tile-indices",
        );

        let mut desc = wgpu::RenderPipelineDescriptor {
            label: Some("tiles"),
            layout: Some(&layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs"),
                compilation_options: Default::default(),
                buffers: &[
                    Some(wgpu::VertexBufferLayout {
                        array_stride: std::mem::size_of::<Vertex>() as u64,
                        step_mode: wgpu::VertexStepMode::Vertex,
                        attributes: &wgpu::vertex_attr_array![0 => Float32x3, 1 => Float32x3],
                    }),
                    Some(wgpu::VertexBufferLayout {
                        array_stride: std::mem::size_of::<Instance>() as u64,
                        step_mode: wgpu::VertexStepMode::Instance,
                        attributes: &wgpu::vertex_attr_array![
                            2 => Float32x2, 3 => Float32x2, 4 => Float32,
                            5 => Float32x2, 6 => Float32, 7 => Float32x2
                        ],
                    }),
                ],
            },
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: Some(wgpu::Face::Back),
                ..Default::default()
            },
            depth_stencil: Some(wgpu::DepthStencilState {
                format: DEPTH_FORMAT,
                depth_write_enabled: Some(true),
                depth_compare: Some(wgpu::CompareFunction::Less),
                stencil: Default::default(),
                bias: Default::default(),
            }),
            multisample: wgpu::MultisampleState {
                count: samples,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs"),
                compilation_options: Default::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    // Premultiplied source: the shader has already scaled colour by
                    // coverage, so this is a straight lerp toward what is behind.
                    blend: Some(wgpu::BlendState::PREMULTIPLIED_ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        };
        let pipeline = device.create_render_pipeline(&desc);

        desc.label = Some("tiles-ghost");
        if let Some(ds) = desc.depth_stencil.as_mut() {
            ds.depth_write_enabled = Some(false);
        }
        let pipeline_ghost = device.create_render_pipeline(&desc);

        let (quad, quad_idx) = build_halo_quad();
        let quad_v = init_buffer(
            device,
            queue,
            bytemuck::cast_slice(&quad),
            wgpu::BufferUsages::VERTEX,
            "halo-quad",
        );
        let quad_i = init_buffer(
            device,
            queue,
            bytemuck::cast_slice(&quad_idx),
            wgpu::BufferUsages::INDEX,
            "halo-quad-indices",
        );
        let pipeline_halo = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("halo"),
            layout: Some(&layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_halo"),
                compilation_options: Default::default(),
                buffers: &[
                    Some(wgpu::VertexBufferLayout {
                        array_stride: 8,
                        step_mode: wgpu::VertexStepMode::Vertex,
                        attributes: &wgpu::vertex_attr_array![0 => Float32x2],
                    }),
                    Some(wgpu::VertexBufferLayout {
                        array_stride: std::mem::size_of::<Instance>() as u64,
                        step_mode: wgpu::VertexStepMode::Instance,
                        attributes: &wgpu::vertex_attr_array![
                            2 => Float32x2, 3 => Float32x2, 4 => Float32,
                            5 => Float32x2, 6 => Float32, 7 => Float32x2
                        ],
                    }),
                ],
            },
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                ..Default::default()
            },
            depth_stencil: Some(wgpu::DepthStencilState {
                format: DEPTH_FORMAT,
                depth_write_enabled: Some(false),
                depth_compare: Some(wgpu::CompareFunction::Always),
                stencil: Default::default(),
                bias: Default::default(),
            }),
            multisample: wgpu::MultisampleState {
                count: samples,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_halo"),
                compilation_options: Default::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::PREMULTIPLIED_ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let (pverts, pindices, prop_ranges) = combine_prop_models();
        let prop_v = init_buffer(
            device,
            queue,
            bytemuck::cast_slice(&pverts),
            wgpu::BufferUsages::VERTEX,
            "prop-verts",
        );
        let prop_i = init_buffer(
            device,
            queue,
            bytemuck::cast_slice(&pindices),
            wgpu::BufferUsages::INDEX,
            "prop-indices",
        );
        let prop_cap = 16usize;
        let props = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("props"),
            size: (prop_cap * std::mem::size_of::<Prop>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        let mut prop_desc = wgpu::RenderPipelineDescriptor {
            label: Some("props"),
            layout: Some(&layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_prop"),
                compilation_options: Default::default(),
                buffers: &[
                    Some(wgpu::VertexBufferLayout {
                        array_stride: std::mem::size_of::<PropVertex>() as u64,
                        step_mode: wgpu::VertexStepMode::Vertex,
                        attributes: &wgpu::vertex_attr_array![
                            0 => Float32x3, 1 => Float32x3, 2 => Float32x3,
                            8 => Float32x2
                        ],
                    }),
                    Some(wgpu::VertexBufferLayout {
                        array_stride: std::mem::size_of::<Prop>() as u64,
                        step_mode: wgpu::VertexStepMode::Instance,
                        // Explicit offsets: the instance struct is padded to 16-byte
                        // rows, which a sequential attribute list would not skip.
                        attributes: &[
                            wgpu::VertexAttribute {
                                format: wgpu::VertexFormat::Float32x3,
                                offset: 0,
                                shader_location: 3,
                            },
                            wgpu::VertexAttribute {
                                format: wgpu::VertexFormat::Float32,
                                offset: 12,
                                shader_location: 4,
                            },
                            wgpu::VertexAttribute {
                                format: wgpu::VertexFormat::Float32x3,
                                offset: 16,
                                shader_location: 5,
                            },
                            wgpu::VertexAttribute {
                                format: wgpu::VertexFormat::Float32x3,
                                offset: 32,
                                shader_location: 6,
                            },
                            wgpu::VertexAttribute {
                                format: wgpu::VertexFormat::Float32x3,
                                offset: 48,
                                shader_location: 7,
                            },
                        ],
                    }),
                ],
            },
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                front_face: wgpu::FrontFace::Ccw,
                // Cartoon geometry with open shells here and there; cheaper to draw
                // both sides than to make every surface watertight.
                cull_mode: None,
                ..Default::default()
            },
            depth_stencil: Some(wgpu::DepthStencilState {
                format: DEPTH_FORMAT,
                depth_write_enabled: Some(true),
                depth_compare: Some(wgpu::CompareFunction::Less),
                stencil: Default::default(),
                bias: Default::default(),
            }),
            multisample: wgpu::MultisampleState {
                count: samples,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_prop"),
                compilation_options: Default::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::PREMULTIPLIED_ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        };
        let pipeline_prop = device.create_render_pipeline(&prop_desc);

        // Embers glow over whatever is behind them and never occlude it.
        prop_desc.label = Some("embers");
        if let Some(ds) = prop_desc.depth_stencil.as_mut() {
            ds.depth_write_enabled = Some(false);
        }
        if let Some(f) = prop_desc.fragment.as_mut() {
            f.entry_point = Some("fs_unlit");
        }
        let pipeline_unlit = device.create_render_pipeline(&prop_desc);

        prop_desc.label = Some("textured-sprites");
        prop_desc.layout = Some(&texture_layout);
        if let Some(f) = prop_desc.fragment.as_mut() {
            f.entry_point = Some("fs_textured");
        }
        let pipeline_textured = device.create_render_pipeline(&prop_desc);

        // The distant sprite is rasterized sharply into its own single-sampled,
        // half-resolution canvas. Blurring happens only after this animation-aware
        // source pass has finished.
        prop_desc.label = Some("distant-layer-source");
        // The shared pass now owns a depth attachment for procedural visitors. Flat
        // Tux/Godzilla sprites explicitly ignore it, preserving their authored layer
        // ordering while keeping the render-pass formats compatible.
        prop_desc.depth_stencil = Some(wgpu::DepthStencilState {
            format: DEPTH_FORMAT,
            depth_write_enabled: Some(false),
            depth_compare: Some(wgpu::CompareFunction::Always),
            stencil: Default::default(),
            bias: Default::default(),
        });
        prop_desc.multisample.count = 1;
        if let Some(f) = prop_desc.fragment.as_mut() {
            f.entry_point = Some("fs_distant_source");
        }
        let pipeline_distant_source = device.create_render_pipeline(&prop_desc);

        // Procedural distant models use soft normal shading without the ordinary
        // prop shader's bright plastic specular. That keeps the navy collar, pupils,
        // mouth and red neckerchief dark and saturated after fog compositing. They
        // render into the same scratch layer with a real depth buffer, so articulated
        // puffs occlude in true 3D before the completed animation is blurred.
        prop_desc.label = Some("distant-layer-procedural-model");
        prop_desc.layout = Some(&layout);
        prop_desc.depth_stencil = Some(wgpu::DepthStencilState {
            format: DEPTH_FORMAT,
            depth_write_enabled: Some(true),
            depth_compare: Some(wgpu::CompareFunction::Less),
            stencil: Default::default(),
            bias: Default::default(),
        });
        if let Some(f) = prop_desc.fragment.as_mut() {
            f.entry_point = Some("fs_unlit");
        }
        let pipeline_distant_prop = device.create_render_pipeline(&prop_desc);

        prop_desc.label = Some("distant-marshmallow-soft-outline");
        prop_desc.vertex.entry_point = Some("vs_distant_outline");
        // The generated superellipsoids use the opposite winding from the authored
        // prop meshes. Culling their nominal back faces therefore retains the actual
        // expanded rear shell and removes the front-facing fill.
        prop_desc.primitive.cull_mode = Some(wgpu::Face::Back);
        if let Some(f) = prop_desc.fragment.as_mut() {
            f.entry_point = Some("fs_distant_outline");
        }
        let pipeline_distant_outline = device.create_render_pipeline(&prop_desc);

        let pipeline_distant_composite =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("distant-layer-composite"),
                layout: Some(&texture_layout),
                vertex: wgpu::VertexState {
                    module: &shader,
                    entry_point: Some("vs_distant_composite"),
                    compilation_options: Default::default(),
                    buffers: &[],
                },
                primitive: wgpu::PrimitiveState {
                    topology: wgpu::PrimitiveTopology::TriangleList,
                    ..Default::default()
                },
                // The compositor draws inside the main pass, which owns a depth
                // attachment for the tiles. wgpu requires the formats to agree even
                // though this full-screen background neither tests nor writes depth.
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: DEPTH_FORMAT,
                    depth_write_enabled: Some(false),
                    depth_compare: Some(wgpu::CompareFunction::Always),
                    stencil: Default::default(),
                    bias: Default::default(),
                }),
                multisample: wgpu::MultisampleState {
                    count: samples,
                    mask: !0,
                    alpha_to_coverage_enabled: false,
                },
                fragment: Some(wgpu::FragmentState {
                    module: &shader,
                    entry_point: Some("fs_distant_composite"),
                    compilation_options: Default::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format,
                        blend: Some(wgpu::BlendState::PREMULTIPLIED_ALPHA_BLENDING),
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                }),
                multiview_mask: None,
                cache: None,
            });

        let (depth, msaa) = make_targets(device, format, samples, width, height);
        let (distant_layer, distant_layer_bind_group, distant_depth) =
            make_distant_layer(device, format, &texture_bgl, width, height);

        let clear = if encode_srgb {
            // We hand the target raw sRGB-encoded values, so the clear must be too.
            wgpu::Color {
                r: ((BG >> 16) & 0xff) as f64 / 255.0,
                g: ((BG >> 8) & 0xff) as f64 / 255.0,
                b: (BG & 0xff) as f64 / 255.0,
                a: 1.0,
            }
        } else {
            let bg = srgb_hex_to_linear(BG);
            wgpu::Color {
                r: bg[0] as f64,
                g: bg[1] as f64,
                b: bg[2] as f64,
                a: 1.0,
            }
        };

        let instance_cap = 4096usize;
        let instances = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("instances"),
            size: (instance_cap * std::mem::size_of::<Instance>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        Scene {
            pipeline,
            pipeline_ghost,
            pipeline_halo,
            quad_v,
            quad_i,
            pipeline_prop,
            pipeline_unlit,
            pipeline_textured,
            pipeline_distant_source,
            pipeline_distant_outline,
            pipeline_distant_prop,
            pipeline_distant_composite,
            texture_bgl,
            distant_layer,
            distant_layer_bind_group,
            distant_depth,
            bee_bind_group,
            bee_legs_bind_group,
            tiki_bind_group,
            astronaut_bind_group,
            noh_bind_group,
            monkey_bind_group,
            comedy_tragedy_bind_group,
            penguin_bind_group,
            godzilla_body_bind_group,
            godzilla_tail_bind_group,
            godzilla_legs_bind_group,
            fog_bind_group,
            prop_v,
            prop_i,
            prop_ranges,
            props,
            prop_cap,
            prop_groups: [(0, 0); MODEL_COUNT],
            bind_group,
            globals,
            vbuf,
            ibuf,
            index_count: indices.len() as u32,
            instances,
            instance_cap,
            depth,
            msaa,
            format,
            samples,
            encode_srgb,
            clear,
        }
    }

    pub fn resize(&mut self, device: &wgpu::Device, width: u32, height: u32) {
        let (d, m) = make_targets(device, self.format, self.samples, width, height);
        self.depth = d;
        self.msaa = m;
        let (layer, bind_group, distant_depth) =
            make_distant_layer(device, self.format, &self.texture_bgl, width, height);
        self.distant_layer = layer;
        self.distant_layer_bind_group = bind_group;
        self.distant_depth = distant_depth;
    }

    pub fn upload_instances(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        inst: &[Instance],
    ) {
        if inst.len() > self.instance_cap {
            let cap = inst.len().next_power_of_two();
            self.instances = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("instances"),
                size: (cap * std::mem::size_of::<Instance>()) as u64,
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
            self.instance_cap = cap;
        }
        queue.write_buffer(&self.instances, 0, bytemuck::cast_slice(inst));
    }

    /// Packs every model's instances into one buffer back to back and remembers where
    /// each group starts, so a model can be drawn with a single instance range.
    pub fn upload_props(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, sink: &PropSink) {
        let total = sink.total();
        self.prop_groups = [(0, 0); MODEL_COUNT];
        if total == 0 {
            return;
        }
        if total > self.prop_cap {
            let cap = total.next_power_of_two();
            self.props = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("props"),
                size: (cap * std::mem::size_of::<Prop>()) as u64,
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
            self.prop_cap = cap;
        }
        let mut packed: Vec<Prop> = Vec::with_capacity(total);
        for m in 0..MODEL_COUNT {
            let group = sink.group(m);
            self.prop_groups[m] = (packed.len() as u32, group.len() as u32);
            packed.extend_from_slice(group);
        }
        queue.write_buffer(&self.props, 0, bytemuck::cast_slice(&packed));
    }

    pub fn set_globals(&self, queue: &wgpu::Queue, g: &Globals) {
        queue.write_buffer(&self.globals, 0, bytemuck::bytes_of(g));
    }

    /// Draws the solid run, then the ghost run over it. `instances` must be the
    /// partitioned buffer from `Sim::draw_list`, with `solid` solid tiles at its head.
    pub fn draw(
        &self,
        encoder: &mut wgpu::CommandEncoder,
        target: &wgpu::TextureView,
        solid: u32,
        total: u32,
    ) {
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("distant-layer-source"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &self.distant_layer,
                    resolve_target: None,
                    depth_slice: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                    view: &self.distant_depth,
                    depth_ops: Some(wgpu::Operations {
                        load: wgpu::LoadOp::Clear(1.0),
                        store: wgpu::StoreOp::Discard,
                    }),
                    stencil_ops: None,
                }),
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            for (model, texture) in [
                (MODEL_PENGUIN, &self.penguin_bind_group),
                (MODEL_GODZILLA_REAR_LOWER, &self.godzilla_legs_bind_group),
                (MODEL_GODZILLA_FRONT_LOWER, &self.godzilla_legs_bind_group),
                (MODEL_GODZILLA_REAR_UPPER, &self.godzilla_legs_bind_group),
                (MODEL_GODZILLA_FRONT_UPPER, &self.godzilla_legs_bind_group),
                (MODEL_GODZILLA_TAIL, &self.godzilla_tail_bind_group),
                (MODEL_GODZILLA_BODY, &self.godzilla_body_bind_group),
            ] {
                let (distant_offset, distant_count) = self.prop_groups[model];
                if distant_count == 0 {
                    continue;
                }
                pass.set_pipeline(&self.pipeline_distant_source);
                pass.set_bind_group(0, &self.bind_group, &[]);
                pass.set_bind_group(1, texture, &[]);
                pass.set_vertex_buffer(0, self.prop_v.slice(..));
                pass.set_vertex_buffer(
                    1,
                    self.props.slice(instance_offset::<Prop>(distant_offset)..),
                );
                pass.set_index_buffer(self.prop_i.slice(..), wgpu::IndexFormat::Uint16);
                let (first, len) = self.prop_ranges[model];
                pass.draw_indexed(first..first + len, 0, 0..distant_count);
            }
            pass.set_bind_group(0, &self.bind_group, &[]);
            pass.set_vertex_buffer(0, self.prop_v.slice(..));
            pass.set_index_buffer(self.prop_i.slice(..), wgpu::IndexFormat::Uint16);
            // The expanded back-face shells write a narrow dark rim into the same
            // depth buffer. The ordinary model then fills the interior, leaving only
            // the silhouette to be softened by the full-layer blur.
            pass.set_pipeline(&self.pipeline_distant_outline);
            for model in [
                MODEL_MARSHMALLOW_PUFF,
                MODEL_MARSHMALLOW_BODY,
                MODEL_MARSHMALLOW_HEAD,
            ] {
                let (offset, count) = self.prop_groups[model];
                if count == 0 {
                    continue;
                }
                pass.set_vertex_buffer(1, self.props.slice(instance_offset::<Prop>(offset)..));
                let (first, len) = self.prop_ranges[model];
                pass.draw_indexed(first..first + len, 0, 0..count);
            }
            pass.set_pipeline(&self.pipeline_distant_prop);
            for model in [
                MODEL_MARSHMALLOW_PUFF,
                MODEL_MARSHMALLOW_BODY,
                MODEL_MARSHMALLOW_HEAD,
            ] {
                let (offset, count) = self.prop_groups[model];
                if count == 0 {
                    continue;
                }
                pass.set_vertex_buffer(1, self.props.slice(instance_offset::<Prop>(offset)..));
                let (first, len) = self.prop_ranges[model];
                pass.draw_indexed(first..first + len, 0, 0..count);
            }
        }

        let (view, resolve) = match &self.msaa {
            Some(ms) => (ms, Some(target)),
            None => (target, None),
        };
        let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("tiles"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view,
                resolve_target: resolve,
                depth_slice: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(self.clear),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: &self.depth,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: wgpu::StoreOp::Discard,
                }),
                stencil_ops: None,
            }),
            timestamp_writes: None,
            occlusion_query_set: None,
            multiview_mask: None,
        });
        pass.set_bind_group(0, &self.bind_group, &[]);

        // Blur the complete animated layer in screen space, upscale it, and composite
        // it over the gray clear colour before any tile or ordinary critter is drawn.
        pass.set_pipeline(&self.pipeline_distant_composite);
        pass.set_bind_group(1, &self.distant_layer_bind_group, &[]);
        pass.draw(0..3, 0..1);

        // The explicit fog plane is composited after the distant visitor but before
        // every halo, tile, and ordinary critter. Its sprites are exactly page-gray,
        // so they disappear over bare background and only veil what walks behind.
        pass.set_pipeline(&self.pipeline_textured);
        pass.set_bind_group(1, &self.fog_bind_group, &[]);
        pass.set_vertex_buffer(0, self.prop_v.slice(..));
        pass.set_index_buffer(self.prop_i.slice(..), wgpu::IndexFormat::Uint16);
        for model in MODEL_FOG_FIRST..=MODEL_FOG_LAST {
            let (offset, count) = self.prop_groups[model];
            if count == 0 {
                continue;
            }
            pass.set_vertex_buffer(1, self.props.slice(instance_offset::<Prop>(offset)..));
            let (first, len) = self.prop_ranges[model];
            pass.draw_indexed(first..first + len, 0, 0..count);
        }
        pass.set_vertex_buffer(1, self.instances.slice(..));

        // Halos first: they lay down shadow and bloom on the floor, and write no
        // depth, so the tiles then draw straight over their own contribution.
        if solid > 0 {
            pass.set_pipeline(&self.pipeline_halo);
            pass.set_vertex_buffer(0, self.quad_v.slice(..));
            pass.set_index_buffer(self.quad_i.slice(..), wgpu::IndexFormat::Uint16);
            pass.draw_indexed(0..6, 0, 0..solid);
        }

        pass.set_vertex_buffer(0, self.vbuf.slice(..));
        pass.set_index_buffer(self.ibuf.slice(..), wgpu::IndexFormat::Uint16);
        if solid > 0 {
            pass.set_pipeline(&self.pipeline);
            pass.draw_indexed(0..self.index_count, 0, 0..solid);
        }
        if self.prop_groups.iter().any(|(_, n)| *n > 0) {
            pass.set_vertex_buffer(0, self.prop_v.slice(..));
            pass.set_index_buffer(self.prop_i.slice(..), wgpu::IndexFormat::Uint16);
            for m in 0..MODEL_COUNT {
                if matches!(
                    m,
                    MODEL_PENGUIN
                        | MODEL_GODZILLA_REAR_UPPER
                        | MODEL_GODZILLA_REAR_LOWER
                        | MODEL_GODZILLA_FRONT_UPPER
                        | MODEL_GODZILLA_FRONT_LOWER
                        | MODEL_GODZILLA_TAIL
                        | MODEL_GODZILLA_BODY
                        | MODEL_MARSHMALLOW_PUFF
                        | MODEL_MARSHMALLOW_BODY
                        | MODEL_MARSHMALLOW_HEAD
                        | MODEL_FOG_FIRST..=MODEL_FOG_LAST
                ) {
                    continue;
                }
                let (offset, count) = self.prop_groups[m];
                if count == 0 {
                    continue;
                }
                if let Some(texture) = match m {
                    MODEL_BEE_LEG_FIRST..=MODEL_BEE_LEG_LAST => Some(&self.bee_legs_bind_group),
                    MODEL_BEE_BODY => Some(&self.bee_bind_group),
                    MODEL_TIKI_MASK => Some(&self.tiki_bind_group),
                    MODEL_ASTRONAUT_HELMET => Some(&self.astronaut_bind_group),
                    MODEL_NOH_MASK => Some(&self.noh_bind_group),
                    MODEL_MONKEY_MASK => Some(&self.monkey_bind_group),
                    MODEL_COMEDY_TRAGEDY_MASK => Some(&self.comedy_tragedy_bind_group),
                    _ => None,
                } {
                    pass.set_pipeline(&self.pipeline_textured);
                    pass.set_bind_group(1, texture, &[]);
                } else {
                    pass.set_pipeline(if MODEL_UNLIT[m] {
                        &self.pipeline_unlit
                    } else {
                        &self.pipeline_prop
                    });
                }
                // Rebind at a byte offset rather than passing a non-zero
                // `first_instance`: WebGL2 has no base-instance draw either, and the
                // draw call would be rejected outright.
                pass.set_vertex_buffer(1, self.props.slice(instance_offset::<Prop>(offset)..));
                let (first, len) = self.prop_ranges[m];
                pass.draw_indexed(first..first + len, 0, 0..count);
            }
            pass.set_vertex_buffer(1, self.instances.slice(..));
        }

        // Settled off-tiles are invisible at GHOST_ALPHA == 0, so skip them entirely.
        if total > solid && GHOST_ALPHA > 0.0 {
            pass.set_pipeline(&self.pipeline_ghost);
            pass.set_vertex_buffer(0, self.vbuf.slice(..));
            // Offset the binding rather than starting the instance range at `solid`,
            // for the same reason as the props above.
            pass.set_vertex_buffer(
                1,
                self.instances.slice(instance_offset::<Instance>(solid)..),
            );
            pass.set_index_buffer(self.ibuf.slice(..), wgpu::IndexFormat::Uint16);
            pass.draw_indexed(0..self.index_count, 0, 0..total - solid);
        }
    }
}

/// Byte offset of the nth instance, for binding a vertex buffer partway in.
fn instance_offset<T>(n: u32) -> u64 {
    n as u64 * std::mem::size_of::<T>() as u64
}

fn make_targets(
    device: &wgpu::Device,
    format: wgpu::TextureFormat,
    samples: u32,
    width: u32,
    height: u32,
) -> (wgpu::TextureView, Option<wgpu::TextureView>) {
    let size = wgpu::Extent3d {
        width: width.max(1),
        height: height.max(1),
        depth_or_array_layers: 1,
    };
    let depth = device
        .create_texture(&wgpu::TextureDescriptor {
            label: Some("depth"),
            size,
            mip_level_count: 1,
            sample_count: samples,
            dimension: wgpu::TextureDimension::D2,
            format: DEPTH_FORMAT,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        })
        .create_view(&wgpu::TextureViewDescriptor::default());
    let msaa = (samples > 1).then(|| {
        device
            .create_texture(&wgpu::TextureDescriptor {
                label: Some("msaa"),
                size,
                mip_level_count: 1,
                sample_count: samples,
                dimension: wgpu::TextureDimension::D2,
                format,
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                view_formats: &[],
            })
            .create_view(&wgpu::TextureViewDescriptor::default())
    });
    (depth, msaa)
}

fn init_buffer(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    data: &[u8],
    usage: wgpu::BufferUsages,
    label: &str,
) -> wgpu::Buffer {
    let buf = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some(label),
        size: ((data.len() + 3) & !3) as u64,
        usage: usage | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });
    queue.write_buffer(&buf, 0, data);
    buf
}

// ---------------------------------------------------------------------------
// Driver: advances the clock, steps the simulation, keeps the GPU in sync
// ---------------------------------------------------------------------------

/// Owns the wall-clock bookkeeping and drives the two simulations: the grid, which
/// runs ahead of the screen, and the visualisation, which chases it.
pub struct Driver {
    pub life: Life,
    pub viz: Viz,
    sim_clock: f64,
    gen: i64,
    inject_debt: f64,
    last_t: f64,
    /// The shader clock is measured from here so it stays small and f32-precise.
    epoch: f64,
    /// Simulation-clock time the present generation landed, for the phase a critter
    /// uses to pace itself between generations.
    gen_started: f64,
    /// Simulation-clock time the next critter is due, and how many have arrived.
    next_critter: f64,
    critters_sent: u32,
    /// Runtime spacing between arrivals; the quieter page modes stretch it.
    critter_every: f64,
    /// Distant-layer pacing, carried here so it survives the Viz rebuild a
    /// resize's `reseed` performs.
    distant_pause_scale: f32,
    distant_enabled: bool,
}

impl Driver {
    pub fn new(cols: usize, rows: usize, seed: u64, now: f64) -> Driver {
        let life = Life::new(cols, rows, seed);
        let viz = Viz::new(&life, seed ^ 0x5bf0_3635_ca8d_9e11);
        Driver {
            life,
            viz,
            sim_clock: 0.0,
            gen: 0,
            inject_debt: 0.0,
            last_t: now,
            epoch: now,
            gen_started: 0.0,
            next_critter: FIRST_CRITTER,
            critters_sent: 0,
            critter_every: CRITTER_EVERY,
            distant_pause_scale: 1.0,
            distant_enabled: true,
        }
    }

    /// Quieter mode: scheduled arrivals every `every` seconds, with distant
    /// crossings resting `pause_scale` times longer and opening on a rest instead
    /// of a walk.
    pub fn calm_visitors(&mut self, every: f64, pause_scale: f32) {
        self.critter_every = every;
        self.distant_pause_scale = pause_scale;
        self.viz.calm_distant(pause_scale);
    }

    /// Quietest mode: no scheduled critters and no distant crossings, ever. The
    /// fog stays.
    pub fn disable_visitors(&mut self) {
        self.next_critter = f64::INFINITY;
        self.distant_enabled = false;
        self.viz.disable_distant();
    }

    /// After a user pause, push the next arrival a full interval out so resuming
    /// doesn't fire one on the spot.
    pub fn rearm_critters(&mut self) {
        if self.next_critter.is_finite() {
            self.next_critter = self.sim_clock + self.critter_every;
        }
    }

    /// Development hook used by the headless renderer to study the complete mimic
    /// performance from frame zero without waiting for or biasing the live scheduler.
    #[doc(hidden)]
    pub fn preview_mimic(&mut self, seed: u64) -> bool {
        let mut rng = Rng::new(seed);
        let Some(mimic) = Mimic::new(&self.life.view(), &mut rng) else {
            return false;
        };
        self.viz.add_critter(Box::new(mimic));
        self.next_critter = f64::INFINITY;
        true
    }

    pub fn shader_clock(&self, now: f64) -> f32 {
        (now - self.epoch) as f32
    }

    pub fn reseed(&mut self, cols: usize, rows: usize) {
        let seed = self.life.rng.next_u64();
        self.life = Life::new(cols, rows, seed);
        self.viz = Viz::new(&self.life, seed ^ 0x5bf0_3635_ca8d_9e11);
        if self.distant_pause_scale != 1.0 {
            self.viz.calm_distant(self.distant_pause_scale);
        }
        if !self.distant_enabled {
            self.viz.disable_distant();
        }
    }

    /// Seconds a generation currently lasts.
    fn gen_secs(&self) -> f64 {
        let u = self.sim_clock % CYCLE;
        if FAST_SECS > 0.0 && u >= SLOW_SECS {
            1.0 / FAST_HZ
        } else {
            1.0 / SLOW_HZ
        }
    }

    /// Pick the clock back up after a pause without banking the time we missed.
    pub fn resume(&mut self, now: f64) {
        self.last_t = now;
    }

    /// Advance to `now` (seconds).
    ///
    /// The caller must re-upload the instance data afterwards, every frame. There is
    /// deliberately no "did anything change" answer to condition that on: the packed
    /// draw list is rebuilt each frame and its layout shifts as tiles retire, which
    /// happens between generations, not on them. Uploading only when the simulation
    /// stepped left the GPU holding an old layout while the draw call used fresh counts,
    /// so slots meant the wrong tiles and the whole field flickered.
    pub fn advance(&mut self, now: f64, rate: f64) {
        let gap = now - self.last_t;
        self.last_t = now;
        // The simulation is meant to pause while the page is away, not fast-forward
        // through everything it missed on the way back.
        let dt = if gap > RESUME_GAP {
            0.0
        } else {
            gap.clamp(0.0, MAX_FRAME_DT)
        };
        self.sim_clock += dt * rate;
        self.inject_debt += dt * rate * INJECT_HZ;

        let target = generation_at(self.sim_clock);
        let mut steps = target - self.gen;
        if steps > MAX_STEPS_PER_FRAME {
            self.gen = target - MAX_STEPS_PER_FRAME;
            steps = MAX_STEPS_PER_FRAME;
        }
        if steps > 0 {
            let shift = (now - 5.0) - self.epoch;
            if shift > 0.0 {
                self.viz.rebase(shift as f32);
                self.epoch += shift;
            }
            let clock = (now - self.epoch) as f32;
            for _ in 0..steps {
                let injections = self.inject_debt.floor().max(0.0) as u32;
                self.inject_debt -= injections as f64;
                self.life.advance(injections);
                self.viz.on_generation(&self.life.view(), clock);
                self.gen += 1;
            }
            self.gen_started = self.sim_clock;
        }

        if self.sim_clock >= self.next_critter {
            let mut rng = Rng::new(self.life.rng.next_u64());
            let view = self.life.view();
            let gen_secs = self.gen_secs();
            let phase = (((self.sim_clock - self.gen_started) / gen_secs) as f32).clamp(0.0, 1.0);
            // Choose before checking board-dependent eligibility. A visitor with no
            // valid plan becomes a rocket rather than delaying or silently losing the
            // arrival.
            let critter: Box<dyn Critter> =
                match scheduled_critter_kind(&mut rng, self.critters_sent) {
                    CritterKind::Rocket => Box::new(Rocket::new(&view, &mut rng)),
                    CritterKind::Walker => Walker::new(&view, &mut rng)
                        .map(|w| Box::new(w) as Box<dyn Critter>)
                        .unwrap_or_else(|| Box::new(Rocket::new(&view, &mut rng))),
                    CritterKind::Bee => Box::new(Bee::new(&view, &mut rng)),
                    CritterKind::Ufo => Ufo::new(&view, phase, &mut rng)
                        .map(|ufo| Box::new(ufo) as Box<dyn Critter>)
                        .unwrap_or_else(|| Box::new(Rocket::new(&view, &mut rng))),
                    CritterKind::Mimic => Mimic::new(&view, &mut rng)
                        .map(|mimic| Box::new(mimic) as Box<dyn Critter>)
                        .unwrap_or_else(|| Box::new(Rocket::new(&view, &mut rng))),
                };
            self.critters_sent += 1;
            self.viz.add_critter(critter);
            self.next_critter += self.critter_every;
        }

        // Critters run every frame, not every generation, so they always have
        // something to do even while the grid is holding still.
        let gen_secs = self.gen_secs();
        let ctx = CritterCtx {
            life: self.life.view(),
            dt: dt as f32,
            now: (now - self.epoch) as f32,
            phase: (((self.sim_clock - self.gen_started) / gen_secs) as f32).clamp(0.0, 1.0),
            gen_secs: gen_secs as f32,
            spinning: None,
        };
        self.viz.update(&ctx);
    }
}

// ---------------------------------------------------------------------------
// Web front-end
// ---------------------------------------------------------------------------

#[cfg(target_arch = "wasm32")]
mod web {
    use super::*;
    use std::cell::{Cell, RefCell};
    use std::rc::Rc;

    /// Boost ("what?" link) multiplier and duration.
    const BOOST_RATE: f64 = 4.0;
    const BOOST_SECS: f64 = 14.0;
    /// How long the pause button takes to fade the visitors out (and, on resuming,
    /// to bring the multiplier back so future visitors render).
    const PAUSE_FADE_SECS: f64 = 0.5;
    /// Rendering a background at 3x on a hidpi display is not worth the fill rate.
    const MAX_DPR: f64 = 2.0;
    use wasm_bindgen::prelude::*;
    use wasm_bindgen::JsCast;

    /// Keep the latest renderer failure inspectable without requiring a browser
    /// console. This is intentionally tiny and inert during healthy rendering.
    fn record_renderer_error(message: &str) {
        if let Some(win) = web_sys::window() {
            let _ = js_sys::Reflect::set(
                win.as_ref(),
                &JsValue::from_str("__conwayBgError"),
                &JsValue::from_str(message),
            );
        }
    }

    struct App {
        surface: wgpu::Surface<'static>,
        device: wgpu::Device,
        queue: wgpu::Queue,
        config: wgpu::SurfaceConfiguration,
        scene: Scene,
        driver: Driver,
        canvas: web_sys::HtmlCanvasElement,
        doc: web_sys::Document,
        /// True while the page is not being displayed, so the next visible frame can
        /// pick the clock up cleanly instead of banking the time away.
        paused: bool,
        css_w: f64,
        css_h: f64,
        dpr: f64,
        reduced_motion: bool,
        /// Set when something changed; under reduced motion nothing else redraws.
        needs_draw: bool,
        /// The page mode's speed multiplier, composed under any boost.
        mode_rate: f64,
        /// Wall-clock timestamp of the previous frame, for the pause fade, which
        /// runs on real time rather than the (pausable) simulation clock.
        last_ts: f64,
        /// 1 while running, easing to 0 across `PAUSE_FADE_SECS` when the user
        /// pauses; multiplied over every visitor's alpha.
        pause_fade: f64,
        /// Once the fade completes: the shader clock and light time captured at
        /// the freeze, held until unpaused so the still frame doesn't drift.
        frozen: Option<(f32, f32)>,
    }

    thread_local! {
        static BOOST_UNTIL: Cell<f64> = const { Cell::new(f64::NEG_INFINITY) };
        static USER_PAUSED: Cell<bool> = const { Cell::new(false) };
    }

    impl App {
        fn check_size(&mut self) {
            let win = web_sys::window().unwrap();
            let w = win
                .inner_width()
                .ok()
                .and_then(|v| v.as_f64())
                .unwrap_or(0.0);
            let h = win
                .inner_height()
                .ok()
                .and_then(|v| v.as_f64())
                .unwrap_or(0.0);
            let dpr = win.device_pixel_ratio().clamp(1.0, MAX_DPR);
            if (w - self.css_w).abs() < 0.5
                && (h - self.css_h).abs() < 0.5
                && (dpr - self.dpr).abs() < 1e-3
            {
                return;
            }
            self.css_w = w.max(1.0);
            self.css_h = h.max(1.0);
            self.dpr = dpr;

            let pw = (self.css_w * dpr).round().max(1.0) as u32;
            let ph = (self.css_h * dpr).round().max(1.0) as u32;
            self.canvas.set_width(pw);
            self.canvas.set_height(ph);
            self.config.width = pw;
            self.config.height = ph;
            self.surface.configure(&self.device, &self.config);
            self.scene.resize(&self.device, pw, ph);

            let (cols, rows) = grid_dims(self.css_w, self.css_h);
            if cols != self.driver.life.cols || rows != self.driver.life.rows {
                self.driver.reseed(cols, rows);
            }
            self.scene
                .upload_instances(&self.device, &self.queue, self.driver.viz.draw_list().0);
            self.needs_draw = true;
        }

        fn frame(&mut self, ts_ms: f64) {
            let now = ts_ms * 0.001;

            // requestAnimationFrame already stops while a tab is in the background, but
            // browsers also merely *throttle* it in some states, and a bfcache restore
            // resumes with the clock far ahead. Check directly, and on the way back
            // rebase the clock so nothing is replayed.
            if self.doc.hidden() {
                self.paused = true;
                return;
            }
            if self.paused {
                self.paused = false;
                self.driver.resume(now);
                self.last_ts = now;
            }

            self.check_size();

            let user_paused = USER_PAUSED.with(|p| p.get());
            let dt = (now - self.last_ts).clamp(0.0, MAX_FRAME_DT);
            self.last_ts = now;
            if user_paused {
                self.pause_fade = (self.pause_fade - dt / PAUSE_FADE_SECS).max(0.0);
            } else {
                if self.frozen.take().is_some() {
                    // Waking from the user pause: nothing missed is replayed, the
                    // field is clean even if a resize reseeded it mid-freeze, and
                    // the next arrival waits a full interval rather than firing
                    // on the spot.
                    self.driver.resume(now);
                    self.driver.viz.clear_critters();
                    self.driver.rearm_critters();
                }
                self.pause_fade = (self.pause_fade + dt / PAUSE_FADE_SECS).min(1.0);
            }

            if self.reduced_motion || self.frozen.is_some() {
                // Hold a single still frame: no stepping, and a fixed key light so the
                // sheen doesn't drift either. Redraw only when the viewport changes.
                if !self.needs_draw {
                    return;
                }
            } else {
                let rate = if BOOST_UNTIL.with(|b| now < b.get()) {
                    BOOST_RATE
                } else {
                    1.0
                } * self.mode_rate;
                self.driver.viz.set_visitor_alpha(self.pause_fade as f32);
                self.driver.advance(now, rate);
                if user_paused && self.pause_fade == 0.0 {
                    // The fade has finished: drop every visitor so unpausing opens
                    // on a clean field, then hold the clocks where they stopped.
                    // This frame's buffers were built at alpha zero, so nothing
                    // visible is being cut.
                    self.driver.viz.clear_critters();
                    self.frozen = Some((self.driver.shader_clock(now), now as f32));
                }
                self.scene.upload_instances(
                    &self.device,
                    &self.queue,
                    self.driver.viz.draw_list().0,
                );
                self.scene
                    .upload_props(&self.device, &self.queue, self.driver.viz.props());
            }
            self.needs_draw = false;

            let (shader_clock, light_time) = match self.frozen {
                Some(held) => held,
                None => (self.driver.shader_clock(now), now as f32),
            };
            let g = globals_for(
                self.driver.life.cols,
                self.driver.life.rows,
                self.config.width as f32 / self.config.height.max(1) as f32,
                self.css_h as f32,
                shader_clock,
                if self.reduced_motion { 0.0 } else { light_time },
                self.scene.encode_srgb,
            );
            self.scene.set_globals(&self.queue, &g);

            use wgpu::CurrentSurfaceTexture as Cst;
            let frame = match self.surface.get_current_texture() {
                Cst::Success(f) | Cst::Suboptimal(f) => f,
                Cst::Outdated | Cst::Lost => {
                    self.surface.configure(&self.device, &self.config);
                    return;
                }
                _ => return,
            };
            let view = frame
                .texture
                .create_view(&wgpu::TextureViewDescriptor::default());
            let mut enc = self
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
            let (list, solid) = self.driver.viz.draw_list();
            self.scene.draw(&mut enc, &view, solid, list.len() as u32);
            self.queue.submit(Some(enc.finish()));
            self.queue.present(frame);
        }
    }

    async fn build(canvas: web_sys::HtmlCanvasElement) -> Result<App, String> {
        let win = web_sys::window().ok_or("no window")?;
        let doc = win.document().ok_or("no document")?;
        let css_w = win
            .inner_width()
            .ok()
            .and_then(|v| v.as_f64())
            .unwrap_or(1280.0)
            .max(1.0);
        let css_h = win
            .inner_height()
            .ok()
            .and_then(|v| v.as_f64())
            .unwrap_or(800.0)
            .max(1.0);
        let dpr = win.device_pixel_ratio().clamp(1.0, MAX_DPR);
        let pw = (css_w * dpr).round() as u32;
        let ph = (css_h * dpr).round() as u32;
        canvas.set_width(pw);
        canvas.set_height(ph);

        let instance = wgpu::util::new_instance_with_webgpu_detection(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::BROWSER_WEBGPU | wgpu::Backends::GL,
            ..wgpu::InstanceDescriptor::new_without_display_handle()
        })
        .await;

        let surface = instance
            .create_surface(wgpu::SurfaceTarget::Canvas(canvas.clone()))
            .map_err(|e| format!("create_surface: {e}"))?;

        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                force_fallback_adapter: false,
                compatible_surface: Some(&surface),
                apply_limit_buckets: false,
            })
            .await
            .map_err(|e| format!("request_adapter: {e}"))?;

        let (device, queue) = adapter
            .request_device(&wgpu::DeviceDescriptor {
                label: Some("conwaybg"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::downlevel_webgl2_defaults()
                    .using_resolution(adapter.limits()),
                ..Default::default()
            })
            .await
            .map_err(|e| format!("request_device: {e}"))?;

        // Log GPU errors rather than letting them abort the module. The build uses
        // `panic = "abort"`, so an unhandled validation error takes the whole wasm
        // instance down and the animation frame loop with it — the page freezes for
        // good, with no way back short of a reload. A handler here degrades that to a
        // console warning and, at worst, a visual glitch.
        device.on_uncaptured_error(std::sync::Arc::new(|e| {
            let message = format!("conwaybg gpu: {e}");
            record_renderer_error(&message);
            web_sys::console::warn_1(&JsValue::from_str(&message));
        }));

        // Prefer a plain (non-sRGB) surface format and encode ourselves, so the
        // WebGPU and WebGL2 paths land on identical output.
        let caps = surface.get_capabilities(&adapter);
        let format = caps
            .formats
            .iter()
            .copied()
            .find(|f| !f.is_srgb())
            .unwrap_or(caps.formats[0]);
        let samples = Scene::preferred_samples(&adapter, format);
        let alpha_mode = if caps.alpha_modes.contains(&wgpu::CompositeAlphaMode::Opaque) {
            wgpu::CompositeAlphaMode::Opaque
        } else {
            caps.alpha_modes[0]
        };

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            color_space: wgpu::SurfaceColorSpace::Auto,
            width: pw,
            height: ph,
            present_mode: wgpu::PresentMode::Fifo,
            desired_maximum_frame_latency: 2,
            alpha_mode,
            view_formats: vec![],
        };
        surface.configure(&device, &config);

        let mut scene = Scene::new(&device, &queue, format, samples, pw, ph);

        let (cols, rows) = grid_dims(css_w, css_h);
        let showcase_mimic = win
            .location()
            .search()
            .map(|query| query.contains("showcase=mimic"))
            .unwrap_or(false);
        let seed = if showcase_mimic {
            0xC0FFEE_1234_5678
        } else {
            (js_sys::Math::random() * 9.0e15) as u64 ^ 0x9e37_79b9_7f4a_7c15
        };
        let now = win.performance().map(|p| p.now() * 0.001).unwrap_or(0.0);
        let mut driver = Driver::new(cols, rows, seed, now);
        if showcase_mimic {
            let _ = driver.preview_mimic(0x71_1e_c4_ab);
        }

        // background.js resolves the page's mode — inline script or ?bgmode=
        // override — onto this global before instantiating the module.
        let mode = js_sys::Reflect::get(win.as_ref(), &JsValue::from_str("conwayBgMode"))
            .ok()
            .and_then(|v| v.as_string())
            .unwrap_or_default();
        let mode_rate = match mode.as_str() {
            "fewcritters" => {
                driver.calm_visitors(CRITTER_EVERY * 4.0, 1.7);
                0.5
            }
            "nocritters" => {
                driver.disable_visitors();
                0.5
            }
            _ => 1.0,
        };
        scene.upload_instances(&device, &queue, driver.viz.draw_list().0);

        let reduced_motion = win
            .match_media("(prefers-reduced-motion: reduce)")
            .ok()
            .flatten()
            .map(|m| m.matches())
            .unwrap_or(false);

        Ok(App {
            surface,
            device,
            queue,
            config,
            scene,
            driver,
            canvas,
            doc,
            paused: false,
            css_w,
            css_h,
            dpr,
            reduced_motion,
            needs_draw: true,
            mode_rate,
            last_ts: now,
            pause_fade: 1.0,
            frozen: None,
        })
    }

    /// Temporarily fast-forward the simulation (wired to the "(what?)" link).
    #[wasm_bindgen]
    pub fn boost() {
        if let Some(p) = web_sys::window().and_then(|w| w.performance()) {
            BOOST_UNTIL.with(|b| b.set(p.now() * 0.001 + BOOST_SECS));
        }
    }

    /// Pause or resume the whole animation (wired to the pause button). Pausing
    /// fades the visitors out over half a second and then holds a still frame;
    /// resuming picks the clock back up without replaying the gap.
    #[wasm_bindgen]
    pub fn set_paused(paused: bool) {
        USER_PAUSED.with(|p| p.set(paused));
    }

    #[wasm_bindgen(start)]
    pub fn start() {
        wasm_bindgen_futures::spawn_local(async {
            if let Err(e) = run().await {
                record_renderer_error(&e);
                web_sys::console::warn_1(&JsValue::from_str(&format!("conwaybg disabled: {e}")));
            }
        });
    }

    async fn run() -> Result<(), String> {
        let win = web_sys::window().ok_or("no window")?;
        let doc = win.document().ok_or("no document")?;
        let canvas: web_sys::HtmlCanvasElement = doc
            .get_element_by_id("bgcanvas")
            .ok_or("no #bgcanvas element")?
            .dyn_into()
            .map_err(|_| "#bgcanvas is not a canvas")?;

        let app = Rc::new(RefCell::new(build(canvas.clone()).await?));
        app.borrow_mut()
            .frame(win.performance().map(|p| p.now()).unwrap_or(0.0));

        // Only reveal the canvas once a device is up and the first frame is drawn.
        // CSS then blends that completed frame linearly over 2.5 seconds; until this
        // handoff the plain #e8e8e8 body background stands in.
        let _ = canvas.style().set_property("opacity", "1");

        let f: Rc<RefCell<Option<Closure<dyn FnMut(f64)>>>> = Rc::new(RefCell::new(None));
        let g = f.clone();
        *g.borrow_mut() = Some(Closure::wrap(Box::new(move |ts: f64| {
            app.borrow_mut().frame(ts);
            if let (Some(w), Some(cb)) = (web_sys::window(), f.borrow().as_ref()) {
                let _ = w.request_animation_frame(cb.as_ref().unchecked_ref());
            }
        }) as Box<dyn FnMut(f64)>));

        win.request_animation_frame(g.borrow().as_ref().unwrap().as_ref().unchecked_ref())
            .map_err(|_| "request_animation_frame failed")?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// A `Life` with the given cells set and nothing else, its lookahead filled.
    fn planted(cols: usize, rows: usize, cells: &[(usize, usize)]) -> Life {
        let n = cols * rows;
        let mut life = Life {
            cols,
            rows,
            boards: vec![vec![false; n]; BOARDS],
            head: 0,
            scratch: vec![false; n],
            nb: vec![0; n],
            rng: Rng::new(1),
        };
        for &(x, y) in cells {
            life.boards[0][y * cols + x] = true;
        }
        // Fill the lookahead from the planted present, without injections.
        for i in 1..BOARDS {
            let prev = life.slot(i - 1);
            let src = life.boards[prev].clone();
            life.boards[i] = next_generation(&src, cols, rows);
        }
        life
    }

    /// Plain B3/S23 with hard edges, for cross-checking `Life::advance`.
    fn next_generation(src: &[bool], cols: usize, rows: usize) -> Vec<bool> {
        (0..cols * rows)
            .map(|i| {
                let (x, y) = ((i % cols) as isize, (i / cols) as isize);
                let mut n = 0;
                for dy in -1isize..=1 {
                    for dx in -1isize..=1 {
                        let (nx, ny) = (x + dx, y + dy);
                        if (dx != 0 || dy != 0)
                            && nx >= 0
                            && ny >= 0
                            && nx < cols as isize
                            && ny < rows as isize
                            && src[ny as usize * cols + nx as usize]
                        {
                            n += 1;
                        }
                    }
                }
                n == 3 || (n == 2 && src[i])
            })
            .collect()
    }

    fn live_cells(board: &[bool], cols: usize) -> Vec<(usize, usize)> {
        board
            .iter()
            .enumerate()
            .filter(|(_, b)| **b)
            .map(|(i, _)| (i % cols, i / cols))
            .collect()
    }

    /// A glider launched from an edge has to fly *onto* the screen. Mirroring any of
    /// the eight orientations would silently send it straight back out of the world.
    #[test]
    fn gliders_travel_inward() {
        // Inward direction for each edge: top moves down, left moves right, etc.
        let inward = [(0i32, 1i32), (0, -1), (1, 0), (-1, 0)];
        for edge in 0..4usize {
            for flip in [false, true] {
                let pat = glider_for(edge, flip);
                let mut cells = Vec::new();
                for (ry, bits) in pat.iter().enumerate() {
                    for rx in 0..3usize {
                        if bits & (0b100 >> rx) != 0 {
                            cells.push((10 + rx, 10 + ry));
                        }
                    }
                }
                let mut life = planted(24, 24, &cells);
                let origin = |l: &Life| {
                    live_cells(l.board(0), 24)
                        .into_iter()
                        .fold((99usize, 99usize), |a, c| (a.0.min(c.0), a.1.min(c.1)))
                };
                let start = origin(&life);

                // One full glider period translates it exactly one cell diagonally.
                for _ in 0..4 {
                    life.advance(0);
                }
                let end = origin(&life);
                let (dx, dy) = (end.0 as i32 - start.0 as i32, end.1 as i32 - start.1 as i32);

                assert_eq!(
                    (dx.abs(), dy.abs()),
                    (1, 1),
                    "edge {edge} flip {flip}: expected a one-cell diagonal hop, got ({dx},{dy})"
                );
                let (wx, wy) = inward[edge];
                assert!(
                    dx * wx + dy * wy > 0,
                    "edge {edge} flip {flip}: glider heads ({dx},{dy}), away from the screen"
                );
            }
        }
    }

    /// The world has hard edges: nothing may wrap from one side to the other.
    #[test]
    fn world_edges_do_not_wrap() {
        // A blinker hugging the left column would gain phantom neighbours from the
        // right column under wraparound and behave differently.
        let mut life = planted(12, 12, &[(0, 4), (0, 5), (0, 6)]);
        life.advance(0);
        let v = life.view();
        assert!(v.alive(0, 5, 0), "blinker should flip through (0,5)");
        assert!(v.alive(1, 5, 0), "blinker should flip through (1,5)");
        assert!(!v.alive(11, 5, 0), "blinker wrapped around the world");
    }

    /// Every board in the lookahead must be the exact generation it claims to be, and
    /// advancing must shift the whole window along by one. Critters plan against these
    /// boards, so a stale or misaligned one would quietly make their plans wrong.
    #[test]
    fn lookahead_holds_the_real_future() {
        let (cols, rows) = (20, 16);
        let mut life = planted(cols, rows, &[(5, 5), (6, 5), (7, 5), (7, 4), (6, 3)]);

        for _ in 0..12 {
            // Each board must follow from the one before it.
            for i in 1..=LOOKAHEAD {
                let expect = next_generation(life.board(i - 1), cols, rows);
                assert_eq!(
                    life.board(i),
                    &expect[..],
                    "board {i} generations ahead is not the successor of board {}",
                    i - 1
                );
            }
            // And the future must actually arrive: today's board(1) is tomorrow's
            // board(0). Injections would break this, so run without them.
            let promised = life.board(1).to_vec();
            life.advance(0);
            assert_eq!(
                life.board(0),
                &promised[..],
                "the generation that arrived is not the one that was promised"
            );
        }
    }

    /// Injections may reach at most one cell into the visible area.    /// Injections may reach at most one cell into the visible area.
    #[test]
    fn injections_stay_in_the_border_band() {
        let (cols, rows) = (40, 28);
        let mut life = planted(cols, rows, &[]);
        for _ in 0..4000 {
            life.scratch.iter_mut().for_each(|c| *c = false);
            life.inject();
            for y in 0..rows {
                for x in 0..cols {
                    if life.scratch[y * cols + x] {
                        let depth = x.min(y).min(cols - 1 - x).min(rows - 1 - y);
                        assert!(
                            depth < BAND,
                            "injection touched ({x},{y}), {depth} cells deep — past the band"
                        );
                    }
                }
            }
        }
    }

    /// The upload buffer must hold every cell exactly once, with all the solid tiles
    /// ahead of all the ghosts — the translucent pass depends on that partition — and
    /// any critter instances spliced into the solid run.
    #[test]
    fn draw_list_partitions_every_cell_once() {
        let (cols, rows) = (30, 22);
        let mut life = Life::new(cols, rows, 7);
        let mut viz = Viz::new(&life, 7);
        viz.add_critter(Box::new(TestCritter::default()));

        let mut t = 0.0f32;
        for step in 0..40 {
            let ctx = CritterCtx {
                life: life.view(),
                dt: 0.3,
                now: t,
                phase: 0.0,
                gen_secs: 3.0,
                spinning: None,
            };
            viz.update(&ctx);

            let (list, solid) = viz.draw_list();
            let critters = TestCritter::default().tiles();
            assert_eq!(
                list.len(),
                cols * rows + critters,
                "step {step}: lost instances"
            );

            let mut seen = vec![false; cols * rows];
            let mut from_critter = 0;
            for (i, inst) in list.iter().enumerate() {
                if inst.palette != PALETTE_FROM_CELL {
                    from_critter += 1;
                    assert!(
                        (i as u32) < solid,
                        "step {step}: a critter landed in the ghost run"
                    );
                    continue;
                }
                let (x, y) = (inst.cell[0] as usize, inst.cell[1] as usize);
                assert!(x < cols && y < rows, "step {step}: cell out of range");
                let slot = &mut seen[y * cols + x];
                assert!(!*slot, "step {step}: cell ({x},{y}) uploaded twice");
                *slot = true;

                let needs_solid = inst.state[0] > 0.5 || inst.state[1] > 0.5;
                assert_eq!(
                    (i as u32) < solid,
                    needs_solid,
                    "step {step}: cell ({x},{y}) is in the wrong run"
                );
            }
            assert!(seen.iter().all(|b| *b), "step {step}: a cell went missing");
            assert_eq!(
                from_critter, critters,
                "step {step}: critter instances lost"
            );

            life.advance(1);
            viz.on_generation(&life.view(), t);
            t += 0.3;
        }
    }

    #[test]
    fn an_adjacent_birth_slides_from_only_one_of_multiple_deaths() {
        let (cols, rows) = (9, 7);
        let mut life = planted(cols, rows, &[(3, 3), (5, 3), (6, 4)]);
        let mut viz = Viz::new(&life, 0x51de);

        // The middle birth has two orthogonal candidates. The lower-right birth has
        // only a diagonal death, so both of those must retain their ordinary fades.
        life.boards[life.head].fill(false);
        life.boards[life.head][3 * cols + 4] = true;
        life.boards[life.head][5 * cols + 7] = true;
        viz.on_generation(&life.view(), 2.0);

        let moved = viz.inst[3 * cols + 4];
        assert_eq!(moved.state, [1.0, 1.0]);
        assert!(
            moved.slide_from == [3.0, 3.0] || moved.slide_from == [5.0, 3.0],
            "birth did not select one of its orthogonal deaths"
        );
        assert_eq!(moved.t, 2.0);

        let left = viz.inst[3 * cols + 3];
        let right = viz.inst[3 * cols + 5];
        let suppressed = [left, right]
            .iter()
            .filter(|inst| inst.state == [0.0, 0.0])
            .count();
        let normally_fading = [left, right]
            .iter()
            .filter(|inst| inst.state == [0.0, 1.0])
            .count();
        assert_eq!(suppressed, 1, "both dying tiles were consumed by one birth");
        assert_eq!(
            normally_fading, 1,
            "unchosen dying tile lost its normal fade"
        );

        let diagonal_death = viz.inst[4 * cols + 6];
        let unpaired_birth = viz.inst[5 * cols + 7];
        assert_eq!(diagonal_death.state, [0.0, 1.0]);
        assert_eq!(diagonal_death.slide_from, [6.0, 4.0]);
        assert_eq!(unpaired_birth.state, [1.0, 0.0]);
        assert_eq!(unpaired_birth.slide_from, [7.0, 5.0]);
    }

    #[test]
    fn one_death_can_slide_into_only_one_of_multiple_births() {
        let (cols, rows) = (9, 7);
        let mut life = planted(cols, rows, &[(4, 3)]);
        let mut viz = Viz::new(&life, 0x0ce1);

        life.boards[life.head].fill(false);
        life.boards[life.head][3 * cols + 3] = true;
        life.boards[life.head][3 * cols + 5] = true;
        viz.on_generation(&life.view(), 4.0);

        let births = [viz.inst[3 * cols + 3], viz.inst[3 * cols + 5]];
        assert_eq!(
            births
                .iter()
                .filter(|inst| inst.state == [1.0, 1.0] && inst.slide_from == [4.0, 3.0])
                .count(),
            1,
            "one death was assigned to more than one birth"
        );
        assert_eq!(
            births
                .iter()
                .filter(|inst| { inst.state == [1.0, 0.0] && inst.slide_from == inst.cell })
                .count(),
            1,
            "unmatched birth lost its normal fade-in"
        );
        assert_eq!(viz.inst[3 * cols + 4].state, [0.0, 0.0]);

        let shader = shader_source();
        assert!(shader.contains("tile_animated_cell"));
        assert!(shader.contains("tile_animated_color"));
        assert!(!shader.contains("TILE_SLIDE_SECS_LIT"));
    }

    /// A critter that reads the whole lookahead and draws a couple of tiles, purely to
    /// keep the scaffolding honest: registration, per-frame update, future queries and
    /// instance emission all have to work before anything real is built on them.
    #[derive(Default)]
    struct TestCritter {
        frames: u32,
        deepest_seen: usize,
    }

    impl TestCritter {
        fn tiles(&self) -> usize {
            2
        }
    }

    impl Critter for TestCritter {
        fn update(&mut self, ctx: &CritterCtx) -> bool {
            self.frames += 1;
            // Everything from the present out to the lookahead must be answerable.
            for ahead in 0..=ctx.life.lookahead() {
                let _ = ctx.life.alive(3, 3, ahead);
                let _ = ctx.life.neighbours(3, 3, ahead);
                self.deepest_seen = ahead;
            }
            // Out of world is always off, at every depth.
            assert!(!ctx.life.alive(-1, 0, 0));
            assert!(!ctx.life.alive(0, -1, LOOKAHEAD));
            assert!(!ctx.life.alive(ctx.life.cols() as isize, 0, 2));
            true
        }

        fn draw(&self, ctx: &CritterCtx, out: &mut Vec<Instance>) {
            out.push(Instance::tile([2.5, 4.0], PALETTE_BLUE));
            out.push(Instance::tile([3.5, 4.0], PALETTE_TEAL).spinning(ctx.now, 1.0));
        }
    }

    /// A critter must be able to see far enough ahead to be worth planning with, and
    /// what it sees has to match what actually arrives.
    #[test]
    fn critters_can_read_the_near_future() {
        let (cols, rows) = (24, 18);
        // A blinker: cell (5,4) is off now, on next generation, off the one after.
        let mut life = planted(cols, rows, &[(4, 4), (5, 4), (6, 4)]);
        let v = life.view();
        assert!(v.alive(5, 4, 0) && v.alive(4, 4, 0));
        assert!(v.alive(5, 3, 1), "blinker should stand up next generation");
        assert!(!v.alive(4, 4, 1), "its ends should be gone next generation");
        assert!(v.alive(4, 4, 2), "and be back the one after");
        assert_eq!(v.changes_in(4, 4), Some(1));
        assert_eq!(v.changes_in(0, 0), None, "empty corner should hold steady");

        // What was foreseen is what arrives.
        let foreseen: Vec<bool> = (0..=LOOKAHEAD)
            .map(|i| life.view().alive(4, 4, i))
            .collect();
        for (i, want) in foreseen.iter().enumerate().skip(1) {
            for _ in 0..1 {
                life.advance(0);
            }
            assert_eq!(
                life.view().alive(4, 4, 0),
                *want,
                "generation {i} did not arrive as foreseen"
            );
        }
    }

    /// Only coloured tiles spin    /// Only coloured tiles spin, only when they survive a generation, and only about
    /// half the time. A spin on a tile that just appeared would fight its birth
    /// spring, and one on a grey tile would turn an accent into a tic across the field.
    #[test]
    fn only_surviving_coloured_tiles_spin() {
        let (cols, rows) = (36, 26);
        let mut life = Life::new(cols, rows, 99);
        let mut viz = Viz::new(&life, 99);
        let (mut eligible, mut spun) = (0usize, 0usize);

        let mut t = 0.0f32;
        for _ in 0..60 {
            let before: Vec<bool> = life.board(0).to_vec();
            let spin_before: Vec<f32> = viz.inst.iter().map(|c| c.spin[0]).collect();
            t += 4.0;
            life.advance(1);
            viz.on_generation(&life.view(), t);

            for i in 0..cols * rows {
                let (x, y) = (i % cols, i / cols);
                let survived = before[i] && life.board(0)[i];
                if survived && is_colored(x, y) {
                    eligible += 1;
                }
                if viz.inst[i].spin[0] != spin_before[i] {
                    spun += 1;
                    assert_eq!(viz.inst[i].spin[0], t, "spin stamped with the wrong time");
                    assert!(is_colored(x, y), "grey tile at ({x},{y}) spun");
                    assert!(survived, "tile at ({x},{y}) spun without surviving");
                    assert_eq!(
                        viz.inst[i].spin[1].abs(),
                        1.0,
                        "tile at ({x},{y}) should turn exactly half a revolution"
                    );
                }
            }
        }

        assert!(
            eligible > 200,
            "not enough survivals to judge the rate ({eligible})"
        );
        let rate = spun as f64 / eligible as f64;
        assert!(
            (rate - SPIN_CHANCE as f64).abs() < 0.08,
            "{:.0}% of eligible tiles spun, expected about {:.0}%",
            rate * 100.0,
            SPIN_CHANCE * 100.0
        );
    }

    /// The rocket is generated rather than authored, so the checks are on the things
    /// that would silently produce a broken model: dangling indices, degenerate
    /// normals, a hull that is not the length it claims, or a part that never got
    /// emitted at all.
    #[test]
    fn rocket_mesh_is_well_formed() {
        let length = 128.0f32;
        let (v, idx) = build_rocket(length);

        assert!(!v.is_empty() && !idx.is_empty(), "empty rocket");
        assert!(v.len() < u16::MAX as usize, "outgrew a u16 index buffer");
        assert_eq!(idx.len() % 3, 0, "index count is not whole triangles");
        for &i in &idx {
            assert!(
                (i as usize) < v.len(),
                "index {i} past the end of the vertices"
            );
        }

        for (n, vert) in v.iter().enumerate() {
            for c in vert
                .pos
                .iter()
                .chain(vert.nrm.iter())
                .chain(vert.col.iter())
            {
                assert!(c.is_finite(), "vertex {n} has a non-finite component");
            }
            let l = (vert.nrm[0].powi(2) + vert.nrm[1].powi(2) + vert.nrm[2].powi(2)).sqrt();
            assert!((l - 1.0).abs() < 1e-3, "vertex {n} normal has length {l}");
        }

        // Nose forward along +X, and the hull about as long as asked for once the
        // nozzle and the fins hanging off the tail are counted.
        let (mut lo, mut hi) = (f32::MAX, f32::MIN);
        let mut max_r = 0.0f32;
        for vert in &v {
            lo = lo.min(vert.pos[0]);
            hi = hi.max(vert.pos[0]);
            max_r = max_r.max((vert.pos[1].powi(2) + vert.pos[2].powi(2)).sqrt());
        }
        assert!(hi > length * 0.5, "nose does not reach forward");
        assert!(lo < -length * 0.4, "tail does not reach back");
        let span = hi - lo;
        assert!(
            span > length && span < length * 1.35,
            "rocket spans {span:.1} for a nominal length of {length}"
        );
        assert!(
            max_r > length * 0.2 && max_r < length * 0.5,
            "fins reach {max_r:.1}, which is out of proportion"
        );

        // Every part actually made it in. A silently-skipped fin or window would
        // otherwise just look like a slightly plainer rocket.
        for (name, hex) in [
            ("hull", ROCKET_SHELL),
            ("nose and fins", ROCKET_RED),
            ("nozzle", ROCKET_NOZZLE),
            ("window rim", ROCKET_RIM),
            ("window ring", ROCKET_RING),
            ("glass", ROCKET_GLASS),
        ] {
            let want = srgb_hex_to_linear(hex);
            assert!(
                v.iter().any(|vert| vert.col == want),
                "no {name} vertices in the mesh"
            );
        }

        let glass = srgb_hex_to_linear(ROCKET_GLASS);
        let glass_faces: Vec<&PropVertex> = v
            .iter()
            .filter(|vert| vert.col == glass && vert.nrm[2].abs() > 0.99)
            .collect();
        assert!(
            glass_faces.iter().any(|vert| vert.pos[2] > 0.0)
                && glass_faces.iter().any(|vert| vert.pos[2] < 0.0),
            "the rocket needs matching glass portholes 180 degrees apart"
        );
    }

    /// The hull profile has to start wide, bulge amidships and close to a point, or
    /// the silhouette stops reading as a rocket.
    #[test]
    fn rocket_profile_is_a_bulging_ogive() {
        assert!(rocket_radius(1.0) < 1e-3, "nose does not close to a point");
        let tail = rocket_radius(0.0);
        assert!(
            (0.35..0.6).contains(&tail),
            "tail radius {tail:.2} should be a broad fraction of the belly"
        );

        let samples: Vec<(f32, f32)> = (0..=200)
            .map(|i| {
                let u = i as f32 / 200.0;
                (u, rocket_radius(u))
            })
            .collect();
        let belly = samples
            .iter()
            .cloned()
            .fold((0.0f32, 0.0f32), |a, b| if b.1 > a.1 { b } else { a });
        assert!(
            (0.35..0.65).contains(&belly.0),
            "belly sits at u={:.2}, expected around the middle",
            belly.0
        );
        assert!(
            (belly.1 - 1.0).abs() < 1e-3,
            "belly should be the unit radius"
        );

        // One bulge and one bulge only: widening all the way to the belly, then
        // closing all the way to the tip.
        for w in samples.windows(2) {
            let (before_belly, widening) = (w[1].0 <= belly.0, w[1].1 >= w[0].1);
            assert_eq!(
                before_belly, widening,
                "profile turns the wrong way at u={:.3}",
                w[1].0
            );
        }
    }

    /// A rocket has to actually cross the field and then retire, or they would pile up
    /// off-screen forever — and the headwinds mean the crossing is no longer a simple
    /// interpolation that arrives by construction.
    #[test]
    fn rocket_crosses_and_retires() {
        let life = Life::new(26, 18, 5);
        let mut rng = Rng::new(3);
        let mut rocket = Rocket::new(&life.view(), &mut rng);

        let ctx = |now: f32| CritterCtx {
            life: life.view(),
            dt: 1.0 / 60.0,
            now,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        };

        let mut sink = PropSink::default();
        rocket.props(&ctx(0.0), &mut sink);
        let start_x = sink.group(MODEL_ROCKET)[0].pos[0];
        assert!(start_x < 0.0, "should enter from off the left");

        let (mut frames, mut t) = (0, 0.0f32);
        let (mut slowest, mut fastest) = (f32::MAX, f32::MIN);
        let mut went_backward = false;
        let mut behind_frames = 0;
        let mut front_frames = 0;
        let mut last_x = start_x;
        while rocket.update(&ctx(t)) {
            t += 1.0 / 60.0;
            frames += 1;
            assert!(frames < 20_000, "rocket never left");

            let v = rocket.speed();
            slowest = slowest.min(v);
            fastest = fastest.max(v);
            went_backward |= rocket.x < last_x;
            last_x = rocket.x;
            if rocket.behind {
                behind_frames += 1;
            } else {
                front_frames += 1;
            }
        }

        // The gusts have to actually bite, and at their worst push it back a little.
        assert!(
            fastest > 0.0 && slowest < 0.0,
            "speed range {slowest:.1}..{fastest:.1}"
        );
        assert!(went_backward, "never lost ground to a headwind");
        // And the crossing must still take a sane amount of time despite them.
        assert!(
            (6.0..40.0).contains(&t),
            "crossing took {t:.1}s, which is out of proportion"
        );

        // It should spend real time on both sides of the tiles, not pick one and stay.
        assert!(
            behind_frames > 30 && front_frames > 30,
            "spent {front_frames} frames in front and {behind_frames} behind"
        );

        sink.clear();
        rocket.props(&ctx(t), &mut sink);
        let end = sink.group(MODEL_ROCKET)[0];
        assert!(end.pos[0] > 0.0, "should leave to the right");
    }

    /// The exhaust has to be a live spray: sparks always present, always young, always
    /// behind the rocket, and never lingering.
    #[test]
    fn rocket_sheds_a_short_lived_exhaust() {
        let life = Life::new(26, 18, 11);
        let mut rng = Rng::new(9);
        let mut rocket = Rocket::new(&life.view(), &mut rng);
        let ctx = |now: f32| CritterCtx {
            life: life.view(),
            dt: 1.0 / 60.0,
            now,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        };

        let mut t = 0.0f32;
        for _ in 0..90 {
            rocket.update(&ctx(t));
            t += 1.0 / 60.0;
        }

        let mut sink = PropSink::default();
        rocket.props(&ctx(t), &mut sink);
        let embers = sink.group(MODEL_EMBER);
        assert!(!embers.is_empty(), "no exhaust after a second and a half");

        let (pos, rot) = rocket.pose();
        let back = rot_x_axis(rot, -1.0);
        for e in embers {
            assert!(
                e.alpha > 0.0 && e.alpha <= 0.8 + 1e-4,
                "ember opacity {} outside 0..0.8",
                e.alpha
            );
            assert!(e.scale[0] > 0.0, "ember with no size");
            // Every spark should be aft of the nose, along the rocket's own axis.
            let d = [e.pos[0] - pos[0], e.pos[1] - pos[1], e.pos[2] - pos[2]];
            let along = d[0] * back[0] + d[1] * back[1] + d[2] * back[2];
            assert!(along > 0.0, "a spark drifted ahead of the rocket");
        }

        // Nothing still burning may be past its span: a spark that never retires would
        // sit in the pool forever and keep the slot out of circulation.
        for e in &rocket.embers {
            assert!(e.life <= 0.0 || e.age < e.life, "a spark outlived its span");
        }
    }

    /// The colour split has to land near the declared 72 / 12 / 8 / 8, and no colour
    /// may line up with rows, columns or diagonals.
    #[test]
    fn cell_colours_are_evenly_spread() {
        let (cols, rows) = (60u32, 44u32);
        let mut counts = [0usize; 4];
        for y in 0..rows {
            for x in 0..cols {
                let h = cell_hash(x, y);
                assert!((0.0..1.0).contains(&h), "hash out of range at ({x},{y})");
                counts[if h > BLUE_CUT {
                    3
                } else if h > TEAL_CUT {
                    2
                } else if h > GREEN_CUT {
                    1
                } else {
                    0
                }] += 1;
            }
        }
        let total = (cols * rows) as f64;
        let share = |i: usize| counts[i] as f64 / total;
        for (i, want) in [
            (0, GREEN_CUT as f64),
            (1, (TEAL_CUT - GREEN_CUT) as f64),
            (2, (BLUE_CUT - TEAL_CUT) as f64),
            (3, 1.0 - BLUE_CUT as f64),
        ] {
            assert!(
                (share(i) - want).abs() < 0.035,
                "colour {i}: {:.3} of the field, expected about {want:.3}",
                share(i)
            );
        }

        // A hash that is really a function of x alone (or of x+y) would tile the field
        // with stripes. Check that no row and no diagonal is monochrome.
        for y in 0..rows {
            let row: Vec<f32> = (0..cols).map(|x| cell_hash(x, y)).collect();
            assert!(
                row.windows(2).any(|w| w[0] != w[1]),
                "row {y} is a single hash value"
            );
        }
        let diag: Vec<f32> = (0..rows.min(cols)).map(|i| cell_hash(i, i)).collect();
        assert!(
            diag.windows(2).any(|w| w[0] != w[1]),
            "the diagonal is a single hash value"
        );
    }

    /// A blue tile has to stand alone: no other coloured tile within one cell in any
    /// direction. Green and teal stay free to sit together.
    #[test]
    fn blue_tiles_keep_their_distance() {
        let (cols, rows) = (60isize, 44isize);
        let mut blues = 0;
        let mut touching_accents = 0;
        for y in 0..rows {
            for x in 0..cols {
                if cell_category(x, y) != 3 {
                    continue;
                }
                blues += 1;
                for dy in -1isize..=1 {
                    for dx in -1isize..=1 {
                        if dx == 0 && dy == 0 {
                            continue;
                        }
                        assert_eq!(
                            cell_category(x + dx, y + dy),
                            0,
                            "blue at ({x},{y}) has a coloured neighbour at ({},{})",
                            x + dx,
                            y + dy
                        );
                    }
                }
            }
        }
        assert!(
            blues > 15,
            "the rule left almost no blue tiles at all ({blues})"
        );

        // The rule is a filter on blue, not a reshuffle of everything: greens and teals
        // are untouched and may still touch each other.
        for y in 0..rows {
            for x in 0..cols {
                let (a, b) = (cell_category(x, y), cell_category(x + 1, y));
                if (a == 1 || a == 2) && (b == 1 || b == 2) {
                    touching_accents += 1;
                }
            }
        }
        assert!(
            touching_accents > 0,
            "greens and teals should still be allowed to sit next to each other"
        );
    }

    /// A headwind should look like effort, not like a hang.
    ///
    /// The guard is on *hovering* rather than on distance covered, because distance is
    /// the wrong measure: a window straddling the gust can cover a respectable total
    /// while containing a quarter second in which the rocket does not move at all, and
    /// that is precisely what reads as the animation freezing. Drifting backward is
    /// fine — that is visible, deliberate motion. Sitting at zero is not.
    #[test]
    fn rocket_never_hovers() {
        /// Speed below this fraction of peak is indistinguishable from stopped.
        const STILL: f32 = 0.12;
        /// Longest run of that a viewer will not notice.
        const MAX_RUN: f32 = 0.15;
        let life = Life::new(28, 20, 5);

        // Several rockets: the gust phase is seeded per rocket, so a single flight only
        // samples one alignment of gusts against the crossing.
        for seed in 0..12u64 {
            let mut rng = Rng::new(seed * 7 + 1);
            let mut r = Rocket::new(&life.view(), &mut rng);
            let ctx = |now: f32| CritterCtx {
                life: life.view(),
                dt: 1.0 / 240.0,
                now,
                phase: 0.0,
                gen_secs: 3.0,
                spinning: None,
            };

            let peak = r.peak;
            let (mut t, mut run, mut worst, mut still) = (0.0f32, 0.0f32, 0.0f32, 0);
            let mut frames = 0;
            let mut trace = vec![r.x];
            while r.update(&ctx(t)) && frames < 240 * 40 {
                t += 1.0 / 240.0;
                frames += 1;
                trace.push(r.x);
                if r.speed().abs() < peak * STILL {
                    run += 1.0 / 240.0;
                    worst = worst.max(run);
                    still += 1;
                } else {
                    run = 0.0;
                }
            }
            assert!(
                (6.0..30.0).contains(&t),
                "seed {seed}: crossing took {t:.1}s"
            );
            assert!(
                worst < MAX_RUN,
                "seed {seed}: hung for {worst:.2}s at a stretch, limit {MAX_RUN}s"
            );
            let frac = still as f32 / frames as f32;
            assert!(
                frac < 0.06,
                "seed {seed}: barely moving for {:.1}% of the flight",
                frac * 100.0
            );

            // And the complementary measure: over any half second it has to have got
            // somewhere. The two together are what "does not look frozen" means — the
            // check above allows a brisk reversal, this one stops the reversal from
            // cancelling the whole window out.
            let span = 240 / 2;
            let mut net = f32::MAX;
            for w in trace.windows(span + 1) {
                net = net.min(w[span] - w[0]);
            }
            let floor = peak * 0.5 * 0.12;
            assert!(
                net > floor,
                "seed {seed}: net {net:.0}px over half a second, wanted over {floor:.0}px"
            );
        }
    }

    /// The gust still has to bite hard enough to read, and to tip the rocket backward
    /// at its very peak — otherwise the fix above would just be "delete the headwind".
    #[test]
    fn headwind_bites_and_briefly_reverses() {
        let life = Life::new(28, 20, 5);
        let mut rng = Rng::new(3);
        let mut r = Rocket::new(&life.view(), &mut rng);
        let ctx = |now: f32| CritterCtx {
            life: life.view(),
            dt: 1.0 / 240.0,
            now,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        };

        let peak = r.peak;
        let (mut reversed, mut laboured, mut total) = (0usize, 0usize, 0usize);
        let mut t = 0.0f32;
        while r.update(&ctx(t)) && total < 240 * 40 {
            t += 1.0 / 240.0;
            total += 1;
            let v = r.speed();
            if v < 0.0 {
                reversed += 1;
            }
            if v < peak * 0.4 {
                laboured += 1;
            }
        }

        let frac = |n: usize| n as f32 / total as f32;
        assert!(
            (0.02..0.16).contains(&frac(reversed)),
            "backward for {:.1}% of the flight; wanted a brief tick, not a habit",
            frac(reversed) * 100.0
        );
        assert!(
            (0.05..0.30).contains(&frac(laboured)),
            "labouring for {:.1}% of the flight",
            frac(laboured) * 100.0
        );
    }

    /// The prop models share one index buffer and are drawn with `base_vertex` fixed at
    /// zero, so every index has to be absolute into the combined vertex buffer and has
    /// to stay inside its own model's vertices.
    ///
    /// This is here because the bug it guards cannot be caught by rendering: WebGL2 has
    /// no base-vertex and no base-instance indexed draw, while desktop GL has both as
    /// extensions. A draw relying on either renders perfectly on a native GL context
    /// and is rejected in a browser — where, with `panic = "abort"`, it takes the whole
    /// module down and freezes the page.
    #[test]
    fn prop_indices_are_absolute_and_in_range() {
        let (verts, indices, ranges) = combine_prop_models();
        assert!(!verts.is_empty() && !indices.is_empty());
        assert!(verts.len() < u16::MAX as usize);

        // The ranges must tile the index buffer exactly, with nothing left over.
        let covered: u32 = ranges.iter().map(|(_, len)| *len).sum();
        assert_eq!(
            covered as usize,
            indices.len(),
            "the model ranges do not account for every index"
        );

        // Each model's own vertices occupy a contiguous span, and its indices must all
        // land inside that span — which is exactly what makes base_vertex 0 correct.
        let meshes = prop_model_meshes();
        let mut base = 0usize;
        for (slot, (first, len)) in ranges.iter().enumerate() {
            let (lo, hi) = (base, base + meshes[slot].0.len());
            for &i in &indices[*first as usize..(*first + *len) as usize] {
                let i = i as usize;
                assert!(
                    i >= lo && i < hi,
                    "model {slot} indexes vertex {i}, outside its own {lo}..{hi}"
                );
            }
            base = hi;
        }
        assert_eq!(base, verts.len(), "vertex spans do not cover the buffer");
    }

    /// The generated cutout is deliberately a body-only image: its six legs have been
    /// moved into independently animated atlas cells, while the body quad keeps the
    /// source aspect and full UV range.
    #[test]
    fn bee_body_asset_and_quad_are_well_formed() {
        let (pixels, width, height) = bee_body_pixels();
        assert_eq!((width, height), (420, 269));
        assert_eq!(pixels.len(), width as usize * height as usize * 4);

        let mut transparent = 0usize;
        let mut visible = 0usize;
        for pixel in pixels.chunks_exact(4) {
            transparent += usize::from(pixel[3] == 0);
            visible += usize::from(pixel[3] > 180);
        }
        let samples = width as usize * height as usize;
        assert!(
            transparent > samples / 3,
            "the bee cutout has too little transparent padding"
        );
        assert!(
            visible > samples / 12,
            "the bee body is missing or nearly transparent"
        );

        let (quad, indices) = build_bee_body_quad();
        assert_eq!(quad.len(), 4);
        assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
        let mut uvs: Vec<[f32; 2]> = quad.iter().map(|v| v.uv).collect();
        uvs.sort_by(|a, b| a.partial_cmp(b).unwrap());
        assert_eq!(uvs, [[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]]);
    }

    #[test]
    fn bee_leg_atlas_has_six_independent_pivoted_cells() {
        let (pixels, width, height) = bee_legs_pixels();
        assert_eq!((width, height), (336, 176));
        assert_eq!(pixels.len(), width as usize * height as usize * 4);
        let visible = pixels.chunks_exact(4).filter(|p| p[3] > 180).count();
        assert!(visible > 1_500, "leg atlas is empty or nearly transparent");

        for slot in 0..6 {
            let (quad, indices) = build_bee_leg_quad(slot);
            assert_eq!(quad.len(), 4);
            assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
            let min_x = quad.iter().map(|v| v.pos[0]).fold(f32::MAX, f32::min);
            let max_x = quad.iter().map(|v| v.pos[0]).fold(f32::MIN, f32::max);
            let min_y = quad.iter().map(|v| v.pos[1]).fold(f32::MAX, f32::min);
            let max_y = quad.iter().map(|v| v.pos[1]).fold(f32::MIN, f32::max);
            assert!(
                min_x < 0.0 && max_x > 0.0 && min_y < 0.0 && max_y > 0.0,
                "leg {slot} pivot is not inside its cell"
            );
        }
        assert!(
            MODEL_BEE_LEG_LAST < MODEL_BEE_BODY && MODEL_BEE_BODY < MODEL_BEE_WING,
            "legs, body, and wings lost their compositing order"
        );
    }

    #[test]
    fn tiki_mask_asset_and_quad_are_well_formed() {
        let (pixels, width, height) = tiki_mask_pixels();
        assert_eq!((width, height), (223, 512));
        assert_eq!(pixels.len(), width as usize * height as usize * 4);
        let transparent = pixels.chunks_exact(4).filter(|pixel| pixel[3] == 0).count();
        let visible = pixels
            .chunks_exact(4)
            .filter(|pixel| pixel[3] > 180)
            .count();
        let samples = width as usize * height as usize;
        assert!(
            transparent > samples / 20,
            "the mask has no transparent silhouette"
        );
        assert!(visible > samples / 2, "the mask is missing or too faint");

        let (quad, indices) = build_tiki_mask_quad();
        assert_eq!(quad.len(), 4);
        assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
        let width = quad[1].pos[0] - quad[0].pos[0];
        let height = quad[3].pos[1] - quad[0].pos[1];
        assert!(
            (height / width - 512.0 / 223.0).abs() < 1e-5,
            "mask quad lost the asset aspect"
        );
    }

    #[test]
    fn astronaut_helmet_asset_and_quad_are_well_formed() {
        let (pixels, width, height) = astronaut_helmet_pixels();
        assert_eq!((width, height), (512, 512));
        assert_eq!(pixels.len(), width as usize * height as usize * 4);
        let transparent = pixels.chunks_exact(4).filter(|p| p[3] == 0).count();
        let visible = pixels.chunks_exact(4).filter(|p| p[3] > 180).count();
        assert!(
            transparent > pixels.len() / 4 / 5,
            "helmet has no transparent outside"
        );
        assert!(visible > pixels.len() / 4 / 3, "helmet is missing or faint");
        let (quad, indices) = build_astronaut_helmet_quad();
        assert_eq!(quad.len(), 4);
        assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
    }

    fn assert_generated_mask(
        label: &str,
        (pixels, width, height): (Vec<u8>, u32, u32),
        expected: (u32, u32),
        (quad, indices): (Vec<PropVertex>, Vec<u16>),
    ) {
        assert_eq!((width, height), expected);
        assert_eq!(pixels.len(), width as usize * height as usize * 4);
        let samples = width as usize * height as usize;
        let transparent = pixels.chunks_exact(4).filter(|p| p[3] == 0).count();
        let visible = pixels.chunks_exact(4).filter(|p| p[3] > 180).count();
        assert!(
            transparent > samples / 20,
            "{label} has no transparent outside"
        );
        assert!(visible > samples / 2, "{label} is missing or too faint");

        assert_eq!(quad.len(), 4);
        assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
        let quad_width = quad[1].pos[0] - quad[0].pos[0];
        let quad_height = quad[3].pos[1] - quad[0].pos[1];
        let expected_aspect = expected.1 as f32 / expected.0 as f32;
        assert!(
            (quad_height / quad_width - expected_aspect).abs() < 1e-5,
            "{label} quad lost the asset aspect"
        );
    }

    #[test]
    fn noh_mask_asset_and_quad_are_well_formed() {
        assert_generated_mask(
            "Noh mask",
            noh_mask_pixels(),
            (290, 384),
            build_noh_mask_quad(),
        );
    }

    #[test]
    fn monkey_mask_asset_and_quad_are_well_formed() {
        assert_generated_mask(
            "monkey mask",
            monkey_mask_pixels(),
            (384, 373),
            build_monkey_mask_quad(),
        );
    }

    #[test]
    fn comedy_tragedy_mask_asset_and_quad_are_well_formed() {
        assert_generated_mask(
            "comedy/tragedy mask",
            comedy_tragedy_mask_pixels(),
            (293, 384),
            build_comedy_tragedy_mask_quad(),
        );
    }

    fn assert_generated_sprite(
        label: &str,
        (pixels, width, height): (Vec<u8>, u32, u32),
        expected: (u32, u32),
        (quad, indices): (Vec<PropVertex>, Vec<u16>),
    ) {
        assert_eq!((width, height), expected);
        assert_eq!(pixels.len(), width as usize * height as usize * 4);
        let samples = width as usize * height as usize;
        let transparent = pixels.chunks_exact(4).filter(|p| p[3] == 0).count();
        let visible = pixels.chunks_exact(4).filter(|p| p[3] > 180).count();
        assert!(
            transparent > samples / 20,
            "{label} has no transparent outside"
        );
        assert!(
            visible > samples / 4,
            "{label} is missing or too faint ({visible}/{samples} pixels)"
        );

        assert_eq!(quad.len(), 4);
        assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
        let quad_width = quad[1].pos[0] - quad[0].pos[0];
        let quad_height = quad[3].pos[1] - quad[0].pos[1];
        let expected_aspect = expected.1 as f32 / expected.0 as f32;
        assert!(
            (quad_height / quad_width - expected_aspect).abs() < 1e-5,
            "{label} quad lost the asset aspect"
        );
    }

    #[test]
    fn penguin_asset_and_quad_are_well_formed() {
        assert_generated_sprite(
            "penguin",
            penguin_pixels(),
            (305, 384),
            build_penguin_quad(),
        );
    }

    #[test]
    fn godzilla_layer_assets_and_quads_are_well_formed() {
        let (body, body_width, body_height) = godzilla_body_pixels();
        let (tail, tail_width, tail_height) = godzilla_tail_pixels();
        assert_eq!((body_width, body_height), (384, 266));
        assert_eq!((tail_width, tail_height), (384, 266));
        let samples = body_width as usize * body_height as usize;
        assert!(body.chunks_exact(4).filter(|p| p[3] > 180).count() > samples / 5);
        assert!(tail.chunks_exact(4).filter(|p| p[3] > 180).count() > samples / 100);
        for y in 0..body_height as usize {
            assert!((0..82).all(|x| body[(y * body_width as usize + x) * 4 + 3] == 0));
            assert!(
                (90..tail_width as usize).all(|x| tail[(y * tail_width as usize + x) * 4 + 3] == 0)
            );
        }
        let overlap_visible = (0..body_height as usize)
            .flat_map(|y| (82..90).map(move |x| (y, x)))
            .filter(|&(y, x)| {
                body[(y * body_width as usize + x) * 4 + 3] > 0
                    && tail[(y * tail_width as usize + x) * 4 + 3] > 0
            })
            .count();
        assert!(
            overlap_visible > 20,
            "Godzilla tail cut lost its seam-hiding overlap"
        );
        for (quad, indices) in [build_godzilla_body_quad(), build_godzilla_tail_quad()] {
            assert_eq!(quad.len(), 4);
            assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
        }
        let (tail_quad, _) = build_godzilla_tail_quad();
        assert!(tail_quad[0].pos[0] < 0.0 && tail_quad[1].pos[0] > 0.0);
        assert!(tail_quad[0].pos[1] < 0.0 && tail_quad[3].pos[1] > 0.0);

        let (pixels, width, height) = godzilla_legs_pixels();
        assert_eq!((width, height), (1536, 266));
        let visible = pixels.chunks_exact(4).filter(|p| p[3] > 180).count();
        assert!(
            visible > pixels.len() / 4 / 50,
            "Godzilla leg atlas is missing or too faint"
        );
        for slot in 0..4 {
            let (quad, indices) = build_godzilla_leg_quad(slot);
            assert_eq!(quad.len(), 4);
            assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
            assert!(quad[0].pos[0] < 0.0 && quad[1].pos[0] > 0.0);
            assert!(quad[0].pos[1] < 0.0 && quad[3].pos[1] > 0.0);
            assert!(quad.iter().all(|v| v.uv[0] >= slot as f32 * 0.25));
            assert!(quad.iter().all(|v| v.uv[0] <= (slot + 1) as f32 * 0.25));
        }
    }

    #[test]
    fn fog_atlas_has_eight_soft_independent_cells() {
        let (pixels, width, height) = fog_blob_pixels();
        assert_eq!((width, height), (1024, 512));
        let transparent = pixels.chunks_exact(4).filter(|p| p[3] == 0).count();
        let partial = pixels
            .chunks_exact(4)
            .filter(|p| p[3] > 0 && p[3] < 255)
            .count();
        assert!(transparent > pixels.len() / 4 * 2 / 3);
        assert!(partial > pixels.len() / 4 / 30);
        for slot in 0..FOG_ATLAS_CELL_COUNT {
            let (quad, indices) = build_fog_blob_quad(slot);
            assert_eq!(quad.len(), 4);
            assert_eq!(indices, [0, 1, 2, 0, 2, 3]);
            let col = slot % FOG_ATLAS_COLS;
            let row = slot / FOG_ATLAS_COLS;
            assert!(quad.iter().all(|v| {
                v.uv[0] >= col as f32 / FOG_ATLAS_COLS as f32
                    && v.uv[0] <= (col + 1) as f32 / FOG_ATLAS_COLS as f32
                    && v.uv[1] >= row as f32 / FOG_ATLAS_ROWS as f32
                    && v.uv[1] <= (row + 1) as f32 / FOG_ATLAS_ROWS as f32
            }));
            assert!(MODEL_UNLIT[MODEL_FOG_FIRST + slot]);
        }
    }

    #[test]
    fn dense_gray_fog_drifts_in_front_of_distant_visitors() {
        assert_eq!(FOG_ALPHA, 0.45);
        let life = Life::new(24, 18, 0xf09);
        let mut fog = FogLayer::new(&life.view(), 0x6169_6d6c_6573_736c);
        let mut before = PropSink::default();
        fog.props(&mut before);
        assert_eq!(before.total(), FOG_BLOB_COUNT);
        assert!(fog.blobs.iter().all(|blob| {
            blob.size[0] >= 330.0 * FOG_SCALE && blob.size[0] <= 690.0 * FOG_SCALE
        }));
        let original_positions: Vec<[f32; 3]> = (MODEL_FOG_FIRST..=MODEL_FOG_LAST)
            .flat_map(|model| before.group(model).iter().map(|blob| blob.pos))
            .collect();
        assert_eq!(original_positions.len(), FOG_BLOB_COUNT);
        let page_gray = srgb_hex_to_linear(BG);
        for model in MODEL_FOG_FIRST..=MODEL_FOG_LAST {
            assert!((7..=8).contains(&before.group(model).len()));
            for blob in before.group(model) {
                assert_eq!(blob.alpha, FOG_ALPHA);
                assert_eq!(blob.tint, page_gray);
                assert_eq!(blob.pos[2], FOG_Z);
            }
        }

        fog.update(5.0);
        let mut after = PropSink::default();
        fog.props(&mut after);
        assert!((MODEL_FOG_FIRST..=MODEL_FOG_LAST)
            .flat_map(|model| after.group(model).iter().map(|blob| blob.pos))
            .zip(original_positions)
            .any(|(new, old)| new != old));
    }

    #[test]
    fn procedural_marshmallow_sailor_turns_and_walks_as_one_depth_rig() {
        for (label, (verts, indices)) in [
            ("marshmallow puff", build_marshmallow_puff()),
            ("marshmallow body", build_marshmallow_body()),
            ("marshmallow head", build_marshmallow_head()),
        ] {
            assert!(verts.len() > 200, "{label} lost its rounded 3D surface");
            assert!(indices.len() > 600, "{label} lost its closed mesh");
            assert!(indices.iter().all(|&i| (i as usize) < verts.len()));
            assert!(verts.iter().all(|v| {
                v.pos.into_iter().all(f32::is_finite)
                    && v.nrm.into_iter().all(f32::is_finite)
                    && v.col.into_iter().all(f32::is_finite)
            }));
        }
        let (body, _) = build_marshmallow_body();
        assert!(
            body.iter().any(|v| v.col[0] > v.col[2] * 4.0),
            "the red neckerchief disappeared"
        );
        assert!(
            body.iter()
                .any(|v| v.col.iter().copied().sum::<f32>() < 0.08),
            "the navy sailor collar disappeared"
        );
        let (head, _) = build_marshmallow_head();
        assert!(
            head.iter()
                .any(|v| v.col.iter().copied().sum::<f32>() < 0.03),
            "the face and cap band lost their dark details"
        );
        for (label, color) in [
            ("white crown and brim", srgb_hex_to_linear(0xf7f8f5)),
            ("navy cap band", srgb_hex_to_linear(0x071722)),
        ] {
            let cap_verts: Vec<_> = head.iter().filter(|v| v.col == color).collect();
            assert!(!cap_verts.is_empty(), "{label} disappeared");
            let center_x = cap_verts.iter().map(|v| v.pos[0]).sum::<f32>() / cap_verts.len() as f32;
            assert!(
                center_x.abs() < 0.015,
                "{label} drifted sideways off the marshmallow man's head: {center_x}"
            );
        }

        let mut max_front = 0.0f32;
        let mut min_front = 1.0f32;
        let mut max_stride = 0.0f32;
        for sample in 0..700 {
            let pose = DistantBackground::marshmallow_pose_at(sample as f32 / 30.0, 0.73);
            max_front = max_front.max(pose.body_front);
            min_front = min_front.min(pose.body_front);
            max_stride = max_stride.max((pose.upper_leg[0] - pose.upper_leg[1]).abs());
            assert!(pose.head_front >= pose.body_front);
            assert!((0.53..=0.80).contains(&pose.speed));
            assert!(pose.depth >= -247.0 && pose.depth <= -193.0);
        }
        assert!(max_front > 0.69 && min_front < 0.001);
        assert!(max_stride > 0.58, "the giant lost his lumbering stride");
        let before_exchange = DistantBackground::marshmallow_foot_reach(-0.0001);
        let after_exchange = DistantBackground::marshmallow_foot_reach(0.0001);
        assert!(
            (after_exchange - before_exchange).abs() < 0.0001,
            "a marshmallow foot still snaps when its stride crosses zero"
        );
        assert!((DistantBackground::marshmallow_foot_reach(-0.30) - 0.030).abs() < 1e-6);
        assert!((DistantBackground::marshmallow_foot_reach(0.30) - 0.080).abs() < 1e-6);

        let life = Life::new(24, 18, 0x5a17);
        let mut background = DistantBackground::new(&life.view(), 0x5a17);
        background.crossing = None;
        background.next_kind = DistantKind::Marshmallow;
        background.start_crossing(&life.view());
        background.crossing.as_mut().unwrap().t = 7.0;
        let mut sink = PropSink::default();
        background.props(&mut sink);
        assert_eq!(sink.group(MODEL_MARSHMALLOW_PUFF).len(), 12);
        assert_eq!(sink.group(MODEL_MARSHMALLOW_BODY).len(), 1);
        assert_eq!(sink.group(MODEL_MARSHMALLOW_HEAD).len(), 1);
        assert_eq!(sink.total(), 14);
        assert!(sink.group(MODEL_PENGUIN).is_empty());
        assert!(sink.group(MODEL_GODZILLA_BODY).is_empty());
        assert_eq!(
            sink.group(MODEL_MARSHMALLOW_BODY)[0].alpha,
            DISTANT_MARSHMALLOW_ALPHA
        );
        assert_ne!(
            sink.group(MODEL_MARSHMALLOW_BODY)[0].rot[1],
            sink.group(MODEL_MARSHMALLOW_HEAD)[0].rot[1],
            "the head stopped leading the occasional camera turn"
        );
        assert!((12.0..=22.0).contains(&DISTANT_MARSHMALLOW_OUTLINE_PX));
        let shader = shader_source();
        assert!(shader.contains("vs_distant_outline"));
        assert!(shader.contains("fs_distant_outline"));
        assert!(!shader.contains("DISTANT_MARSHMALLOW_OUTLINE_PX_LIT"));
    }

    #[test]
    fn distant_visitors_cross_once_exclusively_and_tux_hop_lands() {
        let takeoff = DistantBackground::tux_pose_at(PENGUIN_WADDLE_SECS, 0.0);
        let apex =
            DistantBackground::tux_pose_at(PENGUIN_WADDLE_SECS + PENGUIN_HOP_SECS * 0.5, 0.0);
        let landed = DistantBackground::tux_pose_at(PENGUIN_WADDLE_SECS + PENGUIN_HOP_SECS, 0.0);
        assert!(takeoff.y.abs() < 1e-5);
        assert!((apex.y - PENGUIN_HOP_HEIGHT).abs() < 1e-4);
        assert!(landed.y.abs() < 1e-5);
        let mut max_upper_split = 0.0f32;
        let mut max_knee_flex = 0.0f32;
        let mut min_depth = f32::INFINITY;
        let mut max_depth = f32::NEG_INFINITY;
        for sample in 0..240 {
            let t = sample as f32 / 30.0;
            let pose = DistantBackground::godzilla_pose_at(t, 0.7);
            assert!(pose.y <= 1.46);
            assert!(pose.x_jitter.abs() <= 0.36);
            assert!(pose.rot.abs() <= 0.020);
            assert!((0.60..=0.88).contains(&pose.speed));
            assert!((pose.scale_x - 1.0).abs() <= 0.0041);
            assert!((pose.scale_y - 1.0).abs() <= 0.0041);
            let legs = DistantBackground::godzilla_leg_angles_at(t, 0.7);
            assert!(legs.upper[0].abs() <= 0.124);
            assert!(legs.upper[1].abs() <= 0.111);
            assert!(legs.lower[0].abs() <= 0.130);
            assert!(legs.lower[1].abs() <= 0.120);
            max_upper_split = max_upper_split.max((legs.upper[0] - legs.upper[1]).abs());
            max_knee_flex = max_knee_flex.max(legs.lower[0].abs().max(legs.lower[1].abs()));
            let depth = DistantBackground::godzilla_depth_at(t, 0.7);
            min_depth = min_depth.min(depth);
            max_depth = max_depth.max(depth);
        }
        assert!(
            max_upper_split > 0.205,
            "Godzilla legs lost their opposing stride"
        );
        assert!(max_knee_flex > 0.105, "Godzilla knees stopped articulating");
        assert!(
            max_depth - min_depth > 74.0,
            "Godzilla lost his front/back Z sway"
        );
        let mid_stance = DistantBackground::godzilla_pose_at(0.0, 0.0);
        let foot_exchange = DistantBackground::godzilla_pose_at(
            std::f32::consts::FRAC_PI_2 / GODZILLA_STEP_RATE,
            0.0,
        );
        assert!(
            mid_stance.speed > foot_exchange.speed * 1.45,
            "Godzilla's translation is no longer synchronized to the planted-leg sweep"
        );
        assert!(
            (std::f32::consts::TAU / GODZILLA_STEP_RATE - 5.71).abs() < 0.01,
            "Godzilla's gait no longer matches the wide stride distance"
        );

        let life = Life::new(24, 18, 90);
        let mut background = DistantBackground::new(&life.view(), 90);
        background.crossing = None;
        background.next_kind = DistantKind::Tux;
        background.start_crossing(&life.view());
        let crossing = background.crossing.as_mut().unwrap();
        crossing.t = (PENGUIN_WADDLE_SECS + PENGUIN_HOP_SECS) / DISTANT_TUX_TIME_SCALE;
        let ground_y = crossing.ground_y;
        let direction = crossing.direction;
        let visible_edge = if direction > 0.0 {
            -(life.cols as f32) * CELL_PX * 0.5 + MARGIN as f32 * CELL_PX
        } else {
            life.cols as f32 * CELL_PX * 0.5 - MARGIN as f32 * CELL_PX
        };
        assert!(
            if direction > 0.0 {
                crossing.x + DISTANT_TUX_W * 0.5 < visible_edge
            } else {
                crossing.x - DISTANT_TUX_W * 0.5 > visible_edge
            },
            "background visitor did not begin fully offscreen"
        );
        let mut sink = PropSink::default();
        background.props(&mut sink);
        assert_eq!(sink.group(MODEL_PENGUIN).len(), 1);
        assert!(sink.group(MODEL_GODZILLA_REAR_UPPER).is_empty());
        assert!(sink.group(MODEL_GODZILLA_REAR_LOWER).is_empty());
        assert!(sink.group(MODEL_GODZILLA_FRONT_UPPER).is_empty());
        assert!(sink.group(MODEL_GODZILLA_FRONT_LOWER).is_empty());
        assert!(sink.group(MODEL_GODZILLA_TAIL).is_empty());
        assert!(sink.group(MODEL_GODZILLA_BODY).is_empty());
        assert_eq!(sink.total(), 1);
        let sprite = sink.group(MODEL_PENGUIN)[0];
        assert_eq!(sprite.alpha, DISTANT_TUX_ALPHA);
        assert!(
            sprite.scale[0].abs() > 600.0,
            "Tux was not scaled into a distant giant"
        );
        let bottom = sprite.pos[1] - sprite.scale[1] * (384.0 / 305.0) * 0.5;
        assert!(
            (bottom - (ground_y - DISTANT_TUX_H * 0.5)).abs() < 1e-4,
            "the completed hop did not land on its baseline"
        );

        // Put this individual over its finish line. It must retire, not wrap around.
        let crossing = background.crossing.as_mut().unwrap();
        crossing.x = crossing.to_x - crossing.direction * 0.01;
        let ctx = CritterCtx {
            life: life.view(),
            dt: 1.0,
            now: 1.0,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        };
        background.update(&ctx);
        assert!(
            background.crossing.is_none(),
            "completed background visitor looped instead of retiring"
        );
        sink.clear();
        background.props(&mut sink);
        assert_eq!(sink.total(), 0);

        // Once the one-shot Tux crossing has retired, the next crossing is Godzilla
        // and remains the only source in the fog layer.
        background.cooldown = 0.0;
        background.update(&ctx);
        sink.clear();
        background.props(&mut sink);
        assert!(sink.group(MODEL_PENGUIN).is_empty());
        assert_eq!(sink.group(MODEL_GODZILLA_REAR_UPPER).len(), 1);
        assert_eq!(sink.group(MODEL_GODZILLA_REAR_LOWER).len(), 1);
        assert_eq!(sink.group(MODEL_GODZILLA_FRONT_UPPER).len(), 1);
        assert_eq!(sink.group(MODEL_GODZILLA_FRONT_LOWER).len(), 1);
        assert_eq!(sink.group(MODEL_GODZILLA_TAIL).len(), 1);
        assert_eq!(sink.group(MODEL_GODZILLA_BODY).len(), 1);
        assert_eq!(sink.total(), 6);
        let body = sink.group(MODEL_GODZILLA_BODY)[0];
        let tail = sink.group(MODEL_GODZILLA_TAIL)[0];
        let rear_upper = sink.group(MODEL_GODZILLA_REAR_UPPER)[0];
        let rear_lower = sink.group(MODEL_GODZILLA_REAR_LOWER)[0];
        let front_upper = sink.group(MODEL_GODZILLA_FRONT_UPPER)[0];
        let front_lower = sink.group(MODEL_GODZILLA_FRONT_LOWER)[0];
        assert_eq!(body.alpha, DISTANT_GODZILLA_ALPHA);
        for segment in [rear_upper, rear_lower, front_upper, front_lower] {
            assert_eq!(segment.alpha, DISTANT_GODZILLA_ALPHA);
            assert_eq!(segment.pos[2], body.pos[2]);
        }
        assert_eq!(tail.alpha, DISTANT_GODZILLA_ALPHA);
        assert_eq!(tail.pos[2], body.pos[2]);
        assert_eq!(tail.rot[1], 0.0);
        assert_ne!(rear_upper.rot[2], body.rot[2]);
        assert_ne!(front_upper.rot[2], body.rot[2]);
        assert_ne!(rear_lower.rot[2], rear_upper.rot[2]);
        assert_ne!(front_lower.rot[2], front_upper.rot[2]);

        // The first event is randomly scheduled 4–10 seconds into the crossing.
        // At full recession the cut stays pinned, the tip pitches into -Z, and its
        // source opacity falls; after the 0.13-second hold it snaps exactly home.
        {
            let crossing = background.crossing.as_mut().unwrap();
            assert!(
                (GODZILLA_TAIL_FLICK_MIN..GODZILLA_TAIL_FLICK_MIN + GODZILLA_TAIL_FLICK_SPAN)
                    .contains(&crossing.tail_next_flick_at)
            );
            crossing.tail_flick_started = Some(crossing.t);
            crossing.t += GODZILLA_TAIL_AWAY_SECS + GODZILLA_TAIL_HOLD_SECS * 0.5;
        }
        sink.clear();
        background.props(&mut sink);
        let receded_tail = sink.group(MODEL_GODZILLA_TAIL)[0];
        assert!(receded_tail.rot[1].abs() > 0.77);
        assert!(receded_tail.alpha < DISTANT_GODZILLA_ALPHA * 0.34);
        background.crossing.as_mut().unwrap().t +=
            GODZILLA_TAIL_HOLD_SECS * 0.5 + GODZILLA_TAIL_RETURN_SECS;
        sink.clear();
        background.props(&mut sink);
        let returned_tail = sink.group(MODEL_GODZILLA_TAIL)[0];
        assert!(returned_tail.rot[1].abs() < 1e-5);
        assert_eq!(returned_tail.alpha, DISTANT_GODZILLA_ALPHA);
        assert!((GODZILLA_TAIL_FLICK_SECS - 0.637).abs() < 1e-6);

        assert!((DISTANT_TUX_W / 430.0 - 1.40).abs() < 1e-5);
        assert!((DISTANT_GODZILLA_W / 790.0 - 1.40).abs() < 1e-5);
        assert!((DISTANT_LAYER_OPACITY * DISTANT_TUX_ALPHA - 0.28).abs() < 1e-5);
        assert!((DISTANT_LAYER_OPACITY * DISTANT_GODZILLA_ALPHA - 0.33).abs() < 1e-5);
        assert_eq!(DISTANT_TUX_BLUR_LAYER_PX, 10.0);
        assert!(DISTANT_TUX_BASE_LIFT > 100.0);
        let shader = shader_source();
        assert!(shader.contains("fs_distant_source"));
        assert!(shader.contains("fs_distant_composite"));
        assert!(shader.contains("vs_distant_composite"));
        assert!(!shader.contains("DISTANT_LAYER_OPACITY_LIT"));
        assert!(!shader.contains("DISTANT_TUX_BLUR_LAYER_PX_LIT"));
    }

    fn life_with_ufo_target() -> Life {
        for seed in 1..2_000u64 {
            let life = Life::new(28, 20, seed);
            if !ufo_target_candidates(&life.view(), 0.5).is_empty() {
                return life;
            }
        }
        panic!("could not find a deterministic UFO target board");
    }

    #[test]
    fn ufo_reserves_only_a_death_strictly_between_one_and_two_iterations_away() {
        let life = life_with_ufo_target();
        let view = life.view();
        let candidates = ufo_target_candidates(&view, 0.63);
        assert!(!candidates.is_empty());
        for &(x, y) in &candidates {
            let (x, y) = (x as isize, y as isize);
            assert!(view.alive(x, y, 0));
            assert!(view.alive(x, y, 1));
            assert!(!view.alive(x, y, 2));
            assert!(!view.alive(x, y, 3));
            assert!(!view.alive(x, y, 4));
        }

        let mut ufo = Ufo::new(&view, 0.63, &mut Rng::new(0x0f0)).unwrap();
        assert!(candidates.contains(&ufo.target));
        assert!((ufo.start_iterations - 1.37).abs() < 1e-5);
        assert_eq!(ufo.claimed_cell(), Some(ufo.target));
        ufo.profile_at_capture = false;
        assert!(ufo.tilt_at(ufo.origin).abs() > 1.0);
        assert!(ufo.tilt_at(ufo.hover_position()).abs() < 0.15);
        ufo.profile_at_capture = true;
        assert!(ufo.tilt_at(ufo.origin).abs() < 0.15);
        assert!(ufo.tilt_at(ufo.hover_position()).abs() > 1.0);

        // Exactly on a generation boundary, board(2) would be x == 2 and is not
        // eligible. The strict interval instead uses a board(1) death at x == 1.
        let boundary_life = (1..2_000u64)
            .map(|seed| Life::new(28, 20, seed))
            .find(|life| !ufo_target_candidates(&life.view(), 0.0).is_empty())
            .expect("could not find an exact-boundary UFO board");
        let boundary = ufo_target_candidates(&boundary_life.view(), 0.0);
        for &(x, y) in &boundary {
            let (x, y) = (x as isize, y as isize);
            assert!(boundary_life.view().alive(x, y, 0));
            assert!(!boundary_life.view().alive(x, y, 1));
            assert!(!boundary_life.view().alive(x, y, 2));
            assert!(!boundary_life.view().alive(x, y, 3));
        }
        let boundary_ufo = Ufo::new(&boundary_life.view(), 0.0, &mut Rng::new(7)).unwrap();
        assert!((boundary_ufo.start_iterations - 1.0).abs() < 1e-5);
    }

    #[test]
    fn ufo_replaces_the_dying_tile_on_the_boundary_then_tows_it_away() {
        let mut life = life_with_ufo_target();
        let mut viz = Viz::new(&life, 0xabdc07);
        let ufo = Ufo::new(&life.view(), 0.5, &mut Rng::new(0x5150)).unwrap();
        let target = ufo.target;
        viz.add_critter(Box::new(ufo));

        // The promised first generation arrives and the target remains an ordinary
        // live tile while the saucer finishes its approach.
        life.advance(0);
        viz.on_generation(&life.view(), 1.0);
        assert!(viz.drawn(target.0, target.1));
        viz.update(&CritterCtx {
            life: life.view(),
            dt: 0.0,
            now: 1.0,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        });
        assert!(viz.props().group(MODEL_CAPTURED_TILE).is_empty());

        // On the second boundary Conway says dead. The field copy disappears without
        // a fade and the UFO emits its cargo plus the layered beam in the same frame.
        life.advance(0);
        viz.on_generation(&life.view(), 2.0);
        assert!(!viz.drawn(target.0, target.1));
        viz.update(&CritterCtx {
            life: life.view(),
            dt: 0.0,
            now: 2.0,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        });
        let first_tile = viz.props().group(MODEL_CAPTURED_TILE)[0];
        assert_eq!(viz.props().group(MODEL_UFO_BEAM).len(), 3);

        viz.update(&CritterCtx {
            life: life.view(),
            dt: 0.5,
            now: 2.5,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        });
        let pulled_tile = viz.props().group(MODEL_CAPTURED_TILE)[0];
        assert!(
            pulled_tile.pos[2] > first_tile.pos[2] + 10.0,
            "captured tile did not move outward in Z"
        );
        assert!(
            (pulled_tile.pos[0] - first_tile.pos[0]).hypot(pulled_tile.pos[1] - first_tile.pos[1])
                > 20.0,
            "captured tile stayed hidden directly under the saucer"
        );
        assert_ne!(
            pulled_tile.rot, first_tile.rot,
            "captured tile did not spin"
        );
        let beam = viz.props().group(MODEL_UFO_BEAM);
        assert_eq!(
            beam.len(),
            3,
            "tractor beam did not persist through the pull"
        );
        assert!(beam
            .iter()
            .all(|layer| layer.alpha > 0.0 && layer.alpha < 0.25));

        viz.update(&CritterCtx {
            life: life.view(),
            dt: 0.6,
            now: 3.1,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        });
        assert_eq!(
            viz.props().group(MODEL_UFO_BEAM).len(),
            3,
            "tractor beam switched off when the saucer began its escape"
        );

        viz.update(&CritterCtx {
            life: life.view(),
            dt: 2.8,
            now: 5.9,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        });
        let late_beam = viz.props().group(MODEL_UFO_BEAM);
        assert_eq!(
            late_beam.len(),
            3,
            "tractor beam did not keep towing the tile through the getaway"
        );
        assert!(late_beam.iter().all(|layer| layer.alpha > 0.0));

        viz.update(&CritterCtx {
            life: life.view(),
            dt: 0.31,
            now: 6.21,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        });
        assert!(
            viz.props().group(MODEL_UFO_BEAM).is_empty(),
            "tractor beam remained after the saucer and cargo left"
        );
    }

    #[test]
    fn ufo_cargo_has_no_pull_to_escape_position_jump() {
        let life = life_with_ufo_target();
        let view = life.view();
        let mut ufo = Ufo::new(&view, 0.5, &mut Rng::new(0x5150)).unwrap();
        let [target_x, target_y] = ufo.target_world;
        let approaches = [
            [target_x + 260.0, target_y + 480.0, UFO_START_Z],
            [target_x - 520.0, target_y + 190.0, UFO_START_Z],
            [target_x + 520.0, target_y + 190.0, UFO_START_Z],
        ];
        let ctx = CritterCtx {
            life: view,
            dt: 0.0,
            now: 0.0,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        };
        let frame = 1.0 / 240.0;
        let distance =
            |a: [f32; 3], b: [f32; 3]| (a[0] - b[0]).hypot(a[1] - b[1]).hypot(a[2] - b[2]);

        // Exercise every approach family at several points in the hover-bob cycle.
        // Both frame steps should contain only ordinary motion; the old split formula
        // added the entire 66px hover lift to `after` in one frame.
        for origin in approaches {
            ufo.origin = origin;
            for boundary_t in [0.0, 0.58, 1.16, 2.33] {
                ufo.capture_age = Some(UFO_PULL_SECS - frame);
                ufo.t = boundary_t - frame;
                let before = ufo.captured_tile_position(&ctx, UFO_PULL_SECS - frame);

                ufo.capture_age = Some(UFO_PULL_SECS);
                ufo.t = boundary_t;
                let boundary = ufo.captured_tile_position(&ctx, UFO_PULL_SECS);

                ufo.capture_age = Some(UFO_PULL_SECS + frame);
                ufo.t = boundary_t + frame;
                let after = ufo.captured_tile_position(&ctx, UFO_PULL_SECS + frame);

                let pull_step = distance(before, boundary);
                let escape_step = distance(boundary, after);
                assert!(
                    pull_step < 1.0 && escape_step < 1.0,
                    "cargo jumped at pull/escape boundary: {pull_step:.3}px then \
                     {escape_step:.3}px from origin {origin:?} at t={boundary_t:.2}"
                );
                assert!(
                    (pull_step - escape_step).abs() < 0.25,
                    "cargo speed broke at pull/escape boundary: {pull_step:.3}px then \
                     {escape_step:.3}px"
                );
            }
        }
    }

    #[test]
    fn ufo_models_are_nonempty_and_the_beam_is_translucent_unlit_geometry() {
        for (label, (vertices, indices)) in [
            ("ufo", build_ufo()),
            ("captured tile", build_captured_tile()),
            ("beam", build_ufo_beam()),
        ] {
            assert!(!vertices.is_empty(), "{label} has no vertices");
            assert!(!indices.is_empty(), "{label} has no indices");
            assert!(indices.iter().all(|&i| (i as usize) < vertices.len()));
        }
        assert!(!MODEL_UNLIT[MODEL_UFO]);
        assert!(!MODEL_UNLIT[MODEL_CAPTURED_TILE]);
        assert!(MODEL_UNLIT[MODEL_UFO_BEAM]);

        let (ufo, _) = build_ufo();
        let amber = srgb_hex_to_linear(0xffc84f);
        let porthole_vertices = ufo.iter().filter(|vertex| vertex.col == amber).count();
        assert_eq!(
            porthole_vertices,
            8 * 6 * 10,
            "UFO does not contain eight distinct porthole lenses"
        );

        let (beam, beam_indices) = build_ufo_beam();
        assert_eq!(beam.len(), 65, "beam lost its filled footprint");
        assert_eq!(beam_indices.len(), 32 * 9);
        let (rot, len) = Ufo::beam_pose([0.0, 0.0, 0.0], [30.0, 40.0, 120.0]);
        assert!(rot[0] > 0.0 && rot[2].is_finite());
        assert!((len - 130.0).abs() < 1e-5);
    }

    #[test]
    fn tile_mimic_uses_only_footholds_safe_for_four_more_transitions() {
        let life = Life::new(28, 20, 0xC0FFEE_1234_5678);
        let mimic = Mimic::new(&life.view(), &mut Rng::new(0x71_1e_c4_ab))
            .expect("preview board should support the tile mimic");

        assert!((3..=6).contains(&mimic.route.len()));
        let creature_margin = MARGIN + 2;
        for (index, &(x, y)) in mimic.route.iter().enumerate() {
            assert!(x >= creature_margin && x < life.cols - creature_margin);
            assert!(y >= creature_margin && y < life.rows - creature_margin);
            assert!(
                Mimic::stable_for(&life.view(), x as isize, y as isize, MIMIC_SAFE_TRANSITIONS),
                "waypoint {index} does not survive four forthcoming transitions"
            );
        }
        for pair in mimic.route.windows(2) {
            let dx = pair[0].0.abs_diff(pair[1].0);
            let dy = pair[0].1.abs_diff(pair[1].1);
            assert!(dx <= 2 && dy <= 2 && (dx == 0 || dy == 0 || (dx == 1 && dy == 1)));
        }
    }

    #[test]
    fn tile_mimic_unfolds_into_a_complete_rig_then_camouflages_cleanly() {
        for (label, model) in [
            ("body", build_mimic_body()),
            ("segment", build_mimic_segment()),
            ("eye", build_mimic_eye()),
            ("claw", build_mimic_claw()),
            ("key", build_mimic_key()),
        ] {
            assert!(!model.0.is_empty(), "{label} has no vertices");
            assert!(!model.1.is_empty(), "{label} has no indices");
            assert!(model.1.iter().all(|&i| (i as usize) < model.0.len()));
        }
        for model in MODEL_MIMIC_BODY..=MODEL_MIMIC_KEY {
            assert!(!MODEL_UNLIT[model], "mimic model {model} lost 3D lighting");
        }

        let life = Life::new(28, 20, 0xC0FFEE_1234_5678);
        let mut mimic = Mimic::new(&life.view(), &mut Rng::new(0x71_1e_c4_ab)).unwrap();

        mimic.age = MIMIC_HATCH_SECS + MIMIC_STEP_SECS * 0.5;
        let mut active = PropSink::default();
        mimic.props_at(&mimic.pose(), &mut active);
        assert_eq!(active.group(MODEL_CAPTURED_TILE).len(), 1);
        assert_eq!(active.group(MODEL_MIMIC_BODY).len(), 1);
        assert_eq!(active.group(MODEL_MIMIC_SEGMENT).len(), 22);
        assert_eq!(active.group(MODEL_MIMIC_EYE).len(), 2);
        assert_eq!(active.group(MODEL_MIMIC_CLAW).len(), 2);
        assert_eq!(active.group(MODEL_MIMIC_KEY).len(), 1);
        assert!(active
            .group(MODEL_MIMIC_SEGMENT)
            .iter()
            .all(|segment| segment.alpha > 0.99));

        mimic.age = MIMIC_HATCH_SECS + mimic.travel_secs() + MIMIC_INSPECT_SECS * 0.45;
        let sampling = mimic.pose();
        assert!(sampling.sample_flash > 0.30);
        assert!(sampling.shell_mix > 0.20 && sampling.shell_mix < 0.90);

        let hide_starts =
            MIMIC_HATCH_SECS + mimic.travel_secs() + MIMIC_INSPECT_SECS + MIMIC_ALIGN_SECS;
        let mut hidden_yaws = Vec::new();
        for hide in [0.02, 0.35, 0.78] {
            mimic.age = hide_starts + MIMIC_HIDE_SECS * hide;
            let hiding = mimic.pose();
            let quarter_turns = hiding.yaw / std::f32::consts::FRAC_PI_2;
            assert!((quarter_turns - quarter_turns.round()).abs() < 0.001);
            let mut frame = PropSink::default();
            mimic.props_at(&hiding, &mut frame);
            hidden_yaws.push(frame.group(MODEL_CAPTURED_TILE)[0].rot[2]);
        }
        assert!(
            hidden_yaws
                .windows(2)
                .all(|pair| (pair[0] - pair[1]).abs() < 0.001),
            "the shell twists while becoming a tile"
        );

        mimic.age = hide_starts + MIMIC_HIDE_SECS + MIMIC_EYE_LINGER_SECS * 0.5;
        let peeking = mimic.pose();
        let mut eyes_only = PropSink::default();
        mimic.props_at(&peeking, &mut eyes_only);
        assert!(eyes_only.group(MODEL_MIMIC_BODY).is_empty());
        assert!(eyes_only.group(MODEL_MIMIC_SEGMENT).is_empty());
        assert_eq!(eyes_only.group(MODEL_MIMIC_EYE).len(), 2);
        assert!(eyes_only
            .group(MODEL_MIMIC_EYE)
            .iter()
            .all(|eye| eye.alpha > 0.99 && eye.scale[2] < eye.scale[0] * 0.15));
        assert!(eyes_only.group(MODEL_MIMIC_CLAW).is_empty());
        assert!(eyes_only.group(MODEL_MIMIC_KEY).is_empty());

        for linger in [0.25, 0.75] {
            mimic.age = hide_starts + MIMIC_HIDE_SECS + MIMIC_EYE_LINGER_SECS * linger;
            let open = mimic.pose();
            let mut open_frame = PropSink::default();
            mimic.props_at(&open, &mut open_frame);
            assert!(open_frame
                .group(MODEL_MIMIC_EYE)
                .iter()
                .all(|eye| eye.scale[2] > eye.scale[0] * 0.95));
        }

        mimic.age = mimic.total_secs() - 0.001;
        let folded = mimic.pose();
        let mut hidden = PropSink::default();
        mimic.props_at(&folded, &mut hidden);
        assert!(hidden.group(MODEL_MIMIC_BODY).is_empty());
        assert!(hidden.group(MODEL_MIMIC_SEGMENT).is_empty());
        assert!(hidden.group(MODEL_MIMIC_EYE).is_empty());
        assert!(hidden.group(MODEL_MIMIC_CLAW).is_empty());
        assert!(hidden.group(MODEL_MIMIC_KEY).is_empty());
        let shell = hidden.group(MODEL_CAPTURED_TILE)[0];
        assert!((shell.scale[0] - 1.0).abs() < 0.001);
        assert!((shell.pos[0] - mimic.world.last().unwrap()[0]).abs() < 0.01);
        assert!((shell.pos[1] - mimic.world.last().unwrap()[1]).abs() < 0.01);
        let quarter_turns = shell.rot[2] / std::f32::consts::FRAC_PI_2;
        assert!((quarter_turns - quarter_turns.round()).abs() < 0.001);
    }

    #[test]
    fn bee_flies_from_lower_right_toward_the_left_and_retires() {
        let life = planted(32, 22, &[]);
        let mut rng = Rng::new(0xbeef);
        let mut bee = Bee::new(&life.view(), &mut rng);
        let start = [bee.x, bee.y];
        assert!(bee.x > 0.0 && bee.y < 0.0, "did not begin at lower right");
        assert!(
            bee.vx < 0.0 && bee.vy > 0.0,
            "did not enter left and upward"
        );

        let mut t = 0.0f32;
        let mut alive = true;
        for _ in 0..2400 {
            alive = bee.update(&walker_ctx(&life, t));
            t += 1.0 / 60.0;
            if !alive {
                break;
            }
        }
        assert!(!alive, "bee never retired after leaving the frame");
        assert!(
            bee.x < start[0] - CELL_PX * 4.0,
            "bee did not make meaningful leftward progress"
        );
    }

    fn stable_colored_block(cols: usize, rows: usize) -> (Life, isize, isize) {
        for row in MARGIN..rows - MARGIN - 1 {
            for col in MARGIN..cols - MARGIN - 1 {
                // Pick a coloured cell on the lower row, so the test also proves that
                // another live tile on its screen-top edge does not matter.
                for target_col in [col, col + 1] {
                    if is_colored(target_col, row + 1) {
                        return (
                            planted(
                                cols,
                                rows,
                                &[
                                    (col, row),
                                    (col + 1, row),
                                    (col, row + 1),
                                    (col + 1, row + 1),
                                ],
                            ),
                            target_col as isize,
                            (row + 1) as isize,
                        );
                    }
                }
            }
        }
        panic!("test board contains no coloured 2x2 placement")
    }

    #[test]
    fn bee_courses_to_coloured_faces_and_ignores_grey_ones() {
        let (cols, rows) = (28usize, 20usize);
        let (life, flower_col, flower_row) = stable_colored_block(cols, rows);
        let view = life.view();
        assert!(Bee::landing_is_live(&view, flower_col, flower_row));

        let mut rng = Rng::new(0xc0110);
        let mut bee = Bee::new(&view, &mut rng);
        let flower = Bee::tile_face(&view, flower_col, flower_row);
        bee.x = flower[0] + CELL_PX * 3.0;
        bee.y = flower[1] - CELL_PX * 0.6;
        let (col, row, _, _) = bee.course_ahead(&view).expect("no flower course plotted");
        assert!(
            is_colored(col as usize, row as usize),
            "planner selected a neutral tile"
        );

        let grey = (MARGIN..cols - MARGIN)
            .flat_map(|x| (MARGIN..rows - MARGIN).map(move |y| (x, y)))
            .find(|&(x, y)| !is_colored(x, y))
            .expect("test board contains no neutral cell");
        let grey_life = planted(cols, rows, &[grey]);
        assert!(
            !Bee::landing_is_live(&grey_life.view(), grey.0 as isize, grey.1 as isize),
            "bee treated a live grey face as a flower"
        );
    }

    #[test]
    fn bee_scuttles_but_stays_until_its_flower_changes() {
        let (life, col, row) = stable_colored_block(24, 18);
        let view = life.view();
        let mut rng = Rng::new(0xb00b1e);
        let mut bee = Bee::new(&view, &mut rng);
        let centre = Bee::tile_face(&view, col, row);
        [bee.x, bee.y] = centre;
        bee.act = BeeAct::Landed { col, row };
        bee.choose_scuttle(&view, col, row);

        let start = [bee.x, bee.y];
        let mut farthest = 0.0f32;
        for frame in 0..60 * 10 {
            assert!(
                bee.update(&walker_ctx(&life, frame as f32 / 60.0)),
                "landed bee retired"
            );
            assert!(
                matches!(bee.act, BeeAct::Landed { .. }),
                "bee left an unchanged, non-spinning flower"
            );
            let from_centre = (bee.x - centre[0]).hypot(bee.y - centre[1]);
            farthest = farthest.max((bee.x - start[0]).hypot(bee.y - start[1]));
            assert!(
                from_centre <= BEE_SCUTTLE_RADIUS + 0.01,
                "scuttle escaped its tile by {from_centre:.1}px"
            );
        }
        assert!(farthest > 3.0, "bee never scuttled around the flower");
    }

    #[test]
    fn spinning_flower_kicks_bee_toward_the_camera_then_releases_it() {
        let (life, col, row) = stable_colored_block(24, 18);
        let view = life.view();
        let mut rng = Rng::new(0xbee);
        let mut bee = Bee::new(&view, &mut rng);
        let origin = Bee::tile_face(&view, col, row);
        [bee.x, bee.y] = origin;
        bee.act = BeeAct::Landed { col, row };
        bee.choose_scuttle(&view, col, row);

        let mut spins = vec![false; view.cols() * view.rows()];
        spins[row as usize * view.cols() + col as usize] = true;
        let mut kicked = walker_ctx(&life, 0.0);
        kicked.spinning = Some(&spins);
        bee.update(&kicked);
        assert!(
            matches!(bee.act, BeeAct::Kicked { .. }),
            "tile spin did not kick the bee"
        );

        for frame in 1..=6 {
            bee.update(&walker_ctx(&life, frame as f32 / 60.0));
        }
        let mut midair = PropSink::default();
        bee.props(&walker_ctx(&life, 6.0 / 60.0), &mut midair);
        assert!(
            midair.group(MODEL_BEE_BODY)[0].scale[0] > BEE_BODY_W * 1.55,
            "camera kick did not make the bee loom"
        );
        for leg in MODEL_BEE_LEG_FIRST..=MODEL_BEE_LEG_LAST {
            assert_eq!(midair.group(leg).len(), 1, "leg layer {leg} is missing");
        }
        assert!(
            MODEL_BEE_WING > MODEL_BEE_BODY,
            "flight wings must draw over and obscure the thorax"
        );

        // The camera loom is already over while the outward shove is still resolving.
        for frame in 7..=13 {
            bee.update(&walker_ctx(&life, frame as f32 / 60.0));
        }
        let mut recovered_depth = PropSink::default();
        bee.props(&walker_ctx(&life, 13.0 / 60.0), &mut recovered_depth);
        assert!(
            recovered_depth.group(MODEL_BEE_BODY)[0].scale[0] <= BEE_BODY_W * 1.01,
            "bee stayed enlarged after dropping back near the tile plane"
        );

        for frame in 14..=28 {
            bee.update(&walker_ctx(&life, frame as f32 / 60.0));
        }
        assert!(
            matches!(bee.act, BeeAct::Flying | BeeAct::Approach { .. }),
            "bee did not return to its flower-to-flower flight"
        );
        assert!(
            (bee.x - origin[0]).hypot(bee.y - origin[1]) > BEE_KICK_AWAY * 0.70,
            "kick did not carry the bee away from its flower"
        );
    }

    #[test]
    fn bee_legs_wiggle_lightly_in_flight_and_aggressively_while_walking() {
        let life = planted(24, 18, &[]);
        let mut rng = Rng::new(77);
        let mut bee = Bee::new(&life.view(), &mut rng);
        let mut flight_max = 0.0f32;
        let mut walk_max = 0.0f32;
        for frame in 0..240 {
            bee.t = frame as f32 / 60.0;
            bee.act = BeeAct::Flying;
            flight_max = flight_max.max(bee.leg_wiggle(0).abs());
            bee.act = BeeAct::Landed { col: 5, row: 5 };
            walk_max = walk_max.max(bee.leg_wiggle(0).abs());
        }
        assert!(
            flight_max > 0.04 && flight_max < 0.07,
            "flight leg motion is not a subtle wiggle ({flight_max:.3})"
        );
        assert!(
            walk_max > flight_max * 4.0,
            "walking legs are not substantially more animated ({walk_max:.3})"
        );
    }

    #[test]
    fn landed_bee_has_crisp_twitching_wings_and_startles_when_its_tile_goes() {
        let (cols, rows) = (24usize, 18usize);
        let (life, col, row) = stable_colored_block(cols, rows);
        let view = life.view();
        let mut rng = Rng::new(92);
        let mut bee = Bee::new(&view, &mut rng);
        bee.t = 5.0;
        // Row 11 has another live tile immediately above it. That made it ineligible
        // under the old side-view edge landing, but its visible square face is valid.
        bee.x = 0.0;
        bee.y = 0.0;
        bee.next_twitch = bee.t;
        bee.act = BeeAct::Landed { col, row };
        let face = Bee::tile_face(&view, col, row);
        [bee.x, bee.y] = face;
        bee.choose_scuttle(&view, col, row);

        bee.update(&walker_ctx(&life, 5.0));
        assert!(
            (bee.x - face[0]).hypot(bee.y - face[1]) <= BEE_SCUTTLE_RADIUS,
            "bee walked beyond the tile face"
        );
        assert!(
            matches!(bee.act, BeeAct::Landed { .. }),
            "the occupied screen-top edge incorrectly startled the bee"
        );
        let first_crisp_pose = bee.wing_phase;
        let mut landed = PropSink::default();
        bee.props(&walker_ctx(&life, 5.0), &mut landed);
        assert_eq!(
            landed.group(MODEL_BEE_WING).len(),
            4,
            "landed wings should be two crisp nested pairs"
        );
        assert_eq!(landed.group(MODEL_BEE_BODY).len(), 1);

        for frame in 1..30 {
            bee.update(&walker_ctx(&life, 5.0 + frame as f32 / 60.0));
        }
        assert_eq!(
            bee.wing_phase, first_crisp_pose,
            "crisp wings changed between their roughly one-second twitches"
        );

        let empty = planted(cols, rows, &[]);
        bee.update(&walker_ctx(&empty, 5.5));
        assert_eq!(bee.act, BeeAct::Flying, "bee stayed on a vanished tile");
        assert!(
            bee.vx < -100.0 && bee.vy > 70.0,
            "startle did not launch it sharply left and upward"
        );
        let mut flying = PropSink::default();
        bee.props(&walker_ctx(&empty, 5.5), &mut flying);
        assert_eq!(
            flying.group(MODEL_BEE_WING).len(),
            8,
            "flying wings should fan into several blurred exposures"
        );
        assert_eq!(flying.group(MODEL_BEE_BODY).len(), 1);
    }

    /// The rocket must never snap round.
    ///
    /// Attitude is derived from the path, and deriving it from the *instantaneous*
    /// speed put a singularity precisely where the gust drives that speed through zero:
    /// the nose whipped to 87 degrees and the roll carried the model past upside down,
    /// at some 4500 degrees a second, several times a crossing. The bound here is on the
    /// angular rate, since that is what reads as a glitch rather than as flying.
    #[test]
    fn rocket_attitude_stays_smooth() {
        let life = Life::new(28, 20, 5);
        for seed in 0..8u64 {
            let mut rng = Rng::new(seed * 13 + 1);
            let mut r = Rocket::new(&life.view(), &mut rng);
            let ctx = |now: f32| CritterCtx {
                life: life.view(),
                dt: 1.0 / 240.0,
                now,
                phase: 0.0,
                gen_secs: 3.0,
                spinning: None,
            };

            let mut t = 0.0f32;
            let mut prev: Option<[f32; 3]> = None;
            let start_roll = r.pose().1[0];
            let mut frames = 0;
            while r.update(&ctx(t)) && frames < 240 * 40 {
                t += 1.0 / 240.0;
                frames += 1;
                let (_, rot) = r.pose();

                // Yaw and pitch remain coupled to the path while the local axial roll
                // deliberately keeps turning to show all three fins.
                assert!(
                    rot[2].abs() < ROCKET_YAW_MAX + 0.05,
                    "seed {seed}: yawed to {:.0} degrees",
                    rot[2].to_degrees()
                );
                assert!(
                    rot[1].abs() < ROCKET_PITCH_MAX + 0.05,
                    "seed {seed}: pitched to {:.0} degrees",
                    rot[1].to_degrees()
                );
                if let Some(p) = prev {
                    // Pitch is allowed to move faster than the rest: nosing over toward
                    // the camera is meant to be a decisive turn, and it is the cue that
                    // makes changing sides legible at all.
                    for (axis, name, limit) in
                        [(0usize, "roll", 2.5f32), (2, "yaw", 2.5), (1, "pitch", 5.5)]
                    {
                        let rate = (rot[axis] - p[axis]).abs() * 240.0;
                        assert!(
                            rate < limit,
                            "seed {seed}: {name} moved at {:.0} degrees a second",
                            rate.to_degrees()
                        );
                    }
                }
                prev = Some(rot);
            }
            assert!(
                prev.unwrap()[0] - start_roll > std::f32::consts::PI,
                "seed {seed}: axial roll did not expose the full fin arrangement"
            );
        }
    }

    /// Nothing in the depth-writing run may be invisible.
    ///
    /// The run holds tiles that are on plus tiles still fading out, and the fade is far
    /// shorter than a generation. Retiring only on the generation boundary left fully
    /// transparent tiles in there for seconds, writing depth and eclipsing anything
    /// behind them — which showed up as a rocket disappearing behind nothing at all.
    /// Invisible-but-occluding geometry is impossible to spot until something passes
    /// behind it, so it is worth asserting directly.
    #[test]
    fn the_solid_run_holds_nothing_invisible() {
        let (cols, rows) = grid_dims(1400.0, 900.0);
        let mut driver = Driver::new(cols, rows, 4242, 0.0);

        let mut t = 0.0f64;
        let mut checked = 0usize;
        let mut fading_seen = 0usize;
        while t < 30.0 {
            t += 1.0 / 60.0;
            driver.advance(t, 1.0);
            let now = driver.shader_clock(t);
            let (list, solid) = driver.viz.draw_list();

            for inst in &list[..solid as usize] {
                if inst.palette != PALETTE_FROM_CELL {
                    continue; // a critter's own tile, not a grid cell
                }
                let on = inst.state[0] > 0.5;
                let fading = inst.state[0] != inst.state[1] && now - inst.t < DEATH_FADE;
                if fading {
                    fading_seen += 1;
                }
                assert!(
                    on || fading,
                    "an invisible tile is still writing depth: state {:?}, {:.2}s since \
                     it changed, fade lasts {DEATH_FADE}s",
                    inst.state,
                    now - inst.t
                );
                checked += 1;
            }
        }
        assert!(checked > 100_000, "did not inspect much ({checked})");
        // And the run must genuinely carry tiles mid-fade, or the assertion above would
        // be trivially satisfied by never putting anything transitional in there.
        assert!(fading_seen > 100, "no tiles were ever caught mid-fade");
    }

    /// Flies one rocket across a live field, advancing generations at the real cadence and
    /// handing each frame to the caller.
    ///
    /// Driving the rocket directly rather than waiting for the scene to spawn one keeps
    /// these tests independent of whatever the spawn schedule happens to be doing — and it
    /// has to now, since rockets are parked.
    fn fly_rocket(seed: u64, mut visit: impl FnMut(&Rocket, &LifeView)) {
        let (cols, rows) = grid_dims(1600.0, 1000.0);
        let mut life = Life::new(cols, rows, seed);
        let mut rng = Rng::new(seed ^ 0x9e37_79b9);
        let mut rocket = Rocket::new(&life.view(), &mut rng);

        let gen_secs = (1.0 / SLOW_HZ) as f32;
        let (mut t, mut next_gen) = (0.0f32, gen_secs);
        loop {
            let alive = {
                let ctx = CritterCtx {
                    life: life.view(),
                    dt: 1.0 / 60.0,
                    now: t,
                    phase: 0.0,
                    gen_secs,
                    spinning: None,
                };
                let alive = rocket.update(&ctx);
                if alive {
                    visit(&rocket, &ctx.life);
                }
                alive
            };
            if !alive || t > 60.0 {
                break;
            }
            t += 1.0 / 60.0;
            if t >= next_gen {
                life.advance(1);
                next_gen += gen_secs;
            }
        }
    }

    /// Going behind the tiles has to be something you can *see*.
    ///
    /// This is the property that was quietly missing. The rocket did cross the tile plane,
    /// but it only ever dived through a corridor verified clear for several generations —
    /// so it went behind precisely where there was nothing to go behind, and surfaced
    /// before reaching anything. Measured across a whole crossing, not one tile ever passed
    /// over it: the manoeuvre was invisible by construction. Hence a test on occlusion
    /// actually happening rather than on the flag being set.
    #[test]
    fn diving_behind_the_tiles_is_visible() {
        let (mut flights, mut total_behind, mut total_covered, mut total_front) = (0, 0, 0, 0);
        let mid = (ROCKET_FRONT_Z + ROCKET_BEHIND_Z) * 0.5;

        for seed in [12345u64, 777, 90210, 5150, 31337, 8675309] {
            let (mut behind, mut covered, mut front) = (0, 0, 0);
            fly_rocket(seed, |r, view| {
                let (pos, _) = r.pose();
                // Under the plane means the tiles can occlude it at all.
                if pos[2] > mid {
                    front += 1;
                    return;
                }
                behind += 1;
                // Is a live tile actually over it right now?
                let (cx, cy) = r.cell_ahead(view, 0);
                if (-1isize..=1).any(|dx| view.alive(cx + dx, cy, 0)) {
                    covered += 1;
                }
            });

            assert!(
                behind > 60,
                "seed {seed}: barely went behind ({behind} frames)"
            );
            assert!(
                front > 60,
                "seed {seed}: barely stayed in front ({front} frames)"
            );
            flights += 1;
            total_behind += behind;
            total_covered += covered;
            total_front += front;
        }

        assert_eq!(flights, 6);
        // Most of its time under the plane, something should be over it. The old behaviour
        // scored two percent here.
        let share = total_covered as f32 / total_behind as f32;
        assert!(
            share > 0.25,
            "a tile was over the rocket for only {:.0}% of its time behind the plane",
            share * 100.0
        );
        // And it must not simply live down there — the weave needs both sides.
        let behind_share = total_behind as f32 / (total_behind + total_front) as f32;
        assert!(
            (0.10..0.60).contains(&behind_share),
            "spent {:.0}% of the crossing behind the tiles",
            behind_share * 100.0
        );
    }

    /// The packed draw list changes on frames where no generation landed.
    ///
    /// This is the property that forces the instance buffer to be re-uploaded every
    /// frame. Tiles retire from the depth-writing run when their fade finishes, which is
    /// partway *through* a generation, and retiring one shifts every slot after it. So
    /// uploading only when the simulation stepped left the GPU holding a stale layout
    /// while the draw call used freshly-read counts — slots meant the wrong tiles and the
    /// field flickered. If the upload is ever made conditional again, this is the
    /// assumption it would be violating.
    #[test]
    fn the_draw_list_shifts_between_generations() {
        let (cols, rows) = grid_dims(1200.0, 800.0);
        let mut driver = Driver::new(cols, rows, 2024, 0.0);

        let key = |driver: &Driver| {
            let (list, solid) = driver.viz.draw_list();
            // Layout identity: what is in each slot, and where the runs divide.
            (
                solid,
                list.iter()
                    .map(|i| (i.cell[0] as i32, i.cell[1] as i32))
                    .collect::<Vec<_>>(),
            )
        };

        let mut t = 0.0f64;
        let mut prev_gen = driver.gen;
        let mut shifted_without_a_generation = 0;

        let mut before = key(&driver);
        while t < 25.0 {
            t += 1.0 / 60.0;
            driver.advance(t, 1.0);
            let after = key(&driver);
            let stepped = driver.gen != prev_gen;
            prev_gen = driver.gen;
            if !stepped && after != before {
                shifted_without_a_generation += 1;
            }
            before = after;
        }

        // Roughly once per generation: every tile that changed in a given step shares a
        // timestamp, so they all retire together on one frame partway through. One shift
        // is quite enough — the mismatch it opens then lasts for the rest of the
        // generation, which is most of the time.
        let generations = driver.gen;
        assert!(
            shifted_without_a_generation >= generations / 2,
            "the draw list shifted outside a generation step only \
             {shifted_without_a_generation} times over {generations} generations"
        );
    }

    /// Drops a walker from a given height above a given column and runs him until he
    /// settles or falls out of the world.
    /// Builds a walker over a given column and height directly, bypassing the spawn-site
    /// search so a test can put him exactly where it wants him.
    fn drop_walker(life: &Life, col: isize, from_row: f32) -> Walker {
        let view = life.view();
        let half_h = view.rows() as f32 * CELL_PX * 0.5;
        let half_w = view.cols() as f32 * CELL_PX * 0.5;
        let margin = MARGIN as f32 * CELL_PX;
        let hip_y = view.cell_center(col as f32, from_row)[1];
        Walker {
            col,
            x: view.cell_center(col as f32, 0.0)[0],
            vx: 0.0,
            hip_y,
            vy: 0.0,
            fell_from: hip_y,
            vis_bottom: -(half_h - margin),
            edge: half_w,
            doomed: false,
            grab_plan: None,
            landing_plan: None,
            no_land_until: 0.0,
            hang_release_at: f32::INFINITY,
            last_stuck: None,
            headgear: Headgear::Tiki,
            act: Act::Airborne,
            pose: Pose::falling(0.0),
            support: None,
            facing: 1.0,
            t: 0.0,
            seed: 0.0,
            rng: Rng::new(1),
        }
    }

    fn walker_ctx<'a>(life: &'a Life, now: f32) -> CritterCtx<'a> {
        CritterCtx {
            life: life.view(),
            dt: 1.0 / 60.0,
            now,
            phase: 0.0,
            gen_secs: 3.0,
            spinning: None,
        }
    }

    /// Put a walker on the right edge of one stable block and launch him toward a second
    /// one across a one-cell gap, using the production planner to choose the catch speed.
    fn planned_ledge_jump(life: &Life) -> (Walker, GrabPlan) {
        let view = life.view();
        let mut w = drop_walker(life, 5, 4.0);
        w.col = 5;
        w.x = Walker::cell_x(&view, 5);
        w.pose = Pose::standing();
        w.support = Some(12);
        w.hip_y = w.surface_of(&view, 12) + w.leg_reach();
        w.facing = 1.0;

        let planned = (0..100u64).find_map(|seed| {
            w.rng = Rng::new(seed * 71 + 9);
            w.plan_grab(&view, 1.0)
        });
        let (plan, vx) = planned.expect("test geometry did not offer a planned catch");
        w.leave_ground(WALKER_JUMP_V, vx);
        w.grab_plan = Some(plan);
        (w, plan)
    }

    /// Every rod the figure draws, as (start, end, length).
    fn walker_segments(w: &Walker) -> (Vec<([f32; 2], [f32; 2], f32)>, Prop) {
        let mut sink = PropSink::default();
        w.draw_figure(&mut sink);
        let rods = sink
            .group(MODEL_ROD)
            .iter()
            .map(|p| {
                let len = p.scale[0];
                let a = p.rot[2];
                let start = [p.pos[0], p.pos[1]];
                (
                    start,
                    [start[0] + len * a.cos(), start[1] + len * a.sin()],
                    len,
                )
            })
            .collect();
        assert!(
            sink.group(MODEL_DISC).is_empty(),
            "the old disc head is still present"
        );
        let headgear: Vec<Prop> = sink
            .group(MODEL_TIKI_MASK)
            .iter()
            .chain(sink.group(MODEL_ASTRONAUT_HELMET))
            .chain(sink.group(MODEL_NOH_MASK))
            .chain(sink.group(MODEL_MONKEY_MASK))
            .chain(sink.group(MODEL_COMEDY_TRAGEDY_MASK))
            .copied()
            .collect();
        assert_eq!(
            headgear.len(),
            1,
            "walker should wear exactly one headpiece"
        );
        (rods, headgear[0])
    }

    /// He has to come to rest with his feet on the surface of the tile he landed on, and
    /// on the *right* tile — the highest one under him, not merely one that exists
    /// somewhere below. Getting that wrong first time teleported him to the ground on the
    /// frame he spawned, because "what is beneath me" was being read as "I have landed".
    #[test]
    fn walker_lands_on_the_tile_beneath_him() {
        let (cols, rows) = (24isize, 18isize);
        // Two still lifes in the same column: a 2x2 block high up and another far below.
        // He must settle on the upper one.
        let life = planted(
            cols as usize,
            rows as usize,
            &[
                (5, 8),
                (6, 8),
                (5, 9),
                (6, 9),
                (5, 15),
                (6, 15),
                (5, 16),
                (6, 16),
            ],
        );
        let mut w = drop_walker(&life, 5, 1.0);

        let mut t = 0.0f32;
        for _ in 0..900 {
            assert!(w.update(&walker_ctx(&life, t)), "fell out of the world");
            t += 1.0 / 60.0;
            if matches!(w.act, Act::Deciding(_)) {
                break;
            }
        }

        let row = w.support.expect("never landed");
        assert_eq!(row, 8, "landed on row {row}, not the first tile under him");
        let top = w.surface_of(&life.view(), row);
        assert!(
            (w.foot_y() - top).abs() < 0.75,
            "feet rest {:.2}px off the tile surface",
            w.foot_y() - top
        );
        assert_eq!(w.vy, 0.0, "still moving after settling");
    }

    /// And he drops again the moment that tile stops existing.
    #[test]
    fn walker_falls_when_his_footing_goes() {
        let (cols, rows) = (24usize, 18usize);
        // A lone cell has no neighbours, so it dies on the very next generation.
        let mut life = planted(cols, rows, &[(5, 9)]);
        let mut w = drop_walker(&life, 5, 2.0);

        let mut t = 0.0f32;
        for _ in 0..900 {
            w.update(&walker_ctx(&life, t));
            t += 1.0 / 60.0;
            if w.support.is_some() {
                break;
            }
        }
        assert_eq!(w.support, Some(9), "never landed on the cell");
        let stood_at = w.hip_y;

        // Pull the rug out.
        life.advance(0);
        assert!(!life.view().alive(5, 9, 0), "the cell should have died");

        w.update(&walker_ctx(&life, t));
        assert!(
            w.support.is_none(),
            "still supported by a tile that is gone"
        );
        assert_eq!(w.act, Act::Airborne, "did not start falling");

        for _ in 0..30 {
            t += 1.0 / 60.0;
            w.update(&walker_ctx(&life, t));
        }
        assert!(w.hip_y < stood_at - 5.0, "did not actually fall");
    }

    /// He leaves once he has dropped clear of the world, rather than falling forever.
    #[test]
    fn walker_retires_after_falling_off() {
        let (cols, rows) = (24usize, 18usize);
        let life = planted(cols, rows, &[]);
        let mut w = drop_walker(&life, 5, 1.0);

        let mut t = 0.0f32;
        let mut frames = 0;
        while w.update(&walker_ctx(&life, t)) {
            t += 1.0 / 60.0;
            frames += 1;
            assert!(frames < 3000, "never left over an empty board");
        }
        assert!(
            w.hip_y < -(rows as f32 * CELL_PX * 0.5),
            "retired too early"
        );
    }

    /// The figure is drawn from one unit bar under different stretches, so the only thing
    /// keeping his limbs the right length is the forward kinematics. Check the segments
    /// are exactly the declared lengths in every pose he passes through, and that nothing
    /// strays outside a body-sized box — a limb attached at the wrong joint would show up
    /// as a stray rather than as a wrong length.
    #[test]
    fn walker_limbs_keep_their_lengths_and_stay_attached() {
        let (cols, rows) = (24usize, 18usize);
        let life = planted(cols, rows, &[(5, 9), (6, 9), (5, 10), (6, 10)]);
        let mut w = drop_walker(&life, 5, 1.0);

        let mut want = vec![
            WALKER_TORSO,
            WALKER_SHOULDER * 2.0,
            WALKER_UPPER_ARM,
            WALKER_UPPER_ARM,
            WALKER_FOREARM,
            WALKER_FOREARM,
            WALKER_THIGH,
            WALKER_THIGH,
            WALKER_SHIN,
            WALKER_SHIN,
        ];
        want.sort_by(|a, b| a.partial_cmp(b).unwrap());

        let mut t = 0.0f32;
        let mut checked = 0;
        // He may well run off the block and out of the world within this span, which is
        // fine — every frame he *is* alive for has to hold up.
        while checked < 420 && w.update(&walker_ctx(&life, t)) {
            t += 1.0 / 60.0;

            let (rods, mask) = walker_segments(&w);
            assert_eq!(rods.len(), 10, "expected ten strokes, got {}", rods.len());
            let mut got: Vec<f32> = rods.iter().map(|(_, _, l)| *l).collect();
            got.sort_by(|a, b| a.partial_cmp(b).unwrap());
            for (g, wl) in got.iter().zip(want.iter()) {
                assert!((g - wl).abs() < 0.01, "segment {g:.2} should be {wl:.2}");
            }

            // Nothing may sit further from the hip than a body's worth.
            let hip = [w.x, w.hip_y];
            for (a, b, _) in &rods {
                for pt in [a, b] {
                    let d = ((pt[0] - hip[0]).powi(2) + (pt[1] - hip[1]).powi(2)).sqrt();
                    assert!(d < WALKER_H, "a limb reaches {d:.0}px from the hip");
                }
            }
            let md = ((mask.pos[0] - hip[0]).powi(2) + (mask.pos[1] - hip[1]).powi(2)).sqrt();
            assert!(
                md < WALKER_MASKED_H,
                "the mask centre is {md:.0}px from the hip"
            );
            checked += 1;
        }
        assert!(checked > 120, "only got {checked} frames of him");
    }

    /// Standing, the replacement is unmistakably oversized: broader than the old head,
    /// over the shoulder line, and tall enough to make the whole figure nearly two tiles.
    #[test]
    fn walker_mask_covers_the_neck_and_upper_chest() {
        let (cols, rows) = (24usize, 18usize);
        let life = planted(cols, rows, &[(5, 9), (6, 9), (5, 10), (6, 10)]);
        let mut w = drop_walker(&life, 5, 1.0);

        let mut t = 0.0f32;
        for _ in 0..900 {
            w.update(&walker_ctx(&life, t));
            t += 1.0 / 60.0;
            if matches!(w.act, Act::Deciding(_)) {
                break;
            }
        }
        assert!(matches!(w.act, Act::Deciding(_)), "never settled");
        // Hold him standing while the pose finishes easing out of the landing.
        for _ in 0..24 {
            w.act = Act::Deciding(0.0);
            w.update(&walker_ctx(&life, t));
            t += 1.0 / 60.0;
        }

        let (rods, mask) = walker_segments(&w);
        let (mut lo, mut hi) = (f32::MAX, f32::MIN);
        for (a, b, _) in &rods {
            for pt in [a, b] {
                lo = lo.min(pt[1]);
                hi = hi.max(pt[1]);
            }
        }
        hi = hi.max(mask.pos[1] + WALKER_MASK_H * 0.5);
        lo = lo.min(mask.pos[1] - WALKER_MASK_H * 0.5);

        let shoulder_y = w.hip_y + WALKER_TORSO;
        assert!(
            mask.pos[1] - WALKER_MASK_H * 0.5 < shoulder_y - WALKER_LINE,
            "mask bottom does not overlap the chest"
        );
        assert!(
            mask.scale[0] > WALKER_LINE * 7.0,
            "mask is not oversized relative to the line figure"
        );

        let tile = CELL_PX * TILE_FILL;
        let height = hi - lo;
        assert!(
            height > tile * 1.5 && height < tile * 1.75,
            "stands {height:.0}px against a {tile:.0}px tile"
        );
    }

    #[test]
    fn walkers_choose_headgear_evenly_and_draw_only_their_choice() {
        let mut rng = Rng::new(0xface_cafe);
        let mut counts = [0usize; 5];
        for _ in 0..50_000 {
            counts[match random_headgear(&mut rng) {
                Headgear::Tiki => 0,
                Headgear::Astronaut => 1,
                Headgear::Noh => 2,
                Headgear::Monkey => 3,
                Headgear::ComedyTragedy => 4,
            }] += 1;
        }
        let total = counts.iter().sum::<usize>() as f32;
        for (label, count) in ["tiki", "astronaut", "Noh", "monkey", "comedy/tragedy"]
            .into_iter()
            .zip(counts)
        {
            let share = count as f32 / total;
            assert!(
                (share - 0.2).abs() < 0.015,
                "headgear split was {:.1}% {label}",
                share * 100.0
            );
        }

        let life = planted(24, 18, &[]);
        let mut walker = drop_walker(&life, 5, 4.0);
        walker.pose = Pose::standing();
        for (headgear, model) in [
            (Headgear::Tiki, MODEL_TIKI_MASK),
            (Headgear::Astronaut, MODEL_ASTRONAUT_HELMET),
            (Headgear::Noh, MODEL_NOH_MASK),
            (Headgear::Monkey, MODEL_MONKEY_MASK),
            (Headgear::ComedyTragedy, MODEL_COMEDY_TRAGEDY_MASK),
        ] {
            walker.headgear = headgear;
            let mut sink = PropSink::default();
            walker.draw_figure(&mut sink);
            assert_eq!(sink.group(model).len(), 1);
            for other in [
                MODEL_TIKI_MASK,
                MODEL_ASTRONAUT_HELMET,
                MODEL_NOH_MASK,
                MODEL_MONKEY_MASK,
                MODEL_COMEDY_TRAGEDY_MASK,
            ] {
                if other != model {
                    assert!(sink.group(other).is_empty());
                }
            }
        }
    }

    /// Boxed in on both sides, he must pick one of the two stuck behaviours and never a
    /// run — running into a wall is a situation there is no animation for, which is the
    /// whole reason the decision consults the board rather than just trying it.
    #[test]
    fn walled_in_he_only_does_what_he_can() {
        let (cols, rows) = (24usize, 18usize);
        // A trench: floor across, with walls a row up on either side of the middle.
        let mut cells = vec![(4, 10), (5, 10), (6, 10)];
        cells.extend([(4, 9), (6, 9)]);
        let life = planted(cols, rows, &cells);

        let mut stuck = [0usize; 2];
        for seed in 0..40u64 {
            let mut w = drop_walker(&life, 5, 4.0);
            w.rng = Rng::new(seed * 31 + 7);

            let mut t = 0.0f32;
            let mut acted = None;
            for _ in 0..600 {
                if !w.update(&walker_ctx(&life, t)) {
                    break;
                }
                t += 1.0 / 60.0;
                match w.act {
                    Act::Hop(_) => {
                        acted = Some(0);
                        break;
                    }
                    Act::Shove { .. } => {
                        acted = Some(1);
                        break;
                    }
                    Act::Run { .. } => panic!("seed {seed}: tried to run into a wall"),
                    _ => {}
                }
            }
            stuck[acted.unwrap_or_else(|| panic!("seed {seed}: never did anything"))] += 1;
        }
        // Both behaviours should show up; a coin flip that always lands the same way
        // would mean one of them is unreachable.
        assert!(
            stuck[0] > 4 && stuck[1] > 4,
            "stuck behaviours split {stuck:?}"
        );
    }

    /// Once boxed in, the next decision changes routines. This prevents a run of identical
    /// little hops and guarantees that the straining shove gets a turn.
    #[test]
    fn boxed_idle_routines_alternate() {
        let (cols, rows) = (24usize, 18usize);
        let life = planted(cols, rows, &[(4, 9), (6, 9), (4, 10), (5, 10), (6, 10)]);
        let mut w = drop_walker(&life, 5, 4.0);
        w.support = Some(10);
        w.col = 5;
        w.x = Walker::cell_x(&life.view(), 5);

        w.decide(&life.view());
        let first_shove = matches!(w.act, Act::Shove { .. });
        assert!(
            first_shove || matches!(w.act, Act::Hop(_)),
            "picked a non-boxed action"
        );
        w.act = Act::Deciding(0.0);
        w.decide(&life.view());
        assert_eq!(
            matches!(w.act, Act::Shove { .. }),
            !first_shove,
            "repeated the same boxed idle"
        );
    }

    /// Given room, he runs the roomier way and leaps — and the leap grows with the
    /// approach, which is what makes a long runway worth having.
    #[test]
    fn open_ground_makes_him_run_and_jump() {
        let (cols, rows) = (30usize, 18usize);
        // Floor stretching well to the right of where he lands, nothing to the left.
        let floor: Vec<(usize, usize)> = (6..=12).map(|c| (c, 10)).collect();
        let life = planted(cols, rows, &floor);

        let mut w = drop_walker(&life, 6, 4.0);
        w.rng = Rng::new(3);

        let mut t = 0.0f32;
        let mut ran = None;
        for _ in 0..900 {
            assert!(w.update(&walker_ctx(&life, t)));
            t += 1.0 / 60.0;
            if let Act::Run { dir, take_off, .. } = w.act {
                ran = Some((dir, take_off));
                break;
            }
        }
        let (dir, take_off) = ran.expect("never set off");
        assert_eq!(dir, 1.0, "ran toward the edge instead of the open floor");
        assert!(
            take_off > w.x + CELL_PX * 2.0,
            "barely bothered running before jumping"
        );

        // See it through: he should leave the ground travelling forwards.
        let start_x = w.x;
        for _ in 0..900 {
            if !w.update(&walker_ctx(&life, t)) {
                break;
            }
            t += 1.0 / 60.0;
            if w.support.is_none() && w.vy > 0.0 {
                break;
            }
        }
        assert!(w.vy > 0.0, "never got airborne");
        assert!(w.vx > 0.0, "jumped without carrying the run into it");
        assert!(w.x > start_x + CELL_PX, "jumped from where he started");
    }

    /// When another stable block is in range, leaving the current perch should be a
    /// planned leap that comes back down on screen, not the old guaranteed exit.
    #[test]
    fn walker_prefers_and_completes_a_visible_block_to_block_landing() {
        let (cols, rows) = (24usize, 22usize);
        let life = planted(
            cols,
            rows,
            &[
                (4, 12),
                (5, 12),
                (4, 13),
                (5, 13),
                (7, 12),
                (8, 12),
                (7, 13),
                (8, 13),
            ],
        );
        let view = life.view();
        let mut w = drop_walker(&life, 5, 4.0);
        w.col = 5;
        w.x = Walker::cell_x(&view, 5);
        w.pose = Pose::standing();
        w.support = Some(12);
        w.hip_y = w.surface_of(&view, 12) + w.leg_reach();

        w.decide(&view);
        assert!(
            matches!(w.act, Act::Leap { dir: 1.0, .. }),
            "did not choose the reachable block to the right: {:?}",
            w.act
        );

        let mut t = 0.0f32;
        let mut saw_planned_flight = false;
        let mut landed = false;
        for _ in 0..240 {
            assert!(w.update(&walker_ctx(&life, t)), "left the screen");
            t += 1.0 / 60.0;
            saw_planned_flight |= w.support.is_none() && w.landing_plan.is_some();
            if saw_planned_flight && w.support.is_some() {
                landed = true;
                break;
            }
        }
        assert!(saw_planned_flight, "the leap never carried a landing plan");
        assert!(landed, "the planned leap never came back down");
        assert!(
            matches!(w.col, 7 | 8),
            "landed in column {}, not on the destination block",
            w.col
        );
    }

    /// A block can be born after a run starts. The runner must stop at its face on that
    /// very frame rather than entering the still-fading tile and relying on dislodging.
    #[test]
    fn walker_rechecks_for_new_walls_during_a_run() {
        let (cols, rows) = (24usize, 18usize);
        let floor: Vec<(usize, usize)> = (4..=10).map(|col| (col, 10)).collect();
        let clear = planted(cols, rows, &floor);
        let mut with_wall_cells = floor.clone();
        with_wall_cells.push((7, 9));
        let with_wall = planted(cols, rows, &with_wall_cells);
        let view = clear.view();

        let mut w = drop_walker(&clear, 8, 4.0);
        w.col = 8;
        w.x = Walker::cell_x(&view, 8);
        w.pose = Pose::standing();
        w.support = Some(10);
        w.hip_y = w.surface_of(&view, 10) + w.leg_reach();
        w.act = Act::Run {
            dir: -1.0,
            t: 0.4,
            take_off: Walker::cell_x(&view, 4),
        };

        let wall_face = Walker::cell_x(&with_wall.view(), 7)
            + CELL_PX * TILE_FILL * 0.5
            + WALKER_GRAB_BODY_CLEARANCE;
        for frame in 0..60 {
            w.update(&walker_ctx(&with_wall, frame as f32 / 60.0));
            assert!(
                w.x >= wall_face - 0.01,
                "entered the new wall by {:.1}px",
                wall_face - w.x
            );
            if !matches!(w.act, Act::Run { .. }) {
                break;
            }
        }
        assert!(
            matches!(w.act, Act::Deciding(_)),
            "did not stop to re-plan at the new wall: {:?}",
            w.act
        );
        assert_eq!(w.col, 8, "crossed into the wall's grid cell");
    }

    /// While clearing a tile born in his body space, the chosen escape must remain fixed
    /// until the step finishes. Re-running the random choice every frame caused the
    /// visible left-right jitter reported in the field.
    #[test]
    fn walker_commits_to_one_escape_step_without_oscillating() {
        let (cols, rows) = (24usize, 18usize);
        let life = planted(cols, rows, &[(4, 10), (5, 10), (6, 10), (5, 9)]);
        let view = life.view();
        let mut w = drop_walker(&life, 5, 4.0);
        w.col = 5;
        w.x = Walker::cell_x(&view, 5);
        w.pose = Pose::standing();
        w.support = Some(10);
        w.hip_y = w.surface_of(&view, 10) + w.leg_reach();
        w.act = Act::Deciding(0.0);

        w.update(&walker_ctx(&life, 0.0));
        let (dir, target) = match w.act {
            Act::Sidestep { dir, to_x } => (dir, to_x),
            other => panic!("did not begin an escape step: {other:?}"),
        };
        let mut last_x = w.x;
        for frame in 1..30 {
            w.update(&walker_ctx(&life, frame as f32 / 60.0));
            match w.act {
                Act::Sidestep { dir: still, to_x } => {
                    assert_eq!(still, dir, "reversed the escape direction");
                    assert_eq!(to_x, target, "picked a different escape cell");
                    assert!(
                        (w.x - last_x) * dir >= -0.001,
                        "moved backward during the escape"
                    );
                }
                Act::Deciding(_) => break,
                other => panic!("escape changed into {other:?}"),
            }
            last_x = w.x;
        }
    }

    /// The gesture on the way down has to open up with the drop, so a long fall reads
    /// differently from a short one.
    #[test]
    fn a_longer_fall_flings_him_wider() {
        let (cols, rows) = (24usize, 18usize);
        let life = planted(cols, rows, &[]);

        let spread_after = |cells: f32| {
            let mut w = drop_walker(&life, 5, 1.0);
            let mut t = 0.0f32;
            let want = w.hip_y - cells * CELL_PX;
            while w.hip_y > want {
                assert!(w.update(&walker_ctx(&life, t)), "left the world too soon");
                t += 1.0 / 60.0;
            }
            // Arm spread, as the angle between the two upper arms.
            w.pose.shoulder[0] - w.pose.shoulder[1]
        };

        let short = spread_after(0.6);
        let long = spread_after(4.0);
        assert!(
            long > short * 1.15,
            "arms spread {short:.2} after a short drop and {long:.2} after a long one"
        );
    }

    /// The rocket must not be seen crossing the tile plane through a tile.
    ///
    /// Changing sides means passing through the depth the tiles occupy, so it has to do
    /// that where there is a gap. Two things let it cheat: the check for climbing back out
    /// only looked along its own row, though its fins reach most of a cell either side; and
    /// when it had waited too long it came up through whatever was there, no check at all.
    /// Both showed as the rocket ploughing straight through a block.
    #[test]
    fn the_rocket_does_not_cross_through_tiles() {
        let (cols, _) = grid_dims(1600.0, 1000.0);
        let tile_top = THICK + RISE;
        let vis_edge = cols as f32 * CELL_PX * 0.5 - MARGIN as f32 * CELL_PX;

        let mut inspected = 0usize;
        let mut through = Vec::new();
        for seed in [7u64, 991, 40404, 2718, 161803] {
            fly_rocket(seed, |r, view| {
                let (pos, _) = r.pose();
                // Only the moments it is actually inside the slab the tiles occupy.
                if pos[2] < -RISE || pos[2] > tile_top {
                    return;
                }
                // Off the side of the frame it may cheat, since nobody sees it.
                if pos[0].abs() > vis_edge {
                    return;
                }
                inspected += 1;
                // Its own cell and the rows either side, the span the fins cover.
                let (cx, cy) = r.cell_ahead(view, 0);
                if (-1isize..=1).any(|dy| view.alive(cx, cy + dy, 0)) {
                    through.push((seed, pos[0], pos[1]));
                }
            });
        }

        assert!(
            inspected > 40,
            "hardly ever caught it inside the tile plane ({inspected} frames)"
        );
        assert!(
            through.is_empty(),
            "crossed through a tile {} times, e.g. {:?}",
            through.len(),
            through.first().unwrap()
        );
    }

    /// He is only ever dropped somewhere the whole performance is on screen.
    ///
    /// Each clause here was a way he looked wrong: falling clean through the field because
    /// nothing was under him; stopping half out of frame at the top because the first thing
    /// to catch him was too high; and coming to rest with his legs below the bottom edge.
    #[test]
    fn walker_only_drops_where_he_can_be_seen() {
        let (cols, rows) = grid_dims(1600.0, 1000.0);
        let half_h = rows as f32 * CELL_PX * 0.5;
        let half_w = cols as f32 * CELL_PX * 0.5;
        let margin = MARGIN as f32 * CELL_PX;
        let (vis_top, vis_right) = (half_h - margin, half_w - margin);
        let vis_bottom = -vis_top;
        let half_tile = CELL_PX * TILE_FILL * 0.5;

        let mut placed = 0;
        for seed in 0..60u64 {
            let life = Life::new(cols, rows, seed * 977 + 5);
            let view = life.view();
            let mut rng = Rng::new(seed * 31 + 3);
            let Some(w) = Walker::new(&view, &mut rng) else {
                continue;
            };
            placed += 1;

            // Fully inside the frame, arms and all.
            assert!(
                w.x - Walker::reach() > -vis_right && w.x + Walker::reach() < vis_right,
                "seed {seed}: dropped at x={:.0}, part of him off the side",
                w.x
            );

            // Something will catch him, and in a place where all of him is visible.
            let row = (0..rows as isize)
                .find(|&r| view.alive(w.col, r, 0))
                .unwrap_or_else(|| panic!("seed {seed}: nothing under him at all"));
            let surface = view.cell_center(w.col as f32, row as f32)[1] + half_tile;
            assert!(
                surface <= vis_top - WALKER_H,
                "seed {seed}: would be stopped at {surface:.0}, still partly above the top"
            );
            assert!(
                surface > vis_bottom,
                "seed {seed}: would land at {surface:.0}, below the bottom edge"
            );
            assert!(
                (0..=WALKER_PLAN_GENS).all(|g| view.alive(w.col, row, g)),
                "seed {seed}: the thing meant to catch him is about to vanish"
            );
        }
        assert!(placed > 20, "only found {placed} usable spots in 60 boards");
    }

    /// Anything that gets below the bottom edge keeps going. He must never settle with his
    /// feet off the screen, even though there are live tiles down there to land on.
    #[test]
    fn walker_below_the_screen_never_catches() {
        let (cols, rows) = (24usize, 20usize);
        let half_h = rows as f32 * CELL_PX * 0.5;
        let vis_bottom = -(half_h - MARGIN as f32 * CELL_PX);

        // A block in the bottom margin, well below the visible area.
        let floor_row = rows - 2;
        let life = planted(
            cols,
            rows,
            &[
                (5, floor_row),
                (6, floor_row),
                (5, floor_row - 1),
                (6, floor_row - 1),
            ],
        );
        let mut w = drop_walker(&life, 5, 3.0);

        let mut t = 0.0f32;
        let mut frames = 0;
        while w.update(&walker_ctx(&life, t)) {
            t += 1.0 / 60.0;
            frames += 1;
            assert!(frames < 2000, "never left");
            assert!(
                w.support.is_none() || w.foot_y() >= vis_bottom,
                "settled with his feet at {:.0}, below the screen edge at {vis_bottom:.0}",
                w.foot_y()
            );
        }
    }

    /// A tile growing into his standing space should produce a visible corrective step
    /// when there is supported room beside him, never the old upward hop.
    #[test]
    fn walker_steps_clear_when_a_tile_grows_behind_him() {
        let (cols, rows) = (24usize, 18usize);
        // A block to stand on, plus a blinker beside it whose middle cell will light up
        // exactly where he is standing.
        let life_cells = [(5, 10), (6, 10), (5, 11), (6, 11)];
        let life = planted(cols, rows, &life_cells);
        let mut w = drop_walker(&life, 5, 4.0);

        let mut t = 0.0f32;
        for _ in 0..600 {
            w.update(&walker_ctx(&life, t));
            t += 1.0 / 60.0;
            if matches!(w.act, Act::Deciding(_)) {
                break;
            }
        }
        let row = w.support.expect("never landed");

        // Fill the cell his body occupies, which is the one above his footing.
        let mut filled = planted(cols, rows, &[]);
        for (c, r) in life_cells {
            filled.boards[filled.head][r * cols + c] = true;
        }
        let head = filled.head;
        filled.boards[head][(row - 1) as usize * cols + 5] = true;

        let before_x = w.x;
        w.update(&walker_ctx(&filled, t));
        assert!(
            matches!(w.act, Act::Sidestep { .. }),
            "did not choose the clear supported step: {:?}",
            w.act
        );
        assert_eq!(w.support, Some(row), "a safe step should keep its footing");
        assert!(w.vy <= 0.0, "the collision reintroduced an upward hop");
        assert_eq!(w.x, before_x, "the first response snapped sideways");

        for _ in 0..50 {
            t += 1.0 / 60.0;
            w.update(&walker_ctx(&filled, t));
        }
        let target = Walker::cell_x(&filled.view(), 6);
        assert!(
            (w.x - target).abs() < 1.0,
            "finished {:.1}px away from the safe cell centre",
            w.x - target
        );
        assert_eq!(w.col, 6, "did not move into the clear column");
    }

    /// When every neighbouring body cell is sealed, the escape is a downward fall behind
    /// the floor. In particular it must not instantly re-land and launch another hop.
    #[test]
    fn sealed_in_walker_falls_instead_of_hop_looping() {
        let (cols, rows) = (24usize, 18usize);
        let base = planted(cols, rows, &[(5, 10)]);
        let mut w = drop_walker(&base, 5, 4.0);

        let mut t = 0.0f32;
        for _ in 0..600 {
            w.update(&walker_ctx(&base, t));
            t += 1.0 / 60.0;
            if w.support.is_some() {
                break;
            }
        }
        let stood_at = w.hip_y;

        let mut sealed = planted(cols, rows, &[]);
        let head = sealed.head;
        for (col, row) in [(5usize, 10usize), (4, 9), (5, 9), (6, 9)] {
            sealed.boards[head][row * cols + col] = true;
        }

        w.update(&walker_ctx(&sealed, t));
        assert_eq!(w.act, Act::Airborne, "the squeeze became another idle");
        assert!(w.support.is_none(), "the filled space still supports him");
        assert!(w.vy <= 0.0, "the squeeze launched an upward hop");

        for _ in 0..24 {
            t += 1.0 / 60.0;
            w.update(&walker_ctx(&sealed, t));
            assert!(
                !matches!(w.act, Act::Hop(_) | Act::Landing(_)),
                "re-caught the floor during the escape"
            );
        }
        assert!(
            w.hip_y < stood_at - 20.0,
            "only fell {:.1}px while sealed",
            stood_at - w.hip_y
        );
    }

    /// A descending body near a block corner keeps falling. A centred contact lands and
    /// eases to the exact centre, ruling out persistent half-over-block poses.
    #[test]
    fn walker_rejects_corner_perches_and_centres_real_landings() {
        let (cols, rows) = (24usize, 18usize);
        let life = planted(cols, rows, &[(5, 10), (6, 10), (5, 11), (6, 11)]);
        let view = life.view();
        let top = view.cell_center(5.0, 10.0)[1] + CELL_PX * TILE_FILL * 0.5;

        let falling_at = |x: f32| {
            let mut w = drop_walker(&life, 5, 4.0);
            w.pose = Pose::standing();
            w.x = x;
            w.col = Walker::col_of(&view, x);
            w.hip_y = top + w.leg_reach() + 1.0;
            w.vy = -100.0;
            w
        };

        let centre = Walker::cell_x(&view, 5);
        let mut corner = falling_at(centre + CELL_PX * 0.45);
        corner.update(&walker_ctx(&life, 0.0));
        assert!(
            corner.support.is_none(),
            "accepted a landing {:.1}px off centre",
            corner.x - centre
        );

        let mut good = falling_at(centre + WALKER_LAND_HALF_WIDTH * 0.7);
        good.update(&walker_ctx(&life, 0.0));
        assert_eq!(good.support, Some(10), "rejected a sound landing");
        for frame in 1..40 {
            good.update(&walker_ctx(&life, frame as f32 / 60.0));
        }
        assert!(
            (good.x - centre).abs() < 0.1,
            "landing settled {:.1}px away from centre",
            good.x - centre
        );
    }

    /// A plain vertical drop has no catch plan, so a neighbouring block can never pull
    /// him sideways onto its grid face.
    #[test]
    fn walker_falling_straight_does_not_grab_a_side_ledge() {
        let (cols, rows) = (24usize, 22usize);
        let life = planted(cols, rows, &[(6, 9), (7, 9), (6, 10), (7, 10)]);

        let mut w = drop_walker(&life, 5, 1.0);
        let start_x = w.x;
        let mut t = 0.0f32;
        while w.update(&walker_ctx(&life, t)) && t < 6.0 {
            t += 1.0 / 60.0;
            assert!(
                !matches!(w.act, Act::Hang { .. }),
                "an unplanned fall caught a side ledge"
            );
            assert!(w.grab_plan.is_none(), "a plain fall invented a catch plan");
            assert_eq!(w.x, start_x, "a straight fall was pulled sideways");
        }
    }

    /// A planned run-and-jump does catch when its ballistic hand crossing meets the face.
    /// The transition may resolve within the current frame, but may never move further
    /// than that frame's ordinary horizontal travel.
    #[test]
    fn planned_jump_catches_without_a_grid_snap() {
        let (cols, rows) = (24usize, 22usize);
        let life = planted(
            cols,
            rows,
            &[
                (4, 12),
                (5, 12),
                (4, 13),
                (5, 13),
                (7, 12),
                (8, 12),
                (7, 13),
                (8, 13),
            ],
        );
        let (mut w, plan) = planned_ledge_jump(&life);
        let mut t = 0.0f32;
        let mut caught = false;
        for _ in 0..180 {
            let old_x = w.x;
            let old_vx = w.vx;
            assert!(w.update(&walker_ctx(&life, t)), "left before the catch");
            t += 1.0 / 60.0;
            assert!(
                (w.x - old_x).abs() <= old_vx.abs() / 60.0 + 0.05,
                "catch moved {:.2}px in a {:.2}px frame",
                (w.x - old_x).abs(),
                old_vx.abs() / 60.0
            );
            if matches!(w.act, Act::Hang { .. }) {
                caught = true;
                break;
            }
        }
        assert!(caught, "the planned ballistic path missed its ledge");
        assert!(
            (w.x - plan.hip_x).abs() <= WALKER_GRAB_X_TOLERANCE,
            "hung {:.1}px away from the planned natural crossing",
            w.x - plan.hip_x
        );
        assert!(
            (w.hip_y + w.hand_rise() - plan.ledge_y).abs() < 0.1,
            "hands did not stop on the ledge"
        );
    }

    /// Once safely caught, both outcomes remain available. A voluntary release has a
    /// visible wind-up and carries a continuous ballistic path away from the held face;
    /// on this board that natural path should bring him back to the block he launched
    /// from, without a position correction on release.
    #[test]
    fn hanging_can_wait_or_throw_away_onto_another_block() {
        let (cols, rows) = (24usize, 22usize);
        let life = planted(
            cols,
            rows,
            &[
                (4, 12),
                (5, 12),
                (4, 13),
                (5, 13),
                (7, 12),
                (8, 12),
                (7, 13),
                (8, 13),
            ],
        );

        // The post-catch coin must genuinely contain both performances.
        let mut outcomes = [0usize; 2];
        let mut sample = drop_walker(&life, 6, 4.0);
        for seed in 0..100u64 {
            sample.rng = Rng::new(seed * 131 + 17);
            sample.t = 1.0;
            sample.begin_hang(7, 12);
            outcomes[usize::from(sample.hang_release_at.is_finite())] += 1;
        }
        assert!(
            outcomes[0] > 25 && outcomes[1] > 25,
            "hang outcomes split {outcomes:?}"
        );

        let (mut w, _) = planned_ledge_jump(&life);
        let mut t = 0.0f32;
        for _ in 0..180 {
            assert!(w.update(&walker_ctx(&life, t)), "left before catching");
            t += 1.0 / 60.0;
            if matches!(w.act, Act::Hang { .. }) {
                break;
            }
        }
        assert!(matches!(w.act, Act::Hang { .. }), "never caught the ledge");
        assert!(
            w.plan_hang_throw(&life.view(), -w.facing).is_some(),
            "the launch planner overlooked the block behind him"
        );

        w.hang_release_at = w.t + 0.52;
        let held_x = w.x;
        let mut saw_windup = false;
        let mut released = false;
        for _ in 0..90 {
            let old_x = w.x;
            w.update(&walker_ctx(&life, t));
            t += 1.0 / 60.0;
            if matches!(w.act, Act::Hang { .. }) {
                saw_windup |= w.pose.lean.abs() > 0.035;
                assert_eq!(w.x, held_x, "wind-up pulled the hands off the ledge");
            } else if w.vy > 0.0 {
                released = true;
                assert!(
                    (w.x - old_x).abs() <= w.vx.abs() / 60.0 + 0.05,
                    "release snapped horizontally"
                );
                assert!(w.vx < 0.0, "did not throw away from the right-hand ledge");
                break;
            }
        }
        assert!(saw_windup, "release had no visible backward swing");
        assert!(released, "never threw off the ledge");

        let mut landed = false;
        for _ in 0..180 {
            if !w.update(&walker_ctx(&life, t)) {
                break;
            }
            t += 1.0 / 60.0;
            if w.support.is_some() {
                landed = true;
                break;
            }
        }
        assert!(landed, "the planned throw missed every landing");
        assert!(
            matches!(w.col, 4 | 5),
            "landed in column {}, not on the block behind him",
            w.col
        );
    }

    /// And he lets go the moment the ledge stops being one.
    #[test]
    fn walker_drops_when_his_ledge_vanishes() {
        let (cols, rows) = (24usize, 22usize);
        let life = planted(
            cols,
            rows,
            &[
                (4, 12),
                (5, 12),
                (4, 13),
                (5, 13),
                (7, 12),
                (8, 12),
                (7, 13),
                (8, 13),
            ],
        );
        let (mut w, _) = planned_ledge_jump(&life);
        let mut t = 0.0f32;
        for _ in 0..180 {
            w.update(&walker_ctx(&life, t));
            t += 1.0 / 60.0;
            if matches!(w.act, Act::Hang { .. }) {
                break;
            }
        }
        assert!(matches!(w.act, Act::Hang { .. }), "never reached the ledge");
        let hung_at = w.hip_y;

        // Take the block away.
        let empty = planted(cols, rows, &[]);
        w.update(&walker_ctx(&empty, t));
        assert_eq!(w.act, Act::Airborne, "still hanging from nothing");

        for _ in 0..40 {
            t += 1.0 / 60.0;
            w.update(&walker_ctx(&empty, t));
        }
        assert!(w.hip_y < hung_at - 5.0, "did not drop after letting go");
    }

    /// Boxed in, the shove has to pass through the big straining pose — and be over before
    /// the tile could vanish, so the joke is that he gives up and *then* it goes.
    #[test]
    fn walker_shoving_strains_and_finishes_first() {
        let (cols, rows) = (24usize, 18usize);
        let mut cells = vec![(4, 10), (5, 10), (6, 10)];
        cells.extend([(4, 9), (6, 9)]);
        let life = planted(cols, rows, &cells);

        let mut saw_strain = false;
        for seed in 0..40u64 {
            let mut w = drop_walker(&life, 5, 4.0);
            w.rng = Rng::new(seed * 31 + 7);
            // Force the shove rather than the hop, so this does not depend on a coin.
            let mut t = 0.0f32;
            while w.update(&walker_ctx(&life, t)) && t < 8.0 {
                t += 1.0 / 60.0;
                if matches!(w.act, Act::Deciding(d) if d > WALKER_DECIDE * 0.5) {
                    w.act = Act::Shove { dir: 1.0, t: 0.0 };
                }
                if let Act::Shove { t: st, .. } = w.act {
                    // Midway through, he should be right over: torso well past halfway to
                    // horizontal, and both arms out into the tile.
                    if (0.5..0.9).contains(&st) {
                        let lean = w.pose.lean.abs();
                        if lean > 0.7 {
                            saw_strain = true;
                        }
                    }
                }
                if saw_strain {
                    break;
                }
            }
            if saw_strain {
                break;
            }
        }
        assert!(saw_strain, "never bent over into the push");
    }

    /// The public cadence stays literal. The mimic leads once for discoverability,
    /// then occupies two of six slots without changing the relative odds of any
    /// established critter.
    #[test]
    fn critters_arrive_every_sixteen_seconds_with_a_mimic_forward_rotation() {
        assert_eq!(FIRST_CRITTER, 16.0);
        assert_eq!(CRITTER_EVERY, 16.0);

        let mut rng = Rng::new(0x5eed_cafe);
        assert_eq!(scheduled_critter_kind(&mut rng, 0), CritterKind::Mimic);

        let mut counts = [0usize; 5];
        let draws = 60_000usize;
        for _ in 0..draws {
            let slot = match random_critter_kind(&mut rng) {
                CritterKind::Rocket => 0,
                CritterKind::Walker => 1,
                CritterKind::Bee => 2,
                CritterKind::Ufo => 3,
                CritterKind::Mimic => 4,
            };
            counts[slot] += 1;
        }
        for (slot, (kind, count)) in ["rocket", "walker", "bee", "UFO", "tile mimic"]
            .into_iter()
            .zip(counts)
            .enumerate()
        {
            let share = count as f32 / draws as f32;
            let expected = if slot == 4 { 2.0 / 6.0 } else { 1.0 / 6.0 };
            assert!(
                (share - expected).abs() < 0.012,
                "{kind} selector produced {:.1}% rather than {:.1}%",
                share * 100.0,
                expected * 100.0
            );
        }

        // Exercise the live clock and its evolved board, not only the selector. This
        // seed has no connected four-transition-safe flank route at sixteen seconds,
        // so the cautious stationary performance must keep the promised first mimic
        // from silently turning into the rocket fallback.
        let mut driver = Driver::new(28, 20, 0xC0FFEE_1234_5678, 0.0);
        for frame in 1..=(FIRST_CRITTER as usize * 60 + 2) {
            driver.advance(frame as f64 / 60.0, 1.0);
        }
        assert!(
            driver
                .viz
                .walker_probe()
                .is_some_and(|state| state.starts_with("Mimic ")),
            "the first live scheduled visitor was not the tile mimic"
        );
    }

    /// The clock must be monotonic and deliver the rate the constants declare,
    /// whether or not the second gear is switched on.
    #[test]
    fn generation_clock_delivers_declared_rate() {
        assert_eq!(generation_at(0.0), 0);

        // Measured over many cycles: a single cycle holds only a handful of
        // generations, so what has to hold is the rate delivered on average.
        let cycles = 400;
        let span = cycles as f64 * CYCLE;
        let rate = generation_at(span) as f64 / span;
        let want = (SLOW_SECS * SLOW_HZ + FAST_SECS * FAST_HZ) / CYCLE;
        assert!(
            (rate - want).abs() < 0.01,
            "clock ran at {rate:.4} generations/sec, declared {want:.4}"
        );

        if FAST_SECS > 0.0 {
            let (mut slow, mut fast) = (0i64, 0i64);
            for k in 0..cycles {
                let base = k as f64 * CYCLE;
                slow += generation_at(base + SLOW_SECS) - generation_at(base);
                fast += generation_at(base + CYCLE) - generation_at(base + SLOW_SECS);
            }
            let slow_rate = slow as f64 / (cycles as f64 * SLOW_SECS);
            let fast_rate = fast as f64 / (cycles as f64 * FAST_SECS);
            assert!(
                (slow_rate - SLOW_HZ).abs() < 0.02,
                "slow phase ran at {slow_rate:.3} Hz, declared {SLOW_HZ}"
            );
            assert!(
                (fast_rate - FAST_HZ).abs() < 0.03,
                "fast phase ran at {fast_rate:.3} Hz, declared {FAST_HZ}"
            );
            // A whole number of generations per cycle would lock every cycle to the
            // same coarse split instead of letting it average out.
            assert!(
                (CYCLE_GENS - CYCLE_GENS.round()).abs() > 0.05,
                "CYCLE_GENS is {CYCLE_GENS}, too close to a whole number"
            );
        }

        let mut prev = i64::MIN;
        let mut t = 0.0;
        while t < 4.0 * CYCLE {
            let g = generation_at(t);
            assert!(g >= prev, "generation went backwards at t={t}");
            prev = g;
            t += 0.001;
        }
    }
}

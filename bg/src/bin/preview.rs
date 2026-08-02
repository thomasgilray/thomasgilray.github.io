//! Headless preview: renders the background to PNG frames so the look can be
//! checked without a browser.
//!
//!   cargo run --release --bin preview -- <out-dir> [width] [height] [t0,t1,t2,...]
//!
//! Times are seconds into the simulation. With none given it samples a spread
//! across one 8s slow/fast cycle.

use conwaybg::*;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let out = args.first().cloned().unwrap_or_else(|| ".".into());
    let width: u32 = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(1600);
    let height: u32 = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(1000);
    let times: Vec<f64> = match args.get(3) {
        Some(s) => s.split(',').filter_map(|p| p.trim().parse().ok()).collect(),
        None => vec![2.0, 6.6, 6.75, 6.9, 7.1, 12.0],
    };

    pollster::block_on(run(&out, width, height, &times));
}

async fn run(out: &str, width: u32, height: u32, times: &[f64]) {
    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        // WGPU_BACKEND=gl exercises the same naga WGSL->GLSL path the browser's
        // WebGL2 fallback uses, which is where shader translation tends to break.
        backends: match std::env::var("CONWAYBG_BACKEND").as_deref() {
            Ok("gl") => wgpu::Backends::GL,
            _ => wgpu::Backends::PRIMARY,
        },
        ..wgpu::InstanceDescriptor::new_without_display_handle()
    });
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            force_fallback_adapter: false,
            compatible_surface: None,
            apply_limit_buckets: false,
        })
        .await
        .expect("no adapter");
    eprintln!("adapter: {:?}", adapter.get_info());

    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor {
            label: Some("preview"),
            ..Default::default()
        })
        .await
        .expect("no device");

    // Match the browser path: a non-sRGB target that the shader encodes into.
    let format = wgpu::TextureFormat::Rgba8Unorm;
    let samples = Scene::preferred_samples(&adapter, format);
    eprintln!("format {format:?}, {samples}x MSAA, {width}x{height}");

    let mut scene = Scene::new(&device, &queue, format, samples, width, height);
    let (cols, rows) = grid_dims(width as f64, height as f64);
    let mut driver = Driver::new(cols, rows, 0xC0FFEE_1234_5678, 0.0);
    if std::env::var("CONWAYBG_FOCUS").as_deref() == Ok("mimic") {
        assert!(
            driver.preview_mimic(0x71_1e_c4_ab),
            "the preview board had no safe tile-mimic route"
        );
        eprintln!("focused tile-mimic performance enabled");
    }
    eprintln!("grid {cols}x{rows} = {} tiles", cols * rows);
    if std::env::var("CONWAYBG_CHECK").is_ok() {
        check_rules(&mut driver, cols, rows);
    }

    let target = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("target"),
        size: wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
        view_formats: &[],
    });
    let view = target.create_view(&wgpu::TextureViewDescriptor::default());

    let row_bytes = (width * 4).next_multiple_of(256);
    let readback = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("readback"),
        size: (row_bytes * height) as u64,
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    // Walk the clock forward in small steps so the simulation sees a realistic
    // frame cadence, capturing whenever we pass one of the requested times.
    let mut t = 0.0f64;
    let dt = 1.0 / 60.0;
    let mut pending: Vec<f64> = times.to_vec();
    pending.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let end = pending.last().copied().unwrap_or(0.0);
    let mut next = 0;

    while t <= end + dt {
        driver.advance(t, 1.0);
        scene.upload_instances(&device, &queue, driver.viz.draw_list().0);
        scene.upload_props(&device, &queue, driver.viz.props());
        if next < pending.len() && t >= pending[next] {
            let g = globals_for(
                driver.life.cols,
                driver.life.rows,
                width as f32 / height as f32,
                height as f32,
                driver.shader_clock(t),
                t as f32,
                scene.encode_srgb,
            );
            scene.set_globals(&queue, &g);

            let mut enc =
                device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
            let (list, solid) = driver.viz.draw_list();
            scene.draw(&mut enc, &view, solid, list.len() as u32);
            enc.copy_texture_to_buffer(
                wgpu::TexelCopyTextureInfo {
                    texture: &target,
                    mip_level: 0,
                    origin: wgpu::Origin3d::ZERO,
                    aspect: wgpu::TextureAspect::All,
                },
                wgpu::TexelCopyBufferInfo {
                    buffer: &readback,
                    layout: wgpu::TexelCopyBufferLayout {
                        offset: 0,
                        bytes_per_row: Some(row_bytes),
                        rows_per_image: Some(height),
                    },
                },
                wgpu::Extent3d {
                    width,
                    height,
                    depth_or_array_layers: 1,
                },
            );
            queue.submit(Some(enc.finish()));

            let path = format!("{out}/frame-{:0>6.3}.png", pending[next]);
            save(&device, &readback, row_bytes, width, height, &path);
            eprintln!("wrote {path}");
            next += 1;
        }
        t += dt;
    }
}

/// Reports field statistics after a long run, and confirms the world has hard edges.
fn check_rules(driver: &mut Driver, cols: usize, rows: usize) {
    let mut t = 0.0;
    for _ in 0..1200 {
        driver.advance(t, 1.0);
        t += 1.0 / 60.0;
    }
    let live = (0..rows)
        .flat_map(|y| (0..cols).map(move |x| (x, y)))
        .filter(|(x, y)| driver.viz.drawn(*x, *y))
        .count();
    let (list, solid) = driver.viz.draw_list();
    eprintln!(
        "after 20s: {}/{} live ({:.1}%), {solid} solid / {} ghost instances",
        live,
        cols * rows,
        100.0 * live as f64 / (cols * rows) as f64,
        list.len() as u32 - solid
    );
}

fn save(
    device: &wgpu::Device,
    buf: &wgpu::Buffer,
    row_bytes: u32,
    width: u32,
    height: u32,
    path: &str,
) {
    let slice = buf.slice(..);
    let (tx, rx) = std::sync::mpsc::channel();
    slice.map_async(wgpu::MapMode::Read, move |r| {
        let _ = tx.send(r);
    });
    let _ = device.poll(wgpu::PollType::wait_indefinitely());
    rx.recv().unwrap().unwrap();

    let data = slice.get_mapped_range().unwrap();
    let mut rgba = Vec::with_capacity((width * height * 4) as usize);
    for y in 0..height {
        let s = (y * row_bytes) as usize;
        rgba.extend_from_slice(&data[s..s + (width * 4) as usize]);
    }
    drop(data);
    buf.unmap();

    let file = std::fs::File::create(path).unwrap();
    let mut enc = png::Encoder::new(std::io::BufWriter::new(file), width, height);
    enc.set_color(png::ColorType::Rgba);
    enc.set_depth(png::BitDepth::Eight);
    enc.write_header().unwrap().write_image_data(&rgba).unwrap();
}

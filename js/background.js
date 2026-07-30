// Loads the wasm background renderer (source in /bg, built by /bg/build.sh).
//
// Browsers with WebGPU get a small module; everything else falls back to the build
// that also carries WebGL2, which is roughly twelve times larger. If neither can
// start, the page simply keeps its flat #e8e8e8 background — the canvas fades itself in
// from the Rust side only once a device is up and the first frame is drawn.
//
// The cache-busting version is taken from this module's own URL rather than written
// here, because this file is the one thing that cannot bust itself: a browser holding
// a stale copy of it would go on requesting whatever version that copy names. The
// page stamps it onto the script tag, and build.sh stamps the page.
const VERSION = new URL(import.meta.url).searchParams.get('v') ?? 'dev';

const name = 'gpu' in navigator ? 'conwaybg-webgpu' : 'conwaybg';

import(`./${name}.js?v=${VERSION}`)
  .then((wasm) =>
    wasm.default({
      module_or_path: new URL(`./${name}_bg.wasm?v=${VERSION}`, import.meta.url),
    }).then(() => {
      // Keeps the old "(what?)" link working.
      window.crazyConway = wasm.boost;
    }),
  )
  .catch((err) => console.warn('background renderer unavailable:', err));

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

// Resolve the page's critter-density mode before the module is instantiated: the
// renderer reads window.conwayBgMode once at startup. A ?bgmode= query on the page
// URL wins (for experimentation); otherwise an inline script's choice is respected.
const bgmode = new URLSearchParams(window.location.search).get('bgmode');
if (['default', 'fewcritters', 'nocritters'].includes(bgmode)) {
  window.conwayBgMode = bgmode;
} else if (!window.conwayBgMode) {
  window.conwayBgMode = 'default';
}

import(`./${name}.js?v=${VERSION}`)
  .then((wasm) =>
    wasm.default({
      module_or_path: new URL(`./${name}_bg.wasm?v=${VERSION}`, import.meta.url),
    }).then(() => {
      // Keeps the old "(what?)" link working.
      window.crazyConway = wasm.boost;
      // The pause button's shared contract: flip the renderer, then restyle the
      // #pausebg button (if the page has one) to show the other icon. The choice
      // lives in a cookie so it follows the reader across pages and reloads.
      const applyPause = (paused) => {
        wasm.set_paused(paused);
        document.cookie = `conwaypaused=${paused ? 1 : 0}; path=/; max-age=31536000; samesite=lax`;
        const btn = document.getElementById('pausebg');
        if (btn) {
          btn.classList.toggle('paused', paused);
          btn.setAttribute(
            'aria-label',
            paused ? 'Resume background animation' : 'Pause background animation',
          );
        }
      };
      let paused = /(?:^|;\s*)conwaypaused=1(?:;|\s*$)/.test(document.cookie);
      window.toggleConwayPause = () => {
        paused = !paused;
        applyPause(paused);
      };
      if (paused) applyPause(true);
    }),
  )
  .catch((err) => console.warn('background renderer unavailable:', err));

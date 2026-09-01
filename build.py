#!/usr/bin/env python3
r"""Build standalone markdown pages into site-styled HTML.

    python3 build.py                    # every page.md in the repo (outside notes/)
    python3 build.py 355/index.md       # just these

This is the general single-page builder: one .md in, one .html out beside it,
wrapped in the same rounded white boxes, Palatino type, and Conway background
as the rest of the site. (The /notes tree has its own multi-page builder with
live Racket embeds and LaTeX; see notes/build.py.)

Each page opens with a frontmatter block:

    ---
    title: CptS 355 — Programming Language Design
    subtitle: Washington State University · Fall 2026
    description: One sentence for search engines and link previews.
    keywords: comma, separated, optional
    crumbs: teaching | CptS 355        (optional; "Thomas Gilray" is prepended)
    bg: fewcritters                    (optional: default | fewcritters | nocritters)
    robots: noindex, nofollow          (optional; defaults to "index, follow")
    emails: stu_emails                 (optional; email PNG folder under /img/)
    ---

The body is standard markdown (python-markdown: fenced_code, tables, attr_list,
md_in_html, sane_lists, smarty). Every level-2 heading starts a new rounded
content box; anything before the first level-2 heading joins the title box.

Inline shortcode:

    {{email:someone@wsu.edu}}

renders the address as a monospace PNG under /img/email/, or under the folder
a page names in its `emails:` frontmatter key, drawn at
EMAIL_HEIGHT px and displayed at half that, baseline-aligned with the
surrounding prose. The address appears in no text node, no alt attribute, and
no filename, so it does not fall out of a page scrape.

Run from anywhere:  python3 build.py
"""

import hashlib
import html
import math
import re
import sys
from datetime import date
from pathlib import Path

import markdown
from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parent
BACKGROUND_SRC = "/js/background.js?v=667fb18518"
DEFAULT_BG_MODE = "fewcritters"
SKIP_DIRS = {".git", "notes", "vendor", "node_modules", "bg"}

# Email images are drawn at EMAIL_HEIGHT px tall and shown at 50%, so they stay
# sharp on hidpi screens; 38 puts them on the page at 19px, which sits right
# next to the ~19.3px Palatino body text. Bump EMAIL_VERSION to re-render every
# cached PNG.
EMAIL_ROOT = ROOT / "img"
EMAIL_DIR_DEFAULT = "email"
EMAIL_DIR_NAME = re.compile(r"\A[a-z0-9_-]+\Z")
EMAIL_HEIGHT = 38
EMAIL_COLOR = (34, 34, 34, 255)
EMAIL_VERSION = "v1"
MONO_FONTS = [
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
    "/Library/Fonts/Menlo.ttc",
    "/System/Library/Fonts/Menlo.ttc",
]

EMAIL_SHORTCODE = re.compile(r"\{\{email:\s*([^}\s]+)\s*\}\}")


def fail(message):
    raise SystemExit(f"build.py: {message}")


# --------------------------------------------------------------------------
# frontmatter


def parse_frontmatter(text, path):
    match = re.match(r"\A---\n(.*?)\n---\n", text, re.S)
    if not match:
        fail(f"{path}: page must start with a --- frontmatter block")
    meta = {}
    key = None
    for line in match.group(1).splitlines():
        continuation = re.match(r"\s+(\S.*)$", line)
        if continuation and key:
            meta[key] = f"{meta[key]} {continuation.group(1).strip()}".strip()
            continue
        key, sep, value = line.partition(":")
        if not sep:
            fail(f"{path}: bad frontmatter line: {line!r}")
        key = key.strip()
        value = value.strip()
        meta[key] = "" if value in (">", "|") else value
    return meta, text[match.end():]


# --------------------------------------------------------------------------
# email images


def mono_font(size):
    for candidate in MONO_FONTS:
        if Path(candidate).exists():
            return ImageFont.truetype(candidate, size)
    fail("no monospace TTF found; add one to MONO_FONTS")


def fitted_font():
    """The largest monospace size whose ascender-to-descender box still fits in
    EMAIL_HEIGHT px, with its metrics."""
    best = None
    for size in range(8, 80):
        font = mono_font(size)
        ascent, descent = font.getmetrics()
        if ascent + descent > EMAIL_HEIGHT:
            break
        best = (font, ascent, descent)
    if best is None:
        fail(f"no monospace size fits in {EMAIL_HEIGHT}px")
    return best


def email_png(address, folder):
    """Draw the address to a hash-named PNG in /img/<folder>/; return (src, css
    width, css depth below the baseline) for the half-size rendering. Any
    punctuation around it belongs in the markdown as real text, not baked into
    the image."""
    text = address
    font, ascent, _descent = fitted_font()
    digest = hashlib.sha1(
        f"{EMAIL_VERSION}|{EMAIL_HEIGHT}|{EMAIL_COLOR}|{text}".encode()
    ).hexdigest()[:16]
    directory = EMAIL_ROOT / folder
    directory.mkdir(parents=True, exist_ok=True)
    out = directory / f"{digest}.png"

    if out.exists():
        width = Image.open(out).size[0]
    else:
        pad = 1
        width = math.ceil(font.getbbox(text)[2]) + 2 * pad
        image = Image.new("RGBA", (width, EMAIL_HEIGHT), (0, 0, 0, 0))
        ImageDraw.Draw(image).text(
            (pad, 0), text, font=font, fill=EMAIL_COLOR, anchor="la"
        )
        image.save(out, optimize=True)
        print(f"  email {folder}/{out.name}  {width}x{EMAIL_HEIGHT}")

    return f"/img/{folder}/{out.name}", width / 2, (EMAIL_HEIGHT - ascent) / 2


def email_html(address, folder):
    src, width, depth = email_png(address, folder)
    return (
        f'<img class="email-inline" src="{src}" '
        f'style="width:{width:.1f}px;height:{EMAIL_HEIGHT / 2:.0f}px;'
        f'vertical-align:{-depth:.1f}px" '
        f'width="{round(width)}" height="{EMAIL_HEIGHT // 2}" alt="email address">'
    )


STATIC_FENCE = re.compile(r"^```.*?^```[ \t]*$", re.S | re.M)


def email_folder(meta, path):
    """Which folder under /img/ this page's addresses are drawn into."""
    folder = meta.get("emails", EMAIL_DIR_DEFAULT).strip() or EMAIL_DIR_DEFAULT
    if not EMAIL_DIR_NAME.match(folder):
        fail(f"{path}: emails: {folder!r} is not a plain folder name")
    return folder


def substitute_emails(source, tokens, folder):
    """Swap {{email:...}} for opaque tokens (resolved to <img> after markdown
    runs), leaving fenced blocks and `code spans` alone."""

    def handle_plain(segment):
        def replace(match):
            token = f"qemailtok{len(tokens)}q"
            tokens[token] = email_html(match.group(1), folder)
            return token

        return EMAIL_SHORTCODE.sub(replace, segment)

    out, pos = [], 0
    for fence in STATIC_FENCE.finditer(source):
        plain = source[pos:fence.start()]
        pieces = re.split(r"(`+[^`]*`+)", plain)
        out.extend(handle_plain(p) if k % 2 == 0 else p for k, p in enumerate(pieces))
        out.append(fence.group(0))
        pos = fence.end()
    pieces = re.split(r"(`+[^`]*`+)", source[pos:])
    out.extend(handle_plain(p) if k % 2 == 0 else p for k, p in enumerate(pieces))
    return "".join(out)


# --------------------------------------------------------------------------
# page assembly


BLOCK_CLASS = re.compile(r"\n<p>\{((?:\.[A-Za-z0-9_-]+\s*)+)\}</p>")


def apply_block_classes(text):
    """attr_list cannot reach a table, so a paragraph of the form

        {.compact}

    on its own line after any block attaches those classes to that block and
    then disappears."""
    while True:
        marker = BLOCK_CLASS.search(text)
        if not marker:
            return text
        names = " ".join(name.lstrip(".") for name in marker.group(1).split())
        head, tail = text[:marker.start()], text[marker.end():]
        close = re.search(r"</([a-z][a-z0-9]*)>\s*$", head)
        if not close:
            fail(f"class marker {{{marker.group(1)}}} has no block above it")
        tag = close.group(1)
        depth = 0
        for candidate in reversed(list(
                re.finditer(rf"<(/?){tag}(?=[\s>])", head[:close.start()]))):
            if candidate.group(1):
                depth += 1
            elif depth:
                depth -= 1
            else:
                cut = candidate.end()
                head = f'{head[:cut]} class="{names}"{head[cut:]}'
                break
        else:
            fail(f"unbalanced <{tag}> before class marker {{{marker.group(1)}}}")
        text = head + tail


def promote_row_classes(text):
    """attr_list can only reach the cell, so a cell class written as
    {.row-off} is lifted onto its <tr> as class="off"."""

    def rewrite(match):
        row = match.group(0)
        moved = []

        def strip(cell):
            keep = []
            for name in cell.group(1).split():
                (moved if name.startswith("row-") else keep).append(name)
            if not keep:
                return cell.group(0).replace(f' class="{cell.group(1)}"', "")
            return cell.group(0).replace(cell.group(1), " ".join(keep))

        row = re.sub(r'<t[dh] class="([^"]*)"', strip, row)
        if not moved:
            return row
        names = " ".join(name[len("row-"):] for name in moved)
        return row.replace("<tr>", f'<tr class="{names}">', 1)

    return re.sub(r"<tr>.*?</tr>", rewrite, text, flags=re.S)


def render_markdown(body):
    md = markdown.Markdown(
        extensions=[
            "fenced_code", "tables", "attr_list", "md_in_html",
            "sane_lists", "smarty",
        ]
    )
    return promote_row_classes(apply_block_classes(md.convert(body)))


def asset_version(site_path, _cache={}):
    """Short content hash of a site-root-relative asset, for cache-busting."""
    if site_path not in _cache:
        _cache[site_path] = hashlib.sha1(
            (ROOT / site_path.lstrip("/")).read_bytes()
        ).hexdigest()[:10]
    return _cache[site_path]


def box(inner, extra_class=""):
    classes = f"content {extra_class}".strip()
    return (
        '\t\t<div class="rounded">\n'
        f'\t\t\t<div class="{classes}">\n{inner}\n\t\t\t</div>\n'
        '\t\t\t<div class="a"></div>\n'
        '\t\t\t<div class="b"></div>\n'
        '\t\t\t<div class="c"></div>\n'
        '\t\t\t<div class="d"></div>\n'
        "\t\t</div>"
    )


def crumb_html(spec):
    """"teaching | CptS 355 => /355/" -> the crumb strip, always rooted at
    the site index."""
    items = [("Thomas Gilray", "/")]
    for chunk in (part.strip() for part in spec.split("|")):
        if not chunk:
            continue
        label, sep, href = chunk.partition("=>")
        items.append((label.strip(), href.strip() if sep else None))
    parts = []
    for label, href in items:
        label = html.escape(label)
        parts.append(f'<a href="{html.escape(href, quote=True)}">{label}</a>'
                     if href else label)
    return ' <span class="sep">&middot;</span> '.join(parts)


def document(meta, boxes):
    year = date.today().year
    body = "\n\n".join(boxes)
    bg_mode = meta.get("bg", DEFAULT_BG_MODE)
    stylesheets = "\n".join(
        f'\t\t<link rel="stylesheet" href="{sheet}?v={asset_version(sheet)}">'
        for sheet in ["/screen.css", "/page.css"]
    )
    return f"""<!DOCTYPE html>
<html lang="en-US">
\t<head>
\t\t<meta charset="utf-8">
\t\t<meta name="viewport" content="width=device-width, initial-scale=1">
\t\t<meta name="author" content="Thomas Gilray">
\t\t<meta name="copyright" content="(c) Thomas Gilray, 2018-{year}">
\t\t<meta name="robots" content="{html.escape(meta.get("robots", "index, follow"), quote=True)}">
\t\t<meta name="description" content="{html.escape(meta.get("description", ""), quote=True)}">
\t\t<meta name="keywords" content="{html.escape(meta.get("keywords", ""), quote=True)}">

\t\t<title>{html.escape(meta["title"])}</title>

\t\t<link rel="icon" href="/favicon.ico" sizes="any">
{stylesheets}
\t\t<script>window.conwayBgMode = '{bg_mode}';</script>
\t\t<script type="module" src="{BACKGROUND_SRC}"></script>
\t</head>

\t<body class="page">
\t<a href="https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life" target="_blank" rel="noopener" class="conwaylink" onclick="window.crazyConway &amp;&amp; window.crazyConway();">(what?)</a>
\t<button id="pausebg" class="pausebtn" aria-label="Pause background animation" onclick="window.toggleConwayPause &amp;&amp; window.toggleConwayPause();"><svg class="icon-pause" viewBox="0 0 16 16" aria-hidden="true"><rect x="3" y="2.5" width="3.4" height="11" rx="1"/><rect x="9.6" y="2.5" width="3.4" height="11" rx="1"/></svg><svg class="icon-play" viewBox="0 0 16 16" aria-hidden="true"><path d="M4.5 2.8v10.4c0 .8.9 1.3 1.6.9l8.2-5.2c.6-.4.6-1.4 0-1.8L6.1 1.9c-.7-.4-1.6.1-1.6.9z"/></svg></button>
\t<canvas id="bgcanvas" aria-hidden="true"></canvas>

\t<div id="wrapper">
{body}

\t\t<div id="footer">
\t\t\tCopyright (c) Thomas Gilray, {year}, <a target="_blank" rel="noopener" href="https://creativecommons.org/licenses/by-nc/4.0/">Some Rights Reserved</a>.
\t\t</div>
\t</div>
\t</body>
</html>
"""


def build_page(path):
    meta, source = parse_frontmatter(path.read_text(encoding="utf-8"), path)
    if not meta.get("title"):
        fail(f"{path}: frontmatter needs a title")

    tokens = {}
    body_html = render_markdown(
        substitute_emails(source, tokens, email_folder(meta, path))
    )
    for token, embed in tokens.items():
        if token not in body_html:
            fail(f"{path}: email shortcode {token} was mangled by markdown")
        body_html = body_html.replace(token, embed)

    sections = re.split(r"(?=<h2[ >])", body_html)
    top = ""
    if meta.get("crumbs"):
        top += f'<div class="page-crumbs">{crumb_html(meta["crumbs"])}</div>\n'
    top += f'<h1>{html.escape(meta["title"])}</h1>\n'
    if meta.get("subtitle"):
        top += f'<div class="page-subtitle">{meta["subtitle"]}</div>\n'
    top += sections[0].strip()

    boxes = [box(top.rstrip(), "page-top")]
    boxes.extend(box(section.strip()) for section in sections[1:])

    out_path = path.with_suffix(".html")
    out_path.write_text(document(meta, boxes), encoding="utf-8")
    print(f"built {out_path.relative_to(ROOT)}")
    return out_path


def discover():
    pages = []
    for path in sorted(ROOT.rglob("*.md")):
        if any(part in SKIP_DIRS or part.startswith(".") for part in
               path.relative_to(ROOT).parts[:-1]):
            continue
        if path.read_text(encoding="utf-8").startswith("---\n"):
            pages.append(path)
    return pages


def declared_email_folders(pages):
    """Every /img/ folder the site draws addresses into, default included."""
    folders = {EMAIL_DIR_DEFAULT}
    for page in pages:
        meta, _ = parse_frontmatter(page.read_text(encoding="utf-8"), page)
        folders.add(email_folder(meta, page))
    return folders


def prune_emails(pages):
    """Drop cached email PNGs that no page on the site references any more."""
    referenced = set()
    for page in ROOT.rglob("*.html"):
        if ".git" in page.parts:
            continue
        referenced.update(
            re.findall(r'/img/[a-z0-9_-]+/[^"\']+', page.read_text(encoding="utf-8"))
        )
    for folder in sorted(declared_email_folders(pages)):
        directory = EMAIL_ROOT / folder
        if not directory.is_dir():
            continue
        for stale in directory.glob("*.png"):
            if f"/img/{folder}/{stale.name}" not in referenced:
                stale.unlink()
                print(f"  email pruned {folder}/{stale.name}")


def main():
    args = sys.argv[1:]
    pages = [Path(a).resolve() for a in args] if args else discover()
    if not pages:
        fail("no markdown pages with frontmatter found")
    for page in pages:
        build_page(page)
    if not args:
        prune_emails(pages)


if __name__ == "__main__":
    main()

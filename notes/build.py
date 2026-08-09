#!/usr/bin/env python3
r"""Build the /notes tree: notes/<topic>/<page>.md -> notes/<topic>/<page>.html.

Each topic directory holds a topic.json:

    { "title": "The λ-Calculus",
      "keywords": "...",
      "pages": ["intro", "reduction", ...] }

and one markdown file per page slug. Pages open with a small frontmatter block:

    ---
    title: Reducing expressions
    nav: Reducing expressions        (optional, shorter label for the page strip)
    tldr: Two or three sentences shown under the title.
    ---

The markdown body is standard (python-markdown: fenced_code, tables, attr_list,
toc, smarty). Every level-2 heading starts a new rounded content box. The title
box is generated: breadcrumbs, h1, the tldr, and a cdot-separated strip of all
pages in the topic with the current page in bold.

Fenced code blocks whose info string includes the bare token `run` become live
<run-code> embeds executed by run.gilray.net; all other fences stay static:

    ``` racket run mode=expr include=defs.rkt
    ((λ (x) x) 5)
    ```

Recognized flags: run, readonly, no-run, numbers, resizable.
Recognized options: mode=, name=, include=, label=, rows=, filename=,
entrypoint=, button-label=.

Fenced blocks with the language `latex` are compiled (latex + dvisvgm, cached
by content hash under notes/<topic>/tex/) into tightly cropped SVGs shown as
centered display math; the content is a math-mode fragment (it is wrapped in
$\displaystyle ...$, so use aligned/array directly, never \[ \]). Inline math
written as $...$ in prose becomes a baseline-aligned inline SVG the same way.
The preamble defines \lm for a λ usable inside \texttt.

Run from anywhere:  python3 notes/build.py
"""

import hashlib
import html
import json
import re
import shlex
import subprocess
import tempfile
from datetime import date
from pathlib import Path

import markdown

NOTES = Path(__file__).resolve().parent
BACKGROUND_SRC = "/js/background.js?v=ff04838eec"
NOTES_BG_MODE = "fewcritters"

TEX_PREAMBLE = r"""\documentclass[12pt]{article}
\usepackage[T1]{fontenc}
\usepackage{amsmath,amssymb}
\usepackage{mathpazo}
\newcommand{\lm}{\ensuremath{\lambda}}
\usepackage[active,tightpage]{preview}
\setlength\PreviewBorder{0pt}
"""
TEX_VERSION = "v1"  # bump to invalidate every cached SVG
# SVG dimensions are in TeX pt; CSS px = pt * 4/3, then scaled so 12pt LaTeX
# text lands a bit LARGER than the page's ~19.3px Palatino (about two font-size
# notches, ~23px) — the math should stand out from the prose.
TEX_SCALE = (4.0 / 3.0) * 1.45

FLAG_ATTRS = {
    "readonly": "readonly",
    "no-run": "no-run",
    "norun": "no-run",
    "numbers": "line-numbers",
    "resizable": "resizable",
}
KV_ATTRS = {
    "mode", "name", "include", "label", "rows",
    "filename", "entrypoint", "button-label", "resizable",
}


def fail(message):
    raise SystemExit(f"notes/build.py: {message}")


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


def runcode_element(tokens, code, path):
    attrs = [("language", tokens[0])]
    for token in tokens[1:]:
        if token == "run":
            continue
        if token in FLAG_ATTRS:
            attrs.append((FLAG_ATTRS[token], None))
            continue
        key, sep, value = token.partition("=")
        if not sep or key not in KV_ATTRS:
            fail(f"{path}: unknown run-code flag or option {token!r}")
        attrs.append((key, value))
    rendered = "".join(
        f" {key}" if value is None else f' {key}="{html.escape(value, quote=True)}"'
        for key, value in attrs
    )
    return f"<run-code{rendered}>\n{html.escape(code)}\n</run-code>"


def tex_svg(snippet, display, topic_dir):
    """Compile a LaTeX fragment to a cached, tightly cropped SVG. Returns
    (filename, css width in px, css depth-below-baseline in px)."""
    wrapped = f"$\\displaystyle {snippet}$" if display else f"\\({snippet}\\)"
    doc = (
        TEX_PREAMBLE
        + "\\begin{document}\n\\begin{preview}\n"
        + wrapped
        + "\n\\end{preview}\n\\end{document}\n"
    )
    digest = hashlib.sha1(f"{TEX_VERSION}|{doc}".encode()).hexdigest()[:12]
    tex_dir = topic_dir / "tex"
    tex_dir.mkdir(exist_ok=True)
    out = tex_dir / f"{digest}.svg"
    if not out.exists():
        with tempfile.TemporaryDirectory() as tmp:
            (Path(tmp) / "s.tex").write_text(doc, encoding="utf-8")
            compile_run = subprocess.run(
                ["latex", "-interaction=nonstopmode", "-halt-on-error", "s.tex"],
                cwd=tmp, capture_output=True, text=True,
            )
            if compile_run.returncode != 0:
                log = "\n".join((Path(tmp) / "s.log").read_text(
                    encoding="utf-8", errors="replace").splitlines()[-25:])
                fail(f"latex failed for snippet:\n{snippet}\n--- log tail ---\n{log}")
            svg_run = subprocess.run(
                ["dvisvgm", "--no-fonts", "--exact-bbox", "--bbox=preview",
                 "-o", str(out), "s.dvi"],
                cwd=tmp, capture_output=True, text=True,
            )
            if svg_run.returncode != 0 or not out.exists():
                fail(f"dvisvgm failed for snippet:\n{snippet}\n{svg_run.stderr}")
        print(f"  tex {out.name}  {snippet.strip().splitlines()[0][:60]}")
    box = re.search(
        r"viewBox='([-\d.]+) ([-\d.]+) ([-\d.]+) ([-\d.]+)'", out.read_text()
    )
    if not box:
        fail(f"{out}: no viewBox in generated SVG")
    _, y0, width, height = (float(v) for v in box.groups())
    return out.name, width * TEX_SCALE, (y0 + height) * TEX_SCALE


def tex_display_html(snippet, topic_dir):
    name, width, _ = tex_svg(snippet, True, topic_dir)
    return (
        f'<div class="tex-display"><img src="tex/{name}" '
        f'style="width:{width:.2f}px" alt="{html.escape(snippet.strip())}"></div>'
    )


def tex_inline_html(snippet, topic_dir):
    name, width, depth = tex_svg(snippet, False, topic_dir)
    return (
        f'<img class="tex-inline" src="tex/{name}" '
        f'style="width:{width:.2f}px;vertical-align:{-depth:.2f}px" '
        f'alt="{html.escape(snippet.strip())}">'
    )


def extract_embeds(source, path, topic_dir):
    """Replace ```lang run ...``` and ```latex fences with placeholder
    comments that pass through markdown untouched, returning the rewritten
    source and the HTML embeds to substitute back in."""
    lines = source.split("\n")
    out, embeds = [], []
    i = 0
    while i < len(lines):
        opener = re.match(r"^```+\s*(\S.*)$", lines[i])
        if opener:
            try:
                tokens = shlex.split(opener.group(1))
            except ValueError:
                tokens = opener.group(1).split()
            runnable = len(tokens) > 1 and "run" in tokens[1:]
            latex = tokens[0] == "latex"
            if runnable or latex:
                j = i + 1
                while j < len(lines) and lines[j].strip() != "```":
                    j += 1
                if j == len(lines):
                    fail(f"{path}: unterminated {tokens[0]} fence")
                body = "\n".join(lines[i + 1:j])
                if runnable:
                    embeds.append(runcode_element(tokens, body, path))
                else:
                    embeds.append(tex_display_html(body, topic_dir))
                out.append(f"<!--embed{len(embeds) - 1}-->")
                i = j + 1
                continue
        out.append(lines[i])
        i += 1
    return "\n".join(out), embeds


STATIC_FENCE = re.compile(r"^```.*?^```[ \t]*$", re.S | re.M)
INLINE_MATH = re.compile(r"(?<![\\$])\$([^$\n]+?)\$")


def substitute_inline_math(source, topic_dir, tokens):
    """Turn $...$ into placeholder tokens (resolved to inline SVGs after
    markdown runs), skipping fenced blocks and `code spans`."""

    def handle_plain(segment):
        def replace(match):
            token = f"qtexinline{len(tokens)}q"
            tokens[token] = tex_inline_html(match.group(1), topic_dir)
            return token

        return INLINE_MATH.sub(replace, segment)

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


def render_markdown(body):
    md = markdown.Markdown(
        extensions=["fenced_code", "tables", "attr_list", "toc", "smarty"]
    )
    return md.convert(body)


def asset_version(site_path, _cache={}):
    """Short content hash of a site-root-relative asset, for cache-busting."""
    if site_path not in _cache:
        _cache[site_path] = hashlib.sha1(
            (NOTES.parent / site_path.lstrip("/")).read_bytes()
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


def document(topic, page_title, description, boxes):
    year = date.today().year
    title = html.escape(f'{topic["title"]}: {page_title}')
    body = "\n\n".join(boxes)
    return f"""<!DOCTYPE html>
<html lang="en-US">
\t<head>
\t\t<meta charset="utf-8">
\t\t<meta name="viewport" content="width=device-width, initial-scale=1">
\t\t<meta name="author" content="Thomas Gilray">
\t\t<meta name="copyright" content="(c) Thomas Gilray, 2018-{year}">
\t\t<meta name="robots" content="index, follow">
\t\t<meta name="description" content="{html.escape(description, quote=True)}">
\t\t<meta name="keywords" content="{html.escape(topic.get("keywords", ""), quote=True)}">

\t\t<title>{title}</title>

\t\t<link rel="icon" href="/favicon.ico" sizes="any">
\t\t<link rel="stylesheet" href="/screen.css?v={asset_version("/screen.css")}">
\t\t<link rel="stylesheet" href="/vendor/run-client/runcode.css?v={asset_version("/vendor/run-client/runcode.css")}">
\t\t<link rel="stylesheet" href="/notes/notes.css?v={asset_version("/notes/notes.css")}">
\t\t<script>window.conwayBgMode = '{NOTES_BG_MODE}';</script>
\t\t<script type="module" src="{BACKGROUND_SRC}"></script>
\t\t<script type="module" src="/vendor/run-client/embed.js"></script>
\t</head>

\t<body class="notes">
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


def page_strip(pages, current_index):
    entries = []
    for index, page in enumerate(pages):
        label = html.escape(page.get("nav") or page["title"])
        if index == current_index:
            entries.append(f'<strong class="here">{label}</strong>')
        else:
            entries.append(f'<a href="{page["slug"]}.html">{label}</a>')
    return ' <span class="sep">&middot;</span> '.join(entries)


def render_page(topic, pages, index, body_html):
    page = pages[index]
    top = (
        f'<div class="notes-crumbs"><a href="/">Thomas Gilray</a>'
        f' <span class="sep">&middot;</span> notes'
        f' <span class="sep">&middot;</span> {html.escape(topic["title"])}</div>\n'
        f'<h1>{html.escape(page["title"])}</h1>\n'
        f'<div class="notes-tldr">{page["tldr"]}</div>\n'
        f'<div class="notes-pagenav">{page_strip(pages, index)}</div>'
    )
    boxes = [box(top, "notes-top")]

    sections = re.split(r"(?=<h2[ >])", body_html)
    preamble = sections[0].strip()
    if preamble:
        boxes.append(box(preamble))
    for section in sections[1:]:
        boxes.append(box(section.strip()))

    prev_link, next_link = "<span></span>", "<span></span>"
    if index > 0:
        prev = pages[index - 1]
        prev_link = f'<a href="{prev["slug"]}.html">&#8606; {html.escape(prev["title"])}</a>'
    if index + 1 < len(pages):
        nxt = pages[index + 1]
        next_link = f'<a href="{nxt["slug"]}.html">{html.escape(nxt["title"])} &#8608;</a>'
    boxes.append(box(f'<div class="notes-endnav">{prev_link}{next_link}</div>'))

    return document(topic, page["title"], page["tldr"], boxes)


def build_topic(topic_dir):
    topic = json.loads((topic_dir / "topic.json").read_text(encoding="utf-8"))
    pages = []
    for slug in topic["pages"]:
        path = topic_dir / f"{slug}.md"
        if not path.exists():
            fail(f"{path}: listed in topic.json but missing")
        meta, body = parse_frontmatter(path.read_text(encoding="utf-8"), path)
        for required in ("title", "tldr"):
            if not meta.get(required):
                fail(f"{path}: frontmatter needs a {required}")
        meta["slug"] = slug
        meta["body"] = body
        pages.append(meta)

    for index, page in enumerate(pages):
        source_name = f"notes/{topic_dir.name}/{page['slug']}.md"
        stripped, embeds = extract_embeds(page["body"], source_name, topic_dir)
        inline_tokens = {}
        stripped = substitute_inline_math(stripped, topic_dir, inline_tokens)
        body_html = render_markdown(stripped)
        for k, embed in enumerate(embeds):
            token = f"<!--embed{k}-->"
            if token not in body_html:
                fail(f"{source_name}: embed fence {k} was mangled by markdown")
            body_html = body_html.replace(token, embed)
        for token, embed in inline_tokens.items():
            if token not in body_html:
                fail(f"{source_name}: inline math {token} was mangled by markdown")
            body_html = body_html.replace(token, embed)
        out_path = topic_dir / f"{page['slug']}.html"
        out_path.write_text(render_page(topic, pages, index, body_html), encoding="utf-8")
        print(f"built {out_path.relative_to(NOTES.parent)}")

    # Drop cached SVGs no longer referenced by any page of this topic.
    tex_dir = topic_dir / "tex"
    if tex_dir.is_dir():
        referenced = set()
        for page in pages:
            referenced.update(
                re.findall(r'src="tex/([^"]+)"',
                           (topic_dir / f"{page['slug']}.html").read_text(encoding="utf-8"))
            )
        for stale in tex_dir.glob("*.svg"):
            if stale.name not in referenced:
                stale.unlink()
                print(f"  tex pruned {stale.name}")

    # /notes/<topic>/ has no table-of-contents page; send it to the first page.
    first = f"{pages[0]['slug']}.html"
    (topic_dir / "index.html").write_text(
        "<!DOCTYPE html>\n<html lang=\"en-US\">\n<head>\n"
        "<meta charset=\"utf-8\">\n"
        f"<meta http-equiv=\"refresh\" content=\"0; url={first}\">\n"
        f"<link rel=\"canonical\" href=\"{first}\">\n"
        f"<title>{html.escape(topic['title'])}</title>\n</head>\n"
        f"<body><a href=\"{first}\">{html.escape(topic['title'])}</a></body>\n</html>\n",
        encoding="utf-8",
    )
    print(f"built {topic_dir.relative_to(NOTES.parent)}/index.html (redirect)")


def main():
    topic_dirs = sorted(path.parent for path in NOTES.glob("*/topic.json"))
    if not topic_dirs:
        fail("no notes/<topic>/topic.json found")
    for topic_dir in topic_dirs:
        build_topic(topic_dir)


if __name__ == "__main__":
    main()

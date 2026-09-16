# -*- coding: utf-8 -*-
"""Derive the publishable copy of a deck: the one the public PDF is exported from.

The lecture decks contain a few images that are fine to *show* in the lecture
hall but not to *publish* on a world-readable site: film stills and TV
publicity photos. The live `.pptx` in surfdrive stays the source of truth and
keeps them; this script produces a copy without them, and that copy is what
gets exported to PDF for the GitHub Pages site.

Generated, never hand-edited, so it cannot drift from the master the way a
second maintained deck would.

    python3 publishcopy.py <master.pptx> [<out.pptx>]

Blocked images are matched by content hash, not slide number, so re-ordering or
inserting slides does not silently stop the filtering from working. Add a hash
with `--list` to see what a deck contains:

    python3 publishcopy.py <master.pptx> --list
"""
import hashlib
import os
import re
import sys

from pptx import Presentation

# md5 of the image bytes -> why it may not be published. Get these from --list;
# do not type them from memory.
BLOCKED = {
    # also used as a banner strip on the diagnostic-testing slides, not just the
    # title slide of the example, which is exactly why this matches on content
    "76162f56": "Harry Potter film still (Pomona Sprout) - Warner Bros.",
    "78a0b4ed": "Harry Potter film still (Sybill Trelawney) - Warner Bros.",
    "4a46e64f": "Harry Potter film still (Trelawney reaction) - Warner Bros.",
}

# A slide that carries nothing but a title and a source line once its blocked
# image is gone is dropped rather than published as an empty title slide.
SOURCE_RE = re.compile(r"^\s*(source|picture[s]? source)\s*:", re.I)
KEEP_IF_TEXT_LONGER_THAN = 60


def walk(shapes):
    for sh in shapes:
        yield sh
        if sh.shape_type == 6:  # group
            yield from walk(sh.shapes)


def img_hash(pic):
    return hashlib.md5(pic.image.blob).hexdigest()[:8]


def carries_content(slide, title_text):
    """True if the slide still says something once the pictures are gone."""
    for sh in walk(slide.shapes):
        if sh.shape_type == 13:
            return True
        if not sh.has_text_frame:
            continue
        t = sh.text_frame.text.strip()
        if not t or t == title_text or SOURCE_RE.match(t) or t.isdigit():
            continue
        if len(t) > KEEP_IF_TEXT_LONGER_THAN:
            return True
    return False


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    src = sys.argv[1]
    prs = Presentation(src)
    slides = list(prs.slides)

    if "--list" in sys.argv:
        seen = {}
        for i, s in enumerate(slides, 1):
            for sh in walk(s.shapes):
                if sh.shape_type == 13:
                    seen.setdefault(img_hash(sh), []).append(i)
        for h, where in sorted(seen.items()):
            flag = "  <-- BLOCKED" if h in BLOCKED else ""
            print(f"  {h}  slides {where}{flag}")
        return

    out = sys.argv[2] if len(sys.argv) > 2 else None
    if out is None:
        d = os.path.join(os.path.dirname(os.path.abspath(src)), "_publish")
        os.makedirs(d, exist_ok=True)
        out = os.path.join(d, os.path.basename(src))

    # The output is derived, so editing it loses the edit on the next run. If it
    # is newer than the master, someone has almost certainly worked in the wrong
    # file; say so rather than overwriting their afternoon.
    if os.path.exists(out) and os.path.getmtime(out) > os.path.getmtime(src):
        if "--force" not in sys.argv:
            sys.exit(
                f"\n  refusing to overwrite: {out}\n"
                f"  is NEWER than the master it is derived from.\n\n"
                f"  Edits belong in the master; this copy is regenerated from it.\n"
                f"  Port the change into the master first, or pass --force to discard it.\n"
            )
        print("  --force: discarding the newer publish copy\n")

    removed, dropped = [], []
    for i, s in enumerate(slides, 1):
        for sh in list(walk(s.shapes)):
            if sh.shape_type != 13:
                continue
            h = img_hash(sh)
            if h in BLOCKED:
                sh._element.getparent().remove(sh._element)
                removed.append((i, h, BLOCKED[h]))

    lst = prs.slides._sldIdLst
    ids = list(lst)
    for i in range(len(slides), 0, -1):          # back to front, so ids stay valid
        s = slides[i - 1]
        if not any(r[0] == i for r in removed):
            continue
        title = s.shapes.title.text.strip() if s.shapes.title is not None else ""
        if not carries_content(s, title):
            prs.part.drop_rel(ids[i - 1].rId)
            lst.remove(ids[i - 1])
            dropped.append((i, title))

    prs.save(out)
    for i, h, why in removed:
        print(f"  removed image on slide {i}: {why}")
    for i, t in sorted(dropped):
        print(f"  dropped slide {i} ({t!r}): nothing left but a title and a source line")
    print(f"\n  {len(slides)} -> {len(Presentation(out).slides._sldIdLst)} slides")
    print(f"  wrote {out}")
    print("  Export the PDF from THIS file, in PowerPoint. The master keeps the images.")


if __name__ == "__main__":
    main()

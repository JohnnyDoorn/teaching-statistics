# RMS deck template pipeline

Re-bases an RMS PowerPoint deck onto the shared **RMS 2627** master: one theme
(Calibri Light / Calibri, accent1 `#ED7D31`), six layouts, real title and
slide-number placeholders instead of hand-placed text boxes.

## Usage

```sh
python3 bake.py  <src.pptx>   <baked.pptx>   # resolve old-template inheritance
./run.sh         <baked.pptx> <out.pptx>     # swap template + re-base slides
python3 diff.py  <name>                      # page-by-page render diff (see below)
```

`run.sh` always re-extracts, because `rebase.py` is **not idempotent** — running
it twice on the same `work/` silently produces a deck with no titles.

## Why each stage exists

- **bake.py** — slides pasted in from other decks carry placeholders whose
  geometry and bullets live in a layout we are about to delete. Bake those into
  the slide first, or they lose their bullets and collide with the new title box.
- **build.py** — writes theme + master + 6 layouts, then consolidates to a single
  master and sweeps orphaned parts. Decks accumulate one master per pasted-in
  deck; `Agresti5b` had **11 masters / 133 layouts**.
- **rebase.py** — classifies each slide, converts its title and slide number into
  placeholders, and fixes Cantarell (a Linux-only font that falls back
  unpredictably on macOS/Windows).

## The house title box

`838080, 365040` + `10512720 x 1322640` EMU (0.92", 0.40", 11.50" x 1.45"),
anchor centre. Not invented — it is the modal title box across all three decks.

## Collision guard

A title is only snapped to the house box when the target band is clear. The check
must consider `<p:pic>`, `<p:grpSp>` and `<p:graphicFrame>`, not just `<p:sp>`:
an early version snapped "Questions?" behind an xkcd image and it vanished.
Empty autoshapes are decorative backgrounds and are ignored.

## Verification

`diff.py` renders src and out via LibreOffice and reports the fraction of changed
pixels per page. Expect: near-zero for most pages, larger where Cantarell was
replaced (text re-metrics, usually *fixing* overflowing callout boxes).

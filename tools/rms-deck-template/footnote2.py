# -*- coding: utf-8 -*-
"""Source notes for the Gigerenzer & Hoffrage (1995) points."""
import sys
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor

DECK = sys.argv[1]
URL  = "https://pure.mpg.de/rest/items/item_2543495/component/file_2562163/content"
CITE = "(Gigerenzer & Hoffrage, 1995)"

NOTES = {
    # slide: (left, top, width, height, [lines]) -- CITE is hyperlinked wherever it appears
    52: (1.14, 6.66, 7.90, 0.30,
         ["Presented as frequencies instead of probabilities, students solved these "
          "problems far more often: 16% to 46% " + CITE]),
    53: (1.14, 6.66, 7.90, 0.30,
         ["Presented as frequencies instead of probabilities, students solved these "
          "problems far more often: 16% to 46% " + CITE]),
    3:  (0.92, 6.84, 8.20, 0.34,
         ["Diagnostic testing is hypothesis testing in miniature: P(Pos | M) is not P(M | Pos).",
          "The same confusion appears with p-values, lecture 13 " + CITE]),
}

prs = Presentation(DECK)
for idx, (L, T, W, H, lines) in sorted(NOTES.items()):
    slide = list(prs.slides)[idx - 1]
    # drop a previous run of this script so it stays idempotent
    for sh in list(slide.shapes):
        if sh.has_text_frame and "Gigerenzer & Hoffrage" in sh.text_frame.text:
            sh._element.getparent().remove(sh._element)
    tf = prs.slides[idx - 1].shapes.add_textbox(
        Inches(L), Inches(T), Inches(W), Inches(H)).text_frame
    tf.word_wrap = False
    for i, line in enumerate(lines):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        head, _, tail = line.partition(CITE)
        for text, link in ((head, None), (CITE, URL) if tail == "" and CITE in line else (None, None)):
            if not text: continue
            r = p.add_run(); r.text = text
            r.font.size, r.font.name = Pt(10), "Arial"
            if link: r.hyperlink.address = link
            else:    r.font.color.rgb = RGBColor(0x00, 0x00, 0x00)
    print(f"  slide {idx}: {len(lines)} line(s)")
prs.save(DECK)

# -*- coding: utf-8 -*-
"""L4 practice exercises. Replaces the earlier 2.79/3.15 pair.

All three are odd-numbered, so the book answers them; the text below is the
book's own answer. The r values in 3.17 come from Johnny, who checked them
against the figure (the appendix gives only the letter matching).
"""
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor

D="/Users/johnny/surfdrive/Teaching/RMS/RMS_2026/RMS_2026/RMS2627_4_VariabilityAndAssociation.pptx"
ORANGE=RGBColor(0xED,0x7D,0x31)
prs=Presentation(D)

# --- drop the previous exercise appendix ---
lst=prs.slides._sldIdLst
drop=[]
for i,s in enumerate(prs.slides,1):
    t=" ".join(sh.text_frame.text for sh in s.shapes if sh.has_text_frame)
    if "Highlighted exercises" in t or t.strip().startswith(("2.79","3.15")):
        drop.append(i)
for i in sorted(drop, reverse=True):
    sid=list(lst)[i-1]
    prs.part.drop_rel(sid.rId); lst.remove(sid)
print(f"  removed previous appendix (slides {drop})")

LIST=[("2.49", "Life expectancy 2020",              "computation: compare variability"),
      ("3.5",  "Hygiene awareness and ownership",   "conceptual: conditional proportions"),
      ("3.17", "Match the scatterplots with r",     "conceptual: reading correlation")]

ANSWERS=[
 ("2.49  Life expectancy 2020",
  ["a.  Africa. Life expectancies vary much more than for Europe, where all values "
   "are very similar",
   "b.  Western Europe: s = 1.05        Africa: s = 5.18",
   "→ same kind of average, very different spread"], None),
 ("3.5  Hygiene awareness and ownership status",
  ["a.  Response: hygiene awareness    Explanatory: ownership",
   "b.  (i) 6,105      (ii) 4,218",
   "c.  No. These are counts, not proportions of owners and tenants, and there are "
   "far more owners than tenants in this study",
   "d.  Owner: 0.60 aware / 0.40 unaware (n = 10,224)",
   "     Tenant: 0.75 aware / 0.25 unaware (n = 5,606)",
   "e.  Tenants appear more likely than owners to be aware of hygiene"], None),
 ("3.17  Match the scatterplots with r",
  ["1 → (c)  r = −0.9        strong negative",
   "2 → (a)  r = −0.5        moderate negative",
   "3 → (d)  r = 0            no linear association",
   "4 → (b)  r = 0.6         moderate positive"],
  "letter matching from the book; r values checked by JvD against the figure"),
]

lay=next(l for l in prs.slide_masters[0].slide_layouts if l.name=="Title Only")
def tb(slide,L,T,W,H,lines,size,color=None,bold_first=False):
    tf=slide.shapes.add_textbox(Inches(L),Inches(T),Inches(W),Inches(H)).text_frame
    tf.word_wrap=True
    for j,l in enumerate(lines):
        pa=tf.paragraphs[0] if j==0 else tf.add_paragraph()
        r=pa.add_run(); r.text=l
        r.font.size=Pt(size); r.font.bold=(bold_first and j==0)
        r.font.color.rgb=color or RGBColor(0,0,0)

s=prs.slides.add_slide(lay)
s.shapes.title.text="Highlighted exercises from the book"
tb(s,0.92,2.30,11.2,2.2,[f"{n} – {t}   ({k})" for n,t,k in LIST],26)
tb(s,0.92,4.90,11.2,0.7,["→ try yourself first, then check the next slides for answers"],24,ORANGE)
print("  added index slide (2.49, 3.5, 3.17)")

for title, lines, note in ANSWERS:
    s=prs.slides.add_slide(lay)
    s.shapes.title.text=title
    tb(s,0.92,2.05,11.2,3.9,lines,20)
    tb(s,0.92,6.20,11.2,0.5,
       [note or "Answer as given in Agresti (odd-numbered exercises are answered in the book)"],14)
    print(f"  added answer slide: {title}")
prs.save(D)
print(f"  -> {len(prs.slides._sldIdLst)} slides")

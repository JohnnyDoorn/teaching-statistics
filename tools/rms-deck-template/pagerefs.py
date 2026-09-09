# -*- coding: utf-8 -*-
"""Repoint every Agresti page reference at the Global 5th Edition.

The slide numbers came from an earlier edition; the offset is not constant
(+22 early, +34 mid, +69 late), so each was located by reading the actual page.
"""
import sys
from pptx import Presentation

D = "/Users/johnny/surfdrive/Teaching/RMS/RMS_2026/RMS_2026/"

# deck, slides, old text, new text, what was verified on the new page
FIXES = [
 ("RMS2627_3_WhyStatistics.pptx", [14,17,18], "(p. 4)", "(p. 26)",
  '"In short, statistics is the art and science of learning from data."'),
 ("RMS2627_3_WhyStatistics.pptx", [32], "(p. 25)", "(p. 58)",
  '"Variable: A variable is any characteristic observed in a study."'),
 ("RMS2627_4_VariabilityAndAssociation.pptx", [31], "p. 65", "p. 99",
  '"Three useful percentiles are the quartiles." (Section 2.5)'),
 ("RMS2627_4_VariabilityAndAssociation.pptx", [50], "p. 107", "p. 154",
  '"A summary measure called the correlation coefficient..." (Chapter 3)'),
 ("RMS2627_10_SamplingDistributions.pptx", [18,19], "p. 308", "p. 356",
  '"We call this distribution the sampling distribution..." (Section 7.1)'),
 ("RMS2627_10_SamplingDistributions.pptx", [21], "p. 367", "p. 358",
  'the figure showing population, data and sampling distributions together'),
 ("RMS2627_20_NonparametricTests.pptx", [27], "p. 727", "p. 464",
  'Section 9.2 "Significance Test About a Proportion" begins here'),
 ("RMS2627_20_NonparametricTests.pptx", [61], "p. 736", "p. 464", 'same section 9.2'),
 ("RMS2627_20_NonparametricTests.pptx", [83], "page 736", "p. 464", 'same section 9.2'),
]

def replace_in_slide(slide, old, new):
    n = 0
    for sh in slide.shapes:
        if not sh.has_text_frame: continue
        for pa in sh.text_frame.paragraphs:
            runs = pa.runs
            if not runs: continue
            joined = "".join(r.text for r in runs)
            if old not in joined: continue
            for r in runs:                       # simple case: inside one run
                if old in r.text:
                    r.text = r.text.replace(old, new); n += 1; break
            else:                                # spans runs: rebuild on run 0
                runs[0].text = joined.replace(old, new)
                for r in runs[1:]: r._r.getparent().remove(r._r)
                n += 1
    return n

by_deck = {}
for deck, slides, old, new, why in FIXES:
    by_deck.setdefault(deck, []).append((slides, old, new, why))

for deck, jobs in by_deck.items():
    prs = Presentation(D + deck); sl = list(prs.slides); total = 0
    print(f"\n{deck}")
    for slides, old, new, why in jobs:
        hits = sum(replace_in_slide(sl[i-1], old, new) for i in slides)
        assert hits == len(slides), f"{deck} {old!r}: expected {len(slides)}, got {hits}"
        total += hits
        print(f"   slides {str(slides):<12} {old:<9} -> {new:<8} {why}")
    prs.save(D + deck)
    print(f"   {total} replacement(s) saved")

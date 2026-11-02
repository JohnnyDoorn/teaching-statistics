"""2026 dates and one page reference in lectures 20 and 22 (2026-10-30):

    python3 dates_2026.py <deck.pptx> <out.pptx>

The 2025 Bayes block was rearranged around a strike (no lecture Tue 9 Dec, Thu 11 Dec
in Pathe Tuschinski). In 2026 there is no strike: L22 (Bayes 1) is Thu 3 Dec, L23 Tue
8 Dec, L24 Thu 10 Dec, the same Tue/Thu rhythm as L12 (Thu 15 Oct).

- L20 slide 27: the Wilcoxon normal approximation z = (difference in mean ranks)/se is
  Agresti p. 811. p. 464 is section 9.2 (z test for a proportion), which stays right for
  the sign test on slides 61 and 83.
- L20 slide 71: the Bayes block starts Thu 3 Dec ("dec 5th" was 2024's date).
- L22 slide 5: next two lectures on Tue 8 Dec and Thu 10 Dec.
- L22 slide 6 ("Two important notes", the 2025 strike) is dropped with its notes slide.
- L22 slide 72 (Practical Stuff): JASP 0.95.4 -> 0.99, the latest release (v0.99.0, 21 Oct 2026).

Every replacement asserts its hit count; Deck.save validates the package.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from l12_round1 import Deck

EDITS = {
    'RMS2627_20_NonparametricTests': [
        (27, '(Agresti, p. 464, see formula sheet)', '(Agresti, p. 811, see formula sheet)'),
        (71, '<a:t>(starting on dec 5</a:t>', '<a:t>(starting on dec 3</a:t>'),
        (71, '<a:t>th</a:t>', '<a:t>rd</a:t>'),
    ],
    'RMS2627_22_BayesianStatistics': [
        (5, '<a:t>Thursday Dec 4 (Chapters 1-4)</a:t>', '<a:t>Tuesday Dec 8 (Chapters 1-4)</a:t>'),
        (5, '<a:t>Thursday Dec 11(Chapter 5)</a:t>', '<a:t>Thursday Dec 10 (Chapter 5)</a:t>'),
        (72, '<a:t>JASP 0.95.4 – Free at </a:t>', '<a:t>JASP 0.99 – Free at </a:t>'),
    ],
}
# slide number, and a text it must hold, so a reordered deck fails instead of losing a slide
DROP = {'RMS2627_22_BayesianStatistics': [(6, 'Tuesday Dec 9: No lecture due to the demonstration')]}


def drop_slide(d, n, must_hold):
    sp, rp = d.sp(n), d.rp(n)
    assert must_hold in d.get(sp), f'slide {n} does not hold {must_hold!r}'
    gone = [sp, rp]
    notes = re.search(r'Target="\.\./notesSlides/(notesSlide\d+\.xml)"', d.get(rp))
    if notes:
        np_ = 'ppt/notesSlides/' + notes.group(1)
        gone += [np_, 'ppt/notesSlides/_rels/' + notes.group(1) + '.rels']
    # the slide's rId in presentation.xml.rels, then its sldId entry
    prels = d.get('ppt/_rels/presentation.xml.rels')
    target = 'slides/' + sp.split('/')[-1]
    rel = re.search(r'<Relationship [^>]*Id="(rId\d+)"[^>]*Target="' + re.escape(target) + r'"[^>]*/>', prels) \
        or re.search(r'<Relationship [^>]*Target="' + re.escape(target) + r'"[^>]*Id="(rId\d+)"[^>]*/>', prels)
    rid = rel.group(1)
    d.put('ppt/_rels/presentation.xml.rels', prels.replace(rel.group(0), ''))
    pres = d.get('ppt/presentation.xml')
    sld = re.findall(r'<p:sldId [^>]*r:id="' + rid + r'"/>', pres)
    assert len(sld) == 1, sld
    d.put('ppt/presentation.xml', pres.replace(sld[0], ''))
    ct = d.get('[Content_Types].xml')
    for p in gone:
        ct = re.sub(r'<Override PartName="/' + re.escape(p) + r'"[^>]*/>', '', ct)
        d.parts.pop(p, None)
        if p in d.order: d.order.remove(p)
    d.put('[Content_Types].xml', ct)
    for p, data in d.parts.items():   # nothing else may point at the removed parts
        if p.endswith(('.xml', '.rels')):
            for g in gone:
                assert g.split('/')[-1] not in data.decode('utf-8', 'ignore'), f'{p} still refers to {g}'


if __name__ == '__main__':
    src, out = sys.argv[1], sys.argv[2]
    name = os.path.basename(src).rsplit('.', 1)[0]
    d = Deck(src)
    for n, old, new in EDITS[name]:
        d.rep(n, old, new)
    for n, must_hold in DROP.get(name, []):
        drop_slide(d, n, must_hold)
    d.save(out)

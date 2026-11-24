"""L20 pre-teaching round (2026-11-20, lecture Thu 26 Nov), run one item at a time:

    python3 l20_round1.py <in.pptx> <out.pptx> <item> [<item> ...]

Full check as l16_round2 asks: all 84 slides looked at in PowerPoint's own export, both links curled,
every number recomputed (R), exercises 15.8, 15.10 and 15.20 checked against Agresti 5th Global, book
pointers checked (p. 464, p. 811, section 6.3, ch. 15 Example 1). Notes in rms/_lecture-notes-2026.md (L20).

Approved (his yes 2026-11-20, consistent with SSR and RMS WA13):
  normality   'normally distributed residuals (i.e., model errors)' for 'a sample / the data / the
              population is normally distributed' on slides 9, 16, 54, 64 (2x), 84

Mechanical (applied 2026-11-20, backup _backup/*_pre-round1_2026-11-20.pptx):
  typo        9 '(e.g.,)', 21 '543-21' (slide 25 lists it as 3,4,5 | 1,2), 26 'have same outcomes',
              41 'go an empty cell', 62 'assume alpha level', 65/82 'Kruskal Wallis', 67/82 'Chi Square'
              (L18 and Agresti say chi-squared), 70 'Outiers', 71 'dec 3rd', 79 'whether there the
              groups', 82 'significant at the 0.05.', 83 'Pvalue'
  format      38: the '11th' superscript sat at baseline 101% (one line up); 67: 'Chi Square ...' at
              24 pt in a 26 pt bullet; 83: two '0.068' runs in Lucida Console (28 and 24 pt)
  fit         render check / zoom: 17 and 60 (picture margins over text: pictures sent behind the text),
              42 (arrows drawn through the yellow boxes' sums: sent behind the boxes), 70 (title on the
              top table: tables and note 0.12 in lower), 79 (orange box spilled 0.1 in each side:
              0.30 in wider to the left), 84 (title over the first line: body 0.15 in lower)

His calls (all applied 2026-11-20 on his answers):
  hide2       slide 2 is still the 7 Sept placeholder; hidden (slide 70 has the outlier story)
  hide52      slide 52 advertises SIOS 'The Gift of Open Science', December 5 16:00-17:00: last year's
              poster (5 Dec 2026 is a Saturday); hidden
  ties        slide 82's practice box asks to reproduce H = 7.38, which MINITAB adjusts for ties; the
              lecture's formula (formula sheet 4, no tie correction) gives 7.36, P = 0.025 either way.
              His words: make clear they only need the no-tie formula from the formula sheet for the exam
  residual    slide 16: the residual defined in place ('each score minus its group mean'); no RMS slide
              defined it before
"""
import re, sys
from l16_round1 import L16Deck
from l12_round1 import _geom, _set_geom


def _move(d, n, name, dy, tag='p:sp'):
    s, blk = d.shape_block(n, name, tag)
    _, (x, y, cx, cy) = _geom(blk)
    d.put(d.sp(n), s.replace(blk, _set_geom(blk, x, y + dy, cx, cy)))
    print(f'  slide {n} {name}: y {y:.2f} -> {y + dy:.2f}')


def _place(d, n, name, x=None, y=None, cx=None, cy=None, tag='p:sp'):
    s, blk = d.shape_block(n, name, tag)
    _, (x0, y0, cx0, cy0) = _geom(blk)
    x1, y1 = (x0 if x is None else x), (y0 if y is None else y)
    cx1, cy1 = (cx0 if cx is None else cx), (cy0 if cy is None else cy)
    d.put(d.sp(n), s.replace(blk, _set_geom(blk, x1, y1, cx1, cy1)))
    print(f'  slide {n} {name}: {cx0:.2f} x {cy0:.2f} at ({x0:.2f}, {y0:.2f}) -> {cx1:.2f} x {cy1:.2f} at ({x1:.2f}, {y1:.2f})')


def _behind(d, n, moving, target):
    """Draw the shapes in `moving` [(name, tag)] just before shape `target` (name, tag): lower in z-order."""
    s = d.get(d.sp(n))
    blocks = []
    for name, tag in moving:
        _, blk = d.shape_block(n, name, tag)
        assert s.count(blk) == 1
        s = s.replace(blk, '', 1); blocks.append(blk)
    d.put(d.sp(n), s)
    s, tblk = d.shape_block(n, *target)
    d.put(d.sp(n), s.replace(tblk, ''.join(blocks) + tblk, 1))
    print(f'  slide {n}: {", ".join(m for m, _ in moving)} behind {target[0]}')


# ---------------------------------------------------------------------------------- approved
def item_normality(d):
    d.rep(9, '<a:t>Typical assumption is that a sample is (approximately) normally distributed</a:t>',
          '<a:t>Typical assumption is that the residuals (i.e., model errors) are (approximately) normally distributed</a:t>')
    d.rep(16, '<a:t>Normally distributed</a:t>', '<a:t>Normally distributed residuals (i.e., model errors)</a:t>')
    d.rep(54, '<a:t>Extremely non-normal distribution of the population</a:t>',
          '<a:t>Extremely non-normal distribution of the residuals</a:t>')
    d.rep(64, '<a:t>We do not know whether the data are normally distributed',
          '<a:t>We do not know whether the residuals are normally distributed')
    d.rep(64, '<a:t>The data are not normally distributed and sample size',
          '<a:t>The residuals are not normally distributed and sample size')
    d.rep(84, '<a:t>The data might not be normally distributed, and the sample size',
          '<a:t>The residuals might not be normally distributed, and the sample size')
    print('  slides 9, 16, 54, 64 (2x), 84')


# ---------------------------------------------------------------------------------- mechanical
def item_typo(d):
    d.rep(9, '<a:t>Data may (e.g.,) be skewed</a:t>', '<a:t>Data may, e.g., be skewed</a:t>')
    d.rep(21, ', …, 543-21} </a:t>', ', …, 345-12} </a:t>')
    d.rep(26, '<a:t>But some events have same outcomes</a:t>', '<a:t>But some events have the same outcome</a:t>')
    d.rep(41, 'A1:A18), then go an empty cell and put:</a:t>', 'A1:A18), then go to an empty cell and put:</a:t>')
    d.rep(62, '<a:t>If we assume alpha level of 0.05 </a:t>', '<a:t>If we assume an alpha level of 0.05 </a:t>')
    d.rep(65, '<a:t>The Kruskal Wallis test for comparing', '<a:t>The Kruskal-Wallis test for comparing')
    d.rep(67, '<a:t>Chi Square distribution (df = #groups -1)</a:t>', '<a:t>chi-squared distribution (df = #groups -1)</a:t>')
    d.rep(70, '<a:t>Outiers can have a big impact', '<a:t>Outliers can have a big impact')
    d.rep(71, '<a:t>(starting on dec 3</a:t>', '<a:t>(starting on Dec 3</a:t>')
    d.rep(79, '<a:t>there the groups differ from each other,', '<a:t>the groups differ from each other,')
    d.rep(82, 'Sampling dist is Chi Square with df = 2</a:t>', 'Sampling dist is chi-squared with df = 2</a:t>')
    d.rep(82, 'which is significant at the 0.05. If that was', 'which is significant at the 0.05 level. If that was')
    d.rep(82, '<a:t>Practice: try to compute the Kruskal Wallis statistic', '<a:t>Practice: try to compute the Kruskal-Wallis statistic')
    d.rep(83, '<a:t>Pvalue</a:t>', '<a:t>P-value</a:t>')
    print('  slides 9, 21, 26, 41, 62, 65, 67, 70, 71, 79, 82 (3x), 83')


def item_format(d):
    d.rep(38, 'baseline="101000"', 'baseline="30000"')
    s = d.get(d.sp(67))
    pat = r'(<a:r><a:rPr lang="en-US" )sz="2400"( (?:(?!</a:r>).)*?<a:t>chi-squared distribution \(df)'
    assert len(re.findall(pat, s, re.S)) == 1
    d.put(d.sp(67), re.sub(pat, r'\1sz="2600"\2', s, flags=re.S))
    s = d.get(d.sp(83))
    runs = [m for m in re.finditer(r'<a:r>(?:(?!</a:r>).)*?</a:r>', s, re.S)
            if 'Lucida Console' in m.group(0) and re.search(r'<a:t>0\.068 ?</a:t>', m.group(0))]
    assert len(runs) == 2, len(runs)
    for m in runs:
        r = re.sub(r'<a:latin typeface="Lucida Console"/>', '', m.group(0))
        r = re.sub(r'sz="2[48]00"', 'sz="2600"', r)
        s = s.replace(m.group(0), r, 1)
    d.put(d.sp(83), s)
    print('  slide 38: 11th superscript at 30%; slide 67: 26 pt; slide 83: 0.068 in the body font, 26 pt')


def item_fit(d):
    _behind(d, 17, [('Picture 7', 'p:pic'), ('Picture 8', 'p:pic')], ('CustomShape 2', 'p:sp'))
    _behind(d, 60, [('Picture 601', 'p:pic')], ('CustomShape 2', 'p:sp'))
    _behind(d, 42, [('Line 7', 'p:sp'), ('Line 8', 'p:sp'), ('Line 9', 'p:sp')], ('CustomShape 2', 'p:sp'))
    for name in ('Table 6', 'Table 8', 'Table 7'):
        _move(d, 70, name, 0.12, 'p:graphicFrame')
    _move(d, 70, 'CustomShape 23', 0.12)
    _place(d, 79, 'CustomShape 4', x=6.06, cx=6.94)             # right edge stays at 13.00
    _place(d, 84, 'CustomShape 8', y=1.75, cy=5.10)             # bottom 6.64 -> 6.85, above the slide number


# ---------------------------------------------------------------------------------- his calls
def _hide(d, n):
    s = d.get(d.sp(n))
    m = re.search(r'<p:sld\b[^>]*>', s)
    assert 'show="0"' not in m.group(0)
    d.put(d.sp(n), s.replace(m.group(0), m.group(0)[:-1] + ' show="0">', 1))
    print(f'  slide {n} hidden')


def item_hide2(d): _hide(d, 2)
def item_hide52(d): _hide(d, 52)


def item_ties(d):
    d.rep(82, 'and see if you can reproduce the computer output! </a:t>',
          'and see if you can reproduce the computer output! The output (H = 7.38) corrects for ties. '
          'For the exam you only need the formula on formula sheet 4, which has no tie correction and '
          'gives 7.36 here (P = 0.025 either way).</a:t>')
    _place(d, 82, 'CustomShape 8', x=0.92, y=6.05, cx=10.90, cy=1.30)   # wider and up into the gap under d); clear of the slide number
    print('  slide 82: tie note in the practice box')


def item_residual(d):
    d.rep(16, '<a:t>Normally distributed residuals (i.e., model errors)</a:t>',
          '<a:t>Normally distributed residuals (i.e., model errors, each score minus its group mean)</a:t>')
    print('  slide 16: residual defined')


ITEMS = {'normality': item_normality, 'typo': item_typo, 'format': item_format, 'fit': item_fit,
         'hide2': item_hide2, 'hide52': item_hide52, 'ties': item_ties, 'residual': item_residual}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = L16Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

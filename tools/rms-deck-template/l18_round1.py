"""L18 pre-teaching round (2026-11-10, lecture Thu 12 Nov), run one item at a time:

    python3 l18_round1.py <in.pptx> <out.pptx> <item> [<item> ...]

Full check as l16_round2 asks: every slide looked at in PowerPoint's own export, every link curled,
every number recomputed (R), exercises checked against Agresti 5th Global. Notes in
rms/_lecture-notes-2026.md (L18).

Mechanical (applied 2026-11-10, backup _backup/*_pre-round1_2026-11-10.pptx):
  links       slide 2: flowchart link 404 (Pages is case-sensitive, RMS/ -> rms/), as L16 slide 60
  typo        6 'variabele', 12 'dependent of' -> 'on', 16 'Associations' (the other five overviews say
              'Association'), 53 'assocation', 92 'form' / 'Nickolson', 93 'distributions-->', 97 'connect'
  overview    slides 4, 16, 54, 61, 78, 81: the closing 'Recap' item was numbered 2 (startAt 2 -> 3)
  pointer     slide 80: the expected-frequency formula is on slide 32, not 31
  sup         slides 40-41: 'chi2' -> chi with a superscript 2 (titles and the green box)
  fit         text out of its box in the render check: 46, 64 (off the slide), 69, 70, 71, 76, 77

His call (prepared, not applied):
  ex1110      slides 94/96 answer the 4th edition's 11.10 (P = 0.05; df 1, 2, 4, 16, 16). The 5th Global asks
              P = 0.01 for 2x2, 2x3, 3x5, 4x5, 5x9 (df 1, 2, 8, 12, 32): 6.63, 9.21, 20.09, 26.22, 53.49. The box
              had it backwards in either version (chi2 = 4 is significant with 1 df, not with 4 df).
  props       slides 57/59: the 'Proportions:' table shows 0.26/0.24/0.16/0.34 (totals 0.42/0.58), not this
              lecture's 0.3625/0.1375/0.1625/0.3375 (0.525/0.475) from slides 8 and 21
  assump      slide 18: the ticked table has an expected count of 17 x 16 / 80 = 3.4, so slide 31's own rule
              fails it; the electroshock data (expected 21/19/21/19) go in its place
  venn        slide 20: the diagram says 'Depression'; the example is psychosis recovery
  stale98     slide 98: an empty 'Difference of the conditional proportions:' box, and a pointer to 'the
              X2 result' on the next slide while it is on this one (slide 99 has c and d)
"""
import re, sys
from l16_round1 import L16Deck
from l12_round1 import _geom, _set_geom, EMU


class L18Deck(L16Deck):
    def add_media(self, name, data):
        return super().add_media(name.replace('l16_eq_', 'l18_eq_'), data)


def _place(d, n, name, x=None, y=None, cx=None, cy=None):
    s, blk = d.shape_block(n, name)
    _, (x0, y0, cx0, cy0) = _geom(blk)
    x1, y1 = (x0 if x is None else x), (y0 if y is None else y)
    cx1, cy1 = (cx0 if cx is None else cx), (cy0 if cy is None else cy)
    d.put(d.sp(n), s.replace(blk, _set_geom(blk, x1, y1, cx1, cy1)))
    print(f'  slide {n} {name}: {cx0:.2f} x {cy0:.2f} at ({x0:.2f}, {y0:.2f}) -> {cx1:.2f} x {cy1:.2f} at ({x1:.2f}, {y1:.2f})')


def _sup(d, n, before, after=''):
    """Split '<a:t>{before}2{after}</a:t>' so the 2 becomes a superscript run with the same rPr."""
    s = d.get(d.sp(n))
    pat = re.compile(r'<a:r>(<a:rPr[^>]*?)(/>|>(?:(?!</a:r>).)*?</a:rPr>)<a:t>' + re.escape(before + '2' + after) + '</a:t></a:r>', re.S)
    m = list(pat.finditer(s))
    assert len(m) == 1, (n, before, len(m))
    open_, rest = m[0].group(1), m[0].group(2)
    sup = open_ + ' baseline="30000"' + rest
    new = f'<a:r>{open_}{rest}<a:t>{before}</a:t></a:r><a:r>{sup}<a:t>2</a:t></a:r>'
    if after: new += f'<a:r>{open_}{rest}<a:t>{after}</a:t></a:r>'
    d.put(d.sp(n), s.replace(m[0].group(0), new))


def _cells(d, n, k, values):
    """Table k on slide n: values = {(row, col): text}, each cell holding one run."""
    s = d.get(d.sp(n))
    tbls = list(re.finditer(r'<a:tbl>.*?</a:tbl>', s, re.S))
    tb = tbls[k].group(0); new = tb
    rows = re.findall(r'<a:tr .*?</a:tr>', tb, re.S)
    for (r, c), text in values.items():
        cells = re.findall(r'<a:tc[ >].*?</a:tc>', rows[r], re.S)
        cell = cells[c]
        assert cell.count('<a:t>') == 1, (n, k, r, c)
        new_cell = re.sub(r'<a:t>[^<]*</a:t>', f'<a:t>{text}</a:t>', cell)
        new_row = rows[r].replace(cell, new_cell, 1)
        new = new.replace(rows[r], new_row, 1); rows[r] = new_row
    d.put(d.sp(n), s.replace(tb, new))


# ---------------------------------------------------------------------------------- mechanical
def item_links(d):
    old = 'Target="https://johnnydoorn.github.io/teaching-statistics/RMS/WhichSignificanceTestToUse.pdf"'
    r = d.get(d.rp(2)); assert r.count(old) == 1
    d.put(d.rp(2), r.replace(old, old.replace('/RMS/', '/rms/')))
    print('  slide 2: flowchart link RMS/ -> rms/')


def item_typo(d):
    d.rep(6, '<a:t>Response variabele</a:t>', '<a:t>Response variable</a:t>')
    s = d.get(d.sp(12))
    pat = r'(<a:t>dependent</a:t></a:r><a:r>(?:(?!</a:r>).)*?<a:t>) of the explanatory variable, when the </a:t>'
    assert len(re.findall(pat, s, re.S)) == 1
    d.put(d.sp(12), re.sub(pat, r'\1 on the explanatory variable, when the </a:t>', s, flags=re.S))
    d.rep(16, '<a:t>Associations and (in)dependence </a:t>', '<a:t>Association and (in)dependence </a:t>')
    d.rep(53, 'the strength of the assocation!</a:t>', 'the strength of the association!</a:t>')
    d.rep(92, 'was taken form the 1975 movie', 'was taken from the 1975 movie')
    d.rep(92, 'starring Jack Nickolson.', 'starring Jack Nicholson.')
    d.rep(93, '<a:t>For distributions-→ critical value:</a:t>', '<a:t>For distributions → critical value:</a:t>')
    d.rep(97, 'because of the connect between', 'because of the connection between')
    print('  slides 6, 12, 16, 53, 92 (2x), 93, 97')


def item_overview(d):
    for n in (4, 16, 54, 61, 78, 81):
        s = d.get(d.sp(n))
        ps = [p for p in re.findall(r'<a:p>.*?</a:p>', s, re.S) if 'startAt="2"' in p and '<a:t>Recap</a:t>' in p]
        assert len(ps) == 1, (n, len(ps))
        d.put(d.sp(n), s.replace(ps[0], ps[0].replace('startAt="2"', 'startAt="3"')))
    print('  slides 4, 16, 54, 61, 78, 81: closing Recap numbered 3')


def item_pointer(d):
    d.rep(80, 'using the formula from slide 31</a:t>', 'using the formula from slide 32</a:t>')
    print('  slide 80: slide 31 -> 32')


def item_sup(d):
    _sup(d, 40, 'Sampling distribution for  𝝌')          # two spaces, as in the deck
    _sup(d, 40, 'Sampling distribution of  𝝌', ', ')
    _sup(d, 41, 'Sampling distribution for  𝝌')
    print('  slides 40, 41: chi2 -> superscript 2')


def item_fit(d):
    # render check 2026-11-10 (PowerPoint export): SPILL in 46, 69, 70, 71, 76, 77; 64's note ran off the slide
    _place(d, 46, 'CustomShape 35', y=4.10, cy=1.86)                 # grows up, bottom stays at 5.96
    _place(d, 64, 'CustomShape 8', y=6.80, cx=8.60, cy=0.62)          # two lines instead of three
    _place(d, 69, 'CustomShape 4', x=1.55, cx=10.29)                  # centred text, wider on both sides
    _place(d, 70, 'CustomShape 46', x=4.55, cx=7.20)
    _place(d, 71, 'CustomShape 34', cy=1.50)
    _place(d, 71, 'CustomShape 49', y=6.15, cy=1.20)                  # up into the free band under the boxes
    _place(d, 76, 'CustomShape 52', cy=2.40)
    _place(d, 76, 'CustomShape 53', y=4.30, cy=1.15)
    _place(d, 77, 'CustomShape 50', cy=1.35)
    # 71: the second box sits in Group 803 (child offsets = group offsets), so the group grows with it
    s = d.get(d.sp(71))
    g = re.search(r'<p:grpSp>(?:(?!</p:grpSp>).)*?name="Group 803".*?</p:grpSp>', s, re.S).group(0)
    cy0, cy1 = 1050480, round(1.50 * EMU)
    new = g.replace(f'<a:ext cx="4493160" cy="{cy0}"/><a:chOff', f'<a:ext cx="4493160" cy="{cy1}"/><a:chOff', 1)
    new = new.replace(f'<a:chExt cx="4493160" cy="{cy0}"/>', f'<a:chExt cx="4493160" cy="{cy1}"/>', 1)
    new = new.replace(f'<a:ext cx="2970720" cy="{cy0}"/>', f'<a:ext cx="2970720" cy="{cy1}"/>', 1)
    assert new.count(f'cy="{cy1}"') == 3
    d.put(d.sp(71), s.replace(g, new))
    print(f'  slide 71 Group 803 / CustomShape 36: height {cy0 / EMU:.2f} -> 1.50')


# ---------------------------------------------------------------------------------- his calls
def item_ex1110(d):
    d.rep(94, '<a:t>11.10 (“what gives p-value = 0.05”)  </a:t>', '<a:t>11.10 (“what gives p-value = 0.01”)  </a:t>')
    s = d.get(d.sp(96))
    for old, new in ((' = 3.84</a:t>', ' = 6.63</a:t>'), (' = 5.99</a:t>', ' = 9.21</a:t>'),
                     (' = 4, X</a:t>', ' = 8, X</a:t>'), (' = 9.49</a:t>', ' = 20.09</a:t>')):
        assert s.count(old) == 1, old; s = s.replace(old, new)
    assert s.count(' = 16, X</a:t>') == 2 and s.count('<a:t>= 26.3</a:t>') == 2
    s = s.replace(' = 16, X</a:t>', ' = 12, X</a:t>', 1).replace(' = 16, X</a:t>', ' = 32, X</a:t>', 1)
    s = s.replace('<a:t>= 26.3</a:t>', '<a:t> = 26.22</a:t>', 1).replace('<a:t>= 26.3</a:t>', '<a:t> = 53.49</a:t>', 1)
    s = s.replace('<a:t>2 </a:t>', '<a:t>2</a:t>')            # e)'s superscript carried a trailing space
    old = (' value of 4 would not lead to a significant effect if you have 2 df (e.g., 2 rows and 3 columns), '
           'but does lead to a significant effect when you have 4 df (e.g., 3 rows and 3 columns)!</a:t>')
    assert s.count(old) == 1
    s = s.replace(old, ' value of 8 would lead to a significant effect if you have 1 df (2 rows and 2 columns), '
                       'but not when you have 2 df (e.g., 2 rows and 3 columns)!</a:t>')
    assert s.count('Use Excel: = CHISQ.INV(0,95; 1)') == 1
    s = s.replace('Use Excel: = CHISQ.INV(0,95; 1)', 'Use Excel: = CHISQ.INV(0,99; 1)')
    d.put(d.sp(96), s)
    print('  slides 94, 96: 11.10 as in the 5th Global (P = 0.01; df 1, 2, 8, 12, 32)')


def item_props(d):
    joint = {(1, 1): '0.3625', (1, 2): '0.1375', (2, 1): '0.1625', (2, 2): '0.3375', (3, 1): '0.525', (3, 2): '0.475'}
    for n in (57, 59):
        _cells(d, n, 0, joint)
        # 0.1375 / 0.3375 wrap in the 0.87 in column; widen it as slide 21 does, Total column gives the room
        s = d.get(d.sp(n)); tb = re.findall(r'<a:tbl>.*?</a:tbl>', s, re.S)[0]
        assert tb.count('<a:gridCol w="793080"') == 1 and tb.count('<a:gridCol w="1584360"') == 1
        new = tb.replace('<a:gridCol w="793080"', '<a:gridCol w="901800"').replace('<a:gridCol w="1584360"', '<a:gridCol w="1475640"')
        d.put(d.sp(n), s.replace(tb, new))
    print('  slides 57, 59: Proportions table = slides 8 and 21 (third column 0.87 -> 0.99 in)')


def item_assump(d):
    _cells(d, 18, 0, {(1, 1): '29', (1, 2): '11', (1, 3): '40', (2, 1): '13', (2, 2): '27', (2, 3): '40',
                      (3, 1): '42', (3, 2): '38'})
    print('  slide 18: ticked table = the electroshock data (expected 21/19/21/19)')


def item_venn(d):
    d.rep(20, '<a:t>Depression</a:t>', '<a:t>Recovery</a:t>')
    print('  slide 20: Depression -> Recovery')


def item_stale98(d):
    for name in ('CustomShape 10', 'CustomShape 11'):
        s, blk = d.shape_block(98, name)
        d.put(d.sp(98), s.replace(blk, ''))
    print('  slide 98: empty box and stale pointer removed')


ITEMS = {'links': item_links, 'typo': item_typo, 'overview': item_overview, 'pointer': item_pointer,
         'sup': item_sup, 'fit': item_fit,
         'ex1110': item_ex1110, 'props': item_props, 'assump': item_assump, 'venn': item_venn, 'stale98': item_stale98}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = L18Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

"""L16 pre-teaching fixes (2026-11-01, lecture Thu 5 Nov), run one item at a time:

    python3 l16_round1.py <in.pptx> <out.pptx> <item> [<item> ...]

Mechanical (applied 2026-11-01, backup _backup/*_pre-round1_2026-11-01.pptx):
  typo        slide 34 'A 95% confidence intervals means' -> 'interval' (same slip as L12 slide 72)
  xbar        x-bar in plain text (U+0304, which PT Sans lacks) -> equation objects, slides 24, 30, 31, 46, 47
  subscripts  slide 79 (exercise 10.22 answer): literal 'mu_chelation' / 'H_0' -> real subscripts
  fit         filled boxes the render check found spilling: slides 3, 21, 46, 59

His call (prepared, not applied):
  tvalue      the example's t is -5.1 / 2.285 = -2.23 (slides 55-58 and the plots on 59 say so),
              but slide 54 says 't = -2.3' and slide 59 'T = -2.3' with '-2.3' in its three boxes
  pooled      slide 51 '10.3 is not part of exam' meant the 4th edition's 10.3 (pooled SD); in the
              5th, 10.3 is bootstrap/permutation and the pooled SD sits in 10.2 (Agresti p. 549)
  ci34        slide 34 'It is likely that the true difference is between these bounds!' reads as a
              probability statement about this one interval; Agresti's wording is '95% confident'

Works on the zip parts like l12_round1.py, whose Deck (validates before writing), x-bar
conversion and box fitting it reuses.
"""
import re, sys
from l12_round1 import Deck, item_xbar, _fit_box, _geom, _set_geom


class L16Deck(Deck):
    def add_media(self, name, data):      # l12_round1 names its equation fallbacks l12_eq_*
        return super().add_media(name.replace('l12_eq_', 'l16_eq_'), data)

    def shape_block(self, n, name, tag='p:sp'):
        """'name|text' picks the shape of that name holding text (slide 3 has two CustomShape 3s)."""
        name, _, text = name.partition('|')
        s = self.get(self.sp(n))
        m = [m for m in re.finditer(rf'<{tag}>.*?</{tag}>', s, re.S)
             if f'name="{name}"' in m.group(0)[:600] and text in m.group(0)]
        assert len(m) == 1, (n, name, text, len(m))
        return s, m[0].group(0)


def item_typo(d):
    d.rep(34, '<a:t>A 95% confidence intervals means', '<a:t>A 95% confidence interval means')
    print('  slide 34: intervals -> interval')


SUB = 'baseline="-25000" '

def item_subscripts(d):
    n = 79
    d.rep(n, '<a:t>μ_</a:t>', '<a:t>μ</a:t>', count=4)
    rpr = '<a:rPr lang="en-US" sz="1600" b="0" strike="noStrike" spc="-1" dirty="0"/>'
    rsub = rpr.replace('dirty="0"', SUB + 'dirty="0"')
    for word, tail in (('chelation', ' = '), ('chelation', ' ≠ ')):
        d.rep(n, f'<a:r>{rpr}<a:t>{word}{tail}</a:t></a:r>',
                 f'<a:r>{rsub}<a:t>{word}</a:t></a:r><a:r>{rpr}<a:t>{tail}</a:t></a:r>')
    d.rep(n, f'<a:r>{rpr}<a:t>placebo</a:t></a:r>', f'<a:r>{rsub}<a:t>placebo</a:t></a:r>', count=2)
    d.rep(n, 'so we fail to reject 𝐻_0.', 'so we fail to reject H₀.')   # as 'H₀:' two lines up
    print('  slide 79: mu_chelation / mu_placebo / H_0 -> subscripts')


def _reshape(d, n, name, x=None, cx=None, cy=None, anchor=None):
    s, blk = d.shape_block(n, name)
    _, (x0, y0, cx0, cy0) = _geom(blk)
    new = _set_geom(blk, x0 if x is None else x, y0, cx0 if cx is None else cx, cy0 if cy is None else cy)
    if anchor:
        new = re.sub(r'(<a:bodyPr[^>]*?) anchor="\w+"', r'\1', new, count=1)
        new = new.replace('<a:bodyPr', f'<a:bodyPr anchor="{anchor}"', 1)
    d.put(d.sp(n), s.replace(blk, new))
    _, (x1, y1, cx1, cy1) = _geom(new)
    print(f'  slide {n} {name}: {cx0:.2f} x {cy0:.2f} at x {x0:.2f} -> {cx1:.2f} x {cy1:.2f} at x {x1:.2f}'
          + (f', text anchored {anchor}' if anchor else ''))


def item_fit(d):
    # render check 2026-11-01 (PowerPoint export): text out of its filled box by 0.10-0.20 in
    _fit_box(d, 3, 'CustomShape 3|How do we draw', 'center')
    # 21: the H0 line wraps out of its box; both boxes wider, so each reads on one line
    _reshape(d, 21, 'CustomShape 4', cx=6.60)
    _reshape(d, 21, 'CustomShape 5', cx=6.60)
    # 46: rotated sticky note, top-anchored text pokes out of the band's upper edge
    _reshape(d, 46, 'CustomShape 7', cy=1.00, anchor='ctr')
    # 59: growing down would cover the slide number; widen (right edge to 13.28, 0.5 in clear of yellow)
    _reshape(d, 59, 'CustomShape 37', x=8.80, cx=4.48)


def item_tvalue(d):
    d.rep(54, '<a:t> = -2.3</a:t>', '<a:t> = -2.23</a:t>')
    d.rep(59, '<a:t>T = -2.3</a:t>', '<a:t>t = -2.23</a:t>')
    d.rep(59, 'would provide more evidence than -2.3</a:t>', 'would provide more evidence than -2.23</a:t>')
    d.rep(59, '<a:t>than -2.3</a:t>', '<a:t>than -2.23</a:t>')
    d.rep(59, '<a:t>evidence  than -2.3, </a:t>', '<a:t>evidence than -2.23, </a:t>')
    print('  slides 54, 59: t = -2.3 -> -2.23')


def item_pooled(d):
    d.rep(51, '<a:t>10.3 is not part of exam</a:t>',
              '<a:t>Pooled standard deviation (Agresti, p. 549) and 10.3 are not part of exam</a:t>')
    print('  slide 51: pooled SD (p. 549) named next to 10.3')


def item_ci34(d):
    d.rep(34, '<a:t>It is likely that the true difference is between these bounds!</a:t>',
              '<a:t>We can be 95% confident that the true difference is between these bounds</a:t>')
    print('  slide 34: likely -> 95% confident')


ITEMS = {'typo': item_typo, 'xbar': item_xbar, 'subscripts': item_subscripts, 'fit': item_fit,
         'tvalue': item_tvalue, 'pooled': item_pooled, 'ci34': item_ci34}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = L16Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

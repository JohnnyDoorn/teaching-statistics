"""Excel commands in the formula-sheet format (2026-10-19), lectures 16, 18 and 20:

    python3 excel_syntax.py <deck.pptx> <out.pptx>

Formula-sheet format (Formula sheets/2026/): leading '= ', decimal comma inside the
command, '; ' between arguments, '1 - ' with spaces. The value the command returns
stays in slide notation with a decimal point: '= T.INV(0,975; 58) = 2.0017'.
L10 and L12 were converted earlier (l10_round*.py, l12_round1.py item excel).

L20 also gets the Kruskal-Wallis typo fixed: slide 47 computes H = 6.8158 (checked
against the slide 42 ranks), slides 50-51 said 6.1858, and the p-value came from the
typo (0.0454; the right one is 0.0331). The decision (p < 0.05) does not change.
The typo came from RMS_FiguresLecture20_Nonpara.R (fixed there), so the slide 51 plot
is redrawn from that script's block: SLIDE51_PNG.
Every replacement asserts its hit count; Deck.save validates the package.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from l12_round1 import Deck

SLIDE51_PNG = '/Users/johnny/surfdrive/Teaching/RMS/FiguresLectures/slide51_chisq_pval.png'  # RMS_FiguresLecture20_Nonpara.R, samplingDistChiSqTwoDfPVal block

# the 'Excel commands' overview slide, same lines as L12 slide 80
OVERVIEW = [('= 2.3 + 3 - 7 * SQRT(10) / 4^2', '= 2,3 + 3 - 7 * SQRT(10) / 4^2'),
            ('= NORM.DIST(2, 0, 1, TRUE)', '= NORM.DIST(2; 0; 1; TRUE)'),
            ('= T.DIST(2, 14, TRUE)', '= T.DIST(2; 14; TRUE)'),
            ('= CHISQ.DIST(8, 1, TRUE)', '= CHISQ.DIST(8; 1; TRUE)'),
            ('= NORM.INV(0.025, 0, 1)', '= NORM.INV(0,025; 0; 1)'),
            ('= T.INV(0.025, 20)', '= T.INV(0,025; 20)'),
            ('= CHISQ.INV(0.95, 1)', '= CHISQ.INV(0,95; 1)')]

EDITS = {
    'RMS2627_16_ComparingTwoGroups': [
        (30, '=T.INV(0.975,58)', '= T.INV(0,975; 58)'),
        (30, ': 2.001717484', ' = 2.0017'),
        (56, '= 2*T.DIST(-2.23; 58; TRUE)', '= 2 * T.DIST(-2,23; 58; TRUE)'),
        (57, '= T.DIST(-2.23; 58; TRUE)', '= T.DIST(-2,23; 58; TRUE)'),
        (58, '= 1-T.DIST(-2.23; 58; TRUE)', '= 1 - T.DIST(-2,23; 58; TRUE)'),
        *[(74, o, n) for o, n in OVERVIEW],
        (75, OVERVIEW[0][0], OVERVIEW[0][1]),
        # the math '=' doubles as the command's '=', as in L10: P(z<1.7045) = NORM.DIST(...)
        (81, 'P-value -   T.DIST(-0.8202, 58, TRUE) * 2 =  0.415',
             'P-value = T.DIST(-0,8202; 58; TRUE) * 2 = 0.415'),
    ],
    'RMS2627_18_CategoricalAssociation': [
        (45, 'MS Excel:  ', 'MS Excel: '), (45, '= CHISQ.INV(0.95, 1)', '= CHISQ.INV(0,95; 1)'),
        *[e for n in (47, 48, 49, 50, 51, 69, 70)
          for e in ((n, 'MS Excel:  ', 'MS Excel: '),
                    (n, '= 1-CHISQ.DIST(12.832, 1,TRUE)', '= 1 - CHISQ.DIST(12,832; 1; TRUE)'))],
        *[e for n in (67, 68)
          for e in ((n, 'MS Excel:  ', 'MS Excel: '),
                    (n, '= NORM.DIST(-3.5822, 0 , 1, TRUE)', '= NORM.DIST(-3,5822; 0; 1; TRUE)'))],
        (89, '= 1-CHISQ.DIST(8.39,4,TRUE)', '= 1 - CHISQ.DIST(8,39; 4; TRUE)'),
        *[(93, o, n) for o, n in OVERVIEW],
        (96, 'Use Excel: CHISQ.INV(0.95, 1)', 'Use Excel: = CHISQ.INV(0,95; 1)'),
    ],
    'RMS2627_20_NonparametricTests': [
        (41, '=SORT(A1:A18)', '= SORT(A1:A18)'),
        (50, 'CHISQ.INV(0.95, 2) ', '= CHISQ.INV(0,95; 2) '),
        (50, '=6.1858, p&lt;0.05', '=6.8158, p&lt;0.05'),
        (51, '= 1 - CHISQ.DIST(6.1858, 2, TRUE)', '= 1 - CHISQ.DIST(6,8158; 2; TRUE)'),
        (51, '= 0.0454', '= 0.0331'),
        (79, '1 - CHISQ.DIST(6.27; 2; TRUE) = 0.0435', '= 1 - CHISQ.DIST(6,27; 2; TRUE) = 0.0435'),
        (83, 'Using Excel: 1 - NORM.DIST(', 'Using Excel: = 1 - NORM.DIST('),
        (83, '<a:t>0.8165</a:t></a:r><a:r>', '<a:t>0,8165</a:t></a:r><a:r>'),
        (83, ', 0, 1, TRUE)', '; 0; 1; TRUE)'),
    ],
}


def main(src, out):
    stem = os.path.splitext(os.path.basename(src))[0]
    d = Deck(src)
    for n, old, new in EDITS[stem]:
        if old.startswith('<a:t>'):
            d.rep(n, old, new)
        else:
            d.rep(n, f'<a:t>{old}</a:t>', f'<a:t>{new}</a:t>')
    print(f'{stem}: {len(EDITS[stem])} replacements')
    if stem == 'RMS2627_20_NonparametricTests':
        s, pic = d.shape_block(51, 'Picture 529', 'p:pic')
        assert 'image34.wmf' in d.get(d.rp(51))
        rid = d.add_rel(51, d.add_media('l20_slide51_chisq_pval.png', open(SLIDE51_PNG, 'rb').read()))
        d.put(d.sp(51), s.replace(pic, re.sub(r'r:embed="rId\d+"', f'r:embed="{rid}"', pic, count=1)))
        d.drop_unused_rels(51)
        d.sweep_media()
    d.save(out)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])

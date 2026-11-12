"""L18 lecture-day changes (2026-11-12, lecture Thu 12 Nov), run one item at a time:

    python3 l18_round3.py <in.pptx> <out.pptx> <item> [<item> ...]

His asks of 11-12 morning. Each item finds its slide by content, so the order does not matter.

  hide      slide 14 (John Cena over slide 13's text) hidden: a joke to surprise them in class, so not in
            the PDF. Hidden slides keep their number, so the printed numbers stay as they were
  alpha     slide 'Interim exam 3': alpha is given 'when relevant' instead of 'always'
  link      slide 77 (snack demo): a box mirroring 'How do you feel about these snacks?' that links to the
            results page rms/snack-test/analyze-snacks.R writes (datasets/2026/snack-test-2026.html). It
            404s until he pushes the page after class
  twosided  new slide after 70 ('Note that the z-test can be one-sided!'), his chain: X2 is always
            two-sided -> only the right tail is extreme -> a significant X2 only gives a two-sided
            conclusion (an association, not a 'positive' or 'negative' one). The z line is mine (his
            slides' numbers, 3.5822^2 = 12.832); the picture is slide 51's (image23.png)

The new slide goes after slide 70, so slides 1-70 keep their numbers (WA11's feedback cites L18 slides
9-53, slide 80 points to slide 32). Slides 71-101 become 72-102.
"""
import re, sys
from l18_round1 import L18Deck
from l12_round1 import NS_R
from l18_round2 import bullet_slide, insert_slide

RESULTS_URL = 'https://johnnydoorn.github.io/teaching-statistics/datasets/2026/snack-test-2026.html'


def _text(d, n):
    return ' '.join(re.findall(r'<a:t>([^<]*)</a:t>', d.get(d.sp(n))))


def _find(d, needle):
    hits = [n for n in range(1, len(d.slide_files()) + 1) if needle in _text(d, n)]
    assert len(hits) == 1, (needle, hits)
    return hits[0]


def _sup2(xml):
    """'X²' inside a run -> italic X + italic superscript 2, as slides 53 and 56 write it."""
    def split(m):
        rpr, out = m.group(1), []
        it = rpr.replace(' b="0"', ' b="0" i="1"', 1)
        sup = it.replace(' i="1"', ' i="1" baseline="30000"', 1)
        for k, part in enumerate(m.group(2).split('X²')):
            if k:
                out.append(f'<a:r>{it}<a:t>X</a:t></a:r><a:r>{sup}<a:t>2</a:t></a:r>')
            if part:
                out.append(f'<a:r>{rpr}<a:t>{part}</a:t></a:r>')
        return ''.join(out)
    return re.sub(r'<a:r>(<a:rPr [^>]*>(?:(?!</a:rPr>).)*</a:rPr>)<a:t>([^<]*X²[^<]*)</a:t></a:r>', split, xml, flags=re.S)


def item_hide(d):
    hits = [n for n in range(1, len(d.slide_files()) + 1) if 'media/image5.png"' in d.get(d.rp(n))]
    assert hits == [14], hits
    assert _text(d, 14).startswith('What can we now express?'), _text(d, 14)[:60]
    s = d.get(d.sp(14))
    assert 'show="0"' not in s[:600], 'already hidden'
    d.put(d.sp(14), re.sub(r'<p:sld( [^>]*)>', r'<p:sld\1 show="0">', s, count=1))
    print('  slide 14 hidden')


def item_alpha(d):
    n = _find(d, 'Interim exam 3')
    d.rep(n, '<a:t>On the exam, we will always give you the significance level α</a:t>',
          '<a:t>On the exam, we will give you the significance level α when relevant</a:t>')
    print(f'  slide {n}: alpha "always" -> "when relevant"')


def item_link(d):
    n = _find(d, 'How do you feel about these snacks?')
    s, blk = d.shape_block(n, 'CustomShape 50')   # the question box this one mirrors
    assert '<a:off x="685800" y="1828800"/><a:ext cx="2742120" cy="1234440"/>' in blk
    rels = d.get(d.rp(n))
    rid = 'rId%d' % (max(int(i) for i in re.findall(r'Id="rId(\d+)"', rels)) + 1)
    d.put(d.rp(n), rels.replace('</Relationships>', f'<Relationship Id="{rid}" Type="{NS_R}/hyperlink" '
                                f'Target="{RESULTS_URL}" TargetMode="External"/></Relationships>'))
    nid = max(int(i) for i in re.findall(r'<p:cNvPr id="(\d+)"', s)) + 1
    style = re.search(r'<p:style>.*?</p:style>', blk, re.S).group(0)
    rpr = '<a:rPr lang="en-US" sz="2400" b="0" strike="noStrike" spc="-1"'
    ppr = '<a:pPr algn="ctr"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:buNone/></a:pPr>'
    # mirrored on the right (x = 12192000 - 685800 - 2742120); bottom stays above the bitterballen picture
    box = (f'<p:sp><p:nvSpPr><p:cNvPr id="{nid}" name="Results link"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr>'
           '<p:spPr><a:xfrm><a:off x="8764080" y="1783080"/><a:ext cx="2742120" cy="868680"/></a:xfrm>'
           '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
           '<a:solidFill><a:srgbClr val="B4C7DC"><a:alpha val="19000"/></a:srgbClr></a:solidFill>'
           '<a:ln w="25560"><a:solidFill><a:schemeClr val="accent2"/></a:solidFill><a:round/></a:ln></p:spPr>'
           f'{style}<p:txBody><a:bodyPr lIns="90000" tIns="45000" rIns="90000" bIns="45000" anchor="ctr"><a:noAutofit/></a:bodyPr>'
           f'<a:lstStyle/><a:p>{ppr}<a:r>{rpr}><a:hlinkClick r:id="{rid}"/></a:rPr><a:t>Our results</a:t></a:r></a:p>'
           f'<a:p>{ppr}<a:r>{rpr}><a:solidFill><a:srgbClr val="000000"/></a:solidFill></a:rPr><a:t>(online after class)</a:t></a:r>'
           f'<a:endParaRPr lang="en-US" sz="2400" b="0" strike="noStrike" spc="-1"/></a:p></p:txBody></p:sp>')
    d.put(d.sp(n), s.replace('</p:spTree>', box + '</p:spTree>', 1))
    print(f'  slide {n}: results link ({rid}) -> {RESULTS_URL}')


def item_twosided(d):
    after = _find(d, 'Note that the z-test can be one-sided!')
    assert after == 70, after
    xml = bullet_slide('X² is always two-sided', [
        ('Only one tail (the right one) is extreme', 0),
        ('Both z = -3.5822 and z = 3.5822 give X² = 12.832', 1),
        ('', None),
        ('Based on a significant X², we can only make a two-sided conclusion', 0),
        ('There is an association, not a “positive” or “negative” one', 1, True),
    ])
    # text on the left, slide 51's chi-squared picture (right tail 0.034%) on the right
    old = '<a:off x="838080" y="1825560"/><a:ext cx="10508040" cy="4343760"/>'
    assert xml.count(old) == 1
    xml = xml.replace(old, '<a:off x="838080" y="1825560"/><a:ext cx="5943600" cy="4343760"/>')
    pic = ('<p:pic><p:nvPicPr><p:cNvPr id="5" name="Picture 5"/><p:cNvPicPr><a:picLocks noChangeAspect="1"/></p:cNvPicPr>'
           '<p:nvPr/></p:nvPicPr><p:blipFill><a:blip r:embed="rId2"/><a:stretch><a:fillRect/></a:stretch></p:blipFill>'
           '<p:spPr><a:xfrm><a:off x="6858000" y="1825560"/><a:ext cx="5029200" cy="3352800"/></a:xfrm>'
           '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr></p:pic>')
    anchor = '<p:sp><p:nvSpPr><p:cNvPr id="4" name="Slide Number Placeholder"/>'
    assert xml.count(anchor) == 1
    xml = _sup2(xml.replace(anchor, pic + anchor))
    assert 'X²' not in xml
    insert_slide(d, after, xml)
    assert 'image23.png' in d.get(d.rp(51))
    assert d.add_rel(after + 1, 'image23.png') == 'rId2'


ITEMS = {'hide': item_hide, 'alpha': item_alpha, 'link': item_link, 'twosided': item_twosided}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = L18Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

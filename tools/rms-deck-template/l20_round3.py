"""L20 round 3 (2026-11-25/26, lecture Thu 26 Nov): the opening 'basic idea of hypothesis testing' (slide 4),
aligned with his L16 deck and SSR (alpha before the data).

    python3 l20_round3.py <in.pptx> <out.pptx> <item> [<item> ...]

His ask (2026-11-25): "how likely is an observed result" is more accurate than "how extreme is a sample", and
include alpha as your choice for deciding what is "unlikely enough"; check the opening slides against SSR (he
already has "step 0" for alpha in earlier lectures). 2026-11-26: extend the alpha bullet with "unlikely enough to
warrant rejecting the null hypothesis, or something more smooth" (-> "to reject the null hypothesis", as in his
SSR L3 slides). A step-0 box on slide 15 was tried and dropped (his call: slide 15 is fine as is). Then: "sampling
distribution can fit more closely with the alpha point - it describes which values of the test statistic are more or
less likely to occur under H0".

  question    slide 4: the central question becomes his L16 slide 54 wording, "How likely is our observed result,
              if the null hypothesis were true?" (same run, same orange bold italic)
  sampdist    slide 4: "Sampling distribution: Describes which values of the test statistic are more or less likely
              to occur, if the null hypothesis is true" (his sentence; "under H0" written as the test-statistic
              bullet above it has it). Sets up slide 5 (likely / not unlikely / very unlikely values of t and chi2)
  alpha       slide 4: new last bullet "Alpha (α) is your choice for deciding what is “unlikely enough” to reject
              the null hypothesis" ('Alpha' in the slide's key-term style), revealed on a 4th click like the
              bullets above it. Room: space-after 14.17 pt -> 0 on the five bullets (space-before unchanged),
              instead of shrinking; the box stretched from 6.75 to 6.90 in (slide number starts at 6.95 in),
              because the two-line bullet ends at 6.76 in (PowerPoint export); top-anchored, so nothing moves
  raincloud   slide 2 (the 7 Sept placeholder "Does this group really differ?", hidden): his idea (2026-11-26),
              a clean raincloud plot of the slide-69 outlier data (Condition, Score) in the opening; the
              placeholder text box goes, the plot (FiguresLectures/RMS_FiguresLecture20_raincloud.R) takes
              the body area, and the slide is unhidden. Apply to the live deck, which already has the rest.
"""
import os, re, sys
from l16_round1 import L16Deck

EMU = 914400
BODY = 'CustomShape 2'   # slide 4's text box (id 331)
FIG = os.path.expanduser('~/surfdrive/Teaching/RMS/FiguresLectures/l20_raincloud_outlier.png')
RAIN = ('<p:pic><p:nvPicPr><p:cNvPr id="{id}" name="Raincloud outlier" descr="Raincloud plot of Score by Condition '
        '(Control, Treatment); one Treatment score of 30 lies far above the rest"/><p:cNvPicPr><a:picLocks '
        'noChangeAspect="1"/></p:cNvPicPr><p:nvPr/></p:nvPicPr><p:blipFill><a:blip r:embed="{rid}"/><a:stretch>'
        '<a:fillRect/></a:stretch></p:blipFill><p:spPr><a:xfrm><a:off x="{x}" y="{y}"/><a:ext cx="{cx}" cy="{cy}"/>'
        '</a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr></p:pic>')


def _body(d, n):
    s = d.get(d.sp(n))
    blk = [m.group(0) for m in re.finditer(r'<p:sp>.*?</p:sp>', s, flags=re.S) if f'name="{BODY}"' in m.group(0)]
    if len(blk) != 1:
        sys.exit(f'slide {n}: {BODY} not found once')
    return s, blk[0]


def _swap(d, what, old, new):
    """Replace one whole run text in slide 4's text box; formatting untouched."""
    s, blk = _body(d, 4)
    if blk.count(f'<a:t>{old}</a:t>') != 1:
        sys.exit(f'slide 4: {what} not found once')
    d.put(d.sp(4), s.replace(blk, blk.replace(f'<a:t>{old}</a:t>', f'<a:t>{new}</a:t>')))
    print(f'  slide 4: {what} -> {new!r}')


def item_question(d):
    _swap(d, 'question', 'How extreme is a sample if the null hypothesis is true?',
          'How likely is our observed result, if the null hypothesis were true?')


def item_sampdist(d):
    _swap(d, 'sampling distribution',
          'Sampling distribution: A probability distribution that specifies probabilities for the possible values '
          'the statistic can take',
          'Sampling distribution: Describes which values of the test statistic are more or less likely to occur, '
          'if the null hypothesis is true')


def item_alpha(d):
    s, blk = _body(d, 4)
    paras = re.findall(r'<a:p>.*?</a:p>', blk, flags=re.S)
    if len(paras) != 4 or 'Answer requires a ' not in paras[1]:
        sys.exit('slide 4: expected the four bullets')
    p1 = paras[1]
    ppr = re.search(r'<a:pPr .*?</a:pPr>', p1, flags=re.S).group(0)
    plain = re.search(r'<a:rPr lang="en-US" sz="2800" b="0"[^>]*>.*?</a:rPr>', p1, flags=re.S).group(0)
    key = re.search(r'<a:rPr lang="en-US" sz="2800" b="1" i="1"[^>]*>.*?</a:rPr>', p1, flags=re.S).group(0)
    end = re.search(r'<a:endParaRPr[^>]*/>', p1).group(0)
    new_p = (f'<a:p>{ppr}<a:r>{key}<a:t>Alpha</a:t></a:r>'
             f'<a:r>{plain}<a:t> (α) is your choice for deciding what is “unlikely enough” to reject the null '
             f'hypothesis</a:t></a:r>{end}</a:p>')
    nb = blk.replace(paras[-1], paras[-1] + new_p)
    n_aft = nb.count('<a:spcAft><a:spcPts val="1417"/></a:spcAft>')
    if n_aft != 5:
        sys.exit(f'slide 4: expected 5 space-after values of 14.17 pt, found {n_aft}')
    nb = nb.replace('<a:spcAft><a:spcPts val="1417"/></a:spcAft>', '<a:spcAft><a:spcPts val="0"/></a:spcAft>')
    off = re.findall(r'<a:off x="\d+" y="(\d+)"/>', nb)
    ext = re.findall(r'<a:ext cx="(\d+)" cy="(\d+)"/>', nb)
    if len(off) != 1 or len(ext) != 1:
        sys.exit('slide 4: expected one xfrm on the text box')
    cy = round(6.90 * 914400) - int(off[0])
    nb = nb.replace(f'<a:ext cx="{ext[0][0]}" cy="{ext[0][1]}"/>', f'<a:ext cx="{ext[0][0]}" cy="{cy}"/>')
    s = s.replace(blk, nb)
    # 4th click: copy the click that reveals paragraph 3 and point it at paragraph 4
    spid = re.search(r'<p:cNvPr id="(\d+)" name="' + BODY + '"', blk).group(1)
    clicks = re.findall(r'<p:par><p:cTn id="\d+" fill="hold"><p:stCondLst><p:cond delay="indefinite"/></p:stCondLst>'
                        r'.*?</p:childTnLst></p:cTn></p:par>(?=<p:par><p:cTn id="\d+" fill="hold"><p:stCondLst><p:cond delay="indefinite"/>|</p:childTnLst></p:cTn><p:prevCondLst>)',
                        s, flags=re.S)
    last = [c for c in clicks if f'<p:spTgt spid="{spid}"><p:txEl><p:pRg st="3" end="3"/>' in c]
    if len(clicks) != 3 or len(last) != 1:
        sys.exit(f'slide 4: expected three paragraph clicks, found {len(clicks)}')
    top = max(int(i) for i in re.findall(r'<p:cTn id="(\d+)"', s))
    ids = sorted({int(i) for i in re.findall(r'<p:cTn id="(\d+)"', last[0])})
    click4 = last[0].replace('<p:pRg st="3" end="3"/>', '<p:pRg st="4" end="4"/>')
    for k, old in enumerate(ids, 1):
        click4 = click4.replace(f'<p:cTn id="{old}"', f'<p:cTn id="@{top + k}"')
    click4 = click4.replace('id="@', 'id="')
    s = s.replace(last[0], last[0] + click4)
    d.put(d.sp(4), s)
    print(f'  slide 4: alpha bullet (click 4, cTn {top + 1}-{top + len(ids)}); space-after 14.17 -> 0 pt on 5 bullets; '
          f'box height {int(ext[0][1]) / 914400:.2f} -> {cy / 914400:.2f} in')


def item_raincloud(d):
    from PIL import Image
    s = d.get(d.sp(2))
    if '<a:t>Does this group really differ?</a:t>' not in s:
        sys.exit('slide 2: placeholder title not found')
    body = [m.group(0) for m in re.finditer(r'<p:sp>.*?</p:sp>', s, flags=re.S) if '[ placeholder:' in m.group(0)]
    if len(body) != 1:
        sys.exit('slide 2: placeholder text box not found once')
    w, h = Image.open(FIG).size
    rid = d.add_rel(2, d.add_media(os.path.basename(FIG), open(FIG, 'rb').read()))
    # the layout's body area starts at 1.95 in; 4.9 in tall ends at 6.85 in, above the slide number (6.95 in)
    cy = round(4.9 * EMU); cx = round(cy * w / h); x = (12192000 - cx) // 2; y = round(1.95 * EMU)
    new_id = max(int(i) for i in re.findall(r'<p:cNvPr id="(\d+)"', s)) + 1
    s = s.replace(body[0], RAIN.format(id=new_id, rid=rid, x=x, y=y, cx=cx, cy=cy))
    sld = re.search(r'<p:sld\b[^>]*>', s).group(0)
    s = s.replace(sld, sld.replace(' show="0"', ''), 1)
    d.put(d.sp(2), s)
    print(f'  slide 2: placeholder text -> raincloud plot ({cx / EMU:.2f} x {cy / EMU:.2f} in at {x / EMU:.2f}, '
          f'{y / EMU:.2f}); slide unhidden')


ITEMS = {'question': item_question, 'sampdist': item_sampdist, 'alpha': item_alpha, 'raincloud': item_raincloud}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = L16Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

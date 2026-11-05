"""L16 round 2 (2026-11-05, lecture day): the consistency check of that morning
(rms/_lecture-notes-2026.md, L16), applied on his go ("yes, go with the changes"):

    python3 l16_round2.py <in.pptx> <out.pptx> <item> [<item> ...]
    python3 l16_round2.py <in.pptx> <out.pptx> fallbacks <render.pdf>   # pass 2, after a PowerPoint export

  links     slide 60: flowchart and extra-text links gave 404 (Pages is case-sensitive, RMS/ -> rms/;
            extra-texts went kebab-case in Aug 2026, OneSidedVsTwoSided -> one-sided-vs-two-sided)
  typo      slide 24 'How do compute' -> 'How do we compute' (and its 11-01 fallback picture)
  dv        slide 68 'Different IV (categorical)' -> 'Different DV (categorical)': L17 compares proportions
  sqrt      slide 52: the middle step 8.8²/30 + 8.9²/30 gets its square root (missing since 2025)
  sigma2    slide 47 (picture): second numerator σ1² -> σ2², the subscript 2 of s2² in the same picture
  margin    slides 28-29 (one shared picture): 'margin or error' -> 'margin of error', a Cambria Math 'f'
  examq     slides 69-72, the example exam question at 4 decimals (his rounding rule), from the workbook
            on Canvas (35 per group): means 11.0571 / 8.3714, sds 2.9199 / 2.6578, se 0.6674,
            t = 2.6857 / 0.6674 = 4.0241, option b) 4.02. Slide 70: digits added in its picture.
            Slide 71: s1 / s2 in the picture; the se line and the t line become equation objects
            (their fractions and root change width). Slide 72's note loses its now-false first clause.
  fallbacks pass 2: the equation fallback pictures of slides 52 and 71, cropped from PowerPoint's render

Picture edits follow l12_solution_digits.py: ink is the alpha channel, new glyphs are set in the
picture's own font at a size fitted to its existing ink. Works on the zip parts like l16_round1.py.
"""
import io, os, re, subprocess, sys, tempfile
from PIL import Image, ImageChops, ImageDraw, ImageFont, ImageStat
from l16_round1 import L16Deck
from l12_round1 import NS_MC, NS_A14, NS_M, _fallback_png, _fallback_sp, _plain_paras

EMU = 914400
CAMBRIA_MATH = ('/Applications/Microsoft PowerPoint.app/Contents/Resources/DFonts/Cambria.ttc', 1)
CALIBRI = ('/Applications/Microsoft PowerPoint.app/Contents/Resources/DFonts/Calibri.ttf', 0)   # slide 71's
                                     # s1/s2 lines predate the Sept font swap (fit 0.03 vs PT Sans 0.10)
PAGES = 'https://johnnydoorn.github.io/teaching-statistics/'


class Deck2(L16Deck):
    def media(self, n, old):
        assert f'Target="../media/{old}"' in self.get(self.rp(n)), (n, old)
        return self.parts['ppt/media/' + old]

    def swap_media(self, slides, old, name, data):
        """Point these slides at a new picture; sweep_media drops `old` once nothing uses it."""
        self.add_media(name, data)
        for n in slides:
            r = self.get(self.rp(n)); k = r.count(f'Target="../media/{old}"')
            assert k == 1, (n, old, k)
            self.put(self.rp(n), r.replace(f'Target="../media/{old}"', f'Target="../media/{name}"'))
        self.sweep_media()


# ---------------------------------------------------------------------------------- picture ink
class Ink:
    """Alpha-channel editing of a black-ink picture; every touched box is repainted black."""
    def __init__(self, data):
        self.im = Image.open(io.BytesIO(data)).convert('RGBA')
        self.A = self.im.split()[3]; self.touched = []

    def clear(self, box):
        self.A.paste(0, box); self.touched.append(box)

    def lay(self, part, pos):
        bb = part.getbbox()                          # only the ink: the repaint must not reach colour
        part, pos = part.crop(bb), (pos[0] + bb[0], pos[1] + bb[1])
        box = (pos[0], pos[1], pos[0] + part.size[0], pos[1] + part.size[1])
        self.A.paste(ImageChops.lighter(self.A.crop(box), part), pos); self.touched.append(box)

    def png(self):
        r, g, b, _ = self.im.split(); rgb = Image.merge('RGB', (r, g, b))
        for box in self.touched: rgb.paste((0, 0, 0), box)
        out = rgb.convert('RGBA'); out.putalpha(self.A)
        buf = io.BytesIO(); out.save(buf, 'PNG'); return buf.getvalue()


def render(spec, px, s, pad=40):
    f = ImageFont.truetype(spec[0], px, index=spec[1])
    m = Image.new('L', (int(f.getlength(s)) + 2 * pad, int(4 * px) + 2 * pad), 0)
    ImageDraw.Draw(m).text((pad, pad), s, font=f, fill=255)
    return m, f, pad


def fit(A, box, spec, s):
    """Size (px) and pen origin (picture coordinates) at which the font's s best covers the ink in box."""
    W = box[2] - box[0]
    def inkw(p):
        bb = render(spec, p, s)[0].getbbox(); return bb[2] - bb[0]
    p0 = min((abs(inkw(p / 10) - W), p / 10) for p in range(150, 900))[1]
    R = (box[0] - 4, box[1] - 4, box[2] + 4, box[3] + 4); tgt = A.crop(R); best = None
    for p in [p0 + k / 20 for k in range(-20, 21)]:
        m, f, pad = render(spec, p, s); bb = m.getbbox(); g = m.crop(bb)
        for ox in range(-3, 4):
            for oy in range(-3, 4):
                c = Image.new('L', tgt.size, 0); c.paste(g, (box[0] + ox - R[0], box[1] + oy - R[1]))
                dsum = ImageStat.Stat(ImageChops.difference(c, tgt)).sum[0]
                if best is None or dsum < best[0]:
                    best = (dsum, p, (box[0] + ox - bb[0] + pad, box[1] + oy - bb[1] + pad))
    dsum, p, origin = best
    return p, origin, dsum / (255.0 * (R[2] - R[0]) * (R[3] - R[1]))


def tail(spec, px, s, k):
    """Ink of s[k:] as the font sets the whole of s (the rendered s[:k] subtracted), and its pad."""
    m, f, pad = render(spec, px, s)
    pre = Image.new('L', m.size, 0); pre.paste(render(spec, px, s[:k])[0], (0, 0))
    return ImageChops.subtract(m, pre), pad


def extend(ink, spec, box, old, new, clear=()):
    """Turn `old` (its ink in box) into `new`: clear the glyph boxes in `clear`, then set new[k:]
    where the font puts it after the shared prefix new[:k]."""
    px, origin, miss = fit(ink.A, box, spec, old)
    k = len(os.path.commonprefix([old, new]))
    for b in clear: ink.clear(b)
    part, pad = tail(spec, px, new, k)
    ink.lay(part, (origin[0] - pad, origin[1] - pad))
    print(f'    {old} -> {new}: {px:.2f} px, mismatch {miss:.3f}')
    return px


# ---------------------------------------------------------------------------------- equations
def mrpr(sz):
    return (f'<a:rPr lang="en-US" sz="{sz}" b="0" i="1" smtClean="0"><a:latin typeface="Cambria Math" '
            'panose="02040503050406030204" pitchFamily="18" charset="0"/></a:rPr>')

class OM:
    """OMML as PowerPoint writes it here (slides 51-52): math-italic letters, Cambria Math rPr."""
    def __init__(s, sz): s.p = mrpr(sz)
    def r(s, t): return f'<m:r>{s.p}<m:t>{t}</m:t></m:r>'
    def f(s, a, b): return f'<m:f><m:fPr><m:ctrlPr>{s.p}</m:ctrlPr></m:fPr><m:num>{a}</m:num><m:den>{b}</m:den></m:f>'
    def sup(s, e, x): return f'<m:sSup><m:sSupPr><m:ctrlPr>{s.p}</m:ctrlPr></m:sSupPr><m:e>{e}</m:e><m:sup>{x}</m:sup></m:sSup>'
    def sqrt(s, e): return f'<m:rad><m:radPr><m:degHide m:val="on"/><m:ctrlPr>{s.p}</m:ctrlPr></m:radPr><m:deg/><m:e>{e}</m:e></m:rad>'
    def inline(s, *c): return f'<a14:m><m:oMath xmlns:m="{NS_M}">{"".join(c)}</m:oMath></a14:m>'
    def display(s, *c):                     # as slide 51: full-size fractions
        return (f'<a14:m><m:oMathPara xmlns:m="{NS_M}"><m:oMathParaPr><m:jc m:val="centerGroup"/></m:oMathParaPr>'
                f'<m:oMath xmlns:m="{NS_M}">{"".join(c)}</m:oMath></m:oMathPara></a14:m>')

SE, T = '\U0001D460\U0001D452', '\U0001D461'      # math-italic se, t

def eq_sp(sid, name, xfrm, paras, wrap='none'):
    x, y, cx, cy = xfrm
    return (f'<p:sp><p:nvSpPr><p:cNvPr id="{sid}" name="{name}"/><p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr>'
            f'<p:spPr><a:xfrm><a:off x="{x}" y="{y}"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm>'
            '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:noFill/></p:spPr>'
            f'<p:txBody><a:bodyPr wrap="{wrap}" lIns="0" tIns="0" rIns="0" bIns="0" anchor="ctr"><a:noAutofit/></a:bodyPr>'
            f'<a:lstStyle/>{"".join(paras)}</p:txBody></p:sp>')

def wrap_alt(d, n, choice, media_name, placeholder):
    rid = d.add_rel(n, d.add_media(media_name, placeholder))
    return (f'<mc:AlternateContent xmlns:mc="{NS_MC}" xmlns:a14="{NS_A14}"><mc:Choice Requires="a14">{choice}</mc:Choice>'
            f'<mc:Fallback>{_fallback_sp(choice, rid)}</mc:Fallback></mc:AlternateContent>')

def geom(block):
    g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/>\s*<a:ext cx="(\d+)" cy="(\d+)"/>', block)
    return tuple(int(v) for v in g.groups())

def placeholder(xfrm, text, pt):
    """Interim fallback (plain text); pass 2 replaces it with PowerPoint's own rendering."""
    return _fallback_png([v / EMU for v in xfrm], None, [(text, pt, False)])


# ---------------------------------------------------------------------------------- items
def item_links(d):
    rp = d.rp(60); r = d.get(rp)
    for old, new in (('RMS/TailDecisionTree.pdf', 'rms/TailDecisionTree.pdf'),
                     ('extra-texts/OneSidedVsTwoSided.html', 'extra-texts/one-sided-vs-two-sided.html')):
        assert r.count(f'Target="{PAGES}{old}"') == 1, old
        r = r.replace(f'Target="{PAGES}{old}"', f'Target="{PAGES}{new}"')
        print(f'  slide 60: {old} -> {new}')
    d.put(rp, r)


def item_typo(d):
    n = 24
    d.rep(n, '<a:t>How do compute the margin of error?</a:t>', '<a:t>How do we compute the margin of error?</a:t>')
    # the 11-01 xbar conversion drew this shape's fallback from its text: redraw it the same way
    x = d.get(d.sp(n))
    ch = re.search(r'<mc:Choice Requires="a14">(<p:sp>.*?</p:sp>)</mc:Choice>', x, re.S).group(1)
    assert 'How do we compute' in ch
    fb = re.search(r'<mc:Fallback>.*?r:embed="(rId\d+)".*?</mc:Fallback>', x, re.S).group(1)
    tgt = re.search(rf'Id="{fb}"[^>]*Target="\.\./media/([^"]+)"', d.get(d.rp(n))).group(1)
    assert tgt == 'l16_eq_s24_783.png', tgt
    fs = re.search(r'fontScale="(\d+)"', ch)
    fill = re.search(r'<p:spPr>(?:(?!<a:ln).)*?<a:solidFill><a:srgbClr val="(\w{6})"', ch, re.S)
    d.parts['ppt/media/' + tgt] = _fallback_png([v / EMU for v in geom(ch)], fill.group(1) if fill else None,
                                                _plain_paras(ch), int(fs.group(1)) / 1e5 if fs else 1.0)
    print('  slide 24: How do compute -> How do we compute (fallback redrawn)')


def item_dv(d):
    d.rep(68, '<a:t>Different IV (categorical)</a:t>', '<a:t>Different DV (categorical)</a:t>')
    print('  slide 68: Different IV (categorical) -> Different DV (categorical)')


def item_sqrt(d):
    n = 52; x = d.get(d.sp(n))
    ch = re.search(r'<mc:Choice Requires="a14">.*?</mc:Choice>', x, re.S).group(0)
    maths = re.findall(r'<a14:m>.*?</a14:m>', ch, re.S); assert len(maths) == 2
    radpr = re.search(r'<m:radPr>.*?</m:radPr>', maths[0], re.S).group(0)
    m = re.match(r'(<a14:m><m:oMath [^>]*>)(.*)(</m:oMath></a14:m>)$', maths[1], re.S)
    assert '<m:rad>' not in m.group(2) and '8.8' in m.group(2) and '8.9' in m.group(2)
    x = x.replace(maths[1], f'{m.group(1)}<m:rad>{radpr}<m:deg/><m:e>{m.group(2)}</m:e></m:rad>{m.group(3)}', 1)
    # fallback: interim plain text now, PowerPoint's own rendering in pass 2
    fb = re.search(r'<mc:Fallback[^>]*>.*?r:embed="(rId\d+)".*?</mc:Fallback>', x, re.S).group(1)
    rels = d.get(d.rp(n)); assert f'Id="{fb}" ' in rels and 'Target="../media/image34.png"' in rels
    d.put(d.sp(n), x)
    xfrm = geom(re.search(r'<mc:Choice.*?</mc:Choice>', x, re.S).group(0))
    d.add_media('l16_eq_s52_se.png', placeholder(xfrm, 'SE = √(S1²/n1 + S2²/n2) = √(8.8²/30 + 8.9²/30) = 2.285', 28))
    d.put(d.rp(n), rels.replace('Target="../media/image34.png"', 'Target="../media/l16_eq_s52_se.png"'))
    d.sweep_media()
    print('  slide 52: square root over 8.8²/30 + 8.9²/30')


def item_sigma2(d):
    n, old = 47, 'image24.png'
    ink = Ink(d.media(n, old))
    # blobs (thr 40): sigma 450-466, its subscript '1' 466-476 x 238-252; s2²'s subscript '2' 978-988 same rows
    two = ink.A.crop((977, 236, 990, 253))
    ink.clear((466, 236, 478, 253))
    ink.lay(two, (465, 236))
    d.swap_media([n], old, 'l16_sd47_sigma2.png', ink.png())
    print('  slide 47: second numerator sigma1² -> sigma2²')


def item_margin(d):
    old = 'image20.png'
    ink = Ink(d.media(28, old))
    # line 1 blobs: 'margin' 338-511, 'or' = o 530-556 + r 561-582 (rows 28-56), 'error' from 598
    px, origin, miss = fit(ink.A, (530, 28, 582, 57), CAMBRIA_MATH, 'or')
    ink.clear((559, 26, 590, 58))
    part, pad = tail(CAMBRIA_MATH, px, 'of', 1)
    ink.lay(part, (origin[0] - pad, origin[1] - pad))
    d.swap_media([28, 29], old, 'l16_margin_of_error.png', ink.png())
    print(f'  slides 28-29: margin or error -> margin of error ({px:.2f} px, mismatch {miss:.3f})')


def item_examq(d):
    # slides 69 and 72: option b, and the note whose first clause stops being true
    d.rep(69, '<a:t>4.03</a:t>', '<a:t>4.02</a:t>')
    d.rep(72, '<a:t>4.03</a:t>', '<a:t>4.02</a:t>')
    d.rep(72, 'Note that this answer is slightly different from the Excel answer on Canvas (which is 4.024) '
              'due to rounding – on the exam we advise', 'On the exam we advise')
    print('  slides 69, 72: b) 4.03 -> 4.02; slide 72 note: first clause dropped')

    # slide 70, picture: the two means
    ink = Ink(d.media(70, 'image39.png'))
    extend(ink, CAMBRIA_MATH, (405, 215, 537, 255), '11.06', '11.0571', clear=[(509, 213, 540, 257)])
    extend(ink, CAMBRIA_MATH, (338, 299, 442, 339), '8.37', '8.3714')
    d.swap_media([70], 'image39.png', 'l16_examq_s70.png', ink.png())

    # slide 71, picture: s1 and s2; its se line goes (it comes back as an equation)
    n = 71
    ink = Ink(d.media(n, 'image40.png'))
    extend(ink, CALIBRI, (171, 168, 267, 206), '2.92', '2.9199', clear=[(242, 166, 270, 208)])
    extend(ink, CALIBRI, (171, 251, 268, 289), '2.66', '2.6578', clear=[(242, 249, 271, 291)])
    px_se, _, _ = fit(ink.A, (627, 547, 729, 587), CAMBRIA_MATH, '0.67')
    ink.clear((0, 488, ink.A.size[0], 632))
    d.swap_media([n], 'image40.png', 'l16_examq_s71.png', ink.png())

    x = d.get(d.sp(n))
    _, pic = d.shape_block(n, 'CustomShape 3')
    bx, by, bcx, bcy = geom(pic)
    sx, sy = 1743 / (bcx / EMU), 714 / (bcy / EMU)           # picture px per inch
    pt_se = px_se / sx * 72                                  # 26.8: the picture is set a little under 28 pt
    assert 26 < pt_se < 29, pt_se
    sz_se = int(round(pt_se) * 100)                          # match the picture bullets above it
    print(f'    se line: {pt_se:.2f} pt in the picture -> sz {sz_se}')
    o = OM(sz_se)
    se = o.inline(o.r(SE + '='),
                  o.sqrt(o.f(o.sup(o.r('2.9199'), o.r('2')), o.r('35')) + o.r('+') +
                         o.f(o.sup(o.r('2.6578'), o.r('2')), o.r('35'))),
                  o.r('=0.6674'))
    # picture geometry of the old line: bullet ink at x 35, 'se' at x 73, rows 494-624
    marL = round((73 / sx - 0.02) * EMU); bullet = round((35 / sx - 0.02) * EMU)
    cy = round(1.0 * EMU); y = by + round(559 / sy * EMU) - cy // 2
    para = (f'<a:p><a:pPr marL="{marL}" indent="{bullet - marL}"><a:buClr><a:srgbClr val="000000"/></a:buClr>'
            f'<a:buFont typeface="Arial"/><a:buChar char="•"/></a:pPr>{se}'
            f'<a:endParaRPr lang="en-US" sz="{sz_se}" b="0" strike="noStrike" spc="-1" dirty="0"/></a:p>')
    sid = max(int(i) for i in re.findall(r'<p:cNvPr id="(\d+)"', x)) + 1
    xfrm = (bx, y, bcx, cy)
    blk = wrap_alt(d, n, eq_sp(sid, 'Equation se', xfrm, [para]), 'l16_eq_s71_se.png',
                   placeholder(xfrm, '• se = √(2.9199²/35 + 2.6578²/35) = 0.6674', pt_se))
    x = x.replace(pic, pic + blk, 1)

    # slide 71, the t line (a display equation at 18 pt, CustomShape 6's own size)
    _, tpic = d.shape_block(n, 'CustomShape 6')
    assert 'sz="1800"' in tpic
    tid = re.search(r'<p:cNvPr id="(\d+)"', tpic).group(1); txfrm = geom(tpic)
    o = OM(1800)
    t = o.display(o.r(T + '='), o.f(o.r('(11.0571−8.3714)−0'), o.r('0.6674')), o.r('=4.0241'))
    tpara = (f'<a:p><a:pPr algn="ctr"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:buNone/></a:pPr>{t}'
             '<a:endParaRPr lang="en-US" sz="1800" b="0" strike="noStrike" spc="-1"/></a:p>')
    d.put(d.sp(n), x)                                         # wrap_alt adds rels
    tblk = wrap_alt(d, n, eq_sp(tid, 'CustomShape 6', txfrm, [tpara]), 'l16_eq_s71_t.png',
                    placeholder(txfrm, 't = ((11.0571 − 8.3714) − 0) / 0.6674 = 4.0241', 18))
    d.put(d.sp(n), d.get(d.sp(n)).replace(tpic, tblk, 1))
    d.drop_unused_rels(n); d.sweep_media()
    print('  slide 71: s1 2.9199, s2 2.6578; se line and t line as equations (0.6674, 4.0241)')


# ---------------------------------------------------------------------------------- pass 2
FALLBACKS = {52: [('TextBox 6', 'l16_eq_s52_se.png')],
             71: [('Equation se', 'l16_eq_s71_se.png'), ('CustomShape 6', 'l16_eq_s71_t.png')]}

def item_fallbacks(d, pdf, dpi=200):
    with tempfile.TemporaryDirectory() as tmp:
        for n, shapes in FALLBACKS.items():
            subprocess.run(['pdftoppm', '-r', str(dpi), '-f', str(n), '-l', str(n), '-png', pdf,
                            os.path.join(tmp, 'p')], check=True)
            page = Image.open([os.path.join(tmp, f) for f in os.listdir(tmp) if f.startswith('p')][0]).convert('RGB')
            x = re.sub(r'<mc:Fallback.*?</mc:Fallback>', '', d.get(d.sp(n)), flags=re.S)
            for name, media in shapes:
                blk = [m.group(0) for m in re.finditer(r'<p:sp>.*?</p:sp>', x, re.S) if f'name="{name}"' in m.group(0)[:600]]
                assert len(blk) == 1 and '<a14:m>' in blk[0], (n, name, len(blk))
                gx, gy, gcx, gcy = [v / EMU * dpi for v in geom(blk[0])]
                c = page.crop((round(gx), round(gy), round(gx + gcx), round(gy + gcy)))
                lo = ImageChops.darker(ImageChops.darker(*c.split()[:2]), c.split()[2])
                hi = ImageChops.lighter(ImageChops.lighter(*c.split()[:2]), c.split()[2])
                assert ImageStat.Stat(ImageChops.difference(hi, lo)).extrema[0][1] < 60, (n, name, 'not black ink')
                out = Image.new('RGBA', c.size, (0, 0, 0, 0)); out.putalpha(ImageChops.invert(lo))
                buf = io.BytesIO(); out.save(buf, 'PNG', dpi=(dpi, dpi))
                assert 'ppt/media/' + media in d.parts, media
                d.parts['ppt/media/' + media] = buf.getvalue()
                print(f'  slide {n} {name}: fallback from the render, {c.size[0]} x {c.size[1]} px')
            for f in os.listdir(tmp): os.remove(os.path.join(tmp, f))


ITEMS = {'links': item_links, 'typo': item_typo, 'dv': item_dv, 'sqrt': item_sqrt, 'sigma2': item_sigma2,
         'margin': item_margin, 'examq': item_examq}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = Deck2(src)
    if items[0] == 'fallbacks':
        print('[fallbacks]'); item_fallbacks(d, items[1])
    else:
        for it in items:
            print(f'[{it}]'); ITEMS[it](d)
    d.save(out)

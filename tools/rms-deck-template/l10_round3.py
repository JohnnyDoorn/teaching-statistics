"""L10 v2, round 3 (2026-10-08):
- Excel commands in the formula-sheet format: '= NORM.DIST(2,1; 0; 1; TRUE)', '= 1 - NORM.DIST(...)'
- slides 43-44: count vs proportion. Peach callout rewritten (no 'A binomial distribution with'),
  orange note replaced by a count/proportion table. Native text + equation objects (OMML), wrapped in
  mc:AlternateContent with a picture fallback, the way PowerPoint stores them.
- slide 46: right panel = number correct (label + x-axis); slides 37-39, 74 wording
- slide 55: p-hat as an equation object
- widen filled no-wrap labels whose text spills (PT Sans metrics, see fitcheck.py)
Usage: python3 build_v3.py <in.pptx> <out.pptx> <count_plot.png>
"""
import io, re, sys, zipfile
from PIL import Image, ImageDraw, ImageFont
sys.path.insert(0, '.')
import fitcheck

src, out, count_png = sys.argv[1:4]
zin = zipfile.ZipFile(src)
parts = {i.filename: zin.read(i.filename) for i in zin.infolist()}
order = [i.filename for i in zin.infolist()]
EMU = 914400
def get(p): return parts[p].decode('utf-8')
def put(p, s): parts[p] = s.encode('utf-8')
def sp(n): return f'ppt/slides/slide{n}.xml'
def rp(n): return f'ppt/slides/_rels/slide{n}.xml.rels'
def rep(n, old, new, count=1):
    s = get(sp(n)); k = s.count(old)
    assert k == count, f'slide{n}: {old!r} found {k}x, expected {count}'
    put(sp(n), s.replace(old, new))
def add_media(name, data):
    p = f'ppt/media/{name}'; assert p not in parts
    parts[p] = data; order.append(p); return name
def add_rel(n, media_name):
    rels = get(rp(n)); rid = 'rId%d' % (max(int(i) for i in re.findall(r'Id="rId(\d+)"', rels)) + 1)
    put(rp(n), rels.replace('</Relationships>', f'<Relationship Id="{rid}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/{media_name}"/></Relationships>'))
    return rid
def shape_block(n, name, tag='p:sp'):
    s = get(sp(n))
    m = [m for m in re.finditer(rf'<{tag}>.*?</{tag}>', s, re.S) if f'name="{name}"' in m.group(0)[:500]]
    assert len(m) == 1, (n, name, len(m))
    return s, m[0].group(0)

# ------------------------------------------------------------------ 1. Excel commands: formula-sheet format
X = [(60, 'NORM.DIST(0.6063; 0; 1; TRUE) = 0.7278', '= NORM.DIST(0,6063; 0; 1; TRUE) = 0.7278'),
     (60, 'NORM.DIST(0.7368; 0.7; 0.0607; TRUE) = 0.7278', '= NORM.DIST(0,7368; 0,7; 0,0607; TRUE) = 0.7278'),
     (61, 'NORM.DIST(0.6063; 0; 1; TRUE) = 0.7278', '= NORM.DIST(0,6063; 0; 1; TRUE) = 0.7278'),
     (66, '= 2.3 + 3 - 7 * SQRT(10) / 4^2', '= 2,3 + 3 - 7 * SQRT(10) / 4^2'),
     (67, '= 2.3 + 3 - 7 * SQRT(10) / 4^2', '= 2,3 + 3 - 7 * SQRT(10) / 4^2'),
     (78, 'NORM.DIST(2; 0; 1; TRUE)', '= NORM.DIST(2; 0; 1; TRUE)'),
     (78, 'NORM.DIST(103; 100; 1.5; TRUE) also possible -&gt; without conversion to z ', '= NORM.DIST(103; 100; 1,5; TRUE) also possible -&gt; without conversion to z '),
     (82, 'P(z&lt;1.7045)= ', 'P(z&lt;1.7045) '),
     (82, 'NORM.DIST(1.7045; 0; 1; TRUE) = 0.9559', '= NORM.DIST(1,7045; 0; 1; TRUE) = 0.9559'),
     (82, 'NORM.DIST(-1.7045; 0; 1; TRUE) = 0.0441', '= NORM.DIST(-1,7045; 0; 1; TRUE) = 0.0441'),
     (83, 'NORM.DIST(0.055; 0.04; 0.0088; TRUE) = 0.9559', '= NORM.DIST(0,055; 0,04; 0,0088; TRUE) = 0.9559'),
     (83, '1-NORM.DIST(0.055; 0.04; 0.0088; TRUE) = ', '= 1 - NORM.DIST(0,055; 0,04; 0,0088; TRUE) = '),
     (88, 'NORM.DIST(-1.5432; 0; 1; TRUE) = 0.0614', '= NORM.DIST(-1,5432; 0; 1; TRUE) = 0.0614')]
for n, old, new in X:
    rep(n, f'<a:t>{old}</a:t>', f'<a:t>{new}</a:t>')

# ------------------------------------------------------------------ OMML + shape helpers (structure copied from PowerPoint's own slide 13)
NS_MC = 'http://schemas.openxmlformats.org/markup-compatibility/2006'
NS_A14 = 'http://schemas.microsoft.com/office/drawing/2010/main'
NS_M = 'http://schemas.openxmlformats.org/officeDocument/2006/math'
def esc(t): return t.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
def rpr(sz, color='000000', i=False):
    return (f'<a:rPr lang="en-US" sz="{sz}" b="0" i="{1 if i else 0}" strike="noStrike" spc="-1" dirty="0">'
            f'<a:solidFill><a:srgbClr val="{color}"/></a:solidFill></a:rPr>')
def run(t, sz, color='000000', i=False): return f'<a:r>{rpr(sz, color, i)}<a:t>{esc(t)}</a:t></a:r>'
class M:
    def __init__(s, sz, color='000000'): s.sz, s.c = sz, color
    def _r(s): return (f'<a:rPr lang="en-US" sz="{s.sz}" b="0" i="1" strike="noStrike" spc="-1" smtClean="0"><a:solidFill><a:srgbClr val="{s.c}"/></a:solidFill>'
                       '<a:latin typeface="Cambria Math" panose="02040503050406030204" pitchFamily="18" charset="0"/></a:rPr>')
    def r(s, t): return f'<m:r>{s._r()}<m:t>{esc(t)}</m:t></m:r>'
    def hat(s, e): return f'<m:acc><m:accPr><m:chr m:val="̂"/><m:ctrlPr>{s._r()}</m:ctrlPr></m:accPr><m:e>{e}</m:e></m:acc>'
    def sqrt(s, e): return f'<m:rad><m:radPr><m:degHide m:val="1"/><m:ctrlPr>{s._r()}</m:ctrlPr></m:radPr><m:deg/><m:e>{e}</m:e></m:rad>'
    def frac(s, a, b): return f'<m:f><m:fPr><m:ctrlPr>{s._r()}</m:ctrlPr></m:fPr><m:num>{a}</m:num><m:den>{b}</m:den></m:f>'
    def math(s, *content): return f'<a14:m><m:oMath xmlns:m="{NS_M}">{"".join(content)}</m:oMath></a14:m>'
def para(items, sz, algn=None, tabs=(), bef=0, ln=100):
    a = f' algn="{algn}"' if algn else ''
    tl = ''.join(f'<a:tab pos="{round(t * EMU)}" algn="l"/>' for t in tabs)
    return (f'<a:p><a:pPr{a}><a:lnSpc><a:spcPct val="{ln * 1000}"/></a:lnSpc>'
            + (f'<a:spcBef><a:spcPts val="{bef * 100}"/></a:spcBef>' if bef else '')
            + '<a:buNone/>' + (f'<a:tabLst>{tl}</a:tabLst>' if tl else '') + '</a:pPr>'
            + ''.join(items) + f'<a:endParaRPr lang="en-US" sz="{sz}" b="0" strike="noStrike" spc="-1" dirty="0"/></a:p>')
def text_sp(sid, name, box, fill, line, paras):
    x, y, cx, cy = [round(v * EMU) for v in box]
    return (f'<p:sp><p:nvSpPr><p:cNvPr id="{sid}" name="{name}"/><p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr>'
            f'<p:spPr><a:xfrm><a:off x="{x}" y="{y}"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom>{fill}{line}</p:spPr>'
            '<p:txBody><a:bodyPr wrap="square" lIns="91440" tIns="45720" rIns="91440" bIns="45720" anchor="t"><a:noAutofit/></a:bodyPr><a:lstStyle/>'
            + ''.join(paras) + '</p:txBody></p:sp>')
def fallback_sp(sid, name, box, rid):
    x, y, cx, cy = [round(v * EMU) for v in box]
    return (f'<p:sp><p:nvSpPr><p:cNvPr id="{sid}" name="{name}"/><p:cNvSpPr><a:spLocks noRot="1" noChangeAspect="1" noMove="1" noResize="1" noEditPoints="1" noAdjustHandles="1" noChangeArrowheads="1" noChangeShapeType="1" noTextEdit="1"/></p:cNvSpPr><p:nvPr/></p:nvSpPr>'
            f'<p:spPr><a:xfrm><a:off x="{x}" y="{y}"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:blipFill><a:blip r:embed="{rid}"/><a:stretch><a:fillRect/></a:stretch></a:blipFill></p:spPr>'
            '<p:txBody><a:bodyPr/><a:lstStyle/><a:p><a:r><a:rPr lang="en-US"><a:noFill/></a:rPr><a:t> </a:t></a:r></a:p></p:txBody></p:sp>')
def alt(choice, fb):
    return f'<mc:AlternateContent xmlns:mc="{NS_MC}"><mc:Choice xmlns:a14="{NS_A14}" Requires="a14">{choice}</mc:Choice><mc:Fallback>{fb}</mc:Fallback></mc:AlternateContent>'

PT = '/System/Library/Fonts/Supplemental/PTSans.ttc'
def render_fallback(box_in, fill_rgba, border, lines, dpi=150):
    """Picture used only by apps without equation support: plain text, Unicode math."""
    W, H = round(box_in[2] * dpi), round(box_in[3] * dpi)
    im = Image.new('RGBA', (W, H), fill_rgba); d = ImageDraw.Draw(im)
    if border: d.rectangle([0, 0, W - 1, H - 1], outline=border, width=2)
    y = 0.05 * dpi
    for text, pt, color, italic, algn, tabs in lines:
        f = ImageFont.truetype(PT, round(pt / 72 * dpi), index=1 if italic else 0)
        if tabs:
            for k, seg in enumerate(text.split('\t')):
                xx = 0.1 * dpi + (tabs[k - 1] * dpi if k else 0)
                d.text((xx, y), seg, font=f, fill=color)
        else:
            tw = f.getlength(text); xx = (W - tw) / 2 if algn == 'ctr' else 0.1 * dpi
            d.text((xx, y), text, font=f, fill=color)
        y += pt / 72 * dpi * 1.35
    buf = io.BytesIO(); im.save(buf, 'PNG'); return buf.getvalue()

ORANGE = 'ED7D31'
PEACH_FILL = f'<a:solidFill><a:srgbClr val="{ORANGE}"><a:alpha val="20000"/></a:srgbClr></a:solidFill>'
PEACH_LINE = f'<a:ln w="12700"><a:solidFill><a:srgbClr val="{ORANGE}"/></a:solidFill></a:ln>'
NOTE_FILL = f'<a:solidFill><a:srgbClr val="{ORANGE}"/></a:solidFill>'
NOTE_LINE = '<a:ln w="12700"><a:solidFill><a:srgbClr val="000000"/></a:solidFill></a:ln>'

# ------------------------------------------------------------------ 2. slides 43-44: peach callout (same box), native
PEACH_BOX = (1.1, 2.0, 9962280 / EMU, 1946520 / EMU)
m24, o24 = M(2400), M(2400, ORANGE)
peach_paras = [
    para([run('The sampling distribution of the proportion ', 2400, ORANGE, i=True), o24.math(o24.hat(o24.r('p'))), run(':', 2400)], 2400),
    para([run('mean: ', 2400), m24.math(m24.r('p'))], 2400, algn='ctr', bef=6),
    para([run('standard deviation: ', 2400), m24.math(m24.sqrt(m24.frac(m24.r('p(1−p)'), m24.r('n'))))], 2400, algn='ctr', bef=6)]
peach_png = add_media('countprop_peach.png', render_fallback(PEACH_BOX, (237, 125, 49, 51), (237, 125, 49), [
    ('The sampling distribution of the proportion p̂:', 24, (237, 125, 49), True, None, ()),
    ('mean: p', 24, 'black', False, 'ctr', ()), ('standard deviation: √(p(1−p)/n)', 24, 'black', False, 'ctr', ())]))
for n in (43, 44):
    s, blk = shape_block(n, 'CustomShape 4')
    assert 'blipFill' in blk
    sid = re.search(r'<p:cNvPr id="(\d+)"', blk).group(1)
    rid = add_rel(n, peach_png)
    put(sp(n), s.replace(blk, alt(text_sp(sid, 'CustomShape 4', PEACH_BOX, PEACH_FILL, PEACH_LINE, peach_paras),
                                  fallback_sp(sid, 'CustomShape 4', PEACH_BOX, rid))))

# ------------------------------------------------------------------ 3. slide 44: orange note -> count/proportion table, over the repeated bullets
TABLE_BOX = (0.92, 4.2, 11.2, 2.75)   # full width: covers the repeated bullets underneath
TABS = (1.7, 4.5)
m20 = M(2000)
t = lambda s_: run(s_, 2000)
table_paras = [
    para([t('Count or proportion? Same data, same shape')], 2000),
    para([t('\tNumber correct ('), m20.math(m20.r('X')), t(')\tProportion correct ('), m20.math(m20.hat(m20.r('p')), m20.r('='), m20.r('X/n')), t(')')], 2000, tabs=TABS, bef=6),
    para([t('Distribution\tbinomial (Lecture 9)\tbinomial, divided by '), m20.math(m20.r('n'))], 2000, tabs=TABS, bef=4),
    para([t('Mean\t'), m20.math(m20.r('np')), t('\t'), m20.math(m20.r('p'))], 2000, tabs=TABS, bef=4),
    para([t('SD\t'), m20.math(m20.sqrt(m20.r('np(1−p)'))), t('\t'), m20.math(m20.sqrt(m20.frac(m20.r('p(1−p)'), m20.r('n'))))], 2000, tabs=TABS, bef=4),
    para([t('7 out of 10 correct = 0.7: divide every value by '), m20.math(m20.r('n')), t(', only the axis changes.')], 2000, bef=8)]
table_png = add_media('countprop_table.png', render_fallback(TABLE_BOX, (237, 125, 49, 255), (0, 0, 0), [
    ('Count or proportion? Same data, same shape', 20, 'black', False, None, ()),
    ('\tNumber correct (X)\tProportion correct (p̂ = X/n)', 20, 'black', False, None, TABS),
    ('Distribution\tbinomial (Lecture 9)\tbinomial, divided by n', 20, 'black', False, None, TABS),
    ('Mean\tnp\tp', 20, 'black', False, None, TABS),
    ('SD\t√(np(1−p))\t√(p(1−p)/n)', 20, 'black', False, None, TABS),
    ('7 out of 10 correct = 0.7: divide every value by n, only the axis changes.', 20, 'black', False, None, ())]))
s, blk = shape_block(44, 'CustomShape 5')
assert 'blipFill' in blk
sid = re.search(r'<p:cNvPr id="(\d+)"', blk).group(1)
rid = add_rel(44, table_png)
put(sp(44), s.replace(blk, alt(text_sp(sid, 'CustomShape 5', TABLE_BOX, NOTE_FILL, NOTE_LINE, table_paras),
                               fallback_sp(sid, 'CustomShape 5', TABLE_BOX, rid))))

# ------------------------------------------------------------------ 4. slide 46: right panel is the number correct
rep(46, '<a:t>Binomial distribution  </a:t>', '<a:t>Number correct (binomial)</a:t>')
s, blk = shape_block(46, 'CustomShape 3')
g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/><a:ext cx="(\d+)" cy="(\d+)"/>', blk)
need = (fitcheck.font(18).getlength('Number correct (binomial)') / 10 / 72) + 0.2 + 0.1
cx_old = int(g.group(3)); cx_new = max(cx_old, round(need * EMU)); x_new = int(g.group(1)) - (cx_new - cx_old) // 2
put(sp(46), s.replace(blk, blk.replace(g.group(0), f'<a:off x="{x_new}" y="{g.group(2)}"/><a:ext cx="{cx_new}" cy="{g.group(4)}"/>', 1)))
s, blk = shape_block(46, 'Picture 721', 'p:pic')
rid = add_rel(46, add_media('binomCount10_numbercorrect.png', open(count_png, 'rb').read()))
put(sp(46), s.replace(blk, re.sub(r'r:embed="rId\d+"', f'r:embed="{rid}"', blk, count=1)))

# ------------------------------------------------------------------ 5. wording: 37-39, 74
for n in (37, 38, 39):
    rep(n, '<a:t>Binomial distribution</a:t>', '<a:t>Binomial distribution (of the number correct)</a:t>')
rep(74, '<a:t>For the proportion, the sampling distribution is the binomial distribution</a:t>',
        '<a:t>For the proportion, the sampling distribution is the binomial distribution divided by n</a:t>')

# ------------------------------------------------------------------ 6. slide 55: p-hat as an equation object
s, blk = shape_block(55, 'CustomShape 2')
old_run = re.search(r'<a:r><a:rPr[^>]*>(?:(?!</a:r>).)*<a:t> of the 57 tasters \(p̂ ≥ 0\.7368\) pick the alcoholic beer, when the population proportion is p = 0\.7\.</a:t></a:r>', blk, re.S).group(0)
rpr55 = re.search(r'<a:rPr.*?</a:rPr>', old_run, re.S).group(0)
m55 = M(2400)
new_runs = (f'<a:r>{rpr55}<a:t> of the 57 tasters (</a:t></a:r>' + m55.math(m55.hat(m55.r('p')))
            + f'<a:r>{rpr55}<a:t> ≥ 0.7368) pick the alcoholic beer, when the population proportion is p = 0.7.</a:t></a:r>')
choice = blk.replace(old_run, new_runs)
sid = re.search(r'<p:cNvPr id="(\d+)"', blk).group(1)
g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/><a:ext cx="(\d+)" cy="(\d+)"/>', blk)
box55 = [int(v) / EMU for v in g.groups()]
# fallback: the two bullets as plain text
f24 = ImageFont.truetype(PT, round(24 / 72 * 150)); W55 = round(box55[2] * 150)
def wrap(txt):
    lines, cur = [], ''
    for wd in txt.split():
        if f24.getlength((cur + ' ' + wd).strip()) > W55 - 0.5 * 150 and cur: lines.append(cur); cur = wd
        else: cur = (cur + ' ' + wd).strip()
    return lines + [cur]
b1 = 'When we know the sampling distribution (for instance because of the C.L.T., or because we know or assume it), we can speak about the probability of observing a certain value for a statistic.'
b2 = 'For instance, the probability that 42 or more of the 57 tasters (p̂ ≥ 0.7368) pick the alcoholic beer, when the population proportion is p = 0.7.'
fl = [('• ' + l if k == 0 else '  ' + l, 24, 'black', False, None, ()) for b in (b1, b2) for k, l in enumerate(wrap(b))]
rid = add_rel(55, add_media('slide55_text_fallback.png', render_fallback(box55, (255, 255, 255, 0), None, fl)))
put(sp(55), s.replace(blk, alt(choice, fallback_sp(sid, 'CustomShape 2', box55, rid))))

# ------------------------------------------------------------------ slide 29: "works" made the callout wrap to 3 lines; widen leftwards to keep 2
s, blk = shape_block(29, 'CustomShape 84')
g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/><a:ext cx="(\d+)" cy="(\d+)"/>', blk)
right = int(g.group(1)) + int(g.group(3)); cx29 = round(9.4 * EMU)
put(sp(29), s.replace(blk, blk.replace(g.group(0), f'<a:off x="{right - cx29}" y="{g.group(2)}"/><a:ext cx="{cx29}" cy="{g.group(4)}"/>', 1)))

# ------------------------------------------------------------------ 7. widen filled no-wrap labels whose text spills
def is_centered(b): return 'algn="ctr"' in b
widened = []
pres = get('ppt/presentation.xml'); prels = get('ppt/_rels/presentation.xml.rels')
rid2f = dict(re.findall(r'Id="(rId\d+)"[^>]*Target="slides/(slide\d+\.xml)"', prels))
files = [rid2f[r] for r in re.findall(r'<p:sldId [^>]*r:id="(rId\d+)"', pres)]
for pos, f in enumerate(files, 1):
    p = 'ppt/slides/' + f; s = get(p); new = s
    for m in re.finditer(r'<p:sp>.*?</p:sp>', s, re.S):
        b = m.group(0)
        if '<p:ph' in b[:800] or 'wrap="none"' not in b: continue
        if not re.search(r'<p:spPr>(?:(?!<a:ln).)*<a:solidFill>', b, re.S): continue
        res = fitcheck.check_shape(b, 18)
        if not res or not any('no wrap' in pr for pr in res[0]): continue
        g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/>\s*<a:ext cx="(\d+)" cy="(\d+)"/>', b)
        x, y, cx, cy = [int(v) for v in g.groups()]
        widest = float(re.search(r'line ([\d.]+)in', res[0][0]).group(1))
        inner = cx / EMU - 0.2
        if widest <= inner * 1.04: continue
        cx2 = round((widest + 0.2 + 0.08) * EMU); x2 = x - (cx2 - cx) // 2 if is_centered(b) else x
        nb = b.replace(g.group(0), f'<a:off x="{x2}" y="{y}"/><a:ext cx="{cx2}" cy="{cy}"/>', 1)
        new = new.replace(b, nb)
        widened.append((pos, re.search(r'name="([^"]*)"', b).group(1), round((cx2 - cx) / EMU, 2), res[1][:45]))
    put(p, new)
for w_ in widened: print('widened slide %d %s by %.2f in: %s' % w_)

# ------------------------------------------------------------------ drop rels the edited slides no longer use
for n in (43, 44, 46, 55):
    x, rels = get(sp(n)), get(rp(n))
    used = set(re.findall(r'r:(?:embed|id|link)="(rId\d+)"', x))
    for rid_, tgt in re.findall(r'<Relationship Id="(rId\d+)"[^>]*Target="\.\./media/([^"]+)"/>', rels):
        if rid_ not in used:
            rels = re.sub(rf'<Relationship Id="{rid_}"[^>]*/>', '', rels); print(f'slide {n}: dropped unused rel to {tgt}')
    put(rp(n), rels)

# ------------------------------------------------------------------ orphaned media
alltargets = ' '.join(v.decode('utf-8', 'ignore') for k, v in parts.items() if k.endswith('.rels'))
for p in [k for k in parts if k.startswith('ppt/media/')]:
    if f'media/{p.split("/")[-1]}"' not in alltargets:
        del parts[p]; order.remove(p); print('removed unreferenced', p)

with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:
    for p in order: z.writestr(p, parts[p])
print('written', out)

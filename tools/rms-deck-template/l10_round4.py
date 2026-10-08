"""L10 v2, round 4 (2026-10-08): leftovers.
- plain-text p-hat labels (13-18, 72, 73) -> equation objects, mirroring Johnny's own x-bar label on slide 13
  (upright letter + accent, then '= value')
- recap labels 72-73: 'Binomial distribution' -> 'Binomial distribution, divided by n' (box widened, centred)
Usage: python3 build_v4.py <in.pptx> <out.pptx>
"""
import re, sys, zipfile
sys.path.insert(0, '.')
import fitcheck
from l10_round3_helpers import *   # noqa: F401,F403  (alt, fallback_sp, render_fallback, esc, EMU, NS_M)

src, out = sys.argv[1:3]
zin = zipfile.ZipFile(src)
parts = {i.filename: zin.read(i.filename) for i in zin.infolist()}
order = [i.filename for i in zin.infolist()]
def get(p): return parts[p].decode('utf-8')
def put(p, s): parts[p] = s.encode('utf-8')
def sp(n): return f'ppt/slides/slide{n}.xml'
def rp(n): return f'ppt/slides/_rels/slide{n}.xml.rels'
def add_media(name, data):
    p = f'ppt/media/{name}'; assert p not in parts; parts[p] = data; order.append(p); return name
def add_rel(n, media_name):
    rels = get(rp(n)); rid = 'rId%d' % (max(int(i) for i in re.findall(r'Id="rId(\d+)"', rels)) + 1)
    put(rp(n), rels.replace('</Relationships>', f'<Relationship Id="{rid}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/{media_name}"/></Relationships>'))
    return rid
def shape_block(n, name):
    s = get(sp(n))
    m = [m for m in re.finditer(r'<p:sp>.*?</p:sp>', s, re.S) if f'name="{name}"' in m.group(0)[:500]]
    assert len(m) == 1, (n, name, len(m))
    return s, m[0].group(0)

# ------------------------------------------------------------------ 1. p-hat labels
def phat_math(rpr_inner):
    """Upright p with a hat, same run properties as the label (as in his x-bar)."""
    r = rpr_inner.replace('<a:rPr lang="en-US"', '<a:rPr lang="en-US"', 1)
    r = re.sub(r'<a:rPr([^>]*)>', lambda m_: '<a:rPr' + re.sub(r' i="\d"', '', m_.group(1)) + ' i="1" smtClean="0">', r, count=1)
    return (f'<a14:m><m:oMath xmlns:m="{NS_M}"><m:acc><m:accPr><m:chr m:val="̂"/><m:ctrlPr>{r}</m:ctrlPr></m:accPr>'
            f'<m:e><m:r><m:rPr><m:sty m:val="p"/></m:rPr>{r}<m:t>p</m:t></m:r></m:e></m:acc></m:oMath></a14:m>')
LABELS = [(13, 'CustomShape 2', '0.74'), (14, 'CustomShape 3', '0.74'), (15, 'CustomShape 3', '0.74'), (16, 'CustomShape 3', '0.74'),
          (17, 'CustomShape 3', '0.74'), (18, 'CustomShape 3', '0.74'), (72, 'CustomShape 6', '0.67'), (73, 'CustomShape 5', '0.74')]
fb_cache = {}
for n, name, val in LABELS:
    s, blk = shape_block(n, name)
    r = re.search(r'<a:r>(<a:rPr.*?</a:rPr>)<a:t>p̂ = ' + re.escape(val) + r'</a:t></a:r>', blk, re.S)
    assert r, (n, name)
    rpr_ = r.group(1)
    choice = blk.replace(r.group(0), phat_math(rpr_) + f'<a:r>{rpr_}<a:t>= {val}</a:t></a:r>')
    sid = re.search(r'<p:cNvPr id="(\d+)"', blk).group(1)
    g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/><a:ext cx="(\d+)" cy="(\d+)"/>', blk)
    box = [int(v) / EMU for v in g.groups()]
    key = (val, round(box[2], 2), round(box[3], 2))
    if key not in fb_cache:
        fb_cache[key] = add_media(f'phat_{val.replace(".", "")}_{len(fb_cache)}.png',
                                  render_fallback(box, (255, 255, 255, 0), None, [(f'p̂= {val}', 18, 'black', False, None, ())]))
    rid = add_rel(n, fb_cache[key])
    put(sp(n), s.replace(blk, alt(choice, fallback_sp(sid, name, box, rid))))
    print(f'slide {n}: p-hat label -> equation')

# ------------------------------------------------------------------ 2. recap labels
for n, name in [(72, 'CustomShape 7'), (73, 'CustomShape 6')]:
    s, blk = shape_block(n, name)
    assert blk.count('<a:t>Binomial distribution  </a:t>') == 1
    nb = blk.replace('<a:t>Binomial distribution  </a:t>', '<a:t>Binomial distribution, divided by n</a:t>')
    need = fitcheck.font(18).getlength('Binomial distribution, divided by n') / 10 / 72 + 0.2 + 0.1
    g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/><a:ext cx="(\d+)" cy="(\d+)"/>', nb)
    x, y, cx, cy = [int(v) for v in g.groups()]
    cx2 = max(cx, round(need * EMU)); x2 = x - (cx2 - cx) // 2
    if x2 + cx2 > 12192000 - round(0.1 * EMU): x2 = 12192000 - round(0.1 * EMU) - cx2   # keep on the slide
    nb = nb.replace(g.group(0), f'<a:off x="{x2}" y="{y}"/><a:ext cx="{cx2}" cy="{cy}"/>', 1)
    put(sp(n), s.replace(blk, nb))
    print(f'slide {n}: recap label -> "Binomial distribution, divided by n", box {cx / EMU:.2f} -> {cx2 / EMU:.2f} in')

# ------------------------------------------------------------------ 3. slide 13 p-hat note: ~0.11 in short (filled, no wrap)
s, blk = shape_block(13, 'CustomShape 4')
g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/><a:ext cx="(\d+)" cy="(\d+)"/>', blk)
put(sp(13), s.replace(blk, blk.replace(g.group(0), f'<a:off x="{g.group(1)}" y="{g.group(2)}"/><a:ext cx="{int(g.group(3)) + round(0.2 * EMU)}" cy="{g.group(4)}"/>', 1)))
print('slide 13: p-hat note widened by 0.20 in')

with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:
    for p in order: z.writestr(p, parts[p])
print('written', out)

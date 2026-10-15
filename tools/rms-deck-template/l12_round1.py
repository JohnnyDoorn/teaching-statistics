"""L12 pre-teaching fixes (2026-10-14), run one item at a time:

    python3 l12_round1.py <in.pptx> <out.pptx> <item> [<item> ...]

items:
  recap     slides 4-5 = L10 slides 72-73 as taught (beer tasting); slide 6 gets the
            n = 57 plot, label and 'Behavior of the statistic' box of L10 slide 73
  excel     Excel commands in the formula-sheet format (slide 80; slide 42 plot)
  examq     exam question: option c and solution with t = 2.7045 -> (17.81, 21.79)
  samplesize  round sample sizes up: 8.76 -> 385, slide 62 -> 72
  fit       widen / split boxes that fitcheck and the render check flag
  xbar      x-bar / p-hat in plain text -> equation objects (PT Sans has no U+0304/U+0302)

Works on the zip parts directly, like l10_round3.py. Every run validates the package
(rels targets, content types, r:ids, XML well-formedness) before writing.
"""
import io, re, sys, zipfile
from lxml import etree

EMU = 914400
L10 = '/Users/johnny/surfdrive/Teaching/RMS/RMS_2026/RMS_2026/RMS2627_10_SamplingDistributions.pptx'
NS_R = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships'
NS_PR = 'http://schemas.openxmlformats.org/package/2006/relationships'


class Deck:
    def __init__(self, path):
        z = zipfile.ZipFile(path)
        self.parts = {i.filename: z.read(i.filename) for i in z.infolist()}
        self.order = [i.filename for i in z.infolist()]

    def get(self, p): return self.parts[p].decode('utf-8')
    def put(self, p, s): self.parts[p] = s.encode('utf-8') if isinstance(s, str) else s

    # slide n = position in the deck (1-based), resolved through presentation.xml
    def slide_files(self):
        pres, prels = self.get('ppt/presentation.xml'), self.get('ppt/_rels/presentation.xml.rels')
        rid2f = {r.get('Id'): r.get('Target') for r in etree.fromstring(prels.encode())}
        return [rid2f[r] for r in re.findall(r'<p:sldId [^>]*r:id="(rId\d+)"', pres)]
    def sp(self, n): return 'ppt/' + self.slide_files()[n - 1]
    def rp(self, n):
        f = self.slide_files()[n - 1]
        return 'ppt/slides/_rels/' + f.split('/')[-1] + '.rels'

    def rep(self, n, old, new, count=1):
        s = self.get(self.sp(n)); k = s.count(old)
        assert k == count, f'slide {n}: {old!r} found {k}x, expected {count}'
        self.put(self.sp(n), s.replace(old, new))

    def add_media(self, name, data):
        p = f'ppt/media/{name}'
        assert p not in self.parts, p
        self.parts[p] = data; self.order.append(p)
        return name

    def add_rel(self, n, media_name):
        rels = self.get(self.rp(n))
        rid = 'rId%d' % (max(int(i) for i in re.findall(r'Id="rId(\d+)"', rels)) + 1)
        self.put(self.rp(n), rels.replace('</Relationships>',
                 f'<Relationship Id="{rid}" Type="{NS_R}/image" Target="../media/{media_name}"/></Relationships>'))
        return rid

    def shape_block(self, n, name, tag='p:sp'):
        s = self.get(self.sp(n))
        m = [m for m in re.finditer(rf'<{tag}>.*?</{tag}>', s, re.S) if f'name="{name}"' in m.group(0)[:600]]
        assert len(m) == 1, (n, name, len(m))
        return s, m[0].group(0)

    def drop_unused_rels(self, n):
        x, rels = self.get(self.sp(n)), self.get(self.rp(n))
        used = set(re.findall(r'r:(?:embed|id|link|pict)="(rId\d+)"', x))
        for r in etree.fromstring(rels.encode()):
            if r.get('Type').endswith('/image') and r.get('Id') not in used:
                rels = re.sub(rf'<Relationship [^>]*Id="{r.get("Id")}"[^>]*/>', '', rels)
                print(f'  slide {n}: dropped unused rel to {r.get("Target")}')
        self.put(self.rp(n), rels)

    def sweep_media(self):
        alltargets = ' '.join(v.decode('utf-8', 'ignore') for k, v in self.parts.items() if k.endswith('.rels'))
        for p in [k for k in self.parts if k.startswith('ppt/media/')]:
            if f'media/{p.split("/")[-1]}"' not in alltargets:
                del self.parts[p]; self.order.remove(p); print('  removed unreferenced', p)

    def dedupe_content_types(self):
        """The Sept build left L12 and L23 with every slideLayout Override listed twice; a part name
        may appear only once (OPC), so PowerPoint asks to repair on open."""
        ct, seen, k = self.get('[Content_Types].xml'), set(), 0
        def keep(m):
            nonlocal k
            if m.group(1) in seen: k += 1; return ''
            seen.add(m.group(1)); return m.group(0)
        self.put('[Content_Types].xml', re.sub(r'<Override PartName="([^"]+)"[^>]*/>', keep, ct))
        if k: print(f'  removed {k} duplicate content-type overrides')

    def validate(self):
        ct = self.get('[Content_Types].xml')
        names = re.findall(r'<Override PartName="([^"]+)"', ct)
        assert len(names) == len(set(names)), 'duplicate content-type overrides'
        defaults = set(re.findall(r'<Default Extension="([^"]+)"', ct))
        overrides = set(re.findall(r'<Override PartName="/([^"]+)"', ct))
        for p, data in self.parts.items():
            if p.endswith('/'): continue
            if p.endswith('.xml') or p.endswith('.rels'):
                etree.fromstring(data)  # well-formed
            ext = p.rsplit('.', 1)[-1].lower()
            assert p in overrides or ext in defaults, f'no content type for {p}'
        for o in overrides:
            assert o in self.parts, f'content-type override for missing part {o}'
        for p in [k for k in self.parts if k.endswith('.rels')]:
            base = p.replace('_rels/', '').rsplit('.rels', 1)[0]
            folder = base.rsplit('/', 1)[0] if '/' in base else ''
            rids = set()
            for r in etree.fromstring(self.parts[p]):
                rids.add(r.get('Id'))
                if r.get('TargetMode') == 'External': continue
                t = r.get('Target')
                tgt = t.lstrip('/') if t.startswith('/') else _norm(folder + '/' + t)
                assert tgt in self.parts, f'{p}: {r.get("Id")} -> missing {tgt}'
            if base in self.parts and base.endswith('.xml'):
                used = set(re.findall(r'r:(?:embed|id|link|pict)="(rId\d+)"', self.get(base)))
                missing = used - rids
                assert not missing, f'{base}: r:ids without a relationship {missing}'

    def save(self, out):
        self.dedupe_content_types()
        self.validate()
        with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:
            for p in self.order: z.writestr(p, self.parts[p])
        print('written', out)


def _norm(path):
    out = []
    for seg in path.split('/'):
        if seg == '..': out.pop()
        elif seg and seg != '.': out.append(seg)
    return '/'.join(out)


def renumber_ids(block, new_id):
    return re.sub(r'(<p:cNvPr id=")\d+(")', rf'\g<1>{new_id}\2', block, count=1)


# ---------------------------------------------------------------------------------- recap
def item_recap(d):
    src = Deck(L10)
    srcfiles = src.slide_files()
    for dst_n, src_n in ((4, 72), (5, 73)):
        sfile = 'ppt/' + srcfiles[src_n - 1]
        srels = 'ppt/slides/_rels/' + sfile.split('/')[-1] + '.rels'
        x = src.get(sfile)
        assert ('Sample (n=100)' if src_n == 72 else 'Sample (n=57)') in x
        rels = etree.fromstring(src.parts[srels])
        out = []
        for r in rels:
            typ, tgt = r.get('Type'), r.get('Target')
            if typ.endswith('/slideLayout'):
                assert tgt == '../slideLayouts/slideLayout4.xml'   # 'Section Overview' in both decks
                assert 'name="Section Overview"' in d.get('ppt/slideLayouts/slideLayout4.xml')
            elif typ.endswith('/image'):
                name = 'l10recap_' + tgt.split('/')[-1]
                if f'ppt/media/{name}' not in d.parts:
                    d.add_media(name, src.parts[_norm('ppt/slides/' + tgt)])
                tgt = f'../media/{name}'
            else:
                raise ValueError(f'unexpected relationship on L10 slide {src_n}: {typ}')
            out.append(f'<Relationship Id="{r.get("Id")}" Type="{typ}" Target="{tgt}"/>')
        d.put(d.sp(dst_n), x)
        d.put(d.rp(dst_n), '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
              f'<Relationships xmlns="{NS_PR}">' + ''.join(out) + '</Relationships>')
        print(f'  slide {dst_n} <- L10 slide {src_n}')

    # slide 6: right half identical to the new slide 5 (L10 slide 73), bullets stay
    s5 = d.get(d.sp(5))
    def blk5(name, tag='p:sp'):
        m = [m.group(0) for m in re.finditer(rf'<{tag}>.*?</{tag}>', s5, re.S) if f'name="{name}"' in m.group(0)[:600]]
        assert len(m) == 1, name
        return m[0]
    s6, pic = d.shape_block(6, 'Picture 665', 'p:pic')
    new_pic = renumber_ids(blk5('Picture 905', 'p:pic'), re.search(r'<p:cNvPr id="(\d+)"', pic).group(1))
    rid_src = re.search(r'r:embed="(rId\d+)"', new_pic).group(1)
    tgt = re.search(rf'Id="{rid_src}"[^>]*Target="\.\./media/([^"]+)"', d.get(d.rp(5)))
    tgt = tgt.group(1) if tgt else re.search(rf'Target="\.\./media/([^"]+)"[^>]*Id="{rid_src}"', d.get(d.rp(5))).group(1)
    rid6 = d.add_rel(6, tgt)
    new_pic = new_pic.replace(f'r:embed="{rid_src}"', f'r:embed="{rid6}"')
    s6 = s6.replace(pic, new_pic)
    for old_name, new_name in (('CustomShape 3', 'CustomShape 6'), ('CustomShape 4', 'CustomShape 8'), ('Line 5', 'Line 11')):
        old = [m.group(0) for m in re.finditer(r'<p:sp>.*?</p:sp>', s6, re.S) if f'name="{old_name}"' in m.group(0)[:600]]
        assert len(old) == 1, old_name
        new = renumber_ids(blk5(new_name), re.search(r'<p:cNvPr id="(\d+)"', old[0]).group(1))
        new = new.replace(f'name="{new_name}"', f'name="{old_name}"', 1)
        assert 'r:embed' not in new
        s6 = s6.replace(old[0], new)
    d.put(d.sp(6), s6)
    d.drop_unused_rels(6)
    d.sweep_media()


# ---------------------------------------------------------------------------------- excel
SLIDE42_PNG = '/Users/johnny/surfdrive/Teaching/RMS/FiguresLectures/slide42_tinv.png'   # RMS_FiguresLecture12_slide42_tinv.R

def item_excel(d):
    for old, new in (('= 2.3 + 3 - 7 * SQRT(10) / 4^2', '= 2,3 + 3 - 7 * SQRT(10) / 4^2'),
                     ('= NORM.DIST(2, 0, 1, TRUE)', '= NORM.DIST(2; 0; 1; TRUE)'),
                     ('= T.DIST(2, 14, TRUE)', '= T.DIST(2; 14; TRUE)'),
                     ('= NORM.INV(0.025, 0, 1)', '= NORM.INV(0,025; 0; 1)'),
                     ('= T.INV(0.025, 20)', '= T.INV(0,025; 20)')):
        d.rep(80, f'<a:t>{old}</a:t>', f'<a:t>{new}</a:t>')
    # slide 42: plot label in the formula-sheet syntax, x-axis t instead of z
    s, pic = d.shape_block(42, 'Picture 5', 'p:pic')
    assert 'image62.png' in d.get(d.rp(42))
    rid = d.add_rel(42, d.add_media('l12_slide42_tinv.png', open(SLIDE42_PNG, 'rb').read()))
    d.put(d.sp(42), s.replace(pic, re.sub(r'r:embed="rId\d+"', f'r:embed="{rid}"', pic, count=1)))
    d.drop_unused_rels(42)
    d.sweep_media()


# ---------------------------------------------------------------------------------- examq
def item_examq(d):
    """t.005 = 2.7045 (4 decimals), SE = 0.7340, ME = 1.9851 -> (17.81, 21.79); exact gives the same."""
    import subprocess, tempfile, os
    d.rep(74, '<a:t> (17.82, 21.78)</a:t>', '<a:t> (17.81, 21.79)</a:t>')
    d.rep(77, '<a:t>(17.82, 21.78)</a:t>', '<a:t>(17.81, 21.79)</a:t>')
    for n in (75, 76):
        assert 'Target="../media/image79.png"' in d.get(d.rp(n))
    with tempfile.TemporaryDirectory() as t:
        a, b = os.path.join(t, 'in.png'), os.path.join(t, 'out.png')
        open(a, 'wb').write(d.parts['ppt/media/image79.png'])
        subprocess.run([sys.executable, os.path.join(os.path.dirname(os.path.abspath(__file__)), 'l12_solution_digits.py'), a, b], check=True)
        name = d.add_media('l12_examq_solution.png', open(b, 'rb').read())
    for n in (75, 76):
        d.put(d.rp(n), d.get(d.rp(n)).replace('Target="../media/image79.png"', f'Target="../media/{name}"'))
    d.sweep_media()


# ---------------------------------------------------------------------------------- samplesize
def item_samplesize(d):
    """Sample sizes round up (Agresti p. 419: '600.25, rounded up to 601')."""
    import subprocess, tempfile, os
    d.rep(85, '<a:t> = 384</a:t>', '<a:t> = 384.16, rounded up to 385</a:t>')
    # slide 62: 0.98^2 x 2.58^2 / 0.3^2 = 71.03 -> 72
    assert 'Target="../media/image74.png"' in d.get(d.rp(62))
    with tempfile.TemporaryDirectory() as t:
        a, b = os.path.join(t, 'in.png'), os.path.join(t, 'out.png')
        open(a, 'wb').write(d.parts['ppt/media/image74.png'])
        subprocess.run([sys.executable, os.path.join(os.path.dirname(os.path.abspath(__file__)), 'l12_n72_digit.py'), a, b], check=True)
        name = d.add_media('l12_samplesize_n72.png', open(b, 'rb').read())
    d.put(d.rp(62), d.get(d.rp(62)).replace('Target="../media/image74.png"', f'Target="../media/{name}"'))
    d.sweep_media()


# ---------------------------------------------------------------------------------- xbar
NS_MC = 'http://schemas.openxmlformats.org/markup-compatibility/2006'
NS_A14 = 'http://schemas.microsoft.com/office/drawing/2010/main'
NS_M = 'http://schemas.openxmlformats.org/officeDocument/2006/math'
PT = '/System/Library/Fonts/Supplemental/PTSans.ttc'
CAMBRIA = '<a:latin typeface="Cambria Math" panose="02040503050406030204" pitchFamily="18" charset="0"/>'
ACC = {'̄': '̅', '̂': '̂'}       # macron -> overline (as PowerPoint stores x-bar), hat
BASE = {'X': 'x', 'x': 'x', 'p': 'p', '\U0001D45D': 'p', '\U0001D465': 'x'}

def _math_rpr(rpr, default_sz):
    sz = re.search(r'sz="(\d+)"', rpr); sz = sz.group(1) if sz else str(default_sz)
    fill = re.search(r'<a:solidFill>.*?</a:solidFill>', rpr, re.S)
    return (f'<a:rPr lang="en-US" sz="{sz}" b="0" i="1" strike="noStrike" spc="-1" smtClean="0">'
            f'{fill.group(0) if fill else ""}{CAMBRIA}</a:rPr>')

def _math(items, rpr):
    """items: ('r', text) for a plain math run, ('acc', base, chr) for an accented letter (upright, as in L10)."""
    body = ''
    for it in items:
        if it[0] == 'r':
            body += f'<m:r><m:rPr><m:sty m:val="p"/></m:rPr>{rpr}<m:t>{it[1]}</m:t></m:r>'
        else:
            body += (f'<m:acc><m:accPr><m:chr m:val="{it[2]}"/><m:ctrlPr>{rpr}</m:ctrlPr></m:accPr>'
                     f'<m:e><m:r><m:rPr><m:sty m:val="p"/></m:rPr>{rpr}<m:t>{it[1]}</m:t></m:r></m:e></m:acc>')
    return f'<a14:m><m:oMath xmlns:m="{NS_M}">{body}</m:oMath></a14:m>'

SPECIAL = {   # slide 84: whole expressions as one math zone
    '\U0001D45B \U0001D45D̂  = 825 (0.7491) ≈ 618, greater than 15':
        ([('r', 'n'), ('acc', 'p', '̂')], ' = 825 (0.7491) ≈ 618, greater than 15'),
    '\U0001D45B(1−\U0001D45D̂ ) ≈ 207, greater than 15, so both OK':
        ([('r', 'n(1−'), ('acc', 'p', '̂'), ('r', ')')], ' ≈ 207, greater than 15, so both OK'),
}

def _convert_runs(block, default_sz):
    """Return (new block, plain-text paragraphs for the fallback picture, changed?)."""
    changed = False
    def conv(m):
        nonlocal changed
        r = m.group(0)
        rpr = re.search(r'<a:rPr[^>]*/>|<a:rPr[^>]*>.*?</a:rPr>', r, re.S); rpr = rpr.group(0) if rpr else '<a:rPr lang="en-US"/>'
        t = re.search(r'<a:t>([^<]*)</a:t>', r).group(1)
        t2 = re.sub(r'^S = (\d)', r's = \1', t)
        if t2 in SPECIAL:
            items, tail = SPECIAL[t2]; changed = True
            return _math(items, _math_rpr(rpr, default_sz)) + f'<a:r>{rpr}<a:t>{tail}</a:t></a:r>'
        if not re.search('[̄̂]', t2):
            if t2 != t: changed = True
            return r.replace(f'<a:t>{t}</a:t>', f'<a:t>{t2}</a:t>')
        changed = True
        out = ''
        for k, piece in enumerate(re.split('(.[̄̂])', t2)):
            if not piece: continue
            if k % 2:
                out += _math([('acc', BASE[piece[0]], ACC[piece[1]])], _math_rpr(rpr, default_sz))
            else:
                out += f'<a:r>{rpr}<a:t>{piece.replace(chr(0x1D45B), "n")}</a:t></a:r>'
        return out
    new = re.sub(r'<a:r>(?:(?!</a:r>).)*?<a:t>[^<]*</a:t></a:r>', conv, block, flags=re.S)
    return new, changed

def _plain_paras(block):
    paras = []
    for p in re.findall(r'<a:p>.*?</a:p>', block, re.S):
        txt = ''
        for m in re.finditer(r'<a:r>.*?</a:r>|<a14:m>.*?</a14:m>|<a:br/>', p, re.S):
            b = m.group(0)
            if b.startswith('<a:br'): txt += '\n'; continue
            if b.startswith('<a14:m>'):
                for mm in re.finditer(r'<m:acc>.*?</m:acc>|<m:r>.*?</m:r>', b, re.S):
                    tt = re.search(r'<m:t>([^<]*)</m:t>', mm.group(0)).group(1)
                    if mm.group(0).startswith('<m:acc>'):
                        tt += {'̅': '̄'}.get(re.search(r'm:chr m:val="(.)"', mm.group(0)).group(1), re.search(r'm:chr m:val="(.)"', mm.group(0)).group(1))
                    txt += tt
                continue
            txt += ''.join(re.findall(r'<a:t>([^<]*)</a:t>', b))
        sz = re.search(r'sz="(\d+)"', p)
        bullet = '<a:buChar' in p or '<a:buAutoNum' in p
        paras.append((txt, int(sz.group(1)) / 100 if sz else 18, bullet))
    return paras

def _fallback_png(box, fill_hex, paras, scale=1.0, dpi=150):
    from PIL import Image, ImageDraw, ImageFont
    W, H = max(1, round(box[2] * dpi)), max(1, round(box[3] * dpi))
    bg = (int(fill_hex[0:2], 16), int(fill_hex[2:4], 16), int(fill_hex[4:6], 16), 255) if fill_hex else (255, 255, 255, 0)
    im = Image.new('RGBA', (W, H), bg); d = ImageDraw.Draw(im); y = 0.05 * dpi
    for txt, pt, bullet in paras:
        f = ImageFont.truetype(PT, max(6, round(pt * scale / 72 * dpi)))
        for hard in (txt or ' ').split('\n'):
            words, cur, lines = hard.replace('\t', '    ').split(' '), '', []
            for w in words:
                trial = (cur + ' ' + w) if cur else w
                if f.getlength(trial) > W - 0.3 * dpi and cur: lines.append(cur); cur = w
                else: cur = trial
            lines.append(cur)
            for k, ln in enumerate(lines):
                xx = 0.1 * dpi
                ln = ('• ' if bullet and k == 0 else '  ' if bullet else '') + ln
                # PT Sans has no combining accents: draw the base letter, then the bar / hat by hand
                for seg in re.split('(.[̄̅̂])', ln):
                    if not seg: continue
                    if len(seg) == 2 and seg[1] in '̄̅̂':
                        d.text((xx, y), seg[0], font=f, fill='black')
                        bx0, by0, bx1, _ = d.textbbox((xx, y), seg[0], font=f)
                        lw = max(1, round(f.size / 18)); yb = by0 - 2 * lw
                        if seg[1] == '̂':
                            mx = (bx0 + bx1) / 2
                            d.line([(bx0 + lw, yb + 2 * lw), (mx, yb - lw), (bx1 - lw, yb + 2 * lw)], fill='black', width=lw)
                        else:
                            d.line([(bx0, yb), (bx1, yb)], fill='black', width=lw)
                        xx += f.getlength(seg[0])
                    else:
                        d.text((xx, y), seg, font=f, fill='black'); xx += f.getlength(seg)
                y += pt * scale / 72 * dpi * 1.25
    buf = io.BytesIO(); im.save(buf, 'PNG'); return buf.getvalue()

def _fallback_sp(block, rid):
    """PowerPoint's own fallback: same id/name/geometry, picture fill, locked, one empty run."""
    sid = re.search(r'<p:cNvPr id="(\d+)"', block).group(1); name = re.search(r'<p:cNvPr id="\d+" name="([^"]*)"', block).group(1)
    xfrm = re.search(r'<a:xfrm.*?</a:xfrm>', block, re.S).group(0)
    return (f'<p:sp><p:nvSpPr><p:cNvPr id="{sid}" name="{name}"/><p:cNvSpPr><a:spLocks noRot="1" noChangeAspect="1" noMove="1" noResize="1" noEditPoints="1" noAdjustHandles="1" noChangeArrowheads="1" noChangeShapeType="1" noTextEdit="1"/></p:cNvSpPr><p:nvPr/></p:nvSpPr>'
            f'<p:spPr>{xfrm}<a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:blipFill><a:blip r:embed="{rid}"/><a:stretch><a:fillRect/></a:stretch></a:blipFill></p:spPr>'
            '<p:txBody><a:bodyPr/><a:lstStyle/><a:p><a:r><a:rPr lang="en-US"><a:noFill/></a:rPr><a:t> </a:t></a:r></a:p></p:txBody></p:sp>')

def item_xbar(d):
    total = 0
    for n in range(1, len(d.slide_files()) + 1):
        x = d.get(d.sp(n))
        if not re.search(r'<a:t>[^<]*[̄̂][^<]*</a:t>|<a:t>S = \d', x): continue
        # top-level shapes only, never inside an existing AlternateContent
        spans = [(m.start(), m.end()) for m in re.finditer(r'<mc:AlternateContent.*?</mc:AlternateContent>', x, re.S)]
        new_x, k = x, 0
        for m in re.finditer(r'<p:sp>.*?</p:sp>', x, re.S):
            if any(a <= m.start() < b for a, b in spans): continue
            blk = m.group(0)
            if not re.search(r'<a:t>[^<]*[̄̂][^<]*</a:t>|<a:t>S = \d', blk): continue
            default_sz = 1800
            choice, changed = _convert_runs(blk, default_sz)
            assert changed
            if '<a14:m>' not in choice:                 # only S -> s: plain text edit
                new_x = new_x.replace(blk, choice, 1); k += 1; continue
            g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/>\s*<a:ext cx="(\d+)" cy="(\d+)"/>', blk)
            box = [int(v) / EMU for v in g.groups()]
            fill = re.search(r'<p:spPr>(?:(?!<a:ln).)*?<a:solidFill><a:srgbClr val="(\w{6})"', blk, re.S)
            fs = re.search(r'fontScale="(\d+)"', blk)
            png = _fallback_png(box, fill.group(1) if fill else None, _plain_paras(choice), int(fs.group(1)) / 1e5 if fs else 1.0)
            sid = re.search(r'<p:cNvPr id="(\d+)"', blk).group(1)
            rid = d.add_rel(n, d.add_media(f'l12_eq_s{n}_{sid}.png', png))
            wrapped = (f'<mc:AlternateContent xmlns:mc="{NS_MC}" xmlns:a14="{NS_A14}"><mc:Choice Requires="a14">{choice}</mc:Choice>'
                       f'<mc:Fallback>{_fallback_sp(blk, rid)}</mc:Fallback></mc:AlternateContent>')
            new_x = new_x.replace(blk, wrapped, 1); k += 1
            d.put(d.sp(n), new_x)   # add_rel reads the rels, slide xml is written at the end
        d.put(d.sp(n), new_x)
        left = re.findall(r'<a:t>([^<]*[̄̂][^<]*)</a:t>', re.sub(r'<mc:Fallback>.*?</mc:Fallback>', '', new_x, flags=re.S))
        assert not left, (n, left)
        total += k
        print(f'  slide {n}: {k} shape(s)')
    print('  shapes changed:', total)


# ---------------------------------------------------------------------------------- fit
SLIDE_W, SLIDE_BOTTOM = 13.333, 7.45      # inches; keep boxes 0.05 in clear of the bottom edge

def _geom(block):
    g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/>\s*<a:ext cx="(\d+)" cy="(\d+)"/>', block)
    return g, [int(v) / EMU for v in g.groups()]

def _set_geom(block, x, y, cx, cy):
    g, _ = _geom(block)
    return block.replace(g.group(0), f'<a:off x="{round(x * EMU)}" y="{round(y * EMU)}"/><a:ext cx="{round(cx * EMU)}" cy="{round(cy * EMU)}"/>', 1)

def _need(block):
    import fitcheck
    m = fitcheck.measure_shape(re.sub(r'<mc:Fallback>.*?</mc:Fallback>', '', block, flags=re.S), 18)
    l, t, r, b = m['ins']
    return m['widest'] + l + r + 0.06, m['total_h'] + t + b + 0.04

def _fit_box(d, n, name, anchor, left_limit=0.0, right_limit=SLIDE_W - 0.03):
    """Grow a box until its text fits. anchor: 'left' keeps x, 'right' keeps the right edge,
    'center' keeps the centre; never past left_limit / right_limit, never below SLIDE_BOTTOM."""
    s, blk = d.shape_block(n, name)
    _, (x, y, cx, cy) = _geom(blk)
    need_w, need_h = _need(blk)
    cx2, cy2 = max(cx, need_w), max(cy, need_h)
    if anchor == 'left': x2 = x
    elif anchor == 'right': x2 = x + cx - cx2
    else: x2 = x + (cx - cx2) / 2
    x2 = max(x2, left_limit); x2 = min(x2, right_limit - cx2)
    assert x2 >= left_limit - 1e-6, (n, name, 'no room', x2, left_limit)
    y2 = min(y, SLIDE_BOTTOM - cy2)
    d.put(d.sp(n), s.replace(blk, _set_geom(blk, x2, y2, cx2, cy2)))
    print(f'  slide {n} {name}: {cx:.2f} x {cy:.2f} -> {cx2:.2f} x {cy2:.2f} in at ({x2:.2f}, {y2:.2f})')

def _split_84(d):
    """8.68 answer: one centred box of 21 lines ran off the slide. Two top-anchored columns under
    the title: 'Given' + conditions left, parts a-c right. Text unchanged; empty paragraphs dropped."""
    n = 84
    x = d.get(d.sp(n))
    ac = [m.group(0) for m in re.finditer(r'<mc:AlternateContent.*?</mc:AlternateContent>', x, re.S) if 'name="CustomShape 5"' in m.group(0)]
    assert len(ac) == 1
    choice = re.search(r'<mc:Choice[^>]*>(.*?)</mc:Choice>', ac[0], re.S).group(1)
    paras = re.findall(r'<a:p>.*?</a:p>', choice, re.S)
    txt = lambda p: ''.join(re.findall(r'<(?:a|m):t>([^<]*)</(?:a|m):t>', p)).strip()
    assert txt(paras[2]) == 'Given:' and txt(paras[10]).startswith('Standard error') and txt(paras[12]).startswith('b. Margin')
    left, right = [p for p in paras[2:9] if txt(p)], [p for p in paras[10:] if txt(p)]
    body = re.search(r'(<p:txBody><a:bodyPr[^>]*)(/>|>.*?</a:bodyPr>)(<a:lstStyle/>|<a:lstStyle>.*?</a:lstStyle>)', choice, re.S)
    head = body.group(1).replace('anchor="ctr"', 'anchor="t"') + body.group(2) + body.group(3)
    pre = choice[:choice.index('<p:txBody>')]
    ids = [int(i) for i in re.findall(r'<p:cNvPr id="(\d+)"', x)]
    new_id = max(ids) + 1
    out = ''
    for col, (ps, box) in enumerate(((left, (0.67, 1.35, 6.05, 5.6)), (right, (6.85, 1.35, 6.05, 5.6)))):
        sp = pre + head + ''.join(ps) + '</p:txBody></p:sp>'
        if col == 1:
            sp = re.sub(r'<p:cNvPr id="\d+" name="CustomShape 5"', f'<p:cNvPr id="{new_id}" name="CustomShape 5 (parts a-c)"', sp, count=1)
        sp = _set_geom(sp, *box)
        png = _fallback_png(box, None, _plain_paras(sp))
        rid = d.add_rel(n, d.add_media(f'l12_eq_s84_col{col + 1}.png', png))
        out += (f'<mc:AlternateContent xmlns:mc="{NS_MC}" xmlns:a14="{NS_A14}"><mc:Choice Requires="a14">{sp}</mc:Choice>'
                f'<mc:Fallback>{_fallback_sp(sp, rid)}</mc:Fallback></mc:AlternateContent>')
    d.put(d.sp(n), d.get(d.sp(n)).replace(ac[0], out))
    d.drop_unused_rels(n)
    print(f'  slide 84: answer split into two columns ({len(left)} + {len(right)} paragraphs)')

def item_fit(d):
    d.rep(72, '<a:t>A 95% confidence intervals means', '<a:t>A 95% confidence interval means')
    d.rep(77, '<a:t>An educational psychologist studies school absence of children in the Netherlands She has a sample of 41 children. ',
              '<a:t>An educational psychologist studies school absence of children in the Netherlands. She has a sample of 41 children. ')
    # filled boxes whose text spills, and the CLT link; limits keep clear of the neighbouring boxes
    _fit_box(d, 44, 'CustomShape 38', 'right', left_limit=10.08 + 0.04)
    _fit_box(d, 45, 'CustomShape 33', 'right', left_limit=10.06 + 0.04)
    _fit_box(d, 46, 'CustomShape 9', 'right', left_limit=10.06 + 0.04)
    _fit_box(d, 50, 'CustomShape 4', 'left', right_limit=6.58 - 0.05)
    _fit_box(d, 55, 'CustomShape 4', 'right')
    _fit_box(d, 59, 'CustomShape 3', 'center')
    _fit_box(d, 72, 'CustomShape 3', 'center')
    _split_84(d)
    # render check: on 17 the plot covered the 'is' of 'sample mean x-bar is' (PT Sans sets the line
    # wider than Calibri did; on 16 it just touched). Move the plot, not the text: 12-17 keep one wrap.
    for n, pic_name in ((16, 'Picture 713'), (17, 'Picture 717')):
        s_, pic = d.shape_block(n, pic_name, 'p:pic')
        _, (x, y, cx, cy) = _geom(pic)
        d.put(d.sp(n), s_.replace(pic, _set_geom(pic, x + 0.15, y, cx, cy)))
    print('  slides 16-17: plot 0.15 in to the right, clear of "is"')
    # 26: 'Standard deviation of sampling distribution is' now runs under the sigma/sqrt(n) picture
    s_, pic = d.shape_block(26, 'Picture 746', 'p:pic')
    _, (x, y, cx, cy) = _geom(pic)
    d.put(d.sp(26), s_.replace(pic, _set_geom(pic, 7.84, y, cx, cy)))
    print(f'  slide 26: sigma/sqrt(n) picture {x:.2f} -> 7.84 in, clear of "is"')
    d.sweep_media()


ITEMS = {'recap': item_recap, 'excel': item_excel, 'examq': item_examq, 'samplesize': item_samplesize,
         'xbar': item_xbar, 'fit': item_fit}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

"""Find text that PowerPoint actually rendered outside its box, from an exported PDF.

    python3 rendercheck.py <deck.pptx> <deck.pdf> [--png DIR] [--tol PT] [--all] [slides...]

fitcheck.py estimates line widths from font metrics; this checks the real layout.
PowerPoint writes shapes into the PDF in z-order, so the text of each shape (in
z-order, equations and groups included) is aligned against the PDF's words in
content-stream order. Each shape's rendered words are then compared with its box:

  SPILL text extends past a filled box (the coloured box no longer holds it)
  OFF   text runs off the slide
  HIT   the visible text of two different shapes overlaps
  HIDDEN text sits under a later opaque shape or picture (leftovers stay in the PDF text)
  OUT   text extends past an unfilled box: invisible unless it hits something, so
        only listed with --all

Hidden slides are skipped (they are not in a PDF export). With --png, every
flagged page is rendered with the shape box (blue) and its text extent (red).
"""
import os, re, subprocess, sys, unicodedata, zipfile, difflib, html
from lxml import etree

NS = {'p': 'http://schemas.openxmlformats.org/presentationml/2006/main',
      'a': 'http://schemas.openxmlformats.org/drawingml/2006/main',
      'r': 'http://schemas.openxmlformats.org/officeDocument/2006/relationships',
      'mc': 'http://schemas.openxmlformats.org/markup-compatibility/2006',
      'm': 'http://schemas.openxmlformats.org/officeDocument/2006/math'}
EMU_PT = 12700.0
q = lambda t: '{%s}%s' % (NS[t.split(':')[0]], t.split(':')[1])


def norm(tok):
    t = unicodedata.normalize('NFKC', tok).casefold()
    return ''.join(c for c in t if c.isalnum())


class Deck:
    def __init__(self, path):
        self.z = zipfile.ZipFile(path)
        pres = self.xml('ppt/presentation.xml')
        rels = self.rels('ppt/presentation.xml')
        self.slides = [rels[s.get(q('r:id'))] for s in pres.find(q('p:sldIdLst'))]
        sz = pres.find(q('p:sldSz'))
        self.W, self.H = int(sz.get('cx')) / EMU_PT, int(sz.get('cy')) / EMU_PT

    def xml(self, p): return etree.fromstring(self.z.read(p))

    def rels(self, part):
        d, f = part.rsplit('/', 1)
        try: x = self.xml(f'{d}/_rels/{f}.rels')
        except KeyError: return {}
        out = {}
        for r in x:
            t = r.get('Target')
            if r.get('TargetMode') == 'External': continue
            parts = (d + '/' + t).split('/') if not t.startswith('/') else t.lstrip('/').split('/')
            stack = []
            for s in parts:
                if s == '..': stack.pop()
                elif s and s != '.': stack.append(s)
            out[r.get('Id')] = '/'.join(stack)
        return out

    def related(self, part, kind):
        x = self.xml(part.rsplit('/', 1)[0] + '/_rels/' + part.rsplit('/', 1)[1] + '.rels')
        for r in x:
            if r.get('Type').endswith('/' + kind):
                return self.rels(part)[r.get('Id')]


def ph_key(sp):
    ph = sp.find('.//' + q('p:nvPr') + '/' + q('p:ph'))
    if ph is None: return None
    t = ph.get('type', 'body'); t = 'title' if t == 'ctrTitle' else t
    return t, ph.get('idx')


def inherited_xfrm(deck, slide_part, key):
    layout = deck.related(slide_part, 'slideLayout')
    for part in (layout, deck.related(layout, 'slideMaster')):
        x = deck.xml(part)
        for sp in x.iter(q('p:sp')):
            k = ph_key(sp)
            if k and (k[0] == key[0] and (key[1] is None or k[1] == key[1] or key[0] in ('title', 'sldNum', 'dt', 'ftr'))):
                off = sp.find(q('p:spPr') + '/' + q('a:xfrm'))
                if off is not None: return off
        for sp in x.iter(q('p:sp')):   # fall back on type only
            k = ph_key(sp)
            if k and k[0] == key[0]:
                off = sp.find(q('p:spPr') + '/' + q('a:xfrm'))
                if off is not None: return off
    return None


def shapes(deck, slide_part):
    """(name, box_pt, tokens, info) for every text shape, in z-order; info['z'] is the drawing
    order and deck.occluders[slide_part] lists (z, name, box) of opaque shapes and pictures."""
    root = deck.xml(slide_part)
    out, occ, z = [], [], [0]

    def walk(container, tf):
        for el in container:
            tag = etree.QName(el).localname
            if tag == 'AlternateContent':
                ch = el.find(q('mc:Choice'))
                walk(ch if ch is not None else el.find(q('mc:Fallback')), tf); continue
            if tag == 'grpSp':
                x = el.find(q('p:grpSpPr') + '/' + q('a:xfrm'))
                o, e = x.find(q('a:off')), x.find(q('a:ext'))
                co, ce = x.find(q('a:chOff')), x.find(q('a:chExt'))
                sx = int(e.get('cx')) / max(1, int(ce.get('cx'))); sy = int(e.get('cy')) / max(1, int(ce.get('cy')))
                ox, oy = int(o.get('x')) - int(co.get('x')) * sx, int(o.get('y')) - int(co.get('y')) * sy
                walk(el, lambda X, Y, CX, CY, tf=tf: tf(ox + X * sx, oy + Y * sy, CX * sx, CY * sy)); continue
            if tag not in ('sp', 'pic'): continue
            z[0] += 1
            xf0 = el.find(q('p:spPr') + '/' + q('a:xfrm'))
            sppr = el.find(q('p:spPr'))
            opaque = tag == 'pic' or (sppr is not None and any(sppr.find(q(k)) is not None for k in ('a:solidFill', 'a:gradFill', 'a:blipFill')))
            if opaque and xf0 is not None:
                o0, e0 = xf0.find(q('a:off')), xf0.find(q('a:ext'))
                bx = tf(int(o0.get('x')), int(o0.get('y')), int(e0.get('cx')), int(e0.get('cy')))
                occ.append((z[0], el.find('.//' + q('p:cNvPr')).get('name'), tuple(v / EMU_PT for v in bx)))
            tx = el.find(q('p:txBody'))
            if tag == 'pic' or tx is None: continue
            toks = []
            for p in tx.iter(q('a:p')):
                for node in p.iter():
                    ln = etree.QName(node).localname
                    if ln == 't' and node.text: toks += node.text.split()
            toks = [t for t in toks if norm(t)]
            if not toks: continue
            xf = el.find(q('p:spPr') + '/' + q('a:xfrm'))
            if xf is None:
                key = ph_key(el)
                xf = inherited_xfrm(deck, slide_part, key) if key else None
            if xf is None: continue
            o, e = xf.find(q('a:off')), xf.find(q('a:ext'))
            X, Y, CX, CY = tf(int(o.get('x')), int(o.get('y')), int(e.get('cx')), int(e.get('cy')))
            bp = tx.find(q('a:bodyPr'))
            info = {'wrap': bp.get('wrap', 'square') if bp is not None else 'square',
                    'auto': next((etree.QName(c).localname for c in bp if etree.QName(c).localname.endswith('Autofit') or etree.QName(c).localname == 'spAutoFit'), None) if bp is not None else None,
                    'ph': ph_key(el),
                    'filled': el.find(q('p:spPr') + '/' + q('a:solidFill')) is not None, 'z': z[0]}
            name = el.find('.//' + q('p:cNvPr')).get('name')
            out.append((name, (X / EMU_PT, Y / EMU_PT, CX / EMU_PT, CY / EMU_PT), toks, info))

    walk(root.find('.//' + q('p:spTree')), lambda X, Y, CX, CY: (X, Y, CX, CY))
    return out, occ


def pdf_words(pdf, page):
    h = subprocess.run(['pdftotext', '-raw', '-bbox', '-f', str(page), '-l', str(page), pdf, '-'],
                       capture_output=True, text=True, check=True).stdout
    return [(html.unescape(m.group(5)), float(m.group(1)), float(m.group(2)), float(m.group(3)), float(m.group(4)))
            for m in re.finditer(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">(.*?)</word>', h)]


def covered(word, z, occ):
    """Name of a later opaque shape whose box holds the word's centre, else None."""
    cx, cy = (word[1] + word[3]) / 2, (word[2] + word[4]) / 2
    for oz, name, (X, Y, CX, CY) in occ:
        if oz > z and X <= cx <= X + CX and Y <= cy <= Y + CY: return name
    return None


def check_slide(deck, slide_part, pdf, page, tol, show_all=False):
    shp, occ = shapes(deck, slide_part)
    words = [w for w in pdf_words(pdf, page) if norm(w[0])]
    a = [(norm(t), i) for i, s in enumerate(shp) for t in s[2]]
    b = [norm(w[0]) for w in words]
    sm = difflib.SequenceMatcher(None, [x[0] for x in a], b, autojunk=False)
    owner = {}
    for blk in sm.get_matching_blocks():
        for k in range(blk.size):
            owner[blk.b + k] = a[blk.a + k][1]
    ext = {}
    for wi, si in owner.items():
        _, x0, y0, x1, y1 = words[wi]
        e = ext.setdefault(si, [x0, y0, x1, y1, 0])
        e[0], e[1], e[2], e[3], e[4] = min(e[0], x0), min(e[1], y0), max(e[2], x1), max(e[3], y1), e[4] + 1
    issues = []
    for si, (x0, y0, x1, y1, n) in ext.items():
        name, (X, Y, CX, CY), toks, info = shp[si]
        if n < max(1, len(toks) // 3): continue           # too little of it found to judge
        over = {'left': X - x0, 'right': x1 - (X + CX), 'top': Y - y0, 'bottom': y1 - (Y + CY)}
        bad = {k: v for k, v in over.items() if v > tol}
        if bad and (info['filled'] or show_all):
            note = f"[{info['auto']}]" if info['auto'] in ('spAutoFit', 'normAutofit') else ''
            issues.append(('SPILL' if info['filled'] else 'OUT', name,
                           ', '.join(f'{k} {v / 72:.2f}in' for k, v in bad.items()) + (' ' + note if note else ''),
                           ' '.join(toks)[:60], (X, Y, CX, CY), (x0, y0, x1, y1)))
        if x0 < -tol or y0 < -tol or x1 > deck.W + tol or y1 > deck.H + tol:
            issues.append(('OFF', name, f'text spans {x0 / 72:.2f}-{x1 / 72:.2f} x {y0 / 72:.2f}-{y1 / 72:.2f}in on a {deck.W / 72:.2f} x {deck.H / 72:.2f}in slide',
                           ' '.join(toks)[:60], (X, Y, CX, CY), (x0, y0, x1, y1)))
    # text of two different shapes overlapping (word level)
    ws, hidden = {}, {}
    for wi, si in owner.items():
        w = words[wi]; by = covered(w, shp[si][3]['z'], occ)
        if by: hidden.setdefault((si, by), []).append(w[0])
        else: ws.setdefault(si, []).append(w)
    for (si, by), hw in hidden.items():
        issues.append(('HIDDEN', shp[si][0], f'{len(hw)} word(s) under {by}: "{" ".join(hw)[:40]}"', '', None, None))
    keys = sorted(ws)
    for i in range(len(keys)):
        for j in range(i + 1, len(keys)):
            hit = None
            for w1 in ws[keys[i]]:
                for w2 in ws[keys[j]]:
                    ix = min(w1[3], w2[3]) - max(w1[1], w2[1]); iy = min(w1[4], w2[4]) - max(w1[2], w2[2])
                    if ix > 1.5 and iy > 1.5: hit = (w1[0], w2[0]); break
                if hit: break
            if hit:
                issues.append(('HIT', f'{shp[keys[i]][0]} x {shp[keys[j]][0]}', f'"{hit[0]}" overlaps "{hit[1]}"', '', None, None))
    return issues, shp, ext


def main(argv):
    deck_path, pdf = argv[0], argv[1]
    png_dir = argv[argv.index('--png') + 1] if '--png' in argv else None
    tol = float(argv[argv.index('--tol') + 1]) if '--tol' in argv else 2.5
    show_all = '--all' in argv
    only = {int(x) for x in argv[2:] if x.isdigit() and argv[argv.index(x) - 1] not in ('--tol', '--png')}
    deck = Deck(deck_path)
    page, total = 0, 0
    for n, part in enumerate(deck.slides, 1):
        if deck.xml(part).get('show') == '0': continue
        page += 1
        if only and n not in only: continue
        issues, shp, ext = check_slide(deck, part, pdf, page, tol, show_all)
        for kind, name, what, txt, box, tb in issues:
            print(f'slide {n:2d} {kind} {name:22s} {what}' + (f' | {txt}' if txt else ''))
        total += len(issues)
        if issues and png_dir:
            from PIL import Image, ImageDraw
            os.makedirs(png_dir, exist_ok=True)
            base = os.path.join(png_dir, f'slide{n:02d}')
            subprocess.run(['pdftoppm', '-r', '72', '-f', str(page), '-l', str(page), '-png', '-singlefile', pdf, base], check=True)
            im = Image.open(base + '.png').convert('RGB'); d = ImageDraw.Draw(im)
            for kind, name, what, txt, box, tb in issues:
                if box: d.rectangle([box[0], box[1], box[0] + box[2], box[1] + box[3]], outline=(40, 90, 255), width=2)
                if tb: d.rectangle(tb[:4], outline=(230, 30, 30), width=2)
            im.save(base + '.png')
    print(f'{total} issue(s)' + (f'; marked pages in {png_dir}' if png_dir and total else ''))


if __name__ == '__main__':
    main(sys.argv[1:])

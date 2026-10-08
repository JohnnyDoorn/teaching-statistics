"""Flag text that does not fit its box in a .pptx (PT Sans metrics, word wrap).

Usage: python3 fitcheck.py <deck.pptx> [slide numbers...]
Prints one line per suspect box: slide position, shape name, the problem, the text.
Approximate on purpose: it errs towards flagging. Titles (placeholders) are skipped.
"""
import re, sys, zipfile
from PIL import ImageFont

PT = '/System/Library/Fonts/Supplemental/PTSans.ttc'
_fonts = {}
def font(sz_pt, italic=False, bold=False):
    idx = {(False, False): 0, (True, False): 1, (False, True): 2, (True, True): 3}[(italic, bold)]
    key = (round(sz_pt * 10), idx)
    if key not in _fonts:
        try: _fonts[key] = ImageFont.truetype(PT, max(1, round(sz_pt * 10)), index=idx)
        except OSError: _fonts[key] = ImageFont.truetype(PT, max(1, round(sz_pt * 10)))
    return _fonts[key]
EMU = 914400

def runs_of(p):
    """(text, size_pt, italic, bold) for each run / field / math zone in a paragraph."""
    out = []
    for m in re.finditer(r'<a:r>.*?</a:r>|<a:fld .*?</a:fld>|<a14:m>.*?</a14:m>|<a:br/>|<a:br>.*?</a:br>', p, re.S):
        b = m.group(0)
        if b.startswith('<a:br'): out.append(('\n', None, False, False)); continue
        if b.startswith('<a14:m>'):
            t = ''.join(re.findall(r'<m:t>([^<]*)</m:t>', b)); sz = re.findall(r'sz="(\d+)"', b)
            out.append((t, int(sz[0]) / 100 if sz else None, True, False)); continue
        t = ''.join(re.findall(r'<a:t>([^<]*)</a:t>', b))
        rpr = re.search(r'<a:rPr[^>]*>', b); rpr = rpr.group(0) if rpr else ''
        sz = re.search(r'sz="(\d+)"', rpr)
        out.append((t, int(sz.group(1)) / 100 if sz else None, ' i="1"' in rpr, ' b="1"' in rpr))
    return out

def unesc(t): return t.replace('&lt;', '<').replace('&gt;', '>').replace('&amp;', '&').replace('&quot;', '"').replace('&apos;', "'")

def check_shape(b, default_sz):
    g = re.search(r'<a:off x="(-?\d+)" y="(-?\d+)"/>\s*<a:ext cx="(\d+)" cy="(\d+)"/>', b)
    if not g or '<p:txBody>' not in b: return None
    cx, cy = int(g.group(3)) / EMU, int(g.group(4)) / EMU
    bp = re.search(r'<a:bodyPr[^>]*>', b); bp = bp.group(0) if bp else ''
    ins = lambda k, d: int(re.search(rf'{k}="(\d+)"', bp).group(1)) / EMU if re.search(rf'{k}="(\d+)"', bp) else d
    l, r, t, bt = ins('lIns', 0.1), ins('rIns', 0.1), ins('tIns', 0.05), ins('bIns', 0.05)
    nowrap = 'wrap="none"' in bp
    auto = re.search(r'<a:normAutofit(?: fontScale="(\d+)")?(?: lnSpcReduction="(\d+)")?', b)
    scale = int(auto.group(1)) / 1e5 if auto and auto.group(1) else 1.0
    lnred = int(auto.group(2)) / 1e5 if auto and auto.group(2) else 0.0
    spauto = '<a:spAutoFit/>' in b
    filled = bool(re.search(r'<p:spPr>(?:(?!<a:ln).)*<a:(solidFill|blipFill|gradFill)', b, re.S))
    W = cx - l - r; H = cy - t - bt
    total_h, widest, text_all = 0.0, 0.0, []
    for p in re.findall(r'<a:p>.*?</a:p>', b, re.S):
        ppr = re.search(r'<a:pPr[^>]*>', p); ppr = ppr.group(0) if ppr else ''
        marL = int(re.search(r'marL="(-?\d+)"', ppr).group(1)) / EMU if 'marL=' in ppr else 0
        lnsp = re.search(r'<a:lnSpc><a:spcPct val="(\d+)"', p); lnsp = int(lnsp.group(1)) / 1e5 if lnsp else 1.0
        bef = re.search(r'<a:spcBef><a:spcPts val="(\d+)"', p); bef = int(bef.group(1)) / 100 / 72 if bef else 0
        rs = runs_of(p)
        end_sz = re.search(r'<a:endParaRPr[^>]*sz="(\d+)"', p)
        psz = next((s for _, s, _, _ in rs if s), int(end_sz.group(1)) / 100 if end_sz else default_sz) * scale
        # lay out words
        lines, cur = [], 0.0
        for txt, sz, it, bo in rs:
            if txt == '\n': lines.append(cur); cur = 0.0; continue
            f = font((sz or psz / scale) * scale, it, bo)
            for k, piece in enumerate(re.split(r'( )', unesc(txt))):
                if not piece: continue
                w = f.getlength(piece.replace('\t', '    ')) / 10 / 72
                if not nowrap and piece != ' ' and cur + w > W - marL and cur > 0:
                    lines.append(cur); cur = 0.0
                cur += w
        lines.append(cur)
        text_all.append(''.join(unesc(x[0]) for x in rs))
        widest = max(widest, max(lines) + marL)
        line_h = psz * 1.2 * lnsp * (1 - lnred) / 72
        total_h += bef + line_h * (len(lines) if any(x[0].strip() for x in rs) else 1)
    probs = []
    if nowrap and widest > W * 1.02: probs.append(f'line {widest:.2f}in > box {W:.2f}in (no wrap){" FILLED" if filled else ""}')
    if not nowrap and widest > W * 1.02: probs.append(f'word wider than box ({widest:.2f} > {W:.2f}in)')
    if not spauto and total_h > H * 1.04 and (filled or not auto): probs.append(f'text {total_h:.2f}in tall > box {H:.2f}in')
    if spauto and total_h > H * 1.15 and filled: probs.append(f'auto-fit box not regrown: text {total_h:.2f}in > {H:.2f}in FILLED')
    return probs, ' / '.join(x for x in text_all if x.strip())[:90]

def main(path, only):
    z = zipfile.ZipFile(path)
    pres = z.read('ppt/presentation.xml').decode(); prels = z.read('ppt/_rels/presentation.xml.rels').decode()
    rid2f = dict(re.findall(r'Id="(rId\d+)"[^>]*Target="slides/(slide\d+\.xml)"', prels))
    order = [rid2f[r] for r in re.findall(r'<p:sldId [^>]*r:id="(rId\d+)"', pres)]
    for pos, f in enumerate(order, 1):
        if only and pos not in only: continue
        x = z.read('ppt/slides/' + f).decode()
        x = re.sub(r'<mc:Fallback>.*?</mc:Fallback>', '', x, flags=re.S)   # check the Choice, not the picture fallback
        for m in re.finditer(r'<p:sp>.*?</p:sp>', x, re.S):
            b = m.group(0)
            if '<p:ph' in b[:800]: continue
            res = check_shape(b, 18)
            if res and res[0]:
                name = re.search(r'name="([^"]*)"', b).group(1)
                print('slide %2d %-22s %s | %s' % (pos, name, '; '.join(res[0]), res[1]))

if __name__ == '__main__':
    main(sys.argv[1], {int(a) for a in sys.argv[2:]})

# -*- coding: utf-8 -*-
"""Re-base every slide of the extracted deck onto the RMS 26/27 layouts."""
import os, re, sys, zipfile
from lxml import etree
import tpl

WORK, DST = "work", sys.argv[1]
A = "http://schemas.openxmlformats.org/drawingml/2006/main"
P = "http://schemas.openxmlformats.org/presentationml/2006/main"
R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
q = lambda ns, t: f"{{{ns}}}{t}"
EMU = 914400

LAY = {"Title Slide": 1, "Title and Content": 2, "Title Only": 3,
       "Section Overview": 4, "Statement": 5, "Blank": 6}
SECTION_RX = re.compile(r"overview|recap|agenda|outline|today", re.I)

parse  = lambda p: etree.parse(p).getroot()
shapes = lambda root: root.find(q(P, "cSld")).find(q(P, "spTree")).findall(q(P, "sp"))

def drawables(root):
    """Every positioned top-level element -- pictures, groups and tables too, not
    just <p:sp>.  A title snapped on top of a picture disappears behind it."""
    tree = root.find(q(P, "cSld")).find(q(P, "spTree"))
    out = []
    for tag in ("sp", "pic", "grpSp", "graphicFrame"):
        out.extend(tree.findall(q(P, tag)))
    return out
txt    = lambda sp: "".join(t.text or "" for t in sp.iter(q(A, "t")))

def off(sp):
    x = sp.find(f'{q(P,"spPr")}/{q(A,"xfrm")}/{q(A,"off")}')
    e = sp.find(f'{q(P,"spPr")}/{q(A,"xfrm")}/{q(A,"ext")}')
    return None if x is None or e is None else (
        int(x.get("x")), int(x.get("y")), int(e.get("cx")), int(e.get("cy")))

def max_sz(sp):
    szs = [int(rp.get("sz")) for rp in sp.iter(q(A, "rPr")) if rp.get("sz")]
    return max(szs) if szs else 0

def centred(sp):
    return any(pp.get("algn") == "ctr" for pp in sp.iter(q(A, "pPr")))

def ph_type(sp):
    ph = sp.find(f'{q(P,"nvSpPr")}/{q(P,"nvPr")}/{q(P,"ph")}')
    return None if ph is None else (ph.get("type") or "body")

def find_title(root):
    """A title is wide, near the top, starts left, and is set large -- or is
    already a real title placeholder inherited from an older template."""
    best = None
    for sp in shapes(root):
        o = off(sp)
        if not o or len(txt(sp).strip()) < 3: continue
        if o[1] >= int(2.2 * EMU) or o[2] < int(6.0 * EMU) or o[0] > int(3.5 * EMU): continue
        if max_sz(sp) < 3200 and ph_type(sp) not in ("title", "ctrTitle"): continue
        if best is None or (o[1], o[0]) < (off(best)[1], off(best)[0]): best = sp
    return best

HOUSE = (838080, 365040, 10512720, 1322640)
STATEMENT = (1219200, 2600325, 9753600, 1657350)

def hits_band(root, title, target):
    """True if snapping the title to the house box would land on other content.

    Only the left ~6.6in is checked: that is where title text actually sits, so a
    figure or callout parked on the right of the same band is not a collision."""
    hx, hy, tw, hh = target
    hw = min(tw, int(6.6 * EMU))
    for sp in drawables(root):
        if sp is title: continue
        o = off(sp)
        if not o: continue
        # empty autoshapes are decorative backgrounds; anything else is real content
        if sp.tag == q(P, "sp") and not txt(sp).strip(): continue
        ov_y = min(o[1] + o[3], hy + hh) - max(o[1], hy)
        if o[0] < hx + hw and o[0] + o[2] > hx and ov_y > int(0.25 * EMU):
            return True
    return False

def at_target(title, target):
    o = off(title)
    return o is not None and all(abs(a - b) < 45720 for a, b in zip(o, target))

def find_num(root):
    for sp in shapes(root):
        for f in sp.iter(q(A, "fld")):
            if f.get("type") == "slidenum": return sp
    return None

def make_ph(sp, ph_xml, strip_sz_if=None, keep_xfrm=False):
    """Turn a free text box into a placeholder: inherit position + typography."""
    nvPr = sp.find(f'{q(P,"nvSpPr")}/{q(P,"nvPr")}')
    for ch in list(nvPr): nvPr.remove(ch)
    nvPr.append(etree.fromstring(f'<root {tpl.NS}>{ph_xml}</root>'.encode())[0])
    cNvSpPr = sp.find(f'{q(P,"nvSpPr")}/{q(P,"cNvSpPr")}')
    if cNvSpPr.find(q(A, "spLocks")) is None:
        etree.SubElement(cNvSpPr, q(A, "spLocks"), noGrp="1")
    spPr = sp.find(q(P, "spPr"))
    for tag in ("xfrm", "prstGeom", "noFill", "ln"):            # inherit from layout
        if tag == "xfrm" and keep_xfrm: continue
        for el in spPr.findall(q(A, tag)): spPr.remove(el)
    for st in sp.findall(q(P, "style")): sp.remove(st)           # drop LibreOffice style ref
    for rp in list(sp.iter(q(A, "rPr"))) + list(sp.iter(q(A, "endParaRPr"))):
        for lt in rp.findall(q(A, "latin")) + rp.findall(q(A, "ea")) + rp.findall(q(A, "cs")):
            rp.remove(lt)                                       # inherit +mj-lt / +mn-lt
        if strip_sz_if and rp.get("sz") and int(rp.get("sz")) in strip_sz_if:
            del rp.attrib["sz"]
        for junk in ("strike", "spc"):
            if rp.get(junk) in ("noStrike", "-1"): rp.attrib.pop(junk, None)

def fix_body_fonts(root, title):
    """Cantarell is Linux-only: it falls back unpredictably on macOS/Windows."""
    n = 0
    for sp in shapes(root):
        if sp is title: continue
        for lt in sp.iter(q(A, "latin")):
            if lt.get("typeface") == "Cantarell":
                lt.set("typeface", "Calibri"); n += 1
    return n

# ---- slide order from presentation.xml, not filename ----
pres  = parse(os.path.join(WORK, "ppt", "presentation.xml"))
prels = parse(os.path.join(WORK, "ppt", "_rels", "presentation.xml.rels"))
rid2t = {r.get("Id"): r.get("Target") for r in prels}
order = {os.path.basename(rid2t[s.get(q(R, "id"))]): n
         for n, s in enumerate(pres.find(q(P, "sldIdLst")), 1)}

sl_dir = os.path.join(WORK, "ppt", "slides")
report, cant = [], 0
for f in sorted((x for x in os.listdir(sl_dir) if x.endswith(".xml")),
                key=lambda x: int(re.findall(r"\d+", x)[0])):
    n, path = order.get(f), os.path.join(sl_dir, f)
    root = parse(path)
    title, num = find_title(root), find_num(root)

    has_body = any(ph_type(sp) == "body" for sp in shapes(root))
    if n == 1 and title is None:                     lay = "Blank"   # bespoke title slide
    elif title is None:                              lay = "Blank"
    elif n == 1:                                     lay = "Blank"
    elif max_sz(title) >= 5400 and centred(title):   lay = "Statement"
    elif SECTION_RX.search(txt(title)):              lay = "Section Overview"
    else:                                            lay = "Title and Content"

    if lay == "Title and Content" and not has_body and title is not None:
        lay = "Title Only"

    acts = []
    if lay not in ("Blank",):
        target = STATEMENT if lay == "Statement" else HOUSE
        keep = not at_target(title, target) and hits_band(root, title, target)
        make_ph(title, '<p:ph type="title"/>',
                strip_sz_if={6000} if lay == "Statement" else {4400}, keep_xfrm=keep)
        acts.append("title!" if keep else "title")
        for sp in shapes(root):                       # match this master's body idx
            ph = sp.find(f'{q(P,"nvSpPr")}/{q(P,"nvPr")}/{q(P,"ph")}')
            if ph is not None and ph.get("type") == "body": ph.set("idx", "1")
        if num is not None:
            make_ph(num, '<p:ph type="sldNum" sz="quarter" idx="12"/>', strip_sz_if={1200})
            acts.append("num")
        else:
            tree = root.find(q(P, "cSld")).find(q(P, "spTree"))
            ids = [int(c.get("id")) for c in tree.iter(q(P, "cNvPr")) if c.get("id")]
            tree.append(etree.fromstring(
                f'<root {tpl.NS}>{tpl.num_ph(max(ids) + 1)}</root>'.encode())[0])
            acts.append("num+")
    cant += fix_body_fonts(root, title)

    etree.ElementTree(root).write(path, xml_declaration=True, encoding="UTF-8", standalone=True)
    rp = os.path.join(sl_dir, "_rels", f + ".rels")
    rr = open(rp, encoding="utf8").read()
    open(rp, "w", encoding="utf8").write(re.sub(
        r'Target="\.\./slideLayouts/slideLayout\d+\.xml"',
        f'Target="../slideLayouts/slideLayout{LAY[lay]}.xml"', rr))
    report.append((n, lay, ",".join(acts) or "-", txt(title)[:44] if title is not None else ""))

with zipfile.ZipFile(DST, "w", zipfile.ZIP_DEFLATED) as z:
    for dp, _, fns in os.walk(WORK):
        for fn in fns:
            fp = os.path.join(dp, fn)
            z.write(fp, os.path.relpath(fp, WORK))

from collections import Counter
if os.environ.get("VERBOSE"):
    for r in sorted(report): print("%3d  %-18s %-12s %s" % r)
print("  ".join(f"{k}={v}" for k, v in Counter(r[1] for r in report).most_common()),
      f"| Cantarell runs fixed: {cant}")

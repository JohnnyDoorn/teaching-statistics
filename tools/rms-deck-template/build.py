# -*- coding: utf-8 -*-
"""Build the RMS 26/27 master + layouts and re-base a deck onto it."""
import os, re, shutil, sys, zipfile
from lxml import etree
import tpl

SRC, DST = sys.argv[1], sys.argv[2]
WORK = "work"
A = "http://schemas.openxmlformats.org/drawingml/2006/main"
P = "http://schemas.openxmlformats.org/presentationml/2006/main"
R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
PR = "http://schemas.openxmlformats.org/package/2006/relationships"
q = lambda ns, t: f"{{{ns}}}{t}"

if os.path.isdir(WORK): shutil.rmtree(WORK)
with zipfile.ZipFile(SRC) as z: z.extractall(WORK)
W = lambda *p: os.path.join(WORK, *p)
rd = lambda p: open(p, encoding="utf8").read()
wr = lambda p, s: open(p, "w", encoding="utf8").write(s)

# ---------------------------------------------------------------- 1. theme
t = rd(W("ppt/theme/theme1.xml"))
t = re.sub(r"<a:clrScheme.*?</a:clrScheme>", tpl.CLRSCHEME, t, flags=re.S)
t = re.sub(r"<a:fontScheme.*?</a:fontScheme>", tpl.FONTSCHEME, t, flags=re.S)
t = re.sub(r'<a:theme([^>]*)name="[^"]*"', r'<a:theme\1name="RMS 2627"', t, count=1)
wr(W("ppt/theme/theme1.xml"), t)

# ---------------------------------------------------------------- 2. master
def lvl(n, sz, marL, indent, bullet=True):
    bu = ('<a:buFont typeface="Arial"/><a:buChar char="•"/>' if bullet else "<a:buNone/>")
    return (f'<a:lvl{n}pPr marL="{marL}" indent="{indent}" algn="l" defTabSz="914400" rtl="0" '
            f'eaLnBrk="1" latinLnBrk="0" hangingPunct="1"><a:lnSpc><a:spcPct val="90000"/></a:lnSpc>'
            f'<a:spcBef><a:spcPts val="600"/></a:spcBef>{bu}'
            f'<a:defRPr sz="{sz}" kern="1200"><a:solidFill><a:schemeClr val="tx1"/></a:solidFill>'
            f'<a:latin typeface="+mn-lt"/></a:defRPr></a:lvl{n}pPr>')

TITLE_STYLE = ('<p:titleStyle><a:lvl1pPr algn="l" defTabSz="914400" rtl="0" eaLnBrk="1" latinLnBrk="0" '
               'hangingPunct="1"><a:lnSpc><a:spcPct val="90000"/></a:lnSpc>'
               '<a:spcBef><a:spcPct val="0"/></a:spcBef><a:buNone/>'
               '<a:defRPr sz="4400" b="0" kern="1200"><a:solidFill><a:schemeClr val="tx1"/></a:solidFill>'
               '<a:latin typeface="+mj-lt"/></a:defRPr></a:lvl1pPr></p:titleStyle>')
BODY_STYLE = ("<p:bodyStyle>" + lvl(1, 2800, 285750, -285750) + lvl(2, 2400, 685800, -285750)
              + lvl(3, 2000, 1143000, -228600) + lvl(4, 1800, 1600200, -228600)
              + lvl(5, 1600, 2057400, -228600)
              + "".join(lvl(n, 1600, 2514600 + (n - 6) * 457200, -228600) for n in range(6, 10))
              + "</p:bodyStyle>")
OTHER_STYLE = ('<p:otherStyle><a:lvl1pPr marL="0" algn="l" defTabSz="914400" rtl="0"><a:defRPr sz="1800" '
               'kern="1200"><a:solidFill><a:schemeClr val="tx1"/></a:solidFill>'
               '<a:latin typeface="+mn-lt"/></a:defRPr></a:lvl1pPr></p:otherStyle>')

master = (f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
          f'<p:sldMaster {tpl.NS}><p:cSld><p:bg><p:bgPr><a:solidFill>'
          f'<a:schemeClr val="bg1"/></a:solidFill><a:effectLst/></p:bgPr></p:bg>'
          + tpl.spTree([tpl.title_ph(2), tpl.body_ph(3, idx=1), tpl.num_ph(4)])
          + '</p:cSld><p:clrMap bg1="lt1" tx1="dk1" bg2="lt2" tx2="dk2" accent1="accent1" '
            'accent2="accent2" accent3="accent3" accent4="accent4" accent5="accent5" '
            'accent6="accent6" hlink="hlink" folHlink="folHlink"/>'
          + "<p:sldLayoutIdLst>{LAYOUTS}</p:sldLayoutIdLst>"
          + f'<p:txStyles>{TITLE_STYLE}{BODY_STYLE}{OTHER_STYLE}</p:txStyles></p:sldMaster>')

# ---------------------------------------------------------------- 3. layouts
CENTRED_TITLE_XFRM = '<a:off x="1219200" y="2286000"/><a:ext cx="9753600" cy="1600200"/>'
CENTRED_SUB_XFRM   = '<a:off x="1219200" y="4038600"/><a:ext cx="9753600" cy="1200000"/>'
STATEMENT_XFRM     = '<a:off x="1219200" y="2600325"/><a:ext cx="9753600" cy="1657350"/>'

LAYOUTS = [
    ("Title Slide", "title", [
        tpl._sp(2, "Title Placeholder", '<p:ph type="ctrTitle"/>', CENTRED_TITLE_XFRM,
                f'<a:bodyPr {tpl.INS} anchor="b"><a:noAutofit/></a:bodyPr>',
                '<a:p><a:r><a:rPr lang="en-US"/><a:t>Click to edit title</a:t></a:r></a:p>',
                tpl.lst(5400, algn="ctr", bullet=False)),
        tpl._sp(3, "Subtitle Placeholder", '<p:ph type="subTitle" idx="1"/>', CENTRED_SUB_XFRM,
                f'<a:bodyPr {tpl.INS} anchor="t"><a:normAutofit/></a:bodyPr>',
                '<a:p><a:pPr marL="0" indent="0"/><a:r><a:rPr lang="en-US"/>'
                '<a:t>Click to edit subtitle</a:t></a:r></a:p>',
                tpl.lst(2400, algn="ctr", bullet=False))]),
    ("Title and Content", "obj", [tpl.title_ph(2), tpl.body_ph(3, idx=1), tpl.num_ph(4)]),
    ("Title Only", "titleOnly", [tpl.title_ph(2), tpl.num_ph(4)]),
    ("Section Overview", "obj", [tpl.title_ph(2), tpl.body_ph(3, idx=1), tpl.num_ph(4)]),
    ("Statement", "titleOnly", [
        tpl._sp(2, "Title Placeholder", '<p:ph type="title"/>', STATEMENT_XFRM,
                f'<a:bodyPr {tpl.INS} anchor="ctr"><a:noAutofit/></a:bodyPr>',
                '<a:p><a:r><a:rPr lang="en-US"/><a:t>Click to edit title</a:t></a:r></a:p>',
                tpl.lst(6000, b=1, algn="ctr", bullet=False)),
        tpl.num_ph(4)]),
    ("Blank", "blank", []),
]

for old in os.listdir(W("ppt/slideLayouts")):
    if old.endswith(".xml"): os.remove(W("ppt/slideLayouts", old))
for old in os.listdir(W("ppt/slideLayouts/_rels")):
    os.remove(W("ppt/slideLayouts/_rels", old))

for i, (name, typ, shapes) in enumerate(LAYOUTS, 1):
    xml = (f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
           f'<p:sldLayout {tpl.NS} type="{typ}" preserve="1"><p:cSld name="{name}">'
           + tpl.spTree(shapes)
           + '</p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sldLayout>')
    wr(W(f"ppt/slideLayouts/slideLayout{i}.xml"), xml)
    wr(W(f"ppt/slideLayouts/_rels/slideLayout{i}.xml.rels"),
       '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
       f'<Relationships xmlns="{PR}"><Relationship Id="rId1" '
       'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster" '
       'Target="../slideMasters/slideMaster1.xml"/></Relationships>')

n = len(LAYOUTS)
wr(W("ppt/slideMasters/slideMaster1.xml"), master.replace(
    "{LAYOUTS}", "".join(f'<p:sldLayoutId id="{2147483649+i}" r:id="rId{i+2}"/>' for i in range(n))))
wr(W("ppt/slideMasters/_rels/slideMaster1.xml.rels"),
   '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
   f'<Relationships xmlns="{PR}"><Relationship Id="rId1" '
   'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme" '
   'Target="../theme/theme1.xml"/>'
   + "".join(f'<Relationship Id="rId{i+2}" Type="http://schemas.openxmlformats.org/'
             f'officeDocument/2006/relationships/slideLayout" '
             f'Target="../slideLayouts/slideLayout{i+1}.xml"/>' for i in range(n))
   + "</Relationships>")

# ---------------------------------------------- 4. consolidate to one master
# Decks accumulate a master per pasted-in deck (Agresti5b had 11 / 133 layouts).
# Every slide now points at slideLayout1-6 of master 1, so the rest are dead.
import posixpath
pres_p, prels_p = W("ppt/presentation.xml"), W("ppt/_rels/presentation.xml.rels")
pres, prels = rd(pres_p), rd(prels_p)
keep_rid = None
rid2tgt = dict(re.findall(r'Id="(rId\d+)"[^>]*Target="([^"]+)"', prels))
for rid, tgt in rid2tgt.items():
    if tgt.endswith("slideMasters/slideMaster1.xml"): keep_rid = rid
drop = [rid for rid, t in rid2tgt.items()
        if "slideMasters/slideMaster" in t and rid != keep_rid]
for rid in drop:
    pres = re.sub(r'<p:sldMasterId[^>]*r:id="%s"\s*/>' % rid, "", pres)
    prels = re.sub(r'<Relationship Id="%s"[^>]*/>' % rid, "", prels)
wr(pres_p, pres); wr(prels_p, prels)

# orphan sweep: keep only parts reachable from the package root
def rels_of(part):
    d, f = posixpath.split(part)
    return posixpath.join(d, "_rels", f + ".rels")

reach, queue = set(), ["_rels/.rels"]
while queue:
    r = queue.pop()
    if r in reach or not os.path.exists(W(r)): continue
    reach.add(r)
    base = posixpath.dirname(posixpath.dirname(r))
    for tgt, mode in re.findall(r'Target="([^"]+)"(?:\s+TargetMode="(\w+)")?', rd(W(r))):
        if mode == "External" or tgt.startswith("http"): continue
        p = posixpath.normpath(posixpath.join(base, tgt))
        if p in reach: continue
        reach.add(p); queue.append(rels_of(p))

removed = 0
for dp, _, fns in os.walk(W(""), topdown=False):
    for fn in fns:
        fp = os.path.join(dp, fn)
        rel = os.path.relpath(fp, WORK).replace(os.sep, "/")
        if rel == "[Content_Types].xml" or rel in reach: continue
        os.remove(fp); removed += 1
    if not os.listdir(dp): os.rmdir(dp)
print(f"dropped {len(drop)} extra masters, swept {removed} orphaned parts")

ct = rd(W("[Content_Types].xml"))
present = {os.path.relpath(os.path.join(dp, fn), WORK).replace(os.sep, "/")
           for dp, _, fns in os.walk(W("")) for fn in fns}
def keep_override(tag):
    p = re.search(r'PartName="/([^"]+)"', tag)
    return p and p.group(1) in present
ct = "".join(t if not t.startswith("<Override") or keep_override(t) else ""
             for t in re.split(r'(<Override[^>]*/>)', ct))
ct = ct.replace("</Types>", "".join(
    f'<Override PartName="/ppt/slideLayouts/slideLayout{i+1}.xml" ContentType="application/'
    f'vnd.openxmlformats-officedocument.presentationml.slideLayout+xml"/>' for i in range(n)) + "</Types>")
wr(W("[Content_Types].xml"), ct)
print(f"master + {n} layouts written")

# -*- coding: utf-8 -*-
"""Point slide colours at the shared palette, except where a plot pins them.

Three outcomes per colour reference:
  locked  -> written as explicit RGB (matched to an image on the same slide,
             which is raster and cannot follow the theme)
  mapped  -> written as <a:schemeClr val="accentN"/> so a future theme edit
             recolours it everywhere
  frozen  -> neutral, or no palette family close enough: written as explicit RGB
"""
import os, posixpath, re, shutil, sys, zipfile, collections
from lxml import etree
import palette as PAL

SRC, DST = sys.argv[1], sys.argv[2]
A = "{http://schemas.openxmlformats.org/drawingml/2006/main}"
WORK = "ptmp"
if os.path.isdir(WORK): shutil.rmtree(WORK)
with zipfile.ZipFile(SRC) as z: z.extractall(WORK)
W = lambda *p: os.path.join(WORK, *p)

theme = open(W("ppt/theme/theme1.xml"), encoding="utf8").read()
sch   = re.search(r"<a:clrScheme.*?</a:clrScheme>", theme, re.S).group(0)
tmap  = {}
for name, body in re.findall(r"<a:(\w+)>(.*?)</a:\1>", sch, re.S):
    v = re.search(r'val="([0-9A-Fa-f]{6})"', body) or re.search(r'lastClr="([0-9A-Fa-f]{6})"', body)
    if v: tmap[name] = v.group(1).upper()

stats = collections.Counter()
for f in sorted(os.listdir(W("ppt/slides"))):
    m = re.match(r"(slide\d+)\.xml$", f)
    if not m: continue
    sid = m.group(1)
    # colours present in pictures on this slide
    img, rp = [], W("ppt/slides/_rels", sid + ".xml.rels")
    if os.path.exists(rp):
        for rel in etree.parse(rp).getroot():
            t = rel.get("Target")
            if "/media/" in t or t.startswith("../media/"):
                p = W(posixpath.normpath(posixpath.join("ppt/slides", t)))
                if os.path.exists(p): img += PAL.image_colours(open(p, "rb").read())

    path = W("ppt/slides", f)
    root = etree.parse(path).getroot()
    for el in list(root.iter(f"{A}schemeClr")) + list(root.iter(f"{A}srgbClr")):
        tag = etree.QName(el).localname
        val = el.get("val")
        if tag == "schemeClr":
            if val == "phClr" or val not in tmap: continue
            hexv = tmap[val]
        else:
            hexv = (val or "").upper()
            if len(hexv) != 6: continue
        rgb = PAL.hexrgb(hexv)

        if PAL.neutral(rgb):
            want, slot = "srgbClr", None; stats["frozen"] += 1
        elif img and min(PAL.dist(rgb, c) for c in img) < PAL.LOCK_DIST:
            want, slot = "srgbClr", None; stats["locked"] += 1
        else:
            near = min(PAL.PALETTE.items(), key=lambda kv: PAL.dist(rgb, PAL.hexrgb(kv[1])))
            if PAL.dist(rgb, PAL.hexrgb(near[1])) < PAL.MAP_DIST:
                want, slot = "schemeClr", near[0]; stats["mapped"] += 1
            else:
                want, slot = "srgbClr", None; stats["frozen"] += 1

        new = etree.SubElement(el.getparent(), f"{A}{want}")
        new.set("val", slot if slot else hexv)
        for child in list(el): new.append(child)   # keep lumMod / alpha / shade / tint
        el.getparent().replace(el, new)
    etree.ElementTree(root).write(path, xml_declaration=True, encoding="UTF-8", standalone=True)

with zipfile.ZipFile(DST, "w", zipfile.ZIP_DEFLATED) as z:
    for dp, _, fns in os.walk(WORK):
        for fn in fns:
            fp = os.path.join(dp, fn); z.write(fp, os.path.relpath(fp, WORK))
shutil.rmtree(WORK)
print(f"  locked {stats['locked']:>4} (plot-matched) | mapped {stats['mapped']:>4} to palette "
      f"| frozen {stats['frozen']:>5}")

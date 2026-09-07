# -*- coding: utf-8 -*-
"""Resolve theme colour references into explicit RGB, using the deck's ORIGINAL theme.

Slides refer to colours indirectly (<a:schemeClr val="accent2"/>). Swapping the
theme therefore silently recolours existing content: Morling10's accent2 was
ED7D31 (orange) and the RMS theme's is 2A6099 (blue). Baking the resolved value
onto the slide keeps every existing slide looking exactly as it did, while the
new theme still governs anything created from now on.
"""
import re, shutil, sys, zipfile, os
from lxml import etree

SRC, DST = sys.argv[1], sys.argv[2]
A="{http://schemas.openxmlformats.org/drawingml/2006/main}"
P="{http://schemas.openxmlformats.org/presentationml/2006/main}"
WORK="ctmp"
if os.path.isdir(WORK): shutil.rmtree(WORK)
with zipfile.ZipFile(SRC) as z: z.extractall(WORK)
W=lambda *p: os.path.join(WORK,*p)

theme = etree.parse(W("ppt/theme/theme1.xml")).getroot()
scheme = theme.find(f"{A}themeElements/{A}clrScheme")
raw = {}
for el in scheme:
    tag = etree.QName(el).localname
    v = el.find(f"{A}srgbClr")
    raw[tag] = v.get("val") if v is not None else el.find(f"{A}sysClr").get("lastClr")

master = etree.parse(W("ppt/slideMasters/slideMaster1.xml")).getroot()
cmap = master.find(f"{P}clrMap")
# slide-level names (bg1/tx1/...) resolve through the master's colour map
resolved = dict(raw)
if cmap is not None:
    for slot, target in cmap.attrib.items():
        resolved[slot] = raw.get(target, raw.get("dk1", "000000"))

n = 0
for f in sorted(os.listdir(W("ppt/slides"))):
    if not re.match(r"slide\d+\.xml$", f): continue
    path = W("ppt/slides", f)
    root = etree.parse(path).getroot()
    for el in list(root.iter(f"{A}schemeClr")):
        val = el.get("val")
        if val == "phClr" or val not in resolved: continue
        new = etree.SubElement(el.getparent(), f"{A}srgbClr")
        new.set("val", resolved[val])
        for child in list(el):            # keep lumMod / alpha / shade / tint
            new.append(child)
        el.getparent().replace(el, new)
        n += 1
    etree.ElementTree(root).write(path, xml_declaration=True, encoding="UTF-8", standalone=True)

with zipfile.ZipFile(DST,"w",zipfile.ZIP_DEFLATED) as z:
    for dp,_,fns in os.walk(WORK):
        for fn in fns:
            fp=os.path.join(dp,fn); z.write(fp, os.path.relpath(fp, WORK))
shutil.rmtree(WORK)
print(f"  baked {n} theme-colour references  (accent1={resolved.get('accent1')}, accent2={resolved.get('accent2')})")

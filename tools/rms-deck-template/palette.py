# -*- coding: utf-8 -*-
"""Decide, per slide, which colours may be remapped to the shared palette.

A coloured box is often matched by eye to a curve inside an embedded R plot.
Those images are raster, so their colours cannot follow the theme; remapping the
box would break the pairing. So: sample the pixels of every picture on a slide,
and lock any slide colour that sits near one of them.
"""
import io, os, posixpath, re, sys, zipfile, collections
from lxml import etree
from PIL import Image

A="{http://schemas.openxmlformats.org/drawingml/2006/main}"
R="{http://schemas.openxmlformats.org/officeDocument/2006/relationships}"

PALETTE={"accent1":"ED7D31","accent2":"4472C4","accent3":"C00000",
         "accent4":"00B050","accent5":"A3238E","accent6":"FFC000"}
LOCK_DIST=100.0      # "even sort of matched" -> generous
MAP_DIST =90.0       # only snap when clearly the same family

hexrgb=lambda h:(int(h[0:2],16),int(h[2:4],16),int(h[4:6],16))
dist=lambda a,b:sum((x-y)**2 for x,y in zip(a,b))**.5

def neutral(rgb):
    r,g,b=rgb
    mx,mn=max(rgb),min(rgb)
    return (mx-mn)<28 or mx<40 or mn>228        # grey, near-black, near-white

def image_colours(blob):
    try: im=Image.open(io.BytesIO(blob)).convert("RGB")
    except Exception: return []
    im.thumbnail((90,90))
    q=im.quantize(colors=24, method=Image.MEDIANCUT).convert("RGB")
    cnt=collections.Counter(q.getdata())
    total=sum(cnt.values())
    return [c for c,v in cnt.most_common(24) if v/total>0.004 and not neutral(c)]

def analyse(path):
    z=zipfile.ZipFile(path)
    theme=z.read("ppt/theme/theme1.xml").decode("utf8","replace")
    sch=re.search(r"<a:clrScheme.*?</a:clrScheme>",theme,re.S).group(0)
    tmap={}
    for name,body in re.findall(r"<a:(\w+)>(.*?)</a:\1>",sch,re.S):
        v=re.search(r'val="([0-9A-Fa-f]{6})"',body) or re.search(r'lastClr="([0-9A-Fa-f]{6})"',body)
        if v: tmap[name]=v.group(1).upper()
    locked=collections.Counter(); mapped=collections.Counter(); skipped=collections.Counter()
    lock_slides=set()
    for n in sorted(z.namelist()):
        m=re.match(r"ppt/slides/(slide\d+)\.xml$",n)
        if not m: continue
        sid=m.group(1)
        rels_p=f"ppt/slides/_rels/{sid}.xml.rels"
        img=[]
        if rels_p in z.namelist():
            rels=etree.fromstring(z.read(rels_p))
            for rel in rels:
                t=rel.get("Target")
                if "/media/" in t or t.startswith("../media/"):
                    p=posixpath.normpath(posixpath.join("ppt/slides",t))
                    if p in z.namelist(): img+= image_colours(z.read(p))
        x=z.read(n).decode("utf8","replace")
        used=[c.upper() for c in re.findall(r'<a:srgbClr val="([0-9A-Fa-f]{6})"',x)]
        used+=[tmap[s] for s in re.findall(r'<a:schemeClr val="(\w+)"',x) if s in tmap]
        for h in used:
            rgb=hexrgb(h)
            if neutral(rgb): skipped[h]+=1; continue
            if img and min(dist(rgb,c) for c in img)<LOCK_DIST:
                locked[h]+=1; lock_slides.add(sid); continue
            near=min(PALETTE.items(), key=lambda kv: dist(rgb,hexrgb(kv[1])))
            if dist(rgb,hexrgb(near[1]))<MAP_DIST: mapped[h]+=1
            else: skipped[h]+=1
    return locked,mapped,skipped,len(lock_slides)

if __name__=="__main__":
    O="/Users/johnny/surfdrive/Teaching/RMS/RMS_2026/RMS_2026/_2025_originals/"
    tl=tm=ts=0
    print(f"{'deck':<24}{'locked':>8}{'remap':>8}{'left':>8}   slides w/ locked colour")
    for fn in sys.argv[1:]:
        l,mp,s,ns=analyse(O+fn)
        tl+=sum(l.values()); tm+=sum(mp.values()); ts+=sum(s.values())
        print(f"{fn:<24}{sum(l.values()):>8}{sum(mp.values()):>8}{sum(s.values()):>8}   {ns}")
    print(f"\ntotals: {tl} locked (plot-matched), {tm} remapped to palette, {ts} left alone (neutral/no match)")

import glob, os, re, subprocess, sys
from PIL import Image, ImageChops
n = sys.argv[1]
d = f"dd/{n}"; os.makedirs(d, exist_ok=True)
for f in glob.glob(d + "/*.png"): os.remove(f)
for tag, pdf in (("s", f"rr/src_{n}.pdf"), ("o", f"rr/out_{n}.pdf")):
    subprocess.run(["pdftoppm", "-r", "60", "-png", "-gray", pdf, f"{d}/{tag}"], check=True)
pg = lambda t: {int(re.findall(r"-(\d+)\.png$", f)[0]): f for f in glob.glob(f"{d}/{t}-*.png")}
S, O = pg("s"), pg("o")
rows = []
for i in sorted(S):
    if i not in O: continue
    a, b = Image.open(S[i]).convert("L"), Image.open(O[i]).convert("L")
    if a.size != b.size: rows.append((100.0, i, "SIZE MISMATCH")); continue
    diff = ImageChops.difference(a, b).point(lambda v: 255 if v > 24 else 0)
    changed = sum(diff.histogram()[255:])
    rows.append((changed / (a.size[0] * a.size[1]) * 100, i, ""))
rows.sort(reverse=True)
big = [r for r in rows if r[0] > 1.0]
print(f"  identical: {sum(1 for r in rows if r[0]==0)}/{len(rows)}   "
      f"under 1% changed: {sum(1 for r in rows if 0 < r[0] <= 1)}   over 1%: {len(big)}")
for frac, i, note in big[:12]: print(f"    page {i:>3}  {frac:5.2f}% {note}")

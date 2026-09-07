import os, re, shutil, sys, zipfile
SRC, MAJOR, MINOR, DST = sys.argv[1:5]
W="ftmp"
if os.path.isdir(W): shutil.rmtree(W)
with zipfile.ZipFile(SRC) as z: z.extractall(W)
p=os.path.join(W,"ppt/theme/theme1.xml")
t=open(p,encoding="utf8").read()
t=re.sub(r"<a:fontScheme.*?</a:fontScheme>",
  f'<a:fontScheme name="RMS 2627"><a:majorFont><a:latin typeface="{MAJOR}"/>'
  f'<a:ea typeface=""/><a:cs typeface=""/></a:majorFont><a:minorFont>'
  f'<a:latin typeface="{MINOR}"/><a:ea typeface=""/><a:cs typeface=""/></a:minorFont></a:fontScheme>',
  t, flags=re.S)
open(p,"w",encoding="utf8").write(t)
with zipfile.ZipFile(DST,"w",zipfile.ZIP_DEFLATED) as z:
    for dp,_,fns in os.walk(W):
        for fn in fns:
            fp=os.path.join(dp,fn); z.write(fp, os.path.relpath(fp,W))
shutil.rmtree(W)
print(f"  {MAJOR} / {MINOR}")

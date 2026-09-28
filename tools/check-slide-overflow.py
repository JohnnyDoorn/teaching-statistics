"""Flag revealjs slides whose content runs into the footer.

usage: python3 tools/check-slide-overflow.py <deck.slide.html> [...]

Opens each rendered deck in headless Chrome, shows all fragments, and measures
every slide at the 1050x700 layout. Content inside a scrolling box (long code
output, printed data) is ignored; slides marked .scrollable are listed but not
counted. Checks the rendered .slide.html, so render first.
"""
import sys,os,re,json,subprocess,urllib.parse,tempfile
js=r"""<script>
window.addEventListener('load',()=>{setTimeout(()=>{
 Reveal.configure({transition:'none',backgroundTransition:'none',viewDistance:100});
 const cfg=Reveal.getConfig(), H=cfg.height, W=cfg.width, m=cfg.margin;
 const s700=Math.min(1050/(W*(1+m)),700/(H*(1+m)));
 const slidesEl=document.querySelector('.reveal .slides'), foot=document.querySelector('.reveal .footer');
 const out=[];
 Reveal.getSlides().forEach((s,i)=>{
  const ix=Reveal.getIndices(s); Reveal.slide(ix.h,ix.v);
  s.querySelectorAll('.fragment').forEach(f=>f.classList.add('visible'));
  void s.offsetHeight;
  const sr=slidesEl.getBoundingClientRect(), sc=sr.height/H;
  let b=0;
  s.querySelectorAll('*').forEach(e=>{
   if(e.closest('aside.notes')) return;
   for(let a=e.parentElement;a&&a!==s;a=a.parentElement){const o=getComputedStyle(a).overflowY; if(o!=='visible') return;}
   const cs=getComputedStyle(e); if(cs.visibility==='hidden'||cs.display==='none') return;
   const r=e.getBoundingClientRect(); if(r.height>0&&r.width>0) b=Math.max(b,(r.bottom-sr.top)/sc);
  });
  const y=350+(b-H/2)*s700;
  const ft=foot?700-(window.innerHeight-foot.getBoundingClientRect().top):700;
  const t=(s.querySelector('h1,h2,h3')||{}).textContent||'';
  out.push([i+1,s.id,Math.round(y),Math.round(ft),t.trim().slice(0,50),s.classList.contains('scrollable')]);
 });
 const p=document.createElement('pre'); p.id='ovf'; p.textContent=JSON.stringify(out); document.body.appendChild(p);
},1500)});</script>"""
for arg in sys.argv[1:]:
    deck=os.path.abspath(arg); d=os.path.dirname(deck)
    t=open(deck).read()
    t=t.replace('<head>','<head><base href="file://%s/">'%urllib.parse.quote(d),1).replace('</body>',js+'</body>',1)
    tmp=tempfile.NamedTemporaryFile('w',suffix='.html',delete=False); tmp.write(t); tmp.close()
    C="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    try:
        dom=subprocess.run(["perl","-e","alarm 60; exec @ARGV",C,"--headless=new","--disable-gpu","--allow-file-access-from-files",
            "--window-size=1050,700","--virtual-time-budget=15000","--dump-dom","file://"+tmp.name],capture_output=True,text=True).stdout
    finally: os.unlink(tmp.name)
    m=re.search(r'<pre id="ovf">(.*?)</pre>',dom,re.S)
    if not m: print(f"{os.path.basename(deck)}: no report (page did not finish loading)"); continue
    rows=json.loads(m.group(1).replace('&quot;','"').replace('&amp;','&'))
    bad=[r for r in rows if r[2]>r[3] and not r[5]]
    scroll=[r for r in rows if r[2]>r[3] and r[5]]
    print(f"{os.path.basename(deck)}: {len(rows)} slides, {len(bad)} run into the footer"
          + (f", {len(scroll)} more are .scrollable" if scroll else ""))
    for n,i,b,l,t,_ in bad: print(f"  slide {n:>3} #{i}: {b-l}px over  {t}")
    for n,i,b,l,t,_ in scroll: print(f"  slide {n:>3} #{i}: {b-l}px over, scrollable  {t}")

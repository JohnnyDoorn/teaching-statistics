"""Both student PDFs of RMS lecture 12, from the live deck. Run after the lecture:

    python3 l12_publish.py                 # this year's dot-demo results
    python3 l12_publish.py --year 2025     # test on an earlier cohort
    python3 l12_publish.py --out DIR       # somewhere other than RMS_2026/_publish

1. Rscript generateDotsWisdomCrowd.R [year]: fetches the "Wisdom of the Crowd?" form
   responses and writes wisdomCrowdFigures.(pdf|png) for that year and
   wisdomCrowdFigures_allYears.(pdf|png) in FiguresLectures.
2. RMS2627_12_ConfidenceIntervalsII.pdf: the deck without the dot demo (found by
   content, 66-68 in Oct 2026), as published in 2025.
3. RMS2627_12_ConfidenceIntervalsII_with-demo.pdf: every slide, plus a results slide
   (this year's figure) right after the demo's QR slide. Before the demo has run
   (fewer than 2 guesses this year) it is the full deck without a results slide.
Also prints the render check (rendercheck.py) of the deck as it is.
The deck itself is never changed; the results slide only exists in the export copy.
"""
import os, re, shutil, subprocess, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import l12_round1 as L
import pptexport

RMS = os.path.expanduser('~/surfdrive/Teaching/RMS')
DECK = f'{RMS}/RMS_2026/RMS_2026/RMS2627_12_ConfidenceIntervalsII.pptx'
FIG = f'{RMS}/FiguresLectures'


def insert_picture_slide(d, after_n, png_path, media_name):
    """New Blank-layout slide after slide `after_n` holding one full-width picture."""
    from PIL import Image
    files = d.slide_files()
    k = max(int(re.search(r'slide(\d+)\.xml', f).group(1)) for f in files) + 1
    part = f'ppt/slides/slide{k}.xml'
    blank = [p for p in d.parts if re.match(r'ppt/slideLayouts/slideLayout\d+\.xml$', p)
             and 'name="Blank"' in d.get(p)]
    assert len(blank) == 1
    d.add_media(media_name, open(png_path, 'rb').read())
    w, h = Image.open(png_path).size
    pres = d.get('ppt/presentation.xml')
    sw, sh = [int(v) for v in re.search(r'<p:sldSz cx="(\d+)" cy="(\d+)"', pres).groups()]
    cx = sw - 2 * 228600; cy = round(cx * h / w); x, y = 228600, (sh - cy) // 2
    d.put(part, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
          '<p:sld xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" '
          'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" '
          'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:cSld><p:spTree>'
          '<p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr>'
          '<p:grpSpPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/><a:chOff x="0" y="0"/><a:chExt cx="0" cy="0"/></a:xfrm></p:grpSpPr>'
          f'<p:pic><p:nvPicPr><p:cNvPr id="2" name="Wisdom of the crowd results"/><p:cNvPicPr><a:picLocks noChangeAspect="1"/></p:cNvPicPr><p:nvPr/></p:nvPicPr>'
          '<p:blipFill><a:blip r:embed="rId2"/><a:stretch><a:fillRect/></a:stretch></p:blipFill>'
          f'<p:spPr><a:xfrm><a:off x="{x}" y="{y}"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr></p:pic>'
          '</p:spTree></p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>')
    d.order.append(part)
    rels = f'ppt/slides/_rels/slide{k}.xml.rels'
    d.put(rels, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
          f'<Relationships xmlns="{L.NS_PR}">'
          f'<Relationship Id="rId1" Type="{L.NS_R}/slideLayout" Target="../{blank[0].split("/", 1)[1]}"/>'
          f'<Relationship Id="rId2" Type="{L.NS_R}/image" Target="../media/{media_name}"/></Relationships>')
    d.order.append(rels)
    prels = d.get('ppt/_rels/presentation.xml.rels')
    rid = 'rId%d' % (max(int(i) for i in re.findall(r'Id="rId(\d+)"', prels)) + 1)
    d.put('ppt/_rels/presentation.xml.rels', prels.replace('</Relationships>',
          f'<Relationship Id="{rid}" Type="{L.NS_R}/slide" Target="slides/slide{k}.xml"/></Relationships>'))
    ids = [int(i) for i in re.findall(r'<p:sldId id="(\d+)"', pres)]
    after = re.findall(r'<p:sldId [^>]*/>', pres)[after_n - 1]
    d.put('ppt/presentation.xml', pres.replace(after, after + f'<p:sldId id="{max(ids) + 1}" r:id="{rid}"/>', 1))
    ct = d.get('[Content_Types].xml')
    d.put('[Content_Types].xml', ct.replace('</Types>', f'<Override PartName="/{part}" ContentType='
          '"application/vnd.openxmlformats-officedocument.presentationml.slide+xml"/></Types>'))
    app = d.get('docProps/app.xml')
    d.put('docProps/app.xml', re.sub(r'<Slides>(\d+)</Slides>', lambda m: f'<Slides>{int(m.group(1)) + 1}</Slides>', app))


def demo_slides(deck):
    """(first, last) slide numbers of the dot demo, found by content so added or moved slides
    elsewhere do not matter: the 'Wisdom of the Crowd Demo!' slide and the slides right after
    it that show its dots or the QR picture."""
    import hashlib
    d = L.Deck(deck)
    files = d.slide_files()
    def media(n):
        rels = d.get(d.rp(n))
        return {hashlib.md5(d.parts['ppt/media/' + t]).hexdigest() for t in re.findall(r'Target="\.\./media/([^"]+)"', rels)}
    first = [n for n in range(1, len(files) + 1) if 'Wisdom of the Crowd Demo' in d.get(d.sp(n))]
    assert len(first) == 1, f'demo slide not found ({first})'
    first = first[0]; seen = media(first); last = first
    while last + 1 <= len(files):
        m = media(last + 1)
        if not (m & (seen | media(first + 1))): break
        seen |= m; last += 1
    return first, last


def main(argv):
    year = argv[argv.index('--year') + 1] if '--year' in argv else None
    out = argv[argv.index('--out') + 1] if '--out' in argv else f'{RMS}/RMS_2026/RMS_2026/_publish'
    r = subprocess.run(['Rscript', 'generateDotsWisdomCrowd.R'] + ([year] if year else []), cwd=FIG,
                       capture_output=True, text=True)
    first_line = r.stdout.strip().splitlines()[0] if r.stdout.strip() else ''
    print(first_line, r.stderr.strip()[-300:] if r.returncode else '')
    if r.returncode: raise SystemExit('R script failed')
    n_guesses = int(re.search(r'n = (\d+)', first_line).group(1))
    demo = demo_slides(DECK)
    print(f'dot demo: slides {demo[0]}-{demo[1]}')
    with tempfile.TemporaryDirectory() as t:
        full = os.path.join(t, 'full.pdf')
        pptexport.export(DECK, full)
        n_pages = int(re.search(r'Pages:\s+(\d+)', subprocess.run(['pdfinfo', full], capture_output=True, text=True).stdout).group(1))
        # render check of the deck as it is now (only what you would see: SPILL / OFF / HIT)
        rc = subprocess.run([sys.executable, os.path.join(os.path.dirname(os.path.abspath(__file__)), 'rendercheck.py'), DECK, full],
                            capture_output=True, text=True).stdout
        seen_issues = [l for l in rc.splitlines() if re.match(r'slide +\d+ (SPILL|OFF|HIT)', l)]
        print('render check:', 'nothing spills or overlaps' if not seen_issues else '')
        for l in seen_issues: print('  ' + l)
        a = os.path.join(out, 'RMS2627_12_ConfidenceIntervalsII.pdf')
        subprocess.run(['qpdf', full, '--pages', '.', f'1-{demo[0] - 1},{demo[1] + 1}-z', '--', a])  # exit 3 = warnings only
        print(f'without demo: {a} ({n_pages - (demo[1] - demo[0] + 1)} pages)')
        b = os.path.join(out, 'RMS2627_12_ConfidenceIntervalsII_with-demo.pdf')
        if n_guesses < 2:
            shutil.copy(full, b)
            print(f'with demo, no results slide yet ({n_guesses} guesses this year; run again after the demo): {b} ({n_pages} pages)')
            return
        d = L.Deck(DECK)
        # the figure the R script just wrote (its name has changed: wisdomCrowdFigures.png, ...2026.png)
        import glob
        figs = [f for f in glob.glob(os.path.join(FIG, 'wisdomCrowdFigures*.png')) if 'allYears' not in f]
        fig = max(figs, key=os.path.getmtime)
        print('results slide:', os.path.basename(fig))
        insert_picture_slide(d, demo[1], fig, 'l12_crowd_results.png')
        withres = os.path.join(t, 'with-demo.pptx'); d.save(withres)
        pptexport.export(withres, b)
        print(f'with demo + results: {b} ({n_pages + 1} pages)')


if __name__ == '__main__':
    main(sys.argv[1:])

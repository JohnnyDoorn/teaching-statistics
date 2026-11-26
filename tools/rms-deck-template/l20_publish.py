"""Student PDF of RMS lecture 20 from the live deck, without the pictures publishcopy.BLOCKED lists.

    python3 l20_publish.py <deck.pptx> <out.pdf> [--keep-copy <copy.pptx>]

His call (2026-11-26): the GitHub upload must not carry the small-sample-size photos (slide 8), the Tyler
picture (Kruskal-Wallis, slide 30) or other unsourced pictures; Bach and the 'silence' photo on slide 30 are
credited on the slide ("Sources: Wikipedia & Pixabay") and stay. The hashes live in publishcopy.BLOCKED, the
one list for all decks.

Why not publishcopy.py itself: on L20 its copy (python-pptx deletes the pictures and drops the emptied slide)
opens cleanly but PowerPoint's scripted PDF export silently writes nothing (twice, 2026-11-26); slide 30's
pictures are animation targets, so deleting them leaves dangling p:timing references. Here the same pictures
are hidden instead (<p:cNvPr hidden="1">, what the Selection Pane does), and a slide left with nothing to say
(only a title and labels of 60 characters or less, publishcopy's rule) is hidden too (show="0"). The export
drops it like any hidden slide, and the slide numbers stay those of the live deck. The deck is never changed.
"""
import hashlib, os, posixpath, re, shutil, subprocess, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from l12_round1 import Deck
from publishcopy import BLOCKED, KEEP_IF_TEXT_LONGER_THAN, SOURCE_RE
import pptexport


def _hide_tag(tag):
    """<p:cNvPr ...> or <p:cNvPr .../> with hidden="1"."""
    tag = re.sub(r'\s+hidden="[^"]*"', '', tag)
    return tag[:-2] + ' hidden="1"/>' if tag.endswith('/>') else tag[:-1] + ' hidden="1">'


def _carries_content(s):
    """True if the slide still says something with its blocked pictures hidden."""
    if '<p:graphicFrame' in s:
        return True
    for pic in re.findall(r'<p:pic\b[^>]*>.*?</p:pic>', s, flags=re.S):
        if 'hidden="1"' not in re.search(r'<p:cNvPr\b[^>]*>', pic).group(0):
            return True
    for sp in re.findall(r'<p:sp\b[^>]*>.*?</p:sp>', s, flags=re.S):
        if re.search(r'<p:ph\b[^>]*type="(title|ctrTitle|sldNum)"', sp):
            continue
        t = ''.join(re.findall(r'<a:t>([^<]*)</a:t>', sp)).strip()
        if not t or SOURCE_RE.match(t) or t.isdigit():
            continue
        if len(t) > KEEP_IF_TEXT_LONGER_THAN:
            return True
    return False


def hide_blocked(d):
    pics, slides = [], []
    for n in range(1, len(d.slide_files()) + 1):
        part, rp = d.sp(n), d.rp(n)
        rid2media = {}
        for tag in re.findall(r'<Relationship [^>]*>', d.get(rp) if rp in d.parts else ''):
            if '/image"' in tag:
                rid = re.search(r'Id="([^"]+)"', tag).group(1)
                tgt = re.search(r'Target="([^"]+)"', tag).group(1)
                rid2media[rid] = posixpath.normpath(posixpath.join(posixpath.dirname(part), tgt))
        hits = []

        def hide(m):
            pic = m.group(0)
            rid = re.search(r'r:embed="([^"]+)"', pic)
            media = rid2media.get(rid.group(1)) if rid else None
            if not media or media not in d.parts:
                return pic
            h = hashlib.md5(d.parts[media]).hexdigest()[:8]
            if h not in BLOCKED:
                return pic
            hits.append((n, h, BLOCKED[h]))
            tag = re.search(r'<p:cNvPr\b[^>]*>', pic).group(0)
            return pic.replace(tag, _hide_tag(tag), 1)

        s = re.sub(r'<p:pic\b[^>]*>.*?</p:pic>', hide, d.get(part), flags=re.S)
        if hits:
            pics += hits
            if not _carries_content(s):
                sld = re.search(r'<p:sld\b[^>]*>', s).group(0)
                s = s.replace(sld, re.sub(r'\s+show="[^"]*"', '', sld)[:-1] + ' show="0">', 1)
                title = re.search(r'<p:ph\b[^>]*type="(?:title|ctrTitle)".*?</p:sp>', s, flags=re.S)
                slides.append((n, ''.join(re.findall(r'<a:t>([^<]*)</a:t>', title.group(0))) if title else ''))
            d.put(part, s)
    return pics, slides


def main():
    args = sys.argv[1:]
    keep = args[args.index('--keep-copy') + 1] if '--keep-copy' in args else None
    deck, out = [a for a in args if a != '--keep-copy' and a != keep][:2]
    d = Deck(deck)
    pics, slides = hide_blocked(d)
    for n, h, why in pics:
        print(f'  hidden picture on slide {n}: {why}')
    for n, t in slides:
        print(f'  hidden slide {n} ({t!r}): nothing left but a title and short labels')
    tmp = tempfile.mkdtemp()
    copy = os.path.join(tmp, os.path.basename(deck))
    d.save(copy)
    visible = sum(not re.search(r'<p:sld\b[^>]*show="0"', d.get('ppt/' + f)) for f in d.slide_files())
    pptexport.export(copy, out)
    pages = int(re.search(r'Pages:\s+(\d+)', subprocess.run(['pdfinfo', out], capture_output=True, text=True).stdout).group(1))
    print(f'  {len(d.slide_files())} slides, {visible} visible -> {pages} pages' + ('' if pages == visible else '  <-- MISMATCH'))
    if keep:
        shutil.copy(copy, keep)
    shutil.rmtree(tmp)


if __name__ == '__main__':
    main()

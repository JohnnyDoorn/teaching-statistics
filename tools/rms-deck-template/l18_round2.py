"""L18 lecture-eve additions (2026-11-11, lecture Thu 12 Nov), run one item at a time:

    python3 l18_round2.py <in.pptx> <out.pptx> <item> [<item> ...]

His ask of 11-11: an exam information slide that says alpha will be given, with the usual pointers
(exam information page, formula sheet), then a slide inviting students to the focus group; and the
new 2026 form for the snack demo.

  qr        slide 77: the snack demo QR -> the 2026 form 'RMS Snack Assessment 2026' (edu.nl/wvcrk).
            His own edu.nl image (snack-qr-2026.png, decodes to https://edu.nl/wvcrk); the frame keeps
            its width and top-left, its height follows the image (320 x 346 px instead of 320 x 347)
  examinfo  new slide after 80 (Recap flow), in the overview's 'Next time' slot that never had a slide:
            'Interim exam 3'. The alpha line is new (accent1, as slide 53's takeaway); the two pointers
            are his own L3 sentences (slides 26 and 29)
  focus     new slide after examinfo: the focus group (Tuesday Nov 24 at 14:00, location in a Canvas
            announcement)

The new slides go after slide 80, so slides 1-80 keep their numbers: WA11's feedback cites L18 slides
9-53, and slide 80 points to slide 32. Slides 81-99 become 83-101.
"""
import hashlib, os, re, sys, uuid
from xml.sax.saxutils import escape
from l18_round1 import L18Deck
from l12_round1 import NS_R, NS_PR

DECKS = '/Users/johnny/surfdrive/Teaching/RMS/RMS_2026/RMS_2026'
QR = os.path.join(DECKS, 'snack-qr-2026.png')
QR_MD5 = '11ed37bc1314ec8e462a9099006872bf'
SLIDE_CT = 'application/vnd.openxmlformats-officedocument.presentationml.slide+xml'
NS = ('xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" '
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" '
      'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"')


# --- slide text in slide 53's form: title placeholder, a 28 pt text box with Arial bullets, slide number

def _fill(accent):
    return '<a:schemeClr val="accent1"/>' if accent else '<a:srgbClr val="000000"/>'


def _rpr(sz, accent, tag='a:rPr'):
    return f'<{tag} lang="en-US" sz="{sz}" b="0" strike="noStrike" spc="-1"><a:solidFill>{_fill(accent)}</a:solidFill></{tag}>'


def _para(text, lvl=0, accent=False):
    """lvl 0 = 28 pt bullet, 1 = 24 pt sub-bullet, None = empty spacer line (24 pt, as slide 53)."""
    if lvl is None:
        return ('<a:p><a:pPr><a:lnSpc><a:spcPct val="90000"/></a:lnSpc><a:spcBef><a:spcPts val="1001"/></a:spcBef>'
                '<a:buNone/></a:pPr><a:endParaRPr lang="en-US" sz="2400" b="0" strike="noStrike" spc="-1"/></a:p>')
    sz, marl, bef = (2800, 228600, 1001) if lvl == 0 else (2400, 685800, 499)
    lv = '' if lvl == 0 else f' lvl="{lvl}"'
    return (f'<a:p><a:pPr marL="{marl}"{lv} indent="-223200"><a:lnSpc><a:spcPct val="90000"/></a:lnSpc>'
            f'<a:spcBef><a:spcPts val="{bef}"/></a:spcBef><a:buClr>{_fill(accent)}</a:buClr>'
            f'<a:buFont typeface="Arial"/><a:buChar char="&#8226;"/></a:pPr>'
            f'<a:r>{_rpr(sz, accent)}<a:t>{escape(text)}</a:t></a:r>'
            f'<a:endParaRPr lang="en-US" sz="{sz}" b="0" strike="noStrike" spc="-1"/></a:p>')


def bullet_slide(title, paras):
    fld = '{%s}' % str(uuid.uuid4()).upper()
    return (f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<p:sld {NS}><p:cSld><p:spTree>'
            '<p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr>'
            '<p:grpSpPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/><a:chOff x="0" y="0"/><a:chExt cx="0" cy="0"/></a:xfrm></p:grpSpPr>'
            # title, as slide 53
            '<p:sp><p:nvSpPr><p:cNvPr id="2" name="CustomShape 1"/><p:cNvSpPr><a:spLocks noGrp="1"/></p:cNvSpPr>'
            '<p:nvPr><p:ph type="title"/></p:nvPr></p:nvSpPr><p:spPr/>'
            '<p:txBody><a:bodyPr lIns="90000" tIns="45000" rIns="90000" bIns="45000" anchor="ctr"><a:noAutofit/></a:bodyPr>'
            '<a:lstStyle/><a:p><a:pPr><a:lnSpc><a:spcPct val="90000"/></a:lnSpc><a:buNone/></a:pPr>'
            f'<a:r><a:rPr lang="en-US" b="0"><a:solidFill><a:srgbClr val="000000"/></a:solidFill></a:rPr><a:t>{escape(title)}</a:t></a:r>'
            '<a:endParaRPr lang="en-US" b="0"/></a:p></p:txBody></p:sp>'
            # text box, slide 53's geometry
            '<p:sp><p:nvSpPr><p:cNvPr id="3" name="CustomShape 2"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr>'
            '<p:spPr><a:xfrm><a:off x="838080" y="1825560"/><a:ext cx="10508040" cy="4343760"/></a:xfrm>'
            '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:noFill/><a:ln w="0"><a:noFill/></a:ln></p:spPr>'
            '<p:style><a:lnRef idx="0"><a:scrgbClr r="0" g="0" b="0"/></a:lnRef><a:fillRef idx="0"><a:scrgbClr r="0" g="0" b="0"/></a:fillRef>'
            '<a:effectRef idx="0"><a:scrgbClr r="0" g="0" b="0"/></a:effectRef><a:fontRef idx="minor"/></p:style>'
            '<p:txBody><a:bodyPr lIns="90000" tIns="45000" rIns="90000" bIns="45000" anchor="t"><a:noAutofit/></a:bodyPr>'
            '<a:lstStyle/>' + ''.join(_para(*p) for p in paras) + '</p:txBody></p:sp>'
            # slide number, as slide 53
            '<p:sp><p:nvSpPr><p:cNvPr id="4" name="Slide Number Placeholder"/><p:cNvSpPr><a:spLocks noGrp="1"/></p:cNvSpPr>'
            '<p:nvPr><p:ph type="sldNum" sz="quarter" idx="12"/></p:nvPr></p:nvSpPr>'
            '<p:spPr><a:xfrm><a:off x="8610480" y="6356520"/><a:ext cx="2740320" cy="362160"/></a:xfrm>'
            '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr>'
            '<p:txBody><a:bodyPr lIns="90000" tIns="45000" rIns="90000" bIns="45000" anchor="ctr"><a:noAutofit/></a:bodyPr>'
            f'<a:lstStyle/><a:p><a:pPr algn="r"><a:buNone/></a:pPr><a:fld id="{fld}" type="slidenum">'
            '<a:rPr lang="en-US" sz="1200"><a:solidFill><a:srgbClr val="8B8B8B"/></a:solidFill><a:latin typeface="+mn-lt"/></a:rPr>'
            '<a:t>#</a:t></a:fld><a:endParaRPr lang="en-US" sz="1200"><a:solidFill><a:srgbClr val="8B8B8B"/></a:solidFill>'
            '</a:endParaRPr></a:p></p:txBody></p:sp>'
            '</p:spTree></p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>')


def insert_slide(d, after, xml, layout='slideLayout3.xml'):
    """Add a slide part and put it at position after + 1 (slide 53's layout, Title Only, by default)."""
    nums = [int(m.group(1)) for p in d.parts if (m := re.fullmatch(r'ppt/slides/slide(\d+)\.xml', p))]
    name = f'slide{max(nums) + 1}.xml'
    sp, rp = 'ppt/slides/' + name, 'ppt/slides/_rels/' + name + '.rels'
    d.put(sp, xml); d.order.append(sp)
    d.put(rp, f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="{NS_PR}">'
              f'<Relationship Id="rId1" Type="{NS_R}/slideLayout" Target="../slideLayouts/{layout}"/></Relationships>')
    d.order.append(rp)
    ct = d.get('[Content_Types].xml')
    d.put('[Content_Types].xml', ct.replace('</Types>', f'<Override PartName="/{sp}" ContentType="{SLIDE_CT}"/></Types>'))
    prels = d.get('ppt/_rels/presentation.xml.rels')
    rid = 'rId%d' % (max(int(i) for i in re.findall(r'Id="rId(\d+)"', prels)) + 1)
    d.put('ppt/_rels/presentation.xml.rels',
          prels.replace('</Relationships>', f'<Relationship Id="{rid}" Type="{NS_R}/slide" Target="slides/{name}"/></Relationships>'))
    pres = d.get('ppt/presentation.xml')
    ids = re.findall(r'<p:sldId [^>]*/>', pres)
    new_id = max(int(re.search(r' id="(\d+)"', s).group(1)) for s in ids) + 1
    assert pres.count(ids[after - 1]) == 1
    d.put('ppt/presentation.xml', pres.replace(ids[after - 1], ids[after - 1] + f'<p:sldId id="{new_id}" r:id="{rid}"/>'))
    print(f'  new slide {after + 1} ({name}, sldId {new_id}, {rid})')


def _text(d, n):
    return ' '.join(re.findall(r'<a:t>([^<]*)</a:t>', d.get(d.sp(n))))


def _check_anchor(d):
    # a reordered deck must fail here instead of getting the slides in the wrong place
    assert 'Recap' in _text(d, 80) and 'basic flow' in _text(d, 80), _text(d, 80)[:80]


def item_qr(d):
    data = open(QR, 'rb').read()
    assert hashlib.md5(data).hexdigest() == QR_MD5, 'snack-qr-2026.png is not the edu.nl/wvcrk image'
    assert 'How do you feel about these snacks?' in _text(d, 77)
    s, blk = d.shape_block(77, 'Picture 5', tag='p:pic')
    rid = re.search(r'r:embed="(rId\d+)"', blk).group(1)
    rels = d.get(d.rp(77))
    old = re.search(rf'<Relationship Id="{rid}" [^>]*Target="\.\./media/([^"]+)"', rels).group(1)
    assert old == 'image42.png', old
    name = d.add_media('l18_snack_qr_2026.png', data)
    d.put(d.rp(77), rels.replace(f'Target="../media/{old}"', f'Target="../media/{name}"'))
    cx = int(re.search(r'<a:ext cx="(\d+)" cy="\d+"/>', blk).group(1))
    w, h = 320, 346   # pixel size of the new image (the old was 320 x 347)
    new_blk = re.sub(r'(<a:ext cx="\d+" cy=")(\d+)("/>)', lambda m: m.group(1) + str(round(cx * h / w)) + m.group(3), blk, count=1)
    d.put(d.sp(77), s.replace(blk, new_blk))
    d.sweep_media()
    print(f'  slide 77: {old} -> {name}, height {round(cx * 347 / 320)} -> {round(cx * h / w)} EMU')


def item_examinfo(d):
    _check_anchor(d)
    insert_slide(d, 80, bullet_slide('Interim exam 3', [
        ('On the exam, we will always give you the significance level α', 0, True),
        ('(e.g., “Assume a significance level of 0.05”)', 1),
        ('', None),
        ('Always check the Canvas modules for the interim exam information', 0),   # his L3 slide 26
        ('', None),
        ('Practice the exercises, WA, trial exam, formula sheet (see Canvas)', 0),  # his L3 slide 29
    ]))


def item_focus(d):
    assert _text(d, 81).startswith('Interim exam 3'), 'run examinfo first'
    insert_slide(d, 81, bullet_slide('Join the focus group!', [
        ('A great opportunity for students and teacher to get together and evaluate the course as it is happening', 0),
        ('', None),
        ('Tuesday Nov 24 at 14:00', 0),
        ('', None),
        ('Location will follow in a Canvas announcement', 0),
    ]))


ITEMS = {'qr': item_qr, 'examinfo': item_examinfo, 'focus': item_focus}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = L18Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

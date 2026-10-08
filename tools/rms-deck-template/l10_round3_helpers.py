"""Helpers shared by the L10 round-3/4 builds: OMML math, AlternateContent wrapper, picture fallback."""
import io, re
from PIL import Image, ImageDraw, ImageFont
EMU = 914400
# ------------------------------------------------------------------ OMML + shape helpers (structure copied from PowerPoint's own slide 13)
NS_MC = 'http://schemas.openxmlformats.org/markup-compatibility/2006'
NS_A14 = 'http://schemas.microsoft.com/office/drawing/2010/main'
NS_M = 'http://schemas.openxmlformats.org/officeDocument/2006/math'
def esc(t): return t.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
def rpr(sz, color='000000', i=False):
    return (f'<a:rPr lang="en-US" sz="{sz}" b="0" i="{1 if i else 0}" strike="noStrike" spc="-1" dirty="0">'
            f'<a:solidFill><a:srgbClr val="{color}"/></a:solidFill></a:rPr>')
def run(t, sz, color='000000', i=False): return f'<a:r>{rpr(sz, color, i)}<a:t>{esc(t)}</a:t></a:r>'
class M:
    def __init__(s, sz, color='000000'): s.sz, s.c = sz, color
    def _r(s): return (f'<a:rPr lang="en-US" sz="{s.sz}" b="0" i="1" strike="noStrike" spc="-1" smtClean="0"><a:solidFill><a:srgbClr val="{s.c}"/></a:solidFill>'
                       '<a:latin typeface="Cambria Math" panose="02040503050406030204" pitchFamily="18" charset="0"/></a:rPr>')
    def r(s, t): return f'<m:r>{s._r()}<m:t>{esc(t)}</m:t></m:r>'
    def hat(s, e): return f'<m:acc><m:accPr><m:chr m:val="̂"/><m:ctrlPr>{s._r()}</m:ctrlPr></m:accPr><m:e>{e}</m:e></m:acc>'
    def sqrt(s, e): return f'<m:rad><m:radPr><m:degHide m:val="1"/><m:ctrlPr>{s._r()}</m:ctrlPr></m:radPr><m:deg/><m:e>{e}</m:e></m:rad>'
    def frac(s, a, b): return f'<m:f><m:fPr><m:ctrlPr>{s._r()}</m:ctrlPr></m:fPr><m:num>{a}</m:num><m:den>{b}</m:den></m:f>'
    def math(s, *content): return f'<a14:m><m:oMath xmlns:m="{NS_M}">{"".join(content)}</m:oMath></a14:m>'
def para(items, sz, algn=None, tabs=(), bef=0, ln=100):
    a = f' algn="{algn}"' if algn else ''
    tl = ''.join(f'<a:tab pos="{round(t * EMU)}" algn="l"/>' for t in tabs)
    return (f'<a:p><a:pPr{a}><a:lnSpc><a:spcPct val="{ln * 1000}"/></a:lnSpc>'
            + (f'<a:spcBef><a:spcPts val="{bef * 100}"/></a:spcBef>' if bef else '')
            + '<a:buNone/>' + (f'<a:tabLst>{tl}</a:tabLst>' if tl else '') + '</a:pPr>'
            + ''.join(items) + f'<a:endParaRPr lang="en-US" sz="{sz}" b="0" strike="noStrike" spc="-1" dirty="0"/></a:p>')
def text_sp(sid, name, box, fill, line, paras):
    x, y, cx, cy = [round(v * EMU) for v in box]
    return (f'<p:sp><p:nvSpPr><p:cNvPr id="{sid}" name="{name}"/><p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr>'
            f'<p:spPr><a:xfrm><a:off x="{x}" y="{y}"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom>{fill}{line}</p:spPr>'
            '<p:txBody><a:bodyPr wrap="square" lIns="91440" tIns="45720" rIns="91440" bIns="45720" anchor="t"><a:noAutofit/></a:bodyPr><a:lstStyle/>'
            + ''.join(paras) + '</p:txBody></p:sp>')
def fallback_sp(sid, name, box, rid):
    x, y, cx, cy = [round(v * EMU) for v in box]
    return (f'<p:sp><p:nvSpPr><p:cNvPr id="{sid}" name="{name}"/><p:cNvSpPr><a:spLocks noRot="1" noChangeAspect="1" noMove="1" noResize="1" noEditPoints="1" noAdjustHandles="1" noChangeArrowheads="1" noChangeShapeType="1" noTextEdit="1"/></p:cNvSpPr><p:nvPr/></p:nvSpPr>'
            f'<p:spPr><a:xfrm><a:off x="{x}" y="{y}"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:blipFill><a:blip r:embed="{rid}"/><a:stretch><a:fillRect/></a:stretch></a:blipFill></p:spPr>'
            '<p:txBody><a:bodyPr/><a:lstStyle/><a:p><a:r><a:rPr lang="en-US"><a:noFill/></a:rPr><a:t> </a:t></a:r></a:p></p:txBody></p:sp>')
def alt(choice, fb):
    return f'<mc:AlternateContent xmlns:mc="{NS_MC}"><mc:Choice xmlns:a14="{NS_A14}" Requires="a14">{choice}</mc:Choice><mc:Fallback>{fb}</mc:Fallback></mc:AlternateContent>'

PT = '/System/Library/Fonts/Supplemental/PTSans.ttc'
def render_fallback(box_in, fill_rgba, border, lines, dpi=150):
    """Picture used only by apps without equation support: plain text, Unicode math."""
    W, H = round(box_in[2] * dpi), round(box_in[3] * dpi)
    im = Image.new('RGBA', (W, H), fill_rgba); d = ImageDraw.Draw(im)
    if border: d.rectangle([0, 0, W - 1, H - 1], outline=border, width=2)
    y = 0.05 * dpi
    for text, pt, color, italic, algn, tabs in lines:
        f = ImageFont.truetype(PT, round(pt / 72 * dpi), index=1 if italic else 0)
        if tabs:
            for k, seg in enumerate(text.split('\t')):
                xx = 0.1 * dpi + (tabs[k - 1] * dpi if k else 0)
                d.text((xx, y), seg, font=f, fill=color)
        else:
            tw = f.getlength(text); xx = (W - tw) / 2 if algn == 'ctr' else 0.1 * dpi
            d.text((xx, y), text, font=f, fill=color)
        y += pt / 72 * dpi * 1.35
    buf = io.BytesIO(); im.save(buf, 'PNG'); return buf.getvalue()


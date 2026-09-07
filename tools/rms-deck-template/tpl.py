# -*- coding: utf-8 -*-
"""XML fragments for the RMS 26/27 master."""

NS = ('xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" '
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" '
      'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"')

# --- palette: accent1 is the deck's own house orange; the rest give the
# --- existing ad-hoc emphasis colours a named home instead of being one-offs.
CLRSCHEME = """<a:clrScheme name="RMS 2627"><a:dk1><a:srgbClr val="000000"/></a:dk1>\
<a:lt1><a:srgbClr val="FFFFFF"/></a:lt1><a:dk2><a:srgbClr val="404040"/></a:dk2>\
<a:lt2><a:srgbClr val="F2F2F2"/></a:lt2><a:accent1><a:srgbClr val="ED7D31"/></a:accent1>\
<a:accent2><a:srgbClr val="2A6099"/></a:accent2><a:accent3><a:srgbClr val="800080"/></a:accent3>\
<a:accent4><a:srgbClr val="00B050"/></a:accent4><a:accent5><a:srgbClr val="C00000"/></a:accent5>\
<a:accent6><a:srgbClr val="8B8B8B"/></a:accent6><a:hlink><a:srgbClr val="2A6099"/></a:hlink>\
<a:folHlink><a:srgbClr val="800080"/></a:folHlink></a:clrScheme>"""

FONTSCHEME = """<a:fontScheme name="RMS 2627"><a:majorFont><a:latin typeface="Calibri Light"/>\
<a:ea typeface=""/><a:cs typeface=""/></a:majorFont><a:minorFont><a:latin typeface="Calibri"/>\
<a:ea typeface=""/><a:cs typeface=""/></a:minorFont></a:fontScheme>"""

# --- geometry lifted verbatim from the 45 slides that already agree ---
TITLE_XFRM = '<a:off x="838080" y="365040"/><a:ext cx="10512720" cy="1322640"/>'
BODY_XFRM  = '<a:off x="838080" y="1780000"/><a:ext cx="10512720" cy="4300000"/>'
NUM_XFRM   = '<a:off x="8610480" y="6356520"/><a:ext cx="2740320" cy="362160"/>'

INS = 'lIns="90000" tIns="45000" rIns="90000" bIns="45000"'

def lst(sz, b=0, algn=None, bullet=True):
    """Placeholder-level defaults. Size must live here, not in the prompt run:
    prompt formatting is not inherited by slides that use the layout."""
    a = f' algn="{algn}"' if algn else ""
    bu = "" if bullet else "<a:buNone/>"
    return (f'<a:lstStyle><a:lvl1pPr{a}>{bu}'
            f'<a:defRPr sz="{sz}" b="{b}"/></a:lvl1pPr></a:lstStyle>')


def _sp(sid, name, ph, xfrm, bodypr, paras, lstStyle="<a:lstStyle/>"):
    return (f'<p:sp><p:nvSpPr><p:cNvPr id="{sid}" name="{name}"/>'
            f'<p:cNvSpPr><a:spLocks noGrp="1"/></p:cNvSpPr><p:nvPr>{ph}</p:nvPr></p:nvSpPr>'
            f'<p:spPr><a:xfrm>{xfrm}</a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr>'
            f'<p:txBody>{bodypr}{lstStyle}{paras}</p:txBody></p:sp>')

def title_ph(sid=2, prompt="Click to edit title"):
    return _sp(sid, "Title Placeholder", '<p:ph type="title"/>', TITLE_XFRM,
               f'<a:bodyPr {INS} anchor="ctr"><a:noAutofit/></a:bodyPr>',
               f'<a:p><a:r><a:rPr lang="en-US"/><a:t>{prompt}</a:t></a:r></a:p>')

def body_ph(sid=3, idx=1, xfrm=BODY_XFRM):
    paras = "".join(
        f'<a:p><a:pPr lvl="{l}"/><a:r><a:rPr lang="en-US"/><a:t>{t}</a:t></a:r></a:p>'
        for l, t in [(0, "Click to edit text"), (1, "Second level"), (2, "Third level")])
    return _sp(sid, "Content Placeholder", f'<p:ph type="body" idx="{idx}"/>', xfrm,
               f'<a:bodyPr {INS} anchor="t"><a:normAutofit/></a:bodyPr>',
               paras)

def num_ph(sid=4, idx=12):
    fld = ('<a:p><a:pPr algn="r"><a:buNone/></a:pPr>'
           '<a:fld id="{B7B2E8A1-0C3D-4F6E-9A11-5D3C7E0A4F21}" type="slidenum">'
           '<a:rPr lang="en-US" sz="1200"><a:solidFill><a:srgbClr val="8B8B8B"/></a:solidFill>'
           '<a:latin typeface="+mn-lt"/></a:rPr><a:t>#</a:t></a:fld>'
           '<a:endParaRPr lang="en-US" sz="1200"><a:solidFill><a:srgbClr val="8B8B8B"/></a:solidFill></a:endParaRPr></a:p>')
    return _sp(sid, "Slide Number Placeholder", f'<p:ph type="sldNum" sz="quarter" idx="{idx}"/>',
               NUM_XFRM,
               f'<a:bodyPr {INS} anchor="ctr"><a:noAutofit/></a:bodyPr>',
               fld)

def spTree(shapes):
    return ('<p:spTree><p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr>'
            '<p:grpSpPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/>'
            '<a:chOff x="0" y="0"/><a:chExt cx="0" cy="0"/></a:xfrm></p:grpSpPr>'
            + "".join(shapes) + '</p:spTree>')

"""L20 round 2 (2026-11-24, lecture Thu 26 Nov): students do not rank data themselves on the exam.

    python3 l20_round2.py <in.pptx> <out.pptx> <item> [<item> ...]

His call (2026-11-24): exam items never ask students to rank a dataset; they give the ranks. SORT stays on
slide 41 for practice. His ask: make that clear in the L20 slides. The notes copy the deck's own exam-note
box (slide 61, CustomShape 8: D4EA6B fill, accent1 outline, 16 pt, centred, 'Exam note!' and 'not' in bold)
and the recap's bold parenthesis on slide 66 ('(construction of sampling distribution not needed for exam)').

  note41      slide 41 (Results + the SORT tip): exam note right of the table, above the Tip box
  note73      slide 73 (Example Exam Question, raw scores; slide 74 ranks them): exam note left of the table
  recap       slides 66 and 67: 'Convert to ranks (remember the Excel tip!)' gets '(ranks are given on the exam)'
"""
import re, sys
from l16_round1 import L16Deck

EMU = 914400


def _note_template(d):
    x = d.get(d.sp(61))
    blk = [m.group(0) for m in re.finditer(r'<p:sp>.*?</p:sp>', x, flags=re.S) if 'name="CustomShape 8"' in m.group(0)]
    if len(blk) != 1 or 'Exam note!' not in blk[0]:
        sys.exit("slide 61's exam-note box not found")
    return blk[0]


def _runs(template, parts):
    """Runs in the template's style: parts are (text, bold)."""
    rpr = re.search(r'<a:rPr lang="en-US" sz="1600" b="0"[^>]*>.*?</a:rPr>', template, flags=re.S).group(0)
    out = []
    for text, bold in parts:
        r = rpr.replace(' b="0"', ' b="1"') if bold else rpr
        out.append(f'<a:r>{r}<a:t>{text}</a:t></a:r>')
    return ''.join(out)


def _add_note(d, n, name, x, y, cx, cy, parts):
    t = _note_template(d)
    s = d.get(d.sp(n))
    if f'name="{name}"' in s:
        sys.exit(f'slide {n} already has {name}')
    new_id = max(int(i) for i in re.findall(r'<p:cNvPr id="(\d+)"', s)) + 1
    blk = re.sub(r'<p:cNvPr id="\d+" name="CustomShape 8"/>', f'<p:cNvPr id="{new_id}" name="{name}"/>', t)
    blk = re.sub(r'<a:off x="\d+" y="\d+"/><a:ext cx="\d+" cy="\d+"/>',
                 f'<a:off x="{round(x * EMU)}" y="{round(y * EMU)}"/><a:ext cx="{round(cx * EMU)}" cy="{round(cy * EMU)}"/>', blk)
    p_open = re.search(r'<a:p><a:pPr algn="ctr">.*?</a:pPr>', blk, flags=re.S).group(0)
    end = re.search(r'<a:endParaRPr[^>]*/>', blk).group(0)
    body = p_open + _runs(t, parts) + end + '</a:p>'
    blk = re.sub(r'<a:p>.*</a:p>', lambda _: body, blk, flags=re.S)
    if s.count('</p:spTree>') != 1:
        sys.exit(f'slide {n}: spTree end not unique')
    d.put(d.sp(n), s.replace('</p:spTree>', blk + '</p:spTree>'))
    print(f'  slide {n}: {name} (id {new_id}) {cx:.2f} x {cy:.2f} in at ({x:.2f}, {y:.2f})')


def item_note41(d):
    # table 0.89-8.20 x 1.81-5.55 in, Tip box from y 5.00: the note sits right of the table
    _add_note(d, 41, 'Exam note ranks', 8.55, 1.95, 4.00, 1.00,
              [('Exam note!', True), (' You do ', False), ('not', True),
               (' need to rank the data yourself on the exam. We give you the ranks.', False)])


def item_note73(d):
    # table 5.05-12.60 x 3.19-5.19 in; the space left of it is empty
    _add_note(d, 73, 'Exam note ranks', 0.95, 3.25, 3.85, 1.00,
              [('Exam note!', True), (' On the exam, we give you the ranks. Here, practice ranking the scores yourself first.', False)])


def item_recap(d):
    for n in (66, 67):
        s = d.get(d.sp(n))
        m = re.search(r'(<a:rPr lang="en-US" sz="2600" b="0"[^>]*>.*?</a:rPr>)<a:t>Convert to ranks \(remember the Excel tip!\)</a:t></a:r>', s, flags=re.S)
        if not m or s.count('Convert to ranks (remember the Excel tip!)') != 1:
            sys.exit(f'slide {n}: ranks bullet not found once')
        rpr = m.group(1)
        bold = rpr.replace(' b="0"', ' b="1"')
        new = (f'{rpr}<a:t>Convert to ranks (remember the Excel tip!) </a:t></a:r>'
               f'<a:r>{bold}<a:t>(ranks are given on the exam)</a:t></a:r>')
        d.put(d.sp(n), s.replace(m.group(0), new))
        print(f'  slide {n}: ranks bullet + (ranks are given on the exam)')


ITEMS = {'note41': item_note41, 'note73': item_note73, 'recap': item_recap}

if __name__ == '__main__':
    src, out, *items = sys.argv[1:]
    d = L16Deck(src)
    for it in items:
        print(f'[{it}]')
        ITEMS[it](d)
    d.save(out)

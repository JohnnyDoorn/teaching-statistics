"""Redraw the L12 exam-question solution image (slides 75-76): t.005 = 2.704 -> 2.7045,
17.82 -> 17.81, 21.78 -> 21.79. Existing glyphs are reused where the image has them
('1', '9'); only the new '5' is drawn, in Cambria Math at the image's own size."""
import sys
from PIL import Image, ImageFont, ImageDraw, ImageChops, ImageStat
F = '/Applications/Microsoft PowerPoint.app/Contents/Resources/DFonts/Cambria.ttc'
src, out = sys.argv[1:3]
im = Image.open(src).convert('RGBA'); A = im.split()[3]          # ink = alpha (text is black)

def render(s, sz):
    f = ImageFont.truetype(F, sz, index=1)
    m = Image.new('L', (600, 140), 0); ImageDraw.Draw(m).text((20, 20), s, font=f, fill=255)
    return m, f

def align(s, sz, box):
    """Best (dx, dy) placing rendered s so it matches the ink in box (x0, y0, x1, y1)."""
    m, f = render(s, sz); bb = m.getbbox()
    tgt = A.crop(box); best = None
    for ox in range(-3, 4):
        for oy in range(-3, 4):
            canvas = Image.new('L', A.size, 0)
            canvas.paste(m.crop(bb), (box[0] + ox, box[1] + oy))
            d = ImageStat.Stat(ImageChops.difference(canvas.crop(box), tgt)).sum[0]
            if best is None or d < best[0]: best = (d, ox, oy)
    _, ox, oy = best
    return m, f, bb, (box[0] + ox - bb[0], box[1] + oy - bb[1])   # offset to add to rendered coords

def ink_cols(m, x0, x1):
    cols = [x for x in range(x0, x1) if any(m.getpixel((x, y)) > 40 for y in range(m.size[1]))]
    return cols

SZ = 53.75
def add_five(line_box, rows, shift_from=None):
    """Append '5' after '2.704' whose ink box is line_box; optionally shift ink right of shift_from."""
    m, f, bb, (ox, oy) = align('2.704', SZ, line_box)
    m2, _ = render('2.7045', SZ)
    # the '5': columns of m2 beyond the end of '2.704' in m (same origin, same font)
    end4 = bb[2]
    five = m2.crop((end4, 0, m2.size[0], m2.size[1])); fb = five.getbbox()
    adv = f.getlength('5')
    global A
    if shift_from is not None:
        y0, y1 = rows
        right = A.crop((shift_from, y0, A.size[0] - 60, y1))
        A.paste(0, (shift_from, y0, A.size[0], y1))
        A.paste(right, (shift_from + round(adv), y0))
    glyph = five.crop(fb)
    x, y = end4 + fb[0] + ox, fb[1] + oy
    A.paste(ImageChops.lighter(A.crop((x, y, x + glyph.size[0], y + glyph.size[1])), glyph), (x, y))
    return round(adv)

def swap_digit(src_box, dst_box, word, idx_src, idx_dst, sz=SZ):
    """Replace the glyph in dst_box by a copy of the glyph in src_box (same line), placed where
    the font puts character idx_dst of `word` relative to character idx_src."""
    global A
    m, f = render(word, sz)
    # ink-left of each character, from cumulative advances
    xs = [f.getlength(word[:k]) for k in range(len(word) + 1)]
    lsb = lambda ch: render(ch, sz)[0].getbbox()[0] - 20
    left_src = xs[idx_src] + lsb(word[idx_src]); left_dst = xs[idx_dst] + lsb(word[idx_dst])
    glyph = A.crop(src_box)
    A.paste(0, (dst_box[0] - 2, dst_box[1] - 2, dst_box[2] + 3, dst_box[3] + 3))
    x = round(src_box[0] + (left_dst - left_src))
    # first digit of the target word sits at word_x0; src glyph is that first digit or another word
    return glyph, x

# t line: '2.704' ink (242..367, 339..374), nothing to its right until x = 482
add_five((242, 339, 368, 375), None)
# equation lines: '2.704' at x 564..689; shift the fraction and the result right first
dx2 = add_five((564, 502, 690, 539), (478, 577), shift_from=690)
dx3 = add_five((564, 611, 690, 648), (586, 690), shift_from=690)
assert dx2 == dx3
dx = dx2
f = render('0', SZ)[1]
lsb = lambda ch: render(ch, SZ)[0].getbbox()[0] - 20
def place(glyph_box, word, k_src, word_left_ink, k_dst, clear_box):
    """Copy the glyph at glyph_box onto character k_dst of `word`, whose first ink starts at word_left_ink."""
    global A
    g = A.crop(glyph_box)
    x_first = word_left_ink - lsb(word[0])                     # pen position of the word
    x = round(x_first + f.getlength(word[:k_dst]) + lsb(word[k_dst]))
    A.paste(0, clear_box)
    A.paste(g, (x, glyph_box[1]))
    return x
# line 2: '17.82' (after the shift) -> '17.81', reuse its own first '1'
x = place((847 + dx, 502, 868 + dx, 539), '17.81', 0, 847 + dx, 4, (943 + dx, 498, 975 + dx, 543))
print('line 2 new 1 at', x)
# line 3: '21.78' -> '21.79', reuse the '9' of '19.8' on the same line (427..450)
x = place((427, 611, 451, 649), '21.79', 0, 846 + dx, 4, (943 + dx, 607, 975 + dx, 652))
print('line 3 new 9 at', x)

rgba = Image.merge('RGBA', (Image.new('L', A.size, 0),) * 3 + (A,))
rgba.save(out, dpi=(150, 150)); print('shift dx =', dx, 'written', out)

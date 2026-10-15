"""L12 slide 62: n = 0.98^2 x 2.58^2 / 0.3^2 = 71.03 -> rounds up to 72 (Agresti p. 419).
Replace the '1' of '71' by the image's own '2' (the red one in 2.58, recoloured black),
placed where Latin Modern puts the second digit of '72'."""
import sys
from PIL import Image, ImageFont, ImageDraw
src, out = sys.argv[1:3]
im = Image.open(src).convert('RGBA'); W, H = im.size; A = im.split()[3]
LM = '/usr/local/texlive/2023/texmf-dist/fonts/opentype/public/lm/lmroman10-regular.otf'
def bbox_in(x0, x1, y0, y1):
    return A.crop((x0, y0, x1 + 1, y1)).getbbox()
# the red '2' of 2.58 sits in the numerator (rows above the fraction bar)
b2 = bbox_in(757, 790, 0, 80); two = (757 + b2[0], b2[1], 757 + b2[2], b2[3])
# '7' and '1' of '71' on the main line
b7 = bbox_in(1062, 1098, 0, H); seven = (1062 + b7[0], b7[1], 1062 + b7[2], b7[3])
b1 = bbox_in(1107, 1134, 0, H); one = (1107 + b1[0], b1[1], 1107 + b1[2], b1[3])
print('2:', two, ' 7:', seven, ' 1:', one)
# Latin Modern: em size from the '7' height, then the ink-left of '2' in '72' relative to '7'
h7 = seven[3] - seven[1]
def ink(s, sz):
    f = ImageFont.truetype(LM, sz); m = Image.new('L', (800, 300), 0); ImageDraw.Draw(m).text((50, 50), s, font=f, fill=255)
    return m.getbbox(), f
sz = min(range(60, 160), key=lambda s: abs((ink('7', s)[0][3] - ink('7', s)[0][1]) - h7))
f = ink('7', sz)[1]
lsb = lambda ch: ink(ch, sz)[0][0] - 50
x2 = seven[0] - lsb('7') + f.getlength('7') + lsb('2')
glyph = im.crop(two)
r, g, b, a = glyph.split()
black = Image.merge('RGBA', (Image.new('L', glyph.size, 0),) * 3 + (a,))
# baseline: '2' bottom sits on the same baseline as the '7' bottom (digits have no descenders)
y2 = seven[3] - (two[3] - two[1])
clear = Image.new('RGBA', (one[2] - one[0] + 4, H), (255, 255, 255, 0))
im.paste(clear, (one[0] - 2, 0))
im.alpha_composite(black, (round(x2), y2))
print('LM size', sz, 'placed 2 at', round(x2), y2)
im.save(out, dpi=(600, 600))

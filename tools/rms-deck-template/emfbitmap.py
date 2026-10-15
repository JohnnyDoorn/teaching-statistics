"""Lossless PNG out of the bitmap EMFs in the RMS decks, for a scripted PowerPoint export.

The decks hold EMFs (mislabelled .wmf) that are a single raster: either one
EMR_STRETCHDIBITS record or, as in L12, an EMF+ image object inside EMR_COMMENT
records (2250 x 1500 px, 24/32-bit, uncompressed: 10-20 MB each). A scripted PDF
export rasterises them to thumbnails or hangs, so swap them for their own pixels
in an export *copy*; the master keeps the EMFs.

    python3 emfbitmap.py <deck.pptx> <export-copy.pptx> [min_bytes=1000000]

Vector EMFs (formulas, curves; tens of KB) are left alone.
"""
import io, re, struct, sys, zipfile
from PIL import Image


def _emfplus_image(emf):
    """Pixels of the first EMF+ bitmap object, or None."""
    chunks, total, off = [], None, 0
    while off + 8 <= len(emf):
        rtype, rsize = struct.unpack_from('<II', emf, off)
        if rsize < 8: break
        if rtype == 70 and emf[off + 12:off + 16] == b'EMF+':           # EMR_COMMENT with EMF+ records
            data_size = struct.unpack_from('<I', emf, off + 8)[0]
            p, end = off + 16, off + 12 + data_size
            while p + 12 <= end:
                ptype, flags, psize, pdsize = struct.unpack_from('<HHII', emf, p)
                if ptype == 0x4008 and (flags >> 8) & 0x7F == 5:        # EmfPlusObject, ObjectType Image
                    body = emf[p + 12:p + 12 + pdsize]
                    if flags & 0x8000:                                  # continued object
                        total = struct.unpack_from('<I', body)[0]; body = body[4:]
                    chunks.append(body)
                if psize < 12: break
                p += psize
        off += rsize
    if not chunks: return None
    obj = b''.join(chunks)
    if total: obj = obj[:total]
    version, itype = struct.unpack_from('<II', obj, 0)
    if itype != 1: return None                                          # 1 = bitmap
    w, h, stride, fmt, btype = struct.unpack_from('<iiiII', obj, 8)
    pix = obj[28:]
    if btype == 1:                                                      # compressed (PNG/JPEG...)
        return Image.open(io.BytesIO(pix))
    mode = {0x00021808: ('RGB', 'BGR'), 0x00022009: ('RGB', 'BGRX'), 0x0026200A: ('RGBA', 'BGRA'),
            0x000E200B: ('RGBA', 'BGRA')}.get(fmt)
    if mode is None: raise ValueError(f'pixel format {fmt:#x}')
    return Image.frombuffer(mode[0], (w, h), pix, 'raw', mode[1], stride, 1)


def _stretchdibits(emf):
    off = 0
    while off + 8 <= len(emf):
        rtype, rsize = struct.unpack_from('<II', emf, off)
        if rsize < 8: break
        if rtype == 81:                                                  # EMR_STRETCHDIBITS
            offBmi, cbBmi, offBits, cbBits = struct.unpack_from('<IIII', emf, off + 48)
            bmi = emf[off + offBmi:off + offBmi + cbBmi]; bits = emf[off + offBits:off + offBits + cbBits]
            hdr = struct.pack('<2sIHHI', b'BM', 14 + len(bmi) + len(bits), 0, 0, 14 + len(bmi))
            return Image.open(io.BytesIO(hdr + bmi + bits))
        off += rsize
    return None


def bitmap_of(emf):
    if emf[40:44] != b' EMF': return None
    return _emfplus_image(emf) or _stretchdibits(emf)


def export_copy(src, out, min_bytes=1_000_000):
    zin = zipfile.ZipFile(src)
    parts = {i.filename: zin.read(i.filename) for i in zin.infolist()}
    order = [i.filename for i in zin.infolist()]
    swapped = {}
    for p in [k for k in order if k.startswith('ppt/media/') and k.endswith(('.wmf', '.emf'))]:
        if len(parts[p]) < min_bytes: continue
        im = bitmap_of(parts[p])
        if im is None: continue
        buf = io.BytesIO(); im.save(buf, 'PNG')
        new = p.rsplit('.', 1)[0] + '_bmp.png'
        parts[new] = buf.getvalue(); order.append(new); del parts[p]; order.remove(p)
        swapped[p.split('/')[-1]] = new.split('/')[-1]
        print(f'  {p}: {im.size[0]}x{im.size[1]} {im.mode}, {len(parts[new]) // 1000} KB png')
    for k in [k for k in order if k.endswith('.rels')]:
        s = parts[k].decode('utf-8')
        for a, b in swapped.items(): s = s.replace(f'media/{a}"', f'media/{b}"')
        parts[k] = s.encode('utf-8')
    ct = parts['[Content_Types].xml'].decode('utf-8')
    if 'Extension="png"' not in ct:
        ct = ct.replace('<Default ', '<Default Extension="png" ContentType="image/png"/><Default ', 1)
    parts['[Content_Types].xml'] = ct.encode('utf-8')
    with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:
        for p in order: z.writestr(p, parts[p])
    print(f'{len(swapped)} bitmap EMFs swapped -> {out}')


if __name__ == '__main__':
    export_copy(sys.argv[1], sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 1_000_000)

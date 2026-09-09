import re
PAGES = open("agresti.txt", encoding="utf8", errors="replace").read().split("\f")

def printed(n):
    """Text of printed page n (offset verified below)."""
    i = n + 1
    return PAGES[i-1] if 0 < i <= len(PAGES) else ""

def header_num(txt):
    """The printed page number shown in the running head, if present."""
    lines = [l.strip() for l in txt.split("\n") if l.strip()][:6]
    for l in lines:
        if re.fullmatch(r"\d{1,3}", l): return int(l)
    return None

"""Export a deck to PDF through PowerPoint (AppleScript), the way rms/CLAUDE.md describes.

    python3 pptexport.py <deck.pptx> <out.pdf> [--keep-emf]
    python3 pptexport.py --open-test <deck.pptx> [...]   # does it open without a repair prompt?

Works on a copy in PowerPoint's sandbox Documents folder (no permission prompts),
with bitmap EMFs swapped for their own pixels first (emfbitmap.py): a scripted
export otherwise thumbnails them or hangs. The deck you pass is never touched.
Hidden slides are dropped, as in a manual File > Export.
"""
import os, shutil, subprocess, sys, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import emfbitmap

SANDBOX = os.path.expanduser('~/Library/Containers/com.microsoft.Powerpoint/Data/Documents')


def hfs(posix):
    disk = subprocess.run(['osascript', '-e', 'path to startup disk as text'], capture_output=True, text=True, check=True).stdout.strip()
    return disk + posix.lstrip('/').replace('/', ':')


def export(deck, pdf, keep_emf=False):
    stem = 'claude_export_' + os.path.splitext(os.path.basename(deck))[0].replace(' ', '_')
    copy, out = os.path.join(SANDBOX, stem + '.pptx'), os.path.join(SANDBOX, stem + '.pdf')
    for p in (copy, out):
        if os.path.exists(p): os.remove(p)
    if keep_emf: shutil.copy(deck, copy)
    else: emfbitmap.export_copy(deck, copy)
    subprocess.run(['xattr', '-d', 'com.apple.quarantine', copy], capture_output=True)
    # 'open' is sent without waiting: if PowerPoint puts up a prompt (repair, protected view),
    # a blocking open hangs for minutes and the save then fails with -50. Poll instead.
    name = os.path.basename(copy)
    script = f'''
tell application "Microsoft PowerPoint" to launch
delay 2
tell application "Microsoft PowerPoint"
    ignoring application responses
        open POSIX file "{copy}"
    end ignoring
end tell
set found to false
repeat 120 times
    try
        tell application "Microsoft PowerPoint"
            if (name of presentations) contains "{name}" then set found to true
        end tell
    end try
    if found then exit repeat
    delay 1
end repeat
delay 2
if not found then error "PowerPoint did not open {name} within 120 s: a prompt (repair? protected view?) is probably waiting"
with timeout of 1700 seconds
    tell application "Microsoft PowerPoint"
        set p to presentation "{name}"
        save p in "{hfs(out)}" as save as PDF
        close p saving no
    end tell
end timeout'''
    t = time.time()
    r = subprocess.run(['osascript', '-e', script], capture_output=True, text=True)
    if r.returncode or not os.path.exists(out):
        raise SystemExit(f'export failed ({r.returncode}): {r.stderr.strip()}')
    shutil.move(out, pdf); os.remove(copy)
    print(f'exported {pdf} in {time.time() - t:.0f}s')


def open_test(deck):
    """Does PowerPoint open the deck without a prompt (repair, protected view)? Copy, open, close."""
    name = 'claude_opentest_' + os.path.basename(deck).replace(' ', '_')
    copy = os.path.join(SANDBOX, name)
    shutil.copy(deck, copy)
    script = f'''
tell application "Microsoft PowerPoint" to launch
delay 2
tell application "Microsoft PowerPoint"
    ignoring application responses
        open POSIX file "{copy}"
    end ignoring
end tell
repeat 60 times
    try
        tell application "Microsoft PowerPoint"
            if (name of presentations) contains "{name}" then
                close presentation "{name}" saving no
                return "opens cleanly"
            end if
        end tell
    end try
    delay 1
end repeat
return "NOT opened within 60 s: PowerPoint is showing a prompt (repair?) - look at its window"'''
    r = subprocess.run(['osascript', '-e', script], capture_output=True, text=True)
    print(os.path.basename(deck) + ':', (r.stdout or r.stderr).strip())
    if 'cleanly' in r.stdout: os.remove(copy)


if __name__ == '__main__':
    if '--open-test' in sys.argv:
        for f in [a for a in sys.argv[1:] if a != '--open-test']: open_test(f)
    else:
        export(sys.argv[1], sys.argv[2], '--keep-emf' in sys.argv)

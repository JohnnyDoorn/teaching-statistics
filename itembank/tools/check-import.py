#!/usr/bin/env python3
"""Check an ANS import: compare a re-export of the bank with what the build expected.

    python3 tools/check-import.py EXPORT_DIR --expected examples/EXAMPLES.expected.json

After importing a package, export the question bank from ANS (Settings > Export) as a
QTI 2.X export and, optionally, as a Respondus package, and unzip both into EXPORT_DIR.
--expected is the <NAME>.expected.json that build-ans.R writes next to the package.
Either export may be missing; its checks are then skipped. Items are found by title,
so the rest of the bank is ignored. One line per check; "XX" marks a failure.
"""

import csv, glob, html, json, os, re, sys
from collections import defaultdict

args = sys.argv[1:]
if "--expected" not in args or len(args) < 3:
    sys.exit(__doc__)
i = args.index("--expected")
EXPECTED = args[i + 1]
del args[i:i + 2]
ROOT = args[0]
expected = json.load(open(EXPECTED, encoding="utf-8"))
ROUND = None        # the import tests ran in rounds; a build's expected file has none
PREFIX = ""         # items are found by their full title


def text(s):
    s = re.sub(r"<(td|th)\b[^>]*/?>", " | ", s)
    s = re.sub(r"</?(p|div|br|li|tr|figure|table)\b[^>]*/?>", "\n", s)
    s = html.unescape(re.sub(r"<[^>]+>", "", s))
    return re.sub(r"[ \t ]+", " ", s)


def flat(s):
    return re.sub(r"\s+", " ", text(s)).strip()


# ---------------------------------------------------------------- QTI 2.1 export
qti = defaultdict(list)                 # title -> [part dicts], in part order
qti_dir = None
def current(paths):
    """Skip anything under an old/ folder: earlier exports parked next to new ones."""
    return [p for p in paths if "old" not in os.path.relpath(p, ROOT).split(os.sep)[:-1]]


for f in current(sorted(glob.glob(os.path.join(ROOT, "**", "QUE_*.xml"), recursive=True))):
    s = open(f, encoding="utf-8").read()
    m = re.search(r'<assessmentItem [^>]*title="([^"]*)"', s)
    if not m or PREFIX not in m.group(1):
        continue
    qti_dir = os.path.dirname(f)
    body = s[s.find("<itemBody"):s.find("</itemBody>")]
    rubric = "".join(re.findall(r'<rubricBlock view="scorer">(.*?)</rubricBlock>', s, re.S))
    feedback = {k: flat(v) for k, v in re.findall(
        r'<modalFeedback [^>]*identifier="(correct|incorrect)"[^>]*>(.*?)</modalFeedback>', s, re.S)}
    correct = re.findall(r"<correctResponse>\s*<value>([^<]*)</value>", s)
    weights = dict(re.findall(r'mapKey="([^"]+)" mappedValue="([-\d.]+)"', s))
    choices = re.findall(r'<simpleChoice identifier="([^"]+)"[^>]*>(.*?)</simpleChoice>', s, re.S)
    maxscore = re.search(r'identifier="MAXSCORE".*?<value>([\d.]+)</value>', s, re.S)
    crit = [(float(p), flat(d or "")) for p, d in
            re.findall(r'<criterion points="([\d.]+)"(?:/>|>(.*?)</criterion>)', s, re.S)]
    stem = re.sub(r"<rubricBlock.*?</rubricBlock>|<simpleChoice.*?</simpleChoice>", "",
                  body, flags=re.S)
    refs = re.findall(r'(?:src|href)="([^"]+)"', body)
    qti[html.unescape(m.group(1))].append(dict(
        n=int(re.search(r"QUE_\d+_(\d+)", f).group(1)),
        id=re.search(r"QUE_(\d+)_", f).group(1),
        stem=flat(stem), raw=body, rubric=flat(rubric), crit=crit, feedback=feedback,
        points=float(maxscore.group(1)) if maxscore else None,
        choices=[(cid, flat(c)) for cid, c in choices],
        correct=correct, weights=weights, full=s, dir=os.path.dirname(f),
        basetype=re.findall(r'<responseDeclaration [^>]*baseType="(\w+)"', s),
        refs=refs))
for t in qti:
    qti[t].sort(key=lambda p: p["n"])

# ---------------------------------------------------------------- Respondus export
resp = defaultdict(list)
resp_dir = None
for f in current(glob.glob(os.path.join(ROOT, "**", "*.csv"), recursive=True)):
    with open(f, encoding="utf-8-sig", newline="") as fh:
        rows = list(csv.reader(fh, delimiter=";"))
    if not rows or rows[0][:2] != ["Question Type", "Question Title/Id"]:
        continue
    resp_dir = os.path.dirname(f)
    H = rows[0]
    for r in rows[1:]:
        if PREFIX not in r[1]:
            continue
        d = dict(zip(H, r))
        d["dir"] = os.path.dirname(f)
        base = re.sub(r"_\d+$", "", r[1])
        resp[base].append(d)

# ---------------------------------------------------------------- checks
results = []


def check(item, what, ok, got=""):
    results.append((item, what, "ok" if ok else "FAIL", got))


def exists(folder, ref):
    """ANS's exports put files next to the items; our packages keep them under media/."""
    return folder and (os.path.exists(os.path.join(folder, os.path.basename(ref))) or
                       os.path.exists(os.path.join(folder, ref)))


for title, e in expected.items():
    if ROUND is not None and e.get("round", 1) != ROUND:
        continue
    short = title[:34]
    parts = qti.get(title, [])
    rows = resp.get(title, [])
    n_parts = e.get("parts", 1)
    ma = e.get("model_answer")
    answers = ma if isinstance(ma, list) else [ma]
    pts = e["points"] if isinstance(e["points"], list) else [e["points"]]

    # present, as one exercise with the right number of questions
    if qti_dir and e.get("qti_export", True):
        check(short, "QTI: parts", len(parts) == n_parts, len(parts))
    if resp_dir:
        check(short, "Respondus: parts", len(rows) == n_parts, len(rows))

    for i in range(n_parts):
        tag = f"part {i + 1}: " if n_parts > 1 else ""
        want = answers[i] if i < len(answers) else answers[-1]
        if i < len(parts):
            p = parts[i]
            if want:
                check(short, f"QTI: {tag}model answer", want in p["rubric"], p["rubric"][:70])
                # The one failure that matters most: the answer in student-facing text.
                check(short, f"QTI: {tag}answer NOT visible to students", want not in p["stem"],
                      "LEAKED into the question text")
            check(short, f"QTI: {tag}points", p["points"] == pts[i], p["points"])
        if i < len(rows):
            r = rows[i]
            gf = flat(r["General Feedback"])
            if want:
                check(short, f"Respondus: {tag}model answer", want in gf, gf[:70])
            check(short, f"Respondus: {tag}points", float(r["Points"]) == pts[i], r["Points"])

    for i, fb in enumerate(e.get("feedback", [])):
        if fb and i < len(parts):
            tag = f"part {i + 1}: " if n_parts > 1 else ""
            for outcome in ("correct", "incorrect"):
                got = parts[i]["feedback"].get(outcome, "")
                check(short, f"QTI: {tag}feedback ({outcome})", fb in got, got[:70] or "none")

    for i, snip in enumerate(e.get("leak_check", [])):
        if snip and i < len(parts) and not (answers[i] if i < len(answers) else None):
            tag = f"part {i + 1}: " if n_parts > 1 else ""
            check(short, f"QTI: {tag}answer NOT visible to students", snip not in parts[i]["stem"],
                  "LEAKED into the question text")

    if "correct" in e and parts:
        p = parts[0]
        ids = [cid for cid, _ in p["choices"]]
        got = ([i + 1 for i, c in enumerate(ids) if float(p["weights"].get(c, 0)) > 0]
               if p["weights"] else [ids.index(c) + 1 for c in p["correct"] if c in ids])
        check(short, "QTI: correct options", got == e["correct"], got)
        if p["weights"]:
            check(short, "QTI: option weights", True,
                  [p["weights"].get(c) for c in ids])
        check(short, "QTI: number of options", len(ids) == e["n_choices"], len(ids))

    if "accepted" in e and parts:
        acc = e["accepted"] if isinstance(e["accepted"][0], list) else [e["accepted"]]
        for i, want in enumerate(acc):
            if i < len(parts):
                got = sorted(set(re.findall(r'mapKey="([^"]+)"', parts[i]["full"])))
                check(short, f"QTI: part {i + 1} accepted answers" if len(acc) > 1
                      else "QTI: accepted answers", sorted(want) == got,
                      f"{got} baseType={parts[i]['basetype']}")
    if "accepted" in e and rows:
        acc = e["accepted"] if isinstance(e["accepted"][0], list) else [e["accepted"]]
        for i, want in enumerate(acc):
            if i < len(rows):
                r = rows[i]
                got = [r[f"Choice {k}"] for k in range(1, 11) if r[f"Choice {k}"]] or \
                      [r["Correct Answer"]]
                check(short, "Respondus: accepted answers", sorted(want) == sorted(got), got)

    if "criteria" in e and parts:
        for i, want in enumerate(e["criteria"]):
            if i < len(parts):
                got = parts[i]["crit"]
                ok = [w[0] for w in want] == [g[0] for g in got]
                check(short, f"QTI: part {i + 1} criteria points" if len(e["criteria"]) > 1
                      else "QTI: criteria points", ok, [g[0] for g in got])
                names = [w[1] for w in want if w[1]]
                # ANS's 2.1 export writes a criterion's description, never its name,
                # so name-only criteria cannot be checked from it.
                if names and any(g[1] for g in got):
                    check(short, f"QTI: part {i + 1} criteria text",
                          all(any(n in g[1] for g in got) for n in names),
                          [g[1][:40] for g in got])

    if "tags" in e and rows:
        tags = {}
        for r in rows[:1]:
            if r["Topic"]:
                tags["Topic"] = r["Topic"]
            for k in range(1, 5):
                if "," in r[f"Meta {k}"]:
                    key, val = r[f"Meta {k}"].split(",", 1)
                    tags[key] = val
        for k, v in e["tags"].items():
            check(short, f"Respondus: tag {k}", tags.get(k) == v, tags.get(k))

    if e.get("maths") and parts:
        raw = parts[0]["raw"]
        for m in e["maths"]:
            check(short, f"QTI: maths {m}", m in html.unescape(raw), "")

    if e.get("table") and parts:
        check(short, "QTI: table in part 1", "<table" in parts[0]["raw"], "")

    if "scenario_in_part_1" in e:
        if parts:
            check(short, "QTI: scenario in part 1", e["scenario_in_part_1"] in parts[0]["stem"],
                  parts[0]["stem"][:60])
        if rows:
            check(short, "Respondus: scenario in part 1",
                  e["scenario_in_part_1"] in flat(rows[0]["Question Wording"]),
                  flat(rows[0]["Question Wording"])[:60])

    if "content_block" in e and rows:
        # The Respondus export leaves a real text block out of question 1 entirely.
        w = flat(rows[0]["Question Wording"])
        inq1 = e["content_block"] in w
        check(short, "scenario imported as a separate text block", not inq1,
              "merged into question 1" if inq1 else "")

    for kind in ("attachments", "images"):
        for name in e.get(kind, []):
            if parts:
                refs = [r for p in parts for r in p["refs"]]
                hit = [r for r in refs if r.endswith(os.path.splitext(name)[1])
                       and not r.startswith(("http://", "https://"))]   # web links are not shipped
                check(short, f"QTI: {name} linked and shipped",
                      bool(hit) and all(exists(parts[0]["dir"], r) for r in hit), hit)
            # Fill-ins imported through QTI export to Respondus with an empty Question
            # Wording, so their links only show in the QTI export.
            if rows and any(r["Question Wording"].strip() for r in rows):
                blob = " ".join(r["Question Wording"] for r in rows)
                hit = [u for u in re.findall(r'(?:src|href)="+([^"]+)"', blob)
                       if u.endswith(os.path.splitext(name)[1])]
                check(short, f"Respondus: {name} linked and shipped",
                      bool(hit) and all(exists(rows[0]["dir"], u) for u in hit), hit)

w = max(len(r[1]) for r in results)
for item, what, verdict, got in results:
    flag = "  " if verdict == "ok" else "XX"
    print(f"{flag} {item}  {what:<{w}}  {str(got)[:90] if verdict != 'ok' or 'weights' in what else ''}")
n_fail = sum(r[2] == "FAIL" for r in results)
print(f"\n{len(results)} checks, {n_fail} failed")

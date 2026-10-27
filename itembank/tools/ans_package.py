#!/usr/bin/env python3
"""Reshape an exams2qti21() package into the QTI 2.1 dialect ANS imports cleanly.

Called by build-ans.R; not usually run by hand:

    python3 ans_package.py <exams2qti21.zip> <items.json> <out.zip> [--expected <file>]

items.json lists the items in exam order, as build-ans.R reads them from the R/exams
metadata: file name, type, exextra[anstitle], exextra[exercise], points.

Every rule below was settled by test imports into ANS at the University of Amsterdam
(Sept-Oct 2026; the verification table is in ../ans-agent-instructions.qmd):

* The Solution becomes the model answer, as <rubricBlock view="scorer">: at the end
  of itemBody for choice and open items, which ANS imports; AFTER </itemBody> for a
  fill-in item, because inside it ANS imports it as question text and the answer
  becomes visible. ANS ignores it after </itemBody>, but it keeps the record.
* A fill-in item also gets its Solution as question feedback, the same text for a
  correct and an incorrect response, in the exact form ANS's own export uses
  (<modalFeedback outcomeIdentifier="FEEDBACK" identifier="correct|incorrect">, after
  the scorer rubric), which is how ANS's own exports carry a fill-in's explanation.
  Confirmed by import (2026-10-06).
* A "**Criteria**" list at the end of an open item's Solution ("- 0.5 pt: ...")
  becomes ANS grading criteria (<rubricScoring>). They must add up to the points.
* Items sharing exextra[exercise] become ONE exercise with several questions: files
  QUE_<id>_<n>.xml with one title, which ANS regroups on import. Part 1 carries the
  scenario, which ANS shows inside question a.
* Figures and data files are listed as <file>s of the item that uses them. A separate
  manifest resource per file, which is what exams2qti21 writes, makes ANS create an
  empty exercise titled d41d8cd98f00b204e9800998ecf8427e.
* Maths becomes ANS's own equation component, $$...$$; MathML gets flattened.
* exams2qti21's own modalFeedback goes (ANS drops that form), and so do knitr's "plot of chunk" captions and
  the assessmentTest, which a question bank import does not need.

* A cloze of numeric gaps (extype: cloze, exclozetype: num|num|...) becomes ONE
  question with several gaps, written the way ANS exports its own multi-gap questions:
  a string responseDeclaration per gap whose correctResponse holds the value, the
  margin as a "low;high" range and a mapping (points / number of gaps each), and the
  map_response template instead of exams' own scoring. exams' version of a multi-gap
  question showed only its first gap in ANS (2026-09-22). Caution: ANS imports these
  gaps as alphanumeric (exact keys only), not as numeric gaps with the range (2026-10-26);
  for numeric answers prefer one num gap per question. Confirmed by import
  (2026-10-06).
* With --feedback (practice tests), every choice and fill-in item also gets its
  Solution as question feedback, so students see it after answering.

Two house rules stop the build when broken: a tick-all stem must say that at least one
statement is true (ANS's guessing correction assumes it), and every fill-in gap needs a
label right before it, written as a one-line Answerlist ("* $p$-value:").
"""

import copy
import hashlib
import html
import json
import re
import sys
import zipfile
import xml.etree.ElementTree as ET

QTI = "http://www.imsglobal.org/xsd/imsqti_v2p1"
XSI = "http://www.w3.org/2001/XMLSchema-instance"
ET.register_namespace("", QTI)
ET.register_namespace("xsi", XSI)
Q = "{%s}" % QTI
SCHEMA = ("http://www.imsglobal.org/xsd/imsqti_v2p1 "
          "http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p2.xsd")
IMAGE_EXT = (".png", ".jpg", ".jpeg", ".gif", ".svg")


def die(msg):
    sys.exit(f"_ans_package.py: {msg}")


def text_of(el):
    return re.sub(r"\s+", " ", "".join(el.itertext())).strip()


def num(x):
    return f"{float(x):g}"


# ---------------------------------------------------------------- per item
def strip_answer_list(sol):
    """exams2qti21 ends a choice item's feedback with each option and True/False."""
    if sol and sol[-1].tag == Q + "ul":
        lis = list(sol[-1])
        if lis and all(re.search(r"(True|False)\.?\s*$", text_of(li)) for li in lis):   # "* True." in our files
            sol = sol[:-1]
            if sol and sol[-1].tag == Q + "br":
                sol = sol[:-1]
    return sol


def take_criteria(sol, where):
    """Pull a '**Criteria**' paragraph and the list after it out of the Solution."""
    for i, el in enumerate(sol):
        if el.tag == Q + "p" and text_of(el).rstrip(":").lower() == "criteria":
            if i + 1 >= len(sol) or sol[i + 1].tag != Q + "ul":
                die(f"{where}: '**Criteria**' must be followed by a list")
            crit = []
            for li in sol[i + 1]:
                m = re.match(r"^(\d+(?:\.\d+)?)\s*(?:pt|pts|point|points)?\s*[:|-]\s*(.+)$",
                             text_of(li))
                if not m:
                    die(f"{where}: criterion not of the form '0.5 pt: ...': {text_of(li)!r}")
                crit.append((float(m.group(1)), m.group(2)))
            return sol[:i] + sol[i + 2:], crit
    return sol, []


def drop_chunk_captions(root):
    for parent in root.iter():
        for child in list(parent):
            if child.get("class") == "caption" and text_of(child).startswith("plot of chunk"):
                parent.remove(child)
    for img in root.iter(Q + "img"):
        if (img.get("alt") or "").startswith("plot of chunk"):
            img.set("alt", "")


def to_ans_maths(root):
    for span in root.iter(Q + "span"):
        if span.get("class") in ("math inline", "math display") and len(span) == 0:
            m = re.fullmatch(r"\s*\\[(\[](.*?)\\[)\]]\s*", span.text or "", re.S)
            if m:
                latex = m.group(1).strip()
                span.attrib.clear()
                span.set("class", "redactor-component")
                span.set("data-redactor-type", "equation")
                span.set("data-latex", latex)
                span.text = f"$${latex}$$"


def house_rules(body, inter, kind, where):
    """Two house rules, adopted after the round-4 import test (2026-10-01)."""
    if kind == "choiceInteraction" and inter.get("maxChoices", "1") != "1":
        stem = " ".join(text_of(el) for el in body if el is not inter)
        stem += " " + " ".join(text_of(el) for el in inter if el.tag == Q + "prompt")
        if "at least one" not in stem.lower():
            die(f"{where}: a tick-all question must say that at least one statement is "
                "true, because ANS's guessing correction assumes it. End the stem with: "
                "'Check the statement(s) that is/are true; at least one statement is true.'")
    if kind == "textEntryInteraction":
        parent = next(p for p in body.iter() if inter in list(p))
        label = parent.text or ""
        for sib in parent:
            if sib is inter:
                break
            label += text_of(sib) + (sib.tail or "")
        if not label.strip():
            die(f"{where}: the answer field has no label, and students sometimes miss an "
                "unlabelled field. Add a one-line Answerlist to the Question, e.g. "
                "'* $p$-value:'.")


def key_variants(k):
    """A numeric key as students may type it: 0.035, 0,035, .035 (house rule)."""
    out = [k]
    if re.fullmatch(r"-?\d*\.\d+", k):
        out.append(k.replace(".", ","))
        if re.match(r"-?0\.", k):
            out.append(k.replace("0.", ".", 1))
    return out


def ans_multigap(root, inter, points, where, alt=None):
    """Rewrite exams' cloze fill-ins into ANS's own (multi-)gap form (see the docstring).
    alt: extra accepted answers, gaps separated by "|", answers within a gap by spaces."""
    alts = [a.split() for a in str(alt).split("|")] if alt else []
    rp = root.find(Q + "responseProcessing")
    tol = {}
    for eq in (rp.iter(Q + "equal") if rp is not None else []):
        var = eq.find(Q + "variable")
        if var is not None and eq.get("tolerance"):
            tol[var.get("identifier")] = float(eq.get("tolerance").split()[0])
    weight = num(points / len(inter))
    order = [el.get("responseIdentifier") for el in inter]
    for decl in root.findall(Q + "responseDeclaration"):
        rid = decl.get("identifier")
        val = decl.findtext(f"{Q}correctResponse/{Q}value")
        if val is None:
            die(f"{where}: gap {rid} has no correct value")
        t = tol.get(rid, 0.0)
        gi = order.index(rid) if rid in order else 0
        keys = []
        for v in [val] + (alts[gi] if gi < len(alts) else []):
            try:
                x = float(v.replace(",", "."))
            except ValueError:
                keys.append(v)              # a text answer
                continue
            d = max(len(v.split(".")[1]) if "." in v else 0, 1)
            ts = f"{t:.10g}"                # keys like 0.70 arrive as "0.7": keep the
            if "." in ts and "e" not in ts:  # tolerance's decimals so the range survives
                d = max(d, len(ts.split(".")[1]))
            for k in key_variants(v.replace(",", ".")):
                if k not in keys:
                    keys.append(k)
            # a margin of 0 still needs a range key, or ANS makes the gap alphanumeric
            # (ANS's own export writes an exact numeric gap as "2.4" plus "2.4;2.4")
            kv = v.replace(",", ".")
            rng = f"{x - t:.{d}f};{x + t:.{d}f}" if t else f"{kv};{kv}"
            if rng not in keys:
                keys.append(rng)
        decl.clear()
        decl.attrib.update({"identifier": rid, "cardinality": "single", "baseType": "string"})
        cr = ET.SubElement(decl, Q + "correctResponse")
        for k in keys:
            ET.SubElement(cr, Q + "value").text = k
        mp = ET.SubElement(cr, Q + "mapping", {"defaultValue": "0"})
        for k in keys:
            ET.SubElement(mp, Q + "mapEntry", {"mapKey": k, "mappedValue": weight})
    for od in root.findall(Q + "outcomeDeclaration"):
        if od.get("identifier") not in ("SCORE", "MAXSCORE"):
            root.remove(od)
        elif od.get("identifier") == "MAXSCORE":    # exams sums per-gap points (0.999999)
            v = od.find(f"{Q}defaultValue/{Q}value")
            if v is not None:
                v.text = num(round(points, 3))
    if rp is not None:
        rp.clear()
        rp.set("template", "http://www.imsglobal.org/question/qti_v2p1/rptemplates/map_response")


def reshape(raw, meta, title, ident, where, feedback_all=False):
    root = ET.fromstring(raw)
    root.set("identifier", ident)
    root.set("title", title)
    body = root.find(Q + "itemBody")
    inter = [el for el in body.iter() if el.tag.endswith("Interaction")]
    multigap = (meta["type"] == "cloze" and
                all(el.tag == Q + "textEntryInteraction" for el in inter))
    if len(inter) != 1 and not multigap:
        die(f"{where}: {len(inter)} interactions; ANS takes one per question, except a "
            "cloze of numeric gaps. Split the parts into separate files sharing "
            "exextra[exercise].")
    kind = inter[0].tag[len(Q):]
    if kind == "choiceInteraction" and meta["type"] == "cloze":
        inter[0].set("shuffle", "true")     # a cloze choice gap is never shuffled otherwise
    for el in inter:
        house_rules(body, el, kind, where)
    if multigap:
        ans_multigap(root, inter, float(meta.get("points") or 1), where, meta.get("alt"))

    sol = []
    for fb in root.findall(Q + "modalFeedback"):
        sol.extend(list(fb))
        root.remove(fb)
    if kind == "choiceInteraction":
        sol = strip_answer_list(sol)
    sol, crit = take_criteria(sol, where)

    rubric = ET.Element(Q + "rubricBlock", {"view": "scorer"})
    rubric.extend(sol)
    after = list(root).index(body) + 1
    if kind == "textEntryInteraction":
        root.insert(after, rubric)          # never inside itemBody: it would leak
        after += 1
    else:
        body.append(rubric)
    # ANS imports no model answer for a fill-in, so its explanation goes in as question
    # feedback, written the way ANS exports it; with --feedback every choice item too
    if kind == "textEntryInteraction" or (feedback_all and kind == "choiceInteraction"):
        for outcome in ("correct", "incorrect"):
            fb = ET.Element(Q + "modalFeedback", {"outcomeIdentifier": "FEEDBACK",
                                                  "showHide": "show", "identifier": outcome})
            box = ET.SubElement(fb, Q + "div")
            box.extend(copy.deepcopy(sol))
            root.insert(after, fb)
            after += 1

    points = float(meta.get("points") or 1)
    if crit:
        if kind != "extendedTextInteraction":
            die(f"{where}: criteria only make sense on an open question")
        if abs(sum(p for p, _ in crit) - points) > 1e-9:
            die(f"{where}: criteria add up to {num(sum(p for p, _ in crit))}, "
                f"the item is worth {num(points)}")
        scoring = ET.Element(Q + "rubricScoring")
        for p, d in crit:
            c = ET.SubElement(scoring, Q + "criterion", {"points": num(p)})
            ET.SubElement(c, Q + "div").text = d
        root.insert(after, scoring)
    if kind == "extendedTextInteraction":
        rp = root.find(Q + "responseProcessing")
        if rp is not None:
            root.remove(rp)                 # ANS's own open questions carry none
        # exams2qti21 writes expectedLength="1000", which ANS may read as a length
        # limit; ANS's own open questions say 0.
        inter[0].set("expectedLength", "0")

    for parent in root.iter():              # empty <p>s exams leaves around an interaction
        for child in list(parent):
            if (child.tag == Q + "p" and len(child) == 0 and not (child.text or "").strip()):
                parent.remove(child)

    drop_chunk_captions(root)
    to_ans_maths(root)
    media = sorted({v for el in root.iter() for a in ("src", "href")
                    for v in [el.get(a)] if v and v.startswith("media/")})
    return root, kind, media, rubric, crit


def serialize(root):
    return ('<?xml version="1.0" encoding="UTF-8"?>\n' +
            ET.tostring(root, encoding="unicode")).encode("utf-8")


# ---------------------------------------------------------------- package
def manifest(resources):
    out = []
    for ident, title, itype, files in resources:
        filexml = "".join(f'\n      <file href="{html.escape(f, quote=True)}"/>' for f in files)
        out.append(f"""    <resource identifier="{ident}" type="imsqti_item_xmlv2p1" href="{ident}.xml">
      <metadata>
        <imsmd:lom>
          <imsmd:general>
            <imsmd:title>
              <imsmd:langstring xml:lang="en">{html.escape(title)}</imsmd:langstring>
            </imsmd:title>
          </imsmd:general>
          <imsmd:technical>
            <imsmd:format>text/x-imsqti-item-xml</imsmd:format>
          </imsmd:technical>
        </imsmd:lom>
        <imsqti:qtiMetadata>
          <imsqti:interactionType>{itype}</imsqti:interactionType>
        </imsqti:qtiMetadata>
      </metadata>{filexml}
    </resource>""")
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<manifest xmlns="http://www.imsglobal.org/xsd/imscp_v1p1" xmlns:imsmd="http://www.imsglobal.org/xsd/imsmd_v1p2" xmlns:imsqti="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" identifier="manifest" xsi:schemaLocation="http://www.imsglobal.org/xsd/imscp_v1p1 http://ltsc.ieee.org/xsd/LOM http://www.imsglobal.org/xsd/qti/qtiv2p1/qtiv2p1_imscpv1p2_v1p0.xsd http://www.imsglobal.org/xsd/imsmd_loose_v1p3p2.xsd http://www.imsglobal.org/xsd/imsqti_metadata_v2p1 http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_metadata_v2p1p1.xsd">
  <metadata>
    <schema>QTIv2.1 Package</schema>
    <schemaversion>1.0.0</schemaversion>
  </metadata>
  <resources>
{chr(10).join(out)}
  </resources>
</manifest>
"""


def snippet(el, words=6):
    """The first few words of a model answer, for checking a re-export against."""
    t = text_of(el)
    return " ".join(t.split()[:words]) if t else None


def main():
    args = sys.argv[1:]
    expected_path = None
    if "--expected" in args:
        i = args.index("--expected")
        expected_path = args[i + 1]
        del args[i:i + 2]
    src, meta_path, out = args
    meta = json.load(open(meta_path, encoding="utf-8"))
    items, prefix = meta["items"], meta.get("title_prefix") or ""

    zin = zipfile.ZipFile(src)
    names = zin.namelist()
    files = sorted((n for n in names if re.search(r"_section_\d+_item_\d+_\w+\.xml$", n)),
                   key=lambda n: int(re.search(r"_section_(\d+)_", n).group(1)))
    if len(files) != len(items):
        die(f"{len(files)} items in the package, {len(items)} in the metadata")

    # consecutive items sharing exextra[exercise] form one exercise
    groups = []
    for i, it in enumerate(items):
        key = it.get("exercise") or None
        if key and groups and items[groups[-1][0]].get("exercise") == key:
            groups[-1].append(i)
        else:
            if key and any(items[g[0]].get("exercise") == key for g in groups):
                die(f"the parts of exercise '{key}' are not consecutive; number their "
                    "files so they sort together")
            groups.append([i])

    used, resources, written, expected = set(), [], {}, {}
    for g in groups:
        first = items[g[0]]
        key = first.get("exercise") or first["rmd"]
        base = 10000000 + int(hashlib.md5(f"{meta['name']}:{key}".encode()).hexdigest()[:8], 16) % 90000000
        while base in used:
            base += 1
        used.add(base)
        title = prefix + (first.get("anstitle") or first["rmd"])
        for k in g[1:]:
            t = items[k].get("anstitle")
            if t and t != first.get("anstitle"):
                print(f"note: {items[k]['rmd']} has its own anstitle; the exercise uses "
                      f"part 1's: {first.get('anstitle')}", file=sys.stderr)
        exp = dict(route="qti21", parts=len(g), points=[], model_answer=[], leak_check=[],
                   feedback=[], criteria=[], attachments=[], images=[])
        for n, k in enumerate(g, 1):
            ident = f"QUE_{base}_{n}"
            root, kind, media, rubric, crit = reshape(zin.read(files[k]), items[k], title,
                                                      ident, items[k]["rmd"],
                                                      feedback_all=bool(meta.get("feedback_all")))
            for m in media:
                if m not in names:
                    die(f"{items[k]['rmd']}: refers to {m}, which is not in the package")
            written[f"{ident}.xml"] = serialize(root)
            resources.append((ident, title, kind, [f"{ident}.xml"] + media))
            ans = snippet(rubric)
            exp["points"].append(float(items[k].get("points") or 1))
            # ANS does not import a fill-in's model answer, but it must not leak either
            exp["model_answer"].append(None if kind == "textEntryInteraction" else ans)
            exp["leak_check"].append(ans)
            exp["feedback"].append(ans if kind == "textEntryInteraction" or (
                meta.get("feedback_all") and kind == "choiceInteraction") else None)
            exp["criteria"].append([(p, d[:40]) for p, d in crit])
            for m in media:
                (exp["images"] if m.lower().endswith(IMAGE_EXT) else exp["attachments"]).append(
                    m.rsplit("/", 1)[-1])
        if exp["parts"] == 1:
            exp["points"] = exp["points"][0]
            exp["model_answer"] = exp["model_answer"][0]
        if not any(exp["criteria"]):
            del exp["criteria"]
        if not any(exp["feedback"]):
            del exp["feedback"]
        expected[title] = exp

    with zipfile.ZipFile(out, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("imsmanifest.xml", manifest(resources))
        for name, data in written.items():
            z.writestr(name, data)
        for name in sorted({m for _, _, _, fs in resources for m in fs[1:]}):
            z.writestr(name, zin.read(name))
    if expected_path:
        json.dump(expected, open(expected_path, "w", encoding="utf-8"), indent=1,
                  ensure_ascii=False)
    n_ex = len(groups)
    n_q = len(items)
    print(f"{out}: {n_q} questions in {n_ex} exercises" +
          (f", {sum(len(g) > 1 for g in groups)} of them multi-part" if n_q > n_ex else ""))


if __name__ == "__main__":
    main()

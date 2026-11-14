#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Nightly vs-elixir subset (BT-4844): parse a bench report, compare two nights.

  vs_elixir_nightly.py parse   REPORT.md --out results.json [--meta k=v ...] [--expect ID ...]
  vs_elixir_nightly.py compare results.json [--baseline base.json]
                               [--threshold 25] [--summary FILE]

`parse` reads the "Results" table `benchmarks/vs-elixir/run.sh bench` prints
and keeps, per row, Beamtalk ns/op, reductions/op and words/op (net of the
loop baseline) plus the Elixir ns/op measured in the same node as a control.

`compare` prints a before/after table (to stdout and, with --summary, appended
to the GitHub job summary). Exit status 1 when any row's Beamtalk ns/op is more
than THRESHOLD percent above the baseline; reductions and words are shown but
never gate (BT-4842: reductions stayed flat while latency rose 5x). With no
baseline it prints the current figures and exits 0.
"""
import argparse
import json
import re
import sys

# Columns of the Results table, after the row id and the operation label.
COLS = ["bt_ns", "bt_red", "bt_words", "ex_ns", "ex_red", "ex_words"]
CELL_SPLIT = re.compile(r"(?<!\\)\|")


def parse_report(text):
    """Return {row id: {label, bt_ns, ...}} from the `## Results` table only."""
    rows = {}
    in_results = False
    for line in text.splitlines():
        if line.startswith("## "):
            in_results = line.startswith("## Results")
            continue
        if not in_results or not line.startswith("|"):
            continue
        cells = [c.strip() for c in CELL_SPLIT.split(line)[1:-1]]
        if len(cells) < 2 + len(COLS) or cells[0] in ("Row", "") or cells[0].startswith("-"):
            continue
        try:
            vals = [float(c) for c in cells[2 : 2 + len(COLS)]]
        except ValueError:
            continue
        rows[cells[0]] = {"label": cells[1].replace("\\|", "|"), **dict(zip(COLS, vals))}
    return rows


def pct(cur, base):
    if base is None or base <= 0:
        return None
    return (cur - base) / base * 100.0


def compare(current, baseline, threshold):
    """Return (table lines, regressed row ids)."""
    cur_rows = current["rows"]
    base_rows = (baseline or {}).get("rows", {})
    lines = []
    regressed = []
    head = ["Row", "Operation", "BT ns before", "BT ns after", "time Δ", "red before", "red after",
            "words before", "words after", "EX ns after", "verdict"]
    lines.append("| " + " | ".join(head) + " |")
    lines.append("|---|---|--:|--:|--:|--:|--:|--:|--:|--:|---|")
    # A baseline row absent tonight (errored, printed "-") must not pass
    # silently and then drop out of the next baseline.
    for rid in sorted(set(base_rows) - set(cur_rows), key=row_key):
        b = base_rows[rid]
        lines.append(f"| {rid} | {b['label'].replace('|', chr(92) + '|')} | {fmt(b['bt_ns'])} | - | - | "
                     f"{fmt(b['bt_red'])} | - | {fmt(b['bt_words'])} | - | - | **MISSING** |")
        regressed.append(rid)
    for rid in sorted(cur_rows, key=row_key):
        c = cur_rows[rid]
        b = base_rows.get(rid)
        if b is None:
            delta, verdict = None, "new" if baseline else "no baseline"
        else:
            delta = pct(c["bt_ns"], b["bt_ns"])
            if delta is not None and delta > threshold:
                verdict = "**REGRESSED**"
                regressed.append(rid)
            else:
                verdict = "ok"
        lines.append(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |".format(
                rid,
                c["label"].replace("|", "\\|"),
                fmt(b and b["bt_ns"]), fmt(c["bt_ns"]),
                "-" if delta is None else f"{delta:+.0f}%",
                fmt(b and b["bt_red"]), fmt(c["bt_red"]),
                fmt(b and b["bt_words"]), fmt(c["bt_words"]),
                fmt(c["ex_ns"]), verdict,
            )
        )
    return lines, regressed


def row_key(rid):
    m = re.match(r"(\d+)(.*)", rid)
    return (int(m.group(1)), m.group(2)) if m else (10**9, rid)


def fmt(x):
    if x is None:
        return "-"
    return f"{x:.0f}" if abs(x) >= 100 else f"{x:.1f}"


def cmd_parse(args):
    with open(args.report, encoding="utf-8") as f:
        rows = parse_report(f.read())
    if not rows:
        print("error: no Results table rows found in " + args.report, file=sys.stderr)
        return 2
    missing = [r for r in args.expect if r not in rows]
    if missing:
        print("error: expected rows missing from the report: " + ", ".join(missing), file=sys.stderr)
        return 2
    meta = dict(kv.split("=", 1) for kv in args.meta)
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump({"meta": meta, "rows": rows}, f, indent=2, sort_keys=True)
    print(f"parsed {len(rows)} rows into {args.out}")
    return 0


def cmd_compare(args):
    with open(args.current, encoding="utf-8") as f:
        current = json.load(f)
    baseline = None
    if args.baseline:
        with open(args.baseline, encoding="utf-8") as f:
            baseline = json.load(f)
    lines, regressed = compare(current, baseline, args.threshold)
    out = ["## vs-elixir nightly subset (BT ns/op, time-gated)", ""]
    if baseline is None:
        out.append("No baseline (first run, or no earlier successful night): "
                   "nothing to compare, the check passes. Tonight's figures become the baseline.")
    else:
        bm, cm = baseline.get("meta", {}), current.get("meta", {})
        out.append(f"Baseline: `{bm.get('sha', '?')}` ({bm.get('date', '?')}); "
                   f"current: `{cm.get('sha', '?')}` ({cm.get('date', '?')}). "
                   f"A row fails when Beamtalk ns/op is more than {args.threshold:g}% above the baseline; "
                   "reductions and words are shown, not gated.")
    out.append("")
    out += lines
    out.append("")
    if regressed:
        out.append(f"**FAIL:** {', '.join(regressed)}: more than {args.threshold:g}% slower than the previous night, "
                   "or missing from tonight's results.")
    else:
        out.append("**PASS**")
    text = "\n".join(out) + "\n"
    print(text)
    if args.summary:
        with open(args.summary, "a", encoding="utf-8") as f:
            f.write(text)
    return 1 if regressed else 0


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    pp = sub.add_parser("parse")
    pp.add_argument("report")
    pp.add_argument("--out", required=True)
    pp.add_argument("--meta", nargs="*", default=[])
    pp.add_argument("--expect", nargs="*", default=[], help="row ids that must be in the report")
    pp.set_defaults(fn=cmd_parse)
    pc = sub.add_parser("compare")
    pc.add_argument("current")
    pc.add_argument("--baseline")
    pc.add_argument("--threshold", type=float, default=25.0)
    pc.add_argument("--summary")
    pc.set_defaults(fn=cmd_compare)
    args = p.parse_args(argv)
    return args.fn(args)


if __name__ == "__main__":
    sys.exit(main())

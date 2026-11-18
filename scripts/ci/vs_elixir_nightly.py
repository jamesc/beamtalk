#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Nightly vs-elixir subset (BT-4844): parse a bench report, compare two nights.

  vs_elixir_nightly.py parse   REPORT.md --out results.json [--meta k=v ...] [--expect ID ...]
  vs_elixir_nightly.py compare results.json [--baseline base.json]
                               [--threshold 45] [--summary FILE]
                               [--defer --over-out FILE]
  vs_elixir_nightly.py confirm second.json --first first.json [--baseline base.json]
                               [--threshold 45] [--summary FILE] [--merged-out FILE]

`parse` reads the "Results" table `benchmarks/vs-elixir/run.sh bench` prints
and keeps, per row, Beamtalk ns/op, reductions/op and words/op (net of the
loop baseline) plus the Elixir ns/op measured in the same node as a control.

`compare` prints a before/after table (to stdout and, with --summary, appended
to the GitHub job summary). Exit status 1 when any row's Beamtalk ns/op is more
than THRESHOLD percent above the baseline; reductions and words are shown but
never gate (BT-4842: reductions stayed flat while latency rose 5x). With no
baseline it prints the current figures and exits 0.

Two measurements (BT-4853): runner speed is the noise source, so one slow
reading is not enough to fail a night. `compare --defer` reports the rows over
the threshold (and writes their ids to --over-out) but exits 0. The workflow
then measures again on a fresh runner and `confirm` fails only the rows that
are over the threshold in both measurements against the same baseline. It also
writes the results to store as tonight's baseline: a row that was not
confirmed keeps its baseline figures, so a slow drift cannot ratchet the
baseline up one unconfirmed night at a time.
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


def rel(r):
    """Beamtalk ns/op relative to the Elixir control timed in the same node and
    rounds. Shared runners vary by tens of percent from night to night
    (identical code measured -30% and +56% on the Elixir side alone), and that
    variation moves both sides; the ratio cancels it."""
    ex = r.get("ex_ns", 0)
    # Under 0.5 ns net the control is noise (the harness prints "-" for it too).
    return r["bt_ns"] / ex if ex >= 0.5 else None


def pct(cur, base):
    if base is None or base <= 0:
        return None
    return (cur - base) / base * 100.0


THRESHOLD_DEFAULT = 45.0


def compare(current, baseline, threshold):
    """Return (table lines, regressed row ids)."""
    cur_rows = current["rows"]
    base_rows = (baseline or {}).get("rows", {})
    lines = []
    regressed = []
    head = ["Row", "Operation", "BT ns before", "BT ns after", "EX ns before", "EX ns after",
            "BT/EX before", "BT/EX after", "gated Δ", "red before", "red after",
            "words before", "words after", "verdict"]
    lines.append("| " + " | ".join(head) + " |")
    lines.append("|---|---|" + "--:|" * 11 + "---|")
    # A baseline row absent tonight (errored, printed "-") must not pass
    # silently and then drop out of the next baseline.
    for rid in sorted(set(base_rows) - set(cur_rows), key=row_key):
        b = base_rows[rid]
        lines.append(f"| {rid} | {b['label'].replace('|', chr(92) + '|')} | {fmt(b['bt_ns'])} | - | "
                     f"{fmt(b['ex_ns'])} | - | {fmt(rel(b))} | - | - | {fmt(b['bt_red'])} | - | "
                     f"{fmt(b['bt_words'])} | - | **MISSING** |")
        regressed.append(rid)
    for rid in sorted(cur_rows, key=row_key):
        c = cur_rows[rid]
        b = base_rows.get(rid)
        if b is None:
            delta, verdict = None, "new" if baseline else "no baseline"
        else:
            rc, rb = rel(c), rel(b)
            delta = pct(rc, rb) if rc is not None and rb is not None else None
            if delta is None:
                verdict = "no control"
            elif delta > threshold:
                verdict = "**REGRESSED**"
                regressed.append(rid)
            else:
                verdict = "ok"
        lines.append(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |".format(
                rid,
                c["label"].replace("|", "\\|"),
                fmt(b and b["bt_ns"]), fmt(c["bt_ns"]),
                fmt(b and b["ex_ns"]), fmt(c["ex_ns"]),
                fmt(b and rel(b)), fmt(rel(c)),
                "-" if delta is None else f"{delta:+.0f}%",
                fmt(b and b["bt_red"]), fmt(c["bt_red"]),
                fmt(b and b["bt_words"]), fmt(c["bt_words"]),
                verdict,
            )
        )
    return lines, regressed


def confirm_rows(first, second, baseline, threshold):
    """Split the rows over the threshold in the first measurement.

    Returns (confirmed, unconfirmed, merged): a row is confirmed when the second
    measurement, against the same baseline, is over the threshold too (or the
    row is missing from both). `merged` is what to store as the next baseline:
    the first measurement, with every unconfirmed row put back to its baseline
    figures."""
    _, over_first = compare(first, baseline, threshold)
    _, over_second = compare(second, baseline, threshold)
    confirmed = [r for r in over_first if r in over_second]
    unconfirmed = [r for r in over_first if r not in over_second]
    rows = dict(first["rows"])
    base_rows = (baseline or {}).get("rows", {})
    for rid in unconfirmed:
        if rid in base_rows:
            rows[rid] = base_rows[rid]
    meta = dict(first.get("meta", {}))
    if unconfirmed:
        meta["held_at_baseline"] = ",".join(unconfirmed)
    return confirmed, unconfirmed, {"meta": meta, "rows": rows}


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
    current = load(args.current)
    baseline = load(args.baseline) if args.baseline else None
    lines, regressed = compare(current, baseline, args.threshold)
    out = ["## vs-elixir nightly subset (time-gated: BT ns/op relative to the Elixir control)", ""]
    if baseline is None:
        out.append("No baseline (first run, or no earlier successful night): "
                   "nothing to compare, the check passes. Tonight's figures become the baseline.")
    else:
        bm, cm = baseline.get("meta", {}), current.get("meta", {})
        out.append(f"Baseline: `{bm.get('sha', '?')}` ({bm.get('date', '?')}); "
                   f"current: `{cm.get('sha', '?')}` ({cm.get('date', '?')}). "
                   f"A row fails when Beamtalk ns/op relative to the Elixir control (BT/EX) is more than "
                   f"{args.threshold:g}% above the baseline; "
                   "reductions and words are shown, not gated.")
    out.append("")
    out += lines
    out.append("")
    if regressed and args.defer:
        out.append(f"**OVER THE THRESHOLD in the first measurement:** {', '.join(regressed)} "
                   f"(more than {args.threshold:g}% above the baseline, or missing). "
                   "Not failing yet: a second job measures again on a fresh runner.")
    elif regressed:
        out.append(f"**FAIL:** {', '.join(regressed)}: more than {args.threshold:g}% slower than the previous night, "
                   "or missing from tonight's results.")
    else:
        out.append("**PASS**")
    emit(out, args.summary)
    if args.over_out:
        with open(args.over_out, "w", encoding="utf-8") as f:
            f.write("".join(r + "\n" for r in regressed))
    return 1 if regressed and not args.defer else 0


def emit(out, summary):
    text = "\n".join(out) + "\n"
    print(text)
    if summary:
        with open(summary, "a", encoding="utf-8") as f:
            f.write(text)


def load(path):
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def cmd_confirm(args):
    first, second = load(args.first), load(args.current)
    baseline = load(args.baseline) if args.baseline else None
    confirmed, unconfirmed, merged = confirm_rows(first, second, baseline, args.threshold)
    lines, _ = compare(second, baseline, args.threshold)
    fm, sm = first.get("meta", {}), second.get("meta", {})
    out = ["## vs-elixir nightly subset: second measurement on a fresh runner", "",
           f"The first measurement (`{fm.get('sha', '?')}`) had rows over the {args.threshold:g}% threshold. "
           f"This is the second one (`{sm.get('sha', '?')}`), compared with the same baseline. "
           "A row fails only when it is over the threshold in both.", ""]
    out += lines
    out.append("")
    if unconfirmed:
        out.append(f"**Not confirmed:** {', '.join(unconfirmed)}: over the threshold in the first measurement only, "
                   "so not a failure. These rows keep their baseline figures for the next night.")
    if confirmed:
        out.append(f"**FAIL (confirmed in both measurements):** {', '.join(confirmed)}.")
    else:
        out.append("**PASS**")
    emit(out, args.summary)
    if args.merged_out:
        with open(args.merged_out, "w", encoding="utf-8") as f:
            json.dump(merged, f, indent=2, sort_keys=True)
    return 1 if confirmed else 0


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
    pc.add_argument("--threshold", type=float, default=THRESHOLD_DEFAULT)
    pc.add_argument("--summary")
    pc.add_argument("--defer", action="store_true",
                    help="report rows over the threshold but exit 0 (a second measurement decides)")
    pc.add_argument("--over-out", help="write the ids of the rows over the threshold, one per line")
    pc.set_defaults(fn=cmd_compare)
    pf = sub.add_parser("confirm")
    pf.add_argument("current", help="the second measurement")
    pf.add_argument("--first", required=True, help="results.json of the first measurement")
    pf.add_argument("--baseline")
    pf.add_argument("--threshold", type=float, default=THRESHOLD_DEFAULT)
    pf.add_argument("--summary")
    pf.add_argument("--merged-out", help="write the results to store as the next baseline")
    pf.set_defaults(fn=cmd_confirm)
    args = p.parse_args(argv)
    return args.fn(args)


if __name__ == "__main__":
    sys.exit(main())

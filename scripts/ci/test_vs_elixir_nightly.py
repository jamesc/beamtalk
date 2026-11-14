#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Tests for vs_elixir_nightly.py (BT-4844)."""
import json
import os
import tempfile
import unittest

import vs_elixir_nightly as v

REPORT = """# vs-elixir bench
## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec |
| 7a | actor sync read: c count / GenServer.call | 3315 | 211 | 312 | 945 | 37.2 | 56.0 | 3.5 | 5.7 | 5.6 | 174 | 256 | send |
| 16a | async \\| start | 120.5 | 10.0 | 20.0 | 50.0 | 5.00 | 6.00 | 2.4 | 2.0 | 3.3 | 5 | 14 | x |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 7a | `foo` | 1 | 2 | 3 |
"""


def results(**ns):
    """Rows with a constant Elixir control of 100 ns, so BT/EX moves with bt_ns."""
    return {"meta": {}, "rows": {k: {"label": k, "bt_ns": x, "bt_red": 1.0, "bt_words": 1.0,
                                      "ex_ns": 100.0, "ex_red": 1.0, "ex_words": 1.0}
                                  for k, x in ns.items()}}


class Parse(unittest.TestCase):
    def test_parses_results_table_only(self):
        rows = v.parse_report(REPORT)
        self.assertEqual(sorted(rows), ["0", "16a", "7a"])
        self.assertEqual(rows["7a"]["bt_ns"], 3315.0)
        self.assertEqual(rows["7a"]["bt_words"], 312.0)
        self.assertEqual(rows["7a"]["ex_ns"], 945.0)
        self.assertEqual(rows["16a"]["label"], "async | start")

    def test_expected_row_missing_is_an_error(self):
        with tempfile.TemporaryDirectory() as d:
            r, o = os.path.join(d, "r.md"), os.path.join(d, "o.json")
            with open(r, "w") as f:
                f.write(REPORT)
            self.assertEqual(v.main(["parse", r, "--out", o, "--expect", "7a", "16a"]), 0)
            self.assertEqual(v.main(["parse", r, "--out", o, "--expect", "7a", "7b"]), 2)

    def test_empty_report_is_an_error(self):
        with tempfile.TemporaryDirectory() as d:
            r, o = os.path.join(d, "r.md"), os.path.join(d, "o.json")
            open(r, "w").write("nothing\n")
            self.assertEqual(v.main(["parse", r, "--out", o]), 2)


class Compare(unittest.TestCase):
    def run_cmp(self, cur, base, threshold=25.0):
        with tempfile.TemporaryDirectory() as d:
            c = os.path.join(d, "c.json")
            json.dump(cur, open(c, "w"))
            argv = ["compare", c, "--threshold", str(threshold), "--summary", os.path.join(d, "s.md")]
            if base is not None:
                b = os.path.join(d, "b.json")
                json.dump(base, open(b, "w"))
                argv += ["--baseline", b]
            code = v.main(argv)
            return code, open(os.path.join(d, "s.md")).read()

    def test_first_run_passes(self):
        code, s = self.run_cmp(results(**{"7a": 100.0}), None)
        self.assertEqual(code, 0)
        self.assertIn("No baseline", s)

    def test_within_threshold_passes(self):
        code, s = self.run_cmp(results(**{"7a": 124.0}), results(**{"7a": 100.0}))
        self.assertEqual(code, 0)
        self.assertIn("PASS", s)

    def test_over_threshold_fails(self):
        code, s = self.run_cmp(results(**{"7a": 126.0, "7b": 100.0}),
                               results(**{"7a": 100.0, "7b": 100.0}))
        self.assertEqual(code, 1)
        self.assertIn("REGRESSED", s)
        self.assertIn("+26%", s)

    def test_faster_and_new_rows_pass(self):
        code, _ = self.run_cmp(results(**{"7a": 10.0, "16a": 500.0}), results(**{"7a": 100.0}))
        self.assertEqual(code, 0)

    def test_slow_runner_night_does_not_trip(self):
        # BT and the Elixir control both 50% slower: the ratio is unchanged.
        cur = results(**{"7a": 150.0})
        cur["rows"]["7a"]["ex_ns"] = 150.0
        base = results(**{"7a": 100.0})
        base["rows"]["7a"]["ex_ns"] = 100.0
        code, _ = self.run_cmp(cur, base)
        self.assertEqual(code, 0)

    def test_bt_only_slowdown_trips_even_on_a_fast_runner(self):
        cur = results(**{"7a": 90.0})      # BT 10% faster in absolute terms...
        cur["rows"]["7a"]["ex_ns"] = 60.0  # ...but the control is 40% faster
        code, s = self.run_cmp(cur, results(**{"7a": 100.0}))
        self.assertEqual(code, 1)
        self.assertIn("REGRESSED", s)

    def test_negligible_control_is_not_gated(self):
        cur = results(**{"7a": 1000.0})
        cur["rows"]["7a"]["ex_ns"] = 0.1
        code, s = self.run_cmp(cur, results(**{"7a": 100.0}))
        self.assertEqual(code, 0)
        self.assertIn("no control", s)

    def test_baseline_row_missing_tonight_fails(self):
        code, s = self.run_cmp(results(**{"7a": 100.0}), results(**{"7a": 100.0, "7b": 100.0}))
        self.assertEqual(code, 1)
        self.assertIn("MISSING", s)

    def test_reductions_do_not_gate(self):
        cur = results(**{"7a": 100.0})
        cur["rows"]["7a"]["bt_red"] = 1000.0
        code, _ = self.run_cmp(cur, results(**{"7a": 100.0}))
        self.assertEqual(code, 0)


if __name__ == "__main__":
    unittest.main()

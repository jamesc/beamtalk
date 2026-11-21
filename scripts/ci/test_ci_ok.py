# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Tests for ci_ok.py (BT-5086)."""

import unittest

import ci_ok as c

JOBS = {
    "changes": {},
    "lint": {},
    "test-rust": {"needs": "changes", "if": "${{ !cancelled() }}"},
    "test-vscode": {"needs": "changes", "if": "needs.changes.outputs.vscode == 'true'"},
}


def needs(**results):
    return {k.replace("_", "-"): {"result": v} for k, v in results.items()}


class Verdict(unittest.TestCase):
    def test_all_success(self):
        n = needs(changes="success", lint="success", test_rust="success", test_vscode="success")
        self.assertEqual(c.verdict(n, JOBS), [])

    def test_path_gated_skip_is_fine(self):
        n = needs(changes="success", lint="success", test_rust="success", test_vscode="skipped")
        self.assertEqual(c.verdict(n, JOBS), [])

    def test_failure_and_cancelled_fail(self):
        for bad in ("failure", "cancelled"):
            n = needs(changes="success", lint=bad, test_rust="success", test_vscode="skipped")
            self.assertEqual(c.verdict(n, JOBS), [f"lint: {bad}"])

    def test_skipped_always_run_job_fails(self):
        n = needs(changes="success", lint="skipped", test_rust="skipped", test_vscode="skipped")
        self.assertEqual(c.verdict(n, JOBS), ["lint: skipped", "test-rust: skipped"])

    def test_skips_after_failed_changes_fail(self):
        n = needs(changes="failure", lint="success", test_rust="success", test_vscode="skipped")
        self.assertEqual(c.verdict(n, JOBS), ["changes: failure", "test-vscode: skipped"])


class Structure(unittest.TestCase):
    def test_missing_job(self):
        self.assertEqual(c.structure(JOBS), ["no `ci-ok` job"])

    def test_needs_every_job_and_always(self):
        jobs = dict(JOBS, **{"ci-ok": {"if": "${{ always() }}", "needs": ["changes", "lint", "test-rust"]}})
        self.assertEqual(c.structure(jobs), ["ci-ok does not need: test-vscode"])
        jobs["ci-ok"]["needs"].append("test-vscode")
        self.assertEqual(c.structure(jobs), [])
        jobs["ci-ok"]["if"] = None
        self.assertEqual(len(c.structure(jobs)), 1)

    def test_real_workflow(self):
        self.assertEqual(c.structure(c.load_jobs(c.Path(c.__file__).resolve().parents[2] / ".github/workflows/ci.yml")), [])


class Partition(unittest.TestCase):
    CI = ["**", "!*.md", "!docs/**", "docs/learning/**"]
    DOCS = ["*.md", "docs/**", "!docs/learning/**"]

    def test_partition(self):
        files = ["a.rs", "README.md", "docs/x.md", "docs/learning/y.md", "crates/z/README.md"]
        self.assertEqual(c.partition(self.CI, self.DOCS, files), [])

    def test_gap_and_overlap_are_reported(self):
        self.assertEqual(len(c.partition(self.CI, ["*.md"], ["docs/x.md"])), 1)  # neither
        self.assertEqual(len(c.partition(self.CI, ["**"], ["a.rs"])), 1)  # both

    def test_glob_semantics(self):
        self.assertTrue(c.included(["*.md"], "README.md"))
        self.assertFalse(c.included(["*.md"], "a/README.md"))
        self.assertTrue(c.included(["docs/**"], "docs/a/b.md"))


if __name__ == "__main__":
    unittest.main()

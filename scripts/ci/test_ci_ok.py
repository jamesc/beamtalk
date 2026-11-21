# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Tests for ci_ok.py (BT-5086)."""

import unittest
from pathlib import Path

import ci_ok as c

JOBS = {
    "changes": {"outputs": {"code": "x"}},
    "lint": {"needs": "changes", "if": "needs.changes.outputs.code == 'true'"},
    "test-rust": {"needs": "changes", "if": "${{ !cancelled() && needs.changes.outputs.code != 'false' }}"},
    "otp-matrix": {"needs": "changes", "if": "needs.changes.outputs.code == 'true'"},
    "test-beam": {"needs": "otp-matrix"},
    "test-vscode": {"needs": "changes", "if": "needs.changes.outputs.vscode == 'true'"},
    "test-dialyzer": {"needs": ["changes", "otp-matrix"], "if": "needs.changes.outputs.runtime == 'true'"},
}
CODE_JOBS = ["lint", "test-rust", "otp-matrix", "test-beam", "test-vscode", "test-dialyzer"]


def needs(code="true", **results):
    n = {k.replace("_", "-"): {"result": v} for k, v in results.items()}
    n["changes"] = {"result": results.get("changes", "success"), "outputs": {"code": code}}
    return n


def all_ok(code="true", **override):
    base = {j.replace("-", "_"): "success" for j in CODE_JOBS}
    base.update(override)
    return needs(code, **base)


class Verdict(unittest.TestCase):
    def test_all_success(self):
        self.assertEqual(c.verdict(all_ok(), JOBS), [])

    def test_path_gated_skip_is_fine_on_a_code_pr(self):
        self.assertEqual(c.verdict(all_ok(test_vscode="skipped", test_dialyzer="skipped"), JOBS), [])

    def test_failure_and_cancelled_fail(self):
        for bad in ("failure", "cancelled"):
            self.assertEqual(c.verdict(all_ok(lint=bad), JOBS), [f"lint: {bad}"])

    def test_code_job_skipped_while_code_is_true_fails(self):
        n = all_ok(lint="skipped", test_rust="skipped", otp_matrix="skipped", test_beam="skipped")
        self.assertEqual(
            c.verdict(n, JOBS),
            ["lint: skipped", "otp-matrix: skipped", "test-beam: skipped", "test-rust: skipped"],
        )

    def test_docs_only_pr_skips_every_code_job(self):
        n = needs("false", **{j.replace("-", "_"): "skipped" for j in CODE_JOBS})
        self.assertEqual(c.verdict(n, JOBS), [])

    def test_docs_only_pr_still_fails_on_a_failed_job(self):
        n = needs("false", **{j.replace("-", "_"): "skipped" for j in CODE_JOBS})
        n["lint"]["result"] = "failure"
        self.assertEqual(c.verdict(n, JOBS), ["lint: failure"])

    def test_skips_after_failed_changes_fail(self):
        n = needs("false", changes="failure", **{j.replace("-", "_"): "skipped" for j in CODE_JOBS})
        self.assertEqual(c.verdict(n, JOBS)[0], "changes: failure")
        self.assertEqual(len(c.verdict(n, JOBS)), 1 + len(CODE_JOBS))

    def test_missing_code_output_is_treated_as_code(self):
        n = all_ok(lint="skipped")
        del n["changes"]["outputs"]
        self.assertEqual(c.verdict(n, JOBS), ["lint: skipped"])


class Structure(unittest.TestCase):
    def ok_job(self, extra=()):
        return {"if": "${{ always() }}", "needs": list(JOBS) + list(extra)}

    def test_missing_job(self):
        self.assertEqual(c.structure(JOBS), ["no `ci-ok` job"])

    def test_needs_every_job_and_always(self):
        jobs = dict(JOBS, **{"ci-ok": self.ok_job()})
        self.assertEqual(c.structure(jobs), [])
        jobs["ci-ok"]["needs"].remove("test-beam")
        self.assertEqual(c.structure(jobs), ["ci-ok does not need: test-beam"])
        jobs["ci-ok"]["needs"].append("test-beam")
        jobs["ci-ok"]["if"] = None
        self.assertEqual(len(c.structure(jobs)), 1)

    def test_ungated_job_is_reported(self):
        jobs = dict(JOBS, extra={"runs-on": "x"}, **{"ci-ok": self.ok_job(["extra"])})
        self.assertEqual(c.structure(jobs), ["extra is not gated on a `changes` output (it would run on docs-only PRs)"])

    def test_changes_must_emit_code(self):
        jobs = dict(JOBS, **{"ci-ok": self.ok_job()})
        jobs["changes"] = {}
        self.assertEqual(c.structure(jobs), ["`changes` must emit a `code` output"])

    def test_real_workflow(self):
        jobs = c.load_jobs(Path(c.__file__).resolve().parents[2] / ".github/workflows/ci.yml")
        self.assertEqual(c.structure(jobs), [])

    def test_real_workflow_docs_only_is_green_and_code_job_skip_is_red(self):
        jobs = c.load_jobs(Path(c.__file__).resolve().parents[2] / ".github/workflows/ci.yml")
        names = [j for j in jobs if j not in ("changes", "ci-ok")]
        docs_only = {j: {"result": "skipped"} for j in names}
        docs_only["changes"] = {"result": "success", "outputs": {"code": "false"}}
        self.assertEqual(c.verdict(docs_only, jobs), [])
        code_pr = {j: {"result": "success"} for j in names}
        code_pr["changes"] = {"result": "success", "outputs": {"code": "true"}}
        self.assertEqual(c.verdict(code_pr, jobs), [])
        code_pr["lint"]["result"] = "skipped"
        self.assertEqual(c.verdict(code_pr, jobs), ["lint: skipped"])


if __name__ == "__main__":
    unittest.main()

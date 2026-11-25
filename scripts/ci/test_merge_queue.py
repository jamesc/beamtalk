# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""The workflows are ready for a GitHub merge queue on main (BT-5247).

In a merge queue every required check must also report on the queue's test
commit, which GitHub builds and announces with a `merge_group` event. These
tests pin that:

* ci.yml runs on `merge_group`, its `changes` job treats it like a push to
  main (the full Linux suite, `code` true), and an event it does not know
  fails instead of silently picking a gate set;
* ci.yml's concurrency group never cancels a merge-group run or shares a
  group with a PR run;
* claude-review-queue.yml, on `merge_group` only, reports "Claude BeamTalk
  Review" without running a review, and claude-review.yml stays
  `pull_request`-only. Two files, not one: claude-code-action refuses to run
  on a PR that edits its own workflow, and a second same-named job in
  claude-review.yml would put a skipped check of that name on every PR commit;
* every other workflow a PR starts is listed in NOT_REQUIRED with the reason
  its check cannot be a required one, so a new PR workflow has to decide.
"""

import json
import os
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github/workflows"
CI_YML = WORKFLOWS / "ci.yml"
REVIEW_YML = WORKFLOWS / "claude-review.yml"
QUEUE_YML = WORKFLOWS / "claude-review-queue.yml"
REVIEW_CHECK = "Claude BeamTalk Review"

# Workflows that run on pull_request but must never be required checks, so they
# need no merge_group trigger. A path- or label-gated workflow does not run on
# every PR, and GitHub shows a required check that never started as pending
# forever, so none of these can be required. claude-review.yml IS required:
# its merge_group counterpart is claude-review-queue.yml (see ReviewWorkflow).
NOT_REQUIRED = {
    "ci-platforms.yml": "label-gated (`ci:all-platforms`) and path-filtered; stays PR-only",
    "core-diff.yml": "label-gated (`ci:core-diff`); compares against the PR base",
    "corpus.yml": "label-gated (`ci:corpus`) on PRs; runs on every push to main",
    "liveview-docker.yml": "path-filtered to the LiveView Dockerfile",
    "liveview-e2e.yml": "path-filtered to editors/liveview and the workspace app",
    "liveview.yml": "path-filtered to editors/liveview",
}

# Every `changes` output a push to main sets to true.
FULL_GATES = ["code", "core_diff_modes", "learn", "runtime", "rust", "exec", "vscode",
              "fixture_readers", "cluster"]


def load(path):
    return yaml.safe_load(path.read_text())


def triggers(doc):
    # PyYAML reads the bare key `on` as the boolean True (YAML 1.1).
    on = doc.get("on", doc.get(True))
    if isinstance(on, str):
        return {on: None}
    if isinstance(on, list):
        return {k: None for k in on}
    return on or {}


class _Ctx(dict):
    """Attribute access for evaluating a `${{ }}` expression. A missing property
    is an empty _Ctx: falsy, renders as '', and its own properties are missing
    too, as GitHub's null does."""

    def __getattr__(self, name):
        v = self.get(name, {})
        return _Ctx(v) if isinstance(v, dict) else v

    def __str__(self):
        return "" if not self else dict.__str__(self)


def evaluate(expr, **contexts):
    """Evaluate a GitHub expression built from ==, !=, &&, ||, !, contexts and
    string literals. Python's and/or return an operand, as GitHub's do."""
    body = expr.strip()
    if body.startswith("${{"):
        body = body[3:-2]
    body = body.replace("\n", " ").replace("&&", " and ").replace("||", " or ")
    body = re.sub(r"!(?!=)", " not ", body)
    env = {k: _Ctx(v) if isinstance(v, dict) else v for k, v in contexts.items()}
    env.update(true=True, false=False, null=None, always=lambda: True, hashFiles=lambda *_: "", fromJSON=json.loads,
               contains=lambda hay, needle: needle in hay)
    return eval(body, {"__builtins__": {}}, env)  # our own workflow text, not input


def render(template, contexts):
    """Substitute every `${{ }}` in a string, as GitHub does for `group:`."""
    return re.sub(r"\$\{\{(.*?)\}\}", lambda m: str(evaluate(m.group(1), **contexts)), template)


def gates_step(doc):
    for step in doc["jobs"]["changes"]["steps"]:
        if step.get("id") == "filter":
            return step["run"]
    raise AssertionError("ci.yml `changes` has no step with id `filter`")


class CiWorkflow(unittest.TestCase):
    def setUp(self):
        self.doc = load(CI_YML)

    def run_gates(self, event):
        """Run the `changes` job's gate script for `event`; (exit code, outputs)."""
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp) / "out"
            out.touch()
            env = {**os.environ, "EVENT_NAME": event, "BASE_SHA": "", "GITHUB_OUTPUT": str(out)}
            # -e as GitHub runs `run:` (bash -e); cwd is a scratch dir, so a
            # PR-path `git fetch` would fail rather than touch this checkout.
            proc = subprocess.run(["bash", "-e", "-c", gates_step(self.doc)], cwd=tmp, env=env,
                                  capture_output=True, text=True, check=False)
            pairs = dict(line.split("=", 1) for line in out.read_text().splitlines() if "=" in line)
        return proc.returncode, pairs

    def test_runs_on_merge_group(self):
        self.assertIn("merge_group", triggers(self.doc))

    def test_merge_group_gets_the_full_suite_like_a_push_to_main(self):
        for event in ("merge_group", "push", "workflow_dispatch"):
            code, out = self.run_gates(event)
            self.assertEqual(code, 0, event)
            for gate in FULL_GATES:
                self.assertEqual(out.get(gate), "true", f"{event}: {gate}")
            self.assertEqual(out.get("desktop"), "false", event)

    def test_an_unknown_event_fails_instead_of_guessing_the_gates(self):
        code, out = self.run_gates("schedule")
        self.assertNotEqual(code, 0)
        self.assertEqual(out, {})

    def test_concurrency_never_cancels_or_merges_a_merge_group_run(self):
        conc = self.doc["concurrency"]

        def ctx(event, ref, sha):
            return {"github": {"event_name": event, "ref": ref, "sha": sha, "workflow": "CI"}}

        q1 = ctx("merge_group", "refs/heads/gh-readonly-queue/main/pr-1-aaa", "sha1")
        q2 = ctx("merge_group", "refs/heads/gh-readonly-queue/main/pr-2-sha1", "sha2")
        pr = ctx("pull_request", "refs/pull/1/merge", "sha1")
        self.assertFalse(evaluate(str(conc["cancel-in-progress"]), **q1))
        self.assertTrue(evaluate(str(conc["cancel-in-progress"]), **pr))
        self.assertNotEqual(render(conc["group"], q1), render(conc["group"], q2))
        self.assertNotEqual(render(conc["group"], q1), render(conc["group"], pr))


def jobs_named(name):
    """(workflow file, job) for every job in .github/workflows called `name`."""
    for path in sorted(WORKFLOWS.glob("*.yml")):
        for job in (load(path).get("jobs") or {}).values():
            if job.get("name") == name:
                yield path.name, job


class ReviewWorkflow(unittest.TestCase):
    def setUp(self):
        self.review = load(REVIEW_YML)
        self.queue = load(QUEUE_YML)

    def test_the_review_runs_on_pull_request_only(self):
        self.assertEqual(set(triggers(self.review)), {"pull_request"})

    def test_the_queue_workflow_runs_on_merge_group_only(self):
        self.assertEqual(set(triggers(self.queue)), {"merge_group"})

    def test_exactly_two_jobs_report_the_check_one_per_event(self):
        named = sorted(f for f, _ in jobs_named(REVIEW_CHECK))
        self.assertEqual(named, ["claude-review-queue.yml", "claude-review.yml"])

    def test_merge_group_reports_a_pass_without_a_review(self):
        (job,) = [j for f, j in jobs_named(REVIEW_CHECK) if f == QUEUE_YML.name]
        self.assertNotIn("if", job, "a skipped job may not count as the required check")
        self.assertEqual(job.get("permissions"), {})
        self.assertEqual(self.queue.get("permissions"), {})
        self.assertTrue(job["steps"])
        for step in job["steps"]:
            self.assertIn("run", step, step)
            self.assertNotIn("uses", step, step)
            self.assertNotIn("if", step, step)
            text = yaml.safe_dump(step)
            self.assertNotIn("secrets.", text)
            self.assertNotIn("exit", text)

    def test_queue_concurrency_never_cancels_or_merges_entries(self):
        conc = self.queue["concurrency"]
        q1 = {"github": {"event_name": "merge_group", "sha": "a"}}
        q2 = {"github": {"event_name": "merge_group", "sha": "b"}}
        self.assertNotEqual(render(conc["group"], q1), render(conc["group"], q2))
        self.assertFalse(evaluate(str(conc["cancel-in-progress"]), **q1))


class Inventory(unittest.TestCase):
    def test_every_pull_request_workflow_reports_on_merge_group_or_is_not_required(self):
        for path in sorted(WORKFLOWS.glob("*.yml")):
            on = triggers(load(path))
            if "pull_request" not in on:
                continue
            if path.name in NOT_REQUIRED:
                self.assertNotIn("merge_group", on, f"{path.name} is listed as not required")
            elif path == REVIEW_YML:
                continue  # claude-review-queue.yml reports its check (ReviewWorkflow)
            else:
                self.assertIn("merge_group", on, f"{path.name}: add merge_group or list it in NOT_REQUIRED")

    def test_not_required_lists_only_existing_pull_request_workflows(self):
        for name in NOT_REQUIRED:
            path = WORKFLOWS / name
            self.assertTrue(path.exists(), name)
            self.assertIn("pull_request", triggers(load(path)), name)


if __name__ == "__main__":
    unittest.main()

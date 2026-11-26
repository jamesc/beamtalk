# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""dependabot-auto.yml is safe and only handles patch/minor dependabot PRs (BT-5254).

It runs on `pull_request_target` (a write token), so these tests pin that it
never checks out or runs PR code, that every job is keyed on the dependabot
author and actor, that untrusted values stay out of `run:` text, and that the
patch/minor filter really rejects majors. They also pin the other half of the
design: claude-review.yml skips dependabot (a skipped job passes the required
"Claude BeamTalk Review" check), so no stand-in review job is needed.
"""

import json
import re
import shutil
import subprocess
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
AUTO = ROOT / ".github/workflows/dependabot-auto.yml"
REVIEW = ROOT / ".github/workflows/claude-review.yml"


def load(path):
    return yaml.safe_load(path.read_text())


def triggers(doc):
    on = doc.get("on", doc.get(True))
    return set(on if isinstance(on, (list, dict)) else [on])


class DependabotAuto(unittest.TestCase):
    def setUp(self):
        self.doc = load(AUTO)
        self.job = self.doc["jobs"]["auto-merge"]

    def test_trigger_is_pull_request_target_only(self):
        self.assertEqual(triggers(self.doc), {"pull_request_target"})

    def test_no_checkout_or_pr_code(self):
        for step in self.job["steps"]:
            uses = step.get("uses", "")
            self.assertNotIn("checkout", uses, step)
            self.assertNotIn("setup-", uses, step)
        text = AUTO.read_text()
        self.assertNotIn("head.sha", text)
        self.assertNotIn("head.ref", text)

    def test_job_is_keyed_on_dependabot_author_actor_and_same_repo(self):
        cond = self.job["if"]
        for needle in ("pull_request.user.login == 'dependabot[bot]'",
                       "github.actor == 'dependabot[bot]'",
                       "head.repo.full_name == github.repository",
                       "draft == false"):
            self.assertIn(needle, cond)

    def test_least_privilege_permissions(self):
        self.assertEqual(self.doc["permissions"], {})
        self.assertEqual(self.job["permissions"], {"contents": "write", "pull-requests": "write"})

    def test_actions_are_sha_pinned(self):
        for step in self.job["steps"]:
            if "uses" in step:
                self.assertRegex(step["uses"], r"@[0-9a-f]{40}$", step)
        fm = [s for s in self.job["steps"] if s.get("uses", "").startswith("dependabot/fetch-metadata@")]
        self.assertEqual(len(fm), 1)

    def test_no_expression_inside_run_scripts(self):
        for step in self.job["steps"]:
            self.assertNotIn("${{", step.get("run", ""), step)

    def test_approve_and_merge_only_when_eligible_and_never_admin(self):
        gated = [s for s in self.job["steps"] if s.get("name") in ("Approve", "Enable squash auto-merge")]
        self.assertEqual(len(gated), 2)
        for s in gated:
            self.assertEqual(s["if"], "steps.classify.outputs.eligible == 'true'")
        text = AUTO.read_text()
        self.assertIn("gh pr merge --auto --squash", text)
        self.assertNotIn("--admin", text)

    @unittest.skipUnless(shutil.which("jq"), "jq not installed")
    def test_classifier_accepts_patch_minor_and_rejects_the_rest(self):
        (step,) = [s for s in self.job["steps"] if s.get("id") == "classify"]
        script = step["run"]
        cases = {
            "patch": ([{"updateType": "version-update:semver-patch"}], "true"),
            "minor+patch group": ([{"updateType": "version-update:semver-minor"},
                                   {"updateType": "version-update:semver-patch"}], "true"),
            "major": ([{"updateType": "version-update:semver-major"}], "false"),
            "group with one major": ([{"updateType": "version-update:semver-minor"},
                                      {"updateType": "version-update:semver-major"}], "false"),
            "no update type": ([{"updateType": None}], "false"),
            "empty": ([], "false"),
        }
        for name, (deps, want) in cases.items():
            out = Path(self.id() + ".out")
            out.write_text("")
            try:
                subprocess.run(["bash", "-e", "-c", script], check=True, capture_output=True,
                               env={"PATH": __import__("os").environ["PATH"], "UPDATED": json.dumps(deps),
                                    "GITHUB_OUTPUT": str(out.resolve())})
                self.assertEqual(out.read_text().strip(), f"eligible={want}", name)
            finally:
                out.unlink(missing_ok=True)


class ReviewCheckForDependabot(unittest.TestCase):
    def test_review_skips_dependabot_so_the_required_check_is_a_skip_not_a_run(self):
        job = load(REVIEW)["jobs"]["review"]
        self.assertEqual(job["name"], "Claude BeamTalk Review")
        trusted = re.search(r"fromJSON\('(\[.*?\])'\)", job["if"]).group(1)
        self.assertNotIn("dependabot", json.loads(trusted))

    def test_auto_workflow_has_no_job_named_like_the_required_review(self):
        for job in load(AUTO)["jobs"].values():
            self.assertNotEqual(job.get("name"), "Claude BeamTalk Review")


if __name__ == "__main__":
    unittest.main()

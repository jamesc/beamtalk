# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Tests for ci_code_filter.py and path_filter.py (BT-5086)."""

import subprocess
import tempfile
import unittest
from pathlib import Path

import ci_code_filter as f
from path_filter import included

ROOT = Path(__file__).resolve().parents[2]
CI_YML = ROOT / ".github/workflows/ci.yml"

# The workflow-level `pull_request: paths:` list ci.yml had before it ran on every
# PR (BT-5086), frozen here. A file that list started CI for must still count as
# `code`, and a file it skipped must still be docs-only.
FORMER_PULL_REQUEST_PATHS = [
    "**",
    "!*.md",
    "!docs/**",
    "docs/learning/**",
    "docs/beamtalk-language-features.md",
    "docs/repl-protocol.md",
    "docs/development/surface-parity.md",
    "!LICENSE",
    "!SECURITY.md",
    "!.devcontainer/**",
]

PROBES = [
    "README.md", "CHANGELOG.md", "crates/x/README.md", "docs/a.md", "docs/deep/dir/x.md",
    "docs/learning/ch1.md", "docs/learning/sub/dir/ch2.md", "docs/beamtalk-language-features.md",
    "docs/repl-protocol.md", "docs/development/surface-parity.md", "docs/development/other.md",
    "docs/ADR/0001.md", ".devcontainer/Dockerfile", ".devcontainer/a/b.json", "LICENSE",
    "SECURITY.md", "src/new.rs", ".github/workflows/ci.yml", ".github/workflows/ci-platforms.yml",
    "Justfile", "runtime/rebar.lock", "stdlib/src/A.bt", "editors/vscode/README.md",
]


def tracked_files():
    out = subprocess.run(["git", "-C", str(ROOT), "ls-files"], capture_output=True, text=True, check=True)
    return out.stdout.splitlines()


class CodeFilter(unittest.TestCase):
    def test_equals_former_pull_request_filter_on_every_tracked_file(self):
        patterns = f.code_patterns(CI_YML)
        for path in PROBES + tracked_files():
            self.assertEqual(
                included(patterns, path),
                included(FORMER_PULL_REQUEST_PATHS, path),
                f"{path}: code filter disagrees with the former pull_request paths list",
            )

    def test_push_and_former_pull_request_lists_are_identical(self):
        self.assertEqual(f.code_patterns(CI_YML), FORMER_PULL_REQUEST_PATHS)

    def test_docs_only_change_is_not_code(self):
        p = f.code_patterns(CI_YML)
        self.assertFalse(f.has_code(p, ["docs/a.md", "README.md", "LICENSE", ".devcontainer/x"]))
        self.assertFalse(f.has_code(p, []))
        self.assertFalse(f.has_code(p, [""]))

    def test_one_code_file_makes_the_pr_code(self):
        p = f.code_patterns(CI_YML)
        self.assertTrue(f.has_code(p, ["docs/a.md", "crates/x/src/lib.rs"]))
        self.assertTrue(f.has_code(p, ["docs/learning/ch1.md"]))
        self.assertTrue(f.has_code(p, ["crates/x/README.md"]))  # `*.md` is root-only
        self.assertTrue(f.has_code(p, [".github/workflows/ci.yml"]))

    def test_glob_semantics(self):
        self.assertTrue(included(["*.md"], "README.md"))
        self.assertFalse(included(["*.md"], "a/README.md"))
        self.assertTrue(included(["docs/**"], "docs/a/b.md"))
        self.assertFalse(included(["**", "!docs/**"], "docs/a/b.md"))
        self.assertTrue(included(["**", "!docs/**", "docs/learning/**"], "docs/learning/a.md"))


class RenamedFiles(unittest.TestCase):
    """A code file moved into an excluded path must still count as code (BT-5086)."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.repo = Path(self.tmp.name)
        self.git("init", "-q", ".")
        self.git("config", "user.email", "t@example.invalid")
        self.git("config", "user.name", "t")
        (self.repo / "crates/x").mkdir(parents=True)
        (self.repo / "crates/x/a.rs").write_text("fn main() {}\n" * 20)
        self.git("add", "-A")
        self.git("commit", "-q", "-m", "base")
        (self.repo / "docs").mkdir()
        self.git("mv", "crates/x/a.rs", "docs/a.rs")
        self.git("commit", "-q", "-m", "move")

    def git(self, *args):
        return subprocess.run(["git", "-C", str(self.repo), *args], capture_output=True, text=True, check=True).stdout

    def changed(self, *flags):
        return self.git("diff", "--name-only", *flags, "HEAD~1", "HEAD").split()

    def test_rename_detection_hides_the_old_path(self):
        patterns = f.code_patterns(CI_YML)
        with_renames = self.changed("-M")
        self.assertEqual(with_renames, ["docs/a.rs"])
        self.assertFalse(f.has_code(patterns, with_renames))  # the hole

    def test_no_renames_lists_both_paths_and_counts_as_code(self):
        patterns = f.code_patterns(CI_YML)
        both = self.changed("--no-renames")
        self.assertEqual(sorted(both), ["crates/x/a.rs", "docs/a.rs"])
        self.assertTrue(f.has_code(patterns, both))

    def test_ci_yml_changes_job_uses_no_renames(self):
        text = CI_YML.read_text()
        self.assertIn('git diff --name-only --no-renames "$BASE_SHA" HEAD', text)


if __name__ == "__main__":
    unittest.main()

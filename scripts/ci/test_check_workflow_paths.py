# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Tests for check_workflow_paths.py (BT-5032)."""

import unittest

import check_workflow_paths as c

FILES = {
    "crates/x/src/build_stdlib/mod.rs",
    "docs/a.md",
    "Justfile",
    ".github/workflows/ci.yml",
}

WORKFLOW = """\
on:
  push:
    paths:
      - '**'
      - '!docs/**'
      - 'crates/x/src/build_stdlib.rs'
      - docs/a.md   # trailing comment
    branches: [main]
jobs:
  changes:
    steps:
      - run: |
          core=$(flag matches \\
            crates/x/src/build_stdlib.rs \\
            'crates/x/src/build_stdlib/*' \\
            Justfile)
          matches() {
            local path
          }
"""


class CheckWorkflowPaths(unittest.TestCase):
    def test_filter_patterns_read_the_paths_list_only(self):
        pats = [p for _, p in c.filter_patterns(WORKFLOW)]
        self.assertEqual(pats, ["**", "!docs/**", "crates/x/src/build_stdlib.rs", "docs/a.md"])

    def test_matches_patterns_read_continued_arguments(self):
        pats = [p for _, p in c.matches_patterns(WORKFLOW)]
        self.assertEqual(
            pats, ["crates/x/src/build_stdlib.rs", "crates/x/src/build_stdlib/*", "Justfile"]
        )

    def test_a_file_that_became_a_directory_is_stale(self):
        self.assertFalse(c.has_match("crates/x/src/build_stdlib.rs", FILES))
        self.assertTrue(c.has_match("crates/x/src/build_stdlib/*", FILES))

    def test_globs_and_negations(self):
        self.assertTrue(c.has_match("**", FILES))
        self.assertTrue(c.has_match("!docs/**", FILES))
        self.assertTrue(c.has_match("*.md", FILES))
        self.assertFalse(c.has_match("gone/**", FILES))


if __name__ == "__main__":
    unittest.main()

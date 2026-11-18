#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Fail when a path literal in a workflow gate matches nothing in the repo (BT-5032).

Two kinds of gate are read:

* every positive `paths:` list item in `.github/workflows/*.yml`;
* every argument of a `matches` / `flag matches` call in a workflow's shell
  (ci.yml's `changes` job decides which jobs run from those).

A gate whose literal no longer names anything (a file became a directory, a
module moved) silently stops enabling its job. Patterns are shell-style globs
where `*` crosses `/`, as in the `matches` helper and, for our purposes, in
GitHub's `**`. Exclusions (`!pattern`, `paths-ignore:`) are not checked: a stale
one excludes nothing, so it cannot turn a job off. Tracked files come from `git ls-files`.

Usage: scripts/ci/check_workflow_paths.py [repo-root]
"""

import fnmatch
import re
import shlex
import subprocess
import sys
from pathlib import Path

PATHS_KEY = re.compile(r"^\s*paths:\s*$")
LIST_ITEM = re.compile(r"""^\s*-\s+(['"]?)(.+?)\1\s*(#.*)?$""")


def filter_patterns(text):
    """Yield (line_no, pattern) for each `paths:` list item."""
    in_paths, indent = False, 0
    for no, line in enumerate(text.splitlines(), 1):
        if PATHS_KEY.match(line):
            in_paths, indent = True, len(line) - len(line.lstrip())
            continue
        if not in_paths:
            continue
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        m = LIST_ITEM.match(line)
        if m and len(line) - len(line.lstrip()) >= indent:
            yield no, m.group(2)
        else:
            in_paths = False


def matches_patterns(text):
    """Yield (line_no, pattern) for each argument of a shell `matches` call."""
    lines = text.splitlines()
    i = 0
    while i < len(lines):
        m = re.search(r"(?:^|[\s$(])(?:flag\s+)?matches\s+(?=\S)", lines[i])
        stripped = lines[i].lstrip()
        if not m or stripped.startswith("#") or stripped.startswith("matches()"):
            i += 1
            continue
        start = i
        stmt = lines[i][m.end():]
        while stmt.rstrip().endswith("\\") and i + 1 < len(lines):
            stmt = stmt.rstrip()[:-1] + " " + lines[i + 1].strip()
            i += 1
        stmt = re.sub(r"\)\s*$", "", stmt)
        try:
            tokens = shlex.split(stmt, comments=True)
        except ValueError:
            tokens = []
        for tok in tokens:
            yield start + 1, tok
        i += 1


def has_match(pattern, files):
    pattern = pattern.lstrip("!")
    if pattern in files:
        return True
    return any(fnmatch.fnmatchcase(f, pattern.replace("**", "*")) for f in files)


def find_stale(root, files):
    stale = []
    for wf in sorted((root / ".github" / "workflows").glob("*.y*ml")):
        text = wf.read_text()
        found = list(filter_patterns(text)) + list(matches_patterns(text))
        for no, pat in found:
            # Only repo-path-looking literals; skip shell variables and flags.
            if pat.startswith(("-", "!")) or "$" in pat or not pat.strip():
                continue
            if not has_match(pat, files):
                stale.append(f"{wf.relative_to(root)}:{no}: '{pat}' matches nothing in the repo")
    return stale


def main():
    root = Path(sys.argv[1] if len(sys.argv) > 1 else ".").resolve()
    out = subprocess.run(
        ["git", "-C", str(root), "ls-files"], capture_output=True, text=True, check=True
    ).stdout
    files = set(out.splitlines())
    stale = find_stale(root, files)
    if stale:
        print("❌ workflow path gates naming nothing (renamed or deleted?):", file=sys.stderr)
        for s in stale:
            print("  " + s, file=sys.stderr)
        return 1
    print("✅ every workflow path gate matches something in the repo")
    return 0


if __name__ == "__main__":
    sys.exit(main())

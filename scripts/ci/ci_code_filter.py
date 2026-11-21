#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Print `true` when any changed path (stdin, one per line) is one ci.yml treats as code (BT-5086).

ci.yml runs on every pull_request so that its `ci-ok` aggregate check always
exists; its `changes` job calls this to decide whether the code jobs run. The
answer must equal what the old workflow-level `pull_request: paths:` filter
decided (a PR that filter would not have started is "docs-only"). The one
literal list is ci.yml's `on.push.paths` (GitHub needs it as literal YAML there),
and this reads it rather than keeping a second copy; test_ci_code_filter.py pins
it to the former pull_request list.

Usage: git diff --name-only BASE HEAD | ci_code_filter.py [ci.yml]
"""

import sys
from pathlib import Path

from path_filter import included


def code_patterns(workflow):
    import yaml

    doc = yaml.safe_load(Path(workflow).read_text())
    return (doc.get("on") or doc.get(True))["push"]["paths"]


def has_code(patterns, paths):
    return any(included(patterns, p) for p in paths if p)


def main(argv):
    workflow = argv[1] if len(argv) > 1 else str(Path(__file__).resolve().parents[2] / ".github/workflows/ci.yml")
    paths = [line.strip() for line in sys.stdin]
    print("true" if has_code(code_patterns(workflow), paths) else "false")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

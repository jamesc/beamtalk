#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Fail when a composite action directory is not listed in dependabot.yml (BT-5137).

Dependabot finds workflows under `.github/workflows` from `/`, but each composite
action (`.github/actions/<name>/action.yml`) is a separate directory that must be
listed, or the action SHAs pinned there never get bumped.
"""

import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]


def listed(config):
    dirs = set()
    for u in config["updates"]:
        if u["package-ecosystem"] == "github-actions":
            dirs.update(u.get("directories", []) + ([u["directory"]] if "directory" in u else []))
    return dirs


def missing(root=ROOT):
    config = yaml.safe_load((root / ".github/dependabot.yml").read_text())
    have = listed(config)
    want = {"/" + str(p.parent.relative_to(root)) for p in (root / ".github/actions").glob("*/action.y*ml")}
    return sorted(want - have)


if __name__ == "__main__":
    bad = missing()
    for d in bad:
        print(f"dependabot.yml: github-actions directories does not list {d}")
    sys.exit(1 if bad else 0)

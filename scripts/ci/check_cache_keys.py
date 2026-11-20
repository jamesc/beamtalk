#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Fail when a cache `restore-keys` entry is not a prefix of its step's `key` (BT-5086).

`actions/cache` restores the newest entry whose key *starts with* a restore key.
An entry that is not a prefix of the primary key can never match a sibling of
that key, so a miss on the exact key always starts cold. The check compares the
raw template text (a `${{ ... }}` expression is an opaque token): a restore key
that is a textual prefix of the key template is a prefix of every rendered key.

Usage: scripts/ci/check_cache_keys.py [repo-root]
"""

import re
import sys
from pathlib import Path

KEY = re.compile(r"^(\s*)key:\s*(.+?)\s*$")
RESTORE = re.compile(r"^(\s*)restore-keys:\s*(\|[-+]?|>[-+]?)?\s*(.*?)\s*$")


def indent_of(line):
    return len(line) - len(line.lstrip())


def check_text(text):
    """Return a list of (line_no, restore_key, key) for every non-prefix restore key."""
    problems = []
    lines = text.splitlines()
    key, key_indent = None, -1
    i = 0
    while i < len(lines):
        line = lines[i]
        m = KEY.match(line)
        if m:
            key, key_indent = m.group(2).strip("'\""), len(m.group(1))
        m = RESTORE.match(line)
        if m and key is not None and len(m.group(1)) == key_indent:
            entries = []
            if m.group(2):  # block scalar: following, more-indented lines
                j = i + 1
                while j < len(lines) and (not lines[j].strip() or indent_of(lines[j]) > key_indent):
                    if lines[j].strip():
                        entries.append((j + 1, lines[j].strip()))
                    j += 1
                i = j - 1
            elif m.group(3):
                entries.append((i + 1, m.group(3).strip("'\"")))
            for no, entry in entries:
                if not key.startswith(entry):
                    problems.append((no, entry, key))
        i += 1
    return problems


def main(argv):
    root = Path(argv[1]) if len(argv) > 1 else Path(__file__).resolve().parents[2]
    bad = 0
    for f in sorted((root / ".github").rglob("*.yml")):
        for no, entry, key in check_text(f.read_text()):
            bad += 1
            print(f"{f.relative_to(root)}:{no}: restore-key is not a prefix of the key\n"
                  f"    restore-key: {entry}\n    key:         {key}")
    if bad:
        print(f"check_cache_keys: {bad} restore-key(s) can never match", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

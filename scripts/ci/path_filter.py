# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""GitHub Actions `paths:` filter semantics, shared by ci_ok.py and ci_code_filter.py (BT-5086).

`*` stays inside a path segment, `**` crosses `/`, `?` is one non-`/` character.
Patterns are evaluated in order, the last match wins, and a leading `!` excludes.
"""

import re


def glob_regex(pat):
    out, i = "", 0
    while i < len(pat):
        if pat.startswith("**", i):
            out += ".*"
            i += 2
        elif pat[i] == "*":
            out += "[^/]*"
            i += 1
        elif pat[i] == "?":
            out += "[^/]"
            i += 1
        else:
            out += re.escape(pat[i])
            i += 1
    return re.compile(out + r"\Z")


def included(patterns, path):
    """True when a workflow with this `paths:` list would start for a change to `path`."""
    result = False
    for pat in patterns:
        neg = pat.startswith("!")
        if glob_regex(pat[1:] if neg else pat).match(path):
            result = not neg
    return result

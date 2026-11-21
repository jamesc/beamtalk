#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Verdict for ci.yml's `ci-ok` aggregate job, and a structural check of it (BT-5086).

`ci-ok` is the one check branch protection should require. Each job it `needs`
must have succeeded, except that a job may be *skipped* when ci.yml itself gates
it with a job-level `if:` that reads `needs.changes.outputs.*` (the path gates),
and only while the `changes` job succeeded. Everything else is a failure:
failed, cancelled, or skipped without such a gate (a skipped always-run job, or
every job skipped because an upstream failed, must not read as green).

The set of skippable jobs is read from ci.yml, not listed here, so adding a
gated job needs no change to this script.

  ci_ok.py verdict <ci.yml>     NEEDS_JSON in $NEEDS; exit 1 on a bad verdict
  ci_ok.py check   [ci.yml]     fail unless ci-ok needs every other job, and
                                ci.yml / ci-ok-docs.yml path filters partition the repo
"""

import json
import os
import re
import subprocess
import sys
from pathlib import Path

JOB = "ci-ok"


def skippable(job):
    cond = str(job.get("if", ""))
    return "needs.changes.outputs" in cond


def verdict(needs, jobs):
    """Return a list of problems for `needs` (the `toJSON(needs)` map)."""
    problems = []
    changes_ok = needs.get("changes", {}).get("result") == "success"
    for name, info in sorted(needs.items()):
        result = info.get("result")
        if result == "success":
            continue
        if result == "skipped" and changes_ok and skippable(jobs.get(name, {})):
            continue
        problems.append(f"{name}: {result}")
    return problems


def structure(jobs):
    """ci-ok must exist, run always, and need every other job."""
    if JOB not in jobs:
        return [f"no `{JOB}` job"]
    ok = jobs[JOB]
    problems = []
    if "always()" not in str(ok.get("if", "")):
        problems.append(f"{JOB} must run with `if: always()` (a skipped aggregate reads as green)")
    needs = ok.get("needs", [])
    needs = [needs] if isinstance(needs, str) else list(needs)
    missing = sorted(set(jobs) - {JOB} - set(needs))
    if missing:
        problems.append(f"{JOB} does not need: {', '.join(missing)}")
    return problems


def glob_regex(pat):
    """GitHub path-filter glob: `*` stays inside a path segment, `**` crosses `/`."""
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
    """Last matching pattern wins; a `!` pattern excludes."""
    result = False
    for pat in patterns:
        neg = pat.startswith("!")
        if glob_regex(pat[1:] if neg else pat).match(path):
            result = not neg
    return result


def pr_paths(path):
    import yaml

    doc = yaml.safe_load(Path(path).read_text())
    on = doc.get("on", doc.get(True))
    return on["pull_request"]["paths"]


# Paths that must each start exactly one of the two workflows, besides the
# tracked files: they probe the filter's edges (nested .md, re-included docs).
PROBES = [
    "README.md", "crates/x/README.md", "docs/a.md", "docs/learning/ch1.md", "docs/deep/dir/x.md",
    "docs/beamtalk-language-features.md", "docs/repl-protocol.md", "docs/development/surface-parity.md",
    "docs/development/other.md", ".devcontainer/Dockerfile",
    "LICENSE", "SECURITY.md", "src/new.rs", ".github/workflows/ci.yml",
]


def partition(ci_paths, docs_paths, files):
    """Files that start both workflows' neither, or ci.yml and the docs one: ok only if one."""
    bad = []
    for f in files:
        a, b = included(ci_paths, f), included(docs_paths, f)
        if a == b and not (a and b):  # neither
            bad.append(f"{f}: starts neither ci.yml nor ci-ok-docs.yml")
        elif a and b:
            bad.append(f"{f}: starts both ci.yml and ci-ok-docs.yml")
    return bad


def tracked_files(root):
    out = subprocess.run(["git", "-C", str(root), "ls-files"], capture_output=True, text=True, check=True)
    return out.stdout.splitlines()


def load_jobs(path):
    import yaml

    return yaml.safe_load(Path(path).read_text())["jobs"]


def main(argv):
    cmd = argv[1] if len(argv) > 1 else "check"
    path = argv[2] if len(argv) > 2 else str(Path(__file__).resolve().parents[2] / ".github/workflows/ci.yml")
    jobs = load_jobs(path)
    if cmd == "verdict":
        problems = verdict(json.loads(os.environ["NEEDS"]), jobs)
        for p in problems:
            print(f"::error::ci-ok: {p}")
        if problems:
            return 1
        print("ci-ok: every required job succeeded or was skipped by a path gate")
        return 0
    problems = structure(jobs)
    docs_wf = Path(path).with_name("ci-ok-docs.yml")
    problems += partition(pr_paths(path), pr_paths(docs_wf), PROBES + tracked_files(Path(path).parents[2]))
    for p in problems:
        print(f"{path}: {p}")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

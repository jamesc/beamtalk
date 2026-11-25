#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Verdict for ci.yml's `ci-ok` aggregate job, and a structural check of it (BT-5086).

`ci-ok` is the one check branch protection should require. ci.yml runs on every
pull_request; its `changes` job decides which jobs have work (`code` for a PR
that touches anything but docs, plus one output per path gate). Each job `ci-ok`
needs must have succeeded, except that a job may be *skipped* while `changes`
succeeded when ci.yml itself explains the skip:

* it has a path gate (`if:` reading `needs.changes.outputs.<gate>`), or
* it is a code job (gated on `needs.changes.outputs.code`, directly or through a
  `needs` chain) and `changes` reported `code=false` (a docs-only PR).

Everything else is a failure: failed, cancelled, or skipped without such a
reason. In particular a code job skipped while `code` is true reads as red, and
so does any skip after a failed `changes` job.

The skippable set is read from ci.yml, not listed here, so adding a gated job
needs no change to this script.

  ci_ok.py verdict <ci.yml>     NEEDS_JSON in $NEEDS; exit 1 on a bad verdict
  ci_ok.py check   [ci.yml]     fail unless ci-ok needs every other job and every
                                job is gated on a `changes` output
"""

import json
import os
import re
import sys
from pathlib import Path

JOB = "ci-ok"

# Jobs that deliberately run on every PR, docs-only included (BT-5137): the docs
# checks exist for the PR that touches only docs. They may never be skipped.
ALWAYS_RUN = {"docs-checks"}


OUTPUT = re.compile(r"needs\.changes\.outputs\.(\w+)")


def gates(job):
    return set(OUTPUT.findall(str(job.get("if", ""))))


def needs_of(job):
    needs = job.get("needs", [])
    return [needs] if isinstance(needs, str) else list(needs)


def path_gated(job):
    """A path gate: `if:` reads a `changes` output other than `code`."""
    return bool(gates(job) - {"code"})


def code_gated(name, jobs, seen=()):
    """Gated on `code` directly, or because a job it needs is."""
    job = jobs.get(name, {})
    if "code" in gates(job):
        return True
    return any(code_gated(n, jobs, seen + (name,)) for n in needs_of(job) if n not in seen)


def verdict(needs, jobs):
    """Return a list of problems for `needs` (the `toJSON(needs)` map)."""
    problems = []
    changes = needs.get("changes", {})
    changes_ok = changes.get("result") == "success"
    docs_only = changes_ok and changes.get("outputs", {}).get("code") == "false"
    for name, info in sorted(needs.items()):
        result = info.get("result")
        if result == "success":
            continue
        if result == "skipped" and changes_ok:
            if path_gated(jobs.get(name, {})) or (docs_only and code_gated(name, jobs)):
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
    if "code" not in jobs.get("changes", {}).get("outputs", {}):
        problems.append("`changes` must emit a `code` output")
    # Every job but the two must be skippable for a stated reason; otherwise it
    # would run (and cost a runner) on a docs-only PR.
    for name, job in sorted(jobs.items()):
        if name in ("changes", JOB) or name in ALWAYS_RUN:
            continue
        if not (path_gated(job) or code_gated(name, jobs)):
            problems.append(f"{name} is not gated on a `changes` output (it would run on docs-only PRs)")
    return problems


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
        print("ci-ok: every needed job succeeded or was skipped for a stated reason")
        return 0
    problems = structure(jobs)
    for p in problems:
        print(f"{path}: {p}")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

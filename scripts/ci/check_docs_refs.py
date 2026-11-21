#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Fail when contributor docs name a `just` recipe or repo path that is gone (BT-5089).

Scope: AGENTS.md, CONTRIBUTING.md, docs/agents/*.md, docs/development/*.md (all
checked for recipes and paths) and the ADRs in ADR_PATH_ONLY (paths only; ADRs
are historical, so recipes they name are not checked).

* Recipes: every `just <name>` in a code span or fenced block must be a recipe
  in the Justfile.
* Paths: every backticked token that looks like a repository path (first
  segment is a top-level repo entry; gitignored build outputs are skipped, and it has a `/` or a file extension) must
  exist. Globs, placeholders and URLs are skipped. Intended historical
  references go in scripts/ci/docs-refs-allowlist.txt as `<file>: <token>`
  lines (or `*: <token>`) with a reason comment above; a stale entry fails.
"""
import glob
import os
import re
import subprocess
import sys

ADR_PATH_ONLY = [
    "docs/ADR/0027-*.md",
    "docs/ADR/0040-*.md",
    "docs/ADR/0064-*.md",
    "docs/ADR/0091-*.md",
]
FULL = ["AGENTS.md", "CONTRIBUTING.md", "docs/agents/*.md", "docs/development/*.md"]
ALLOWLIST = "scripts/ci/docs-refs-allowlist.txt"
PLACEHOLDER = re.compile(r"my_|YourClass|NNNN|abc123|crash-|033_")
EXTS = (".rs", ".erl", ".hrl", ".bt", ".btscript", ".md", ".sh", ".py", ".toml",
        ".yml", ".yaml", ".json", ".escript", ".txt", ".ts", ".js", ".ex", ".exs")


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def recipes():
    out = subprocess.check_output(["just", "--summary", "--unsorted"], text=True)
    names = set(out.split())
    # Windows variants and private recipes are covered by --summary; add aliases.
    return names


def tracked_top_level():
    out = subprocess.check_output(["git", "ls-files"], text=True).splitlines()
    return {p.split("/")[0] for p in out}


def ignored(path):
    """Build outputs (runtime/_build, coverage reports, lock files) are gitignored."""
    return subprocess.run(["git", "check-ignore", "-q", path.rstrip("/") + "/"]).returncode == 0


def load_allowlist():
    entries = {}
    if os.path.exists(ALLOWLIST):
        for line in open(ALLOWLIST):
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            f, _, tok = line.partition(": ")
            entries[(f, tok)] = False
    return entries


def spans(text):
    """Yield (lineno, content) for inline code spans and fenced-block lines."""
    fenced = False
    for n, line in enumerate(text.splitlines(), 1):
        if line.lstrip().startswith("```"):
            fenced = not fenced
            continue
        if fenced:
            yield n, line, True
        else:
            for m in re.finditer(r"`([^`\n]+)`", line):
                yield n, m.group(1), False


def candidate_paths(content, fenced, tops):
    toks = [content] if not fenced else content.split()
    for tok in toks:
        tok = tok.strip().rstrip(".,:;)")
        if tok.startswith("./"):
            tok = tok[2:]
        tok = re.sub(r"::\w+$", "", tok)  # path::item
        tok = re.sub(r"[:#]\d+(-\d+)?$", "", tok)  # file:line
        if any(c in tok for c in "*<>{}$ ()[]|=\"'") or "://" in tok or not tok:
            continue
        if tok.startswith(("/", "~", "-")) or ".." in tok:
            continue
        if PLACEHOLDER.search(tok):  # illustrative names in how-to examples
            continue
        first = tok.split("/")[0]
        if first not in tops:
            continue
        if "/" not in tok and not tok.endswith(EXTS):
            continue
        yield tok.rstrip("/")


def main():
    os.chdir(repo_root())
    names = recipes()
    tops = tracked_top_level()
    allow = load_allowlist()
    bad = []

    def files(pats):
        return sorted({f for p in pats for f in glob.glob(p)})

    for path in files(FULL) + files(ADR_PATH_ONLY):
        text = open(path, encoding="utf-8").read()
        full = path in files(FULL)
        for n, content, fenced in spans(text):
            if full:
                for m in re.finditer(r"(?:^\s*\$?\s*|&&\s*|;\s*|\|\|?\s*)just\s+([a-z][\w-]*)", content):
                    name = m.group(1)
                    tok = f"just {name}"
                    hit = next((k for k in ((path, tok), ("*", tok)) if k in allow), None)
                    if hit:
                        allow[hit] = True
                    elif name not in names:
                        bad.append(f"{path}:{n}: `{tok}` is not a Justfile recipe")
            for tok in candidate_paths(content, fenced, tops):
                if os.path.exists(tok) or ignored(tok):
                    continue
                hit = next((k for k in ((path, tok), ("*", tok)) if k in allow), None)
                if hit:
                    allow[hit] = True
                    continue
                bad.append(f"{path}:{n}: `{tok}` does not exist")

    for (f, tok), used in allow.items():
        if not used:
            bad.append(f"{ALLOWLIST}: stale entry `{f}: {tok}`")

    if bad:
        print("docs reference check failed:\n  " + "\n  ".join(bad))
        sys.exit(1)
    print("docs references ok")


if __name__ == "__main__":
    main()

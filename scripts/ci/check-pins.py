#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Check that downloaded build tools are pinned and that repeated pins agree.

BT-5087: CI, the cloud setup script and the devcontainer each install some of
the same tools. A pin that is written in more than one place is only a pin if
the places agree, so this fails when they differ, and when a floating
nightly / `latest` / `main` reference comes back.
"""
import re
import sys
from pathlib import Path


def read(p):
    return Path(p).read_text()


problems = []


def distinct(label, found):
    vals = {v for _, v in found}
    if len(vals) > 1:
        where = ", ".join(f"{f}={v}" for f, v in found)
        problems.append(f"{label}: pins disagree ({where})")
    if not vals:
        problems.append(f"{label}: no pin found")


# Fuzz nightly toolchain: one date everywhere.
fuzz_files = [
    ".github/workflows/fuzz.yml",
    ".github/workflows/fuzz-triage.yml",
    ".github/claude-fuzz-triage-prompt.md",
]
found = []
for f in fuzz_files:
    text = read(f)
    for m in re.finditer(r"nightly-\d{4}-\d{2}-\d{2}", text):
        found.append((f, m.group(0)))
    if re.search(r"toolchain: nightly\s*$|\+nightly\s", text, re.M):
        problems.append(f"{f}: floating nightly toolchain; pin it to a date")
distinct("fuzz nightly", found)

# cargo-fuzz.
found = []
for f in [".github/workflows/fuzz.yml", ".github/workflows/fuzz-triage.yml"]:
    found += [(f, v) for v in re.findall(r"cargo install cargo-fuzz --locked --version (\S+)", read(f))]
found += [("scripts/setup-cloud.sh", v) for v in re.findall(r"cargo-fuzz@([\d.]+)", read("scripts/setup-cloud.sh"))]
found += [(".devcontainer/Dockerfile", v) for v in re.findall(r"cargo-fuzz@([\d.]+)", read(".devcontainer/Dockerfile"))]
distinct("cargo-fuzz", found)

# cargo-nextest.
found = [(".github/actions/install-nextest/action.yml", v)
         for v in re.findall(r"get\.nexte\.st/([\d.]+)/", read(".github/actions/install-nextest/action.yml"))]
found += [("scripts/setup-cloud.sh", v) for v in re.findall(r'NEXTEST_PIN="([\d.]+)"', read("scripts/setup-cloud.sh"))]
found += [(".devcontainer/Dockerfile", v) for v in re.findall(r"cargo-nextest@([\d.]+)", read(".devcontainer/Dockerfile"))]
distinct("cargo-nextest", found)

# cargo-binstall.
found = [("scripts/setup-cloud.sh", v) for v in re.findall(r'BINSTALL_PIN="([\d.]+)"', read("scripts/setup-cloud.sh"))]
found += [(".devcontainer/Dockerfile", v) for v in re.findall(r"ARG BINSTALL_VERSION=([\d.]+)", read(".devcontainer/Dockerfile"))]
distinct("cargo-binstall", found)

# cargo-llvm-cov.
found = [(".github/workflows/coverage.yml", v) for v in re.findall(r"tool: cargo-llvm-cov@([\d.]+)", read(".github/workflows/coverage.yml"))]
found += [("scripts/setup-cloud.sh", v) for v in re.findall(r"cargo-llvm-cov@([\d.]+)", read("scripts/setup-cloud.sh"))]
found += [(".devcontainer/Dockerfile", v) for v in re.findall(r"cargo-llvm-cov@([\d.]+)", read(".devcontainer/Dockerfile"))]
distinct("cargo-llvm-cov", found)

# Floating references that must not come back.
for f, pat, why in [
    ("scripts/setup-cloud.sh", r"get\.nexte\.st/latest", "nextest `latest`"),
    ("scripts/setup-cloud.sh", r"cargo-binstall/main/", "cargo-binstall `main`"),
    (".devcontainer/Dockerfile", r"cargo-binstall/main/", "cargo-binstall `main`"),
]:
    if re.search(pat, read(f)):
        problems.append(f"{f}: floating download ({why})")

if problems:
    print("pin check failed:\n  " + "\n  ".join(problems))
    sys.exit(1)
print("pins ok")

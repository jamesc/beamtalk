#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Fail if a crate outside the Cargo workspace drifts from its lint levels.

`fuzz` and `desktop/src-tauri` are not workspace members, so they cannot say
`[lints] workspace = true`; they carry a copy of `[workspace.lints]` instead
(BT-5087). This check is what keeps that copy honest. Every workspace member
must inherit with `[lints] workspace = true`.
"""
import glob
import sys
import tomllib

ROOT = tomllib.load(open("Cargo.toml", "rb"))
WANT = ROOT["workspace"]["lints"]
OUTSIDE = ["fuzz/Cargo.toml", "desktop/src-tauri/Cargo.toml"]

bad = []
for path in OUTSIDE:
    got = tomllib.load(open(path, "rb")).get("lints", {})
    if got != WANT:
        bad.append(f"{path}: [lints] differs from the root [workspace.lints]")

members = []
for pat in ROOT["workspace"]["members"]:
    members += glob.glob(pat + "/Cargo.toml")
for path in sorted(members):
    if tomllib.load(open(path, "rb")).get("lints", {}).get("workspace") is not True:
        bad.append(f"{path}: missing `[lints] workspace = true`")

if bad:
    print("lint parity check failed:\n  " + "\n  ".join(bad))
    sys.exit(1)
print("lint parity ok")

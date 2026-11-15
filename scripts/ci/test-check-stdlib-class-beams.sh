#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Test (BT-4925): scripts/ci/check-stdlib-class-beams.sh passes on a populated
# ebin dir every time, and fails with a diagnostic on an empty or missing one.
#
# The old inline guard in `just test-install` was
# `! find ... | grep -q .` under `set -o pipefail`, which fails when find is
# SIGPIPEd after grep -q exits on the first match. A directory big enough that
# find writes more than one pipe buffer trips it every time, so this builds
# one and first proves the old pattern would have failed on it.
#
# Usage: scripts/ci/test-check-stdlib-class-beams.sh

set -euo pipefail

REPO_ROOT="$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel)"
GUARD="$REPO_ROOT/scripts/ci/check-stdlib-class-beams.sh"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT

FAILED=0
check() { # check <name> <exit-code-of-assertion>
    if [[ "$2" -eq 0 ]]; then echo "ok   $1"; else echo "FAIL $1"; FAILED=1; fi
}

BIG="$T/big"
mkdir -p "$BIG"
for i in $(seq 1 2000); do : > "$BIG/bt@stdlib@collections@a_fairly_long_class_name_$i.beam"; done
: > "$BIG/beamtalk_stdlib.app"

# Fixture sanity: the old guard really misreads this directory as empty.
old_guard() { ! find "$BIG" -maxdepth 1 -name 'bt@*.beam' 2>/dev/null | grep -q .; }
if old_guard; then rc=0; else rc=1; fi
check "fixture: the old 'find | grep -q' guard misfires on a populated dir" "$((rc == 0 ? 0 : 1))"

# The regression: a populated dir passes, repeatedly.
pass_all=0
for _ in $(seq 1 20); do "$GUARD" "$BIG" >/dev/null 2>&1 || pass_all=1; done
check "populated dir passes 20/20 runs" "$pass_all"

# A dir with only non-class beams / the .app is a real miss.
ONLY_ERL="$T/only-erl"
mkdir -p "$ONLY_ERL"
: > "$ONLY_ERL/beamtalk_stdlib.app"
: > "$ONLY_ERL/beamtalk_stdlib_app.beam"
if out="$("$GUARD" "$ONLY_ERL" 2>&1)"; then rc=1; else rc=0; fi
check "dir without bt@*.beam fails" "$rc"
grep -q '1 .beam file(s), 1 .app file(s)' <<<"$out" && rc=0 || rc=1
check "failure output counts what the dir holds" "$rc"
grep -q 'beamtalk_stdlib_app.beam' <<<"$out" && rc=0 || rc=1
check "failure output lists the dir contents" "$rc"

# A missing dir is a real miss and says so.
if out="$("$GUARD" "$T/nope" 2>&1)"; then rc=1; else rc=0; fi
check "missing dir fails" "$rc"
grep -q 'does not exist' <<<"$out" && rc=0 || rc=1
check "missing-dir output says it does not exist" "$rc"

exit "$FAILED"

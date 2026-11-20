#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Test (BT-5094): scripts/ci/determine-version.sh ignores rolling tags such as
# `edge` and `nightly` when it falls back to `git describe`, and rejects any
# tag that is not a version.
#
# Usage: scripts/ci/test-determine-version.sh

# shellcheck disable=SC2034,SC2016  # OUT/RC are read inside eval'd conditions
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$REPO_ROOT/scripts/ci/determine-version.sh"

T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT

failures=0
check() { # label, condition string (evaluated; a failing condition must not abort the run)
  if eval "$2"; then echo "ok: $1"; else echo "FAIL: $1" >&2; failures=$((failures + 1)); fi
}

# run <args...>: runs the script in the scratch repo, stdout -> $OUT, status -> $RC
run() {
  set +e
  OUT="$(cd "$T/repo" && env -u GITHUB_OUTPUT bash "$SCRIPT" "$@" 2>"$T/stderr")"
  RC=$?
  set -e
}

mkdir "$T/repo"
git -C "$T/repo" init -q
git -C "$T/repo" config user.email t@example.invalid
git -C "$T/repo" config user.name t
git -C "$T/repo" config commit.gpgsign false
git -C "$T/repo" config tag.gpgsign false

# No tags at all: there is no version to build.
git -C "$T/repo" commit -q --allow-empty -m one
run workflow_dispatch "" ""
check "no tags is rejected" '[ "$RC" -ne 0 ]'

# Only rolling tags reachable: still no version.
git -C "$T/repo" tag edge
git -C "$T/repo" tag nightly
run workflow_dispatch "" ""
check "edge/nightly alone are rejected" '[ "$RC" -ne 0 ]'

# A real release tag on an older commit wins over newer rolling tags.
git -C "$T/repo" tag v0.5.0
git -C "$T/repo" commit -q --allow-empty -m two
git -C "$T/repo" tag -f edge >/dev/null
git -C "$T/repo" tag -f nightly >/dev/null
run workflow_dispatch "" ""
check "describe fallback picks v0.5.0, not edge/nightly" \
  '[ "$RC" -eq 0 ] && grep -qx "tag=v0.5.0" <<<"$OUT" && grep -qx "version=0.5.0" <<<"$OUT"'

# Explicit tags: release event and dispatch input.
run release v0.6.1 ""
check "release event tag v0.6.1" '[ "$RC" -eq 0 ] && grep -qx "version=0.6.1" <<<"$OUT"'
run workflow_dispatch "" v1.0.0-rc.1
check "input tag v1.0.0-rc.1" '[ "$RC" -eq 0 ] && grep -qx "version=1.0.0-rc.1" <<<"$OUT"'

for bad in edge nightly dev latest v1 v1.2 'v1.2.3;x'; do
  run release "$bad" ""
  check "release tag '$bad' is rejected" '[ "$RC" -ne 0 ]'
  run workflow_dispatch "" "$bad"
  check "input tag '$bad' is rejected" '[ "$RC" -ne 0 ]'
done

[ "$failures" -eq 0 ] || { echo "$failures failure(s)" >&2; exit 1; }
echo "all determine-version tests passed"

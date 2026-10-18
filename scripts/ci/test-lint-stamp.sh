#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Test (BT-3926): scripts/lint-stamp.sh writes the same fingerprint whatever
# the cwd and whatever repository-local GIT_* variables it inherits.
#
# The `lint-js` recipe runs `lint-stamp.sh write js` from `editors/vscode`, and
# under the pre-push hook git exports GIT_DIR, which (without GIT_WORK_TREE)
# makes the cwd the work tree, so `hash-object` failed to open the
# repo-root-relative paths `ls-files` printed. This writes the js stamp from
# the repo root with a clean environment, then from `editors/vscode` with and
# without hook-style GIT_* variables, and asserts every stamp is equal.
#
# The developer's existing .lint-js-stamp is restored on exit, so running this
# never marks the JS lint as passed.
#
# Usage: scripts/ci/test-lint-stamp.sh

set -euo pipefail

# shellcheck disable=SC2046  # the variable names contain no whitespace
unset $(git rev-parse --local-env-vars)

REPO_ROOT="$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel)"
GIT_DIR_ABS="$(git -C "$REPO_ROOT" rev-parse --absolute-git-dir)"
LINT_STAMP="$REPO_ROOT/scripts/lint-stamp.sh"
STAMP_FILE="$REPO_ROOT/.lint-js-stamp"
SUBDIR="$REPO_ROOT/editors/vscode"
# sha1 of empty input: what the fingerprint would be if no file got hashed.
EMPTY_SHA1="da39a3ee5e6b4b0d3255bfef95601890afd80709"

BACKUP="$(mktemp)"
had_stamp=false
if [[ -f "$STAMP_FILE" ]]; then
  cp "$STAMP_FILE" "$BACKUP"
  had_stamp=true
fi
restore() {
  if $had_stamp; then cp "$BACKUP" "$STAMP_FILE"; else rm -f "$STAMP_FILE"; fi
  rm -f "$BACKUP"
}
trap restore EXIT

failures=0

# write_from <label> <dir> [VAR=value ...]: write the js stamp from <dir> with
# the given extra environment and print the stamp's contents.
write_from() {
  local dir="$2"
  shift 2
  rm -f "$STAMP_FILE"
  (cd "$dir" && env "$@" "$LINT_STAMP" write js) && cat "$STAMP_FILE"
}

expect_same() {
  local label="$1" got
  if ! got="$(write_from "$@")"; then
    echo "FAIL: $label: lint-stamp.sh write js failed" >&2
    failures=$((failures + 1))
  elif [[ "$got" != "$reference" ]]; then
    echo "FAIL: $label: stamp '$got' != repo-root stamp '$reference'" >&2
    failures=$((failures + 1))
  else
    echo "ok: $label"
  fi
}

reference="$(write_from "repo root" "$REPO_ROOT")"
if [[ -z "$reference" || "$reference" == "$EMPTY_SHA1" ]]; then
  echo "FAIL: repo-root js stamp is '$reference' (no tracked JS file was hashed)" >&2
  exit 1
fi
echo "ok: repo root ($reference)"

expect_same "editors/vscode" "$SUBDIR"
expect_same "editors/vscode, hook GIT_DIR" "$SUBDIR" \
  "GIT_DIR=$GIT_DIR_ABS"
expect_same "editors/vscode, hook GIT_DIR + GIT_WORK_TREE=." "$SUBDIR" \
  "GIT_DIR=$GIT_DIR_ABS" "GIT_WORK_TREE=."
expect_same "editors/vscode, GIT_PREFIX" "$SUBDIR" \
  "GIT_DIR=$GIT_DIR_ABS" "GIT_PREFIX=editors/vscode/"
expect_same "repo root, hook GIT_DIR" "$REPO_ROOT" \
  "GIT_DIR=$GIT_DIR_ABS"

if ((failures > 0)); then
  echo "❌ test-lint-stamp: $failures case(s) disagree with the repo-root stamp" >&2
  exit 1
fi
echo "✅ test-lint-stamp: js stamp is cwd- and GIT_*-independent"

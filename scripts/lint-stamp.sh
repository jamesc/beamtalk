#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# One home for the lint stamps shared by the pre-push hook and the Justfile.
#
#   scripts/lint-stamp.sh check <lang>   exit 0 when <lang>'s sources are
#                                        unchanged since the last `write`
#   scripts/lint-stamp.sh write <lang>   record the current fingerprint
#
# <lang> is rust, erlang, js, beamtalk or elixir. The fingerprint is a hash of
# the git-tracked files matching the language's pathspecs, so it is the same
# in any working directory of the repo. The Justfile's `lint-<lang>` recipes
# `write` on success and the hook `check`s before running one, so a lint that
# `just lint` or `just ci-changed` just ran is not run a second time by
# `git push`. Stamps live at <repo root>/.lint-<lang>-stamp (gitignored).
#
# The fingerprint must not depend on the caller's cwd or git environment
# (BT-3926). Git exports GIT_DIR (and friends) to the hooks it runs, and they
# override both `-C` and repository discovery: with GIT_DIR set and no
# GIT_WORK_TREE the work tree is the *cwd*, so from the `lint-js` recipe (which
# runs in `editors/vscode`) `ls-files` listed repo-root-relative paths that
# `hash-object` then failed to open relative to `editors/vscode`. So the script
# drops every repository-local GIT_* variable (the list git itself reports) and
# locates the repository from its own path, not the cwd. Pathname expansion is
# off too: the pathspecs are word-split unquoted below, and with globbing on
# `*.ts` expanded against the cwd (`vitest.config.ts` in `editors/vscode`), so
# the js fingerprint written there hashed a different file set.
# scripts/ci/test-lint-stamp.sh pins both.
set -euo pipefail -o noglob

# shellcheck disable=SC2046  # the variable names contain no whitespace
unset $(git rev-parse --local-env-vars)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"

patterns() {
  case "$1" in
    rust)     echo '*.rs' 'Cargo.toml' 'Cargo.lock' 'clippy.toml' 'rustfmt.toml' '.cargo/*' ;;
    erlang)   echo '*.erl' '*.hrl' 'rebar.config' ;;
    js)       echo '*.js' '*.ts' '*.jsx' '*.tsx' ;;
    beamtalk) echo '*.bt' '*.btscript' ;;
    elixir)   echo 'editors/liveview/*.ex' 'editors/liveview/*.exs' ;;
    *) echo "lint-stamp: unknown language '$1' (rust|erlang|js|beamtalk|elixir)" >&2; exit 2 ;;
  esac
}

fingerprint() {
  # shellcheck disable=SC2046  # no whitespace in the pathspecs; noglob is set
  set -- $(patterns "$1")
  git -C "$REPO_ROOT" ls-files -- "$@" \
    | git -C "$REPO_ROOT" hash-object --stdin-paths \
    | { sha1sum 2>/dev/null || shasum -a 1; } \
    | cut -d' ' -f1
}

stamp_file() { echo "$REPO_ROOT/.lint-$1-stamp"; }

case "${1:-}" in
  check)
    [[ -n "${2:-}" ]] || { echo "usage: $0 check <lang>" >&2; exit 2; }
    [[ -f "$(stamp_file "$2")" ]] && [[ "$(cat "$(stamp_file "$2")")" == "$(fingerprint "$2")" ]]
    ;;
  write)
    [[ -n "${2:-}" ]] || { echo "usage: $0 write <lang>" >&2; exit 2; }
    fingerprint "$2" > "$(stamp_file "$2")"
    ;;
  *)
    echo "usage: $0 check|write <rust|erlang|js|beamtalk|elixir>" >&2
    exit 2
    ;;
esac

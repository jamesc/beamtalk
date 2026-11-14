#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Guard (BT-4822, ADR 0139 decision 8): user-facing output must not contain a
# compiler-made name -- a `bt@package@module` atom, a `class_`-prefixed
# function name, `dispatch/4`, `safe_dispatch` or an anonymous `-fun-N-`.
# "Hide what the compiler made up; show what a person named."
#
# The pattern list lives in ONE file, scripts/ci/compiler-made-name-patterns.txt,
# shared with the EUnit trace guard (`has_raw_mfa/1` in
# beamtalk_test_case_tests.erl). Files that show these names on purpose
# (raw-mode fixtures) are listed, each with a reason, in ONE file:
# scripts/ci/compiler-made-names-allowlist.txt.
#
# What is scanned (tracked files only):
#   1. golden files: `*.snap` snapshots, except the `*codegen*` ones, which
#      hold generated Core Erlang (compiler output, not user-facing text);
#      and `*.json` / `*.expected` goldens under tests/;
#   2. expected-output lines (`// => ...`) in `.bt` / `.btscript` tests under
#      stdlib/test, stdlib/bootstrap-test, tests/ and examples/;
#   3. message text in the runtime message sites: a prose string literal (one
#      with a space, `~` or `{`) in runtime/apps/*/src/*.erl and in
#      crates/*/src/**/*.rs (not beamtalk-codegen, whose job is to write Core
#      Erlang). Comments and `-doc` text are ignored; so is a bare atom or
#      prefix test such as `lists:prefix("bt@", Name)`.
#
# To clear a failure: reword the message to use the display name (`Class>>sel`,
# `Class class>>sel`) -- see docs/development/debugging.md. If the file is a
# raw-mode fixture, add it to the allowlist with a reason.
#
# Usage: scripts/ci/lint-compiler-made-names.sh [--self-test]

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

PATTERNS_FILE="scripts/ci/compiler-made-name-patterns.txt"
ALLOWLIST_FILE="scripts/ci/compiler-made-names-allowlist.txt"

# Pattern file -> one ERE alternation.
ALT="$(grep -vE '^[[:space:]]*(#|$)' "$PATTERNS_FILE" | paste -sd'|' -)"
if [[ -z "$ALT" ]]; then
    echo "❌ lint-compiler-made-names: $PATTERNS_FILE has no patterns"
    exit 1
fi

# Allowlisted paths (before the `#`), with the reason required.
declare -a ALLOWED=()
while IFS= read -r line; do
    [[ "$line" =~ ^[[:space:]]*(#|$) ]] && continue
    path="${line%%#*}"
    reason="${line#*#}"
    path="$(echo "$path" | xargs)"
    if [[ "$line" != *"#"* || -z "$(echo "$reason" | xargs)" ]]; then
        echo "❌ lint-compiler-made-names: allowlist entry '$path' has no reason (write '<path>  # <reason>')"
        exit 1
    fi
    ALLOWED+=("$path")
done < "$ALLOWLIST_FILE"

# Scan one directory/file set; emit `path:line:text` hits.
scan() {
    local root="$1"
    local hits=""
    # 1. goldens
    hits+="$(git -C "$root" grep -nE "$ALT" -- '*.snap' ':(exclude)*codegen*' \
        'tests/*.json' 'tests/**/*.json' 'tests/**/*.expected' 2>/dev/null || true)"$'\n'
    # 2. expected-output lines in tests
    hits+="$(git -C "$root" grep -nE "$ALT" -- \
        'stdlib/test/**' 'stdlib/bootstrap-test/**' 'tests/**' 'examples/**' \
        ':(exclude)*.json' ':(exclude)*.erl' ':(exclude)*.rs' 2>/dev/null \
        | grep -E '^[^:]*:[0-9]+:[[:space:]]*//[[:space:]]*=>' || true)"$'\n'
    # 3. prose string literals in message sites
    local prose="\"[^\"]*[ ~][^\"]*($ALT)[^\"]*\"|\"[^\"]*($ALT)[^\"]*[ ~][^\"]*\""
    hits+="$(git -C "$root" grep -nE "$prose" -- 'runtime/apps/*/src/*.erl' 2>/dev/null \
        | grep -vE '^[^:]*:[0-9]+:[[:space:]]*(%|-doc|-moduledoc)' || true)"$'\n'
    hits+="$(git -C "$root" grep -nE "$prose" -- \
        'crates/beamtalk-core/src/*.rs' 'crates/beamtalk-core/src/**/*.rs' 'crates/beamtalk-language-service/src/*.rs' 'crates/beamtalk-language-service/src/**/*.rs' \
        'crates/beamtalk-repl-protocol/src/*.rs' 'crates/beamtalk-repl-protocol/src/**/*.rs' 'crates/beamtalk-repl/src/*.rs' 'crates/beamtalk-repl/src/**/*.rs' \
        'crates/beamtalk-mcp/src/*.rs' 'crates/beamtalk-mcp/src/**/*.rs' 'crates/beamtalk-lsp/src/*.rs' 'crates/beamtalk-lsp/src/**/*.rs' \
        'crates/beamtalk-workspace/src/*.rs' 'crates/beamtalk-workspace/src/**/*.rs' \
        ':(exclude)*/tests/*' ':(exclude)*/tests.rs' ':(exclude)*_tests.rs' 2>/dev/null \
        | grep -vE '^[^:]*:[0-9]+:[[:space:]]*//' || true)"$'\n'
    echo "$hits" | sed '/^$/d' | sort -u
}

filter_allowed() {
    local hits="$1"
    local out="$hits"
    for p in "${ALLOWED[@]}"; do
        out="$(echo "$out" | grep -vF "$p:" || true)"
    done
    echo "$out" | sed '/^$/d'
}

if [[ "${1:-}" == "--self-test" ]]; then
    # The pattern list must catch each name the ADR calls out, and only those.
    fail=0
    check() { # expected(0|1) text
        if echo "$2" | grep -qE "$ALT"; then got=1; else got=0; fi
        if [[ "$got" != "$1" ]]; then
            echo "❌ self-test: expected match=$1 for: $2"
            fail=1
        fi
    }
    check 1 "at bt@user@Hpack>>class_encode:headers:table: (hpack.bt:40)"
    check 1 "Undefined function: 'bt@catalina':'class_start:'/2"
    check 1 "at Ticker>>dispatch/4"
    check 1 "at safe_dispatch"
    check 1 "at -collect:/2-fun-0-"
    check 0 "at Hpack class>>encode:headers:table: (hpack.bt:40)"
    check 0 "at [] in Counter>>increment (counter.bt:14)"
    check 0 "at lists:foldl/3 (lists.erl:2471)"
    check 0 "Counter>>dispatch (counter.bt)"
    check 0 "the class_side flag"
    # The allowlist must not rot: every entry exists and still has a name.
    for p in "${ALLOWED[@]}"; do
        if [[ ! -f "$p" ]]; then
            echo "❌ allowlisted file '$p' no longer exists; remove it from $ALLOWLIST_FILE"
            fail=1
        elif ! git grep -qE "$ALT" -- "$p"; then
            echo "❌ allowlisted file '$p' no longer contains a compiler-made name; remove it from $ALLOWLIST_FILE"
            fail=1
        fi
    done
    [[ "$fail" == 0 ]] && echo "✅ lint-compiler-made-names self-test passed"
    exit "$fail"
fi

hits="$(filter_allowed "$(scan .)")"
if [[ -n "$hits" ]]; then
    echo "❌ Compiler-made names (bt@, class_ function names, dispatch/4, safe_dispatch, -fun-N-) in user-facing output (ADR 0139, BT-4822):"
    echo "$hits"
    echo
    echo "Reword to the display name (Class>>selector / Class class>>selector), or, for a raw-mode"
    echo "fixture, add the file to $ALLOWLIST_FILE with a reason."
    exit 1
fi
echo "✅ No compiler-made names in user-facing output"

#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Guard (BT-4408): every `--skip <name>` in the Justfile's `coverage_rust_skip`
# list must still match a Rust function in the workspace. The coverage recipe
# runs the `#[ignore]` tests (`-- --ignored`) and keeps the live-release and
# PLT-building ones out by name. libtest's `--skip` is a silent substring
# filter, so renaming one of those tests without updating the list re-enables
# it on main's coverage job (BT-4398 renamed a live_front test and turned
# coverage-rust red). This lint makes a stale name fail on the PR instead.
#
# A skip matches when some `fn <ident>` in a tracked `crates/**/*.rs` file has
# the skip name as a substring of <ident> — the same substring rule libtest
# applies to test names (a prefix such as `adr0131_measure_` covers a family).
#
# To clear a failure: update the skip name in the Justfile to the test's new
# name, or drop it if the test was deleted.
#
# Usage: scripts/ci/lint-coverage-skips.sh [--self-test]

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Print the skip names from a Justfile's `coverage_rust_skip := "…"` block.
skip_names() {
    awk '
        /^coverage_rust_skip :=/ { in_block = 1 }
        in_block {
            line = $0
            while (match(line, /--skip [^ \\"]+/)) {
                print substr(line, RSTART + 7, RLENGTH - 7)
                line = substr(line, RSTART + RLENGTH)
            }
            # The block ends at the first line with no trailing backslash.
            if ($0 !~ /\\[[:space:]]*$/) { in_block = 0 }
        }
    ' "$1"
}

# check JUSTFILE FN_LIST_FILE → prints stale names, exit 1 if any
check() {
    local justfile="$1" fns="$2" stale=0 name
    local names
    names="$(skip_names "$justfile")"
    if [[ -z "$names" ]]; then
        echo "lint-coverage-skips: found no --skip names in $justfile (parser out of date?)" >&2
        return 1
    fi
    while IFS= read -r name; do
        if ! grep -qF -- "$name" "$fns"; then
            echo "  --skip $name"
            stale=1
        fi
    done <<<"$names"
    return "$stale"
}

if [[ "${1:-}" == "--self-test" ]]; then
    tmp="$(mktemp -d)"
    trap 'rm -rf "$tmp"' EXIT
    cat >"$tmp/Justfile" <<'EOF'
coverage_rust_skip := "--skip live_test_renamed \
    --skip family_prefix_ \
    --skip present_test"
EOF
    printf '%s\n' present_test family_prefix_one other >"$tmp/fns"
    out="$(check "$tmp/Justfile" "$tmp/fns" || true)"
    if [[ "$out" != "  --skip live_test_renamed" ]]; then
        echo "lint-coverage-skips self-test FAILED: expected only live_test_renamed to be stale, got:" >&2
        echo "$out" >&2
        exit 1
    fi
    echo "lint-coverage-skips self-test passed"
    exit 0
fi

fns_file="$(mktemp)"
trap 'rm -f "$fns_file"' EXIT
git ls-files -z -- 'crates/**/*.rs' | xargs -0 grep -hoE '\bfn[[:space:]]+[A-Za-z_][A-Za-z0-9_]*' \
    | awk '{ print $2 }' | sort -u >"$fns_file"

if ! stale="$(check Justfile "$fns_file")"; then
    echo "lint-coverage-skips: these coverage_rust_skip names in Justfile match no Rust function:" >&2
    echo "$stale" >&2
    echo "A renamed #[ignore] test would now run in coverage-rust. Update the name in the Justfile." >&2
    exit 1
fi
echo "lint-coverage-skips: all coverage_rust_skip names match a test"

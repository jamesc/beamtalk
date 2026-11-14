#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Summarise a failed `rebar3 eunit` run (BT-4427).
#
# Usage: eunit-failure-summary.sh <label> < eunit-output
#
# Reads the captured EUnit output on stdin and writes, to stderr, a short
# summary meant to be printed *after* the full output: the
# `N tests, N failures, N cancelled` line and the names of every failed
# (`Failures:`) and cancelled (`Pending:`) test. Under GitHub Actions it also
# emits one `::error::` annotation per named test, so a failure is visible at
# the bottom of the log and in the job summary even when the counts line says
# `0 failures` and the only casualty is a cancelled (timed-out) test.
#
# Always exits 0 — the caller owns the exit status.

set -uo pipefail

label="${1:-EUnit}"

# Strip ANSI colour codes; rebar3 colours its report.
# Also strip CR: the Windows recipe feeds PowerShell `Out-String` output (CRLF).
output="$(sed -e 's/\x1b\[[0-9;]*m//g' -e 's/\r$//')"

# Test-name lines sit at a two-space indent directly under the `Failures:` /
# `Pending:` headings ("  1) mod:fun/0: desc" and "  mod:fun/0: desc");
# detail lines are indented further. A block ends at the next unindented line.
extract_block() {
    local heading="$1"
    printf '%s\n' "$output" | awk -v h="$heading" '
        $0 == h { inblock = 1; next }
        inblock && /^[^ ]/ { inblock = 0 }
        inblock && /^  [^ ]/ { sub(/^  ([0-9]+\) )?/, ""); print }
    '
}

failed="$(extract_block 'Failures:')"
cancelled="$(extract_block 'Pending:')"
counts="$(printf '%s\n' "$output" | grep -E '^[0-9]+ tests, ' | tail -1)"

annotate() {
    local kind="$1" name="$2"
    if [ -n "${GITHUB_ACTIONS:-}" ]; then
        # Workflow-command data must escape `%` (and CR/LF, which a name
        # extracted line-by-line cannot contain).
        printf '::error title=%s %s::%s\n' "$label" "$kind" "${name//%/%25}" >&2
    fi
}

{
    echo ""
    echo "──────── ${label}: failure summary ────────"
    echo "${counts:-(no EUnit summary line found — see the output above for a compile or setup error)}"
    if [ -n "$failed" ]; then
        echo "Failed:"
        printf '%s\n' "$failed" | sed 's/^/  /'
    fi
    if [ -n "$cancelled" ]; then
        echo "Cancelled (timed out, or skipped after a setup or sibling failure):"
        printf '%s\n' "$cancelled" | sed 's/^/  /'
    fi
} >&2

while IFS= read -r name; do
    [ -n "$name" ] && annotate "failed" "$name"
done <<< "$failed"
while IFS= read -r name; do
    [ -n "$name" ] && annotate "cancelled" "$name"
done <<< "$cancelled"
if [ -z "$failed" ] && [ -z "$cancelled" ] && [ -n "${GITHUB_ACTIONS:-}" ]; then
    printf '::error title=%s failed::%s\n' "$label" "${counts:-rebar3 eunit failed before reporting a result}" >&2
fi

exit 0

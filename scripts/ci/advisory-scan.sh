#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Advisory scan for the repository's own dependencies (BT-5087): cargo-audit
# on each committed Cargo.lock, `npm audit --omit=dev` on each npm project and
# `mix deps.audit` on the LiveView project. Writes a markdown report to
# $1 (default advisory-report.md) and exits 0 whether or not it found
# anything -- this is an advisory job, never a gate. A scanner that itself
# fails to run is reported as a finding, not swallowed. Prints
# `findings=true|false` to $GITHUB_OUTPUT when that is set.
set -uo pipefail

REPORT="${1:-advisory-report.md}"
: > "${REPORT}"
FOUND=0

run_scan() { # title, dir, command...
    local title="$1" dir="$2"; shift 2
    local out rc
    out="$(cd "${dir}" && "$@" 2>&1)"
    rc=$?
    if [ "${rc}" -ne 0 ]; then
        FOUND=1
        {
            echo "### ${title}"
            echo
            echo '```'
            # Keep the issue body well under GitHub's size limit.
            printf '%s\n' "${out}" | head -c 6000
            echo
            echo '```'
            echo
        } >> "${REPORT}"
    fi
    echo "${title}: exit ${rc}"
}

run_scan "cargo audit (Cargo.lock)" . cargo audit --file Cargo.lock
run_scan "cargo audit (desktop/src-tauri/Cargo.lock)" . cargo audit --file desktop/src-tauri/Cargo.lock
for d in editors/vscode editors/github-chrome editors/liveview/assets; do
    run_scan "npm audit --omit=dev (${d})" "${d}" npm audit --omit=dev
done
run_scan "mix deps.audit (editors/liveview)" editors/liveview mix deps.audit

if [ -n "${GITHUB_OUTPUT:-}" ]; then
    if [ "${FOUND}" -eq 1 ]; then echo "findings=true" >> "${GITHUB_OUTPUT}"; else echo "findings=false" >> "${GITHUB_OUTPUT}"; fi
fi
echo "findings: ${FOUND}"
exit 0

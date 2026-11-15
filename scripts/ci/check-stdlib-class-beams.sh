#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Guard (BT-4925): the stdlib build's output directory holds at least one
# Beamtalk-compiled class beam (`bt@*.beam`).
#
# Used by `just test-install` before it cross-checks the installed
# beamtalk_stdlib.app against the beams: a count of 0 means the stdlib build
# no longer writes where scripts/install-otp-apps.sh reads.
#
# The existence test is `find -print -quit`, NOT `find | grep -q .`. Under
# `set -o pipefail`, grep -q exits on the first match; if find still has output
# to write it gets SIGPIPE (status 141) and the pipeline fails, so a populated
# directory read as empty (main run 43747689341: 125 beams, ~3% of runs).
#
# Usage: scripts/ci/check-stdlib-class-beams.sh [ebin-dir]
#   ebin-dir defaults to runtime/apps/beamtalk_stdlib/ebin (repo-root relative).

set -euo pipefail

EBIN_DIR="${1:-runtime/apps/beamtalk_stdlib/ebin}"

if [[ -n "$(find "$EBIN_DIR" -maxdepth 1 -name 'bt@*.beam' -print -quit 2>/dev/null)" ]]; then
    exit 0
fi

# A real miss: say what is there, so the next occurrence is diagnosable.
{
    echo "❌ No bt@*.beam in ${EBIN_DIR} after build-stdlib — has its output dir moved?"
    if [[ -d "$EBIN_DIR" ]]; then
        echo "   ${EBIN_DIR} exists: $(find "$EBIN_DIR" -maxdepth 1 -type f -name '*.beam' | wc -l | tr -d ' ') .beam file(s), $(find "$EBIN_DIR" -maxdepth 1 -type f -name '*.app' | wc -l | tr -d ' ') .app file(s)"
        echo "   contents (first 20):"
        find "$EBIN_DIR" -maxdepth 1 -mindepth 1 | sort | head -n 20 | sed 's/^/     /' || true
    else
        echo "   ${EBIN_DIR} does not exist (cwd: $(pwd))"
    fi
} >&2
exit 1

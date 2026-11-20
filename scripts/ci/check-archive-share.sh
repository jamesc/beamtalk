#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Assert an extracted release archive (or install prefix) carries the share/
# data `scripts/install-share.sh` installs: the MCP example corpus and the
# stdlib sources the LSP navigates into (BT-5077). Run on the Windows release
# and nightly archives after packaging; also usable against any install prefix.
#
# Usage: scripts/ci/check-archive-share.sh <extracted-root>

set -euo pipefail

ROOT="${1:?Usage: check-archive-share.sh <extracted-root>}"

fail() {
    echo "❌ $1"
    exit 1
}

test -s "${ROOT}/share/beamtalk/corpus.json" ||
    fail "share/beamtalk/corpus.json missing or empty in ${ROOT}"
test -d "${ROOT}/share/beamtalk/stdlib/src" ||
    fail "share/beamtalk/stdlib/src missing in ${ROOT}"

# Every stdlib source in the repo must be in the archive.
expected=$(find stdlib/src -type f -name '*.bt' | wc -l)
actual=$(find "${ROOT}/share/beamtalk/stdlib/src" -type f -name '*.bt' | wc -l)
[ "${expected}" -gt 0 ] || fail "no stdlib/src/*.bt in the repo checkout"
[ "${actual}" -eq "${expected}" ] ||
    fail "expected ${expected} stdlib sources in the archive, found ${actual}"

echo "✅ share/ data present: corpus.json and ${actual} stdlib sources"

#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Package a LiveView IDE (bt_attach) release archive with SHA-256 checksum.
#
# The IDE ships on its own release lane (BT-2512), separate from the core
# beamtalk-<version> toolchain bundle (scripts/ci/package-release.sh). This
# packages the self-contained, ERTS-embedded `mix release` produced by
# `just dist-liveview` (BT-2513) into a tarball a user can extract and run with
# `bin/bt_attach start` — no Elixir/Mix required on the host.
#
# Usage: scripts/ci/package-liveview-release.sh <version> <platform>
#
# Platform should be one of: linux-x86_64, macos-x86_64, macos-arm64.
# (Windows is out of scope — the remote IDE host is Linux/macOS per ADR 0091.)
#
# Expects the prod release at dist-liveview/ at the repo root (run
# `just dist-liveview` first).
#
# Outputs (to $GITHUB_OUTPUT if set, else stdout):
#   archive=beamtalk-ide-0.4.0-linux-x86_64.tar.gz

set -euo pipefail

VERSION="${1:?Usage: package-liveview-release.sh <version> <platform>}"
PLATFORM="${2:?Usage: package-liveview-release.sh <version> <platform>}"

REL_DIR="dist-liveview"
if [ ! -d "${REL_DIR}" ] || [ ! -x "${REL_DIR}/bin/bt_attach" ]; then
    echo "❌ Release not found at ${REL_DIR}. Run 'just dist-liveview' first."
    exit 1
fi

ARCHIVE="beamtalk-ide-${VERSION}-${PLATFORM}.tar.gz"
TOPLEVEL="beamtalk-ide-${VERSION}"

rm -rf "${TOPLEVEL}"
# Dereference symlinks (-L on cp): the release bundles the ERTS, which the
# archive must carry as real files so it is self-contained on extraction.
cp -RL "${REL_DIR}" "${TOPLEVEL}"
tar -czf "${ARCHIVE}" "${TOPLEVEL}"
rm -rf "${TOPLEVEL}"

# Generate SHA-256 checksum (cross-platform: macOS uses shasum).
if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "${ARCHIVE}" > "${ARCHIVE}.sha256"
else
    shasum -a 256 "${ARCHIVE}" > "${ARCHIVE}.sha256"
fi

if [ -n "${GITHUB_OUTPUT:-}" ]; then
    echo "archive=${ARCHIVE}" >> "${GITHUB_OUTPUT}"
else
    echo "archive=${ARCHIVE}"
fi

echo "📦 Created ${ARCHIVE} ($(du -h "${ARCHIVE}" | cut -f1))"
echo "🔒 Checksum: $(cat "${ARCHIVE}.sha256")"

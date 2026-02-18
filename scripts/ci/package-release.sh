#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Package a beamtalk release archive with SHA-256 checksum.
# Works on Linux, macOS, and Windows (Git Bash / MSYS2).
#
# Usage: scripts/ci/package-release.sh <version> <platform>
#
# Platform should be one of: linux-x86_64, macos-x86_64, macos-arm64, windows-x86_64
#
# Expects either:
#   - dist/ directory (from `just dist` or `just install dist`) for Unix
#   - Build artifacts in target/release/ and runtime/ for Windows
#
# Outputs (to $GITHUB_OUTPUT if set, else stdout):
#   archive=beamtalk-0.1.0-linux-x86_64.tar.gz

set -euo pipefail

VERSION="${1:?Usage: package-release.sh <version> <platform>}"
PLATFORM="${2:?Usage: package-release.sh <version> <platform>}"

BINARIES=(beamtalk beamtalk-compiler-port beamtalk-lsp beamtalk-mcp)
OTP_APPS=(beamtalk_runtime beamtalk_workspace beamtalk_compiler jsx cowboy cowlib ranch)

if [ "${PLATFORM}" = "windows-x86_64" ]; then
    ARCHIVE="beamtalk-${VERSION}-${PLATFORM}.zip"
    STAGING="beamtalk-staging"
    rm -rf "${STAGING}"
    mkdir -p "${STAGING}/bin" "${STAGING}/lib/beamtalk/lib"

    # Binaries
    for bin in "${BINARIES[@]}"; do
        cp "target/release/${bin}.exe" "${STAGING}/bin/"
    done

    # OTP application ebin directories
    for app in "${OTP_APPS[@]}"; do
        SRC="runtime/_build/default/lib/${app}/ebin"
        if [ ! -d "${SRC}" ] || ! ls "${SRC}"/*.beam 1>/dev/null 2>&1; then
            echo "❌ No .beam files found in ${SRC}. Erlang build may have failed."
            exit 1
        fi
        mkdir -p "${STAGING}/lib/beamtalk/lib/${app}/ebin"
        cp "${SRC}"/*.beam "${STAGING}/lib/beamtalk/lib/${app}/ebin/"
        cp "${SRC}"/*.app "${STAGING}/lib/beamtalk/lib/${app}/ebin/" 2>/dev/null || true
    done

    # Stdlib
    STDLIB_SRC="runtime/apps/beamtalk_stdlib/ebin"
    if [ ! -d "${STDLIB_SRC}" ] || ! ls "${STDLIB_SRC}"/*.beam 1>/dev/null 2>&1; then
        echo "❌ No stdlib .beam files found in ${STDLIB_SRC}. Stdlib build may have failed."
        exit 1
    fi
    mkdir -p "${STAGING}/lib/beamtalk/lib/beamtalk_stdlib/ebin"
    cp "${STDLIB_SRC}"/*.beam "${STAGING}/lib/beamtalk/lib/beamtalk_stdlib/ebin/"
    cp "${STDLIB_SRC}"/*.app "${STAGING}/lib/beamtalk/lib/beamtalk_stdlib/ebin/" 2>/dev/null || true

    # Create zip
    (cd "${STAGING}" && 7z a "../${ARCHIVE}" .)
    rm -rf "${STAGING}"
else
    # Unix: expect dist/ from `just dist` or `just install dist`
    if [ ! -d dist ] || [ ! -f dist/bin/beamtalk ]; then
        echo "❌ dist/ directory not found or incomplete. Run 'just dist' first."
        exit 1
    fi
    ARCHIVE="beamtalk-${VERSION}-${PLATFORM}.tar.gz"
    tar -czf "${ARCHIVE}" -C dist .
fi

# Generate SHA-256 checksum (cross-platform: macOS uses shasum)
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

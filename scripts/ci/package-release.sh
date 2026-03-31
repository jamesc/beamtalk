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

BINARIES=(beamtalk beamtalk-compiler-port beamtalk-lsp beamtalk-mcp beamtalk-exec)

if [ "${PLATFORM}" = "windows-x86_64" ]; then
    ARCHIVE="beamtalk-${VERSION}-${PLATFORM}.zip"
    TOPLEVEL="beamtalk-${VERSION}"
    STAGING="beamtalk-staging/${TOPLEVEL}"
    rm -rf "beamtalk-staging"
    mkdir -p "${STAGING}/bin" "${STAGING}/lib/beamtalk/lib"

    # Binaries
    for bin in "${BINARIES[@]}"; do
        cp "target/release/${bin}.exe" "${STAGING}/bin/"
    done

    # OTP application directories (discovered from rebar3 build output)
    OTP_APP_COUNT=0
    for ebin_dir in runtime/_build/default/lib/*/ebin; do
        app_dir="$(dirname "${ebin_dir}")"
        app="$(basename "${app_dir}")"
        if ! ls "${ebin_dir}"/*.beam 1>/dev/null 2>&1; then
            continue
        fi
        mkdir -p "${STAGING}/lib/beamtalk/lib/${app}/ebin"
        cp "${ebin_dir}"/*.beam "${STAGING}/lib/beamtalk/lib/${app}/ebin/"
        if ls "${ebin_dir}"/*.app 1>/dev/null 2>&1; then
            cp "${ebin_dir}"/*.app "${STAGING}/lib/beamtalk/lib/${app}/ebin/"
        fi
        # Copy priv/ directory if present (e.g. beamtalk_workspace browser UI)
        if [ -d "${app_dir}/priv" ]; then
            cp -rL "${app_dir}/priv" "${STAGING}/lib/beamtalk/lib/${app}/priv"
        fi
        OTP_APP_COUNT=$((OTP_APP_COUNT + 1))
    done
    if [ "${OTP_APP_COUNT}" -eq 0 ]; then
        echo "❌ No OTP apps found in runtime/_build/default/lib/. Run 'just build-erlang' first."
        exit 1
    fi

    # OTP application include directories (for native Erlang compilation)
    for app in beamtalk_runtime; do
        INC_SRC="runtime/apps/${app}/include"
        if [ ! -d "${INC_SRC}" ] || ! ls "${INC_SRC}"/*.hrl 1>/dev/null 2>&1; then
            echo "❌ No .hrl headers found in ${INC_SRC}. Run 'just build-erlang' first."
            exit 1
        fi
        mkdir -p "${STAGING}/lib/beamtalk/lib/${app}/include"
        cp "${INC_SRC}"/*.hrl "${STAGING}/lib/beamtalk/lib/${app}/include/"
    done

    # Create zip
    (cd "beamtalk-staging" && 7z a "../${ARCHIVE}" "${TOPLEVEL}")
    rm -rf "beamtalk-staging"
else
    # Unix: expect dist/ from `just dist` or `just install dist`
    if [ ! -d dist ] || [ ! -f dist/bin/beamtalk ]; then
        echo "❌ dist/ directory not found or incomplete. Run 'just dist' first."
        exit 1
    fi
    ARCHIVE="beamtalk-${VERSION}-${PLATFORM}.tar.gz"
    TOPLEVEL="beamtalk-${VERSION}"
    rm -rf "${TOPLEVEL}"
    cp -r dist "${TOPLEVEL}"
    tar -czf "${ARCHIVE}" "${TOPLEVEL}"
    rm -rf "${TOPLEVEL}"
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

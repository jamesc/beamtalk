#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Collect and checksum the installable artifacts `cargo tauri build`
# produced for the Beamtalk desktop picker (ADR 0097, BT-2986/BT-2987),
# renaming them to a version+platform-stable name the desktop-release.yml
# workflow can upload as CI artifacts / GitHub Release assets.
#
# Tauri's bundler writes into desktop/src-tauri/target/release/bundle/<kind>/
# with its own product-name-derived filenames; this script does not rebuild
# anything, it just gathers what's already there.
#
# Usage: scripts/ci/package-desktop-release.sh <version> <platform>
#   platform: linux-x86_64 | macos-arm64 | macos-x86_64 | windows-x86_64
#
# windows-x86_64 (BT-2988) runs on a GitHub-hosted `windows-latest` runner via
# `shell: bash`, which is Git Bash — `find`/`sha256sum`/etc. below all work
# there the same as on Linux/macOS; only the Tauri bundle-dir subpaths and
# artifact extensions differ per platform.
#
# Outputs (to $GITHUB_OUTPUT if set, else stdout):
#   archives=<newline-separated list of produced archive paths>
# One line per artifact; each has a sibling <archive>.sha256 checksum file.

set -euo pipefail

VERSION="${1:?Usage: package-desktop-release.sh <version> <platform>}"
PLATFORM="${2:?Usage: package-desktop-release.sh <version> <platform>}"

BUNDLE_DIR="desktop/src-tauri/target/release/bundle"
NAME="beamtalk-desktop-${VERSION}-${PLATFORM}"

if [ ! -d "${BUNDLE_DIR}" ]; then
    echo "❌ Bundle dir not found: ${BUNDLE_DIR}. Run 'cargo tauri build' from desktop/ first." >&2
    exit 1
fi

checksum() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" > "$1.sha256"
    else
        shasum -a 256 "$1" > "$1.sha256"
    fi
}

archives=()

case "${PLATFORM}" in
linux-x86_64)
    # .AppImage: portable, no install step. .deb: apt-installable — both are
    # produced by the same `cargo tauri build --bundles appimage,deb` (ADR
    # 0097 §5's "pick per Tauri's bundler support").
    appimage_src=$(find "${BUNDLE_DIR}/appimage" -maxdepth 1 -name '*.AppImage' | head -1 || true)
    deb_src=$(find "${BUNDLE_DIR}/deb" -maxdepth 1 -name '*.deb' | head -1 || true)

    if [ -n "${appimage_src}" ]; then
        dest="${NAME}.AppImage"
        cp "${appimage_src}" "${dest}"
        checksum "${dest}"
        archives+=("${dest}")
    fi
    if [ -n "${deb_src}" ]; then
        dest="${NAME}.deb"
        cp "${deb_src}" "${dest}"
        checksum "${dest}"
        archives+=("${dest}")
    fi
    ;;

macos-arm64 | macos-x86_64)
    # .dmg: the installable double-click artifact end users get. The .app is
    # also zipped separately (ditto, not `zip` — plain zip drops the code
    # signature's extended attributes / resource forks) so CI can smoke-test
    # or CD-verify the raw bundle without mounting a disk image.
    dmg_src=$(find "${BUNDLE_DIR}/dmg" -maxdepth 1 -name '*.dmg' | head -1 || true)
    app_src=$(find "${BUNDLE_DIR}/macos" -maxdepth 1 -name '*.app' | head -1 || true)

    if [ -n "${dmg_src}" ]; then
        dest="${NAME}.dmg"
        cp "${dmg_src}" "${dest}"
        checksum "${dest}"
        archives+=("${dest}")
    fi
    if [ -n "${app_src}" ]; then
        dest="${NAME}.app.zip"
        ditto -c -k --sequesterRsrc --keepParent "${app_src}" "${dest}"
        checksum "${dest}"
        archives+=("${dest}")
    fi
    ;;

windows-x86_64)
    # .msi (WiX) and .nsis (NSIS installer .exe) — Tauri's two Windows
    # bundle targets (`cargo tauri build --bundles msi,nsis`, ADR 0097 §5's
    # "pick per Tauri's bundler support", extended to Windows by BT-2988).
    # Not verified against a real `cargo tauri build` on Windows (no Windows
    # sandbox was available to develop this against) — the bundle subpaths
    # below follow Tauri v2's documented layout; confirm against a real CI
    # run before relying on this case rather than silently trusting it.
    msi_src=$(find "${BUNDLE_DIR}/msi" -maxdepth 1 -name '*.msi' | head -1 || true)
    nsis_src=$(find "${BUNDLE_DIR}/nsis" -maxdepth 1 -name '*.exe' | head -1 || true)

    if [ -n "${msi_src}" ]; then
        dest="${NAME}.msi"
        cp "${msi_src}" "${dest}"
        checksum "${dest}"
        archives+=("${dest}")
    fi
    if [ -n "${nsis_src}" ]; then
        dest="${NAME}-setup.exe"
        cp "${nsis_src}" "${dest}"
        checksum "${dest}"
        archives+=("${dest}")
    fi
    ;;

*)
    echo "❌ Unknown platform: ${PLATFORM} (expected linux-x86_64, macos-arm64, macos-x86_64, or windows-x86_64)" >&2
    exit 1
    ;;
esac

if [ "${#archives[@]}" -eq 0 ]; then
    echo "❌ No bundle artifacts found under ${BUNDLE_DIR} for platform ${PLATFORM}" >&2
    exit 1
fi

total_size=$(du -ch "${archives[@]}" 2>/dev/null | tail -1 | cut -f1)
echo "📦 Desktop app artifacts for ${PLATFORM} (${total_size} total):"
for a in "${archives[@]}"; do
    echo "   ${a} ($(du -h "${a}" | cut -f1))"
done

if [ -n "${GITHUB_OUTPUT:-}" ]; then
    {
        echo "archives<<DESKTOP_ARCHIVES_EOF"
        printf '%s\n' "${archives[@]}"
        echo "DESKTOP_ARCHIVES_EOF"
    } >> "${GITHUB_OUTPUT}"
else
    printf '%s\n' "${archives[@]}"
fi

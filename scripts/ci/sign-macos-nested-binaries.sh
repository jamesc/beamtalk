#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Code-sign every nested Mach-O binary (executable, dylib, or .so) inside a
# built bt_attach ERTS release, before it is copied into the desktop app's
# .app bundle as a Tauri resource (ADR 0097 Implementation §5c, BT-2987).
#
# Why this exists: Apple's notary service rejects a bundle containing ANY
# unsigned Mach-O file, but Tauri's own macOS bundler only reliably signs the
# top-level .app — arbitrary binaries copied in via `bundle.resources`
# (beam.smp, epmd, and any NIF .so/.dylib under the release's erts-*/bin and
# lib/*/priv) are not covered by that pass (see tauri-apps/tauri#11992, a
# reported notarization failure from exactly this "unsigned resource binary"
# shape). Signing them here, before `cargo tauri build` assembles the
# bundle, means they are already validly signed by the time Tauri's own pass
# (re-)signs the outer .app.
#
# Each file is signed independently (not `--deep`, which only has bundle-
# aware meaning for nested .app/.framework directories, neither of which
# appear in this flat ERTS layout), so there is no bottom-up ordering
# requirement here the way there would be for nested app bundles.
#
# Entitlements (desktop/src-tauri/beam-entitlements.plist) go on the BEAM VM
# executable (`beam*.smp`) only (BT-5194): allow-jit +
# allow-unsigned-executable-memory for BeamAsm's JIT-allocated executable
# memory, and disable-library-validation because NIFs are independently
# built/signed dynamic libraries loaded into beam.smp at runtime. Every other
# nested Mach-O (epmd, erlexec, NIF libraries, ...) is signed with the
# Hardened Runtime and no entitlements: none of them JIT or load NIFs, and
# entitlements only take effect on a process's main executable anyway.
#
# Usage: sign-macos-nested-binaries.sh <release-dir> <signing-identity>
#   release-dir:       path to the built dist-liveview/ release
#   signing-identity:  a "Developer ID Application: ..." identity already
#                      imported into the active keychain (APPLE_SIGNING_IDENTITY)
set -euo pipefail

RELEASE_DIR="${1:?Usage: sign-macos-nested-binaries.sh <release-dir> <signing-identity>}"
IDENTITY="${2:?Usage: sign-macos-nested-binaries.sh <release-dir> <signing-identity>}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENTITLEMENTS="${REPO_ROOT}/desktop/src-tauri/beam-entitlements.plist"

if [ ! -d "${RELEASE_DIR}" ]; then
    echo "❌ Release dir not found: ${RELEASE_DIR}" >&2
    exit 1
fi

if [ ! -f "${ENTITLEMENTS}" ]; then
    echo "❌ Entitlements file not found: ${ENTITLEMENTS}" >&2
    exit 1
fi

echo "🔏 Signing nested Mach-O binaries under ${RELEASE_DIR}"
echo "   Identity: ${IDENTITY}"

count=0
beam_count=0
while IFS= read -r -d '' f; do
    # `file -b` reports Mach-O executables, dylibs, and bundles; skip shell
    # scripts (bin/server, bin/bt_attach's launcher wrapper), text config,
    # and .beam bytecode (interpreted by beam.smp, not itself Mach-O).
    #
    # `find -L` (below) follows symlinks: mix releases are full of them
    # (see package-liveview-release.sh's own `cp -RL` comment) — without
    # `-L`, `-type f` would skip a symlinked Mach-O outright, and if Tauri's
    # resource copy later dereferences it into a real file in the .app
    # bundle, that copy would carry no signature at all.
    if file -b "${f}" | grep -q "Mach-O"; then
        rel="${f#"${RELEASE_DIR}"/}"
        case "$(basename "${f}")" in
            beam*.smp)
                echo "  signing (BEAM entitlements): ${rel}"
                codesign --force --timestamp --options runtime \
                    --entitlements "${ENTITLEMENTS}" \
                    --sign "${IDENTITY}" \
                    "${f}"
                beam_count=$((beam_count + 1))
                ;;
            *)
                echo "  signing: ${rel}"
                codesign --force --timestamp --options runtime \
                    --sign "${IDENTITY}" \
                    "${f}"
                ;;
        esac
        count=$((count + 1))
    fi
done < <(find -L "${RELEASE_DIR}" -type f -print0)

if [ "${count}" -eq 0 ]; then
    echo "⚠️  No Mach-O binaries found under ${RELEASE_DIR} — is the path correct?" >&2
    exit 1
fi

# The VM is the one binary that cannot run without the entitlements; a
# release where none matched `beam*.smp` would be signed, notarized, and then
# fail at first launch.
if [ "${beam_count}" -eq 0 ]; then
    echo "❌ No beam*.smp found under ${RELEASE_DIR}: the BEAM VM would be signed without its entitlements" >&2
    exit 1
fi

echo "✅ Signed ${count} nested Mach-O binaries (${beam_count} with BEAM entitlements)"

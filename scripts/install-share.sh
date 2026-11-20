#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Install the platform-independent `share/` data into an installed/packaged layout.
#
# Usage (from the repo root): scripts/install-share.sh <dest-share-dir>
#
# <dest-share-dir> is the installed `share/` directory, i.e. `PREFIX/share`.
# Installs, relative to it:
#   beamtalk/stdlib/src/**.bt   stdlib sources, for LSP go-to-definition
#   beamtalk/stubs/**.bt        curated FFI type stubs (ADR 0075 layer 3)
#   beamtalk/corpus.json (...)  everything under target/share/ — the MCP example
#                               corpus (BT-4614), staged by `just build-corpus`
#
# The one implementation shared by `just install` (and so `just dist` /
# `just test-install`), the Windows `just dist` recipe and
# scripts/ci/package-release.sh's Windows branch, so the Unix and Windows
# archives cannot drift apart (BT-5077). Same shape as install-otp-apps.sh.

set -euo pipefail

DEST="${1:?Usage: install-share.sh <dest-share-dir>}"

# Mirror a source tree of `.bt` files (including subdirectories) so LSP
# goto-definition resolves to the same relative layout as the repo.
install_bt_tree() {
    local src="$1" dest="$2"
    [ -d "${src}" ] || return 0
    find "${src}" -type f -name '*.bt' | while read -r bt; do
        rel="${bt#"${src}"/}"
        install -d "${dest}/$(dirname "${rel}")"
        install -m 644 "${bt}" "${dest}/${rel}"
    done
}

install_bt_tree "stdlib/src" "${DEST}/beamtalk/stdlib/src"
# A no-op until curated stub content exists (BT-1848).
install_bt_tree "stubs" "${DEST}/beamtalk/stubs"

# Generated share/ data: build-corpus stages it under target/ in the installed
# layout, so the tree is copied as is and its file names are defined in one
# place (the Rust side).
if [ ! -s "target/share/beamtalk/corpus.json" ]; then
    echo "❌ target/share/beamtalk/corpus.json not found. Run 'just build-corpus' first."
    exit 1
fi
install -d "${DEST}"
cp -R target/share/. "${DEST}/"

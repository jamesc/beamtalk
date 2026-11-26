#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Check that a GitHub release carries a channel's assets, each matching its
# `.sha256` sidecar (BT-5313). The publish half of liveview-release.yml (IDE
# archives) and desktop-release.yml (installers) runs this: those assets are
# already attached to the pre-release by build-and-attach, and they reach users
# when promotion marks the release latest, so publishing them means proving the
# attached bytes are whole before that happens.
#
# Usage: scripts/ci/verify-release-assets.sh <tag> <name-prefix> [min-count]
#   tag          the release tag (e.g. v0.6.0)
#   name-prefix  asset name prefix (e.g. beamtalk-ide-0.6.0-)
#   min-count    fewest archives (non-sidecar assets) expected; default 1
#
# Needs: gh authenticated (GH_TOKEN, GH_REPO or a checkout), sha256sum or shasum.

set -euo pipefail

TAG="${1:?Usage: verify-release-assets.sh <tag> <name-prefix> [min-count]}"
PREFIX="${2:?Usage: verify-release-assets.sh <tag> <name-prefix> [min-count]}"
MIN="${3:-1}"

hash_of() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | cut -d' ' -f1
    else
        shasum -a 256 "$1" | cut -d' ' -f1
    fi
}

dir="$(mktemp -d)"
trap 'rm -rf "${dir}"' EXIT
# No match is not an error here: the count check below reports it with context.
gh release download "${TAG}" --pattern "${PREFIX}*" --dir "${dir}" >&2 || true

shopt -s nullglob
archives=0
failures=0
for f in "${dir}"/*; do
    name="${f##*/}"
    case "${name}" in
        *.sha256)
            if [ ! -f "${f%.sha256}" ]; then
                echo "::error::${name} has no asset next to it on ${TAG}"
                failures=$((failures + 1))
            fi
            continue
            ;;
    esac
    archives=$((archives + 1))
    if [ ! -f "${f}.sha256" ]; then
        echo "::error::${name} has no .sha256 sidecar on ${TAG}"
        failures=$((failures + 1))
        continue
    fi
    want="$(cut -d' ' -f1 < "${f}.sha256")"
    got="$(hash_of "${f}")"
    if [ "${want}" != "${got}" ]; then
        echo "::error::${name}: sha256 ${got} does not match its sidecar (${want})"
        failures=$((failures + 1))
    else
        echo "ok: ${name} ${got}"
    fi
done

if [ "${archives}" -lt "${MIN}" ]; then
    echo "::error::${TAG} has ${archives} asset(s) named ${PREFIX}*, expected at least ${MIN}"
    exit 1
fi
[ "${failures}" -eq 0 ] || exit 1

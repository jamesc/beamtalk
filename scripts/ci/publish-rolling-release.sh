#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Publish a rolling pre-release (`edge`, `nightly`) without a window in which
# no release exists (BT-5094). The old workflows deleted the release and then
# created it, so a failed create left nothing for `install.sh --edge` /
# `--nightly` to download.
#
# Order when the release already exists: upload the new assets (replacing
# same-named ones), drop assets the new build no longer ships, move the tag to
# the new commit, then refresh title and notes. A failure at any step leaves the
# previous release in place. When it does not exist the tag is placed first and
# the release created from it.
#
# Usage: scripts/ci/publish-rolling-release.sh <tag> <sha> <title> <notes-file> <asset>...
# Needs: gh authenticated (GH_TOKEN), GITHUB_REPOSITORY, contents: write.

set -euo pipefail

if [ $# -lt 5 ]; then
    echo "Usage: $0 <tag> <sha> <title> <notes-file> <asset>..." >&2
    exit 2
fi
TAG="$1"
SHA="$2"
TITLE="$3"
NOTES_FILE="$4"
shift 4

for f in "$@"; do
    [ -f "$f" ] || { echo "error: asset not found: $f" >&2; exit 1; }
done

move_tag() {
    # PATCH moves an existing tag; POST creates it when there is none yet.
    if ! gh api -X PATCH "repos/${GITHUB_REPOSITORY}/git/refs/tags/${TAG}" \
        -f sha="${SHA}" -F force=true >/dev/null 2>&1; then
        gh api -X POST "repos/${GITHUB_REPOSITORY}/git/refs" \
            -f ref="refs/tags/${TAG}" -f sha="${SHA}" >/dev/null
    fi
}

if gh release view "${TAG}" >/dev/null 2>&1; then
    gh release upload "${TAG}" "$@" --clobber

    keep=""
    for f in "$@"; do keep+="$(basename "$f")"$'\n'; done
    while IFS= read -r name; do
        [ -n "${name}" ] || continue
        if ! grep -qxF -- "${name}" <<<"${keep}"; then
            gh release delete-asset "${TAG}" "${name}" --yes
        fi
    done < <(gh release view "${TAG}" --json assets --jq '.assets[].name')

    move_tag
    gh release edit "${TAG}" --title "${TITLE}" --notes-file "${NOTES_FILE}" --prerelease
else
    move_tag
    gh release create "${TAG}" --title "${TITLE}" --notes-file "${NOTES_FILE}" \
        --prerelease --target "${SHA}" "$@"
fi

#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Publish a rolling pre-release (`edge`, `nightly`) without a window in which
# no release exists (BT-5094). The old workflows deleted the release and then
# created it, so a failed create left nothing for `install.sh --edge` /
# `--nightly` to download.
#
# Order when the release already exists: upload the new assets (each archive
# followed by its checksum, see upload_ordered; replacing same-named ones), drop assets the new build no longer ships, move the tag to
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
    # PATCH moves an existing tag. Only a missing ref falls through to POST
    # (create); any other PATCH failure is reported as itself and stops, so the
    # real cause (permissions, rate limit, ...) is not replaced by a POST error
    # (BT-5101).
    local out
    if out="$(gh api -X PATCH "repos/${GITHUB_REPOSITORY}/git/refs/tags/${TAG}" \
        -f sha="${SHA}" -F force=true 2>&1)"; then
        return 0
    fi
    if grep -qi 'reference does not exist' <<<"${out}"; then
        gh api -X POST "repos/${GITHUB_REPOSITORY}/git/refs" \
            -f ref="refs/tags/${TAG}" -f sha="${SHA}" >/dev/null
    else
        echo "error: could not move tag ${TAG} to ${SHA}: ${out}" >&2
        return 1
    fi
}

# Upload order when replacing assets (BT-5101). install.sh requires the
# .sha256 to match the archive, so a client must not see a new archive next to
# an old checksum for longer than one small upload. Each archive is therefore
# uploaded alone, immediately followed by its own <archive>.sha256, one call per
# file (a multi-file `gh release upload` uploads concurrently, so its argument
# order guarantees nothing). Uploading all archives first and all checksums
# last would only move the mismatch window, not shrink it. Any checksum whose
# archive is not in the list goes last. `--clobber` deletes the old asset
# before uploading the new one, so an archive can be briefly absent (a clear
# download error, not a mismatch); GitHub has no atomic asset replace or rename
# over an existing name.
upload_ordered() {
    local f done_list=$'\n'
    for f in "$@"; do
        [[ "${f}" == *.sha256 ]] && continue
        gh release upload "${TAG}" "${f}" --clobber
        done_list+="${f}"$'\n'
        if printf '%s\n' "$@" | grep -qxF -- "${f}.sha256"; then
            gh release upload "${TAG}" "${f}.sha256" --clobber
            done_list+="${f}.sha256"$'\n'
        fi
    done
    for f in "$@"; do
        grep -qxF -- "${f}" <<<"${done_list}" || gh release upload "${TAG}" "${f}" --clobber
    done
}

if gh release view "${TAG}" >/dev/null 2>&1; then
    upload_ordered "$@"

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

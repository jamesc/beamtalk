#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Build the `beamtalk-artifact` repository_dispatch client-payload that
# beamtalk-uat consumes (ADR 0154 § 3, BT-5306) and print it as JSON.
#
# Usage: scripts/ci/uat-dispatch-payload.sh <channel> <version> <tag> <sha>
#        scripts/ci/uat-dispatch-payload.sh --otp   (print just the otp object)
#   channel  edge | nightly | release
#   version  e.g. 0.6.0-edge+c6b28b3, 0.6.0
#   tag      the GitHub release tag whose assets are listed (edge, nightly, v0.6.0)
#   sha      the commit the artifacts were built from
#
# The OTP window comes from otp-support.toml. `assets` maps every asset
# currently attached to the release (archives, IDE archives, .vsix, ...) to its
# sha256; the `.sha256` sidecar files are skipped, they are not artifacts.
# GitHub reports each asset's digest; an asset without one is downloaded and
# hashed here so the payload never lacks a hash.
#
# Needs: gh authenticated (GH_TOKEN), GITHUB_REPOSITORY, jq, sha256sum.

set -euo pipefail

read_otp() {
    local toml="${OTP_SUPPORT_TOML:-otp-support.toml}" min max
    min="$(sed -n 's/^min-major[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' "${toml}")"
    max="$(sed -n 's/^max-major[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' "${toml}")"
    if [ -z "${min}" ] || [ -z "${max}" ]; then
        echo "error: could not read min-major/max-major from ${toml}" >&2
        return 1
    fi
    jq -n -c --argjson min "${min}" --argjson max "${max}" '{"min-major": $min, "max-major": $max}'
}
# Pinned to the Rust reader by crates/beamtalk-cli/tests/cli/cli_otp_matrix.rs.
if [ "${1:-}" = "--otp" ]; then
    read_otp
    exit
fi

CHANNEL="${1:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"
VERSION="${2:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"
TAG="${3:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"
SHA="${4:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"

case "${CHANNEL}" in
    edge | nightly | release) ;;
    *) echo "error: channel must be edge, nightly or release, got '${CHANNEL}'" >&2; exit 2 ;;
esac

otp="$(read_otp)"

# Resolve the release id, then list its assets (paginated, 100 per page): the
# release object itself embeds at most 30. `.sha256` sidecars are not artifacts.
release_id="$(gh api "repos/${GITHUB_REPOSITORY}/releases/tags/${TAG}" --jq '.id')"
rows="$(gh api "repos/${GITHUB_REPOSITORY}/releases/${release_id}/assets?per_page=100" --paginate \
    --jq '.[] | [.name, (.digest // "")] | @tsv' | awk -F'\t' '$1 !~ /\.sha256$/')"
if [ -z "${rows}" ]; then
    echo "error: release ${TAG} has no assets to dispatch" >&2
    exit 1
fi

assets='{}'
tmp="$(mktemp -d)"
trap 'rm -rf "${tmp}"' EXIT
while IFS=$'\t' read -r name digest; do
    hash="${digest#sha256:}"
    if ! [[ "${hash}" =~ ^[0-9a-f]{64}$ ]]; then
        gh release download "${TAG}" --pattern "${name}" --dir "${tmp}" --clobber >&2
        hash="$(sha256sum "${tmp}/${name}" | cut -d' ' -f1)"
        rm -f "${tmp}/${name}"
    fi
    assets="$(jq -c --arg n "${name}" --arg h "${hash}" '. + {($n): $h}' <<<"${assets}")"
done <<<"${rows}"

jq -n -c \
    --arg channel "${CHANNEL}" --arg version "${VERSION}" --arg tag "${TAG}" --arg sha "${SHA}" \
    --argjson otp "${otp}" --argjson assets "${assets}" \
    '{channel: $channel, version: $version, tag: $tag, sha: $sha,
      otp: $otp, assets: $assets}'

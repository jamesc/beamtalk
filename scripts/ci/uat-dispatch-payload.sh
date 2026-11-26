#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Build the `beamtalk-artifact` repository_dispatch client-payload that
# beamtalk-uat consumes (ADR 0154 § 3, BT-5306) and print it as JSON.
#
# Usage: scripts/ci/uat-dispatch-payload.sh <channel> <version> <tag> <sha>
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

CHANNEL="${1:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"
VERSION="${2:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"
TAG="${3:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"
SHA="${4:?Usage: uat-dispatch-payload.sh <channel> <version> <tag> <sha>}"

case "${CHANNEL}" in
    edge | nightly | release) ;;
    *) echo "error: channel must be edge, nightly or release, got '${CHANNEL}'" >&2; exit 2 ;;
esac

TOML="${OTP_SUPPORT_TOML:-otp-support.toml}"
min_major="$(sed -n 's/^min-major[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' "${TOML}")"
max_major="$(sed -n 's/^max-major[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' "${TOML}")"
if [ -z "${min_major}" ] || [ -z "${max_major}" ]; then
    echo "error: could not read min-major/max-major from ${TOML}" >&2
    exit 1
fi

rows="$(gh api "repos/${GITHUB_REPOSITORY}/releases/tags/${TAG}" --paginate \
    --jq '.assets[] | select(.name | endswith(".sha256") | not) | [.name, (.digest // "")] | @tsv')"
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
        gh release download "${TAG}" --pattern "${name}" --dir "${tmp}" --clobber
        hash="$(sha256sum "${tmp}/${name}" | cut -d' ' -f1)"
        rm -f "${tmp}/${name}"
    fi
    assets="$(jq -c --arg n "${name}" --arg h "${hash}" '. + {($n): $h}' <<<"${assets}")"
done <<<"${rows}"

jq -n -c \
    --arg channel "${CHANNEL}" --arg version "${VERSION}" --arg tag "${TAG}" --arg sha "${SHA}" \
    --argjson min "${min_major}" --argjson max "${max_major}" --argjson assets "${assets}" \
    '{channel: $channel, version: $version, tag: $tag, sha: $sha,
      otp: {"min-major": $min, "max-major": $max}, assets: $assets}'

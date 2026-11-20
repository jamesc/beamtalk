#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Determine the release version from CI context.
# Used by release.yml to avoid duplicating version logic across jobs.
#
# Usage: scripts/ci/determine-version.sh <event_name> [release_tag] [input_tag]
# Outputs (to $GITHUB_OUTPUT if set, else stdout):
#   tag=v0.1.0
#   version=0.1.0

set -euo pipefail

EVENT_NAME="${1:?Usage: determine-version.sh <event_name> [release_tag] [input_tag]}"
RELEASE_TAG="${2:-}"
INPUT_TAG="${3:-}"

if [ "${EVENT_NAME}" = "release" ] && [ -n "${RELEASE_TAG}" ]; then
    TAG="${RELEASE_TAG}"
elif [ -n "${INPUT_TAG}" ]; then
    TAG="${INPUT_TAG}"
else
    # Only release tags count: rolling tags such as `edge` and `nightly` are
    # reachable from main and would otherwise win (BT-5094).
    TAG=$(git describe --tags --abbrev=0 --match 'v[0-9]*' 2>/dev/null || echo "dev")
fi

# Reject anything that is not a version (`edge`, `nightly`, the `dev`
# fallback, a typo in the dispatch input) before it names release artifacts.
if ! [[ "${TAG}" =~ ^v?[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.-]+)?(\+[0-9A-Za-z.-]+)?$ ]]; then
    echo "error: '${TAG}' is not a release version (expected e.g. v0.5.0)" >&2
    exit 1
fi

VERSION="${TAG#v}"

if [ -n "${GITHUB_OUTPUT:-}" ]; then
    echo "tag=${TAG}" >> "${GITHUB_OUTPUT}"
    echo "version=${VERSION}" >> "${GITHUB_OUTPUT}"
else
    echo "tag=${TAG}"
    echo "version=${VERSION}"
fi

echo "📦 Building release: ${VERSION}"

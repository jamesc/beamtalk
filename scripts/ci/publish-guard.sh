#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Refuse a channel publish (VS Code Marketplace, the ghcr version/latest retag,
# the desktop and IDE-archive publish) that the release flow does not allow
# (ADR 0154 § 10, BT-5313).
#
# Publishing moves behind promotion: a tag push only builds and attaches to a
# pre-release, and the publish halves run from promote.yml once uat passes.
# Until promote.yml exists (BT-5315), publishing keeps today's behaviour so
# releases are not broken. The repository variable RELEASE_PUBLISH_MODE says
# which rule holds:
#
#   legacy (or unset)  any caller may publish, as before the split. release.yml's
#                      interim-publish jobs run on a tag push.
#   promote            only .github/workflows/promote.yml on main may publish.
#                      release.yml's interim-publish jobs are skipped.
#
# In a reusable workflow GITHUB_WORKFLOW_REF names the top-level caller, so a
# publish job calls this first to learn who started the run.
#
# Usage: scripts/ci/publish-guard.sh <what>   (e.g. "VS Code Marketplace")
# Env:   RELEASE_PUBLISH_MODE, GITHUB_WORKFLOW_REF, GITHUB_REPOSITORY

set -euo pipefail

WHAT="${1:?Usage: publish-guard.sh <what>}"
MODE="${RELEASE_PUBLISH_MODE:-legacy}"
CALLER="${GITHUB_WORKFLOW_REF:?GITHUB_WORKFLOW_REF is not set}"
REPO="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is not set}"
PROMOTE="${REPO}/.github/workflows/promote.yml@refs/heads/main"

case "${MODE}" in
    legacy)
        echo "::notice::${WHAT}: RELEASE_PUBLISH_MODE is legacy, so ${CALLER} may publish (set it to 'promote' once promote.yml lands, BT-5315)."
        ;;
    promote)
        if [ "${CALLER}" != "${PROMOTE}" ]; then
            echo "::error::${WHAT}: only ${PROMOTE} may publish (RELEASE_PUBLISH_MODE=promote); this run was started by ${CALLER}."
            exit 1
        fi
        echo "${WHAT}: publishing from ${CALLER}."
        ;;
    *)
        echo "::error::${WHAT}: RELEASE_PUBLISH_MODE must be 'legacy' or 'promote', got '${MODE}'."
        exit 1
        ;;
esac

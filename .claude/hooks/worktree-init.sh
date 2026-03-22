#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# SessionStart hook: when starting in a git worktree, check for an existing
# remote branch and pull latest if it exists.

set -euo pipefail

# --- Cloud environment bootstrap ---
# Run setup-cloud.sh on first session (marker file prevents re-runs).
# The setup script is idempotent so it's safe to run even if some tools exist.
MARKER="${HOME}/.beamtalk-cloud-setup-done"
if [[ ! -f "${MARKER}" ]]; then
  SETUP_SCRIPT="${CLAUDE_PROJECT_DIR:-${PWD}}/scripts/setup-cloud.sh"
  if [[ -f "${SETUP_SCRIPT}" ]]; then
    echo "First session — running cloud environment setup..."
    bash "${SETUP_SCRIPT}"
    touch "${MARKER}"
  else
    # Fallback: download from GitHub pinned to a known commit, verify integrity,
    # then execute. Trap ensures temp file is cleaned up even on failure.
    echo "First session — fetching and running cloud environment setup..."
    _SETUP_TMP="$(mktemp)"
    trap 'rm -f "${_SETUP_TMP}"' EXIT
    SETUP_URL="https://raw.githubusercontent.com/jamesc/beamtalk/5c22fc5f7116fbe3cf7c8d3c919e6682f68c83ab/scripts/setup-cloud.sh"
    SETUP_SHA256="717b717108c25e310f4232158316ba2eddd2ba05ee530a5c0b6bb1b0c4b06312"
    curl -fsSL "${SETUP_URL}" -o "${_SETUP_TMP}"
    echo "${SETUP_SHA256}  ${_SETUP_TMP}" | sha256sum -c -
    bash "${_SETUP_TMP}"
    rm -f "${_SETUP_TMP}"
    trap - EXIT
    touch "${MARKER}"
  fi

  # After first-time setup, run `just build` so rebar3 plugins (erlfmt) are
  # compiled and BEAM artifacts exist before any lint or test runs.
  if command -v just &>/dev/null; then
    echo "Running initial build after cloud setup..."
    just build 2>&1 || echo "Warning: initial build failed — some tools may not work until build succeeds."
  fi
fi

# Ensure the pre-push lint hook is active (local config, needs setting per-clone/worktree)
git config core.hooksPath .githooks 2>/dev/null || true

GIT_DIR_FILE="${PWD}/.git"

# Only act when we are inside a worktree (.git is a file, not a directory)
if [[ ! -f "${GIT_DIR_FILE}" ]]; then
  exit 0
fi

BRANCH=$(git branch --show-current 2>/dev/null || true)

if [[ -z "${BRANCH}" ]]; then
  exit 0
fi

# Check whether the branch exists on origin (anchor match to avoid substring hits)
if git ls-remote --heads origin "${BRANCH}" 2>/dev/null | grep -qF "refs/heads/${BRANCH}"; then
  if ! git fetch origin "${BRANCH}" 2>&1; then
    echo "Warning: failed to fetch '${BRANCH}' from origin — continuing without pull."
    exit 0
  fi
  BEHIND=$(git rev-list HEAD..origin/"${BRANCH}" --count 2>/dev/null || echo 0)
  if git pull --ff-only origin "${BRANCH}" 2>&1; then
    echo "Worktree branch '${BRANCH}' pulled from origin (was ${BEHIND} commit(s) behind)."
  else
    echo "Warning: branch '${BRANCH}' has diverged from origin (${BEHIND} commit(s) behind) — run 'git pull --rebase' to sync."
  fi
else
  echo "Worktree branch '${BRANCH}' has no remote counterpart on origin — nothing to pull."
fi

# --- Build in worktree to avoid stale BEAM artifacts ---
# CLAUDE.md: "After entering or creating a worktree, run `just build` before
# running tests. Stale BEAM artifacts from another build cause false failures."
if command -v just &>/dev/null; then
  echo "Building in worktree to avoid stale artifacts..."
  if just build 2>&1; then
    echo "Worktree build complete."
  else
    echo "Warning: worktree build failed — tests may see stale artifacts."
  fi
fi

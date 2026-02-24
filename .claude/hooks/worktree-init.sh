#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# SessionStart hook: when starting in a git worktree, check for an existing
# remote branch and pull latest if it exists.

set -euo pipefail

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

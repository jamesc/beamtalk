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
fi

# Ensure the pre-push lint hook is active (local config, needs setting per-clone/worktree)
git config core.hooksPath .githooks 2>/dev/null || true

# --- Hex bridge proxy for cloud environments ---
# Erlang's httpc can't negotiate TLS through egress proxies that do MITM
# interception. Start a local HTTP-to-HTTPS bridge so rebar3/hex can fetch
# packages. Only starts when HTTP_PROXY is set and the proxy uses auth.
if [[ -n "${HTTP_PROXY:-}" ]] && [[ "${HTTP_PROXY}" == *"@"* ]]; then
  HEX_BRIDGE_PORT="${HEX_BRIDGE_PORT:-18081}"
  HEX_BRIDGE_SCRIPT="${CLAUDE_PROJECT_DIR:-${PWD}}/scripts/hex-bridge-proxy.py"
  if [[ -f "${HEX_BRIDGE_SCRIPT}" ]] && ! lsof -i:"${HEX_BRIDGE_PORT}" &>/dev/null; then
    HEX_BRIDGE_PORT="${HEX_BRIDGE_PORT}" python3 "${HEX_BRIDGE_SCRIPT}" &>/dev/null &
    disown
    sleep 1
    # Configure rebar3 to use the bridge (project-local, not global)
    HEX_BRIDGE_REBAR="${CLAUDE_PROJECT_DIR:-${PWD}}/runtime/hex_bridge_rebar.config"
    cat > "${HEX_BRIDGE_REBAR}" << REBAR_CONF
{hex, [{repos, [
    #{name => <<"hexpm">>,
      repo_url => <<"http://127.0.0.1:${HEX_BRIDGE_PORT}">>,
      repo_verify_origin => false}
]}]}.
REBAR_CONF
    # Point rebar3 at the project-local override via REBAR_GLOBAL_CONFIG_DIR
    export REBAR_GLOBAL_CONFIG_DIR="${CLAUDE_PROJECT_DIR:-${PWD}}/runtime"
    echo "Started hex-bridge proxy on localhost:${HEX_BRIDGE_PORT}"
  fi
fi

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

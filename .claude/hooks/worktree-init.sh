#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# SessionStart hook: when starting in a git worktree, check for an existing
# remote branch and pull latest if it exists.

set -euo pipefail

# --- Cloud environment bootstrap ---
# Run setup-cloud.sh when the pinned toolchain isn't actually present. We gate on
# BOTH a marker AND the real presence of mise/Elixir/mix: a cloud base snapshot
# can carry the marker (setup "ran") yet still lack the BEAM toolchain if the
# snapshot was built before the mise install took. Trusting the marker alone left
# every session without elixir/mix (and on a stale system OTP). Re-running
# setup-cloud.sh is idempotent — it skips already-installed tools and only fills
# the gaps — and self-heals such snapshots; once the install persists into a new
# snapshot, later sessions short-circuit here. Setup failure is non-fatal so the
# rest of the hook (hex bridge, git sync) still runs.
MARKER="${HOME}/.beamtalk-cloud-setup-done"
_need_cloud_setup() {
  [[ -f "${MARKER}" ]] || return 0
  command -v mise >/dev/null 2>&1 || return 0
  command -v elixir >/dev/null 2>&1 || return 0
  command -v mix >/dev/null 2>&1 || return 0
  return 1
}
if _need_cloud_setup; then
  SETUP_SCRIPT="${CLAUDE_PROJECT_DIR:-${PWD}}/scripts/setup-cloud.sh"
  if [[ -f "${SETUP_SCRIPT}" ]]; then
    echo "Toolchain incomplete — running cloud environment setup..."
    if bash "${SETUP_SCRIPT}"; then
      touch "${MARKER}"
    else
      echo "Warning: cloud setup did not complete cleanly — continuing (re-runs next session)."
    fi
  else
    # Fallback: download from GitHub pinned to a known commit, verify integrity,
    # then execute. Trap ensures temp file is cleaned up even on failure.
    echo "Toolchain incomplete — fetching and running cloud environment setup..."
    _SETUP_TMP="$(mktemp)"
    trap 'rm -f "${_SETUP_TMP}"' EXIT
    SETUP_URL="https://raw.githubusercontent.com/jamesc/beamtalk/5c22fc5f7116fbe3cf7c8d3c919e6682f68c83ab/scripts/setup-cloud.sh"
    SETUP_SHA256="717b717108c25e310f4232158316ba2eddd2ba05ee530a5c0b6bb1b0c4b06312"
    curl -fsSL "${SETUP_URL}" -o "${_SETUP_TMP}"
    echo "${SETUP_SHA256}  ${_SETUP_TMP}" | sha256sum -c -
    if bash "${_SETUP_TMP}"; then
      touch "${MARKER}"
    else
      echo "Warning: cloud setup did not complete cleanly — continuing (re-runs next session)."
    fi
    rm -f "${_SETUP_TMP}"
    trap - EXIT
  fi
fi

# Ensure the pre-push lint hook is active (local config, needs setting per-clone/worktree)
git config core.hooksPath .githooks 2>/dev/null || true

# --- Hex bridge proxy for cloud environments ---
# Start a local HTTP-to-HTTPS bridge so rebar3/hex/mix can fetch packages.
# The bridge terminates TLS in Python and hands plaintext to the BEAM over
# loopback, because the BEAM's own HTTP client cannot reach the network here.
#
# ROOT CAUSE (verified 2026-06, OTP 27, cloud_default sandbox — do not "simplify"
# the bridge away without re-checking this):
#   It is NOT primarily a cert-trust problem. On OTP 27, `public_key:cacerts_get()`
#   already trusts the system-installed MITM root, so Erlang's TLS *verifies fine*
#   (no "Unknown CA"). The real blocker is that the egress proxy REJECTS the BEAM's
#   TLS client at the upstream-connect layer: `erl`/`httpc` gets Envoy 502/503
#   ("upstream connect error … connection termination") on EVERY https host, while
#   `curl`/Python/`git` succeed on the same URLs (likely TLS-fingerprint allowlisting).
#   So `curl works` is NOT evidence that rebar3/mix will work — only a fetch
#   *through this bridge* is. `scripts/cloud-doctor.sh` probes exactly that.
#   (Older note claimed httpc "rejects the MITM cert"; that rationale is obsolete.)
#
# Two trigger modes:
#   1. Authenticated upstream proxy (older sandbox)   — HTTP_PROXY contains creds
#   2. Transparent TLS interception (Claude Code cloud) — CLAUDE_CODE_REMOTE=true
#      In mode 2 there is no HTTP_PROXY; the bridge routes BEAM egress through
#      Python, which the proxy accepts where the BEAM's TLS client is refused.
#
# The rebar3 wrapper / Node --use-env-proxy bits are ONLY needed in mode 1
# (to stop httpc routing localhost through the proxy and to make Node fetch
# proxy-aware). In mode 2 they are no-ops, so they stay gated on HTTP_PROXY.
_HAS_AUTH_PROXY=0
if [[ -n "${HTTP_PROXY:-}" ]] && [[ "${HTTP_PROXY}" == *"@"* ]]; then
  _HAS_AUTH_PROXY=1
fi
_IN_CLOUD_SANDBOX=0
if [[ "${CLAUDE_CODE_REMOTE:-}" == "true" ]]; then
  _IN_CLOUD_SANDBOX=1
fi

if (( _HAS_AUTH_PROXY || _IN_CLOUD_SANDBOX )); then
  HEX_BRIDGE_PORT="${HEX_BRIDGE_PORT:-18081}"
  HEX_BRIDGE_SCRIPT="${CLAUDE_PROJECT_DIR:-${PWD}}/scripts/hex-bridge-proxy.py"
  # Start the bridge if not already running
  if [[ -f "${HEX_BRIDGE_SCRIPT}" ]] && ! lsof -i:"${HEX_BRIDGE_PORT}" &>/dev/null; then
    HEX_BRIDGE_PORT="${HEX_BRIDGE_PORT}" python3 "${HEX_BRIDGE_SCRIPT}" &>/dev/null &
    disown
    sleep 1
    echo "Started hex-bridge proxy on localhost:${HEX_BRIDGE_PORT}"
  fi

  # Tell rebar3 to fetch packages from the local bridge (not hex.pm directly)
  export HEX_CDN="http://127.0.0.1:${HEX_BRIDGE_PORT}"

  WRAPPER_DIR="${CLAUDE_PROJECT_DIR:-${PWD}}/.claude/bin"
  _EXTRA_EXPORT_LINES=""

  if (( _HAS_AUTH_PROXY )); then
    # Node.js built-in fetch (undici) doesn't respect HTTP_PROXY/HTTPS_PROXY by
    # default. --use-env-proxy (Node 22.21+) enables proxy-aware fetch, which
    # fixes tools like streamlinear-cli that use bare fetch() for API calls.
    # Guard: only set when Node supports the flag, and skip if already present.
    if command -v node >/dev/null 2>&1; then
      _NODE_MAJOR="$(node -p 'process.versions.node.split(".")[0]' 2>/dev/null || echo "")"
      if [[ "${_NODE_MAJOR}" =~ ^[0-9]+$ ]] && (( _NODE_MAJOR >= 22 )); then
        case " ${NODE_OPTIONS:-} " in
          *" --use-env-proxy "*) ;;
          *) export NODE_OPTIONS="${NODE_OPTIONS:+${NODE_OPTIONS} }--use-env-proxy" ;;
        esac
      fi
    fi

    # Install a rebar3 wrapper that strips proxy env vars before calling the real
    # rebar3. Erlang's httpc ignores no_proxy, so without this it routes even
    # localhost requests through the egress proxy (which returns 407).
    # Resolve the real rebar3 with the wrapper dir stripped from PATH to avoid
    # an infinite exec loop if the hook runs twice in the same session.
    CLEAN_PATH="${PATH//"${WRAPPER_DIR}":/}"    # Remove from start/middle
    CLEAN_PATH="${CLEAN_PATH%:"${WRAPPER_DIR}"}" # Remove from end
    REAL_REBAR3="$(PATH="${CLEAN_PATH}" command -v rebar3 || true)"
    if [[ -z "${REAL_REBAR3:-}" ]] || [[ ! -x "${REAL_REBAR3}" ]]; then
      echo "Warning: rebar3 not found on PATH; skipping hex bridge wrapper setup."
    else
      mkdir -p "${WRAPPER_DIR}"
      cat > "${WRAPPER_DIR}/rebar3" << WRAPPER
#!/usr/bin/env bash
export HEX_CDN="http://127.0.0.1:${HEX_BRIDGE_PORT}"
unset http_proxy https_proxy HTTP_PROXY HTTPS_PROXY
exec "${REAL_REBAR3}" "\$@"
WRAPPER
      chmod +x "${WRAPPER_DIR}/rebar3"
      export PATH="${WRAPPER_DIR}:${PATH}"
    fi

    # Persisted NODE_OPTIONS snippet: guarded by a runtime Node-version probe
    # so a future shell on a Node <22.21 (no --use-env-proxy support) doesn't
    # break with "bad option" on every node/npm invocation.
    _NODE_OPTS_BLOCK='if command -v node >/dev/null 2>&1; then
  _hb_node_major=$(node -p "process.versions.node.split(\".\")[0]" 2>/dev/null || echo 0)
  if [[ "${_hb_node_major}" =~ ^[0-9]+$ ]] && (( _hb_node_major >= 22 )); then
    case " ${NODE_OPTIONS:-} " in
      *" --use-env-proxy "*) ;;
      *) export NODE_OPTIONS="${NODE_OPTIONS:+${NODE_OPTIONS} }--use-env-proxy" ;;
    esac
  fi
  unset _hb_node_major
fi'
    _EXTRA_EXPORT_LINES="export PATH=\"${WRAPPER_DIR}:\${PATH}\"
${_NODE_OPTS_BLOCK}"
  fi

  # --- Persist proxy env vars to shell configs ---
  # HEX_CDN is always exported. PATH / Node-options block only when HTTP_PROXY
  # is set (mode 1) — they're no-ops in the cloud-sandbox-only mode.
  #
  # The block is delimited by START_MARKER / END_MARKER so we can replace it
  # atomically when upgrading from cloud-only to auth-proxy mode (or vice
  # versa). The previous sed-append backfill could only add the NODE_OPTIONS
  # line and never inserted the wrapper PATH, leaving httpc routing localhost
  # through the proxy after an upgrade.
  _MARKER="# hex-bridge-proxy PATH (auto-added by worktree-init.sh)"
  _END_MARKER="# hex-bridge-proxy END"
  # HEX_CDN routes rebar3's hex through the bridge; HEX_MIRROR does the same for
  # Mix/Hex (editors/liveview, BT-2401). ELIXIR_ERL_OPTIONS=+fnu keeps the
  # precompiled Elixir from tripping on a non-UTF-8 container locale.
  _EXPORT_BLOCK="export HEX_CDN=\"http://127.0.0.1:${HEX_BRIDGE_PORT}\"
export HEX_MIRROR=\"http://127.0.0.1:${HEX_BRIDGE_PORT}\"
export ELIXIR_ERL_OPTIONS=\"\${ELIXIR_ERL_OPTIONS:-+fnu}\""
  if [[ -n "${_EXTRA_EXPORT_LINES}" ]]; then
    _EXPORT_BLOCK="${_EXPORT_BLOCK}
${_EXTRA_EXPORT_LINES}"
  fi

  # Sentinel inside the block lets us cheaply detect mode mismatches without
  # diffing the whole content. "wrapper-path" appears only in auth-proxy mode.
  if (( _HAS_AUTH_PROXY )); then
    _MODE_SENTINEL="wrapper-path"
  else
    _MODE_SENTINEL="cdn-only"
  fi
  _FULL_BLOCK="${_MARKER} (${_MODE_SENTINEL})
${_EXPORT_BLOCK}
${_END_MARKER}"

  # _hb_install_block FILE PREPEND  — write/replace the marked block in FILE.
  # When PREPEND=1, a freshly-added block is inserted at the top (bashrc needs
  # this so it runs before the non-interactive guard). Otherwise appended.
  _hb_install_block() {
    local file="$1" prepend="$2"
    [[ -f "${file}" ]] || : > "${file}"
    if grep -qF "${_MARKER}" "${file}" 2>/dev/null; then
      # Already present — replace only if the mode sentinel doesn't match.
      if grep -qF "${_MARKER} (${_MODE_SENTINEL})" "${file}" 2>/dev/null; then
        return 0
      fi
      # Strip existing block (start through end marker) and re-insert.
      local tmp
      tmp="$(mktemp)"
      awk -v start="${_MARKER}" -v endm="${_END_MARKER}" '
        index($0, start) == 1 { skip = 1; next }
        skip && index($0, endm) == 1 { skip = 0; next }
        !skip { print }
      ' "${file}" > "${tmp}"
      if (( prepend )); then
        { printf '%s\n\n' "${_FULL_BLOCK}"; cat "${tmp}"; } > "${file}"
      else
        { cat "${tmp}"; printf '\n%s\n' "${_FULL_BLOCK}"; } > "${file}"
      fi
      rm -f "${tmp}"
      echo "Replaced hex-bridge block in ${file} (now: ${_MODE_SENTINEL})"
    else
      if (( prepend )); then
        local tmp
        tmp="$(mktemp)"
        { printf '%s\n\n' "${_FULL_BLOCK}"; cat "${file}"; } > "${tmp}"
        mv "${tmp}" "${file}"
        echo "Added hex-bridge block to ${file} (mode: ${_MODE_SENTINEL})"
      else
        printf '\n%s\n' "${_FULL_BLOCK}" >> "${file}"
        echo "Added hex-bridge block to ${file} (mode: ${_MODE_SENTINEL})"
      fi
    fi
  }

  # Method 1: /etc/profile.d/ (works for login shells, and `just`/`make`)
  # Always overwrite — this file is entirely ours. Best-effort (|| true).
  _PROFILED="/etc/profile.d/hex-bridge.sh"
  cat > "${_PROFILED}" 2>/dev/null << PROFBLOCK || true
${_FULL_BLOCK}
PROFBLOCK

  # Method 2: ~/.bashrc — insert BEFORE the non-interactive guard
  _hb_install_block "${HOME}/.bashrc" 1

  # Method 3: ~/.zshenv (always sourced by zsh, even non-interactive)
  if [[ "$(basename "${SHELL:-bash}")" == "zsh" ]]; then
    _hb_install_block "${HOME}/.zshenv" 0
  fi
fi

# --- Environment health summary (cloud/proxy sandboxes only) ---
# Print a one-line doctor verdict so a broken env (dead bridge, blocked BEAM
# egress, missing tool) is visible NOW instead of failing mid-build. Bounded
# by a timeout and fully best-effort — never blocks or fails the session.
if (( _HAS_AUTH_PROXY || _IN_CLOUD_SANDBOX )); then
  _DOCTOR="${CLAUDE_PROJECT_DIR:-${PWD}}/scripts/cloud-doctor.sh"
  if [[ -x "${_DOCTOR}" ]]; then
    timeout 25 bash "${_DOCTOR}" --summary 2>/dev/null || true
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

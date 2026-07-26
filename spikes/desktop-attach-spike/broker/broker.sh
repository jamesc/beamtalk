#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# BT-2984 spike: a throwaway stand-in for the desktop broker's process-broker
# responsibilities (ADR 0097 "Broker responsibilities"). This is NOT the
# shipped broker (BT-2985 builds that, in Rust, inside the chosen shell) — it
# exists only to exercise the real front (dist-liveview/bin/server) against
# real workspaces so the spike's exit criteria (a)-(g) can be validated against
# actual epmd/distribution/HTTP behaviour instead of asserted from reading code.
#
# Subcommands:
#   discover                       — list ~/.beamtalk/workspaces/*
#   attach <ws_id> [port]          — spawn a front for <ws_id>, two-stage probe
#   attach-bad-cookie <ws_id> [port]   — like attach, but with a corrupted cookie
#   attach-dead <fake_node> [port] — spawn a front pointed at a non-existent node
#   detach <ws_id_or_port>         — graceful stop (removes the pidfile)
#   sweep                          — orphan-reaping: kill+clear any pidfile'd
#                                     front still alive from a prior broker
#                                     lifetime (the mechanism prototyped for g)
#   status                         — list tracked fronts + liveness
#   readiness <port>               — GET /readiness, print JSON
#   wait-ready <port> [timeout_s]  — two-stage probe: HTTP up, then /readiness
#
# State lives under $STATE_DIR (default ~/.beamtalk/desktop-broker-spike/),
# deliberately separate from ~/.beamtalk/workspaces/ (owned by the Rust CLI).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
LIVEVIEW_SERVER="${REPO_ROOT}/dist-liveview/bin/server"
STATE_DIR="${STATE_DIR:-${HOME}/.beamtalk/desktop-broker-spike}"
WORKSPACES_DIR="${HOME}/.beamtalk/workspaces"

mkdir -p "${STATE_DIR}"

log() { echo "[broker] $*" >&2; }

free_port() {
  python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()'
}

ws_node_name() {
  python3 -c "import json,sys; print(json.load(open(sys.argv[1]))['node_name'])" \
    "${WORKSPACES_DIR}/$1/metadata.json"
}

discover() {
  if [ ! -d "${WORKSPACES_DIR}" ]; then
    log "no ~/.beamtalk/workspaces/ directory"
    return 0
  fi
  for d in "${WORKSPACES_DIR}"/*/; do
    id="$(basename "${d}")"
    meta="${d}metadata.json"
    [ -f "${meta}" ] || continue
    node="$(python3 -c "import json;print(json.load(open('${meta}')).get('node_name',''))" 2>/dev/null || true)"
    [ -n "${node}" ] || continue
    # Liveness via epmd, mirroring the ADR's discovery contract (Broker §1):
    # a *dist ping* isn't available to a non-BEAM broker, so a raw epmd NAMES
    # query is the liveness check (not the beamtalk CLI, to keep this script
    # standalone / not depend on `cargo run`).
    short="${node%@*}"
    if epmd -names 2>/dev/null | grep -q "name ${short} "; then
      echo "${id} ${node} alive"
    else
      echo "${id} ${node} dead"
    fi
  done
}

# Spawn a front. Positional: workspace id (used to look up node+cookie AND as
# the BT_ATTACH_NODE_SUFFIX seed — ADR 0097 Impl §1a), port, and two optional
# overrides for the negative-path tests ((c): bad cookie / dead workspace).
#
# Finding worth flagging up front: `bin/server <id>` — the exact invocation
# ADR 0097's Decision section specifies (`PORT=<free-port> bin/server <id>`) —
# UNCONDITIONALLY RE-RESOLVES BT_WORKSPACE_NODE/BT_WORKSPACE_COOKIE from
# ~/.beamtalk/workspaces/<id>/ when given a workspace-id argument (see
# `bin/server` lines ~40-49), silently overwriting any override passed via
# env. That is the right behaviour for the happy path (a broker never wants
# a stale cookie) but means the negative-path tests below (bad cookie / dead
# node) must invoke `bin/server` with NO positional argument — the script's
# own documented "remote-workspace topology" mode, where
# "BT_WORKSPACE_NODE / BT_WORKSPACE_COOKIE are taken from the environment
# as-is". `server_arg=""` selects that mode.
_spawn() {
  local ws_id="$1" port="$2" node_override="${3:-}" cookie_override="${4:-}" extra_env="${5:-}" server_arg="${6-${1}}"
  local node cookie
  if [ -n "${node_override}" ]; then
    node="${node_override}"
  else
    node="$(ws_node_name "${ws_id}")"
  fi
  if [ -n "${cookie_override}" ]; then
    cookie="${cookie_override}"
  else
    cookie="$(cat "${WORKSPACES_DIR}/${ws_id}/cookie")"
  fi

  local logfile="${STATE_DIR}/${ws_id}.${port}.log"
  local pidfile="${STATE_DIR}/${ws_id}.${port}.pid"
  local metafile="${STATE_DIR}/${ws_id}.${port}.meta"

  log "spawning front for '${ws_id}' → node=${node} port=${port}"

  # setsid: the spawned front becomes its own session/process-group leader,
  # NOT a child this shell's job control can wait on — the same relationship
  # a real desktop broker has to the OS processes it forks (they keep running
  # if the broker is SIGKILLed). This is what makes the sweep/orphan tests (g)
  # meaningful rather than trivially true.
  #
  # RELEASE_DISTRIBUTION=none is the fix for a real bug this spike found
  # (see README "Finding: RELEASE_NODE defaults collide"): `mix release`'s
  # generated launcher boots the VM ALREADY distributed under `-sname
  # bt_attach` (RELEASE_NODE defaults to RELEASE_NAME) before any Elixir code
  # runs, which pre-empts ensure_distributed/0's BT_ATTACH_NODE_SUFFIX seeding
  # entirely (Node.alive?/0 is already true) and makes every spawned instance
  # collide on the identical epmd registration. Booting non-distributed and
  # letting the front's own lazy ensure_distributed/0 assign the seeded name
  # on first /readiness call is what makes the ADR's sname-seeding mechanism
  # apply to the shipped release, not just the `just web` dev flow.
  #
  # shellcheck disable=SC2086
  if [ -n "${server_arg}" ]; then
    env \
      BT_WORKSPACE_NODE="${node}" \
      BT_WORKSPACE_COOKIE="${cookie}" \
      BT_ATTACH_BIND_IP="127.0.0.1" \
      BT_ATTACH_NODE_SUFFIX="${ws_id}" \
      RELEASE_DISTRIBUTION="none" \
      PORT="${port}" \
      PHX_SERVER="true" \
      MIX_ENV="prod" \
      SECRET_KEY_BASE="$(python3 -c 'import secrets; print(secrets.token_hex(32))')" \
      ${extra_env} \
      setsid "${LIVEVIEW_SERVER}" "${server_arg}" >"${logfile}" 2>&1 &
  else
    # No positional arg: bin/server's "environment as-is" mode — required so
    # BT_WORKSPACE_NODE/BT_WORKSPACE_COOKIE overrides actually reach the front
    # instead of being silently re-resolved from disk (see comment above).
    env \
      BT_WORKSPACE_NODE="${node}" \
      BT_WORKSPACE_COOKIE="${cookie}" \
      BT_ATTACH_BIND_IP="127.0.0.1" \
      BT_ATTACH_NODE_SUFFIX="${ws_id}" \
      RELEASE_DISTRIBUTION="none" \
      PORT="${port}" \
      PHX_SERVER="true" \
      MIX_ENV="prod" \
      SECRET_KEY_BASE="$(python3 -c 'import secrets; print(secrets.token_hex(32))')" \
      ${extra_env} \
      setsid "${LIVEVIEW_SERVER}" >"${logfile}" 2>&1 &
  fi
  disown
  local pid=$!
  echo "${pid}" >"${pidfile}"
  printf 'workspace=%s\nport=%s\nnode=%s\npid=%s\n' "${ws_id}" "${port}" "${node}" "${pid}" >"${metafile}"
  echo "${pid}"
}

attach() {
  local ws_id="$1" port="${2:-$(free_port)}"
  _spawn "${ws_id}" "${port}" >/dev/null
  echo "${port}"
}

attach_bad_cookie() {
  local ws_id="$1" port="${2:-$(free_port)}"
  _spawn "${ws_id}" "${port}" "" "0000000000000000000000000000000badc0de" "" "" >/dev/null
  echo "${port}"
}

attach_dead() {
  local fake_node="$1" port="${2:-$(free_port)}"
  # No real workspace backs this node; any cookie will do since epmd has no
  # record of the target at all (the `:dead_workspace` taxonomy bucket).
  _spawn "attach-dead" "${port}" "${fake_node}" "deadcookie00000000000000000000000000000" "" "" >/dev/null
  echo "${port}"
}

# Like attach, but pass through arbitrary extra env (e.g. BT_OIDC_* for the
# no-OIDC posture test, (d)).
attach_with_env() {
  local ws_id="$1" port="$2"
  shift 2
  _spawn "${ws_id}" "${port}" "" "" "$*" >/dev/null
  echo "${port}"
}

wait_http_up() {
  local port="$1" timeout="${2:-15}"
  local waited=0
  while ! curl -s -o /dev/null "http://127.0.0.1:${port}/" 2>/dev/null; do
    sleep 0.5
    waited=$((waited + 1))
    if [ "${waited}" -ge $((timeout * 2)) ]; then
      log "HTTP never came up on :${port}"
      return 1
    fi
  done
  return 0
}

readiness() {
  local port="$1"
  curl -s -w '\nHTTP_STATUS:%{http_code}\n' "http://127.0.0.1:${port}/readiness"
}

# Two-stage probe (ADR 0097 Broker §2): HTTP port up, THEN /readiness for true
# workspace reachability. Prints the final /readiness JSON body; exit code 0
# only on a 200.
wait_ready() {
  local port="$1" timeout="${2:-20}"
  wait_http_up "${port}" "${timeout}" || {
    echo '{"status":"http_never_up"}'
    return 1
  }
  local waited=0
  local body status
  while true; do
    local resp
    resp="$(curl -s -w '\n%{http_code}' "http://127.0.0.1:${port}/readiness")"
    status="$(echo "${resp}" | tail -1)"
    body="$(echo "${resp}" | sed '$d')"
    if [ "${status}" = "200" ]; then
      echo "${body}"
      return 0
    fi
    waited=$((waited + 1))
    if [ "${waited}" -ge $((timeout * 2)) ]; then
      echo "${body}"
      return 1
    fi
    sleep 0.5
  done
}

detach() {
  local key="$1"
  shopt -s nullglob
  for pf in "${STATE_DIR}"/*."${key}".pid "${STATE_DIR}/${key}".*.pid; do
    [ -f "${pf}" ] || continue
    local pid
    pid="$(cat "${pf}")"
    if kill -0 "${pid}" 2>/dev/null; then
      log "detach: stopping pid ${pid} (${pf})"
      kill -TERM "${pid}" 2>/dev/null || true
      sleep 0.3
      kill -0 "${pid}" 2>/dev/null && kill -KILL "${pid}" 2>/dev/null || true
    fi
    rm -f "${pf}" "${pf%.pid}.meta" "${pf%.pid}.log"
  done
  shopt -u nullglob
}

# Orphan-reaping (g): PID-file sweep, chosen over process-group kill / a
# parent-death-watch (prctl PR_SET_PDEATHSIG) because it is the only one of
# the three that is portable to Windows without OS-specific syscalls — the
# ADR's shell packaging (§5) targets macOS + Linux + Windows. On broker start,
# any pidfile pointing at a still-alive process is presumed orphaned: a full
# broker restart means the in-memory picker state (which fronts are attached)
# is gone, so there is no legitimate reason for a pre-existing front to still
# be tracked. Stale pidfiles (process already dead) are cleaned up too.
sweep() {
  local reaped=0
  shopt -s nullglob
  for pf in "${STATE_DIR}"/*.pid; do
    local pid
    pid="$(cat "${pf}")"
    if kill -0 "${pid}" 2>/dev/null; then
      log "sweep: reaping orphaned front pid=${pid} ($(basename "${pf}"))"
      kill -TERM "${pid}" 2>/dev/null || true
      sleep 0.3
      if kill -0 "${pid}" 2>/dev/null; then
        kill -KILL "${pid}" 2>/dev/null || true
      fi
      reaped=$((reaped + 1))
    else
      log "sweep: clearing stale pidfile $(basename "${pf}") (pid ${pid} already gone)"
    fi
    rm -f "${pf}" "${pf%.pid}.meta" "${pf%.pid}.log"
  done
  shopt -u nullglob
  log "sweep complete: reaped ${reaped} orphan(s)"
  echo "${reaped}"
}

status() {
  shopt -s nullglob
  for mf in "${STATE_DIR}"/*.meta; do
    local ws port node pid alive
    ws="$(sed -n 's/^workspace=//p' "${mf}")"
    port="$(sed -n 's/^port=//p' "${mf}")"
    node="$(sed -n 's/^node=//p' "${mf}")"
    pid="$(sed -n 's/^pid=//p' "${mf}")"
    alive="dead"
    kill -0 "${pid}" 2>/dev/null && alive="alive"
    echo "${ws} port=${port} node=${node} pid=${pid} ${alive}"
  done
  shopt -u nullglob
}

cmd="${1:-}"
shift || true
case "${cmd}" in
discover) discover ;;
attach) attach "$@" ;;
attach-bad-cookie) attach_bad_cookie "$@" ;;
attach-dead) attach_dead "$@" ;;
attach-with-env) attach_with_env "$@" ;;
detach) detach "$@" ;;
sweep) sweep ;;
status) status ;;
readiness) readiness "$@" ;;
wait-ready) wait_ready "$@" ;;
*)
  echo "usage: broker.sh <discover|attach|attach-bad-cookie|attach-dead|attach-with-env|detach|sweep|status|readiness|wait-ready> [args]" >&2
  exit 64
  ;;
esac

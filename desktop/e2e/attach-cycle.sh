#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# BT-2989 (ADR 0097 Phase 5): scripted E2E validation of the desktop attach
# client's core flow — `beamtalk workspace create` a real workspace, "launch
# the desktop app -> attach" (spawn a front with the exact env-var contract
# the broker uses, crates/beamtalk-desktop-broker/src/spawn.rs's `build_env`
# / module doc), confirm the window loads and an eval round-trips through
# the LiveView UI (desktop/e2e/eval-roundtrip.mjs, a real Chromium via
# Playwright), detach and confirm the front process exits, then a negative
# path: attach to a *dead* (stopped, but not deleted) workspace and confirm
# /readiness surfaces its failure taxonomy rather than hanging or crashing.
#
# See desktop/e2e/README.md for why this substitutes the broker's real spawn
# contract for an actual compiled `beamtalk-desktop` (Tauri) binary — no
# development sandbox to date, including the one this script was written in,
# has had a working Tauri/webview toolchain (desktop/README.md's "What was
# and wasn't verified").
#
# Prerequisites (not built by this script — see desktop/e2e/README.md):
#   just build                     # beamtalk CLI (BEAMTALK_BIN)
#   just dist-liveview              # from editors/liveview/ (BT_ATTACH_LAUNCHER)
#   npm --prefix editors/liveview/assets install
#   npx --prefix editors/liveview/assets playwright install chromium --with-deps
#
# Usage:
#   BEAMTALK_BIN=/path/to/beamtalk BT_ATTACH_LAUNCHER=/path/to/dist-liveview/bin/server \
#     desktop/e2e/attach-cycle.sh
#
# POSIX-only, matching bin/server itself (ADR 0097 Implementation §5b —
# Windows has no bin/server for the broker to invoke this way; BT-2988's
# Windows spawn path resolves everything itself and calls
# `bin\bt_attach.bat start` directly, which this script does not attempt to
# reproduce). Run on Linux or macOS.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

BEAMTALK_BIN="${BEAMTALK_BIN:-$REPO_ROOT/target/debug/beamtalk}"
BT_ATTACH_LAUNCHER="${BT_ATTACH_LAUNCHER:-$REPO_ROOT/dist-liveview/bin/server}"
WORKSPACE_ID="${BT_E2E_WORKSPACE_ID:-bt2989_desktop_e2e}"

fail() {
  echo "FAIL: $1" >&2
  exit 1
}

[ -x "$BEAMTALK_BIN" ] || fail "BEAMTALK_BIN not found/executable at '$BEAMTALK_BIN' — run 'just build' first"
[ -x "$BT_ATTACH_LAUNCHER" ] || fail "BT_ATTACH_LAUNCHER not found/executable at '$BT_ATTACH_LAUNCHER' — run 'just dist-liveview' (from editors/liveview/) first"
command -v node >/dev/null 2>&1 || fail "node is required for the Playwright eval-roundtrip step"
command -v curl >/dev/null 2>&1 || fail "curl is required to poll /readiness"

FRONT_PID=""
DEAD_FRONT_PID=""
# mktemp, not a hardcoded /tmp/ path (repo convention — see CLAUDE.md's
# cross-platform temp-path rule) — TMPDIR-aware and collision-free.
READINESS_BODY_FILE="$(mktemp)"

cleanup() {
  [ -n "$FRONT_PID" ] && kill "$FRONT_PID" >/dev/null 2>&1 || true
  [ -n "$DEAD_FRONT_PID" ] && kill "$DEAD_FRONT_PID" >/dev/null 2>&1 || true
  "$BEAMTALK_BIN" workspace stop "$WORKSPACE_ID" >/dev/null 2>&1 || true
  rm -f "$READINESS_BODY_FILE"
}
trap cleanup EXIT

# A TOCTOU heuristic, same caveat as
# crates/beamtalk-desktop-broker/src/port.rs's `find_free_port`: bind
# ephemeral port 0, read what the OS assigned, release it — the window
# between release and the front's own bind is small but real.
free_port() {
  node -e "
    const net = require('net');
    const s = net.createServer();
    s.listen(0, '127.0.0.1', () => {
      const p = s.address().port;
      s.close(() => console.log(p));
    });
  "
}

# GET /readiness, writing the body to $READINESS_BODY_FILE and printing the
# status code — one request (not two separate curls, which could race a
# state transition between them).
probe_readiness() {
  local port="$1"
  curl -s -o "$READINESS_BODY_FILE" -w '%{http_code}' "http://127.0.0.1:${port}/readiness" 2>/dev/null || echo "000"
}

# ── 1. `beamtalk workspace create` a real workspace ─────────────────────────
echo "==> creating workspace '$WORKSPACE_ID'"
"$BEAMTALK_BIN" workspace create "$WORKSPACE_ID" --background --persistent

node_name=""
for _ in $(seq 1 10); do
  node_name=$("$BEAMTALK_BIN" workspace status "$WORKSPACE_ID" | awk '/^Node:/ {print $2}')
  [ -n "$node_name" ] && break
  sleep 1
done
[ -n "$node_name" ] || fail "workspace '$WORKSPACE_ID' did not report a Node: within 10s"
echo "==> workspace is live: $node_name"

# ── 2. "launch the desktop app -> attach": spawn a front with the exact
#      broker contract (spawn.rs's build_env: PORT, BT_ATTACH_BIND_IP,
#      BT_ATTACH_NODE_SUFFIX, RELEASE_DISTRIBUTION=none). ───────────────────
port=$(free_port)
suffix="$WORKSPACE_ID"
echo "==> spawning a front on port $port (suffix $suffix)"
PORT="$port" BT_ATTACH_BIND_IP=127.0.0.1 BT_ATTACH_NODE_SUFFIX="$suffix" RELEASE_DISTRIBUTION=none \
  "$BT_ATTACH_LAUNCHER" "$WORKSPACE_ID" &
FRONT_PID=$!

# ── 3. "confirm the window loads": two-stage readiness (HTTP up is implicit
#      in a 200/503 from curl; GET /readiness forces connect/0 + one cheap
#      RPC — the same probe the broker runs before opening a window). ──────
echo "==> waiting for /readiness"
ready=""
for _ in $(seq 1 150); do
  code=$(probe_readiness "$port")
  if [ "$code" = "200" ]; then
    ready="1"
    break
  fi
  kill -0 "$FRONT_PID" 2>/dev/null || fail "front process exited before becoming ready"
  sleep 0.2
done
[ -n "$ready" ] || fail "/readiness never returned 200 within 30s"
echo "==> front is ready"

# ── 4. "confirm an eval round-trips through the LiveView UI" ────────────────
echo "==> running the Playwright eval round-trip"
(cd "$REPO_ROOT/editors/liveview/assets" && node "$SCRIPT_DIR/eval-roundtrip.mjs" "http://127.0.0.1:${port}/" "3 + 4" "7") \
  || fail "eval did not round-trip through the LiveView UI"

# ── 5. "detach -> confirm the front process exits" ──────────────────────────
echo "==> detaching (killing the front) and confirming it exits"
kill "$FRONT_PID"
exited=""
for _ in $(seq 1 50); do
  if ! kill -0 "$FRONT_PID" 2>/dev/null; then
    exited="1"
    break
  fi
  sleep 0.2
done
[ -n "$exited" ] || fail "front process (pid $FRONT_PID) did not exit within 10s of being killed"
FRONT_PID=""
echo "==> front process exited cleanly"

# ── 6. Negative path: attach to a DEAD workspace (stopped, but its
#      metadata.json/cookie remain on disk — spawn_front only checks the
#      former exists) must surface the /readiness failure taxonomy, not
#      hang or crash. ───────────────────────────────────────────────────────
echo "==> stopping the workspace, then re-attaching to exercise the dead-workspace path"
"$BEAMTALK_BIN" workspace stop "$WORKSPACE_ID"

dead_port=$(free_port)
dead_suffix="$WORKSPACE_ID"
PORT="$dead_port" BT_ATTACH_BIND_IP=127.0.0.1 BT_ATTACH_NODE_SUFFIX="$dead_suffix" RELEASE_DISTRIBUTION=none \
  "$BT_ATTACH_LAUNCHER" "$WORKSPACE_ID" &
DEAD_FRONT_PID=$!

dead_body=""
for _ in $(seq 1 150); do
  code=$(probe_readiness "$dead_port")
  if [ "$code" = "503" ]; then
    dead_body=$(cat "$READINESS_BODY_FILE" 2>/dev/null || echo "")
    break
  fi
  if [ "$code" = "200" ]; then
    body=$(cat "$READINESS_BODY_FILE" 2>/dev/null || echo "")
    fail "expected the dead workspace's /readiness to fail, got 200: $body"
  fi
  kill -0 "$DEAD_FRONT_PID" 2>/dev/null || fail "front process exited unexpectedly while probing the dead workspace"
  sleep 0.2
done
[ -n "$dead_body" ] || fail "dead-workspace /readiness never resolved (200 or 503) within 30s — should fail fast, not hang"
case "$dead_body" in
  *dead_workspace*) : ;;
  *) fail "expected /readiness's failure reason to be dead_workspace, got: $dead_body" ;;
esac
echo "==> dead workspace surfaced its failure cleanly: $dead_body"

kill "$DEAD_FRONT_PID" >/dev/null 2>&1 || true
wait "$DEAD_FRONT_PID" 2>/dev/null || true
DEAD_FRONT_PID=""

echo "==> BT-2989 E2E attach cycle passed"

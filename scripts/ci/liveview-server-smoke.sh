#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# BT-5189: run the built LiveView IDE release's `bin/server <workspace-id>`
# once, the way a user does, and check it serves HTTP.
#
# Usage: scripts/ci/liveview-server-smoke.sh <dist-liveview-dir>
#
# The workspace is a stand-in: a BEAMTALK_HOME holding
# workspaces/<id>/{metadata.json,cookie}, which is all `bin/server <id>` reads
# to resolve BT_WORKSPACE_NODE / BT_WORKSPACE_COOKIE. No workspace node needs
# to run: the front boots and serves its pages without one (it attaches on
# LiveView mount). SECRET_KEY_BASE is left unset, so the launcher's own key
# generation is exercised too.
set -euo pipefail

dist=${1:?usage: liveview-server-smoke.sh <dist-liveview-dir>}
dist=$(cd -P -- "$dist" && pwd)

work=$(mktemp -d)
server_pid=
cleanup() {
    if [ -n "$server_pid" ]; then
        kill "$server_pid" 2>/dev/null || true
        wait "$server_pid" 2>/dev/null || true
    fi
    rm -rf "$work"
}
trap cleanup EXIT

ws_id=ci-smoke
ws_dir="$work/home/workspaces/$ws_id"
mkdir -p "$ws_dir"
printf '{"node_name": "beamtalk_workspace_%s@localhost"}\n' "$ws_id" >"$ws_dir/metadata.json"
printf 'ci-smoke-cookie\n' >"$ws_dir/cookie"
port_file="$work/port"
log="$work/server.log"

env -u SECRET_KEY_BASE \
    BEAMTALK_HOME="$work/home" \
    PORT=0 \
    BT_ATTACH_PORT_FILE="$port_file" \
    "$dist/bin/server" "$ws_id" >"$log" 2>&1 &
server_pid=$!

# Wait (up to 60 s) for the endpoint to report its bound port.
for _ in $(seq 1 120); do
    [ -s "$port_file" ] && break
    if ! kill -0 "$server_pid" 2>/dev/null; then
        echo "❌ bin/server exited before serving:" >&2
        cat "$log" >&2
        exit 1
    fi
    sleep 0.5
done

if [ ! -s "$port_file" ]; then
    echo "❌ bin/server did not report a port within 60 s:" >&2
    cat "$log" >&2
    exit 1
fi

port=$(tr -d '[:space:]' <"$port_file")
status=$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:${port}/")

grep -q "LiveView IDE → beamtalk_workspace_${ws_id}@localhost" "$log" || {
    echo "❌ bin/server did not resolve the workspace node:" >&2
    cat "$log" >&2
    exit 1
}
grep -q "generated an ephemeral one" "$log" || {
    echo "❌ bin/server did not generate SECRET_KEY_BASE:" >&2
    cat "$log" >&2
    exit 1
}

case "$status" in
2?? | 3??)
    echo "✅ bin/server ${ws_id} served HTTP ${status} on port ${port}"
    ;;
*)
    echo "❌ bin/server answered HTTP ${status} on port ${port}:" >&2
    cat "$log" >&2
    exit 1
    ;;
esac

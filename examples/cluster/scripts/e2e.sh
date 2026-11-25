#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# End-to-end run of examples/cluster (BT-5143): builds the release, starts
# three nodes on this host (two workers and a coordinator), runs the Demo
# scenario against the coordinator, asserts its output, and removes every node
# it started.
#
# Usage: examples/cluster/scripts/e2e.sh
#   BEAMTALK="<command>" picks the CLI (default `beamtalk`; `just test-cluster`
#   passes `cargo run --bin beamtalk --quiet --` so the checkout's runtime is used).
#
# What it proves: ADR 0126 distribution works the way the example uses it —
# remote spawn and lookup, sync sends and casts, Values on the wire, a
# cluster-wide name, NodeUp/NodeDown, and a worker node dying mid-call with the
# job reassigned. See ../README.md for the same steps by hand.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
pkg="$(cd "$here/.." && pwd)"
read -r -a bt <<<"${BEAMTALK:-beamtalk}"
# Ordering: the release launcher's `rpc` client gives up after 120 s
# (?RPC_CALL_TIMEOUT_MS in beamtalk_release_launcher.erl) and reports a
# structured timeout; this outer wall-clock limit is a backstop that must stay
# above it, hence the default of 150 s.
TIMEOUT_SECS="${CLUSTER_E2E_TIMEOUT:-150}"

work="$(mktemp -d "${TMPDIR:-/tmp}/beamtalk-cluster-e2e.XXXXXX")"
rel="$work/release"
# Node names are unique per run (letters, digits, '-' and '_' only, as the
# release launcher requires) so two runs on one host never share a node.
suffix="$$"
coord="coord_$suffix"
w1="worker1_$suffix"
w2="worker2_$suffix"
# A throwaway cookie: not a secret, but it keeps this run's nodes from joining
# anyone else's cluster on the host.
RELEASE_COOKIE="e2e-$(od -An -N8 -tx1 /dev/urandom | tr -d ' \n')"
export RELEASE_COOKIE

# epmd survives its nodes by design; only stop it if this run started it.
epmd_was_running=false
if epmd -names >/dev/null 2>&1; then epmd_was_running=true; fi

launcher() { local node="$1"; shift; RELEASE_NODE="$node" "$rel/bin/cluster" "$@"; }

cleanup() {
  local status=$?
  set +e
  for n in "$coord" "$w1" "$w2"; do
    launcher "$n" stop >/dev/null 2>&1
  done
  # Anything that ignored a graceful stop: kill by the unique node name.
  for _ in 1 2 3 4 5; do
    pgrep -f "sname ($coord|$w1|$w2)@" >/dev/null || break
    sleep 1
  done
  pkill -9 -f "sname ($coord|$w1|$w2)@" 2>/dev/null
  # Verify nothing of ours is left registered with epmd or running. epmd drops
  # a killed node's name a moment after the socket closes, so give it a few
  # seconds before calling that a leak.
  leaked=false
  for _ in 1 2 3 4 5 6 7 8 9 10; do
    epmd -names 2>/dev/null | grep -qE "name ($coord|$w1|$w2) " || break
    sleep 0.5
  done
  if epmd -names 2>/dev/null | grep -E "name ($coord|$w1|$w2) " >/dev/null; then
    echo "e2e: node still registered with epmd after cleanup" >&2
    epmd -names >&2
    leaked=true
  fi
  if pgrep -f "sname ($coord|$w1|$w2)@" >/dev/null; then
    echo "e2e: node process still running after cleanup" >&2
    leaked=true
  fi
  if [ "$epmd_was_running" = false ]; then epmd -kill >/dev/null 2>&1; fi
  if [ "$status" -ne 0 ]; then
    for n in "$coord" "$w1" "$w2"; do
      echo "--- $n log ---" >&2
      tail -n 40 "$work/$n.log" >&2 2>/dev/null
    done
  fi
  rm -rf "$work"
  if [ "$leaked" = true ] && [ "$status" -eq 0 ]; then exit 1; fi
  exit "$status"
}
trap cleanup EXIT

echo "e2e: building release"
(cd "$pkg" && "${bt[@]}" release --output "$rel" --force-output --no-include-erts >"$work/release.log" 2>&1) || {
  cat "$work/release.log" >&2
  exit 1
}

echo "e2e: starting nodes $w1 $w2 $coord"
for n in "$w1" "$w2" "$coord"; do
  # `foreground` is the launcher's one start verb (no daemon mode, ADR 0125).
  RELEASE_NODE="$n" setsid "$rel/bin/cluster" foreground >"$work/$n.log" 2>&1 &
done

for n in "$w1" "$w2" "$coord"; do
  up=false
  for _ in $(seq 1 60); do
    if launcher "$n" ping >/dev/null 2>&1; then up=true; break; fi
    sleep 0.5
  done
  [ "$up" = true ] || { echo "e2e: node $n did not come up" >&2; exit 1; }
done

echo "e2e: running the scenario on $coord"
out="$work/scenario.out"
set +e
timeout "$TIMEOUT_SECS" env RELEASE_NODE="$coord" "$rel/bin/cluster" rpc \
  "Demo main: $w1@localhost $w2@localhost" >"$out" 2>&1
rc=$?
set -e
cat "$out"
[ "$rc" -eq 0 ] || { echo "e2e: scenario exited $rc" >&2; exit 1; }

fail=0
expect() {
  if ! grep -qF -- "$1" "$out"; then echo "e2e: MISSING: $1" >&2; fail=1; fi
}
if grep -q '^FAIL ' "$out"; then echo "e2e: a check failed" >&2; fail=1; fi
expect "PASS NodeUp announced for both"
expect "PASS remote async send fails loudly instead of hanging"
expect "PASS worker1 reaches the global Ledger from its own node"
expect "PASS a Value that crossed the wire equals a locally built one"
expect "PASS NodeDown announced for worker2"
expect "PASS the failed call surfaced as node_down"
expect "PASS all six jobs finished despite the node loss"
# The full count: a scenario that skips checks must not pass.
expect "RESULT 33 ok, 0 failed"

# The node that died mid-run must really be gone, and the survivor still up.
if launcher "$w2" ping >/dev/null 2>&1; then echo "e2e: $w2 should be down" >&2; fail=1; fi
launcher "$w1" ping >/dev/null 2>&1 || { echo "e2e: $w1 should still be up" >&2; fail=1; }

if [ "$fail" -ne 0 ]; then exit 1; fi
echo "e2e: ok"

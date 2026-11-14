#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Test (BT-4813): scripts/refresh-edge.sh reinstalls only when the published
# edge checksum differs from the marker under the prefix, and never fails.
# Runs against a fake release directory (file:// URLs) and a fake installer.
#
# Usage: scripts/ci/test-refresh-edge.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$REPO_ROOT/scripts/refresh-edge.sh"
ASSET="beamtalk-edge-linux-x86_64.tar.gz"
NEW="$(printf 'new' | sha256sum | cut -d' ' -f1)"
OLD="$(printf 'old' | sha256sum | cut -d' ' -f1)"

T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/release"
printf '%s  %s\n' "$NEW" "$ASSET" > "$T/release/$ASSET.sha256"

# Fake installer: records the call; writes the marker like install.sh does.
cat > "$T/install.sh" <<'INST'
#!/bin/sh
prefix=""
while [ $# -gt 0 ]; do [ "$1" = "--prefix" ] && prefix="$2"; shift; done
echo run >> "$prefix/installer-calls"
printf '%s  %s\n' "$FAKE_SUM" "beamtalk-edge-linux-x86_64.tar.gz" > "$prefix/.beamtalk-build.sha256"
INST

failures=0
check() { # label, condition-exit-status
  if [ "$2" -eq 0 ]; then echo "ok: $1"; else echo "FAIL: $1" >&2; failures=$((failures + 1)); fi
}

# run_case <name> <marker-sum|-|ro> [extra env ...]; sets OUT, RC, CALLS, P
run_case() {
  local name="$1" marker="$2"
  shift 2
  P="$T/$name"
  mkdir -p "$P"
  [ "$marker" != "-" ] && [ "$marker" != "ro" ] && printf '%s  %s\n' "$marker" "$ASSET" > "$P/.beamtalk-build.sha256"
  [ "$marker" = "ro" ] && chmod 555 "$P"
  RC=0
  OUT="$(env BEAMTALK_PREFIX="$P" BEAMTALK_EDGE_BASE_URL="file://$T/release" \
    BEAMTALK_INSTALLER="$T/install.sh" FAKE_SUM="$NEW" "$@" bash "$SCRIPT" 2>"$T/$name.err")" || RC=$?
  CALLS=0
  [ -f "$P/installer-calls" ] && CALLS="$(wc -l < "$P/installer-calls")"
  chmod 755 "$P"
}

# current: marker == release -> no install, one status line
run_case current "$NEW"
check "current: exit 0" "$RC"
check "current: installer not run" "$([ "$CALLS" -eq 0 ]; echo $?)"
check "current: one line saying current" "$([[ "$OUT" == *"current, not refreshed"* && "$(wc -l <<<"$OUT")" -eq 1 ]]; echo $?)"

# stale: marker differs -> installer runs once, marker now matches
run_case stale "$OLD"
check "stale: exit 0" "$RC"
check "stale: installer ran once" "$([ "$CALLS" -eq 1 ]; echo $?)"
check "stale: reports refreshed" "$([[ "$OUT" == *"refreshed (was ${OLD:0:12})"* ]]; echo $?)"
check "stale: next run is current" "$(bash -c 'BEAMTALK_PREFIX="$1" BEAMTALK_EDGE_BASE_URL="file://$2/release" BEAMTALK_INSTALLER="$2/install.sh" bash "$3" | grep -q "current, not"' _ "$P" "$T" "$SCRIPT"; echo $?)"

# no marker (every pre-existing environment): stale once
run_case nomarker -
check "no marker: exit 0" "$RC"
check "no marker: installer ran" "$([ "$CALLS" -eq 1 ]; echo $?)"
check "no marker: reports it" "$([[ "$OUT" == *"was no marker"* ]]; echo $?)"

# network failure: warn, exit 0, installer untouched, marker untouched
run_case offline "$OLD" BEAMTALK_EDGE_BASE_URL="file://$T/nonexistent"
check "network failure: exit 0" "$RC"
check "network failure: installer not run" "$([ "$CALLS" -eq 0 ]; echo $?)"
check "network failure: warns" "$(grep -q 'warning' "$T/offline.err"; echo $?)"
check "network failure: marker kept" "$(grep -q "^$OLD" "$P/.beamtalk-build.sha256"; echo $?)"

# malformed checksum asset: warn, exit 0, no install
mkdir -p "$T/bad"
echo "<html>not found</html>" > "$T/bad/$ASSET.sha256"
run_case malformed "$OLD" BEAMTALK_EDGE_BASE_URL="file://$T/bad"
check "malformed checksum: exit 0, no install, warns" "$([ "$RC" -eq 0 ] && [ "$CALLS" -eq 0 ] && grep -q malformed "$T/malformed.err"; echo $?)"

# installer failure: warn, exit 0
printf '#!/bin/sh\nexit 1\n' > "$T/fail-install.sh"
run_case instfail "$OLD" BEAMTALK_INSTALLER="$T/fail-install.sh"
check "installer failure: exit 0 and warns" "$([ "$RC" -eq 0 ] && grep -q 'reinstall failed' "$T/instfail.err"; echo $?)"

if [ "$failures" -gt 0 ]; then
  echo "❌ test-refresh-edge: $failures case(s) failed" >&2
  exit 1
fi
echo "✅ test-refresh-edge: current / stale / no marker / failure paths behave"

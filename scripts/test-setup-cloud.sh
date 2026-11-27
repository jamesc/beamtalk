#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Regression test for scripts/setup-cloud.sh (BT-5341): the install/verify
# logic must require only the tools the resolved .tool-versions pins.
#
# Runs the real script with SKIP_ERLANG=1 (so no mise/apt/network work) against
# fixture checkouts and a PATH of stub binaries, then asserts on exit status
# and output. Usage: scripts/test-setup-cloud.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/setup-cloud.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "${WORK}"' EXIT
FAILURES=0

# Stub tools. erl/rebar3/elixir print the versions the fixtures pin.
BIN="${WORK}/bin"
mkdir -p "${BIN}"
stub() { printf '#!/bin/sh\n%s\n' "$2" > "${BIN}/$1"; chmod +x "${BIN}/$1"; }
stub erl 'printf 28.5'
stub rebar3 'echo "rebar 3.27.0 on Erlang/OTP 28"'
stub rustc 'echo "rustc 1.0.0"'
stub dpkg 'exit 0'
for t in cargo node npm gh just; do stub "$t" 'exit 0'; done
STUB_ELIXIR='echo "Elixir 1.20.1 (compiled with Erlang/OTP 28)"'

# Sandboxed PATH: stubs first, then symlinks to just the system tools the script
# needs — so a real elixir/mix on the host can never leak in (or skip the test).
SYS="${WORK}/sys"
mkdir -p "${SYS}"
for t in awk grep sed cat dirname basename mktemp rm mkdir id head tr sort uname env bash sh date cut tee ln install curl; do
  p="$(command -v "$t" 2>/dev/null)" && ln -sf "${p}" "${SYS}/${t}"
done
SYSPATH="${SYS}"

# run_case NAME TOOL_VERSIONS_CONTENT WITH_ELIXIR_STUB [SKIP_ELIXIR] → sets OUT, RC
# Pass SKIP_ELIXIR=1 for cases where elixir is pinned: with SKIP_ERLANG=1 that
# skips the whole mise section, keeping the test hermetic (no installs, no
# writes outside WORK). Verification keys off the pin, not SKIP_ELIXIR.
run_case() {
  local name="$1" pins="$2" with_elixir="$3" skip_elixir="${4:-}" repo="${WORK}/repo-$1"
  mkdir -p "${repo}"
  printf '%s\n' "${pins}" > "${repo}/.tool-versions"
  rm -f "${BIN}/elixir" "${BIN}/mix"
  if [ "${with_elixir}" = "1" ]; then
    stub elixir "${STUB_ELIXIR}"
    stub mix 'exit 0'
  fi
  OUT="$(cd "${repo}" && env -i HOME="${WORK}" PATH="${BIN}:${SYSPATH}" \
    CLAUDE_PROJECT_DIR="${repo}" SKIP_ERLANG=1 SKIP_ELIXIR="${skip_elixir}" SKIP_NODE=1 SKIP_RUST_TOOLS=1 \
    SKIP_SKILLS=1 bash "${SCRIPT}" 2>&1)"
  RC=$?; [ -n "${DEBUG_CASES:-}" ] && { echo "--- $name rc=$RC"; echo "$OUT"; }
}

expect() { # DESCRIPTION CONDITION(0=true)
  if [ "$2" -eq 0 ]; then echo "PASS: $1"; else echo "FAIL: $1"; FAILURES=$((FAILURES + 1)); fi
}
has() { printf '%s' "${OUT}" | grep -q -- "$1"; }

# 1. Satellite checkout (erlang + rebar only, no elixir anywhere): must succeed.
run_case satellite $'erlang 28.5\nrebar 3.27.0' 0
expect "satellite (no elixir pin): exit 0" "$([ "${RC}" -eq 0 ]; echo $?)"
has "elixir NOT FOUND" && r=1 || r=0
expect "satellite: elixir not required" "${r}"
has "mix NOT FOUND" && r=1 || r=0
expect "satellite: mix not required" "${r}"
has "erlang 28.5 (matches pin)"; expect "satellite: erlang verified" $?
has "rebar3 3.27.0 (matches pin)"; expect "satellite: rebar3 verified" $?

# 2. Beamtalk checkout (elixir pinned, present): still installed + verified.
run_case beamtalk $'erlang 28.5\nelixir 1.20.1-otp-28\nrebar 3.27.0' 1 1
expect "beamtalk (elixir pinned): exit 0" "$([ "${RC}" -eq 0 ]; echo $?)"
has "elixir 1.20.1 (matches pin)"; expect "beamtalk: elixir verified" $?
has "elixir not pinned" && r=1 || r=0
expect "beamtalk: elixir not treated as unpinned" "${r}"

# 3. Beamtalk checkout with elixir pinned but missing: must still fail.
run_case beamtalk-missing $'erlang 28.5\nelixir 1.20.1-otp-28\nrebar 3.27.0' 0 1
expect "pinned elixir missing: exit 1" "$([ "${RC}" -eq 1 ]; echo $?)"
has "elixir NOT FOUND"; expect "pinned elixir missing: reported" $?

# 4. Install step: SKIP_ELIXIR is derived when elixir is unpinned.
run_case install $'erlang 28.5\nrebar 3.27.0' 0
has "not installing or verifying it"; expect "unpinned elixir: install skipped (SKIP_ELIXIR derived)" $?

if [ "${FAILURES}" -eq 0 ]; then echo "All setup-cloud tests passed"; else echo "${FAILURES} failure(s)"; exit 1; fi

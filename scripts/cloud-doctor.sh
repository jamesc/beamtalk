#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# cloud-doctor.sh — one-shot health check for Beamtalk dev environments,
# tuned for Claude Code cloud sessions (and other MITM-proxy sandboxes).
#
# Why this exists: in the cloud sandbox the egress proxy rejects the BEAM's
# TLS client at the upstream layer — `curl` succeeds but Erlang/Elixir httpc
# gets 502/503 on *every* https host (not just hex.pm). So "curl works" is a
# useless signal for whether `rebar3`/`mix deps.get` will work. The decisive
# probe is an *Erlang* fetch through the hex-bridge. This script makes that
# (and the rest of the toolchain) visible in one fast pass instead of failing
# four minutes into `just build`.
#
# Usage:
#   scripts/cloud-doctor.sh            # full check, exit non-zero if unhealthy
#   scripts/cloud-doctor.sh --summary  # one-line result (used at session start)
#   SKIP_NET=1 scripts/cloud-doctor.sh # skip network probes (offline/fast)
#
# Exit code: 0 = healthy, 1 = one or more required checks failed.

set -uo pipefail

SUMMARY_ONLY=0
[[ "${1:-}" == "--summary" ]] && SUMMARY_ONLY=1

if [ -t 1 ] && [[ "${NO_COLOR:-}" == "" ]]; then
  GREEN='\033[0;32m'; YELLOW='\033[1;33m'; RED='\033[0;31m'; DIM='\033[2m'; NC='\033[0m'
else
  GREEN='' YELLOW='' RED='' DIM='' NC=''
fi

FAILS=0
WARNS=0
ok()   { (( SUMMARY_ONLY )) || printf '  %b %s\n' "${GREEN}✓${NC}" "$1"; }
warn() { WARNS=$((WARNS+1)); (( SUMMARY_ONLY )) || printf '  %b %s\n' "${YELLOW}!${NC}" "$1"; }
bad()  { FAILS=$((FAILS+1)); (( SUMMARY_ONLY )) || printf '  %b %s\n' "${RED}✗${NC}" "$1"; }
note() { (( SUMMARY_ONLY )) || printf '    %b%s%b\n' "${DIM}" "$1" "${NC}"; }
have() { command -v "$1" &>/dev/null; }

(( SUMMARY_ONLY )) || printf '\n%b==> Beamtalk env doctor%b\n' '\033[1m' '\033[0m'

# --- 1. Toolchain presence -------------------------------------------------
# Version probe per tool. `erl --version` would drop into a shell and hang, so
# erl gets a bounded -noshell eval instead.
tool_version() {
  case "$1" in
    erl) timeout 6 erl -noshell -eval 'io:format("OTP ~s",[erlang:system_info(otp_release)]),halt().' 2>/dev/null ;;
    *)   "$1" --version 2>/dev/null | head -1 ;;
  esac
}
(( SUMMARY_ONLY )) || echo "Toolchain:"
for cmd in rustc cargo erl rebar3 just node npm; do
  if have "$cmd"; then ok "$cmd ($(tool_version "$cmd" | cut -c1-40))"
  else bad "$cmd NOT FOUND"; fi
done
# Elixir/mix are only needed for editors/liveview — missing is a warning, not fatal.
for cmd in elixir mix gh; do
  if have "$cmd"; then ok "$cmd"; else warn "$cmd missing (needed for liveview/CI, not core build)"; fi
done

# --- 2. Cloud-sandbox detection + bridge -----------------------------------
IN_CLOUD=0
[[ "${CLAUDE_CODE_REMOTE:-}" == "true" ]] && IN_CLOUD=1
[[ -n "${HTTP_PROXY:-}" && "${HTTP_PROXY:-}" == *"@"* ]] && IN_CLOUD=1

if (( IN_CLOUD )); then
  (( SUMMARY_ONLY )) || echo "Cloud sandbox (MITM egress proxy):"
  BRIDGE_PORT="${HEX_BRIDGE_PORT:-18081}"
  if have lsof && lsof -i:"${BRIDGE_PORT}" &>/dev/null; then
    ok "hex-bridge proxy listening on :${BRIDGE_PORT}"
  elif (command -v curl >/dev/null && curl -s -o /dev/null --max-time 3 "http://127.0.0.1:${BRIDGE_PORT}/names"); then
    ok "hex-bridge proxy responding on :${BRIDGE_PORT}"
  else
    bad "hex-bridge proxy NOT running on :${BRIDGE_PORT} — rebar3/mix fetches will hang/fail"
    note "start it: HEX_BRIDGE_PORT=${BRIDGE_PORT} python3 scripts/hex-bridge-proxy.py &"
  fi
  # Env wiring that routes BEAM package managers through the bridge.
  if [[ "${HEX_CDN:-}" == http://127.0.0.1:* ]]; then ok "HEX_CDN → bridge"
  else warn "HEX_CDN not pointed at bridge (rebar3 hex may bypass it)"; fi
  if [[ "${HEX_MIRROR:-}" == http://127.0.0.1:* ]]; then ok "HEX_MIRROR → bridge"
  else warn "HEX_MIRROR not set (mix/hex for liveview may bypass bridge)"; fi
fi

# --- 3. Network reachability (the decisive part) ---------------------------
if [[ "${SKIP_NET:-}" == "1" ]]; then
  warn "network probes skipped (SKIP_NET=1)"
else
  (( SUMMARY_ONLY )) || echo "Network:"
  # 3a. curl egress — baseline "is the box online at all".
  if have curl; then
    if curl -s -o /dev/null --max-time 8 "https://repo.hex.pm/names"; then
      ok "curl https egress OK (repo.hex.pm)"
    else
      bad "curl https egress FAILED — box appears offline or proxy down"
    fi
  fi

  # 3b. THE decisive probe: can the BEAM actually fetch packages?
  #     In cloud sandboxes Erlang's own TLS client is rejected by the proxy,
  #     so we probe through the bridge (http://127.0.0.1:PORT) exactly as
  #     rebar3 does. This is what predicts `just build` success.
  if have erl; then
    BP="${HEX_BRIDGE_PORT:-18081}"
    PROBE_URL="http://127.0.0.1:${BP}/names"
    (( IN_CLOUD )) || PROBE_URL="https://repo.hex.pm/names"
    RES=$(erl -noshell -eval "
      inets:start(), ssl:start(),
      Ssl = [{verify, verify_peer}, {cacerts, public_key:cacerts_get()},
             {customize_hostname_check,
              [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}],
      Opts = case lists:prefix(\"https\", \"${PROBE_URL}\") of true -> [{ssl, Ssl}]; false -> [] end,
      R = (catch httpc:request(get, {\"${PROBE_URL}\", []}, [{timeout, 8000} | Opts], [{body_format, binary}])),
      case R of
        {ok, {{_, 200, _}, _, B}} -> io:format(\"OK ~p~n\", [byte_size(B)]);
        {ok, {{_, C, _}, _, _}}   -> io:format(\"HTTP ~p~n\", [C]);
        Other                     -> io:format(\"ERR ~p~n\", [Other])
      end, halt()." 2>/dev/null)
    case "${RES}" in
      OK\ *) ok "BEAM package fetch OK (erl httpc → ${PROBE_URL%%\?*})" ;;
      *)
        if (( IN_CLOUD )); then
          bad "BEAM package fetch FAILED via bridge: ${RES:-no response}"
          note "rebar3/mix deps.get will fail. Check the hex-bridge proxy above."
        else
          warn "BEAM direct fetch returned: ${RES:-no response}"
          note "Outside cloud this may be a corporate MITM — set HEX_CACERTS_PATH or start the bridge."
        fi ;;
    esac
  fi
fi

# --- 4. Locale (Elixir/UTF-8) ----------------------------------------------
if have mix; then
  if [[ "${ELIXIR_ERL_OPTIONS:-}" == *"+fnu"* ]] || locale 2>/dev/null | grep -qiE 'UTF-?8'; then
    ok "locale/UTF-8 OK for Elixir"
  else
    warn "non-UTF-8 locale and no +fnu — Elixir may trip; export ELIXIR_ERL_OPTIONS=+fnu"
  fi
fi

# --- Summary ---------------------------------------------------------------
if (( SUMMARY_ONLY )); then
  if (( FAILS > 0 )); then
    printf '%benv doctor: %d failed, %d warnings%b — run scripts/cloud-doctor.sh\n' "${RED}" "$FAILS" "$WARNS" "${NC}"
  elif (( WARNS > 0 )); then
    printf '%benv doctor: healthy, %d warning(s)%b\n' "${YELLOW}" "$WARNS" "${NC}"
  else
    printf '%benv doctor: healthy%b\n' "${GREEN}" "${NC}"
  fi
else
  echo ""
  if (( FAILS > 0 )); then
    printf '%b✗ %d check(s) failed, %d warning(s)%b\n\n' "${RED}" "$FAILS" "$WARNS" "${NC}"
  elif (( WARNS > 0 )); then
    printf '%b✓ healthy with %d warning(s)%b\n\n' "${YELLOW}" "$WARNS" "${NC}"
  else
    printf '%b✓ environment healthy%b\n\n' "${GREEN}" "${NC}"
  fi
fi

if (( FAILS > 0 )); then exit 1; else exit 0; fi

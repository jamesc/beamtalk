#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Refresh the installed edge compiler when the published `edge` release is
# newer (BT-4813).
#
# Cloud environments cache the result of their setup script, so the edge
# compiler installed there goes stale. This script is cheap enough to run at
# every session start: it fetches the release's small `.sha256` asset (one
# HTTP request), compares it with the marker `install.sh` wrote under the
# install prefix, and only reinstalls (`install.sh --edge --prefix <prefix>`)
# when they differ. A prefix with no marker (an install that predates this
# script) counts as stale once.
#
# It never fails the caller: every problem (no network, missing release,
# failed download, read-only prefix) prints a warning and exits 0, leaving the
# installed build in place. It prints exactly one status line on stdout
# (warnings go to stderr).
#
# Usage (from a file, or piped like scripts/setup-cloud.sh):
#   scripts/refresh-edge.sh [--prefix PATH]
#   curl -fsSL https://raw.githubusercontent.com/jamesc/beamtalk/main/scripts/refresh-edge.sh | bash
#
# The installer is not trusted on TLS alone: the edge workflow publishes
# `install.sh` and `install.sh.sha256` as edge release assets, and the
# download is checked against that checksum (the way install.sh checks the
# tarball) before it runs. Both come from the same release, so this catches a
# corrupted download, not a compromised release. A mismatch warns and keeps the installed build.
# The reinstall runs under an exclusive `flock` on `<prefix>/.beamtalk-refresh.lock`;
# a session that cannot get the lock skips with a status line.
#
# Environment variables:
#   BEAMTALK_PREFIX         install prefix (default: /usr/local)
#   BEAMTALK_INSTALLER_URL  installer to run when stale (default: the edge
#                           release's install.sh asset); its checksum is read
#                           from "<url>.sha256". An override is for tests and
#                           lifts the https-only rule for the installer fetches only.
#   BEAMTALK_INSTALLER      path of a local installer, run unverified; wins over the URL
#   BEAMTALK_EDGE_BASE_URL  release download base for the tarball checksum
#                           (default: the edge release); an override is for tests
#                           and lifts the https-only rule for that fetch only
#
# Force a refresh: delete the marker (`rm <prefix>/.beamtalk-build.sha256`)
# and rerun, or run `install.sh --edge --prefix <prefix>` directly.

set -uo pipefail

REPO="jamesc/beamtalk"
PREFIX="${BEAMTALK_PREFIX:-/usr/local}"
INSTALLER_URL="${BEAMTALK_INSTALLER_URL:-https://github.com/${REPO}/releases/download/edge/install.sh}"
BASE_URL="${BEAMTALK_EDGE_BASE_URL:-https://github.com/${REPO}/releases/download/edge}"

while [ $# -gt 0 ]; do
  case "$1" in
    --prefix)
      if [ $# -lt 2 ]; then echo "refresh-edge: --prefix requires a value" >&2; exit 0; fi
      PREFIX="$2"; shift 2 ;;
    *) echo "refresh-edge: unknown option: $1" >&2; exit 0 ;;
  esac
done

MARKER="${PREFIX}/.beamtalk-build.sha256"

warn() { echo "refresh-edge: warning: $*" >&2; }

# The installed binary's own identity (BT-4812: `beamtalk 0.4.0-edge+<sha7> (<date>)`),
# appended to the status line when the binary exists and answers.
build_identity() {
  local v
  v="$(timeout 5 "${PREFIX}/bin/beamtalk" --version 2>/dev/null | head -n 1)" || return 0
  [ -n "${v}" ] && printf ' [%s]' "${v}"
  return 0
}

# The edge release is built for linux-x86_64 only.
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) ;;
  *) warn "edge builds exist for linux-x86_64 only; skipping"; exit 0 ;;
esac

ASSET="beamtalk-edge-linux-x86_64.tar.gz"
WORK="$(mktemp -d 2>/dev/null)" || { warn "no temp directory; skipping"; exit 0; }
trap 'rm -rf "${WORK}"' EXIT

# https-only is the default for every fetch; each override lifts it for its own
# fetches only (a test base URL must not weaken the installer download).
BASE_PROTO=(--proto '=https')
[ -n "${BEAMTALK_EDGE_BASE_URL:-}" ] && BASE_PROTO=()
INSTALLER_PROTO=(--proto '=https')
[ -n "${BEAMTALK_INSTALLER_URL:-}" ] && INSTALLER_PROTO=()

# --- 1. The published checksum: one small request ---
if ! curl -fsSL "${BASE_PROTO[@]}" --connect-timeout 5 --max-time 10 \
     -D "${WORK}/headers" -o "${WORK}/remote" "${BASE_URL}/${ASSET}.sha256" 2>"${WORK}/err"; then
  warn "could not read the edge release checksum ($(tr '\n' ' ' <"${WORK}/err" | cut -c1-120)); keeping the installed build"
  exit 0
fi
REMOTE_SUM="$(awk 'NR==1 {print $1}' "${WORK}/remote")"
if ! [[ "${REMOTE_SUM}" =~ ^[0-9a-f]{64}$ ]]; then
  warn "the edge release checksum is malformed; keeping the installed build"
  exit 0
fi
PUBLISHED="$(grep -i '^last-modified:' "${WORK}/headers" | tail -1 | cut -d: -f2- | tr -d '\r' | sed 's/^ *//')"
PUBLISHED="${PUBLISHED:-unknown}"

# --- 2. The installed checksum: the marker install.sh wrote ---
LOCAL_SUM=""
[ -r "${MARKER}" ] && LOCAL_SUM="$(awk 'NR==1 {print $1}' "${MARKER}" 2>/dev/null)"

if [ "${LOCAL_SUM}" = "${REMOTE_SUM}" ]; then
  echo "beamtalk edge ${REMOTE_SUM:0:12} (published ${PUBLISHED}): current, not refreshed$(build_identity)"
  exit 0
fi

# --- 3. Stale (or no marker): reinstall, one session at a time ---
# Hold the lock until the reinstall finishes (fd 9 closes at exit).
if ! command -v flock >/dev/null 2>&1; then
  warn "flock not found; keeping the installed build"
  exit 0
fi
mkdir -p "${PREFIX}" 2>/dev/null
# (the brace group scopes 2>/dev/null; a bare `exec ... 2>/dev/null` would silence stderr for good)
if ! { exec 9>"${PREFIX}/.beamtalk-refresh.lock"; } 2>/dev/null; then
  warn "cannot create the lock file under ${PREFIX}; keeping the installed build"
  exit 0
fi
if ! flock -n 9; then
  echo "beamtalk edge ${REMOTE_SUM:0:12} (published ${PUBLISHED}): skipped, another session is refreshing$(build_identity)"
  exit 0
fi

# Another session may have finished the reinstall while we waited for the lock.
LOCAL_SUM=""
[ -r "${MARKER}" ] && LOCAL_SUM="$(awk 'NR==1 {print $1}' "${MARKER}" 2>/dev/null)"
if [ "${LOCAL_SUM}" = "${REMOTE_SUM}" ]; then
  echo "beamtalk edge ${REMOTE_SUM:0:12} (published ${PUBLISHED}): current, not refreshed$(build_identity)"
  exit 0
fi

if [ -n "${BEAMTALK_INSTALLER:-}" ]; then
  INSTALLER="${BEAMTALK_INSTALLER}"
else
  INSTALLER="${WORK}/install.sh"
  if ! curl -fsSL "${INSTALLER_PROTO[@]}" --connect-timeout 5 --max-time 30 -o "${INSTALLER}" "${INSTALLER_URL}" 2>/dev/null; then
    warn "could not download install.sh; keeping the installed build"
    exit 0
  fi
  if ! curl -fsSL "${INSTALLER_PROTO[@]}" --connect-timeout 5 --max-time 10 -o "${WORK}/installer.sha256" "${INSTALLER_URL}.sha256" 2>/dev/null; then
    warn "could not read the install.sh checksum; keeping the installed build"
    exit 0
  fi
  WANT_SUM="$(awk 'NR==1 {print $1}' "${WORK}/installer.sha256")"
  GOT_SUM="$(sha256sum "${INSTALLER}" 2>/dev/null | awk '{print $1}')"
  if ! [[ "${WANT_SUM}" =~ ^[0-9a-f]{64}$ ]] || [ "${WANT_SUM}" != "${GOT_SUM}" ]; then
    warn "install.sh does not match its published checksum; not running it, keeping the installed build"
    exit 0
  fi
fi

if [ -n "${LOCAL_SUM}" ]; then WAS="${LOCAL_SUM:0:12}"; else WAS="no marker"; fi
if ! timeout 300 sh "${INSTALLER}" --edge --prefix "${PREFIX}" >"${WORK}/install.log" 2>&1; then
  warn "reinstall failed; keeping the installed build. Last installer output:"
  tail -n 5 "${WORK}/install.log" >&2
  exit 0
fi

NOW_SUM=""
[ -r "${MARKER}" ] && NOW_SUM="$(awk 'NR==1 {print $1}' "${MARKER}" 2>/dev/null)"
if [ "${NOW_SUM}" != "${REMOTE_SUM}" ]; then
  warn "reinstalled, but the marker does not match the release (an older install.sh?); it will retry next session"
fi
echo "beamtalk edge ${REMOTE_SUM:0:12} (published ${PUBLISHED}): refreshed (was ${WAS})$(build_identity)"
exit 0

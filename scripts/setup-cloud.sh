#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

# Beamtalk Cloud Development Environment Setup
#
# Installs the same dependencies as .devcontainer/Dockerfile for use in
# cloud environments (e.g. Claude Code cloud sessions).
#
# The BEAM toolchain (Erlang/OTP, Elixir, rebar3) is installed via mise, pinned
# by the repo-root .tool-versions — the single source of truth shared with CI's
# setup-beam and the devcontainer Dockerfile, so the cloud toolchain never
# drifts from what CI compiles with. (Previously this script apt-installed
# OTP 27 + a hand-pinned Elixir/rebar3, which silently fell behind the
# .tool-versions pin to OTP 28.5 / Elixir 1.20.1-otp-28 / rebar 3.27.0.)
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/jamesc/beamtalk/main/scripts/setup-cloud.sh | bash
#
# Or run locally:
#   ./scripts/setup-cloud.sh
#
# Environment variables:
#   MISE_VERSION    - mise version to install (default: v2026.6.3, matches Dockerfile)
#   SKIP_RUST_TOOLS - set to 1 to skip cargo tool installation
#   SKIP_ERLANG     - set to 1 to skip Erlang installation (mise erlang)
#   SKIP_ELIXIR     - set to 1 to skip Elixir installation (mise elixir)
#   SKIP_NODE       - set to 1 to skip Node.js installation
#   SKIP_SKILLS     - set to 1 to skip Claude Code skills setup

MISE_VERSION="${MISE_VERSION:-v2026.6.3}"
# mise installs the world-readable toolchain here (mirrors the Dockerfile's
# MISE_DATA_DIR) so its shims can sit on PATH for every shell.
MISE_DATA_DIR="${MISE_DATA_DIR:-/usr/local/share/mise}"
export MISE_DATA_DIR
MISE_SHIMS="${MISE_DATA_DIR}/shims"

# --- Helpers ---

if [ -t 1 ]; then
  GREEN='\033[0;32m'
  YELLOW='\033[1;33m'
  RED='\033[0;31m'
  BOLD='\033[1m'
  NC='\033[0m'
else
  GREEN='' YELLOW='' RED='' BOLD='' NC=''
fi

info()  { echo -e "${BOLD}==> $1${NC}"; }
ok()    { echo -e "  ${GREEN}✓${NC} $1"; }
warn()  { echo -e "  ${YELLOW}!${NC} $1"; }
fail()  { echo -e "  ${RED}✗${NC} $1"; }

have() { command -v "$1" &>/dev/null; }

# Repo root (the directory containing .tool-versions). Resolution order:
#   1. CLAUDE_PROJECT_DIR if the harness exports it (the checkout the hook runs in)
#   2. the script's own parent dir — only when run from a file on disk
#   3. the current working directory — the `curl … | bash` case, where the
#      script arrives on stdin and BASH_SOURCE is unset (so guard it under set -u)
# Then, if .tool-versions isn't at the guessed root, locate the checkout: walk
# up from the cwd (running from a subdirectory of the checkout), then look one
# level down. The cloud builds the environment from a parent dir (cwd=/home/user)
# with the checkout in a child (/home/user/beamtalk), so .tool-versions sits
# *below* the cwd, not at or above it.
if [ -n "${CLAUDE_PROJECT_DIR:-}" ]; then
  REPO_ROOT="${CLAUDE_PROJECT_DIR}"
elif [ -n "${BASH_SOURCE[0]:-}" ]; then
  REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
else
  REPO_ROOT="$(pwd)"
fi
if [ ! -f "${REPO_ROOT}/.tool-versions" ]; then
  _guess="${REPO_ROOT}"
  # (a) Walk up from the cwd.
  _dir="$(pwd)"
  while [ "${_dir}" != "/" ]; do
    if [ -f "${_dir}/.tool-versions" ]; then
      REPO_ROOT="${_dir}"
      break
    fi
    _dir="$(dirname "${_dir}")"
  done
  # (b) Not at/above the cwd — look one level down into the cwd's and $HOME's
  #     children (the cloud's checkout-in-a-subdir layout). The [ -f ] guard
  #     handles the no-match case where the glob stays literal.
  if [ ! -f "${REPO_ROOT}/.tool-versions" ]; then
    for _cand in "$(pwd)"/*/.tool-versions "${HOME:-/root}"/*/.tool-versions; do
      [ -f "${_cand}" ] || continue
      REPO_ROOT="$(cd "$(dirname "${_cand}")" && pwd)"
      break
    done
  fi
  # Surface the reroute — otherwise a wrong .tool-versions (e.g. a parent repo's)
  # is impossible to trace back from the resolved toolchain pins.
  [ "${REPO_ROOT}" = "${_guess}" ] || warn "REPO_ROOT resolved to ${REPO_ROOT} (searched from $(pwd))"
fi
TOOL_VERSIONS="${REPO_ROOT}/.tool-versions"

# Last resort: no checkout on disk anywhere we looked (e.g. the cloud builds the
# environment snapshot before the repo is cloned). Fetch the canonical pins from
# the default branch so mise can still resolve the BEAM toolchain — the same
# GitHub-asset path the rest of this script already relies on. The live session
# later runs against the real checkout, which the SessionStart hook trusts.
if [ ! -f "${TOOL_VERSIONS}" ]; then
  _PINS_REF="${BEAMTALK_PINS_REF:-main}"
  _PINS_DIR="$(mktemp -d)"
  if curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
       "https://raw.githubusercontent.com/jamesc/beamtalk/${_PINS_REF}/.tool-versions" \
       -o "${_PINS_DIR}/.tool-versions" 2>/dev/null && [ -s "${_PINS_DIR}/.tool-versions" ]; then
    # rust-toolchain.toml is optional — only the rustc pin assertion reads it.
    curl -fsSL --retry 3 \
      "https://raw.githubusercontent.com/jamesc/beamtalk/${_PINS_REF}/rust-toolchain.toml" \
      -o "${_PINS_DIR}/rust-toolchain.toml" 2>/dev/null || true
    REPO_ROOT="${_PINS_DIR}"
    TOOL_VERSIONS="${REPO_ROOT}/.tool-versions"
    warn "No checkout found — using BEAM pins from origin/${_PINS_REF}"
  fi
fi

# The repo ships a committed mise.toml. mise refuses to read ANY config in a
# directory whose config is untrusted — including .tool-versions — so a fresh
# cloud container (no trust record in mise's state dir) makes `mise install`
# bail out entirely, silently skipping the BEAM toolchain. mise's trust store
# lives in ~/.local/state/mise and is only intermittently captured by the cloud
# snapshot, which is exactly the "elixir isn't always installed" flakiness.
# Trust the checkout via env var so it holds regardless of snapshot state; this
# is persisted to /etc/profile.d below so every future shell trusts it too.
case ":${MISE_TRUSTED_CONFIG_PATHS:-}:" in
  *":${REPO_ROOT}:"*) ;;
  *) export MISE_TRUSTED_CONFIG_PATHS="${REPO_ROOT}${MISE_TRUSTED_CONFIG_PATHS:+:${MISE_TRUSTED_CONFIG_PATHS}}" ;;
esac

# Compute SUDO prefix — defer failure until a command actually needs it.
# This allows the script to succeed when all tools are pre-installed even if
# sudo is unavailable (e.g. rootless container with everything cached).
if [ "$(id -u)" -eq 0 ]; then
  SUDO=""
elif have sudo; then
  SUDO="sudo"
else
  SUDO=""
  _NEED_SUDO_WARNING=1
fi

# Call before any privileged operation to fail fast with a clear message.
require_sudo() {
  if [ "${_NEED_SUDO_WARNING:-}" = "1" ]; then
    fail "Root privileges required but sudo not available"
    exit 1
  fi
}

# Detect OS
if [ -f /etc/os-release ]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  OS_ID="${ID:-unknown}"
  OS_VERSION="${VERSION_CODENAME:-${VERSION_ID:-unknown}}"
else
  OS_ID="unknown"
  OS_VERSION="unknown"
fi

echo ""
info "Beamtalk cloud environment setup"
echo "  OS: ${OS_ID} ${OS_VERSION}"
echo ""

# --- BEAM toolchain (Erlang + Elixir + rebar3) via mise ---
#
# Pinned by repo-root .tool-versions (OTP 28.5 / Elixir 1.20.1-otp-28 /
# rebar 3.27.0 at time of writing). mise fetches precompiled BEAM builds — no
# source compile — exactly like the Dockerfile. We never apt-install erlang or
# the distro `elixir` package (it pins an old erlang).

if [ "${SKIP_ERLANG:-}" = "1" ] && [ "${SKIP_ELIXIR:-}" = "1" ]; then
  warn "Skipping BEAM toolchain (SKIP_ERLANG=1, SKIP_ELIXIR=1)"
elif [ ! -f "${TOOL_VERSIONS}" ]; then
  fail "No .tool-versions at ${TOOL_VERSIONS} — cannot resolve BEAM toolchain pins"
  exit 1
else
  # 1. mise itself.
  if have mise; then
    MISE_BIN="$(command -v mise)"
    ok "mise already installed ($("${MISE_BIN}" --version 2>/dev/null | head -1))"
  elif [ -x /usr/local/bin/mise ]; then
    MISE_BIN=/usr/local/bin/mise
    ok "mise already installed ($("${MISE_BIN}" --version 2>/dev/null | head -1))"
  else
    info "Installing mise ${MISE_VERSION}..."
    curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 https://mise.run \
      | MISE_VERSION="${MISE_VERSION}" MISE_INSTALL_PATH=/usr/local/bin/mise sh
    MISE_BIN=/usr/local/bin/mise
    ok "mise installed"
  fi

  # Record trust in mise's state dir too (belt-and-suspenders alongside
  # MISE_TRUSTED_CONFIG_PATHS) so a bare `mise` invocation by the user — in a
  # shell that didn't source the env var — still resolves the pinned toolchain.
  "${MISE_BIN}" trust "${REPO_ROOT}" >/dev/null 2>&1 || true

  # 2. Erlang + Elixir from .tool-versions. A GITHUB_TOKEN (if a valid one is in
  #    the env) lifts the unauthenticated GitHub API rate limit mise hits while
  #    resolving release lists; it degrades gracefully without one.
  export GITHUB_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}"

  # mise's precompiled OTP links against system libssl + libncurses at RUNTIME.
  # The devcontainer never needed these installed explicitly because its
  # rust:1-trixie base already ships them — but a bare cloud base image (ubuntu
  # noble) may not, so the downloaded `erl` extracts fine (no mise warning) yet
  # fails to start with a shared-library loader error, which then cascades into
  # "elixir/rebar3 NOT FOUND" at verify. Install the deps up front. Harmless if
  # already present; non-fatal so a flaky apt mirror doesn't abort setup.
  if [ "${SKIP_ERLANG:-}" != "1" ] && command -v apt-get >/dev/null 2>&1; then
    _OTP_DEPS=""
    for pkg in libssl-dev libncurses-dev; do
      dpkg -s "$pkg" >/dev/null 2>&1 || _OTP_DEPS="${_OTP_DEPS} $pkg"
    done
    if [ -n "${_OTP_DEPS}" ]; then
      info "Installing Erlang runtime deps (${_OTP_DEPS# })..."
      require_sudo
      # shellcheck disable=SC2086
      if $SUDO apt-get update -qq && $SUDO apt-get install -y -qq --no-install-recommends ${_OTP_DEPS}; then
        ok "Erlang runtime deps installed"
      else
        warn "Could not install Erlang runtime deps (${_OTP_DEPS# }) — erl may fail to start"
      fi
    fi
  fi

  MISE_TOOLS=""
  [ "${SKIP_ERLANG:-}" = "1" ] || MISE_TOOLS="${MISE_TOOLS} erlang"
  [ "${SKIP_ELIXIR:-}" = "1" ] || MISE_TOOLS="${MISE_TOOLS} elixir"
  info "Installing BEAM toolchain via mise (${MISE_TOOLS# }) — pinned by .tool-versions..."
  # shellcheck disable=SC2086
  (cd "${REPO_ROOT}" && "${MISE_BIN}" install ${MISE_TOOLS}) \
    || warn "mise install reported issues (continuing — erlang/elixir may still be present)"
  (cd "${REPO_ROOT}" && "${MISE_BIN}" reshim >/dev/null 2>&1) || true

  # 3. Put the mise shims first on PATH so erl/elixir/mix resolve to the pinned
  #    OTP — both for the rest of this script and (persisted below) every shell.
  case ":${PATH}:" in *":${MISE_SHIMS}:"*) ;; *) export PATH="${MISE_SHIMS}:${PATH}" ;; esac
  hash -r 2>/dev/null || true

  # 3b. Functional smoke test: a precompiled OTP can download cleanly yet fail to
  #     *run* if a runtime lib is still missing. Surface the real loader error
  #     here (the `if` condition exempts this from set -e) instead of letting it
  #     resurface as a cryptic "elixir NOT FOUND" 200 lines later at verify.
  if [ "${SKIP_ERLANG:-}" != "1" ]; then
    if _erl_smoke="$(erl -noshell -eval 'halt(0).' 2>&1)"; then
      ok "erl runs"
    else
      fail "erl installed but cannot start — toolchain will not work:"
      printf '%s\n' "${_erl_smoke}" | sed 's/^/      /'
      warn "Usually a missing runtime library (libssl/libncurses/libtinfo); install the lib it names and re-run."
    fi
  fi

  # 4. rebar3 — install the pinned escript directly. mise's rebar backend lists
  #    releases through the GitHub *API* (rate-limited to 60/h without a token);
  #    the release *asset* download URL is not rate-limited, so this is the
  #    reliable path in a token-less sandbox.
  REBAR3_PIN="$(awk '/^rebar[[:space:]]/{print $2}' "${TOOL_VERSIONS}")"
  if [ -z "${REBAR3_PIN}" ]; then
    warn "No 'rebar' pin in .tool-versions — skipping rebar3 install"
  elif rebar3 --version 2>/dev/null | grep -q "rebar ${REBAR3_PIN} "; then
    ok "rebar3 ${REBAR3_PIN} already installed"
  else
    info "Installing rebar3 ${REBAR3_PIN} (escript)..."
    require_sudo
    _REBAR3_TMP="$(mktemp)"
    curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
      "https://github.com/erlang/rebar3/releases/download/${REBAR3_PIN}/rebar3" -o "${_REBAR3_TMP}"
    $SUDO install -m 755 "${_REBAR3_TMP}" /usr/local/bin/rebar3
    rm -f "${_REBAR3_TMP}"
    hash -r 2>/dev/null || true
    ok "rebar3 ${REBAR3_PIN} installed"
  fi

  # 5. Persist MISE_DATA_DIR + shims PATH + the Elixir UTF-8 guard for every
  #    future shell. The setup script's filesystem changes are snapshotted by
  #    the cloud cache, so this lands the toolchain on PATH at session start.
  require_sudo
  $SUDO tee /etc/profile.d/beamtalk-mise.sh > /dev/null << PROFILE
# Beamtalk BEAM toolchain (managed by scripts/setup-cloud.sh) — do not edit.
export MISE_DATA_DIR="${MISE_DATA_DIR}"
case ":\${PATH}:" in
  *":${MISE_SHIMS}:"*) ;;
  *) export PATH="${MISE_SHIMS}:\${PATH}" ;;
esac
# Trust the repo's mise.toml without depending on mise's snapshot-fragile trust
# store, so .tool-versions always resolves the pinned BEAM toolchain.
case ":\${MISE_TRUSTED_CONFIG_PATHS:-}:" in
  *":${REPO_ROOT}:"*) ;;
  *) export MISE_TRUSTED_CONFIG_PATHS="${REPO_ROOT}\${MISE_TRUSTED_CONFIG_PATHS:+:\${MISE_TRUSTED_CONFIG_PATHS}}" ;;
esac
export ELIXIR_ERL_OPTIONS="\${ELIXIR_ERL_OPTIONS:-+fnu}"
PROFILE
  ok "Toolchain PATH persisted to /etc/profile.d/beamtalk-mise.sh"

  # 5b. Symlink the pinned BEAM tools into /usr/local/bin. The /etc/profile.d
  #     export above only reaches LOGIN shells. The agent harness runs build
  #     commands in NON-login, NON-interactive shells whose PATH it controls
  #     directly — it sources neither /etc/profile.d nor ~/.bashrc, so a
  #     profile-only install left escript/erl/elixir off PATH and broke
  #     `just build` mid-session (rebar3 only worked because it is installed
  #     into /usr/local/bin as a real binary). /usr/local/bin IS on that bare
  #     PATH, so linking the shims there is what actually makes the toolchain
  #     resolve for harness builds. The shims still defer to the .tool-versions
  #     pin at run time, so this does not bypass version management.
  _linked=()
  for _shim in "${MISE_SHIMS}"/*; do
    [[ -f "${_shim}" && -x "${_shim}" ]] || continue
    _name="$(basename "${_shim}")"
    case "${_name}" in *.ps1) continue ;; esac
    _dest="/usr/local/bin/${_name}"
    # Never clobber a real binary already in /usr/local/bin (e.g. rebar3); only
    # create/refresh symlinks that we own.
    [[ -e "${_dest}" && ! -L "${_dest}" ]] && continue
    $SUDO ln -sf "${_shim}" "${_dest}" && _linked+=("${_name}")
  done
  ok "Linked BEAM tools into /usr/local/bin (${#_linked[@]}): ${_linked[*]}"

  # 6. Decommission the stale esl-erlang baked into the cloud base image. It
  #    ships an old OTP (27.x) from the Erlang Solutions apt repo and installs
  #    /usr/bin/erl, which shadows the mise-pinned OTP for any shell that hasn't
  #    sourced the profile.d shims — and silently passed the old presence-only
  #    verify. We install the BEAM toolchain exclusively via mise now, so purge
  #    the package and its apt source to stop it from ever coming back.
  if [ "${SKIP_ERLANG:-}" != "1" ] && dpkg -s esl-erlang &>/dev/null; then
    info "Removing stale esl-erlang (apt OTP) — toolchain is mise-managed now..."
    require_sudo
    $SUDO apt-get purge -y -qq esl-erlang >/dev/null 2>&1 || true
    $SUDO apt-get autoremove -y -qq >/dev/null 2>&1 || true
    $SUDO rm -f /etc/apt/sources.list.d/erlang-solutions.list \
                /etc/apt/keyrings/erlang-solutions.gpg 2>/dev/null || true
    hash -r 2>/dev/null || true
    if dpkg -s esl-erlang &>/dev/null; then
      warn "esl-erlang still present after purge"
    else
      ok "esl-erlang removed"
    fi
  fi
fi

# --- System packages ---

info "Installing system packages..."
case "$OS_ID" in
  ubuntu|debian)
    PKGS=""
    for pkg in git gettext-base curl wget jq htop strace socat netcat-openbsd lsof unzip shellcheck; do
      if ! dpkg -s "$pkg" &>/dev/null; then
        PKGS="$PKGS $pkg"
      fi
    done
    if [ -n "$PKGS" ]; then
      require_sudo
      $SUDO apt-get update -qq
      # shellcheck disable=SC2086
      $SUDO apt-get install -y -qq --no-install-recommends $PKGS
      ok "System packages installed"
    else
      ok "System packages already present"
    fi
    ;;
  *)
    warn "Skipping system packages on ${OS_ID} — install manually if needed"
    ;;
esac

# --- Node.js ---

if [ "${SKIP_NODE:-}" = "1" ]; then
  warn "Skipping Node.js (SKIP_NODE=1)"
elif have node; then
  ok "Node.js already installed ($(node --version))"
else
  info "Installing Node.js LTS..."
  require_sudo
  _NODE_TMP="$(mktemp)"
  curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
    https://deb.nodesource.com/setup_lts.x -o "${_NODE_TMP}"
  $SUDO bash "${_NODE_TMP}"
  rm -f "${_NODE_TMP}"
  $SUDO apt-get install -y -qq nodejs
  ok "Node.js $(node --version) installed"
fi

# --- GitHub CLI ---

if have gh; then
  ok "GitHub CLI already installed ($(gh --version | head -1))"
else
  info "Installing GitHub CLI..."
  require_sudo
  curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
    https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    | $SUDO dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg 2>/dev/null
  $SUDO chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
  echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
    | $SUDO tee /etc/apt/sources.list.d/github-cli.list > /dev/null
  $SUDO apt-get update -qq
  $SUDO apt-get install -y -qq gh
  ok "GitHub CLI installed"
fi

# --- Rust toolchain ---

if have rustc; then
  ok "Rust already installed ($(rustc --version))"
else
  info "Installing Rust..."
  curl --proto '=https' --tlsv1.2 -sSf \
    --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
    https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
  # shellcheck disable=SC1091
  source "${CARGO_HOME:-$HOME/.cargo}/env"
  ok "Rust $(rustc --version) installed"
fi

# --- Cargo tools ---

if [ "${SKIP_RUST_TOOLS:-}" = "1" ]; then
  warn "Skipping Rust tools (SKIP_RUST_TOOLS=1)"
else
  info "Installing cargo tools..."

  # cargo-binstall for fast binary installs
  if ! have cargo-binstall; then
    curl -L --proto '=https' --tlsv1.2 -sSf \
      --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
      https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash
    ok "cargo-binstall installed"
  else
    ok "cargo-binstall already installed"
  fi

  # cargo-nextest refuses ANY source build without --locked (its locked-tripwire
  # crate compile_error!s). cargo-binstall's compile fallback omits --locked, so a
  # transient prebuilt-fetch failure turns into a hard build error (exit 70).
  # Install nextest from its official prebuilt CDN — no GitHub release API, no
  # compile — and keep it out of the binstall batch entirely.
  if have cargo-nextest; then
    ok "cargo-nextest already installed"
  else
    info "Installing cargo-nextest (prebuilt from get.nexte.st)..."
    _CARGO_BIN="${CARGO_HOME:-$HOME/.cargo}/bin"
    mkdir -p "${_CARGO_BIN}"
    curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
      https://get.nexte.st/latest/linux | tar -xz -C "${_CARGO_BIN}" cargo-nextest
    ok "cargo-nextest installed"
  fi

  CARGO_TOOLS="cargo-watch cargo-insta cargo-llvm-cov cargo-fuzz just"
  MISSING_TOOLS=""
  for tool in $CARGO_TOOLS; do
    # just and cargo-watch have matching binary names; the rest match the crate.
    case "$tool" in
      just)        bin_name="just" ;;
      cargo-watch) bin_name="cargo-watch" ;;
      *)           bin_name="$tool" ;;
    esac
    if ! have "$bin_name"; then
      MISSING_TOOLS="$MISSING_TOOLS $tool"
    else
      ok "$tool already installed"
    fi
  done

  if [ -n "$MISSING_TOOLS" ]; then
    # Prefer binstall's prebuilt binaries; never let it fall back to an unlocked
    # source compile (slow, and breaks crates that demand --locked). If no
    # prebuilt is available, do a controlled source build with --locked.
    # shellcheck disable=SC2086
    if cargo binstall -y --disable-strategies compile $MISSING_TOOLS; then
      ok "Cargo tools installed (binstall)"
    else
      warn "binstall could not fetch prebuilt binaries — building from source (--locked)"
      for tool in $MISSING_TOOLS; do
        cargo install --locked "$tool"
      done
      ok "Cargo tools installed (source)"
    fi
  fi
fi

# Linear integration is handled by the connected Linear MCP server (no local
# CLI needed). The old streamlinear-cli install — a git clone + npm install on
# every cold start — was removed once the skills moved to the MCP tools.

# Install Hex + register the system rebar3 with Mix so `mix deps.get` works with
# no further setup. `mix local.hex` fetches from builds.hex.pm, which some MITM
# egress proxies block (cloud sandboxes) — fall back to installing Hex from
# GitHub in that case. +fnu guards against a C locale tripping Elixir's UTF-8
# expectation during these calls.
if have mix; then
  export ELIXIR_ERL_OPTIONS="${ELIXIR_ERL_OPTIONS:-+fnu}"
  if mix local.hex --force >/dev/null 2>&1; then
    ok "Hex installed (builds.hex.pm)"
  elif mix archive.install github hexpm/hex branch latest --force >/dev/null 2>&1; then
    ok "Hex installed (GitHub fallback)"
  else
    warn "Could not install Hex — 'mix deps.get' may need manual Hex setup"
  fi
  if have rebar3; then
    if mix local.rebar rebar3 "$(command -v rebar3)" --force >/dev/null 2>&1; then
      ok "Registered system rebar3 with Mix"
    else
      warn "Could not register rebar3 with Mix"
    fi
  fi
fi

# --- Skills repo (Claude Code skills & agents) ---

if [ "${SKIP_SKILLS:-}" = "1" ]; then
  warn "Skipping skills setup (SKIP_SKILLS=1)"
else
  info "Setting up Claude Code skills..."
  # init.sh clones the skills repo into .claude/skills-repo and links
  # skills/agents into .claude/commands/ and .claude/agents/.
  # It's idempotent — pulls latest if already cloned.
  curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 --connect-timeout 20 \
    https://raw.githubusercontent.com/jamesc/skills/main/scripts/init.sh | bash
  ok "Skills and agents linked"
fi

# --- Verify ---

echo ""
info "Verifying installations..."
ERRORS=0

# The verify block is purely diagnostic: every capture is meant to degrade to an
# empty string (→ "NOT FOUND") and be tallied in ERRORS, with a single exit at
# the end. errexit/pipefail defeat that — a `$(awk … missing-file)` (exit 2) or a
# version pipeline whose grep finds nothing (exit 1) would abort the whole script
# with NO output before any check prints. Disable them for this section so verify
# always runs to completion and reports what actually failed.
set +e +o pipefail

# Read the pins so the assertions stay in lockstep with .tool-versions /
# rust-toolchain.toml — the single sources of truth.
PIN_ERLANG="$(awk '/^erlang[[:space:]]/{print $2}' "${TOOL_VERSIONS}" 2>/dev/null)"
PIN_ELIXIR="$(awk '/^elixir[[:space:]]/{print $2}' "${TOOL_VERSIONS}" 2>/dev/null)"
PIN_REBAR="$(awk '/^rebar[[:space:]]/{print $2}' "${TOOL_VERSIONS}" 2>/dev/null)"
PIN_RUST="$(awk -F'"' '/^[[:space:]]*channel[[:space:]]*=/{print $2}' "${REPO_ROOT}/rust-toolchain.toml" 2>/dev/null)"
# Elixir's pin carries an -otp-NN suffix that `elixir --version` never prints,
# so compare against just the numeric Elixir version (e.g. 1.20.1-otp-28 -> 1.20.1).
PIN_ELIXIR_NUM="${PIN_ELIXIR%%-otp-*}"

# check_present NAME            — assert a command exists (no pin).
check_present() {
  if have "$1"; then
    ok "$1"
  else
    fail "$1 NOT FOUND"
    ERRORS=$((ERRORS + 1))
  fi
}

# check_version NAME EXPECTED ACTUAL — assert ACTUAL matches the EXPECTED pin.
# An empty EXPECTED means there is no pin to check, so fall back to presence.
check_version() {
  local name="$1" expected="$2" actual="$3"
  if [ -z "$actual" ]; then
    fail "${name} NOT FOUND"
    ERRORS=$((ERRORS + 1))
  elif [ -z "$expected" ]; then
    warn "${name} ${actual} (no pin to verify)"
  elif [ "$actual" = "$expected" ]; then
    ok "${name} ${actual} (matches pin)"
  else
    fail "${name} ${actual} != pinned ${expected}"
    ERRORS=$((ERRORS + 1))
  fi
}

# Pinned BEAM + Rust toolchain — assert the running binary equals the pin, not
# just that *some* build is on PATH (an apt esl-erlang would otherwise mask a
# mise toolchain that never installed).
if have erl; then
  ERL_VER="$(erl -noshell -eval \
    '{ok,V}=file:read_file(filename:join([code:root_dir(),"releases",erlang:system_info(otp_release),"OTP_VERSION"])),io:format("~s",[string:trim(V)]),halt().' \
    2>/dev/null)"
else
  ERL_VER=""
fi
check_version "erlang" "${PIN_ERLANG}" "${ERL_VER}"

if have elixir; then
  ELIXIR_VER="$(elixir --version 2>/dev/null | grep -o 'Elixir [0-9][0-9.]*' | awk '{print $2}')"
else
  ELIXIR_VER=""
fi
check_version "elixir" "${PIN_ELIXIR_NUM}" "${ELIXIR_VER}"

if have rebar3; then
  REBAR_VER="$(rebar3 --version 2>/dev/null | awk '/^rebar[[:space:]]/{print $2}')"
else
  REBAR_VER=""
fi
check_version "rebar3" "${PIN_REBAR}" "${REBAR_VER}"

# rustc: rust-toolchain.toml pins the toolchain rustup auto-installs per-project,
# so the *ambient* rustc (often the base image's, e.g. 1.94.1 vs a 1.94.0 pin)
# legitimately differs and the repo still builds with the pinned one. Treat a
# mismatch as a note, not a hard failure — only genuine absence is an error.
if have rustc; then
  RUST_VER="$(rustc --version 2>/dev/null | awk '{print $2}')"
else
  RUST_VER=""
fi
if [ -z "${RUST_VER}" ]; then
  fail "rustc NOT FOUND"
  ERRORS=$((ERRORS + 1))
elif [ -n "${PIN_RUST}" ] && [ "${RUST_VER}" != "${PIN_RUST}" ]; then
  warn "rustc ${RUST_VER} (ambient; rust-toolchain.toml pins ${PIN_RUST} per-project via rustup)"
else
  ok "rustc ${RUST_VER}"
fi

# Remaining tools are presence-only. node is installed via NodeSource LTS (not
# mise-pinned in the cloud path, and skipped when already present), so the
# .tool-versions nodejs pin isn't a guarantee here — asserting it would fail
# spuriously on any LTS patch drift. mix tracks elixir, npm tracks node, cargo
# tracks rustc.
for cmd in cargo mix node npm gh just; do
  check_present "$cmd"
done

echo ""
if [ "$ERRORS" -gt 0 ]; then
  fail "${ERRORS} tool(s) missing or not matching pins"
  exit 1
else
  info "All dependencies installed successfully!"
  echo ""
  echo "  Next steps:"
  echo "    cd $(pwd)"
  echo "    just build"
  echo "    just test"
  echo ""
fi

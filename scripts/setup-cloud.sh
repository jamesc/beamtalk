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

# Repo root (the directory containing .tool-versions). The hook invokes this
# script from the checkout; fall back to the script's own location.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${CLAUDE_PROJECT_DIR:-$(cd "${SCRIPT_DIR}/.." && pwd)}"
TOOL_VERSIONS="${REPO_ROOT}/.tool-versions"

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
    curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 https://mise.run \
      | MISE_VERSION="${MISE_VERSION}" MISE_INSTALL_PATH=/usr/local/bin/mise sh
    MISE_BIN=/usr/local/bin/mise
    ok "mise installed"
  fi

  # 2. Erlang + Elixir from .tool-versions. A GITHUB_TOKEN (if a valid one is in
  #    the env) lifts the unauthenticated GitHub API rate limit mise hits while
  #    resolving release lists; it degrades gracefully without one.
  export GITHUB_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}"
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
    curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 \
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
export ELIXIR_ERL_OPTIONS="\${ELIXIR_ERL_OPTIONS:-+fnu}"
PROFILE
  ok "Toolchain PATH persisted to /etc/profile.d/beamtalk-mise.sh"
fi

# --- System packages ---

info "Installing system packages..."
case "$OS_ID" in
  ubuntu|debian)
    PKGS=""
    for pkg in git gettext-base curl wget jq htop strace socat netcat-openbsd lsof unzip; do
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
  curl -fsSL https://deb.nodesource.com/setup_lts.x -o "${_NODE_TMP}"
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
  curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
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
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
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
      https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash
    ok "cargo-binstall installed"
  else
    ok "cargo-binstall already installed"
  fi

  CARGO_TOOLS="cargo-watch cargo-nextest cargo-insta cargo-llvm-cov cargo-fuzz just"
  MISSING_TOOLS=""
  for tool in $CARGO_TOOLS; do
    bin_name="${tool#cargo-}"
    # just and cargo-watch have matching binary names; nextest is 'cargo nextest' etc.
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
    # shellcheck disable=SC2086
    cargo binstall -y $MISSING_TOOLS
    ok "Cargo tools installed"
  fi
fi

# --- streamlinear CLI ---

if have streamlinear-cli; then
  ok "streamlinear-cli already installed"
else
  info "Installing streamlinear CLI..."
  # npm install -g from a GitHub URL leaves dangling symlinks because npm clones
  # into a temp dir that gets cleaned up. npm link fails because the prepare
  # script calls simple-git-hooks. Instead, clone to a permanent location,
  # install mcp deps, and manually symlink the binaries.
  STREAMLINEAR_DIR="/opt/streamlinear"
  require_sudo
  if [[ ! -d "${STREAMLINEAR_DIR}" ]]; then
    $SUDO git clone --depth 1 https://github.com/obra/streamlinear.git "${STREAMLINEAR_DIR}"
  fi
  if (cd "${STREAMLINEAR_DIR}/mcp" && $SUDO npm install --ignore-scripts 2>/dev/null); then
    NPM_PREFIX="$(npm prefix -g)"
    $SUDO ln -sf "${STREAMLINEAR_DIR}" "${NPM_PREFIX}/lib/node_modules/streamlinear"
    $SUDO ln -sf "${STREAMLINEAR_DIR}/mcp/dist/index.js" "${NPM_PREFIX}/bin/streamlinear"
    $SUDO ln -sf "${STREAMLINEAR_DIR}/mcp/dist/cli.js" "${NPM_PREFIX}/bin/streamlinear-cli"
    $SUDO chmod +x "${STREAMLINEAR_DIR}/mcp/dist/index.js" "${STREAMLINEAR_DIR}/mcp/dist/cli.js"
    ok "streamlinear-cli installed"
  else
    warn "streamlinear-cli installation failed — Linear skills won't be available"
  fi
fi

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
  curl -fsSL https://raw.githubusercontent.com/jamesc/skills/main/scripts/init.sh | bash
  ok "Skills and agents linked"
fi

# --- Verify ---

echo ""
info "Verifying installations..."
ERRORS=0

for cmd in rustc cargo erl elixir mix node npm gh just rebar3; do
  if have "$cmd"; then
    ok "$cmd"
  else
    fail "$cmd NOT FOUND"
    ERRORS=$((ERRORS + 1))
  fi
done

echo ""
if [ "$ERRORS" -gt 0 ]; then
  fail "${ERRORS} tool(s) failed to install"
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

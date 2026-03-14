#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

# Beamtalk Cloud Development Environment Setup
#
# Installs the same dependencies as .devcontainer/Dockerfile for use in
# cloud environments (e.g. Claude Code cloud sessions).
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/jamesc/beamtalk/main/scripts/setup-cloud.sh | bash
#
# Or run locally:
#   ./scripts/setup-cloud.sh
#
# Environment variables:
#   REBAR3_VERSION  - rebar3 version to install (default: 3.26.0)
#   SKIP_RUST_TOOLS - set to 1 to skip cargo tool installation
#   SKIP_ERLANG     - set to 1 to skip Erlang installation
#   SKIP_NODE       - set to 1 to skip Node.js installation
#   SKIP_SKILLS     - set to 1 to skip Claude Code skills setup

REBAR3_VERSION="${REBAR3_VERSION:-3.26.0}"

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

need_sudo() {
  if [ "$(id -u)" -ne 0 ]; then
    if have sudo; then
      echo "sudo"
    else
      fail "Root privileges required but sudo not available"
      exit 1
    fi
  fi
}

SUDO="$(need_sudo)"

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

# --- Erlang/OTP 27 ---

if [ "${SKIP_ERLANG:-}" = "1" ]; then
  warn "Skipping Erlang (SKIP_ERLANG=1)"
elif have erl; then
  OTP_VSN=$(erl -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().' -noshell 2>/dev/null || echo "unknown")
  ok "Erlang/OTP already installed (OTP ${OTP_VSN})"
else
  info "Installing Erlang/OTP 27..."
  case "$OS_ID" in
    ubuntu|debian)
      $SUDO apt-get update -qq
      $SUDO apt-get install -y -qq --no-install-recommends ca-certificates gnupg curl
      $SUDO mkdir -p /etc/apt/keyrings
      curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 \
        https://binaries2.erlang-solutions.com/GPG-KEY-pmanager.asc \
        -o /tmp/GPG-KEY-pmanager.asc
      $SUDO gpg --dearmor -o /etc/apt/keyrings/erlang-solutions.gpg /tmp/GPG-KEY-pmanager.asc
      rm -f /tmp/GPG-KEY-pmanager.asc
      CODENAME="${VERSION_CODENAME:-bookworm}"
      echo "deb [signed-by=/etc/apt/keyrings/erlang-solutions.gpg] http://binaries2.erlang-solutions.com/debian/ ${CODENAME}-esl-erlang-27 contrib" \
        | $SUDO tee /etc/apt/sources.list.d/erlang-solutions.list > /dev/null
      $SUDO apt-get update -qq
      $SUDO apt-get install -y -qq --no-install-recommends esl-erlang
      ok "Erlang/OTP 27 installed"
      ;;
    *)
      fail "Unsupported OS for Erlang installation: ${OS_ID}"
      fail "Install Erlang/OTP 27 manually and re-run with SKIP_ERLANG=1"
      exit 1
      ;;
  esac
fi

# --- System packages ---

info "Installing system packages..."
case "$OS_ID" in
  ubuntu|debian)
    PKGS=""
    for pkg in git gettext-base curl wget jq htop strace socat netcat-openbsd lsof; do
      if ! dpkg -s "$pkg" &>/dev/null; then
        PKGS="$PKGS $pkg"
      fi
    done
    if [ -n "$PKGS" ]; then
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
  curl -fsSL https://deb.nodesource.com/setup_lts.x | $SUDO bash -
  $SUDO apt-get install -y -qq nodejs
  ok "Node.js $(node --version) installed"
fi

# --- GitHub CLI ---

if have gh; then
  ok "GitHub CLI already installed ($(gh --version | head -1))"
else
  info "Installing GitHub CLI..."
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
  npm install -g streamlinear@github:obra/streamlinear --ignore-scripts
  ok "streamlinear-cli installed"
fi

# --- rebar3 ---

if have rebar3; then
  ok "rebar3 already installed ($(rebar3 --version))"
else
  info "Installing rebar3 v${REBAR3_VERSION}..."
  TMPDIR="$(mktemp -d)"
  git clone --depth 1 --branch "${REBAR3_VERSION}" https://github.com/erlang/rebar3.git "$TMPDIR/rebar3"
  (cd "$TMPDIR/rebar3" && ./bootstrap)
  $SUDO install -m 755 "$TMPDIR/rebar3/rebar3" /usr/local/bin/rebar3
  rm -rf "$TMPDIR"
  ok "rebar3 v${REBAR3_VERSION} installed"
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

for cmd in rustc cargo erl node npm gh just rebar3; do
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

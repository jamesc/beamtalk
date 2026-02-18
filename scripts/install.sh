#!/bin/sh
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Beamtalk installer script
#
# Usage:
#   curl --proto '=https' -LsSf https://raw.githubusercontent.com/jamesc/beamtalk/main/scripts/install.sh | sh
#   curl ... | sh -s -- --version v0.1.0         # specific version
#   curl ... | sh -s -- --prefix /usr/local       # custom install location
#
# Environment variables:
#   BEAMTALK_VERSION   - version to install (default: latest)
#   BEAMTALK_PREFIX    - install prefix (default: ~/.beamtalk)

set -eu

REPO="jamesc/beamtalk"
DEFAULT_PREFIX="${HOME}/.beamtalk"
GITHUB_API="https://api.github.com"
GITHUB_RELEASES="https://github.com/${REPO}/releases"

# ─── Parse arguments ────────────────────────────────────────────────────

VERSION="${BEAMTALK_VERSION:-}"
PREFIX="${BEAMTALK_PREFIX:-${DEFAULT_PREFIX}}"

while [ $# -gt 0 ]; do
    case "$1" in
        --version)  VERSION="$2"; shift 2 ;;
        --prefix)   PREFIX="$2"; shift 2 ;;
        --help)
            echo "Usage: install.sh [--version VERSION] [--prefix PATH]"
            echo ""
            echo "Options:"
            echo "  --version VERSION   Install a specific version (e.g., v0.1.0)"
            echo "  --prefix PATH       Install to PATH (default: ~/.beamtalk)"
            echo ""
            echo "Environment variables:"
            echo "  BEAMTALK_VERSION    Same as --version"
            echo "  BEAMTALK_PREFIX     Same as --prefix"
            exit 0
            ;;
        *)  echo "Unknown option: $1"; exit 1 ;;
    esac
done

# ─── Detect platform ────────────────────────────────────────────────────

detect_platform() {
    OS="$(uname -s)"
    ARCH="$(uname -m)"

    case "${OS}" in
        Linux)   PLATFORM_OS="linux" ;;
        Darwin)  PLATFORM_OS="macos" ;;
        MINGW*|MSYS*|CYGWIN*)  PLATFORM_OS="windows" ;;
        *)  echo "Error: Unsupported operating system: ${OS}"; exit 1 ;;
    esac

    case "${ARCH}" in
        x86_64|amd64)   PLATFORM_ARCH="x86_64" ;;
        aarch64|arm64)  PLATFORM_ARCH="arm64" ;;
        *)  echo "Error: Unsupported architecture: ${ARCH}"; exit 1 ;;
    esac

    # Only certain platform combinations are supported
    case "${PLATFORM_OS}-${PLATFORM_ARCH}" in
        linux-x86_64)   PLATFORM="linux-x86_64"; EXT="tar.gz" ;;
        macos-x86_64)   PLATFORM="macos-x86_64"; EXT="tar.gz" ;;
        macos-arm64)    PLATFORM="macos-arm64"; EXT="tar.gz" ;;
        windows-x86_64) PLATFORM="windows-x86_64"; EXT="zip" ;;
        *)  echo "Error: Unsupported platform: ${PLATFORM_OS}-${PLATFORM_ARCH}"; exit 1 ;;
    esac
}

# ─── Determine version ──────────────────────────────────────────────────

resolve_version() {
    if [ -n "${VERSION}" ]; then
        # Strip leading 'v' if present for consistency, then add it back
        VERSION="${VERSION#v}"
        TAG="v${VERSION}"
        return
    fi

    echo "Fetching latest release..."
    if command -v curl >/dev/null 2>&1; then
        TAG=$(curl -sS "${GITHUB_API}/repos/${REPO}/releases/latest" | grep '"tag_name"' | head -1 | sed 's/.*"tag_name": *"\([^"]*\)".*/\1/')
    elif command -v wget >/dev/null 2>&1; then
        TAG=$(wget -qO- "${GITHUB_API}/repos/${REPO}/releases/latest" | grep '"tag_name"' | head -1 | sed 's/.*"tag_name": *"\([^"]*\)".*/\1/')
    else
        echo "Error: curl or wget required"
        exit 1
    fi

    if [ -z "${TAG}" ]; then
        echo "Error: Could not determine latest version"
        exit 1
    fi

    VERSION="${TAG#v}"
}

# ─── Download and verify ────────────────────────────────────────────────

download() {
    URL="$1"
    DEST="$2"
    if command -v curl >/dev/null 2>&1; then
        curl --proto '=https' -LsSf "${URL}" -o "${DEST}"
    elif command -v wget >/dev/null 2>&1; then
        wget -qO "${DEST}" "${URL}"
    else
        echo "Error: curl or wget required"
        exit 1
    fi
}

verify_checksum() {
    ARCHIVE="$1"
    CHECKSUM_FILE="$2"

    if [ ! -f "${CHECKSUM_FILE}" ]; then
        echo "Warning: No checksum file found, skipping verification"
        return 0
    fi

    EXPECTED=$(awk '{print $1}' "${CHECKSUM_FILE}")

    if command -v sha256sum >/dev/null 2>&1; then
        ACTUAL=$(sha256sum "${ARCHIVE}" | awk '{print $1}')
    elif command -v shasum >/dev/null 2>&1; then
        ACTUAL=$(shasum -a 256 "${ARCHIVE}" | awk '{print $1}')
    else
        echo "Warning: sha256sum/shasum not found, skipping verification"
        return 0
    fi

    if [ "${EXPECTED}" != "${ACTUAL}" ]; then
        echo "Error: Checksum verification failed!"
        echo "  Expected: ${EXPECTED}"
        echo "  Actual:   ${ACTUAL}"
        rm -f "${ARCHIVE}" "${CHECKSUM_FILE}"
        exit 1
    fi

    echo "✅ Checksum verified"
}

# ─── Install ────────────────────────────────────────────────────────────

install_beamtalk() {
    ARCHIVE_NAME="beamtalk-${VERSION}-${PLATFORM}.${EXT}"
    ARCHIVE_URL="${GITHUB_RELEASES}/download/${TAG}/${ARCHIVE_NAME}"
    CHECKSUM_URL="${ARCHIVE_URL}.sha256"

    TMPDIR=$(mktemp -d)
    trap 'rm -rf "${TMPDIR}"' EXIT

    echo "Downloading beamtalk ${VERSION} for ${PLATFORM}..."
    download "${ARCHIVE_URL}" "${TMPDIR}/${ARCHIVE_NAME}"
    download "${CHECKSUM_URL}" "${TMPDIR}/${ARCHIVE_NAME}.sha256" 2>/dev/null || true

    verify_checksum "${TMPDIR}/${ARCHIVE_NAME}" "${TMPDIR}/${ARCHIVE_NAME}.sha256"

    echo "Installing to ${PREFIX}..."
    mkdir -p "${PREFIX}"

    case "${EXT}" in
        tar.gz)
            tar -xzf "${TMPDIR}/${ARCHIVE_NAME}" -C "${PREFIX}"
            ;;
        zip)
            if command -v unzip >/dev/null 2>&1; then
                unzip -qo "${TMPDIR}/${ARCHIVE_NAME}" -d "${PREFIX}"
            elif command -v 7z >/dev/null 2>&1; then
                7z x -o"${PREFIX}" -y "${TMPDIR}/${ARCHIVE_NAME}" >/dev/null
            else
                echo "Error: unzip or 7z required to extract .zip archives"
                exit 1
            fi
            ;;
    esac
}

# ─── Configure PATH ─────────────────────────────────────────────────────

configure_path() {
    BIN_DIR="${PREFIX}/bin"

    # Check if already in PATH
    case ":${PATH}:" in
        *":${BIN_DIR}:"*) return ;;
    esac

    echo ""
    echo "Add beamtalk to your PATH by adding this to your shell profile:"
    echo ""

    SHELL_NAME="$(basename "${SHELL:-/bin/sh}")"
    case "${SHELL_NAME}" in
        zsh)
            PROFILE="${HOME}/.zshrc"
            ;;
        bash)
            if [ -f "${HOME}/.bashrc" ]; then
                PROFILE="${HOME}/.bashrc"
            else
                PROFILE="${HOME}/.bash_profile"
            fi
            ;;
        fish)
            echo "  set -Ux fish_user_paths ${BIN_DIR} \$fish_user_paths"
            echo ""
            echo "Or run: fish_add_path ${BIN_DIR}"
            return
            ;;
        *)
            PROFILE="${HOME}/.profile"
            ;;
    esac

    echo "  export PATH=\"${BIN_DIR}:\${PATH}\""
    echo ""
    echo "Or run:  echo 'export PATH=\"${BIN_DIR}:\${PATH}\"' >> ${PROFILE}"
}

# ─── Main ────────────────────────────────────────────────────────────────

main() {
    detect_platform
    resolve_version

    echo ""
    echo "  beamtalk installer"
    echo "  ─────────────────"
    echo "  Version:  ${VERSION}"
    echo "  Platform: ${PLATFORM}"
    echo "  Prefix:   ${PREFIX}"
    echo ""

    install_beamtalk

    echo ""
    echo "✅ beamtalk ${VERSION} installed successfully!"
    echo ""
    echo "  Binary:  ${PREFIX}/bin/beamtalk"
    echo "  LSP:     ${PREFIX}/bin/beamtalk-lsp"
    echo "  MCP:     ${PREFIX}/bin/beamtalk-mcp"
    echo "  Runtime: ${PREFIX}/lib/beamtalk/lib/"

    configure_path
}

main

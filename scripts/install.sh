#!/bin/sh
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Beamtalk installer script
#
# Usage:
#   curl -fsSL https://jamesc.github.io/beamtalk/install.sh | sh
#   curl ... | sh -s -- --version v0.1.0         # specific version
#   curl ... | sh -s -- --nightly                # latest nightly build
#   curl ... | sh -s -- --edge                   # latest main (linux-x86_64, macos-arm64)
#   curl ... | sh -s -- --prefix /usr/local       # custom install location
#   curl ... | sh -s -- --uninstall               # remove an installed copy
#   curl ... | sh -s -- --insecure-skip-checksum  # install without verifying
#                                                 # the archive's SHA-256
#
# Environment variables:
#   BEAMTALK_VERSION   - version to install (default: latest)
#   BEAMTALK_NIGHTLY   - set to 1 for nightly build
#   BEAMTALK_EDGE      - set to 1 for the edge build
#   BEAMTALK_PREFIX    - install prefix (default: ~/.beamtalk)
#   GITHUB_TOKEN       - optional; authenticates the "latest release" lookup
#                        (GH_TOKEN also works). Nightly and edge need no
#                        API call.

set -eu

REPO="jamesc/beamtalk"
DEFAULT_PREFIX="${HOME}/.beamtalk"
GITHUB_API="https://api.github.com"
GITHUB_RELEASES="https://github.com/${REPO}/releases"
# Targets the rolling edge release is built for: the `platform:` entries of
# .github/workflows/edge.yml's matrix. scripts/ci/test-install.sh checks the two
# lists are the same.
EDGE_PLATFORMS="linux-x86_64 macos-arm64"

# ─── Parse arguments ────────────────────────────────────────────────────

VERSION="${BEAMTALK_VERSION:-}"
NIGHTLY="${BEAMTALK_NIGHTLY:-}"
EDGE="${BEAMTALK_EDGE:-}"
PREFIX="${BEAMTALK_PREFIX:-${DEFAULT_PREFIX}}"
SKIP_CHECKSUM=""
UNINSTALL=""

while [ $# -gt 0 ]; do
    case "$1" in
        --version)
            if [ $# -lt 2 ]; then echo "Error: --version requires a value"; exit 1; fi
            VERSION="$2"; shift 2 ;;
        --nightly)
            NIGHTLY=1; shift ;;
        --edge)
            EDGE=1; shift ;;
        --prefix)
            if [ $# -lt 2 ]; then echo "Error: --prefix requires a value"; exit 1; fi
            PREFIX="$2"; shift 2 ;;
        --insecure-skip-checksum)
            SKIP_CHECKSUM=1; shift ;;
        --uninstall)
            UNINSTALL=1; shift ;;
        --help)
            echo "Usage: install.sh [--version VERSION] [--nightly | --edge] [--prefix PATH]"
            echo "                  [--insecure-skip-checksum] | --uninstall"
            echo ""
            echo "Options:"
            echo "  --version VERSION   Install a specific version (e.g., v0.1.0)"
            echo "  --nightly           Install the latest nightly build"
            echo "  --edge              Install the build of the latest main commit"
            echo "                      (linux-x86_64 and macos-arm64; for CI of downstream packages)"
            echo "  --prefix PATH       Install to PATH (default: ~/.beamtalk)"
            echo "  --insecure-skip-checksum"
            echo "                      Install even if the archive's SHA-256 cannot be fetched"
            echo "                      or verified (by default that is an error)"
            echo "  --uninstall         Remove beamtalk from PATH's prefix and exit"
            echo ""
            echo "Environment variables:"
            echo "  BEAMTALK_VERSION    Same as --version"
            echo "  BEAMTALK_NIGHTLY    Set to 1 for nightly build"
            echo "  BEAMTALK_EDGE       Set to 1 for edge build"
            echo "  BEAMTALK_PREFIX     Same as --prefix"
            echo "  GITHUB_TOKEN        Optional; authenticates the latest-release lookup"
            exit 0
            ;;
        *)  echo "Unknown option: $1"; exit 1 ;;
    esac
done

# The swap journal is `dest|slot` per line, so a `|` or newline in the prefix
# would truncate a rollback path.
case "${PREFIX}" in
    *'|'*|*'
'*) echo "Error: --prefix may not contain '|' or a newline"; exit 1 ;;
esac

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
    if [ -n "${VERSION}" ] && [ "${NIGHTLY}" = "1" ]; then
        echo "Error: --version and --nightly cannot be used together"
        exit 1
    fi
    if [ "${EDGE}" = "1" ] && { [ -n "${VERSION}" ] || [ "${NIGHTLY}" = "1" ]; }; then
        echo "Error: --edge cannot be used with --version or --nightly"
        exit 1
    fi

    if [ -n "${VERSION}" ]; then
        # Strip leading 'v' if present for consistency, then add it back
        VERSION="${VERSION#v}"
        # It is spliced into a download URL and a file name: no path or URL
        # metacharacters.
        case "${VERSION}" in
            ''|*[!A-Za-z0-9._+-]*|[!A-Za-z0-9]*)
                echo "Error: invalid version '${VERSION}'"
                exit 1 ;;
        esac
        TAG="v${VERSION}"
        return
    fi

    if [ "${NIGHTLY}" = "1" ]; then
        # The rolling nightly release always has tag `nightly` and versionless
        # asset names (beamtalk-nightly-<platform>.<ext>), so it is downloaded
        # directly. No GitHub API call: release downloads are not subject to
        # the API's unauthenticated rate limit (60 requests/hour per IP, shared
        # by CI runners), which used to fail installs with a misleading
        # "no nightly release" error.
        TAG="nightly"
        VERSION="nightly"
        return
    fi

    if [ "${EDGE}" = "1" ]; then
        # The rolling edge release (edge.yml) is rebuilt on every merge to
        # main, with the same versionless asset naming as nightly. It is built
        # for the targets in EDGE_PLATFORMS only (edge.yml's matrix; BT-5307).
        case " ${EDGE_PLATFORMS} " in
            *" ${PLATFORM} "*) ;;
            *)
                echo "Error: edge is not built for ${PLATFORM} (--edge covers: $(echo "${EDGE_PLATFORMS}" | sed 's/ /, /g'))."
                echo "  Use --nightly, or build from source with 'just dist'."
                exit 1 ;;
        esac
        TAG="edge"
        VERSION="edge"
        return
    fi

    echo "Fetching latest release..."
    RELEASE_URL="${GITHUB_API}/repos/${REPO}/releases/latest"
    # Authenticate when a token is available (GitHub Actions sets
    # GITHUB_TOKEN; `gh` users have GH_TOKEN): 1,000+ requests/hour instead of
    # 60 per IP.
    TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}"
    if command -v curl >/dev/null 2>&1; then
        if [ -n "${TOKEN}" ]; then
            RELEASE_JSON=$(curl -sS --proto '=https' --proto-redir '=https' --retry 3 -H "Authorization: Bearer ${TOKEN}" "${RELEASE_URL}")
        else
            RELEASE_JSON=$(curl -sS --proto '=https' --proto-redir '=https' --retry 3 "${RELEASE_URL}")
        fi
    elif command -v wget >/dev/null 2>&1; then
        require_https_only_wget
        if [ -n "${TOKEN}" ]; then
            RELEASE_JSON=$(wget -qO- --https-only --header="Authorization: Bearer ${TOKEN}" "${RELEASE_URL}")
        else
            RELEASE_JSON=$(wget -qO- --https-only "${RELEASE_URL}")
        fi
    else
        echo "Error: curl or wget required"
        exit 1
    fi

    # Parse tag_name from JSON (handles variable whitespace)
    TAG=$(echo "${RELEASE_JSON}" | sed -n 's/.*"tag_name" *: *"\([^"]*\)".*/\1/p' | head -1)

    if [ -z "${TAG}" ]; then
        # Surface GitHub's own error (e.g. "API rate limit exceeded ...")
        # rather than guessing at the cause.
        API_MESSAGE=$(echo "${RELEASE_JSON}" | sed -n 's/.*"message" *: *"\([^"]*\)".*/\1/p' | head -1)
        echo "Error: Could not determine the latest version from ${RELEASE_URL}"
        if [ -n "${API_MESSAGE}" ]; then
            echo "  GitHub API: ${API_MESSAGE}"
        fi
        echo "  Set GITHUB_TOKEN to authenticate, or pass --version VERSION."
        exit 1
    fi

    VERSION="${TAG#v}"
}

# ─── Download and verify ────────────────────────────────────────────────

# BT-5185: downloads must stay on https, redirects included. GNU wget has
# --https-only; BusyBox wget (Alpine) does not and would fail with an
# unrelated-looking usage error, so say what is missing instead.
require_https_only_wget() {
    if ! wget --help 2>&1 | grep -q -- '--https-only'; then
        echo "Error: this wget does not support --https-only (BusyBox wget?), so downloads"
        echo "  cannot be kept on https. Install curl (e.g. 'apk add curl') or GNU wget and retry."
        exit 1
    fi
}

download() {
    URL="$1"
    DEST="$2"
    if command -v curl >/dev/null 2>&1; then
        curl --proto '=https' --proto-redir '=https' -LsSf "${URL}" -o "${DEST}"
    elif command -v wget >/dev/null 2>&1; then
        require_https_only_wget
        wget -qO "${DEST}" --https-only "${URL}"
    else
        echo "Error: curl or wget required"
        exit 1
    fi
}

verify_checksum() {
    ARCHIVE="$1"
    CHECKSUM_FILE="$2"

    if [ "${SKIP_CHECKSUM}" = "1" ]; then
        echo "Warning: --insecure-skip-checksum given, NOT verifying the archive"
        return 0
    fi

    if [ ! -s "${CHECKSUM_FILE}" ]; then
        echo "Error: Could not fetch the checksum file (${CHECKSUM_URL})."
        echo "  Refusing to install an unverified archive."
        echo "  Retry, or pass --insecure-skip-checksum to install without verification."
        exit 1
    fi

    EXPECTED=$(awk '{print $1}' "${CHECKSUM_FILE}")

    if command -v sha256sum >/dev/null 2>&1; then
        ACTUAL=$(sha256sum "${ARCHIVE}" | awk '{print $1}')
    elif command -v shasum >/dev/null 2>&1; then
        ACTUAL=$(shasum -a 256 "${ARCHIVE}" | awk '{print $1}')
    else
        echo "Error: sha256sum/shasum not found, cannot verify the archive."
        echo "  Install one, or pass --insecure-skip-checksum to install without verification."
        exit 1
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
    if [ "${SKIP_CHECKSUM}" != "1" ]; then
        download "${CHECKSUM_URL}" "${TMPDIR}/${ARCHIVE_NAME}.sha256" 2>/dev/null || true
    fi

    verify_checksum "${TMPDIR}/${ARCHIVE_NAME}" "${TMPDIR}/${ARCHIVE_NAME}.sha256"

    echo "Installing to ${PREFIX}..."
    mkdir -p "${PREFIX}"

    # Extract into a fresh staging directory inside PREFIX (same filesystem, so
    # the swaps below are renames), never over the existing install: files a
    # newer release dropped must not linger.
    STAGE=$(mktemp -d "${PREFIX}/.beamtalk-stage.XXXXXX")
    trap 'rm -rf "${TMPDIR}" "${STAGE}"' EXIT

    # Archives contain a top-level beamtalk-{version}/ directory.
    case "${EXT}" in
        tar.gz)
            tar -xzf "${TMPDIR}/${ARCHIVE_NAME}" -C "${STAGE}" --strip-components=1
            ;;
        zip)
            if command -v unzip >/dev/null 2>&1; then
                unzip -qo "${TMPDIR}/${ARCHIVE_NAME}" -d "${TMPDIR}/extract"
            elif command -v 7z >/dev/null 2>&1; then
                7z x -o"${TMPDIR}/extract" -y "${TMPDIR}/${ARCHIVE_NAME}" >/dev/null
            else
                echo "Error: unzip or 7z required to extract .zip archives"
                exit 1
            fi
            cp -r "${TMPDIR}/extract/beamtalk-${VERSION}/"* "${STAGE}/"
            ;;
    esac

    swap_into_prefix

    write_build_marker
}

# What --uninstall removes (the paths a release archive installs).
OWNED_DIRS="lib/beamtalk share/beamtalk"
OWNED_BINS="beamtalk beamtalk-compiler-port beamtalk-lsp beamtalk-mcp beamtalk-exec"
MANIFEST_NAME=".beamtalk-installed"

# Every unit the archive supplies is replaced wholesale: each child of a
# top-level directory (bin/beamtalk, lib/beamtalk, share/beamtalk, and the
# same for any other top-level directory such as include/ or man/), and each
# top-level file. A top-level directory itself is never a unit (BT-5185):
# PREFIX, and anything else in its directories, is untouched, so a shared
# prefix such as /usr/local is safe.
#
# The swap is journalled: before a unit is replaced its old copy is moved into
# $OLD and recorded; if any later step fails, rollback_swap undoes every unit
# done so far (new copy out, old copy back) and only then deletes $OLD.
rollback_swap() {
    echo "Error: install failed part-way; restoring the previous install."
    restore_failed=""
    if [ -f "${OLD}/journal" ]; then
        while IFS='|' read -r dest slot; do
            rm -rf "${dest}"
            if [ -e "${slot}" ] || [ -L "${slot}" ]; then
                mv "${slot}" "${dest}" || restore_failed=1
            fi
        done < "${OLD}/journal"
    fi
    if [ -n "${restore_failed}" ]; then
        echo "  Restore was incomplete; the previous files are kept in ${OLD}."
        exit 1
    fi
    rm -rf "${OLD}"
    exit 1
}

# install_unit SRC DEST: back DEST up into $OLD (journalled), then move SRC in.
install_unit() {
    src="$1"; dest="$2"
    mkdir -p "$(dirname "${dest}")" || rollback_swap
    UNIT_N=$((UNIT_N + 1))
    slot="${OLD}/unit.${UNIT_N}"
    if [ -e "${dest}" ] || [ -L "${dest}" ]; then
        mv "${dest}" "${slot}" || rollback_swap
    fi
    if ! printf '%s|%s\n' "${dest}" "${slot}" >> "${OLD}/journal"; then
        # Not journalled, so rollback cannot restore it: put the old copy back.
        if [ -e "${slot}" ] || [ -L "${slot}" ]; then mv "${slot}" "${dest}" || true; fi
        rollback_swap
    fi
    mv "${src}" "${dest}" || rollback_swap
    printf '%s\n' "${dest#"${PREFIX}"/}" >> "${OLD}/manifest" || rollback_swap
}

swap_into_prefix() {
    # Checks that need no swap come first, so they fail with nothing changed.
    if [ -d "${STAGE}/bin" ]; then
        for f in "${STAGE}/bin/"*; do
            [ -e "${f}" ] || continue
            dest="${PREFIX}/bin/$(basename "${f}")"
            if [ -d "${dest}" ] && [ ! -L "${dest}" ]; then
                echo "Error: ${dest} is a directory; remove it and retry."
                exit 1
            fi
        done
    fi

    # The backups live in a directory outside STAGE, so the EXIT trap never
    # deletes them.
    OLD=$(mktemp -d "${PREFIX}/.beamtalk-old.XXXXXX")
    : > "${OLD}/journal"
    : > "${OLD}/manifest"
    UNIT_N=0
    for top in "${STAGE}/"*; do
        [ -e "${top}" ] || continue
        name="$(basename "${top}")"
        if [ -d "${top}" ] && [ ! -L "${top}" ]; then
            for unit in "${top}/"*; do
                [ -e "${unit}" ] || continue
                install_unit "${unit}" "${PREFIX}/${name}/$(basename "${unit}")"
            done
        else
            install_unit "${top}" "${PREFIX}/${name}"
        fi
    done
    # Record what this install owns so --uninstall removes exactly it, even
    # for units a release adds later (the OWNED_* lists are only a fallback).
    # BT-5185: keep entries an earlier install recorded that are still there,
    # so a unit a newer release dropped is not orphaned.
    if [ -f "${PREFIX}/${MANIFEST_NAME}" ]; then
        while IFS= read -r rel; do
            case "${rel}" in
                ''|/*|..|../*|*/..|*/../*) continue ;;
            esac
            if [ -d "${PREFIX}/${rel}" ] && [ ! -L "${PREFIX}/${rel}" ] \
                && [ "${rel}" = "${rel%%/*}" ]; then
                # A bare top-level directory an older install.sh installed
                # wholesale: carry its children as units instead, since
                # --uninstall never removes a bare directory.
                for child in "${PREFIX}/${rel}/"*; do
                    [ -e "${child}" ] || [ -L "${child}" ] || continue
                    unit="${rel}/$(basename "${child}")"
                    grep -qxF -- "${unit}" "${OLD}/manifest" \
                        || printf '%s\n' "${unit}" >> "${OLD}/manifest" || rollback_swap
                done
            elif [ -e "${PREFIX}/${rel}" ] || [ -L "${PREFIX}/${rel}" ]; then
                grep -qxF -- "${rel}" "${OLD}/manifest" \
                    || printf '%s\n' "${rel}" >> "${OLD}/manifest" || rollback_swap
            fi
        done < "${PREFIX}/${MANIFEST_NAME}"
    fi
    cp "${OLD}/manifest" "${PREFIX}/${MANIFEST_NAME}" || rollback_swap
    rm -rf "${OLD}"
}

uninstall_beamtalk() {
    echo "Removing beamtalk from ${PREFIX}..."
    for b in ${OWNED_BINS}; do
        rm -f "${PREFIX}/bin/${b}" "${PREFIX}/bin/${b}.exe"
    done
    for d in ${OWNED_DIRS}; do
        rm -rf "${PREFIX:?}/${d}"
    done
    if [ -f "${PREFIX}/${MANIFEST_NAME}" ]; then
        while IFS= read -r rel; do
            case "${rel}" in
                ''|/*|..|../*|*/..|*/../*) continue ;;
            esac
            # BT-5185: a bare top-level name is a whole shared directory
            # (include, man) in a manifest an older install.sh wrote; never
            # remove one of those wholesale. Units are always <dir>/<name>.
            case "${rel}" in
                */*) ;;
                *) [ -d "${PREFIX}/${rel}" ] && [ ! -L "${PREFIX}/${rel}" ] && continue ;;
            esac
            rm -rf "${PREFIX:?}/${rel}"
            # Tidy its directory if that leaves it empty.
            case "${rel}" in
                */*) rmdir "${PREFIX}/${rel%/*}" 2>/dev/null || true ;;
            esac
        done < "${PREFIX}/${MANIFEST_NAME}"
        rm -f "${PREFIX}/${MANIFEST_NAME}"
    fi
    rm -f "${PREFIX}/.beamtalk-build.sha256"
    # Tidy up now-empty directories; rmdir refuses non-empty ones, so a shared
    # prefix keeps whatever else lives in it.
    for d in bin lib share; do
        rmdir "${PREFIX}/${d}" 2>/dev/null || true
    done
    rmdir "${PREFIX}" 2>/dev/null || true
    echo "✅ beamtalk removed. Remove the PATH entry from your shell profile if you added one."
}

# Record which rolling build is installed (BT-4813) so scripts/refresh-edge.sh
# can tell whether the published edge release is newer. Only edge and nightly
# are rolling; a pinned version never goes stale. The checksum is the one
# verify_checksum just matched against the archive.
write_build_marker() {
    if [ "${EDGE}" != "1" ] && [ "${NIGHTLY}" != "1" ]; then return; fi
    if [ -z "${EXPECTED:-}" ]; then return; fi
    printf '%s  %s\n' "${EXPECTED}" "${ARCHIVE_NAME}" > "${PREFIX}/.beamtalk-build.sha256"
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

# ─── Report build identity ──────────────────────────────────────────────

# After an --edge/--nightly install, print what was actually installed:
# those channels are rolling, so the version number alone doesn't say which
# commit you got (BT-4812). Informational only — never fails the install.
print_build_identity() {
    if [ "${EDGE}" != "1" ] && [ "${NIGHTLY}" != "1" ]; then
        return 0
    fi
    # `beamtalk --version` prints `beamtalk <build id>`; Windows .exe installs
    # are not runnable from this shell, so fall back silently.
    IDENTITY=$("${PREFIX}/bin/beamtalk" --version 2>/dev/null) || IDENTITY=""
    if [ -n "${IDENTITY}" ]; then
        echo ""
        echo "  Build:   ${IDENTITY}"
    fi
}

# ─── Main ────────────────────────────────────────────────────────────────

main() {
    if [ "${UNINSTALL}" = "1" ]; then
        uninstall_beamtalk
        return
    fi

    detect_platform
    resolve_version

    echo ""
    echo "  beamtalk installer"
    echo "  ─────────────────"
    echo "  Version:  ${VERSION}"
    echo "  Platform: ${PLATFORM}"
    echo "  Prefix:   ${PREFIX}"
    if [ "${NIGHTLY}" = "1" ]; then
        echo "  Channel:  nightly (pre-release)"
    elif [ "${EDGE}" = "1" ]; then
        echo "  Channel:  edge (latest main, pre-release)"
    fi
    echo ""

    install_beamtalk

    echo ""
    echo "✅ beamtalk ${VERSION} installed successfully!"
    echo ""
    echo "  Binary:  ${PREFIX}/bin/beamtalk"
    echo "  LSP:     ${PREFIX}/bin/beamtalk-lsp"
    echo "  MCP:     ${PREFIX}/bin/beamtalk-mcp"
    echo "  Runtime: ${PREFIX}/lib/beamtalk/lib/"

    print_build_identity

    configure_path
}

main

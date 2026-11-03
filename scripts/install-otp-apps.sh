#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Copy the built OTP applications into an installed/packaged layout.
#
# Usage (from the repo root): scripts/install-otp-apps.sh <dest-lib-dir>
#
# <dest-lib-dir> is the installed `lib/` directory, i.e.
# `PREFIX/lib/beamtalk/lib` — each app lands in `<dest-lib-dir>/<app>/ebin`
# (plus `priv/` where the app has one). The one implementation shared by
# `just install` (and so `just dist`/`just test-install`), the Windows `just
# dist` recipe and scripts/ci/package-release.sh's Windows branch.
#
# Every app comes from rebar3's build output, runtime/_build/default/lib/*,
# EXCEPT the Beamtalk-compiled stdlib class beams (`bt@*.beam`) and
# `beamtalk_stdlib.app`. Those are
# written by `beamtalk build-stdlib` to runtime/apps/beamtalk_stdlib/ebin
# (`STDLIB_EBIN_DIR` in crates/beamtalk-cli/src/commands/build_stdlib.rs; the
# dev-layout `stdlib_ebin` in crates/beamtalk-core/src/ffi_type_specs.rs reads
# the same directory), not under _build. rebar3 only copies that directory into
# _build/default/lib/beamtalk_stdlib/ebin when `rebar3 compile` runs AFTER the
# stdlib build — never on a fresh checkout, where `build-erlang` runs before
# `build-stdlib` — so taking them from _build shipped an install with no stdlib
# classes (BT-4119), or stale ones from an earlier build. So for
# beamtalk_stdlib: Erlang modules come from _build, the `bt@*` beams AND the
# `.app` only from the stdlib build's own output directory. `build-stdlib`
# writes that `.app` complete (class + Erlang modules, `classes` env) in the
# same run as the beams, whereas `build-erlang` compiles _build's copy from
# `.app.src` before `build-stdlib` regenerates it, so its `classes` env is a
# stdlib build behind (BT-4384). `just test-install` fails if the installed
# `.app` and beams disagree.

set -euo pipefail

DEST="${1:?Usage: scripts/install-otp-apps.sh <dest-lib-dir>}"

BUILD_LIB_DIR="runtime/_build/default/lib"
STDLIB_CLASS_EBIN_DIR="runtime/apps/beamtalk_stdlib/ebin"

# Copy files (already expanded by the caller's glob) into a directory, 0644.
copy_files() {
    local dest_dir="$1"
    shift
    mkdir -p "${dest_dir}"
    for f in "$@"; do
        cp "${f}" "${dest_dir}/"
        chmod 644 "${dest_dir}/$(basename "${f}")"
    done
}

shopt -s nullglob

OTP_APP_COUNT=0
STDLIB_SEEN=0
for ebin_dir in "${BUILD_LIB_DIR}"/*/ebin; do
    app_dir="$(dirname "${ebin_dir}")"
    app="$(basename "${app_dir}")"
    beams=("${ebin_dir}"/*.beam)
    if [ "${#beams[@]}" -eq 0 ]; then
        continue
    fi
    dest_ebin="${DEST}/${app}/ebin"

    if [ "${app}" = "beamtalk_stdlib" ]; then
        STDLIB_SEEN=1
        erlang_beams=()
        for b in "${beams[@]}"; do
            case "$(basename "${b}")" in
                bt@*) ;;
                *) erlang_beams+=("${b}") ;;
            esac
        done
        class_beams=("${STDLIB_CLASS_EBIN_DIR}"/bt@*.beam)
        if [ "${#class_beams[@]}" -eq 0 ]; then
            echo "❌ No stdlib class beams (bt@*.beam) in ${STDLIB_CLASS_EBIN_DIR}. Run 'just build-stdlib' first." >&2
            exit 1
        fi
        # `${a[@]+...}`: an empty array is "unbound" under `set -u` in the
        # bash 3.2 macOS ships.
        copy_files "${dest_ebin}" ${erlang_beams[@]+"${erlang_beams[@]}"} "${class_beams[@]}"
    else
        copy_files "${dest_ebin}" "${beams[@]}"
    fi

    apps=("${ebin_dir}"/*.app)
    if [ "${app}" = "beamtalk_stdlib" ]; then
        apps=("${STDLIB_CLASS_EBIN_DIR}/beamtalk_stdlib.app")
        if [ ! -f "${apps[0]}" ]; then
            echo "❌ No ${apps[0]}. Run 'just build-stdlib' first." >&2
            exit 1
        fi
    fi
    if [ "${#apps[@]}" -gt 0 ]; then
        copy_files "${dest_ebin}" "${apps[@]}"
    fi
    # Copy priv/ directory if present (e.g. beamtalk_workspace browser UI).
    # Contents-into-dir, so re-installing over an existing PREFIX doesn't nest
    # a second priv/priv.
    if [ -d "${app_dir}/priv" ]; then
        mkdir -p "${DEST}/${app}/priv"
        cp -RL "${app_dir}/priv/." "${DEST}/${app}/priv/"
    fi
    OTP_APP_COUNT=$((OTP_APP_COUNT + 1))
done

if [ "${OTP_APP_COUNT}" -eq 0 ]; then
    echo "❌ No OTP apps found in ${BUILD_LIB_DIR}/. Run 'just build-erlang' first." >&2
    exit 1
fi
if [ "${STDLIB_SEEN}" -eq 0 ]; then
    echo "❌ beamtalk_stdlib not found in ${BUILD_LIB_DIR}/. Run 'just build-erlang' first." >&2
    exit 1
fi

#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Tests for scripts/install.sh (BT-5087): the checksum is mandatory unless
# --insecure-skip-checksum is given, an install replaces the previous one
# instead of extracting over it, and --uninstall removes it.
#
# install.sh downloads with `curl --proto '=https'`, so the tests put a fake
# `curl` first on PATH that serves a local fixture directory.
set -u

SCRIPT="$(cd "$(dirname "$0")/../.." && pwd)/scripts/install.sh"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
FAILED=0

check() { # name, exit-status-of-the-assertion (0 = pass)
    if [ "$2" -eq 0 ]; then echo "ok   - $1"; else echo "FAIL - $1"; FAILED=1; fi
}

# ── fixture release: beamtalk-9.9.9-<platform>.tar.gz ──
case "$(uname -s)-$(uname -m)" in
    Linux-x86_64) PLAT=linux-x86_64 ;;
    Darwin-x86_64) PLAT=macos-x86_64 ;;
    Darwin-arm64) PLAT=macos-arm64 ;;
    *) echo "skip - unsupported platform for install.sh tests"; exit 0 ;;
esac
mkdir -p "$T/stage/beamtalk-9.9.9/bin" "$T/stage/beamtalk-9.9.9/lib/beamtalk/lib" "$T/release"
printf '#!/bin/sh\necho beamtalk\n' > "$T/stage/beamtalk-9.9.9/bin/beamtalk"
chmod +x "$T/stage/beamtalk-9.9.9/bin/beamtalk"
echo new > "$T/stage/beamtalk-9.9.9/lib/beamtalk/lib/new.beam"
mkdir -p "$T/stage/beamtalk-9.9.9/extra"
echo x > "$T/stage/beamtalk-9.9.9/extra/file.txt"
ARCHIVE="beamtalk-9.9.9-${PLAT}.tar.gz"
(cd "$T/stage" && tar -czf "$T/release/$ARCHIVE" beamtalk-9.9.9)
if command -v sha256sum >/dev/null 2>&1; then SUM="$(sha256sum "$T/release/$ARCHIVE" | awk '{print $1}')"
else SUM="$(shasum -a 256 "$T/release/$ARCHIVE" | awk '{print $1}')"; fi

# ── fake curl ──
mkdir -p "$T/fakebin"
cat > "$T/fakebin/curl" <<'FAKE'
#!/bin/sh
out=""; url=""
while [ $# -gt 0 ]; do
    case "$1" in
        -o) out="$2"; shift 2 ;;
        --proto) shift 2 ;;
        -*) shift ;;
        *) url="$1"; shift ;;
    esac
done
src="$FAKE_RELEASE_DIR/$(basename "$url")"
[ -f "$src" ] || exit 22
cp "$src" "$out"
FAKE
chmod +x "$T/fakebin/curl"

run_install() { # prefix, extra args...
    prefix="$1"; shift
    PATH="$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/release" BEAMTALK_PREFIX="$prefix" \
        sh "$SCRIPT" --version 9.9.9 "$@" >"$T/out" 2>&1
}

# 1. missing checksum: fails, installs nothing, names the escape hatch
P="$T/p1"
run_install "$P"; rc=$?
[ $rc -ne 0 ] && [ ! -e "$P/bin/beamtalk" ] && grep -q -- '--insecure-skip-checksum' "$T/out"
check "missing checksum is an error and installs nothing" $?

# 2. missing checksum with the explicit flag: installs
P="$T/p2"
run_install "$P" --insecure-skip-checksum; rc=$?
[ $rc -eq 0 ] && [ -x "$P/bin/beamtalk" ]
check "--insecure-skip-checksum installs without a checksum file" $?

# 3. mismatching checksum fails even with the file present
echo "0000000000000000000000000000000000000000000000000000000000000000  $ARCHIVE" > "$T/release/$ARCHIVE.sha256"
P="$T/p3"
run_install "$P"; rc=$?
[ $rc -ne 0 ] && [ ! -e "$P/bin/beamtalk" ] && grep -q 'Checksum verification failed' "$T/out"
check "wrong checksum is an error" $?

# 4. good checksum installs; a reinstall drops stale files but keeps unrelated ones
echo "$SUM  $ARCHIVE" > "$T/release/$ARCHIVE.sha256"
P="$T/p4"
mkdir -p "$P/lib/beamtalk/lib" "$P/lib/other"
echo stale > "$P/lib/beamtalk/lib/stale.beam"
echo keep > "$P/lib/other/keep.txt"
run_install "$P"; rc=$?
[ $rc -eq 0 ] && [ -x "$P/bin/beamtalk" ] && [ -f "$P/lib/beamtalk/lib/new.beam" ] \
    && [ ! -e "$P/lib/beamtalk/lib/stale.beam" ] && [ -f "$P/lib/other/keep.txt" ] \
    && [ -z "$(ls -A "$P" | grep '^\.beamtalk-stage')" ]
check "good checksum installs, replaces stale files, keeps unrelated ones" $?

P4="$P"
# 4b. an archive entry outside bin/ lib/beamtalk share/beamtalk is still installed
[ -f "$P/extra/file.txt" ]
check "other top-level archive entries are merged, not dropped" $?
# --uninstall only removes what beamtalk owns, so drop the fixture's extra entry first
rm -rf "$P/extra"

# 4c. a failed swap puts the previous install back (a plain file blocks the new tree)
P="$T/p4c"
mkdir -p "$P/lib/beamtalk"
echo old > "$P/lib/beamtalk/old.txt"
mkdir -p "$T/mvbin"
cat > "$T/mvbin/mv" <<'FAKEMV'
#!/bin/sh
# fail only the move of the new lib/beamtalk into place
case "$*" in *.beamtalk-stage*/lib/beamtalk\ *) exit 1 ;; esac
exec /bin/mv "$@"
FAKEMV
chmod +x "$T/mvbin/mv"
PATH="$T/mvbin:$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/release" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --version 9.9.9 >"$T/out" 2>&1; rc=$?
[ $rc -ne 0 ] && [ -f "$P/lib/beamtalk/old.txt" ]
check "failed swap restores the previous install" $?

# 5. uninstall removes beamtalk, keeps unrelated files, needs no network
P="$P4"
PATH="$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/none" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --uninstall >"$T/out" 2>&1; rc=$?
[ $rc -eq 0 ] && [ ! -e "$P/bin/beamtalk" ] && [ ! -e "$P/lib/beamtalk" ] && [ -f "$P/lib/other/keep.txt" ]
check "--uninstall removes beamtalk and keeps unrelated files" $?

# 6. uninstall of a beamtalk-only prefix removes the prefix
P="$T/p6"
run_install "$P"; rm -rf "$P/extra"; PATH="$T/fakebin:$PATH" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --uninstall >/dev/null 2>&1
[ ! -e "$P" ]
check "--uninstall removes an otherwise empty prefix" $?

exit $FAILED

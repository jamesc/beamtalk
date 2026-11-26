#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Tests for scripts/install.sh (BT-5087): the checksum is mandatory unless
# --insecure-skip-checksum is given, an install replaces the previous one
# instead of extracting over it, and --uninstall removes it. BT-5307: --edge
# installs on every target edge.yml builds, and only on those.
#
# install.sh downloads with `curl --proto '=https'`, so the tests put a fake
# `curl` first on PATH that serves a local fixture directory. The --edge tests
# also put a fake `uname` first, so every target's detection runs on any host.
set -u

SCRIPT="$(cd "$(dirname "$0")/../.." && pwd)/scripts/install.sh"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
FAILED=0

check() { # name, exit-status-of-the-assertion (0 = pass)
    if [ "$2" -eq 0 ]; then echo "ok   - $1"; else echo "FAIL - $1"; FAILED=1; fi
}

no_entries() { # dir, name-prefix: true when no entry of dir starts with the prefix
    for e in "$1"/"$2"*; do
        if [ -e "$e" ] || [ -L "$e" ]; then return 1; fi
    done
    return 0
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
mkdir -p "$T/stage/beamtalk-9.9.9/share/beamtalk" "$T/stage/beamtalk-9.9.9/extra"
echo stub > "$T/stage/beamtalk-9.9.9/share/beamtalk/stub.bt"
echo x > "$T/stage/beamtalk-9.9.9/extra/file.txt"
mkdir -p "$T/stage/beamtalk-9.9.9/include"
echo h > "$T/stage/beamtalk-9.9.9/include/beamtalk.h"
ARCHIVE="beamtalk-9.9.9-${PLAT}.tar.gz"
(cd "$T/stage" && tar -czf "$T/release/$ARCHIVE" beamtalk-9.9.9)
if command -v sha256sum >/dev/null 2>&1; then SUM="$(sha256sum "$T/release/$ARCHIVE" | awk '{print $1}')"
else SUM="$(shasum -a 256 "$T/release/$ARCHIVE" | awk '{print $1}')"; fi

# ── fake curl ──
mkdir -p "$T/fakebin"
cat > "$T/fakebin/curl" <<'FAKE'
#!/bin/sh
echo "$*" >> "${FAKE_CURL_LOG:-/dev/null}"
out=""; url=""
while [ $# -gt 0 ]; do
    case "$1" in
        -o) out="$2"; shift 2 ;;
        --proto|--proto-redir) shift 2 ;;
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
        FAKE_CURL_LOG="$T/curl.log" \
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
    && no_entries "$P" .beamtalk-stage
check "good checksum installs, replaces stale files, keeps unrelated ones" $?

P4="$P"
# 4b. an archive entry outside bin/ lib/beamtalk share/beamtalk is still installed
[ -f "$P/extra/file.txt" ]
check "other top-level archive entries are merged, not dropped" $?

# 4c. a failure part-way through the swap rolls back every unit already swapped
mkdir -p "$T/mvbin"
cat > "$T/mvbin/mv" <<'FAKEMV'
#!/bin/sh
# fail only the move of the new $FAIL_UNIT into place
case "$*" in *.beamtalk-stage*/"$FAIL_UNIT"\ *) exit 1 ;; esac
exec /bin/mv "$@"
FAKEMV
chmod +x "$T/mvbin/mv"
for unit in share/beamtalk bin/beamtalk; do
    P="$T/p4c-$(echo "$unit" | tr / _)"
    mkdir -p "$P/lib/beamtalk" "$P/share/beamtalk" "$P/bin"
    echo old > "$P/lib/beamtalk/old.txt"
    echo old > "$P/share/beamtalk/old.txt"
    echo oldbin > "$P/bin/beamtalk"
    FAIL_UNIT="$unit" PATH="$T/mvbin:$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/release" BEAMTALK_PREFIX="$P" \
        sh "$SCRIPT" --version 9.9.9 >"$T/out" 2>&1; rc=$?
    [ $rc -ne 0 ] && [ -f "$P/lib/beamtalk/old.txt" ] && [ ! -e "$P/lib/beamtalk/lib/new.beam" ] \
        && [ -f "$P/share/beamtalk/old.txt" ] && [ "$(cat "$P/bin/beamtalk")" = oldbin ] \
        && no_entries "$P" .beamtalk-
    check "failure while swapping $unit restores the whole previous install" $?
done

# 4d. a directory where a binary should go fails before anything is changed
P="$T/p4d"
mkdir -p "$P/bin/beamtalk" "$P/lib/beamtalk"
echo old > "$P/lib/beamtalk/old.txt"
run_install "$P"; rc=$?
[ $rc -ne 0 ] && [ -f "$P/lib/beamtalk/old.txt" ] && no_entries "$P" .beamtalk-
check "a directory in place of a binary is refused before any change" $?

# 5. uninstall removes beamtalk, keeps unrelated files, needs no network
P="$P4"
PATH="$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/none" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --uninstall >"$T/out" 2>&1; rc=$?
[ $rc -eq 0 ] && [ ! -e "$P/bin/beamtalk" ] && [ ! -e "$P/lib/beamtalk" ] && [ ! -e "$P/extra" ] && [ -f "$P/lib/other/keep.txt" ]
check "--uninstall removes beamtalk and keeps unrelated files" $?

# 6. uninstall of a beamtalk-only prefix removes the prefix
P="$T/p6"
run_install "$P"; PATH="$T/fakebin:$PATH" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --uninstall >/dev/null 2>&1
[ ! -e "$P" ]
check "--uninstall removes an otherwise empty prefix" $?

# 7. hostile --version / --prefix are refused before any download
for v in 'v1/../x' '1.0;rm' '../x' ''; do
    [ -z "$v" ] && continue
    PATH="$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/none" BEAMTALK_PREFIX="$T/p7" sh "$SCRIPT" --version "$v" >"$T/out" 2>&1; rc=$?
    [ $rc -ne 0 ] && grep -q "invalid version" "$T/out"
    check "--version '$v' is rejected" $?
done
PATH="$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/release" sh "$SCRIPT" --version 9.9.9 --prefix "$T/a|b" >"$T/out" 2>&1; rc=$?
[ $rc -ne 0 ] && [ ! -e "$T/a|b" ]
check "--prefix containing '|' is rejected" $?

# 8. BT-5185: every curl download refuses a redirect off https
[ -s "$T/curl.log" ] && ! grep -v -- "--proto-redir =https" "$T/curl.log" | grep -q .
check "curl downloads pin redirects to https (--proto-redir)" $?

# 9. BT-5185: a shared top-level directory (include/) is merged into, never
# replaced or removed wholesale
P="$T/p9"
mkdir -p "$P/include"
echo theirs > "$P/include/other.h"
run_install "$P"; rc=$?
[ $rc -eq 0 ] && [ -f "$P/include/other.h" ] && [ -f "$P/include/beamtalk.h" ]
check "install keeps other files in a shared top-level directory" $?
PATH="$T/fakebin:$PATH" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --uninstall >/dev/null 2>&1
[ -f "$P/include/other.h" ] && [ ! -e "$P/include/beamtalk.h" ]
check "--uninstall keeps other files in a shared top-level directory" $?

# 10. BT-5185: a unit an earlier release installed and a later one dropped is
# still removed by --uninstall (the manifest is merged, not overwritten)
mkdir -p "$T/stage/beamtalk-9.9.8/bin"
cp "$T/stage/beamtalk-9.9.9/bin/beamtalk" "$T/stage/beamtalk-9.9.8/bin/beamtalk"
ARCHIVE8="beamtalk-9.9.8-${PLAT}.tar.gz"
(cd "$T/stage" && tar -czf "$T/release/$ARCHIVE8" beamtalk-9.9.8)
if command -v sha256sum >/dev/null 2>&1; then SUM8="$(sha256sum "$T/release/$ARCHIVE8" | awk '{print $1}')"
else SUM8="$(shasum -a 256 "$T/release/$ARCHIVE8" | awk '{print $1}')"; fi
echo "$SUM8  $ARCHIVE8" > "$T/release/$ARCHIVE8.sha256"
P="$T/p10"
run_install "$P" && PATH="$T/fakebin:$PATH" FAKE_RELEASE_DIR="$T/release" BEAMTALK_PREFIX="$P" \
    sh "$SCRIPT" --version 9.9.8 >"$T/out" 2>&1 \
    && PATH="$T/fakebin:$PATH" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --uninstall >/dev/null 2>&1
[ ! -e "$P/extra/file.txt" ] && [ ! -e "$P/include/beamtalk.h" ]
check "--uninstall removes units an earlier release installed" $?

# 11. BT-5185: with only a wget that lacks --https-only (BusyBox), the error
# says so instead of failing on an unknown option
mkdir -p "$T/nocurl"
for d in /usr/local/bin /usr/bin /bin; do
    [ -d "$d" ] || continue
    for f in "$d"/*; do
        n="$(basename "$f")"
        case "$n" in curl|wget) continue ;; esac
        [ -e "$T/nocurl/$n" ] || ln -s "$f" "$T/nocurl/$n"
    done
done
cat > "$T/nocurl/wget" <<'FAKEWGET'
#!/bin/sh
case "$*" in *--help*) echo "BusyBox v1.36 multi-call binary. Usage: wget [-cqS] [-O FILE] URL"; exit 1 ;; esac
echo "wget: unrecognized option: https-only" >&2
exit 1
FAKEWGET
chmod +x "$T/nocurl/wget"
PATH="$T/nocurl" BEAMTALK_PREFIX="$T/p11" sh "$SCRIPT" --version 9.9.9 >"$T/out" 2>&1; rc=$?
[ $rc -ne 0 ] && grep -q "does not support --https-only" "$T/out" && [ ! -e "$T/p11/bin/beamtalk" ]
check "BusyBox wget gets a clear error, nothing installed" $?

# 12. BT-5185: a bare top-level directory an older install.sh recorded is
# carried forward as its children, so --uninstall still removes them
P="$T/p12"
mkdir -p "$P/legacy"
echo old > "$P/legacy/old.txt"
printf 'legacy\n' > "$P/.beamtalk-installed"
run_install "$P" && PATH="$T/fakebin:$PATH" BEAMTALK_PREFIX="$P" sh "$SCRIPT" --uninstall >/dev/null 2>&1
[ ! -e "$P/legacy/old.txt" ]
check "--uninstall removes what a legacy bare manifest entry installed" $?

# 13. BT-5307: --edge (and BEAMTALK_EDGE=1) installs on exactly the targets
# edge.yml builds. The list is read from the workflow's matrix, so a target
# added there and not to install.sh (or the reverse) fails here.
EDGE_YML="$(dirname "$SCRIPT")/../.github/workflows/edge.yml"
EDGE_TARGETS="$(sed -n 's/^ *- platform: *\([a-z0-9_-]*\).*/\1/p' "$EDGE_YML")"
[ -n "$EDGE_TARGETS" ] && echo "$EDGE_TARGETS" | grep -qx linux-x86_64 \
    && echo "$EDGE_TARGETS" | grep -qx macos-arm64; rc=$?
check "edge.yml builds linux-x86_64 and macos-arm64 (read: $(echo "$EDGE_TARGETS" | tr '\n' ' '))" $rc

mkdir -p "$T/unamebin"
cat > "$T/unamebin/uname" <<'FAKEUNAME'
#!/bin/sh
case "$1" in
    -s) echo "$FAKE_UNAME_S" ;;
    -m) echo "$FAKE_UNAME_M" ;;
    *) echo "$FAKE_UNAME_S" ;;
esac
FAKEUNAME
chmod +x "$T/unamebin/uname"

uname_for() { # platform -> "S M" as uname reports them
    case "$1" in
        linux-x86_64) echo "Linux x86_64" ;;
        macos-arm64) echo "Darwin arm64" ;;
        macos-x86_64) echo "Darwin x86_64" ;;
        windows-x86_64) echo "MINGW64_NT-10.0 x86_64" ;;
        *) return 1 ;;
    esac
}

# An edge fixture per target: beamtalk-edge-<platform>.tar.gz plus its checksum.
mkdir -p "$T/edge"
for plat in linux-x86_64 macos-arm64 macos-x86_64; do
    rm -rf "$T/stage/beamtalk-edge"
    cp -r "$T/stage/beamtalk-9.9.9" "$T/stage/beamtalk-edge"
    a="beamtalk-edge-${plat}.tar.gz"
    (cd "$T/stage" && tar -czf "$T/edge/$a" beamtalk-edge)
    if command -v sha256sum >/dev/null 2>&1; then s="$(sha256sum "$T/edge/$a" | awk '{print $1}')"
    else s="$(shasum -a 256 "$T/edge/$a" | awk '{print $1}')"; fi
    echo "$s  $a" > "$T/edge/$a.sha256"
done

run_edge() { # platform, prefix, extra args... ($EDGE_ENV is passed as BEAMTALK_EDGE)
    plat="$1"; prefix="$2"; shift 2
    read -r s m <<<"$(uname_for "$plat")"
    : > "$T/edge-curl.log"
    PATH="$T/unamebin:$T/fakebin:$PATH" FAKE_UNAME_S="$s" FAKE_UNAME_M="$m" \
        FAKE_RELEASE_DIR="$T/edge" FAKE_CURL_LOG="$T/edge-curl.log" \
        BEAMTALK_PREFIX="$prefix" BEAMTALK_EDGE="${EDGE_ENV:-}" sh "$SCRIPT" "$@" >"$T/out" 2>&1
}

for plat in $EDGE_TARGETS; do
    P="$T/p13-$plat"
    run_edge "$plat" "$P" --edge; rc=$?
    [ $rc -eq 0 ] && [ -x "$P/bin/beamtalk" ] \
        && grep -q "/releases/download/edge/beamtalk-edge-${plat}.tar.gz " "$T/edge-curl.log" \
        && grep -q "  beamtalk-edge-${plat}.tar.gz\$" "$P/.beamtalk-build.sha256"
    check "--edge installs beamtalk-edge-${plat}.tar.gz on ${plat}" $?
done

P="$T/p13-env"
EDGE_ENV=1 run_edge macos-arm64 "$P"; rc=$?
[ $rc -eq 0 ] && [ -x "$P/bin/beamtalk" ] \
    && grep -q "/releases/download/edge/beamtalk-edge-macos-arm64.tar.gz " "$T/edge-curl.log"
check "BEAMTALK_EDGE=1 installs the edge build on macos-arm64" $?

for plat in macos-x86_64 windows-x86_64; do
    if echo "$EDGE_TARGETS" | grep -qx "$plat"; then continue; fi
    P="$T/p13-no-$plat"
    run_edge "$plat" "$P" --edge; rc=$?
    [ $rc -ne 0 ] && [ ! -e "$P/bin/beamtalk" ] && [ ! -s "$T/edge-curl.log" ] \
        && grep -q "edge is not built for ${plat}" "$T/out" \
        && grep -q "covers: linux-x86_64, macos-arm64" "$T/out"
    check "--edge on ${plat} is a clear error before any download" $?
done

exit $FAILED

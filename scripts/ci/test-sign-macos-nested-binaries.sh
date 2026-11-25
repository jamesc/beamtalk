#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Test (BT-5194): scripts/ci/sign-macos-nested-binaries.sh gives the broad
# Hardened Runtime entitlements (allow-jit, allow-unsigned-executable-memory,
# disable-library-validation) to the BEAM VM executable (beam*.smp) only, signs
# every other nested Mach-O without entitlements, and fails when no VM is
# found. The host app itself gets no entitlements file (tauri.conf.json).
#
# Runs on Linux: `file` and `codesign` are stubbed on PATH, so this checks
# which binaries the script signs and with which flags, not a real signature.
#
# Usage: scripts/ci/test-sign-macos-nested-binaries.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$REPO_ROOT/scripts/ci/sign-macos-nested-binaries.sh"
BEAM_ENTITLEMENTS="$REPO_ROOT/desktop/src-tauri/beam-entitlements.plist"

T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT

failures=0
check() { # label, condition string (evaluated; a failing condition must not abort the run)
  if eval "$2"; then echo "ok: $1"; else echo "FAIL: $1" >&2; failures=$((failures + 1)); fi
}

# Stubs: `file -b` reports Mach-O for any file whose content starts with
# MACHO; `codesign` logs one line per call: "<signed path> | <all args>".
mkdir -p "$T/stubs"
cat >"$T/stubs/file" <<'STUB'
#!/usr/bin/env bash
f="${!#}"
if head -c5 "$f" | grep -q MACHO; then echo "Mach-O 64-bit executable arm64"; else echo "ASCII text"; fi
STUB
cat >"$T/stubs/codesign" <<'STUB'
#!/usr/bin/env bash
echo "${!#} | $*" >>"$CODESIGN_LOG"
STUB
chmod +x "$T/stubs/file" "$T/stubs/codesign"

# A release layout: the VM, ERTS helpers, a NIF, and non-Mach-O files.
rel="$T/release"
mkdir -p "$rel/erts-15.0/bin" "$rel/lib/crypto-5.0/priv/lib" "$rel/bin"
for f in erts-15.0/bin/beam.smp erts-15.0/bin/epmd erts-15.0/bin/erlexec \
  lib/crypto-5.0/priv/lib/crypto.so; do
  echo MACHO >"$rel/$f"
done
echo '#!/bin/sh' >"$rel/bin/server"
echo 'not mach-o' >"$rel/lib/crypto-5.0/ebin.beam"

run() { # release dir; output -> $OUT, status -> $RC, codesign calls -> $T/codesign.log
  : >"$T/codesign.log"
  set +e
  OUT="$(PATH="$T/stubs:$PATH" CODESIGN_LOG="$T/codesign.log" bash "$SCRIPT" "$1" "Developer ID Application: Test" 2>&1)"
  RC=$?
  set -e
}

signed_with() { # path suffix -> the codesign args line for it
  grep -F "$1 |" "$T/codesign.log" || true
}

run "$rel"
check "signs a full release" '[ "$RC" -eq 0 ]'
check "signs every Mach-O and nothing else" '[ "$(wc -l <"$T/codesign.log")" -eq 4 ]'
check "beam.smp gets the BEAM entitlements" \
  '[[ "$(signed_with erts-15.0/bin/beam.smp)" == *"--entitlements $BEAM_ENTITLEMENTS"* ]]'
for helper in erts-15.0/bin/epmd erts-15.0/bin/erlexec lib/crypto-5.0/priv/lib/crypto.so; do
  check "$helper is signed with the Hardened Runtime" \
    '[[ "$(signed_with '"$helper"')" == *"--options runtime"* ]]'
  check "$helper gets no entitlements" \
    '[[ -n "$(signed_with '"$helper"')" && "$(signed_with '"$helper"')" != *--entitlements* ]]'
done

# A release with no VM: refuse rather than ship a beam.smp-less signing pass.
rm "$rel/erts-15.0/bin/beam.smp"
run "$rel"
check "fails when no beam*.smp is found" '[ "$RC" -ne 0 ] && [[ "$OUT" == *"No beam*.smp"* ]]'

# The VM's entitlements file holds the three keys; the host app has none.
for key in allow-jit allow-unsigned-executable-memory disable-library-validation; do
  check "beam-entitlements.plist grants $key" \
    'grep -q "com.apple.security.cs.'"$key"'" "$BEAM_ENTITLEMENTS"'
done
check "the Tauri host app is given no entitlements file" \
  '! grep -q "\"entitlements\"" "$REPO_ROOT/desktop/src-tauri/tauri.conf.json"'
check "the host app keeps the Hardened Runtime" \
  'grep -q "\"hardenedRuntime\": true" "$REPO_ROOT/desktop/src-tauri/tauri.conf.json"'

if [ "$failures" -ne 0 ]; then
  echo "❌ $failures check(s) failed" >&2
  exit 1
fi
echo "✅ sign-macos-nested-binaries.sh entitlement scoping"

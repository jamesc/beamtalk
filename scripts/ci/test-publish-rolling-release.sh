#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Test (BT-5094): scripts/ci/publish-rolling-release.sh never deletes the
# existing release, uploads before it removes anything, drops stale assets, and
# leaves the old release untouched when an upload fails. Runs against a fake
# `gh` that records its calls.
#
# Usage: scripts/ci/test-publish-rolling-release.sh

# shellcheck disable=SC2016,SC2034  # conditions are single-quoted and eval'd by check(); RC is read there
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$REPO_ROOT/scripts/ci/publish-rolling-release.sh"

T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/bin" "$T/assets"
echo a > "$T/assets/new.tar.gz"
echo b > "$T/assets/new.tar.gz.sha256"
echo c > "$T/assets/other.tar.gz"
echo d > "$T/assets/other.tar.gz.sha256"
echo notes > "$T/notes.md"

# Fake gh. FAKE_RELEASE_EXISTS=1 makes `release view` succeed; the asset list
# comes from FAKE_ASSETS; FAKE_FAIL_ON makes the named subcommand fail.
cat > "$T/bin/gh" <<'GH'
#!/usr/bin/env bash
echo "$*" >> "$FAKE_LOG"
case "$1 $2" in
  "api repos/o/r/git/ref/tags/edge")
    # FAKE_REF_ERR: the existence check fails with this message.
    if [[ -n "${FAKE_REF_ERR:-}" ]]; then echo "gh: ${FAKE_REF_ERR}" >&2; exit 1; fi ;;
  "api -X")
    # FAKE_PATCH_ERR: PATCH of the tag ref fails with this message.
    if [[ "$*" == *"-X PATCH"* && -n "${FAKE_PATCH_ERR:-}" ]]; then
      echo "gh: ${FAKE_PATCH_ERR}" >&2; exit 1
    fi ;;
  "release view")
    [ "${FAKE_RELEASE_EXISTS:-0}" = 1 ] || exit 1
    if [[ "$*" == *--json* ]]; then printf '%s\n' ${FAKE_ASSETS:-}; fi ;;
esac
[ -z "${FAKE_FAIL_ON:-}" ] || [[ "$1 $2" != "$FAKE_FAIL_ON"* ]] || exit 1
exit 0
GH
chmod +x "$T/bin/gh"

failures=0
check() { # label, condition (eval'd so a false condition cannot abort the run)
  if eval "$2"; then echo "ok: $1"; else echo "FAIL: $1" >&2; failures=$((failures + 1)); fi
}
run() {
  : > "$T/log"
  set +e
  PATH="$T/bin:$PATH" FAKE_LOG="$T/log" GITHUB_REPOSITORY=o/r \
    bash "$SCRIPT" edge abc123 "Edge" "$T/notes.md" "$T/assets/new.tar.gz" "$T/assets/other.tar.gz" "$T/assets/new.tar.gz.sha256" "$T/assets/other.tar.gz.sha256" >/dev/null 2>&1
  RC=$?
  set -e
}
line_of() { grep -n -- "$1" "$T/log" | head -1 | cut -d: -f1; }

# Existing release: update in place, never delete the release.
FAKE_RELEASE_EXISTS=1 FAKE_ASSETS="new.tar.gz new.tar.gz.sha256 old.tar.gz" run
check "existing: succeeds" '[ "$RC" -eq 0 ]'
check "existing: never runs release delete" '! grep -q "^release delete " "$T/log"'
check "existing: never runs release create" '! grep -q "^release create" "$T/log"'
check "existing: stale asset removed" 'grep -q "^release delete-asset edge old.tar.gz --yes" "$T/log"'
check "existing: current assets kept" '! grep -q "delete-asset edge new" "$T/log"'
check "existing: upload comes before the stale-asset removal" \
  '[ "$(line_of "release upload")" -lt "$(line_of "delete-asset")" ]'
check "existing: tag moved after assets" \
  '[ "$(line_of "delete-asset")" -lt "$(line_of "api -X PATCH repos/o/r/git/refs/tags/edge")" ]'
check "existing: notes refreshed last" \
  '[ "$(line_of "api -X PATCH")" -lt "$(line_of "release edit edge")" ]'

# Asset order (BT-5101): every archive is followed immediately by its own
# .sha256, one upload call per file, however the caller ordered the arguments.
# Archives-first/checksums-last would only move the stale-checksum window.
check "order: one upload call per asset" '[ "$(grep -c "^release upload" "$T/log")" -eq 4 ]'
check "order: archive, then its checksum, then the next pair" \
  '[ "$(grep "^release upload" "$T/log" | sed -E "s#.*/([^/ ]+) --clobber#\1#" | tr "\n" " ")" = "new.tar.gz new.tar.gz.sha256 other.tar.gz other.tar.gz.sha256 " ]'

# Upload fails: nothing else happens, the old release is untouched.
FAKE_RELEASE_EXISTS=1 FAKE_FAIL_ON="release upload" run
check "failed upload: script fails" '[ "$RC" -ne 0 ]'
check "failed upload: no tag move, delete or edit" \
  '! grep -qE "^(api|release delete|release edit)" "$T/log"'

# Tag move (BT-5101): only a missing ref falls through to create.
FAKE_RELEASE_EXISTS=1 FAKE_REF_ERR="Not Found (HTTP 404)" run
check "missing ref: succeeds" '[ "$RC" -eq 0 ]'
check "missing ref: tag created with POST" 'grep -q "^api -X POST repos/o/r/git/refs .*refs/tags/edge" "$T/log"'
check "missing ref: no PATCH" '! grep -q "^api -X PATCH" "$T/log"'
FAKE_RELEASE_EXISTS=1 FAKE_REF_ERR="Resource not accessible by integration (HTTP 403)" run
check "ref check failure: script fails" '[ "$RC" -ne 0 ]'
check "ref check failure: no POST fall-through" '! grep -q "^api -X POST" "$T/log"'
FAKE_RELEASE_EXISTS=1 FAKE_PATCH_ERR="Resource not accessible by integration (HTTP 403)" run
check "other PATCH failure: script fails" '[ "$RC" -ne 0 ]'
check "other PATCH failure: no POST fall-through" '! grep -q "^api -X POST" "$T/log"'
check "other PATCH failure: release notes not edited" '! grep -q "^release edit" "$T/log"'
ERR_OUT="$(FAKE_RELEASE_EXISTS=1 FAKE_PATCH_ERR="Resource not accessible by integration (HTTP 403)" PATH="$T/bin:$PATH" FAKE_LOG="$T/log2" GITHUB_REPOSITORY=o/r bash "$SCRIPT" edge abc "t" "$T/notes.md" "$T/assets/new.tar.gz" 2>&1 || true)"
check "other PATCH failure: own error reported" '[[ "$ERR_OUT" == *"Resource not accessible"* ]]'

# No release yet: tag placed, then the release created from it.
FAKE_RELEASE_EXISTS=0 run
check "absent: succeeds" '[ "$RC" -eq 0 ]'
check "absent: created with the target sha" 'grep -q "^release create edge .*--target abc123" "$T/log"'
check "absent: tag placed before create" '[ "$(line_of "api -X PATCH")" -lt "$(line_of "release create")" ]'

# Missing asset is rejected before anything is called.
: > "$T/log"
set +e; PATH="$T/bin:$PATH" FAKE_LOG="$T/log" GITHUB_REPOSITORY=o/r bash "$SCRIPT" edge abc "t" "$T/notes.md" "$T/assets/missing" >/dev/null 2>&1; RC=$?; set -e
check "missing asset: rejected without calling gh" '[ "$RC" -ne 0 ] && [ ! -s "$T/log" ]'

[ "$failures" -eq 0 ] || { echo "$failures failure(s)" >&2; exit 1; }
echo "all publish-rolling-release tests passed"

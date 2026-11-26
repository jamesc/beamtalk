#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Tests scripts/ci/publish-guard.sh and scripts/ci/verify-release-assets.sh
# (BT-5313), the latter against a stub `gh`.
# shellcheck disable=SC2015  # pass/fail always return 0, so A && B || C is if/else here
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "${tmp}"' EXIT
failures=0
pass() { echo "ok: $1"; }
fail() { echo "FAIL: $1" >&2; failures=$((failures + 1)); }

# guard <mode|-> <caller>: exit status of the guard (mode '-' = unset)
guard() {
    local mode="$1" caller="$2"
    if [ "${mode}" = - ]; then
        env -u RELEASE_PUBLISH_MODE GITHUB_REPOSITORY=o/r GITHUB_WORKFLOW_REF="${caller}" \
            "${here}/publish-guard.sh" test >/dev/null 2>&1
    else
        RELEASE_PUBLISH_MODE="${mode}" GITHUB_REPOSITORY=o/r GITHUB_WORKFLOW_REF="${caller}" \
            "${here}/publish-guard.sh" test >/dev/null 2>&1
    fi
}
PROMOTE="o/r/.github/workflows/promote.yml@refs/heads/main"
RELEASE="o/r/.github/workflows/release.yml@refs/tags/v0.6.0"

guard - "${RELEASE}" && pass "unset mode is legacy: release.yml may publish" || fail "unset mode refused release.yml"
guard legacy "${RELEASE}" && pass "legacy: release.yml may publish" || fail "legacy refused release.yml"
guard promote "${PROMOTE}" && pass "promote: promote.yml on main may publish" || fail "promote refused promote.yml on main"
guard promote "${RELEASE}" && fail "promote: release.yml was allowed" || pass "promote: release.yml is refused"
guard promote "o/r/.github/workflows/promote.yml@refs/heads/feature" \
    && fail "promote: promote.yml on a branch was allowed" || pass "promote: promote.yml off main is refused"
guard promote "x/r/.github/workflows/promote.yml@refs/heads/main" \
    && fail "promote: another repo's promote.yml was allowed" || pass "promote: another repo is refused"
guard bogus "${PROMOTE}" && fail "an unknown mode was allowed" || pass "an unknown mode is refused"

# ─── verify-release-assets.sh ────────────────────────────────────────────
# The stub copies the files of ${tmp}/release/<case>/ that match --pattern.
mkdir -p "${tmp}/bin"
cat > "${tmp}/bin/gh" <<'STUB'
#!/usr/bin/env bash
# release download <tag> --pattern <glob> --dir <dir>
[ "$1 $2" = "release download" ] || exit 2
shopt -s nullglob
n=0
for f in "${STUB_RELEASE}"/$5; do cp "$f" "$7/"; n=$((n + 1)); done
# Like gh: no matching asset is an error.
[ "$n" -gt 0 ] || { echo "no assets match the file pattern" >&2; exit 1; }
STUB
chmod +x "${tmp}/bin/gh"

mk() { # mk <case> <name> <content> [bad]: writes an asset and its sidecar
    mkdir -p "${tmp}/release/$1"
    printf '%s' "$3" > "${tmp}/release/$1/$2"
    if [ "${4:-}" = bad ]; then
        echo "$(printf 0%.0s {1..64})  $2" > "${tmp}/release/$1/$2.sha256"
    else
        (cd "${tmp}/release/$1" && sha256sum "$2" > "$2.sha256")
    fi
}
verify() { # verify <case> <min>
    STUB_RELEASE="${tmp}/release/$1" PATH="${tmp}/bin:${PATH}" \
        "${here}/verify-release-assets.sh" v0.6.0 beamtalk-ide-0.6.0- "$2" >/dev/null 2>&1
}

mk good beamtalk-ide-0.6.0-linux-x86_64.tar.gz a
mk good beamtalk-ide-0.6.0-macos-arm64.tar.gz b
mk good beamtalk-0.6.0-linux-x86_64.tar.gz other   # another channel: not matched
verify good 2 && pass "matching assets verify" || fail "matching assets refused"
verify good 3 && fail "too few assets accepted" || pass "too few assets are refused"

mk badhash beamtalk-ide-0.6.0-linux-x86_64.tar.gz a bad
verify badhash 1 && fail "a hash mismatch was accepted" || pass "a hash mismatch is refused"

mk nosidecar beamtalk-ide-0.6.0-linux-x86_64.tar.gz a
rm "${tmp}/release/nosidecar/beamtalk-ide-0.6.0-linux-x86_64.tar.gz.sha256"
verify nosidecar 1 && fail "a missing sidecar was accepted" || pass "a missing sidecar is refused"

mk orphan beamtalk-ide-0.6.0-linux-x86_64.tar.gz a
mk orphan beamtalk-ide-0.6.0-macos-arm64.tar.gz b
rm "${tmp}/release/orphan/beamtalk-ide-0.6.0-macos-arm64.tar.gz"
verify orphan 1 && fail "an orphan sidecar was accepted" || pass "an orphan sidecar is refused"

mkdir -p "${tmp}/release/empty"
verify empty 1 && fail "no assets accepted" || pass "no assets are refused"

[ "${failures}" -eq 0 ] || { echo "${failures} failure(s)" >&2; exit 1; }
echo "all publish-guard / verify-release-assets tests passed"

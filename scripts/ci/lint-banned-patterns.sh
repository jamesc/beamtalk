#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Guard (BT-5009): known-bad patterns that have a shared replacement stay at
# zero. Every hit prints file:line and the replacement to use. A legitimate
# exception goes in scripts/ci/banned-patterns-allowlist.txt (one line:
# `<pattern-id><TAB><path><TAB><reason>`); an allowlist entry that no longer
# matches anything fails the check, so the list cannot rot.
#
# Patterns are plain extended regexes applied line by line, so they only catch
# a call written on ONE line (a chain or `try` that rustfmt/erlfmt splits across
# lines gets past `chain-any` and `try-stop`). The `rust` kind skips tests/ dirs,
# tests.rs, test_support.rs, `//` comment lines and every inline
# `#[cfg(test)] mod … {` block (it ends at the closing `}` in column 0, where
# rustfmt puts a top-level module's brace). `rust-any` scans inline test modules
# too but still skips test files. The `erl` kind scans only */test/*.erl and
# skips `%` comment lines.
#
# Usage:
#   scripts/ci/lint-banned-patterns.sh               # check the repo
#   scripts/ci/lint-banned-patterns.sh --self-test   # prove each pattern fires on its fixture

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"
ALLOWLIST="scripts/ci/banned-patterns-allowlist.txt"

# id | kind | pathspecs (space separated) | ERE | replacement
# Fixture lines for --self-test live in scripts/ci/fixtures/banned-patterns/<id>.{rs,erl}.
PATTERNS=(
  "send-literal|rust|crates/beamtalk-codegen/src/core_erlang|'beamtalk_message_dispatch':'send'\(|runtime_send_call (dispatch_codegen/mod.rs)"
  "send-form-call|rust-join|crates/beamtalk-codegen/src/core_erlang|form::call\([[:space:]]*\"beamtalk_message_dispatch\",[[:space:]]*\"send\"|runtime_send_call (dispatch_codegen/mod.rs)"
  "raw-op-json|rust|crates/beamtalk-cli/src crates/beamtalk-mcp/src crates/beamtalk-lsp/src|\"op\"[[:space:]]*:|RequestBuilder (beamtalk-repl-protocol)"
  "catch-error-map|erl|runtime/apps|error:#\{error[[:space:]]*:=|?assertBtError macro family (beamtalk_test_assert.hrl)"
  "unique-integer|erl|runtime/apps|erlang:unique_integer\(|beamtalk_test_unique:id/0 / atom/1"
  "build-literal|rust|crates|\"_build\"|BuildLayout (beamtalk-core/src/build_layout.rs)"
  "putenv-home|erl|runtime/apps|os:putenv\(\"HOME\"|beamtalk_test_fs:with_tmp_home/1"
  "boot-compiler|erl|runtime/apps|ensure_all_started\(compiler\)|beamtalk_test_boot:compiler/0"
  "self-name|rust|crates/beamtalk-core/src|\.name == \"self\"|Expression::is_self()"
  "chain-any|rust-any|crates/beamtalk-core/src|superclass_chain\(.*\)\.any\(|is_subclass_of"
  "try-stop|erl|runtime/apps|try[[:space:]]+gen_server:stop|beamtalk_test_proc:stop/1 or beamtalk_test_class:stop/1"
)

# Emit `file:line:text` for every scannable line of the given files (NUL list
# on stdin). $1 = rust|erl|rust-any (rust-any keeps inline test modules in scope; test files are still skipped).
scan_files() {
  xargs -0 awk -v kind="$1" '
    FNR == 1 { intest = 0; pend = 0 }
    {
      t = $0; sub(/^[ \t]+/, "", t)
      if (kind != "erl" && kind != "rust-any") {
        if (pend && t !~ /^#\[/) { if (t ~ /^(pub(\([a-z]+\))? )?mod [A-Za-z0-9_]+ *\{/) intest = 1; pend = 0 }
        if (t == "#[cfg(test)]") pend = 1
        if (intest && t == "}" && $0 ~ /^}/) { intest = 0; next }
        if (intest) next
      }
      if (kind == "erl") { if (t ~ /^%/) next } else if (t ~ /^\/\//) next
      print FILENAME ":" FNR ":" $0
    }'
}

# Files for a pattern: tracked, right extension, no test code for non-test kinds.
files_for() { # kind pathspecs...
  local kind="$1"; shift
  local ext='\.rs$'; [[ "$kind" == erl ]] && ext='/test/[^/]*\.erl$'
  local specs=("$@")
  if [[ "$kind" == rust ]]; then
    git ls-files -z -- "${specs[@]}" | { grep -z -E "$ext" || true; } |
      { grep -z -v -E '(/tests/|/tests\.rs$|/test_support\.rs$)' || true; }
  else
    git ls-files -z -- "${specs[@]}" | { grep -z -E "$ext" || true; }
  fi
}

# Rust kind `rust-join` (scanned like `rust`): each line is matched together with the two lines
# after it, so a call split across lines is still seen. Reported at its first line.
join_lines() { # $1 = regex; the match must start on the first of the joined lines
  RE="$1" awk '
    { n++; line[n] = $0; match($0, /^[^:]+:[0-9]+:/); pre[n] = substr($0, 1, RLENGTH); file[n] = substr(pre[n], 1, index(pre[n], ":") - 1) }
    END {
      re = ENVIRON["RE"]
      for (i = 1; i <= n; i++) {
        out = line[i]
        for (j = i + 1; j <= i + 2 && j <= n && file[j] == file[i]; j++) {
          t = substr(line[j], length(pre[j]) + 1); sub(/^[ \t]+/, "", t); out = out " " t
        }
        if (match(out, re) && RSTART > length(pre[i]) && RSTART <= length(line[i])) print line[i]
      }
    }'
}

# stdin: NUL file list; $1 = scan kind; $2 = regex. Prints matching file:line:text.
match_lines() {
  if [[ "$1" == rust-join ]]; then scan_files "$1" | join_lines "$2" || true
  else scan_files "$1" | grep -E -- ":[0-9]+:.*($2)" || true; fi
}

self_test() {
  local fail=0 spec id kind specs re repl fx
  for spec in "${PATTERNS[@]}"; do
    IFS='|' read -r id kind specs re repl <<<"$spec"
    fx=$(ls scripts/ci/fixtures/banned-patterns/"$id".* 2>/dev/null | head -1 || true)
    if [[ -z "$fx" ]]; then echo "❌ no fixture for pattern '$id'"; fail=1; continue; fi
    local skind="$kind"; [[ "$fx" == *.erl ]] && skind=erl
    if [[ -z "$(printf '%s\0' "$fx" | match_lines "$skind" "$re")" ]]; then
      echo "❌ pattern '$id' did not fire on $fx"; fail=1
    fi
  done
  # Sleep rule: the annotated fixture passes, the bare one is reported.
  [[ -z "$(sleep_hits scripts/ci/fixtures/banned-patterns/sleep-annotated.erl)" ]] || { echo "❌ annotated sleep flagged"; fail=1; }
  [[ -n "$(sleep_hits scripts/ci/fixtures/banned-patterns/sleep-bare.erl)" ]] || { echo "❌ bare sleep not flagged"; fail=1; }
  # Test-region and comment filtering for Rust scopes.
  local out
  out="$(printf '%s\0' scripts/ci/fixtures/banned-patterns/send-literal-in-test-mod.rs scripts/ci/fixtures/banned-patterns/send-literal-in-test-mod-attrs.rs | match_lines rust "'beamtalk_message_dispatch':'send'\\(")"
  [[ -z "$out" ]] || { echo "❌ test-module / comment lines were not skipped:"; echo "$out"; fail=1; }
  out="$(printf '%s\0' scripts/ci/fixtures/banned-patterns/send-literal-after-test-mod.rs | match_lines rust "'beamtalk_message_dispatch':'send'\\(")"
  [[ "$(grep -c . <<<"$out")" -eq 1 ]] || { echo "❌ production code after a mid-file test module must be scanned (want exactly 1 hit):"; echo "$out"; fail=1; }
  [[ $fail -eq 0 ]] && echo "✅ banned-patterns self-test: ${#PATTERNS[@]} patterns + sleep rule detected"
  return $fail
}

# `file:line:text` for each timer:sleep lacking `%% sleep: <why>` (put a `(class N)` on it when it has one) on its
# own line or within the three lines above.
SLEEP_AWK='
  FNR == 1 { for (i = 0; i < 4; i++) win[i] = "" }
  { t = $0; sub(/^[ \t]+/, "", t) }
  t !~ /^%/ && $0 ~ /(^|[^A-Za-z_])timer:sleep\(/ {
    ok = ($0 ~ /%% sleep:/)
    for (i = 1; i <= 3 && !ok; i++) if (win[(FNR - i) % 4] ~ /%% sleep:/) ok = 1
    if (!ok) print FILENAME ":" FNR ":" $0
  }
  { win[FNR % 4] = $0 }'
sleep_hits() { awk "$SLEEP_AWK" "$@"; }

if [[ "${1:-}" == "--self-test" ]]; then self_test; exit $?; fi

fail=0
declare -A used=()
report() { # id file:line:text replacement
  local id="$1" hit="$2" repl="$3" file="${2%%:*}"
  if awk -F'\t' -v id="$id" -v f="$file" '$1 == id && $2 == f { found = 1 } END { exit !found }' \
      <(grep -v '^#' "$ALLOWLIST" 2>/dev/null || true); then
    used["$id	$file"]=1
    return
  fi
  echo "❌ [$id] $hit"
  echo "   use: $repl"
  fail=1
}

for spec in "${PATTERNS[@]}"; do
  IFS='|' read -r id kind specs re repl <<<"$spec"
  fk="${kind%-any}"; fk="${fk%-join}"
  # shellcheck disable=SC2086
  hits="$(files_for "$fk" $specs | match_lines "$kind" "$re")"
  while IFS= read -r hit; do
    [[ -n "$hit" ]] && report "$id" "$hit" "$repl"
  done <<<"$hits"
done

# timer:sleep annotation rule (class N documents why the sleep is unavoidable).
while IFS= read -r hit; do
  [[ -n "$hit" ]] && report sleep-annotation "$hit" "a condition wait (beamtalk_test_wait:until/1), or annotate: %% sleep: <why> (class N)"
done < <(git ls-files -z -- runtime/apps | { grep -z -E '/test/[^/]*\.erl$' || true; } | xargs -0 awk "$SLEEP_AWK")

# Stale allowlist entries.
if [[ -f "$ALLOWLIST" ]]; then
  while IFS=$'\t' read -r id file reason; do
    [[ -z "$id" || "$id" == \#* ]] && continue
    if [[ -z "$reason" ]]; then echo "❌ allowlist entry '$id $file' has no reason"; fail=1; fi
    if [[ -z "${used["$id	$file"]:-}" ]]; then
      echo "❌ stale allowlist entry (matches nothing, delete it): $id	$file"
      fail=1
    fi
  done <"$ALLOWLIST"
fi

if [[ $fail -ne 0 ]]; then
  echo "Banned patterns found. Exceptions go in $ALLOWLIST with a reason."
  exit 1
fi
echo "✅ No banned patterns (${#PATTERNS[@]} patterns + sleep-annotation rule)"

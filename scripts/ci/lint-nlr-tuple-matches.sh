#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Guard (BT-3758): the `^` non-local-return tuple shape (`{'$bt_nlr', Token,
# Value}` / `{'$bt_nlr', Token, Value, State}`, ADR 0041) has one definition,
# `?IS_NLR(T)` in runtime/apps/beamtalk_runtime/include/beamtalk.hrl, built on
# `?BT_NLR_TAG` from the generated beamtalk_sentinel_tags.hrl. Both spellings
# (the quoted atom and the macro) count as hand-rolling. Runtime
# and stdlib Erlang sources must use `throw:Nlr:Stack when ?IS_NLR(Nlr)` (or
# `{error, Nlr} when ?IS_NLR(Nlr)`) instead of hand-rolling `{'$bt_nlr', ...}`
# patterns, so a future shape change cannot silently miss a call site.
#
# Scans tracked `.erl`/`.hrl` files under runtime/apps/*/src and
# runtime/apps/*/include for any use of the quoted atom `'$bt_nlr'` or `?BT_NLR_TAG` (a tuple
# pattern on one or several lines, or a hand-rolled `element(1, T) =:=` check).
# Prose is ignored: a hit that follows a `%` on the same line (an Erlang
# comment), and the elided `{'$bt_nlr', ...}` form used in `-doc` text.
#
# To clear a failure: replace the literal match with a `?IS_NLR(...)` guard.
# If the code genuinely has to construct or destructure the tuple, add the
# file to ALLOWLIST below with a one-line justification.
#
# Usage: scripts/ci/lint-nlr-tuple-matches.sh

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

ALLOWLIST=(
    # Generated from codegen's `sentinel_tags.rs` (BT-4901): the one definition
    # of the atom, as `?BT_NLR_TAG`.
    'runtime/apps/beamtalk_runtime/include/beamtalk_sentinel_tags.hrl'
    # Owns ?IS_NLR / ?NLR: builds the tuple shape from ?BT_NLR_TAG.
    'runtime/apps/beamtalk_runtime/include/beamtalk.hrl'
    # Distribution codec: destructures and rebuilds the tuple to walk its
    # Value field (ADR 0126 §5.1/§5.5).
    'runtime/apps/beamtalk_runtime/src/beamtalk_wire.erl'
)

ATOM="('\\\$bt_nlr'|\?BT_NLR_TAG)"
ELIDED="\{$ATOM, \.\.\.\}"

excludes=()
for f in "${ALLOWLIST[@]}"; do
    if [[ ! -f "$f" ]]; then
        echo "❌ lint-nlr-tuple-matches: allowlisted file '$f' no longer exists; update ALLOWLIST in $0"
        exit 1
    fi
    excludes+=(":(exclude)$f")
done

# `git grep -n` emits `path:line:text`. Drop comment hits, then the elided
# prose form; anything left uses the atom in code.
hits="$(git grep -nE "$ATOM" -- \
    'runtime/apps/*/src/*.erl' \
    'runtime/apps/*/src/*.hrl' \
    'runtime/apps/*/include/*.hrl' \
    "${excludes[@]}" \
    | grep -vE "^[^:]*:[0-9]+:.*%.*$ATOM" \
    | grep -vE "$ELIDED" || true)"

if [[ -n "$hits" ]]; then
    echo "❌ Hand-rolled {'\$bt_nlr', ...} matches found; use the ?IS_NLR(T) guard from beamtalk.hrl (BT-3758):"
    echo "$hits"
    exit 1
fi
echo "✅ No hand-rolled {'\$bt_nlr', ...} matches outside ?IS_NLR"

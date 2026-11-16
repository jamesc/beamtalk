#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Per-call dispatch overhead micro-benchmark (BT-4983).
#
#   benchmarks/dispatch-overhead/run.sh count [ROW_IDS...]   calls per operation (deterministic)
#   benchmarks/dispatch-overhead/run.sh time [ROUNDS] [ROW_IDS...]   ns and reductions per operation
#   benchmarks/dispatch-overhead/run.sh cost                 isolated cost of each leaf mechanism
#   benchmarks/dispatch-overhead/run.sh report [ROUNDS]      time x count x isolated cost: share of each row per mechanism
#
# BEAMTALK selects the compiler (default: `beamtalk` on PATH, else dist/bin/beamtalk);
# its sysroot supplies the runtime and stdlib, as in benchmarks/vs-elixir/run.sh.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
mode="${1:-count}"
shift || true

if [[ -z "${BEAMTALK:-}" ]]; then
  if command -v beamtalk >/dev/null 2>&1; then BEAMTALK=beamtalk; else BEAMTALK="$repo/dist/bin/beamtalk"; fi
fi
sysroot="$("$BEAMTALK" --print-sysroot)"

# --force: the build stamp does not cover the compiler.
(cd "$here/beamtalk" && "$BEAMTALK" build --force >&2)

out="$here/harness/_build"
mkdir -p "$out"
erlc -o "$out" "$here/harness/vd_bench.erl"

bt_ebin="$here/beamtalk/_build/dev/ebin"
if [[ -d "$sysroot/lib/beamtalk/lib" ]]; then
  libs=("$sysroot"/lib/beamtalk/lib/*/ebin)
else
  rt="$(dirname "$sysroot")/runtime"
  libs=("$rt"/_build/default/lib/*/ebin "$rt/apps/beamtalk_stdlib/ebin")
fi

case "$mode" in
  count|time|cost|report|keys) ;;
  *) echo "usage: $0 [count|time|cost|report|keys] ..." >&2; exit 2 ;;
esac
args="["
for a in "$@"; do args+="\"$a\","; done
args="${args%,}]"

export VD_BENCH_BT_EBIN="$bt_ebin"
export BEAMTALK_NO_FILE_LOG=1
exec erl -noshell -pa "${libs[@]}" "$bt_ebin" "$out" -eval "vd_bench:$mode($args), halt()."

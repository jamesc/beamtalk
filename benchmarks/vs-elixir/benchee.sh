#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Benchee port of the concurrent actor rows 16a-16d (BT-4935). Builds and sets
# up the node exactly as run.sh does, then runs elixir/bench/actors.exs.
#
#   benchmarks/vs-elixir/benchee.sh            # env knobs: see actors.exs
#   ERL_FLAGS="+S 1:1" BENCHEE_PARALLEL=1 benchmarks/vs-elixir/benchee.sh
#   BENCH_SCRIPT=bench/reductions.exs benchmarks/vs-elixir/benchee.sh   # exact reductions per start (BT-4997)
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"

if [[ -z "${BEAMTALK:-}" ]]; then
  if command -v beamtalk >/dev/null 2>&1; then BEAMTALK=beamtalk; else BEAMTALK="$repo/dist/bin/beamtalk"; fi
fi
sysroot="$("$BEAMTALK" --print-sysroot)"

(cd "$here/beamtalk" && "$BEAMTALK" build --force >&2)
# Benchee lives in the :bench env only; the default env (run.sh) never needs it.
(cd "$here/elixir" && MIX_ENV=bench mix deps.get >&2 && MIX_ENV=bench mix compile >&2)

out="$here/harness/_build"
mkdir -p "$out"
erlc -o "$out" "$here/harness/vs_bench.erl"

elixir_ebin="$(cd "$here/elixir" && elixir -e 'IO.write(:code.lib_dir(:elixir))')/ebin"
bt_ebin="$here/beamtalk/_build/dev/ebin"
bt_native_ebin="$here/beamtalk/_build/dev/native/ebin"
ex_build="$here/elixir/_build/bench/lib"

paths=()
if [[ -d "$sysroot/lib/beamtalk/lib" ]]; then
  libs=("$sysroot"/lib/beamtalk/lib/*/ebin)
else
  rt="$(dirname "$sysroot")/runtime"
  libs=("$rt"/_build/default/lib/*/ebin "$rt/apps/beamtalk_stdlib/ebin")
fi
for d in "${libs[@]}"; do paths+=("$d"); done
paths+=("$elixir_ebin")
for d in "$ex_build"/*/ebin; do paths+=("$d"); done
paths+=("$ex_build/ve_bench/consolidated" "$bt_ebin" "$bt_native_ebin" "$out")

export VS_BENCH_BT_EBIN="$bt_ebin"
export BEAMTALK_NO_FILE_LOG=1
cd "$here/elixir"
# Same launch as run.sh (plain erl, -pa so the consolidated protocols shadow
# the unconsolidated ones); the script is evaluated once Elixir is started.
exec erl -noshell +B -kernel logger_level error -pa "${paths[@]}" -eval \
  "{ok,_}=application:ensure_all_started(elixir), 'Elixir.Code':require_file(<<\"${BENCH_SCRIPT:-bench/actors.exs}\">>), halt()."

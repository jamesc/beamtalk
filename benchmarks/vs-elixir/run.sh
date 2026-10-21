#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Build both sides and run the Beamtalk vs Elixir microbenchmarks in one node.
#
#   benchmarks/vs-elixir/run.sh [bench|profile] [ROUNDS] [ROW_IDS]
#
#   bench    (default) interleaved rounds, markdown table on stdout
#   profile  per-row function call counts (what each operation calls)
#
# BEAMTALK selects the compiler (default: `beamtalk` on PATH, else the repo's
# dist/bin/beamtalk); its sysroot supplies the runtime and stdlib.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
mode="${1:-bench}"
rounds="${2:-7}"
rows="${3:-}"

if [[ -z "${BEAMTALK:-}" ]]; then
  if command -v beamtalk >/dev/null 2>&1; then
    BEAMTALK=beamtalk
  else
    BEAMTALK="$repo/dist/bin/beamtalk"
  fi
fi
sysroot="$("$BEAMTALK" --print-sysroot)"

# --force: the build stamp does not cover the compiler, so switching
# BEAMTALK must not reuse another compiler's output.
(cd "$here/beamtalk" && "$BEAMTALK" build --force >&2)
(cd "$here/elixir" && mix compile >&2)

out="$here/harness/_build"
mkdir -p "$out"
erlc -o "$out" "$here/harness/vs_bench.erl"

elixir_ebin="$(cd "$here/elixir" && elixir -e 'IO.write(:code.lib_dir(:elixir))')/ebin"
bt_ebin="$here/beamtalk/_build/dev/ebin"
ex_build="$here/elixir/_build/dev/lib/ve_bench"

# `erl -pa A B` puts B ahead of A: list lowest priority first, so the
# consolidated protocols (ours and Elixir's Enumerable etc.) shadow the
# unconsolidated ones in the ebin directories.
paths=()
if [[ -d "$sysroot/lib/beamtalk/lib" ]]; then
  libs=("$sysroot"/lib/beamtalk/lib/*/ebin)                # installed (dist)
else
  rt="$(dirname "$sysroot")/runtime"                       # dev tree
  libs=("$rt"/_build/default/lib/*/ebin "$rt/apps/beamtalk_stdlib/ebin")
fi
for d in "${libs[@]}"; do paths+=("$d"); done
paths+=("$elixir_ebin" "$ex_build/ebin" "$ex_build/consolidated" "$bt_ebin" "$out")

case "$mode" in
  bench) args="[\"$rounds\"${rows:+, \"$rows\"}]"; entry="main" ;;
  profile) args="[${rows:+\"$rows\"}]"; entry="profile" ;;
  *) echo "usage: $0 [bench|profile] [ROUNDS] [ROW_IDS]" >&2; exit 2 ;;
esac

export VS_BENCH_BT_EBIN="$bt_ebin"
export BEAMTALK_NO_FILE_LOG=1
exec erl -noshell -pa "${paths[@]}" -eval "vs_bench:$entry($args), halt()."

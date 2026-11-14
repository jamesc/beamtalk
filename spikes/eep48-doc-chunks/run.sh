#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# PROTOTYPE (BT-4852, ADR 0149). Runs the spike's experiments end to end.
# Needs `beamtalk`, `erl`/`erlc`/`escript` on PATH; `elixir`/`mix` for the
# Elixir and ExDoc steps. Set ERL_BIN to a second OTP's bin dir to repeat the
# OTP-only steps there. Everything is written under ./_out.
set -euo pipefail
cd "$(dirname "$0")"
rm -rf _out && mkdir -p _out

echo "### fixture: compile two classes with the real compiler, keep the .core"
(cd _out && beamtalk new demo >/dev/null && cp ../fixture/*.bt demo/src/ && cd demo && beamtalk build >/dev/null)
CORES=$(ls "$PWD"/_out/demo/_build/dev/ebin/bt@demo@{counter,tally}.core)

for BIN in "" ${ERL_BIN:+"$ERL_BIN/"}; do
  "${BIN}erlc" -o _out eep48_proto.erl eep48_runtime_proto.erl
  echo "### E1 doc attributes in Core Erlang";            "${BIN}escript" e1_core_attributes.escript
  echo "### E3 extra_chunks from Core Erlang";            "${BIN}escript" e3_extra_chunks.escript _out/beams beamtalk load $CORES
  echo "### E4 Erlang shell h/1, h/2"
  "${BIN}erl" -noshell -pa _out/beams -eval 'c:h(bt@demo@counter), c:h(bt@demo@counter, increment), c:h(bt@demo@tally, total), halt().'
  echo "### E5 memory-loaded modules";                    "${BIN}escript" e5_memory_loaded.escript $(echo $CORES | cut -d' ' -f1)
  echo "### E9 docs term carried in the Core module";     "${BIN}escript" e9_docs_carrier.escript
  echo "### E7 compile-time cost";                        "${BIN}escript" e7_compile_cost.escript $CORES
done
erlc -o _out eep48_proto.erl eep48_runtime_proto.erl

if command -v elixir >/dev/null; then
  escript e3_extra_chunks.escript _out/beams_noload beamtalk noload $CORES >/dev/null
  escript e3_extra_chunks.escript _out/beams_erlang erlang noload $CORES >/dev/null
  echo "### E8 IEx h";  elixir -pa _out/beams_noload e8_iex.exs
  escript e8_exdoc_fake_abstract_code.escript _out/beams_erlang _out/beams_erlang_abst
  echo "### E8 ExDoc (fetches ex_doc from Hex into exdoc/deps)"
  (cd exdoc && mix deps.get >/dev/null && mix deps.compile >/dev/null &&
    for V in beams_noload beams_erlang beams_erlang_abst; do
      echo "--- ExDoc over $V"; mix run run_exdoc.exs ../_out/$V ../_out/exdoc_$V 2>&1 | grep -E "warning|RuntimeError|entrypoint" | sort -u || true
    done)
fi

# E6 (whole stdlib, docs_v1 built from live class_state) needs an installed
# runtime; point BEAMTALK_LIB at its lib dir, e.g. /usr/local/lib/beamtalk/lib.
if [ -n "${BEAMTALK_LIB:-}" ]; then
  mkdir -p _out/stdlib/beamtalk_stdlib && cp -r "$BEAMTALK_LIB/beamtalk_stdlib/ebin" _out/stdlib/beamtalk_stdlib/
  E="$PWD/_out/stdlib/beamtalk_stdlib/ebin"
  echo "### E6 docs_v1 from class_state, whole stdlib"
  erl -noshell -pa _out $(ls -d "$BEAMTALK_LIB"/*/ebin | grep -v beamtalk_stdlib | sed 's/^/-pa /') -pa "$E" \
      -eval "eep48_runtime_proto:report(\"$E\"), halt()."
  escript e6_mapping_stats.escript "$E"
fi

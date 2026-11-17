# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Exact reductions per actor start for rows 16a, 16c and 16d (BT-4997). Run
# through benchee.sh so the node is set up as for the timing rows:
#
#   BENCH_SCRIPT=bench/reductions.exs VS_BENCH_NO_TRACE=1 ERL_FLAGS="+S 1:1" \
#     benchmarks/vs-elixir/benchee.sh
#
# The count is the VM-wide `erlang:statistics(reductions)` delta around N starts
# (all processes, so it includes the spawned actors, the monitoring caller and
# the runtime services the start touches), divided by the number of starts.
# Reductions are deterministic where the timings are not; the node should be
# otherwise idle. Env: RED_N (starts per row, default 3000).

:ok = :vs_bench.boot()

bt = :bt@vs_elixir_bench@vb_bench
bt_self = :beamtalk_class_registry.resolve_class_object(:VbBench)
n = String.to_integer(System.get_env("RED_N") || "3000")

# {row, class method, calls, starts per call}
rows = [
  {"16a", :"class_asyncStartRun:", n, 1},
  {"16c", :"class_spawnStop:", n, 1},
  {"16d", :"class_asyncStartBatch:", div(n, 100), 100}
]

# Warm up (code loaded, caches filled) before counting.
for {_, f, _, _} <- rows, do: apply(bt, f, [bt_self, 200])

for {row, f, calls, per_call} <- rows do
  :erlang.garbage_collect()
  Process.sleep(200)
  {r0, _} = :erlang.statistics(:reductions)
  t0 = System.monotonic_time(:nanosecond)
  apply(bt, f, [bt_self, calls])
  t1 = System.monotonic_time(:nanosecond)
  {r1, _} = :erlang.statistics(:reductions)
  starts = calls * per_call

  IO.puts(
    "RESULT #{row} starts=#{starts} reductions_per_start=#{Float.round((r1 - r0) / starts, 1)} " <>
      "us_per_start=#{Float.round((t1 - t0) / starts / 1000, 3)}"
  )
end

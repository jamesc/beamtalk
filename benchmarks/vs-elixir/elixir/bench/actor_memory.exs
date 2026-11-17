# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Memory per idle actor (BT-4997), for choosing `{beamtalk_runtime,
# actor_min_heap_size}`. Starts N idle `VbCounter` actors, keeps them alive, and
# prints the average `process_info(Pid, memory)` (bytes) and the change in
# `erlang:memory(:total)` per actor, plus the setting in force. Run through
# benchee.sh so the node is set up as for the timing rows:
#
#   BENCH_SCRIPT=bench/actor_memory.exs VS_BENCH_NO_TRACE=1 \
#     benchmarks/vs-elixir/benchee.sh                                   # unset
#   ERL_FLAGS="-beamtalk_runtime actor_min_heap_size 2000" BENCH_SCRIPT=bench/actor_memory.exs \
#     VS_BENCH_NO_TRACE=1 benchmarks/vs-elixir/benchee.sh               # 2000 words
#
# Env: MEM_N (actors, default 20000).

:ok = :vs_bench.boot()

n = String.to_integer(System.get_env("MEM_N") || "20000")
words = Application.get_env(:beamtalk_runtime, :actor_min_heap_size, :unset)

spawn_actor = fn ->
  # `VbCounter spawn` answers the actor object; its pid is the 4th element.
  elem(:bt@vs_elixir_bench@vb_counter.spawn(), 3)
end

# Warm up (code loaded, tables created) before taking the baseline.
for pid <- Enum.map(1..200, fn _ -> spawn_actor.() end), do: GenServer.stop(pid)
:erlang.garbage_collect()
Process.sleep(500)

before = :erlang.memory(:total)
pids = Enum.map(1..n, fn _ -> spawn_actor.() end)
:erlang.garbage_collect()
Process.sleep(500)
after_total = :erlang.memory(:total)

sample = Enum.take_random(pids, min(500, n))

per_proc =
  Enum.sum(Enum.map(sample, fn p -> elem(:erlang.process_info(p, :memory), 1) end)) / length(sample)

min_heap =
  sample
  |> Enum.map(fn p -> :erlang.process_info(p, :garbage_collection)[:garbage_collection] end)
  |> Enum.map(fn gc -> Keyword.get(gc, :min_heap_size) end)
  |> Enum.uniq()

IO.puts(
  "RESULT actor_min_heap_size=#{inspect(words)} actors=#{n} " <>
    "min_heap_size_words=#{inspect(min_heap)} " <>
    "process_memory_bytes_avg=#{Float.round(per_proc, 0)} " <>
    "vm_total_bytes_per_actor=#{Float.round((after_total - before) / n, 0)}"
)

for p <- pids, do: Process.unlink(p)
for p <- pids, do: GenServer.stop(p)

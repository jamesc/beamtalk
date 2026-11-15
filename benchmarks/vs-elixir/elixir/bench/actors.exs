# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Benchee port of rows 16a-16d (BT-4935). Run through benchee.sh, which sets up
# the same node as run.sh. Calls the same Beamtalk class methods and Elixir
# floor functions the harness calls, with N = 1 (one operation per call).
#
# Env: BENCHEE_PARALLEL (default 1), BENCHEE_TIME (s, default 5),
#      BENCHEE_WARMUP (s, default 2), BENCHEE_ROWS (default 16a,16b,16c,16d),
#      BENCHEE_SAVE (name to save under), BENCHEE_LOAD (saved name to compare),
#      BENCHEE_OVERHEAD=1 (empty scenario and no-op Beamtalk call only).

:ok = :vs_bench.boot()

env = fn k, d -> System.get_env(k) || d end
parallel = String.to_integer(env.("BENCHEE_PARALLEL", "1"))
time = String.to_integer(env.("BENCHEE_TIME", "5"))
warmup = String.to_integer(env.("BENCHEE_WARMUP", "2"))
rows = String.split(env.("BENCHEE_ROWS", "16a,16b,16c,16d"), ",")

bt = :bt@vs_elixir_bench@vb_bench
bt_self =
  case :beamtalk_class_registry.resolve_class_object(:VbBench) do
    :undefined -> raise "VbBench not registered"
    c -> c
  end

ex = VeBench

jobs = %{
  "16a" => {"16a async start+run+stop", :"class_asyncStartRun:", :spawn_monitor_exit, 1},
  "16b" => {"16b as 16a, init args", :"class_asyncStartRunWith:", :spawn_monitor_exit, 1},
  "16c" => {"16c Actor spawn + stop", :"class_spawnStop:", :gen_start_stop, 1},
  "16d" => {"16d 100 in flight (1 op = 100 starts)", :"class_asyncStartBatch:", :spawn_monitor_batch, 1}
}

common = [
  warmup: warmup,
  time: time,
  parallel: parallel,
  percentiles: [50, 90, 99],
  formatters: [{Benchee.Formatters.Console, extended_statistics: true}],
  print: [fast_warning: false]
]

common =
  common
  |> Keyword.merge(if s = System.get_env("BENCHEE_SAVE"), do: [save: [path: "bench/saves/#{s}.benchee", tag: s]], else: [])
  |> Keyword.merge(if l = System.get_env("BENCHEE_LOAD"), do: [load: "bench/saves/#{l}.benchee"], else: [])

IO.puts("# schedulers=#{:erlang.system_info(:schedulers_online)} otp=#{:erlang.system_info(:otp_release)} elixir=#{System.version()} parallel=#{parallel} time=#{time}s warmup=#{warmup}s")

if System.get_env("BENCHEE_OVERHEAD") == "1" do
  Benchee.run(
    %{
      "empty fun" => fn -> :ok end,
      "no-op Beamtalk call (asyncStartRun n=0)" => fn -> apply(bt, :"class_asyncStartRun:", [bt_self, 0]) end,
      "cheap Beamtalk row 11b (a + 1, typed)" => fn -> apply(bt, :"class_add:a:", [bt_self, 1, 5]) end
    },
    Keyword.merge(common, parallel: 1, formatters: [Benchee.Formatters.Console])
  )
else
  for id <- rows do
    {title, btf, exf, n} = Map.fetch!(jobs, id)
    IO.puts("\n## #{title}")

    Benchee.run(
      %{
        "Beamtalk #{id}" => fn -> apply(bt, btf, [bt_self, n]) end,
        "Elixir #{id}" => fn -> apply(ex, exf, [n, 0]) end
      },
      common
    )
  end
end

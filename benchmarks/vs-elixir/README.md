# Beamtalk vs Elixir microbenchmarks

Where does Beamtalk pay extra per operation, compared with the equivalent
Elixir on the same BEAM? Each row runs one operation N times on both sides,
in the same node and the same loop shape, and reports what it costs in
reductions, heap words and time. Each Beamtalk row also records what the
compiler emits for it and what the runtime then calls, so a number comes with
its cause.

This is research input for a static dispatch / zero-overhead sends ADR. It
changes nothing in the compiler or the runtime.

## Running it

```bash
just bench-vs-elixir                      # 7 interleaved rounds, all rows
just bench-vs-elixir bench 9 1a,7a,12b    # 9 rounds, chosen rows (row "0" always runs)
just bench-vs-elixir profile              # per-row function call counts
```

or `benchmarks/vs-elixir/run.sh [bench|profile] [ROUNDS] [ROWS]`. The script
builds the Beamtalk package with `beamtalk build --force`, compiles the mix
project, compiles the harness, then starts one `erl` node with the Beamtalk
runtime and stdlib, Elixir, and both sides' modules on the code path.
`BEAMTALK=/path/to/beamtalk` picks the compiler; its sysroot supplies the
runtime and stdlib (an installed `dist/` or a dev tree's `runtime/`), which is
how the in-flight PRs below were measured. Elixir comes from the repo's
`.tool-versions`.

| Path | What |
|---|---|
| `beamtalk/src/vb_bench.bt` | the Beamtalk rows: class methods of the sealed, stateless `VbBench` |
| `beamtalk/src/vb_*.bt` | fixtures: Values (sealed and open), a subclass, an actor, sealed and open class-side methods |
| `elixir/lib/ve_bench.ex` | the Elixir rows, `VeBench.<row>(n, ctx)` |
| `elixir/lib/fixtures.ex` | structs, a protocol with two implementations, a `defdelegate`, a GenServer |
| `harness/vs_bench.erl` | the row table, the measurement, the report, the profiler |
| `results/` | the runs this README quotes |

## Method

**One loop shape on both sides.** Every Beamtalk row is

```beamtalk
last := 0
n timesRepeat: [last := <operation>]
last
```

Because the block assigns `last`, the compiler inlines `timesRepeat:` as a
local `letrec` loop that carries `last` as a loop argument. The Elixir side's
`defbench` macro writes the same loop: count up from 1 with `=<`, carry the
last result. The result is live on both sides, so neither compiler can drop the
operation as dead code. Row `0` runs the loop with a constant body; every
figure below is net of its own side's row `0`. Both sides cost 1 reduction and
0 words per iteration for the bare loop.

**One harness, one node.** The Beamtalk rows are called as
`'class_<selector>'(nil, N, Ctx)`: `VbBench` is sealed and stateless, so its
class methods run in the calling process. Each measurement runs in a fresh
worker process. A controller process snapshots every process in the node,
traces every process's garbage collections (an OTP 28 trace session) while
the worker runs `Fun(N)`, then snapshots again. So:

- **reductions/op** is the sum over all processes of the `reductions` delta,
  divided by N. Work done for the operation in another process (an actor, a
  GenServer, a class process) counts.
- **words/op** is heap words allocated, divided by N: for each process, the
  growth of `heap_size + mbuf_size` between consecutive GCs inside the window,
  plus the growth since the last one. This counts young-heap and heap-fragment
  allocation in every process. Off-heap binaries and ETS are not included.
  Allocations of known size come out exact: `{:ok, x}` is 3 words,
  `Enum.map` over 100 elements is 200 (one cons cell each).
- **ns/op** is the worker's wall time for `Fun(N)`, divided by N.

The controller, the GC tracer and `erts_trace_cleaner` are excluded.
Reductions and words are deterministic: they are identical across rounds and
across runs to the first decimal place, and do not depend on load. Lead with
them. Time is the median of the rounds and is noisy on this machine. Note that
reductions count function calls, not BIF work: a `maps:put/3` or
`binary_to_list/1` is nearly free in reductions but not in time or words (rows
6b and 10e).

**Rounds.** A warm-up of min(N, 2000) operations, then N (5,000 to 200,000 by
row cost) per measurement, 9 rounds, interleaved, with the order of the two
sides alternating by round and by row.

**Diagnosis.** `run.sh profile` counts calls to every function in every loaded
module (call_count tracing, all processes) over 2,000 operations and prints the
functions called at least 0.5 times per operation (`results/profile.md`). The
Core Erlang the compiler kept is in `beamtalk/_build/dev/ebin/*.core`.

**Machine and load.** Apple M5 Max, 18 cores (18 schedulers), 64 GB, macOS
(Darwin 25.6), OTP 28 (erts 16.4), Elixir 1.20.1. Beamtalk at `dc216bf52`
(main, after BT-4053, BT-4064 and BT-4066), the installed `dist/` build. The
machine was shared with other builds and agents: load average 26.7 at the
start of the run, 19.6 at the end (1-minute), and between 26 and 59 during the
PR runs. Ratios of ns are indicative only. The reduction and word columns are
not affected.

## Results

Main at `dc216bf52`, 9 rounds (`results/bench.md`). BT = Beamtalk, EX =
Elixir; `+red` and `+words` are Beamtalk minus Elixir, per operation; `x`
columns are ratios, `-` where the Elixir figure is under 0.5. For the
collection rows (6b-6e, 11a) one operation is a whole pass over 100 elements.

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 95.6 | 26.0 | 10.0 | -0.04 | 0.00 | 0.00 | - | - | - | 26.0 | 10.0 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 158 | 29.0 | 28.0 | 1.68 | 0.00 | 0.00 | 94.1 | - | - | 29.0 | 28.0 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 |
| 1b | getter on an open Value: p x / p.x | 152 | 29.0 | 28.0 | 1.47 | 0.00 | 0.00 | 103.4 | - | - | 29.0 | 28.0 | send(P, x, []), same path as 1a |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 165 | 29.0 | 28.0 | 1.27 | 2.00 | 0.00 | 130.1 | 14.5 | - | 27.0 | 28.0 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 325 | 30.2 | 34.0 | 31.8 | 0.00 | 6.00 | 10.2 | - | 5.7 | 30.2 | 28.0 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 2790 | 147 | 480 | 32.3 | 0.03 | 6.00 | 86.4 | - | 80.0 | 147 | 474 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 0.94 | 2.00 | 0.00 | 0.83 | 2.00 | 0.00 | 1.1 | 1.0 | - | 0.00 | 0.00 | direct call Mod:'class_double:'(nil, 21) |
| 3b | class method, sealed class / Mod.fun(arg) | 2293 | 145 | 444 | 2.89 | 2.00 | 0.00 | 792.6 | 72.4 | - | 143 | 444 | whereis_class + class_send: not `class sealed`, so gen_server:call into the class process |
| 3c | class method, open class / Mod.fun(arg) | 2642 | 146 | 480 | 2.83 | 2.00 | 0.00 | 932.2 | 73.1 | - | 144 | 480 | whereis_class + class_send: gen_server:call into the class process |
| 4a | untyped receiver: o x / o.x | 169 | 29.0 | 28.0 | 1.53 | 0.00 | 0.00 | 110.7 | - | - | 29.0 | 28.0 | send(O, x, []), same path as 1a |
| 4b | untyped receiver: o area / protocol | 165 | 29.0 | 28.0 | 9.06 | 7.00 | 0.00 | 18.2 | 4.1 | - | 22.0 | 28.0 | send(O, area, []), same path as 1a |
| 4c | typed open Value: s area / Square.area(s) | 160 | 29.0 | 28.0 | 1.38 | 2.00 | 0.00 | 116.6 | 14.5 | - | 27.0 | 28.0 | send(S, area, []), same path as 1a |
| 5a | inherited method: s base / defdelegate | 198 | 33.1 | 31.0 | 1.55 | 3.00 | 0.00 | 127.1 | 11.0 | - | 30.1 | 31.0 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 457 | 61.2 | 59.0 | 1.95 | 4.00 | 0.00 | 234.7 | 15.3 | - | 57.2 | 59.0 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 166 | 29.1 | 28.0 | 12.2 | 0.00 | 0.00 | 13.6 | - | - | 29.0 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 1.09 | 2.00 | 0.00 | 0.71 | 2.00 | 0.00 | 1.5 | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 2348 | 303 | 404 | 278 | 303 | 0.00 | 8.4 | 1.0 | - | 0.07 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 342 | 303 | 0.00 | 314 | 303 | 0.00 | 1.1 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 1345 | 403 | 200 | 1544 | 404 | 200 | 0.9 | 1.0 | 1.0 | -1.00 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 230 | 302 | 0.00 | 261 | 303 | 0.00 | 0.9 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 3315 | 211 | 312 | 945 | 37.2 | 56.0 | 3.5 | 5.7 | 5.6 | 174 | 256 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 3528 | 211 | 318 | 965 | 37.2 | 56.0 | 3.7 | 5.7 | 5.7 | 174 | 262 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 2309 | 194 | 242 | 135 | 18.5 | 12.0 | 17.1 | 10.4 | 20.2 | 175 | 230 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 23170 | 849 | 3265 | 934 | 37.2 | 56.0 | 24.8 | 22.8 | 58.3 | 812 | 3209 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 991 | 48.3 | 101 | 2.61 | 0.00 | 0.00 | 379.8 | - | - | 48.3 | 101 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 61053 | 2658 | 22383 | 170 | 13.1 | 36.0 | 358.2 | 202.8 | 621.7 | 2645 | 22347 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 9a | (Result ok: x) value / {:ok, x} match | 3138 | 216 | 762 | 18.4 | 2.01 | 3.00 | 171.0 | 107.2 | 254.0 | 214 | 759 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 181 | 25.1 | 27.0 | 0.17 | 0.00 | 0.00 | - | - | - | 25.1 | 27.0 | send(R, isOk, []), same path as 1a |
| 10a | Dictionary at:put: / Map.put | 89.2 | 18.0 | 10.0 | 41.1 | 1.03 | 6.00 | 2.2 | 17.6 | 1.7 | 17.0 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 20.2 | 17.0 | 0.00 | 1.56 | 3.00 | 0.00 | 12.9 | 5.7 | - | 14.0 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 337 | 17.7 | 202 | 316 | 4.69 | 200 | 1.1 | 3.8 | 1.0 | 13.0 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 39.5 | 19.0 | 0.00 | 29.7 | 8.00 | 0.00 | 1.3 | 2.4 | - | 11.0 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 81.4 | 9.06 | 28.0 | 91.8 | 0.04 | 8.00 | 0.9 | - | 3.5 | 9.03 | 20.0 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 154 | 102 | 7.00 | 66.3 | 102 | 0.00 | 2.3 | 1.0 | - | 0.03 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.23 | 0.00 | 0.00 | 0.01 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 1.31 | 0.00 | 0.00 | 0.24 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 43.1 | 14.0 | 4.00 | 0.24 | 0.00 | 0.00 | - | - | - | 14.0 | 4.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 323 | 26.2 | 83.0 | 1.48 | 0.00 | 0.00 | 217.6 | - | - | 26.2 | 83.0 | beamtalk_primitive:class_of_object: ETS lookups + list_to_atom per call |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 6814 | 415 | 1358 | 11.1 | 0.00 | 0.00 | 611.2 | - | - | 415 | 1358 | class ref built per call (gen_server:call) + send(P, 'isKindOf:') -> includesBehaviour: via class processes |

The `Elixir alternates` and `Raw` sections of `results/bench.md` hold the other
Elixir forms of each row (`Point.x(p)`, a pattern match, `apply/3`, a dynamic
module call, `:lists.foldl`, `Enum.reduce` over a range) and the caller-only
share of each figure.

### What matches or beats Elixir

- **A `class sealed` method on a sealed class (3a)** compiles to a direct
  `Mod:'class_double:'(nil, 21)` and costs exactly what `Math.double(21)` costs.
  This is the static-dispatch target, and it already exists for one case.
- **Blocks (6a)**: `blk value: 41` is an `is_function/2` guard and an `apply`,
  the same as `fun.(41)`.
- **`collect:`, `inject:into:` and a `do:` that assigns nothing (6c-6e)**
  compile to `lists:map/2`, `lists:foldl/3` and `lists:foreach/2` behind an
  `is_list` guard. That matches Elixir in reductions and words and is a little
  faster than `Enum`, which goes through the consolidated `Enumerable`
  protocol first.
- **Typed integer arithmetic (11b)** is a bare `erlang:'+'`. Untyped (11c)
  adds an `is_number` guard, which costs about 1 ns and no reductions.
- **List `add:`/`includes:` and `String ++` (10c-10e)** are within 1.0-1.3x in
  time: the dispatch overhead (11-13 reductions) is small next to the O(n)
  work. String `++` does allocate 3.5x the words (below).
- **A dynamic send against an Elixir protocol (4b)** is the closest fight in
  the dispatch rows: 18x in time, 4x in reductions (29 vs 7). Elixir's
  consolidated protocol is not free either.

## Analysis

The overhead groups into three kinds. Each figure is the per-operation excess
over Elixir from the table.

### 1. Dispatch

- **Every instance-side send to a Value is a full dynamic send, whatever the
  receiver's static type.** `p x` on a sealed Value with a typed receiver
  (1a), on an open Value (1b), on an untyped receiver (4a), a user method on
  a sealed Value (1c), `r isOk` on the sealed `Result` (9b): all compile to
  `beamtalk_message_dispatch:send/3` and all cost the same, +27 to +29
  reductions, +28 words, about 160 ns, about 100x Elixir. The path is 22
  function calls: `classify_receiver`, `beamtalk_primitive:send` → `send_map`
  → `tagged_map_dispatch` → `tagged_class_dispatch`, an `ets:lookup` of the
  class row, a `persistent_term` and `atomics` read for the BT-4064 cache
  generation, `module_loaded` and `function_exported` checks, then
  `Mod:dispatch/3` and finally the method. **Type annotations currently buy
  nothing at a send site.**
- **Inheritance adds a hop (5a)**: the subclass's `dispatch/3` misses, checks
  the extension table, and calls the superclass's `dispatch/3`: +4
  reductions, +3 words on top of 1a.
- **A self-send inside an open Value is a second full send (5b)**: +57
  reductions, +59 words, about 230x.
- **Class-side sends that are not `class sealed` go to the class process
  (3b, 3c, 2b, 9a).** `VbMath double: 21` on a *sealed*, stateless class
  whose method is not declared `class sealed` compiles to
  `whereis_class` + `class_send`; `lookup_direct_call` finds no direct-call
  entry, so it becomes a `gen_server:call` into the class process, which
  installs class variables, seeds a session context, adopts the group leader
  and restores 5 process-dictionary keys around the call: +143 reductions,
  +444 words, 2.3 µs, about 800x. The open class (3c) costs the same. So do
  the keyword constructor of a Value (`VbPoint x: a y: 4`, 2b: +147
  reductions, +474 words) and `Result ok:` (9a: +214 reductions, +759 words).
  Every caller in the node also queues on that one process.
- **Conditionals and counted loops whose blocks assign nothing are sends
  (11d, 0b).** `a > 0 ifTrue: [1] ifFalse: [2]` compiles to
  `send(Bool, 'ifTrue:ifFalse:', [fun, fun])`: two closures, a dispatch to
  `True`, +14 reductions, +4 words, 41 ns against Elixir's 0.2 ns `case`. A
  `timesRepeat:` (or `to:do:`) whose block assigns nothing is a
  `timesRepeat:` send to `Integer`, whose stdlib loop then dispatches
  `ifTrue:ifFalse:` per step: +26 reductions and +10 words *per iteration*.
  Only the forms that thread a local are inlined.
- **Class objects are rebuilt per use (12a, 12b, 8a).** `p class` makes 2
  `list_to_atom` calls and ETS reads per call: +26 reductions, +83 words.
  `p isKindOf: VbPoint` builds the `VbPoint` class object (a
  `gen_server:call` for its module name) and then walks `includesBehaviour:`
  through class processes: +415 reductions, +1,358 words, 6.8 µs, against a
  `is_struct/2` guard.
- **Collection primitives go through dispatch (10a, 10b)**: `d at: #k` is
  +14 reductions for what is one `maps:get` (13x in time).

### 2. Allocation

- **The send path itself allocates 28 words** (1a) on every send, for a getter
  that allocates nothing in Elixir.
- **Local-state threading through maps (6b)**: a `do:` whose block assigns
  `sum` runs `lists:foldl` over a fun that `maps:get`s and `maps:put`s `sum` in
  a state map: 4 words per element (404 per pass) and 8x the time, where
  `inject:into:` (6e) threads the accumulator directly and matches Elixir.
- **Functional update (2a)**: `p withX: 3` is the 28-word send plus the 6-word
  map, against 6.
- **String `++` (10e)** converts both binaries to lists and back
  (`iolist_to_binary([binary_to_list(S), binary_to_list(T)])`): 28 words for
  an 11-byte string, where `<>` allocates 8, and it grows with the string.
- **Closures for non-inlined blocks (11d, 0b)**: 4 words per conditional.

### 3. Runtime machinery

- **Actor sends (7a-7c)**: after BT-4113, about +103 reductions and +89
  words per sync call over `GenServer.call` (was +174 / +256), and +91 / +68
  per cast (was +184 / +230). Of the ~141 reductions, 79 are in the caller
  (Elixir: 17) and about 61 in the actor (Elixir: 21). BT-4113 made the
  `[beamtalk, actor, dispatch, *]` telemetry spans opt-in (the always-on
  aggregates are now recorded directly), passed the actor's class with the
  send instead of scanning the instance registry, propagated only context
  that is present, dropped the liveness pre-check, and made the per-message
  `__local__` strip and pending-action checks O(1) in the common case. What
  remains: the always-on trace-store aggregate update (`record_dispatch`:
  an ETS lookup, three `atomics:add`s, `cas_min`, `cas_max`), the send
  wrapper chain (`sync_send_known` → `sync_send_pid` → `sync_send_remote` →
  `sync_call` → `dispatch_span`), `classify_receiver`'s class-object check
  (`atom_to_binary` + `binary:part` per send), and in the actor `make_self`
  (class name through `class_name_from_mod`), the mailbox-continuation check,
  actor-state install/uninstall, the call-`From` cell, the watch check in
  `notify_state_change`, `encode_reply_for_tagged` and the pending-action
  reads.
- **`self hasField:` inside an actor (7d, BT-4071)**: the selector is not in
  the class module, so it falls through to the runtime method resolver, which
  builds `CompiledMethod`s with xref provenance and copies the xref table: +812
  reductions, +3,209 words on top of the actor call, 23 µs.
- **`on:do:` (8a)** builds the `Error` class object on every entry (a
  `gen_server:call` for its module name) and snapshots class variables: +48
  reductions, +101 words, about 1 µs, for a `try` that costs Elixir nothing.
- **A raise (8b)**: `Error signal:` is a class send into the Error class
  process, the method raises there, the class process logs it with
  `?LOG_ERROR` (dropped by a domain filter after the event is built), the
  error is relayed back and rewrapped, and building the Beamtalk stack frames
  scans the whole class table (`module_to_class_map/0`, 128 ETS rows) per
  raise: +2,645 reductions, +22,347 words, 61 µs, 360x `raise`/`rescue`.

### The five largest costs for a Catalina-like workload

Catalina handles each message in an actor, through handler code built from
Value objects, `Result`s, class references and conditionals. In a recent
Catalina build, the compiled
`handler`, `socket`, `connection` and `listener` modules contain 190
`beamtalk_message_dispatch:send` sites, 8 `class_send`s and 11
`ifTrue:ifFalse:` sends between them. Ranked by excess per handled message,
weighted by how often each appears on the path:

| # | Cost | Excess per occurrence | Rows | Kind | Fixable? |
|---|---|---|---|---|---|
| 1 | Actor dispatch hooks (trace-store aggregates, send wrapper chain, `make_self`, mailbox/pending-action checks; telemetry spans opt-in since BT-4113) | +103 red, +89 words per sync send; +91 red, +68 words per cast (BT-4113; was +174 / +256 and +175 / +230) | 7a-7c | machinery | Partly: the aggregates are a product decision (BT-4103); the rest is not needed for the send itself. The gen_server round trip (37 red, 56 words) is the structural floor. |
| 2 | Class-side sends through the class process (non-`class sealed` class methods, Value keyword constructors, `Result ok:`, class objects in `on:do:` and `isKindOf:`) | +143 to +415 red, +444 to +1,358 words, 1-7 µs, and a node-wide serialization point | 3b, 3c, 2b, 9a, 8a, 12b | dispatch + machinery | Yes for sealed stateless classes and constructors (direct call); BT-4065 covers class objects, `isKindOf:` and `Result`. Classes with `classState:` structurally need their process for writes. |
| 3 | Dynamic sends to Value instances, regardless of static type | +27 to +29 red, +28 words, about 160 ns per send; a self-send is a second one | 1a-1c, 4a-4c, 5a, 5b, 9b, 10a, 10b | dispatch + allocation | Yes for sealed receivers (BT-4069). For open classes, late binding is structural, but its cost is not: a guarded direct call costs a map-key match. |
| 4 | Non-inlined conditionals and loops | +14 red, +4 words per conditional; +26 red, +10 words per loop step | 11d, 0b | dispatch + allocation | Yes: compile to `case` and `letrec` with a fallback send for non-Booleans. |
| 5 | Map-threaded locals in iteration blocks | +4 words per element, 8x time | 6b | allocation | Yes: thread as a fold accumulator, as `inject:into:` already does. |

Rarer on a message path but very expensive when hit: a raise (+2,645 red,
+22,347 words), `self hasField:` in an actor (+812 red, +3,209 words), and
`isKindOf:` (+415 red, +1,358 words).

**Structural vs fixable.** Structural: a sync actor send cannot be cheaper
than a `gen_server:call` (37 reductions, 56 words); a send to an open class
must check the receiver's class at run time (a guard, not a lookup); a class
with `classState:` needs its process for writes (ADR 0130); a non-local `^`
is a throw. Everything else measured here is fixable without changing the
language: it is either a send the compiler could resolve, a hook that could be
off, or a value rebuilt on every use.

## Effect of the in-flight PRs

Measured by building each PR (`BEAMTALK=<worktree>/target/debug/beamtalk`,
7 rounds; `results/bench-pr4504.md`, `results/bench-pr4503.md`,
`results/bench-pr4530.md`). Reductions and words per operation, Beamtalk
side, main → PR. Rows not listed did not move.

| Row | Main | #4504 (BT-4065) | #4503 (BT-4071) |
|---|---|---|---|
| 8a `on: Error do:` without a raise | 48.3 red / 101 w | **21.1 / 21** | 48.3 / 101 |
| 9a `(Result ok: x) value` | 216 / 762 | **68.2 / 76** | 218 / 772 |
| 12b `isKindOf:` | 415 / 1,358 | **118 / 194** | 412 / 1,368 |
| 12a `p class` | 26.2 / 83 | 25.2 / **44** | 26.2 / 83 |
| 2b `VbPoint x: a y: 4` | 147 / 480 | 143 / 371 | 148 / 494 |
| 3b, 3c class method, not `class sealed` | 145 / 444, 146 / 480 | 144 / 337, 144 / 366 | unchanged |
| 7d actor `self hasField:` | 849 / 3,265 | 819 / 2,760 | **265 / 430** |

- **BT-4065 (#4504)** removes the class-process round trip from class
  objects, `isKindOf:` and `Result ok:`, as it says. `Result ok:` is now a
  direct call; what is left of 9a is the `value` send (+25 reductions, the 9b
  path) plus the class-object build. `isKindOf:` is still a dynamic send
  plus a cached hierarchy check, 118 reductions against Elixir's 0. It does
  not cover user Value keyword constructors (2b) or class methods that are not
  `class sealed` (3b, 3c): those still `gen_server:call` the class process,
  only a little cheaper.
- **BT-4071 (#4503)** cuts `self hasField:` in an actor from +812 to +228
  reductions and from +3,209 to +374 words over a plain actor call. The rest
  is the runtime method walk, which a compiled `hasField:` would avoid.
- **BT-4069 (#4530)** compiles sends to a sealed-Value-typed receiver to a
  tag-guarded direct `Mod:dispatch/3` (byte-identical dynamic send on
  mismatch). Moved rows, main → PR, Beamtalk side (7 rounds,
  `results/bench-pr4530.md`): 1a sealed getter 29.1 red / 28 w → **3.00 / 0**
  (191 ns → 3.6 ns);
  1c sealed method 29.1 / 28 → **3.00 / 0**; 9b `isOk` 25.1 / 27 →
  **3.00 / 0**; 2a `withX:` 30.2 / 34 → **4.01 / 6** (the 6 words are
  Elixir's too — the `maps:put` result, not the send). Unmoved as designed:
  1b, 4a–4c, 5a, 5b (open or untyped receivers stay dynamic); 10a, 10b
  (`at:`/`at:put:` are dict intrinsics claimed before dispatch selection, and
  Dictionary instances are untagged maps the guard could never match); 9a's
  `value` (the block-or-send handler owns the `value` selector).
- **BT-4153** adds rows 13a–13d (FFI call overhead vs raw `apply/3`) and a
  fast path: result-shape-first coercion plus a compiler-precomputed
  `stock`/`beamtalk` mode, so the per-call `atom_to_list` and prefix scans
  are gone (7 rounds, `results/bench-bt4153-before.md`,
  `results/bench-bt4153-after.md`). Beamtalk side, before → after: 13a
  integer result 16.1 red / 14 w → **7.01 / 2** (111 ns → 29.5 ns); 13b
  caller-typed binary 16.1 / 17 → **7.03 / 5**; 13c `{ok, V}` 19.2 / 25 →
  **13.1 / 17**; 13d bare `ok` 19.2 / 23 → **10.0 / 7**. The `Result` wrap on
  13c/13d stays — it is mandatory semantics, still applied on the slow path.
  Non-FFI rows match the same base run-for-run; multi-process rows (3b, 3c,
  7a, 7b, 8b) wobble a few words between run configurations with or without
  this change.
- **BT-4221 (#4649)** adds row 12c (`s isKindOf: String`, a built-in class)
  and memoises the `isKindOf:` decision for instance receivers under
  `{is_kind_of, Class, Behaviour}` in the derived cache (7 rounds, idle
  machine, `results/bench-bt4221-before.md`, `results/bench-bt4221-after.md`).
  Beamtalk side, before → after: 12b 74.1 red / 108 w → **52.2 / 67**;
  12c 93.1 / 111 → **71.2 / 70**. Time barely moves (12c 2.95 µs → 2.82 µs,
  within noise): what is left is not the decision but the `String` class
  reference built per use and the inherited-method walk to `Object>>isKindOf:`
  (BT-4260).
- **BT-4113 (#4656)** makes the actor dispatch telemetry events opt-in
  (the always-on `{Class, Selector}` aggregates are recorded directly), passes
  the class with the send, drops the liveness pre-check and empty context
  propagation, and skips the receiver's `__local__` key scan when the
  committed state holds exactly the declared keys (7 rounds, idle machine,
  before = main c5b7f23, after = this branch at 487fb54;
  `results/bench-bt4113-before.md`, `results/bench-bt4113-after.md`).
  Beamtalk side, before → after: 7a 226 red / 311 w / 10.5 µs →
  **140 / 149 / 5.3 µs**; 7b 226 / 317 / 12.7 µs → **140 / 155 / 4.1 µs**;
  7c 199 / 239 / 8.5 µs → **110 / 76 / 1.8 µs**; 7d 283 / 388 / 14.9 µs →
  **196 / 226 / 4.8 µs** (Elixir `GenServer.call` 37.1 / 56 / ~2.5 µs, cast
  18.6 / 12). The review fix that makes the state guard match every declared
  key, not just the count, leaves the reductions where they were (140, 140,
  110, 196–198 in single-round reruns); words stay within the few-word
  run-to-run wobble of these multi-process rows. What remains, about 100
  reductions over Elixir per call, is `make_self` per message, the
  `classify_receiver` class-object check, the send wrapper chain, the
  always-on aggregate atomics and the mailbox/`From`/watch/reply checks
  (BT-4265).

# BT-5145: baseline before ADR 0153 (class variables)

The "before" numbers for epic BT-5144. BT-5150 migrates the benchmarks off `classState:` and **the "after" run (BT-5164 and the Phase 3 gate in ADR 0153) compares against this file.** It records what the class-variable snapshot before every `on:do:`, `protect/1` in `Result tryDo:`, and the class-variable input to the class-send guard cost today.

Nothing here edits a benchmark; the rows below are the ones that exist on `main`. Rows 3f, 3g, 3h, 3i, 3j and c4 are *controls that go away or change meaning* when class variables are removed (3f, 3g, 3h, c4 are stateful-class rows; 3i, 3j are ADR 0152's). Keep them in the "before" table so the "after" table shows what the rows' removal does not hide.

## Conditions

| | |
|---|---|
| Commit | `main` at `b464155` (BT-5128), clean tree; this PR adds documents only |
| Compiler | `target/debug/beamtalk` (dev build), runtime from `runtime/_build/default` (dev tree), as in `bench-bt4298-baseline.md` |
| Toolchain | OTP 28 (erts 16.4), Elixir 1.20.1, 4 schedulers, a 4-core cloud container (not a quiet machine; load average 2.3-3.1 during the runs) |
| Date | 2026-11-24 |
| Determinism | Reductions and heap words are deterministic. **Time is noisy on this host: compare ns only within one run, or by ratio to the Elixir column**, and re-run before and after on the same machine. The dev build of the compiler makes the Beamtalk *loop* slower than a release build; the class-send and `on:do:` costs are in the runtime, which is the same code in both |

## 1. vs-elixir, rows 3a-3j, 8a, 8b

```bash
BEAMTALK=target/debug/beamtalk just bench-vs-elixir bench 7 3a,3b,3c,3d,3e,3f,3g,3h,3i,3j,8a,8b
```

Rows 3a-3j are the class-side sends (3f-3h are the stateful-class rows; 3g and 3h fake the class-variable flag with `put_class_variables/2`). Rows 8a and 8b are `on: Error do:` without and with a raise: the "class_vars snapshot" in row 8a's "compiles to" column is the cost ADR 0153 removes from every compiled `on:do:`.

# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 22:58:54 up 11 min,  0 user,  load average: 2.42, 3.10, 1.85
# rounds 7

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 21.3 | 3.00 | 0.00 | 4.47 | 2.00 | 0.00 | 4.8 | 1.5 | - | 1.00 | 0.00 | guarded direct call: one persistent_term:get of the class's static call key (an atom), its {Mod, Fun, Tag} target matched in the map to ClassRef (BT-4316, BT-4442), then Mod:'class_double:'(ClassRef, 21) |
| 3b | class method, sealed class / Mod.fun(arg) | 19.7 | 3.00 | 0.00 | 4.74 | 2.00 | 0.00 | 4.2 | 1.5 | - | 1.00 | 0.00 | guarded direct call, as 3a: `class sealed` plays no part (ADR 0142, BT-4301) |
| 3c | class method, open class / Mod.fun(arg) | 26.3 | 3.00 | 0.00 | 4.38 | 2.00 | 0.00 | 6.0 | 1.5 | - | 1.00 | 0.00 | guarded direct call, as 3a: open class (ADR 0142, BT-4301) |
| 3d | class method, class in a variable / mod.fun(arg) | 608 | 24.1 | 42.0 | 20.9 | 2.00 | 0.00 | 29.1 | 12.0 | - | 22.1 | 42.0 | send(C, 'double:', [21]) -> class_send: runs in the caller (ADR 0142, BT-4300) |
| 3e | inherited class method, open stateless subclass / Mod.fun(arg) | 958 | 31.1 | 36.0 | 3.80 | 2.00 | 0.00 | 252.0 | 15.5 | - | 29.1 | 36.0 | whereis_class + class_send: inherited method resolved by the owner memo, runs in the caller (BT-4300) |
| 3f | class method, class with classState: / Mod.fun(arg) | 6518 | 140 | 167 | 4.19 | 2.00 | 0.00 | 1555.8 | 69.8 | - | 138 | 167 | whereis_class + class_send: gen_server:call; stays there under ADR 0142 (class state) |
| 3g | class method, class variables published after compile / Mod.fun(arg) | 6825 | 165 | 187 | 5.29 | 2.00 | 0.00 | 1289.5 | 82.7 | - | 163 | 187 | guarded direct call whose key is never installed: no target in the class's static call map -> static_call_target/4 declines (class variables) -> whereis_class + class_send: gen_server:call (BT-4431) |
| 3h | as 3g, class in a variable / mod.fun(arg) | 6080 | 142 | 171 | 21.0 | 2.00 | 0.00 | 289.6 | 71.2 | - | 140 | 171 | send(C, 'double:', [21]) -> class_send: gen_server:call (control for 3g, BT-4431) |
| 3i | class method shadowed by a class-side extension / Mod.fun(arg) | 6690 | 150 | 199 | 5.01 | 2.00 | 0.00 | 1335.3 | 74.7 | - | 148 | 199 | guarded direct call whose key is never installed: no target in the class's static call map -> static_call_target/4 declines (extension) -> whereis_class + class_send runs the extension (BT-4431) |
| 3j | as 3i, class in a variable / mod.fun(arg) | 5500 | 127 | 183 | 24.1 | 2.00 | 0.00 | 227.8 | 63.2 | - | 125 | 183 | send(C, 'double:', [21]) -> class_send runs the extension (control for 3i, BT-4431) |
| 8a | on: Error do: without a raise / try-rescue | 67.7 | 5.00 | 0.00 | 4.30 | 0.00 | 0.00 | 15.8 | - | - | 5.00 | 0.00 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 2210 | 99.1 | 167 | 574 | 13.1 | 36.0 | 3.8 | 7.6 | 4.6 | 86.0 | 131 | as 8a + `Error signal:` raised in the caller behind its caller-safe raise key (one persistent_term read, BT-4270), wrapped and class-matched; frames built only on read |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 3a | `remote_call` | 4.47 | 2.00 | 0.00 |
| 3b | `remote_call` | 4.74 | 2.00 | 0.00 |
| 3c | `remote_call` | 4.38 | 2.00 | 0.00 |
| 3d | `dyn_remote_call` | 20.9 | 2.00 | 0.00 |
| 3d | `remote_call` | 3.75 | 2.00 | 0.00 |
| 3e | `remote_call` | 3.80 | 2.00 | 0.00 |
| 3f | `remote_call` | 4.19 | 2.00 | 0.00 |
| 3g | `remote_call` | 5.29 | 2.00 | 0.00 |
| 3h | `dyn_remote_call` | 21.0 | 2.00 | 0.00 |
| 3h | `remote_call` | 4.62 | 2.00 | 0.00 |
| 3i | `remote_call` | 5.01 | 2.00 | 0.00 |
| 3j | `dyn_remote_call` | 24.1 | 2.00 | 0.00 |
| 3j | `remote_call` | 4.41 | 2.00 | 0.00 |
| 8a | `try_no_raise` | 4.30 | 0.00 | 0.00 |
| 8b | `try_raise` | 574 | 13.1 | 36.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.05 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.33 | 1.00 | 0.00 | 1.00 | 0.00 |
| 3a | bt | classSealedMethod: | 23.4 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3a | ex | remote_call | 5.80 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 21.8 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3b | ex | remote_call | 6.07 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 28.3 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3c | ex | remote_call | 5.71 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3d | bt | dynClassMethod:c: | 610 | 25.1 | 42.0 | 25.1 | 42.0 |
| 3d | ex | dyn_remote_call | 22.2 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3d | ex | remote_call | 5.07 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3e | bt | inheritedClassMethod: | 960 | 32.1 | 36.0 | 32.1 | 36.0 |
| 3e | ex | remote_call | 5.13 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3f | bt | statefulClassMethod: | 6520 | 141 | 167 | 52.2 | 78.0 |
| 3f | ex | remote_call | 5.52 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3g | bt | lateStateClassMethod: | 6827 | 166 | 187 | 78.2 | 100 |
| 3g | ex | remote_call | 6.62 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3h | bt | dynClassMethod:c: | 6082 | 143 | 171 | 55.2 | 84.1 |
| 3h | ex | dyn_remote_call | 22.3 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3h | ex | remote_call | 5.95 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3i | bt | shadowedClassMethod: | 6692 | 151 | 199 | 78.2 | 100 |
| 3i | ex | remote_call | 6.34 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3j | bt | dynClassMethod:c: | 5502 | 128 | 183 | 55.2 | 84.1 |
| 3j | ex | dyn_remote_call | 25.5 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3j | ex | remote_call | 5.73 | 3.00 | 0.00 | 3.00 | 0.00 |
| 8a | bt | onDoNoRaise: | 69.8 | 6.00 | 0.00 | 6.00 | 0.00 |
| 8a | ex | try_no_raise | 5.62 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 2212 | 100 | 167 | 100 | 167 |
| 8b | ex | try_raise | 576 | 14.1 | 36.0 | 14.1 | 36.0 |


## 2. dispatch-overhead, rows e1, e2, c1-c5

```bash
BEAMTALK=target/debug/beamtalk just bench-dispatch time 9 e1 e2 c1 c2 c3 c4 c5
```

# load average before: 22:59:30 up 12 min,  0 user,  load average: 2.00, 2.93, 1.84

| row | operation | ns/op net | reductions/op net |
|---|---|--:|--:|
| c1 | `class sealed` on a sealed class | 18.6 | 3.0 |
| c2 | class method on a sealed class | 24.1 | 3.0 |
| c3 | class method on an open class | 25.7 | 3.0 |
| c4 | class method on a class with classState: | 3528.8 | 51.2 |
| c5 | class method, class in a variable | 307.6 | 24.0 |
| e1 | on: Error do: without a raise | 65.5 | 5.0 |
| e2 | ensure: without a raise | 2.0 | 4.0 |

(baseline loop: 1.3 ns/op, 1.0 reductions/op; indicative only)

c4 is the class with `classState:`; c5 is a class held in a variable. e1 is `on: Error do:` without a raise (it carries the snapshot and restore); e2 is `ensure:` (it does not).

## 3. `runtime/perf/self_send_bench`: `on:do:` in a class and in an object

Three consecutive runs of `cd runtime/perf/self_send_bench && target/debug/beamtalk run SsbMain run` (`PERF: <name> <ns>ns/op`). The rows that move are `class_on_do_raise_loop` (a class with `classState:`, so its class methods run in the class process) against `instance_on_do_raise_loop` (the object control), and `class_var_10r_3w_loop*`. These rows disappear or are rewritten by BT-5150, so the "after" comparison uses the object twin.

| Row | run 1 | run 2 | run 3 | median |
|---|--:|--:|--:|--:|
| `actor_self_send_open` | 55 | 57 | 59 | **57** |
| `actor_self_send_sealed` | 69 | 69 | 80 | **69** |
| `actor_self_send_inherited_override` | 55 | 52 | 52 | **52** |
| `class_self_send_open` | 42 | 40 | 51 | **42** |
| `class_self_send_open_in_arm` | 42 | 41 | 49 | **42** |
| `class_self_send_sealed` | 40 | 40 | 42 | **40** |
| `class_self_send_inherited_override` | 688 | 714 | 774 | **714** |
| `class_self_send_inherited_plain` | 1113 | 1165 | 1159 | **1159** |
| `class_var_10r_3w_loop` | 456 | 384 | 415 | **415** |
| `class_var_10r_3w_loop_threaded` | 447 | 384 | 435 | **435** |
| `instance_on_do_loop` | 84 | 81 | 83 | **83** |
| `class_on_do_raise_loop` | 1479 | 1235 | 1277 | **1277** |
| `instance_on_do_raise_loop` | 1923 | 1064 | 1086 | **1086** |
| `actor_counter_loop_untyped` | 36 | 25 | 25 | **25** |
| `actor_counter_loop_typed` | 85 | 87 | 89 | **87** |
| `actor_counter_send_untyped` | 2714 | 2496 | 2717 | **2714** |
| `actor_counter_send_typed` | 2488 | 2424 | 2548 | **2488** |

## 4. `Result tryDo:`

There is **no `Result tryDo:` row** in `benchmarks/vs-elixir` or `benchmarks/dispatch-overhead` (row 9a is `Result ok:`). Adding one belongs to the measurement issue. For the baseline, the runtime function was timed directly with a throw-away escript (not checked in; the script is the short listing below), 200,000 calls per round, median of 9 rounds, after one warm-up round. `bare` is a plain Erlang `try ... catch` around the same fun, `tryDo` is `beamtalk_result:tryDo/1` (Tier 1: runs the block under `beamtalk_class_vars:protect/1` and builds the Result):

| | ns/op |
|---|--:|
| bare try, block answers | 8.4 |
| `beamtalk_result:tryDo/1`, block answers | 23.0 |
| bare try/catch, block raises | 76.8 |
| `beamtalk_result:tryDo/1`, block raises | 1207.0 |

The raising case includes building the error `Result` (a `#beamtalk_error{}` and its wrapping), not only `protect/1`, so the part ADR 0153 removes is the difference *within a column over time*, not the gap between the columns.

```erlang
main(_) ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    N = 200000, Rounds = 9,
    Ok = fun() -> 1 end,
    Raise = fun() -> erlang:error(boom) end,
    Bare = fun(F) -> try F() catch _:_ -> err end end,
    Time = fun(Fun) ->
        T0 = erlang:monotonic_time(nanosecond),
        loop(Fun, N),
        (erlang:monotonic_time(nanosecond) - T0) / N
    end,
    Med = fun(L) -> lists:nth((length(L) + 1) div 2, lists:sort(L)) end,
    Rs = [{bare_ok, fun() -> Bare(Ok) end},
          {tryDo_ok, fun() -> beamtalk_result:tryDo(Ok) end},
          {bare_raise, fun() -> Bare(Raise) end},
          {tryDo_raise, fun() -> beamtalk_result:tryDo(Raise) end}],
    _ = [Time(F) || {_, F} <- Rs],
    [{Name, Med([Time(F) || _ <- lists:seq(1, Rounds)])} || {Name, F} <- Rs].
```

Run with `cd runtime && ERL_LIBS=_build/default/lib escript <script>` after `just build`.

## How to compare

Same commands, same machine, same build (dev or release, not mixed), after ADR 0152 and ADR 0153 have both landed (Decided question 4 in ADR 0153 and BT-5164). Compare reductions and words exactly; compare time as a ratio to the Elixir column or to the loop's own row in the same run. Rows removed by BT-5150 (3f-3h, c4, `ssb_class_var*`) have no "after"; the question for the rest is whether rows 3a-3e, 8a, 8b, e1 and the object `on:do:` rows moved.

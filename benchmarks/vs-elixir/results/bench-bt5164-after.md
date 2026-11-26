# BT-5164: after ADR 0152 and ADR 0153, against the BT-5145 baseline

The "after" run for `bench-bt5145-baseline.md`, and the measurement behind the question "can the per-send class guard read (BT-4987) go?". No guard code was changed.

## Conditions

| | |
|---|---|
| Base | `b464155e` (BT-5128), the commit `bench-bt5145-baseline.md` was measured at, built in its own worktree |
| Change | `main` at `1e3755e` (after BT-5141, BT-5155 to BT-5163) |
| Compiler | `target/debug/beamtalk` (dev build) on both sides, runtime from each tree's `runtime/_build/default` |
| Host | 4-core cloud container, OTP 28, Elixir 1.20.1, not a quiet machine |
| Method | Three runs, **interleaved** (base, main, base, main, base, main), same host, same build profile. Each run is `bench-vs-elixir bench 5 <rows>` followed by `bench-dispatch time 7 <rows>`. Tables show the median of the three runs. Reductions and words are exact; time is noisy |

```bash
for i in 1 2 3; do for tree in base main; do
  BEAMTALK=$tree/target/debug/beamtalk just bench-vs-elixir bench 5 3a,3b,3c,3d,3e,8a,8b
  BEAMTALK=$tree/target/debug/beamtalk just bench-dispatch time 7 e1 e2 c1 c2 c3 c5
done; done
```

Rows 3f-3j and c4 no longer exist (ADR 0152 and ADR 0153 removed what they measured), so they have no "after".

## 1. vs-elixir (BT net ns, median of 3 runs; reductions and words exact)

| Row | Operation | base ns | main ns | base red | main red | base words | main words |
|---|---|--:|--:|--:|--:|--:|--:|
| 3a | `class sealed` method, guarded direct call | 22.3 | 26.5 | 3.0 | 3.0 | 0 | 0 |
| 3b | class method, sealed class | 21.7 | 21.4 | 3.0 | 3.0 | 0 | 0 |
| 3c | class method, open class | 23.7 | 33.5 | 3.0 | 3.0 | 0 | 0 |
| 3d | class in a variable | 660 | 746 | 24.1 | **31.0** | 42 | **46** |
| 3e | inherited class method, open subclass | 939 | **1516** | 31.1 | 28.3 | 36 | 40 |
| 8a | `on: Error do:` without a raise | 65.3 | 64.6 | **5.0** | **3.0** | 0 | 0 |
| 8b | `on: Error do:` with a raise | 2546 | 2462 | 99.2 | 95.2 | 167 | 167 |

Per run (base / main), 3a-3c ns: run 1 21.8, 21.1, 23.7 / 29.6, 20.0, 33.5; run 2 22.3, 21.7, 22.4 / 26.1, 21.4, 49.4; run 3 22.6, 25.8, 28.8 / 26.5, 28.7, 33.5. The 3a-3c reductions are identical (3.0), so the spread is host noise; the dispatch table below, which shows 3c faster on main, says the same.

## 2. dispatch-overhead (net ns/op, median of 3 runs)

| row | operation | base ns | main ns | base red | main red |
|---|---|--:|--:|--:|--:|
| c1 | `class sealed` on a sealed class | 18.9 | 21.3 | 3.0 | 3.0 |
| c2 | class method on a sealed class | 25.2 | 28.1 | 3.0 | 3.0 |
| c3 | class method on an open class | 25.6 | 21.2 | 3.0 | 3.0 |
| c5 | class method, class in a variable | 330 | 376 | 24.0 | **31.1** |
| e1 | `on: Error do:` without a raise | 67.9 | 74.6 | 5.0 | **3.0** |
| e2 | `ensure:` without a raise | 2.2 | 5.7 | 4.0 | 4.0 |

Run 3 is slower on both sides (host load), which is why the medians are used.

## 3. `Result tryDo:` (direct timing of `beamtalk_result:tryDo/1`, same escript as the baseline, 200,000 calls, median of 9 rounds, ns/op)

| run | base bare | base tryDo | main bare | main tryDo |
|---|--:|--:|--:|--:|
| 1 | 9.5 | 25.7 | 9.5 | 19.3 |
| 2 | 8.4 | 23.6 | 9.6 | 19.5 |
| 3 | 6.7 | 18.2 | 8.4 | 19.4 |
| median | 8.4 | 23.6 | 9.5 | 19.4 |

Over the bare `try`, `tryDo/1` costs 15.2 ns on base and 9.9 ns on main: the `protect/1` snapshot and restore was about 4-5 ns of the success path. The raising case (`tryDo` 1278 / 1231 / 1295 base, 1141 / 1238 / 1289 main) did not move beyond noise.

## 4. What the guard read costs now

Persistent-term read, isolated (`bench-dispatch cost`, ns net of the empty loop; four interleaved runs per side): base 20.2, 44.0, 24.3, 22.0; main 28.3, 16.9, 32.9, 16.7. The host's noise is larger than any change in that one mechanism, so the figure to use is the floor: **about 16-17 ns, the same as the BT-4987 figure (15 ns)**. Nothing in ADR 0152 or ADR 0153 changed it, because the read is still there.

`bench-dispatch keys` on main: a static class call (c1, c3) still makes exactly one `persistent_term:get` of `'<Class> static calls'`; a class-side self send (r1) still makes one of `'<Class> class'`. Against a class send of 19-21 ns net (c1), the read is **about 75-85% of the send**, and a send with the read removed would cost about 3-5 ns, in line with Elixir's remote call (3.5-5 ns, rows 3a-3c).

The measured mechanisms that could replace it are all dearer: `atomics:get` 21 ns, `ets:lookup` 27 ns (`bench-dispatch cost`, main).

## 5. Findings the issue did not ask for

**The class-in-a-variable send (3d, 3e, c5) got slower on main.** Reductions on 3d and c5 rose from 24 to 31 and words from 42 to 46; `profile 3d` shows `persistent_term:get/2` 2 → 3 per op and five more calls. Row 3e is +60% in time (939 → 1516 ns) with fewer reductions (31.1 → 28.3). `bench-dispatch keys c5` shows why:

| | reads |
|---|---|
| base | `{beamtalk_class_derived_cache, generation}`, then `'<Class> class'` in `direct_call_ok/2` (the derived flag) |
| main | `{beamtalk_class_derived_cache, generation}`, then `{beamtalk_class_shadow, runtime_fun, '<Class>'}` in `is_shadowed/1` and `beamtalk_class_shadow_ready` in `is_ready/0` |

The dynamic path (`in_caller_owner/3`, BT-5161) now re-derives the answer from two raw flags on every send instead of reading the one derived flag. It is a regression from the guard's new shape, not a noise effect (reductions and the key list are exact). Filed as BT-5267.

**What the removals saved.** The class-variable snapshot before every compiled `on:do:` went from 2 reductions to 0 on 8a and e1 (5 → 3), with no change in time beyond noise (65.3 vs 64.6 ns; 67.9 vs 74.6 ns). `Result tryDo:` over the bare `try`: 15.2 → 9.9 ns. The class-send rows 3a-3c did not change (3.0 reductions, same guard).

## Conclusion

The guard read can go in principle (BT-5164 conclusion (a)): it is 75-85% of a class send, nothing cheaper than `persistent_term` exists to replace it, and no part of ADR 0152 or ADR 0153 removed it. Whether it can go *safely* needs two owner decisions (a runtime fun shadowing a compiled selector; a `ClassSelf` that carries a pid). That is BT-5268, a spike labelled `needs-spec`. The 3d/3e regression is BT-5267.

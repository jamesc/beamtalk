# BT-5267: class-in-a-variable send back to two reads

A class held in a variable (`c double: 21`, vs-elixir 3d and 3e, dispatch c5) made three `persistent_term` reads per send after ADR 0153 Phase 3 (BT-5161), not two. This run compares the fix with the pre-Phase-3 baseline and with `main`.

## Conditions

| | |
|---|---|
| Base | `b464155e` (BT-5128), the baseline of `bench-bt5145-baseline.md` and `bench-bt5164-after.md` |
| Main | `b66c683` (BT-5164), the tip of `main` the branch started from |
| Change | the BT-5267 branch (`016d8ede`) |
| Compiler | `target/debug/beamtalk` (dev build) on every side, runtime from each tree's `runtime/_build/default` |
| Host | 4-core cloud container, OTP 28, Elixir 1.20.1, not a quiet machine |
| Method | Three runs, **interleaved** (base, main, change, then again twice). Each run is `bench-vs-elixir bench 5 3a,3b,3c,3d,3e`, the same with `VS_BENCH_NO_TRACE=1` for 3d and 3e, then `bench-dispatch time 7 c1 c2 c3 c5`. The tables show the median of the three runs. Reductions and words are exact; time is noisy |

```bash
# BASE, MAIN and CHANGE are absolute paths to a checkout of each commit, each built with `just build`
for i in 1 2 3; do for tree in "$BASE" "$MAIN" "$CHANGE"; do
  export BEAMTALK="$tree/target/debug/beamtalk"
  (cd "$tree" && just bench-vs-elixir bench 5 3a,3b,3c,3d,3e
   VS_BENCH_NO_TRACE=1 just bench-vs-elixir bench 5 3d,3e
   just bench-dispatch time 7 c1 c2 c3 c5)
done; done
```

## 1. Reads per send (`bench-dispatch keys c5`)

| | reads |
|---|---|
| base | `{beamtalk_class_derived_cache, generation}`, `'VdOpenMath class'` (`direct_call_ok/2`) |
| main | `{beamtalk_class_derived_cache, generation}`, `{beamtalk_class_shadow, runtime_fun, 'VdOpenMath'}` (`is_shadowed/1`), `beamtalk_class_shadow_ready` (`is_ready/0`) |
| change | `{beamtalk_class_derived_cache, generation}`, `'VdOpenMath class'` (`body_route/2`, the derived flag) |

## 2. vs-elixir (BT net ns, median of 3; reductions and words exact)

| Row | Operation | base ns | main ns | change ns | base red | main red | change red | base words | main words | change words |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| 3a | `class sealed` method, guarded direct call | 22.0 | 19.8 | 19.4 | 3.0 | 3.0 | 3.0 | 0 | 0 | 0 |
| 3b | class method, sealed class | 20.8 | 28.4 | 19.4 | 3.0 | 3.0 | 3.0 | 0 | 0 | 0 |
| 3c | class method, open class | 22.9 | 23.5 | 23.2 | 3.0 | 3.0 | 3.0 | 0 | 0 | 0 |
| 3d | class in a variable | 620 | 711 | **584** | 24.1 | 31.0 | **24.0** | 42 | 46 | **41** |
| 3e | inherited class method, open subclass | 851 | 1429 | **876** | 31.1 | 28.3 | **21.1** | 36 | 40 | **35** |

Per run, 3d ns: base 620, 628, 620; main 658, 711, 749; change 634, 570, 584. 3e ns: base 841, 973, 851; main 1429, 1420, 1613; change 876, 854, 947.

Untraced (`VS_BENCH_NO_TRACE=1`, ns, median of 3):

| Row | base | main | change |
|---|--:|--:|--:|
| 3d | 320 | 361 | **335** |
| 3e | 477 | 388 | **356** |

Per run, untraced 3d: base 321, 310, 320; main 350, 366, 361; change 325, 335, 341. Untraced 3e: base 477, 470, 515; main 388, 385, 424; change 365, 346, 356.

## 3. dispatch-overhead (net ns/op, median of 3)

| row | operation | base ns | main ns | change ns | base red | main red | change red |
|---|---|--:|--:|--:|--:|--:|--:|
| c1 | `class sealed` on a sealed class | 18.6 | 20.4 | 18.8 | 3.0 | 3.0 | 3.0 |
| c2 | class method on a sealed class | 23.9 | 25.6 | 24.2 | 3.0 | 3.0 | 3.0 |
| c3 | class method on an open class | 25.6 | 24.2 | 22.0 | 3.0 | 3.0 | 3.0 |
| c5 | class method, class in a variable | 307.3 | 330.5 | 318.1 | 24.0 | 31.1 | **24.0** |

Per run, c5: base 303.9, 307.3, 308.0; main 316.6, 330.5, 341.8; change 332.8, 318.1, 311.3.

## 4. Why 3e jumped

The vs-elixir harness traces garbage collection on every process while it times a row (`trace_flags/0` in `harness/vs_bench.erl`), so each minor GC of the worker sends a trace message. On these rows that tracing is most of the measured time. Untraced, 3e on main is 388 ns (faster than base's 477); traced, it is 1429. The traced time moves with how much each op allocates, and not linearly. Main added 4 words per op on 3d and 3e: the `{beamtalk_class_shadow, runtime_fun, Class}` key tuple that `is_shadowed/1` builds on every send. With that, 3e's traced time went from 851 to 1429 ns while its untraced time fell. The same tracing effect inflates every op on this path in a micro-probe (a GC-traced worker runs `class_send` about 3x slower than an untraced one).

The fix removes both raw-flag reads and the tuple. It also gives the owner memo a pid-first key, `{ClassPid, Selector}`, which saves two more words per send (the key is built per send and copied out of ETS). The defining class's tag goes into the memo so the derived flag can be read in one atom-keyed lookup. Net result: 3d 41 words, 3e 35, against base's 42 and 36. Traced 3e is back to 876 ns (base 851); untraced it is the fastest of the three (356 ns).

## Conclusion

- Two reads per send again, as on the base: the memo's generation and the defining class's derived flag.
- 3d and c5 are back at 24 reductions; 3d is at 41 words (base 42).
- 3e: the cause was allocation under the harness's GC tracing, not the send's own cost; it is back near the base (876 vs 851 ns traced) and faster untraced.
- 3a-3c and c1-c3 are untouched (same one guarded read); their time differences are noise.

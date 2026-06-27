# Benchmarks

Performance measurements for Beamtalk, recorded as features land. Each entry
notes the harness, the workspace shape, and the before/after numbers so claims
in ADRs and issues can be checked against real data.

## SystemNavigation `sendersOf:` — xref index migration (ADR 0087 Phase 3, BT-2299)

ADR 0087 introduces a runtime-maintained cross-reference index
(`beamtalk_xref`) so navigation queries read from ETS instead of re-parsing
every method source on each call. Phase 3 migrated `SystemNavigation default
sendersOf:` to the index, with a source-scan fallback for loaded-but-unindexed
classes. The ADR commits to "sub-millisecond ETS read" vs "a few seconds on a
200-class workspace"; this benchmark confirms the order-of-magnitude win.

### Harness

`runtime/perf/bench_senders_xref.escript`. Run from the `runtime/` directory
after `just build`:

```bash
escript perf/bench_senders_xref.escript
```

It starts the full runtime + compiler, force-loads the compiled stdlib modules
(so their `on_load` hooks register the full 81-class workspace), warms both
paths once, then times:

- **before** — the legacy path: walk every loaded class, fetch each method's
  source, and call `beamtalk_compiler:find_senders_in_source/2` to find matching
  sends. Averaged over 5 iterations (each is expensive).
- **after** — the migrated path: a single `beamtalk_xref:senders_of_bt/1` ETS
  read (plus the miss-policy partition). Averaged over 1000 iterations.

It also isolates the **loaded-class-set** computation the miss-policy partition
depends on (BT-2384): the old `beamtalk_class_registry:live_class_entries/0`
registry walk (one `gen_server:call` per loaded class) vs the new
`loaded_class_entries/0` single ETS scan.

### Workspace

The loaded standard library: **81 classes, >1000 methods** (the issue calls for
stdlib + ≥10 classes / ≥1000 methods; the stdlib alone satisfies this).

### Results

Query: `SystemNavigation default sendersOf: #asString`

| Path | Iterations | ms/op | Hits |
|---|---|---|---|
| before (source-scan) | 5 | ~193 ms | 31 |
| after (xref ETS read), pre-BT-2384 | 1000 | ~1.0 ms | 27 |
| after (xref ETS read), post-BT-2384 | 1000 | ~0.42 ms | 27 |

**Speedup vs source-scan: ~460x** — comfortably past the 100x target, and now
**sub-millisecond** end-to-end after BT-2384.

Loaded-class set (the miss-partition input), 81-class workspace:

| Path | ms/op |
|---|---|
| registry walk (`live_class_entries/0`, O(classes) `gen_server:call`s) | ~0.40 ms |
| ETS read (`loaded_class_entries/0`, single scan + `is_process_alive/1`) | ~0.012 ms |

**~35x reduction** on the loaded-set step — and, more importantly, it no longer
scales with the workspace size, so the ~800-class workspace no longer
re-introduces the per-call cost the index was meant to remove.

### Notes

- The "after" hit count (27) is lower than "before" (31) in this raw read
  because a few stdlib classes are loaded but not yet baked into the index (a
  Phase 2 codegen gap — see below). In the real `sendersOf:`, the miss-policy
  fallback source-scans exactly those classes and restores the missing hits, so
  the migrated query returns the same set as the legacy one (verified
  byte-for-byte by the REPL-protocol E2E suite). The fallback adds cost only for
  the unindexed minority, not the whole workspace.
- **BT-2384:** the pre-BT-2384 ~1 ms "after" figure was dominated by the
  `beamtalk_class_registry:live_class_entries/0` walk (one `gen_server:call` per
  loaded class) that the miss policy needs to partition
  loaded-vs-stale-vs-missing — not by the ETS lookup itself, which is
  sub-microsecond. BT-2384 replaced that walk with a fast loaded-class ETS index
  (`beamtalk_loaded_classes`, maintained by the class lifecycle in
  `beamtalk_object_class:init/1` + `terminate/1`), so `miss_partition/1` now
  reads the loaded-class set in pure ETS with no per-class messaging. The
  remaining ~0.42 ms is dominated by the fallback source-scan of the handful of
  unindexed classes below, not the loaded-set computation.
- **Follow-up:** a handful of method-bearing stdlib classes (`Printable`,
  `TranscriptStream`, `Subprocess`) are loaded but absent from the index,
  meaning every navigation query currently source-scans them via the fallback
  and logs an `xref_miss` warning. They should be baked into `register_class/0`
  like the rest; tracked as a Phase 2 completeness gap.

## Self-hosting cost: pure-BT enumeration vs native primitives (BT-2692 / BT-2708)

A spike for the de-primitivization direction: how much slower is a pure-BT
collection method (built on `do:`/`inject:into:` and dispatched per element)
than the native `@primitive`? This determines whether the per-class enumeration
overrides (`List`/`Array` re-implementing `Collection`'s `collect:`/`select:`/…)
are worth removing, and what self-hosting the aggregates (BT-2711) would cost.

### Harness

`runtime/perf/bench_collect_selfhost.escript`. Run from `runtime/` after
`just build`:

```bash
escript perf/bench_collect_selfhost.escript
```

Compares, on a `List` receiver (outputs asserted identical):
- `collect:`/`select:` — native `bt@stdlib@list` `@primitive` vs the pure-BT
  `bt@stdlib@collection` versions (`do:`/`inject:`-based).
- `sum` — native `beamtalk_collection:sum/1` (`lists:foldl`) vs the
  `inject:into:` + `+` block a self-hosted `sum` would compile to.

### Results (N = 100 000)

| Op | native µs/op | pure-BT µs/op | ratio |
|----|---|---|---|
| `collect:` | ~1100 | ~10800 | **~10×** |
| `select:`  | ~670  | ~12200 | **~18×** |
| `sum`      | ~363  | ~870   | **~2.4×** |

Ratios hold at N = 1000 (collect ~13×, select ~16×, sum ~2.5×).

### Interpretation

The cost is **per-element BT-level dispatch**, and it splits by operation shape:

- **List-building** (`collect:`/`select:`/`reject:`/`flatMap:`): 10–18×. Each
  element pays a dispatched `addFirst:` to build the result cons, plus a
  `reversed` second pass and `species withAll:`, vs native's single tight
  `lists:map`/`filter`. **Conclusion: keep these `@primitive`** — self-hosting
  them is a major regression.
- **Reducing** (`sum`/`max`/`min`/`count:`): ~2.4×. Single fold, no list
  building; the overhead is double-fun-indirection (`inject:into:` →
  `beamtalk_collection:inject_into`'s arg-swap wrapper → the BT block → `+`),
  i.e. two fun calls per element vs native's one. The arithmetic itself is free.

### Takeaways

- Per-element collection primitives are **earned** — de-primitivization should
  not target hot per-element paths.
- Self-hosting the aggregates (BT-2711) costs ~2.4× on `sum`; treat it as
  optional, not a free win.
- The 2.4× is mostly `inject:into:`'s wrapper. A leaner `List>>inject:into:`
  (inline `lists:foldl`, no `beamtalk_collection` indirection) would narrow
  *every* inject-based pure-BT fold at once — higher ROI than self-hosting
  individual aggregates.

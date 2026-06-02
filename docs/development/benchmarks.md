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

It starts the full runtime + compiler, lets the stdlib finish loading, warms
both paths once, then times:

- **before** — the legacy path: walk every loaded class, fetch each method's
  source, and call `beamtalk_compiler:find_senders_in_source/2` to find matching
  sends. Averaged over 5 iterations (each is expensive).
- **after** — the migrated path: a single `beamtalk_xref:senders_of_bt/1` ETS
  read. Averaged over 1000 iterations.

### Workspace

The loaded standard library: **81 classes, >1000 methods** (the issue calls for
stdlib + ≥10 classes / ≥1000 methods; the stdlib alone satisfies this).

### Results

Query: `SystemNavigation default sendersOf: #asString`

| Path | Iterations | ms/op | Hits |
|---|---|---|---|
| before (source-scan) | 5 | ~146 ms | 29 |
| after (xref ETS read) | 1000 | ~1.0 ms | 25 |

**Speedup: ~140x** — comfortably past the 100x target.

### Notes

- The "after" hit count (25) is lower than "before" (29) in this raw read
  because 10 stdlib classes are loaded but not yet baked into the index (a
  Phase 2 codegen gap — see below). In the real `sendersOf:`, the miss-policy
  fallback source-scans exactly those classes and restores the missing hits, so
  the migrated query returns the same set as the legacy one (verified
  byte-for-byte by the REPL-protocol E2E suite). The fallback adds cost only for
  the unindexed minority, not the whole workspace.
- The ~1 ms "after" figure is dominated by the `beamtalk_class_registry:
  live_class_entries/0` walk (one `gen_server:call` per loaded class) that the
  miss policy needs to partition loaded-vs-stale-vs-missing — not by the ETS
  lookup itself, which is sub-microsecond. A future optimisation could cache the
  loaded-class set or push the partition into ETS.
- **Follow-up:** a handful of method-bearing stdlib classes (`Printable`,
  `TranscriptStream`, `Subprocess`) are loaded but absent from the index,
  meaning every navigation query currently source-scans them via the fallback
  and logs an `xref_miss` warning. They should be baked into `register_class/0`
  like the rest; tracked as a Phase 2 completeness gap.

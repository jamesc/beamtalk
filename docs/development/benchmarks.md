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

## Reload re-check fan-out — xref receiver-type-key decision (ADR 0105 Phase 2, BT-2781)

ADR 0105's re-check orchestration (BT-2778) looks up a changed selector's
callers via `beamtalk_xref:senders_of/1`, which is selector-keyed with no
receiver-class component (ADR 0087's schema). The receiver-type filter that
separates real dependents from same-selector-different-receiver false
positives is not a pre-filter — it is an emergent property of re-checking
each candidate through the compiler (`beamtalk_recheck`'s moduledoc). For a
common selector this means paying a compile per candidate just to discover
it isn't a real dependent, which is why the per-reload numeric caller cap
(`recheck_caller_cap`, default 20) exists. This benchmark measures that
fan-out to decide whether ADR 0087 needs a receiver-type key (ADR 0105
Alternatives) to make the lookup precise instead of selector-wide.

### Harness

`runtime/perf/bench_recheck_fanout.escript`. Run from the `runtime/`
directory after `just build`:

```bash
escript perf/bench_recheck_fanout.escript
```

Two parts:

- **Part A (large-image survey)** — boots the full loaded stdlib workspace
  (mirrors `bench_senders_xref.escript`) and ranks every indexed sent
  selector by its distinct-caller-class count (the unit `beamtalk_recheck`
  actually caps and re-checks — `group_by_owner/1` collapses multiple sites
  in one caller class to a single candidate).
- **Part B (controlled fan-out)** — drives the real
  `beamtalk_recheck:trigger/4` orchestration (real compiler port, real xref,
  real `beamtalk_workspace_meta`) over synthetic candidate sets of 5 / 20 /
  50 / 200, with a fixed 10% real-dependent / 90% false-positive split
  (modelling a heavily overloaded common selector), at both the default cap
  (20) and uncapped. Real and false-positive candidates are interleaved by
  index so alphabetic sort order — what `apply_cap/2` actually keeps under a
  cap — doesn't systematically favour or penalise either group.

### Workspace

The loaded standard library: **103 classes** (grown from 81 at BT-2299's
measurement), **1928 indexed send sites**, **416 distinct sent selectors**.

### Results

**Part A — today's real fan-out.** 325 of 416 selectors (78%) have exactly
one distinct caller class. Ranked by **distinct caller-class count** (not
raw site count — a selector sent many times by few classes is cheaper to
re-check than one sent once each by many classes, since `beamtalk_recheck`
caps and re-checks one candidate *per caller class*, not per site):

| selector | sites | distinct caller classes |
|---|---|---|
| `delegate` | 193 | 23 |
| `ifTrue:ifFalse:` | 114 | 17 |
| `asString` | 35 | 17 |
| `error:` | 39 | 13 |
| `++` | 34 | 13 |

The script also computes this exhaustively rather than by eyeballing the
top-10 (an earlier draft ranked the "top 10" table by site count while
claiming distinct-owner order — caught in review, since the two orderings
disagree): **exactly 1 of 416 selectors** (`delegate`) has a distinct-owner
count exceeding the default cap of 20, and only by 3.

**Part B — controlled fan-out, default cap (20):**

| candidates | checked | dropped | real findings | wall-clock | ms/checked |
|---|---|---|---|---|---|
| 5 | 5 | 0 | 1 of 1 | ~240 ms | ~48 ms |
| 20 | 20 | 0 | 2 of 2 | ~730 ms | ~37 ms |
| 50 | 20 | 30 | 2 of 5 | ~750 ms | ~38 ms |
| 200 | 20 | 180 | 2 of 20 | ~760 ms | ~38 ms |

**Part B — uncapped (full candidate set checked):**

| candidates | checked | real findings | wall-clock | ms/checked |
|---|---|---|---|---|
| 5 | 5 | 1 | ~240 ms | ~47 ms |
| 20 | 20 | 2 | ~790 ms | ~39 ms |
| 50 | 50 | 5 | ~1980 ms | ~40 ms |
| 200 | 200 | 20 | ~7580 ms | ~38 ms |

### Interpretation

- **Per-check cost is flat, ~33-48 ms/candidate**, regardless of fan-out size
  — consistent with the Phase 0 spike's ~18.5 ms warm / ~58 ms cold figures
  (these synthetic candidates are fresh classes, so nearer the cold end).
  There is no quadratic or superlinear blowup as candidate count grows; cost
  scales linearly with candidates checked. **Caveat:** every synthetic
  candidate is a trivial one-method class, so this is a cost *floor* — real
  caller classes (multi-method, larger source) will compile slower per
  candidate. The floor is enough to establish "no superlinear blowup," but
  not to bound absolute worst-case per-check latency in a large real
  codebase.
- **The cap does bound worst-case latency as designed** for this class-size
  floor: wall-clock plateaus at ~700-760 ms once fan-out reaches/exceeds the
  cap, regardless of whether the true candidate pool is 20 or 200. In the
  *healthy* (non-degraded) case this stays comfortably interactive; with
  larger real caller classes the plateau would sit higher (see caveat
  above), but the *shape* — bounded regardless of true fan-out size — holds.
- **The cap silently drops most real findings once fan-out exceeds it by a
  wide margin.** At 50 candidates (2.5x the cap), only 2 of 5 real findings
  (40%) survive; at 200 candidates (10x the cap), only 2 of 20 (10%) survive
  — a **90% loss of genuine stale-caller findings**, because
  `apply_cap/2` keeps the alphabetically-first N candidates without any
  relevance ranking (its own documented limitation). This directly
  undercuts the ADR's headline promise ("only genuinely-affected callers
  surface") once a selector's real fan-out grows well past the cap.
  **Caveat:** the fixture interleaves real/false candidates uniformly by
  index, so this ~proportional loss is an *expected-value* reading; real
  class-name distributions could cluster real dependents earlier or later
  in alphabetic order, giving anywhere from ~0% to 100% loss for the same
  candidate-pool shape — the fixture demonstrates the failure mode exists
  and is severe on average, not a worst-case bound.
- **This is not purely a future risk — it is already happening today, at
  low grade.** `delegate` (23 distinct owners) already exceeds the cap of 20
  in the live stdlib image, so a reload of a class implementing `delegate`
  already silently drops re-checks for up to 3 alphabetically-last callers
  today. It has not been a *visible* problem because `delegate` is an
  internal proxy-forwarding selector (ADR 0104 territory), not one typical
  user code sends directly — but the mechanism is live now, not hypothetical.
- **Today's real stdlib fan-out (Part A) does not yet justify the xref
  receiver-type-key extension** as an urgent fix: only one real selector
  exceeds the cap, and only by 3. But the *shape* of the risk is confirmed
  and quantified by Part B: as the image grows (more classes implementing a
  common protocol selector like `size`/`at:`), the cap's failure mode is not
  a graceful latency degradation — it is a silent, severe drop in finding
  completeness.
- **Synchronous tail-latency (flagged on BT-2778's issue thread) — not
  simulated here.** A wedged/degraded compiler port was not reproduced
  (would require mocking the port's timeout path); the theoretical worst
  case remains `Cap × 30s` (the `beamtalk_compiler_server` call timeout)
  under a fully serialized, wedged port, per the BT-2778 comment. The
  *healthy*-case numbers above (cap=20 always finishes in under 1s) confirm
  this is a tail-only risk, not a typical-case one.
- **Methodology notes:** single-run timings, no explicit warmup — the N=5
  round (first to run) is consistently the slowest per-check (~45-48 ms vs
  ~33-38 ms for later rounds), most plausibly cold-start compiler-port /
  ambient-class-hierarchy effects rather than a true N=5 cost difference.
  Repeated full runs (3x) show the qualitative conclusions (flat per-check
  cost, cap-bounded latency, severe finding-loss past 2.5x the cap) hold
  consistently; the specific ms figures above should be read as
  representative, not as tight bounds. The compiler-server's ambient class
  cache is never cleared between the 8 rounds in one script run, so later
  rounds run against a larger accumulated hierarchy — a possible confound
  for absolute (not relative) timings that a from-scratch-per-round harness
  would eliminate.

### Decision (ADR 0105 Phase 2, BT-2781)

**The xref receiver-type-key extension is not implemented now, but is
warranted as a proactive (non-urgent) follow-up.** Rationale:

1. Current real-world fan-out (Part A) is within the existing cap for
   415 of 416 measured stdlib selectors; the one exception (`delegate`, an
   internal proxy-forwarding selector) exceeds it by only 3 — a small,
   already-occurring, low-visibility gap, not an urgent correctness or
   latency problem.
2. The controlled benchmark (Part B) proves the interim design's known
   limitation (cap keeps an arbitrary, non-relevance-ranked subset) is not
   theoretical: it causes a **quantified 90% loss of real findings** once a
   selector's fan-out reaches 10x the cap — a plausible future state as the
   image grows and common protocol selectors (`size`, `at:`, `do:`) accrue
   more implementors and senders.
3. The receiver-type key would fix *both* problems the cap only partially
   addresses today — it shrinks the candidate set to true dependents before
   the cap is even applied, so it is a completeness win (no more silently
   dropped real findings) as well as a latency win (fewer wasted compiles).
4. Given ADR 0105's own phased approach and that this changes ADR 0087's
   shipped schema/generation lifecycle (non-trivial migration, per ADR 0105
   Alternatives), the recommendation is to track this as a follow-up rather
   than block current phases — see BT-2798 for scope.

No regression: `bench_senders_xref.escript` (ADR 0087 Phase 3, BT-2299)
re-run alongside this benchmark, unaffected (~1400x speedup vs source-scan,
consistent with prior measurement; workspace grew from 81 to 103 classes in
the interim, expected).

### Update — `senders_of/2` implemented and measured (ADR 0115 Phase 5, BT-3220)

The receiver-type-key extension the Decision above recorded as a warranted,
non-urgent follow-up is now shipped: `recv_type` on the xref schema (Phase 2,
BT-3217), `beamtalk_xref:senders_of/2` (Phase 3, BT-3218), and
`beamtalk_recheck` wired to call it (Phase 4, BT-3219). This phase (BT-3220)
extended both harnesses to measure the shipped mechanism directly, against
this section's own methodology and numbers, rather than re-deciding whether
to build it.

**`bench_senders_xref.escript`** (workspace regrown to **109 classes, 2184
indexed send sites, 478 distinct sent selectors** at measurement time) adds a
`senders_of/1` vs `senders_of/2` comparison over the same `#asString`
selector the source-scan-vs-xref comparison above already uses, keyed on a
real loaded class (`Printable`):

| query | sites returned | ms/op (1000 iters) |
|---|---|---|
| `senders_of/1` | 37 | 0.029 |
| `senders_of/2` (`ChangedClass = 'Printable'`) | 6 | 0.050 |

`senders_of/2` costs **~1.7x** `senders_of/1` here — the added
`hierarchy_related_classes/1` walk (ancestor-chain + `direct_subclasses/1`
closure) — while narrowing 37 candidate sites down to 6 relevant ones for
this selector/class pair. Critically, **`senders_of/1` itself is
unaffected**: measured again immediately after `senders_of/2` runs against
the identical selector, its own cost is unchanged (0.029 ms/op before,
0.030 ms/op after — noise-level difference, not a regression), confirming
acceptance criterion 1 (`recv_type` is additive; `senders_of/1`'s code path
is untouched).

**`bench_recheck_fanout.escript`** adds **Part C**, the identical 10%-real/
90%-false-positive synthetic shape as Part B above, but every candidate
site now carries `recv_type` (mirroring what the real compile-time write
path populates for actual compiled code, instead of Part B's deliberately
untyped/legacy rows) — this is BT-2781's synthetic scenario, now run
through the shipped `senders_of/2` filter instead of `senders_of/1`:

**Part C — typed fan-out, default cap (20):**

| candidates | checked | dropped | real findings | wall-clock | ms/checked | total candidates (post-filter) |
|---|---|---|---|---|---|---|
| 5 | 1 | 0 | 1 of 1 | ~25 ms | ~25 ms | 1 |
| 20 | 2 | 0 | 2 of 2 | ~53 ms | ~26 ms | 2 |
| 50 | 5 | 0 | 5 of 5 | ~130 ms | ~26 ms | 5 |
| 200 | 20 | 0 | 20 of 20 | ~520 ms | ~26 ms | 20 |
| 400 | 20 | 20 | 20 of 40 | ~590 ms | ~29 ms | 40 |

Compare directly against Part B's identical-shape, default-cap row at the
same fan-out sizes:

| fan-out | Part B (untyped, pre-existing) findings | Part C (typed, BT-3220) findings | loss eliminated |
|---|---|---|---|
| 50 | 2 of 5 (60% loss) | 5 of 5 (0% loss) | yes |
| 200 | 2 of 20 (**90% loss**) | 20 of 20 (**0% loss**) | yes |

At 200 candidates (10x the cap) — the exact scenario this section's Decision
quantified as a 90% loss — the false-positive share is now excluded from
`total_candidates` by `senders_of/2` itself, **before** `apply_cap/2` ever
runs: `total_candidates` reads 20 (the true real-dependent count), not 200,
so every real dependent is checked and every one is found stale. This
confirms acceptance criterion 2: the fix is a pre-cap filter, not a
re-check-and-discard step — Part B's own numbers (unchanged, still showing
the 90% loss for untyped/legacy candidates) are the direct control for this
comparison, and remain accurate documentation of the residual gap ADR 0115
Constraint 2 leaves for `dynamic`-typed and legacy rows.

The 400-candidate row additionally confirms the cap remains a real backstop
for typed code too, once the *narrowed* pool itself exceeds it: 40 real
candidates (10% of 400) survive `senders_of/2`'s filter, and the cap (20)
then drops half of *those* — a materially different, much smaller-scale
problem than dropping 180 of 200 raw candidates the untyped path faces at
the same fan-out.

**E2E coverage:** `tests/repl-protocol/cases/adr_0115_recv_type_recheck_fanout.btscript`
exercises the identical false-positive/real-dependent shape end to end from
the REPL, via `Behaviour>>precheckCompile:source:` against real compiled
`.bt` classes (not synthetic xref fixtures, unlike the escripts above) —
proving `senders_of/2`'s filtering is reachable from the actual REPL
surface, not just correct at the EUnit level (`docs/development/
testing-strategy.md`'s "REPL command testing — use E2E" guidance).

## Leaf-change re-check fan-out (ADR 0107 Phase A, BT-2856 follow-up, BT-2873)

BT-2856 added `beamtalk_recheck:trigger_leaf_change/1`: when a live
class-body reload gives a previously-leaf class its first subclass, this
re-checks **every** live class's own recorded source against the updated
hierarchy (the same "recompile everything" strategy as `trigger_image/0`,
since no xref index exists yet for `Type`-pattern/`matchExhaustive:` sites —
see the moduledoc). Unlike `trigger_image/0`, this fires *automatically*,
once per distinct base class gaining its first subclass. BT-2873's
adversarial-review follow-up asks whether that automatic fan-out is a real
cost during a bulk `:load dir` of a project with a deep class hierarchy
(finding #1), and whether a single reload introducing multiple new
hierarchies at once pays for redundant independent sweeps (finding #2),
following BT-2781's "measure, then decide" precedent.

### Harness

`runtime/perf/bench_leaf_change_fanout.escript`. Run from the `runtime/`
directory after `just build`:

```bash
escript perf/bench_leaf_change_fanout.escript
```

Two parts:

- **Part A (real hierarchy shape)** — walks the full loaded stdlib class
  hierarchy and counts classes with >= 1 direct subclass: each one underwent
  exactly one leaf -> non-leaf transition the first time its first subclass
  loaded, so this count is the exact number of `trigger_leaf_change/1`
  sweeps a from-scratch bulk load of this workspace already fires today.
  Also checks finding #2 directly: does any stdlib source file declare more
  than one class (real declarations only — `///` doc-comment examples like
  `actor.bt`'s `/// Actor subclass: Counter` are filtered out, since they
  are not real class definitions)?
- **Part B (sweep cost vs. live-class count)** — drives the real
  `beamtalk_recheck:trigger_leaf_change/1` orchestration (real compiler
  port, real `beamtalk_workspace_meta`) over a synthetic set of 5 / 20 / 50 /
  100 / 200 live class sources, to measure the sweep's wall-clock cost as a
  function of workspace size.

### Workspace

The loaded standard library: **104 classes** (1 more than BT-2781's 103 —
stdlib grows over time).

### Results

**Part A — today's real hierarchy shape:**

- **17 of 104 classes (16.3%)** have at least one direct subclass — i.e. a
  from-scratch bulk load of the full stdlib fires **17** independent
  `trigger_leaf_change/1` sweeps today.
- **0 of 104 source files declare more than one class.** Every stdlib file
  follows a strict one-class-per-file convention, so finding #2's scenario
  (a single reload making two or more superclasses newly-non-leaf at once,
  paying for N independent sweeps instead of one) **cannot occur via the
  stdlib's own bulk-load path** — `superclasses_losing_leaf_status/1`'s
  input list is a singleton on every real stdlib reload. It remains
  reachable in principle via other call sites that can install multiple
  classes from one compile unit (e.g. a REPL paste defining two `subclass:`
  statements in one expression, feeding `load_compiled_module/6`), just not
  demonstrated as occurring anywhere in the current codebase.

**Part B — sweep cost vs. live-class count:**

| live classes | checked | wall-clock | ms/checked |
|---|---|---|---|
| 5 | 5 | ~215 ms | ~43 ms |
| 20 | 20 | ~684 ms | ~34 ms |
| 50 | 50 | ~1740 ms | ~35 ms |
| 100 | 100 | ~3620 ms | ~36 ms |
| 200 | 200 | ~7440 ms | ~37 ms |

### Interpretation

- **Per-check cost is flat, ~34-43 ms/candidate**, consistent with
  `trigger/4`'s own measured ~33-48 ms/candidate (BT-2781) — no
  superlinear blowup; `trigger_leaf_change/1` costs scale linearly with the
  live-class count, same shape as `trigger_image/0`.
- **Projected worst-case cost of today's real stdlib bulk load:** 17 sweeps
  at up to ~3.6 s each (the full 104-class workspace size, an upper bound
  since earlier transitions in the load see a smaller live-class set) is up
  to **~61 s of serialized background compiler-port work**; a more
  realistic estimate (transitions roughly spread through the load, so the
  average sweep sees about half the final live-class count) is **~25-30 s**.
  This is not a *blocking* cost — `spawn_leaf_change_recheck/1` already
  keeps every sweep off the reload's own response path, queued through
  `beamtalk_workspace_shape_recheck_worker`'s single-worker serialisation
  (bounding concurrent compiler-port contention from this path to 1, same as
  every other ADR 0105 mechanism) — but it is a real, non-trivial tail of
  background compiler-port contention that other clients' hover/completion/
  save requests queue up behind for tens of seconds after a full bulk load
  "finishes" from the loading client's point of view, and it grows linearly
  with both hierarchy depth (more leaf-change events) and workspace size
  (more expensive per sweep).
- **Finding #2 has zero measured benefit today** (0 of 104 files could ever
  trigger it) but remains cheap, mechanical, and strictly non-regressive to
  fix given the code path already exists for other reload sites capable of
  installing multiple classes at once.

### Decision (BT-2873)

**Cross-reload debouncing/batching for finding #1 (collapsing the 17
separate bulk-load sweeps above into one) is not implemented now.** The
measured cost (tens of seconds of serialized, off-response-path background
work for a full from-scratch stdlib load) is real but bounded, self-healing
(resolves once the bulk load's queued sweeps drain), and — unlike BT-2798's
xref receiver-type-key finding — does not cause any *lost* findings or
incorrect behaviour, only a temporary latency/contention tail. It also only
manifests on a full/deep bulk load (`:load dir` of a whole project or a
large chunk of it), not on the ordinary single-class-at-a-time edit loop
interactive development mostly consists of. Implementing genuine
cross-reload debouncing (coalescing multiple queued `{leaf_change, _}`
messages arriving within a short window into one combined sweep) would
require the worker to peek ahead in its own mailbox or add a delay/timer
state machine — meaningfully more complexity than this proactively-filed,
non-urgent finding currently justifies, mirroring BT-2798's own "quantify,
document, defer" resolution for a similarly-shaped bounded cost. Revisit if
real-world bulk loads of deep hierarchies are reported as actually
regressing interactive responsiveness.

**Finding #2's same-reload batching is implemented** (`beamtalk_recheck:
trigger_leaf_change/1` now takes the full list of newly-non-leaf
superclasses from one reload event and runs a single sweep attributing
findings to whichever of them a diagnostic names, instead of
`maybe_trigger_leaf_change_recheck/1`'s previous one-sweep-per-superclass
`lists:foreach`) — free today given the stdlib's one-class-per-file
convention (0 measured benefit), but a correct, low-risk fix for any other
reload path capable of installing multiple classes at once.

No regression: `bench_recheck_fanout.escript` (BT-2781) and
`bench_senders_xref.escript` (BT-2299) unaffected by this change (neither
touches `trigger_leaf_change/1` or its call sites).

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
| `sum`      | ~363  | ~870   | **~2.4×** (was; see BT-2713 below) |

Ratios hold at N = 1000 (collect ~13×, select ~16×, sum ~2.5×).

### Interpretation

The cost is **per-element BT-level dispatch**, and it splits by operation shape:

- **List-building** (`collect:`/`select:`/`reject:`/`flatMap:`): 10–18×. Each
  element pays a dispatched `addFirst:` to build the result cons, plus a
  `reversed` second pass and `species withAll:`, vs native's single tight
  `lists:map`/`filter`. **Conclusion: keep these `@primitive`** — self-hosting
  them is a major regression.
- **Reducing** (`sum`/`max`/`min`/`count:`): ~2.4× originally. Single fold, no
  list building, so the gap is pure machinery, not arithmetic. The original
  reading blamed "double-fun-indirection" (the arg-swap wrapper around the
  block). That was **wrong** — see BT-2713.

### BT-2713: leaner `inject:into:` — the real cause was the loop's register layout

`beamtalk_collection:inject_into/3` used to wrap the block in a `lists:foldl`
arg-swapper (`fun(Elem, Acc) -> Block(Acc, Elem) end`). Removing that wrapper in
favour of a hand-rolled fold turned out to make **no difference** (~0%): on a
controlled best-of-N micro-benchmark, `lists:foldl`+wrapper and a hand-rolled
fold that calls `Block(Acc, Elem)` directly both clocked ~900 µs/op. The extra
fun call was not the bottleneck.

The real lever (OTP 28 BEAM JIT) is **which argument carries the accumulator in
the recursive loop function**:

| fold loop shape (block still called `Block(Acc, Elem)`) | µs/op | vs native |
|---|---|---|
| `lists:foldl` + arg-swap wrapper (old) | ~940 | ~1.8× |
| hand-rolled, accumulator in the *middle* arg `(List, Acc, Block)` | ~900 | ~1.8× |
| hand-rolled, accumulator as the *last* arg `(List, Block, Acc)` | ~590 | **~1.1×** |
| native `lists:foldl` baseline (`Fun(Elem, Acc)`) | ~510 | 1.0× |

Threading the accumulator as the loop's **last** argument (list first, block in
the middle) lets the JIT keep it in a register the fun-call ABI doesn't disturb,
recovering ~1.6× and landing within ~1.1–1.3× of native — *without* touching the
block call (BT blocks are unavoidably accumulator-first, `Block(Acc, Elem)`).
`inject_into/3` now uses that shape, so every inject-based pure-BT fold is leaner
at once. Re-measured `sum`-via-`inject:` ratio: **~1.1–1.3×** (N = 100 000),
down from ~1.8–2.4×.

### Takeaways

- Per-element collection primitives are **earned** — de-primitivization should
  not target hot per-element paths.
- Reducing/fold-based aggregates are now within ~1.1–1.3× of native after
  BT-2713; self-hosting them (BT-2711) is no longer a ~2.4× tax.
- On the BEAM JIT, a tail-recursive fold's **accumulator-argument position**
  matters more than fun-call count. Accumulator-last is the fast shape; prefer
  it for hot hand-rolled folds.

### Arithmetic operator guard vs bare BIF (BT-2709)

Phase 1 makes `+ - * /` dispatchable messages. A statically-numeric receiver
(numeric literal, `self` in `Integer`/`Float`, a `:: Integer/Float/Number`
param, or a `self.<field>` read) keeps the **bare** `erlang:'+'`; any other
receiver emits a runtime `is_number` guard that picks the BIF for numbers and
`beamtalk_message_dispatch:send/3` for objects. Two cases measure it: `bench_guard/0`
(a tight add-loop — the worst case) and `bench_guard_fold/0` (a `lists:foldl`
accumulator — the realistic shape `sum`/`inject:into:` compile to).

| Case | overhead | ratio |
|------|---|---|
| `bench_guard` — tight add-loop (N = 5M adds) | ~5.5 ns/add | **~2.7–3.0×** (run-dependent) |
| `bench_guard_fold` — foldl accumulator (N = 1M elems) | **~0.5 ns/elem** | **~1.09×** |

#### Interpretation

- The ~3× tight-loop ratio is the **worst case**: in a loop that does nothing but
  add, the guard is a large fraction of the work. What matters in real code is the
  **absolute** ~5 ns/add — and once it's one step in a larger expression the ratio
  collapses: the realistic fold A/B measures only **~1.09× (~0.5 ns/elem)**.
- It applies **only to the guarded path**. The bare fast path — all hot stdlib
  arithmetic and ~95% of user code — is byte-for-byte unchanged and pays nothing
  (asserted by codegen regression tests in `tests/expressions.rs`).
- The guarded path includes synthetic fold accumulators (e.g. the `I + 1` index
  increment compiled for `do:`/`eachWithIndex:`), since codegen can't statically
  prove the accumulator is numeric. **Measured, this is negligible** (~1.09× on a
  bare foldl, and pure-BT collection iteration is already dominated 10–18× by
  per-element *dispatch*, not the add). So returning fold accumulators to the bare
  path (by teaching `receiver_is_statically_numeric` about numeric-seeded
  accumulators) is **not worth pursuing** — recorded here so the decision isn't
  re-litigated.

### Number-on-the-left arithmetic coercion — try/catch vs bare BIF (BT-3265, ADR 0116)

ADR 0116 adds double-dispatch coercion so `5 + aVector`-shaped expressions
(numeric operand on the left, non-numeric operand on the right — a case the
guard above has no answer for) dispatch to a reflected `plusFromNumber:`-style
method instead of crashing with a raw `badarith`. The compile-time skip
(`receiver_is_statically_numeric` applied to the right operand, same rule
already applied to the left) means a `total + delta`-shaped call site — right
operand statically numeric — never enters this mechanism at all: codegen
emits the identical bare BIF it always has, with no `try` whatsoever. Only a
right operand whose type is genuinely unknown at compile time (`5 + aVector`)
gets wrapped in the `try`/`catch`/`is_number`/`send_number_coercion` shape
(§ Dispatch mechanism of the ADR).

This turns the ADR's own de-risking spike (a hand-written, standalone `.core`
module measured ad hoc) into a permanent benchmark against the real codegen
output (BT-3263), in `bench_number_coercion/0` and
`bench_number_coercion_dispatch/0` (`runtime/perf/bench_collect_selfhost.escript`),
mirroring `bench_guard/0`'s own methodology (same tight-loop shape, same
`N`/`Reps`, same `min_us` best-of sampling).

| Case | overhead | ratio |
|------|---|---|
| `bench_number_coercion` — `total + delta` shape (bare BIF, no `try`) vs `bench_guard`'s own `Bare` (N = 5M adds) | noise-level (~0.2%) | **~1.00×** — statistically the same code |
| `bench_number_coercion` — `5 + aVector` shape, happy path (`try`/`catch`/`is_number`, never fails) vs bare (N = 5M adds) | ~8.7–8.8 ns/add | **~7.2×** (run-dependent — see below) |
| `bench_number_coercion_dispatch` — `5 + "not a vector"`, dispatch actually fires (real `send_number_coercion/4`, DNU + hint) (N = 20 000) | — | **~12.1 µs/op** |

#### Interpretation

- **Zero-cost claim, confirmed empirically.** The `total + delta` row is not a
  new code path — it's the same bare `erlang:'+'` loop `bench_guard/0`
  already measures as `Bare`, run again here to give an explicit before/after
  data point for this ADR rather than relying solely on "the codegen test
  asserts no `try` is emitted." Both runs landed within ~0.2% of each other
  (~7046–7083 µs/loop across two full runs) — noise, not a regression. This
  is the structural guarantee (§ Dispatch mechanism's "Trigger condition,
  refined") getting an empirical check: `total + delta` truly pays nothing.
- **The `try`/`catch` happy-path cost is comparable to the already-accepted
  `is_number` guard's, on the same sandbox, same run** — exactly the ADR's
  own conclusion. `bench_guard`'s guard-vs-bare ratio measured in the same
  run as the table above came out to **~6.75×**, next to this mechanism's
  **~7.2×** — the same ballpark, not dramatically worse, consistent with the
  ADR's Implementation § De-risking spike claim that the `try`/`catch`
  wrapper contributes only a small fraction beyond what the `is_number` check
  itself already costs.
- **Run-dependent, confirmed again.** This doc's own `bench_guard` entry
  above records **~2.7–3.0×** from an earlier measurement run; this session's
  re-measurement of that *same, unmodified* benchmark came out to **~6.75×**
  on this sandbox — matching the ADR's own de-risking-spike finding that its
  sandbox reproduced `bench_guard/0` "well outside" the recorded range. The
  **relative** comparison (this mechanism vs. the guard it sits next to, same
  run, same hardware) is the trustworthy reading; neither ratio's absolute
  value is portable off the machine it was measured on. Both figures above
  came from two consecutive full script runs on the same sandbox and agreed
  with each other to within ~1%, so the run-to-run variance is a
  machine/scheduling effect, not measurement noise within a single run.
- **The dispatch-triggering reference point (~12.1 µs/op) is orders of
  magnitude slower than the happy-path `try`, as expected** — it exercises
  the real `beamtalk_message_dispatch:send_number_coercion/4` (`RightClass`
  lookup, DNU class/selector match, hint formatting via `io_lib:format`,
  re-raise), not a stub, using the ADR's own REPL example (`5 + "not a
  vector"`, § REPL example). This cost is paid **only** on the already-
  failing path — never on the happy path the two rows above measure — the
  same "catch cost is exclusive to the failure path" property the ADR's
  § Dispatch mechanism argues for.
- Applies **only to the residual call sites this ADR's compile-time skip
  doesn't remove** — right operand type genuinely unknown at compile time.
  All hot stdlib arithmetic and `total + delta`-shaped user code (the
  overwhelming majority) is byte-for-byte unchanged and pays nothing,
  asserted by codegen regression tests in
  `crates/beamtalk-codegen/src/core_erlang/tests/expressions.rs`
  (`test_number_coercion_bare_bif_unaffected_for_total_plus_delta` and
  siblings).

## Array backing: map (current) vs alternatives — does Vector earn its place? (BT-2696)

BT-2696 proposed a bit-partitioned-trie `Vector` on the premise that core lacked
an indexed sequence. It doesn't: `Array` already provides O(log n) random access
+ persistent `at:put:`, backed by a canonical index→value map (ADR 0090 / BT-2680,
chosen *for* `=:=`/`phash2` correctness after the Erlang `array` module's
copy-on-write cache broke equality). This benchmark asks whether a faster backing
(what a trie resembles) would justify the correctness cost.

### Harness

`runtime/perf/bench_array_backing.escript` — single random `at:`/`at:put:`
(persistent: each op on the original structure, result discarded), comparing the
current `beamtalk_array` map backing against raw `maps`, the Erlang `:array`
module, a raw tuple, and a list.

### Results (N = 100 000, 200k random ops)

| op | `beamtalk_array` | raw map | `:array` | tuple | list |
|----|---|---|---|---|---|
| `at:`    | 135 ns | 76 ns | **45 ns** | 18 ns | O(n) |
| `at:put:`| 377 ns | 311 ns | **154 ns** | O(n) | — |

`at:put:` is flat across N=1k→100k (374→377 ns) — O(log n), no degradation.

### Conclusion

- The current map-backed `Array` is sub-microsecond and **canonical** (correct as
  a Dictionary/Set key). Good enough for a general indexed sequence.
- `:array` (≈ a trie's profile) is ~2–3× faster but is *exactly* the backing
  ADR 0090 rejected for breaking `=:=`. A bit-trie `Vector` inherits that
  tradeoff. **Not worth trading correctness for 2–3× on an already-sub-µs op.**
- **No perf justification for a `Vector` class or a backing swap. BT-2696 closed.**
- Minor optional win: `beamtalk_array:at` adds ~2× over raw `maps:get` (it
  recomputes `maps:size` for the bounds check each call) — trimmable, low priority.

## Selector-aware UTF-8 scan skip — dynamic dispatch on large binaries (BT-3033)

BT-2999 made `module_for_value/1` classify a bare binary by `is_utf8/1`
(String vs Binary) on every dynamic send, replacing an O(1) `is_binary` guard
with an O(byte_size) validating scan. That cost only bites for **large,
valid-UTF-8** binaries sent to **dynamically**-typed receivers: repeatedly
messaging the same multi-megabyte binary re-scans it every send, so an
N-iteration loop over one large binary is O(N x size). This benchmark
establishes that baseline and measures the fix: for selectors where String
and Binary behave identically (`byteAt:`, `byteSize`, `part:size:`,
`concat:`, `toBytes`, `asStringUnchecked`, `asBase64`, `asBase64Url`,
`asHex` — see `is_string_binary_shared_selector/1`), `send/3` and
`responds_to/2` now skip the scan entirely via `module_for_value/2`.

### Harness

`runtime/perf/bench_string_binary_dispatch.escript`. Run from the `runtime/`
directory after `just build`:

```bash
escript perf/bench_string_binary_dispatch.escript
```

For each binary size, sends `byteAt:` dynamically (`beamtalk_primitive:send/3`,
no static type annotation) in a loop over the *same* receiver, 2000 times:

- **before** — the pre-fix path, simulated inline (`is_utf8/1` then dispatch)
  since `module_for_value/1` itself is unchanged and still used by callers
  without a selector in hand.
- **after** — the real current `send/3` path (`module_for_value/2`, skips the
  scan for `byteAt:`).
- **after, `size`** — a contrasting selector that genuinely differs between
  String and Binary, so it must (and does) keep paying the scan — proving the
  fix is selector-specific, not a blanket skip.

A sanity check asserts all three paths agree with `binary:at/2` on a sample
index before timing starts, so the speedup isn't a correctness regression in
disguise.

### Workspace

Valid-UTF-8 (ASCII) binaries of 1 KB, 64 KB, and 1 MB — the sizes BT-2999's
own measurements used, plus the >=1 MB acceptance-criteria size.

### Results (K = 2000 dynamic sends per size)

| size | before (always scans) | after (`byteAt:`, skips) | speedup | after (`size`, still scans) |
|---|---|---|---|---|
| 1 KB | 0.670 us/op | 0.214 us/op | ~3.1x | 2.954 us/op |
| 64 KB | 28.878 us/op | 0.192 us/op | ~150x | 148.439 us/op |
| 1 MB | 368.910 us/op | 0.224 us/op | **~1650x** | 2389.153 us/op |

**Note:** these numbers were captured before a follow-up fix added a
`beamtalk_extensions:has('String', Selector)` guard to the fast path (a
review-caught correctness gap: an ADR 0066 `String` extension on one of the
9 shared selectors must not fire for a genuinely non-UTF-8 receiver). That
guard is a single `ets:member` check, negligible next to the numbers above —
re-measure if the table is ever regenerated, but no material change is
expected.

### Interpretation

- **`byteAt:`'s cost is now flat (~0.2 us/op) regardless of receiver size** —
  the scan it used to pay is gone, and what's left is ordinary dispatch
  overhead. The N-iteration-over-one-large-binary case the issue was filed
  for goes from ~369 us/send to ~0.22 us/send at 1 MB.
- **`size` is deliberately unaffected** (in fact its per-op cost is *higher*
  than the simulated `is_utf8/1`-only "before" row, since it pays both the
  scan in `module_for_value/1` — `size` is not in the shared-selector set —
  *and* String's own O(size) grapheme count once dispatched). This is
  expected and correct: `size` means different things on String vs Binary,
  so it must keep discriminating.
- Confirms BT-2999's own measurements (~0.4 ns/byte scan cost) at all three
  sizes, and confirms the fix removes exactly that cost for the selectors
  it's safe to remove it for.

### Decision (BT-3033)

**Implemented approach 1/2 from the issue** (selector-aware skip, backed by a
build-time-checked selector set rather than a purely hand-maintained one):
`beamtalk_primitive:is_string_binary_shared_selector/1` lists the `binary.bt`
instance selectors `string.bt` does not redefine, and
`crates/beamtalk-cli/src/commands/build_stdlib.rs`'s
`test_binary_string_shared_selectors_stay_in_sync` recomputes that same set
from the real `.bt` sources on every `cargo test` run, failing if either file
changes the override relationship the hardcoded Erlang list assumes — the
fragility the issue flagged for approach 1 is caught by CI, not left to
manual vigilance. `class_of/1` and every selector not in the shared set are
untouched and still pay the full `is_utf8/1` scan, so dispatch stays exactly
as consistent with `class` as BT-2999 made it.

### Follow-up: route straight to `bt@stdlib@binary` (BT-3049)

BT-3033's initial fast path routed to `'bt@stdlib@string'` for the 9 shared
selectors. Since none of them are locally defined in `string.bt`, that
module's compiled `dispatch/3` re-checks `beamtalk_extensions:lookup/2` for
a `String` extension and then delegates to `'bt@stdlib@binary'` anyway — a
redundant hop the fast path pays on every shared-selector send. Since all 9
selectors are locally defined on `Binary`, and ADR 0066 forbids an `extend`
from overriding a class-body-defined method, `Binary` itself can never have
a competing extension for one of them — so once the `String`-extension
check (already required for correctness, see above) passes, routing
straight to `'bt@stdlib@binary'` is safe and skips that hop entirely.

**Isolated measurement** (`bench_hop/0` in the same escript, 1 MB binary,
200,000 direct `dispatch/3` calls, no `send/3` wrapper — isolates the hop
cost from `module_for_value/2`'s own ~40-200 ns of overhead):

| path | us/op |
|---|---|
| via `'bt@stdlib@string'` (old: extra hop + extension re-check) | 0.214 |
| via `'bt@stdlib@binary'` (new: direct) | 0.036 |

**~6x reduction on the isolated hop** — consistent across repeated runs
(0.21-0.22 vs 0.035-0.04 us/op). In the full `send/3` pipeline (the `after`
rows in the table above), this shows up as a smaller, noisier effect since
`module_for_value/2`'s own checks (selector match, `extensions:has`) and
`send/3`'s dispatch wrapper dominate the ~0.2 us total — the removed hop was
a large fraction of a small number, not a large fraction of the whole path.

**Decision:** implemented — `module_for_value/2`'s shared-selector branch
now returns `'bt@stdlib@binary'` directly (see `beamtalk_primitive.erl`'s
doc comment on `module_for_value/2` for the ADR 0066 argument this relies
on). `test_binary_string_shared_selectors_stay_in_sync` gained a note that
it can't see `extend`-registered overrides (ADR 0066), documenting the
guarantee boundary the issue asked for.

# Benchmarks

Performance measurements for Beamtalk, recorded as features land. Each entry
notes the harness, the workspace shape, and the before/after numbers so claims
in ADRs and issues can be checked against real data.

## SystemNavigation `sendersOf:` — xref index migration (ADR 0087 Phase 3, BT-2299)

ADR 0087 introduces a runtime-maintained cross-reference index
(`beamtalk_xref`) so navigation queries read from ETS instead of re-parsing
every method source on each call. Phase 3 migrated `SystemNavigation
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

Query: `SystemNavigation sendersOf: #asString`

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

## Class-side self-send cost (BT-3666 / BT-3669 / BT-3675, measured in BT-3676)

> **Historical.** The per-scope token, commit, export and `class_var_scope_*` helpers measured below were deleted by [ADR 0130](../ADR/0130-class-variables-live-in-the-class-process.md) (class variables now live in the class process). The harness section still describes the current method; for the current numbers see [ADR 0130 Phase 0](#adr-0130-phase-0-single-home-class-variables-perf-spike-and-gates-bt-3702), in particular its final gate summary.

### Harness

`runtime/perf/self_send_bench` is a small Beamtalk package whose `SsbMain run`
times 200,000 self-sends per case (after a 1,000-iteration warmup) and prints
`PERF: <case> <ns>ns/op` to stderr:

```bash
cd runtime/perf/self_send_bench
beamtalk run SsbMain run
```

It is not part of `just perf` (`rebar3 eunit --dir=perf` only runs the Erlang
suite in `runtime/perf/beamtalk_perf_tests.erl`). Run it on a quiet machine,
build the compiler and stdlib for the commit under test first, and compare
**interleaved** runs (A, B, A, B, ...) by median; run-to-run spread on one box
is roughly +/-15%.

### Results (ns/op, median of 6 interleaved runs, 4-core VM, otherwise idle)

Baseline is `f12484e06` (the parent of BT-3666, with the bench directory copied
in); "before" is `d8e35f815`; "after" is the BT-3676 change.

| case | profile | baseline | before | after |
|---|---|---|---|---|
| class self-send, open class | debug | 106 | 469 | 338 |
| class self-send, open class | release | 97 | 449 | 291 |
| class self-send, sealed class | debug | 102 | 103 | 106 |
| class self-send, sealed class | release | 101 | 100 | 100 |
| actor self-send, open | debug | 89 | 104 | 107 |
| actor self-send, open | release | 87 | 106 | 101 |
| actor self-send, sealed | debug / release | 67 / 66 | 70 / 81 | 73 / 70 |

"release" is the release-profile Rust CLI; the Erlang runtime is built by
rebar3 either way. Min-max spread of the open class-side case is 272-379 ns
(after, debug) and 249-383 ns (after, release); runs of the same side differ by
up to ~40%, so the baseline-to-before gap (4x) is far outside noise but the
before-to-after gain (about 30-35%) is only moderately so.

### Where the open class-side cost went

An open class's self-send inside a block or loop pays, per iteration: one
`make_ref` (the scope token), `class_var_scope_read`, the
`class_self_direct_ok` guard, `class_var_scope_commit` and
`class_var_scope_export` (BT-3675). Standalone microbenchmarks of each helper
(debug runtime, `erl` shell) put the commit/export process-dictionary
read-modify-write at ~25-100 ns, the `make_ref` at ~30 ns, and the guard's
`ets:whereis/1` at ~50-90 ns plus ~30 ns for its two `persistent_term` reads.
BT-3676 replaces the `ets:whereis/1` with a `persistent_term` readiness flag
(`beamtalk_class_shadow_flags:is_ready/0`).

Skipping the per-send commit when the callee returned the same `ClassVars`
term was also tried (it brought the open class-side case to ~185-198 ns), but
it was dropped: the arm refresh `take`s only its own token and falls back to
the lexical version, so a skipped commit changes what a later refresh sees
(it flipped the pinned `testBlockPassedToClassSideHomInLoopBody` answer), and
doing it safely means changing the refresh's fallback at three codegen sites.
That is the remaining ~190 ns over baseline and needs a follow-up with a
proper design. The actor open self-send increase (~85 to ~105) comes from
BT-3666's late binding and was not profiled.

### `NestedImprovementRatio >= 1.5`

`block/nested_list_op_improvement` in `beamtalk_perf_tests.erl` compares two
hand-written Erlang functions in `bench_block_threading.erl` (a StateAcc map
vs an expanded tuple); it exercises no Beamtalk codegen or class dispatch. It
is flaky near its threshold: over 4 (baseline) and 5 (main) runs the ratio
ranged 1.34x to 1.88x, on both the pre-BT-3666 baseline and current main (the
tuple variant is bimodal, ~730 us or ~930 us).

### BT-3690: re-measurement, profile and what changed

Same method as above (`cd runtime/perf/self_send_bench && beamtalk run SsbMain run`), idle 4-core VM
(`uptime` load 1.6-1.9 is the benchmark's own BEAM; nothing else ran), 7 interleaved rounds per side
and CLI profile, medians in ns/op with the min-max in brackets. The bench gained one case,
`class_self_send_open_in_arm` (`1 to: n do: [:i | i > 0 ifTrue: [self foo]]`), so the arm refresh and
BT-3683's extra read are measured. Baseline is `f12484e06` (parent of BT-3666 with the bench directory
copied in), "main" is `bcb40b028`, "after" is BT-3690. The bench's `_build` is deleted before each
side's first run (`beamtalk run` skips recompilation when only the compiler changed).

| case | profile | baseline | main | after BT-3690 |
|---|---|---|---|---|
| class self-send, open (defining class) | debug | 115 [114-124] | 354 [341-452] | 215 [208-221] |
| class self-send, open (defining class) | release | 119 [111-134] | 373 [326-401] | 216 [210-231] |
| class self-send, open, in an `ifTrue:` arm | debug | 161 [154-167] | 572 [543-679] | 287 [275-358] |
| class self-send, open, in an `ifTrue:` arm | release | 156 [153-178] | 567 [531-707] | 289 [274-303] |
| class self-send, sealed | debug / release | 116 / 115 | 119 / 116 | 118 / 117 |
| class self-send via a subclass receiver (walk) | debug / release | 127 / 128 (statically bound, ignores the override) | 1770 / 1800 | 1559 / 1545 |
| actor self-send, open | debug / release | 99 / 99 | 122 / 123 | 121 / 125 |
| actor self-send, sealed | debug / release | 76 / 83 | 89 / 86 | 85 / 84 |

The "release" CLI changes only the Rust compiler; the generated code and the Erlang runtime are the same
for both, so the two rows of a pair are two samples of the same thing (their spread is the noise floor).

**BT-3683's extra scope read costs nothing measurable.** Debug, 7 interleaved rounds, `bcb40b028` vs its
ancestor `8bb07e752` (before #4130): open send 364 [356-432] vs 369 [350-407], open send in an arm
577 [536-651] vs 586 [540-619].

**15% target: not met.** The defining-class open send is about 1.8x the baseline after the change
(215 vs 117), down from about 3.1x. 41% less time than main for the plain send, 49% less in an arm.

#### Profile of the per-send path

Microbenchmarks in an `erl` shell against the real `beamtalk_class_dispatch` / `beamtalk_class_shadow_flags`
modules (3-5 million iterations, loop overhead subtracted; the numbers are run-dependent by a few ns):

| step of an open-class send in a loop | cost |
|---|---|
| `make_ref()` for the closure region's token | ~12 ns (28 ns with its map store) |
| `class_var_scope_read/3` (pdict get + map lookup) | ~14 ns |
| `class_self_direct_ok/4`: 3 `persistent_term` reads | ~75 ns (a present key ~21 ns, a missing key ~8 ns, a freshly built tuple key +9 ns) |
| `class_var_scope_commit/3` (pdict get, map put, pdict put) | ~46 ns |
| `class_var_scope_export/3` with an entry to move | ~85 ns (with no entry: ~15 ns) |
| token + read + commit + export together | ~150-180 ns |
| same without the commit (plain reply) | ~60 ns |
| one pdict get + put of a one-key map | ~31 ns |

The commit (and the export it feeds) was the largest piece: about 120 ns of the machinery. Changes the
profile supports, all taken:

1. **Commit only a reply that may have changed the class variables** (codegen, ADR 0110 amendment BT-3690).
   Sends with a block literal argument keep the unconditional commit. Measured effect: ~360 to ~220 ns.
2. **One inlined guard function** (`beamtalk_class_shadow_flags:direct_call_ok/2`): the shadow flags first (a
   missing key is the cheap lookup), then readiness, with no intermediate calls. 79 to 58 ns in isolation.

Not taken, with the reason:

- *Merging the flag reads into one key.* The `extension` flag is written under a per-tag lock by arbitrary
  processes and the `runtime_fun` flag by the class process; one combined key needs a common writer lock
  (the `beamtalk_class_shadow_flags` moduledoc explains why they are separate). Readiness (BT-3676) is pinned
  by `beamtalk_class_shadow_flags_tests`. Floor with three reads: ~55 ns, about a quarter of what is left.
- *Skipping the token or the pre-call read for a callee known not to write or read class variables.* In the
  guard-true branch the callee is statically `class_foo`, but the other branch (a subclass receiver) needs
  both, the token is bound once per scope entry before the branch, and "does not read class variables" is a
  new whole-class fixed point next to `compute_class_var_mutating_selectors`. It would save ~15 ns (read)
  plus the export; not worth a new analysis in a path the lost-write bugs came from.
- *`unique_integer()` instead of `make_ref()` for tokens:* ~8 ns, and it changes the token type everywhere.
- *Per-token process-dictionary keys instead of one map:* slower (246 vs 179 ns).

What remains per send (~100 ns over the baseline): guard ~55, token ~12-26, read ~14, export ~15, extra reply
`case`s. Removing the guard needs a different invalidation scheme (a per-selector "overridden anywhere"
flag, which would also speed up the subclass walk); removing the token needs the scope to be lazy. Both are
design changes, not tuning.

The subclass-receiver walk (`class_self_send_inherited_override`, ~1.5 us) is the cost of reaching an override
(`class_self_send/4` hierarchy walk); it was ~1.7 us before, it improves only through the shared pieces.

#### Actor open self-send

Profile (isolated, `erl` shell with the compiled `ssb_open_actor`): an open actor's `self foo` compiles to
`maps:get('__class_mod__', StateAcc, M)` + a dynamic call of `ClassMod:safe_dispatch('foo', [], StateAcc)`,
whose body calls `beamtalk_actor:make_self(State)` (the actor's real state has no `$beamtalk_class` key, so
`class_of/1` takes the `function_exported` + `Mod:class_name()` path), enters a `try`, calls `dispatch/4` and
returns a `{reply, Result, State}` that the call site unpacks. Measured with the real (untagged) state:
`make_self/1` ~55 ns, so about 45% of the ~125 ns send and the only piece worth a change; `dispatch/4` itself
is ~10 ns. The sealed actor calls the method directly (~84 ns).

A runtime-only change to `make_self` (read `__class_mod__` once, a `class_and_mod/1` helper) measured
61 vs 77 ns when called inline in the microbenchmark but gave no difference through the generated code
(122 vs 121 ns debug, 9 rounds), because the helper call and its tuple cost what the second lookup saved. It was
not kept. The real saving is for the call site to pass the `Self` it already has (a `safe_dispatch/4`, ~50 ns
or ~40% of the open actor send, getting it to about the sealed actor's cost), which touches every actor
module's generated exports and the gen_server entry points, so it is left as a follow-up rather than done
here. The +25% over the baseline (99 vs 122-125) is the BT-3666 late binding and is intentional.

#### `NestedImprovementRatio >= 1.5`

Reproduced: in a full `rebar3 eunit --dir=perf` run the ratio was 1.43x (StateAcc median 1557 us, tuple 1092 us),
while a standalone `erl` run of the same two functions gave 1.78x (1975 / 1110 us), and a fresh process in the
suite gave 2.72x (3017 / 1109 us). The tuple variant is stable (~1.1 ms); the StateAcc variant allocates a map
per element and its time follows the GC conditions of the measuring process (a long-lived EUnit process with the
heap of the earlier benchmarks vs a fresh one). The test timed the two variants back to back in that
long-lived process, so the ratio depended on its heap history. It now times them alternately (A, B, A, B, ...)
in one fresh process with a fixed `min_heap_size` (`run_paired_benchmark/4`). Over four full runs after the
change the ratio was 1.83x, 1.89x, 1.90x and 1.86x against the unchanged 1.5x threshold. The threshold is not
changed and the test is not skipped.

## ADR 0130 Phase 0: single-home class variables, perf spike and gates (BT-3702)

Throwaway spike for the four gates in ADR 0130 § Implementation Phase 0. Nothing from the spike is merged: the
PR adds two bench cases and this section. The spike hand-lowers the bench classes to the ADR's convention (no
`ClassVars` argument, reads and writes through a stub `beamtalk_class_vars`, `snapshot/0` + `restore/1` around
the `try`) with the key shapes hardcoded, and times them next to the compiler's current output.

### Harness

- New `SsbMain` cases in `runtime/perf/self_send_bench`: `class_var_10r_3w_loop` (`SsbClassVarClass`, a
  `1 to: n do:` body with ten class-variable reads and three writes, no sends, plus one local write because a
  loop body whose only mutation is a class-variable write is rejected at compile time) and `instance_on_do_loop`
  (`SsbOnDoObject`, a `Value` whose method runs `[i + 1] on: Error do: [:e | 0]` in a loop, outside any class
  invocation, so the new lowering's `snapshot/0` would answer `none`). Values stay small integers.
- The hand-lowered variants cannot be reached through `beamtalk run`, so the gates were measured with an
  `erl` driver that loads the bench's compiled modules, registers the bench classes, and calls today's
  `class_loop:/3` and the spike's `class_loop:/2` in the same process, 200,000 iterations after a 1,000
  iteration warmup, one case after another per round. The spike modules were a hand-edited copy of the
  generated `.core` for the self-send and `on:do:` cases (`snapshot` before the `try`, `restore` as the first
  statement of the non-NLR catch branch, after both `$bt_nlr` arms) and Erlang for the class-variable loop
  (a mirror of the generated Core with the threading removed). The stub follows ADR 0130 §2 and §4: `get`
  derives `{'$bt_class_vars', Tag}` from `ClassSelf`, `put` also checks the `{'$bt_class_vars_ro', Tag}`
  marker, `snapshot/0` is one `get` of `'$bt_class_vars_home'`.
- Baseline is `f12484e06` (the parent of BT-3666, bench sources copied in), built and run in its own worktree.
  "Main" is `96dd867` (current `main`, which is BT-3690's state). 8 rounds per side, interleaved
  (baseline, main, baseline, main, ...), each round one fresh `erl` per side. Medians with min-max.
- **The machine was not idle.** Four cores, load average 5 to 11 from other builds running in parallel during
  the measurements. The interleaving and the medians are there for that, but every figure below carries more
  noise than BT-3690's, and the numbers that pass narrowly should be read as "within noise of the bound". The
  `SsbMain` end-to-end numbers (single runs) are in the last table.

### Results (ns/op, median [min-max], 8 rounds per side)

| case | baseline `f12484e06` | main today | spike (ADR 0130 lowering) |
|---|---|---|---|
| open class-side self-send, top level | 129 [123-151] | 250 [234-314] | 217 [199-274] |
| open class-side self-send, in an `ifTrue:` arm | 180 [169-259] | 295 [285-518] | 254 [242-379] |
| sealed class-side self-send | 133 [124-234] | 132 [122-164] | not lowered (unchanged) |
| class-variable loop, 10 reads + 3 writes | not comparable (see below) | 289 [268-521] | helper calls 670 [607-808], inlined 183 [181-236] |
| instance-side `on:do:` loop (no invocation) | 2348 [2293-3123] | 2365 [2263-2748] | helper `snapshot`/`restore` 2396 [2276-2753], inlined 2485 [2270-2820] |

The baseline's class-variable loop could not be timed through the driver (the driver hands the old convention a
plain map and the baseline's write path answered at ~31 us/op, an artefact of the driver, not a measurement of
the baseline); the `SsbMain` run of the baseline gives 203 ns/op for the same case.

### Gate 1: open self-send, `median_after <= 1.15 * baseline + guard`

`guard` re-measured (loop-subtracted `class_self_direct_ok/4` on a registered open class): 63 to 77 ns across
four runs, 73 ns in the 9-round run used below (BT-3690 profiled about 55 to 75).

| case | bound | spike | verdict |
|---|---|---|---|
| top level | 1.15 x 129 + 73 = 221 | 217 | **passes, by 4 ns, inside the noise** |
| in an `ifTrue:` arm | 1.15 x 180 + 73 = 280 | 254 | **passes** |

What the spike removed from main's 250: the `make_ref()` token, the scope read, the reply `case`s, the commit and
the export (about 35 ns top level, about 40 ns in an arm). What it keeps is the guard (`class_self_direct_ok/4`,
about 73 of the 217), which BT-3700 owns. Without the guard the spike is about 145 ns top level against the
baseline's 129, so BT-3700's own "within 15% of baseline" target is within reach of the guard cut alone. The
gate passes only narrowly at top level because the guard re-measured at the high end of its range; it should
be re-run on an idle machine when Phase 3 lands.

### Gate 2: sealed self-send

132 [122-164] on main against 133 [124-234] on the baseline: no regression beyond noise. Sealed self-sends
never touch class variables, so the lowering leaves them unchanged.

### Gate 3: ten reads and three writes per iteration, `median_after <= 2 * median_today`

| lowering | median | vs today (289) | verdict |
|---|---|---|---|
| helper calls (`beamtalk_class_vars:get/2`, `put/3`) | 670 | 2.3x | **fails** |
| inlined (`erlang:get/1` + `maps:get/2` / `maps:put/3`, key hardcoded) | 183 | 0.63x | **passes** |

Per-operation costs (loop-subtracted, 2,000,000 iterations, median of 9; an upper bound for the inlined form,
because in the unrolled loop the compiler hoists the literal key tuple):

| operation | today (threaded, with shadow `put` and `class_var_scope_commit`) | helper call | inlined |
|---|---|---|---|
| read | 2 ns (a lexical `maps:get`) | 29 ns | 12 ns |
| write | 133 ns | 110 ns | 61 ns |

The helper's read is the cost: ten reads at 29 ns each is about 290 ns of a 670 ns body, against nearly free
lexical reads today, and two remote calls and a key tuple per access (`get` builds `{'$bt_class_vars', Tag}`,
`put` builds two). Writes are cheaper in either form than today's, because today every write pays the
shadow `put` and the commit. The inlined form beats today's in total because the three writes save about 72 ns
each (133 to 61, about 215 ns in all), which outweighs the ten reads costing about 10 ns more each (about 100 ns
in all): about 115 ns net saving predicted from the per-operation figures, 106 ns measured.

**Chosen lowering for class-variable access: inlined.** The helper form fails the 2x gate, the inlined form
passes with room. This is the case ADR 0130 Phase 0 names for the conditional work: the `class_var_keys` leaf in
`beamtalk-codegen`, the `build-stdlib` regeneration of the checked-in `beamtalk_class_vars_keys.hrl`, its
inclusion by `beamtalk_class_vars` and the `check-generated-builtins` extension land with Phase 3.

### Gate 4: `snapshot/0` + restore arm on an instance-side `on:do:`, within 10%

| lowering | median | vs today (2365) | verdict |
|---|---|---|---|
| helper calls (`beamtalk_class_vars:snapshot/0`, `restore/1`) | 2396 | +1.3% | **passes** |
| inlined `get` of `'$bt_class_vars_home'` | 2485 | +5.1% | passes (within noise: the three variants' ranges overlap) |

Isolated cost of the pair outside any invocation (answers `none`, `restore(none)` does nothing), loop-subtracted:
helper 6 ns, inlined 3 ns, so about 0.3% and 0.1% of the 2.4 us loop. The loop itself is dominated by what an
`on:do:` already pays (a closure, `beamtalk_class_registry:whereis_class/1` for the filter, the handler fun),
which is why the percentages are small and the measured medians differ only by noise. **Helper calls are
enough for `snapshot/0` and `restore/1`** (the inlined `get` is not needed for gate 4). Since inlining is adopted
for class-variable access anyway, Phase 3 may inline the pair too; the 3 ns it saves is not a reason to.

### Not measured

- The cost of the invocation boundary itself (`assert_absent/1` + `install/2` on entry, read-back and `erase` in
  `after` of `invoke_class_method/7`), which is Phase 2 and was assumed small next to the `gen_server` round trip.
- The restore arm taken (an error crossing a catch): only the pass-through path was measured.
- `Result tryDo:` and `protect/1`; the subclass-receiver walk after the change (`class_self_send_inherited_override`,
  not hand-lowered); the actor open self-send (BT-3692); the release profile of the CLI (the generated code is the
  same either way).
- An idle machine: see the note under Harness.

### `SsbMain` end to end (single runs, ns/op)

These are today's compiler output only, one run per side, on the same loaded machine; they are the numbers the
new cases print, kept as a reference for the Phase 3 re-run, not as gate evidence.

| case | baseline | main |
|---|---|---|
| `class_var_10r_3w_loop` | 203 | 282, 294 |
| `instance_on_do_loop` | 2500 | 2434, 2468, 2618 |

### Phase 3 re-measure of gate 4 with the real lowering (BT-3711)

Only gate 4 was re-measured here (the other gates belong to BT-3709's lowering). The compiler now emits
`let Snap = beamtalk_class_vars:snapshot() in` before every `on:do:`'s `try` and `do
beamtalk_class_vars:restore(Snap)` as the first statement of the non-NLR catch arm, as helper calls (gate 4 chose
helpers). Case: `SsbOnDoObject run:` (`runtime/perf/self_send_bench`, `[i + 1] on: Error do: [:e | 0]` in a
`1 to: n do:` loop, outside any class invocation, so `snapshot/0` answers `none`).

Method: the bench package was built with the BT-3711 compiler; its `ssb_on_do_object.core` is the "instrumented"
side, and the "today" side is the same `.core` with exactly the two inserted expressions removed. That is what
`origin/adr-0130` emits for this module: the corpus `.core` diff of the whole stdlib + test corpus against
`origin/adr-0130` is, after removing the snapshot, restore and capture insertions and renumbering temporaries,
empty. Both sides were compiled with `erlc +from_core` under different module names and timed in one `erl`
process (runtime and stdlib applications started), 200,000 iterations after 1,000 warmup, 15 interleaved rounds
(plain, instrumented, plain, ...) per run, three runs. 4-core VM, load average under 1 at the start of each run.

| run | today (plain) | BT-3711 (snapshot + restore arm) | delta |
|---|---|---|---|
| 1 | 2300 [2208-2544] | 2330 [2155-2632] | +1.3% |
| 2 | 2400 [2140-2982] | 2338 [2181-3221] | -2.6% |
| 3 | 2327 [2120-2853] | 2329 [2200-2661] | +0.1% |

Medians in ns/op with min-max. **Gate 4 passes**: within 10% (and within the noise of the run-to-run spread).
Not measured here: the restore arm taken (an error crossing a catch), and the end-to-end `SsbMain` run through
`beamtalk run`.

### Phase 3 re-measure of gates 1 to 4 on the integration branch (BT-3713)

All four gates, with the real lowering, through `runtime/perf/self_send_bench` as in "Harness" above
(`beamtalk run SsbMain run`, 200,000 iterations per case after 1,000 warmup). Three sides, each built from its
own checkout with its own compiler, stdlib and bench package and run in turn (baseline, main, branch, baseline,
...), 9 interleaved rounds, medians with min-max, after one discarded warm-up run per side:

- **baseline**: `f12484e06` (the parent of BT-3666; the bench package copied in from the branch).
- **main**: `3fe282c76`, `origin/main`, the "today" of gates 3 and 4 (threaded class variables, BT-3690).
- **branch**: this PR, `adr-0130` plus BT-3713 (single-home class variables, the real lowering).

Machine: 4 cores, nothing else running (checked with `ps` before and after; no builds, no other agent). The
1-minute load average at the start of each round was 1.31 to 1.57, which is the benchmark's own BEAM
schedulers: the `beamtalk run` of the previous round. The Rust CLI of the baseline and main sides was built with
`CARGO_PROFILE_DEV_DEBUG=0` to save disk, the branch with the default dev profile; the generated code and the
runtime beams do not depend on either. Harness: a throwaway script that alternates the three binaries (not
checked in; the method above is the recipe).

| case (ns/op, median [min-max], n = 9) | baseline `f12484e06` | main `3fe282c76` | branch |
|---|---|---|---|
| class self-send, open class (top level) | 120 [114-157] | 223 [213-318] | 199 [193-230] |
| class self-send, open class in an `ifTrue:` arm | 168 [160-247] | 330 [293-361] | 260 [240-279] |
| class self-send, sealed class | 121 [113-152] | 124 [115-133] | 119 [116-135] |
| class-variable loop, 10 reads + 3 writes | 171 [164-208] | 285 [275-394] | 582 [566-1819] |
| instance-side `on:do:` loop (no invocation) | 2399 [2266-2723] | 2267 [2179-3078] | 2313 [2191-2553] |
| class self-send, inherited override (walk) | 128 [126-170] | 1627 [1539-1925] | 1908 [1745-2434] |
| actor self-send, open | 104 [93-222] | 132 [122-193] | 127 [119-197] |
| actor self-send, sealed | 77 [74-126] | 87 [80-97] | 86 [80-162] |
| actor self-send, inherited override | 87 [83-123] | 118 [112-150] | 117 [112-161] |

#### Gate 1: open self-send, `median_after <= 1.15 * baseline + guard`

`guard`, re-measured on this branch (loop-subtracted `beamtalk_class_dispatch:class_self_direct_ok/4` on a
registered open class, 2,000,000 iterations, 9 rounds): **75 ns** [71-83].

| case | bound | branch | verdict |
|---|---|---|---|
| top level | 1.15 x 120 + 75 = 213 | 199 [193-230] | **passes** (main: 223, which would have failed) |
| in an `ifTrue:` arm | 1.15 x 168 + 75 = 268 | 260 [240-279] | **passes**, by 8 ns (the arm's max, 279, is above the bound) |

The token, scope read, export and commit that main paid per send are gone: the branch is 24 ns (top level) and
70 ns (arm) below main. What is left over the baseline is the guard (75 ns) plus noise; BT-3700 owns it.

#### Gate 2: sealed self-send

119 [116-135] against 121 [113-152] on the baseline and 124 [115-133] on main: **no regression**.

#### Gate 3: ten reads and three writes per iteration, `median_after <= 2 * median_today`

Branch 582 [566-1819] against main 285 [275-394]: **2.04x, the gate fails by 12 ns (2%)**. Seven of the nine
rounds are in 566-644; two rounds hit scheduler or GC outliers (1819, 1042), which the median ignores. The
paired per-round ratio branch/main has median 2.11. The access is already the inlined form the ADR's fallback
asks for (`erlang:get/1` plus `maps:find/2` for a read, `erlang:get/1`, `maps:put/3` and `erlang:put/2` for a
write, helper call only on a miss; ADR 0130 §2, BT-3709), so the fallback is used up. **Handled as the ADR
prescribes: the number is recorded and accepted, not used to reopen the decision.** Why it is above the spike's
183 ns: the spike hardcoded the key as a literal tuple, which the compiler stores once. The real lowering
builds `{'$bt_class_vars', element(2, ClassSelf)}` on every access (13 times per iteration), because the key
depends on the receiver's class tag. A cheap follow-up that is not done here: bind the key once per method (a
`let`) and reuse it for every access in the method, which removes 13 tuple builds and `element/2` calls per
iteration. Until then a class method that touches class variables in a hot loop is about twice as expensive as it
was on main, and about 3.4x the baseline (which had no class-variable threading to pay for at all: its 171 ns is
a lexical `maps:get`).

#### Gate 4: `snapshot/0` + restore arm around an instance-side `on:do:`, within 10%

Branch 2313 [2191-2553] against main 2267 [2179-3078]: **+2.0%, passes**. (The real lowering, end to end
through `beamtalk run`; the BT-3711 measurement, which timed the instrumented and plain `.core` of the same
module in one process, gave +1.3%, -2.6%, +0.1%.)

#### Other observations (not gates)

- `class_self_send_inherited_override`, the late-bound hierarchy walk (`class_self_send/4`), is 1908 ns on the
  branch, 1627 on main and 128 on the baseline: roughly 13x the baseline already on main, and 17% above main
  now. No gate covers it (ADR 0130 leaves the late-binding guard and the walk to BT-3700); recorded here so the
  17% is not lost.
- Actor self-sends are unchanged by this ADR and within noise of main.

#### Final gate summary (ADR 0130 Phase 4, BT-3714)

The numbers above are the final ones; nothing was re-run for the docs sweep.

| gate | bound | result (median) | verdict |
|---|---|---|---|
| 1. open self-send, top level | 213 ns | 199 ns | passes |
| 1. open self-send, in an `ifTrue:` arm | 268 ns | 260 ns | passes (by 8 ns) |
| 2. sealed self-send | no regression | 119 ns vs 121 ns baseline | passes |
| 3. 10 reads + 3 writes per iteration | `2 * 285 = 570` ns | **582 ns** | **fails by 12 ns (2%); accepted** |
| 4. `snapshot/0` + restore around `on:do:` | within 10% of main | +2.0% | passes |

Gate 3 was accepted rather than used to reopen the single-home decision, as the ADR prescribes. The follow-up is [BT-3719](https://linear.app/beamtalk/issue/BT-3719): bind the class-variable key once per method instead of at each inlined access, then re-run gate 3 (target at or under 570 ns) and re-measure the late-bound hierarchy walk (`class_self_send_inherited_override`, 1908 ns, +17% over main), recovering or explaining it. Until then a class method that touches class variables in a hot loop costs about twice what it did on `main` before ADR 0130.

#### Key bound once per method (BT-3719)

The compiler now binds the class key once per class-method body, at the first inlined access, as
`let _CVKeyN = {'$bt_class_vars', call 'erlang':'element'(2, ClassSelf)} in <body>`, and every inlined read and
write in the body (loop bodies and blocks included) reuses `_CVKeyN`. A method with no class-variable access binds
nothing. It is one lowering point shared by compiled class methods, `ClassBuilder` funs and class-side extension
funs (`class_method_body_doc`), emitted through `class_var_keys::key_binding_doc` (`Document` + `leaf::*`).
No state-threading scope is involved (class variables are written in place), so there is no `ThreadedIr` node to
change; `just verify-threaded-ir` and `just test-class-var-corpus` pass.

Method: `SsbMain run` through `beamtalk run`, two compilers built from the same checkout (before = `origin/main`
`fbeac1c39`, after = this change), the bench package rebuilt from scratch with each, 7 interleaved rounds
(before, after, before, ...), medians with min-max, ns/op. This was a shared, loaded 4-core VM, so the absolute
numbers are about 1.8x those of the BT-3713 table above (before: 1026 here against 582 there); only the
paired before/after ratio is meaningful.

| case (ns/op, median [min-max], n = 7) | before | after | ratio |
|---|---|---|---|
| class-variable loop, 10 reads + 3 writes | 1026 [917-1149] | 723 [710-819] | 0.70 |
| class self-send, inherited override (walk) | 2497 [2142-2677] | 2448 [2197-2685] | 0.98 (noise) |

**Gate 3.** The loop is 30% cheaper (every round of "after" is below every round of "before" but one). Applied to
the BT-3713 figure, 582 ns x 0.70 is about 410 ns, under the 570 ns budget (2 x 285 ns on main); the ratio is
the measured quantity, the 410 ns is a projection, not a re-run on the idle machine of the BT-3713 table.
The remaining gap to the spike's 183 ns is the `element/2` plus tuple build per method entry and the
`erlang:get/1` per access, which a per-access `ClassSelf`-derived key cannot avoid.

**Late-bound hierarchy walk.** Not recovered by this change, and not expected to be: `class_self_send_inherited_override`
touches no class variable, so the key binding does not appear in it (0.98, inside the run-to-run spread of
2142-2677). The +17% over main recorded under BT-3713 comes from the runtime path around the walk
(`class_self_send/4` plus the single-home invocation boundary), not from the inlined access lowering. It is
accepted here, and its owner is BT-3700 (late-binding guard and walk), as ADR 0130 already assigns.

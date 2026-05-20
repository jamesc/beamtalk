# ADR 0081: Class-Keyed ETS Table Topology

## Status
Accepted (2026-05-20)

## Context

The runtime keeps several named ETS tables that each hold one slice of "what we
know about class X". BT-2222 asked whether the six class-keyed tables should be
consolidated into a single per-class `class_metadata` row, with each current
owner module reading/writing one field of a shared row so module boundaries
survive.

The concern is **fragmentation by module-owner**: one class identity has state
spread across six tables, with cleanup fanning out across all six on class
destroy/reload. This ADR records the investigation, the read/write survey, the
result of spiking the suggested first step, and the decision.

### Survey of the six tables

Flags and key shapes are read from source, not assumed.

| Table | Type / flags | Key | Writers | Readers | Path |
|---|---|---|---|---|---|
| `beamtalk_class_pids` | `set`, `read_concurrency` | **`Pid`** | `init` (record), `terminate` (delete self), `restart_class` (match_delete) | `class_name_for_pid` | cold; crash-recovery only |
| `beamtalk_class_module` | `set`, `read_concurrency` | `ClassName` | `object_class` init / update / terminate | restart, supervisor superclass walk, stdlib, actor spawn, logging (×3), repl_ops_dev (×2) | warm read |
| `beamtalk_class_methods` | `set`, `read_concurrency` | `ClassName` | `object_class` init / update / terminate | dispatch `find_class_method_in_ancestors` | hot read |
| `beamtalk_class_hierarchy` | `set`, `read_concurrency` | `ClassName` | `object_class` init / update / terminate | dispatch, `inherits_from`, subclasses, return-type walk, supervisor, runtime_api foldl, compiler_server, repl_ops_dev | hottest read |
| `beamtalk_class_warnings` | **`bag`**, `write_concurrency` | **`{Package, ClassName}`** | `record_class_collision_warning` (only on module-mismatch collision) | drain (REPL load) | cold; off happy path |
| `beamtalk_pending_load_errors` | `set`, `write_concurrency` | `ClassName` | `record_pending_load_error` (only on stdlib-shadowing rejection) | drain (REPL load, 5 sites) | cold; off happy path |

Four structural facts fall out of the survey:

1. **Two concurrency profiles.** The four warm/hot tables are `read_concurrency`;
   the two cold tables are `write_concurrency`. A single merged table can honour
   only one profile and de-optimises the other path.
2. **Three key shapes.** Only `module`/`methods`/`hierarchy` are keyed by a bare
   `ClassName`. `pids` is keyed by `Pid` (a reverse index). `warnings` is keyed
   by the compound `{Package, ClassName}` **and is a `bag`** (many rows per
   class). A single `ClassName`-keyed row cannot represent the latter two.
3. **Three lifecycles.** `module`/`methods`/`hierarchy` are born in class `init`
   and die in `terminate` (class-lifetime). `pids` is born in `init` but
   **deliberately survives `terminate` on crash** to drive auto-restart — it
   outlives the class on purpose. `warnings`/`errors` are written on the
   *exceptional* load path and **drained (deleted) by the REPL independently of
   the class** — they can exist for a class name that never produced a live class
   (a rejected shadowing) and vanish while the class lives on. They are transient
   diagnostic mailboxes, not class state.
4. **One field is already shared on purpose.** `Module` is stored in both
   `beamtalk_class_module` and `beamtalk_class_methods` (BT-2008) so the dispatch
   chain walker resolves module + selectors in a single read.

## Decision

Reject the six-into-one consolidation. Specifically:

- **`pids`** — leave as-is. It is a `Pid`-keyed reverse index whose whole purpose
  is to survive class death; it is not class metadata and cannot share a
  `ClassName` row.
- **`warnings` + `pending_load_errors`** (the cold pair the issue suggested
  spiking first) — leave as-is. The spike (below) shows the merge is all cost and
  no benefit.
- **`module` + `methods` + `hierarchy`** (the hot trio) — the only genuinely
  cohesive class-lifetime state, written as a unit by the same process at the
  same three lifecycle points. Worth a **follow-up issue** (BT-2227) to merge
  into one `set` keyed by `ClassName`, gated on a hot-path perf sanity check.

Net: **(c) leave as-is** for the cold pair and `pids`; **(b) partial
consolidation in a follow-up** for the hot trio. This inverts the issue's
premise — the cold pair proposed as the safe first step is the *worst* merge
candidate, while the hot trio is the only one worth touching.

## Spike: consolidating the cold pair (`warnings` + `errors`)

A runtime benchmark of a change we reject would be wasted effort, so the spike is
a static read/write-path trace against the issue's three requested metrics.

- **Boilerplate removed:** merging the two removes one `ensure_*_table/0`
  (~23 lines) and one heir-registration site, but *adds* logic to reconcile a
  `bag` keyed by `{Package, ClassName}` (many rows/class) with a `set` keyed by
  `ClassName` (one row/class) under a single schema, plus read-modify-write on
  both insert and drain since the two would share a row. Net ≈ neutral-to-worse.
- **Lookup-count delta on a class-load + dispatch trace: zero.** Neither cold
  table is touched on the happy class-load path or on any dispatch path. Writes
  occur only on collision/shadowing; reads only on REPL drain. The headline pro —
  "cross-field reads in one ETS lookup" — does not apply to these tables at all.
- **Contention:** `warnings` uses `bag` + `write_concurrency` with per-package
  atomic `ets:insert`/`ets:take`. Collapsing to one `set` row per class replaces
  these with read-modify-write, introducing a lost-update window during
  concurrent multi-package loads where today there is none. Net: a regression.

## Alternatives Considered

### Full six-into-one `class_metadata` row
Rejected. Cannot represent the `Pid` reverse index or the `bag`-of-warnings
keyed by `{Package, ClassName}` in a single `ClassName` row; forces one
concurrency profile onto three different access patterns; merges three lifecycles
(class-lifetime, survives-crash, drain-on-read) that must stay independent.

### Cold-pair-first consolidation (the issue's suggested spike)
Rejected per the spike: zero hot-path benefit, neutral-to-worse boilerplate, and
a new contention window from bag→row read-modify-write.

### Hot-trio consolidation (`module` + `methods` + `hierarchy`)
Deferred to BT-2227, not done here. This is the real opportunity: same key, same
`read_concurrency` profile, same writer, same three lifecycle points, and it
removes the existing `Module` duplication. Deferred because it rewrites ~15 read
sites across three owner modules on the hottest read path (`inherits_from` runs
per exception match), so it needs its own change with a perf check, not a
drive-by.

## Consequences

### Positive
- The six tables keep distinct concurrency profiles, key shapes, and lifecycles —
  each tuned to its access pattern.
- The contention-free, atomic `bag` semantics of load diagnostics are preserved.
- The genuine smell (the hot trio) is captured as a scoped, gated follow-up.

### Negative
- Class `terminate` still fans out deletes across `hierarchy` + `module` +
  `methods` + `pids` until BT-2227 lands.
- Per-class state remains documented across modules rather than one schema.

### Neutral
- No code changes in this ADR; it is an investigation outcome.

## References
- Related issues: BT-2222 (this investigation), BT-2227 (hot-trio follow-up),
  BT-2008 (origin of the `module`/`methods` dispatch caches), BT-1888 (ETS heir
  survival pattern), BT-1768 (`pids` crash-recovery reverse index)
- Source: `beamtalk_class_registry.erl`, `beamtalk_class_module_table.erl`,
  `beamtalk_class_methods_table.erl`, `beamtalk_class_hierarchy_table.erl`,
  `beamtalk_object_class.erl`

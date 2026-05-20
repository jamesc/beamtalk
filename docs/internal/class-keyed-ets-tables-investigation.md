# Class-Keyed ETS Table Topology — Consolidation Investigation

**Issue:** [BT-2222](https://linear.app/beamtalk/issue/BT-2222)
**Status:** Decided — do not consolidate the cold pair or `pids`; hot trio tracked as [BT-2227](https://linear.app/beamtalk/issue/BT-2227)
**Follow-up:** [BT-2227](https://linear.app/beamtalk/issue/BT-2227)

## Question

Should the six class-keyed ETS tables be consolidated into a single per-class
`class_metadata` row, so cleanup is one delete instead of a six-table fan-out and
per-class state lives in one schema? BT-2222 asked specifically to spike the two
cold tables first (`beamtalk_class_warnings` + `beamtalk_pending_load_errors`).

## Survey of the six tables

Flags and key shapes read from source, not assumed.

| Table | Type / flags | Key | Writers | Readers | Path |
|---|---|---|---|---|---|
| `beamtalk_class_pids` | `set`, `read_concurrency` | **`Pid`** | `init` (record), `terminate` (delete self), `restart_class` (match_delete) | `class_name_for_pid` | cold; crash-recovery only |
| `beamtalk_class_module` | `set`, `read_concurrency` | `ClassName` | `object_class` init / update / terminate | restart, supervisor superclass walk, stdlib, actor spawn, logging (×3), repl_ops_dev (×2) | warm read |
| `beamtalk_class_methods` | `set`, `read_concurrency` | `ClassName` | `object_class` init / update / terminate | dispatch `find_class_method_in_ancestors` | hot read |
| `beamtalk_class_hierarchy` | `set`, `read_concurrency` | `ClassName` | `object_class` init / update / terminate | dispatch, `inherits_from`, subclasses, return-type walk, supervisor, runtime_api foldl, compiler_server, repl_ops_dev | hottest read |
| `beamtalk_class_warnings` | **`bag`**, `write_concurrency` | **`{Package, ClassName}`** | `record_class_collision_warning` (only on module-mismatch collision) | drain (REPL load) | cold; off happy path |
| `beamtalk_pending_load_errors` | `set`, `write_concurrency` | `ClassName` | `record_pending_load_error` (only on stdlib-shadowing rejection) | drain (REPL load, 5 sites) | cold; off happy path |

Four structural facts:

1. **Two concurrency profiles.** The four warm/hot tables are `read_concurrency`;
   the two cold tables are `write_concurrency`. A merged table honours only one
   and de-optimises the other path.
2. **Three key shapes.** Only `module`/`methods`/`hierarchy` are keyed by a bare
   `ClassName`. `pids` is `Pid`-keyed (a reverse index). `warnings` is keyed by
   the compound `{Package, ClassName}` **and is a `bag`** (many rows per class).
   A single `ClassName`-keyed row cannot represent the latter two.
3. **Three lifecycles.** `module`/`methods`/`hierarchy` are born in class `init`
   and die in `terminate` (class-lifetime). `pids` is born in `init` but
   **deliberately survives `terminate` on crash** to drive auto-restart. `warnings`/
   `errors` are written on the *exceptional* load path and **drained (deleted) by
   the REPL independently of the class** — they can exist for a class name that
   never produced a live class (a rejected shadowing) and vanish while the class
   lives on. They are transient diagnostic mailboxes, not class state.
4. **One field is already shared on purpose.** `Module` is stored in both
   `beamtalk_class_module` and `beamtalk_class_methods` (BT-2008) so the dispatch
   chain walker resolves module + selectors in a single read.

## Spike: consolidating the cold pair (`warnings` + `errors`)

A runtime benchmark of a change we reject would be wasted effort, so the spike is
a static read/write-path trace against the issue's three requested metrics.

- **Boilerplate removed:** merging removes one `ensure_*_table/0` (~23 lines) and
  one heir-registration site, but *adds* logic to reconcile a `bag` keyed by
  `{Package, ClassName}` (many rows/class) with a `set` keyed by `ClassName` (one
  row/class) under a single schema, plus read-modify-write on both insert and
  drain. Net ≈ neutral-to-worse.
- **Lookup-count delta on a class-load + dispatch trace: zero.** Neither cold
  table is touched on the happy class-load path or on any dispatch path. Writes
  occur only on collision/shadowing; reads only on REPL drain. The headline pro —
  "cross-field reads in one ETS lookup" — does not apply to these tables at all.
- **Contention:** `warnings` uses `bag` + `write_concurrency` with per-package
  atomic `ets:insert`/`ets:take`. Collapsing to one `set` row per class replaces
  these with read-modify-write, introducing a lost-update window during concurrent
  multi-package loads where today there is none. Net: a regression.

## Decision

Reject the six-into-one consolidation.

- **`pids`** — leave as-is. A `Pid`-keyed reverse index whose purpose is to
  survive class death; not class metadata, cannot share a `ClassName` row.
- **`warnings` + `pending_load_errors`** (the cold pair) — leave as-is. The spike
  shows the merge is all cost and no benefit.
- **`module` + `methods` + `hierarchy`** (the hot trio) — the only genuinely
  cohesive class-lifetime state: same key, same `read_concurrency` profile, same
  writer, same three lifecycle points (`init` `beamtalk_object_class.erl:415-420`,
  `update_class` `:1077-1083`, `terminate` `:792-794`), and it already duplicates
  the `Module` field. Worth a follow-up (BT-2227) to merge into one `set` keyed by
  `ClassName`, gated on a hot-path perf sanity check (the hierarchy walk in
  `inherits_from/2` runs per exception match).

Net: **leave the cold pair and `pids` as-is**; **partial consolidation of the hot
trio in a follow-up**. This inverts the issue's premise — the cold pair proposed
as the safe first step is the *worst* merge candidate, while the hot trio is the
only one worth touching.

## References

- Source: `beamtalk_class_registry.erl`, `beamtalk_class_module_table.erl`,
  `beamtalk_class_methods_table.erl`, `beamtalk_class_hierarchy_table.erl`,
  `beamtalk_object_class.erl`
- BT-2008 — origin of the `module`/`methods` dispatch caches
- BT-1888 — ETS heir survival pattern; BT-1768 — `pids` crash-recovery index

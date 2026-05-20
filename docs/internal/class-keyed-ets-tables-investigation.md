# Class-Keyed ETS Table Topology — Consolidation Investigation

**Issue:** [BT-2222](https://linear.app/beamtalk/issue/BT-2222)
**Status:** Implemented — cold pair + `pids` left as-is; the hot trio was merged into one `beamtalk_class_metadata` table in this change.

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
  writer, same three lifecycle points (`init`, `update_class`, `terminate`), and
  it already duplicated the `Module` field. **Merged** into one `set` keyed by
  `ClassName`.

Net: **left the cold pair and `pids` as-is**; **merged the hot trio**. This
inverts the issue's premise — the cold pair proposed as the safe first step is the
*worst* merge candidate, while the hot trio is the only one worth touching.

## Implementation

- New module `beamtalk_class_metadata` owns one `set` ETS table keyed by
  `ClassName`, with rows `#class_metadata{name, module, selectors, superclass}`
  and `{read_concurrency, true}` + the BT-1888 heir. `undefined` is the
  "field unset" sentinel, distinct from `none` (root class) and `[]` (no class
  methods), so a row carrying one field behaves like the old per-table absence of
  the others.
- The three former owner modules (`beamtalk_class_module_table`,
  `beamtalk_class_methods_table`, `beamtalk_class_hierarchy_table`) were deleted.
  Readers call `lookup_module/1`, `lookup_methods/1`, `lookup_superclass/1`,
  `match_subclasses/1`, `foldl/2`, `all_builtins/0`.
- `beamtalk_object_class` `init`/`update_class` now do a single full-row
  `insert/4` (atomic for readers; was three inserts), and `terminate` a single
  `delete/1` (was a three-table fan-out). `Module` is stored once.
- `beamtalk_class_registry:ensure_{hierarchy,module,methods}_table/0` are kept as
  aliases that all create the one table, so bootstrap and test call sites are
  unchanged.

## Performance

The merge widens the hierarchy row from a 2-tuple `{Class, Super}` to a 5-tuple
`#class_metadata{}`. A naive whole-row `ets:lookup/2` therefore copies more out of
ETS, which a micro-benchmark of `inherits_from/2` (the hottest reader — runs per
exception match) confirmed: ~+6–8% / ~7–9 ns per lookup.

The single-field reads (`lookup_superclass/1`, `lookup_module/1`) instead use
`ets:lookup_element/4` (OTP 26+), which copies only the requested element and uses
a default to fold the table-absent / key-absent / field-unset cases into one
return. That more than recovers the regression — it beats the old 2-tuple table,
because it never materialises the full row:

| `inherits_from/2` path | old 2-tuple | new whole-row | new `lookup_element` |
| -- | -- | -- | -- |
| single hop (1 lookup) | 107 ns | 128 ns (+20%) | **92 ns (-14%)** |
| full chain (8 lookups, miss) | 882 ns | 952 ns (+8%) | **792 ns (-10%)** |

`lookup_methods/1` still reads the whole row (it needs two fields), but it is off
the per-exception path. Benchmark: `/tmp/ets_bench.escript` (synthetic, isolates
the row-shape change on one build; throwaway, not committed).

## References

- Source: `beamtalk_class_metadata.erl` (new), `beamtalk_class_registry.erl`,
  `beamtalk_object_class.erl`, `beamtalk_class_dispatch.erl`, `beamtalk_supervisor.erl`
- BT-2008 — origin of the `module`/`methods` dispatch caches
- BT-1888 — ETS heir survival pattern; BT-1768 — `pids` crash-recovery index

# ADR 0150: Actor Registry Off the Start Path — Insert, Then Monitor

## Status
Proposed (2026-11-15). Issue BT-4854; the rest of the start path is BT-4932.

Amends [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md),
[ADR 0009](0009-otp-application-structure.md),
[ADR 0069](0069-actor-observability-and-tracing.md),
[ADR 0093](0093-announcements-event-substrate.md),
[ADR 0123](0123-versioned-state-migration.md),
[ADR 0126](0126-distribution-location-transparent-actors.md) and
[ADR 0129](0129-class-side-system-facades.md), and ADR-5 of
`docs/beamtalk-ddd-model.md`. The amendments to those documents are made by
the implementation PR, after this ADR is accepted (see
[Consequences for earlier ADRs](#consequences-for-earlier-adrs)).

## Context

### Problem

Two singletons sit on every actor start:

- `beamtalk_object_instances` (`allInstances`, hot reload, `class_of/1`). The
  generated `spawn`, `spawn_monitored` and `spawn_monitored_async` call its
  `register/2` — a synchronous `gen_server:call` — from the *spawning* process.
  The server monitors the pid and writes two ETS tables.
- `beamtalk_actor_registry` (`beamtalk_repl_actors`; `Node current actors`, the
  REPL, IDE and LiveView actor lists). Each actor's own `init/1` emits
  `[beamtalk, actor, lifecycle, start]`; `maybe_announce_lifecycle/2` in
  `beamtalk_actor.erl` turns that into a `track_spawned/2` cast, and the server
  calls the class process from its handler and monitors the pid.

Both are single processes, so every start in the node queues behind them. Phase 1
of BT-4854 measured what this costs and where the rest of the cost sits. The
numbers are from a cloud VM (Firecracker, 4 cores, OTP 28.5), so they compare
only with each other and not with the maintainer's M5 Max numbers (rows 16a–16d
in the issue). `+S 1` is stable and measures total CPU; `+S 4` is noisy (16a/16c
varied by up to 40% between identical runs) and is shown as a median.

| Per actor, µs | 16a start→run→stop `+S4` / `+S1` | 16c plain spawn + stop `+S4` / `+S1` | 16d, 100 in flight `+S4` / `+S1` |
|---|---|---|---|
| bare floor | 1.05 (`spawn_monitor` + DOWN) | 3.5 (`gen_server` start + stop) | 2.0 |
| baseline (today) | 31.9–36.3 / 10.5–10.9 | 14.3–14.4 / 11.6–11.8 | 14.6–15.0 / 10.6–11.0 |
| ets-reaper | 11.6–11.8 / 8.0–8.3 | 10.5–10.7 / 8.1 | 6.9–8.5 / 7.9–8.6 |
| lazy-batch | 11.7 / 8.3 | 10.7 / 8.4 | 6.9 / 8.1 |
| **insert-then-monitor (chosen)** | **12.7 / 8.9** | **11.8 / 9.4** | **9.8 / 9.1** |
| label-scan (no table) | 11.5 / 7.4 | 9.7 / 7.7 | 4.9 / 7.2 |
| sharded (4 servers) | 38.7 / 10.3 | 36.8 / 11.4 | 17.4 / 10.5 |
| partition-link | 14.0–42.6 / 8.7 | 19–23 / 10.5 | 8.3 / 8.5 |

What the profile says:

- **The cost is cross-scheduler, not CPU.** At `+S 1` async and sync start cost
  the same (10.5 vs 11.8 µs). At `+S 4`, 16a is 30× the floor: the `track_spawned`
  cast saves about 10 µs of wall time for 1.4 µs of CPU, which is wake-ups and
  contention on the two singletons. The cloud VM probably exaggerates this
  compared with the M5 Max.
- **The registries are about a third of the start cost at `+S 1`** (~3.5 µs of
  ~10.9 µs: `register` call, `track` cast, two monitors, `DOWN` handling). Lifecycle
  announce and telemetry emit are ~2.0 µs, bare `spawn_monitor` ~0.9 µs, and the
  remaining ~4.5 µs is `proc_lib` setup, `init`, `continueWith:`/`stopWith:`
  dispatch and the harness. **No registry design reaches 2–3× of `spawn_monitor`.**
  That target belongs to BT-4932 and is not part of this ADR.
- **The synchronous call is also the flow control.** BT-4845 made `register/2`
  caller-side ETS writes plus a cast; with the call gone, the registry mailbox
  and dead rows grew without bound and buccaneer fell from 133.6k to about 7k
  req/s. BT-4842 restored the call. Baseline open loop (16 spawners, 3 s, async):
  `beamtalk_object_instances` mailbox 24, `beamtalk_actor_registry` mailbox
  **1972**. Any design that removes the call has to bound its own queues.

### Current state

`beamtalk_object_instances` keeps two ETS tables, owned by the server and with
`beamtalk_ets_heir` as heir: `beamtalk_instance_registry` (`ordered_set`, keyed
`{Class, Pid}`) and `beamtalk_instance_class` (`set`, `{Pid, Class}`, for O(1)
`class_of/1`). `beamtalk_actor_registry` keeps its own map of pid → metadata
(`pid`, `class`, `module`, `spawned_at`).

**The two registries do not hold the same set.** Verified in the code:

- `beamtalk_object_instances:register/2` is called only from the generated
  `spawn`, `spawn_monitored` and `spawn_monitored_async`
  (`gen_server/spawn.rs`, `gen_server/native_facade.rs`), through
  `instance_registration_doc`. Nothing in `runtime/` calls it.
- `safe_spawn_linked/2` and `safe_spawn_named/5` (supervised children,
  `spawnAs:`, `withName:` children) call `gen_server:start_link(Module, …)`
  directly, so they never run the generated `spawn` and are **not** in
  `beamtalk_object_instances`. A `native:` backing module started by
  `safe_start_native` is not either. Supervised children started through a
  `withClassMethod:` factory *are*, because the factory calls `self spawn`.
- `beamtalk_actor_registry` is fed from the actor's own `init/1`
  (`beamtalk_actor.erl`, the lifecycle-start telemetry), so it holds every actor
  whose generated `init/1` ran, including supervised and named children.

So `allInstances` (and hot reload, which uses it) misses supervised and named
actors today, while `Node current actors` lists them.

### Readers

Phase 1 listed every reader. What each one needs:

| Reader | Needs |
|---|---|
| `Node current actors` / `actorsOf:` / `actorAt:` (`beamtalk_node.erl` `local_actors/0`, `local_actor_at/1`; erpc targets) | Exact, unordered. Visible when `spawn` returns. Pinned by `node_test.bt`, `workspace_actors.btscript`, `dynamic_supervisor_children_test.bt`. |
| `stop_class_actors` (`removeFromSystem`), `should_purge_module` | Exact. A missed actor keeps a purged module or has its code purged. |
| Hot reload (`beamtalk_repl_loader_activate`, via `beamtalk_runtime_api:all_instances/1`) | Exact for actors alive at reload. A missed actor silently keeps old code and state shape. |
| REPL `inspect` / `kill` / `pid-stats` (`is_known_actor`) | Exact within about a second of spawn. |
| REPL / CLI / MCP / VS Code actor lists, `actor_snapshot_frames` (LiveView) | Eventual, but `:actors` right after a spawn must show it, and a snapshot must leave no gap with the pushes. |
| `trace_store` bottlenecks (raw ETS fold of `beamtalk_instance_registry`), `actor_health` | Eventual, unordered; missing actors are skipped. |
| `lookup_class` → `class_of/1` on every pid-based send (`beamtalk_actor.erl`) | Hot path, O(1) ETS lookup. A missing row gives class `unknown`, and `sync_send_remote` then uses the 5 s timeout instead of `infinity` for `TimeoutProxy`. |
| `count_actors_for_module` (`list-classes`), `workspace_meta` actor count | Eventual. |

Nothing in stdlib reads `allInstances` or `instanceCount`.

## Decision

**Insert first, monitor later, in one table set.** Registration stops being a
call to a server. The caller inserts its rows immediately; a batched pass in
the registry server monitors the new rows whose actors are still alive and
deletes the rows of those that are already dead. After that, death is reported
by the runtime (`'DOWN'`), and the live table is never swept.

### 1. Registration is an insert in the caller

`beamtalk_object_instances:register(Class, Pid)` becomes, in the calling
process:

1. Insert the row `{{Class, Pid}, Token, …}` into `beamtalk_instance_registry`
   and `{Pid, Class, Token}` into `beamtalk_instance_class`. `Token` is a
   fresh `erlang:unique_integer([monotonic])`. The row also carries what
   `beamtalk_actor_registry` keeps today (`module`, `spawned_at`).
2. Add `Pid` to the pending table and increment the pending counter (an
   `atomics` counter).
3. If `Pid` is not alive, delete the rows it just inserted (the caller can win
   the race against a short-lived actor, as seen in about 1% of async starts).
4. If the counter is above the bound, apply back-pressure (below). Otherwise
   return `ok`.

The order is rows first, then pending, so a pass never sees a pending pid
without rows. A caller killed between 1 and 2 leaves rows that nothing
monitors; the backstop reaper (§4) covers that.

When an actor becomes visible does **not change**: the row exists before
`spawn` returns, and in the async case before `init/1` has run, exactly as
today.

### 2. One source of truth

`beamtalk_actor_registry` stops being a separate store. Its API
(`list_actors/1`, `get_actor/2`, `count_actors_for_module/2`, `list_objects/0`,
`object_at/1`, `track_spawned/2`) becomes views over the same tables, and the
`track_spawned` cast is removed. The class-process call in its handler is
gone with it (its job — resolving `module` — is done once, by the registering
caller, from data it already has).

The actor's own `init/1` (via `maybe_announce_lifecycle`) also does the same
insert, idempotently (same `{Class, Pid}` key; the earlier caller-side row wins
and is kept, so `spawned_at` is not rewritten). That is what makes the table
cover **every** start path.

**What newly becomes visible to `allInstances`, hot reload and the IDE lists:**
the actors that today are in `beamtalk_actor_registry` but not in
`beamtalk_object_instances`: supervised and named children
(`safe_spawn_linked/2`, `safe_spawn_named/5`, `start_actor_child/2,3`) and
`native:` backing-module actors if their `init/1` runs the lifecycle event
(**to be verified by the implementation**, which must test each start path in
the table in `beamtalk_actor.erl`'s Spawn Architecture). Consequences:

- `Foo allInstances` and `Foo instanceCount` include supervised children of
  `Foo`.
- Hot reload (`loader_activate`) now migrates supervised actors too. Today it
  silently skips them. This is a bug fix, but it is an observable change and
  goes in the CHANGELOG.
- `stop_class_actors` and `should_purge_module` see them, so `removeFromSystem`
  and purge stop supervised children of the class, and the supervisor restarts
  them according to its strategy. This is the behaviour users expect, but it
  has not happened before.
- Nothing is removed from any list: every row either registry had stays.

### 3. Cleanup by batched monitor

The registry server drains the pending table every 50 ms, or sooner when a
"kick" message arrives (sent by the caller that moves the counter from 0 to 1,
so the mailbox holds at most one kick). For each pending pid it checks
`is_process_alive/1`: a live pid is monitored (one monitor per pid, kept in a
`pid → {Ref, Token}` map); a dead pid has its rows deleted with
`ets:delete_object/2` on the exact row. It then handles `'DOWN'` for the
monitored pids by deleting the same exact rows.

A clean stop also deletes its own rows from `terminate/2`, so the `'DOWN'` for
it finds nothing to do. That is an optimisation, not a correctness rule.

**Stale window.** A row can outlive its actor by at most the interval between
the actor's death and the next pass (50 ms by default) if it died before being
monitored, and by the `'DOWN'` delivery lag (µs to ms) after. Enumerating
readers (`all/1`, `count/1`, the list views, `object_at/1`) filter with
`is_process_alive/1`, so the window is not visible to them. `class_of/1` and the
raw `trace_store` fold do not filter; a dead pid's class is harmless there.

### 4. Bounded pending buffer, and the backstop reaper

**The bound.** The pending counter is limited to `PendingMax` (initial value
8192, set from the open-loop test in the implementation and kept in
`persistent_term` / application env). At the bound, `register/2` does a
`gen_server:call` that makes the server drain the pending table, then returns.
So above the bound, registration is the synchronous call it is today and
spawners are paced by the server again, but only until the buffer is back
below the bound. Below the bound nothing waits. The kick mailbox holds at most
one message, the `'DOWN'` messages are bounded by the live monitored pids, and
the pending table is bounded by `PendingMax` plus the number of callers
in flight. Memory is bounded by live actors plus the dead rows of actors that
died before their monitor, i.e. ≤ one drain period of kills.

If the server is down (restarting), the call fails; the caller ignores the
failure (the codegen already wraps `register` in a `try`), so the bound is soft
for the length of a restart.

**The backstop reaper.** A low-frequency pass for the rows nothing monitors:
rows from a caller killed between steps 1 and 2, rows inserted while the
server was restarting, and rows whose `'DOWN'` was lost with a crashed server.
It is triggered by:

- a timer, with period `max(30 s, 20 × the last pass time)`, so a pass costs at
  most about 5% of a core (one pass costs ~0.3 µs per row: 26.8 ms at 100k rows,
  346 ms at 1M);
- a server restart (a full pass at `init/1`, §5).

The pass walks the pid index in chunks (`ets:select` with a continuation, 1000
rows, yielding between chunks), skips every pid that is in the monitor map,
and for the rest either monitors it (alive) or deletes its rows (dead). It is a
backstop: the primary cleanup is the monitor, and in steady state the pass
finds nothing.

### 5. Pid-reuse guard and owner crash

**Pid reuse.** A pid can be reused after about 2^28 spawns. Every row carries
the registration `Token`, the monitor map records the token it monitored, and
every delete (`'DOWN'`, pass, reaper, `terminate/2`) is an
`ets:delete_object/2` of the exact `{Key, Token, …}` row. A late `'DOWN'` for an
old incarnation cannot delete the row the new incarnation inserted, because the
tokens differ. A `'DOWN'` whose monitor ref is not the current one for that pid
is ignored.

**Owner crash.** Both tables, and the pending table, name `beamtalk_ets_heir`
as heir, as today. When the supervisor restarts the server, `init/1` reclaims
the tables and runs the same pass as the reaper over every row (monitor the
live ones, delete the dead ones). Callers keep inserting while the server is
down; their rows are picked up by that pass. If the heir dies too and the
tables come back empty, `init/1` rebuilds them by a **label scan** (§6) instead
of leaving running actors unregistered, as `beamtalk_object_instances`
documents today.

### 6. The label scan, and who uses it

Every actor already writes `'$beamtalk_actor'` (its class) and
`'$beamtalk_actor_started_at'` into its process dictionary in `init/1`. A
reader can enumerate actors with `erlang:processes_iterator/0` and
`process_info(Pid, {dictionary, '$beamtalk_actor'})`, with no table and no
start-path write. Its cost is about **2.5–4.5 µs per process in the node**,
whatever the fraction that are actors:

| live processes | one enumeration (`+S 4`) |
|---|---|
| 1k | 3–6 ms |
| 100k | 0.21–0.30 s |
| 1M | 2.6–4.6 s (two concurrent scans slow each other: 4.9–5.7 s each) |

A scan is not a consistent snapshot, it signals every target process, and it
scales with all processes. It is used only by readers that are rare and
exact-at-enumeration:

- hot reload (`loader_activate`), `stop_class_actors`, `should_purge_module`:
  they need every live actor of a class and run at most a few times a session;
  the scan sees actors that were never registered;
- the rebuild of the tables after the heir is lost (§5).

Everything else keeps the table: `class_of/1` and `lookup_class` (hot path,
0.7 µs vs 1.6 µs plus a signal), `allInstances`/`instanceCount` by class
(O(class rows), not O(all processes)), `Node current actors`/`actorAt:`
(erpc targets, polled by tooling), the REPL, IDE and LiveView lists and
`actor_snapshot_frames` (need a consistent snapshot and metadata), the
`trace_store` bottlenecks, `count_actors_for_module` and `workspace_meta`.

### 7. What changes observably

- **Registration no longer paces spawners** below the bound. A spawn flood
  queues runnable actors, as a bare `spawn_monitor` flood does (120k–700k
  runnable actors in the open loop). That is process backlog, not registry
  growth. The old pacing comes back only above `PendingMax`.
- **`allInstances` and the IDE lists** include supervised and named children
  (§2). The set is otherwise unchanged.
- **Dead rows** are removed by monitor, with the stale window in §3, and
  readers filter by liveness.
- `Node current actors` and the REPL lists are views over the one table:
  `beamtalk_actor_registry`'s own state, its cast and its monitor per actor are
  gone.
- `sustained_concurrent_register_keeps_up_test_` pins "register must be a
  synchronous call". It is replaced by an open-loop test (many spawners, never
  draining) asserting bounded registry mailboxes, bounded pending size and
  bounded dead rows, under clean stops and `exit(kill)` floods. It asserts
  rows and mailboxes, not process count.

## Prior Art

- **Elixir `Registry`:** partitioned ETS tables; registered processes *link* to
  the partition process, and the partition deletes rows on `EXIT`. Death is
  reported by the runtime, which is the property we keep, but with monitors
  instead of links (see Alternatives).
- **OTP `pg` and `global`:** monitor-driven membership with a registrar process.
  Both put a process on the registration path, which is what this ADR removes.
- **OTP `proc_lib` labels (OTP 27) and `processes_iterator` (OTP 28):** make an
  enumeration of labelled processes possible without a registry. Idiomatic for
  identification, not for lookup.
- **Smalltalk `allInstances`:** a heap scan. BEAM has per-process heaps, so
  ADR 0005 chose explicit tracking; this ADR keeps that and uses the scan
  only where Smalltalk would also scan: rarely, on a human's request.

## User Impact

- **Newcomer / Smalltalk developer:** `Foo allInstances` now includes
  supervised instances of `Foo`; it matches what `Node current actors` already
  showed. Nothing to learn.
- **Erlang/Elixir developer:** the registry is ETS plus monitors, with a
  bounded buffer — the shape they expect. `process_info(links)` is unchanged
  (no registry link, unlike the partition design).
- **Operator:** actor start is cheaper under concurrency and no longer
  funnels through two singletons. The cost: a spawn flood now builds a
  runnable queue instead of being slowed by the registry, and a hot reload
  or purge on a node with ~1M processes costs seconds (§6).
- **Tooling developer:** the IDE and LiveView lists read the same table as
  `allInstances`, so there is one answer. A row can be stale for up to 50 ms
  after a death; the readers filter, and the tooling sees no change.

## Steelman Analysis

| Option | Strongest case |
|---|---|
| Insert-then-monitor (chosen) | **BEAM veteran:** death is reported by the runtime and the live table is never swept; the idiom of every registry in OTP. **Operator:** visibility timing is unchanged, so nothing that works today moves. |
| Periodic reaper as primary | **Language designer:** nothing on the start path at all and no monitor per actor; the simplest to reason about. **Operator:** the lowest start cost (8.0 vs 8.9 µs at `+S 1`). |
| Partition processes with links | **BEAM veteran:** it is the Elixir `Registry` design, so it is a known quantity. |
| Label scan alone | **Smalltalk purist:** it is `allInstances` as Smalltalk does it. **Language designer:** no state to keep consistent; the table cannot disagree with reality. |
| Lazy batch | **Operator:** the same cost as the reaper with monitor-driven cleanup. |

Tension: the reaper is cheapest at start and the label scan has no state at
all, but both cost the readers or the operator at scale; the monitor-driven
design costs about 1 µs of CPU per start at `+S 1` (8.9 vs 8.0 µs) and keeps
every reader's behaviour.

## Alternatives Considered

### Periodic reaper as the primary cleanup (`research/bt-4854-ets-reaper`)
Caller-side inserts; no monitor per actor; a reaper walks the pid index and
deletes the rows of dead actors. Fastest registry-with-table design (16a
11.8 / 8.3 µs). Rejected as the *primary* mechanism: a pass walks every live
row, at 346 ms per pass at 1M rows (26.8 ms at 100k), so a fixed 250 ms period
costs more than a core at about 1M actors and the period has to grow with the
table, which pushes the stale window to seconds at that scale; dead rows peak
at kills × period (63k with `exit(kill)` in the open loop); and a periodic sweep
of live state is not the OTP convention, where death is reported by the
runtime. Kept as a low-frequency backstop (§4).

### Partition processes with links (`research/bt-4854-partition-link`)
The Elixir `Registry` model: actors link to a partition process that deletes
rows on `EXIT`. Rejected: the link is two-way, so a partition crash kills its
actors. Measured: `exit(Partition, kill)` killed **143 of 400** live actors.
Also: the link appears in `process_info(links)` and, for a trapping actor, as an
`{'EXIT', Partition, _}` to `onLinkedExit:reason:`; start cost was unstable at
`+S 4` (14.0–42.6 µs for 16a), because link and unlink are signals to another
process; and the actor must link from its own `init/1`, which leaves a window
between `init` and the link that needs a sweep anyway. No BIF sets a monitor
on behalf of another process, so a "monitor with no message at start" variant
does not exist either.

### Sharded registries (`research/bt-4854-sharded`)
One server per scheduler, routed by pid. No gain: 16a 38.7 µs at `+S 4`,
because the synchronous round trip stays on the path and sharding only spreads
the queue.

### Lazy batch (`research/bt-4854-lazy-batch`)
Writes go to a pending buffer; the server drains on a timer and before any
enumeration. Same start cost as the reaper, but readers need a flush and rows
are invisible until the drain. Under an async flood the buffer reached 680k
rows (168 MB). Insert-then-monitor is this design with the rows inserted
at once, so the flush disappears; the bound in §4 closes the same buffer
problem.

### Label scan as the registry (`research/bt-4854-label-scan`)
No tables; readers scan `processes_iterator`. Cheapest start (16a 7.4 µs at
`+S 1`, about the registry-free floor of 7.0). Rejected as the registry:
2.6–4.6 s per enumeration at 1M processes; it cannot serve `class_of/1` on
every send (1.6 µs plus a signal to the target, against 0.7 µs); it is not a
consistent snapshot; and the cost is per process in the node, not per actor of
the class. Adopted for the rare readers in §6.

### Register only when a workspace is attached
Not prototyped. It breaks `Node current actors` and hot reload under plain
`beamtalk run` and `beamtalk test`, and adds a mode with different meaning
in different boot contexts, which `docs/beamtalk-principles.md` §4 rules out.

## Consequences

### Positive
- Both singletons are off the start path: no call, no cast, no class-process
  call per actor. 16a/16c/16d improve by 2.9×/1.2×/1.5× at `+S 4` and 17–19% at
  `+S 1` in the insert-then-monitor prototype (same VM, medians, `+S 4` noisy).
- One table, one source of truth; `allInstances`, hot reload and purge stop
  missing supervised and named actors.
- Mailboxes and dead rows are bounded by construction, and tested in CI.
- Visibility timing is unchanged.

### Negative
- The 2–3× target of BT-4854 is not met by this change; the rest of the start
  path (~7 µs) is BT-4932.
- A bounded buffer, a drain pass, a reaper and a rebuild are four mechanisms
  to test and maintain, where the baseline had one call.
- Above `PendingMax` registration paces spawners again, and below it a flood
  can leave a large runnable backlog.
- The label scan makes hot reload and purge cost seconds on a node with ~1M
  processes.
- Supervised actors becoming visible to `allInstances`/hot reload/purge is a
  behaviour change that existing programs may notice.

### Neutral
- `beamtalk_actor_registry` keeps its name and API.
- The pid-keyed `beamtalk_instance_class` table stays.
- Nothing in stdlib depends on `allInstances`.

## Implementation

Phase 2 of BT-4854, one PR, after this ADR is accepted:

1. `beamtalk_object_instances`: caller-side insert, pending table and counter,
   drain pass, monitor map with tokens, bound, reaper, restart pass, scan
   rebuild. Rows carry `module` and `spawned_at`.
2. `beamtalk_repl_actors`: views over the tables; `track_spawned/2` becomes
   the idempotent insert; the cast and per-actor monitor are removed. In
   `beamtalk_actor.erl` the change is limited to `maybe_announce_lifecycle/2`
   and the registration calls.
3. `loader_activate`, `stop_class_actors`, `should_purge_module`: use the scan
   reader.
4. Tests: open-loop CI test; the reader table above; pid-reuse guard;
   owner-crash recovery (kill the server, kill server and heir); every start
   path in the Spawn Architecture table; the existing EUnit, stdlib, BUnit,
   workspace and LiveView suites.
5. Docs: amendments to the ADRs below, `docs/beamtalk-ddd-model.md` ADR-5,
   CHANGELOG.
6. Measure rows 16a/16c/16d before and after on the same machine. The numbers
   in this ADR are from a 4-core cloud VM; the implementation PR lists what to
   re-measure on the maintainer's machine at `+S 18`.

Follow-up issue (filed, not done here): `beamtalk-buccaneer` makes HTTP/2
streams actors again, with 133.6k req/s as the figure to beat.

### Consequences for earlier ADRs

| ADR | Amendment made by the implementation PR |
|---|---|
| [0005](0005-beam-object-model-pragmatic-hybrid.md) | "`allInstances` tracks actors via ETS" and "process monitors" stay true; add that cleanup is by batched monitor, rows are inserted by the caller, and supervised and named actors are included. |
| [0009](0009-otp-application-structure.md) | Its coupling problem (core runtime calling `beamtalk_repl_actors:register_actor/4`) is now a table insert; note `beamtalk_repl_actors` is a view over `beamtalk_object_instances`' tables. |
| [0069](0069-actor-observability-and-tracing.md) | The class-name lookup description is stale (a bag, `ets:match` by pid). Replace with `class_of/1` on the pid-keyed table, and note that a row may exist up to 50 ms after death. |
| [0093](0093-announcements-event-substrate.md) | `ActorSpawned` / `ActorStopped` are unchanged; only the tracking that `maybe_announce_lifecycle` also did moves. |
| [0123](0123-versioned-state-migration.md) | Hot-reload migration enumerates through the scan reader, so it now includes supervised and named actors. |
| [0126](0126-distribution-location-transparent-actors.md) | `local_actors/0` and `local_actor_at/1` read the one table; erpc behaviour is unchanged. Update the `unknown` class note for a missing row. |
| [0129](0129-class-side-system-facades.md) | `Node current actors` is "backed by `beamtalk_actor_registry`": now a view over the instance tables; the lifecycle-start tracking is an idempotent insert. |
| `docs/beamtalk-ddd-model.md` ADR-5 | "ETS table with process monitors": now rows inserted by the caller and monitored in batches, with a reaper backstop. |

## References
- Related issues: BT-4854, BT-4932 (rest of the start path), BT-4845, BT-4842,
  BT-4871
- Related ADRs: 0005, 0009, 0069, 0093, 0123, 0126, 0129, 0079, 0139
- Research branches: `research/bt-4854-harness` (bench and open-loop tooling,
  `benchmarks/vs-elixir/research-bt4854/`), `-insert-then-monitor`,
  `-ets-reaper`, `-label-scan`, `-partition-link`, `-lazy-batch`, `-sharded`
- Measurements: BT-4854 phase 1 report and addendum (Linear comments, 2026-11-15)

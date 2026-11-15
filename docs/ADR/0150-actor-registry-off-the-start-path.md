# ADR 0150: Actor Registration Off the Start Path

## Status
Proposed (2026-11-15; revised the same day after owner review, which sent the first draft back). Issue BT-4854; the rest of the start path is BT-4932.

> **Revision note.** The first draft decided "insert, then monitor" with a table, a bounded pending buffer, a drain pass, a backstop reaper and a scan rebuild, and dismissed a no-table design in six lines. This revision puts four options side by side, corrects the facts the first draft got wrong, and **recommends the no-table design (D)**, with the minimal table design (C) as the fallback if one of the acceptance conditions below fails. Nothing in this ADR is implemented. The WIP prototype of the first draft is on `bt-4854-registry-impl`; it is evidence, not a decision.

Amends, if accepted: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md), [0009](0009-otp-application-structure.md), [0069](0069-actor-observability-and-tracing.md), [0079](0079-named-actor-registration.md), [0093](0093-announcements-event-substrate.md), [0123](0123-versioned-state-migration.md), [0126](0126-distribution-location-transparent-actors.md), [0129](0129-class-side-system-facades.md) and ADR-5 of `docs/beamtalk-ddd-model.md` (see [Consequences for earlier ADRs](#consequences-for-earlier-adrs)). Those documents are edited by the implementation PR, after acceptance.

## Context

### Problem

Every actor start passes through up to **three** registrations:

1. `beamtalk_object_instances` (a gen_server and two ETS tables: `allInstances`, hot reload, `class_of/1`). The generated `spawn`, `spawn_monitored` and `spawn_monitored_async` (and the native facade's `spawn`) call its `register/2`, a synchronous `gen_server:call`, from the *spawning* process (`gen_server/spawn.rs`, `instance_registration_doc`).
2. The node-wide actor list. Naming matters here, because two different things are called "actor registry":
   - the process **registered locally as `beamtalk_actor_registry`** is `beamtalk_repl_actors` (`start_link(registered)`): the actor list behind `Node current actors`, the REPL, IDE and LiveView lists, `stop_class_actors` and `should_purge_module`. Each compiled actor's generated `init/1` emits `[beamtalk, actor, lifecycle, start]`; `maybe_announce_lifecycle/2` in `beamtalk_actor.erl` turns it into a `track_spawned/2` cast; the server resolves the module with a call to the class process and monitors the pid;
   - the **module `beamtalk_actor_registry.erl`** is the ADR 0079 *name* registry (`register_name/2`, `all_registered/0`, `actor_started_at/1`). It is not part of this change. Below, "the actor list" means the first, and "the name registry" the second.
3. On **REPL-originated spawns** only, generated code calls `beamtalk_actor:register_spawned/4` (`dispatch_codegen/spawn.rs`) → `on_actor_spawned/4` → `register_actor/4`, a second synchronous call to the actor list, plus the `actor_spawned_hook` that feeds `beamtalk_workspace_meta`. It adds the module name (the lifecycle path resolves it anyway) and the hook.

The two servers are singletons, so every start on the node queues behind them. Phase 1 of BT-4854 measured the cost and where the rest of the cost sits. The numbers are from a cloud VM (Firecracker, 4 cores, OTP 28.5), so **they compare only with each other**, not with the maintainer's M5 Max numbers (rows 16a–16d in the issue). `+S 1` is stable and measures total CPU; `+S 4` is noisy (16a/16c varied by up to 40% between identical runs) and is shown as a median.

| Per actor, µs | 16a start→run→stop `+S4` / `+S1` | 16c plain spawn + stop `+S4` / `+S1` | 16d, 100 in flight `+S4` / `+S1` |
|---|---|---|---|
| bare floor | 1.05 (`spawn_monitor` + DOWN) | 3.5 (`gen_server` start + stop) | 2.0 |
| baseline (today) | 31.9–36.3 / 10.5–10.9 | 14.3–14.4 / 11.6–11.8 | 14.6–15.0 / 10.6–11.0 |
| ets-reaper | 11.6–11.8 / 8.0–8.3 | 10.5–10.7 / 8.1 | 6.9–8.5 / 7.9–8.6 |
| lazy-batch | 11.7 / 8.3 | 10.7 / 8.4 | 6.9 / 8.1 |
| insert-then-monitor | 12.7 / 8.9 | 11.8 / 9.4 | 9.8 / 9.1 |
| **label-scan (no table)** | **11.5 / 7.4** | **9.7 / 7.7** | **4.9 / 7.2** |
| sharded (4 servers) | 38.7 / 10.3 | 36.8 / 11.4 | 17.4 / 10.5 |
| partition-link | 14.0–42.6 / 8.7 | 19–23 / 10.5 | 8.3 / 8.5 |

What the profile says:

- **The cost is cross-scheduler, not CPU.** At `+S 1` async and sync start cost the same (10.5 vs 11.8 µs). At `+S 4`, 16a is 30× the floor: removing the `track_spawned` cast saved about 10 µs of wall time for 1.4 µs of CPU, which is wake-ups and contention on the singletons. A VM probably exaggerates this compared with the M5 Max.
- **The registrations are about a third of the start cost at `+S 1`** (~3.5 µs of ~10.9 µs). Lifecycle announce and telemetry emit are ~2.0 µs, bare `spawn_monitor` ~0.9 µs, and the remaining ~4.5 µs is `proc_lib` setup, `init`, dispatch and the harness. **No registry design reaches 2–3× of `spawn_monitor`.** The registry-free floor is 7.0 µs at `+S 1`. The rest is BT-4932.
- **The synchronous call is also the flow control.** BT-4845 made `register/2` caller-side ETS writes plus a cast; with the call gone, the registry mailbox and dead rows grew without bound and buccaneer fell from 133.6k to about 7k req/s. BT-4842 restored the call. Baseline open loop (16 spawners, 3 s, async): `beamtalk_object_instances` mailbox 24, the actor list's mailbox **1972**. Any design that keeps a queue-fed registry has to bound it.
- **A table moves contention from a mailbox to ETS; the measurements here cannot show that.** Every table design adds caller-side writes on every scheduler (about four per spawn once the actor's own `init/1` registers too), and a shared counter. The prototype numbers above come from a 4-core VM, cover the caller-side inserts of the *first* row shape only (not the `init/1` insert nor merged columns), and so say little about 18 schedulers. A design with no shared write on the start path has no such risk.

### Current state, corrected

- **Who is in which registry.** `register/2` of `beamtalk_object_instances` is called only from generated `spawn*` (and the native facade's). `safe_spawn_linked/2` and `safe_spawn_named/5` (supervised children, `spawnAs:`, `withName:`) call `gen_server:start_link` directly and are **not** in `beamtalk_object_instances`. An unnamed supervised child started through a `withClassMethod:` factory *is* (the factory calls `self spawn`). The actor list holds every actor whose generated `init/1` emits the lifecycle event (compiled actors, supervised and named included), and `native:` actors only when REPL-spawned. So `allInstances` and hot reload miss plain supervised and named children today; `Node current actors`, `stop_class_actors` and `should_purge_module` do not.
- **Who carries the process-dictionary markers.** `'$beamtalk_actor'` (value: the class) is written by *generated* `init/1` code (`beamtalk_actor_marker_doc`, `gen_server/callbacks.rs`), at the top of `init/1`, **except** when `init/1` is called with `__skip_initialize__ => true` (a subclass's `init/1` using the parent's as a state builder; hot reload and shape migration also call it that way in the *calling* process, which must not be marked). It is written for compiled actors including supervised and named children. The interpretive `beamtalk_actor:init/1` (deleted by BT-4871) writes it too. `native:` backing-module actors (`safe_start_native`, user gen_servers) carry **no** marker and emit no lifecycle event; their class is recoverable from `'$initial_call'` through the index `beamtalk_actor_registry` (the name-registry module) already keeps for them. **`'$beamtalk_actor_started_at'` is written only by the interpretive `beamtalk_actor:init/1`** (`beamtalk_actor.erl`); no generated code writes it, `grep started_at crates/` is empty, and BT-4871 removes the only writer. So `actor_started_at/1`, used by the ADR 0126 §4 `global` name-conflict tie-break, returns `undefined` for a compiled actor, and `resolve_global_conflict/3` then falls back to `global:random_exit_name/3`. The comment "every actor carries this" is false for compiled actors. This is separate from the ADR (reported on BT-4854); it matters here because a design that needs `spawned_at` from the process must write it.
- **Pid reuse.** An ERTS local pid has 28 bits of identity, so a pid can be reused only after at least 2^28 (≈268M) spawns. At the ~325k registrations per second the open-loop test reached, that is about 14 minutes of continuous flooding, not "never". It matters only to a design that keeps rows across a pid's death for that long.

### Readers

Phase 1 listed every reader. Frequency matters, and the first draft got it wrong.

| Reader | Source today | What it asks | How often |
|---|---|---|---|
| `class_of/1` via `lookup_class/1` in `beamtalk_actor.erl` (5 direct sites + `resolve_class/2`) | instance table | the class of a bare pid, for dispatch telemetry and the `{Class, Selector}` aggregates, the `TimeoutProxy` → `infinity` timeout rule in `sync_send_remote`, and `deadlock_detected` error context | see below |
| `Node current actors` / `actorsOf:` / `actorAt:` (`beamtalk_node.erl`, erpc targets) | actor list | every live actor | on demand, local or over erpc; tests pin it (`node_test.bt`, `workspace_actors.btscript`) |
| REPL / CLI / MCP / VS Code actor lists, `actor_snapshot_frames` (LiveView) | actor list | every live actor with class, module, spawn time; the snapshot must leave no gap with the pushes | on demand, plus on connect |
| `stop_class_actors` (`removeFromSystem`) | actor list | live actors of one class | per `removeFromSystem` |
| `should_purge_module` (`beamtalk_repl_eval.erl`) | actor list | does any tracked actor's recorded module equal this eval module? | **after every REPL evaluation** (`cleanup_module` in `after` clauses) |
| Hot reload (`beamtalk_repl_loader_activate.erl`) | instance table, `all_instances/1` | live pids of one class | once per reloaded class and once per descendant, per reload |
| `trace_store` bottlenecks (raw fold of `beamtalk_instance_registry`), `actor_health` | instance table | queue depth of every live actor / class of one pid | on demand |
| `count_actors_for_module` (`list-classes`), `workspace_meta` count | actor list | counts | on demand |

What `class_of/1` is used for, site by site (read from the code, not measured):

| Site | Class used for | Hint available? |
|---|---|---|
| `async_send/4` final clause (`beamtalk_actor.erl` ~1511) | `dispatch_span` metadata and always-on aggregate | no `_known` variant exists |
| `cast_send/3` → `cast_send_pid/4` (~1609) | same | `cast_send_known/4` passes the record's class |
| `sync_send/3` → `sync_send_remote/4` (~1829) | span, **and the `TimeoutProxy` rule** (semantic) | `sync_send_known/4` passes it |
| `sync_send/4` family (~2027) | span only | `sync_send_known/5` passes it |
| `raise_self_send_during_away_super/2` (~2072), `raise_call_cycle/3` (~2183) | context field of a `deadlock_detected` error | error path only |
| `kill_and_wait/1` (~2420) | metadata of the `kill` lifecycle event | pid only |

The ordinary Beamtalk send path (`beamtalk_message_dispatch`) already passes the `#beamtalk_object{}` record's class (BT-4113) and never calls `lookup_class`. The bare-pid callers are: `async_send/4` (futures), the generated native facade (`native_facade.rs`, which has the record and a static class in hand), `beamtalk_reactive_subprocess`, `sync_send(ActorPid, node, [])` inside `is_remote`, the two error paths and `kill`. Today a lookup of an unregistered pid (a supervised child) already answers `unknown`.

## Options

### S — Status quo
Keep both singletons and the synchronous call. Cost: 16a is 30× the floor at `+S 4` in the VM; buccaneer's HTTP/2 streams stay bare processes. Benefit: nothing changes, nothing to test. It does not meet BT-4854.

### B — Insert, then monitor, as specified in the first draft
Caller inserts rows (pid-keyed row with a registration token, class-table row), adds the pid to a pending table, bumps an `atomics` counter; a server drains the pending table in 50 ms batches, monitoring live pids and deleting dead ones; a bounded pending buffer with a synchronous `flush/0` at the bound; a backstop timer reaper (`max(30 s, 20 × pass time)`); a restart pass; a scan rebuild when the heir is lost; the actor list becomes a view; the actor's own `init/1` inserts again. Six mechanisms. 16a 12.7 / 8.9 µs.

### C — The minimal table design
Reviewer's reduction of B, which this ADR accepts as the form a table design should take:
- **Order:** the caller adds the pid to the pending table *first*, then inserts the rows. A pending pid without rows is harmless if the `'DOWN'` handler deletes by pid lookup, which removes the reaper's main job.
- **No `is_process_alive` calls** in the drain or the caller: `erlang:monitor/2` on a dead pid delivers `'DOWN'` (`noproc`) immediately.
- **Drain monitors everything.** It removes exactly what it read (`ets:take/2` per key, never "delete all after a list") and decrements the counter by the number drained, never resets it.
- **One arbiter for the double insert.** `ets:insert_new/2` on the pid-keyed table is the arbiter; the loser writes nothing (no second row in the class table). No registration token: a pid-reuse guard on the *current monitor reference per pid* is enough, because stale rows survive at most one drain period (50 ms), not 2^28 spawns.
- **No timer reaper.** Rows written while the server is down are covered by the restart pass, because the pending table survives under the heir (BT-4123). The restart pass monitors every row (monitoring a dead pid yields `'DOWN'`).
- **Bound** on the pending table with back-pressure (`flush/0` call at the bound); **scan** only to rebuild after the heir itself is lost.
- `write_concurrency` / `decentralized_counters` on every table written from callers.
- `trace_store` and every other raw reader go through an API function, not a pattern match on row shape (the current `trace_store` fold matches `{{Class, Pid}}` only and skips everything else as malformed, so a wider row would silently return empty bottlenecks).

Four mechanisms: pending + drain, bound, restart pass, scan rebuild.

### D — No registry table; the class travels; the rare readers scan
Remove all three registrations from the start path. There is no table, no server, no monitor, no cast.

1. **The class travels.** Every `lookup_class` site either gets the class from the caller or computes it only when needed:
   - sites with a `#beamtalk_object{}` or a static class already pass it (`*_known` variants); add `async_send_known/…` and make the native facade emit the `_known` call (it has `Self` and a static `class_name`); `beamtalk_reactive_subprocess` passes the class it holds;
   - the error paths and `kill_and_wait/1` are rare and may read `process_info(Pid, {dictionary, '$beamtalk_actor'})` (≈1.6 µs plus a signal to the target, against ≈0.7 µs for the ETS read);
   - the remaining bare-pid callers use the same read. This is the only cost D adds to a send, and only for sends that arrive with a bare pid. The `{Class, Selector}` aggregate is always on, so a bare-pid send needs the class whether or not telemetry is attached; the `TimeoutProxy` rule needs it semantically, but proxies are reached through `beamtalk_message_dispatch`, which has the record.
2. **Readers become scans or stop enumerating.** `erlang:processes_iterator/0` plus `process_info(Pid, {dictionary, '$beamtalk_actor'})`. One enumeration costs about **2.5–4.5 µs per process in the node**, whatever the fraction that are actors:

   | live processes | one enumeration (`+S 4`) |
   |---|---|
   | 1k | 3–6 ms |
   | 100k | 0.21–0.30 s |
   | 1M | 2.6–4.6 s (two concurrent scans slow each other: 4.9–5.7 s each) |

   Reader by reader:

   | Reader | Under D | Cost |
   |---|---|---|
   | `Node current actors`, `actorsOf:`, `actorAt:`, REPL / CLI / MCP / IDE lists, `trace_store` bottlenecks, `count_actors_for_module`, `workspace_meta` count | one scan per call; `actorAt:` is `process_info` on the parsed pid plus the marker check, no scan | 3–6 ms at 1k; 0.2–0.3 s at 100k; seconds at 1M |
   | `allInstances` / `instanceCount` | scan, filter by class | O(processes in the node), not O(instances of the class) |
   | `stop_class_actors` | one scan | per `removeFromSystem` |
   | **`should_purge_module`** | **does not enumerate actors.** The question is "does anything still run this module's code", which `code:soft_purge/1` / `erlang:check_process_code/2` answer directly, including processes outside the actor list and processes holding funs of the module. The actor list's recorded module is the class module, never the eval module `beamtalk_repl_eval_N`, so the present check is in practice always `true` after the read. To be verified when implemented. | no registry read, no scan; this also removes the per-evaluation O(actors) call today |
   | Hot reload | **one scan per reload**, grouped by class, instead of one `ets:select` per class and per descendant | one scan per save |
   | `class_of/1` readers | see item 1 | ≈0 for the common path |
   | `actor_snapshot_frames` and the incremental IDE feed | the consistent-snapshot problem is real: a scan is not one, and the feed must leave no gap with pushes. Not solved by D alone. | see below |

3. **An index only while someone is watching (optional, not part of D's first step).** `ActorSpawned` / `ActorStopped` already exist as system announcements (ADR 0093). An IDE or workspace session that wants an incremental feed subscribes, takes one scan as its initial snapshot, and applies the announcements after it (subscribe first, then scan, then drop announcements older than the scan). The index lives in the subscriber, not in the runtime: no runtime state exists without a consumer, which is the property `docs/beamtalk-principles.md` §4 asks for (the same operations mean the same thing in the REPL, `beamtalk run`, `beamtalk test` and releases). It differs from the dismissed "register only when a workspace is attached" in that `Node current actors`, hot reload and purge work everywhere, by scan; only the incremental feed needs a subscriber.
4. **What it gives and removes.** Start path removes: both `register` calls, the `track_spawned` cast and its monitor, the class-process call in the cast handler, and the REPL `register_spawned` → `on_actor_spawned` → `register_actor/4` call (the REPL no longer needs to tell the runtime about an actor; the `actor_spawned_hook` for `beamtalk_workspace_meta` moves to the ActorSpawned announcement subscriber). What remains: the `'$beamtalk_actor'` write the actor already does, one `erlang:system_time/1` write for `'$beamtalk_actor_started_at'` (see below), the lifecycle announce and telemetry (BT-4932). Measured prototype: 16a **7.4 µs at `+S 1`** against 8.9 for B and 10.9 today, 11.5 vs 12.7 vs 31.9–36.3 at `+S 4`.
5. **What `spawned_at` needs.** A scan cannot recover `'$beamtalk_actor_started_at'` unless the process wrote it. D has the generated `init/1` write it (one `system_time` call and one `put`; the cost is not yet measured and goes into the acceptance measurements). That also fixes the ADR 0126 §4 tie-break for compiled actors and does not depend on BT-4871.
6. **`native:` backing-module actors.** User gen_servers with no marker and no lifecycle event. They are in `allInstances` today only because the facade's `spawn` registers them. Under D they would disappear from `allInstances` and hot reload unless the scan also matches `'$initial_call'` against the backing-module index `beamtalk_actor_registry` (the name-registry module) already keeps for `Node current actors`. D uses that index in the scan; this must be tested, and it is a point where B or C would be safer.

What D costs, plainly:
- `allInstances`, hot reload and the actor lists are O(**all processes in the node**), not O(actors of the class). At development scale (1k–10k processes) that is milliseconds. At 100k processes it is 0.2–0.3 s per call, which hurts anything polled; at 1M it is seconds and two concurrent scans slow each other.
- No O(1) `instanceCount`. No consistent snapshot from a scan.
- A scan signals every process in the node.
- Bare-pid sends pay ≈1.6 µs plus a signal until their callers are fixed to pass the class.
- Native actors need the `'$initial_call'` match.

Who is hurt: a node with ~1M processes that runs hot reload, `removeFromSystem` or a polled actor list. The project does not currently serve that case through those readers (hot reload is a development operation; production nodes are not hot-reloaded in bulk), and ordinary runtime paths do not touch them. Whether the project needs to is the one owner call D hinges on.

### What each option leaves to build and maintain

| | mechanisms | start path (`+S 1`, 16a) | readers | risk |
|---|---|---|---|---|
| S | 0 new | 10.9 µs | unchanged | the problem stays |
| B | pending buffer, drain, bound, timer reaper, restart pass, scan rebuild (6) | 8.9 µs | table views | highest: most code and states to test; shared counter and ETS writes on every scheduler |
| C | pending + drain, bound, restart pass, scan rebuild (4) | ≈8.9 µs | table views | medium: shared writes at `+S 18` unmeasured |
| D | scan, plumbing the class through sends, `started_at` write (3 small) | 7.4 µs | scans | readers O(processes); native actors; snapshot only with a subscriber |

## Decision (recommended)

**Adopt D.** It removes the work instead of making it cheaper, it needs the fewest mechanisms, it has no shared write on the start path (so nothing for 18 schedulers to contend on), it needs no recovery story (no owner, no heir, no rebuild), and the two readers that run most often (`class_of/1` on the send path, `should_purge_module` after every evaluation) stop enumerating or stop looking up. Its costs fall on rare, human-driven readers, which is also where the owner's steer (the registries "may not be needed as much now") points.

**Acceptance conditions.** D is accepted only if each of these holds when measured; if one fails, fall back to **C**:
1. Rows 16a/16c/16d on the maintainer's machine at `+S 18`, before and after, and the nightly vs-elixir gate does not regress. (The cloud-VM numbers in this ADR are not sufficient to accept either design; contention is the thing they cannot show.)
2. One enumeration at the process counts of the project's real nodes (the maintainer's, and a buccaneer load test), including with 100k–1M live processes, stays inside a bound the owner accepts for hot reload and the REPL actor list.
3. The `'$beamtalk_actor_started_at'` write in generated `init/1` costs less than the registry hop it replaces.
4. Native-actor listing through `'$initial_call'` works in a test.
5. Every reader in the table above has a test (including `trace_store` bottlenecks and `actor_health`, which must not return empty silently).

**If C is chosen instead,** it must be built as specified in [Option C](#c--the-minimal-table-design), with `PendingMax` **a placeholder** (the first draft's 8192 was asserted before any harness result). The open-loop harness (16 spawners, `PendingMax` 512) showed pending peaking at ≈520, server mailbox 36–58 and dead rows ≤530 over ≈400k registrations per mode; the value for production has to come from the `+S 18` run (where does pacing start, how much memory does an async flood hold), and is part of acceptance.

## Observable changes

Common to any option that removes the synchronous call:
- Registration no longer paces spawners: a spawn flood queues runnable actors, as a bare `spawn_monitor` flood does (120k–700k runnable actors in the open loop). That is process backlog, not registry growth. The CI test asserts rows and mailboxes (or, under D, that no registry process or table exists), not process count.
- The two registries stop being two sources: under D neither exists; under C the actor list is a view of the instance table.

Membership. Under D, scanning by the marker finds every compiled actor, supervised and named children included, so **`allInstances` and hot reload gain members** (today they miss plain supervised and named children). The actor list readers (`Node current actors`, `stop_class_actors`, `should_purge_module`) already have them. Option C with the actor's own `init/1` insert has the same effect. If the owner wants membership kept as today, that is a separate decision (see Alternatives, "Keep membership"). The effect on purge is analysed next.

### Supervised children and `removeFromSystem` / purge

This hazard exists **on main today**, independent of this ADR: `stop_class_actors/1` reads the registered actor list, which already holds supervised and named children, and kills each. `removeFromSystem` kills the actors *before* it stops the class process and deletes the module. **Reproduction result: pending (a test on main is running); this section is completed before the revision is final.**

## Prior Art

- **Elixir `Registry`:** partitioned ETS tables; registered processes *link* to the partition, which deletes rows on `EXIT`. We do not copy it: the link is two-way (killing a partition killed 143 of 400 live actors).
- **OTP `pg` / `global`:** monitor-driven membership with a registrar process, a process on the registration path.
- **`proc_lib` labels (OTP 27) and `processes_iterator` (OTP 28):** make enumeration of labelled processes possible without a registry; `observer` and `recon` enumerate processes this way.
- **Smalltalk `allInstances`:** a heap scan (ADR 0005). D keeps Smalltalk's semantics (a scan, on request) and drops the BEAM-specific bookkeeping ADR 0005 added.

## User Impact

- **Newcomer / Smalltalk developer:** `Foo allInstances` has the same meaning; supervised instances are now included.
- **Erlang/Elixir developer:** under D there is nothing to inspect: no registry process, no tables, no links, `process_info` shows the marker. Under C the shape is ETS plus monitors.
- **Operator:** actor start is cheaper under concurrency. Under D, a scan-backed list or hot reload costs seconds at 1M processes.
- **Tooling developer:** the IDE gets its snapshot from one scan and its updates from the existing announcements; an IDE that needs an always-on index builds it itself.

## Steelman Analysis

| Option | Strongest case |
|---|---|
| S | **Operator:** nothing moves; the synchronous call is proven flow control. |
| B/C (table) | **BEAM veteran:** monitor-driven membership, O(class) reads, exact `instanceCount`, a consistent snapshot, the Elixir `Registry` shape. **Tooling developer:** the IDE lists stay O(actors). |
| D (no table) | **Language designer:** nothing to keep consistent; the table cannot disagree with reality. **Smalltalk purist:** `allInstances` is a scan, as in Smalltalk. **Operator:** no singleton, no recovery path, the lowest start cost. |
| Partition-link | **BEAM veteran:** the known Elixir design. |

Tension: tooling wants an index, operators want no shared state on the start path. D resolves it by putting the index in the subscriber; C keeps it in the runtime.

## Alternatives Considered

### Periodic reaper as the primary cleanup (`research/bt-4854-ets-reaper`)
Caller-side inserts, no monitor per actor, a reaper walks the pid index. 16a 11.8 / 8.3 µs. Rejected as the primary mechanism: a pass walks every live row, 346 ms per pass at 1M rows (26.8 ms at 100k), so a fixed 250 ms period costs more than a core at about 1M actors and the period has to grow with the table, pushing the stale window to seconds; dead rows peak at kills × period (63k with `exit(kill)`); a periodic sweep of live state is not the OTP convention, where the runtime reports death.

### Partition processes with links (`research/bt-4854-partition-link`)
Rejected: a partition crash kills its actors (143 of 400 measured); the link is visible in `process_info(links)` and, for a trapping actor, in `onLinkedExit:reason:`; 16a at `+S 4` was unstable (14.0–42.6 µs); the actor must link from its own `init/1`, leaving a window that needs a sweep anyway. No BIF sets a monitor on behalf of another process, so a monitor variant with no start-path message does not exist.

### Sharded registries (`research/bt-4854-sharded`)
No gain (16a 38.7 µs at `+S 4`): the synchronous round trip stays on the path.

### Lazy batch (`research/bt-4854-lazy-batch`)
Pending buffer drained on a timer and before enumeration. Same start cost; readers need a flush; the buffer reached 680k rows (168 MB) under an async flood. C is this design with the rows inserted at once and the bound added.

### Keep membership as it is today
The decision of this ADR bundles four separable things: registration off the start path; merging the two registries; widening `allInstances` and hot reload to supervised and named actors; moving rare readers to scans. All the measured gain comes from the first. One could keep membership as it is today (for example a `via` column on a table, set by the generated `spawn`, which `allInstances` and hot reload filter on) and take the widening as its own decision once the supervision interplay above is settled. It is rejected here only because it keeps the table and the two-source problem it exists to remove; if the owner wants the widening separated, that is C with a `via` column, and the hazard analysis above decides the second step.

### Register only when a workspace is attached
Not prototyped. Breaks `Node current actors` and hot reload under plain `beamtalk run` and `beamtalk test`, and adds a mode whose meaning differs by boot context (`docs/beamtalk-principles.md` §4). D's optional subscriber-held index is not this: no runtime state depends on it.

## Consequences

### Positive
- Registration is off the start path under D: 16a 7.4 µs at `+S 1` (from 10.9), no shared write.
- Nothing to recover after a crash, and no mailbox or table to bound under D.
- `should_purge_module` stops reading the actor list on every REPL evaluation.

### Negative
- The 2–3× target of BT-4854 is not met by this change (BT-4932).
- D makes `allInstances`, hot reload and the actor lists O(processes in the node).
- Behaviour changes: `allInstances` and hot reload include supervised and named children; native actors need a separate match under D.
- Tooling that wants an index builds its own from announcements.

### Neutral
- The name-registry module `beamtalk_actor_registry.erl` is unchanged.
- ADR 0126's `actor_started_at/1` starts returning values for compiled actors if D's `started_at` write is adopted (a behaviour change to the `global` tie-break, which today falls back to `random_exit_name/3`).

## Migration Path

Observable changes and what to do about them:
- **`allInstances` includes supervised and named children** of the class. Code that assumed only `spawn`-started instances now sees more; filter on `supervisor` ancestry if needed. CHANGELOG entry.
- **Hot reload migrates them too.** Previously missed supervised actors kept old code and old state shape. A supervised actor whose migration fails stays suspended (ADR 0123), so a supervisor shutdown waits out its timeout for it; the implementation documents this on the reload path.
- **`removeFromSystem` / purge:** see the section above; no change to the set of actors it touches (they were already in the actor list).
- **`global` name conflicts between compiled actors** resolve by start time instead of `random_exit_name/3` if the `started_at` write is adopted.
- Anything reading `beamtalk_instance_registry` / `beamtalk_instance_class` directly (`trace_store`) moves to an API or a scan in the implementation PR.

## Implementation

After acceptance, one PR for the chosen option, plus a small PR per behaviour fix found on the way. For D:
1. Plumb the class: `async_send_known`, native-facade `_known` call, `beamtalk_reactive_subprocess`; the remaining bare-pid sites read the marker.
2. Generated `init/1` writes `'$beamtalk_actor_started_at'`; `actor_started_at/1` is unchanged.
3. Scan reader in one runtime module (`beamtalk_actor_scan` or similar) used by `allInstances`, hot reload (one scan per reload, grouped), `stop_class_actors`, the actor lists, `trace_store` bottlenecks, `actor_health`; `'$initial_call'` match for native actors.
4. `should_purge_module` answers with `code:soft_purge/1` / `erlang:check_process_code/2`.
5. Remove `register` from generated `spawn*` and the native facade, `track_spawned`, `register_spawned`, `beamtalk_object_instances`, `beamtalk_repl_actors`' state; the `actor_spawned_hook` for `beamtalk_workspace_meta` moves to an announcement subscriber. Update `docs/development/surface-parity.md` for the lists.
6. Tests: a reader test per row of the reader table; a scan-cost guard; native actors in `allInstances`; a CI test that a spawn flood leaves no registry state.
7. Measurements as the acceptance conditions; list in the PR what to re-measure on the maintainer's machine at `+S 18`.

Follow-up issue (filed, not done here): `beamtalk-buccaneer` makes HTTP/2 streams actors again, with 133.6k req/s as the figure to beat.

### Consequences for earlier ADRs

| ADR | Amendment made by the implementation PR |
|---|---|
| [0005](0005-beam-object-model-pragmatic-hybrid.md) | "`allInstances` tracks actors via ETS … process monitors" becomes a scan of the process marker (D), or batched monitors (C); supervised and named actors are included. |
| [0009](0009-otp-application-structure.md) | `beamtalk_object_instances` / `beamtalk_repl_actors` leave the supervision tree (D) or become a view (C). |
| [0069](0069-actor-observability-and-tracing.md) | The class lookup description (a bag, `ets:match` by pid) is already stale; replace with: the class travels with the reference, a bare pid reads the marker. |
| [0079](0079-named-actor-registration.md) | The `'$beamtalk_actor'` marker was introduced for identification and `all_registered/0` filtering, not enumeration; it now also backs the scan. `'$beamtalk_actor_started_at'` is written by generated `init/1`. |
| [0093](0093-announcements-event-substrate.md) | `ActorSpawned` / `ActorStopped` unchanged; they become the only incremental feed, and the source of `actor_spawned_hook`. |
| [0123](0123-versioned-state-migration.md) | Hot-reload migration enumerates by one scan and includes supervised actors; a failed migration leaves a supervised actor suspended and a supervisor shutdown waits out its timeout. |
| [0126](0126-distribution-location-transparent-actors.md) | `local_actors/0`, `local_actor_at/1` become scans; the `unknown` class note for a missing row goes away; `actor_started_at/1` returns values for compiled actors. |
| [0129](0129-class-side-system-facades.md) | `Node current actors` "backed by `beamtalk_actor_registry`" (the process `beamtalk_repl_actors`) becomes a scan. |
| `docs/beamtalk-ddd-model.md` ADR-5 | "ETS table with process monitors" becomes "process marker, enumerated on demand" (D). |

## References
- Related issues: BT-4854, BT-4932 (rest of the start path), BT-4845, BT-4842, BT-4871
- Related ADRs: 0005, 0009, 0069, 0079, 0093, 0123, 0126, 0129, 0139
- Research branches: `research/bt-4854-harness` (`benchmarks/vs-elixir/research-bt4854/`), `-insert-then-monitor`, `-ets-reaper`, `-label-scan`, `-partition-link`, `-lazy-batch`, `-sharded`; WIP of the first draft on `bt-4854-registry-impl`
- Measurements: BT-4854 phase 1 report and addendum (Linear comments, 2026-11-15)

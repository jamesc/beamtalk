# ADR 0126: Distribution and Location-Transparent Actors

## Status
Proposed (2026-09-22)

## Context

### Problem statement

Distribution is BEAM's defining capability, and no Smalltalk has ever had it
natively. Beamtalk actors are already `gen_server` processes, so a remote
actor is *physically* one `gen_server:call` away — but the language has no
notion of a node, of spawning or finding an actor on another node, or of what
happens when two nodes disagree about a class. Today the only way in is FFI:

```beamtalk
Erlang erlang node                        // => the current node atom
(Erlang net_adm) ping: #'worker@host'     // => #pong — and now what?
```

Every prerequisite this ADR needs has landed, and each one explicitly left the
cross-node question open for this ADR (BT-3527):

| ADR | What it gives | What it deferred to BT-3527 |
|-----|---------------|-----------------------------|
| 0103 Sendability | Static tiers: `Sendable`, `SendableRef`, `HandleScoped(#process \| #node)`, `Unknown` | "remote-node grading of `#node` scopes (blocked on a future cluster-registration ADR)" (0103 L20, L170-173) |
| 0104 Typed actor protocols | Sends type like method calls | Nothing — "Cross-node calls type the same as local (location transparency preserved)" (0104 L168) |
| 0079 Named registration | `spawnAs:`, `named:`, reserved names — **local** `erlang:register` only | "must leave room for future cluster-wide (`global`) and pluggable (`{via, Mod, Term}`) registration" (0079 L47) |
| 0123 Versioned state | `shapeVersion:`, `migrateFromVN:`, the `{beamtalk_shape, Class, Vsn, Fields}` envelope, `pack/1`/`unpack/1` | "Whether cross-node *messages* are wrapped, or versions negotiated per class at connect time, is BT-3527's decision" (0123 L515-539) |
| 0125 OTP releases | Always-on loopback distribution; `unpack_strict/1` → `shape_version_ahead`; `shapes.json` / `Beamtalk shapeManifest` | "this ADR *defines* `unpack_strict/1`; BT-3527 implements it" and "BT-3527 uses [the manifest] to negotiate per class at connect time" (0125 §3.4) |
| 0124 Slots | `late` slots are absent map keys until assigned | "an unassigned `late` slot travels as absent" (0124 §8) |
| 0093 Announcements | `SystemAnnouncer`, typed `Announcement` subclasses | netsplits and multi-node clusters deferred (0093 L161-185) |
| 0069 Observability | Propagated context rides in every sync message | — |

This ADR closes those loops for a deliberately small v1: **what Erlang `dist`
plus `global` give for free, made safe and typed.**

### Current state

**Actor references.** An actor is `#beamtalk_object{class, class_mod, pid}`
where `pid :: pid() | {registered, atom()}` (`runtime/apps/beamtalk_runtime/include/beamtalk.hrl:35-42`).
A raw pid already works across nodes. A `{registered, Name}` ref resolves via
local `whereis/1` on every send, so a registered ref shipped to another node
silently resolves against *that* node's registry.

**Sends are not remote-safe.** Every send path in `beamtalk_actor.erl` guards
with `is_process_alive(ActorPid)` (lines 599, 712, 749, 793, 899, 1003), which
raises `badarg` for a remote pid. `lookup_class/1` reads the local
`beamtalk_instance_registry` ETS table (line 1339) and returns `unknown` for a
remote pid. The exit mapping (lines 952-965) has no `nodedown`/`noconnection`
case: a partition falls into the catch-all and is reported as `actor_dead`,
which is false — the actor may be alive on the far side. BT-2530 already fixed
the same `is_process_alive` bug in announcements with a
`node(Pid) =:= node()` guard (`beamtalk_announcements.erl:621`); similar ad-hoc
guards exist in `beamtalk_inspector.erl:261` and
`beamtalk_object_class.erl:1811`.

**Spawn.** Unnamed `spawn`/`spawnWith:` use `gen_server:start` — **unlinked**
in the common case (`safe_spawn/2`, `beamtalk_actor.erl:437`). Named spawn
stays linked (`safe_spawn_named/3`). Supervisor children use `start_link`.

**Blocks** compile to local Core Erlang `fun`s (`crates/beamtalk-codegen/src/core_erlang/blocks.rs:307`).
A local fun is callable on another node only if the defining module is loaded
there **at the same version** (Erlang checks the module's MD5/unique id);
otherwise the call raises `badfun` (or `undef` if the module is absent).

**Handles.** Native objects are maps tagged `'$beamtalk_class'`. `Ets` is
`#{'$beamtalk_class' => 'Ets', table => Name}` — a named table that exists on
one node only. `Ets`, `AtomicCounter` and `Timer` are ADR 0103's `#node`
*candidates* but carry no `handleScope:` declaration, so their tier is
`Unknown` and they are silent everywhere. `Subscription` is `#node` via the
builtin table (`sendability.rs:153-170`).

**Distribution is on, loopback-only.** ADR 0125 §1.6: releases run
`-sname` with `inet_dist_use_interface {127,0,0,1}`; workspaces start with
`-sname`/`-name` and a per-workspace cookie passed via `-args_file`
(`startup_command.rs:70-94`). ADR 0058 L83: the workspace cookie is **both**
the WebSocket auth token and the distribution cookie — "a valid cookie = full
RCE". ADR 0091/0125: remote attach **must** use TLS distribution or a tunnel,
"never a `-name` on a routable interface". The ADR 0097 attach front connects
as a *visible* (non-hidden) node via `:rpc.call`.

**Propagated context** (ADR 0069) travels inside every sync message as
`{Selector, Args, PropCtx}`; `restore_propagated_ctx/1` ignores unknown keys,
and the deadlock-detection `call_stack` is a list of pids — both already
node-agnostic.

### Constraints

1. **Location transparency is already promised** by ADR 0104 — a remote send
   must type-check identically to a local one.
2. **Local sends must not get slower.** Anything wire-related is paid only
   when the peer is on another node.
3. **Version skew is real**: ADR 0125 makes rolling upgrades a supported
   deployment, so a cluster routinely contains two releases.
4. **Security posture must not regress**: loopback-only by default, cookie =
   RCE, TLS required off-host (ADR 0020, 0058, 0091, 0125).
5. **No duplicate implementations**: the remote-pid liveness check, the
   envelope, and the reserved-name list each already exist once; this ADR
   routes through them.
6. **Out of scope for v1**: actor migration between nodes, distributed
   persistence, consensus, sharding, virtual actors, cross-node supervision.

## Decision

### 1. Scope of v1

| In v1 | Out of v1 (future ADRs) |
|-------|-------------------------|
| `Node` value object: identity, connect, list, ping | Actor migration (move a live actor to another node) |
| Remote spawn (`spawnOn:`, `spawnWith:on:`, `spawnAs:on:`) | Distributed persistence / replicated state |
| Remote named lookup (`named:on:`) | Consensus, leader election beyond `global` |
| Opt-in cluster-unique names (`scope: #cluster`, OTP `global`) | Process groups / cluster pub-sub (`pg`) |
| Remote sync send, cast, async send — unchanged syntax | Sharding, virtual actors (Orleans-style) |
| Wire versioning (ADR 0123 envelope + ADR 0125 strict unpack) | Rename-aware envelope resolution (ADR 0123 known issue) |
| Cluster events: `NodeUp`, `NodeDown`, `NodeShapeSkew` | Cross-node supervision trees |
| Failure mapping: `node_down`, `remote_code_mismatch` | Cluster formation/discovery (libcluster-style strategies) |
| Remote supervision-tree introspection | Separate cookie vs. WebSocket secret (ADR 0058 hardening) |

The rule of thumb: **if Erlang `dist` + `global` does it for free, v1 exposes
it; if it needs new distributed state, it is out.**

### 2. `Node` — a first-class value object

`Node` is a `sealed typed Value subclass:` wrapping the node atom. It is a
**value, not a proxy**: two `Node`s are equal iff their names are equal, a
`Node` is `Sendable` (an atom), and creating one never touches the network.
All effectful operations route to `net_kernel`/`erlang` from the calling
process; there is no per-node process to crash or leak.

```beamtalk
sealed typed Value subclass: Node
  field: name :: Symbol

  /// The node this code is running on.
  class current -> Node => ...

  /// A Node value for `aName`. Pure: validates the `name@host` shape only.
  class named: aName :: Symbol -> Result(Node, Error) => ...

  /// Visible nodes this node is connected to (hidden nodes excluded).
  class connected -> List(Node) => ...

  /// Establish a connection. Applies the §9 host policy.
  connect -> Result(Node, Error) => ...

  disconnect -> Boolean => ...
  isConnected -> Boolean => ...
  isCurrent -> Boolean => ...

  /// `net_adm:ping/1` — true iff the node answered (and is now connected).
  ping -> Boolean => ...

  /// The remote node's shape manifest (ADR 0125 §3.4), fetched via erpc.
  shapeManifest -> Result(Dictionary, Error) => ...
```

`Pid` and `Actor` gain a `node` accessor, and `Actor` gains `isRemote`:

```beamtalk
counter node            // => a Node
counter isRemote        // => false
counter pid node        // => a Node  (Pid>>node, erlang:node/1)
```

**REPL session:**

```beamtalk
> Node current
// => Node(dev@localhost)
> worker := (Node named: #'worker@localhost') unwrap
// => Node(worker@localhost)
> worker connect
// => Result ok: Node(worker@localhost)
> Node connected
// => #(Node(worker@localhost))
> (Node named: #nohost) unwrap
// => raises: #beamtalk_error{kind = invalid_node_name, hint = "expected name@host"}
```

`Node named:` returns a `Result` because a malformed name is an expected
failure (ADR 0060); `connect` returns a `Result` because an unreachable node is
an expected failure. `ping` returns `Boolean` to match `net_adm:ping/1`'s
common use as a probe.

Node monitoring is **not** a method: it is `SystemAnnouncer` subscription
(§8). A per-caller `monitor_node/2` would duplicate what the announcement
substrate already does with better lifecycle management.

### 3. Remote spawn and lookup

Every local spawn/lookup selector gains an `on:` variant. Syntax, return types
and error kinds mirror the local forms exactly; the only additions are the
network failures.

| Local (today) | Remote (this ADR) | Returns |
|---------------|-------------------|---------|
| `Counter spawn` | `Counter spawnOn: node` | `Result(Self, Error)` |
| `Counter spawnWith: args` | `Counter spawnWith: args on: node` | `Result(Self, Error)` |
| `Counter spawnAs: #c` | `Counter spawnAs: #c on: node` | `Result(Self, Error)` |
| `Counter spawnWith: args as: #c` | `Counter spawnWith: args as: #c on: node` | `Result(Self, Error)` |
| `Counter named: #c` | `Counter named: #c on: node` | `Result(Self, Error)` |
| `Actor allRegistered` | `Actor allRegisteredOn: node` | `Result(List(Actor), Error)` |

Remote spawn returns `Result` even where local `spawn` returns `Self`, because
"the node is down" or "the class is not loaded there" are expected failures,
not programming errors (ADR 0060 rule). Error kinds added:

- `node_down` — node unreachable (connect failed or `noconnection`)
- `class_not_found` (existing kind) — the class is not loaded on the target node
- `shape_version_ahead` (ADR 0125) — the `spawnWith:` args carry a Value newer
  than the target node understands

```beamtalk
worker := (Node named: #'worker@localhost') unwrap
c := (Counter spawnOn: worker) unwrap
c increment              // => 1   — gen_server:call to a remote pid
c node                   // => Node(worker@localhost)
c isRemote               // => true

(Counter spawnAs: #hits on: worker) unwrap
hits := (Counter named: #hits on: worker) unwrap
hits increment           // => 1
```

**Implementation shape.** `spawnOn:` is an `erpc:call(Node,
beamtalk_actor, remote_spawn, [Class, Args, NameOrUndefined], Timeout)`. The
spawn runs on the target node through the *existing* `safe_spawn/2` /
`safe_spawn_named/3` entry points — which is why initialisation,
`initialize`, reserved-name checks (ADR 0079 `reserved_name/1`) and
`ActorSpawned` announcements behave identically; they fire on the node where
the actor lives. The class is resolved **by name on the target node**; the
caller's module is never shipped.

**Linking and supervision.** A remotely spawned actor is **never linked to the
caller**, named or not. A link across a node boundary turns every partition
into a crash of the caller (`noconnection` exit), which is the opposite of
what a v1 user expects. The actor is exactly as supervised as a local
`spawn`: not at all. Supervised actors on node B are started by node B's own
supervision tree — its release's `[application] supervisor` (ADR 0125) or a
class method invoked remotely — and then *found* with `named:on:` or
`scope: #cluster`. Cross-node supervision (a supervisor on A owning a child on
B) is rejected; see Alternatives.

**Registered refs are node-qualified on the wire.** A ref created by
`named:on:` carries `pid = {registered, Name, Node}` and sends to it address
`{Name, Node}` (the native `gen_server` form). A local `{registered, Name}` ref
that is sent to another node is rewritten to `{registered, Name, node()}` by
the wire encoder (§5), so it keeps resolving against **its origin node**
rather than silently re-resolving against the receiver's registry. The
`pid` field type widens to
`pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()}`.

### 4. Cluster-unique names (opt-in `global`)

Node-local names are the default and stay the default. A cluster-unique name
is requested explicitly with a `scope:` keyword:

```beamtalk
leader := (Scheduler spawnAs: #scheduler scope: #cluster) unwrap
// on any node in the cluster:
s := (Scheduler named: #scheduler scope: #cluster) unwrap
s tick
```

| Selector | Backing | Returns |
|----------|---------|---------|
| `spawnAs: name scope: #cluster` | `gen_server:start({global, Name}, …)` | `Result(Self, Error)` |
| `spawnWith: args as: name scope: #cluster` | same | `Result(Self, Error)` |
| `named: name scope: #cluster` | `global:whereis_name/1`, class check | `Result(Self, Error)` |
| `scope: #node` | identical to the existing selectors | — |

The scope symbol deliberately reuses ADR 0103's scope vocabulary (`#node`) —
"node-scoped" means the same thing for handles and names. Any other scope
symbol is `#beamtalk_error{kind = type_error}`.

`global` names go through the **same** `reserved_name/1` check as local names
(ADR 0079; the blocklist already contains `global_name_server`,
`global_group`, `net_kernel`, `rex`). Refs carry `{global, Name}` and resolve
via `global:whereis_name/1` per send, like registered refs.

**Partition heal.** When a netsplit heals with the same global name on both
sides, OTP's default resolver (`global:random_exit_name/3`) kills one
registrant. v1 keeps the default and makes it observable: the losing actor's
`ActorStopped` announcement carries `reason: #globalNameConflict`. A
user-supplied resolver is future work.

`pg` process groups are **not** in v1 — they are a pub/sub/membership feature,
not a lookup feature, and belong with a future "cluster announcements" ADR.

### 5. The wire: what crosses a node boundary, and how

#### 5.1 Wrap on remote send only

When the target of a send (sync, cast, async, or `spawnWith:on:` args) is on
another node, the sender encodes the message through a single module,
`beamtalk_wire`, and sends a **tagged wire message** instead of the local
`{Selector, Args, PropCtx}`:

```erlang
%% local (unchanged, zero cost):
{Selector, Args, PropCtx}
%% remote:
{'$beamtalk_wire', 1, Selector, WireArgs, PropCtx}
```

`WireArgs` is `Args` with every Beamtalk `Value` instance replaced by its ADR
0123 envelope `{beamtalk_shape, Class, ShapeVersion, Fields}` (recursively —
nested Values carry their own versions, ADR 0123), every local registered ref
node-qualified (§3), and everything else passed through as an Erlang term. The
receiving actor's dispatch prelude recognises the tag, runs
`beamtalk_shape_migration:unpack_strict/1` over `WireArgs`, and dispatches
normally. **Replies are symmetric**: the callee encodes its reply when
`node(FromPid) =/= node()`, and the caller's `sync_send` decodes it.

"Remote" is decided by `node(Pid) =/= node()` at send time (for
`{registered, N, Node}` / `{global, N}` refs, by the resolved node). This is
one comparison on the local path — constraint 2 holds.

#### 5.2 Version skew (consumes ADR 0123 + ADR 0125 §3.4)

| Receiver sees | Outcome |
|---------------|---------|
| Envelope at the receiver's current version | unpacked as-is |
| Envelope at an **older** version | migrated forward through the `migrateFromVN:` chain (missing steps are structural no-ops, ADR 0123) |
| Envelope at a **newer** version | refused **before** the chain runs: `#beamtalk_error{kind = shape_version_ahead, class, details = #{sent => V, known => K, node}}` |
| Envelope whose class is not loaded | `#beamtalk_error{kind = class_not_found}` |
| Migration step raises | `#beamtalk_error{kind = shape_migration_failed}` (ADR 0123) |

For a **sync** send, the error is raised in the *sender* (the reply is an
error) — the receiving actor's state is untouched, it never saw the message.
For a **cast**, there is no one to tell: the receiver logs it (`?LOG_WARNING`,
domain `[beamtalk, runtime, dist]`) and emits a telemetry event
`[beamtalk, dist, wire_rejected]`; the cast is dropped, matching Erlang's
fire-and-forget semantics.

Method-set skew needs no new mechanism: a selector the older node lacks is a
normal `does_not_understand`.

Renamed classes (ADR 0114) are **not** resolved across nodes in v1 — the
envelope keys on the class atom, so a peer that knows the class only under
its old name reports `class_not_found`. This is ADR 0123's documented known
issue; the alias table it proposes is deferred.

#### 5.3 Connect-time negotiation (early warning, not a gate)

When a node connects (§8), `beamtalk_node_monitor` fetches the peer's
`Beamtalk shapeManifest` (ADR 0125) and compares it with its own. For each
shared class whose `shapeVersion` differs, it announces one `NodeShapeSkew`:

```beamtalk
SystemAnnouncer current when: NodeShapeSkew do: [:e |
  Transcript showLine: e node name asString, ": ", e className asString,
    " local v", e localVersion printString, " remote v", e remoteVersion printString
]
```

Negotiation **informs; it does not refuse the connection**. Refusing would
make every rolling upgrade (ADR 0125: "one shape-version bump per deploy,
upgrade the receiving side first") impossible. The per-envelope strict check
in §5.2 is the safety net; the handshake makes the skew visible before the
first failure.

#### 5.4 Sendability across nodes — per tier

The wire encoder applies a **wire policy** to `pack/1`. ADR 0123's `pack/1`
rejects anything non-`Sendable` because it serves persistence (a pid on disk is
meaningless). A pid on the wire is fine, so `pack/2` gains a policy argument:
`pack(Term, persist)` (today's behaviour) and `pack(Term, wire)`.

| Tier (ADR 0103) | Examples | Cross-node, `wire` policy |
|-----------------|----------|---------------------------|
| `Sendable` | Integer, String, Symbol, Value classes, `Node`, `Reference` | **Allowed, copied**; Values enveloped (§5.1) |
| `SendableRef` | actors, `Pid` | **Allowed**; pids are node-qualified natively; registered refs rewritten (§3) |
| `HandleScoped(#process)` | `Port`, `FileHandle` | **Rejected**: `#beamtalk_error{kind = not_serialisable}` at encode time, in the sender |
| `HandleScoped(#node)` | `Ets`, `AtomicCounter`, `Timer`, `Subscription` | **Rejected**: `not_serialisable` at encode time, in the sender |
| `Unknown` | `Dynamic`, untyped FFI results, blocks | **Allowed as raw terms** (Erlang semantics); failures mapped at use (§5.5) |

Rejecting `#node` handles at encode time is a deliberate change from local
behaviour (where they pass freely): an `Ets` value that arrives on node B names
a table that does not exist there, and the failure would surface far from the
cause. Failing in the sender, naming the field, is the fail-loudly choice ADR
0124 §8 made for `late` slots.

This requires **declaring** the ADR 0103 candidates. `Ets`, `AtomicCounter`
and `Timer` gain `handleScope: #node`; `Subscription` keeps its builtin-table
entry. Since `#node` stays silent for local sends (ADR 0103), the declarations
change no existing diagnostic.

`late` slots (ADR 0124 §8): an unassigned `late` slot is an absent map key and
**travels as absent**; the receiver's first read raises
`UninitializedStateError`. No sentinel crosses the wire.

#### 5.5 Blocks

Blocks are **allowed** across nodes, exactly as Erlang funs are. A block that
is invoked on a node where its defining module is missing or at a different
version raises `badfun`/`undef` there; the runtime maps both — when the fun's
module is the culprit — to:

```
#beamtalk_error{kind = remote_code_mismatch, class = 'Block',
                details = #{module => ..., node => ...},
                hint = "the block's defining class is not loaded at the same version on this node"}
```

The error surfaces **where the block runs**. For a block passed as an argument
to a sync send and invoked during that call, that is the callee — and the
error is relayed back to the sender like any other callee error. For a block
stored and invoked later (e.g. a remote `Timer every:do:`), it surfaces in the
remote actor, like any other runtime error there.

Non-local return (`^`) from a block invoked on another node during a sync call
behaves as it does across a local process hop: the escape travels back in the
reply message (the mechanism ADR 0110 hardens for class-method hops), which is
node-agnostic. Phase 3 includes a two-node test to pin this. A `^` from a block that outlives
its defining method raises as it does locally.

Blocks through class methods (§ Passing Blocks Through Class Methods) need no
special rule: a class method runs in its class's gen_server **on the node the
class-side send was addressed to**, which for `Driver run: aBlock over: xs` is
always the local node. Class-side sends are never implicitly remote in v1.

### 6. Typing: transparent, with "known-remote" provenance

There is **no `Remote(T)` type**. `Counter spawnOn: node` has type
`Result(Counter, Error)`; `unwrap` gives `Counter`; every send types exactly as
ADR 0104 specifies. This keeps location transparency: code that takes a
`Counter` parameter works identically whether the caller passed a local or a
remote actor.

The checker does, however, track a **flow fact** — not a type — marking a
local variable as *known-remote* when it is bound from `spawnOn:`,
`spawnWith:on:`, `spawnAs:on:`, `named:on:`, or any `scope: #cluster`
selector (through `unwrap`, `value`, `ifOk:ifError:` ok-branches). Known-remote
receivers upgrade ADR 0103's boundary checks:

| Argument / capture | Receiver not known-remote | Receiver known-remote |
|--------------------|---------------------------|------------------------|
| `HandleScoped(#process)` | Warning (ADR 0103, unchanged) | Warning |
| `HandleScoped(#node)` | silent (ADR 0103, unchanged) | **Warning** — "`cache` (Ets — node-bound handle) sent to a remote actor; it will be rejected at runtime" |
| Block | silent | **Warning** — "block sent to a remote actor runs only if its class is loaded at the same version on `worker@host`" |

All are `DiagnosticCategory::Sendability`, adjustable via
`[diagnostics] sendability = "hint"`. Provenance does not flow through fields,
collections or method returns; the runtime check (§5.4) is authoritative.

```beamtalk
cache := Ets new: #sessions type: #set
c := (SessionStore spawnOn: worker) unwrap
c remember: cache
// Warning: `cache` (Ets — node-bound handle) sent to a remote actor;
//          it will be rejected at runtime (not_serialisable)
```

### 7. Failure semantics

#### 7.1 Error mapping

| Erlang condition | Today | This ADR |
|------------------|-------|----------|
| `exit:{{nodedown, N}, _}` / `noconnection` during call | catch-all → `actor_dead` (wrong) | **`node_down`**, `details = #{node => N}` |
| `exit:{noproc, _}` on remote pid | `actor_dead` | `actor_dead` (unchanged — the node answered, the actor is gone) |
| `exit:{timeout, _}` | `timeout` | `timeout` (unchanged) |
| `{registered, N, Node}` not registered | — | `no_such_process` (as local) |
| `badfun`/`undef` for a block's module | `erlang_error` | **`remote_code_mismatch`** |
| envelope newer than receiver | — | `shape_version_ahead` |
| `Port`/`Ets`/… in remote args | silently shipped | **`not_serialisable`** (sender) |

`node_down` and `remote_code_mismatch` map to `RuntimeError` in
`kind_to_class/1`, like `actor_dead`. `node_down` is distinct from
`actor_dead` on purpose: after a partition the actor may be perfectly alive,
and retry logic must be able to tell "try again later" from "it's gone".

**Result vs raise** follows ADR 0060 unchanged: connect, spawn and lookup
(`Node connect`, `spawnOn:`, `named:on:`) are expected-failure operations and
return `Result`; **sends raise**, exactly like local sends. A caller that wants
a Result from a send uses `Result tryDo:` as it would locally.

#### 7.2 Timeouts across a partition

The 5000 ms default sync timeout (ADR 0043) and `withTimeout:` apply
unchanged. Erlang detects a silent partition only after `net_ticktime`
(default 60 s), so during a partition a sync send will normally raise
`timeout` **before** anything raises `node_down`. This is documented, not
papered over: `timeout` means "no answer in time", `node_down` means "the
runtime knows the node is gone". Operators who want faster detection tune
`net_ticktime` in `vm.args` (ADR 0125).

A timed-out remote call can still execute on the far side. This is standard
at-most-once-*reply* / possibly-executed semantics, identical to a local
`gen_server:call` timeout, and is called out in the language docs.

#### 7.3 Liveness checks

All six `is_process_alive/1` guards in `beamtalk_actor.erl` route through one
shared helper, extracted from the three existing ad-hoc copies
(`beamtalk_announcements.erl:621`, `beamtalk_inspector.erl:261`,
`beamtalk_object_class.erl:1811`) into a leaf module (`beamtalk_pid:is_alive/1`):
local pids use `is_process_alive/1`; remote pids return `true`
optimistically and let the send fail with its real reason. `lookup_class/1`
for a remote pid uses the `class` already in the `#beamtalk_object{}` record
instead of the local ETS registry.

#### 7.4 Monitors and links

`anActor monitor` already works on a remote pid (`erlang:monitor/2` is
node-transparent); the `DOWN` reason is `noconnection` on partition. v1 does
not add links across nodes (§3) and adds no link API.

### 8. Cluster events

A new `beamtalk_node_monitor` gen_server under `beamtalk_runtime_sup` calls
`net_kernel:monitor_nodes(true, [nodedown_reason])` and announces on
`SystemAnnouncer current`:

```beamtalk
sealed typed Announcement subclass: NodeUp
  field: node :: Node

sealed typed Announcement subclass: NodeDown
  field: node :: Node
  field: reason :: Symbol = #unknown

sealed typed Announcement subclass: NodeShapeSkew
  field: node :: Node
  field: className :: Symbol
  field: localVersion :: Integer
  field: remoteVersion :: Integer
```

```beamtalk
SystemAnnouncer current when: NodeDown do: [:e |
  Transcript showLine: "lost ", e node name asString, " (", e reason asString, ")"
]
```

Only **visible** nodes produce events; hidden nodes (tooling, §9) do not.
Announcements stay node-local (ADR 0093): each node announces its own view of
membership. Cluster-wide announcement delivery is out of v1.

**Supervision-tree introspection across nodes** (ADR 0092):
`ProcessNavigation on: node` returns `Result(ProcessNavigation, Error)` whose
snapshot is taken on the target node via `erpc` (the existing
`supervisor:which_children` walk, run remotely) and returned as
`SupervisionNode` Values — which are `Sendable` and cross the wire through §5.
`SupervisionNode` gains a `node` field.

```beamtalk
tree := (ProcessNavigation on: worker) unwrap tree
tree nodesOfKind: #beamtalkActor
```

### 9. Security model

v1 does **not** change the trust model of ADR 0020/0058: a cookie is full
code execution, and the distribution boundary is single-user.

1. **Loopback by default.** Workspaces and releases keep ADR 0125 §1.6's
   loopback-only distribution. v1 clusters are same-host out of the box
   (useful for development, testing, and multi-release deployments on one
   box).
2. **Off-host requires TLS.** `Node connect` to a host that does not resolve to
   loopback succeeds only if this node runs TLS distribution (`-proto_dist
   inet_tls`); otherwise it returns
   `#beamtalk_error{kind = insecure_distribution, hint = "configure TLS distribution (ADR 0091) or use a tunnel"}`.
   This is a guard on the Beamtalk API, not a firewall — FFI and raw
   `net_kernel` remain available to operators who configure distribution
   themselves.
3. **No cookie API.** `Node` exposes no cookie getter or setter. Cookies come
   from `RELEASE_COOKIE`/`vm.args` (releases) or the workspace cookie file
   (workspaces), never from Beamtalk code. Per-peer cookies remain an FFI
   escape hatch (`erlang:set_cookie/2`).
4. **Workspaces are not cluster members by accident.** A workspace connects
   to other nodes only through an explicit `Node connect` (or `spawnOn:` /
   `named:on:`, which connect implicitly and apply the same policy).
5. **Tooling nodes are hidden.** The ADR 0097 attach front should start as a
   hidden node (`-hidden`) so it does not appear in `Node connected`, does not
   raise `NodeUp`, and does not join the `global` mesh. This is a follow-up
   change to ADR 0097's front, filed from this ADR.

### 10. Tooling and observability

- **Tracing (ADR 0069):** `PropCtx` crosses nodes inside the wire message
  unchanged; `trace_id`/`span_id` join traces across nodes. The OTel context
  term is dropped (not failed) when the receiving node lacks the OTel
  application. Cross-node deadlock detection works as-is (pid call stacks).
- **Inspector (ADR 0095):** `sys:get_state/2` already works on remote pids and
  is timeout-guarded; an unreachable node yields the existing
  `#status: #unavailable`. `#actor` inspector entries gain a `node` field.
- **Display:** `printString` of an actor is **unchanged**
  (`Actor(Counter, <12345.123.0>)` — the pid already encodes a non-local node).
  Any change to REPL display forms needs separate confirmation per CLAUDE.md.
- **Surface parity** (`docs/development/surface-parity.md`): a new `nodes`
  operation (list connected visible nodes with name and shape-skew count) on
  REPL, MCP and LiveView; `Workspace actors` / `list_actors` stay local-node.
  Remote actor listing uses `Actor allRegisteredOn:` from any surface's eval.

## Prior Art

**Erlang/OTP.** Location transparency is native: a pid is a pid, `{Name,
Node}` addresses a remote registered process, `global` gives cluster-unique
names, `pg` gives groups, `net_kernel:monitor_nodes/2` gives membership events.
What Erlang does *not* give: any notion of message schema versioning (records
across mixed-version nodes silently mismatch), or any static check that a
message is meaningful on the receiving node (ETS table ids, ports and funs are
shipped and fail later). **Adopted:** addressing, `global`, monitor_nodes,
unlinked-by-default remote spawn. **Added:** versioned envelopes and
encode-time handle rejection.

**Elixir.** `Node.self/0`, `Node.list/0`, `Node.connect/1`, `Node.spawn/2`,
`:global` via `{:global, name}` in GenServer names; Horde and Swarm for
distributed registries and supervisors; libcluster for discovery. **Adopted:**
the `Node` module shape (`Node current` ≈ `Node.self`, `Node connected` ≈
`Node.list`). **Deferred:** Horde-style distributed supervisors and
libcluster-style discovery — both require CRDT/consensus machinery outside v1.

**Gleam.** `gleam_erlang`'s typed `Subject(msg)` makes local messaging
type-safe, but a Subject's reply channel is tied to the owning process, and
Gleam offers no typed story for distribution. Beamtalk's advantage: a typed
send is a method call on an actor ref (ADR 0104), and actor refs are
node-transparent.

**Akka Cluster (Scala).** Location-transparent `ActorRef`s, cluster membership
events (`MemberUp`, `MemberRemoved`, `UnreachableMember`), and an explicit rule
that **all remote messages must be serialisable** with a configured
serializer; schema evolution is the user's problem (Jackson/protobuf).
**Adopted:** membership events as first-class objects; reachability ≠ death
(`node_down` ≠ `actor_dead`). **Improved on:** versioning is built in via ADR
0123 instead of left to the serializer.

**Orleans (virtual actors).** Grains are addressed by identity, activated on
demand, placed and moved by the runtime. Elegant for elastic services, but
requires a distributed directory and a placement service. Rejected for v1; see
Alternatives.

**Pony.** No distribution in the released language (distributed Pony was a
research effort that was not merged). Its reference capabilities are the model
for ADR 0103's tiers; this ADR extends those tiers to a node boundary the way
Pony's `iso`/`val` would have had to.

**E / Newspeak / Smalltalk.** E distinguishes *near* and *far* references
statically and makes far sends eventual — the strongest argument for
`Remote(T)` (see Steelman). Newspeak's actor design (inspired by E) was never
distributed in practice. Classic Smalltalk distribution (Distributed Smalltalk,
GemStone, Pharo's Seamless/TelePharo) uses transparent **proxies** that forward
every message over a socket, with well-known problems: object identity,
garbage collection of remote refs, and chatty fine-grained traffic. Beamtalk
avoids all three because only *actors* are remote-addressable and everything
else is copied — the BEAM model, not the proxy model.

## User Impact

**Newcomer.** Distribution becomes discoverable: `Node current`, `Node
connected`, `Counter spawnOn: node` are guessable from the local forms, and
every new selector is an `on:`/`scope:` suffix on one they already know.
Errors name the problem (`node_down`, `shape_version_ahead`,
`not_serialisable` naming the field) rather than surfacing raw `badarg` or
`{nodedown, …}` tuples. The risk: location transparency hides latency and
partial failure; the docs lead with the "timeout does not mean it didn't run"
caveat.

**Smalltalk developer.** Remote actors are sent messages exactly like local
ones — message-passing purity is preserved, and there are no proxies with
broken identity. `Node` is a plain value with a class-side API, as a Pharo
user would expect. The departure — only actors are remotely addressable,
values are copied — is justified by the BEAM model and avoids the known
failure modes of Smalltalk remote proxies.

**Erlang/Elixir developer.** Everything maps to something they know:
`spawnOn:` is `erpc` + `gen_server:start`, `named:on:` is `{Name, Node}`,
`scope: #cluster` is `global`, events are `monitor_nodes`. Messages to remote
Beamtalk actors from Erlang must use the wire form or go through
`beamtalk_actor:sync_send/3` (which encodes); a raw `gen_server:call` with a
local-form message still works when no Values are involved. They gain what OTP
never gave them: versioned payloads and encode-time rejection of ETS/port
handles.

**Production operator.** Rolling upgrades across a cluster are supported and
observable (`NodeShapeSkew` at connect, `shape_version_ahead` per message,
`[beamtalk, dist, wire_rejected]` telemetry). Standard tools work: remote pids
in observer, `recon`, `dbg`. The security default (loopback-only; TLS required
off-host) is conservative. Partition behaviour is standard Erlang, tunable via
`net_ticktime`.

**Tooling developer.** No new type constructor to render or complete. The
known-remote provenance fact is local to a method body and cheap. The LSP can
show `node` in hover for actor-typed variables when the value is live (REPL).
Inspector, supervision navigation and `nodes` work through existing
Dictionary/Value wire forms.

## Steelman Analysis

### Alternative: Distinct `Remote(T)` type

- 🧑‍💻 **Newcomer**: "I *want* to know when a call might go over the network —
  hiding it is how people write chatty, fragile code."
- 🎩 **Smalltalk purist**: "E's near/far distinction is the most rigorous
  object-capability model ever built; it's the natural heir to Smalltalk
  message passing."
- ⚙️ **BEAM veteran**: "Erlang itself hides it, and it's the #1 source of
  surprise in production distribution."
- 🏭 **Operator**: "Every cross-node send is visible in the source, so code
  review can catch them."
- 🎨 **Language designer**: "With `Remote(T)` the checker knows exactly where
  node-scoped handles and blocks are illegal — no provenance heuristics."

*Response:* ADR 0104 already committed to location transparency, and a
`Remote(T)` would infect every signature that accepts an actor (does
`process: aCounter` take `Counter` or `Remote(Counter)` or both?). Registered
and global refs can move between nodes across restarts, so "remote" is often
not a static property at all. The known-remote provenance fact captures most
of the checking value at none of the signature cost, and the runtime encoder
is authoritative.

### Alternative: Handshake-only skew handling (refuse on mismatch)

- 🏭 **Operator**: "Fail fast at connect time; never run a mixed cluster I
  didn't vet."
- 🎨 **Language designer**: "No per-message encoding at all — cheaper, simpler."

*Response:* It makes ADR 0125's rolling-upgrade procedure impossible, and a
single shared class at a different version would partition the cluster. We
keep the handshake as an early warning and put the safety at the envelope.

### Alternative: Always wrap every message (local and remote)

- 🎨 **Language designer**: "One code path — no local/remote bifurcation to
  test."
- 🏭 **Operator**: "Messages on disk, in traces, or forwarded via Erlang
  intermediaries are always self-describing."

*Response:* It taxes every local send (the overwhelming majority) with a deep
term walk. The bifurcation is one `node/1` comparison, and a two-node test
suite exercises the remote path.

### Alternative: Reject blocks at encode time

- 🏭 **Operator**: "Funs across nodes are a hot-reload time bomb — ban them."
- 🎩 **Smalltalk purist**: "A block is a closure over *this* image; shipping it
  is a category error."

*Response:* It would rule out remote `Timer every:do:`, remote
`collect:`-style APIs and every Erlang library that takes a fun. Erlang allows
it and the failure is well-defined; we map it to a clear
`remote_code_mismatch` and warn when the receiver is known-remote.

### Alternative: Pure FFI (status quo)

- ⚙️ **BEAM veteran**: "`Erlang net_kernel`, `Erlang global` and `Erlang erpc`
  already work. Don't wrap OTP — document it."

*Response:* FFI gives addressing but none of the safety: sends to remote pids
currently crash with `badarg` in `is_process_alive`, partitions are misreported
as `actor_dead`, Values cross without version checks, and ETS handles cross
silently. The runtime fixes are needed regardless; the language surface is thin
on top.

### Tension points

- Operators and language designers pull toward explicitness (`Remote(T)`,
  handshake refusal); newcomers and BEAM veterans pull toward transparency.
  This ADR chooses transparency in *syntax and types* and explicitness in
  *diagnostics, errors and events*.
- Smalltalk purists would ban blocks across nodes; BEAM veterans would allow
  them. We allow them with a mapped error and a warning.

## Alternatives Considered

### `Remote(T)` / far-reference type
Rejected — see Steelman. Breaks ADR 0104 location transparency and every
actor-accepting signature.

### Registry: `pg` groups in v1
`pg` is the right tool for "all members of group X" (pub/sub, worker pools),
not for "the one actor named X". Adding it in v1 widens scope into cluster
announcements. Deferred.

### Registry: `syn` / Horde-style distributed registries
Better partition handling and conflict resolution than `global`, but a new
dependency with its own consistency model. `global` ships with OTP, is what
`{global, Name}` names already mean to every BEAM developer, and is sufficient
for singletons. A future `{via, Mod, Term}` pluggable scope (anticipated by ADR
0079) can add them without changing the `scope:` syntax.

### Virtual actors (Orleans)
Actors addressed by identity, activated on demand, placed by the runtime.
Requires a distributed directory, placement, and deactivation/persistence — at
least three ADRs of machinery (including actor migration and persistence,
both out of scope). Revisit after persistence lands.

### `Node` as a proxy actor
A per-node gen_server that forwards operations and tracks connection state.
Rejected: it adds a process per known node that can crash, leak or be
partitioned itself, and `Node` identity would stop being value equality.
`net_kernel` already holds the connection state.

### Cross-node supervision (supervisor on A, child on B)
OTP supervisors require linked children; a link across a partition makes the
supervisor restart a child that is still running on the other side
(split-brain duplicates). Horde exists precisely because this is hard.
Rejected for v1; supervised remote actors come from the remote node's own tree.

### Link remote spawns to the caller
Matches `spawn_link` intuition, but every partition would kill the caller with
`noconnection`. Remote spawn is unlinked; users who want failure propagation
use `monitor`.

### Handshake-only / always-wrap wire policies
Rejected — see Steelman.

## Consequences

### Positive
- Distribution becomes a language feature with a small, guessable surface:
  one `Node` class, `on:`/`scope:` variants of existing selectors, three
  announcements.
- Existing runtime bugs are fixed on the way: remote-pid `badarg` in
  `is_process_alive`, partitions misreported as `actor_dead`, `lookup_class`
  returning `unknown`.
- ADR 0123's envelope and ADR 0125's `unpack_strict/1` get their promised
  consumer; rolling cluster upgrades are safe by construction.
- ADR 0103's `#node` tier finally does something: node-bound handles are
  rejected at the boundary where they would break.
- Location transparency (ADR 0104) is kept; no new type constructor.
- Three ad-hoc remote-pid liveness checks collapse into one shared helper.

### Negative
- Remote sends pay a deep term walk for encoding/decoding; large payloads of
  nested Values are measurably slower remotely than a raw Erlang message.
- Local and remote paths diverge in the runtime (tagged wire message vs local
  form); both need tests, and a multi-node test harness (`peer`) becomes a CI
  requirement.
- Erlang code sending raw local-form messages containing Values to a remote
  Beamtalk actor bypasses version checks.
- `timeout` vs `node_down` during a partition is subtle and must be documented.
- Blocks across nodes can still fail at runtime (mapped, not prevented).
- Known-remote provenance is shallow (method-local), so some remote sends of
  node handles are only caught at runtime.
- `global` has known scaling limits (full-mesh locking) and arbitrary conflict
  resolution on heal.

### Neutral
- `Node named:`, remote spawn and lookup return `Result`; sends raise — same
  split as local code (ADR 0060).
- `printString` and existing REPL outputs are unchanged.
- Cast rejections on skew are dropped and logged, matching Erlang cast
  semantics.
- The attach front becoming a hidden node is a follow-up change to ADR 0097's
  implementation, not a decision here.

## Implementation

Each phase is independently shippable. Sizes are rough.

**Phase 0 — Remote-safe runtime and multi-node test harness (M).**
Extract `beamtalk_pid:is_alive/1` from the three ad-hoc copies and route all
`beamtalk_actor` guards through it; `lookup_class/1` uses the object record for
remote pids; map `{nodedown, N}`/`noconnection` to `node_down` and add
`node_down`/`remote_code_mismatch` to `kind_to_class/1` and the `.hrl` kind
list. Add a `peer`-based (OTP 25+) two-node EUnit/CT fixture. Raw remote pids
(via FFI) work end-to-end at the end of this phase.
*Files:* `beamtalk_actor.erl`, `beamtalk_announcements.erl`,
`beamtalk_inspector.erl`, `beamtalk_object_class.erl`,
`beamtalk_exception_handler.erl`, `beamtalk.hrl`, new `beamtalk_pid.erl`.

**Phase 1 — `Node` class and cluster events (M).**
`stdlib/src/node.bt` (`native:` backing `beamtalk_node.erl`), `Pid>>node`,
`Actor>>node`/`isRemote`; `beamtalk_node_monitor` under
`beamtalk_runtime_sup`; `NodeUp`/`NodeDown` announcement classes; the §9
`insecure_distribution` connect policy. BUnit tests in `stdlib/test/node_test.bt`
plus two-node runtime tests.

**Phase 2 — Remote spawn and lookup (M).**
`spawnOn:`, `spawnWith:on:`, `spawnAs:on:`, `spawnWith:as:on:`, `named:on:`,
`allRegisteredOn:` in `actor.bt`; `beamtalk_actor:remote_spawn/3` via `erpc`;
`{registered, Name, Node}` ref form and `?IS_REGISTERED_REF` update.
Depends on Phase 0.

**Phase 3 — The wire (L).**
New `beamtalk_wire.erl` (encode/decode, tagged message, reply symmetry);
`pack/2` wire/persist policy and `unpack_strict/1` in
`beamtalk_shape_migration.erl` (ADR 0125 §3.4, including the private
recursions at lines 507-517 and 684-690); registered-ref rewriting;
`handleScope: #node` on `Ets`, `AtomicCounter`, `Timer`; `badfun`/`undef` →
`remote_code_mismatch`; dispatch prelude recognising the wire tag; two-node
tests for version-ahead, migrate-forward, `late` slot absence, NLR relay.
Depends on Phases 0 and 2.

**Phase 4 — Connect-time shape negotiation (S/M).**
`NodeShapeSkew`, manifest exchange in `beamtalk_node_monitor`, `Node>>shapeManifest`.
Depends on Phase 1 and ADR 0125's `Beamtalk shapeManifest`.

**Phase 5 — Cluster scope (M).**
`scope: #cluster` selectors via `global`; `{global, Name}` refs;
`reserved_name/1` applied; `#globalNameConflict` stop reason.
Depends on Phase 2.

**Phase 6 — Known-remote diagnostics (M).**
Provenance fact in `crates/beamtalk-core/src/semantic_analysis/type_checker/`
(flow-local); extend `validation.rs` actor-message checks and
`sendability_validators.rs::check_block_captures` with the known-remote rows
of §6. Tests in the type-checker suite.

**Phase 7 — Tooling and docs (M).**
`ProcessNavigation on:`, `SupervisionNode node`, inspector `node` field,
`nodes` surface op (REPL/MCP/LiveView) and `surface-parity.md`;
`docs/beamtalk-language-features.md` § Distribution; ADR 0097 hidden-node
follow-up issue.

**Conformance.** The wire tag and envelope cross Rust ↔ Erlang only via
`beamtalk_shape_chain`/`beamtalk_shape_migration` (Erlang-only — the compiler
emits `__beamtalk_meta` `shape_migrations`, which ADR 0123 already covers). The
new error kinds follow the existing `.hrl` ↔ `kind_to_class/1` pattern. No new
cross-language table is introduced.

## Migration Path

No existing Beamtalk code changes behaviour for local sends. Two observable
changes for code already using distribution via FFI:

1. A partition during a sync send now raises `node_down` instead of
   `actor_dead`. Code matching `#actor_dead` to detect unreachability should
   also match `#node_down`.
2. `Ets`, `AtomicCounter` and `Timer` values sent to a remote actor now raise
   `not_serialisable` in the sender instead of arriving as dangling handles.

## References
- Related issues: BT-3527 (this ADR), BT-3524 (versioned state), BT-3525
  (slots), BT-3528 (OTP releases), BT-2530 (announcements remote-pid fix),
  BT-3536 (shape chain/migration modules), BT-3574 (release shape preflight)
- Related ADRs: 0020 (connection security), 0043 (sync-by-default
  messaging), 0058 (platform security), 0059 / 0092 (supervision + introspection),
  0060 (Result), 0069 (observability), 0079 (named registration), 0091
  (attach), 0093 (announcements), 0095 (inspector), 0097 (desktop attach),
  0101 (native objects), 0103 (sendability), 0104 (typed actor protocols),
  0110 (NLR relay), 0114 (rename), 0123 (versioned state), 0124 (slots),
  0125 (OTP releases)
- Documentation: `docs/beamtalk-language-features.md` § Actor Message Passing,
  § Named Actor Registration, § Passing Blocks Through Class Methods, § Erlang FFI;
  `docs/development/surface-parity.md`
- External: Erlang `global`, `pg`, `net_kernel:monitor_nodes/2`, `erpc`,
  `peer`; Akka Cluster membership and serialization docs; Orleans virtual
  actors; E language near/far references

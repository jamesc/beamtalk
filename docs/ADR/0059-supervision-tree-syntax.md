# ADR 0059: Supervision Tree Syntax

## Status
Proposed (2026-03-07)

## Context

### The Problem

Beamtalk's Principle 10 states: "Embrace BEAM's 'let it crash' philosophy — actors crash independently, the supervisor restarts them." But today, supervision configuration requires Erlang FFI. There is no Beamtalk syntax for declaring which actors should be supervised, what restart strategy to use, or how to compose a supervision tree.

ADR 0015 deferred this as future work with the explicit note: "A declarative supervision tree DSL (custom restart strategies, supervision hierarchies) is planned but not yet available from Beamtalk syntax (tracked BT-448)."

The gap leaves Smalltalk developers without a bridge from their familiar `on:do:` / resume-based thinking to BEAM's let-it-crash model. The feature works at the OTP level — the `beamtalk_runtime_sup` and `beamtalk_subprocess_sup` supervisors already exist — but it is invisible to Beamtalk code.

### Current State

Actors are spawned unsupervised by default:

```beamtalk
counter := Counter spawn.
counter increment.
// If counter crashes, the process is gone — no automatic restart.
```

The runtime has two hand-written OTP supervisors:
- `beamtalk_runtime_sup` — one_for_one, supervises bootstrap/stdlib/instances + `beamtalk_subprocess_sup`
- `beamtalk_subprocess_sup` — simple_one_for_one/temporary, started by `beamtalk_actor.erl`'s `start_link_supervised/3`

There is no way to define a user-level supervision tree from `.bt` files.

### How Class-Level Declarations Work Today (ClassBuilder, ADR 0038)

`state:` declarations in a class body are grammar productions that codegen compiles to ClassBuilder messages in the generated module's on_load:

```erlang
%% Object subclass: Counter  state: count = 0
CB = Supervisor classBuilder,
CB name: 'Counter';
   fields: #{count => 0};
   methods: #{...};
   register
```

`strategy:`, `maxRestarts:`, `restartWindow:`, `children:`, and `supervisionPolicy:` follow the **identical mechanism**: new class-level declaration keywords parsed from the class body, compiled by codegen to ClassBuilder messages. No new syntax mechanism is required — this is an extension of the existing ClassBuilder pattern.

### Constraints

1. **OTP supervisor behaviour** — OTP `supervisor` requires `init/1` to return `{ok, {SupFlags, ChildSpecs}}`. SupFlags is `#{strategy, intensity, period}`. Each ChildSpec is `#{id, start => {Module, Function, Args}, restart, shutdown, type, modules}`.
2. **Supervisor is not a gen_server** — A supervisor process handles child lifecycle, not user messages. `Supervisor subclass:` must generate `supervisor` behaviour, not `gen_server` behaviour.
3. **Actor `spawn` vs Supervisor `start`** — Actors use `spawn`/`spawnWith:` for instantiation (ADR 0042). Supervisors use `start`/`start:` for starting the supervision tree. These are semantically distinct.
4. **`@native` actors** — `@native`-backed actors (ADR 0056) are standard gen_servers from OTP's perspective and can be supervised identically to generated actors.
5. **No new syntax mechanism** — Supervision metadata compiles through the existing ClassBuilder protocol (ADR 0038). The parser gains new class-level declaration forms; codegen emits ClassBuilder messages; ClassBuilder's `register` handles the new metadata.
6. **Retry and fallback patterns** — `retryTimes:onError:backoff:` and `valueOrDefault:onError:` are block-level error recovery patterns, not supervision tree declarations. They are deferred to a separate ADR.

## Decision

### Core Design: Option B — `Supervisor subclass:` with Actor-Side Defaults

Two complementary declarations:

1. **`Supervisor subclass:`** — declares a supervision tree with a restart strategy and a list of child classes
2. **`supervisionPolicy:`** on an `Actor subclass:` — declares the actor's default restart type

Children are listed by class name. The child's `supervisionPolicy:` provides the restart default; the supervisor body can override it.

### `Supervisor subclass:` — Declaring a Supervision Tree

```beamtalk
Supervisor subclass: WebApp
  strategy: #oneForOne
  maxRestarts: 5
  restartWindow: 60
  children: #(DatabasePool, HTTPRouter, MetricsCollector)
```

Class-level declarations in the supervisor body:

| Declaration | OTP mapping | Default |
|---|---|---|
| `strategy: Symbol` | `SupFlags.strategy` — `#oneForOne`, `#oneForAll`, `#restForOne` | `#oneForOne` |
| `maxRestarts: Integer` | `SupFlags.intensity` | `10` |
| `restartWindow: Integer` (seconds) | `SupFlags.period` | `60` |
| `children: ArrayLiteral` | `ChildSpecs` list | required |

Children are Actor class references. Each entry is either a bare class name (uses `supervisionPolicy:` default from that class) or a class name with a `restart:` override:

```beamtalk
Supervisor subclass: WebApp
  strategy: #oneForOne
  maxRestarts: 5
  restartWindow: 60
  children: #(
    DatabasePool,                          // uses DatabasePool supervisionPolicy: #permanent
    HTTPRouter restart: #transient,        // override for this supervisor
    MetricsCollector restart: #temporary   // override for this supervisor
  )
```

Supervisor classes support class-side methods in the body like any other class:

```beamtalk
Supervisor subclass: WebApp
  strategy: #oneForOne
  maxRestarts: 5
  restartWindow: 60
  children: #(DatabasePool, HTTPRouter, MetricsCollector)

  /// Start the supervision tree and return the supervisor pid.
  class start => self supervise
```

### `supervisionPolicy:` on Actor — Default Restart Type

Each Actor optionally declares its supervision default:

```beamtalk
Actor subclass: DatabasePool
  supervisionPolicy: #permanent   // always restart — a crashed pool must come back
  state: pool = nil

Actor subclass: HTTPRouter
  supervisionPolicy: #transient   // restart only on abnormal exit

// Actor subclass: MetricsCollector
//   no supervisionPolicy: — defaults to #temporary (OTP default: do not restart)
```

Valid values: `#permanent` (always restart), `#transient` (restart on abnormal exit only), `#temporary` (never restart). These map directly to OTP child spec `restart` values.

`supervisionPolicy:` is a class-level declaration, not an instance variable. It compiles to a ClassBuilder message and is stored as class metadata, readable via `ClassName supervisionPolicy`.

### Starting and Inspecting Supervisors

Supervisors use `supervise` (class-side) to start the tree:

```beamtalk
app := WebApp supervise.
```

`supervise` is a class-side method synthesized by the compiler on all `Supervisor subclass:` definitions (like `spawn` is synthesized for `Actor subclass:`).

Supervisor instances respond to inspection messages:

```beamtalk
app children.                       // => #(DatabasePool HTTPRouter MetricsCollector)
app restartCount: DatabasePool.     // => 0
app which: DatabasePool.            // => #Actor<DatabasePool, <0.201.0>>
app terminate: DatabasePool.        // gracefully stop the child
```

These are message sends to the supervisor pid — the compiler generates `handle_call/3` clauses for each.

### Codegen: What Gets Generated

For `Supervisor subclass: WebApp`:

The compiler generates an OTP `supervisor` module (`bt@webapp`) instead of a `gen_server` module:

```erlang
%% Generated: bt@webapp.erl (Erlang notation for clarity)
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export(['supervise'/0, 'children'/1, 'restartCount:'/2, 'which:'/2, 'terminate:'/2]).

%% Class-side 'supervise' — starts the supervision tree
'supervise'() ->
    case supervisor:start_link(?MODULE, []) of
        {ok, Pid} -> {'beamtalk_object', 'WebApp', ?MODULE, Pid};
        {error, Reason} -> beamtalk_error:raise(...)
    end.

%% OTP supervisor callback
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 60},
    ChildSpecs = [
        #{id => 'DatabasePool',
          start => {'bt@databasepool', 'spawn', []},
          restart => permanent,       %% from DatabasePool supervisionPolicy: #permanent
          shutdown => 5000,
          type => worker,
          modules => ['bt@databasepool']},
        #{id => 'HTTPRouter',
          start => {'bt@httprouter', 'spawn', []},
          restart => transient,       %% override: restart: #transient in Supervisor body
          shutdown => 5000,
          type => worker,
          modules => ['bt@httprouter']},
        #{id => 'MetricsCollector',
          start => {'bt@metricscollector', 'spawn', []},
          restart => temporary,       %% override: restart: #temporary in Supervisor body
          shutdown => 5000,
          type => worker,
          modules => ['bt@metricscollector']}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% Instance-side inspection methods (called via beamtalk_actor:sync_send)
'children'(Self) ->
    Pid = element(4, Self),
    [Id || {Id, _, _, _} <- supervisor:which_children(Pid)].

'restartCount:'(Self, Class) ->
    Pid = element(4, Self),
    %% read from supervisor stats
    ...
```

The ClassBuilder on_load for `WebApp` registers it as a `Supervisor` subclass. ClassBuilder's `register` detects the `Supervisor` superclass and routes to supervisor codegen rather than gen_server codegen.

### REPL Session

```
> app := WebApp supervise.
#Supervisor<WebApp, <0.200.0>>

> app children.
#(DatabasePool HTTPRouter MetricsCollector)

> app which: DatabasePool.
#Actor<DatabasePool, <0.201.0>>

> app restartCount: DatabasePool.
0

> DatabasePool supervisionPolicy.
#permanent

> HTTPRouter supervisionPolicy.
#transient
```

### Error Cases

```
> DatabasePool start.
RuntimeError: 'DatabasePool' is an Actor, not a Supervisor.
  Hint: Use 'spawn' to create an actor instance; use 'supervise' to start a supervision tree.

> WebApp spawn.
RuntimeError: 'WebApp' is a Supervisor, not an Actor.
  Hint: Use 'supervise' to start the supervision tree.

> Supervisor subclass: BadSup  strategy: #unknown  children: #(DatabasePool)
error: unknown supervision strategy '#unknown'
  --> BadSup.bt:2
  |
2 |   strategy: #unknown
  |             ^^^^^^^^
  |
  = help: valid strategies are #oneForOne, #oneForAll, #restForOne
```

```
> Supervisor subclass: EmptySup
error: supervisor 'EmptySup' must declare at least one child
  --> EmptySup.bt:1
  |
1 | Supervisor subclass: EmptySup
  |                      ^^^^^^^^
  = help: add a 'children:' declaration with at least one Actor class
```

### Nested Supervisors

Supervisors can appear as children of other supervisors. The compiler detects that a named child is a `Supervisor subclass:` and sets `type => supervisor` in the child spec:

```beamtalk
Supervisor subclass: AppSup
  strategy: #oneForOne
  children: #(
    DatabasePool,
    WebSup           // WebSup is itself a Supervisor subclass: — type => supervisor
  )

Supervisor subclass: WebSup
  strategy: #oneForAll
  children: #(HTTPRouter, WebsocketRouter)
```

### `@native` Actors as Children

`@native`-backed actors (ADR 0056) are standard gen_servers. They participate in supervision identically to generated actors:

```beamtalk
Supervisor subclass: IOSup
  strategy: #oneForOne
  children: #(
    Subprocess restart: #temporary,   // @native actor — temporary, not restarted
    TranscriptStream                  // @native actor — uses its supervisionPolicy:
  )
```

The child spec's `start` MFA for a `@native` actor calls the facade's `spawn/1` (which calls `BackingModule:start_link/1`), consistent with how `@native` actors start normally.

## Prior Art

### Elixir — `use Supervisor` + `child_spec/1`

```elixir
defmodule WebApp.Supervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      DatabasePool,       # uses DatabasePool.child_spec/1 for defaults
      {HTTPRouter, port: 4000},
      MetricsCollector
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule DatabasePool do
  use GenServer

  def child_spec(opts) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, [opts]}, restart: :permanent}
  end
end
```

**What we adopted:** The separation of concerns — each module declares its own `child_spec` defaults; the supervisor just names children. Per-supervisor overrides are expressed by wrapping the child module in a tuple. This is the proven pattern for large Elixir codebases.

**What we adapted:** `child_spec/1` is a function; in Beamtalk, `supervisionPolicy:` is a class-level declaration (same mechanism as `state:`). The override syntax (`ClassName restart: #transient` in the children list) is more Smalltalk-idiomatic than Elixir's `{Module, opts}` tuple.

**What we improved:** No `def init/1` boilerplate — the `strategy:`, `maxRestarts:`, `restartWindow:`, `children:` declarations replace the imperative `Supervisor.init/2` call.

### OTP Erlang — Full Spec in Supervisor

```erlang
init([]) ->
    ChildSpecs = [
        #{id => db_pool,
          start => {db_pool, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [db_pool]},
        #{id => http_router,
          start => {http_router, start_link, [4000]},
          restart => transient,
          ...}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 60}, ChildSpecs}}.
```

**What we adopted:** The `SupFlags` and `ChildSpec` structures map directly to the `strategy:`/`maxRestarts:`/`restartWindow:` + `children:` declarations.

**What we improved:** The declarative class-body syntax replaces the verbose map construction. Child spec defaults from `supervisionPolicy:` eliminate per-supervisor boilerplate.

### Pharo Smalltalk — No Built-In Supervision

Pharo has no supervision primitive. Concurrency is `[block] fork` (a raw process). Fault tolerance is via `on:do:` handlers. If a process crashes, it is not automatically restarted — the application must implement its own restart logic.

**Why this doesn't work on BEAM:** Pharo's error model and BEAM's are fundamentally different. Pharo has continuations and the `resume:` protocol; BEAM has process isolation and supervisor restart. BEAM's model scales to millions of processes with hardware-level fault tolerance; Pharo's model requires defensive programming in every `on:do:` handler. The Beamtalk design (ADR 0015: "Why Supervision Is Better") explicitly embraces BEAM's model.

**What we adopted:** The Smalltalk principle that "the user shouldn't have to write OTP boilerplate." `Supervisor subclass: WebApp` reads like a Smalltalk class definition, not like OTP configuration.

### Gleam — `gleam_otp` Functional Builder

```gleam
pub fn init(_args) -> supervisor.Spec {
  supervisor.new(supervisor.OneForOne)
  |> supervisor.add(supervisor.worker_child("db", fn(_) { database.start() }))
  |> supervisor.add(supervisor.worker_child("http", fn(_) { http.start() }))
}
```

**What we noted:** The functional builder/pipeline pattern is idiomatic for Gleam (no classes, no class bodies). It is not idiomatic for a Smalltalk-inspired language where class bodies are the natural place for declarations.

**What we rejected:** Pipeline syntax for supervision structure. Beamtalk's class body declarations are more discoverable (the structure is right there in the class definition), static-analysis friendly (the compiler sees the children list at compile time), and consistent with how `state:` and `supervisionPolicy:` work.

### Newspeak — Module Nesting as Supervision Hierarchy

Newspeak's module system provides isolation and injected dependencies, but no built-in supervision. Actors in Newspeak are similar to BEAM processes but the restart model is not addressed at the language level.

**What we noted:** Newspeak's approach to concurrent hierarchies (nested modules with injected actors) is philosophically interesting but not applicable here. We need OTP-style restart semantics, not module nesting.

### Akka (Scala/JVM) — Actor Hierarchy with Parent Supervision

```scala
class WebApp extends Actor {
  val dbPool = context.actorOf(DatabasePool.props(), "db")
  val httpRouter = context.actorOf(HTTPRouter.props(), "http")

  override def supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5) {
    case _: DatabaseException => Restart
    case _: Exception => Stop
  }
}
```

**What we adopted:** The concept of a supervisor as a class that declares a strategy and names children. `OneForOneStrategy` maps to `strategy: #oneForOne`.

**What we rejected:** Akka's supervision strategy is a per-message-type decision (each exception class gets a different response). This is powerful but complex. Beamtalk uses OTP's simpler per-child restart type (`permanent`/`transient`/`temporary`), which is proven at production scale and requires no user-defined exception matching logic.

### Summary

| Feature | Elixir | Erlang OTP | Pharo | Gleam | Beamtalk |
|---|---|---|---|---|---|
| Declarative supervisor | ✅ class | ✅ init/1 | ❌ none | ✅ builder | **✅ class body** |
| Child defaults from actor | ✅ child_spec/1 | ❌ all in supervisor | N/A | ❌ inline | **✅ supervisionPolicy:** |
| Per-supervisor overrides | ✅ {Module, opts} | ✅ always explicit | N/A | ✅ inline | **✅ restart: in children:** |
| Restart strategies | ✅ 3 strategies | ✅ 3 strategies | ❌ | ✅ 3 strategies | **✅ 3 strategies** |
| REPL inspection | ✅ via Erlang | ✅ via Erlang | ❌ | ✅ via Erlang | **✅ message sends** |

## User Impact

### Newcomer (from Python/JS/Ruby)

**Positive:** `Supervisor subclass: WebApp` reads like any other class definition. The pattern is discoverable: "what's a Supervisor? It's a class that watches other classes and restarts them." The `supervisionPolicy: #permanent` declaration on an Actor is self-documenting — "always restart this."

**Positive:** REPL inspection (`app children`, `app restartCount: DatabasePool`) makes the live supervision tree introspectable — consistent with Beamtalk's interactive-first design.

**Concern:** The concept of restart strategies (`#permanent`/`#transient`/`#temporary`) is OTP-specific and not immediately intuitive. Clear documentation and error messages are important. A newcomer who omits `supervisionPolicy:` gets `#temporary` by default — actors that crash are not restarted, which may be surprising.

**Mitigation:** The compiler should emit a warning when an Actor subclass is listed as a child in a `Supervisor` without a `supervisionPolicy:` declaration and without an explicit `restart:` override, noting that the default is `#temporary` (not restarted on crash).

### Smalltalk Developer

**Positive:** `Supervisor subclass: WebApp` is syntactically identical to `Actor subclass: Counter` — class creation looks like class creation. The supervision tree is a class, not an OTP configuration ceremony. This bridges the Smalltalk mental model to BEAM's fault tolerance primitives.

**Positive:** `supervisionPolicy:` on an Actor is a natural property of the Actor class, not an external configuration. Smalltalk developers are accustomed to objects describing their own behavior.

**Neutral:** No `resume:` / `retry` support. ADR 0015 explains why resumption is deferred: BEAM doesn't support continuations, and supervision-based restart is the BEAM-idiomatic equivalent. The Supervisor DSL makes this trade-off visible and usable rather than leaving it as "go write Erlang."

### Erlang/BEAM Developer

**Positive:** The generated `supervisor` module is standard OTP. The `init/1` callback is familiar. The `SupFlags` and `ChildSpecs` structures map exactly to the `strategy:`/`children:` declarations. No surprises in Observer, `:sys` tracing, or crash logs.

**Positive:** `@native` actors participate in supervision without ceremony — they are gen_servers from OTP's perspective, and the generated child spec uses the facade's `spawn/1` as the start MFA.

**Concern:** The `restart:` override syntax in the children list (`DatabasePool restart: #temporary`) desugars to a keyword message send at parse time, not a runtime message. This may look like a runtime operation to an Erlang developer familiar with data construction. The distinction is clear in context but worth documenting.

### Production Operator

**Positive:** The generated supervisor is standard OTP and fully observable: `supervisor:which_children/1`, `observer:start()`, `:sys.get_state/1`, and `recon` all work. No new observability primitives needed.

**Positive:** `maxRestarts:` and `restartWindow:` are explicit, not hidden defaults. The compiler generates `{ok, {#{strategy, intensity, period}, ...}}` — the exact values from the class body, no magic.

**Neutral:** The `shutdown` timeout (OTP `shutdown` in child spec) defaults to `5000ms`. A future enhancement could add `shutdown:` as a per-child declaration. For now, BEAM interop (`supervisor:terminate_child/2`) is available for custom shutdown behaviour.

### Tooling Developer (LSP/IDE)

**Positive:** `supervisionPolicy:` is a class-level declaration parsed from source — the LSP can read it from the static `ClassHierarchy` without a live workspace. Completions for `strategy:` values (`#oneForOne`, `#oneForAll`, `#restForOne`) are statically known.

**Positive:** The compiler validates children at compile time (class must exist and be an `Actor subclass:` or `Supervisor subclass:`). The LSP can surface "DatabasePool is not an Actor" errors inline.

**Neutral:** Supervision tree visualization (a tree view in the IDE showing the supervisor hierarchy) is possible but deferred. The static information (class hierarchy) is available from source; live information (actual pids, restart counts) requires a running workspace.

## Steelman Analysis

### Option A: Full Specs in `Supervisor subclass:` Body (rejected)

All child restart types live in the Supervisor, not the Actor:

```beamtalk
Supervisor subclass: WebApp
  strategy: #oneForOne
  children: #(
    DatabasePool restart: #permanent,
    HTTPRouter restart: #transient,
    MetricsCollector restart: #temporary
  )
```

| Cohort | Strongest argument |
|---|---|
| 🧑‍💻 **Newcomer** | "Everything I need to understand the supervision structure is in one file. I don't have to look at DatabasePool.bt to know it's permanent here." |
| 🎩 **Smalltalk purist** | "The Supervisor class is self-contained. No coupling between the Supervisor and the Actor's source file — I can change restart behavior without touching the Actor." |
| ⚙️ **BEAM veteran** | "This maps exactly to OTP: `init/1` owns all the specs. That's idiomatic Erlang. The supervisor, not the worker, declares restart behavior — because different supervisors may want different behavior for the same worker." |
| 🏭 **Operator** | "I can audit the restart topology by reading one file. No cross-file resolution needed." |
| 🎨 **Language designer** | "No cross-file compile-time dependency. The compiler doesn't need to read `DatabasePool.bt` to compile `WebApp.bt`." |

**Why Option B wins:** The operator/BEAM veteran argument is valid but cuts both ways — `DatabasePool` IS always permanent. It's a database pool. Any supervisor that includes it should restart it on crash. Having every supervisor that uses `DatabasePool` redeclare `restart: #permanent` is boilerplate and a source of bugs (forgetting the override). Option B makes the default correct; Option A makes repetition mandatory. The override-at-supervisor mechanism still exists in Option B for the cases where you genuinely want a different policy.

### Option C: `@supervisor` Annotation on Actor (rejected)

```beamtalk
@supervisor strategy: #oneForOne maxRestarts: 5 restartWindow: 60
Actor subclass: WebApp
  children: #(DatabasePool, HTTPRouter, MetricsCollector)
```

| Cohort | Strongest argument |
|---|---|
| ⚙️ **BEAM veteran** | "Erlang doesn't have a 'Supervisor' base type — you just use the `supervisor` behaviour. `@supervisor` mirrors that: declare the behaviour, the annotation does the work." |
| 🎨 **Language designer** | "The class hierarchy stays clean. `Supervisor` as a base class adds an entry to the class hierarchy for a concept that is really an OTP behaviour, not a Beamtalk-level abstraction." |
| 🧑‍💻 **Newcomer** | "I see one class hierarchy: everything extends Actor or Object. `@supervisor` is like `@native` — it says 'this is special' without creating a parallel hierarchy." |

**Why it is rejected:** A supervisor is fundamentally not an Actor (it does not receive user messages; it watches other processes). `@supervisor` on `Actor subclass:` buries a critical semantic difference in an annotation that looks like an implementation detail — the same visual weight as `@native` or `@primitive`. The `Supervisor` base class makes the distinction explicit and self-documenting. New users reading the Beamtalk stdlib will immediately see that `Supervisor subclass: WebApp` is a different thing from `Actor subclass: Counter` — just as `Actor subclass:` is clearly different from `Object subclass:`. The `@annotation` pattern (ADR 0055, ADR 0056) is reserved for implementation details; `Supervisor` is a first-class language concept.

### Tension Points

- **One-file vs two-file visibility:** Option A operators want the full restart topology in one file; Option B developers want actors to own their restart semantics. The compiler warning for missing `supervisionPolicy:` (with explicit defaults) reduces the Option A advantage — a developer reading `WebApp.bt` can always see the effective defaults by checking the Actor files.
- **`Supervisor` in the class hierarchy:** Adding `Supervisor` to `ProtoObject → Object → Actor` is a real cost. Option C avoids it. But the semantic clarity justifies it — supervision trees are a first-class abstraction in BEAM programming, not an implementation detail.

## Alternatives Considered

### Option A: Full Specs in Supervisor Body

All restart types declared in the `children:` list; `supervisionPolicy:` on Actor is not added.

**Rejected because:** Forces every Supervisor that uses `DatabasePool` to declare `restart: #permanent` — this is boilerplate that belongs on the Actor. Elixir's `child_spec/1` pattern proved that actor-owned defaults reduce errors and make supervisors leaner.

### Option C: `@supervisor` Annotation on Actor

`@supervisor` annotation on `Actor subclass:` generates supervisor behaviour instead of gen_server.

**Rejected because:** Obscures a first-class semantic distinction (`Supervisor` vs `Actor`) behind an annotation. The `@annotation` pattern is for implementation details (`@native`, `@primitive`); supervision tree membership is a language-level abstraction deserving a base class. See Steelman Analysis.

### Option D: Pure Runtime Protocol (No New Syntax)

No new syntax. Users invoke Erlang supervision APIs via BEAM interop:

```beamtalk
(Erlang supervisor) start_link: WebAppSup module: beamtalk_supervisor args: childSpecs.
```

**Rejected because:** This is the current state — it requires Erlang FFI and leaves Principle 10 ("fault tolerance without Erlang boilerplate") unmet. The goal of this ADR is to make supervision trees expressible in Beamtalk.

## Consequences

### Positive

- Principle 10 fully satisfied: supervision tree syntax available in pure Beamtalk
- `Supervisor subclass:` reads like any other Beamtalk class definition — no OTP ceremony
- `supervisionPolicy:` on Actors makes restart semantics self-documenting and discoverable
- No new syntax mechanism: extends the ClassBuilder protocol (ADR 0038) with new declaration keywords
- Generated supervisor modules are standard OTP — fully observable with existing BEAM tools
- REPL inspection via message sends (`app children`, `app restartCount: ClassName`) is consistent with Beamtalk's interactive-first design
- `@native` actors participate in supervision without special handling
- Static analysis: the compiler validates children at compile time (class must be Actor or Supervisor subclass, strategy must be a valid symbol)

### Negative

- `Supervisor` joins the bootstrap class hierarchy: `ProtoObject → Object → Actor → [user actors]` and `ProtoObject → Object → Supervisor → [user supervisors]`. One more class to bootstrap before user modules load.
- New class-level declaration keywords: `strategy:`, `maxRestarts:`, `restartWindow:`, `children:`, `supervisionPolicy:` must be added to the parser's class body grammar and to ClassBuilder.
- Cross-file compile dependency: to compile `WebApp.bt`, the compiler must know the `supervisionPolicy:` of each listed child class (to fill in restart defaults). This is a new dependency in the compilation graph. If the child class hasn't been compiled yet, the compiler uses `#temporary` and emits a warning.
- `shutdown:` per-child configuration is not in scope for this ADR. The default of `5000ms` is used for all children. Production systems needing custom shutdown timeouts must use BEAM interop.
- No dynamic supervisor (no equivalent of `supervisor:start_child/2` at runtime from Beamtalk syntax). Adding children dynamically is deferred.

### Neutral

- Retry patterns (`retryTimes:onError:backoff:`) and fallback patterns (`valueOrDefault:onError:`) are block-level error recovery, not supervision tree declarations. They are deferred to a separate ADR and are explicitly out of scope here.
- `Supervisor` instances are OTP supervisor processes, not gen_servers — `beamtalk_actor:sync_send/3` is not used for inspection messages. Instead, the generated supervisor module directly wraps `supervisor:which_children/1` etc. in its exported functions, called via `gen:call/4` (same wire protocol, different dispatch path).
- The `start` class-side method name is a convention, not enforced by the compiler. The required method is `supervise`, which the compiler synthesizes. `start` is just an optional alias that classes may define.

## Implementation

### Phase 0 — Supervisor Class in Bootstrap (S)

Add `Supervisor` as a new base class in the bootstrap sequence:

- `beamtalk_bootstrap.erl` — add `Supervisor` after `Actor` in the bootstrap sequence
- `stdlib/src/Supervisor.bt` — minimal class with `supervise` class-side method stub
- `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs` — add `Supervisor` as a known superclass (alongside `Actor`, `Object`, etc.)
- `beamtalk_object_class.erl` — `Supervisor` subclasses detected and routed to supervisor codegen

**Goal:** `Supervisor subclass: MySup` parses and registers without error. No `init/1` generated yet — just the class registration.

### Phase 1 — Parser and ClassBuilder Extensions (M)

**Parser (`crates/beamtalk-core/src/source_analysis/parser/`):**
- Add `strategy:`, `maxRestarts:`, `restartWindow:`, `children:` as new class-level declaration forms in the grammar (alongside `state:`)
- Add `supervisionPolicy:` as a new class-level declaration form for `Actor subclass:` bodies
- Parse the `children:` array — each element is either a bare class name or a `ClassName restart: Symbol` keyword message

**AST (`crates/beamtalk-core/src/ast.rs`):**
- `ClassDefinition` gains `supervision_spec: Option<SupervisionSpec>` field
- `SupervisionSpec { strategy, max_restarts, restart_window, children: Vec<ChildEntry> }`
- `ChildEntry { class_name, restart_override: Option<Symbol> }`
- `ActorClass` gains `supervision_policy: Option<Symbol>` field

**ClassBuilder (`stdlib/src/ClassBuilder.bt`, `beamtalk_class_builder.erl`):**
- New messages: `strategy:`, `maxRestarts:`, `restartWindow:`, `children:`, `supervisionPolicy:`
- `register` detects `Supervisor` superclass and routes to supervisor module generation

**Semantic Analysis:**
- Validate `strategy:` is one of `#oneForOne`, `#oneForAll`, `#restForOne`
- Validate `children:` is non-empty
- Validate each child class exists in the class hierarchy and is an `Actor subclass:` or `Supervisor subclass:`
- Validate `supervisionPolicy:` is one of `#permanent`, `#transient`, `#temporary`
- Warn if a child Actor has no `supervisionPolicy:` and no override in the children list (default `#temporary`)

### Phase 2 — Supervisor Codegen (L)

**Codegen (`crates/beamtalk-core/src/codegen/core_erlang/`):**
- New `supervisor_codegen.rs` — generates OTP `supervisor` behaviour modules:
  - `start_link/0` — `supervisor:start_link(?MODULE, [])`
  - `init/1` — returns `{ok, {SupFlags, ChildSpecs}}` built from the `SupervisionSpec`
  - `supervise/0` class-side method — calls `start_link/0`, wraps result as `beamtalk_object`
  - Inspection methods: `children/1`, `restartCount:/2`, `which:/2`, `terminate:/2`
- Child spec resolution: for each child in `children:`, look up `supervisionPolicy:` from the child class's compiled metadata; use override if present
- `beamtalk-core/src/codegen/core_erlang/actor_codegen.rs` — detect `Supervisor` superclass in `ClassDefinition` and branch to `supervisor_codegen.rs`

**Affected components (full list):**
- `crates/beamtalk-core/src/source_analysis/lexer.rs` — lex `strategy`, `maxRestarts`, `restartWindow`, `children`, `supervisionPolicy` as class-body keyword tokens
- `crates/beamtalk-core/src/source_analysis/parser/` — parse new class-level declarations
- `crates/beamtalk-core/src/ast.rs` — `SupervisionSpec`, `ChildEntry`, `supervision_policy` on actor class
- `crates/beamtalk-core/src/semantic_analysis/` — validation pass for supervision spec
- `crates/beamtalk-core/src/codegen/core_erlang/supervisor_codegen.rs` — new file
- `crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs` — routing to supervisor codegen
- `stdlib/src/Supervisor.bt` — Supervisor base class with `supervise` and inspection methods
- `beamtalk_class_builder.erl` — new messages + supervisor register path
- `runtime/apps/beamtalk_runtime/src/beamtalk_bootstrap.erl` — add `Supervisor` to sequence

### Phase 3 — Tests and Docs (M)

- `stdlib/test/SupervisorTest.bt` — BUnit tests: start a supervision tree, verify children, crash a child and verify restart, verify `supervisionPolicy:` on Actor
- `tests/e2e/cases/supervisor.bt` — REPL integration: `WebApp supervise`, `app children`, `app which: ClassName`
- `docs/beamtalk-language-features.md` — add Supervision Tree section after Actor Message Passing
- `docs/beamtalk-principles.md` — update Principle 10 from "planned but not available" to "available via `Supervisor subclass:`"

## References

- Related issues: BT-448
- Related ADRs: ADR 0009 (OTP Application Structure), ADR 0015 (Signal-Time Exceptions — supervision motivation and pseudocode), ADR 0038 (ClassBuilder Protocol — class-level declarations mechanism), ADR 0042 (Immutable Value Objects / Actor-Only Mutable State), ADR 0043 (Sync-by-Default Actor Messaging), ADR 0056 (`@native` Erlang-Backed Actors)
- OTP Supervisor: https://www.erlang.org/doc/man/supervisor.html
- Elixir Supervisor: https://hexdocs.pm/elixir/Supervisor.html
- Armstrong thesis — supervision trees: https://erlang.org/download/armstrong_thesis_2003.pdf

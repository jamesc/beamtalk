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

There is no way to define a user-level supervision tree from `.bt` files, neither static (children known at startup) nor dynamic (children added at runtime).

### Constraints

1. **OTP supervisor behaviour** — OTP `supervisor` requires `init/1` to return `{ok, {SupFlags, ChildSpecs}}`. SupFlags is `#{strategy, intensity, period}`. Each ChildSpec is `#{id, start => {Module, Function, Args}, restart, shutdown, type, modules}`.
2. **Supervisor is not a gen_server** — A supervisor process handles child lifecycle, not user messages. `Supervisor subclass:` must generate `supervisor` behaviour, not `gen_server` behaviour.
3. **`@native` actors** — `@native`-backed actors (ADR 0056) are standard gen_servers from OTP's perspective and can be supervised identically to generated actors.
4. **Retry and fallback patterns** — `retryTimes:onError:backoff:` and `valueOrDefault:onError:` are block-level error recovery patterns, not supervision tree declarations. They are deferred to a separate ADR.

## Decision

### Core Design: Abstract `Supervisor` with Method Overrides

`Supervisor` is an **abstract class** in the stdlib. Concrete supervisors subclass it and override methods to declare their structure. There are no special class-body declaration keywords — everything is a method.

The stdlib `Supervisor` base class:

```beamtalk
// stdlib/src/Supervisor.bt
abstract Object subclass: Supervisor
  class children      => subclassResponsibility
  class strategy      => #oneForOne
  class maxRestarts   => 10
  class restartWindow => 60
```

Concrete supervisors subclass it and override the methods they need:

```beamtalk
Supervisor subclass: WebApp
  class children => #(DatabasePool HTTPRouter MetricsCollector)
```

The `children` method returns an `Array` of Actor or Supervisor class references. The generated `init/1` calls `self children` at startup to build the OTP child specs. All other methods have sensible defaults and can be selectively overridden:

```beamtalk
Supervisor subclass: WebApp
  class strategy      => #oneForAll
  class maxRestarts   => 3
  class children      => #(DatabasePool HTTPRouter MetricsCollector)
```

Because `children` is a method, it can do anything — read environment variables, inspect feature flags, compose conditionally, delegate to helpers:

```beamtalk
Supervisor subclass: AppSup
  class children =>
    children := Array with: DatabasePool with: HTTPRouter.
    (Feature flagged: #metrics) ifTrue: [children := children copyWith: MetricsCollector].
    children

Supervisor subclass: MultiRegionSup
  class regions => #(#us-east #eu-west #ap-south)

  class children =>
    self regions collect: [:region |
      RegionalWorker supervisionSpec withId: region]
```

Forgetting to implement `children` raises a meaningful DNU at `supervise` time:

```text
> BrokenSup supervise.
DoesNotUnderstand: BrokenSup does not understand 'children'
  (Supervisor subclass: BrokenSup — override 'children' to return the child class list)
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

`supervisionPolicy:` is a class-level declaration, stored as class metadata, readable via `ClassName supervisionPolicy`.

### `supervisionSpec` on Actor — Building Child Specs

Every `Actor subclass:` gains a synthesized class-side method `supervisionSpec` that returns a `SupervisionSpec` value object describing how to start this actor as a supervised child. This is used inside `children` methods when per-child configuration is needed:

```beamtalk
// Default — id is class name, restart from supervisionPolicy:
DatabasePool supervisionSpec

// Fluent overrides for the cases that need them:
DatabasePool supervisionSpec withId: #primary
DatabasePool supervisionSpec withId: #replica withRestart: #transient
```

`SupervisionSpec` is a simple value type:

```beamtalk
Value subclass: SupervisionSpec
  state: id = nil
  state: actorClass = nil
  state: restart = #temporary
  state: shutdown = 5000
```

This is used inside a `children` method when the simple array-of-classes form isn't sufficient:

```beamtalk
Supervisor subclass: DBSup
  class children =>
    Array
      with: DatabasePool supervisionSpec withId: #primary
      with: (DatabasePool supervisionSpec withId: #replica withRestart: #transient)
```

For actor initialization that varies per instance, the actor is responsible for reading its own config (from class variables, environment, ETS, etc.) in its own init. The supervisor names the class; the actor handles its own startup.

### Dynamic Supervision — `strategy: #dynamic`

When the number of children isn't known at startup, override `strategy` to return `#dynamic` and implement `childClass`:

```beamtalk
Supervisor subclass: WorkerPool
  class strategy   => #dynamic
  class childClass => Worker
```

`childClass` returns the Actor class for dynamic children. The generated `init/1` builds the `simple_one_for_one` child spec template from it. Dynamic supervisors start with no children; `startChild:` and `terminateChild:` add and remove them at runtime:

```beamtalk
pool := WorkerPool supervise.

w1 := pool startChild: #{#config => "db-a"}.
w2 := pool startChild: #{#config => "db-b"}.
pool count.                // => 2

pool terminateChild: w1.
pool count.                // => 1
```

`startChild:` passes the dict to `Worker spawnWith:`. `startChild` (no arg) calls `Worker spawn`.

The `children` method is not used for dynamic supervisors — the codegen detects `strategy = #dynamic` and uses `childClass` instead.

**Codegen:** `strategy: #dynamic` generates `simple_one_for_one`. `startChild:` calls `supervisor:start_child/2`; `terminateChild:` calls `supervisor:terminate_child/2` with the child pid (`supervisor:delete_child/2` must **not** be called — OTP removes the entry automatically for `simple_one_for_one`).

```erlang
%% Generated: bt@workerpool.erl (simplified)
init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 60},
    ChildSpec = #{id => 'Worker', start => {'bt@worker', 'spawnWith:', []},
                  restart => temporary, shutdown => 5000, type => worker,
                  modules => ['bt@worker']},
    {ok, {SupFlags, [ChildSpec]}}.

'startChild:'(Self, Config) ->
    Pid = element(4, Self),
    case supervisor:start_child(Pid, [Config]) of
        {ok, ChildPid} -> {'beamtalk_object', 'Worker', 'bt@worker', ChildPid};
        {error, Reason} -> beamtalk_error:raise(...)
    end.
```

### Starting and Inspecting Supervisors

`supervise` is a class-side method synthesized by the compiler on all `Supervisor subclass:` definitions (like `spawn` is synthesized for `Actor subclass:`):

```beamtalk
app := WebApp supervise.
```

Supervisor instances respond to inspection messages. OTP supervisors cannot define custom `handle_call/3` clauses — the compiler generates exported module functions that call `supervisor:which_children/1` and related OTP APIs directly, invoked via `gen:call/4`:

```beamtalk
app children.                // => #(DatabasePool HTTPRouter MetricsCollector)
app which: DatabasePool.     // => #Actor<DatabasePool, <0.201.0>>
app terminate: DatabasePool. // gracefully stop the child
```

Note: per-child restart counts are not part of this API — OTP exposes no standard API for them. Use `observer:start()` or `supervisor:count_children/1` for aggregate diagnostics.

### Codegen: What Gets Generated

For `Supervisor subclass: WebApp` with `class children => #(DatabasePool HTTPRouter MetricsCollector)`:

```erlang
%% Generated: bt@webapp.erl
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export(['supervise'/0, 'children'/1, 'which:'/2, 'terminate:'/2]).

'supervise'() ->
    case supervisor:start_link(?MODULE, []) of
        {ok, Pid} -> {'beamtalk_object', 'WebApp', ?MODULE, Pid};
        {error, Reason} -> beamtalk_error:raise(...)
    end.

init([]) ->
    %% Calls class-side 'children' method to get the child list at startup.
    %% Reads supervisionPolicy from each child class for restart defaults.
    ChildClasses = 'bt@webapp':'children'(),
    SupFlags = #{strategy => 'bt@webapp':'strategy'(),
                 intensity => 'bt@webapp':'maxRestarts'(),
                 period => 'bt@webapp':'restartWindow'()},
    ChildSpecs = beamtalk_supervisor:build_child_specs(ChildClasses),
    {ok, {SupFlags, ChildSpecs}}.

%% Instance-side inspection — exported functions, not handle_call/3 clauses.
'children'(Self) ->
    Pid = element(4, Self),
    [Id || {Id, _, _, _} <- supervisor:which_children(Pid), Id =/= undefined].
```

`beamtalk_supervisor:build_child_specs/1` looks up `supervisionPolicy` for each child class (or uses the `restart` from a `SupervisionSpec` value), detects whether the child is a `Supervisor` subclass (setting `type => supervisor, shutdown => infinity`), and constructs the OTP child spec maps.

### REPL Session

Static supervisor:
```text
> app := WebApp supervise.
#Supervisor<WebApp, <0.200.0>>

> app children.
#(DatabasePool HTTPRouter MetricsCollector)

> app which: DatabasePool.
#Actor<DatabasePool, <0.201.0>>

> DatabasePool supervisionPolicy.
#permanent
```

Dynamic supervisor:
```text
> pool := WorkerPool supervise.
#Supervisor<WorkerPool, <0.210.0>>

> pool count.
0

> w1 := pool startChild: #{#config => "primary"}.
#Actor<Worker, <0.211.0>>

> pool count.
1
```

### Nested Supervisors

Supervisors appear in other supervisors' `children` method just like actors. The runtime detects that a class is a `Supervisor` subclass and sets `type => supervisor, shutdown => infinity` in the child spec. Default restart for a nested supervisor is `#permanent`:

```beamtalk
Supervisor subclass: AppSup
  class children => #(DatabasePool WebSup)  // WebSup is a Supervisor subclass

Supervisor subclass: WebSup
  class strategy => #oneForAll
  class children => #(HTTPRouter WebsocketRouter)
```

### `@native` Actors as Children

`@native`-backed actors (ADR 0056) are standard gen_servers and participate in supervision identically to generated actors. They appear in `children` by class name; the generated child spec uses the facade's `spawn/1`:

```beamtalk
Supervisor subclass: IOSup
  class children =>
    Array
      with: (Subprocess supervisionSpec withRestart: #temporary)
      with: TranscriptStream
```

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

**What we adopted:** The separation of concerns — each worker declares its own restart defaults (`supervisionPolicy:`); the supervisor just names children. This is the proven pattern for large Elixir codebases.

**What we did not adopt:** `child_spec/1` as a callable method on the worker. Beamtalk's `supervisionPolicy:` covers the common case (restart default); complex child spec construction lives in the supervisor's `children` method, not in the worker. The supervisor is the right place to know how many children it needs and in what configuration.

**What we improved:** No `def init/1` / `Supervisor.init/2` boilerplate. The `children` method IS `init/1` — expressed as a natural Beamtalk method override. Strategy and restart limits are method overrides with sensible defaults, not mandatory parameters.

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
| Declarative supervisor | ✅ class | ✅ init/1 | ❌ none | ✅ builder | **✅ method overrides** |
| Child defaults from actor | ✅ child_spec/1 | ❌ all in supervisor | N/A | ❌ inline | **✅ supervisionPolicy:** |
| Per-supervisor overrides | ✅ {Module, opts} | ✅ always explicit | N/A | ✅ inline | **✅ supervisionSpec withRestart:** |
| Restart strategies | ✅ 3 strategies | ✅ 3 strategies | ❌ | ✅ 3 strategies | **✅ 4 strategies** |
| REPL inspection | ✅ via Erlang | ✅ via Erlang | ❌ | ✅ via Erlang | **✅ message sends** |

## User Impact

### Newcomer (from Python/JS/Ruby)

**Positive:** `Supervisor subclass: WebApp` reads like any other class definition. The pattern is discoverable: "what's a Supervisor? It's a class that watches other classes and restarts them." The `supervisionPolicy: #permanent` declaration on an Actor is self-documenting — "always restart this."

**Positive:** REPL inspection (`app children`, `app which: DatabasePool`) makes the live supervision tree introspectable — consistent with Beamtalk's interactive-first design.

**Concern:** The concept of restart strategies (`#permanent`/`#transient`/`#temporary`) is OTP-specific and not immediately intuitive. Clear documentation and error messages are important. A newcomer who omits `supervisionPolicy:` gets `#temporary` by default — actors that crash are not restarted, which may be surprising.

**Mitigation:** The compiler should emit a warning when an Actor subclass is listed as a child in a `Supervisor` without a `supervisionPolicy:` declaration and without an explicit `restart:` override, noting that the default is `#temporary` (not restarted on crash).

### Smalltalk Developer

**Positive:** `Supervisor subclass: WebApp` is syntactically identical to `Actor subclass: Counter` — class creation looks like class creation. The supervision tree is a class, not an OTP configuration ceremony. This bridges the Smalltalk mental model to BEAM's fault tolerance primitives.

**Positive:** `supervisionPolicy:` on an Actor is a natural property of the Actor class, not an external configuration. Smalltalk developers are accustomed to objects describing their own behavior.

**Neutral:** No `resume:` / `retry` support. ADR 0015 explains why resumption is deferred: BEAM doesn't support continuations, and supervision-based restart is the BEAM-idiomatic equivalent. The Supervisor DSL makes this trade-off visible and usable rather than leaving it as "go write Erlang."

### Erlang/BEAM Developer

**Positive:** The generated `supervisor` module is standard OTP. The `init/1` callback is familiar. The `SupFlags` and `ChildSpecs` structures map exactly to the `strategy:`/`children:` declarations. No surprises in Observer, `:sys` tracing, or crash logs.

**Positive:** `@native` actors participate in supervision without ceremony — they are gen_servers from OTP's perspective, and the generated child spec uses the facade's `spawn/1` as the start MFA.

**Concern:** `supervisionSpec withRestart: #transient` looks like a regular message send (because it is one), but it is evaluated at `supervise` time, not once at class definition time. This is the standard Beamtalk / Smalltalk trade-off: method-based dispatch means the value is computed lazily, which is usually fine for startup logic but could surprise an Erlang developer expecting static init declarations.

### Production Operator

**Positive:** The generated supervisor is standard OTP and fully observable: `supervisor:which_children/1`, `observer:start()`, `:sys.get_state/1`, and `recon` all work. No new observability primitives needed.

**Positive:** `maxRestarts:` and `restartWindow:` are explicit, not hidden defaults. The compiler generates `{ok, {#{strategy, intensity, period}, ...}}` — the exact values from the class body, no magic.

**Neutral:** The `shutdown` timeout (OTP `shutdown` in child spec) defaults to `5000ms` for worker children and `infinity` for nested supervisor children (matching OTP's own defaults). A future enhancement could add `shutdown:` as a per-child declaration. For now, BEAM interop (`supervisor:terminate_child/2`) is available for custom shutdown behaviour.

### Tooling Developer (LSP/IDE)

**Positive:** `supervisionPolicy:` is a class-level declaration parsed from source — the LSP can read it from the static `ClassHierarchy` without a live workspace. Completions for `supervisionPolicy:` values (`#permanent`, `#transient`, `#temporary`) are statically known.

**Neutral:** `strategy`, `maxRestarts`, `restartWindow`, `children`, `childClass` are ordinary method overrides — the LSP can provide completions for method names (autocomplete `class children =>` etc.) but cannot validate the return type of `children` at edit time. The compiler flags `DNU: 'children'` at `supervise` time rather than statically. This is the standard trade-off of the method-based approach.

**Positive:** `Supervisor subclass:` is identifiable from source — the LSP can distinguish it from `Actor subclass:` and show different completions/hover text.

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

**Why the final design wins:** The operator/BEAM veteran argument is valid but cuts both ways — `DatabasePool` IS always permanent. It's a database pool. Any supervisor that includes it should restart it on crash. Having every supervisor that uses `DatabasePool` redeclare `restart: #permanent` is boilerplate and a source of bugs (forgetting the override). `supervisionPolicy:` on the Actor makes the default correct at the source; the override-at-supervisor mechanism (`supervisionSpec withRestart: #transient`) still exists for the cases where you genuinely want a different policy.

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

- **One-file vs two-file visibility:** Operators who want the full restart topology in one file prefer all specs in the `Supervisor`. The `supervisionPolicy:` design requires cross-file reading to determine effective restart behaviour. The compiler warning for missing `supervisionPolicy:` (with explicit defaults) reduces this tension — a developer reading `WebApp.bt` can always see the effective defaults by checking the Actor files.
- **`Supervisor` in the class hierarchy:** Adding `Supervisor` to `ProtoObject → Object → Supervisor → [user supervisors]` is a real cost. The `@supervisor` annotation alternative (Option C) avoids it. But the semantic clarity justifies it — supervision trees are a first-class abstraction in BEAM programming, not an implementation detail.

## Alternatives Considered

### Option A: Full Specs in Supervisor Body

All restart types declared inline in the `children` method; `supervisionPolicy:` on Actor is not added.

```beamtalk
Supervisor subclass: WebApp
  class children => Array
    with: (DatabasePool supervisionSpec withRestart: #permanent)
    with: (HTTPRouter supervisionSpec withRestart: #transient)
    with: MetricsCollector
```

**Rejected because:** Forces every Supervisor that uses `DatabasePool` to declare `restart: #permanent` — this is boilerplate that belongs on the Actor. Elixir's `child_spec/1` pattern proved that actor-owned defaults reduce errors and make supervisors leaner. `supervisionPolicy:` on the Actor puts the declaration where it is most stable.

### Option B: ClassBuilder Declaration Syntax for Supervision

`strategy:`, `maxRestarts:`, `restartWindow:`, `children:` as new class-body declaration keywords parsed by the ClassBuilder (analogous to `state:` for instance variables):

```beamtalk
Supervisor subclass: WebApp
  strategy: #oneForOne
  maxRestarts: 5
  restartWindow: 60
  children: #(DatabasePool HTTPRouter MetricsCollector)
```

**Rejected because:** This approach requires new parser grammar for every supervision keyword, new AST node types, and new ClassBuilder messages — for no expressive benefit. `strategy`, `maxRestarts`, `restartWindow` are simple scalar values with defaults; they are exactly what class-side method overrides are designed for. The only keyword that genuinely needs a ClassBuilder declaration is `supervisionPolicy:` on Actor (because it stores class metadata for later lookup). Everything else in the Supervisor body is plain Beamtalk.

### Option C: `@supervisor` Annotation on Actor

`@supervisor` annotation on `Actor subclass:` generates supervisor behaviour instead of gen_server:

```beamtalk
@supervisor strategy: #oneForOne maxRestarts: 5 restartWindow: 60
Actor subclass: WebApp
  children: #(DatabasePool HTTPRouter MetricsCollector)
```

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
- No new parser grammar for declaration keywords — supervision is expressed entirely through method overrides, the same mechanism as all other Beamtalk behaviour
- `children` is just a method: no special-case syntax, no escape hatches, no two-mechanism problem. Static list, computed list, conditional list — all the same construct
- `supervisionPolicy:` on Actors makes restart semantics self-documenting and discoverable
- Generated supervisor modules are standard OTP — fully observable with existing BEAM tools
- REPL inspection via message sends (`app children`, `app which: ClassName`) consistent with Beamtalk's interactive-first design
- `@native` actors participate in supervision without special handling
- `subclassResponsibility` on `children` gives a meaningful error if a concrete supervisor forgets to implement it

### Negative

- `Supervisor` joins the bootstrap class hierarchy: `ProtoObject → Object → Supervisor → [user supervisors]`. One more class to bootstrap before user modules load.
- Only `supervisionPolicy:` requires a new ClassBuilder declaration keyword. `strategy`, `maxRestarts`, `restartWindow` are now just method overrides — no parser or ClassBuilder changes for them.
- No compile-time validation of the `children` method return value. An incorrect return type (e.g. not an Array) fails at `supervise` time. This is the standard trade-off for method-based dispatch.
- `strategy: #dynamic` supervisors use OTP's `simple_one_for_one` — all dynamic children are the same class. Mixed-class dynamic supervisors require Erlang FFI.
- `shutdown:` per-child configuration is not in scope for this ADR. The default of `5000ms` is used for all children. Production systems needing custom shutdown timeouts use BEAM interop or `SupervisionSpec withShutdown:`.

### Neutral

- Retry patterns (`retryTimes:onError:backoff:`) and fallback patterns (`valueOrDefault:onError:`) are block-level error recovery, not supervision tree declarations. They are deferred to a separate ADR and are explicitly out of scope here.
- `Supervisor` instances are OTP supervisor processes, not gen_servers — `beamtalk_actor:sync_send/3` is not used for inspection messages. The generated supervisor module exports functions calling `supervisor:which_children/1` etc. directly, invoked via `gen:call/4`.
- `supervise` is the canonical start method, synthesized by the compiler. Supervisors may additionally define `class start => self supervise` as a named alias, but this is convention, not required.

## Implementation

### Phase 0 — Bootstrap `Supervisor` Abstract Class (S)

Add `Supervisor` and `SupervisionSpec` to the stdlib and bootstrap sequence:

- `stdlib/src/Supervisor.bt` — abstract base class with `children` declared `subclassResponsibility`; defaults for `strategy` (`#oneForOne`), `maxRestarts` (`10`), `restartWindow` (`60`); `supervise` stub
- `stdlib/src/SupervisionSpec.bt` — `SupervisionSpec` value type with `id`, `actorClass`, `restart`, `shutdown` fields; `withId:`, `withRestart:`, `withId:withRestart:` fluent overrides
- `runtime/apps/beamtalk_runtime/src/beamtalk_bootstrap.erl` — add `Supervisor` after `Actor` in the bootstrap sequence
- `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs` — add `Supervisor` as a known base class (alongside `Actor`, `Object`)

**Goal:** `Supervisor subclass: MySup` parses and registers as a distinct class kind. `SupervisionSpec` is available for use in `children` methods. No supervisor codegen yet.

### Phase 1 — `supervisionPolicy:` Declaration + `supervisionSpec` Synthesis (S)

This is the **only** phase that requires new parser grammar. Everything else in the supervision design uses ordinary method overrides.

**Parser (`crates/beamtalk-core/src/source_analysis/parser/`):**
- Add `supervisionPolicy: Symbol` as a class-level declaration form in `Actor subclass:` bodies (alongside `state:`)
- No new grammar for `strategy`, `maxRestarts`, `restartWindow`, `children`, or `childClass` — these are ordinary class-side method definitions (`class foo =>`)

**AST (`crates/beamtalk-core/src/ast.rs`):**
- `ClassDefinition` gains `supervision_policy: Option<Symbol>` for Actor subclasses
- `ClassDefinition` gains `is_supervisor_subclass: bool` flag (set by semantic analysis when the class hierarchy walks back to `Supervisor`)

**ClassBuilder (`stdlib/src/ClassBuilder.bt`, `beamtalk_class_builder.erl`):**
- New message: `supervisionPolicy:` — stores the symbol as class metadata on the Actor class
- `register` detects `Supervisor` ancestry and routes to supervisor codegen path

**Semantic Analysis:**
- Validate `supervisionPolicy:` is one of `#permanent`, `#transient`, `#temporary`
- Warn if an Actor subclass used as a child in a static `children` array literal has no `supervisionPolicy:` (default `#temporary` — "not restarted on crash")
- No validation of the `children` method return type — this is method dispatch, validated at runtime

**Codegen (`crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs`):**
- Synthesize `supervisionSpec` class-side method on all `Actor subclass:` definitions (alongside existing `spawn`/`spawnWith:`)
- `supervisionSpec` returns a `SupervisionSpec` value with `actorClass` set to the class, `restart` set from `supervisionPolicy:` metadata (defaulting to `#temporary`)

### Phase 2 — Supervisor Codegen (L)

**New file: `crates/beamtalk-core/src/codegen/core_erlang/supervisor_codegen.rs`**

Generates OTP `supervisor` behaviour modules from `Supervisor subclass:` definitions. The codegen calls the class-side methods at startup — it does not parse or statically analyse the `children` method body.

- `-behaviour(supervisor)` header; exports `start_link/0`, `init/1`
- `supervise/0` — synthesized class-side method; calls `supervisor:start_link(?MODULE, [])`, wraps pid as `beamtalk_object`
- `init/1` — calls `bt@classname:'children'()`, `bt@classname:'strategy'()`, `bt@classname:'maxRestarts'()`, `bt@classname:'restartWindow'()`; delegates child spec construction to `beamtalk_supervisor:build_child_specs/1`
- Static inspection: `'children'/1`, `'which:'/2`, `'terminate:'/2` — exported functions calling `supervisor:which_children/1` etc. via `gen:call/4`
- Dynamic management (generated when `strategy = #dynamic`): `'startChild:'/1`, `'startChild'/0`, `'terminateChild:'/1`, `'count'/1`

**`crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs`:**
- Detect `is_supervisor_subclass` flag in `ClassDefinition` and branch to `supervisor_codegen.rs` instead of generating `gen_server` behaviour

**New file: `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`**

Runtime helper module:
- `build_child_specs/1` — takes a list of class references (or `SupervisionSpec` values); for each, reads `supervisionPolicy` from class metadata (or uses the spec's `restart`); detects `Supervisor` ancestry (sets `type => supervisor, shutdown => infinity`); constructs OTP child spec maps
- `build_child_spec/1` — single-item variant

**No new lexer tokens** — `strategy`, `maxRestarts`, `restartWindow`, `children`, `childClass` are parsed as ordinary Beamtalk identifiers in method definitions.

**Full affected file list:**
- `crates/beamtalk-core/src/source_analysis/parser/` — `supervisionPolicy:` declaration grammar only
- `crates/beamtalk-core/src/ast.rs` — `supervision_policy` and `is_supervisor_subclass` fields on `ClassDefinition`
- `crates/beamtalk-core/src/semantic_analysis/` — validate `supervisionPolicy:` value; set `is_supervisor_subclass`; warn on children without policy
- `crates/beamtalk-core/src/codegen/core_erlang/supervisor_codegen.rs` — new file
- `crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs` — routing + `supervisionSpec` synthesis
- `stdlib/src/Supervisor.bt` — abstract base class
- `stdlib/src/SupervisionSpec.bt` — value type
- `stdlib/src/ClassBuilder.bt`, `beamtalk_class_builder.erl` — `supervisionPolicy:` message
- `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl` — new file
- `runtime/apps/beamtalk_runtime/src/beamtalk_bootstrap.erl` — add `Supervisor` to sequence

### Phase 3 — Tests and Docs (M)

- `stdlib/test/SupervisorTest.bt` — BUnit tests: static supervision (`class children => #(...)`), dynamic supervision (`class strategy => #dynamic`), `supervisionPolicy:` on Actor, `supervisionSpec` fluent overrides (`withId:`, `withRestart:`), nested supervisors, conditional `children` method
- `tests/e2e/cases/supervisor.bt` — REPL integration: `WebApp supervise`, `app children`, `app which: DatabasePool`, `pool startChild:`, `pool count`
- `docs/beamtalk-language-features.md` — add Supervision Tree section after Actor Message Passing
- `docs/beamtalk-principles.md` — update Principle 10 from "planned but not available" to "available via `Supervisor subclass:`"

## References

- Related issues: BT-448 (supervision tree syntax epic), BT-567 (Pharo-style Announcements / pub-sub — named process registry gap), BT-1189 (ETS shared in-memory table class), BT-1190 (actor message timeout configuration syntax)
- Related ADRs: ADR 0009 (OTP Application Structure), ADR 0015 (Signal-Time Exceptions — supervision motivation and pseudocode), ADR 0038 (ClassBuilder Protocol — class-level declarations mechanism), ADR 0042 (Immutable Value Objects / Actor-Only Mutable State), ADR 0043 (Sync-by-Default Actor Messaging), ADR 0056 (`@native` Erlang-Backed Actors)
- OTP Supervisor: https://www.erlang.org/doc/man/supervisor.html
- Elixir Supervisor: https://hexdocs.pm/elixir/Supervisor.html
- Armstrong thesis — supervision trees: https://erlang.org/download/armstrong_thesis_2003.pdf

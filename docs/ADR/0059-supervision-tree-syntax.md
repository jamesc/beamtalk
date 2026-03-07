# ADR 0059: Supervision Tree Syntax

## Status
Accepted (2026-03-07)

## Context

### The Problem

Beamtalk's Principle 10 states: "Embrace BEAM's 'let it crash' philosophy — actors crash independently, the supervisor restarts them." But today, supervision configuration requires Erlang FFI. There is no Beamtalk syntax for declaring which actors should be supervised, what restart strategy to use, or how to compose a supervision tree.

ADR 0015 deferred this as future work with the explicit note: "A declarative supervision tree DSL (custom restart strategies, supervision hierarchies) is planned but not yet available from Beamtalk syntax (tracked BT-448)."

The gap leaves Smalltalk developers without a bridge from their familiar `on:do:` / resume-based thinking to BEAM's let-it-crash model. The feature works at the OTP level — the `beamtalk_runtime_sup` and `beamtalk_subprocess_sup` supervisors already exist — but it is invisible to Beamtalk code.

### Current State

Actors are spawned unsupervised by default:

```beamtalk
counter := Counter spawn
counter increment
// If counter crashes, the process is gone — no automatic restart.
```

The runtime has two hand-written OTP supervisors:
- `beamtalk_runtime_sup` — one_for_one, supervises bootstrap/stdlib/instances + `beamtalk_subprocess_sup`
- `beamtalk_subprocess_sup` — simple_one_for_one/temporary, started by `beamtalk_actor.erl`'s `start_link_supervised/3`

There is no way to define a user-level supervision tree from `.bt` files, neither static (children known at startup) nor dynamic (children added at runtime).

### Constraints

1. **OTP supervisor behaviour** — OTP `supervisor` requires `init/1` to return `{ok, {SupFlags, ChildSpecs}}`. SupFlags is `#{strategy, intensity, period}`. Each ChildSpec is `#{id, start => {Module, Function, Args}, restart, shutdown, type, modules}`.
2. **Supervisor is not a gen_server** — A supervisor process handles child lifecycle, not user messages. `Supervisor subclass:` must generate `supervisor` behaviour, not `gen_server` behaviour.
3. **Dispatch routing** — ADR 0043 routes actor instance sends through `gen_server:call/cast` (`beamtalk_actor:sync_send/3`). Supervisors cannot receive arbitrary `gen_server:call` messages — OTP's internal `handle_call/3` only handles OTP-defined messages. `Supervisor subclass:` instances need a different dispatch path: generated inspection methods are called directly in the caller's process context, not via gen_server. The runtime must distinguish Supervisor objects from Actor objects to route correctly.
4. **`@native` actors** — `@native`-backed actors (ADR 0056) are standard gen_servers from OTP's perspective and can be supervised identically to generated actors.
5. **Retry and fallback patterns** — `retryTimes:onError:backoff:` and `valueOrDefault:onError:` are block-level error recovery patterns, not supervision tree declarations. They are deferred to a separate ADR.

## Decision

### Core Design: `Supervisor` and `DynamicSupervisor`

Supervision trees are expressed via two abstract base classes in the stdlib (`Supervisor` and `DynamicSupervisor`), plus a `SupervisionSpec` value type for per-child configuration. There are no special class-body declaration keywords — everything is a method.

The stdlib base classes:

```beamtalk
// stdlib/src/Supervisor.bt
abstract Object subclass: Supervisor
  class strategy      -> Symbol    => #oneForOne
  class maxRestarts   -> Integer   => 10
  class restartWindow -> Integer   => 60
  class isSupervisor  -> Boolean   => true
  class children      -> Array     => self subclassResponsibility
  class supervise     -> Supervisor => (Erlang beamtalk_supervisor) startLink: self
  class current       -> Supervisor => (Erlang beamtalk_supervisor) current: self
  children            -> Array     => (Erlang beamtalk_supervisor) whichChildren: self
  which: aClass       -> Object    => (Erlang beamtalk_supervisor) whichChild: self class: aClass
  terminate: aClass   -> Nil       => (Erlang beamtalk_supervisor) terminateChild: self class: aClass
  count               -> Integer   => (Erlang beamtalk_supervisor) countChildren: self
  stop                -> Nil       => (Erlang beamtalk_supervisor) stop: self

abstract Object subclass: DynamicSupervisor
  class maxRestarts   -> Integer          => 10
  class restartWindow -> Integer          => 60
  class isSupervisor  -> Boolean          => true
  class childClass    -> Class            => self subclassResponsibility
  class supervise     -> DynamicSupervisor => (Erlang beamtalk_supervisor) startLink: self
  class current       -> DynamicSupervisor => (Erlang beamtalk_supervisor) current: self
  startChild          -> Object           => (Erlang beamtalk_supervisor) startChild: self
  startChild: args :: Dictionary -> Object => (Erlang beamtalk_supervisor) startChild: self with: args
  terminateChild: child :: Object -> Nil  => (Erlang beamtalk_supervisor) terminateChild: self child: child
  count               -> Integer          => (Erlang beamtalk_supervisor) countChildren: self
  stop                -> Nil              => (Erlang beamtalk_supervisor) stop: self
```

`Supervisor` handles the static case (children known at startup); `DynamicSupervisor` handles the dynamic case (children added at runtime). Both subclass `Object` directly — there is no shared abstract base. Neither inherits the other's `subclassResponsibility` method — `Supervisor` subclasses are never asked for `childClass`; `DynamicSupervisor` subclasses are never asked for the static `children` list.

`supervise` and `current` are inherited class-side methods on both bases. `supervise` starts the supervisor (or returns the already-running instance). `current` returns the running instance by name, or `nil` if not started. All instance-side inspection methods (`children`, `which:`, `terminate:`, `count`, `stop`) are inherited from the abstract base via BEAM interop, making concrete subclasses completely minimal — they only need to override the configuration methods.

**Naming note:** `Supervisor` defines two `children` methods with different receivers. The class-side `class children =>` is implemented by concrete subclasses and returns the list of child classes for use during `init/1` startup. The instance-side `children =>` is inherited from the abstract base and returns the OTP child IDs of currently-running children from the live supervisor process. They have different arities at the Erlang level (0 vs 1) and are never ambiguous in dispatch, but serve different purposes: `WebApp children` returns the class list; `app children` returns the running child ids.

The two shared defaults (`maxRestarts`, `restartWindow`) are duplicated across both classes — a deliberate trade-off. A shared abstract base (`SupervisorBase`) would require a non-idiomatic `Base`-suffixed name and would create a false implied symmetry between static and dynamic models. The internal Erlang ancestry check for nesting (determining whether a child is itself a supervisor) uses a simple helper:

```erlang
%% beamtalk_supervisor.erl
is_supervisor(Module) ->
    beamtalk_class:is_subclass(Module, 'Supervisor') orelse
    beamtalk_class:is_subclass(Module, 'DynamicSupervisor').
```

This is the only place dual ancestry matters, and it is invisible to users.

Concrete supervisors subclass the appropriate class:

```beamtalk
Supervisor subclass: WebApp
  class children => #(DatabasePool HTTPRouter MetricsCollector)
```

The `children` method returns an `Array` whose elements are either bare class references (e.g. `DatabasePool`) or `SupervisionSpec` values (e.g. `DatabasePool supervisionSpec withId: #primary withArgs: #{#role => #primary}`). Bare class references use defaults: the class name as the OTP child id and `supervisionPolicy` for the restart type. `SupervisionSpec` values carry per-child overrides for id, restart, and startup args. Both forms can be mixed freely in the same array. `beamtalk_supervisor:build_child_specs/1` handles both. The generated `init/1` calls `self children` at startup to build the OTP child specs. All other methods have sensible defaults and can be selectively overridden:

```beamtalk
Supervisor subclass: WebApp
  class strategy    => #oneForAll
  class maxRestarts => 3
  class children    => #(DatabasePool HTTPRouter MetricsCollector)
```

Because `children` is a method, it can do anything — read environment variables, inspect feature flags, compose conditionally, delegate to helpers:

```beamtalk
Supervisor subclass: AppSup
  class children =>
    children := Array with: DatabasePool with: HTTPRouter
    (Feature flagged: #metrics) ifTrue: [children := children copyWith: MetricsCollector]
    children

Supervisor subclass: MultiRegionSup
  class regions => #(#us-east #eu-west #ap-south)

  class children =>
    self regions collect: [:region |
      RegionalWorker supervisionSpec withId: region]
```

Forgetting to implement `children` raises a `SubclassResponsibility` error at `supervise` time:

```text
> BrokenSup supervise
SubclassResponsibility: BrokenSup does not implement 'children'
  (Supervisor subclass: BrokenSup — override 'children' to return the child class list)
```

Similarly, a dynamic supervisor that omits `childClass`:

```text
> BrokenPool supervise
SubclassResponsibility: BrokenPool does not implement 'childClass'
  (DynamicSupervisor subclass: BrokenPool — override 'childClass' to return the worker class)
```

### `supervisionPolicy` on Actor — Default Restart Type

Each Actor class overrides the inherited `supervisionPolicy` class-side method to declare its default restart behaviour. The default is defined on the `Actor` base class:

```beamtalk
// stdlib/src/Actor.bt — defaults on the base class
abstract Object subclass: Actor
  class supervisionPolicy -> Symbol  => #temporary  // default: do not restart on crash
  class isSupervisor      -> Boolean => false
```

Actor subclasses override it when they need a different default:

```beamtalk
Actor subclass: DatabasePool
  class supervisionPolicy -> Symbol => #permanent  // always restart — a crashed pool must come back
  state: pool = nil

Actor subclass: HTTPRouter
  class supervisionPolicy -> Symbol => #transient  // restart only on abnormal exit

// Actor subclass: MetricsCollector — inherits #temporary from Actor base (not restarted)
```

Valid values: `#permanent` (always restart), `#transient` (restart on abnormal exit only), `#temporary` (never restart). These map directly to OTP child spec `restart` values.

`supervisionPolicy` is a class-side method: `ClassName supervisionPolicy` returns the actor's declared restart default.

**Why a method override rather than a ClassBuilder declaration?** `beamtalk_supervisor:build_child_specs/1` reads `supervisionPolicy` by calling `Module:'supervisionPolicy'()` directly — a plain Erlang function call with no Beamtalk dispatch overhead. This is identical to how `init/1` calls `bt@webapp:'strategy'()` and `bt@webapp:'children'()`. Method inheritance means the `#temporary` default from `Actor` is returned automatically for any subclass that does not override. No new parser grammar, no new ClassBuilder messages, no new AST fields — `supervisionPolicy` is just another class-side method with a useful default.

### `supervisionSpec` on Actor — Building Child Specs

Every `Actor subclass:` gains a synthesized class-side method `supervisionSpec` that returns a `SupervisionSpec` value object describing how to start this actor as a supervised child. This is used inside `children` methods when per-child configuration is needed:

```beamtalk
// Default — id is class name, restart from supervisionPolicy:
DatabasePool supervisionSpec

// Fluent overrides for the cases that need them
DatabasePool supervisionSpec withId: #primary
DatabasePool supervisionSpec withId: #replica withRestart: #transient
DatabasePool supervisionSpec withId: #primary withArgs: #{#role => #primary}
DatabasePool supervisionSpec withId: #replica withRestart: #transient withArgs: #{#role => #replica}
```

`SupervisionSpec` is a value type that owns the logic for building its OTP child spec. The construction logic lives in Beamtalk — no `@primitive` or Erlang interop needed, just conditionals and dict construction:

```beamtalk
Value subclass: SupervisionSpec
  state: id = nil          // Symbol or nil — nil uses the actor class name
  state: actorClass = nil  // Class
  state: restart = #temporary  // Symbol
  state: args = nil        // Dictionary or nil

  withId: anId :: Symbol -> SupervisionSpec => ...
  withRestart: policy :: Symbol -> SupervisionSpec => ...
  withArgs: aDict :: Dictionary -> SupervisionSpec => ...
  withId: anId :: Symbol withRestart: policy :: Symbol -> SupervisionSpec => ...
  withId: anId :: Symbol withArgs: aDict :: Dictionary -> SupervisionSpec => ...
  withId: anId :: Symbol withRestart: policy :: Symbol withArgs: aDict :: Dictionary -> SupervisionSpec => ...

  childSpec -> Dictionary =>
    childId   := self id isNil ifTrue: [self actorClass name] ifFalse: [self id]
    startFn   := self args isNil ifTrue: [#spawn] ifFalse: [#'spawnWith:']
    startArgs := self args isNil ifTrue: [#()] ifFalse: [Array with: self args]
    shutdown  := self actorClass isSupervisor ifTrue: [#infinity] ifFalse: [5000]
    childType := self actorClass isSupervisor ifTrue: [#supervisor] ifFalse: [#worker]
    #{
      #id       => childId,
      #start    => Array with: self actorClass with: startFn with: startArgs,
      #restart  => self restart,
      #shutdown => shutdown,
      #type     => childType
    }
```

`childSpec` uses `self actorClass isSupervisor` — a class-side method returning `true` on `Supervisor`/`DynamicSupervisor` and `false` on `Actor` — to determine `shutdown` and `type` without any Erlang ancestry check. The result is a Beamtalk dict (`#{}`), which compiles to an Erlang map and is consumed directly by OTP's `supervisor:start_link/2`.

The `shutdown` timeout defaults (`5000ms` for worker children, `#infinity` for nested supervisor children) are now encoded in `childSpec`, not in a separate Erlang helper. A future `withShutdown:` fluent override would add a `shutdown` field to `SupervisionSpec` and `childSpec` would read it, with the same defaults as fallback.

`withArgs:` solves a specific gap: a static supervisor with multiple instances of the same actor class, each needing different startup configuration. The alternative — each actor reading its config from ETS or process name in its own `init` — is indirect and scatters configuration across files:

```beamtalk
Supervisor subclass: DBSup
  class children =>
    Array
      with: (DatabasePool supervisionSpec withId: #primary withArgs: #{#role => #primary})
      with: (DatabasePool supervisionSpec withId: #replica withArgs: #{#role => #replica})
```

This generates:
- `primary`: `start => {'bt@databasepool', 'spawnWith:', [#{role => primary}]}`
- `replica`: `start => {'bt@databasepool', 'spawnWith:', [#{role => replica}]}`

The multi-region pattern also benefits — passing the region directly at start time rather than having the actor infer it from its process name:

```beamtalk
Supervisor subclass: MultiRegionSup
  class regions => #(#us-east #eu-west #ap-south)

  class children =>
    self regions collect: [:region |
      RegionalWorker supervisionSpec withId: region withArgs: #{#region => region}]
```

`DynamicSupervisor` is not the right answer for these patterns — the children are known at definition time, so `DynamicSupervisor`'s runtime `startChild:` calls would just push the configuration problem to a startup script.

### Dynamic Supervision — `DynamicSupervisor subclass:`

When the number of children isn't known at startup, subclass `DynamicSupervisor` and implement `childClass`:

```beamtalk
DynamicSupervisor subclass: WorkerPool
  class childClass => Worker
```

`childClass` returns the Actor class for dynamic children. The generated `init/1` builds the `simple_one_for_one` child spec template from it. Dynamic supervisors start with no children; `startChild:` and `terminateChild:` add and remove them at runtime:

```beamtalk
pool := WorkerPool supervise

w1 := pool startChild: #{#config => "db-a"}
w2 := pool startChild: #{#config => "db-b"}
pool count                // => 2

pool terminateChild: w1
pool count                // => 1
```

`startChild:` passes the dict to `Worker spawnWith:`. `startChild` (no arg) calls `Worker spawn`.

`terminateChild:` takes the actor object returned by `startChild:`. If the child has already crashed (the pid is dead), `supervisor:terminate_child/2` returns `{error, not_found}` — Beamtalk surfaces this as a `BeamtalkError` with reason `#childNotFound`. If the child crashes and is restarted by the supervisor (which does not apply to `simple_one_for_one` with `temporary` restart, but is possible if the user overrides `supervisionPolicy:` on the child actor), the pid in the returned actor object is stale. Callers holding the result of `startChild:` should treat it as valid only until the next crash; use `app count` to verify the child is still alive before sending messages.

The `children` method is not defined on `DynamicSupervisor`. The codegen detects `DynamicSupervisor` ancestry at compile time and generates a `simple_one_for_one` `init/1` using `childClass` — no `subclassResponsibility` for `children` is ever reached.

**Codegen:** `DynamicSupervisor` ancestry generates `simple_one_for_one`. `startChild:` calls `supervisor:start_child/2`; `terminateChild:` calls `supervisor:terminate_child/2` with the child pid. **`supervisor:delete_child/2` must NOT be called** — for `simple_one_for_one`, OTP removes the template automatically and calling `delete_child` crashes the supervisor.

Note: `simple_one_for_one` is deprecated since OTP 24 in favour of `one_for_one` with `significant: false`. Beamtalk uses it for v0.x consistency with `beamtalk_subprocess_sup`, which also uses `simple_one_for_one`. Migration to the OTP 24+ pattern is tracked as future work.

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
        {ok, ChildPid} -> beamtalk_actor:wrap_child(ChildPid, 'Worker', 'bt@worker');
        {error, Reason} -> beamtalk_error:raise(...)
    end.
```

### Starting, Finding, and Inspecting Supervisors

`supervise` is a class-side method inherited from the `Supervisor` (or `DynamicSupervisor`) abstract base. It starts the supervisor and registers it locally under its Erlang module name, then returns a wrapped supervisor object:

```beamtalk
app := WebApp supervise
```

The supervisor is registered under `?MODULE` (e.g. `'bt@webapp'`), which is the OTP-standard named registration pattern for application supervisors. This makes the supervisor immediately findable from anywhere on the node — without a separate process registry. `supervise` is idempotent: if the supervisor is already running, it returns a handle to the existing instance:

```beamtalk
app  := WebApp supervise   // starts and registers as 'bt@webapp'
app2 := WebApp supervise   // same process — returns existing instance
app = app2                 // => true
```

`current` returns the running instance by name, or `nil` if not started:

```beamtalk
WebApp current             // => #Supervisor<WebApp, <0.200.0>>  if running
WebApp current             // => nil                             if not started
```

This covers the common "how do I find my supervisor?" question: a module that needs to inspect the supervision tree sends `WebApp current` rather than holding a reference in a variable.

**Dispatch routing:** The `beamtalk_object` tuple for a supervisor uses a distinct type tag `'beamtalk_supervisor'` (not `'beamtalk_object'`). This allows `beamtalk_dispatch` to detect supervisor instances at the call site and route to `Module:'method'(Self)` directly, bypassing `beamtalk_actor:sync_send/3`. Actor objects continue to use `'beamtalk_object'` and route through gen_server as before (ADR 0043).

Supervisor instances respond to inspection messages. OTP supervisor behaviour implements `handle_call/3` internally (for OTP's own `which_children`, `count_children`, etc.) and user code cannot add further clauses without conflicting. Instead, the inspection methods are exported module functions that call `supervisor:which_children/1` and related OTP APIs directly from the requesting process's context. These are safe because they only invoke OTP APIs — they carry no actor process state and never access the supervisor's process dictionary. The asymmetry — Actor methods execute in the actor's process, Supervisor inspection methods execute in the caller's process — is intentional and documented here so implementers are not surprised.

**Blast radius of the new type tag:** `isKindOf:`, `respondsTo:`, and the REPL pretty-printer must all handle `'beamtalk_supervisor'` tuples. `isKindOf:` and `respondsTo:` operate on the class hierarchy (stored in `__beamtalk_meta/0`), not the runtime tag — no change needed there. The REPL pretty-printer must add a `'beamtalk_supervisor'` clause to display `#Supervisor<ClassName, Pid>`. The `beamtalk_dispatch` change is the only routing change; reflection APIs are unaffected.

```beamtalk
app children                // => #(#DatabasePool #HTTPRouter #MetricsCollector)
app which: DatabasePool     // => #Actor<DatabasePool, <0.201.0>>
app terminate: DatabasePool // gracefully stop the child
app count                   // => 3
app stop                    // stops the supervisor and all children
```

Note: per-child restart counts are not part of this API — OTP exposes no standard API for them. Use `observer:start()` or `supervisor:count_children/1` for aggregate diagnostics.

### Codegen: What Gets Generated

For `Supervisor subclass: WebApp` with `class children => #(DatabasePool HTTPRouter MetricsCollector)`:

```erlang
%% Generated: bt@webapp.erl
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export(['supervise'/0, 'current'/0, 'children'/1, 'which:'/2, 'terminate:'/2,
         'count'/1, 'stop'/1]).

%% OTP-compatible start — registers locally under the module name.
%% Named registration makes the supervisor findable via whereis/1 and OTP tools
%% without a separate process registry. Standard OTP pattern for named supervisors.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Beamtalk API — starts or returns the already-running supervisor.
%% Idempotent: calling supervise on an already-running supervisor returns
%% the existing instance rather than failing.
'supervise'() ->
    case start_link() of
        {ok, Pid} -> {'beamtalk_supervisor', 'WebApp', ?MODULE, Pid};
        {error, {already_started, Pid}} -> {'beamtalk_supervisor', 'WebApp', ?MODULE, Pid};
        {error, Reason} -> beamtalk_error:raise(...)
    end.

%% Returns the running supervisor instance, or nil if not started.
'current'() ->
    case whereis(?MODULE) of
        undefined -> nil;
        Pid -> {'beamtalk_supervisor', 'WebApp', ?MODULE, Pid}
    end.

init([]) ->
    %% Calls class-side 'children' method to get the child list at startup.
    %% Reads supervisionPolicy from each child class via Module:'supervisionPolicy'().
    ChildClasses = 'bt@webapp':'children'(),
    SupFlags = #{strategy => 'bt@webapp':'strategy'(),
                 intensity => 'bt@webapp':'maxRestarts'(),
                 period => 'bt@webapp':'restartWindow'()},
    ChildSpecs = beamtalk_supervisor:build_child_specs(ChildClasses),
    {ok, {SupFlags, ChildSpecs}}.

%% Instance-side inspection — exported functions, not handle_call/3 clauses.
%% Returns the OTP child IDs (atoms). Default IDs are the class name atoms
%% (e.g. 'DatabasePool'). When withId: is used, the id is the custom symbol.
'children'(Self) ->
    Pid = element(4, Self),
    [Id || {Id, _, _, _} <- supervisor:which_children(Pid), Id =/= undefined].
```

`beamtalk_supervisor:build_child_specs/1` normalizes bare class references to `SupervisionSpec` values (by calling `ClassName supervisionSpec`), then delegates all child spec construction to `SupervisionSpec childSpec`. All the logic — start function selection, `isSupervisor` type/shutdown dispatch, id defaulting — lives in `childSpec` in Beamtalk. The Erlang helper is now minimal:

```erlang
%% beamtalk_supervisor.erl
build_child_specs(Children) ->
    [child_spec(C) || C <- Children].

child_spec(Class) when is_atom(Class) ->
    Spec = beamtalk_dispatch:class_send(Class, 'supervisionSpec', []),
    beamtalk_dispatch:send(Spec, 'childSpec', []);
child_spec(Spec) ->
    beamtalk_dispatch:send(Spec, 'childSpec', []).
```

### REPL Session

Static supervisor:
```text
> app := WebApp supervise
#Supervisor<WebApp, <0.200.0>>

> app children
#(#DatabasePool #HTTPRouter #MetricsCollector)   "ids — class name symbols by default"

> app which: DatabasePool
#Actor<DatabasePool, <0.201.0>>

> DatabasePool supervisionPolicy
#permanent
```

Finding a running supervisor by class name (from any process, any session):
```text
> WebApp current
#Supervisor<WebApp, <0.200.0>>

> WebApp supervise       "idempotent — returns existing instance"
#Supervisor<WebApp, <0.200.0>>

> WebApp current = WebApp current
true
```

For supervisors with `withId:` overrides, `app children` returns the custom ids:
```text
> app children
#(#primary #replica)   "custom ids from supervisionSpec withId:"
```

Dynamic supervisor:
```text
> pool := WorkerPool supervise
#DynamicSupervisor<WorkerPool, <0.210.0>>

> pool count
0

> w1 := pool startChild: #{#config => "primary"}
#Actor<Worker, <0.211.0>>

> pool count
1

> WorkerPool current
#DynamicSupervisor<WorkerPool, <0.210.0>>
```

### Nested Supervisors

Supervisors appear in other supervisors' `children` method just like actors. The runtime detects that a class is a `Supervisor` or `DynamicSupervisor` subclass and sets `type => supervisor, shutdown => infinity` in the child spec. Default restart for a nested supervisor is `#permanent`:

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

**What we rejected:** Pipeline syntax for supervision structure. Beamtalk's class body method overrides are more discoverable (the structure is right there in the class definition) and consistent with how all other Beamtalk class behaviour is expressed.

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
| Restart strategies | ✅ 4 strategies | ✅ 4 strategies | ❌ | ✅ 3 strategies | **✅ 4 strategies (1:1 OTP)** |
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

**Neutral:** `strategy`, `maxRestarts`, `restartWindow`, `children` (on `Supervisor`), and `childClass` (on `DynamicSupervisor`) are ordinary method overrides — the LSP can provide completions for method names (autocomplete `class children =>` on `Supervisor subclass:`, `class childClass =>` on `DynamicSupervisor subclass:`) but cannot validate the return type at edit time. The compiler flags missing implementations at `supervise` time rather than statically. This is the standard trade-off of the method-based approach.

**Positive:** `Supervisor subclass:` and `DynamicSupervisor subclass:` are identifiable from source — the LSP can distinguish them from `Actor subclass:` and show appropriate completions/hover text for each.

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

**Why the final design wins:** The operator/BEAM veteran argument is valid but cuts both ways — `DatabasePool` IS always permanent in production. It's a database pool; any supervisor that includes it should restart it on crash. Having every supervisor that uses `DatabasePool` redeclare `restart: #permanent` is boilerplate and a source of bugs (forgetting the override in one supervisor).

The "different supervisors may want different behaviour" case is addressed by the `supervisionSpec withRestart:` override — `TestSup` can use `DatabasePool supervisionSpec withRestart: #temporary` without touching `DatabasePool.bt`. `supervisionPolicy:` sets the *default* the Actor considers correct, not an immutable constraint. The override mechanism exists precisely for the cases where a supervisor genuinely needs a different policy; `supervisionPolicy:` just means the common case requires no boilerplate.

### Option B: ClassBuilder Declaration Syntax (rejected)

```beamtalk
Supervisor subclass: WebApp
  strategy: #oneForOne
  maxRestarts: 5
  restartWindow: 60
  children: #(DatabasePool HTTPRouter MetricsCollector)
```

| Cohort | Strongest argument |
|---|---|
| 🎨 **Language designer** | "Declarations are metadata, not methods. `state: pool = nil` is structural metadata — so is the `children:` list. A method that returns `#(DatabasePool HTTPRouter)` looks like behavior when it's really just configuration. Declarations communicate intent; methods communicate computation." |
| ⚙️ **BEAM veteran** | "Static analysis is possible: the compiler can validate that each child class exists and is an `Actor subclass:` AT COMPILE TIME, not at `supervise` time. The method-based approach gives up this safety entirely." |
| 🧑‍💻 **Newcomer** | "`class children => #(...)` reads as 'a method that happens to return a list.' `children: #(...)` reads as 'this class has these children.' The declaration form is self-evidently structural." |
| 🏭 **Operator** | "Static declarations are greppable. `grep 'children:' **/*.bt` finds all supervision trees. `class children =>` methods require the reader to understand that this particular method is structural metadata." |
| 🎩 **Smalltalk purist** | "Declarations like `strategy:` and `children:` follow the Smalltalk class protocol — we already use this pattern with `state:` and `supervisionPolicy:`. Extending it to supervision configuration is consistent, not a new mechanism. Newspeak handles module-level configuration with declarations for the same reason." |

**Why the final design wins:** The compile-time validation argument is real but overstated. The most impactful error — forgetting `children` entirely — is caught at `supervise` time with a clear `SubclassResponsibility` message. Class-existence checks on literal array bodies (`#(DatabasePool HTTPRouter)`) are equally possible with the method approach — the compiler CAN inspect the literal. And for computed children (`class children => self regions collect: [...]`), runtime validation is unavoidable regardless of syntax. The declaration approach would need two mechanisms (declarations for the common case, an `init` override escape hatch for the computed case) — exactly the two-mechanism problem the method design avoids. The method design handles all cases uniformly without new parser grammar.

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
| 🎩 **Smalltalk purist** | "Fault tolerance is a meta-level concern, orthogonal to object hierarchy. A supervisor is a *role*, not a *type*. `@supervisor` is honest about that — it says 'this class plays the supervisor role' without claiming that supervising children is a fundamental property of a class's identity. Smalltalk's image never distinguished 'process that supervises' from 'process that computes' at the type level." |
| 🏭 **Operator** | "I don't care about class hierarchy — I care about what the OTP crash report looks like and what `observer:start()` shows. If `@supervisor` generates identical `-behaviour(supervisor)` code, the annotation vs. base class distinction is completely invisible in production." |

**Why it is rejected:** A supervisor is fundamentally not an Actor (it does not receive user messages; it watches other processes). `@supervisor` on `Actor subclass:` buries a critical semantic difference in an annotation that looks like an implementation detail — the same visual weight as `@native` or `@primitive`. The `Supervisor` base class makes the distinction explicit and self-documenting. New users reading the Beamtalk stdlib will immediately see that `Supervisor subclass: WebApp` is a different thing from `Actor subclass: Counter` — just as `Actor subclass:` is clearly different from `Object subclass:`. The `@annotation` pattern (ADR 0055, ADR 0056) is reserved for implementation details; `Supervisor` is a first-class language concept.

### Tension Points

- **One-file vs two-file visibility:** Operators who want the full restart topology in one file prefer all specs in the `Supervisor`. The `supervisionPolicy:` design requires cross-file reading to determine effective restart behaviour. The compiler warning for missing `supervisionPolicy:` (with explicit defaults) reduces this tension — a developer reading `WebApp.bt` can always see the effective defaults by checking the Actor files.
- **`Supervisor`/`DynamicSupervisor` in the class hierarchy:** Adding `Supervisor` and `DynamicSupervisor` to `ProtoObject → Object → ...` is a real cost — two classes to bootstrap before user modules load. The `@supervisor` annotation alternative (Option C) avoids it. But the semantic clarity justifies it — supervision trees are a first-class abstraction in BEAM programming, and distinct classes make the static/dynamic distinction self-documenting at the point of `subclass:`.
- **Two flat classes vs a shared abstract base:** A `SupervisorBase` (or similar) could hold the two shared defaults (`maxRestarts`, `restartWindow`) and provide a single `isKindOf:` check. The chosen design rejects this: `Base`-suffixed names are not idiomatic in Beamtalk (compare `Collection`, `Number`, `Actor` — never `CollectionBase`). The nesting ancestry check (`is_supervisor/1` in `beamtalk_supervisor.erl`) is a one-line Erlang `orelse` — not worth a user-visible abstract class. Two duplicated one-line defaults are a simpler trade-off.

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

### Option E: Raw Map Child Specs in `children` (No `SupervisionSpec`)

`children` returns plain maps directly matching the OTP child spec format, with no new value type:

```beamtalk
Supervisor subclass: WebApp
  class children =>
    Array
      with: #{#id => #db, #restart => #permanent,
              #start => #(DatabasePool #spawn #nil)}
      with: #{#id => #http, #restart => #transient,
              #start => #(HTTPRouter #spawn #nil)}
```

**Rejected because:** This is essentially Option D extended into the `children` method body — it requires the user to know the OTP child spec structure (`{id, start, restart, shutdown, type, modules}`) and construct it manually. It is Erlang FFI embedded in Beamtalk syntax. `SupervisionSpec` exists precisely to provide a Beamtalk-idiomatic layer over this structure, the same way `Actor spawn` is a Beamtalk layer over `gen_server:start_link`. The `withId:` / `withRestart:` fluent API is also easier to read than map construction.

### Option D: Pure Runtime Protocol (No New Syntax)

No new syntax. Users invoke Erlang supervision APIs via BEAM interop:

```beamtalk
(Erlang supervisor) start_link: WebAppSup module: beamtalk_supervisor args: childSpecs
```

**Rejected because:** This is the current state — it requires Erlang FFI and leaves Principle 10 ("fault tolerance without Erlang boilerplate") unmet. The goal of this ADR is to make supervision trees expressible in Beamtalk.

## Consequences

### Positive

- Principle 10 fully satisfied: supervision tree syntax available in pure Beamtalk
- No new parser grammar required. `supervisionPolicy` is now a class-side method override on `Actor` (like `strategy`, `maxRestarts`, etc.) — not a ClassBuilder declaration. All supervision configuration is expressed as ordinary method overrides
- `children` is just a method: no special-case syntax, no escape hatches, no two-mechanism problem. Static list, computed list, conditional list — all the same construct
- `supervisionPolicy:` on Actors makes restart semantics self-documenting and discoverable
- Generated supervisor modules are standard OTP — fully observable with existing BEAM tools
- `SupervisionSpec childSpec` encapsulates all child spec construction logic in Beamtalk — no Erlang needed for the core logic; `build_child_specs/1` is a thin normalization + dispatch layer. Child spec construction is testable from Beamtalk (`spec childSpec` in the REPL) without exercising Erlang
- REPL inspection via message sends (`app children`, `app which: ClassName`) consistent with Beamtalk's interactive-first design
- `@native` actors participate in supervision without special handling
- `self subclassResponsibility` on `Supervisor children` and `DynamicSupervisor childClass` gives a meaningful error if a concrete supervisor forgets to implement the required method

### Negative

- `Supervisor` and `DynamicSupervisor` join the bootstrap class hierarchy: `ProtoObject → Object → {Supervisor, DynamicSupervisor} → [user supervisors]`. Two more classes to bootstrap before user modules load.
- No compile-time validation of the `children` method return value. An incorrect return type (e.g. not an Array) fails at `supervise` time. This is the standard trade-off for method-based dispatch. (The declaration-based approach would catch this statically — see Option B steelman.)
- `DynamicSupervisor` subclasses use OTP's `simple_one_for_one` — all dynamic children must be the same class. Mixed-class dynamic supervisors require Erlang FFI.
- `shutdown:` per-child configuration is not in scope for this ADR. The default of `5000ms` is used for worker children (and `infinity` for nested supervisor children). Production systems needing custom per-child shutdown timeouts use `SupervisionSpec withShutdown:` (future) or BEAM interop.
- **`children` method must not dispatch to actors** that depend on this supervisor's children being alive. OTP calls `init/1` synchronously before any children are started; if `children` sends a message to an actor (e.g. a feature-flag actor) that is itself a child of this supervisor, the call will deadlock. Beamtalk does not enforce this constraint — it is the user's responsibility. The compiler will warn if `children` is non-trivial (not a literal array or collect over a literal), reminding the user that dispatch inside `children` may block at startup.
- Actor object references returned by `DynamicSupervisor startChild:` become stale if the child crashes and is restarted under a non-temporary restart policy. The new pid is not reflected in the original actor object. Use `app count` to verify liveness; use a process registry (BT-567) for long-lived references to dynamic children.
- `DynamicSupervisor` codegen is committed to `simple_one_for_one` semantics for v0.x. When Beamtalk migrates to the OTP 24+ `one_for_one` with `significant: false` pattern, the `startChild:`/`terminateChild:` call signatures may change. Treat `DynamicSupervisor` as unstable across minor versions until this migration is complete.

### Neutral

- Retry patterns (`retryTimes:onError:backoff:`) and fallback patterns (`valueOrDefault:onError:`) are block-level error recovery, not supervision tree declarations. They are deferred to a separate ADR and are explicitly out of scope here.
- **No OTP application root:** `supervise` starts a named standalone process — it is not a child of any OTP application supervisor. If the node restarts, user supervisors do not come back automatically. `observer:start()` shows them as unattached named processes, not rooted in an application tree. The proper fix is a `[application] supervisor = "AppSup"` key in `beamtalk.toml` that generates an OTP application callback (`start/2` calling `AppSup start_link`), replacing the current `[run] entry = "Main run"` pattern for long-running services. This also enables `Workspace supervisor` — a stable, class-name-agnostic entry point into the application's supervision tree (the Beamtalk equivalent of `application:get_supervisor/1`). This is a follow-on to ADR 0026 and tracked in BT-1191. Until BT-1191 is implemented, `beamtalk run` with a supervision tree requires a one-line `Main run` that calls `AppSup supervise`.
- **Hot code reload:** OTP `supervisor` behaviour does not support `code_change/3`. Reloading a `Supervisor subclass:` in a live REPL session updates the module code but does not change the running supervisor's live child set. To apply a changed `children` or `strategy` definition, the supervisor must be stopped and re-started (`app stop` then `WebApp supervise`). The compiler should warn when a supervisor class is hot-reloaded in a live workspace.
- **Named registration and singleton-per-node:** `supervise` registers the supervisor locally under its Erlang module name (`{local, ?MODULE}`), the standard OTP pattern for named application supervisors. This makes each `Supervisor subclass:` a singleton per node: only one instance of `WebApp` can run at a time. `ClassName current` finds it from anywhere on the node via `whereis(?MODULE)`. `supervise` is idempotent: calling it on an already-running supervisor returns the existing instance. If two instances of the same topology are needed, define two classes. This is node-local registration only; cluster-wide named registration is deferred to future work. Ad-hoc `DynamicSupervisor new supervise` (no subclass) uses anonymous registration and is not a singleton.
- The generated module exports both `start_link/0` (OTP-compatible, returns `{ok, Pid}`, used when embedding in an OTP application supervision tree) and `supervise/0` (Beamtalk-wrapped object, used in Beamtalk code and the REPL).
- `Supervisor` instances are OTP supervisor processes, not gen_servers — `beamtalk_actor:sync_send/3` is not used for inspection messages. The generated supervisor module exports functions (e.g. `'children'/1`, `'which:'/2`) that call `supervisor:which_children/1` etc. directly from the calling process's context, passing the supervisor's pid.
- `supervise` is the canonical start method, inherited from the abstract base. Supervisors may additionally define `class start => self supervise` as a named alias, but this is convention, not required.

## Implementation

### Phase 0 — Bootstrap `Supervisor`, `DynamicSupervisor` (S)

Add `Supervisor`, `DynamicSupervisor`, and `SupervisionSpec` to the stdlib and bootstrap sequence:

- `stdlib/src/Supervisor.bt` — two abstract classes: `Supervisor` (`strategy => #oneForOne`, `maxRestarts => 10`, `restartWindow => 60`, `children => self subclassResponsibility`); `DynamicSupervisor` (`maxRestarts => 10`, `restartWindow => 60`, `childClass => self subclassResponsibility`)
- `stdlib/src/SupervisionSpec.bt` — `SupervisionSpec` value type with `id`, `actorClass`, `restart`, `args` fields; `withId:`, `withRestart:`, `withArgs:`, `withId:withRestart:`, `withId:withArgs:`, `withId:withRestart:withArgs:` fluent overrides
- `runtime/apps/beamtalk_runtime/src/beamtalk_bootstrap.erl` — add `Supervisor`, `DynamicSupervisor` after `Actor` in the bootstrap sequence
- `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs` — add `Supervisor`, `DynamicSupervisor` as known base classes (alongside `Actor`, `Object`)

**Goal:** `Supervisor subclass: MySup` and `DynamicSupervisor subclass: WorkerPool` parse and register as distinct class kinds. `SupervisionSpec` is available for use in `children` methods. No supervisor codegen yet.

### Phase 1 — `supervisionSpec` Synthesis (S)

**No new parser grammar required.** `supervisionPolicy` is a class-side method override (`class supervisionPolicy => #permanent`) — the parser already handles this. The default is defined in `stdlib/src/Actor.bt` as `class supervisionPolicy => #temporary` on the `Actor` base class, inherited automatically by all Actor subclasses that do not override it.

**AST (`crates/beamtalk-core/src/ast.rs`):**
- `ClassDefinition` gains `supervisor_kind: Option<SupervisorKind>` where `SupervisorKind` is `Static` (inherits from `Supervisor`) or `Dynamic` (inherits from `DynamicSupervisor`), set by semantic analysis
- No new field for `supervision_policy` — it is a regular class-side method, visible to the parser as an ordinary method definition

**Semantic Analysis:**
- Validate `supervisionPolicy` override returns one of `#permanent`, `#transient`, `#temporary` (static check when the method body is a single literal)
- Warn if an Actor subclass used as a child in a static `children` array literal does not explicitly override `supervisionPolicy` (default `#temporary` — "not restarted on crash")
- No validation of the `children` method return type — this is method dispatch, validated at runtime

**Stdlib (`stdlib/src/Actor.bt`):**
- Add `class supervisionPolicy => #temporary` to the `Actor` abstract base

**Codegen (`crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs`):**
- Synthesize `supervisionSpec` class-side method on all `Actor subclass:` definitions (alongside existing `spawn`/`spawnWith:`)
- `supervisionSpec` returns a `SupervisionSpec` value with `actorClass` set to the class, `restart` set by calling `Module:'supervisionPolicy'()` (which returns the inherited or overridden value)

### Phase 2 — Supervisor Codegen (M)

The compiler work is intentionally minimal. Most logic lives in the stdlib (`Supervisor.bt`, `SupervisionSpec.bt`) and the runtime Erlang helper (`beamtalk_supervisor.erl`). The compiler generates only two functions per supervisor subclass.

**New file: `crates/beamtalk-core/src/codegen/core_erlang/supervisor_codegen.rs`**

For each `Supervisor subclass:` or `DynamicSupervisor subclass:`, generates exactly:

```erlang
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Static supervisor (Supervisor subclass:)
init([]) ->
    ChildClasses = 'bt@classname':'children'(),
    SupFlags = #{strategy  => 'bt@classname':'strategy'(),
                 intensity => 'bt@classname':'maxRestarts'(),
                 period    => 'bt@classname':'restartWindow'()},
    {ok, {SupFlags, beamtalk_supervisor:build_child_specs(ChildClasses)}}.

%% Dynamic supervisor (DynamicSupervisor subclass:)
init([]) ->
    ChildClass = 'bt@classname':'childClass'(),
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 'bt@classname':'maxRestarts'(),
                 period    => 'bt@classname':'restartWindow'()},
    ChildSpec = beamtalk_supervisor:build_child_specs([ChildClass]),
    {ok, {SupFlags, ChildSpec}}.
```

Everything else — `supervise`, `current`, all instance-side methods, `childSpec` logic — is inherited from the stdlib abstract bases or lives in `beamtalk_supervisor.erl`. No per-subclass generation needed for any of those.

**`crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs`:**
- Check `supervisor_kind` on `ClassDefinition`; if set, delegate to `supervisor_codegen.rs` instead of generating `gen_server` behaviour

**New file: `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`**

Thin dispatch glue — no construction logic (that lives in `SupervisionSpec childSpec`):
- `build_child_specs/1` — normalize bare class refs to `SupervisionSpec` (via `supervisionSpec`), then call `childSpec` on each
- BEAM interop entry points called by inherited stdlib methods: `startLink:`, `current:`, `whichChildren:`, `whichChild:class:`, `terminateChild:class:`, `startChild:`, `startChild:with:`, `terminateChild:child:`, `countChildren:`, `stop:`
- `is_supervisor/1` — compile-time codegen helper (ancestry check for routing)

**No new lexer tokens, no new parser grammar, no new AST node types** beyond the `supervisor_kind` field already added in Phase 1.

**Full affected file list:**
- `crates/beamtalk-core/src/codegen/core_erlang/supervisor_codegen.rs` — new file (~60 lines)
- `crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs` — add `supervisor_kind` branch (~5 lines)
- `runtime/apps/beamtalk_runtime/src/beamtalk_dispatch.erl` — detect `'beamtalk_supervisor'` tag; route to `Module:'method'(Self)` directly instead of `beamtalk_actor:sync_send/3`
- `runtime/apps/beamtalk_runtime/src/beamtalk_repl.erl` — add `'beamtalk_supervisor'` clause to display `#Supervisor<ClassName, Pid>` and `#DynamicSupervisor<ClassName, Pid>`
- `stdlib/src/Actor.bt` — add `class supervisionPolicy -> Symbol => #temporary` and `class isSupervisor -> Boolean => false`
- `stdlib/src/Supervisor.bt` — `Supervisor` and `DynamicSupervisor` abstract classes with all inherited methods and type annotations
- `stdlib/src/SupervisionSpec.bt` — value type with `childSpec` and fluent override methods
- `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl` — new file
- `runtime/apps/beamtalk_runtime/src/beamtalk_bootstrap.erl` — add `Supervisor`, `DynamicSupervisor` to sequence

### Phase 3 — Tests and Docs (M)

- `stdlib/test/SupervisorTest.bt` — BUnit tests: static supervision (`Supervisor subclass:` with `class children => #(...)`), dynamic supervision (`DynamicSupervisor subclass:` with `class childClass =>`), `supervisionPolicy:` on Actor, `supervisionSpec` fluent overrides (`withId:`, `withRestart:`, `withArgs:`, `withId:withRestart:withArgs:`), `withArgs:` with multiple instances of same class, nested supervisors, conditional `children` method
- `tests/e2e/cases/supervisor.bt` — REPL integration: `WebApp supervise`, `app children`, `app which: DatabasePool`, `pool startChild:`, `pool count`
- `docs/beamtalk-language-features.md` — add Supervision Tree section after Actor Message Passing
- `docs/beamtalk-principles.md` — update Principle 10 from "planned but not available" to "available via `Supervisor subclass:`"

## Migration Path

Not applicable. This ADR introduces new syntax only (`Supervisor subclass:`, `supervisionPolicy:`, `supervisionSpec`). No existing Beamtalk code uses supervision tree syntax — the feature does not exist today. No migration is required.

## References

- Related issues: BT-448 (supervision tree syntax epic), BT-567 (Pharo-style Announcements / pub-sub — named process registry gap), BT-1189 (ETS shared in-memory table class), BT-1190 (actor message timeout configuration syntax), BT-1191 (OTP application root supervisor — `[application] supervisor =` in `beamtalk.toml`)
- Related ADRs: ADR 0009 (OTP Application Structure), ADR 0015 (Signal-Time Exceptions — supervision motivation and pseudocode), ADR 0026 (Package Definition and Project Manifest — `[run] entry =` deferred OTP application callback, to be amended by BT-1191), ADR 0038 (ClassBuilder Protocol — class-level declarations mechanism), ADR 0042 (Immutable Value Objects / Actor-Only Mutable State), ADR 0043 (Sync-by-Default Actor Messaging), ADR 0056 (`@native` Erlang-Backed Actors)
- OTP Supervisor: https://www.erlang.org/doc/man/supervisor.html
- Elixir Supervisor: https://hexdocs.pm/elixir/Supervisor.html
- Armstrong thesis — supervision trees: https://erlang.org/download/armstrong_thesis_2003.pdf

## Implementation Tracking

**Epic:** BT-448 — Design Supervision Tree Syntax for Beamtalk

| Issue | Title | Phase | Size | Status |
|-------|-------|-------|------|--------|
| BT-1217 | Bootstrap Supervisor, DynamicSupervisor, SupervisionSpec stdlib classes | 0 | S | Backlog |
| BT-1218 | AST supervisor_kind + semantic analysis + supervisionSpec synthesis | 1a | S | Backlog |
| BT-1219 | beamtalk_supervisor.erl runtime helper + dispatch routing + REPL display | 1b | S | Backlog |
| BT-1220 | supervisor_codegen.rs — generate -behaviour(supervisor) + start_link/0 + init/1 | 2 | M | Backlog |
| BT-1222 | BUnit tests for Supervisor, DynamicSupervisor, and SupervisionSpec stdlib classes | 3 | M | Backlog |
| BT-1223 | E2E tests and language feature docs for supervision tree syntax | 4 | S | Backlog |

**Recommended start:** BT-1217 (Phase 0, no dependencies)

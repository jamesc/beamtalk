# ADR 0079: Named Actor Registration

## Status
Proposed (2026-04-15)

## Context

Beamtalk actors are currently identified only by pid. An actor reference held by another process is a snapshot — if the target restarts under a supervisor, the stored pid becomes stale and the holder either crashes on the next send or silently messages a dead process.

OTP solves this with the **process registry**: a name (atom) maps to the current pid of a process, and the mapping survives restarts because the supervisor re-registers the name when it restarts the child. Every production Erlang/Elixir system relies on this — it is how `gen_server:call(my_service, ...)` works, how `Application` environments locate singletons, and how supervision trees wire cross-child dependencies without threading pids through startup arguments.

Beamtalk has no equivalent. This forces two workarounds:

**1. The `Supervisor which:` / `initialize:` pattern.** A supervisor exposes `which:` to look up a child by class, and an `initialize:` hook re-runs after every restart to re-wire dependencies. The Exdura workflow engine (`exdura_supervisor.bt`) is a representative example:

```beamtalk
typed Supervisor subclass: ExduraSupervisor
  class strategy -> Symbol => #restForOne
  class children -> List(Actor) =>
    storeSpec := EventStore supervisionSpec withRestart: #permanent
    poolSpec := ActivityWorkerPool supervisionSpec withRestart: #permanent
    engineSpec := WorkflowEngine supervisionSpec withRestart: #permanent
    timerSpec := TimerManager supervisionSpec withRestart: #permanent
    #(storeSpec, poolSpec, engineSpec, timerSpec)

  class initialize: sup :: Supervisor -> nil =>
    store := sup which: EventStore
    pool := sup which: ActivityWorkerPool
    engine := sup which: WorkflowEngine
    timer := sup which: TimerManager
    engine initWithStore: store pool: pool
    timer initWith: engine eventStore: store
    nil
```

Every line of `initialize:` is a workaround for the missing registry. The `WorkflowEngine` class carries an `initWithStore:pool:` method that exists only to receive pids after spawn, and `rest_for_one` is partially chosen to force all downstream children to restart together so their stale-pid caches get rebuilt. With named registration, `initialize:` disappears entirely and the engine calls `Actor named: #eventStore` at use-time — automatically picking up the current pid across restarts.

**2. Cross-tree consumers can't use `which:` at all.** Anything outside the owning supervisor — an HTTP handler, a test harness, a REPL workspace — has no way to locate a supervised actor without a pid being passed to it. Named processes eliminate the need.

ADR 0065 explicitly deferred this to a dedicated ADR and proposed that naming would live on the `Server` class. This ADR revises that placement (see Alternatives Considered): naming is about process identity, not raw-message handling, and forcing users to subclass `Server` purely to name a process would conflate two unrelated decisions. The four classes in the Exdura example are Actors, not Servers, and would remain so.

### Constraints
- Must map to a well-understood OTP primitive — no bespoke runtime registry.
- Must survive supervised restarts: a held reference to `#eventStore` should resolve to whatever pid is currently registered, not a pid captured at acquisition time.
- Must not break existing `Actor spawn` / `spawnWith:` semantics — registration is opt-in.
- Names use Beamtalk `Symbol`, which maps 1:1 to Erlang atom — no new term type.
- The final API must leave room for future cluster-wide (`global`) and pluggable (`{via, Mod, Term}`) registration without breaking changes.

## Decision

Add a local-scope process registry on `Actor`, backed by `erlang:register/2` and `erlang:whereis/1`. Introduce a name-resolving proxy so held references survive restarts.

### API

**Spawn + register (returns `Result(Actor, Error)`):**
```beamtalk
Counter spawnAs: #counter                                // -> Result(Counter, Error)
Counter spawnWith: #{#count => 10} as: #counter         // -> Result(Counter, Error)

(Counter spawnAs: #counter)
  onSuccess: [:c | c increment]
  onError: [:e | Logger warn: "name taken: " ++ e printString]

c := (Counter spawnAs: #counter) unwrap   // explicit "I expect success; crash if not"
```

**Register/unregister an existing actor:**
```beamtalk
someActor registerAs: #myName       // -> Result(Actor, Error) — Ok carries the receiver for fluent chaining
someActor unregister                // -> Symbol (#ok, idempotent — teardown of own resource)
someActor registeredName            // -> Symbol or nil
someActor isRegistered              // -> Boolean
```

**Why `Result` here, not raise?** `spawnAs:` and `registerAs:` operate at a registry boundary where multiple legitimate outcomes exist (`#ok` vs `#name_registered` vs `#reserved_name`) — the same shape OTP itself uses for `gen_server:start_link/3,4` and `supervisor:start_child/2`. This is *consistent* with let-it-crash, not in tension with it: let-it-crash applies to operational errors during normal running; tagged-tuple/Result returns are appropriate at startup boundaries where the caller (often a supervisor) needs to distinguish outcomes. Callers who want the crash-on-failure form write `(spawnAs: #foo) unwrap` explicitly.

By contrast, `unregister`, `stop`, and `kill` return `Symbol` because they are teardowns of the actor's own resources where "already gone" is benign and any *real* failure is a programming bug — raise is correct there. See "Future Work" below for the planned migration of Supervisor lifecycle methods to the same Result convention.

`spawnAs:` is the atomic form: it uses `gen_server:start_link({local, Name}, Module, Args)`, registering the name during process startup. `registerAs:` post-spawn is non-atomic — between `spawn` and `registerAs:` another process could claim the name. Prefer `spawnAs:` whenever the name is known up front; reserve `registerAs:` for cases where naming is decided dynamically after construction.

When an actor process exits, Erlang unregisters its name automatically. There is no need to call `unregister` from `terminate:`.

**Lookup (typed, declared once on `Actor`):**
```beamtalk
class named: name :: Symbol -> Result(Self, Error)
```

`Self` resolves to the receiver class at the call site, so subclasses inherit a typed lookup with no per-class redeclaration:

```beamtalk
Counter named: #counter            // -> Result(Counter, Error)
WorkflowEngine named: #workflowEngine   // -> Result(WorkflowEngine, Error)
Actor named: #anything             // -> Result(Actor, Error) — base/untyped form
```

The lookup performs a runtime class check using the `'$beamtalk_actor' => ClassName` process-dict marker (see Implementation):
- `Ok(actor)` — name is registered and the registered actor's class is (or descends from) the receiver class.
- `Error(#name_not_registered)` — nothing registered under this name.
- `Error(#wrong_class)` — registered, but the actor is not a `Self` (or subclass thereof). E.g., `Counter named: #x` when `#x` is registered to a `Logger`.

Class hierarchy walk is part of the check: `Counter named: #x` succeeds for any `Counter` *or any subclass of `Counter`*.

**Enumeration (tooling/REPL discovery):**
```beamtalk
Actor allRegistered                  // => Array(Actor) of currently-registered Beamtalk actors
```

`allRegistered` returns resolved `Actor` proxies (not symbols), paralleling `Class allClasses`. Each proxy carries its real class — `Actor allRegistered first class` returns `Counter`, not `Actor`. The list excludes raw Erlang FFI-registered processes (kernel, logger, mnesia, user-registered Erlang pids) — they are not Beamtalk actors and the API would lie about their type. This method is intended primarily for tooling and REPL exploration; production code should use `Actor named: #foo` to address known names directly rather than enumerate.

`Actor named:` returns a lightweight proxy. The proxy does **not** cache a pid; each message send re-resolves the name to the current pid via the Erlang runtime. This is the key restart-survival property:

```beamtalk
engine := Actor named: #workflowEngine
engine runWorkflow: w1    // resolves #workflowEngine, sends to that pid
// (workflowEngine crashes and is restarted by its supervisor)
engine runWorkflow: w2    // re-resolves #workflowEngine, sends to the NEW pid
```

If the name is not currently registered at send time, the send raises a `#beamtalk_error{kind: #no_such_process, name: ...}` — the same shape as other structured runtime errors.

**Supervisor integration:**
```beamtalk
Supervisor subclass: ExduraSupervisor
  class strategy -> Symbol => #restForOne
  class children -> List(Actor) => #(
    EventStore supervisionSpec withName: #eventStore withRestart: #permanent,
    ActivityWorkerPool supervisionSpec withName: #workerPool withRestart: #permanent,
    WorkflowEngine supervisionSpec withName: #workflowEngine withRestart: #permanent,
    TimerManager supervisionSpec withName: #timerManager withRestart: #permanent
  )
  // No initialize: hook needed.
```

`SupervisionSpec withName:` tells the runtime to start the child with `{local, Name}` registration — matching the OTP idiom — so the name is re-established every time the supervisor restarts the child. `WorkflowEngine` internally does `Actor named: #eventStore` whenever it needs the store; no wiring call required.

### Errors

| Condition | Result |
|---|---|
| `spawnAs:` / `registerAs:` — duplicate registration | `Error(#beamtalk_error{kind: #name_registered, name: ...})` |
| `spawnAs:` / `registerAs:` — invalid name (non-Symbol) | `Error(#beamtalk_error{kind: #type_error, ...})` |
| `spawnAs:` / `registerAs:` — reserved name (see below) | `Error(#beamtalk_error{kind: #reserved_name, name: ...})` |
| `spawnAs:` / `registerAs:` — success | `Ok(actor)` |
| `T named:` — name not registered | `Error(#beamtalk_error{kind: #name_not_registered, name: ...})` |
| `T named:` — registered to actor of wrong class | `Error(#beamtalk_error{kind: #wrong_class, name: ..., expected: ..., actual: ...})` |
| `T named:` — success | `Ok(actor)` |
| Send to proxy whose name is not currently registered (e.g., target died after lookup) | Raises `#beamtalk_error{kind: #no_such_process, name: ...}` |
| `unregister` on unregistered actor | `#ok` (idempotent, raises only on type error) |

Lookup returns `Result(Self, Error)` rather than raising — callers branch explicitly on presence and type compatibility (`Ok(actor)` / `Error(#name_not_registered)` / `Error(#wrong_class)`). Sending to a proxy whose name has since vanished (the process died between lookup and send) is distinct and *does* raise, because the caller has already committed to a send.

**Reserved names** — the following OTP-kernel atoms are blocked at registration time, regardless of whether the corresponding process is currently running:

```text
application_controller, code_server, error_logger, file_server_2,
global_name_server, init, inet_db, kernel_safe_sup, kernel_sup,
logger, net_kernel, rex, standard_error, standard_error_sup,
user, user_drv
```

Plus any atom prefixed with `beamtalk_` (reserves the namespace for runtime infrastructure). The list is small, static, and lives next to the register intrinsic in `beamtalk_actor.erl`. Beamtalk-stdlib singletons will be added to the list as they are introduced (none today). User code that registers names via Erlang FFI is not statically blockable — those collisions surface naturally as `#name_registered` errors.

### REPL session

```text
> c := (Counter spawnAs: #counter) unwrap
 => an Actor(Counter)
> c registeredName
 => #counter
> (Counter named: #counter) unwrap increment
 => 1
> Counter spawnAs: #counter
 => Error(#beamtalk_error{kind: #name_registered, name: #counter})
> Logger named: #counter
 => Error(#beamtalk_error{kind: #wrong_class, name: #counter, expected: Logger, actual: Counter})
> c stop
 => #ok
> Counter named: #counter
 => Error(#beamtalk_error{kind: #name_not_registered, name: #counter})
```

### Scope

This ADR covers **local (per-node)** registration only. Cluster-wide (`global`) and pluggable (`{via, Module, Term}`) registration are deferred to a future ADR. The API is designed to admit them additively via a `scope:` keyword:

```beamtalk
// Future, not part of this ADR:
Counter spawnAs: #counter scope: #global
Actor named: #counter scope: #global
spec withName: #counter scope: #global
```

No such method is introduced now. Users who need cluster registration today can call the Erlang `global` module via FFI.

## Prior Art

**Erlang / OTP.** The reference. `erlang:register(Name, Pid)`, `erlang:whereis(Name)`, `erlang:unregister(Name)` — all take atoms, all are per-node. Sending via `Name ! Msg` and `gen_server:call(Name, Msg)` transparently resolves the name. `{local, Name}` in `gen_server:start_link` registers atomically at process start. **This ADR adopts the Erlang model directly**, exposing it through Beamtalk's message-passing surface.

**Elixir.** Same primitives (`Process.register/2`, `Process.whereis/1`) plus `GenServer.start_link(Module, Args, name: Name)`. Elixir also popularised `{:via, Registry, {registry, key}}` via the `Registry` module, which we leave as future work.

**Pharo / Squeak.** `SmalltalkImage current at: #MyService put: anObject` — a single global `SystemDictionary`. Gives ergonomic name lookup but has no restart semantics (Smalltalk has no processes in the OTP sense). We adopt the *ergonomics* (Symbol-keyed lookup feels Smalltalk-native) but ground the semantics in OTP restart behavior.

**In-Beamtalk precedent.** The `named:` class-method convention is already established: `Package named: "stdlib"` (`stdlib/src/Package.bt`) and `AtomicCounter named: #hits` (`stdlib/src/AtomicCounter.bt`) both use Symbol-keyed constructors that resolve a runtime entity by name. `Actor named:` extends the same convention to processes.

**Newspeak.** Module instances are composed through explicit slot wiring; there is no global registry. Beamtalk's package namespacing already covers module composition; process identity is a different concern and belongs in a registry.

**Akka (Scala).** Actor paths (`/user/worker1`) give hierarchical, restart-safe addressing. More powerful than a flat atom namespace, but substantially more machinery. Not a fit for v1 — we can layer path-like addressing later via `{via, ...}` if needed.

**Gleam.** Uses OTP naming under the hood via `gleam_otp`. Same primitive, different typed surface.

## User Impact

**Newcomer (Python/JS background).** "Name this thing so I can find it later" is a universal pattern (service locators, `app.config['db']`, module-level singletons). `spawnAs: #counter` / `Actor named: #counter` should be guessable from the REPL and reads like English. Error message when forgetting to register is explicit (`#no_such_process, name: #counter`), pointing at the fix.

**Smalltalk developer.** Registration feels like `SystemDictionary at:` — Symbol-keyed, reflection-friendly, inspectable. The `Actor named:` proxy preserves message-passing purity: you still send messages to an object, the object just happens to be a name-resolving handle. No new syntax required.

**Erlang/Elixir developer.** Direct mapping to `register/2`, `whereis/1`, `{local, Name}`. `Actor named:` returning a proxy that forwards to `gen_server:call({local, Name}, ...)` is idiomatic — and means the BEAM developer can `observer:start()` and see processes registered with the expected atoms. No magic.

**Production operator.** Standard BEAM tools work: `erlang:registered/0` shows all names; `observer` and `recon` see names as first-class. Reserved-name blacklist prevents accidental clobbering of `kernel`, `logger`, etc. No hot-code-reload impact (names are runtime state, not compile-time).

**Tooling developer.** LSP can offer completion for known registered names by scanning supervision trees statically (`Supervisor children` + `withName:` arguments). `Actor named:` is a single method, trivial to recognise. No new AST node.

## Steelman Analysis

### Option A (Actor-level, all three scopes now) — Rejected

- Newcomer: "If the tutorial walks me through local naming and then a later chapter introduces `scope: #global` as a separate concept, I'll experience that as 'the language changed.' One richer signature on day one is friendlier than two near-identical concepts to keep straight."
- BEAM veteran: "`{via, Mod, Term}` is how real OTP systems register — pg, syn, gproc. Shipping without it tells me Beamtalk doesn't really mean it about distributed work."
- Operator: "Distributed naming on day one means no `global:register_name` FFI hacks creeping into user code that I'll have to grep for during incidents."
- Language designer: "Designing all three scopes together forces you to find the right factoring up front. Designing local in isolation risks discovering later that the abstraction should have been `register: scope:` from the start — at which point the local-only signature is a wart you can't remove."
- Smalltalk purist: (no strong view — registration ergonomics dominate; scope choice is BEAM-specific machinery)

**Why not chosen:** Global registration has netsplit semantics that deserve their own design treatment (leader election, conflict resolution, when to use `global` vs `pg` vs external registries). Shipping it bundled with local registration risks baking in defaults we'd revisit. The `scope:` keyword leaves the door open additively. The language designer's factoring concern is the strongest argument *for* A; the mitigation is that `scope:` is a clean keyword extension — adding it later does not reshape the rest of the API.

### Option C (Server-only naming, per ADR 0065's deferred intent) — Rejected

- Newcomer: "When I see `Server subclass:` I know this thing is publicly addressable — the class choice carries information. If every Actor can be named, I have to read the supervisor to learn what's externally visible vs internal scaffolding."
- Smalltalk purist: "Identity is a serious property — forcing `Server subclass:` makes the weight of that decision explicit. Actors should stay simple; processes-with-names are a different category of object."
- BEAM veteran: "ADR 0065 split Server precisely to mark 'this is a process you reason about as a process.' Naming is a process-identity decision. Putting it on Actor weakens the discipline 0065 set up."
- Operator: "When I `observer:start()` and see a registered process, knowing it had to be declared `Server subclass:` tells me at a glance that the author thought about its public surface. Removing that signal removes a debugging hint."
- Language designer: (no strong view — placement is a stylistic choice; the API is identical either way)

**Why not chosen:** ADR 0065's Server split was specifically about raw BEAM primitives (`handleInfo:`, `trapExit`, `codeChange:`) — mechanics that require the user to think in BEAM terms. Named registration is orthogonal: the user still speaks in Beamtalk messages. The newcomer/operator "class choice carries information" argument is real but addressable through documentation and naming convention (e.g., supervised+named actors are de facto public). The Exdura example (`EventStore`, `ActivityWorkerPool`, `WorkflowEngine`, `TimerManager`) uses Actors; forcing them to become Servers just to get names would conflate "needs an identity" with "needs raw message access." This ADR supersedes 0065's placement suggestion; the deferral itself stands.

### Option D (Beamtalk-native `Registry` class) — Rejected

- Newcomer: "A `Registry` object is a thing I can pass around, scope, and reason about. A flat global namespace is invisible — I can't tell what's registered without out-of-band tooling."
- Smalltalk purist: "First-class objects over magic global tables. A `Registry` instance is inspectable; `erlang:registered/0` is not a Beamtalk object."
- BEAM veteran: "OTP's flat atom namespace is a known production footgun: atom exhaustion, no scoping, no per-tenant isolation. Designing a Beamtalk-native Registry from the start means we never inherit the wart."
- Operator: "I lose `observer` integration, but I gain the ability to scope registries (per-app, per-tenant) and enumerate what's where. `recon` and our own tooling can replace observer's name view."
- Language designer: "This is the same shape as Elixir's `Registry` and the eventual `{via, ...}` story — designing the abstraction now means we don't paint ourselves into a corner."

**Why not chosen:** The steelman is genuine, but the tradeoff lands the wrong way for v1: invisible-to-OTP-tooling is a *real* cost (observer, recon, OTP error reports all key off `erlang:registered/0`), and the namespacing benefit is exactly what the deferred `{via, Module, Term}` ADR will deliver — at which point users get the best of both. Shipping a Beamtalk-only Registry now would conflict with that future design.

### Option E (Auto-healing pid handles) — Rejected

- Newcomer: "Names are a tax I have to remember. If references just *work* across restarts, that's strictly simpler — hold a ref, send to it, done."
- Smalltalk purist: "Object identity should be stable. A handle that auto-updates when the underlying process restarts preserves the illusion of a single object — exactly what message passing is supposed to give us."
- BEAM veteran: "OTP supervision is the abstraction; pids are the implementation. If references hide the pid churn, users never have to learn that distinction."
- Language designer: "Naming is a workaround for pids being unstable. The right fix is to fix pids, not introduce a parallel naming scheme."
- Operator: (no support — invisible to `observer`, harder to debug)

**Why not chosen:** The user-model simplicity is real, but the implementation is a distributed-systems problem in disguise: how does a holder learn its target restarted? Either the holder subscribes to the supervisor (cross-tree coupling, doesn't scale) or the runtime maintains a global handle-to-current-pid table (which is just a registry with worse ergonomics). Equality also breaks subtly: two holders of the "same" reference may observe different restart events. The OTP ecosystem tried similar mechanisms (process aliases, `monitor` + reconnect) and converged on names because they're operationally legible.

### Option F (Implicit `singleton` modifier) — Rejected (revisit if explicit form proves noisy)

- Newcomer: "I want to say 'there's one EventStore' and have it work. Declaring the class **and** registering its name is bookkeeping I'll forget — the Exdura supervisor having to spell out `withName: #eventStore` next to `EventStore` is obviously redundant."
- Smalltalk purist: "Singletons are a recognised pattern (`Smalltalk current`); a class-side declaration is the right Smalltalk-y way to express 'one of these exists.'"
- BEAM veteran: "If 95% of named processes are one-per-class, factor that case out. The remaining 5% can use explicit `withName:`."
- Operator: "Less ceremony in supervisor specs means fewer typos in production code."
- Language designer: "The current design forces every supervised singleton to repeat its name in two places. That's a clear DRY violation."

**Why not chosen:** Strong steelmans across the board, and the Exdura example is exactly the singleton-per-class pattern this would optimise. But: (a) implicit registration hides a global side-effect of a class declaration, which conflicts with Beamtalk's preference for explicit message-passing semantics; (b) it collapses "this class is a singleton" with "this process has a public name," which are conceptually distinct (you might want one without the other); (c) the explicit form costs one keyword and preserves the ability to spawn multiple instances with different names. **Worth revisiting** if real-world usage shows the explicit form is genuinely noisy. For v1, explicit wins on conceptual clarity.

### Option Z (Status quo: keep `which:` only) — Rejected

- Newcomer: "Less to learn. The supervisor pattern is enough for my first apps."
- Smalltalk purist: "Adding a global namespace is a step away from message-passing purity — a registry is a side channel for object identity outside the message graph."
- BEAM veteran: "OTP ran production systems for 30 years with `which:`-equivalents and explicit wiring. Eight lines of `initialize:` in Exdura is not a crisis."
- Operator: "Atom exhaustion is a real BEAM footgun; deferring named registration defers that whole class of incident."
- Language designer: "Doing nothing is always the cheapest option. If the only motivating example is one project's `initialize:` hook, the cost-benefit doesn't justify a language feature."

**Why not chosen:** The operator and language-designer steelmans have real bite, but the Exdura pattern is not unique — every multi-actor system needs cross-process discovery, and `which:` doesn't compose across supervision trees (HTTP handlers, REPL workspaces, tests can't use it). ADR 0065 itself flags `which:` as O(n) and a motivator for this ADR. Atom exhaustion is mitigated by the bounded-naming guidance in Negatives + the deferred `{via, Module, Term}` story for unbounded keys. The OTP ecosystem long ago concluded that named registration is the right answer; deferring further imposes ongoing tax on every Beamtalk app that grows past a single supervisor.

### Tension Points

- **Scope coverage (A vs B):** BEAM veterans and language designers want the full factoring up front; operators and the project's incrementalist posture prefer the smaller surface. The `scope:` extension point is the load-bearing mitigation — if it weren't a clean keyword extension, A would win.
- **Class placement (B vs C):** Newcomers and operators see appeal in C's "the class signals it's public" property. Ergonomic considerations and the concrete Exdura use case decisively favor B. Documentation should compensate by making "supervised + named = de facto public" a stated convention.
- **Singleton ergonomics (B vs F):** F's steelman is the strongest of any rejected option. The decision rests on conceptual clarity (explicit > implicit registration) over DRY. If post-shipping usage shows the explicit form is repeatedly painful, F is the most likely revisit.
- **OTP integration vs Beamtalk-native (B vs D):** Choosing OTP's flat namespace inherits its warts (atom exhaustion, no scoping) but keeps `observer`/`recon`/error-report integration. The deferred `{via, Module, Term}` ADR is the planned answer to D's namespacing concerns.

## Alternatives Considered

### Option Z: Status quo — keep `Supervisor which:` as the only mechanism

Recommend that users continue solving startup discovery via `Supervisor which:` and per-supervisor `initialize:` hooks.

**Rejected because:** The Exdura example demonstrates concretely that this pattern (a) requires an `initialize:` hook on every supervisor with cross-child wiring, (b) doesn't compose across supervision trees (HTTP handlers, REPL workspaces, tests can't use it), (c) ties dependency resolution to `rest_for_one` so cached pids stay coherent, and (d) is O(n) in child count per ADR 0065's own caveat. The OTP ecosystem long ago concluded that named registration is the right answer; deferring further imposes ongoing tax on every Beamtalk app that grows past a single supervisor.

### Option A: Actor-level, all three scopes at once

Same API as the chosen option, but `scope:` keyword added immediately, supporting `#local`, `#global`, and `{via, Mod, Term}`.

**Rejected because:** Global and via registration have meaningful semantic surface (netsplits, leader election, Registry module design) that deserves its own ADR. Shipping them now would lock in defaults before we have in-tree consumers driving the design. The chosen API is forward-compatible.

### Option C: Naming on Server

Only `Server subclass:` classes can be named. Exdura's four classes would need to become `Server subclass:`.

**Rejected because:** ADR 0065 drew the Actor/Server boundary at raw-message handling, not identity. Requiring a class-hierarchy change purely for naming punishes the common case and conflates two unrelated concerns. See Steelman Analysis for the full argument.

### Option D: `Registry` class with explicit namespaces

Introduce a `Registry` object; names are keyed in a registry rather than a single global namespace:

```beamtalk
appRegistry := Registry new
appRegistry register: counter as: #counter
c := appRegistry at: #counter
```

**Rejected because:** It re-solves a problem OTP already solved per-node. The flat atom namespace is what every BEAM library expects; a Beamtalk-specific `Registry` would be invisible to OTP tooling (observer, dbg, `gen_server:call/2`). Namespaced registries are valuable at cluster scale — exactly the design space we're deferring to a future `{via, ...}` ADR.

### Option F: Implicit `singleton` modifier — auto-name after class

Introduce a class-side `singleton` flag; classes so marked are automatically registered under a name derived from the class name (`EventStore` → `#eventStore`).

```beamtalk
Actor singleton subclass: EventStore  // implicitly registered on spawn
```

**Steelman:** Many real uses of named processes *are* one-per-class singletons (Exdura is exactly this). Implicit naming would make the common case zero-config — no `withName:` clutter on every supervision spec.

**Rejected because:** Implicit registration hides a global side effect of a class declaration, which conflicts with Beamtalk's preference for explicit message sends. It also collapses the singleton-vs-instance design choice into a class-level flag, where today a user can spawn two `Counter`s with different names. The explicit `spawnAs:` / `withName:` form costs one keyword and preserves both clarity and flexibility. Worth revisiting if a "singleton actor" pattern emerges that shows the explicit form is genuinely noisy in practice.

### Option E: Pid handles with automatic restart-tracking

Make `Actor` references internally subscribe to exit signals and transparently update on supervisor restart — no explicit naming.

**Rejected because:** It's a Beamtalk-invented mechanism competing with OTP's registry. Two pids holding the "same" reference could see different restart events, breaking equality. Doesn't work across-tree (the holder may be in a different supervision subtree and can't observe the target's supervisor). Enormous runtime complexity for a worse version of the standard solution.

## Consequences

### Positive
- Eliminates the `Supervisor which:` + `initialize:` re-wiring pattern for the common case of one-child-per-class.
- Cross-supervision-tree access becomes trivial (`Actor named: #foo` from anywhere).
- Held references survive restarts without caller action.
- Opens relaxation of `#restForOne` where it was chosen only for re-wiring (not for data coherence).
- Maps 1:1 to OTP primitives — observable in `observer`, `recon`, `erlang:registered/0`.
- Reduces boilerplate in supervisor subclasses; less surface area for startup-order bugs.

### Negative
- Introduces a flat per-node atom namespace — collisions must be managed by convention.
- **Atom exhaustion is a theoretical risk** if users register dynamically-generated names (e.g., `#user_42`, `#req_abc123`). Compile-time symbol literals do consume atom-table entries when the module loads, but the set is bounded by program size — registering `Counter spawnAs: #counter` adds no *new* atoms beyond what the compiler already emitted for the literal. The realistic exhaustion path is a user reaching for `String asSymbol` or `(Erlang erlang) binary_to_atom:` in a per-tenant/per-request loop, where each call mints a previously-unseen atom. Documentation should steer users toward bounded, statically-known names and recommend `{via, Module, Term}`-backed registries for genuinely unbounded keys — exactly the use case the deferred future-ADR will address.
- `T named:` returning `Result(Self, Error)` requires callers to handle `Error(#name_not_registered)` / `Error(#wrong_class)` explicitly (via `onSuccess:/onError:` or `unwrap`). This is deliberately more ceremony than a raw pid/nil pair — the type system catches forgotten checks — but can feel noisy at the REPL. Mitigated by `unwrap` for "I expect this to succeed" and by `Actor allRegistered` for discovery flows.
- Reserved-name blacklist requires ongoing curation as the stdlib and OTP grow.
- Proxy handles add a small runtime indirection per send (one extra `whereis` lookup). Negligible in practice but not zero.
- **Test isolation.** Two test cases registering the same name will conflict if run concurrently in the same node. Interim guidance: tests should suffix names with a per-test discriminator (e.g., `#counter_<testId>`), being mindful that atom-suffixed names participate in the atom-exhaustion concern above — bound the suffix space (test method name, not an unbounded counter). A first-class `unique:` test helper is deferred to a follow-up; the right shape will become clear once we observe stdlib + Exdura usage patterns.

### Neutral
- `Supervisor which:` remains — still useful for dynamic children without names and for introspecting un-named children.
- `SupervisionSpec withName:` is additive; existing supervisor definitions continue to work unchanged.
- Future `scope:` keyword will extend the API without breaking existing callers.
- **Hot code reload:** registered names persist across module upgrade — the registration is process state, not module state. This is generally desirable (named services stay reachable across upgrades) but means a class rename does not transparently rename the registered process.

## Implementation

**Existing runtime support.** `beamtalk_actor:start_link/3` already accepts `{local, Name} | {global, Name} | {via, Mod, Term}` as its first argument (called out in ADR 0065). Most of the runtime plumbing is in place; this ADR is mostly stdlib API + wiring it through `SupervisionSpec`.

**Implementation risk: `Result(Self, ...)`.** ADR 0079's typed `class named:` declaration uses `Self` as a type argument to a generic (`Result(Self, Error)`). `Self` and parameterised types both exist in the typechecker (ADR-adjacent commit `facc2d52`), but no current stdlib code combines them — `grep -r 'Result(Self' stdlib/src/` returns no matches. If the typechecker doesn't already substitute `Self` correctly inside generic type arguments, this ADR's typed lookup needs a small targeted typechecker fix (extending the existing `Self`-substitution code path to recurse into generic arguments). It is not a redesign — `Self` semantics are already defined; this is a missing case. Phase 0 of implementation should be a one-line typechecker probe to determine whether the fix is needed before the API work begins.

**Affected components:**
- **Stdlib (`stdlib/src/Actor.bt`):** add `spawnAs:`, `spawnAs:with:`, `registerAs:`, `unregister`, `registeredName`, `isRegistered`, `class named:`, `class allRegistered`. The `class named:` method follows the existing `Package named:` / `AtomicCounter named:` convention. `allRegistered` returns resolved `Actor` proxies (parallel to `Class allClasses`, ADR/BT-1953).
- **Stdlib (`stdlib/src/SupervisionSpec.bt`):** add `name` field and `withName:` / multi-keyword combinators. `childSpec` selects a new `startFn = #spawnAs:` (or `#spawnAsWith:`) when `name` is set, since the existing `#spawn` / `#spawnWith:` selectors don't carry a name argument.
- **Runtime (`runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl`):** intrinsics for register/unregister/whereis, reserved-name check, and a `spawnAs/2,3` entry point that delegates to the existing `start_link/3` with `{local, Name}`. **Add a `'$beamtalk_actor' => ClassName` marker to the process dictionary in `init/1`** — this is the basis for `allRegistered` filtering (`is_beamtalk_actor(Pid)` becomes a fast `erlang:process_info(Pid, dictionary)` lookup) and unlocks future tooling that needs to distinguish Beamtalk actors from raw OTP processes (debugger filtering, observer integration).
- **Runtime (`beamtalk_actor.erl` send-site or new proxy module):** name-resolving proxy. The cheapest implementation is a tagged record `#beamtalk_named_actor{name = Atom, class = ClassName}` that the existing actor send site recognises and dispatches via `gen_server:call(Name, ...)` instead of `gen_server:call(Pid, ...)`. OTP's `gen_server:call/2` accepts a registered atom directly, so no extra runtime work beyond the recognition branch.
- **Runtime (`beamtalk_supervisor.erl`):** translate `#spawnAs:` / `#spawnAsWith:` startFns from the child spec into the appropriate `start_link({local, Name}, Module, Args)` call.
- **Codegen:** no new AST nodes. New intrinsics slot into the existing intrinsic dispatch.
- **REPL / observability:** workspace command to list registered names is out of scope for this ADR but a natural follow-up. Inspecting an `Actor named:` proxy should show `{name, current pid}` rather than treating it as an opaque object.

**Phases:**
1. Runtime intrinsics + EUnit tests (register, whereis, reserved-name policy).
2. `Actor` stdlib API (`spawnAs:`, `registerAs:`, `named:`) + BUnit tests.
3. Proxy-dispatch path + integration tests for restart survival.
4. `SupervisionSpec withName:` + supervisor `start_link` wiring + e2e btscript tests.
5. Docs update (`docs/beamtalk-language-features.md` actor chapter).

Estimated effort: M (4–6 medium Linear issues).

## Migration Path

No migration required — this is purely additive.

Existing code using `Supervisor which:` / `initialize:` continues to work. Projects can migrate to named registration opportunistically. A representative migration:

**Before (Exdura):** 4 `supervisionSpec withRestart:` lines + 8-line `initialize:` hook + `initWithStore:pool:` methods on `WorkflowEngine` and `TimerManager`.

**After:** 4 `supervisionSpec withName: ... withRestart:` lines. Delete `initialize:`. Replace `initWithStore:pool:` with `Actor named: #eventStore` calls inside the methods that actually use the store.

A follow-up issue may add a linter/warning for "supervisor `initialize:` hook that only performs `which:` lookups," suggesting the named-registration replacement.

## Future Work

This ADR establishes `Result(Actor, Error)` as the return shape for boundary operations on `Actor` (`spawnAs:`, `registerAs:`). The same reasoning applies to several Supervisor lifecycle methods that currently raise:

- `Supervisor supervise` / `DynamicSupervisor supervise` — `start_link` returns `{ok, Pid} | {error, {already_started, Pid}} | {error, Reason}`. The `already_started` case carries genuinely useful information (the existing pid) that raise-style discards.
- `DynamicSupervisor startChild` / `startChild:` — `supervisor:start_child/2` is the canonical Result-shaped OTP API: `{ok, Pid} | {ok, Pid, Info} | {error, already_present} | {error, {already_started, Pid}} | {error, Reason}`.
- `Supervisor terminate:` / `DynamicSupervisor terminateChild:` — can fail with `not_found`.

A follow-up ADR ("Migrate Supervisor Lifecycle to Result") should do the whole class together with a coordinated migration plan for existing user code (Exdura, symphony, etc., which all call `supervise`). This ADR provides the precedent.

`Actor named:` / `T named:` returns `Result(Self, Error)` (per this ADR's main contract). The Supervisor lookup methods (`Supervisor which:`, `Supervisor current`) stay nil-on-miss for now — their migration is part of the Supervisor lifecycle follow-up ADR, which can decide coherently across the whole class.

Teardown methods (`Actor stop`, `Actor kill`, `Supervisor stop`, `unregister`) deliberately stay raise-on-real-failure with idempotent success — let-it-crash applies to teardown of own resources.

## References
- Related issues: BT-XXX (to be created by `/plan-adr`)
- Related ADRs:
  - ADR 0065 — Complete OTP Primitives & Actor Lifecycle (deferred this ADR; this ADR supersedes its placement suggestion on Server)
  - ADR 0056 — Native Erlang-Backed Actors (dispatch infrastructure the proxy builds on)
  - ADR 0059 — SupervisionSpec (fluent builder this extends)
  - ADR 0078 — Actor Initialize Inheritance
- External: [Erlang `erlang:register/2`](https://www.erlang.org/doc/man/erlang#register-2), [Elixir `Process.register/2`](https://hexdocs.pm/elixir/Process.html#register/2), `global(3erl)`, `pg(3erl)`
- Motivating example: [`beamtalk-exdura/src/exdura_supervisor.bt`](https://github.com/jamesc/beamtalk-exdura/blob/main/src/exdura_supervisor.bt)

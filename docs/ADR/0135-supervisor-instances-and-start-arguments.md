# ADR 0135: Supervisor Instances and Start Arguments

## Status
Proposed (2026-10-20). Extends ADR 0059 and delivers ADR 0080 § Future Work #4 ("anonymous supervisor start"). Specifies BT-3962 and BT-3963.

## Context

### What a supervisor is today

ADR 0059 made supervisors classes whose configuration is a set of class-side methods: `class strategy`, `class children` (static) or `class childClass` (dynamic), `class maxRestarts`, `class restartWindow`, later `class initialize: sup` (BT-1285). Starting one is `MySup supervise`, which the runtime turns into

```erlang
supervisor:start_link({local, 'bt@MySup'}, 'bt@MySup', [])   % supervisor_codegen.rs:257-266
```

so every supervisor class is **a singleton per node, registered under its module name**, `supervise` is idempotent (ADR 0080's "idempotent-startup convention": `Ok(sup)` whether this call started it or it was already running), and `MySup current` is `whereis(Module)` (`beamtalk_supervisor.erl:231`). The generated `init/1` ignores its argument and reads the class-side methods by direct module call (`static_init/2`, `dynamic_init/2`, `beamtalk_supervisor.erl:176-230`).

ADR 0059 was explicit about the consequence: "only one instance of `WebApp` can run at a time … **If two instances of the same topology are needed, define two classes.**" It also said that an ad-hoc `DynamicSupervisor new supervise` would use anonymous registration; that was never implemented. ADR 0080 listed the gap as Future Work #4: "Anonymous supervisor start — `supervisor:start_link/2` without `{local, Name}`. Enables per-tenant / per-request supervision trees where the same `Supervisor` subclass is instantiated multiple times. Requires a companion ADR on how auto-registration-by-class-name interacts with explicit anonymous starts."

This is that ADR.

### Why "define two classes" stops working

The Thousand Island port (Catalina, `beamtalk-catalina/PLAN.md`) needs this tree **per running server**, with `numAcceptors` (default 100) copies of the middle layer:

```
ServerSupervisor (restForOne)                  args: ServerConfig
├── Listener
├── AcceptorPoolSupervisor (oneForOne)         children: 1..numAcceptors
│   └── AcceptorSupervisor[i] (restForOne)     args: {i, config}
│       ├── ConnectionSupervisor (dynamic)     childClass: config handler   maxChildren: numConnections
│       └── Acceptor[i]
└── ShutdownListener
```

Nothing here can be a class-per-instance: the count comes from an option, the handler class is the user's, and a node may run several servers plus a test suite that starts one per test. Today's runtime also hard-wires a *nested* supervisor child to `{Module, start_link, []}` with `id => Class` (`build_child_spec/2`, `beamtalk_supervisor.erl:1166-1172`), so a parent cannot even hold two children of one supervisor class.

The same shape already exists in first-party code: `beamtalk-exdura`'s `ActivityWorkerSupervisor` is a singleton shared by every `ActivityWorkerPool` ("`maxRestarts => 1000` since all pools share this singleton supervisor") where one supervisor per pool is what the design wants; and every supervisor test in `stdlib/test` is `serial` because "supervise/stop registers a single name".

### Constraints

- **`class initialize: sup` runs in the caller's process.** ADR 0080 Option 3 (a helper process) was rejected because it deadlocks against the class gen_server. Any new start path keeps the hook in the caller, and the BT-3759 init gate (keyed by pid) must keep working.
- **Configuration is read in the supervisor's `init/1` by direct module call**, not through the class gen_server (deadlock, `beamtalk_supervisor.erl:165-173`). Args must reach that path.
- **Atoms are not collected.** Per-instance *names* must come from a bounded, statically-known namespace (ADR 0079, language-features § `spawnAs:`); per-instance *ids* in a parent must not mint atoms.
- **Non-breaking for the three packages** that use supervisors today: exdura (`ExduraSupervisor supervise valueOrDo:` relies on idempotence; `[application] supervisor =`), symphony (`class initialize: sup`, computed `children` that read files), watcher (`(MonitorSupervisor supervise) unwrap`).
- The MCP `supervision_tree` tool and `Node current supervisors` (ADR 0092) already identify supervisors by pid with a name-based bridge only when a name exists, so anonymous instances need no tooling change.
- `[application] supervisor = "X"` and `beamtalk_supervisor:register_root/1` keep working: the app root stays a named singleton.

## Decision

Three additions, no removals.

### 1. Supervisor classes gain the Actor spawn family; `supervise` is unchanged

| Message (class side) | Registration | Returns | Exists today? |
|---|---|---|---|
| `supervise` | `{local, Module}`, idempotent | `Result(Self, Error)` | yes, unchanged |
| `spawn` | none (anonymous) | `Self` | **new** |
| `spawnWith: args` | none | `Self` | **new** |
| `spawnAs: name` | `{local, name}` | `Result(Self, Error)` | **new** |
| `spawnWith: args as: name` | `{local, name}` | `Result(Self, Error)` | **new** |
| `named: name` | lookup | `Self \| Nil` | **new** (mirrors `Actor named:`) |
| `current` | lookup by Module | `Self \| Nil` | yes, unchanged |

The vocabulary and semantics are exactly `Actor`'s (`actor.bt:44-152`): `spawn`/`spawnWith:` raise on start failure like actor spawn does; the `as:` forms return a `Result` because a name can be taken. `supervise` keeps meaning "ensure the application-level instance of this class is running"; `spawn` means "make one". That is OTP's own split between `supervisor:start_link/3` with a name and `start_link/2` without.

```beamtalk
srv := ServerSupervisor spawnWith: config                // anonymous, one per Catalina start:
srv2 := ServerSupervisor spawnWith: otherConfig          // a second one on the same node
tenant := TenantSupervisor spawnWith: cfg as: #tenant_42 // registered; name must be bounded
TenantSupervisor named: #tenant_42                       // => the instance, or nil
ExduraSupervisor supervise                               // still the idempotent singleton
srv stop                                                 // per instance
```

`class initialize: sup` fires once for every started instance, in the caller's process, exactly as it does for `supervise` today. It does not fire when OTP restarts a supervisor that is itself a child (same rule as today's "does not re-fire on restart").

### 2. Start arguments reach configuration through keyword variants of the class-side methods

```beamtalk
typed Supervisor subclass: AcceptorPoolSupervisor
  class strategy -> Symbol => #oneForOne
  class children: config -> List =>
    (1 to: config numAcceptors) collect: [:i |
      AcceptorSupervisor supervisionSpec withId: i withArgs: #{#index => i, #config => config}]

typed DynamicSupervisor subclass: ConnectionSupervisor
  class childClass: config -> Class => config handler
  class maxChildren: config -> Integer => config numConnections      // once BT-3964 lands
```

- `class children: args` and `class childClass: args` are new overridable class-side methods. Their defaults delegate to the 0-arity forms: `class children: _args => self children`, `class childClass: _args => self childClass`. A class overrides one or the other; defining both is a compile error (`supervision_validators.rs`).
- `class strategy:`, `class maxRestarts:`, `class restartWindow:` get the same keyword shape with the same delegating defaults, so a whole topology can depend on args. They are rarely needed and cost nothing.
- `supervise` and `spawn` call the keyword forms with `nil`; `spawnWith: args` and a parent's `withArgs:` pass the args. The generated `init(Args)` stops ignoring its argument and passes it to `static_init/3` / `dynamic_init/3`, which call `class children: Args` etc. by the existing direct-module-call path.
- `class initialize: sup` is unchanged in arity; an instance that needs its args afterwards reads them with `sup args` (new instance accessor, the term passed at start, `nil` for `supervise`/`spawn`).

Purity rules are unchanged: `children:` must be a pure function of its argument and class-side constants, like `children` today (ADR 0130: class-variable writes raise `class_state_read_only`).

### 3. Supervisor classes answer `supervisionSpec`, and child ids may be integers

Today `supervisionSpec` lives on `Actor` and a nested supervisor child is built by hand with `start_link/0` and `id => Class`. Now:

- `MySup supervisionSpec` returns a `SupervisionSpec` with `actorClass` = the supervisor class and `type => supervisor`, `shutdown => infinity`, `restart => permanent` (OTP's defaults for supervisor children). `withId:`, `withArgs:`, `withRestart:` and (BT-3965) `withShutdown:` apply as for actors; `withName:` registers the child under that name.
- `SupervisionSpec withId:` accepts `Symbol | Integer`. OTP child ids are any term; integers are what a generated pool wants and they do not touch the atom table. `which:`/`terminate:` by class keep working for singleton-of-class children; BT-3966 adds lookup by id.
- Codegen emits `start_link/1` (anonymous, args) beside the existing `start_link/0` (named, `[]`), and `start_link/2` (name, args) for `spawnAs:`/`withName:`. `build_child_spec/2` emits `{Module, start_link, [Args]}` for supervisor children with the spec's id, instead of the hard-wired `{Module, start_link, []}` / `id => Class`.

### REPL session

```beamtalk
pool := AcceptorPoolSupervisor spawnWith: #{#numAcceptors => 3}
pool count                                   // => 3
pool children                                // => #(1, 2, 3)
pool2 := AcceptorPoolSupervisor spawnWith: #{#numAcceptors => 1}
AcceptorPoolSupervisor current               // => nil   (anonymous instances are not "current")
pool stop. pool2 stop
```

### Error examples

```beamtalk
TenantSupervisor spawnWith: cfg as: #tenant_42
TenantSupervisor spawnWith: cfg as: #tenant_42
// => Result error: #already_registered  (same kind actors return from spawnAs:)

Supervisor subclass: Both
  class children => #(A)
  class children: args => #(A, B)
// ⛔ Error: `Both` defines both `children` and `children:`; override one — `children:`
//    receives the start arguments, `children` is the no-argument default.

pool := AcceptorPoolSupervisor spawnWith: #{}        // children: reads config numAcceptors
// => raises does_not_understand from inside init: supervisor_start_failed with the
//    wrapped error as reason — same as a failing `class children` today.
```

### Single source of truth

- Child-spec construction stays in `SupervisionSpec childSpec` (Beamtalk) → `spec_to_otp/2` (Erlang). The supervisor branch of `build_child_spec/2` is replaced by a `SupervisionSpec` with `type => supervisor`, so there is one builder, not two.
- The spawn family is implemented by the same runtime entry points actors use (`gen_server`/`supervisor` `start_link` with or without `{local, Name}`, `named:` via `whereis` plus class check); no second registry.
- Keyword/0-arity delegation lives in `Supervisor` / `DynamicSupervisor` stdlib defaults, not in codegen; codegen only changes which selector `init/1` calls.

## Prior Art

| Platform | Instances | Args |
|---|---|---|
| **Erlang** | `supervisor:start_link(Mod, Args)` anonymous; `start_link(Name, Mod, Args)` named | `Args` → `Mod:init(Args)` returns `{ok, {Flags, ChildSpecs}}`; one module serves any number of trees |
| **Elixir** | `use Supervisor`; `start_link(arg)` with optional `name:`; placed in a parent as `{MySup, arg}` | `init(arg)` builds children from `arg`; `DynamicSupervisor` has `max_children` and heterogeneous `start_child/2` |
| **Gleam `gleam_otp`** | `static_supervisor.new(strategy) |> add(...) |> start_link()`; the builder is a value, call it twice for two instances | closures capture the args; names optional |
| **Akka** | supervision is a property of every actor; no supervisor objects | n/a |

Adopted: Erlang's named/anonymous split and `init(Args)`, Elixir's "a supervisor is placed in its parent as `{Module, arg}`" (our `supervisionSpec withArgs:`). Adapted: Beamtalk keeps per-setting methods (ADR 0059) rather than one `init` returning a spec, adding an argument to them instead. Rejected: Gleam's builder value — Beamtalk supervisors are classes, and a builder would be a second way to say what `class children:` already says.

## User Impact

- **Newcomer.** `spawn` on a supervisor does what `spawn` on an actor does. `children: config` reads like `children` with a parameter. Error on defining both forms names the fix.
- **Smalltalk developer.** Class-side factory messages, same protocol family as `Actor`; nothing declarative added to the class body.
- **Erlang/BEAM developer.** Maps 1:1 onto `start_link/2` vs `/3` and `init(Args)`. Supervisors as children with args are the `{Mod, start_link, [Args]}` they already write.
- **Operator.** Anonymous instances show up in `Node current supervisors` by pid and class, as they do for anonymous actors. Nothing about `[application]` roots or `current` changes. Hot reload: as ADR 0059 noted, a reloaded supervisor class does not change live child sets; with N instances there are N to restart. Documented, not solved here.
- **Tooling developer.** `supervisionSpec` on supervisor classes and `withId: Integer` are the only surface changes visible to the LSP; the MCP `supervision_tree` tool is unchanged.

## Steelman Analysis

### B: `supervise` always starts a fresh instance; `superviseAs:` registers
- 🧑‍💻 **Newcomer:** "One verb. I never have to ask whether `supervise` gave me an old process."
- ⚙️ **BEAM veteran:** "`start_link` never hands back an existing pid; keeping that behaviour in `supervise` keeps a surprise alive."
- 🎨 **Language designer:** "Fewer concepts than a parallel spawn family."

### C: class-side `class registration => #singleton | #none`
- 🧑‍💻 **Newcomer:** "No new messages to learn."
- 🎩 **Smalltalk purist:** "A declarative class property, like `strategy`."

### D: single `class init: args` returning a spec (Elixir style)
- ⚙️ **BEAM veteran:** "Exactly `init/1`; one place to read the whole topology."
- 🎨 **Language designer:** "One override point instead of five keyword variants."

### Tension points
- The BEAM veteran's case for B is the strongest argument against the decision: `supervise` keeps returning an existing instance while `spawn` makes new ones. The counter is that `supervise` has a precise meaning ("ensure the application-level one is running", ADR 0080) that three packages and the `[application]` boot path depend on, and OTP itself has the same two behaviours under two call shapes.
- Operators and newcomers side with the decision (no migration, familiar words). Language designers split between the decision and D.

## Alternatives Considered

### B: `supervise` starts fresh; `superviseAs:` registers
Rejected: breaks `ExduraSupervisor supervise valueOrDo:` and every `current` call after a `supervise`, and reverses ADR 0080's idempotent-startup convention a few months after it shipped. Everything B offers is available as `spawn`.

### C: class-side registration switch
Rejected: a class could not be both an app root (named) and a per-test instance (anonymous), which Catalina's test suite needs; and the meaning of `supervise` would depend on a declaration elsewhere.

### D: `class init: args` returning a spec
Rejected: ADR 0059 chose per-setting methods and documented why; a second configuration style would leave two ways to say the same thing. The keyword variants add an argument without adding a style.

### Keep singletons; have Catalina generate classes
Rejected: `numAcceptors` is a runtime option and the handler class belongs to the user; code generation at start time is not a thing Beamtalk does.

## Consequences

### Positive
- Libraries can build OTP-shaped trees from runtime configuration; Catalina Phase 2 is unblocked, exdura's per-pool supervisor becomes expressible, supervisor tests no longer need to be serial.
- No change for existing code; the spawn vocabulary is already known from `Actor`.
- Delivers ADR 0080 Future Work #4 with the `Result(Self, Error)` shape it asked for.

### Negative
- Two ways to start a supervisor (`supervise` vs `spawn`), with different idempotence. The docs must state the rule in one sentence: *`supervise` ensures, `spawn` creates.*
- Five keyword variants (`children:`, `childClass:`, `strategy:`, `maxRestarts:`, `restartWindow:`) where most classes will override one.
- Anonymous instances are only reachable through the object you hold (or the process list); losing the reference means `Node current supervisors` to find it. Same as anonymous actors.

### Neutral
- `current` keeps its narrow meaning (the class-named singleton). `named:` is the lookup for explicitly named instances.
- `SupervisionSpec withId:` widens to `Symbol | Integer`; existing symbol ids are unaffected.

## Implementation

1. **Stdlib** (`supervisor.bt`, `dynamic_supervisor.bt`): add `spawn`, `spawnWith:`, `spawnAs:`, `spawnWith:as:`, `named:` (class side), `args` (instance side); keyword config variants with delegating defaults; `supervisionSpec` on supervisor classes (`SupervisionSpec` gains `type`). Size S.
2. **Runtime** (`beamtalk_supervisor.erl`): `startLink/1` grows `startLink/2` (args) and `startLink/3` (name, args) using `supervisor:start_link/2|3`; `static_init/3` / `dynamic_init/3` take `Args` and call the keyword selectors; `build_child_spec/2` routes supervisor children through `spec_to_otp/2`; `run_initialize/1` and the init gate unchanged (pid-keyed); the `beamtalk_supervisor_new` post-dispatch hook in `beamtalk_class_dispatch.erl` fires for the spawn selectors too. Size M.
3. **Codegen** (`supervisor_codegen.rs`): emit `start_link/1` and `start_link/2`; `init(Args)` passes `Args`. Size S.
4. **Validators** (`supervision_validators.rs`): error on defining both `children` and `children:` (same for the other four pairs); accept `withId:` with integer literals. Size S.
5. **Tests**: BUnit — two anonymous instances of one class with independent children and stop; named instance + `named:`; `spawnAs:` collision; pool of N parameterised supervisor children with integer ids; `childClass:` chosen from args; `initialize:` fires per instance; `supervise` idempotence unchanged. REPL e2e btscript for the REPL session above. Size M.
6. **Docs**: language-features § Supervision Trees (new "Instances and arguments" subsection; replace "define two classes"); ADR 0059 status note; ADR 0080 Future Work #4 marked delivered.

Affected components: stdlib, runtime, codegen, semantic analysis, tests, docs. No parser change. Tracked by BT-3962 (1, 2 partly, 3, 5, 6) and BT-3963 (2 remainder, 4); `/plan-adr` will re-cut them.

## Migration Path

None required. Existing `supervise` / `current` / `[application]` behaviour is byte-for-byte unchanged, and the `withArgs:` meaning for actor children is unchanged. Packages that want per-instance supervisors (exdura's `ActivityWorkerSupervisor`) opt in by calling `spawnWith:` where they called `supervise`.

## References
- Related issues: BT-3962 (multi-instance supervisors), BT-3963 (start-time configuration), BT-3964 (`maxChildren`), BT-3965 (`withShutdown:`), BT-3966 (child control by id); epic BT-3957
- Related ADRs: ADR 0059 (supervision tree syntax, extended), ADR 0080 (lifecycle `Result`, Future Work #4), ADR 0079 (atom namespaces), ADR 0092 (process navigation), ADR 0130 (class-variable purity in `children`/`initialize:`)
- Documentation: `docs/beamtalk-language-features.md` § Supervision Trees; `docs/beamtalk-principles.md` §10; `beamtalk-catalina/PLAN.md`
- Code: `stdlib/src/supervisor.bt`, `stdlib/src/dynamic_supervisor.bt`, `stdlib/src/supervision_spec.bt`, `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`, `crates/beamtalk-codegen/src/core_erlang/supervisor_codegen.rs`, `crates/beamtalk-core/src/semantic_analysis/validators/supervision_validators.rs`

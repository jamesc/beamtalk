# ADR 0135: Supervisor Instances and Start Arguments

## Status
Proposed (2026-10-20). Extends ADR 0059 and delivers ADR 0080 § Future Work #4 ("anonymous supervisor start"). Specifies BT-3962 and BT-3963.

## Context

### What a supervisor is today

ADR 0059 made supervisors classes whose configuration is a set of class-side methods: `class strategy`, `class children` (static) or `class childClass` (dynamic), `class maxRestarts`, `class restartWindow`, later `class initialize: sup` (BT-1285), `class maxChildren` (BT-3964) and `class childShutdown` (BT-3965). Starting one is `MySup supervise`, which the runtime turns into

```erlang
supervisor:start_link({local, 'bt@MySup'}, 'bt@MySup', [])   % supervisor_codegen.rs generate_sup_start_link
```

so every supervisor class is **a singleton per node, registered under its module name**, `supervise` is idempotent (ADR 0080's "idempotent-startup convention": `Ok(sup)` whether this call started it or it was already running), and `MySup current` is `whereis(Module)` (`beamtalk_supervisor:current/1`). The generated `init/1` ignores its argument and reads the class-side methods by direct module call (`static_init/2`, `dynamic_init/2`). `supervise` runs in the class gen_server, so a top-level supervisor is linked to the class process.

ADR 0059 was explicit about the consequence: "only one instance of `WebApp` can run at a time … **If two instances of the same topology are needed, define two classes.**" It also said that an ad-hoc `DynamicSupervisor new supervise` would use anonymous registration; that was never implemented. ADR 0080 listed the gap as Future Work #4: "Anonymous supervisor start — `supervisor:start_link/2` without `{local, Name}`. Enables per-tenant / per-request supervision trees where the same `Supervisor` subclass is instantiated multiple times. Requires a companion ADR on how auto-registration-by-class-name interacts with explicit anonymous starts."

This is that ADR.

### Why "define two classes" stops working

The Thousand Island port (Catalina, `beamtalk-catalina/PLAN.md`) needs this tree **per running server**, with `numAcceptors` (default 100) copies of the middle layer:

```
ServerSupervisor (restForOne)                  args: ServerConfig
├── Listener
├── AcceptorPoolSupervisor (oneForOne)         children: 1..numAcceptors
│   └── AcceptorSupervisor[i] (restForOne)     args: {i, server, config}
│       ├── ConnectionSupervisor (dynamic)     childClass: config handler   maxChildren / shutdown: per server
│       └── Acceptor[i]                        holds its parent AcceptorSupervisor's handle
└── ShutdownListener
```

Nothing here can be a class-per-instance: the count comes from an option, the handler class is the user's, `num_connections` and `shutdown_timeout` are per-server options, and a node may run several servers plus a test suite that starts one per test. Thousand Island also passes each supervisor's own pid down to its children (`{server_pid, self(), config}`), which a class-side configuration method has no way to express. Today's runtime hard-wires a *nested* supervisor child to `{Module, start_link, []}` with `id => Class` (`build_child_spec/2` and the `is_supervisor` branch of `spec_to_otp/2`), so a parent cannot even hold two children of one supervisor class, and a `DynamicSupervisor` whose child class is a supervisor can start only one child.

The same shape already exists in first-party code: `beamtalk-exdura`'s `ActivityWorkerSupervisor` is a singleton shared by every `ActivityWorkerPool` ("`maxRestarts => 1000` since all pools share this singleton supervisor") where one supervisor per pool is what the design wants; `beamtalk-watcher/src/watcher.bt` avoids `Supervisor` because "two `Watcher`s ended up silently sharing one Dashboard"; and every supervisor test in `stdlib/test` is `serial` because "supervise/stop registers a single name".

### Constraints

- **`class initialize: sup` runs in the caller's process.** ADR 0080 Option 3 (a helper process) was rejected because it deadlocks against the class gen_server. Any new start path keeps the hook in the caller, and the BT-3759 init gate (keyed by pid) must keep working.
- **Configuration is read in the supervisor's `init/1` by direct module call**, not through the class gen_server (deadlock, see the `static_init/2` doc). Args must reach that path.
- **`spawn` / `spawnWith:` are compiler intrinsics for actors.** `dispatch_codegen.rs` `try_handle_spawn_await` lowers `ClassRef spawn` / `ClassRef spawnWith: x` straight to actor spawn; `self spawn` / `self spawnWith:` in a class method lower to `beamtalk_class_instantiation:class_self_spawn`; and `type_checker/validation.rs` checks `spawnWith:` map keys against the class's `state:` fields. None of these look at whether the receiver is a supervisor class.
- **Instance-side `children` means live children.** BT-3966 makes `Supervisor>>children` return the running child objects, like `DynamicSupervisor>>children`. Class-side `class children` is the static spec list. A start-time hook must not be a third `children`.
- **Atoms are not collected.** Per-instance *names* must come from a bounded, statically-known namespace (ADR 0079, language-features § `spawnAs:`); per-instance *ids* in a parent must not mint atoms, and neither may per-server names used only to pass a handle down.
- **Non-breaking for the three packages** that use supervisors today: exdura (`ExduraSupervisor supervise valueOrDo:` relies on idempotence; `[application] supervisor =`), symphony (`class initialize: sup`, computed `children` that read files), watcher (`(MonitorSupervisor supervise) unwrap`).
- **Tooling finds standalone supervisors by registered name.** `beamtalk_process_navigation:default_root_pids/0` roots the default walk at the app root, the workspace supervisors and every *registered* Beamtalk supervisor. An anonymous top-level instance is in none of these, so `Node current supervisors` (ADR 0092) would not see it without a change.
- `[application] supervisor = "X"` and `beamtalk_supervisor:register_root/1` keep working: the app root stays a named singleton.

## Decision

Three additions and one narrow behaviour change (§3, bare nested supervisor classes become anonymous children).

### 1. Supervisor classes gain the Actor spawn family; `supervise` is unchanged

| Message (class side) | Registration | Returns | Exists today? |
|---|---|---|---|
| `supervise` | `{local, Module}`, idempotent, linked to the class process | `Result(Self, Error)` | yes, unchanged |
| `spawn` | none (anonymous), unlinked | `Self` | **new** |
| `spawnWith: args` | none, unlinked | `Self` | **new** |
| `spawnAs: name` | `{local, name}`, unlinked | `Result(Self, Error)` | **new** |
| `spawnWith: args as: name` | `{local, name}`, unlinked | `Result(Self, Error)` | **new** |
| `named: name` | lookup + class check | `Result(Self, Error)` | **new** (mirrors `Actor named:`) |
| `current` | lookup by Module | `Self \| Nil` | yes, unchanged |

The vocabulary and semantics are exactly `Actor`'s (`actor.bt`, `spawn` … `named:`): `spawn`/`spawnWith:` raise on start failure (kind `#supervisor_start_failed`) like actor spawn does; the `as:` forms return a `Result` because a name can be taken, and a taken name gives `#name_registered`, the kind actors return. `named:` returns `Result(Self, Error)` with `#not_found` for an unknown name and `#type_error` for a process that is not a supervisor of this class or a subclass, as `Actor named:` does. `supervise` keeps meaning "ensure the application-level instance of this class is running"; `spawn` means "make one". That is OTP's own split between `supervisor:start_link/3` with a name and `start_link/2` without.

```beamtalk
srv := ServerSupervisor spawnWith: config                          // anonymous, one per Catalina start:
srv2 := ServerSupervisor spawnWith: otherConfig                    // a second one on the same node
tenant := (TenantSupervisor spawnWith: cfg as: #tenant_42) unwrap  // registered; name must be bounded
(TenantSupervisor named: #tenant_42) unwrap == tenant              // => true
ExduraSupervisor supervise                                         // still the idempotent singleton
srv stop                                                           // per instance; srv2 keeps running
```

- **Compiler carve-out.** When the receiver class is a `Supervisor` or `DynamicSupervisor` subclass (`class_hierarchy.is_supervisor_subclass || is_dynamic_supervisor_subclass`, as `patterns/type_tests.rs` already checks), `try_handle_spawn_await` and the `class_self_spawn` arm fall through to ordinary class-method dispatch, so `ClassRef spawn`, `ClassRef spawnWith:`, `self spawn` and `self spawnWith:` reach the stdlib methods. The type checker skips the `spawnWith:` key check against `state:` fields for supervisor receivers: supervisors have no `state:` and their args are free-form. Constructor inference (`spawn` → instance of the class) is unchanged and already matches `-> Self`.
- **Not linked.** A top-level spawned supervisor is unlinked from the caller and from the class process, like `Actor spawn` (`beamtalk_actor:safe_spawn/2`). To tie its lifetime to something, nest it via `supervisionSpec` or `stop` it. (`supervise` keeps its existing link to the class process; changing that is out of scope.)
- **Discoverable.** Every anonymous top-level instance joins the `pg` group `beamtalk_supervisor_instances` (default `pg` scope, already started in `beamtalk_bootstrap`). `default_root_pids/0` adds `pg:get_members(beamtalk_supervisor_instances)`; the existing pid dedup covers instances that are also nested. `pg` drops a member when its process exits, so `Node current supervisors` lists a spawned instance while it runs and not after `stop`. Named instances are found by the existing registered-name scan.
- **`class initialize: sup`** fires once for every top-level start (`supervise` and each `spawn*`), in the caller's process, through the existing `beamtalk_supervisor_new` post-dispatch hook. It does not fire for a nested instance or when OTP restarts one (same rule as today). `await_initialized` stays `supervise`-only.

### 2. Start arguments reach configuration through `…For:` hooks

Every class-side configuration method gains a keyword variant that receives the start args. Each default delegates to its no-argument counterpart, so the static form stays the simple case, and the runtime always calls the `For:` form:

```beamtalk
// Supervisor (stdlib/src/supervisor.bt)
class childSpecsFor: args :: Object -> List => self children
class childSpecsFor: args :: Object supervisor: sup :: Self -> List => self childSpecsFor: args   // what the runtime calls
class strategyFor: args :: Object -> SupervisionStrategy => self strategy
class maxRestartsFor: args :: Object -> Integer => self maxRestarts
class restartWindowFor: args :: Object -> Integer => self restartWindow

// DynamicSupervisor (stdlib/src/dynamic_supervisor.bt)
class childClassFor: args :: Object -> Class => self childClass
class maxChildrenFor: args :: Object -> Integer | Symbol => self maxChildren
class childShutdownFor: args :: Object -> ShutdownTimeout | Nil => self childShutdown
class maxRestartsFor: args :: Object -> Integer => self maxRestarts
class restartWindowFor: args :: Object -> Integer => self restartWindow
```

The Catalina tree, in Beamtalk:

```beamtalk
Supervisor subclass: AcceptorPoolSupervisor
  class childSpecsFor: args =>
    ((1 to: (args at: #config) numAcceptors) collect: [:i |
      AcceptorSupervisor supervisionSpec withId: i
        withArgs: #{#id => i, #server => (args at: #server), #config => (args at: #config)}]) asList

Supervisor subclass: AcceptorSupervisor
  class strategy => #restForOne
  class childSpecsFor: args supervisor: me =>
    #((ConnectionSupervisor supervisionSpec withId: #connections withArgs: (args at: #config)),
      (Acceptor supervisionSpec withId: #acceptor withRestart: #permanent
         withArgs: #{#id => (args at: #id), #parent => me, #config => (args at: #config)}))

DynamicSupervisor(Handler) subclass: ConnectionSupervisor
  class childClassFor: config => config handler
  class maxChildrenFor: config => config numConnections
  class childShutdownFor: config => config shutdownTimeout
```

- **Names.** The hooks are `childSpecsFor:`, `childClassFor:`, `strategyFor:`, `maxRestartsFor:`, `restartWindowFor:`, `maxChildrenFor:`, `childShutdownFor:`. With BT-3966, instance-side `children` returns live children and class-side `class children` is the static list; a `class children:` hook would be a third near-identical name with a third meaning. `childSpecsFor:` says what it returns, and the uniform `For:` suffix reads as "the X for these start args" while keeping each hook visibly distinct from its unchanged no-arg default.
- **Args path.** `X spawnWith: a`, `spawnWith: a as: n` and a parent's `X supervisionSpec withArgs: a` → OTP `init(a)` → every hook receives `a`. `supervise`, `spawn`, a bare nested class and a spec without `withArgs:` give `nil`. The generated `init(Args)` stops ignoring its argument and passes it to `static_init/3` / `dynamic_init/3`, which call the hooks by the existing direct-module-call path (`call_class_method_direct`, so ADR 0084 runtime overrides still win). **OTP restart replays the same args**, so a restarted instance recomputes the same children, child class and limits. `DynamicSupervisor startChild: a` with a supervisor child class passes `a` to that child's hooks.
- **Parent handle.** The runtime calls `childSpecsFor:supervisor:` with the args and the handle of the supervisor being initialised (`{beamtalk_supervisor, ClassName, Module, self()}`). Beamtalk has no `self()` for a class method running in `init/1`, and per-server registered names would mint atoms per test, so this is the only way to give a child its parent. **Children may store the handle but must not call it while the parent is still starting:** the supervisor is still in `init/1`, so a call on it from the hook or from a child's own `initialize` blocks until `init/1` returns, which deadlocks. Children use it later, e.g. from their first `handleInfo:`. The rule goes in the hook doc and in the language-features section.
- **Per-instance `childClass`, `maxChildren`, `childShutdown`.** `dynamic_init/3` resolves `childClassFor:`, `maxChildrenFor:` and `childShutdownFor:` with the args and stores the child class (`{ChildClass, ChildModule}`, key `'$beamtalk_child_class'`) in the supervisor's process dictionary next to `?MAX_CHILDREN_KEY`. `startChild/1,2,3` read the child class from the **instance** through one helper (`pool_child_class/1`, also used by BT-3966's `DynamicSupervisor children`), falling back to `SupMod:'childClass'()` for a remote or pre-upgrade supervisor. A pool still has exactly one `simple_one_for_one` child spec; `childShutdownFor:` feeds `apply_child_shutdown/2` as `childShutdown` does today. This replaces BT-3972 (per-`startChild:` shutdown, cancelled): OTP's `simple_one_for_one` holds one spec, and Catalina's need is per server, not per child.
- **Validation before start.** `precheck_dynamic_config` grows an args parameter (`/3`) and runs the same resolver as `dynamic_init/3` (`read_dynamic_config/4`, `class_config_value/3`). `supervise` calls it with `nil` (still skipped when the singleton is running); every `spawn*` entry always calls it with the args. A bad `maxChildrenFor:` / `childShutdownFor:` value fails a top-level start with `#supervisor_start_failed` and the validator's hint, and fails the parent's start for a nested instance (`validate_max_children/2` / `validate_child_shutdown/2` in `dynamic_init/3`, as today).
- **`class initialize: sup`** is unchanged in arity; an instance that needs its args reads them with `sup args` (new instance accessor: the term passed at start, `nil` for `supervise`/`spawn`). `static_init/3` / `dynamic_init/3` store the args in the supervisor's process dictionary (`'$beamtalk_sup_args'`), re-stored on every restart; `args` reads it via `erlang:process_info/2` (pattern of `max_children/1`) and returns `#stale_handle` for a dead process.
- **Purity** is unchanged: the hooks run inside the new supervisor process during `init/1` under `beamtalk_class_vars:with_snapshot/2`, as `children` does today. Class-variable writes raise `class_state_read_only` (ADR 0130), and a raise fails the start. Args should be plain values: they are stored in the parent's child spec and replayed on restart.
- **Validators** (`supervision_validators.rs`). A new `check_supervisor_config_overrides` makes it a **compile error** for one class to define more than one form of the same setting: {`children`, `childSpecsFor:`, `childSpecsFor:supervisor:`}, {`strategy`, `strategyFor:`}, {`maxRestarts`, `maxRestartsFor:`}, {`restartWindow`, `restartWindowFor:`} on `Supervisor` subclasses; {`childClass`, `childClassFor:`}, {`maxChildren`, `maxChildrenFor:`}, {`childShutdown`, `childShutdownFor:`}, {`maxRestarts`, `maxRestartsFor:`}, {`restartWindow`, `restartWindowFor:`} on `DynamicSupervisor` subclasses. Overriding different forms at different levels of the hierarchy is allowed. The existing restart-policy warning (`check_children_supervision_policy`) also scans `childSpecsFor:` and `childSpecsFor:supervisor:` bodies.

### 3. Supervisor classes answer `supervisionSpec`, and child ids may be integers

Today `supervisionSpec` lives on `Actor` and a nested supervisor child is built by hand with `start_link/0` and `id => Class`. Now:

- `MySup supervisionSpec` returns a `SupervisionSpec` with `actorClass` = the supervisor class, `type => supervisor`, `shutdown => infinity`, restart from `class supervisionPolicy` (default `#permanent`, OTP's default for supervisor children). `withId:`, `withArgs:`, `withRestart:`, `withShutdown:` (BT-3965) apply as for actors; `withName:` registers the child under that name. `scope: #global` and `withClassMethod:` fail the parent's start with a structured `type_error` naming the supported fields: a class-method factory would run the start in the class process and link the child to it instead of the parent.
- `SupervisionSpec withId:` accepts `Symbol | Integer`. OTP child ids are any term; integers are what a generated pool wants and they do not touch the atom table. Lookup and control by id are BT-3966.
- Codegen emits `start_link/1` (anonymous, `supervisor:start_link(M, Args)`) and `start_link/2` (`supervisor:start_link({local, Name}, M, Args)`) beside the existing `start_link/0`, which now passes `nil` as the init arg. `init/1` forwards `Args` to `static_init/3` / `dynamic_init/3`; `static_init/2` / `dynamic_init/2` stay as `nil`-args wrappers so previously compiled modules still start (ADR 0125).
- In `spec_to_otp/2`'s supervisor branch, a static child maps its start function to `{M, start_link, [nil]}` (`spawn`), `{M, start_link, [Args]}` (`spawnWith:`) or `{M, start_link, [Name, Args]}` (`withName:`).
- **DynamicSupervisor of supervisors needs an indirection template.** `simple_one_for_one` appends the `startChild:` arguments to the template MFA, so a plain `{M, start_link, []}` template would make a no-arg `startChild` call the *registered* `start_link/0` (the second child fails `already_started`), and `startChild:name:` would land on `start_link/2` with its arguments in the wrong order. The template is `{beamtalk_supervisor, start_dynamic_supervisor_child, [M, DefaultArgs]}` with arities 2/3/4 for `startChild` / `startChild:` / `startChild:name:`, mirroring the existing `start_dynamic_child/2,3,4` for actor children. Each `startChild` then starts a new anonymous supervisor child, and `startChild:name:` registers it.

**Behaviour change: a bare supervisor class in `class children` becomes an anonymous child.** `build_child_spec/2`'s hand-built supervisor branch is deleted; a bare class goes through `Class supervisionSpec` → `childSpec` → `spec_to_otp/2` like any other child (see "Single source of truth"). So a nested `InnerSup` is no longer registered under its module name: `InnerSup current` no longer finds it, and `InnerSup supervise` no longer returns the nested instance (it starts a separate singleton). Use `parent which: InnerSup` or BT-3966's by-id lookup instead. Nothing in this repo or in the sibling `beamtalk-*` repos relies on the old behaviour (grepped), and the change removes a footgun: two parents that each nest the same bare supervisor class used to fail with `already_started`.

### REPL session

```beamtalk
pool := AcceptorPoolSupervisor spawnWith: #{#config => cfg3, #server => nil}   // cfg3 numAcceptors = 3
pool count                                   // => 3
pool whichChildren collect: [:c | c id]      // => #(1, 2, 3)   (BT-3966)
pool2 := AcceptorPoolSupervisor spawnWith: #{#config => cfg1, #server => nil}
pool == pool2                                // => false
AcceptorPoolSupervisor current               // => nil   (spawned instances are not "current")
pool args at: #config                        // => cfg3
pool stop. pool2 stop
```

### Error examples

```beamtalk
TenantSupervisor spawnWith: cfg as: #tenant_42
TenantSupervisor spawnWith: cfg as: #tenant_42
// => Result error: #name_registered  (the kind actors return from spawnAs:)

TenantSupervisor named: #tenant_99
// => Result error: #not_found

DynamicSupervisor subclass: Pool
  class childClass => Worker
  class childClassFor: args => args handler
// ⛔ Error: `Pool` defines both `childClass` and `childClassFor:`; override one —
//    `childClassFor:` receives the start arguments, `childClass` is the no-argument default.

pool := AcceptorPoolSupervisor spawnWith: #{}        // childSpecsFor: reads (args at: #config)
// => raises with kind #supervisor_start_failed, wrapping the error from inside init —
//    same as a failing `class children` today.

ConnectionSupervisor spawnWith: (cfg withNumConnections: -1)
// => raises #supervisor_start_failed with the maxChildren validator's hint (precheck, before start)
```

### Single source of truth

- Child-spec construction stays in `SupervisionSpec childSpec` (Beamtalk) → `spec_to_otp/2` (Erlang). The supervisor branch of `build_child_spec/2` is replaced by `Class supervisionSpec`, so there is one builder, not two.
- The spawn family is implemented by one runtime start helper (generalising `start_link_trapping_exits/1` to take the start fun), using `supervisor:start_link` with or without `{local, Name}`; `named:` is `whereis` plus a class check; `{already_started, _}` maps through the actor `spawnAs:` error builder. No second registry.
- `For:`/no-arg delegation lives in `Supervisor` / `DynamicSupervisor` stdlib defaults, not in codegen; the runtime calls only the `For:` selectors. `precheck_dynamic_config/3` and `dynamic_init/3` share one config resolver.

## Prior Art

| Platform | Instances | Args |
|---|---|---|
| **Erlang** | `supervisor:start_link(Mod, Args)` anonymous; `start_link(Name, Mod, Args)` named | `Args` → `Mod:init(Args)` returns `{ok, {Flags, ChildSpecs}}`; one module serves any number of trees; `self()` in `init/1` is the supervisor |
| **Elixir** | `use Supervisor`; `start_link(arg)` with optional `name:`; placed in a parent as `{MySup, arg}` | `init(arg)` builds children from `arg`; `DynamicSupervisor` has `max_children` and heterogeneous `start_child/2` |
| **Gleam `gleam_otp`** | `static_supervisor.new(strategy) |> add(...) |> start_link()`; the builder is a value, call it twice for two instances | closures capture the args; names optional |
| **Akka** | supervision is a property of every actor; no supervisor objects | n/a |

Adopted: Erlang's named/anonymous split and `init(Args)`, Elixir's "a supervisor is placed in its parent as `{Module, arg}`" (our `supervisionSpec withArgs:`), and Thousand Island's use of `self()` in `init/1` (our `childSpecsFor:supervisor:`). Adapted: Beamtalk keeps per-setting methods (ADR 0059) rather than one `init` returning a spec, adding an argument to them instead. Rejected: Gleam's builder value — Beamtalk supervisors are classes, and a builder would be a second way to say what `class childSpecsFor:` already says.

## User Impact

- **Newcomer.** `spawn` on a supervisor does what `spawn` on an actor does, including not linking to the caller. `childSpecsFor: config` reads as "the child specs for this config". The error on defining both forms names the fix.
- **Smalltalk developer.** Class-side factory messages, same protocol family as `Actor`; nothing declarative added to the class body.
- **Erlang/BEAM developer.** Maps 1:1 onto `start_link/2` vs `/3` and `init(Args)`; the parent handle is `self()` in `init/1`, with the usual "don't call yourself from init" rule. Supervisors as children with args are the `{Mod, start_link, [Args]}` they already write.
- **Operator.** Anonymous instances show up in `Node current supervisors` by pid and class through the `beamtalk_supervisor_instances` `pg` group. Nothing about `[application]` roots or top-level `current` changes; a nested bare supervisor is no longer `current`. Hot reload: as ADR 0059 noted, a reloaded supervisor class does not change live child sets; with N instances there are N to restart. Documented, not solved here.
- **Tooling developer.** `default_root_pids/0` gains the `pg` group. `supervisionSpec` on supervisor classes, the `For:` hooks and `withId: Integer` are the surface changes visible to the LSP; the type checker no longer flags `spawnWith:` keys on supervisor classes.

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
- 🎨 **Language designer:** "One override point instead of seven keyword variants."

### E: a new verb (`start` / `startWith:`) instead of `spawn`
- 🎨 **Language designer:** "No compiler carve-out: `spawn` stays an actor intrinsic and supervisors get their own word."
- ⚙️ **BEAM veteran:** "A supervisor is started, not spawned."

### Tension points
- The BEAM veteran's case for B is the strongest argument against the decision: `supervise` keeps returning an existing instance while `spawn` makes new ones. The counter is that `supervise` has a precise meaning ("ensure the application-level one is running", ADR 0080) that three packages and the `[application]` boot path depend on, and OTP itself has the same two behaviours under two call shapes.
- E trades a compiler special case for a new word users must learn and that collides with their own code; the decision pays the carve-out once in the compiler.
- Operators and newcomers side with the decision (no migration, familiar words). Language designers split between the decision and D.

## Alternatives Considered

### B: `supervise` starts fresh; `superviseAs:` registers
Rejected: breaks `ExduraSupervisor supervise valueOrDo:` and every `current` call after a `supervise`, the repl-protocol `supervisor.btscript` / `supervisor_initialize.btscript` cases and `testSuperviseIsIdempotentWithCap`, and reverses ADR 0080's idempotent-startup convention a few months after it shipped. Everything B offers is available as `spawn`.

### C: class-side registration switch
Rejected: a class could not be both an app root (named) and a per-test instance (anonymous), which Catalina's test suite needs; and the meaning of `supervise` would depend on a declaration elsewhere.

### D: `class init: args` returning a spec
Rejected: ADR 0059 chose per-setting methods and documented why; a second configuration style would leave two ways to say the same thing. The `For:` hooks add an argument without adding a style.

### E: `start` / `startWith:` instead of `spawn`
Rejected: it would avoid the intrinsic carve-out, but `start:` is a common user facade name (symphony, watcher, exdura, http, Catalina), and the spawn family already carries the named / unnamed / `named:` semantics users know from `Actor`.

### `children:` / `childClass:` as the hook names
Rejected: with BT-3966's live-children `sup children`, `class children` / `class children:` / `sup children` would be three near-identical names with three meanings; `childSpecsFor:` and the uniform `For:` suffix keep them apart.

### `superviseWith: args`
Rejected: the instance's identity (singleton or not) would depend on whether args were passed.

### Keep singletons; have Catalina generate classes
Rejected: `numAcceptors` is a runtime option and the handler class belongs to the user; code generation at start time is not a thing Beamtalk does.

## Consequences

### Positive
- Libraries can build OTP-shaped trees from runtime configuration, including per-server limits and parent handles; Catalina Phase 2 is unblocked, exdura's per-pool supervisor becomes expressible, supervisor tests no longer need to be serial.
- No change for top-level code; the spawn vocabulary, its unlinked semantics and `named:`'s `Result` are already known from `Actor`.
- Delivers ADR 0080 Future Work #4 with the `Result(Self, Error)` shape it asked for.
- A nested bare supervisor class no longer fails `already_started` in a multi-instance parent, and a DynamicSupervisor of supervisors can start more than one child.

### Negative
- Two ways to start a supervisor (`supervise` vs `spawn`), with different idempotence and linking. The docs must state the rule in one sentence: *`supervise` ensures, `spawn` creates.*
- `spawn` / `spawnWith:` are no longer uniformly actor intrinsics: codegen and the type checker each need a supervisor carve-out, and must keep it as the intrinsics evolve.
- Seven `For:` hooks (plus `childSpecsFor:supervisor:`) where most classes will override one.
- The parent handle is a deadlock hazard if a child calls it during the parent's `init/1`. It is a documented rule, not something the compiler can check.
- Behaviour change: a supervisor nested as a bare class is anonymous, so `InnerSup current` returns nil for it and `InnerSup supervise` starts a separate instance.
- Anonymous instances are only reachable through the object you hold or `Node current supervisors`. Same as anonymous actors.

### Neutral
- `current` keeps its narrow meaning (the class-named singleton). `named:` is the lookup for explicitly named instances.
- `SupervisionSpec withId:` widens to `Symbol | Integer`; existing symbol ids are unaffected. `whichChildren/1`'s id type widens with it until BT-3966 replaces it.
- A DynamicSupervisor's child class, `maxChildren` and child shutdown become per instance, read from the instance's process dictionary; the generated `childClass/0` stays as the fallback for old or remote supervisors.

## Implementation

**BT-3962** (spawn family, nesting, discovery):
1. **Stdlib** (`supervisor.bt`, `dynamic_supervisor.bt`, `supervision_spec.bt`): `spawn`, `spawnWith:`, `spawnAs:`, `spawnWith:as:`, `named:`, `supervisionPolicy`, `supervisionSpec` (class side); `args` (instance side); `withId:` accepts `Symbol | Integer`.
2. **Codegen**: `supervisor_codegen.rs` emits `start_link/1`, `start_link/2`, `init(Args)` → `static_init/3` / `dynamic_init/3`. `dispatch_codegen.rs` `try_handle_spawn_await` and the `class_self_spawn` arm fall through for supervisor receivers; `type_checker/validation.rs` skips the `spawnWith:` state-key check for them.
3. **Runtime** (`beamtalk_supervisor.erl`): one start helper for anonymous (unlink, init gate, `pg:join`) and named (`#name_registered`) starts; `named/2`; `precheck_dynamic_config` on every non-idempotent start; `build_child_spec/2` supervisor branch deleted; `spec_to_otp/2` honours the start function for supervisor children; `start_dynamic_supervisor_child/2,3,4`; args stored for `sup args`. `beamtalk_process_navigation:default_root_pids/0` adds the `pg` group.
4. **Tests**: `stdlib/test/supervisor_instances_test.bt` (not serial) and `bt3962_*` fixtures; eunit for `spec_to_otp/2` supervisor mapping, no-link, `pg` membership, `named/2`; codegen tests for the carve-out and `start_link/1,2`; a type-checker test for `spawnWith:` on a supervisor class.

**BT-3963** (start-time configuration):
5. **Stdlib**: the `For:` hooks and `childSpecsFor:supervisor:` with delegating defaults and docs (including the no-call-during-init rule).
6. **Runtime**: `static_init/3` / `dynamic_init/3` call the `For:` selectors; `read_dynamic_config/4`, `class_config_value/3`, `precheck_dynamic_config/3`; `'$beamtalk_child_class'` and `pool_child_class/1` for `startChild/1,2,3`.
7. **Validators** (`supervision_validators.rs`): `check_supervisor_config_overrides` (both-forms error for each pair) and `check_children_supervision_policy` extended to `childSpecsFor:` bodies.
8. **Tests**: `stdlib/test/supervisor_start_args_test.bt` and `bt3963_*` fixtures; validator unit tests; eunit for the args-aware resolver.

**Docs** (both issues): language-features § Supervision Trees gets an "Instances and arguments" subsection (`supervise` ensures, `spawn` creates; unlinked; nesting via `supervisionSpec`; integer ids; the hooks table; args/restart rule; parent-handle rule) and replaces "define two classes"; ADR 0059 status note; ADR 0080 Future Work #4 marked delivered.

Affected components: stdlib, runtime, codegen, type checker, semantic analysis, tests, docs. No parser change.

## Migration Path

None for top-level code: `supervise`, `current`, `[application] supervisor =` and `Workspace startSupervisor:` are unchanged, as are the no-arg configuration methods and the `withArgs:` meaning for actor children. One behaviour change: a supervisor nested as a bare class in `class children` is now an anonymous child, so `InnerSup current` returns nil for it; use `parent which: InnerSup` or BT-3966's by-id lookup. A DynamicSupervisor with a supervisor child class now starts anonymous children (before, the second `startChild` always failed). Packages that want per-instance supervisors (exdura's `ActivityWorkerSupervisor`) opt in by calling `spawnWith:` where they called `supervise`.

## References
- Related issues: BT-3962 (multi-instance supervisors), BT-3963 (start-time configuration), BT-3964 (`maxChildren`), BT-3965 (`withShutdown:` / `childShutdown`), BT-3966 (child control by id; live `children`), BT-3972 (per-child shutdown, cancelled in favour of `childShutdownFor:`); epic BT-3957
- Related ADRs: ADR 0059 (supervision tree syntax, extended), ADR 0079 (atom namespaces, `spawnAs:`), ADR 0080 (lifecycle `Result`, Future Work #4), ADR 0084 (runtime class-method overrides), ADR 0092 (process navigation), ADR 0125 (compiled-module compatibility), ADR 0130 (class-variable purity in configuration hooks)
- Documentation: `docs/beamtalk-language-features.md` § Supervision Trees; `docs/beamtalk-principles.md` §10; `beamtalk-catalina/PLAN.md`
- Code: `stdlib/src/supervisor.bt`, `stdlib/src/dynamic_supervisor.bt`, `stdlib/src/supervision_spec.bt`, `stdlib/src/actor.bt` (`spawn` … `named:`), `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`, `runtime/apps/beamtalk_runtime/src/beamtalk_process_navigation.erl` (`default_root_pids/0`), `crates/beamtalk-codegen/src/core_erlang/supervisor_codegen.rs`, `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs` (`try_handle_spawn_await`), `crates/beamtalk-core/src/semantic_analysis/type_checker/validation.rs`, `crates/beamtalk-core/src/semantic_analysis/validators/supervision_validators.rs`

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

The same shape already exists in first-party code: `beamtalk-exdura`'s `ActivityWorkerSupervisor` is a singleton shared by every `ActivityWorkerPool` ("`maxRestarts => 1000` since all pools share this singleton supervisor") where one supervisor per pool is what the design wants; `beamtalk-watcher/src/watcher.bt` avoids `Supervisor` because "two `Watcher`s ended up silently sharing one Dashboard"; and the supervisor tests in `stdlib/test` run `serial` because each `supervise` starts the one name-registered process for its class (e.g. `dynamic_supervisor_max_children_test.bt`, `dynamic_supervisor_start_child_name_test.bt`).

### Constraints

- **The post-start hook (today `class initialize: sup`) runs in the caller's process.** ADR 0080 Option 3 (a helper process) was rejected because it deadlocks against the class gen_server. Any new start path keeps the hook in the caller, and the BT-3759 init gate (keyed by pid) must keep working.
- **Configuration is read in the supervisor's `init/1`**, not through the class gen_server (deadlock, see the `static_init/2` doc). Args must reach that path.
- **`spawn` / `spawnWith:` are compiler intrinsics for actors.** `dispatch_codegen.rs` `try_handle_spawn_await` lowers `ClassRef spawn` / `ClassRef spawnWith: x` straight to actor spawn; `self spawn` / `self spawnWith:` in a class method lower to `beamtalk_class_instantiation:class_self_spawn`; and `type_checker/validation.rs` checks `spawnWith:` map keys against the class's `state:` fields. None of these look at whether the receiver is a supervisor class.
- **Instance-side `children` means live children.** BT-3966 makes `Supervisor>>children` return the running child objects, like `DynamicSupervisor>>children`. Whatever replaces `class children` as the child-spec list must not also be called `children`.
- **Class-side configuration cannot see start args or its own process.** A class method running in `init/1` has no argument and no Beamtalk `self()`; Thousand Island's tree needs both (`init(arg)` and `self()` in `init/1`).
- **Atoms are not collected.** Per-instance *names* must come from a bounded, statically-known namespace (ADR 0079, language-features § `spawnAs:`); per-instance *ids* in a parent must not mint atoms, and neither may per-server names used only to pass a handle down.
- **Top-level use must not change for the three packages** that use supervisors today (their *configuration methods* may move, mechanically, with a compiler fix-it): exdura (`ExduraSupervisor supervise valueOrDo:` relies on idempotence; `[application] supervisor =`), symphony (`class initialize: sup`, computed `children` that read files), watcher (`(MonitorSupervisor supervise) unwrap`).
- **Tooling finds standalone supervisors by registered name.** `beamtalk_process_navigation:default_root_pids/0` roots the default walk at the app root, the workspace supervisors and every *registered* Beamtalk supervisor. An anonymous top-level instance is in none of these, so `Node current supervisors` (ADR 0092) would not see it without a change.
- `[application] supervisor = "X"` and `beamtalk_supervisor:register_root/1` keep working: the app root stays a named singleton.

## Decision

Three changes, one of them breaking: supervisor classes gain the spawn family (§1); configuration moves from class-side methods to instance-side methods read from the instance being started (§2, breaking but mechanical, with a compiler fix-it); and supervisor classes answer `supervisionSpec` with integer ids allowed (§3). Two narrow behaviour changes ride along: `supervise` no longer links the tree to the class process (§1), and bare nested supervisor classes become anonymous children (§3).

### 1. Supervisor classes gain the Actor spawn family; `supervise` becomes sugar over it

| Message (class side) | Registration | Returns | Exists today? |
|---|---|---|---|
| `supervise` | `{local, Module}`, idempotent, unlinked | `Result(Self, Error)` | yes; now sugar over the spawn start helper, no longer linked to the class process |
| `spawn` | none (anonymous), unlinked | `Self` | **new** |
| `spawnWith: args` | none, unlinked | `Self` | **new** |
| `current` | lookup by Module | `Self \| Nil` | yes, unchanged |

The vocabulary and semantics are `Actor`'s (`actor.bt`): `spawn`/`spawnWith:` raise on start failure, as actor spawn does, with kind `#supervisor_start_failed` (the kind `supervise` already uses, where actor spawn uses `#instantiation_error`). `supervise` keeps meaning "ensure the application-level instance of this class is running"; `spawn` means "make one". That is OTP's own split between `supervisor:start_link/3` with a name and `start_link/2` without.

**Deferred: named instances.** `spawnAs:`, `spawnWith:as:`, `named:` and `withName:` on a supervisor child are not part of this ADR. Catalina, exdura and watcher need only anonymous instances plus the class-named `supervise` instance. When a use appears, they follow `Actor`'s definitions exactly (`#name_registered` for a taken name; `named:` returning `Result(Self, Error)` with `#name_not_registered` / `#wrong_class`), on the same start helper, without breaking anything here.

**`supervise` is not a second mechanism.** It is defined as the spawn start helper, registered under the class's own module name and made idempotent:

```beamtalk
// Supervisor / DynamicSupervisor (class side), in effect:
class supervise -> Result(Self, Error) =>
  // the spawn start helper, registered as {local, <module name>}; if already started, answer the running instance
```

It goes through the same runtime start helper as `spawn`/`spawnWith:` (see "Single source of truth"), keeps the two properties nothing else provides (idempotence and a class-derived name that mints no atom and that `current` finds), and keeps the `initialize` gate (`await_initialized`). OTP has no idempotent start; Beamtalk keeps one because re-evaluating `MySup supervise` in a REPL or workspace, and the `[application]` boot path, must not fail on the second run. So there is one way to start a supervisor plus one convenience, and the docs' rule *`supervise` ensures, `spawn` creates* describes a single concept.

```beamtalk
srv := ServerSupervisor spawnWith: config                          // anonymous, one per Catalina start:
srv2 := ServerSupervisor spawnWith: otherConfig                    // a second one on the same node
ExduraSupervisor supervise                                         // still the idempotent singleton
srv stop                                                           // per instance; srv2 keeps running
```

- **Compiler carve-out.** When the receiver class is a `Supervisor` or `DynamicSupervisor` subclass (`class_hierarchy.is_supervisor_subclass || is_dynamic_supervisor_subclass`, as `patterns/type_tests.rs` already checks), `try_handle_spawn_await` and the `class_self_spawn` arm lower `ClassRef spawn`, `ClassRef spawnWith:`, `self spawn` and `self spawnWith:` to a direct call of the runtime start helper **in the caller's process** (as actor spawn is lowered), not to ordinary class-method dispatch. Ordinary dispatch on a non-sealed supervisor class would run the whole tree start inside the class gen_server, as `supervise` does today: every `spawnWith:`, `supervise` and `current` on that class would queue behind tree starts, and any child that sent to the class during start would deadlock. Dynamic sends (`cls spawnWith: x` with `cls` unknown at compile time) take the same route actor `spawnWith:` does in `beamtalk_class_dispatch:class_send/3`, which hands the start to the runtime helper rather than running it in the class process. The stdlib `spawn`/`spawnWith:` methods exist for documentation, reflection and `respondsTo:`, and delegate to the same helper. `supervise` uses the same helper, so it too stops running the tree start in the class process. The type checker skips the `spawnWith:` key check against `state:` fields for supervisor receivers: supervisors have no `state:` and their args are free-form. Constructor inference (`spawn` → instance of the class) is unchanged and already matches `-> Self`.
- **Not linked.** A top-level supervisor started by `spawn*` **or `supervise`** is unlinked from the caller and from the class process, like `Actor spawn` (`beamtalk_actor:safe_spawn/2`). To tie its lifetime to something, nest it via `supervisionSpec` or `stop` it. For `supervise` this is a behaviour change: today `supervise` runs in the class gen_server, so `proc_lib:start_link` links the tree to the class process. That link was never designed and has already done harm: exdura documents that exhausting `ExduraSupervisor`'s restart intensity "took the whole class down with it" (`beamtalk-exdura/src/http/exdura_http_server.bt`, `startListening` doc). No package relies on the class process dying with its supervisor (grepped exdura, symphony, watcher).
- **Discoverable.** Every anonymous top-level instance joins the `pg` group `beamtalk_supervisor_instances` (default `pg` scope, already started in `beamtalk_bootstrap`). It joins from its own `init/1`, before `start_link` returns, so there is no window in which the instance runs but is not listed. The start helper tells `init/1` that this is a top-level start through the init arg (a wrapper term the generated `init/1` unwraps before reading configuration); nested and dynamically started instances do not join, because the parent walk already reaches them. `default_root_pids/0` adds `pg:get_members(beamtalk_supervisor_instances)`; the existing pid dedup covers instances that are also nested. `pg` drops a member when its process exits, so `Node current supervisors` lists a spawned instance while it runs and not after `stop`. Named instances are found by the existing registered-name scan.
- **`initialize` becomes an instance method** (was `class initialize: sup`). It fires once for every top-level start (`supervise`, `spawn`, `spawnWith:`), in the caller's process, through the existing `beamtalk_supervisor_new` post-dispatch hook, which now sends `initialize` to the new instance instead of `initialize:` to the class. That is where supervisor instance methods already run (`beamtalk_message_dispatch:send/3`), so inside it `self` is the new supervisor and `self args` its start args, and messages to `self` work (the supervisor is past `init/1`). It does not fire for a nested instance or when OTP restarts one (same rule as today). `await_initialized` stays `supervise`-only. The default is a no-op on `Supervisor` / `DynamicSupervisor`. Actors already spell their hook `initialize`; supervisors now match.

### 2. Configuration moves instance-side and is read from the instance being started

Supervisor configuration stops being class-side. Each setting is an **instance method**, and the runtime calls it **on the supervisor being started**, from inside that supervisor's `init/1`. So `self` is the supervisor's own handle and `self args` is its start args. There is one form per setting, with the default on the base class:

```beamtalk
// Supervisor (stdlib/src/supervisor.bt), instance side
childSpecs -> List => self subclassResponsibility        // was: class children
strategy -> SupervisionStrategy => #oneForOne            // was: class strategy
maxRestarts -> Integer => 10                             // was: class maxRestarts
restartWindow -> Integer => 60                           // was: class restartWindow
args -> Object => ...                                    // new: the start args, nil for supervise / spawn

// DynamicSupervisor (stdlib/src/dynamic_supervisor.bt), instance side
childClass -> Class => self subclassResponsibility       // was: class childClass
maxChildren -> Integer | Symbol => #infinity             // was: class maxChildren
childShutdown -> ShutdownTimeout | Nil => nil            // was: class childShutdown
maxRestarts -> Integer => 10
restartWindow -> Integer => 60
```

(The literal defaults are today's class-side defaults, moved unchanged.)

A static supervisor reads exactly as before, without `class`:

```beamtalk
Supervisor subclass: WebApp
  strategy => #oneForAll
  childSpecs => #(DatabasePool HTTPRouter)
```

The Catalina tree:

```beamtalk
Supervisor subclass: AcceptorPoolSupervisor
  childSpecs =>
    ((1 to: self args config numAcceptors) collect: [:i |
      AcceptorSupervisor supervisionSpec withId: i
        withArgs: #{#id => i, #server => self args server, #config => self args config}]) asList

Supervisor subclass: AcceptorSupervisor
  strategy => #restForOne
  childSpecs => #(
    (ConnectionSupervisor supervisionSpec withId: #connections withArgs: (self args at: #config)),
    (Acceptor supervisionSpec withId: #acceptor withRestart: #permanent
       withArgs: #{#id => (self args at: #id), #parent => self, #config => (self args at: #config)}))

DynamicSupervisor(Handler) subclass: ConnectionSupervisor
  childClass => self args handler
  maxChildren => self args numConnections
  childShutdown => self args shutdownTimeout
```

- **Why instance side.** Configuration is a property of a running supervisor, and every input it needs (start args, its own identity) belongs to the instance. Class-side methods had neither, which is why the earlier draft needed a keyword variant of every method (`childSpecsFor: args`), a second variant carrying the handle (`childSpecsFor: args supervisor: sup`), a compile error for classes that defined two forms, and a pre-start check that evaluated the methods a second time. Instance-side methods need none of that: `self args` replaces the argument, `self` replaces the handle, and there is one method per setting.
- **Names.** `childSpecs`, not `children`: instance-side `children` returns the live children (BT-3966). The other settings keep their names.
- **How they are called.** The generated `init(Args)` passes `Args` to `static_init/3` / `dynamic_init/3`. These store the args in the process dictionary (`'$beamtalk_sup_args'`), build the handle `{beamtalk_supervisor, ClassName, Module, self()}`, and send each setting to it through ordinary instance dispatch (`beamtalk_message_dispatch:send/3` → `beamtalk_dispatch:lookup/5`, which already serves supervisor instance methods in the caller's process). User overrides, inherited defaults and ADR 0084 runtime overrides all resolve as for any instance method. This replaces `call_class_method_direct` for configuration; the class gen_server is still not involved.
- **Args path.** `X spawnWith: a` and a parent's `X supervisionSpec withArgs: a` reach OTP `init(a)`; `supervise`, `spawn`, a bare nested class and a spec without `withArgs:` give `nil`. **OTP restart replays the same args**, so a restarted instance recomputes the same children, child class and limits. `DynamicSupervisor startChild: a` with a supervisor child class passes `a` as that child's args. `sup args` also works from outside: it reads `'$beamtalk_sup_args'` via `erlang:process_info/2` (the pattern of `max_children/1`) and returns `#stale_handle` for a dead process.
- **Calling `self` during configuration fails fast.** The configuration methods run inside the supervisor's own `init/1`. Reading `self args` is a process-dictionary read, but any message that reaches the supervisor process (`self count`, `self childWithId:`) is a `gen_server:call` to itself, which OTP rejects at once with `calling_self`. The runtime maps that exit to `#supervisor_start_failed` with the hint "configuration methods run inside the supervisor's init; use `self args` and return specs, do not send messages to `self`". Passing `self` *as a value* (`#parent => self`) is the intended use.
- **Parent handle and the hang rule.** `self`, passed down in a child's args, is how a child gets its parent; nothing else is needed (no hook variant, no ancestor lookup), and replaying it on restart is safe because a child never outlives its parent. **The rule: never synchronously call a supervisor from code that supervisor is waiting on.** A supervisor waits on (a) its own configuration methods and those of every nested supervisor child (they run inside its synchronous `init/1` or the `start_link` of that child; `supervisor` calls use `infinity`, so a call to the parent or any further ancestor hangs for ever; a call to `self` fails fast as above); (b) the `initialize` of a `withName:` or `withClassMethod:` actor child, started through `safe_spawn_named`/`safe_spawn` → `await_initialize` (`sys:get_state(Child, 5000)`), so a call back stalls 5 s and then fails the start. This applies **during the initial start, every OTP restart and every `DynamicSupervisor startChild:`**: the supervisor blocks in the child's start function in all three. Outside those cases a call is safe and merely waits: an unnamed actor child's `initialize` runs in `handle_continue` after `start_link` has returned (`beamtalk_actor.erl` `await_initialize` doc), so a call on the parent from it queues until the parent has started its remaining children. That wait is bounded by the default 5 s `gen_server:call` timeout, which a large tree (100 acceptor subtrees) can exceed; under ADR 0134 a timeout raised in `handleInfo:` crashes the child and, with `#permanent`, loops. Catalina's acceptor therefore makes its first parent call from a `continueWith:` step (BT-3968) with an explicit timeout, or tolerates a retry. The rule goes in the `childSpecs` doc and the language-features section; the compiler cannot check it.
- **Per-instance `childClass`, `maxChildren`, `childShutdown`.** `dynamic_init/3` reads the three settings from the instance and stores the child class (`{ChildClass, ChildModule}`, key `'$beamtalk_child_class'`) next to `?MAX_CHILDREN_KEY`. `startChild/1,2,3` read it through one helper (`pool_child_class/1`, also used by BT-3966's `DynamicSupervisor children`). Where the process dictionary is unavailable (a remote supervisor, or one started before the upgrade), the helper reads the child module from the pool's `simple_one_for_one` template (`supervisor:get_childspec/2`, which works remotely) and maps it to its class, or returns a structured `#beamtalk_error{}`. A pool still has exactly one `simple_one_for_one` child spec; `childShutdown` feeds `apply_child_shutdown/2` as today. This replaces BT-3972 (per-`startChild:` shutdown, cancelled): OTP's `simple_one_for_one` holds one spec, and Catalina's need is per server, not per child.
- **Validation happens once, in `init/1`.** `validate_max_children/2` and `validate_child_shutdown/2` run in `dynamic_init/3` on the values the instance returned. A bad value fails `init/1`, which the start helper already reports to the caller as `#supervisor_start_failed` with the validator's hint (BT-3965's `start_link_trapping_exits`), and fails the parent's start for a nested instance. `precheck_dynamic_config` is deleted: it existed only because class-side values could be read before start, and with args-dependent values it would have to run the methods twice.
- **Purity** is unchanged: configuration runs inside the new supervisor process during `init/1` under `beamtalk_class_vars:with_snapshot/2`, as `children` does today. Class-variable writes raise `class_state_read_only` (ADR 0130), and a raise fails the start. **Arg lifetime:** args are stored in the parent's child spec and replayed on every restart, so any pid or handle in them must outlive the child. An ancestor's handle does. A sibling's pid, a socket or a caller's pid does not; a `DynamicSupervisor startChild: a` whose `a` carries such a resource must use `#temporary` (the default), so OTP never replays it.
- **Only `class supervisionPolicy` stays class-side.** It describes how the class behaves *as a child of another supervisor*, which is a property of the class. `initialize` moves to the instance side (§1), so a supervisor's configuration and lifecycle are all instance methods.
- **Removed class-side configuration is a compile error, not a silent no-op.** A supervisor subclass that defines `class children`, `class childClass`, `class strategy`, `class maxRestarts`, `class restartWindow`, `class maxChildren`, `class childShutdown` or `class initialize:` gets an error with a fix-it: "`class children` is no longer read; define instance-side `childSpecs`", "`class initialize: sup` is now instance-side `initialize`; use `self` for `sup`", and "drop `class`" for the others. The check lives in `supervision_validators.rs` (`check_class_side_supervisor_config`). The existing restart-policy warning (`check_children_supervision_policy`) scans `childSpecs` bodies instead of `class children`.

### 3. Supervisor classes answer `supervisionSpec`, and child ids may be integers

Today `supervisionSpec` lives on `Actor` and a nested supervisor child is built by hand with `start_link/0` and `id => Class`. Now:

- `MySup supervisionSpec` returns a `SupervisionSpec` with `actorClass` = the supervisor class, `type => supervisor`, `shutdown => infinity`, restart from `class supervisionPolicy` (default `#permanent`, OTP's default for supervisor children). `withId:`, `withArgs:`, `withRestart:`, `withShutdown:` (BT-3965) apply as for actors. `withName:` (deferred with the other named forms), `scope: #global` and `withClassMethod:` fail the parent's start with a structured `type_error` naming the supported fields: a class-method factory would run the start in the class process and link the child to it instead of the parent.
- `SupervisionSpec withId:` accepts `Symbol | Integer`. OTP child ids are any term; integers are what a generated pool wants and they do not touch the atom table. Lookup and control by id are BT-3966.
- Codegen emits `start_link/1` (anonymous, `supervisor:start_link(M, Args)`) beside the existing `start_link/0` (a named `start_link/2` arrives with the deferred named forms), which now passes `nil` as the init arg. `init/1` forwards `Args` to `static_init/3` / `dynamic_init/3`; `static_init/2` / `dynamic_init/2` stay as `nil`-args wrappers so previously compiled modules still start (ADR 0125).
- In `spec_to_otp/2`'s supervisor branch, a static child starts through a runtime trampoline, `{beamtalk_supervisor, start_supervisor_child, [M, Args]}`, not `{M, start_link, …}` directly. The trampoline calls `M:start_link/1` when `M` exports it and falls back to `M:start_link/0` for a child module compiled before this ADR (ADR 0125: a parent recompiled first must not fail with `undef` on an old child; the old child then ignores args and registers under its module, as today). It is the same entry the DynamicSupervisor template below uses, so static and dynamic supervisor children have one start path.
- **DynamicSupervisor of supervisors needs an indirection template.** `simple_one_for_one` appends the `startChild:` arguments to the template MFA, so a plain `{M, start_link, []}` template would make a no-arg `startChild` call the *registered* `start_link/0` (the second child fails `already_started`), and `startChild:` would land on the wrong arity. The template is `{beamtalk_supervisor, start_dynamic_supervisor_child, [M, DefaultArgs]}` with arities 2/3 for `startChild` / `startChild:`, mirroring the existing `start_dynamic_child/2,3` for actor children; each starts a new anonymous supervisor child. `startChild:name:` with a supervisor child class returns a structured `#unsupported` error until named instances land.

**Behaviour change: a bare supervisor class in `childSpecs` becomes an anonymous child.** `build_child_spec/2`'s hand-built supervisor branch is deleted; a bare class goes through `Class supervisionSpec` → `childSpec` → `spec_to_otp/2` like any other child (see "Single source of truth"). So a nested `InnerSup` is no longer registered under its module name: `InnerSup current` no longer finds it, and `InnerSup supervise` no longer returns the nested instance (it starts a separate singleton). Use `parent which: InnerSup` or BT-3966's by-id lookup instead. Nothing in this repo or in the sibling `beamtalk-*` repos relies on the old behaviour (grepped), and the change removes a footgun: two parents that each nest the same bare supervisor class used to fail with `already_started`.

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
DynamicSupervisor subclass: Pool
  class childClass => Worker
// ⛔ Error: `class childClass` is no longer read by DynamicSupervisor; define it on the
//    instance side (drop `class`): `childClass => Worker`.

Supervisor subclass: Broken
  childSpecs => self count > 0 ifTrue: [#()] ifFalse: [#(Worker)]
Broken spawn
// => raises #supervisor_start_failed: configuration methods run inside the supervisor's
//    init; use `self args` and return specs, do not send messages to `self` (calling_self).

pool := AcceptorPoolSupervisor spawnWith: #{}        // childSpecs reads self args config
// => raises with kind #supervisor_start_failed, wrapping the error from inside init —
//    same as a failing `class children` today.

ConnectionSupervisor spawnWith: (cfg withNumConnections: -1)
// => raises #supervisor_start_failed with the maxChildren validator's hint (from init/1)
```

### Single source of truth

- Child-spec construction stays in `SupervisionSpec childSpec` (Beamtalk) → `spec_to_otp/2` (Erlang). The supervisor branch of `build_child_spec/2` is replaced by `Class supervisionSpec`, so there is one builder, not two.
- The spawn family **and `supervise`** are implemented by one runtime start helper (generalising `start_link_trapping_exits/1` to take the start fun, and unlinking after start), using `supervisor:start_link` with or without `{local, Module}`; `supervise` is that helper with `{local, Module}` plus "already started → return the running instance". No second registry.
- Configuration defaults live in the `Supervisor` / `DynamicSupervisor` stdlib instance methods, not in codegen or the runtime; the runtime reads every setting by sending it to the instance, once, in `init/1`. There is no second resolver and no pre-start evaluation.

## Prior Art

| Platform | Instances | Args |
|---|---|---|
| **Erlang** | `supervisor:start_link(Mod, Args)` anonymous; `start_link(Name, Mod, Args)` named | `Args` → `Mod:init(Args)` returns `{ok, {Flags, ChildSpecs}}`; one module serves any number of trees; `self()` in `init/1` is the supervisor |
| **Elixir** | `use Supervisor`; `start_link(arg)` with optional `name:`; placed in a parent as `{MySup, arg}` | `init(arg)` builds children from `arg`; `DynamicSupervisor` has `max_children` and heterogeneous `start_child/2` |
| **Gleam `gleam_otp`** | `static_supervisor.new(strategy) |> add(...) |> start_link()`; the builder is a value, call it twice for two instances | closures capture the args; names optional |
| **Akka** | supervision is a property of every actor; no supervisor objects | n/a |
| **Pharo / Squeak** | no supervision; instances come from class-side creation messages (`new`, `on:`, `with:`) | arguments are keyword parts of the creation message |

Adopted: Smalltalk's class-side creation messages (the `spawn` family, with keyword arguments), Erlang's named/anonymous split and `init(Args)`, Elixir's "a supervisor is placed in its parent as `{Module, arg}`" (our `supervisionSpec withArgs:`), and Thousand Island's use of `self()` in `init/1` (our `self` in `childSpecs`). Adapted: Beamtalk keeps per-setting methods (ADR 0059) rather than one `init` returning a spec, and moves them to the instance, where Erlang's `init/1` already lives. Rejected: Gleam's builder value — Beamtalk supervisors are classes, and a builder would be a second way to say what `childSpecs` already says.

## User Impact

- **Newcomer.** `spawn` on a supervisor does what `spawn` on an actor does, including not linking to the caller. Configuration is ordinary instance methods, `self args` is the input, and `self` is this supervisor. Leftover class-side configuration is a compile error that names the fix.
- **Smalltalk developer.** Class-side factory messages, same protocol family as `Actor`; nothing declarative added to the class body.
- **Erlang/BEAM developer.** Maps 1:1 onto `start_link/2` vs `/3` and `init(Args)`; the parent handle is `self()` in `init/1`, with the usual "don't call yourself from init" rule. Supervisors as children with args are the `{Mod, start_link, [Args]}` they already write.
- **Operator.** Anonymous instances show up in `Node current supervisors` by pid and class through the `beamtalk_supervisor_instances` `pg` group. Nothing about `[application]` roots or top-level `current` changes; a nested bare supervisor is no longer `current`. Hot reload: as ADR 0059 noted, a reloaded supervisor class does not change live child sets; with N instances there are N to restart. Documented, not solved here.
- **Tooling developer.** `default_root_pids/0` gains the `pg` group. `supervisionSpec` on supervisor classes, instance-side configuration methods and `withId: Integer` are the surface changes visible to the LSP; the type checker no longer flags `spawnWith:` keys on supervisor classes.

## Steelman Analysis

### B: `supervise` always starts a fresh instance; `superviseAs:` registers
- 🧑‍💻 **Newcomer:** "One verb. I never have to ask whether `supervise` gave me an old process."
- ⚙️ **BEAM veteran:** "`start_link` never hands back an existing pid; keeping that behaviour in `supervise` keeps a surprise alive."
- 🎨 **Language designer:** "You are already breaking supervisor code in this ADR. Break this too and end up with one start verb instead of two."

### C: class-side `class registration => #singleton | #none`
- 🧑‍💻 **Newcomer:** "No new messages to learn."
- 🎩 **Smalltalk purist:** "A declarative class property, like `strategy`."

### D: single `init: args` returning the whole spec (Elixir style)
- ⚙️ **BEAM veteran:** "Exactly `init/1`, and honest about it: you can see you are inside `init`, so nobody expects to message the supervisor from there. Instance-side configuration methods make `self` look like an ordinary supervisor that you cannot actually talk to."
- 🎨 **Language designer:** "One override point instead of seven methods, the whole topology readable in one place, and no special rule about which instance methods run inside `init/1`."

### F: keep class-side configuration and add `…For: args` hooks (this ADR's first draft)
- 🏭 **Operator / existing user:** "Zero migration. Some 150 class-side definitions across stdlib, docs and four packages keep compiling, and every existing tutorial stays right."
- 🎩 **Smalltalk purist:** "Settings that describe a *kind* of supervisor (its strategy, its restart limits) are class knowledge; the instance side is for the behaviour of a running thing."

### E: a new verb (`start` / `startWith:`) instead of `spawn`
- 🎨 **Language designer:** "No compiler carve-out: `spawn` stays an actor intrinsic and supervisors get their own word."
- ⚙️ **BEAM veteran:** "A supervisor is started, not spawned."

### G: an ancestor accessor (`self supervisor`, from `'$ancestors'`) instead of passing `self` down
- ⚙️ **BEAM veteran:** "`proc_lib` already records the parent. It is correct after every restart with nothing replayed, and every child gets it, including ones whose specs never mention it."

### H: ship the named forms (`spawnAs:`, `named:`, `withName:`) now
- ⚙️ **BEAM veteran:** "Registered names are how you find things from observer, a remote shell or `sys:get_state`. An anonymous Catalina server is invisible to all of them."
- 🧑‍💻 **Newcomer:** "Actors have `spawnAs:`; finding it missing on supervisors is a surprise."

### Tension points
- **D's context argument is the strongest case against the decision.** Instance-side configuration runs in an unusual place (inside the supervisor's own `init/1`), and `self` there is a handle you can pass but not message. The decision accepts that because the failure is immediate and explained (`calling_self` → `#supervisor_start_failed` with a hint), only the configuration methods run there, and D would make every subclass rebuild the full flags-and-children value to change one setting, which is the per-setting design ADR 0059 chose deliberately. The cost is a rule users must learn; it is documented, not removed.
- **F's zero migration is the honest price of the decision.** The decision trades a one-time, mechanical, compiler-guided migration (while every supervisor user is first-party) for one form per setting forever. F would keep two forms of every setting, a both-forms compile check and a pre-start evaluation for the life of the language. The purist's "class knowledge" point is real for `strategy` and restart limits, which rarely vary per instance; it does not hold for `childSpecs` or `childClass`, which Catalina computes per instance, and splitting the settings between the two sides would be worse than either.
- **B is rejected on merit, not compatibility.** This ADR breaks configuration anyway, so "it would break exdura" is not the argument. The argument is that an idempotent "ensure it is running" start is useful in its own right, for re-evaluated REPL and workspace code and for the `[application]` boot path, and defining `supervise` as sugar over the same helper means it costs no second mechanism. The surprise B wants to remove shrinks to "a second call returns the running instance", which is the point of the message.
- **G and H are deferred, not rejected.** Passing `self` covers Catalina, and an accessor can be added later without breaking anything; the same holds for the named forms, which would mirror `Actor` exactly. The `pg` group covers `Node current supervisors`; observer and remote-shell users get pids, not names, until H lands.
- E trades a compiler special case for a new word users must learn and that collides with their own `start:` facades; the decision pays the carve-out once.
- **Who sides with whom:** existing users and operators side with F (no migration). Newcomers and Smalltalk developers side with the decision (one form, `self` and `self args`, the same `initialize` hook as actors). BEAM veterans split between the decision and D, and want H soon.

## Alternatives Considered

### B: `supervise` starts fresh; `superviseAs:` registers
Rejected on merit (see Tension points): an idempotent "ensure it is running" start is needed for re-evaluated REPL/workspace code and the `[application]` boot path, and as sugar over the spawn helper it adds no mechanism. Everything B offers is available as `spawn`. (It would also break `ExduraSupervisor supervise valueOrDo:`, `current` after `supervise`, the repl-protocol `supervisor.btscript` / `supervisor_initialize.btscript` cases and ADR 0080's idempotent-startup convention, but that is secondary: this ADR breaks configuration anyway.)

### C: class-side registration switch
Rejected: a class could not be both an app root (named) and a per-test instance (anonymous), which Catalina's test suite needs; and the meaning of `supervise` would depend on a declaration elsewhere.

### D: `class init: args` returning a spec
Rejected: ADR 0059 chose per-setting methods and documented why. Moving those methods to the instance gives D's main benefit (configuration computed from `init`'s argument, with access to the supervisor) without collapsing every setting into one return value that each subclass must rebuild.

### E: `start` / `startWith:` instead of `spawn`
Rejected: it would avoid the intrinsic carve-out, but `start:` is a common user facade name (symphony, watcher, exdura, http, Catalina), and the spawn family already carries the semantics users know from `Actor` (and, later, its named forms).

### F: keep class-side configuration and add `…For: args` hooks
The earlier draft of this ADR: every class-side setting gains a keyword variant (`childSpecsFor:`, `strategyFor:`, … seven in all), plus `childSpecsFor:supervisor:` for the parent handle, with delegating defaults. Rejected: two forms of every setting, a compile error for classes that define both, a pre-start check that evaluated the methods twice, and a special variant just to pass `self`. Instance-side methods give the same power with one form per setting, at the cost of a mechanical migration.

### G: an ancestor accessor (`self supervisor`, from `'$ancestors'`) for the parent handle
Deferred: with instance-side configuration the parent passes `self` explicitly, which is visible in the code and needs no pid-to-handle mapping. An accessor can be added later, without breaking anything, for children that were not given their parent.

### H: named supervisor instances now
Deferred: no current user needs them (Catalina, exdura and watcher use anonymous instances or `supervise`). They mirror `Actor`'s `spawnAs:` / `named:` exactly and can be added on the same start helper later.

### `children:` as the configuration name
Rejected: with BT-3966's live-children `sup children`, the same name would mean "running children" and "child specs" on the same object.

### `superviseWith: args`
Rejected: the instance's identity (singleton or not) would depend on whether args were passed.

### Keep singletons; have Catalina generate classes
Rejected: `numAcceptors` is a runtime option and the handler class belongs to the user; code generation at start time is not a thing Beamtalk does.

## Consequences

### Positive
- Libraries can build OTP-shaped trees from runtime configuration, including per-server limits and parent handles; Catalina Phase 2 is unblocked, exdura's per-pool supervisor becomes expressible, supervisor tests no longer need to be serial.
- No change to how top-level code starts supervisors; the spawn vocabulary and its unlinked semantics are already known from `Actor`.
- Delivers ADR 0080 Future Work #4 with the `Result(Self, Error)` shape it asked for.
- A nested bare supervisor class no longer fails `already_started` in a multi-instance parent, and a DynamicSupervisor of supervisors can start more than one child.

### Negative
- Two entry points to learn (`supervise` vs `spawn`), differing only in idempotence and the implicit name; `supervise` is sugar over the same start helper, not a separate path. The docs must state the rule in one sentence: *`supervise` ensures, `spawn` creates.*
- Behaviour change: `supervise` no longer links the tree to the class process. A supervisor that exhausts its restart intensity now dies alone instead of taking its class process down (the exdura failure); code that relied on the class process dying with it would notice, and none is known.
- `spawn` / `spawnWith:` are no longer uniformly actor intrinsics: codegen and the type checker each need a supervisor carve-out, and must keep it as the intrinsics evolve.
- **Breaking:** class-side configuration (`class children`, `class strategy`, `class childClass`, …) moves to the instance side, and `class children` becomes `childSpecs`. About 75 definitions in stdlib, tests and examples, ~60 in docs, and 17 across exdura, symphony, watcher and catalina. The change is mechanical and the compiler names the fix for each one.
- Configuration methods run in an unusual context: inside the supervisor's own `init/1`, where messages to `self` fail (`calling_self`). Other supervisor instance methods run in the caller. One sentence in the docs, and the failure is immediate and explained.
- The parent handle is a hang hazard: calling a supervisor from code it is waiting on (its configuration methods, nested supervisors' configuration methods, `withName:`/`withClassMethod:` children's `initialize`) hangs during start, restart and `startChild:`. It is a documented rule, not something the compiler can check, and unnamed children that call early still wait up to the 5 s call timeout.
- Behaviour change: a supervisor nested as a bare class is anonymous, so `InnerSup current` returns nil for it and `InnerSup supervise` starts a separate instance.
- Anonymous instances are only reachable through the object you hold or `Node current supervisors`. Same as anonymous actors.
- Class-based `which:` and `terminate:` lose their meaning in multi-instance parents: `which: AcceptorSupervisor` answers the first of 100 matching children, and `terminate: aClass` treats the class name as the child id, so on integer-id or `withId:` children it silently does nothing. BT-3966 fixes `terminate:` (resolve by module, `#ambiguous_child` on several matches); `which:` should get the same `#ambiguous_child` rule there. Until then, use by-id control.
- Integer ids widen types that tooling assumes are atoms: `beamtalk_process_navigation:classify/3` (`Id :: atom() | undefined`) and `whichChildren/1` (`{ok, [atom()]}`) need their specs widened, and REPL rendering of supervision trees must print integer ids.
- Runtime class-method overrides (ADR 0084) can still install a class-side `children`/`strategy`/… that the compile-time check never saw; the runtime ignores it. The override path warns when it installs a removed class-side configuration selector on a supervisor class.
- Hot upgrade: a parent's child specs built before the upgrade still name `{M, start_link, []}`, so an OTP restart of a nested supervisor after the upgrade recreates a *registered* child until the parent itself restarts and rebuilds its specs. Mixed trees are possible in that window; documented, not solved.
- Tests stop being `serial` only if they use `spawn`/`spawnWith:` and avoid global side effects in `initialize`; tests using `supervise` stay serial.

### Neutral
- `current` keeps its narrow meaning (the class-named singleton). Anonymous instances are reached through the object you hold.
- `SupervisionSpec withId:` widens to `Symbol | Integer`; existing symbol ids are unaffected. `whichChildren/1`'s id type widens with it until BT-3966 replaces it.
- A DynamicSupervisor's child class, `maxChildren` and child shutdown become per instance, read from the instance's process dictionary; the generated `childClass/0` stays as the fallback for old or remote supervisors.

## Implementation

**BT-3963** (instance-side configuration, `initialize`, args into `init/1`; size L, ~3 days; first):
1. **Stdlib**: move every configuration method to the instance side (`childSpecs`, `strategy`, `maxRestarts`, `restartWindow`; `childClass`, `maxChildren`, `childShutdown`), with docs (including the no-messages-to-`self` and hang rules). Migrate every supervisor in stdlib, tests, examples and docs.
2. **Codegen and runtime**: `supervisor_codegen.rs` emits `start_link/1` and `init(Args)` → `static_init/3` / `dynamic_init/3`, which build the handle and read each setting by instance dispatch; map `calling_self` to `#supervisor_start_failed` with the hint; delete `precheck_dynamic_config` and the class-side config readers (`class_config_value`, `call_class_method_direct` for configuration); `'$beamtalk_child_class'` and `pool_child_class/1` for `startChild/1,2,3`.
3. **Validators** (`supervision_validators.rs`): `check_class_side_supervisor_config` (compile error with fix-it for each removed class-side selector) and `check_children_supervision_policy` reading `childSpecs` bodies.
4. **Tests**: `stdlib/test/supervisor_start_args_test.bt` and `bt3963_*` fixtures; validator unit tests; eunit for the args-aware resolver.

**BT-3962** (spawn family, nesting, discovery; size L, ~3–4 days; after BT-3963):
5. **Stdlib** (`supervisor.bt`, `dynamic_supervisor.bt`, `supervision_spec.bt`): `spawn`, `spawnWith:`, `supervisionPolicy`, `supervisionSpec` (class side); `args` and `initialize` (instance side; `class initialize:` removed); `withId:` accepts `Symbol | Integer`.
6. **Codegen**: (`start_link/1` and `init(Args)` come from BT-3963.) `dispatch_codegen.rs` `try_handle_spawn_await` and the `class_self_spawn` arm lower supervisor receivers to the runtime start helper in the caller; `type_checker/validation.rs` skips the `spawnWith:` state-key check for them.
7. **Runtime** (`beamtalk_supervisor.erl`): one start helper for anonymous starts (unlink, init gate, `pg:join`), with `supervise` (`start_link_supervisor/2`) rewritten onto it (registered under the module, already-started → existing instance, unlinked); the `beamtalk_supervisor_new` post-dispatch hook sends instance-side `initialize`; `build_child_spec/2` supervisor branch deleted; `spec_to_otp/2` starts supervisor children through the `start_supervisor_child` trampoline (with the `start_link/0` fallback for old modules); `start_dynamic_supervisor_child/2,3` on the same trampoline; `pg` join in `init/1` for top-level starts; widened id specs in `whichChildren/1` and `beamtalk_process_navigation:classify/3`; args stored for `sup args`. `beamtalk_process_navigation:default_root_pids/0` adds the `pg` group.
8. **Tests**: `stdlib/test/supervisor_instances_test.bt` (not serial) and `bt3962_*` fixtures; eunit for `spec_to_otp/2` supervisor mapping, no-link, `pg` membership, `named/2`; codegen tests for the carve-out and `start_link/1,2`; a type-checker test for `spawnWith:` on a supervisor class.

**Docs** (both issues): language-features § Supervision Trees gets an "Instances and arguments" subsection (`supervise` ensures, `spawn` creates; unlinked; nesting via `supervisionSpec`; integer ids; instance-side configuration table; args/restart rule; parent-handle rule) and replaces "define two classes"; ADR 0059 status note; ADR 0080 Future Work #4 marked delivered.

Affected components: stdlib, runtime, codegen, type checker, semantic analysis, tests, docs. No parser change.

## Migration Path

None for top-level code: `supervise`'s result and idempotence, `current`, `[application] supervisor =` and `Workspace startSupervisor:` are unchanged, as is the `withArgs:` meaning for actor children. `supervise` no longer links the started tree to the class process; no code change is needed, and a crashing tree now leaves its class process alive. **Configuration moves to the instance side** (breaking, mechanical): drop `class` from `strategy`, `maxRestarts`, `restartWindow`, `childClass`, `maxChildren`, `childShutdown`, rename `class children` to `childSpecs`, and turn `class initialize: sup` into instance-side `initialize` (with `self` for `sup`). The compiler rejects the old forms with that fix-it, so nothing silently changes behaviour. One further behaviour change: a supervisor nested as a bare class in `childSpecs` is now an anonymous child, so `InnerSup current` returns nil for it; use `parent which: InnerSup` or BT-3966's by-id lookup. A DynamicSupervisor with a supervisor child class now starts anonymous children (before, the second `startChild` always failed). Packages that want per-instance supervisors (exdura's `ActivityWorkerSupervisor`) opt in by calling `spawnWith:` where they called `supervise`.

## References
- Related issues: BT-3962 (multi-instance supervisors), BT-3963 (start-time configuration), BT-3964 (`maxChildren`), BT-3965 (`withShutdown:` / `childShutdown`), BT-3966 (child control by id; live `children`), BT-3972 (per-child shutdown, cancelled in favour of per-instance `childShutdown`); epic BT-3957
- Related ADRs: ADR 0059 (supervision tree syntax; amended: configuration moves to the instance side), ADR 0079 (atom namespaces; named forms deferred), ADR 0080 (lifecycle `Result`, Future Work #4), ADR 0084 (runtime class-method overrides), ADR 0092 (process navigation), ADR 0125 (compiled-module compatibility), ADR 0130 (class-variable purity in configuration hooks)
- Documentation: `docs/beamtalk-language-features.md` § Supervision Trees; `docs/beamtalk-principles.md` §10; `beamtalk-catalina/PLAN.md`
- Code: `stdlib/src/supervisor.bt`, `stdlib/src/dynamic_supervisor.bt`, `stdlib/src/supervision_spec.bt`, `stdlib/src/actor.bt` (`spawn`, `spawnWith:`, `initialize`), `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`, `runtime/apps/beamtalk_runtime/src/beamtalk_process_navigation.erl` (`default_root_pids/0`), `crates/beamtalk-codegen/src/core_erlang/supervisor_codegen.rs`, `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs` (`try_handle_spawn_await`), `crates/beamtalk-core/src/semantic_analysis/type_checker/validation.rs`, `crates/beamtalk-core/src/semantic_analysis/validators/supervision_validators.rs`

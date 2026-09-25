# ADR 0129: Replace Injected `Beamtalk`/`Workspace`/`Transcript` Bindings with Class-Side Facades

## Status
Accepted (2026-09-25)

Amends ADRs 0010, 0019, 0040, 0081 and 0125 (see [Amended ADRs](#amended-adrs)).
Settles BT-3632 (§6).

## Context

### Problem

`Beamtalk`, `Workspace` and `Transcript` look like class names but are not
classes. Each is a binding name for a singleton instance of another class:
`BeamtalkInterface`, `WorkspaceInterface` and `TranscriptStream`. The
workspace creates those instances at boot and stores each one in a `current`
class variable. The REPL resolves the short names to them. Whether any of
this works depends on the boot context the code runs in.

This surfaced after BT-3622 let `classNamed:` take a dynamic symbol.
beamtalk-exdura tried to replace its untyped FFI call
`(Erlang beamtalk_interface) findClass: aName` in
`ExduraHttpServer>>resolveWorkflowClassNames:` with a typed one. Every
spelling fails:

| Spelling | Result |
|---|---|
| `Beamtalk classNamed: aName` | A REPL-only binding. Compiled code gets an "Unresolved class" warning and a runtime `class_not_found`. |
| `BeamtalkInterface classNamed: aName` | `classNamed:` is instance-side: DNU. The type checker infers `Dynamic` and stays silent. |
| `BeamtalkInterface new classNamed: aName` | "Object-kind class `BeamtalkInterface` cannot be instantiated with `new`". |
| `BeamtalkInterface current classNamed: aName` | Works in the REPL. Under `beamtalk test` it raises `UndefinedObject does not understand 'classNamed:'`. |

`current` is set only by `beamtalk_workspace_bootstrap`, which
`beamtalk_workspace_sup` starts in `run`, `workspace` and `release` modes
(ADR 0125). `beamtalk test` starts only `beamtalk_stdlib`, so `current` is
`nil` there.

### The bug class: "works in the REPL, breaks elsewhere"

The same identifier resolves three ways, depending on where the code was
compiled:

| Where the code runs | What `Transcript show: x` compiles to | Outcome |
|---|---|---|
| REPL expression | Session locals, then `beamtalk_workspace:resolve_singleton_instance/1` (`generate_binding_aware_class_send`) | Works |
| Method body of a class loaded into the REPL | `whereis_class('Transcript')`, `undefined` → `nil` (`generate_workspace_class_send`) | Silently `nil`; prints nothing |
| Batch compile (`beamtalk build`, `beamtalk test`) | `whereis_class` → `class_send(undefined, …)` | "Unresolved class" warning, then `class_not_found` |

The codebase works around this:

- **Short names avoided in compiled code.** Examples and BUnit tests write
  `TranscriptStream current`, `BeamtalkInterface current` and
  `WorkspaceInterface current`.
- **Transcripts swapped by hand.** Tests that need a transcript spawn one
  and install it with `TranscriptStream current:`
  (`examples/getting-started/test/hello_test.bt`).
- **Live behaviour tested only end-to-end.** Reflection BUnit tests can
  only check that methods exist
  (`stdlib/test/beamtalk_interface_test.bt`), so live behaviour is tested
  only through the REPL e2e suite.
- **The `beamtalk new` template prints nothing.** Its `Main.bt` prints
  through `TranscriptStream current`, and under `beamtalk run` that stream
  has no subscribers, so the output only goes into its buffer.

### Current mechanics

About 35 sites in the runtime and compiler exist only to make these three
names resolve:

- **Configuration and bootstrap.**
  - `beamtalk_workspace_config` declares `singletons/0` (`Transcript`) and
    `value_singletons/0` (`Beamtalk`, `Workspace`), plus `binding_names/0`
    and `binding_name_for_class/1`.
  - `beamtalk_workspace_bootstrap` writes each instance into its class's
    `current` variable. It monitors the Transcript pid, and retries value
    singletons on `class_not_found` (5 × 200 ms).
- **Resolution** (`beamtalk_workspace_interface_primitives`).
  - A resolver that checks locals, then `bind:as:`, then the singletons,
    then the class registry.
  - A singleton-first `resolve_class_reference/2`, plus
    `resolve_singleton_instance/1`, `lookup_singleton/1` and
    `is_singleton_binding_name/1`.
  - Two functions that hard-code the three names: `handle_session_bindings/1`
    and `is_protected_name/1`.
- **REPL glue.** The singleton branch in run-entry (`beamtalk_repl_eval`),
  binding names added to completions (`beamtalk_repl_ops_dev`), and DNU
  messages renamed from "WorkspaceInterface" to "Workspace"
  (`beamtalk_repl_json`).
- **Compiler.**
  - A `workspace_mode` flag threads through core, codegen, the compiler
    port and `beamtalk_compiler_server`.
  - Two send paths in `dispatch_codegen.rs` and a `ClassReference` branch
    in `core_erlang/mod.rs` depend on that flag.
  - `structural_validators.rs` exempts `known_vars` (the three names) from
    the "Unresolved class" warning, and warns when one of them shadows a
    class.
- **Stdlib.**
  - Three `late classState: current` variables, each with `current` typed
    `… | Nil`, `resetCurrent` and `@expect type` workarounds.
  - `BeamtalkInterface` and `WorkspaceInterface` are "receiver-ignoring"
    instance facades: every method ignores `self` and calls a stateless
    Erlang function.

### Why the Smalltalk model doesn't fit

In Pharo, `Smalltalk` is the `SystemDictionary` of a single live image. It
holds classes *and* globals, and every free identifier in all code binds to
an entry in it. This works because every line is compiled and run inside
that one image.

Beamtalk has no image. It compiles ahead of time to BEAM modules that run in
several boot contexts: the REPL workspace, `beamtalk run` or an escript,
`beamtalk test`, and an OTP release. Each context starts a different set of
applications. A name that one context injects is missing in the others.

Beamtalk also has two things called "globals", and neither is Pharo's:

- **`Beamtalk globals`** is a read-only snapshot of the class registry.
  Nothing resolves names through it. It duplicates `classNamed:`,
  `allClasses` and `SystemNavigation`, and it has no callers outside its own
  tests and docs.
- **`Workspace globals`** is a `BindingsView` (ADR 0081) over the `bind:as:`
  entries plus the three singletons. Only REPL evals resolve names through
  it. Compiled code resolves a free capitalised name only as a class. The
  singleton step that compiled code lacks is exactly where REPL and
  compiled behaviour diverge.

The rest of the stdlib already uses the shape this ADR adopts. `System`,
`File`, `Console`, `Logger` and `Erlang` are class-side APIs with no
singleton, and they behave the same everywhere.

The original arguments for injection no longer apply:

- **The stepping-stone argument.** ADR 0010 presented injection as a
  stepping stone to module imports. ADR 0070 rejected per-file imports.
- **The dispatch argument.** ADR 0010 rejected class-level calls because
  they bypassed dispatch. Since ADR 0013, class-side sends get full
  metaclass dispatch: DNU, cascades, `respondsTo:` and `class`.
- **The `Session` precedent.** ADR 0081 rejected an injected `Session`
  binding in favour of "resolved through the class registry, like
  `Counter`".

### Constraints

1. **Class-side dispatch.**
   - By default, a class-side method runs in its class's gen_server. So
     process-local context is lost, all callers queue on one process,
     `class_send` has 60 s / 5 min timeouts, and a re-entrant send raises
     `dispatch_error`.
   - Codegen emits a direct function call instead when three conditions
     hold: the class is sealed, it has no class variables, and the method
     is `class sealed` (`compute_direct_call_eligible`, `driver.rs`).
   - That direct path is applied only to statically resolved sends in
     compiled modules. REPL expression codegen
     (`crates/beamtalk-repl/src/codegen.rs`) does not compute eligibility,
     and dynamic sends (`beamtalk_class_dispatch:class_send/3`) always use
     the gen_server.
2. **Dependencies flow downward.**
   - `beamtalk_stdlib` depends on `beamtalk_runtime`, not on
     `beamtalk_workspace`.
   - Under `beamtalk test`, the workspace application's code is on the load
     path, but none of its processes run: compiler server, loader,
     ChangeLog, session supervisor.
   - So "is a workspace running?" cannot be answered by checking whether
     its code is loaded.
3. **Refusals have one home.** `beamtalk_capability` (ADR 0125 §1.4–1.5)
   answers "is this operation available on this node?". Every refusal it
   issues is a `#beamtalk_error{}`.
4. **One user.** The only external consumer is beamtalk-exdura, which has
   the same owner. A hard break with no shims is acceptable.

## Decision

### Guiding principle: Smalltalk's vocabulary, not the image's mechanics

Beamtalk is Smalltalk-*like*, not Smalltalk-*compatible*
(`docs/beamtalk-principles.md`):

- **Keep the vocabulary.** Keep what users type and read: message syntax,
  cascades, blocks, reflection selectors (`implementorsOf:`,
  `respondsTo:`, …) and familiar names (`Transcript show:`,
  `Beamtalk classNamed:`).
- **Drop the image mechanics.** Drop mechanisms that depend on a single
  live image:
  - a global `SystemDictionary`;
  - REPL-injected names;
  - singletons that bootstrap code stores in class variables;
  - anything that changes what compiled code means depending on where it
    runs.
- **One meaning everywhere.** An identifier means the same thing in every
  context. A service that is genuinely context-bound raises a structured
  error; it never evaluates to `nil`.
- **Tiebreaker.** When a Pharo habit and idiomatic BEAM behaviour
  conflict, BEAM wins, and the Smalltalk name is kept where possible.

This principle is added to `docs/beamtalk-principles.md` §4 and to the
essential rules in `CLAUDE.md`.

### Summary

The compiler and runtime inject no names. `Beamtalk`, `Workspace` and
`Transcript` become ordinary sealed, stateless stdlib classes with
class-side APIs. `SystemNavigation` follows the same rule. Identifiers
resolve the same way in REPL expressions, method bodies, `beamtalk build`,
`beamtalk test`, `beamtalk run` and releases.

| Name | Class | Wraps | Outside a workspace |
|---|---|---|---|
| `Beamtalk` | renamed from `BeamtalkInterface` | Class registry, `help:`, version, release info, logging config | Works everywhere |
| `Workspace` | renamed from `WorkspaceInterface` | Sessions, bindings, ChangeLog, `load:`, flush, supervisors, tests | Raises `no_workspace` |
| `Transcript` | new | The REPL's `TranscriptStream` process | Routes output through `Logger` |

### 1. How the stdlib exposes system services

Every stdlib class that exposes a system service uses exactly one of three
shapes:

1. **Class-side facade.** For a stateless service with a single scope.
   - A `sealed` class with class-side methods only.
   - No `classState:`; the class is never instantiated.
   - Examples: `Beamtalk`, `Workspace`, `Transcript`, `SystemNavigation`,
     `System`, `File`, `Console`, `Logger`.
2. **`default` and named factories.** Only for classes whose *instances
   carry state or scope*, where `default` selects one scope among several.
   - Examples: `ProcessNavigation default` / `system` / `from:` / `on:`;
     `AnnouncementNavigation default` / `of:`; `Node named:` (ADR 0126).
3. **`current`.** Only for a genuinely live runtime entity.
   - It is found by asking the runtime at call time.
   - It returns `| Nil` only when no such entity legitimately exists.
   - Examples: `Session current` (none outside a REPL session),
     `SystemAnnouncer current`, `Supervisor current`.

**Forbidden:** a singleton stored in a class variable by bootstrap code.
- It fails in every boot context that doesn't run the bootstrap.
- It races class loading, which is why the retry loop exists.
- It forces `| Nil` types and `@expect type` workarounds.

### 2. Properties every facade has

- **Sealed and stateless.**
  - `sealed typed Object subclass: …` with no `classState:`, no instance
    API, and every method `class sealed`.
  - State lives in Erlang: the class registry, logger configuration, the
    workspace supervisor tree and the registered `'Transcript'` process.
- **Same behaviour however it is reached.** Every send to a facade runs as
  a direct call in the caller's process, whether it is:
  - static: `Workspace test`;
  - in a REPL expression;
  - dynamic: `ws := Workspace. ws test`, `(Beamtalk classNamed:
    #Workspace) test`, `perform:`, a class passed as an argument, or
    `class_send` from Erlang.

  This rules out the four class-process hazards (Constraint 1): lost
  process context (`Workspace currentSession` reads the caller's process
  dictionary), queuing on one process, timeouts, and re-entrancy (a
  `Workspace test` whose tests call `Workspace`). Two mechanisms deliver
  it:
  - **REPL expressions.** REPL codegen computes `direct_call_eligible`
    exactly as module codegen does.
  - **Dynamic sends.**
    - Codegen records each module's direct-callable class selectors in
      `__beamtalk_meta/0` as `direct_class_methods => #{Selector =>
      SafeFn}`, computed by `compute_direct_call_eligible`.
    - `beamtalk_class_dispatch:class_send/3` reads that set (cached at class
      registration, refreshed on reload). For a listed selector it calls
      `Module:SafeFn(nil, #{}, Args)`, the same convention as the static
      direct call. Otherwise it uses the gen_server.
    - This implements the direct dispatch that ADR 0013 §4 describes for
      class methods that don't touch class state.
    - The eligibility rule is computed in one place (Rust) and delivered
      to the runtime as generated metadata. There is no second
      implementation in Erlang.

  This applies to *every* sealed, stateless class, not only the facades.
  Classes with class state keep the gen_server, which their correctness
  depends on.
- **First-class, with no `new` and no `current`.** The class is the object:
  `Beamtalk class`, `Workspace respondsTo: #load:` and `x := Beamtalk. x
  version` all work.
- **Real signatures.** The type checker sees ordinary metaclass methods;
  `Beamtalk classNamed:` is `Class | Nil` because the method declares it.
- **Backing FFI.**
  - `Workspace` and `Transcript` are `native:` over their primary backing
    module and use `self delegate`.
  - `Beamtalk` is `native: beamtalk_interface` for its reflection
    selectors. Its logging selectors (`beamtalk_logging_config`) and
    release selectors (`beamtalk_release`) use inline `(Erlang …)` FFI.
  - `beamtalk_interface.erl` also backs `SystemNavigation`, so the export
    renames for the delegate convention are limited to the functions
    `Beamtalk` uses.

### 3. `Beamtalk`: system reflection, everywhere

`BeamtalkInterface` becomes `Beamtalk`. Its methods become `class sealed`
with unchanged selectors and types:

- `allClasses`, `classNamed:`;
- `help:`, `help:selector:`, `erlangHelp:`, `erlangHelp:selector:`;
- `version`, `releaseInfo`, `shapeManifest`;
- `logLevel`, `logLevel:`, `logFormat`, `logFormat:`, `debugTargets`,
  `enableDebug:`, `disableDebug:`, `activeDebugTargets`, `disableAllDebug`,
  `loggerInfo`.

The `LogLevel` and `LogFormat` type aliases move with the class. None of
these methods needs a workspace.

**`Beamtalk globals` is removed**, with `beamtalk_interface:globals/0` and
`handle_globals/0`. Use `Beamtalk classNamed:`, `Beamtalk allClasses` or
`SystemNavigation` instead.

```beamtalk
// exdura, compiled package
resolveWorkflowClassNames: names :: List(Symbol) -> List(Class) =>
  names collect: [:aName |
    (Beamtalk classNamed: aName) ifNil: [
      self error: "unknown workflow class " ++ aName printString]]
```

```beamtalk
Beamtalk classNamed: #Integer      // => Integer
Beamtalk classNamed: #NoSuchThing  // => nil
Beamtalk version                   // => "0.x.y"
```

### 4. `Workspace`: project operations where a workspace runs

`WorkspaceInterface` becomes `Workspace`. Its methods become `class sealed`
with unchanged selectors and types, except that `globals` becomes `bindings`
(§7) and `isAvailable` is new:

- `isAvailable`;
- `load:`, `newClass:at:`, `moveClass:to:`;
- `classes`, `testClasses`;
- `bindings`, `bind:as:`, `unbind:`;
- `currentSession`, `sessions`;
- `actors`, `actorAt:`, `actorsOf:`, `processes`, `nodes`;
- `changes`, `sync`, `recheckImage`;
- `flush`, `flush:`, `flush:confirmDestructive:`,
  `flushIncludingDestructive`, `autoflush`, `autoflush:`;
- `test`, `test:`;
- `supervisor`, `startSupervisor:`, `stopSupervisor:`, `supervisors`;
- `dependencies`.

**A workspace is running** once `beamtalk_workspace_sup` has started and
recorded the node's capabilities. That happens in `run`, `workspace` and
`release` modes.

**Outside a workspace** (under `beamtalk test`, or on a bare runtime), every
method except `isAvailable` raises:

```
#beamtalk_error{
  kind     = no_workspace,
  class    = 'Workspace',
  selector = 'load:',
  message  = <<"Workspace>>load: needs a running workspace; none is running on this node">>,
  hint     = <<"Workspace operations are available in the REPL (beamtalk repl), "
               "beamtalk run and releases, not under beamtalk test. "
               "For class lookup use Beamtalk classNamed:.">>
}
```

**`Workspace isAvailable -> Boolean`** never raises. A test class can run
both under `beamtalk test` and inside a workspace (`Workspace test`, REPL
`:test`, MCP `run_tests`). So a test of the refusal states its context:

```beamtalk
testWorkspaceRefusesWithoutWorkspace =>
  Workspace isAvailable ifTrue: [^self skip: "runs only outside a workspace"]
  self should: [Workspace classes] raise: #no_workspace
```

**Availability is decided in `beamtalk_capability`:**

- **`require_workspace/1`.**
  - It answers `ok` or `no_workspace`, according to whether a workspace
    supervisor has recorded capabilities on this node.
  - It deliberately differs from `current/0`'s permissive default, which
    `classify/1` keeps for bare-runtime unit tests.
- **One guard site.** The facade runs `require_workspace/1` once, at the
  delegate entry of `beamtalk_workspace_interface_primitives`, before any
  capability check or call into the workspace application.
- **`Behaviour` workspace operations use the same guard.** These are
  `classReload`, `compile:source:`/`tryCompile:source:`, `precheck`,
  `remove_local_method`, and the `renameTo:`/`renameSelector:to:` site
  rewriters in `beamtalk_behaviour_intrinsics`.
- **Release-mode refusals are unchanged.** After `require_workspace/1`
  passes, the ADR 0125 §1.5 checks apply: `release_mode_no_compiler` and
  `release_mode_no_workspace`.
- **Compiler availability is recorded in every mode.**
  `beamtalk_workspace_sup` records `include_compiler` in `run` mode, taken
  from `start_compiler`, and `permitted(compiler, …)` consults it in every
  mode. A packaged escript without a compiler therefore refuses
  `Workspace load:` with `run_mode_no_compiler`.
- **Capabilities are cleared** when `beamtalk_workspace_sup` shuts down.
- **In `run` mode**, `Workspace` behaves as ADR 0125 §1.5 specifies.
  Session-oriented selectors answer truthfully and empty: `sessions` is
  `#()`, `currentSession` is `nil`, and `bindings` is empty.

### 5. `Transcript`: the REPL's shared log and a day-0 convenience

`Transcript` exists for newcomers and REPL use. Programs use `Logger`
(ADR 0064), or `Console` for plain stdout/stderr. `Transcript` has no stream
protocol, no capture API and no instance. The language guide, the class
comment and the `beamtalk new` template all say this.

| Selector | In an interactive workspace | Elsewhere |
|---|---|---|
| `show: value` | Appends `value printString` (a `String` as-is) to the `TranscriptStream` | One Logger `notice` event, domain `[beamtalk, user, transcript]` |
| `cr` | Appends a newline | No-op |
| `showCr: value` | `show:` then `cr` | One Logger event |
| `recent` | The buffered lines | `no_workspace` |
| `clear` | Empties the buffer | `no_workspace` |

- **Interactive workspace.**
  - A node running a REPL server: `workspace` mode, or a release with its
    console enabled.
  - `beamtalk_workspace_sup` starts the registered `'Transcript'` process
    only alongside the REPL server, never in `run` mode.
  - The facade routes to that process when it is registered *and* is the
    workspace's `TranscriptStream`, so a user actor registered as
    `#Transcript` cannot capture the output.
  - While the stream restarts, output goes to Logger.
- **Logger route.**
  - `notice` is OTP's default primary level, so the output is visible under
    `beamtalk run`, in tests and in releases.
  - The runtime installs a dedicated handler, `beamtalk_transcript_log`: a
    `logger_std_h` on `standard_io` with template `[msg, "\n"]`, accepting
    only the transcript domain.
  - The default handler filters that domain out. The output is plain text,
    one line per event, with no report header.
  - Capturing, silencing or redirecting it is ordinary Logger
    configuration.
- **Exit.** `beamtalk_script_harness`, `Program exit:` and `System halt:`
  flush the transcript handler before `erlang:halt/1`.
- **Formatting and ordering.**
  - The Logger route is stateless: `show:` followed by `show:` produces two
    events, not one line.
  - Ordering relative to `Console`, which writes synchronously, is not
    guaranteed.
- **`Object>>show:` / `showCr:`** delegate to `Transcript`.
- **`TranscriptStream`** remains as the REPL stream's actor class, without
  `current`, `current:` or `resetCurrent`.

```beamtalk
// REPL
Transcript show: "Hello"; cr; show: "World"   // shown in the transcript pane
Transcript recent                             // => #("Hello", "World")
```

```beamtalk
// compiled code under beamtalk run
Transcript showCr: "starting"    // prints: starting
Transcript recent                // raises no_workspace
```

### 6. `SystemNavigation` is a class-side facade; Object-kind classes are never instantiated

`SystemNavigation` is a stateless service over the node's single class
registry, so it follows rule 1. Every query becomes `class sealed`, and
`default` is removed:

```beamtalk
SystemNavigation sendersOf: #printString
SystemNavigation implementorsOf: #asString
SystemNavigation actorClasses
```

- **Instance or class side makes no difference to where queries run.**
  They run in the caller either way (§2), so a walk over every class is
  never re-entrant.
- **Scoping.** Package scoping is expressed as query arguments
  (`classesInPackage:`, `subclassesIn:`). A future scoped navigation would
  be a separate rule-2 `Value` carrying its scope.

This settles **BT-3632**:

- **Object-kind classes are never instantiable.** No stdlib code
  instantiates one.
- **The check does not depend on how the receiver is spelled.**
  `check_actor_new_usage` / `object_kind_new_error` in
  `class_validators.rs` rejects `self new` and `super new` in a class
  method of an Object-kind class, as well as `Foo new`.
- **Hint text.** "Object subclasses are not instantiable. For a stateless
  service, use class-side methods; for data or a scoped query object, use
  `Value subclass:`."
- **The rule-2 classes already conform.** `AnnouncementNavigation` creates
  its handles through FFI, and `ProcessNavigation` is a `Value`.

### 7. Name resolution and `Workspace bindings`

A free identifier in a REPL expression resolves through:

1. session locals;
2. `Workspace bind:as:` entries;
3. the class registry.

Method bodies and batch-compiled code use only the class registry. The three
names therefore resolve exactly as `Integer` does, which is the rule ADR 0081
already applies to `Session`.

**`Workspace globals` is renamed `Workspace bindings`.**
- "Globals" implies Smalltalk's "visible to all code", but these entries
  are visible only to REPL evals.
- The view holds only the user's `bind:as:` entries.
- The name matches what the view is: it returns a `BindingsView`, pairs with
  `bind:as:` and `unbind:`, and parallels `Session current bindings`.
- `beamtalk_session_primitives:globalsView` is renamed to match.

```beamtalk
Workspace bind: 42 as: #answer
Workspace bindings                // => a BindingsView {#answer => 42}
Workspace bindings at: #answer    // => 42
```

**Protecting class names.**
- `bind:as:` and `Workspace bindings at:put:` refuse to shadow *any*
  stdlib class name, with `name_conflict`. This replaces the hard-coded
  `is_protected_name/1`.
- The `known_vars` exemption and the `check_workspace_shadows` warning are
  deleted.
- A plain REPL assignment such as `Transcript := 3` behaves like
  `Integer := 3`: it creates a session local that shadows the class.

### 8. Distribution: facades are node-local

Facades act on the node that runs the code. That matches ADR 0126, where a
class object that crosses nodes is resolved by name on the receiving node,
so every class name means "this node's class". It also matches Erlang, where
`code:` and `logger:` are node-local and remote access is an explicit
`erpc:call/4`.

| Node kind | `Beamtalk` | `Workspace` | `Transcript` |
|---|---|---|---|
| Workspace (REPL) | Its registry | Its workspace | Its `TranscriptStream` |
| `run` / escript | Its registry | Its `run`-mode workspace (§4) | Its Logger |
| Release | Its registry | Partially available per ADR 0125 §1.5: introspection such as `actors`/`sessions` works; compiler ops need `include_compiler`; `flush`/rename are refused | Its Logger, or its `TranscriptStream` when the console runs |
| Bare runtime (e.g. `beamtalk test`) | Its registry | `no_workspace` | Its Logger |

- **Dynamic sends stay local.** A direct call is a local `Module:Fun(...)`.
  A class object received from a peer resolves to the local class, so a
  dynamic send to it is a local direct call.
- **Output from peer nodes.** An actor running on node B writes
  `Transcript` output to B's transcript or B's Logger, not to a REPL
  attached to node A. Cross-node output uses a forwarding Logger handler or
  an actor passed in explicitly.
- **Remote access is a separate decision**, outside ADR 0126 v1 scope. The
  design keeps one generic path open, following the pattern of ADR 0126's
  `Node`: class side for "here", a value handle for "there".

  ```beamtalk
  worker := (Node named: #'worker@localhost') unwrap
  (Beamtalk on: worker) classNamed: #Counter     // future ADR
  (Workspace on: worker) actors                  // future ADR
  ```

  - The handle is a `Value` carrying the `Node`. It forwards each message
    as `erpc:call(Node, Module, SafeFn, [nil, #{} | Args])`, taking
    `SafeFn` from the peer's `direct_class_methods` metadata (§2).
  - One generic handle therefore serves every facade.
  - It inherits ADR 0126's version-skew handling and failure mapping.
  - It is possible only because every facade is stateless with `class
    sealed` methods.

### 9. Removed names

No shims or deprecation period:

- `BeamtalkInterface` and `WorkspaceInterface`, including their `current`.
- `TranscriptStream current`, `current:` and `resetCurrent`.
- `Beamtalk globals`, `Workspace globals` and `SystemNavigation default`.

Code that uses a removed name gets the standard `class_not_found` or DNU,
with a hint naming the replacement.

### 10. Related context-dependent behaviour

Other places where behaviour depends on boot context are resolved as
follows.

**Fixed by this ADR:**

| Behaviour | Resolution |
|---|---|
| A class body loaded into the REPL turns *any* missing class into `nil` (`generate_workspace_class_send`). Batch-compiled code raises `class_not_found`. | That path is deleted (Phase 4). Both raise `class_not_found`. |
| `beamtalk_capability` reports `workspace` capabilities on nodes that never started a workspace, so every capability guard passes under `beamtalk test`. | `require_workspace/1` (§4). |
| `Behaviour` workspace operations detect a missing workspace by catching `error:undef`. Under test the code is loaded, so they fail later with `compile_failed {noproc…}`. | Guarded by `require_workspace/1` (§4). |

**Separate follow-ups:**

- **`Program exit:` depends on the `node_owning` application
  environment (BT-3634).** In a normally booted release it throws
  `{beamtalk_script_exit, N}`, which crashes the calling process while the
  node keeps running. What it means in a release needs its own decision.
- **File-handle ownership differs by context (BT-3635).** `beamtalk_file:
  resolve_owner` makes a handle belong to the session shell in the REPL
  and to the calling process elsewhere.
- **Node introspection moves from `Workspace` to `Node` (BT-3633).**
  - `actors`, `actorAt:`, `actorsOf:`, `processes`, `supervisors` and the
    root supervisor are facts about a node, not the development workspace.
  - They become `Node` instance methods (`Node current actors`, `worker
    actors`), so they work in every boot context and on peers, without the
    remote facade handle of §8. `Workspace nodes` folds into
    `Node connected`.
  - The actor registry moves into `beamtalk_runtime` to support this.
  - `Workspace` keeps development-environment operations, including
    `startSupervisor:`/`stopSupervisor:`.
  - Whether logging control moves to `Logger` is decided there too.

**Context-bound by design, and already conforming:**

- **`current` lookups (rule 3).** `Session current`, `Supervisor current`
  and `DynamicSupervisor current` return a documented `| Nil`.
- **Announcements.** `SystemAnnouncer current` and
  `AnnouncementNavigation default` run on the runtime supervisor.
  Workspace-only announcements describe workspace activity.
- **Process navigation.** `ProcessNavigation default` returns a smaller tree
  without a workspace.
- **Workspace-side lookups.** Metadata lookups, ChangeLog reads, the
  inspector's `evaluate:` and `pg` shell lookups return empty results or
  structured errors when the workspace is absent.

### Misuse and error examples

```beamtalk
Beamtalk new                        // compile error: Object-kind class `Beamtalk` cannot be instantiated with `new`
BeamtalkInterface current           // class_not_found: BeamtalkInterface. Hint: renamed to Beamtalk (ADR 0129)
Workspace changes                   // under beamtalk test: #beamtalk_error{kind: no_workspace, selector: 'changes', …}
Workspace bind: 3 as: #Transcript   // name_conflict: Transcript is a stdlib class and cannot be shadowed
Beamtalk classNamd: #Foo            // unknown-selector warning at compile time; DNU at runtime
SystemNavigation default            // DNU. Hint: SystemNavigation queries are class-side (ADR 0129)
```

## Prior Art

| Language | How system-level objects are reached | Takeaway |
|---|---|---|
| **Pharo / Squeak** | `Smalltalk` (a `SystemDictionary`) and `Transcript` (a `ThreadSafeTranscript`) are image globals; all code is compiled inside the image. | Works only with one image and in-image compilation. Beamtalk has neither. |
| **GNU Smalltalk** | The same globals, plus a scripting mode where `Transcript` flushes to stdout. | Even Smalltalk needs a non-interactive fallback for `Transcript`. Ours is `Logger`. |
| **Newspeak** | No globals. The platform object (transcript, mirrors) is passed to the module constructor. | Explicit capabilities. It needs a module-parameter system that Beamtalk lacks (ADR 0010: not planned). |
| **Erlang** | Module functions: `code:which/1`, `logger:notice/1`, `io:format/1`. Node-local, available in every node type. | Exactly what a sealed, stateless class-side facade compiles to. |
| **Elixir** | `Code`, `Logger`, `IO` and `System` work everywhere. IEx helpers (`h/1`, `recompile/0`) are imported only into the shell. `Mix.Project` raises when Mix isn't running. | Shell-only helpers stay out of compiled code. A context-bound service raises clearly, as `Workspace` does with `no_workspace`. |
| **Gleam** | `io.println`; no ambient objects. | A BEAM language works without globals. |
| **Python / IPython** | `sys` and `logging` are ordinary modules. IPython injects `get_ipython()`, `display()` and magics into the shell only; code copied into a `.py` file fails with `NameError`. | The same "works in the REPL, breaks in the script" trap. |
| **Livebook / Kino** | `Kino` functions are ordinary module calls that degrade outside a Livebook runtime. | A facade that adapts to its context, like `Transcript` routing to `Logger`. |
| **Pony** | `env.out` is a capability passed to `Main`. | Explicit, but it must be threaded through everywhere; ADR 0058 rejected capability restriction. |

- **Adopted:** the Erlang/Elixir model. Ambient, stateless, node-local
  module-level APIs that behave the same everywhere, and a clear error where
  a context-bound service is absent.
- **Kept from Smalltalk:** the names and message-send syntax, including
  cascades.
- **Rejected:** image globals (Pharo) and passed capabilities (Newspeak,
  Pony).

## User Impact

**Newcomer.**
- `Transcript show: "hi"` and `Beamtalk help: Integer` work in the REPL and
  in the first compiled program.
- The `beamtalk new` program prints under `beamtalk run`.
- `Workspace …` in a test explains itself instead of reporting
  `UndefinedObject does not understand`.
- Completion shows the three names as classes with class-side selectors.

**Smalltalk developer.**
- The expected names and cascades are all there.
- `Transcript`, `Beamtalk` and `SystemNavigation` are classes rather than
  instances. They are still first-class objects: they answer `class`,
  `respondsTo:` and `printString`, and can be stored and sent messages
  dynamically.
- What is lost is substituting a different instance, such as a mock
  transcript.

**Erlang/Elixir developer.**
- A facade call is a direct call to one exported function, whether sent
  statically or dynamically.
- From Erlang, a facade is a plain module function, with no gen_server
  involved.
- `Transcript` output outside the REPL is an ordinary `logger` event in a
  known domain.

**Production operator.**
- **Fewer moving parts.** No bootstrap retry loop, no `current` slots to
  re-wire after a crash, and no class-process bottleneck on stateless
  classes.
- **Less idle state in `run` mode.** It no longer runs a transcript process
  whose unread buffer fills to 1000 entries.
- **Visible output.** Transcript output from releases and `beamtalk run`
  appears in the normal log.
- **Release metadata from compiled code.** `Beamtalk releaseInfo` resolves
  in compiled code on any node.

**Tooling developer.**
- The compiler loses the `known_vars` exemption, two special send paths and
  the singleton uses of `workspace_mode`.
- The LSP, MCP and language service see ordinary classes. Hover, go-to
  and completion need no runtime-supplied binding list.
- The source strings tools generate (`Workspace changes`, `Workspace flush`,
  `Beamtalk help:`) remain valid.

## Steelman Analysis

### A. Status quo
- 🎩 **Smalltalk purist**: "`Transcript` is an *instance*, as in Pharo. It
  can be passed, stored and substituted like any object."
- 🧑‍💻 **Newcomer**: "It already works in the REPL, which is where I live."
- ⚙️ **BEAM veteran**: "It's built. Don't churn 35 sites for aesthetics."

### B. Keep injection; initialise `current` at runtime startup
- 🏭 **Operator**: "The smallest change that fixes exdura:
  `BeamtalkInterface current classNamed:` would work everywhere."
- 🎩 **Smalltalk purist**: "It keeps the instance model."

### C. Extend the injected bindings into compiled code
- 🧑‍💻 **Newcomer**: "One spelling everywhere, and nothing renamed."
- ⚙️ **BEAM veteran**: "The resolver exists; call it from batch codegen too."

### D. Class-side facades (chosen)
- 🧑‍💻 **Newcomer**: "The names in the docs are real classes. They work in
  every file, and completion knows them."
- 🎩 **Smalltalk purist**: "The class is still an object, and message sends
  and cascades are intact."
- ⚙️ **BEAM veteran**: "It compiles to a direct call. It's `code:` and
  `logger:` with nicer syntax."
- 🏭 **Operator**: "No bootstrap wiring to fail, and no class process to
  queue on."
- 🎨 **Language designer**: "Identifier resolution becomes locals → bindings
  → classes, everywhere."

### E. Smalltalk-style globals: ADR 0081's resolver in compiled code
- 🎩 **Smalltalk purist**: "The faithful model. Compiled code resolves free
  identifiers through a global table that every boot path populates, and
  users can add their own globals."
- 🎨 **Language designer**: "One uniform mechanism for classes *and*
  well-known objects."

### F. Keep the old class names; add class-side methods and aliases
- ⚙️ **BEAM veteran**: "`BeamtalkInterface` says what it is, and every
  existing reference keeps compiling."
- 🎨 **Language designer**: "Class aliases are generally useful, beyond
  this case."

### G. `Beamtalk` facade only; `Workspace` stays REPL-scoped
- 🏭 **Operator**: "The smallest change that fixes the actual bug. It
  doesn't reserve `Workspace` as a class name, and it sidesteps what
  `Workspace` means in `run` mode or in an escript."
- 🎨 **Language designer**: "A compile-time 'REPL-only' error beats a
  runtime one, and `IEx.Helpers` shows the split works."

### Tension points
- **Instance substitutability versus consistency.** Smalltalk purists value
  being able to substitute the instance (A, E). The only use of that in
  practice is test capture of Transcript output, which Logger configuration
  covers.
- **G is the strongest alternative.** It is smaller, and it keeps
  `Workspace` free as a user class name. It loses because it keeps a
  REPL-only injected name, which is the principle this ADR establishes, and
  because `Workspace` is legitimately used by compiled code in `run` mode
  and in release consoles.
- **Transcript's fallback.** Candidates were a process in every mode, stdout
  and an error. Logger won because it is visible by default, configurable,
  and doesn't compete with `Console` as an output API.

## Alternatives Considered

### A. Status quo
Leaves silent `nil` in REPL-loaded method bodies, keeps exdura on untyped
FFI, and limits live facade testing to the REPL e2e suite.

### B. Keep injection; initialise `current` at runtime startup
Fixes `Beamtalk` only; `Workspace` has no instance to set outside a
workspace. Two spellings remain per concept, `current` stays `| Nil`, and all
the injection machinery stays.

### C. Extend injected bindings into compiled code
Makes a runtime-resolved, `Dynamic`-typed name a permanent language feature.
Every send becomes a runtime lookup, and the lookup still yields `nil` where
no workspace runs.

### E. Smalltalk-style globals
Extend the REPL resolver (`bind:as:`, singletons, class registry) to every
free capitalised identifier in compiled code, and populate it in every boot
context.
- **It needs an image.** It fits a model where code compiles inside the
  image that holds the globals.
- **It is slower.** Every class reference becomes a runtime lookup that
  cannot use direct calls.
- **It weakens typing.** Any name a binding might shadow types as
  `Dynamic`.
- **It is fragile.** It depends on every boot path populating the table,
  which is the property that fails today.
- **It lets the REPL change compiled code.** User `bind:as:` entries would
  change what compiled methods mean.

### F. Keep the old names, with aliases
Beamtalk has no class aliases. ADR 0019 rejected names that "look like class
names but aren't". It leaves two names per concept.

### G. `Beamtalk` facade only
- **It keeps an injected name.** The compiler would still need to know
  REPL-only names, so `known_vars` survives, and REPL and compiled code
  still disagree about `Workspace`.
- **Its error is wrong.** "REPL-only" rejects legitimate compiled uses in
  `run` mode and in release consoles (ADR 0040's `BuildScript`).

### Transcript alternatives
- **Start the transcript process in every mode.** Output becomes a buffer
  nobody reads in tests and `run` mode.
- **Fall back to stdout.** Duplicates `Console`, and cannot be silenced by
  configuration.
- **Keep `Transcript` REPL-only, with `Object>>show:` on `Console`.** Keeps
  an injected name, and writes to stdout unconditionally.
- **Raise `no_workspace` from `show:`.** Hostile to the day-0 use
  `Transcript` exists for. It is kept for `recent` and `clear`.

## Consequences

### Positive
- **One resolution rule.** Identifiers resolve the same way in every
  context. The "works in the REPL, breaks elsewhere" class of bug goes away,
  including silent `nil` in REPL-loaded method bodies.
- **Typed reflection.** exdura, and any package, can use typed
  `Beamtalk classNamed:`.
- **Real signatures.** The type checker sees ordinary class-side methods,
  with no `| Nil` from `current`.
- **Consistent class-side dispatch.**
  - Static and dynamic sends behave identically for every sealed, stateless
    class, with no process hop, queuing, timeouts or re-entrancy errors.
  - They also run faster.
- **Better tests.** BUnit can test live `Beamtalk` behaviour and the
  `no_workspace` refusal.
- **Large deletions.**
  - Singleton config and bootstrap, including the retry loop.
  - Three `current` slots.
  - The singleton resolution tier, the protected-name list and the
    DNU-message rename.
  - The `known_vars` exemption, the shadow warning and two codegen send
    paths.
- **Visible output.** `beamtalk run` shows Transcript output.
- **One answer to "is a workspace here?"** `beamtalk_capability` owns it,
  and all workspace guards use it.
- **One meaning of "globals".** There is no API left that suggests a global
  scope shared by compiled code.

### Negative
- **Breaking renames.** They land as one in-repo sweep plus an exdura update.
  About 216 `SystemNavigation default` sites change, as do the
  `Workspace globals` uses (22 files).
- **Three reserved class names.** `Beamtalk`, `Workspace` and `Transcript`
  become protected stdlib class names, which user code and dependencies
  cannot define (ADR 0070 §3). Neither this repository nor beamtalk-exdura
  defines them.
- **No substitution.** A facade cannot be replaced by a different instance,
  such as a mock.
- **System-wide dispatch change.** For every sealed, stateless class,
  dynamic sends now run in the caller rather than the class process. Code
  that relied on the class process for such a class would behave
  differently; none is known. The language guide's *Passing Blocks Through
  Class Methods* section and `CLAUDE.md`'s "Blocks into class methods" rule
  are qualified accordingly.
- **Transcript caveats.**
  - The Logger route is not line-exact.
  - Its ordering relative to `Console` is not guaranteed.
  - Capturing it depends on context: Logger configuration outside a
    workspace, the stream inside one.
- **New output in `run` mode and tests.** Transcript output now appears at
  `notice` in `run` mode and in `beamtalk test` output. The migration
  removes Transcript use from tests and examples.
- **Node-local facades.** Remote reflection and workspace operations need
  `Node`'s own methods or `erpc` until the remote handle (§8) is specified.

### Neutral
- `TranscriptStream` remains as the REPL stream's implementation class.
- `Logger` and `Console` are documented as the output APIs. `Transcript` may
  be deprecated later.
- ADR 0048 (class-side method syntax, deferred) gains about 125 more `class
  sealed` methods to migrate if that syntax changes.
- In a mixed-version cluster, the removed and new classes appear as
  ordinary shape-manifest skew until every node is upgraded.
- `Beamtalk` and `Workspace` compile to `bt@stdlib@beamtalk` and
  `bt@stdlib@workspace`, which do not collide with the `beamtalk_*` Erlang
  modules.

## Implementation

The work is one epic; main stays green after each phase.

- **Each facade phase (1–3) removes its own name from the singleton
  config.** The REPL resolver checks singletons before the class registry,
  so a leftover entry would shadow the new class. That means
  `value_singletons/0` or `singletons/0`, plus `handle_session_bindings`,
  `is_protected_name` and `known_vars`.
- **Phase 4** deletes the remaining machinery.
- **Order.**
  - Phases 0a and 0b come first.
  - Phase 1 can ship on its own after them.
  - Phase 6 depends on 0b.
  - Phase 5 follows the facade phases.

**Phase 0a: direct calls from REPL expressions** (codegen, S)
- Compute `direct_call_eligible` in `crates/beamtalk-repl/src/codegen.rs`
  from the class hierarchy, as `driver.rs` does.
- Test: a REPL expression such as `System osPlatform` emits a direct call.
  This proves the core assumption of §2 before any facade changes.

**Phase 0b: direct dispatch for dynamic class sends** (codegen + runtime, M)
- **Codegen.** `class_meta.rs` emits `direct_class_methods => #{Selector =>
  SafeFn}` in `__beamtalk_meta/0`, computed by
  `compute_direct_call_eligible`.
- **Runtime.** `beamtalk_class_dispatch:class_send/3` consults the per-class
  set, cached at registration and refreshed on reload. It calls
  `Module:SafeFn(nil, #{}, Args)` for listed selectors, and the gen_server
  otherwise.
- **Tests.**
  - For a sealed, stateless class, static and dynamic sends (variable,
    `perform:`, `classNamed:`) return the same result in the caller's
    process.
  - A method that messages its own class runs without `dispatch_error`.
  - A class with class state still uses its gen_server.
  - Reload refreshes the cache.
- **Caller-mirror dependency.** `File open:mode:`'s `resolve_owner/0` is the
  only class-side code that reads the caller mirrored into the class process
  (`dispatch_caller_pid/0`). Its ownership rule must be settled first, or in
  the same change (BT-3635).
- **Docs.** Qualify *Passing Blocks Through Class Methods* and the
  `CLAUDE.md` rule, and note in ADR 0013 that its §4 direct dispatch is
  implemented.

**Phase 1: `Beamtalk`** (stdlib, S–M)
- Rename `beamtalk_interface.bt` to `beamtalk.bt`, as a sealed class-side
  facade with no `classState`.
- Delete `current`, `current:`, `resetCurrent` and `globals`, together with
  `beamtalk_interface:globals/0` / `handle_globals/0` and their tests.
- Make `beamtalk_interface` the `native:` module for the reflection
  selectors.
- Replace existence checks in `beamtalk_interface_test.bt` with live
  behaviour tests.

**Phase 2: `Workspace`** (stdlib + runtime, M)
- **Capability layer** (with EUnit tests against explicit capability
  records):
  - `beamtalk_capability:require_workspace/1` and the `no_workspace`
    error kind;
  - `include_compiler` recorded in `run` mode, a mode-independent
    `permitted(compiler, …)` and `run_mode_no_compiler`;
  - capabilities cleared on `beamtalk_workspace_sup` shutdown.
- **Facade.**
  - Rename `workspace_interface.bt` to `workspace.bt`, as a sealed
    class-side facade guarded once at the delegate entry.
  - Add `isAvailable`.
  - Rename `globals` to `bindings`, and `globalsView` to match; update the
    `Session` doc comments.
- **Behaviour guards.** Replace the `error:undef` workspace guards in
  `beamtalk_behaviour_intrinsics` with `require_workspace/1`.
- **Tests.**
  - BUnit: `no_workspace` from `Workspace` and from `Behaviour` under
    `beamtalk test`.
  - Update the REPL e2e cases.

**Phase 3: `Transcript`** (stdlib + runtime, M)
- **Facade.**
  - Add `transcript.bt`, with routing to the registered `TranscriptStream`
    process or to Logger.
  - Remove `current`, `current:` and `resetCurrent` from
    `TranscriptStream`.
  - Repoint `Object>>show:`/`showCr:` at `Transcript`.
- **Logger.**
  - Install the `beamtalk_transcript_log` handler and the default-handler
    domain filter.
  - Flush the handler in `beamtalk_script_harness`, `Program exit:` and
    `System halt:`.
- **Supervision.** Start the `'Transcript'` process only alongside the REPL
  server.
- **Tests.**
  - BUnit: `Transcript show:` under `beamtalk test` succeeds, and `recent`
    raises `no_workspace`.
  - EUnit: the domain handler receives the event.
  - REPL e2e: output reaches the transcript pane.

**Phase 4: remove injection** (compiler + runtime, L)
- **Runtime.**
  - Delete the singleton config (`value_singletons/0`, `binding_names/0`,
    `binding_name_for_class/1`).
  - Delete the bootstrap's value path, retry loop and `current` wiring.
  - Delete the singleton tier of `resolve_name`/`resolve_class_reference`,
    along with `resolve_singleton_instance`, `lookup_singleton`,
    `is_singleton_binding_name` and `is_protected_name`. Replace the last
    with the stdlib-class-name conflict check.
  - Delete the run-entry singleton branch, binding-name completions and the
    DNU-message rename.
- **Codegen.**
  - Delete `generate_binding_aware_class_send`,
    `generate_workspace_class_send` and the workspace-mode
    `ClassReference` branch.
  - Remove `workspace_mode` wherever it only served singleton resolution.
  - Test: a missing-class send raises `class_not_found` in both REPL-loaded
    and batch-compiled code.
- **Semantic analysis.** Delete the `known_vars` exemption,
  `check_workspace_shadows` and their compiler-port plumbing.
- **Tests.** Update the workspace config, REPL compiler, workspace
  supervisor, primitives and structural-validator tests.

**Phase 5: docs, examples and templates** (M)
- **Language guide** (`docs/beamtalk-language-features.md`). Rewrite:
  - *Workspace and Reflection API*, with complete `Beamtalk` and
    `Workspace` tables;
  - *Sessions and binding layers*;
  - a *Transcript* section with the REPL/Logger split and the
    `Logger`/`Console` guidance.
- **Other docs.** Update `docs/beamtalk-tooling.md` (the resolution chain),
  `docs/learning/22-workspace-globals.md` (renamed for `bindings`),
  `docs/stdlib-implementation-status.md`, `docs/beamtalk-ddd-model.md`
  (including its `SystemDictionary` description) and
  `docs/development/testing-strategy.md`.
- **`showLine:`.** Replace the 34 uses of the non-existent `Transcript
  showLine:` with `showCr:`, including the examples in ADR 0126.
- **Template.** The `beamtalk new` `Main.bt` template uses `Console
  printLine:`, and its test is updated.
- **Examples and doc comments.** Update uses of `TranscriptStream current`
  and `WorkspaceInterface current`, and remove the transcript-capture setUps.
- **Surface parity.** Update `docs/development/surface-parity.md` if any
  surface text changes. The tool-generated `Workspace …`/`Beamtalk …`
  strings remain valid.

**Phase 6: `SystemNavigation` + BT-3632** (S–M)
- **Facade.**
  - Make every query `class sealed`, and delete `default`.
  - Update the ~216 call sites, and the file header's instance-side
    rationale.
- **Validator.** Make the Object-kind `new` check independent of the
  receiver, and update the hint.
- **Tests.**
  - Validator tests for `Foo new` and `self new`.
  - A dynamic-send `SystemNavigation` test.

**Single source of truth.**
- **Workspace availability.** `beamtalk_capability` owns "is a workspace
  running?".
- **Direct-call eligibility.** It is computed once, by
  `compute_direct_call_eligible`, and reaches the runtime as generated
  metadata.
- **Name lists.** None survive in either Rust or Erlang: class names live
  only in the class registry generated from stdlib sources.

## Migration Path

| Before | After |
|---|---|
| `BeamtalkInterface current <sel>` / REPL `Beamtalk <sel>` | `Beamtalk <sel>` |
| `WorkspaceInterface current <sel>` / REPL `Workspace <sel>` | `Workspace <sel>` |
| `TranscriptStream current show: x` / REPL `Transcript show: x` | `Transcript show: x`; `Logger` or `Console` in programs |
| `(Erlang beamtalk_interface) findClass: n` | `Beamtalk classNamed: n` |
| Test setUp swapping `TranscriptStream current:` | Assert on return values, or configure a Logger handler on `[beamtalk, user, transcript]` |
| `Transcript showLine: x` | `Transcript showCr: x` |
| `Beamtalk globals` | `Beamtalk classNamed:`, `Beamtalk allClasses`, `SystemNavigation …` |
| `Workspace globals` | `Workspace bindings` |
| `SystemNavigation default sendersOf: #x` | `SystemNavigation sendersOf: #x` |
| `self new` in a class method of an `Object subclass:` | A class-side API, or `Value subclass:` |

REPL users type what they typed before. beamtalk-exdura switches
`resolveWorkflowClassNames:` to `Beamtalk classNamed:` once Phase 1 ships.

## Amended ADRs

- **ADR 0010.**
  - Workspace-injected bindings are replaced by class-side facades.
  - Code outside a workspace can use `Beamtalk` and `Transcript`.
  - `Transcript` and `Beamtalk` are class names.
- **ADR 0013.** Its §4 direct dispatch for stateless class methods is
  implemented (Phase 0b).
- **ADR 0019.**
  - Class-variable singletons and their bootstrap wiring are removed.
  - `Transcript` has a class-side API in place of "`TranscriptStream
    show:` is a DNU".
- **ADR 0040.**
  - The `Beamtalk`/`Workspace` split stands, as classes rather than
    instances.
  - `Beamtalk globals` is removed; `Workspace globals` becomes `Workspace
    bindings`.
  - The resolution chain has no singleton layer.
- **ADR 0081.**
  - The resolver loses its singleton step.
  - `Workspace bindings` holds only `bind:as:` entries.
  - The "no class-side mirror" rationale no longer applies.
  - The `Session` design is unchanged.
- **ADR 0125.**
  - The `'Transcript'` process starts only alongside the REPL server.
  - `Beamtalk releaseInfo` resolves in compiled code.
  - `beamtalk_capability` gains `no_workspace` and `run_mode_no_compiler`,
    alongside `release_mode_no_compiler` and `release_mode_no_workspace`.
  - Compiler availability is recorded in `run` mode.
- **ADR 0126.** Consistent, with no change in substance. Facades are
  node-local like every class, and remote access follows the `Node`-handle
  pattern (§8).

## References
- Related issues: BT-3631 (this ADR); BT-3622 (`classNamed:` dynamic symbols);
  BT-3632 (Object-kind `new` rule, settled in §6); BT-3633 (node
  introspection moves to `Node`); BT-3634 (`Program exit:` outside run
  mode); BT-3635 (`File` handle ownership)
- Related ADRs:
  - [0010](0010-global-objects-and-singleton-dispatch.md) — global objects and singleton dispatch
  - [0013](0013-class-variables-class-methods-instantiation.md) — class methods and dispatch
  - [0019](0019-singleton-class-variables.md) — singleton class variables
  - [0040](0040-workspace-native-repl-commands.md) — `BeamtalkInterface`/`WorkspaceInterface`
  - [0058](0058-platform-security-model.md) — ambient authority
  - [0064](0064-runtime-logging-control-and-observability-api.md) — Logger
  - [0070](0070-package-namespaces-and-dependencies.md) — no per-file imports; protected names
  - [0081](0081-first-class-session-object.md) — Session and binding layers
  - [0083](0083-metaclass-aware-type-inference.md) — typing of `new`
  - [0124](0124-slots-late-assignment-definite-assignment.md) — `late classState: current`
  - [0125](0125-otp-releases-and-upgrade-compatibility.md) — modes and capability refusals
  - [0126](0126-distribution-location-transparent-actors.md) — `Node`, node-local class references
- Documentation: `docs/beamtalk-principles.md` §4; `docs/beamtalk-language-features.md`
  (*Workspace and Reflection API*, *Sessions and binding layers*, *Passing
  Blocks Through Class Methods*)
- External: Elixir `IEx.Helpers` and `Mix.Project`; IPython; Newspeak platform
  modules

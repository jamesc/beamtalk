# ADR 0129: Replace Injected `Beamtalk`/`Workspace`/`Transcript` Bindings with Class-Side Facades

## Status
Proposed (2026-09-25)

Amends ADR 0010 (workspace-injected bindings), ADR 0019 (class-variable
singletons as the canonical access path), ADR 0040 (singletons as a
Workspace-globals binding layer), ADR 0081 (the "no class-side mirror"
rationale) and ADR 0125 (how `Beamtalk releaseInfo` resolves on a release
node). See [§ Amended ADRs](#amended-adrs).

## Context

### Problem statement

`Beamtalk`, `Workspace` and `Transcript` look like class names but are not
classes. Each is a **binding name for a singleton instance** of another
class. The instances are `BeamtalkInterface`, `WorkspaceInterface` and
`TranscriptStream`. The workspace creates them at boot and parks them in a
`current` class variable. The REPL then resolves the short names to those
instances. Whether any of this works depends on **which boot context the
code runs in**.

The motivating failure is BT-3622. That issue let `classNamed:` take a
dynamic symbol. beamtalk-exdura then tried to replace its untyped FFI
workaround in `ExduraHttpServer>>resolveWorkflowClassNames:`, which is
`(Erlang beamtalk_interface) findClass: aName`, with a typed call. Every
spelling fails:

| Spelling | Result |
|---|---|
| `Beamtalk classNamed: aName` | `Beamtalk` is a REPL-injected binding. In a compiled package it emits an "Unresolved class `Beamtalk`" warning, then raises `class_not_found` at runtime. |
| `BeamtalkInterface classNamed: aName` | `classNamed:` is instance-side, so this is a DNU. The type checker infers `Dynamic` and gives no hard error. |
| `BeamtalkInterface new classNamed: aName` | "Object-kind class `BeamtalkInterface` cannot be instantiated with `new`". |
| `BeamtalkInterface current classNamed: aName` | Compiles cleanly and works in the REPL. Under `beamtalk test` it fails with `UndefinedObject does not understand 'classNamed:'`. |

The last row is the dangerous one. `current` is set only by
`beamtalk_workspace_bootstrap`, which `beamtalk_workspace_sup` starts in
`run`, `workspace` and `release` modes (ADR 0125). `beamtalk test` never starts
`beamtalk_workspace`: it only calls
`application:ensure_all_started(beamtalk_stdlib)` (`commands/test.rs`). So the
code is correct in one boot context and `nil` in another.

### The bug class: "works in the REPL, breaks elsewhere"

There are three independent resolution paths for the same identifier today.
They disagree:

| Where the code runs | What `Transcript show: x` compiles to | Outcome |
|---|---|---|
| A REPL expression (`workspace_mode`, REPL context) | `maps:find` in session locals, then `beamtalk_workspace:resolve_singleton_instance('Transcript')` (`dispatch_codegen.rs` `generate_binding_aware_class_send`) | Works |
| A method body of a class loaded into the REPL (`workspace_mode`, non-REPL context) | `beamtalk_class_registry:whereis_class('Transcript')`, with `undefined` → `nil` (`generate_workspace_class_send`) | **Silently evaluates to `nil` and prints nothing** |
| Batch compile (`beamtalk build`, `beamtalk test`) | `whereis_class('Transcript')` → `class_send(undefined, …)` | "Unresolved class" warning at compile time, then a `class_not_found` error at runtime |

Workarounds have spread through the codebase as a result:

- **Examples and BUnit tests avoid the short names entirely.** They write
  `TranscriptStream current`, `BeamtalkInterface current` and
  `WorkspaceInterface current`.
- **Tests that need a transcript spawn their own and swap it in.** They
  call `TranscriptStream current:` in `setUp`
  (`examples/getting-started/test/hello_test.bt`).
- **Reflection tests can only check that methods exist.**
  `stdlib/test/beamtalk_interface_test.bt` tests "class-level introspection
  … since the Beamtalk workspace singleton is not available in the BUnit
  test context". Live behaviour can only be tested through the REPL
  protocol e2e suite.
- **`beamtalk run` prints nothing from the project template.** The
  `beamtalk new` template's `Main.bt` does
  `TranscriptStream current show: "Hello from …"; cr.`, and the stream has
  no subscribers in `run` mode. `beamtalk_transcript_stream` buffers
  unsubscribed output: "When no subscribers: output goes to buffer only
  (no stdout)".

### Current mechanics

About 35 sites across the runtime and compiler exist only to make three
names resolve.

**Runtime config and bootstrap**
- `beamtalk_workspace_config` declares the actor singleton
  (`singletons/0`: `Transcript` → `TranscriptStream`) and the value
  singletons (`value_singletons/0`: `Beamtalk` → `BeamtalkInterface`,
  `Workspace` → `WorkspaceInterface`). It also provides `binding_names/0`
  and `binding_name_for_class/1`.
- `beamtalk_workspace_bootstrap` builds each instance and writes it into
  the class's `current` class variable.
  - The actor path monitors the Transcript pid and re-bootstraps on
    `'DOWN'`.
  - The value path (`bootstrap_value_singleton/3`) retries on
    `class_not_found`, up to 5 times at 200 ms intervals.

**Runtime name resolution** (`beamtalk_workspace_interface_primitives`)
- A five-tier `resolve_name/2`: locals, then `bind:as:` ETS, then
  singletons, then the class registry, then `undefined_variable`.
- `resolve_class_reference/2` checks singletons before the class
  registry.
- `resolve_singleton_instance/1`, `lookup_singleton/1`,
  `is_singleton_binding_name/1` and `handle_session_bindings/1`, which
  hard-codes the three names.
- `is_protected_name/1`, which hard-codes the three names so that
  `bind:as:` refuses them.

**Other runtime sites**
- `beamtalk_repl_eval` (`resolve_entry_receiver`, for run-entry).
- `beamtalk_repl_ops_dev`, which merges binding names into completions.
- `beamtalk_repl_json`, which rewrites "WorkspaceInterface does not
  understand" to say "Workspace".

**Compiler**
- `workspace_mode` threads through `beamtalk-core`, the codegen options,
  the compiler port and `beamtalk_compiler_server`.
- `dispatch_codegen.rs` has two send paths that depend on it: the
  binding-aware one and the workspace-class one. `core_erlang/mod.rs`
  resolves `ClassReference` differently in workspace mode.
- `structural_validators.rs` exempts `known_vars` (the three names,
  supplied by `beamtalk_repl_compiler:known_vars/1`) from "Unresolved
  class". It also warns when one shadows a class.

**Stdlib**
- Each of the three classes carries `late classState: current` plus
  `class current`, which returns `… | Nil`, and `class resetCurrent`.
  `BeamtalkInterface` and `TranscriptStream` also have `class current:`.
- `BeamtalkInterface` and `WorkspaceInterface` are "receiver-ignoring
  singleton facade[s]" (`beamtalk_interface.bt` header). Every instance
  method ignores `self` and calls a stateless Erlang function.

### Why the Smalltalk model doesn't fit

In Pharo, `Smalltalk` is the `SystemDictionary` of a single live image, and
`Transcript` is a global in it. Every line of code is compiled *inside* that
image, where the globals already exist, and runs there. That is why a global
lookup works.

Beamtalk has no image. It compiles ahead of time to BEAM modules, and those
modules run in several boot contexts, each starting a different set of OTP
applications:

- the REPL workspace;
- `beamtalk run` or an escript;
- `beamtalk test`;
- an OTP release, with or without a console.

A name that is injected by one of those contexts is, by construction,
missing in the others.

The rest of the stdlib already reflects this. `System getEnv:`,
`File readAll:`, `Console printLine:`, `Logger info:` and `Erlang` are all
class-side facades with no singleton, and they work identically everywhere.
`BeamtalkInterface` and `WorkspaceInterface` are the odd ones out.

The original case for injection has also lapsed:

- **ADR 0010 presented injection as "a stepping stone toward explicit
  module-level imports".** ADR 0070 then decided against per-file
  `import` statements, so injection is no longer a stepping stone to
  anything.
- **ADR 0010's case against class-level calls no longer holds.** It
  rejected them because they "bypassed dispatch, no DNU, no inheritance,
  no cascades". That was true of the raw module calls of the time.
  Beamtalk has since gained real metaclass dispatch (ADR 0013): class-side
  sends now get DNU, cascades, `respondsTo:` and `class`.
- **ADR 0081 rejected an injected `Session` binding for reasons that apply
  here too.** They were "Less Smalltalk than the factory approach", a
  shadowing footgun, and the implementation cost of injected keys.

### Two things called "globals", neither of which is Smalltalk's

Pharo has one `SystemDictionary` that holds classes *and* globals. The
compiler binds every free identifier in *all* code to an entry in it:
`Smalltalk at: #Foo put: 3` makes `Foo` visible to everything compiled
afterwards.

Beamtalk has two things called "globals", and neither plays that role:

- **`Beamtalk globals`** is a read-only `Dictionary` snapshot of the class
  registry (`handle_globals/0` in `beamtalk_interface.erl`). Nothing resolves
  names through it. It duplicates `classNamed:`/`allClasses` and
  `SystemNavigation allClasses`, which offers much richer queries
  (`actorClasses`, `usersOf:`, `extendersOf:`, …). Outside its own tests and
  docs it has no callers.
- **`Workspace globals`** is a live `BindingsView` (ADR 0081) over the
  `bind:as:` entries *plus the three singletons*. **Only REPL evals** resolve
  names through it. ADR 0081's lazy resolver checks, in order:
  1. `bind:as:` bindings;
  2. the singleton registry (`singletons/0` + `value_singletons/0`);
  3. the class registry.

Compiled code resolves a free capitalised name **only as a class**, which is
step 3. The missing step 2 is the root of the "works in the REPL, fails in
test" bug.

ADR 0081 has already set the precedent for fixing this. It gave `Session`
"no injected binding… resolved through the class registry, like `Counter` or
`Integer`". This ADR applies the same rule to `Beamtalk`, `Workspace` and
`Transcript`.

### Constraints

1. **Class-side sends to a stateful class cost a process hop.** A
   class-side method on a class that holds class variables runs in that
   class's gen_server (see *Passing Blocks Through Class Methods* in the
   language guide). That means process-local side effects are lost, and a
   re-entrant send to the same class raises `dispatch_error`. The codegen
   compiles a class-side send to a **direct function call** only when the
   class is **sealed and has no class variables**
   (`compute_direct_call_eligible`, `driver.rs`).
2. **Dependencies flow downward only.** `beamtalk_stdlib` depends on
   `beamtalk_runtime`, not on `beamtalk_workspace`. Under `beamtalk test`
   the workspace application isn't started, and its modules may not be on
   the code path at all.
3. **Refusals already have one home.** `beamtalk_capability`, a runtime
   leaf module (ADR 0125 §1.4–1.5), is *the* answer to "is this operation
   available on this node?". Every refusal is a `#beamtalk_error{}`.
4. **There is one user.** No external code depends on the current names
   except beamtalk-exdura, which has the same owner. A hard break with no
   shims is acceptable.

## Decision

### Guiding principle: Smalltalk's vocabulary, not the image's mechanics

Beamtalk is "Smalltalk-**like**, not Smalltalk-**compatible**"
(`docs/beamtalk-principles.md`). This ADR makes that concrete for system
objects, and records it as a general principle:

- **Keep what users type and read.** That means message syntax, cascades,
  blocks and the reflection selectors (`implementorsOf:`, `sendersOf:`,
  `allClasses`, `respondsTo:`, `printString`). It also means the familiar
  names: `Transcript show:`, `Beamtalk classNamed:`. A Pharo developer's
  habits should pay off here.
- **Drop the mechanisms that depend on a single live image.** Examples are
  a `SystemDictionary` of globals, singletons created at startup and reached
  through globals or class variables, and `Smalltalk at:put:` changing what
  compiled code means. In Pharo these work because every line is compiled
  and run inside one image. On BEAM the same module runs in the REPL, a
  compiled app, `beamtalk test` and a release, so each of these mechanisms
  becomes a "works here, not there" bug.
- **Tiebreaker:** does a Pharo user's habit produce a *working* program,
  and would an Erlang developer find the generated code unsurprising? When
  the two conflict, BEAM wins, and the Smalltalk *name* is kept where
  possible.

The same principle is added to `docs/beamtalk-principles.md`, so future
designs are checked against it.

### Summary

**The compiler and runtime inject no names.** `Beamtalk`, `Workspace` and
`Transcript` become ordinary, sealed, stateless stdlib classes with
**class-side** APIs. An identifier resolves the same way in every context:
REPL expression, method body, `beamtalk build`, `beamtalk test`,
`beamtalk run` and release.

| Name | Class | Wraps | Outside a workspace |
|---|---|---|---|
| `Beamtalk` | `Beamtalk` (renamed from `BeamtalkInterface`) | Class registry, reflection, `help:`, version, release info, logging config | Works everywhere (stateless) |
| `Workspace` | `Workspace` (renamed from `WorkspaceInterface`) | Sessions, bindings, ChangeLog, `load:`, flush, supervisors, tests | Raises `#beamtalk_error{kind: no_workspace}` |
| `Transcript` | `Transcript` (new) | The REPL's `TranscriptStream` process | Routes `show:`/`cr` through `Logger` |

### 1. How stdlib reaches "the system thing"

Today the stdlib reaches a system service in three different ways:

| Pattern | Classes | Behaviour |
|---|---|---|
| `class current` reads a class variable set at bootstrap; typed `\| Nil` | `BeamtalkInterface`, `WorkspaceInterface`, `TranscriptStream` | `nil` outside a workspace. This is the bug. |
| `class default => self new` | `SystemNavigation` | Stateless and works everywhere. It is also the only stdlib code that instantiates an Object-kind class (BT-3632). |
| `class default` / `of:` / `on:`, building a scoped value or handle | `ProcessNavigation`, `AnnouncementNavigation` | Instances carry a snapshot or an announcer. `default` chooses one scope among several. |
| `class current` asks the runtime at call time | `SystemAnnouncer`, `Session`, `Supervisor` | Returns the live entity, or `nil` when there legitimately is none. |

**The rule.** Every stdlib class that exposes a system service uses exactly
one of these three shapes:

1. **Class-side facade.** A service that is stateless and has a single
   scope is a `sealed` class with class-side methods only. It has no
   `classState:` and is never instantiated.
   - Examples: `Beamtalk`, `Workspace`, `Transcript`, `SystemNavigation`,
     `System`, `File`, `Console`, `Logger`.
2. **`default` and named factories.** Used only when *instances carry state
   or scope*, so that `default` picks one scope among several.
   - Examples: `ProcessNavigation default` / `system` / `from:` / `on:`,
     and `AnnouncementNavigation default` / `of:`.
3. **`current`.** Used only for a genuinely live runtime entity, **found by
   asking the runtime at call time**. It returns `| Nil` only when there
   legitimately is no such entity, for example `Session current` outside
   a REPL session.
   - Examples: `SystemAnnouncer`, `Session`, `Supervisor`.

**Forbidden:** a singleton held in a class variable that bootstrap code sets.
That is the image mechanism this ADR removes. It fails in every boot context
that doesn't run the bootstrap. It also races class loading, which is why
the value-singleton retry loop (`rebootstrap_value`, 200 ms × 5) exists. And
it forces `@expect type` workarounds on `class current` / `resetCurrent`,
because `hasField:`/`clearField:` infer as `Dynamic`.

### 1a. `SystemNavigation` moves class-side; BT-3632 is settled

- **`SystemNavigation` is rule 1.** It is stateless, and a node has exactly
  one class registry, so `default` has nothing to choose between. In
  Pharo, `default` picks the default *environment*. Beamtalk has no
  environments to pick from. So `default` is image plumbing, not
  vocabulary.
- **The selectors carry over unchanged.** `SystemNavigation implementorsOf:
  #foo`, `SystemNavigation sendersOf: #bar`, `SystemNavigation
  actorClasses`, and so on. `default` is removed.
- **A DNU on `default` points to the new form.** The DNU hint for
  `SystemNavigation default` names the class-side form, the same way the
  DNU hints for the removed `*Interface` names do (§6).
- **If package-scoped navigation ever arrives** (ADR 0070 namespaces), it
  becomes a rule-2 value class carrying a scope. It would have its own
  `default` and `forPackage:` factories, as `ProcessNavigation` does.

This settles **BT-3632** as its option 2:

- **Object-kind classes are never instantiable.** With `SystemNavigation`
  class-side, no stdlib code instantiates an Object-kind class.
- **The validator stops depending on how the receiver is spelled.**
  `check_actor_new_usage` / `object_kind_new_error` in
  `class_validators.rs` also rejects `self new` / `super new` inside a
  class method of an Object-kind class, not just `Foo new`.
- **The hint stays accurate.** It keeps "Object subclasses are not
  instantiable", and adds: "for a stateless service, use class-side
  methods; for data, use `Value subclass:`".
- **Rule-2 classes are unaffected.** `AnnouncementNavigation` creates its
  handles through FFI (`navigationFor:`), not `new`, and `ProcessNavigation`
  is a `Value`.

### 1b. Rules every facade follows

- **Sealed, stateless, class-side only.** A facade is declared
  `sealed typed Object subclass: …`, with no `classState:` and no
  instance-side API. That makes every static send to it eligible for the
  direct-call path (Constraint 1). `Beamtalk classNamed: x` therefore
  compiles to a plain function call in the caller's process: there is no
  class gen_server hop and no re-entrancy hazard.
- **No `new` and no `current`.** The class *is* the object. It is still
  first-class: `Beamtalk class`, `Workspace respondsTo: #load:`, and
  `x := Beamtalk. x version` all work, and the last goes through ordinary
  dynamic class dispatch.
- **State lives in Erlang, not in class variables.** That means the
  runtime class registry, the logger configuration, the workspace
  supervisor tree and the registered `'Transcript'` process.
- **Typed signatures.** The type checker sees real metaclass methods.
  `Beamtalk classNamed:` is `Class | Nil`, as the method itself declares.
  There is no longer a `| Nil` from `current` to guard against.
- **Backing modules use the same pattern as `System`/`File`/`Console`.**
  Where the backing Erlang lives in a `beamtalk_stdlib`/`beamtalk_runtime`
  module, the facade is `native:` and uses `self delegate`. The
  "receiver-ignoring instance FFI" comment in `beamtalk_interface.bt`
  disappears along with the thing it apologises for.

### 2. `Beamtalk`: system reflection, everywhere

`BeamtalkInterface` is renamed `Beamtalk`. Every current instance method
except `globals` becomes a `class sealed` method with the same selector and
type:

- `allClasses`, `classNamed:`;
- `help:`, `help:selector:`, `erlangHelp:`, `erlangHelp:selector:`;
- `version`, `releaseInfo`, `shapeManifest`;
- `logLevel`, `logLevel:`, `logFormat`, `logFormat:`, `debugTargets`,
  `enableDebug:`, `disableDebug:`, `activeDebugTargets`, `disableAllDebug`,
  `loggerInfo`.

The two type aliases, `LogLevel` and `LogFormat`, move with the class.

None of these needs a workspace. The class registry and logger belong to
`beamtalk_runtime`, which every boot context starts.

**`Beamtalk globals` is removed.**
- **What replaces it:**
  - `Beamtalk classNamed:` for lookup by name;
  - `Beamtalk allClasses` for enumeration;
  - `SystemNavigation` for anything richer.
- **Why remove it:**
  - The name leads Smalltalkers to read it as the global scope, which it
    is not.
  - Nothing resolves through it.
  - It has no callers outside its own tests and docs.
- **Effect on the word "globals":** it is left with one meaning, the REPL
  namespace (renamed `Workspace bindings` in §5).
- **Erlang side:** `beamtalk_interface:globals/0` and `handle_globals/0`
  go with it.

```beamtalk
// exdura: ExduraHttpServer>>resolveWorkflowClassNames:, compiled package
resolveWorkflowClassNames: names :: List(Symbol) -> List(Class) =>
  names collect: [:aName |
    (Beamtalk classNamed: aName) ifNil: [
      self error: "unknown workflow class " , aName printString]]
```

The same line now works in the REPL, in `beamtalk test` and in a release:

```beamtalk
Beamtalk classNamed: #Integer      // => Integer
Beamtalk classNamed: #NoSuchThing  // => nil
Beamtalk version                   // => "0.x.y"
```

### 3. `Workspace`: project operations, only where a workspace runs

`WorkspaceInterface` is renamed `Workspace`. Every current instance method
becomes a `class sealed` method with the same selector and type:

- `load:`, `newClass:at:`, `moveClass:to:`;
- `classes`, `testClasses`, `bindings` (renamed from `globals`, see §5);
- `currentSession`, `sessions`;
- `actors`, `actorAt:`, `actorsOf:`, `processes`, `nodes`;
- `bind:as:`, `unbind:`;
- `changes`, `sync`, `recheckImage`;
- `flush`, `flush:`, `flush:confirmDestructive:`,
  `flushIncludingDestructive`, `autoflush`, `autoflush:`;
- `test`, `test:`;
- `supervisor`, `startSupervisor:`, `stopSupervisor:`, `supervisors`;
- `dependencies`.

**A workspace is running** when `beamtalk_workspace_sup` has started. That
is true in `run`, `workspace` and `release` modes (ADR 0125), and it is the
point where `beamtalk_capability:set/1` records the node's mode.

**Outside a workspace**, for example under `beamtalk test` or on a bare
runtime, every `Workspace` method raises one structured error. It never
raises a DNU on `nil`:

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

A BUnit test can assert on it like any other structured error:

```beamtalk
testWorkspaceUnavailableUnderBUnit =>
  self should: [Workspace classes] raise: #no_workspace
```

**One check for availability.** The check is a new function in
`beamtalk_capability`, the runtime leaf that already owns the ADR 0125 §1.5
table. For example `require_workspace(Selector)`, which answers `ok` or the
`no_workspace` error.

- **It distinguishes "never recorded" from "recorded".** Today
  `beamtalk_capability:current/0` treats a node with nothing recorded as a
  `workspace` node. That permissive default stays for `classify/1`, which
  bare-runtime unit tests rely on. The new function instead answers "has a
  workspace supervisor recorded capabilities on this node?".
- **The facade calls it before touching any module in
  `beamtalk_workspace`.** Under `beamtalk test` that application is not
  started, and its code may not be on the path (Constraint 2).
- **Release-mode refusals don't change.** They are still raised by
  `beamtalk_capability:check/*` after `require_workspace` passes. So a
  `Workspace load:` on a release node still says
  `release_mode_no_compiler`, never `no_workspace`.

### 4. `Transcript`: the REPL's shared log, and a day-0 convenience

`Transcript` exists **for newcomers and REPL use only**. It is the name a
Smalltalker types first, and it gives a first REPL session somewhere visible
for output to go. It is **not** an output API for programs:

- **`Logger`** is the output API for programs and libraries (ADR 0064).
- **`Console`** is plain stdout/stderr, for CLI apps and scripts.

The language guide, the class comment and the `beamtalk new` template all
say this. `Transcript` has no stream protocol, no redirection or capture
API, and no instance to pass around.

`Transcript` is a new `sealed typed Object subclass: Transcript` with this
class-side API:

| Selector | In an interactive workspace | Elsewhere |
|---|---|---|
| `show: value` | Appends `value printString` (a `String` as-is) to the workspace's `TranscriptStream` | `Logger` event at `notice` level with the text, domain `[beamtalk, user, transcript]` |
| `cr` | Appends a newline | No-op (each Logger event is already a line) |
| `showCr: value` | `show:` then `cr` | One Logger event |
| `recent` | The stream's buffered lines | `no_workspace` error |
| `clear` | Empties the buffer | `no_workspace` error |

**Interactive workspace** means that the node runs a REPL server: `workspace`
mode, or a `release` with its opt-in console (ADR 0125). Only in those cases
is there anyone to subscribe, whether the REPL, the WebSocket push or
`beamtalk workspace transcript`. `beamtalk_workspace_sup` therefore starts the
registered `'Transcript'` `TranscriptStream` process **only** when it starts
the REPL server. It no longer starts it in `run` mode, where the process
buffered output that nobody could read. The facade routes on
`whereis('Transcript')`. That is one check, and it is correct by
construction.

Choosing Logger's `notice` level is deliberate. It is OTP's default primary
level, so `Transcript show:` output is visible by default under `beamtalk run`
and in releases. Anyone who wants to capture, silence or redirect it uses the
same `Logger` handler and filter configuration as any other log output. The
`[beamtalk, user, transcript]` domain lets them filter it specifically, with
no Transcript-specific machinery.

```beamtalk
// REPL (interactive workspace)
Transcript show: "Hello"; cr; show: "World"   // appears in the REPL transcript pane
Transcript recent                             // => #("Hello", "World")
```

```beamtalk
// compiled code under `beamtalk run`: no REPL server
Transcript showCr: "starting"    // logged at notice: "starting"
Transcript recent                // raises #beamtalk_error{kind: no_workspace}
```

`TranscriptStream` stays as the actor class that implements the REPL's
stream. It loses `current`, `current:` and `resetCurrent`, and it is no
longer something user code reaches for. `Object>>show:` and `Object>>showCr:`
delegate to `Transcript`. Today they are nil-safe no-ops when there is no
stream; after this change they reach the Logger fallback, so their output is
never silently dropped.

`show:` followed by `show:` without a `cr` produces two Logger events,
not one line. The fallback is deliberately stateless: there is no
per-process line buffer, because `Transcript` is not the API for output
formatting.

### 5. Name resolution after this ADR

In a REPL expression a free identifier resolves through:

1. session locals;
2. `Workspace bind:as:` entries;
3. the class registry.

The singleton tier is gone (it amends the ADR 0081 / ADR 0040 chain). Method
bodies and batch-compiled code use only the class registry, as they do for
every other class. So `Beamtalk`, `Workspace` and `Transcript` resolve the
same way as `Integer`. This is the rule ADR 0081 already applied to
`Session`.

**`Workspace globals` becomes `Workspace bindings`.** It now holds only the
user's `bind:as:` entries. The name "globals" would keep implying Smalltalk's
semantics, "visible to all code", and that implication is the bug this ADR
fixes. The entries are visible only to REPL evals.

`bindings` fits what the view actually is:
- it returns a `BindingsView`, the class name ADR 0081 already chose;
- it pairs with `bind:as:` and `unbind:`.

It also parallels the existing `Session current bindings`. Session locals and
workspace bindings are two tiers of the same REPL namespace, and now they have
matching names. There is no `globals` alias, per §6. The Erlang primitive
`beamtalk_session_primitives:globalsView` is renamed with the selector, and
the `Session` doc comments that point at `Workspace globals` are updated.

```beamtalk
Workspace bind: 42 as: #answer
Workspace bindings                // => a BindingsView {#answer => 42}
Workspace bindings at: #answer    // => 42
```

The hard-coded protected-name list (`is_protected_name/1`) goes away. The
three names are protected the same way every stdlib class name is: `bind:as:`
and `Workspace bindings at:put:` refuse to shadow a stdlib class with a
`name_conflict` error. The existing `check_workspace_shadows` warning and the
`known_vars` exemption have nothing left to cover, and are deleted.

### 6. Old names are removed outright, with no shims

With one user, there is no deprecation period:

- **`BeamtalkInterface`, `WorkspaceInterface`** — removed; their contents move
  to `Beamtalk` and `Workspace`.
- **`TranscriptStream current` / `current:` / `resetCurrent`** — removed.
- **`BeamtalkInterface current`, `WorkspaceInterface current`** — removed.
  Code gets the standard `class_not_found` / DNU errors, and each hint names
  the replacement.

The rename lands in the same change as the in-repo sweep and the
beamtalk-exdura update (see [§ Migration Path](#migration-path)).

### Misuse and error examples

```beamtalk
Beamtalk new                  // Error: Beamtalk is a sealed class-side facade and cannot be instantiated
BeamtalkInterface current     // Error: class_not_found: BeamtalkInterface. Hint: renamed to Beamtalk (ADR 0129)
Workspace changes             // under beamtalk test → #beamtalk_error{kind: no_workspace, selector: 'changes', …}
Transcript := 3               // REPL: name_conflict — Transcript is a stdlib class and cannot be shadowed
Beamtalk classNamd: #Foo      // DNU on the Beamtalk class, with "did you mean classNamed:?"
```

## Prior Art

| Language | How system-level objects are reached | Takeaway |
|---|---|---|
| **Pharo / Squeak** | `Smalltalk` (a `SystemDictionary`) and `Transcript` (a `ThreadSafeTranscript`) are globals in the image's `Smalltalk globals`. All code is compiled inside the image. | This is the model we used. It works because there is one image and every compilation happens inside it. Beamtalk has neither. |
| **GNU Smalltalk** | The same globals, plus a scripting mode where `Transcript` buffers and flushes to stdout. | Shows that even Smalltalk needed a "no interactive UI" fallback for `Transcript`. Ours is `Logger`. |
| **Newspeak** | No globals. The platform object (the transcript, the system mirror) is passed to the module constructor. | This is the most principled answer: explicit capabilities. It needs a module-parameter system that Beamtalk does not have, and ADR 0010 already listed it as "not planned". |
| **Erlang** | Module functions: `code:which/1`, `logger:notice/1`, `io:format/1`. There is no singleton object, and they work in every node type. | This is exactly what a sealed, stateless class-side facade compiles to. |
| **Elixir** | `Code`, `Logger`, `IO` and `System` are modules that work everywhere. The IEx-only helpers (`h/1`, `recompile/0`) are *imported only into the shell*. `Mix.Project` raises clearly when Mix isn't running. | The `IEx.Helpers` split is the injected-bindings pattern, and Elixir keeps it deliberately out of compiled code. `Mix.Project` raising when Mix isn't running is the `Workspace`→`no_workspace` pattern. |
| **Gleam** | `io.println` and `logger` via Erlang. No ambient objects at all. | Confirms that a BEAM language can do without globals. |
| **Livebook / Kino** | `Kino` functions are ordinary module calls. Outside a Livebook runtime they degrade or no-op instead of vanishing. | The "facade that knows what context it is in" is the same design as `Transcript` routing to `Logger`. |
| **Pony** | `env.out` is a capability passed to `Main`. | Like Newspeak: explicit, but it needs threading everywhere. ADR 0058 already rejected capability restriction. |

**What we adopt:** the Erlang/Elixir model. Ambient, stateless module-level
APIs that behave the same everywhere, and raise a clear error where a
context-bound service is absent (Elixir's `Mix.Project`).

**What we keep from Smalltalk:** the *names* users type and message-send
syntax, including cascades.

**What we reject:** image-bound globals (Pharo) and passed capabilities
(Newspeak, Pony).

## User Impact

**Newcomer.**
- Nothing to unlearn. `Transcript show: "hi"` and `Beamtalk help: Integer`
  work as before in the REPL, and now also in the first file they compile
  and run.
- The first program `beamtalk new` generates prints under `beamtalk run`.
  Today it prints nothing.
- The error for `Workspace …` in a test names the problem and the fix,
  instead of `UndefinedObject does not understand`.
- Tab completion finds the three names as ordinary classes, with their
  class-side selectors.

**Smalltalk developer.**
- The names they expect are all there, and cascades still work.
- In Pharo, `Transcript` is an instance of `ThreadSafeTranscript` and
  `Smalltalk` is an instance of `SystemDictionary`. In Beamtalk they are
  classes.
- The class is a first-class object, so the purity loss is small. It
  answers `class`, `respondsTo:` and `printString`, and it can be stored
  in a variable and sent messages dynamically.
- What they lose is swapping in a different instance, such as a mock
  transcript. That pattern appears in the codebase only as a workaround
  for the missing singleton.

**Erlang/Elixir developer.**
- `Beamtalk classNamed: x` compiles to a direct call to one exported
  function. That is the most predictable thing BEAM can do.
- From Erlang, the facade is callable as a plain module function (the
  class's module). No gen_server state is involved.
- `Transcript` output outside the REPL is an ordinary `logger` event with
  a domain, so existing handler and filter configuration applies to it.

**Production operator.**
- **No process hop, less bootstrap.** Hot reloading and observability
  behave like any other stateless module: there is no class-process
  bottleneck, and the bootstrap no longer has a retry loop or `current`
  slots to re-wire after a crash.
- **One fewer process in `run` mode.** Run mode no longer starts a
  `Transcript` process whose unread buffer grows to 1000 entries.
- **Transcript output from a release is visible.** It lands in the
  release's normal log.
- **`Beamtalk releaseInfo` works without eval tricks.** On a release node
  it now resolves in compiled code as well as through `eval`
  (ADR 0125 §2.4).

**Tooling developer.**
- The compiler loses its special cases: the `known_vars` exemption, two
  codegen send paths, and part of `workspace_mode`.
- The LSP, MCP and language service see three ordinary classes. Hover,
  go-to-definition and completion work with no runtime-supplied binding
  list.
- The Rust tools that generate `Workspace changes`, `Workspace flush` and
  `Beamtalk help:` source strings keep working unchanged. Those strings
  are now plain class sends.

## Steelman Analysis

### A. Status quo
- 🎩 **Smalltalk purist**: "`Transcript` is an *instance*, exactly as in
  Pharo. It can be passed, stored and substituted like any object. Making
  it a class is a category error."
- 🧑‍💻 **Newcomer**: "It already works in the REPL, which is where I live."
- ⚙️ **BEAM veteran**: "It's already built. Don't churn 35 call sites for
  aesthetics."
- **Why it still loses:** "works in the REPL" is exactly the problem. The
  same line fails in tests and silently does nothing in loaded method
  bodies. Purity isn't worth that, and the instance was never substitutable
  in practice: tests swap a *class variable*, not an argument.

### B. Keep injection, but set `current` eagerly (at stdlib/runtime startup)
- 🏭 **Operator**: "It's the smallest change that fixes exdura: set
  `BeamtalkInterface current` when `beamtalk_runtime` starts, and
  `BeamtalkInterface current classNamed:` works everywhere."
- 🎩 **Smalltalk purist**: "It keeps the instance model."
- **Why it still loses:**
  - It fixes only `Beamtalk`. `Workspace` has no instance to set outside a
    workspace, so it stays `nil`.
  - The REPL-only spelling (`Beamtalk …`) still differs from the compiled
    spelling (`BeamtalkInterface current …`).
  - `current` stays typed `| Nil`.
  - All the injection machinery remains.

### C. Extend the injected bindings into compiled code
- 🧑‍💻 **Newcomer**: "One spelling everywhere, and nothing renamed."
- ⚙️ **BEAM veteran**: "The resolver already exists; just call
  `resolve_singleton_instance` from batch codegen too."
- **Why it still loses:**
  - It makes "globals in disguise" (ADR 0010's own steelman) permanent in
    the compiler.
  - Every send becomes a runtime lookup.
  - Under `beamtalk test` the lookup still finds nothing, so the nil moves
    rather than disappearing.
  - The type checker still sees a `Dynamic` binding rather than a class.

### E. Real Smalltalk-style globals: ADR 0081's resolver extended into compiled code
Concretely, compiled code resolves a free capitalised name through the same
three steps REPL evals use: `bind:as:`, then the singleton registry, then the
class registry. Every boot context, including `beamtalk test`, populates the
`Workspace globals` layer.

- 🎩 **Smalltalk purist**: "This is the faithful model. `Smalltalk at:
  #Transcript` holds the object, compiled code resolves free identifiers
  through it, and every boot path populates it. Users can add their own
  globals."
- 🎨 **Language designer**: "One uniform mechanism for classes *and*
  well-known objects, instead of two."
- **Why it still loses:**
  - **Lookup cost.** Every reference becomes a runtime dictionary lookup,
    where today it is a static call.
  - **Weaker typing.** The type checker can only see `Dynamic` for a
    global.
  - **Every boot path must populate the dictionary.** Missing one
    recreates today's bug.
  - **It relies on the image.** In Smalltalk this works because code is
    compiled inside the one image where the globals already exist.
    Beamtalk compiles ahead of time to modules that run in several boot
    contexts.
  - **It reintroduces what ADR 0010 and ADR 0058 steered away from.**
    ADR 0010 argued against "language-level globals" as a category, and
    ADR 0058 against ambient mutable authority. Replacing a global in a
    dictionary is ambient mutable state.

### D. Class-side facades (chosen)
- 🧑‍💻 **Newcomer**: "The names I see in docs are real classes. They work in
  every file, and completion knows them."
- 🎩 **Smalltalk purist** (grudgingly): "The class is still an object, and
  message-sends and cascades are intact. `Smalltalk` in Pharo 12 is a thin
  facade anyway."
- ⚙️ **BEAM veteran**: "It compiles to a direct remote call. It's `code:`
  and `logger:` with nicer syntax."
- 🏭 **Operator**: "No bootstrap retry loop, no `current` slots to go nil
  after a class-process restart, one fewer process in `run` mode."
- 🎨 **Language designer**: "It removes a whole resolution tier.
  Identifier resolution becomes locals → bindings → classes, everywhere."

### Tension points
- **Purists prefer A/E, pragmatists and tooling prefer D.** Smalltalk
  purists value instance substitutability. The only real use of it in the
  codebase is a test workaround, and the design moves that capture to
  Logger configuration.
- **Transcript's fallback.** The alternatives were to start the transcript
  process everywhere (a buffer nobody reads in tests), to print to stdout
  (which pollutes test output and duplicates `Console`), or to raise
  (hostile to a beginner's first compiled program). Logger won because it
  is already configurable, and because `Transcript` isn't the output API.

## Alternatives Considered

### A. Status quo
This is described above. It leaves the silent-`nil` bug in loaded method
bodies, keeps exdura on untyped FFI, and confines live facade testing to
the REPL e2e suite. Rejected.

### B. Eager/lazy `current` initialisation, keeping injection
Set `BeamtalkInterface current` from `beamtalk_runtime`, or initialise it
lazily on first read. This fixes exdura's one call. It does nothing for
`Workspace` or `Transcript`, keeps two spellings per concept, and keeps all
of the §Context machinery. Rejected as a partial fix.

### C. Extend injected globals into compiled code
Have batch codegen emit the REPL's binding-aware send. This makes a
runtime-resolved, `Dynamic`-typed name a permanent language feature, and it
still yields `nil` where no workspace ran. Rejected.

### D. Class-side facades
Chosen; this is the Decision above.

### E. Real Smalltalk-style globals (ADR 0081's resolver in compiled code)
Extend the REPL's lazy resolver (`bind:as:`, then singletons, then class
registry) to every free capitalised identifier in compiled code. Every boot
context, including `beamtalk test`, bare runtimes and releases, would
populate the `Workspace globals` layer.

This is the model that is faithful to Smalltalk. It is rejected:

- **It fits the image model, which Beamtalk lacks.** Smalltalk can do this
  because code is compiled inside the one live image where the globals
  already exist. Beamtalk compiles ahead of time to modules that run in
  several boot contexts.
- **Its costs fall on every class reference.** Each one becomes a runtime
  lookup, and it cannot use the direct-call path.
- **The type checker loses precision.** It could see only `Dynamic` for any
  name a binding might shadow.
- **It depends on every boot path populating the layer.** That is exactly
  the property that failed here.
- **User `bind:as:` entries would leak into compiled code.** A REPL session
  could change what a compiled method's `Foo` means. That destroys the
  "compiled code means the same thing everywhere" property this ADR is
  restoring.

### F. Keep the old class names; add class-side mirrors and short-name aliases
Keep `BeamtalkInterface` and friends, give them class-side methods, and add
a class-alias mechanism so `Beamtalk` names the same class. Rejected:

- Beamtalk has no class aliases.
- ADR 0019's Alternative A already rejected names that "look like class
  names but aren't" as a learnability trap.
- It leaves two names per concept with no user for the long one.

### Transcript sub-alternatives
- **Start the transcript process in every mode, including `beamtalk test`.**
  This makes output a buffer nobody reads, and needs a new supervisor
  placement in the runtime. Rejected.
- **Fall back to stdout.** This duplicates `Console`, prints into test
  output, and can't be silenced by configuration. Rejected.
- **Raise `no_workspace` from `show:`.** This is correct but hostile to the
  day-0 use `Transcript` exists for. Rejected for `show:`/`cr`/`showCr:`,
  and kept for the workspace-only `recent`/`clear`.

## Consequences

### Positive
- **Identifiers resolve one way everywhere.** The "works in the REPL, breaks
  in test/build/loaded method" bug class goes away for these names, and no
  other names are injected.
- **exdura drops its FFI.** It, and any package, can use typed
  `Beamtalk classNamed:`.
- **Real signatures reach the type checker.** There are no `| Nil` guards,
  and a misspelled selector is a class-side DNU the checker can flag.
- **Live facade tests can run in BUnit.** `stdlib/test` can test
  `Beamtalk` behaviour directly, and can test `Workspace`'s `no_workspace`
  refusal.
- **Large deletions.** The following all go:
  - the singleton and value-singleton config and bootstrap, including the
    retry loop;
  - the three `current` slots;
  - the singleton resolution tier, protected-name list and DNU-renaming
    shim;
  - the `known_vars` exemption and shadow warning;
  - two codegen send paths.
- **`beamtalk run` shows Transcript output.** It goes through Logger, and
  run mode stops starting a transcript process nobody can read.
- **One availability answer.** `beamtalk_capability` answers both "is
  there a workspace?" and "is this op allowed here?".
- **"Globals" stops meaning two things.** `Beamtalk globals` is gone, and
  the REPL namespace is called `Workspace bindings`. No API suggests that
  there is a Smalltalk-style global scope that compiled code sees.

### Negative
- **The instance can't be substituted.** You can't substitute a different
  `Transcript` or `Beamtalk` instance, for example a mock. Tests that
  captured Transcript output must assert on return values or use Logger
  handler configuration instead.
- **Dynamic sends to `Workspace` still hop into its class process.** A
  send through a variable, such as `ws := Workspace. ws test`, uses the
  class gen_server, as it does for any class. A block passed into such a
  send that messages `Workspace` again raises `dispatch_error`. Static
  sends, which are what anyone writes, avoid this.
- **The Logger fallback is not line-exact.** `show: "a"; show: "b"; cr`
  logs two events, "a" and "b", rather than one line "ab".
- **It is a breaking rename.** Every in-repo use of the old class names and
  `current` must change in one sweep, and so must exdura.
- **Behaviour changes in `run` mode.** `Transcript` output that used to
  vanish into a buffer now appears in the log.
- **`Workspace globals` → `Workspace bindings` is a high-churn rename.**
  The selector appears in 23 files, about 76 doc mentions and 22 REPL e2e
  cases.

### Neutral
- `TranscriptStream` remains, as an implementation class for the REPL's
  stream.
- `Logger` and `Console` are unchanged. The docs now point to them
  explicitly as the output APIs, with `Transcript` described as a REPL and
  beginner convenience. `Transcript` may be deprecated later; that is not
  decided here.
- ADR 0048 (class-side method syntax, deferred) gains about 60 more
  `class sealed` methods to migrate if that syntax ever changes.
- `ProcessNavigation default` and `AnnouncementNavigation default` are
  unchanged. They are rule-2 scope factories over stateful values, and they
  already conform.
- The class names `Beamtalk` and `Workspace` compile to modules
  `bt@stdlib@beamtalk` and `bt@stdlib@workspace`. These don't collide with
  the Erlang `beamtalk_*` application modules.

## Implementation

The rename and the injection removal must land together, because the old
names disappear. The phases below are therefore stages within one epic. The
branch is green after each phase except where noted.

**Phase 1: `Beamtalk` facade** (stdlib, S–M)
- Rename `stdlib/src/beamtalk_interface.bt` to `beamtalk.bt`. The class
  becomes `sealed typed Object subclass: Beamtalk`, with all methods
  `class sealed` and no `classState`.
- Delete `current`, `current:` and `resetCurrent`.
- Delete `globals`, together with `beamtalk_interface:globals/0` and
  `handle_globals/0`, and their tests in `beamtalk_interface_tests.erl` and
  `beamtalk_stdlib_tests.erl`.
- Make the backing `beamtalk_interface.erl` the `native:` module, and
  adjust its export names to the class-side delegate convention.
- Add a codegen snapshot test asserting that
  `Beamtalk classNamed: #Integer` emits a direct call in both batch and
  REPL compiles (Constraint 1).
- Convert `stdlib/test/beamtalk_interface_test.bt` from existence checks to
  live behaviour tests.

**Phase 2: `Workspace` facade** (stdlib + runtime, M)
- Add `beamtalk_capability:require_workspace/1` and the `no_workspace`
  error kind, with EUnit coverage for "never recorded" and "recorded" nodes.
- Rename `workspace_interface.bt` to `workspace.bt`, as a sealed class-side
  facade. Each method guards with `require_workspace` before calling
  `beamtalk_workspace_interface_primitives`.
- Rename `globals` to `bindings`, and the Erlang
  `beamtalk_session_primitives:globalsView` primitive to match. Update the
  `Session` doc comments.
- Add a BUnit test for the `no_workspace` refusal.
- Update the REPL-protocol e2e cases.

**Phase 3: `Transcript` facade** (stdlib + runtime, M)
- Add `stdlib/src/transcript.bt`: `show:`, `cr`, `showCr:`, `recent` and
  `clear`, with routing on `whereis('Transcript')` and Logger at `notice`
  in the `[beamtalk, user, transcript]` domain.
- Strip `current`, `current:` and `resetCurrent` from `TranscriptStream`.
- Repoint `Object>>show:`/`showCr:` at `Transcript`.
- In `beamtalk_workspace_sup`, start the `'Transcript'` process only
  alongside the REPL server, and update the ADR 0125 mode table.

**Phase 4: remove injection** (compiler + runtime, L)
- **Runtime workspace app:**
  - In `beamtalk_workspace_config`, delete `value_singletons/0`,
    `binding_names/0` and `binding_name_for_class/1`. Keep the
    Transcript child spec in its REPL-only form.
  - In `beamtalk_workspace_bootstrap`, delete the value path, the retry
    loop and the `current` wiring. If nothing remains beyond the
    `bind:as:` ETS table, fold that into its owner.
  - In `beamtalk_workspace_interface_primitives`, delete the singleton
    tier of `resolve_name`/`resolve_class_reference`, plus
    `resolve_singleton_instance`, `lookup_singleton`,
    `is_singleton_binding_name`, `handle_session_bindings`'s hard-coded
    names and `is_protected_name`. Generalise the conflict check to
    "stdlib class name".
  - `beamtalk_workspace`: delete the `resolve_singleton_instance` export.
  - `beamtalk_repl_eval`: delete the singleton branch of
    `resolve_entry_receiver`.
  - `beamtalk_repl_compiler`: delete `known_vars/1`.
  - `beamtalk_repl_ops_dev`: remove the binding-name completions.
  - `beamtalk_repl_json`: delete the DNU renaming.
  - `beamtalk_session_primitives`: update `globalsView`.
- **Codegen:**
  - In `dispatch_codegen.rs`, delete `generate_binding_aware_class_send`
    and `generate_workspace_class_send`, and route through
    `generate_class_method_call`.
  - In `core_erlang/mod.rs`, delete the workspace-mode `ClassReference`
    branch.
  - Remove `workspace_mode` wherever its only remaining effect was
    singleton resolution. Keep it only if REPL-local variable lookup still
    needs it.
- **Semantic analysis:** in `structural_validators.rs`, delete the
  `known_vars` exemption and `check_workspace_shadows`, plus the
  compiler-port plumbing that feeds them.
- **Tests:** update `beamtalk_workspace_config_tests`,
  `beamtalk_repl_compiler_tests`, `beamtalk_workspace_sup_tests`, the
  primitives load tests and the structural-validator tests.

**Phase 4b: `SystemNavigation` class-side + BT-3632** (S–M, independent of Phases 1–4)
- Make every `SystemNavigation` query `class sealed`, and delete `default`.
- Repoint about 218 `SystemNavigation default` references, most of them in
  docs.
- In `class_validators.rs`, make the Object-kind `new` check independent of
  the receiver: `self new` / `super new` in a class method of an
  Object-kind class is rejected. Fix the hint text.
- Add validator tests for both `Foo new` and `self new`.
- Update the `SystemNavigation` comment that cites ADR 0083's implicit
  `new`.

**Phase 5: sweep docs, examples and templates** (M)
- **`docs/beamtalk-language-features.md`:**
  - Rewrite *Workspace and Reflection API* around the class-side facades,
    and complete the `Beamtalk` table with the logging, debug and release
    selectors.
  - Rewrite *Sessions and binding layers* for the three-tier chain.
  - Add a short *Transcript* section with the REPL/Logger split and the
    "use Logger or Console in programs" guidance.
  - Drop `Beamtalk globals` from the reflection table (around line 4493).
- Update every doc that describes the singletons or `Beamtalk globals` as a
  resolution layer:
  - `docs/beamtalk-tooling.md`, around line 709 and the resolution chain at
    lines 765–773;
  - `docs/learning/22-workspace-globals.md`, which is renamed for
    `Workspace bindings`;
  - `docs/stdlib-implementation-status.md`;
  - `docs/beamtalk-ddd-model.md`, including the stale description (around
    line 1065) of a nonexistent `BeamtalkSystemDictionary` /
    `stdlib/src/SystemDictionary.bt`;
  - `docs/development/testing-strategy.md`.
- Replace the 20 uses of the non-existent `Transcript showLine:` with
  `showCr:`.
- Change the `beamtalk new` `Main.bt` template to `Console printLine:`,
  and update its test.
- Fix `examples/` and stdlib doc comments: `TranscriptStream current`,
  `WorkspaceInterface current`, and the `hello_test.bt`-style capture
  setUp.
- Update `docs/development/surface-parity.md` if any surface text
  changes. The `crates/beamtalk-cli/templates/agents.md`, MCP and LSP
  tool strings need no change, because they use `Workspace …`/`Beamtalk
  …`, which remain valid.

**Single source of truth.**
- `beamtalk_capability` owns "is a workspace running?". Nothing else tests
  `whereis(beamtalk_workspace_sup)`.
- The Transcript routing decision is the facade's alone.
- No Rust/Erlang name list survives. Name knowledge now lives only in the
  class registry, which is generated from stdlib sources, so this ADR
  needs no cross-boundary conformance fixture.

## Migration Path

There are no shims. Everything moves in one change:

| Before | After |
|---|---|
| `BeamtalkInterface current <sel>` / REPL `Beamtalk <sel>` | `Beamtalk <sel>` |
| `WorkspaceInterface current <sel>` / REPL `Workspace <sel>` | `Workspace <sel>` |
| `TranscriptStream current show: x` / REPL `Transcript show: x` | `Transcript show: x`, or `Logger`/`Console` in programs |
| `(Erlang beamtalk_interface) findClass: n` (exdura) | `Beamtalk classNamed: n` |
| Test setUp swapping `TranscriptStream current:` | Assert on return values, or configure a Logger handler on domain `[beamtalk, user, transcript]` |
| `Transcript showLine: x` (docs only; never existed) | `Transcript showCr: x` |
| `Beamtalk globals` | `Beamtalk classNamed:` / `Beamtalk allClasses` / `SystemNavigation …` |
| `Workspace globals` | `Workspace bindings` |
| `SystemNavigation default implementorsOf: #x` | `SystemNavigation implementorsOf: #x` |
| `self new` in a class method of an `Object subclass:` | A class-side API (rule 1), or `Value subclass:` for data |

REPL users type exactly what they typed before.

beamtalk-exdura's `resolveWorkflowClassNames:` switches to
`Beamtalk classNamed:` in a follow-up to that repository once Phase 1 has
shipped.

## Amended ADRs

- **ADR 0010:**
  - The core decision (workspace-injected bindings) and the "code outside a
    workspace does not have these bindings; this is deliberate" stance are
    reversed.
  - `Transcript` and `Beamtalk` are now class names.
- **ADR 0019:**
  - `current` class-variable singletons and their bootstrap wiring are
    removed.
  - The "`TranscriptStream show:` is a DNU" rule is replaced by a real
    class-side API on `Transcript`.
- **ADR 0040:**
  - The `Beamtalk`/`Workspace` split and their method sets stand, except
    that `Beamtalk globals` is removed and `Workspace globals` is renamed
    `Workspace bindings`.
  - The facades become classes rather than instances.
  - The *Workspace globals* layer no longer holds singletons (Amendment
    name-resolution chain).
- **ADR 0081:**
  - "There is no class-side operation mirror … the system's true singletons
    are reached through an injected binding" no longer holds; the singletons
    are gone.
  - The resolver loses its singleton step.
  - `Workspace globals` is renamed `Workspace bindings` and holds only
    `bind:as:` entries.
  - The `Session` decision itself is unaffected. This ADR generalises its
    "resolved through the class registry, like `Counter`" rule.
- **ADR 0125:**
  - The mode table changes: the `Transcript` process starts only alongside
    the REPL server.
  - `Beamtalk releaseInfo` resolves in compiled code on any node.
  - `no_workspace` joins `release_mode_no_compiler` /
    `release_mode_no_workspace` in `beamtalk_capability`.

## References
- Related issues:
  - BT-3631 (this ADR)
  - BT-3622 (`classNamed:` dynamic symbols, commit `0d2cb5231`)
  - BT-3632 (Object-kind `new` rule; settled here as option 2, §1a)
- Related ADRs:
  - [0010](0010-global-objects-and-singleton-dispatch.md) — global objects and singleton dispatch
  - [0013](0013-class-variables-class-methods-instantiation.md) — class methods
  - [0019](0019-singleton-class-variables.md) — singleton class variables
  - [0040](0040-workspace-native-repl-commands.md) — BeamtalkInterface/WorkspaceInterface facades
  - [0058](0058-platform-security-model.md) — ambient authority
  - [0064](0064-runtime-logging-control-and-observability-api.md) — Logger
  - [0070](0070-package-namespaces-and-dependencies.md) — no per-file imports
  - [0081](0081-first-class-session-object.md) — Session and binding layers
  - [0083](0083-metaclass-aware-type-inference.md) — typing of `new` (the implicit-`new` note in `SystemNavigation`)
  - [0124](0124-slots-late-assignment-definite-assignment.md) — `late classState: current`
  - [0125](0125-otp-releases-and-upgrade-compatibility.md) — modes and capability refusals
- Documentation:
  - `docs/beamtalk-language-features.md`: *Workspace and Reflection API*, *Sessions and binding layers*, *Passing Blocks Through Class Methods*
- External:
  - beamtalk-exdura `src/http/exdura_http_server.bt` `resolveWorkflowClassNames:`
  - Elixir `IEx.Helpers` and `Mix.Project`
  - Newspeak platform modules

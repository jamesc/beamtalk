# ADR 0019: Singleton Access via Class Variables

## Status
Proposed (2026-02-11)

## Context

### Problem

`Transcript` and `Beamtalk` are workspace singletons (ADR 0010) that currently rely on special codegen magic:

1. A hardcoded `WORKSPACE_BINDING_NAMES` list in the compiler recognizes these names
2. The compiler generates `persistent_term:get({beamtalk_binding, 'Name'})` lookups instead of normal dispatch
3. This only works in `workspace_mode` — compiled code cannot access these singletons at all, erroring with `WorkspaceBindingInBatchMode`

This creates two problems:

- **Compiled code is locked out.** Metaprogramming tools, build scripts, or library code cannot access `SystemDictionary` or `TranscriptStream` singletons. You must be in a REPL to use them.
- **Unnecessary compiler magic.** The codegen has a special dispatch path (`generate_workspace_binding_send`) with `persistent_term` lookups, PID extraction, and future creation — duplicating logic that the normal class/actor dispatch already handles.

### Current State

ADR 0010 established the workspace-injected bindings model:

```
Workspace supervisor starts
  → spawns TranscriptStream singleton actor
  → spawns SystemDictionary singleton actor
  → each calls persistent_term:put({beamtalk_binding, 'Name'}, BeamtalkObject)
  → codegen generates persistent_term:get lookups for these names
```

With ADR 0013 (BT-319) now complete, we have class variables and class-side methods. The singleton pattern can be expressed naturally in the language itself:

```beamtalk
Actor subclass: SystemDictionary
  classVar: current = nil
  class current => self.current
```

### Constraints

- Singletons must be accessible from both compiled code and the REPL
- The class process (gen_server) already exists for every registered class — no new processes needed
- Class variables are VM-global (shared across all code in the same BEAM node)
- Workspace isolation for `Transcript` is desirable but not critical — in practice, Pharo also has one global `Transcript`

## Decision

**Replace workspace binding magic with class variable singletons.** Remove the special codegen path for `Transcript`, `Beamtalk`, and `Workspace`. Instead, access singletons through class-side methods backed by class variables.

### Class Definitions

All three workspace singletons get the same pattern:

```beamtalk
Actor subclass: TranscriptStream
  classVar: current = nil

  class current => self.current
  class current: instance => self.current := instance

  show: value => @primitive 'show:'
  cr => @primitive 'cr'
  // ... other instance methods
```

```beamtalk
Actor subclass: SystemDictionary
  classVar: current = nil

  class current => self.current
  class current: instance => self.current := instance

  allClasses => @primitive 'allClasses'
  classNamed: className => @primitive 'classNamed:'
  // ... other instance methods
```

```beamtalk
Actor subclass: Workspace
  classVar: current = nil

  class current => self.current
  class current: instance => self.current := instance

  actors => @primitive 'actors'
  actorAt: pidString => @primitive 'actorAt:'
  actorsOf: className => @primitive 'actorsOf:'
```

### Usage — Everywhere

```beamtalk
// In compiled code, in the REPL, anywhere:
TranscriptStream current show: 'Hello, world!'
TranscriptStream current cr

SystemDictionary current allClasses
SystemDictionary current classNamed: #Counter
SystemDictionary current version

Workspace current actors
Workspace current actorsOf: #Counter
```

### REPL Session

```
> TranscriptStream current show: 'Hello from the REPL'
nil
> SystemDictionary current version
'0.1.0'
> SystemDictionary current allClasses
[Integer, Float, String, ...]
> Workspace current actors
[#Actor<Counter,0.132.0>]
```

### Error on Misuse

```
> TranscriptStream current
// Before workspace startup:
// => nil

> TranscriptStream show: 'oops'
// => ERROR: does_not_understand — TranscriptStream does not understand 'show:'
//    Hint: TranscriptStream is a class. Did you mean: TranscriptStream current show: 'hello'
```

### What Gets Removed

- `WORKSPACE_BINDING_NAMES` constant in codegen
- `is_workspace_binding()` function
- `generate_workspace_binding_send()` function
- `WorkspaceBindingInBatchMode` error variant
- `persistent_term` binding registration in singleton `init/1`
- `persistent_term` cleanup in singleton `terminate/1`

### Workspace Bootstrap Changes

The workspace supervisor currently spawns singletons in Erlang and registers them via `persistent_term`. With this ADR, the supervisor still spawns and supervises the singleton processes (OTP supervision is non-negotiable for crash recovery), but after startup it runs a bootstrap step that sets the class variables to point to the supervised processes.

The bootstrap step is Beamtalk code — similar to Pharo's package `postload` initialization:

```beamtalk
// workspace_bootstrap.bt — run by workspace supervisor after child processes start
// The supervisor has already spawned the singletons; bootstrap wires them into
// class variables so Beamtalk code can find them.
TranscriptStream current: TranscriptStream spawn
SystemDictionary current: SystemDictionary spawn
Workspace current: Workspace spawn
```

**Note on supervision:** The `spawn` calls here go through the class gen_server which calls `start_link`, linking the new process to the class. The workspace supervisor's role is to ensure the *bootstrap runs* and to restart singletons if they crash. The exact supervision strategy (supervisor starts processes directly vs. bootstrap starts them) is an implementation detail to be resolved in Phase 2. Options include:

1. **Supervisor spawns, bootstrap wires:** Supervisor starts children via `start_link` as today, bootstrap reads PIDs and sets class variables (safest OTP pattern)
2. **Bootstrap spawns, supervisor monitors:** Bootstrap code creates the actors, supervisor adopts them via monitoring (more Beamtalk-native but less standard OTP)

Option 1 is recommended. The bootstrap then becomes:

```erlang
%% In workspace supervisor, after children start:
TranscriptPid = whereis('Transcript'),
TranscriptObj = {beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, TranscriptPid},
beamtalk_object_class:set_class_var('TranscriptStream', current, TranscriptObj)
%% ... same for SystemDictionary and Workspace
```

This is Erlang code in the supervisor initially. Moving to Beamtalk bootstrap code is a future enhancement once the language has better OTP integration primitives.

This parallels Pharo's approach where:
- `ManifestMyPackage >> postload` runs initialization code when a package loads
- `MyClass class >> initialize` runs when a class is first loaded
- The system bootstrap loads core packages in order, each initializing themselves

## Prior Art

### Pharo/Squeak Smalltalk

`Smalltalk` and `Transcript` are global variables in the system dictionary — entries in `Smalltalk globals`. They're not class variables; they're namespace bindings. Access is `Smalltalk globals at: #Transcript`.

In practice, most code just uses `Transcript` as a bare name because the compiler resolves globals from the system dictionary. This is *more* magic than our approach — we're making the indirection explicit.

### Newspeak

Newspeak has no globals at all. Everything is accessed through module parameters (dependency injection). `Transcript` would be passed as a parameter to any module that needs it. This is the purist extreme — we're closer to this by requiring `TranscriptStream current` (explicit access through the class), but without the full module parameterization overhead.

### Erlang/OTP

Erlang singletons use registered processes (`whereis/1`) or `persistent_term`. Our class variable approach maps naturally to the OTP pattern — the class gen_server owns the singleton reference, and `class_send` provides the lookup.

### Elixir

Elixir uses `Application.get_env/3` for configuration singletons and `GenServer.call({:via, Registry, name})` for process singletons. The class variable pattern is analogous to a module attribute holding a registered process reference.

### Ruby

Ruby's singleton pattern uses `MyClass.instance` (via the `Singleton` mixin) or class-level instance variables (`@@current`). The `current` accessor pattern is widespread — `Thread.current`, `Fiber.current`, `Process.current`. Our `TranscriptStream current` follows this convention directly.

## User Impact

**Newcomer (from Python/JS):** `TranscriptStream current show: 'hello'` reads naturally — "get the current transcript stream, show hello on it." The `current` pattern is familiar from many languages' singleton accessors.

**Smalltalk developer:** This is more explicit than Pharo's bare `Transcript` global, but follows the standard `current` class-side accessor pattern used for singletons in Smalltalk. The departure from implicit globals is deliberate — Beamtalk doesn't have a system dictionary for name resolution.

**Erlang/BEAM developer:** Clean mapping to OTP — the class gen_server owns the singleton reference. No hidden `persistent_term` lookups in generated code. Standard `gen_server:call` path for access.

**Production operator:** Observable via `observer` — the singleton is a named process under the workspace supervisor. Class variable state visible through standard BEAM introspection tools.

**Tooling developer:** Simpler codegen — no special dispatch path for workspace bindings. The LSP can provide completions for `TranscriptStream current` through normal class-side method analysis.

## Steelman Analysis

### Alternative A: Keep codegen aliases + add class variables

- 🧑‍💻 **Newcomer**: "Short names are friendlier — `Beamtalk` is nicer than `SystemDictionary current`"
- 🎩 **Smalltalk purist**: "Pharo has `Transcript` as a bare global — we should too"
- ⚙️ **BEAM veteran**: "persistent_term is ~13ns, class_send is a gen_server:call — aliases are faster"
- 🏭 **Operator**: "Two access paths means two things to debug"
- 🎨 **Language designer**: "Having both is confusing — which is canonical?"

### Alternative C: Class variables + REPL auto-binds short names

- 🧑‍💻 **Newcomer**: "I get short names in the REPL where I'm exploring, and explicit names in code I'll maintain"
- 🎩 **Smalltalk purist**: "The REPL is the live environment — it should feel like Smalltalk"
- ⚙️ **BEAM veteran**: "Normal variable binding, no compiler magic — clean"
- 🏭 **Operator**: "REPL sugar doesn't affect production code paths"
- 🎨 **Language designer**: "Best of both worlds, but adds implicit REPL state"

### Tension Points

- Newcomers and Smalltalk purists prefer shorter names (A or C)
- BEAM veterans and language designers prefer explicit-only (B)
- Operators prefer fewer code paths (B)
- The deciding factor: **compiled code access** (the metaprogramming use case) works in all three options, but only B eliminates the dual-path confusion entirely

## Alternatives Considered

### Alternative: Do Nothing (Status Quo)

Keep the current `persistent_term` + codegen magic approach from ADR 0010.

**Rejected because:** Compiled code cannot access singletons — the `WorkspaceBindingInBatchMode` error blocks metaprogramming. The codegen has a special dispatch path that duplicates logic already handled by normal class/actor dispatch. Now that ADR 0013 provides class variables, the infrastructure exists to do this properly.

### Alternative: Registered Process Names (`whereis/1`)

Back `TranscriptStream current` with `whereis('Transcript')` instead of a class variable. The `current` method would call `whereis/1` directly.

```erlang
%% TranscriptStream class >> current
handle_call({current, []}, _From, State) ->
    Pid = whereis('Transcript'),
    Obj = {beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid},
    {reply, Obj, State}.
```

**Rejected because:** This still requires the class gen_server call (to dispatch the `current` message), so the performance benefit over a class variable is negligible. It also couples the singleton pattern to Erlang's process registry, which has a flat namespace. Class variables are the Beamtalk-native mechanism and compose better with future features (workspace-scoped class variables).

### Alternative A: Class Variable Singletons + Keep Codegen Aliases

Keep `Beamtalk` and `Transcript` as compiler-recognized names that resolve to `SystemDictionary current` / `TranscriptStream current` under the hood.

```beamtalk
// REPL (alias):
Beamtalk allClasses
// Compiled code:
SystemDictionary current allClasses
```

**Rejected because:** Two ways to do the same thing creates confusion about which is canonical. The codegen magic (`WORKSPACE_BINDING_NAMES`) is the exact thing we want to eliminate. Having aliases that "look like class names but aren't" is a learnability trap.

### Alternative C: Class Variables + REPL Auto-Binds Variables

The REPL injects `Beamtalk := SystemDictionary current` at session startup. No codegen magic — just a normal variable binding.

```beamtalk
// REPL (auto-bound variable):
Beamtalk allClasses
// Compiled code:
SystemDictionary current allClasses
```

**Rejected because:** While less magic than A, it still creates two vocabularies. Documentation and examples would need to explain both forms. New users would see `Beamtalk` in REPL tutorials and `SystemDictionary current` in library code and wonder why they differ. One way to do it is simpler.

## Consequences

### Positive

- Removes compiler magic — no special-cased names in codegen
- Compiled code can access singletons naturally (`SystemDictionary current`)
- Enables metaprogramming tools written outside the REPL
- Follows standard Smalltalk `current` singleton pattern
- Fewer codegen paths = fewer bugs, easier to maintain
- Class variable lifecycle is managed by the class gen_server (already supervised)

### Negative

- More verbose: `TranscriptStream current show: 'hello'` vs `Transcript show: 'hello'`
- Breaking change for existing REPL usage and E2E tests
- Class variable access goes through `gen_server:call` (~microseconds) vs `persistent_term:get` (~13ns) — slightly slower singleton lookup. However, the singleton *method call* itself is already a `gen_server:call`, so this adds one extra hop, not a new bottleneck category. In practice, `TranscriptStream current` will be called infrequently (not in tight loops).
- Workspace isolation is lost — class variables are VM-global. This conflicts with ADR 0004's vision of multiple workspaces on the same node (e.g., `dev` and `prod` would share the same `Transcript`). Mitigation: workspace-scoped class variables are a future extension. For now, Pharo also has one global `Transcript`.
- Class variable state is volatile — if the class gen_server crashes or hot-reloads, `current` resets to `nil`. Mitigation: the workspace supervisor's `post_init` step must re-set class variables after any restart. `persistent_term` survives process crashes but has its own GC issues.

### Neutral

- Workspace supervisor still spawns the singleton actors (unchanged)
- The singleton processes themselves are unchanged — same gen_server, same methods
- `TranscriptStream` and `SystemDictionary` remain Actor subclasses
- OTP supervision and restart behavior is preserved

## Implementation

### Phase 1: Add class variables and setters to stdlib classes

- Add `classVar: current = nil`, `class current`, and `class current:` to `lib/SystemDictionary.bt` and `lib/TranscriptStream.bt`
- Create `lib/Workspace.bt` with actor introspection methods (`actors`, `actorAt:`, `actorsOf:`)
- Verify class variable access works via `SystemDictionary current` (returns nil before bootstrap)

### Phase 2: Supervisor bootstrap wires class variables

- Workspace supervisor spawns children as today (OTP supervision unchanged)
- After children start, supervisor sets class variables via `beamtalk_object_class:set_class_var/3`
- Add restart hook: on child restart, re-set the class variable with new PID
- Future enhancement: move bootstrap logic to Beamtalk code once OTP integration primitives exist

### Phase 3: Remove codegen magic

- Remove `WORKSPACE_BINDING_NAMES`, `is_workspace_binding()`, `generate_workspace_binding_send()`
- Remove `WorkspaceBindingInBatchMode` error
- `Transcript` and `Beamtalk` now resolve as class references through normal dispatch
- Update `try_handle_class_reference` to no longer special-case these names

### Phase 4: Remove persistent_term binding infrastructure

- Remove `persistent_term:put/get` for `{beamtalk_binding, ...}` from singleton init/terminate in all three actors
- Remove `start_link_singleton` variants (use regular `start_link`)
- Remove workspace binding tests that test persistent_term behavior
- Add new tests for `TranscriptStream current`, `SystemDictionary current`, and `Workspace current`

### Phase 5: Update all references

- Update E2E tests: `Transcript show:` → `TranscriptStream current show:`, `Workspace actors` → `Workspace current actors`
- Update `examples/repl-tutorial.md` and documentation
- Update AGENTS.md workspace commands section

### Affected Components

| Component | Change |
|-----------|--------|
| `lib/SystemDictionary.bt` | Add `classVar: current`, `class current`, `class current:` |
| `lib/TranscriptStream.bt` | Add `classVar: current`, `class current`, `class current:` |
| `lib/Workspace.bt` | **New file** — stdlib class for workspace actor introspection |
| `dispatch_codegen.rs` | Remove workspace binding codegen path |
| `beamtalk_workspace_sup.erl` | Set class variables after child startup (replace persistent_term) |
| `beamtalk_system_dictionary.erl` | Remove persistent_term singleton logic |
| `beamtalk_transcript_stream.erl` | Remove persistent_term singleton logic |
| `beamtalk_workspace_actor.erl` | Remove persistent_term singleton logic |
| `beamtalk_workspace_binding_tests.erl` | Rewrite for class variable pattern |
| E2E tests, examples, docs | Update `Transcript` → `TranscriptStream current`, etc. |

## Migration Path

### Code Changes

| Before (ADR 0010) | After (ADR 0019) |
|---|---|
| `Transcript show: 'hello'` | `TranscriptStream current show: 'hello'` |
| `Transcript cr` | `TranscriptStream current cr` |
| `Beamtalk allClasses` | `SystemDictionary current allClasses` |
| `Beamtalk classNamed: #Counter` | `SystemDictionary current classNamed: #Counter` |
| `Beamtalk version` | `SystemDictionary current version` |
| `Workspace actors` | `Workspace current actors` |
| `Workspace actorsOf: Counter` | `Workspace current actorsOf: #Counter` |

### Diagnostic

Using the old names will produce a `does_not_understand` error since `Transcript` is no longer a recognized binding — it would be interpreted as the class `TranscriptStream` itself (via class reference lookup), which doesn't have instance methods like `show:`.

The error message should guide users:
```
TranscriptStream does not understand 'show:'
  Hint: 'show:' is an instance method. Did you mean: TranscriptStream current show: 'hello'
```

## References

- Supersedes: ADR 0010 (Global Objects and Singleton Dispatch)
- Builds on: ADR 0013 (Class Variables, Class-Side Methods, and Instantiation Protocol)
- Tension with: ADR 0004 (Persistent Workspace Management) — multi-workspace isolation deferred
- Related: ADR 0009 (OTP Application Structure) — Workspace actor lives in `beamtalk_workspace` app
- Related epic: BT-319 (Metaclass & First-Class Classes) — now complete
- Related epic: BT-320 (Object Protocol & Live Development)
- Beamtalk principles: `docs/beamtalk-principles.md` (Interactive-first, BEAM-native) 

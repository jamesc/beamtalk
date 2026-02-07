# ADR 0010: Global Objects and Singleton Dispatch

## Status
Proposed (2026-02-07)

## Context

Beamtalk currently has two "global" objects — `Transcript` and `Beamtalk` — that behave unlike any other object in the system. They are accessed by class name but have no instances. This creates several problems:

### Current Implementation (Ad-hoc)

**Transcript** is a bare Erlang module (`transcript.erl`) with exported functions:
```beamtalk
Transcript show: 'Hello'   // codegen → call 'transcript':'show:'(<<"Hello">>)
Transcript cr               // codegen → call 'transcript':'cr'()
```

**Beamtalk** is defined in `lib/Beamtalk.bt` and backed by `beamtalk_stdlib.erl`:
```beamtalk
Beamtalk allClasses         // codegen → call 'beamtalk':'allClasses'()
Beamtalk classNamed: #Counter
```

Both are registered as classes in `beamtalk_stdlib.erl` with class methods but no instance methods.

### Problems

1. **Bypass dispatch entirely** — Class-level method calls compile to direct module function calls (`call 'module':'method'()`), skipping `beamtalk_dispatch:lookup/5`. This means:
   - No `doesNotUnderstand:` — unknown methods produce raw Erlang `undef` errors instead of `#beamtalk_error{}`
   - No hierarchy walking — can't inherit Object methods like `respondsTo:`, `class`, `describe`
   - No extension methods — can't add methods to Transcript at runtime
   - Would bypass method combinations (before/after) once implemented

2. **Module naming collision** — Transcript's module must be named `transcript` (matching `to_module_name("Transcript")`) rather than `beamtalk_transcript`, breaking the `beamtalk_*` naming convention. Any global whose class name collides with an Erlang stdlib module would shadow it.

3. **Not real objects** — In Smalltalk, `Transcript` is an instance of `TranscriptStream` and `Smalltalk` is an instance of `SystemDictionary`. They respond to `class`, `respondsTo:`, `inspect`, etc. In Beamtalk, they're pseudo-objects that don't participate in the object model.

4. **Violates design principles** — `docs/beamtalk-principles.md` states "Newspeak-style: no global namespace, all access through message chains." Yet Transcript and Beamtalk ARE globals accessed by bare name.

### Constraints

- **BEAM has no shared mutable state** — Singletons with state must be processes (actors)
- **Class names are uppercase identifiers** — Parser treats them as `ClassReference`, codegen generates class-level calls
- **Codegen is the boundary** — Whatever model we choose must compile to valid Core Erlang
- **Backward compatibility** — `Transcript show:` and `Beamtalk allClasses` must keep working

## Decision

Adopt a **workspace-injected bindings** model: well-known objects (`Transcript`, `Beamtalk`) are singleton actor instances provided by the workspace as pre-bound variables — not language-level globals. This is a stepping stone toward explicit module-level imports.

### Design

| Smalltalk | Beamtalk (Phase 1: interim) | Beamtalk (Phase 2: modules) |
|-----------|---------------------------|----------------------------|
| `Transcript` — global in SystemDictionary | Variable bound by workspace | Declared via module `import:` |
| `Smalltalk` — global in SystemDictionary | Variable bound by workspace | Declared via module `import:` |

**User-facing syntax is unchanged:**
```beamtalk
Transcript show: 'Hello'        // works exactly as before
Transcript cr
Beamtalk allClasses
Beamtalk classNamed: #Counter
```

**But now these are real objects, not bare Erlang modules:**
```beamtalk
Transcript class                // => TranscriptStream
Transcript respondsTo: #show:   // => true
Transcript inspect              // => "a TranscriptStream"

Beamtalk class                  // => SystemDictionary
```

### Runtime Model

Each well-known object is a **singleton actor** owned by the workspace. The workspace injects them as pre-bound variables into the evaluation environment:

```erlang
%% Workspace startup:
%% 1. Spawn singleton actors
{ok, TranscriptPid} = beamtalk_transcript_stream:spawn(),
{ok, BeamtalkPid} = beamtalk_system_dictionary:spawn(),

%% 2. Inject as workspace bindings (available to REPL and :load'd code)
WorkspaceBindings = #{
    'Transcript' => TranscriptPid,
    'Beamtalk'   => BeamtalkPid
}.
```

This is NOT a global registry — it's the workspace's environment. Code outside a workspace (e.g., `beamtalk build` without a workspace) does not have these bindings. This is deliberate: `Transcript` is a workspace service, not a language primitive.

### Evolution: Module-Level Imports

The interim model (workspace-injected variables via `persistent_term`) is a stepping stone to **explicit module-level imports**, where classes declare their workspace dependencies:

```beamtalk
// Phase 2: module declares dependencies, workspace resolves at load time
Object subclass: MyApp
  import: Transcript, Beamtalk

  run =>
    Transcript show: 'Starting'   // resolved from imports, not persistent_term
    Beamtalk classNamed: #Counter
```

**Benefits over interim model:**
- **Explicit dependencies** — the compiler knows what a module needs
- **Mockable** — tests can substitute imports (e.g., a silent Transcript)
- **Verifiable** — compiler errors for unresolved imports, not runtime failures
- **No `persistent_term`** — workspace resolves imports at load time, binds into module scope

**Prerequisites:** Import syntax, workspace-aware module loader.

**Full Newspeak-style lexical scoping** (nested classes accessing enclosing scope) is a possible future evolution but is not planned. A separate ADR should evaluate whether it's warranted given the module import model.

### Sync vs Async Dispatch

In Pharo, all message sends (including `Transcript show:`) are **synchronous** — the caller blocks until the method completes. Pharo's `ThreadSafeTranscript` achieves thread safety via internal locking, not async messaging.

On BEAM, actors use **async sends** (`gen_server:cast`) by default — the caller gets a Future back and doesn't wait. This creates a tension for well-known instances:

| Method | Needs return value? | Natural fit |
|--------|-------------------|-------------|
| `Transcript show: 'Hello'` | No (I/O side effect) | Async (`cast`) |
| `Transcript cr` | No | Async (`cast`) |
| `Beamtalk allClasses` | Yes (returns list) | Sync (`call`) |
| `Beamtalk classNamed: #Counter` | Yes (returns class) | Sync (`call`) |
| `Transcript class` | Yes (returns `TranscriptStream`) | Sync (`call`) |

**Decision: Use standard actor dispatch — async by default, `await` when needed.**

Well-known instances are actors, so they follow the same rules as any actor:

```beamtalk
// Async (fire-and-forget) — returns a Future, but usually ignored
Transcript show: 'Hello'

// Sync (needs the value) — caller awaits the Future
classes := Beamtalk allClasses     // implicit await on assignment
Transcript class                    // implicit await (result used)
```

This means workspace binding dispatch is uniform — always async via `gen_server:cast` + Future. The caller decides whether to await. No special sync/async annotation needed on methods.

```erlang
%% Codegen dispatch for workspace-bound names:
dispatch_binding(Pid, Selector, Args) ->
    Future = beamtalk_future:new(),
    gen_server:cast(Pid, {Selector, Args, Future}),
    Future.

%% Class method sends remain direct calls (no binding lookup):
%% Counter spawn  →  call 'counter':'spawn'()
```

### Transcript as Shared Workspace Log

**Context:** In Pharo, Transcript is a dev-time convenience — production Pharo apps use proper logging frameworks (e.g., Beacon). On BEAM, Beamtalk already uses OTP `logger` for structured runtime logging. Transcript's role in Beamtalk is therefore limited to interactive development:

| Use case | Tool |
|----------|------|
| Learning / tutorials | `Transcript show:` (Beamtalk) |
| Quick REPL debugging | `Transcript show:` (Beamtalk) |
| Production logging | OTP `logger` (Erlang — structured, leveled) |

**Design: Transcript is a shared log actor, separate from the REPL — following Pharo's model.**

In Pharo, Transcript is a **separate window** from the Playground (REPL). You write code in the Playground, output appears in the Transcript window. They are distinct UI surfaces. Beamtalk follows the same separation:

- **REPL** — shows eval results only (`=> 4`, `=> nil`)
- **Transcript** — separate output channel for `Transcript show:` output from any source

```
┌──────────────────┐  ┌──────────────────┐
│ REPL             │  │ Transcript       │
│                  │  │                  │
│ > 2 + 2          │  │ Hello            │
│ => 4             │  │ Got request /foo │
│ > Transcript     │  │ tick             │
│     show: 'Hi'   │  │ Hi               │
│ => nil           │  │                  │
└──────────────────┘  └──────────────────┘
```

**Why separate?**

1. **Library/app code** — An actor doing `Transcript show: 'Got request'` should work regardless of whether a REPL is attached. There's no "caller's REPL" to route to.
2. **Shared workspace** — Multiple REPL sessions share the same workspace (ADR 0004). Transcript output from any source is relevant to all viewers.
3. **No confusion** — Newcomers see `=> nil` in the REPL and Transcript output elsewhere. No interleaved output, no "where did that come from?" surprises.
4. **Simplicity** — No group_leader tricks, no caller context in messages. Just a log sink with subscribers.

**Transcript is a pub/sub actor:**

```
                    ┌─────────────────────┐
  REPL eval    ──→  │                     │ ──→ `beamtalk transcript` (CLI viewer)
  MyHttpServer ──→  │  Transcript Actor   │ ──→ REPL-2 (opted in via subscribe)
  background   ──→  │  (shared log sink)  │ ──→ IDE Transcript panel (future)
                    └─────────────────────┘
```

The actor maintains a ring buffer of recent output and a list of subscriber pids. Subscribers receive `{transcript_output, Text}` messages. Dead subscribers are cleaned up via process monitors.

**Accessing Transcript output:**

```bash
# Separate CLI viewer (like `tail -f` on the workspace log)
beamtalk transcript
```

```beamtalk
// In the REPL — just send messages to the object:
Transcript subscribe              // subscribe this session — output streams in
Transcript unsubscribe            // unsubscribe — REPL goes quiet again
Transcript recent                 // returns buffer contents (last N entries)
Transcript clear                  // clear the buffer
```

No special REPL commands needed — Transcript is a real object, so subscription is just message sends. This follows the principle that behavior lives on objects, not in REPL magic.

**REPLs do NOT subscribe by default.** The REPL is for eval results. Transcript is a separate concern — you subscribe when you want it, like opening Pharo's Transcript window.

**When no subscribers exist** (batch compile, headless app):
- Output goes to the ring buffer only
- Optionally forwarded to OTP `logger:info/2` for persistence
- No output is lost — can be inspected via `Transcript recent` (returns buffer contents)

### Cascade Semantics

Cascades send multiple messages to the **same receiver**, returning the result of the **last** message. Since `Transcript` is a real actor (bound via workspace), cascades work naturally:

```beamtalk
Transcript show: 'Hello'; cr; show: 'World'
```

Compiles to (conceptually):
```erlang
%% 1. Resolve Transcript from workspace bindings — it's a pid
Pid = lookup_binding('Transcript'),

%% 2. Send all messages to same pid (async, discard intermediate Futures)
gen_server:cast(Pid, {'show:', [<<"Hello">>], _F1}),
gen_server:cast(Pid, {'cr', [], _F2}),

%% 3. Last message — return its Future
Future3 = beamtalk_future:new(),
gen_server:cast(Pid, {'show:', [<<"World">>], Future3}),
Future3
```

Because `gen_server` processes messages sequentially from its mailbox, the three messages execute in order — `show: 'Hello'`, then `cr`, then `show: 'World'` — even though the sends are async. This gives us **ordered execution without blocking the caller**.

Message dispatch uses the standard actor path. This means:
- ✅ Full dispatch through `beamtalk_dispatch:lookup/5`
- ✅ `doesNotUnderstand:` produces `#beamtalk_error{}`
- ✅ Inherits Object methods (`class`, `respondsTo:`, `describe`, `inspect`)
- ✅ Extension methods work
- ✅ Module naming follows `beamtalk_*` convention (`beamtalk_transcript_stream.erl`)
- ✅ Cascades work — ordered execution guaranteed by gen_server mailbox

### Codegen Change

The codegen currently special-cases `ClassReference` receivers, generating direct Erlang function calls. This ADR changes the codegen to check workspace bindings first.

#### How It Works

When the compiler encounters an uppercase identifier as a receiver, it generates code that:

1. **Checks workspace bindings** — Is this name bound to a workspace-injected object?
2. **Falls back to class method** — If not a binding, treat as a class-level method call (existing behavior)

```erlang
%% Current (all ClassReference → direct function call):
call 'transcript':'show:'(<<"Hello">>)    %% Transcript show: 'Hello'
call 'counter':'spawn'()                   %% Counter spawn

%% Proposed (check workspace bindings first):
%% Transcript — found in workspace bindings → actor send
let Pid = lookup_binding('Transcript') in
gen_server:cast(Pid, {'show:', [<<"Hello">>], Future})

%% Counter — not in workspace bindings → class method call (unchanged)
call 'counter':'spawn'()
```

#### Binding Resolution

The workspace provides bindings via `persistent_term` (internal implementation detail):

```erlang
%% Workspace startup — populate bindings
persistent_term:put({beamtalk_binding, 'Transcript'}, TranscriptPid).
persistent_term:put({beamtalk_binding, 'Beamtalk'}, BeamtalkPid).

%% Codegen lookup — ~13ns, O(1), lock-free
lookup_binding(Name) ->
    persistent_term:get({beamtalk_binding, Name}, undefined).
```

**Key difference from the global registry approach:** Only workspace-injected names are in the binding table. Classes (`Counter`, `Point`, `Array`) are NOT — they continue to use direct module function calls. This means:

- **Zero overhead** on class method sends (`Counter spawn`, `Point new`) — no lookup at all
- **~13ns overhead** on workspace binding sends (`Transcript show:`, `Beamtalk allClasses`) — negligible
- **No workspace = no bindings** — `beamtalk build` without a workspace produces a clear "unbound variable" error for `Transcript`

### Class Definitions (Future — not yet in `lib/`)

```beamtalk
// lib/TranscriptStream.bt (future)
Actor subclass: TranscriptStream
  show: value => @primitive 'show:'
  cr => @primitive 'cr'
  subscribe => @primitive 'subscribe'
  unsubscribe => @primitive 'unsubscribe'
  recent => @primitive 'recent'
  clear => @primitive 'clear'

// lib/SystemDictionary.bt (future — renamed from lib/Beamtalk.bt)
Actor subclass: SystemDictionary
  allClasses => @primitive 'allClasses'
  classNamed: className => @primitive 'classNamed:'
  globals => @primitive 'globals'
  version => @primitive 'version'
```

### Workspace Binding Names

Following Pharo's model, `Transcript` and `Beamtalk` are **not class names** — they are binding names for singleton instances. The classes are `TranscriptStream` and `SystemDictionary` respectively.

```beamtalk
Transcript class              // => TranscriptStream  (not Transcript)
Beamtalk class                // => SystemDictionary   (not Beamtalk)

TranscriptStream              // => the class object itself
SystemDictionary              // => the class object itself
```

This is exactly how Pharo works: `Smalltalk class` returns `SystemDictionary`, not `Smalltalk`. The name `Beamtalk` is an alias for the singleton instance, not a class.

In the workspace, bindings and classes coexist in separate namespaces:

| Name | Namespace | Value |
|------|-----------|-------|
| `'Transcript'` | workspace binding | pid of the TranscriptStream singleton |
| `'Beamtalk'` | workspace binding | pid of the SystemDictionary singleton |
| `'TranscriptStream'` | class | class process |
| `'SystemDictionary'` | class | class process |
| `'Counter'` | class | class process |

### Workspace Startup

The workspace spawns and injects well-known objects during initialization:

```erlang
%% In beamtalk_workspace.erl:
init_workspace_bindings() ->
    %% 1. Spawn singleton actors
    {ok, TranscriptPid} = beamtalk_transcript_stream:spawn(),
    {ok, BeamtalkPid} = beamtalk_system_dictionary:spawn(),
    
    %% 2. Inject as workspace bindings (persistent_term for fast codegen lookup)
    persistent_term:put({beamtalk_binding, 'Transcript'}, TranscriptPid),
    persistent_term:put({beamtalk_binding, 'Beamtalk'}, BeamtalkPid),
    
    %% 3. Register as named processes (for supervision and observer)
    register('Transcript', TranscriptPid),
    register('Beamtalk', BeamtalkPid).
```

**Ordering constraint:** Classes must be registered before their instances can be spawned. Bootstrap sequence:
1. Register core classes (Object, Integer, String, etc.)
2. Register TranscriptStream and SystemDictionary classes
3. Workspace starts → spawns and binds singleton instances

## Prior Art

### Smalltalk (Squeak/Pharo)
- `Smalltalk` is an instance of `SystemDictionary` — a real object with state
- `Transcript` is an instance of `ThreadSafeTranscript` (Pharo) — backed by a stream
- **Both are entries in the same global dictionary** (`Smalltalk globals`), alongside classes
- The compiler generates identical bytecode for `Transcript show:` and `Counter new` — global lookup then message send
- No distinction between "class name" and "global variable" at the language level — they are both keys in the global dictionary
- Both respond to all Object protocol (`class`, `respondsTo:`, `inspect`, etc.)
- `Smalltalk class` returns `SystemDictionary`, not `Smalltalk` — the name is an alias, not a class

### Newspeak
- **No globals at all** — everything accessed through the module hierarchy
- `Transcript` equivalent is passed as a parameter to the top-level module
- Pure capability-based: you can only use what you're given
- More principled but harder for beginners

### Erlang/OTP
- Registered processes (`register/2`) are the idiomatic singleton pattern
- `whereis/1` is O(1) — very cheap lookup
- Used for supervision trees, named gen_servers, application masters
- Natural fit for BEAM-based globals

### Gleam
- No globals — all state is passed explicitly or held in actors
- Uses OTP registered processes for shared state

## User Impact

**Newcomer:** No change — `Transcript show: 'Hello'` works the same. Better error messages when methods are misspelled.

**Smalltalk developer:** Familiar model — globals are instances of real classes. `Transcript class` returns `TranscriptStream` as expected.

**Erlang/BEAM developer:** Natural mapping to registered processes. Can interact with globals from Erlang code via `gen_server:call(Transcript, ...)`.

**Operator:** Globals are visible in `observer` as named processes. Can inspect state, restart if crashed (via supervisor).

## Steelman Analysis

### 1. Performance Purist: "You're adding overhead to workspace binding dispatch"

Workspace binding dispatch adds a `persistent_term:get/1` (~13ns) lookup before sending — and then an actor message send (~1-5μs) where the old `transcript.erl` was a direct function call (~10ns). That's a 100x slowdown for `Transcript show:`. And the codegen now has a branch: "is this a workspace binding? If so, lookup + actor send. Otherwise, direct call." Two dispatch paths means two sets of bugs.

**Counter:** The 13ns lookup is unmeasurable against actual work. `Transcript show:` is a dev debugging tool, not a hot loop — nobody profiles `println`. Class sends (`Counter spawn`, `Point new`) have ZERO overhead — they skip the binding check entirely and use direct module calls as before. The two-path codegen is simple: one `persistent_term:get/2` with a default fallback. The hot path (class sends) is unchanged.

### 2. BEAM Purist: "This isn't idiomatic Erlang"

Erlang solves "global named service" with `register/2` and `whereis/1`. You're layering `persistent_term` bindings, a workspace supervisor, and a pub/sub system on top. OTP already has `logger` for logging, `pg` for process groups, and `sys.config` for configuration. This is reinventing standard OTP infrastructure behind a Smalltalk-flavored API.

**Counter:** Workspace injection IS standard OTP — `persistent_term` for configuration, `gen_server` for actors, supervisors for crash recovery. The pub/sub for Transcript is ~20 lines of standard `gen_server`. And `logger` serves a different purpose: structured production logging vs. dev-time visible output. They coexist. The workspace model aligns with OTP's application environment pattern.

### 3. Simplicity Advocate: "This is a lot of machinery for two bindings"

The entire ADR — workspace injection, `persistent_term` bindings, supervision tree, pub/sub, bootstrap ordering, crash recovery — exists to make `Transcript show: 'Hello'` and `Beamtalk allClasses` "real objects." That's two bindings. The current 50-line `transcript.erl` works. You're proposing workspace-owned actors with supervision, Transcript pub/sub, a separate viewer CLI, and codegen changes. YAGNI — when you need a third binding, build the infrastructure then.

**Counter:** The machinery IS minimal — workspace spawns two actors, stores two `persistent_term` entries, one supervisor. That's it. No registry module, no framework. The pub/sub on Transcript is ~20 lines of `gen_server`. And the codegen change is a single `persistent_term:get/2` fallback — 5 lines of Rust. The real value is making these first-class objects with proper `doesNotUnderstand:`, `class`, and `respondsTo:` — something bare module functions can't provide.

### 4. Smalltalk Purist: "Pharo's Transcript is synchronous and simple"

In Pharo, `Transcript show: 'Hello'` is a synchronous method call on a shared instance — write to the stream, done. Beamtalk's version involves: async cast to an actor, Future allocation, pub/sub dispatch to subscribers, ring buffer management, and asynchronous delivery to REPL sessions. The ordering guarantee comes from gen_server mailbox semantics, not from the language itself. A newcomer reading `Transcript show: 'Hello'` has no idea this machinery exists, and when output appears asynchronously between prompts instead of inline, it will confuse them.

**Counter:** Pharo is single-image, single-threaded UI. Beamtalk runs on BEAM — concurrent, distributed, multi-session. The async model is the honest one: output from a background actor genuinely IS asynchronous. Hiding that behind a synchronous facade would be misleading on BEAM. And the REPL can present Transcript output cleanly — Pharo's own Transcript window doesn't update until the UI thread yields, so Pharo users already experience "delayed" Transcript output in practice.

### 5. Capability/Security Advocate: "Workspace bindings are still globals in disguise"

`docs/beamtalk-principles.md` says "Newspeak-style: no global namespace, all access through message chains." Workspace bindings are better than a true global registry, but `persistent_term` IS globally readable — any code on the node can call `persistent_term:get({beamtalk_binding, 'Transcript'})` to bypass the workspace abstraction. A compromised module can write to Transcript (information leak) or call `Beamtalk allClasses` (reconnaissance). And code compiled outside a workspace context — what happens when it references `Transcript`?

**Counter:** This is explicitly an *interim* step. The ADR documents the evolution to module-level imports, where dependencies are declared explicitly and resolved by the workspace at load time — making them mockable and verifiable. The interim uses `persistent_term` because it's the simplest OTP mechanism, not because it's the security model. Code outside a workspace gets a compile error for unresolved `Transcript` — that's the whole point of workspace-scoped bindings. And in practice, BEAM applications already have globally accessible process registrations. The workspace model is strictly *more* contained than Erlang's default.

### 6. Newcomer/DX Advocate: "Where did my output go?"

A newcomer types `Transcript show: 'Hello'` in their REPL and sees `=> nil`. Where's "Hello"? It went to the Transcript channel, which they haven't subscribed to. In Python, `print('Hello')` shows output immediately. Beamtalk requires knowing that Transcript is a separate output channel and that you need to subscribe or open a viewer. That's a steeper learning curve for the most basic debugging tool.

**Counter:** The mental model is clear and consistent: Transcript is a shared log, like a separate window in Pharo's IDE. The REPL tutorial can explain this in one line: "Type `Transcript subscribe` to see output here, or run `beamtalk transcript` in another terminal." And since Transcript is a real object, the newcomer learns the object model by interacting with it — `Transcript subscribe`, `Transcript recent`, `Transcript class`. Every interaction reinforces "everything is a message send." The alternative — inline output that sometimes interleaves with unrelated background actor output — is more confusing, not less.


## Alternatives Considered

### Alternative A: Fix Class-Level Dispatch Only
Add error handling to class-level method calls without changing the object model:
```erlang
%% Wrap class method calls in try/catch
try transcript:'show:'(Value) catch error:undef -> ... end
```

**Rejected:** This is a band-aid. Globals still wouldn't be real objects, cascades still wouldn't work, and every new global would need a custom module with a naming collision risk.

### Alternative B: Newspeak Pure Module Parameters
Pass globals explicitly to every module via lexical scoping:
```beamtalk
Object subclass: MyApp platform: platform
  run =>
    platform transcript show: 'Hello'
```

**Not planned:** Requires nested class scoping — a major compiler effort. The module-level `import:` model (see "Evolution" section) provides explicit dependencies and mockability without the full Newspeak machinery. A separate ADR can evaluate full lexical scoping if the module import model proves insufficient.

### Alternative C: Value Type Singletons (Not Actors)
Make globals value types (maps) rather than actors:
```erlang
%% Transcript is just a tagged map
Transcript = #{'__class__' => 'TranscriptStream'}
```

**Rejected:** Value types are copied and have no shared identity. `Transcript` needs to be a single entity that all code sends messages to, especially for I/O coordination. Actors are the natural fit.

## Consequences

### Positive
- Well-known objects become first-class — `class`, `respondsTo:`, `inspect`, `describe` all work
- Consistent dispatch — messages go through `beamtalk_dispatch`, producing `#beamtalk_error{}` on failure
- Cascades work — `Transcript show: 'Hello'; cr; show: 'World'`
- Module naming is consistent — `beamtalk_transcript_stream.erl`, not `transcript.erl`
- Extensible — users can add methods to TranscriptStream via extensions
- Observable — singletons are named processes visible in `observer`
- Supervisable — singletons can be restarted if they crash
- No language-level globals — workspace bindings, not a global namespace
- Zero overhead on class sends — only workspace bindings use `persistent_term` lookup; `Counter spawn` etc. unchanged
- Clear evolution path — workspace bindings → module-level `import:` declarations (no globals needed)

### Negative
- Slightly more complex workspace startup — must spawn and register singleton processes
- Actor message overhead for Transcript — `gen_server:cast` vs direct function call (negligible for dev tool)
- More moving parts — processes can crash, need supervision
- `persistent_term` write cost — updating bindings triggers global GC, but this only happens at workspace startup
- Newcomer confusion — `Transcript show:` returns `nil` in REPL, output goes to separate channel

### Neutral
- `lib/Beamtalk.bt` renamed to `lib/SystemDictionary.bt`
- Transcript module renamed from `transcript.erl` to `beamtalk_transcript_stream.erl`
- Workspace bindings use `persistent_term` internally — implementation detail, not public API

## Implementation

### OTP Application Placement (ADR 0009)

ADR 0009 splits the runtime into `beamtalk_runtime` (core language) and `beamtalk_workspace` (interactive development).

| Component | OTP App | Rationale |
|-----------|---------|-----------|
| `SystemDictionary` (Beamtalk) | `beamtalk_runtime` | Introspection of classes is a core language feature |
| `TranscriptStream` (Transcript) | `beamtalk_workspace` | I/O is a workspace service |
| Workspace bindings (`persistent_term`) | `beamtalk_workspace` | Populated at workspace startup |

This means:
- **Batch compile** (`beamtalk build`): No workspace bindings exist. Code referencing `Transcript` gets a clear "unbound variable" error. Classes (`Counter`, `Point`) work normally via direct module calls.
- **REPL/workspace**: Workspace spawns singletons and injects bindings at startup. `Transcript` and `Beamtalk` are available immediately.
- **One workspace per node** (ADR 0004): Simple `register/2` is sufficient — no scoping needed.

### Supervision Strategy

Singleton actors are **permanent workers** under their owning supervisor:

```
beamtalk_runtime_sup (one_for_one)            [beamtalk_runtime app]
├── beamtalk_bootstrap
├── beamtalk_stdlib
├── beamtalk_object_instances
└── beamtalk_system_dictionary                 ← Beamtalk singleton

beamtalk_workspace_sup (one_for_one)          [beamtalk_workspace app]
├── beamtalk_workspace_meta
├── beamtalk_transcript_stream                 ← Transcript singleton
├── beamtalk_repl_server
├── beamtalk_actor_sup
└── ...
```

**Crash recovery:**

| Scenario | Behavior |
|----------|----------|
| Transcript crashes | Supervisor restarts. New process self-registers, updates `persistent_term` binding |
| Beamtalk (SystemDictionary) crashes | Supervisor restarts. Class metadata reconstructed from `beamtalk_object_class` processes |

Restarted processes update their own `persistent_term` binding in `init/1` — no external monitoring needed.

### Phase 1: Singleton Actor Classes
- Create `TranscriptStream` class with pub/sub actor dispatch (`lib/TranscriptStream.bt`)
- Create `SystemDictionary` class with actor dispatch (`lib/SystemDictionary.bt`)
- Runtime modules: `beamtalk_transcript_stream.erl`, `beamtalk_system_dictionary.erl`

### Phase 2: Workspace Binding Injection
- Workspace startup spawns singletons and populates `persistent_term` bindings
- Add supervision for singleton actors under appropriate supervisor

### Phase 3: Codegen Update
- `ClassReference` dispatch checks `persistent_term` workspace bindings first
- If bound → actor message send (existing actor codegen path)
- If not bound → direct module function call (existing class method codegen path)
- Class method calls (`Counter spawn`, `Point new`) are completely unchanged

### Phase 4: Migration
- Rename `lib/Beamtalk.bt` → `lib/SystemDictionary.bt`
- Rename `transcript.erl` → `beamtalk_transcript_stream.erl`
- Update E2E tests and examples
- Deprecate old module names

### Affected Components
- **Codegen:** `dispatch_codegen.rs` — `ClassReference` checks workspace bindings first
- **Workspace:** Startup code spawns singletons, injects `persistent_term` bindings
- **Runtime (new):** `beamtalk_transcript_stream.erl`, `beamtalk_system_dictionary.erl`
- **Stdlib:** `lib/TranscriptStream.bt`, `lib/SystemDictionary.bt`
- **Tests:** E2E and unit tests for workspace bindings and singletons

## Migration Path

Existing code using `Transcript show:` and `Beamtalk allClasses` continues to work unchanged in a workspace context. The syntax is identical; only the dispatch path changes internally.

Outside a workspace (`beamtalk build`), references to `Transcript` produce a clear compile-time error: "Transcript is a workspace binding, not available in batch compilation." This is a deliberate design choice — Transcript is a dev tool, not a production dependency.

The current `transcript.erl` module (from BT-328) serves as the initial implementation and will be refactored into `beamtalk_transcript_stream.erl` with actor dispatch.

## Future Considerations

### Repl as a First-Class Object

A `Repl` workspace binding representing the current REPL session would allow:

```beamtalk
Transcript subscribe: Repl   // explicit subscriber (no implicit CallerPid)
Repl history                  // session history as messages, not :history command
Repl bindings                 // inspect current variable bindings
```

This aligns with the principle that behavior lives on objects, not REPL commands. Unlike `Transcript` and `Beamtalk` (workspace singletons), `Repl` would be **per-session** — each REPL connection gets its own binding. The codegen is unchanged (`persistent_term` lookup), but the binding is set per-eval-context rather than at workspace startup.

### Workspace / Beamtalk Consolidation

As module-level imports mature, the `Beamtalk` object (SystemDictionary) and the Workspace concept may merge. The workspace already provides the bindings — it's a small step for it to also provide class lookup directly:

```beamtalk
Object subclass: MyApp
  import: Transcript, Beamtalk    // today: two separate objects

// future: workspace IS the system dictionary
Object subclass: MyApp
  import: Workspace               // provides transcript, classNamed:, etc.
```

This suggests a future where:
- `Beamtalk` (SystemDictionary) is absorbed into the Workspace object
- `Workspace` becomes the single import for platform services
- `Transcript` and `Repl` are accessed as `workspace transcript`, `workspace repl`

A follow-up ADR should evaluate this consolidation when the module import system is in place.

### Full Newspeak Lexical Scoping

Newspeak eliminates all imports by passing the platform at the entry point and using nested class lexical scoping for inner access. This is a significantly larger compiler effort (nested class scoping) and may not be warranted given the module import model. A separate discussion/ADR can evaluate this if module imports prove insufficient for capability isolation or testing.

## References
- Related issues: BT-353 (this ADR), BT-328 (Transcript implementation), BT-329 (Towers of Hanoi — needs Transcript)
- Related ADRs: ADR 0005 (BEAM Object Model), ADR 0006 (Unified Method Dispatch), ADR 0007 (Compilable Stdlib), ADR 0009 (OTP Application Structure)
- Design principles: `docs/beamtalk-principles.md` — "Newspeak-style: no global namespace"
- Smalltalk: `SystemDictionary`, `TranscriptStream` in Squeak/Pharo
- Newspeak: Module-based capability system (no globals)

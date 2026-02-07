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
   - No method combinations (before/after)

2. **Module naming collision** — Transcript's module must be named `transcript` (matching `to_module_name("Transcript")`) rather than `beamtalk_transcript`, breaking the `beamtalk_*` naming convention. Any global whose class name collides with an Erlang stdlib module would shadow it.

3. **Not real objects** — In Smalltalk, `Transcript` is an instance of `TranscriptStream` and `Smalltalk` is an instance of `SystemDictionary`. They respond to `class`, `respondsTo:`, `inspect`, etc. In Beamtalk, they're pseudo-objects that don't participate in the object model.

4. **Violates design principles** — `docs/beamtalk-principles.md` states "Newspeak-style: no global namespace, all access through message chains." Yet Transcript and Beamtalk ARE globals accessed by bare name.

### Constraints

- **BEAM has no shared mutable state** — Singletons with state must be processes (actors)
- **Class names are uppercase identifiers** — Parser treats them as `ClassReference`, codegen generates class-level calls
- **Codegen is the boundary** — Whatever model we choose must compile to valid Core Erlang
- **Backward compatibility** — `Transcript show:` and `Beamtalk allClasses` must keep working

## Decision

Adopt a **well-known instance** model: globals are singleton actor instances of named classes, registered in the runtime and accessible by name.

### Design

| Smalltalk | Beamtalk | Class | Instance |
|-----------|----------|-------|----------|
| `Smalltalk` (SystemDictionary) | `Beamtalk` | `SystemDictionary` | Singleton actor |
| `Transcript` (TranscriptStream) | `Transcript` | `TranscriptStream` | Singleton actor |

**User-facing syntax is unchanged:**
```beamtalk
Transcript show: 'Hello'        // works exactly as before
Transcript cr
Beamtalk allClasses
Beamtalk classNamed: #Counter
```

**But now these are real objects:**
```beamtalk
Transcript class                // => TranscriptStream
Transcript respondsTo: #show:   // => true
Transcript inspect              // => "a TranscriptStream"

Beamtalk class                  // => SystemDictionary
```

### Runtime Model

Each well-known instance is a **registered actor process** (like any other actor, but with a well-known name):

```erlang
%% At bootstrap/stdlib init:
%% 1. Register the TranscriptStream class
%% 2. Spawn a singleton instance
%% 3. Register the pid under the atom 'Transcript'
{ok, Pid} = transcript_stream:spawn(),
register('Transcript', Pid)
```

Message dispatch uses the standard actor path:
```erlang
%% Transcript show: 'Hello' compiles to:
%%   async message to registered process 'Transcript'
gen_server:cast(Transcript, {message, 'show:', [<<"Hello">>]})
```

This means:
- ✅ Full dispatch through `beamtalk_dispatch:lookup/5`
- ✅ `doesNotUnderstand:` produces `#beamtalk_error{}`
- ✅ Inherits Object methods (`class`, `respondsTo:`, `describe`, `inspect`)
- ✅ Extension methods work
- ✅ Module naming follows `beamtalk_*` convention (`beamtalk_transcript_stream.erl`)
- ✅ Cascades work (`Transcript show: 'Hello'; cr; show: 'World'`)

### Codegen Change

The codegen currently special-cases `ClassReference` receivers. For well-known instances, it should instead generate actor message sends to the registered name:

```erlang
%% Current (class-level direct call):
call 'transcript':'show:'(<<"Hello">>)

%% Proposed (actor message send to registered name):
%% Same as any other actor message, but receiver is the atom 'Transcript'
call 'beamtalk_actor':'sync_send'('Transcript', 'show:', [<<"Hello">>])
```

The codegen needs a way to distinguish class method sends (`Counter spawn`) from well-known instance sends (`Transcript show:`). Options:

**Option A: Runtime-only** — All `ClassReference` sends first check if the name is a registered process. If so, send a message. If not, treat as class method call. This is the simplest but adds a runtime check on every class-level send.

**Option B: Annotation in stdlib** — The compiler knows which names are well-known instances (from stdlib definitions or a pragma). Codegen emits actor sends for those names and direct calls for others.

**Option C: Syntax distinction** — Well-known instances are lowercase (like variables) and bound at module load time. This would change `Transcript` to `transcript` which conflicts with the "uppercase = class" convention.

**Recommended: Option A** for simplicity. The registered process check (`whereis/1`) is O(1) and the cost is negligible. This also supports user-defined globals without compiler changes.

### Class Definitions

```beamtalk
// lib/TranscriptStream.bt
Object subclass: TranscriptStream
  show: value => @primitive 'show:'
  cr => @primitive 'cr'
  // Future: could add log:, clear, etc.

// lib/SystemDictionary.bt  (renamed from lib/Beamtalk.bt)
Object subclass: SystemDictionary
  allClasses => @primitive 'allClasses'
  classNamed: className => @primitive 'classNamed:'
  globals => @primitive 'globals'
  version => @primitive 'version'
```

Well-known instances are registered during bootstrap:
```erlang
%% In beamtalk_stdlib.erl or beamtalk_bootstrap.erl:
register_well_known_instances() ->
    %% Spawn singleton actors and register with well-known names
    {ok, TranscriptPid} = transcript_stream:spawn(),
    register('Transcript', TranscriptPid),
    
    {ok, BeamtalkPid} = system_dictionary:spawn(),
    register('Beamtalk', BeamtalkPid).
```

## Prior Art

### Smalltalk (Squeak/Pharo)
- `Smalltalk` is an instance of `SystemDictionary` — a real object with state
- `Transcript` is an instance of `ThreadSafeTranscript` (Pharo) — backed by a stream
- Globals are stored in the system dictionary, accessible by name
- Both respond to all Object protocol (`class`, `respondsTo:`, `inspect`, etc.)

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

### "Keep class-level methods (current approach)"
The current approach is simpler: no processes, no supervision, no message passing overhead. For stateless utilities like `Transcript show:`, a direct function call is faster than an actor message. The `undef` error issue could be fixed by adding error handling in the codegen without changing the dispatch model.

**Counter:** The performance difference is negligible (one `gen_server:cast` vs one function call), and the consistency benefit of having globals be real objects far outweighs the simplicity of the current hack. The `undef` fix would be yet another special case in already-complex codegen.

### "Newspeak-style: no globals at all"
Beamtalk's principles say "no global namespace." The purist approach is to pass `Transcript` and `Beamtalk` as module parameters, making dependencies explicit.

**Counter:** While philosophically appealing, requiring every module to declare `Transcript` as a dependency creates ceremony that hurts learnability. Beamtalk is "Smalltalk-like, not Smalltalk-compatible" — we can have well-known instances as a pragmatic convenience while still keeping the Newspeak ideal as a future migration path.

## Alternatives Considered

### Alternative A: Fix Class-Level Dispatch Only
Add error handling to class-level method calls without changing the object model:
```erlang
%% Wrap class method calls in try/catch
try transcript:'show:'(Value) catch error:undef -> ... end
```

**Rejected:** This is a band-aid. Globals still wouldn't be real objects, cascades still wouldn't work, and every new global would need a custom module with a naming collision risk.

### Alternative B: Newspeak Pure Module Parameters
Pass globals explicitly to every module:
```beamtalk
Object subclass: MyApp platform: platform
  run =>
    platform transcript show: 'Hello'
```

**Rejected for now:** Too much ceremony for the current state of the language. Could be added later as an optional pattern for dependency injection / testing.

### Alternative C: Value Type Singletons (Not Actors)
Make globals value types (maps) rather than actors:
```erlang
%% Transcript is just a tagged map
Transcript = #{'__class__' => 'TranscriptStream'}
```

**Rejected:** Value types are copied and have no shared identity. `Transcript` needs to be a single entity that all code sends messages to, especially for I/O coordination. Actors are the natural fit.

## Consequences

### Positive
- Globals become first-class objects — `class`, `respondsTo:`, `inspect`, `describe` all work
- Consistent dispatch — all messages go through `beamtalk_dispatch`, producing `#beamtalk_error{}` on failure
- Cascades work — `Transcript show: 'Hello'; cr; show: 'World'`
- Module naming is consistent — `beamtalk_transcript_stream.erl`, not `transcript.erl`
- Extensible — users can add methods to TranscriptStream via extensions
- Observable — globals are named processes visible in `observer`
- Supervisable — globals can be restarted if they crash

### Negative
- Slightly more complex bootstrap — must spawn and register singleton processes
- Actor message overhead — `gen_server:cast` vs direct function call (negligible in practice)
- More moving parts — processes can crash, need supervision
- Codegen change required — class-level sends need runtime check for registered names

### Neutral
- `lib/Beamtalk.bt` renamed to `lib/SystemDictionary.bt`
- Transcript module renamed from `transcript.erl` to `beamtalk_transcript_stream.erl`
- Class registry in `beamtalk_stdlib.erl` updated

## Implementation

### Phase 1: ADR Acceptance and Design
- Accept this ADR
- Define the well-known instance registry API

### Phase 2: Runtime Infrastructure
- Create `TranscriptStream` class with actor dispatch
- Create `SystemDictionary` class with actor dispatch
- Add well-known instance registration to bootstrap
- Supervisor tree for global processes

### Phase 3: Codegen Update
- Modify `ClassReference` dispatch to check for registered names
- Generate actor sends for well-known instances
- Keep direct calls for class methods (`Counter spawn`, `Point new`)

### Phase 4: Migration
- Rename `lib/Beamtalk.bt` → `lib/SystemDictionary.bt`
- Rename `transcript.erl` → `beamtalk_transcript_stream.erl`
- Update E2E tests and examples
- Deprecate old module names

### Affected Components
- **Codegen:** `dispatch_codegen.rs` — ClassReference dispatch logic
- **Runtime:** `beamtalk_bootstrap.erl`, `beamtalk_stdlib.erl` — registration
- **Runtime:** New `beamtalk_transcript_stream.erl`, `beamtalk_system_dictionary.erl`
- **Stdlib:** `lib/TranscriptStream.bt`, `lib/SystemDictionary.bt`
- **Tests:** E2E and unit tests for globals

## Migration Path

Existing code using `Transcript show:` and `Beamtalk allClasses` continues to work unchanged. The syntax is identical; only the dispatch path changes internally.

The current `transcript.erl` module (from BT-328) serves as the initial implementation and will be refactored into `beamtalk_transcript_stream.erl` with actor dispatch.

## References
- Related issues: BT-328 (Transcript implementation), BT-329 (Towers of Hanoi — needs Transcript)
- Related ADRs: ADR 0005 (BEAM Object Model), ADR 0006 (Unified Method Dispatch), ADR 0007 (Compilable Stdlib)
- Design principles: `docs/beamtalk-principles.md` — "Newspeak-style: no global namespace"
- Smalltalk: `SystemDictionary`, `TranscriptStream` in Squeak/Pharo
- Newspeak: Module-based capability system (no globals)

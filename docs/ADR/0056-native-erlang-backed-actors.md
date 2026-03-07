# ADR 0056: Native Erlang-Backed Actors — `native:` and `self delegate`

## Status
Proposed (2026-03-07) — Revised from initial `@native` annotation design

## Context

### The Problem

Beamtalk Actor classes today are compiled to gen_server modules by the Rust compiler. The gen_server uses `beamtalk_actor:dispatch/4` to route messages to method functions stored in the `__methods__` map at init time. This pipeline works well for Actors whose logic is entirely in Beamtalk.

However, some Actors require hand-written Erlang gen_server implementations:

- **`Subprocess`** — manages an OS port, deferred gen_server replies, and line buffering across two I/O channels. This logic cannot be expressed in Beamtalk.
- **`TranscriptStream`** — maintains a ring buffer with pub/sub subscriber management and dead-process monitoring via `handle_info/2`. These OTP patterns require direct gen_server control.

Currently, these classes use `(Erlang module)` FFI to call wrapper functions on the backing Erlang module, passing `self` explicitly. The wrapper functions extract the pid and call `gen_server:call/2` or `gen_server:cast/2` internally. This approach:

- Requires repetitive boilerplate — every method repeats `(Erlang beamtalk_module) selector: self`
- Bypasses `beamtalk_actor:sync_send/3` — missing dead actor detection, timeout handling, and consistent error wrapping
- Requires the backing Erlang module to export public wrapper functions in addition to its gen_server callbacks
- Provides no declared relationship between the `.bt` file and its backing module — the compiler has no knowledge of which Erlang module backs the Actor
- Previously used `@primitive` stubs, requiring hardcoded entries in `generated_builtins.rs` — some classes have been migrated to FFI but the `generated_builtins.rs` entries remain

### Current State

```beamtalk
// TranscriptStream.bt today — FFI wrapper calls
Actor subclass: TranscriptStream
  classState: current = nil

  class current -> TranscriptStream => self.current

  show: value :: Object -> Nil =>
    (Erlang beamtalk_transcript_stream) show: self value: value
  cr -> Nil => (Erlang beamtalk_transcript_stream) cr: self
  subscribe -> Nil => (Erlang beamtalk_transcript_stream) subscribe: self
  unsubscribe -> Nil => (Erlang beamtalk_transcript_stream) unsubscribe: self
  recent -> List => (Erlang beamtalk_transcript_stream) recent: self
  clear -> Nil => (Erlang beamtalk_transcript_stream) clear: self
```

The backing `beamtalk_transcript_stream.erl` implements the full `gen_server` behaviour with `handle_call/3` using the `{Selector, [Args]}` wire protocol. It also exports public wrapper functions (`'show:value:'/2`, `'cr:'/1`, etc.) that extract the pid and forward to `gen_server:call`. These wrapper functions exist solely because the FFI path calls them directly rather than going through `sync_send`.

Similarly for `Subprocess`:

```beamtalk
// Subprocess.bt today — FFI wrapper calls
Actor subclass: Subprocess
  writeLine: data -> Nil =>
    (Erlang beamtalk_subprocess) 'writeLine:': self data: data
  readLine -> Object =>
    (Erlang beamtalk_subprocess) readLine: self
  ...
```

### Constraints

1. **Hand-written logic must stay in Erlang** — line buffering, deferred replies, port management, ring buffers, and pub/sub require direct OTP gen_server control
2. **No `state:` declarations** — instance state lives entirely in the gen_server; the `.bt` file declares the Beamtalk API, not the internal state. (`classState:` for class-level state is fine — it's independent of the gen_server)
3. **Wire protocol compatibility** — sync requests use `{Selector, [Args]}` matching `beamtalk_actor:sync_send/3` via `gen:call/4`. For `gen_server` backing modules this means `handle_call/3`; for `gen_statem` backing modules this means `{call, From}` events in state callbacks
4. **Works with gen_server and gen_statem** — both route through `gen:call/4`, so a single dispatch path covers both OTP behaviour types
5. **Explicit module naming** — the `.bt` file must declare which Erlang module it is backed by; implicit discovery is not acceptable
6. **Open to library authors** — any library author must be able to back an Actor with a hand-written gen_server without modifying the Rust compiler
7. **ClassBuilder integration** — the mechanism must fit the ClassBuilder protocol (ADR 0038), not introduce a parallel annotation system
8. **Static compiler visibility** — the backing module name must be visible to the Rust compiler at parse time (no runtime-only class methods like Pharo's `ffiLibraryName`, since the compiler is not running inside the image)

### Wire Protocol

`beamtalk_actor:sync_send/3` — used for all Beamtalk Actor message sends — dispatches via:

```erlang
gen_server:call(ActorPid, {Selector, Args})
```

where `Selector` is an atom and `Args` is a list. Generated actors wrap replies as `{ok, Result}` or `{error, Error}` per BT-918. However, `sync_send/3` also has a backward-compatibility `DirectValue` fallback (beamtalk_actor.erl:435) that passes through unwrapped values — this is how `beamtalk_subprocess.erl` and `beamtalk_transcript_stream.erl` work today.

For **async** methods, `beamtalk_actor:cast_send/3` sends `gen_server:cast(Pid, {cast, Selector, Args})`. Some backing gen_servers (e.g. TranscriptStream's `show:`, `cr`) use casts for non-blocking semantics.

The current FFI approach **bypasses** `sync_send/3` — the wrapper functions call `gen_server:call` directly. This means dead actor detection, timeout handling, and consistent `#beamtalk_error{}` wrapping are missing from these classes. This ADR fixes that.

## Decision

### `native:` Keyword on `subclass:`

An `Actor subclass:` may include a `native:` keyword to declare that its gen_server implementation is provided by the named Erlang module rather than generated by the compiler. This is a keyword argument on the `subclass:` message, not a class-level annotation — it integrates with the ClassBuilder protocol (ADR 0038).

```beamtalk
Actor subclass: Subprocess native: beamtalk_subprocess

  /// Convenience factory — open a subprocess with command and args.
  class open: command args: args =>
    self spawnWith: #{"command" => command, "args" => args}

  /// Convenience factory — open a subprocess with environment and working directory.
  class open: command args: args env: env dir: dir =>
    self spawnWith: #{
      "command" => command,
      "args" => args,
      "env" => env,
      "dir" => dir
    }

  /// Write a line to the subprocess's stdin.
  writeLine: data -> Nil => self delegate

  /// Read one line from stdout. Blocks until a line is available.
  readLine -> Object => self delegate

  /// Read one line from stdout with a timeout in milliseconds.
  readLine: timeout -> Object => self delegate

  /// Read one line from stderr.
  readStderrLine -> Object => self delegate

  /// Read one line from stderr with a timeout.
  readStderrLine: timeout -> Object => self delegate

  /// Return a Stream of stdout lines.
  lines -> Stream => self delegate

  /// Return a Stream of stderr lines.
  stderrLines -> Stream => self delegate

  /// Get the exit code. Returns nil if still running.
  exitCode -> Object => self delegate

  /// Force-close the subprocess.
  close -> Nil => self delegate
```

### `self delegate` — Pharo `ffiCall:` Pattern

Method bodies that are `=> self delegate` are **delegation declarations** — the compiler generates a facade that forwards the message to the backing gen_server via `beamtalk_actor:sync_send/3`. Methods with full Beamtalk bodies (like `open:args:`) compile normally.

`delegate` is a real `sealed` method defined on `Actor`, following Pharo's `ffiCall:` pattern:

```beamtalk
// Actor.bt
/// Delegate to the native backing module.
/// The compiler transforms this call on native: classes.
/// Calling on a non-native Actor raises an error.
/// Sealed to prevent subclasses from shadowing the name.
sealed delegate =>
  Error signal: "delegate called on a non-native Actor"
```

The method is `sealed` to prevent user-defined Actor subclasses from accidentally shadowing it with a business-logic method (e.g. a `DelegationManager` actor). Since `delegate` is a compiler-recognized pattern, shadowing it would silently break the `native:` mechanism.

**Compiler recognition:** Only the literal unary send `self delegate` as the entire method body is recognized and transformed. Indirect forms (`x := self. x delegate`, `self perform: #delegate`) are not recognized — they compile normally and hit the `sealed delegate` fallback, which raises an error. The compiler or LSP should warn if a `native:` class has a method body that is neither `self delegate` nor a full Beamtalk expression.

**Reflection:** `delegate` appears in `Actor localMethods` and `respondsTo: #delegate` returns `true` for all Actors. This is a visible implementation detail but consistent with Pharo's `ffiCall:` appearing in Object's method dictionary. The LSP should exclude `delegate` from autocompletion suggestions for non-`native:` actors.

The compiler recognizes `self delegate` in the AST of a `native:` class and transforms it into a `sync_send` facade call. This is the same pattern as Pharo's `ffiCall:` — a real method exists as a safety net, but the compiler intercepts and replaces it:

| Pharo | Beamtalk |
|-------|----------|
| `ffiLibraryName` on class | `native: module` on `subclass:` |
| `self ffiCall: #(...)` in method | `self delegate` in method |
| Compiler generates C trampoline | Compiler generates `sync_send` facade |
| `primitiveFailed` fallback | `Error signal:` fallback |

Unlike Pharo's `ffiLibraryName` (a class-side method), Beamtalk uses `native:` as a keyword on `subclass:` because the Rust compiler needs this information at parse time — there is no live image to query during compilation.

### ClassBuilder Integration

`native:` is a keyword argument on `subclass:` that flows through the ClassBuilder protocol (ADR 0038). ClassBuilder gains a `native:` method:

```beamtalk
// ClassBuilder.bt — added
/// Set the backing Erlang module for native delegation.
native: anErlangModule =>
  backingModule := anErlangModule
```

The codegen cascade for a `native:` class:

```erlang
%% Generated module init
CB = send('bt@object':'module_class'(), 'classBuilder', []),
_ = send(CB, 'name:', ['Subprocess']),
_ = send(CB, 'native:', [beamtalk_subprocess]),
_ = send(CB, 'methods:', [#{'writeLine:' => fun ?MODULE:'dispatch_writeLine:'/2,
                             readLine     => fun ?MODULE:'dispatch_readLine'/1,
                             ...}]),
send(CB, 'register', []).
```

This is the same ClassBuilder cascade pattern used for all class creation — `native:` is just another setter, like `name:`, `fields:`, `methods:`, and `supervisionPolicy:` (ADR 0059 for supervisors).

### What the Compiler Generates

For a `native:` class, the compiler generates a **facade module** (`bt@stdlib@subprocess`) instead of a full gen_server module. The facade:

1. **`spawn/1`** — calls `BackingModule:start_link(Config)` and wraps the result:

```erlang
%% Generated facade — bt@stdlib@subprocess (Erlang source notation for clarity)
'spawn'/1 = fun(Config) ->
    case beamtalk_subprocess:start_link(Config) of
        {ok, Pid} ->
            {'beamtalk_object', 'Subprocess', 'bt@stdlib@subprocess', Pid};
        {error, Reason} ->
            %% raise beamtalk instantiation_error
            ...
    end
```

2. **`spawnWith:`** (class method) — passes the config dictionary to `spawn/1`

3. **`has_method/1`** — returns `true` for all selectors declared with `=> self delegate` or any Beamtalk body

4. **Dispatch functions** (for `self delegate` methods) — delegate via `beamtalk_actor:sync_send/3`:

```erlang
%% Generated: writeLine: data dispatch
'dispatch_writeLine:'/2 = fun(Data, Self) ->
    Pid = element(4, Self),
    beamtalk_actor:sync_send(Pid, 'writeLine:', [Data])
```

5. **Class-side method bodies** (like `open:args:`) compile to normal Beamtalk codegen — they call `self spawnWith:` which invokes the generated `spawnWith:` class method.

### Backing Gen_Server Protocol

The hand-written gen_server module must implement:

```erlang
%% start_link/1 — called by the generated facade's spawn/1
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) -> gen_server:start_link(?MODULE, Config, []).

%% handle_call/3 — uses {Selector, [Args]} wire format
handle_call({'writeLine:', [Data]}, _From, State) ->
    NewState = do_writeLine(Data, State),
    {reply, nil, NewState};
handle_call({readLine, []}, From, State) ->
    %% Deferred reply: gen_server:reply(From, Line) called later
    {noreply, register_waiter(stdout, From, State)};
handle_call({exitCode, []}, _From, State) ->
    {reply, maps:get(exit_code, State, nil), State};
```

**Reply format:** `sync_send/3` prefers `{ok, Result}` / `{error, Error}` wrapped replies (per BT-918) but also supports a `DirectValue` fallback for backward compatibility. Existing hand-written gen_servers return raw values today and work correctly via the fallback path.

**New `native:` gen_servers MUST use `{ok, Result}` wrapping.** The `DirectValue` fallback cannot distinguish a legitimate return value of `{error, Reason}` from an actual error — if a backing gen_server returns `{error, <<"not found">>}` as a value (e.g. a database query result), `sync_send/3` will misinterpret it as an error and raise a `#beamtalk_error{}`. Existing stdlib gen_servers (`beamtalk_subprocess.erl`, `beamtalk_transcript_stream.erl`) are grandfathered via the `DirectValue` fallback because they do not return `{error, _}` tuples as values; they should be migrated to `{ok, Result}` wrapping as part of Phase 2. The stub generator produces `{ok, Result}` wrapping by default.

**Async (cast) methods:** When called with `!`, `cast_send/3` sends `{cast, Selector, Args}` to the backing gen_server's `handle_cast/2`. Backing gen_servers that support fire-and-forget semantics (e.g. TranscriptStream's `show:`) implement `handle_cast({cast, 'show:', [Value]}, State)` alongside the standard `handle_call` clause. Both call and cast paths are generated by the facade — no method-level annotation is required. Library authors who want a selector callable via both `.` (sync) and `!` (async) should implement both `handle_call` and `handle_cast` clauses for that selector. The stub generator generates `handle_call` clauses only — add `handle_cast` manually for selectors that support fire-and-forget.

**Other gen_server callbacks:** The facade does NOT intercept `handle_info/2`, `terminate/2`, or `code_change/3`. These callbacks are implemented directly by the backing gen_server. The facade only generates `spawn/1`, `spawnWith:`, `has_method/1`, and dispatch functions.

**`start_link` arity:** The facade always calls `BackingModule:start_link(Config)` where Config is the `spawnWith:` dictionary. When `spawn` is called without arguments, Config is `#{}` (empty map). Backing modules MUST export `start_link/1` accepting a map. There is no fallback to `start_link/0` — a single arity simplifies the contract.

**`start_link` failure:** If `start_link/1` returns `{error, Reason}`, the facade raises a `#beamtalk_error{type => instantiation_error}` with the original Reason preserved in the error metadata. If `start_link/1` crashes (throws an exception), the exception propagates to the caller as a `#beamtalk_error{type => instantiation_error, data => #{reason => CrashReason}}`.

### Stub Generation

Because the `.bt` file fully describes the Actor's API — selectors, arities, and return types — tooling can auto-generate a skeleton gen_server:

```
$ beamtalk gen-native MyActor
```

```erlang
%% Generated from MyActor.bt — fill in implementations
-module(my_library_actor).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3]).

start_link(Config) -> gen_server:start_link(?MODULE, Config, []).
init(Config) -> {ok, #{}}. %% TODO: initialise state from Config

handle_call({'doWork:', [Task]}, _From, State) ->
    {reply, {ok, todo}, State}; %% TODO: implement doWork:
handle_call({status, []}, _From, State) ->
    {reply, {ok, todo}, State}. %% TODO: implement status
```

Generated once, developer fills in the bodies. No regeneration, no mixed generated/hand-written code. The LSP could also flag mismatches: "MyActor declares `doWork:` but `my_library_actor.erl` has no matching `handle_call` clause."

### State Exclusivity

**`native:` Actors MUST NOT declare `state:` fields.** The gen_server owns all instance state — it is opaque to the Beamtalk compiler. If a `state:` declaration is found on a `native:` Actor, the compiler raises an error:

```text
error: native actor 'Subprocess' cannot declare state fields — state is owned by the backing gen_server 'beamtalk_subprocess'
```

**`classState:` is permitted** — class-level state (e.g. `TranscriptStream`'s singleton `current`) is independent of the gen_server instance state.

### gen_server vs gen_statem

Both `gen_server` and `gen_statem` expose the same `gen:call/4` API internally. `beamtalk_actor:sync_send/3` uses `gen_server:call/2` which routes through `gen:call/4` for both behaviour types, so a `gen_statem`-backed module receives the call correctly.

However, `gen_statem` does **not** have a `handle_call/3` callback — synchronous calls arrive as `{call, From}` events in state callbacks:

```erlang
%% gen_statem handle_event_function mode
handle_event({call, From}, {'readLine', []}, State, Data) ->
    {keep_state, Data, [{reply, From, read_line(Data)}]};
handle_event({call, From}, {'exitCode', []}, State, Data) ->
    {keep_state, Data, [{reply, From, maps:get(exit_code, Data, nil)}]}.
```

### Sync and Async Dispatch

Per ADR 0043, the call site determines dispatch mode — the `.bt` file does not need to declare it:

- `agent selector.` → `sync_send/3` → `gen_server:call(Pid, {Selector, [Args]})`
- `agent selector!` → `cast_send/3` → `gen_server:cast(Pid, {cast, Selector, Args})`

The backing gen_server implements `handle_call/3` for selectors that must return values, `handle_cast/2` for fire-and-forget selectors, or both. No method-level sync/async annotation is required.

**Self-sends:** `native:` actors with Beamtalk method bodies should not call their own `self delegate` methods via `self` synchronously — this causes a gen_server deadlock, the same constraint as any gen_server. This is existing OTP behaviour, not a new constraint.

### Ports, NIFs, and Raw Processes

`native:` is scoped to gen_server-compatible OTP processes. For actors backed by ports, NIFs, or raw processes, use per-method `(Erlang module)` FFI (ADR 0028) instead:

```beamtalk
// NIF-backed class — use FFI per method, not native:
sealed Object subclass: NativeAccelerator
  class compute: data -> Object =>
    (Erlang nif_accelerator) compute: data
```

### Complete Example — TranscriptStream

```beamtalk
/// TranscriptStream — Per-workspace shared log with pub/sub semantics.
Actor subclass: TranscriptStream native: beamtalk_transcript_stream
  classState: current = nil

  /// Return the current singleton instance.
  class current -> TranscriptStream => self.current

  /// Set the current singleton instance.
  class current: instance :: TranscriptStream -> Nil => self.current := instance

  /// Clear the current singleton instance.
  class resetCurrent -> Nil => self.current := nil

  /// Write a value to the transcript.
  show: value :: Object -> Nil => self delegate

  /// Write a newline to the transcript.
  cr -> Nil => self delegate

  /// Subscribe the calling process to receive transcript output.
  subscribe -> Nil => self delegate

  /// Unsubscribe the calling process from transcript output.
  unsubscribe -> Nil => self delegate

  /// Return recent transcript entries as a list.
  recent -> List => self delegate

  /// Clear the transcript buffer.
  clear -> Nil => self delegate
```

### REPL Example

```beamtalk
agent := Subprocess open: "echo" args: #("hello")
line := agent readLine.   // => "hello"
agent exitCode.           // => 0
agent close.

// Streaming lines
(Subprocess open: "ls" args: #("-la")) lines do: [:line | Transcript show: line]
```

### Error Examples

```beamtalk
// native: with state: raises a compile error
Actor subclass: Broken native: some_module
  state: count = 0   // => compile error
```

```text
error: native actor 'Broken' cannot declare state fields — state is owned by the backing gen_server 'some_module'
  --> Broken.bt:2
  |
2 |   state: count = 0
  |   ^^^^^^^^^^^^^^^^
  |
  = help: remove state declarations from native actors; state lives in the gen_server
```

```beamtalk
// self delegate on a non-native Actor raises a runtime error
Actor subclass: NotNative
  doStuff => self delegate

NotNative spawn doStuff
// => Error: delegate called on a non-native Actor
```

### Library Author Example

Any library author can create a native-backed Actor without modifying the compiler:

```beamtalk
// my_library/src/DatabasePool.bt
Actor subclass: DatabasePool native: my_db_pool

  class connect: config => self spawnWith: config

  query: sql -> List => self delegate
  query: sql params: params -> List => self delegate
  transaction: block -> Object => self delegate
  close -> Nil => self delegate
```

```erlang
%% my_db_pool.erl — standard gen_server
-module(my_db_pool).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3]).

start_link(Config) -> gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    {ok, Conn} = connect_db(Config),
    {ok, #{conn => Conn}}.

handle_call({'query:', [SQL]}, _From, #{conn := Conn} = State) ->
    {reply, {ok, execute(Conn, SQL, [])}, State};
handle_call({'query:params:', [SQL, Params]}, _From, #{conn := Conn} = State) ->
    {reply, {ok, execute(Conn, SQL, Params)}, State};
handle_call({'transaction:', [Block]}, _From, #{conn := Conn} = State) ->
    Result = run_transaction(Conn, Block),
    {reply, {ok, Result}, State};
handle_call({close, []}, _From, #{conn := Conn} = State) ->
    close_db(Conn),
    {reply, {ok, nil}, State}.
```

## Prior Art

### Pharo — `ffiCall:` and `<primitive: N>`

Pharo's UFFI allows method bodies to declare `ffiCall:` — a message send that the compiler intercepts and transforms into a foreign function call. The method body looks like a normal message send, but the compiler generates native call trampolines. `ffiCall:` is defined as a real method on Object (returning `self primitiveFailed`), so calling it without compiler support produces a clear error. The library is named at the class level via `ffiLibraryName`.

**What we adopted:** The `ffiCall:` pattern directly — `self delegate` is a message send the compiler recognizes and transforms. The real method on Actor provides a runtime safety net, just as `ffiCall:` falls back to `primitiveFailed`. The class-level module declaration (`native:` on `subclass:`) mirrors `ffiLibraryName`.

**What doesn't translate:** Pharo's `ffiLibraryName` is a class-side method evaluated at compile time because Pharo's compiler runs inside the live image. Beamtalk's Rust compiler has no image to query, so the backing module must be declared syntactically via the `native:` keyword — visible to the parser at compile time.

### Elixir — `use GenServer` and Bare Module Wrappers

Elixir `GenServer` behaviour expects `handle_call/3` with `{reply, Result, State}`. Wrapping an existing Erlang gen_server in Elixir requires explicit delegation in `handle_call/3` bodies — there is no annotation to say "this module backs a GenServer; generate the delegation for me." Library authors hand-write every delegation clause.

**What we adopted:** The per-selector `{Selector, [Args]}` wire protocol mirrors Elixir's `handle_call/3` pattern matching style.

**What we improved:** `self delegate` generates the delegation facade automatically from the `.bt` declaration. Elixir has no equivalent of the compiler generating `handle_call` delegation clauses from a type declaration.

### Gleam — `@external` per Function

Gleam binds each function individually to an Erlang MFA using `@external(erlang, "module", "function")`. There is no class-level or actor-level annotation. Gleam has no actor or gen_server concept in the language.

**What we adopted:** The concept of explicit module naming — `native: beamtalk_subprocess` names the exact Erlang module, analogous to `@external(erlang, "beamtalk_subprocess", "function")`.

**What we rejected:** Per-function MFA binding for Actor methods. The `native:` keyword on `subclass:` names the module once; `self delegate` in each method is minimal and uniform. Per-method module/function binding is unnecessary since the `{Selector, [Args]}` wire protocol means the selector name IS the function routing key.

### Erlang — `gen_server` Wrapper Pattern

Standard Erlang practice is to write thin wrapper modules with `start_link/N`, `stop/1`, and per-method delegation functions that call `gen_server:call(Pid, {selector, args})`. No annotation or code generation exists.

**What we adopted:** The `{Selector, [Args]}` tuple as the message format — this is idiomatic Erlang gen_server call protocol.

**What we improved:** The generated facade eliminates boilerplate delegation functions and ensures `beamtalk_actor:sync_send/3` lifecycle semantics (dead actor detection, timeout handling) are applied consistently.

### Smalltalk — `<primitive: N>` and `subclassResponsibility`

Smalltalk's `<primitive: N>` pragma is metadata inside the method body — the method always has a body, never bodyless. Abstract methods use `self subclassResponsibility` as the method body. Both patterns are explicit method bodies, not absent bodies.

**What we adopted:** The principle that every method has a body. `self delegate` is a method body, not an absent body. This follows Smalltalk convention where the body always expresses the method's intent — delegation, abstraction, or implementation.

## User Impact

### Newcomer (coming from Python/JS)

`native:` on a class declaration is a recognizable "this is backed by Erlang" signal. `self delegate` reads naturally as "delegate this to the native implementation." Class factory methods (`open:args:`) remain pure Beamtalk, so newcomers interact with a normal API. The `(Erlang module)` FFI they may have seen elsewhere is conceptually related — `native:` is the Actor-specific version that routes through the gen_server protocol.

### Smalltalk Developer

`self delegate` follows the Smalltalk pattern that every method has a body expressing its intent — analogous to `<primitive: N>` in Pharo. The `native:` keyword on `subclass:` is a natural extension of the keyword message pattern they already know from `Actor subclass: Name`. From a usage perspective, `agent writeLine: "hello"` is still a message send — the delegation is invisible at the call site.

### Erlang/BEAM Developer

`native:` maps directly to the standard Erlang gen_server wrapper pattern they already know. The `{Selector, [Args]}` wire format is the main constraint — documented and consistent across all Beamtalk Actor messaging. The stub generation tool (`beamtalk gen-native`) scaffolds the `handle_call/3` clauses from the `.bt` declaration. `gen_statem` compatibility means existing state machines can be wrapped without behavioural changes.

### Production Operator

`native:` actors behave identically to generated actors from an OTP perspective — they are gen_server processes under the standard supervision tree. `observer:start()`, `:sys.get_state/1`, and standard tracing tools work. The backing gen_server module can implement `code_change/3` for hot reload state migration independently of the `.bt` facade. Moving to `sync_send` adds dead actor detection and timeout handling that the previous FFI approach was missing.

### Tooling Developer (LSP/IDE)

`native: BackingModule` on the `subclass:` line gives the LSP an explicit module to link to for "go to implementation." `self delegate` methods in the IDE can offer "open backing Erlang file" navigation. Since the `.bt` file declares all selectors with types, completion and hover documentation work without understanding the gen_server internals. The LSP could flag mismatches between declared selectors and `handle_call` clauses in the backing module.

## Steelman Analysis

### Steelman for keeping `(Erlang module)` FFI for all Actor methods (current state)

- **Newcomer:** "I already know `(Erlang module)` FFI from Value classes. One mechanism for all Erlang interop is simpler than learning `native:` and `self delegate` as an Actor-specific variant."
- **BEAM veteran:** "The FFI path is explicit — I can see exactly which Erlang function is being called in each method. `self delegate` hides the dispatch path behind a compiler transformation."
- **Language designer:** "`native:` adds a new keyword on `subclass:`, a new ClassBuilder method, and a compiler pattern-match on `self delegate`. The FFI approach requires no new concepts."
- **Tension:** The implementation cost is real — a new codegen path for a small number of classes. However, the FFI approach bypasses `sync_send` (missing dead actor detection and error wrapping), requires boilerplate wrapper functions in Erlang, and doesn't scale to library authors who want consistent Actor semantics.

### Steelman for class-level `@native` annotation (previous draft of this ADR)

- **Language designer:** "`@native BackingModule` as a class annotation is familiar from Gleam's `@external`. It's explicit, grep-able, and doesn't require integrating with ClassBuilder."
- **BEAM veteran:** "Annotations are a well-understood pattern. Adding a keyword to `subclass:` changes the grammar of the most important declaration in the language."
- **Tension:** `@native` introduces a class-level annotation syntax that sits outside Beamtalk's message-send model. `native:` as a keyword on `subclass:` integrates with ClassBuilder and is consistent with `strategy:` for supervisors (ADR 0059) — it's a message argument, not an annotation.

### Steelman for bodyless methods instead of `self delegate`

- **Newcomer:** "Why do I need to write `=> self delegate` nine times? The class says `native:` — isn't that enough? Bodyless methods would be cleaner."
- **Language designer:** "Bodyless methods are more concise. The `native:` keyword on the class already provides all the context needed."
- **Smalltalk purist:** "Smalltalk methods always have a body — `self subclassResponsibility`, `<primitive: N>`, or actual code. A bodyless method is not Smalltalk. But `self delegate` IS Smalltalk — it's a message send in the method body that expresses intent."
- **Tension:** Conciseness vs explicitness. `self delegate` is slightly more verbose but follows Smalltalk convention, provides a runtime safety net (the real method on Actor), and doesn't overload "no body" which could conflict with future abstract class support or simply look like an incomplete method.

### Tension Points

- BEAM veterans prefer explicit FFI; Smalltalk developers prefer `self delegate` as a message-send pattern
- Newcomers want fewer concepts; library authors need the `native:` mechanism to avoid compiler modifications
- The stub generation tool and LSP mismatch detection reduce the cost of the `{Selector, [Args]}` convention for Erlang authors

## Alternatives Considered

### Per-Method `(Erlang module)` FFI for All Actor Methods (Current State)

Keep using `(Erlang beamtalk_subprocess) 'writeLine:': self data: data` in each method body:

```beamtalk
Actor subclass: Subprocess
  writeLine: data -> Nil =>
    (Erlang beamtalk_subprocess) 'writeLine:': self data: data
  readLine -> Object =>
    (Erlang beamtalk_subprocess) readLine: self
```

Rejected because: bypasses `sync_send` (missing dead actor detection, timeout handling, error wrapping), requires public wrapper functions in the Erlang module, is repetitive (module name repeated in every method), and doesn't use the Actor message-passing protocol. Library authors must manually handle pid extraction and error translation.

### `@native BackingModule` Class Annotation (Previous Draft)

Annotate the class with `@native` above the `subclass:` declaration:

```beamtalk
@native beamtalk_subprocess
Actor subclass: Subprocess
  writeLine: data -> Nil => @native
```

Rejected because: introduces an annotation syntax (`@`) outside Beamtalk's message-send model, doesn't integrate with the ClassBuilder protocol, and is inconsistent with how other class metadata (like `supervisionPolicy:` for supervisors) uses keyword arguments on `subclass:`. The `@native` on both class and method was "two annotation types to learn."

### Bodyless Methods on `native:` Classes

Methods without bodies on a `native:` class implicitly delegate:

```beamtalk
Actor subclass: Subprocess native: beamtalk_subprocess
  writeLine: data -> Nil
  readLine -> Object
```

Rejected because: violates Smalltalk convention that every method has a body (`self subclassResponsibility`, `<primitive: N>`, or real code); overloads "no body" syntax which will conflict with future abstract class support (where bodyless methods mean "subclass must implement" — a fundamentally different intent from "delegate to backing gen_server"); provides no runtime safety net if called incorrectly; and makes methods look incomplete to readers unfamiliar with the `native:` class context. `self delegate` is explicit and follows the Pharo `ffiCall:` precedent.

### `(native)` Shorthand FFI Syntax

Use the FFI syntax with `native` as a backreference to the class-level module:

```beamtalk
writeLine: data -> Nil => (native) 'writeLine:': data
```

Rejected because: `native` in `(native)` is syntactically ambiguous — it's not a module name, not a variable, and introduces a new meaning for the `(Erlang module)` syntax. `self delegate` is a clean message send that the compiler transforms, with no new syntax forms.

### Auto-Generate Backing Gen_Server Boilerplate

Generate the `handle_call/3` clauses from the `.bt` declarations, with user-fillable implementation bodies:

Rejected as the primary mechanism because generated code mixed with hand-written code creates maintenance problems — regeneration overwrites customizations. However, a **one-shot scaffold** tool (`beamtalk gen-native`) is included as a convenience — generated once, developer owns the result, no regeneration.

### Direct `sync_send` via Existing FFI (No Compiler Changes)

Route through `sync_send` directly in each method body using the existing `(Erlang module)` FFI:

```beamtalk
Actor subclass: Subprocess
  writeLine: data -> Nil =>
    (Erlang beamtalk_actor) syncSend: self selector: #'writeLine:' args: #(data)
  readLine -> Object =>
    (Erlang beamtalk_actor) syncSend: self selector: #readLine args: #()
```

This achieves the primary goal (routing through `sync_send` for consistent error handling) with zero compiler changes. Rejected because: it is verbose (every method repeats the `syncSend:selector:args:` boilerplate with manual selector and argument packing), error-prone (selector names as symbols must match exactly), provides no declared relationship between the class and its backing module, offers no path for library authors to avoid the boilerplate, and cannot support stub generation or LSP mismatch detection. The verbosity is worse than the current FFI wrapper approach. However, this approach could serve as an interim fix for the error handling gap before Phase 1 lands.

### `NativeActor` Superclass

Introduce a `NativeActor` subclass of `Actor` that owns `delegate`, keeping Actor's namespace clean:

```beamtalk
NativeActor subclass: Subprocess backing: beamtalk_subprocess
  writeLine: data -> Nil => self delegate
```

Rejected because: sentinel methods on the base class are the universal Smalltalk pattern — `subclassResponsibility`, `primitiveFailed`, `ffiCall:`, `shouldNotImplement`, and `doesNotUnderstand:` all live on Object, not on purpose-built subclasses. Creating a `NativeActor` class to own one `sealed` method fragments the hierarchy unnecessarily, adds a new class to the bootstrap sequence, and forces library authors to know about `NativeActor` as a distinct superclass. No Smalltalk derivative has introduced sentinel classes for this pattern — the convention is a method on the base class, available everywhere, used by intent.

### `@primitive` with Module Declaration

Extend `@primitive` to accept a module name: `@primitive beamtalk_subprocess "writeLine:"`:

Rejected because it conflates two different mechanisms (BIF-level primitives and gen_server delegation), complicates the `@primitive` narrowing from ADR 0055, and still requires per-method repetition of the module name.

## Consequences

### Positive

- **ClassBuilder integration:** `native:` is a keyword argument on `subclass:`, flowing through the same ClassBuilder cascade as `name:`, `fields:`, `methods:`, and `supervisionPolicy:` — no separate annotation system
- **Consistent error handling:** `self delegate` routes through `sync_send/3`, adding dead actor detection, timeout handling, and `#beamtalk_error{}` wrapping that the current FFI approach is missing
- **Open to library authors:** Any library author can back an Actor with a hand-written gen_server by adding `native: module` to their `subclass:` declaration — no compiler modifications required
- **Stub generation:** The `.bt` declaration contains enough information to auto-generate skeleton `handle_call/3` clauses for the backing gen_server
- **`@primitive` scope further narrowed:** Subprocess and TranscriptStream no longer use `@primitive` or FFI wrappers for module-backed delegation
- **Erlang module cleanup:** Public wrapper functions and `has_method/1`/`dispatch/3` exports can be removed from backing gen_server modules — only standard gen_server callbacks remain
- **LSP navigation:** `native: BackingModule` gives the LSP an explicit link to the Erlang implementation; mismatch detection between declared selectors and `handle_call` clauses is possible
- **Smalltalk idiom:** `self delegate` follows the Pharo `ffiCall:` pattern — every method has a body, the body expresses intent, and a runtime fallback exists
- **`gen_statem` supported:** Both `gen_server` and `gen_statem` route through `gen:call/4`, so `native:` works transparently with either

### Negative

- **New codegen path:** The compiler needs facade generation for `native:` classes — `spawn/1`, `has_method/1`, and dispatch functions that route through `sync_send/3`
- **Compiler pattern recognition:** The compiler must detect `self delegate` in the AST and transform it — a new form of compiler-recognized message pattern
- **`generated_builtins.rs` migration:** Subprocess and TranscriptStream must be moved from `generated_builtins.rs` to be parsed from their `.bt` files with the `native:` keyword
- **Constraint on gen_server API:** Backing gen_servers are constrained to the `{Selector, [Args]}` wire protocol — Erlang authors accustomed to arbitrary message formats must adapt
- **`self delegate` repetition:** Each delegating method says `=> self delegate` — more verbose than bodyless methods, but explicit
- **`delegate` namespace occupation:** Adding `sealed delegate` to Actor means every Actor subclass responds to `delegate`. The name is reserved — a library author cannot use `delegate` as a business-logic selector on an Actor. The `sealed` modifier prevents accidental shadowing but the name is consumed
- **Gradual typing interaction:** When the type checker (ADR 0025) validates return types, `self delegate` methods return whatever the gen_server returns — opaque to the type checker. The type checker will need a special case: on `native:` classes, `self delegate` is assumed to return the method's declared return type. This is analogous to how `@primitive` return types are trusted today
- **Parser grammar extension:** Adding `native:` as an optional keyword argument after the class name in `subclass:` is a grammar change with no current precedent — the same extension needed for `supervisionPolicy:` on `Supervisor subclass:` (ADR 0059). These should be designed together as a general "keyword arguments on `subclass:`" parser feature

### Neutral

- `spawn/0` behaviour: for `native:` actors, `spawn/1` is the primary entry point (since `spawnWith:` passes config). The `spawn/0` form raises an error if the backing `start_link/0` doesn't exist
- `code_change/3` hot reload support lives entirely in the backing gen_server — no changes to the Beamtalk compiler's hot reload machinery
- `classState:` is permitted on `native:` actors — only instance `state:` is prohibited
- **Hot code reload:** Reloading a `native:` class's facade module updates the dispatch functions but does not disrupt existing actor processes — the pid identity is stable, and callers continue reaching the same gen_server. The backing gen_server's own `code_change/3` handles state migration independently
- **REPL limitation:** `native:` requires a pre-existing compiled Erlang gen_server module on the code path. It is not suitable for interactive class definition at the REPL — the backing module must exist before the `native:` class can be loaded. Attempting to spawn a `native:` actor whose backing module doesn't exist produces `instantiation_error: module not found`

## Implementation

### Phase 0 — Hand-Written Facade (80% Solution, No Compiler Changes)

**Status: partially complete.** `beamtalk_subprocess.erl` (hand-written gen_server), `Subprocess.bt` (FFI stubs), and the `generated_builtins.rs` entry are already in place and all tests pass.

The primary remaining Phase 0 deliverable is documentation: write and publish the facade shape (`spawn/1`, `has_method/1`, dispatch functions) as the library author protocol so external authors can hand-write their own facades today, before Phase 1 ships.

**Note:** Removing entries from `generated_builtins.rs` requires Phase 1's `native:` keyword support so the compiler parses class metadata from the `.bt` file.

**Issues:** BT-1155

### Phase 1 — Compiler `native:` Support

- Parse `native:` as a keyword argument on `subclass:` in the class definition grammar
- Store the backing module name on the `ClassDefinition` AST node
- Add `native:` method to ClassBuilder (both `.bt` and Erlang backing)
- Recognize `self delegate` in the AST of `native:` classes → generate `sync_send` facade dispatch
- Define `delegate` method on `Actor.bt` with error fallback
- Validate: `state:` declarations on `native:` actors produce a compile error
- Generate facade module: `spawn/1`, `spawnWith:`, `has_method/1`, dispatch functions for `self delegate` methods
- Full Beamtalk method bodies on `native:` actors compile normally (e.g. `open:args:`)
- Remove `Subprocess` and `TranscriptStream` from `generated_builtins.rs`

**Issues:** BT-1156, BT-1157

### Phase 2 — Stdlib Migration

- Migrate `Subprocess.bt` from FFI wrappers to `native: beamtalk_subprocess` with `self delegate`
- Migrate `TranscriptStream.bt` from FFI wrappers to `native: beamtalk_transcript_stream` with `self delegate`
- Remove public wrapper functions from `beamtalk_subprocess.erl` and `beamtalk_transcript_stream.erl` — retain only gen_server callbacks
- Remove `has_method/1` and `dispatch/3` exports from both modules
- Add integration tests covering `native:` spawn, sync dispatch, async/cast dispatch, deferred replies, and error propagation

**Issues:** BT-1158, BT-1159

### Phase 3 — Tooling

- Implement `beamtalk gen-native` stub generation from `.bt` declarations
- LSP: "go to Erlang implementation" for `self delegate` methods
- LSP: mismatch detection between declared selectors and `handle_call` clauses

### Affected Components

- `crates/beamtalk-core/src/source_analysis/lexer.rs` — no changes needed; `native` is not a keyword, it's a keyword-argument identifier in the `subclass:` message
- `crates/beamtalk-core/src/source_analysis/parser/` — parse `native:` as a keyword argument on `subclass:` class definitions. This is a grammar extension: the current parser consumes `subclass:` then an identifier (class name) then enters the class body. Adding `native:` requires parsing an additional optional keyword argument after the class name — the same kind of extension needed for `supervisionPolicy:` on `Supervisor subclass:` (ADR 0059). The parser should implement a general "optional keyword arguments on `subclass:`" mechanism that both `native:` and `supervisionPolicy:` use
- `crates/beamtalk-core/src/ast.rs` — add optional `backing_module` field to `ClassDefinition`
- `crates/beamtalk-core/src/codegen/core_erlang/actor_codegen.rs` — detect `native:` on class; branch to facade codegen instead of full gen_server codegen
- `crates/beamtalk-core/src/codegen/core_erlang/gen_server/` — new `native_facade.rs` generating `spawn/1`, `has_method/1`, dispatch functions
- `crates/beamtalk-core/src/semantic_analysis/` — recognize `self delegate` in `native:` class methods; validate no `state:` on `native:` classes
- `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs` — remove hardcoded `Subprocess`, `TranscriptStream`
- `stdlib/src/Actor.bt` — add `delegate` method with error fallback
- `stdlib/src/ClassBuilder.bt` — add `native:` setter method and `state: backingModule`
- `stdlib/src/Subprocess.bt` — replace FFI with `native: beamtalk_subprocess` + `self delegate`
- `stdlib/src/TranscriptStream.bt` — replace FFI with `native: beamtalk_transcript_stream` + `self delegate`
- `runtime/apps/beamtalk_stdlib/src/beamtalk_subprocess.erl` — remove public wrapper functions; retain gen_server callbacks
- `runtime/apps/beamtalk_stdlib/src/beamtalk_transcript_stream.erl` — remove public wrapper functions; retain gen_server callbacks

## Migration Path

### Existing FFI Actor Classes

Replace `(Erlang beamtalk_module) selector: self` method bodies with `self delegate` and add `native: beamtalk_module` to the `subclass:` declaration. Remove public wrapper functions from the Erlang module — retain only the gen_server callbacks with `{Selector, [Args]}` pattern matching.

### Existing `@primitive` Actor Classes

Replace `@primitive "selector"` method bodies with `self delegate` and add `native: BackingModule` to the `subclass:` declaration. The selector names in the `.bt` file must match the `{Selector, [Args]}` patterns in the gen_server `handle_call/3`.

### Existing Hand-Written Gen_Server Modules

No changes required for `handle_call/3` reply format — `sync_send/3`'s `DirectValue` fallback handles raw values. Public wrapper functions can be removed once the `.bt` file uses `self delegate`. Optionally, wrap replies as `{ok, Result}` for future-proofing.

### No Breaking Changes for Callers

The Beamtalk API (message selectors and return types) does not change. Callers of `agent writeLine: data`, `agent readLine`, etc. are unaffected — the generated facade produces identical behaviour to the current FFI dispatch, with improved error handling.

## References

- Related issues: BT-1155, BT-1156, BT-1157, BT-1158, BT-1159
- Related ADRs: ADR 0005 (BEAM Object Model — Actor vs Value distinction), ADR 0028 (BEAM Interop Strategy — FFI mechanism), ADR 0038 (ClassBuilder Protocol — `native:` as ClassBuilder method), ADR 0042 (Immutable Value Objects and Actor-Only Mutable State), ADR 0043 (Sync-by-Default Actor Messaging — `sync_send/3` and `cast_send/3` protocols, `!` bang semantics), ADR 0048 (Class-Side Method Syntax Redesign), ADR 0051 (Subprocess Execution — proof-of-concept), ADR 0055 (Erlang-Backed Class Authoring Protocol — `state:` and FFI for Value classes), ADR 0059 (Supervisors — `supervisionPolicy:` keyword on `Supervisor subclass:`, same ClassBuilder keyword-argument pattern)
- gen_server protocol: https://www.erlang.org/doc/man/gen_server.html
- gen_statem protocol: https://www.erlang.org/doc/man/gen_statem.html
- Pharo UFFI / ffiCall: https://files.pharo.org/books-pdfs/booklet-uFFI/UFFIDRAFT.pdf

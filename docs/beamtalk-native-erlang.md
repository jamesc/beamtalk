# Native Erlang Integration

Beamtalk compiles to BEAM via Core Erlang, which means it runs on the same virtual machine as Erlang and Elixir. While most Beamtalk code is written in `.bt` files, some use cases require hand-written Erlang — OTP gen_server lifecycle management, port handling, NIF wrappers, or direct access to hex.pm packages. Beamtalk provides a structured integration path for these cases.

This guide covers the complete native Erlang workflow: project layout, FFI calls, native-backed actors, hex dependencies, the stub generator, and practical examples.

## Overview

There are three mechanisms for integrating Erlang code with Beamtalk, each suited to different scenarios:

| Mechanism | Use case | Declared in |
|-----------|----------|-------------|
| `(Erlang module)` FFI | Call any Erlang function from a method body | Per-method |
| `native:` + `self delegate` | Actor backed by a hand-written gen_server | Class declaration |
| `[native.dependencies]` | Use hex.pm packages from native Erlang code | `beamtalk.toml` |

These mechanisms are complementary. A single package might use all three — FFI for simple utility calls, `native:` for actors that need direct OTP control, and hex dependencies for ecosystem libraries like `gun` or `cowboy`.

## Directory Layout

Native Erlang source files live in a `native/` directory, separate from `src/`:

```text
my_package/
  beamtalk.toml                     # Package manifest
  src/                              # Beamtalk sources
    MyClass.bt
    MyActor.bt
  native/                           # Erlang sources
    my_backing_module.erl
  native/include/                   # Erlang headers (optional)
    my_records.hrl
  native/test/                      # Erlang tests (EUnit/Common Test)
    my_backing_module_tests.erl
  test/                             # Beamtalk tests (BUnit)
    MyClassTest.bt
    MyActorTest.bt
```

The `native/` directory name is deliberate — it matches the `native:` keyword on actor classes and signals that the contents are a "native escape hatch" rather than primary Beamtalk source.

**Convention:** Prefix native Erlang module names with the package name to avoid collisions (e.g., `my_package_pool.erl` for a package named `my_package`). BEAM has a flat module namespace — only one version of any module can be loaded at a time.

## Erlang FFI — `(Erlang module)` Calls

The simplest integration mechanism. Any Beamtalk method can call an Erlang function using the `(Erlang module)` syntax:

```beamtalk
// Call a function on the native module
Object subclass: NativeCalc

  class add: a with: b =>
    (Erlang native_calc) add: a with: b

  class multiply: a with: b =>
    (Erlang native_calc) multiply: a with: b

  class version =>
    (Erlang native_calc) version
```

The corresponding Erlang module in `native/`:

```erlang
%% native/native_calc.erl
-module(native_calc).
-export([add/2, multiply/2, version/0]).

add(A, B) -> A + B.
multiply(A, B) -> A * B.
version() -> 1.
```

**Keyword mapping:** In Erlang FFI calls, the function name is taken from the **first keyword** (with the colon removed), and all arguments are passed positionally. `(Erlang my_mod) do: x with: y` calls `my_mod:do(X, Y)`. Unary selectors map directly: `(Erlang my_mod) status` calls `my_mod:status()`.

**Instance methods** pass `self` explicitly when delegating to Erlang:

```beamtalk
sealed retryWith: opts :: Dictionary -> HTTPResponse =>
  (Erlang beamtalk_http) retry: self options: opts
```

**Class-side utility objects** (no instances, just namespace wrappers) delegate each class method:

```beamtalk
sealed Object subclass: System

  class getEnv: name :: String -> String | Nil =>
    (Erlang beamtalk_system) getEnv: name

  class osPlatform -> String =>
    (Erlang beamtalk_system) osPlatform
```

FFI is the right choice when:
- You need to call a handful of Erlang functions
- The Erlang module is stateless or manages its own state externally
- You want explicit control over which Erlang function each method calls

## Native Actors — `native:` and `self delegate`

When an Actor needs a hand-written gen_server (for port management, deferred replies, `handle_info/2`, or complex OTP patterns), declare it with `native:`:

```beamtalk
Actor subclass: DatabasePool native: my_db_pool

  class connect: config => self spawnWith: config

  query: sql -> List => self delegate
  query: sql params: params -> List => self delegate
  transaction: block -> Object => self delegate
  close -> Nil => self delegate
```

### How It Works

The `native:` keyword on `subclass:` tells the compiler to generate a **facade module** instead of a full gen_server. Methods with `=> self delegate` bodies are compiled into dispatch calls that forward through `beamtalk_actor:sync_send/3` to the backing gen_server. Methods with full Beamtalk bodies (like `connect:` above) compile normally.

The result is that callers interact with `DatabasePool` using standard Beamtalk message sends — they never see the Erlang gen_server behind it:

```beamtalk
pool := DatabasePool connect: #{"host" => "localhost", "port" => 5432}
rows := pool query: "SELECT * FROM users"
pool close
```

### Writing the Backing Gen_Server

The hand-written Erlang module must implement the standard gen_server protocol. Messages arrive in the wire format `{Selector, Args, PropCtx}` where `PropCtx` is a propagated context map (ADR 0069). Add a guard clause to strip it, then match on `{Selector, Args}`:

```erlang
%% native/my_db_pool.erl
-module(my_db_pool).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    {ok, Conn} = connect_db(Config),
    {ok, #{conn => Conn}}.

%% Strip propagated context (ADR 0069 Phase 2b) — required boilerplate
handle_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    handle_call({Selector, Args}, From, State);
handle_call({'query:', [SQL]}, _From, #{conn := Conn} = State) ->
    {reply, {ok, execute(Conn, SQL, [])}, State};
handle_call({'query:params:', [SQL, Params]}, _From, #{conn := Conn} = State) ->
    {reply, {ok, execute(Conn, SQL, Params)}, State};
handle_call({'transaction:', [Block]}, _From, #{conn := Conn} = State) ->
    Result = run_transaction(Conn, Block),
    {reply, {ok, Result}, State};
handle_call({close, []}, _From, #{conn := Conn} = State) ->
    close_db(Conn),
    {reply, {ok, nil}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{conn := Conn}) ->
    close_db(Conn),
    ok.
```

### Key Rules

**`start_link/1` is required.** The facade always calls `BackingModule:start_link(Config)` where Config is the map passed to `spawnWith:`. When `spawn` is called without arguments, Config is `#{}` (empty map).

**Use `{ok, Result}` reply wrapping.** Replies must be `{ok, Value}` or `{error, Reason}`. The `DirectValue` fallback exists for legacy modules but new native actors must use wrapped replies to avoid ambiguity with error tuples.

**No `state:` declarations.** Native actors cannot declare `state:` fields — all instance state lives in the gen_server. `classState:` is permitted for class-level state (e.g., a singleton reference).

```beamtalk
// This is a compile error:
Actor subclass: Broken native: some_module
  state: count = 0   // => error: native actor cannot declare state fields
```

**Type annotations are recommended.** Since `self delegate` method bodies are opaque, the type annotation is the only type information available to the compiler and LSP:

```beamtalk
query: sql :: String -> List => self delegate
```

The compiler emits a warning if a `self delegate` method has no return type annotation.

### Sync and Async Dispatch

The call site determines the dispatch mode — no annotation needed in the `.bt` file:

- `pool query: sql` -- sync via `gen_server:call` (`handle_call/3`)
- `pool query: sql!` -- async cast via `gen_server:cast` (`handle_cast/2`)

Backing gen_servers that support fire-and-forget should implement both `handle_call` and `handle_cast` clauses for the relevant selectors.

### gen_statem Support

`native:` also works with `gen_statem` backing modules. Both `gen_server` and `gen_statem` expose the same `gen:call/4` API internally. Synchronous calls arrive as `{call, From}` events in state callbacks:

```erlang
%% gen_statem handle_event_function mode
%% Strip propagated context (ADR 0069 Phase 2b)
handle_event({call, From}, {Selector, Args, PropCtx}, State, Data) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    handle_event({call, From}, {Selector, Args}, State, Data);
handle_event({call, From}, {'readLine', []}, _State, Data) ->
    {keep_state, Data, [{reply, From, {ok, read_line(Data)}}]}.
```

## The `gen-native` Stub Generator

Rather than writing the gen_server boilerplate by hand, use the `gen-native` CLI command to generate a skeleton from the `.bt` file:

```bash
beamtalk gen-native DatabasePool
```

This reads `src/DatabasePool.bt` (or `DatabasePool.bt` in the current directory), extracts the `native:` module name and all `self delegate` methods, and generates a skeleton Erlang file:

```erlang
%% Generated from DatabasePool.bt — fill in implementations
%% @doc Backing gen_server for the DatabasePool native Actor.

-module(my_db_pool).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-spec init(map()) -> {ok, map()}.
init(_Config) ->
    %% TODO: initialise state from Config
    {ok, #{}}.

%% Strip propagated context (ADR 0069 Phase 2b) — required boilerplate
handle_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    handle_call({Selector, Args}, From, State);

%% --- Delegate methods from DatabasePool.bt ---

%% query: -> List
handle_call({'query:', [Sql]}, _From, State) ->
    %% TODO: implement query:
    {reply, {ok, todo}, State};
%% query:params: -> List
handle_call({'query:params:', [Sql, Params]}, _From, State) ->
    %% TODO: implement query:params:
    {reply, {ok, todo}, State};
%% transaction: -> Object
handle_call({'transaction:', [Block]}, _From, State) ->
    %% TODO: implement transaction:
    {reply, {ok, todo}, State};
%% close -> Nil
handle_call({close, []}, _From, State) ->
    %% TODO: implement close
    {reply, {ok, todo}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
```

The generated file is a starting point — fill in the `TODO` implementations, then move the file to `native/`. The generator produces `{ok, todo}` reply wrapping by default, matching the required `{ok, Result}` protocol.

**File search order:** The command looks for `{ClassName}.bt` in the current directory, then `src/{ClassName}.bt`, then `lib/{ClassName}.bt`.

**Error cases:**
- Class without `native:` declaration: error with guidance to add `native:` to the `subclass:` declaration
- Missing `.bt` file: error listing the searched paths
- Class with no `self delegate` methods: warning (generates a skeleton with only the catch-all clause)

## Hex Dependencies

Packages that need hex.pm libraries (e.g., `gun` for HTTP, `cowboy` for servers) declare them in `beamtalk.toml`:

```toml
[package]
name = "http"
version = "0.1.0"
description = "HTTP client and server for Beamtalk"

[dependencies]
# Beamtalk package dependencies (if any)

[native.dependencies]
# Hex dependencies for native Erlang code
gun = "~> 2.1"
cowboy = "~> 2.12"
```

### Version Constraint Syntax

Constraints follow hex.pm conventions:

| Constraint | Meaning |
|-----------|---------|
| `"~> 2.1"` | `>= 2.1.0 and < 3.0.0` |
| `"~> 2.1.0"` | `>= 2.1.0 and < 2.2.0` |
| `">= 1.0.0 and < 2.0.0"` | Explicit range |
| `"2.12.0"` | Exact version |

### How the Build Works

`beamtalk build` handles native Erlang compilation automatically using one of two paths, selected by whether `[native.dependencies]` is present:

**Without hex deps** — `.erl` files in `native/` are compiled directly via `compile:file/2`. This is fast (~200ms) and requires no external tooling.

**With hex deps** — a `rebar.config` is generated from `beamtalk.toml` and rebar3 compiles both hex deps and `native/*.erl` files in a single invocation. Beamtalk bundles a pinned copy of rebar3, so no separate installation is needed.

The compilation order is always:
1. Hex dependencies (if any)
2. Native Erlang files in `native/`
3. Beamtalk `.bt` files in `src/`

This ensures that native modules are available when `.bt` files reference them via FFI or `native:`.

### Packages Without Native Erlang

A package can declare `[native.dependencies]` without a `native/` directory — for example, to call a hex package directly via `(Erlang module)` FFI without any hand-written Erlang glue:

```toml
[native.dependencies]
jiffy = "~> 1.1"
```

```beamtalk
Object subclass: FastJson
  class parse: str => (Erlang jiffy) decode: str
  class stringify: obj => (Erlang jiffy) encode: obj
```

### Dependency Resolution

All native dependencies across the entire Beamtalk dependency graph are resolved together. `beamtalk build` collects `[native.dependencies]` from every transitive package and passes them to a single rebar3 invocation. Resolved versions are pinned in `beamtalk.lock` for reproducible builds.

This top-level resolution is required by BEAM's flat module namespace — only one version of any module can be loaded at a time. If two packages declare different constraints for the same hex package, rebar3's constraint solver finds a version satisfying both (or reports a conflict).

## Compilation and Build Integration

### Build Pipeline

```text
beamtalk build
  |
  +-- Phase 1: Native Erlang
  |     Path A (no hex deps): compile:file/2 for each native/*.erl
  |     Path B (with hex deps): rebar3 compile (hex deps + native/*.erl)
  |
  +-- Phase 2: Beamtalk .bt files
        Compile .bt -> .core -> .beam
        Code path includes Phase 1 output
```

### Testing Native Code

Native Erlang tests live in `native/test/` and follow standard Erlang conventions:

```text
beamtalk test
  -> rebar3 eunit (native/test/*_tests.erl)
  -> rebar3 ct    (native/test/*_SUITE.erl, if present)
  -> BUnit        (test/*.bt)
```

EUnit modules should be named `*_tests.erl` and Common Test suites `*_SUITE.erl`. BUnit tests exercise the Beamtalk-facing API, while native tests cover the Erlang implementation directly.

### REPL Hot-Loading

Native Erlang modules participate in workspace hot-loading:

- **`:load_project`** scans `native/` alongside `src/`, recompiling changed `.erl` files before `.bt` files
- **`:reload ClassName`** demand-compiles the referenced native `.erl` file if it is newer than its `.beam`

Both paths use `compile:file/2` directly — no rebar3 invocation for interactive reloading.

### Runtime Dependencies

All native Erlang modules in a Beamtalk package have an implicit dependency on `beamtalk_runtime`. Common runtime modules available to native code:

| Module | Purpose |
|--------|---------|
| `beamtalk_error` | Structured error creation (`#beamtalk_error{}`) |
| `beamtalk_actor` | `sync_send/3` for actor dispatch |
| `beamtalk_object_ops` | Object protocol operations |
| `beamtalk_result` | `ok/1`, `error/1` result wrapping |

This dependency does not need to be declared in `beamtalk.toml` — the build tool ensures the runtime is on the code path.

## Practical Example: Creating a Native-Backed Class

This walkthrough creates a key-value store backed by an ETS table. ETS tables require Erlang for lifecycle management, and the actor owns the table (ETS tables die with their owner process), so Actor supervision gives table durability for free.

### Step 1: Define the Beamtalk API

Create `src/KeyValueStore.bt`:

```beamtalk
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/// A persistent key-value store backed by an ETS table.
///
/// ## Examples
///
/// ```beamtalk
/// store := KeyValueStore create
/// store put: #name value: "Alice"
/// store get: #name            // => "Alice"
/// store keys                  // => #(#name)
/// store size                  // => 1
/// ```
Actor subclass: KeyValueStore native: kv_store

  class create => self spawn
  class create: name => self spawnWith: #{"name" => name}

  get: key -> Object => self delegate
  put: key value: value -> Nil => self delegate
  delete: key -> Nil => self delegate
  keys -> List => self delegate
  size -> Integer => self delegate
```

### Step 2: Generate the Erlang Skeleton

```bash
beamtalk gen-native KeyValueStore
```

This produces `kv_store.erl` with `handle_call/3` clauses for each delegate method. Move it to the `native/` directory:

```bash
mkdir -p native
mv kv_store.erl native/
```

### Step 3: Implement the Gen_Server

Edit `native/kv_store.erl`:

```erlang
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Backing gen_server for the KeyValueStore native Actor.
-module(kv_store).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    Name = maps:get(<<"name">>, Config, undefined),
    Tid = case Name of
        undefined -> ets:new(?MODULE, [set, private]);
        _         -> ets:new(binary_to_atom(Name), [set, private, named_table])
    end,
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    {ok, #{tid => Tid}}.

%% Strip propagated context (ADR 0069 Phase 2b) — required boilerplate
handle_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    handle_call({Selector, Args}, From, State);
handle_call({'get:', [Key]}, _From, #{tid := Tid} = State) ->
    Result = case ets:lookup(Tid, Key) of
        [{_, Value}] -> Value;
        []           -> nil
    end,
    {reply, {ok, Result}, State};
handle_call({'put:value:', [Key, Value]}, _From, #{tid := Tid} = State) ->
    true = ets:insert(Tid, {Key, Value}),
    {reply, {ok, nil}, State};
handle_call({'delete:', [Key]}, _From, #{tid := Tid} = State) ->
    true = ets:delete(Tid, Key),
    {reply, {ok, nil}, State};
handle_call({keys, []}, _From, #{tid := Tid} = State) ->
    Keys = [K || {K, _V} <- ets:tab2list(Tid)],
    {reply, {ok, Keys}, State};
handle_call({size, []}, _From, #{tid := Tid} = State) ->
    {reply, {ok, ets:info(Tid, size)}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{tid := Tid}) ->
    ets:delete(Tid),
    ok.
```

### Step 4: Write Tests

Create `native/test/kv_store_tests.erl` for EUnit tests of the Erlang implementation:

```erlang
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(kv_store_tests).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, Pid} = kv_store:start_link(#{}),
    {ok, nil} = gen_server:call(Pid, {'put:value:', [key, <<"hello">>]}),
    {ok, <<"hello">>} = gen_server:call(Pid, {'get:', [key]}),
    gen_server:stop(Pid).
```

Create `test/KeyValueStoreTest.bt` for BUnit tests of the Beamtalk API:

```beamtalk
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

TestCase subclass: KeyValueStoreTest

  testPutAndGet =>
    store := KeyValueStore create
    store put: #name value: "Alice"
    self assert: (store get: #name) equals: "Alice"

  testSize =>
    store := KeyValueStore create
    self assert: (store size) equals: 0
    store put: #a value: 1
    self assert: (store size) equals: 1
```

### Step 5: Build and Run

```bash
beamtalk build
beamtalk test
```

## Quick Reference

### When to Use Each Mechanism

| Need | Use |
|------|-----|
| Call a stateless Erlang function | `(Erlang module)` FFI in method body |
| Wrap a utility library as class methods | `Object subclass:` with FFI calls |
| Actor with OTP gen_server control | `native:` + `self delegate` |
| Actor with Beamtalk-only logic | Standard `Actor subclass:` (no `native:`) |
| Hex.pm package dependency | `[native.dependencies]` in `beamtalk.toml` |
| Port, NIF, or raw process | Per-method `(Erlang module)` FFI |

### `self delegate` vs FFI in Native Actors

A `native:` class can mix both patterns:

```beamtalk
Actor subclass: MyActor native: my_backing_module

  // Delegates through gen_server — lifecycle-aware, timeout-aware
  getData -> Object => self delegate

  // Direct FFI call — bypasses gen_server for concurrent reads
  cachedValue -> Object =>
    (Erlang my_backing_module) directLookup: self
```

Use `self delegate` for operations that modify state or need gen_server guarantees. Use FFI for read-only operations that can safely bypass the gen_server mailbox (e.g., concurrent ETS reads).

### Related Documentation

- [Erlang FFI](beamtalk-language-features.md#erlang-ffi) -- FFI syntax reference
- [ADR 0055](ADR/0055-erlang-backed-class-authoring-protocol.md) -- Erlang-backed class authoring protocol
- [ADR 0056](ADR/0056-native-erlang-backed-actors.md) -- Native actors design decisions
- [ADR 0072](ADR/0072-user-erlang-sources-in-packages.md) -- User Erlang sources in packages

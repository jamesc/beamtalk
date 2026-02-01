# Beamtalk Architecture

How the compiler, tooling, and runtime fit together to deliver live programming.

For the Smalltalk-style IDE experience (browser, inspector, debugger), see [beamtalk-ide.md](beamtalk-ide.md).

---

## Table of Contents

- [Overview](#overview)
- [Why Rust for the Compiler?](#why-rust-for-the-compiler)
- [Component Responsibilities](#component-responsibilities)
- [Compilation Pipeline](#compilation-pipeline)
- [Live Development Flow](#live-development-flow)
- [Compiler Daemon](#compiler-daemon)
- [BEAM Node Integration](#beam-node-integration)
- [Directory Structure](#directory-structure)
- [Performance Targets](#performance-targets)
- [Actor Runtime Model](#actor-runtime-model)
- [Code Generation Details](#code-generation-details)
- [State Migration During Hot Reload](#state-migration-during-hot-reload)
- [Future/Promise Implementation](#futurepromise-implementation)
- [Architecture: Next Steps](#architecture-next-steps)
- [References](#references)

---

## Overview

The Beamtalk compiler is written in **Rust** and runs as a daemon on the developer's machine. It compiles `.bt` source files to BEAM bytecode, which is then hot-loaded into a running BEAM node. The Rust compiler is **not** part of the runtime — it's build infrastructure.

```
┌─────────────────────────────────────────────────────────────┐
│                    Developer Machine                         │
│                                                              │
│  ┌──────────────┐      ┌──────────────┐      ┌────────────┐ │
│  │ Editor/REPL  │ ──── │ Rust Compiler│ ──── │ .beam files│ │
│  │ (VS Code)    │ IPC  │ (beamtalk)   │      │ (bytecode) │ │
│  └──────────────┘      └──────────────┘      └─────┬──────┘ │
│                                                     │        │
└─────────────────────────────────────────────────────┼────────┘
                                                      │ hot load
                                                      ▼
                              ┌────────────────────────────────┐
                              │       Running BEAM Node        │
                              │                                │
                              │  ┌────────┐  ┌────────┐       │
                              │  │Counter │  │ Agent  │  ...  │
                              │  └────────┘  └────────┘       │
                              └────────────────────────────────┘
```

---

## Why Rust for the Compiler?

BEAM is optimized for concurrency and fault tolerance, not compiler workloads. Compilers need:

- Fast tree traversal
- Heavy string manipulation
- Efficient symbol table lookups
- Low-latency file I/O

Rust delivers **10-100x better performance** for these tasks than BEAM languages.

### Additional Benefits

| Benefit | Explanation |
|---------|-------------|
| **No bootstrap problem** | Don't need Beamtalk to build Beamtalk |
| **Single binary distribution** | No Erlang/OTP dependency for the compiler |
| **Cross-compilation** | Build for any platform from any platform |
| **Memory safety** | Compiler bugs don't crash production systems |

### What About Self-Hosting?

Self-hosted compilers (compiler written in its own language) are elegant but costly:

- **Gleam**: Written in Rust, not Gleam
- **Elixir**: Bootstrapped from Erlang, not pure Elixir
- **Rust**: Self-hosted, but took years to stabilize

For Beamtalk, self-hosting would delay shipping by 1+ years with no user-facing benefit. The liveness advantage shows up in *running* Beamtalk code, not in compiling it.

---

## Component Responsibilities

| Component | Runs Where | Written In | Purpose |
|-----------|------------|------------|---------|
| **Compiler** | Dev machine (daemon) | Rust | Parse, type-check, generate Core Erlang |
| **LSP Server** | Dev machine | Rust | IDE features (completions, errors, hover) |
| **REPL CLI** | Dev machine | Rust | Thin shell, sends input to BEAM node |
| **REPL Backend** | BEAM node | Erlang | Receives code, coordinates with compiler, evaluates |
| **Runtime** | BEAM node | Erlang | Supervision, distribution, standard library |
| **Your Actors** | BEAM node | Compiled Beamtalk | Your application code |

---

## Compilation Pipeline

```
  .bt source
      │
      ▼
┌─────────────┐
│   Lexer     │  Tokens with source spans
└─────────────┘
      │
      ▼
┌─────────────┐
│   Parser    │  AST with error recovery
└─────────────┘
      │
      ▼
┌─────────────┐
│  Analyzer   │  Type checking, name resolution
└─────────────┘
      │
      ▼
┌─────────────┐
│  Codegen    │  Core Erlang output
└─────────────┘
      │
      ▼
┌─────────────┐
│   erlc     │  BEAM bytecode (.beam)
└─────────────┘
      │
      ▼
  Running BEAM node (hot load)
```

### Incremental Compilation

The compiler daemon maintains state between compilations:

- **File cache**: Only reparse changed files
- **Dependency graph**: Only recompile affected modules
- **Query cache**: Salsa-style incremental computation

Target: **<50ms** for single-file change to loaded code.

---

## Live Development Flow

### 1. Editing in VS Code

```
┌─────────────────────────────────────────────────────────────┐
│  VS Code                                                    │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  counter.bt                                          │   │
│  │  ─────────────────────────────────────────────────   │   │
│  │  Actor subclass: Counter                             │   │
│  │    state: value = 0                                  │   │
│  │                                                      │   │
│  │    increment => self.value += 1   ← you edit here   │   │
│  │    getValue => ^self.value                           │   │
│  └─────────────────────────────────────────────────────┘   │
│                           │                                 │
│                           │ LSP (JSON-RPC over stdio)       │
│                           ▼                                 │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  Beamtalk LSP Server (Rust)                         │   │
│  │  - Receives textDocument/didChange                   │   │
│  │  - Incremental recompile                             │   │
│  │  - Returns diagnostics                               │   │
│  │  - Provides completions, hover, go-to-def            │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

### 2. Hot Reload on Save

```
  Ctrl+S in VS Code
        │
        ▼
  LSP Server compiles counter.bt
        │
        ▼
  Produces counter.beam
        │
        ▼
  Sends to BEAM node via:           ┌────────────────────────┐
  - TCP connection, or              │    Running BEAM Node   │
  - Unix socket, or         ────────►                        │
  - File watch + signal             │  code:load_file(...)   │
        │                           │         │              │
        │                           │         ▼              │
        │                           │  ┌─────────────────┐   │
        │                           │  │ Counter actors  │   │
        │                           │  │ now use new     │   │
        │                           │  │ increment code  │   │
        │                           │  └─────────────────┘   │
        │                           └────────────────────────┘
        │
  Total time: <100ms
```

### 3. REPL Interaction

```
┌─────────────────┐         ┌─────────────────────────────┐
│  REPL CLI       │  TCP    │      Running BEAM Node      │
│  (Rust)         │ ◄─────► │                             │
│                 │         │  ┌─────────────────────┐    │
│  > counter := Counter spawn     │ REPL Server Process │    │
│  > counter increment      │  │ (Erlang)            │    │
│  > counter getValue await │  │                     │    │
│  => 1                     │  │ 1. Receive input    │    │
│                 │         │  │ 2. Call compiler    │    │
│                 │         │  │ 3. Load bytecode    │    │
│                 │         │  │ 4. Evaluate         │    │
│                 │         │  │ 5. Return result    │    │
│                 │         │  └─────────────────────┘    │
└─────────────────┘         └─────────────────────────────┘
```

The REPL compiles each expression on demand:

1. **Input**: `counter increment`
2. **REPL server** sends to compiler daemon
3. **Compiler** returns bytecode for the expression
4. **REPL server** loads and evaluates
5. **Result** sent back to CLI for display

---

## Compiler Daemon

The compiler runs as a long-lived daemon process for performance:

```bash
# Started automatically by VS Code extension or CLI
beamtalk daemon start

# Or run in foreground for debugging
beamtalk daemon --foreground
```

### IPC Protocol

Communication via Unix socket (or TCP on Windows):

```
~/.beamtalk/daemon.sock
```

Protocol: JSON-RPC 2.0 (same as LSP)

```json
// Request: Compile file
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "compile",
  "params": {
    "path": "/project/src/counter.bt"
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "beam_path": "/project/_build/counter.beam",
    "diagnostics": []
  }
}
```

### Compiler State

The daemon maintains:

```rust
struct CompilerState {
    // Parsed files (invalidated on change)
    file_cache: HashMap<PathBuf, ParsedFile>,

    // Module dependency graph
    deps: DependencyGraph,

    // Salsa-style incremental queries
    db: CompilerDatabase,

    // Connected BEAM nodes for hot reload
    nodes: Vec<NodeConnection>,
}
```

---

## BEAM Node Integration

### Loader Process

A small Erlang process runs in the BEAM node to receive hot reloads:

```erlang
-module(beamtalk_loader).
-behaviour(gen_server).

%% Receives compiled .beam files from compiler daemon
handle_cast({load_module, Module, Binary}, State) ->
    code:load_binary(Module, "", Binary),
    {noreply, State};

%% Receives expressions to evaluate (REPL)
handle_call({eval, Binary}, _From, State) ->
    code:load_binary('beamtalk_repl_temp', "", Binary),
    Result = 'beamtalk_repl_temp':eval(),
    {reply, Result, State}.
```

### Connection to Compiler

Options for compiler-to-node communication:

| Method | Pros | Cons |
|--------|------|------|
| **TCP socket** | Works across machines | Need to manage port |
| **Unix socket** | Fast, local | Unix-only |
| **Distributed Erlang** | Native, supports remote | Requires cookie setup |
| **File watch** | Simple | Slower, polling |

**Recommended**: TCP socket for simplicity, with Distributed Erlang for remote nodes.

---

## Directory Structure

```
my_project/
├── beamtalk.toml           # Project config
├── src/
│   ├── my_app.bt           # Main module
│   └── actors/
│       ├── counter.bt
│       └── agent.bt
├── test/
│   └── counter_test.bt
├── _build/                  # Compiler output
│   ├── dev/
│   │   ├── counter.beam
│   │   └── agent.beam
│   └── test/
└── deps/                    # Hex dependencies
```

---

## Performance Targets

| Operation | Target | Notes |
|-----------|--------|-------|
| Keystroke to diagnostics | <50ms | LSP responsiveness |
| Save to hot reload | <100ms | Edit-run cycle |
| Cold compile (100 files) | <5s | Initial build |
| Incremental compile (1 file) | <50ms | Typical edit |
| REPL expression | <100ms | Interactive feel |

---

## Actor Runtime Model

Every Beamtalk actor is a BEAM process running a `gen_server`. This section describes the runtime representation.

### Process State Structure

Each actor maintains state in a map:

```erlang
%% Runtime state for a Counter actor
#{
  '__class__' => 'Counter',
  '__methods__' => #{
    increment => fun handle_increment/2,
    decrement => fun handle_decrement/2,
    getValue => fun handle_getValue/2,
    'incrementBy:' => fun 'handle_incrementBy:'/2
  },

  %% User-defined state fields
  value => 0
}
```

### gen_server Callbacks

Generated actors implement `gen_server`:

```erlang
-module(beamtalk_counter).
-behaviour(gen_server).

%% Start with initial state
init(Args) ->
    InitialState = #{
        '__class__' => 'Counter',
        '__methods__' => method_table(),
        value => proplists:get_value(initial, Args, 0)
    },
    {ok, InitialState}.

%% Async messages (cast) - returns future
handle_cast({Selector, Args, FuturePid}, State) ->
    case dispatch(Selector, Args, State) of
        {reply, Result, NewState} ->
            FuturePid ! {resolved, Result},
            {noreply, NewState};
        {noreply, NewState} ->
            {noreply, NewState}
    end.

%% Sync messages (call) - blocks caller
handle_call({Selector, Args}, _From, State) ->
    case dispatch(Selector, Args, State) of
        {reply, Result, NewState} ->
            {reply, Result, NewState}
    end.
```

### Message Dispatch

Message sends compile to `gen_server:cast` (async) or `gen_server:call` (sync):

```
// Beamtalk
counter increment

// Compiles to (async, returns future)
FuturePid = spawn_future(),
gen_server:cast(CounterPid, {increment, [], FuturePid}),
FuturePid
```

The dispatch function looks up the method:

```erlang
dispatch(Selector, Args, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} ->
            Fun(Args, State);
        error ->
            %% doesNotUnderstand: handler
            handle_dnu(Selector, Args, State)
    end.
```

### doesNotUnderstand: Metaprogramming

Unknown messages trigger `doesNotUnderstand:` if defined:

```
Actor subclass: Proxy
  state: target = nil

  doesNotUnderstand: selector args: args =>
    // Forward to target
    self.target perform: selector withArgs: args
```

```erlang
handle_dnu(Selector, Args, State) ->
    case maps:find('doesNotUnderstand:args:', maps:get('__methods__', State)) of
        {ok, Fun} ->
            Fun([Selector, Args], State);
        error ->
            %% No handler - crash (let supervisor handle)
            error({unknown_message, Selector})
    end.
```

---

## Code Generation Details

Beamtalk compiles to Core Erlang, which `erlc` then compiles to BEAM bytecode. This section shows concrete examples.

### Simple Actor

**Beamtalk source:**
```
Actor subclass: Counter
  state: value = 0

  increment => self.value += 1
  getValue => ^self.value
```

**Generated Core Erlang:**
```erlang
module 'beamtalk_counter' ['start_link'/1, 'init'/1,
                           'handle_cast'/2, 'handle_call'/3]
  attributes ['behaviour'='gen_server']

'start_link'/1 = fun (Args) ->
    call 'gen_server':'start_link'('beamtalk_counter', Args, [])

'init'/1 = fun (Args) ->
    let State = #{
        '__class__' => 'Counter',
        '__methods__' => #{
            'increment' => fun 'handle_increment'/2,
            'getValue' => fun 'handle_getValue'/2
        },
        'value' => 0
    }
    in {'ok', State}

'handle_increment'/2 = fun (Args, State) ->
    let Value = call 'maps':'get'('value', State)
    in let NewValue = call 'erlang':'+'(Value, 1)
    in let NewState = call 'maps':'put'('value', NewValue, State)
    in {'noreply', NewState}

'handle_getValue'/2 = fun (Args, State) ->
    let Value = call 'maps':'get'('value', State)
    in {'reply', Value, State}
```

### Block Compilation

Blocks compile to Erlang funs:

**Beamtalk:**
```
doubled := list collect: [:x | x * 2]
```

**Core Erlang:**
```erlang
let Fun = fun (X) -> call 'erlang':'*'(X, 2)
in let Doubled = call 'lists':'map'(Fun, List)
```

### Pattern Matching

Pattern matching compiles to Erlang's `case`:

**Beamtalk:**
```
handle: {#ok, value} => self process: value
handle: {#error, reason} => self logError: reason
handle: _ => self handleUnknown
```

**Core Erlang:**
```erlang
'handle'/2 = fun (Msg, State) ->
    case Msg of
        {'ok', Value} ->
            call 'dispatch'('process', [Value], State)
        {'error', Reason} ->
            call 'dispatch'('logError', [Reason], State)
        _ ->
            call 'dispatch'('handleUnknown', [], State)
    end
```

### Keyword Message Compilation

Keyword messages flatten to function calls:

**Beamtalk:**
```
array at: 1 put: 'hello'
```

**Core Erlang:**
```erlang
%% Selector becomes 'at:put:'
call 'dispatch'('at:put:', [1, <<"hello">>], ArrayPid)
```

### Binary Operations with Math Precedence

**Beamtalk:**
```
result := 2 + 3 * 4  // => 14 (standard precedence)
```

**Core Erlang:**
```erlang
%% Parser handles precedence, generates correct tree
let Temp = call 'erlang':'*'(3, 4)
in let Result = call 'erlang':'+'(2, Temp)
```

---

## State Migration During Hot Reload

The "live programming" promise requires preserving actor state across code changes. This is one of the trickier parts of the system.

### BEAM's Code Upgrade Mechanism

BEAM supports two versions of a module simultaneously:
- **Current**: New code, used for new calls
- **Old**: Previous code, still running in existing processes

When hot-loading:
1. New code becomes "current"
2. Old code becomes "old"
3. Processes running old code continue until they make a fully-qualified call
4. `code_change/3` callback allows state transformation

### Generated code_change Callback

```erlang
%% Called when module is hot-reloaded
code_change(OldVsn, State, Extra) ->
    %% Migrate state to new schema
    NewState = migrate_state(OldVsn, State),
    {ok, NewState}.

migrate_state(_OldVsn, State) ->
    %% Add new fields with defaults
    State1 = maps:merge(#{
        newField => default_value
    }, State),

    %% Remove obsolete fields
    State2 = maps:without([obsoleteField], State1),

    State2.
```

### Explicit State Migration in Beamtalk

The `patch` syntax can include explicit migration:

```
// Patch with state migration
patch Agent >> state {
  // Add new field
  self.memory := self.history ifNil: [OrderedCollection new]

  // Remove old field
  self removeField: #oldCache
}
```

This compiles to a custom `code_change/3`:

```erlang
code_change(_OldVsn, State, _Extra) ->
    %% Execute migration block
    State1 = case maps:find('history', State) of
        {ok, History} -> maps:put('memory', History, State);
        error -> maps:put('memory', orderedcollection:new(), State)
    end,
    State2 = maps:remove('oldCache', State1),
    {ok, State2}.
```

### Automatic Field Migration

When the compiler detects state schema changes:

| Change | Automatic Behavior |
|--------|-------------------|
| New field with default | Add field with default value |
| New field without default | Compilation error (must specify) |
| Removed field | Keep in state (warn) unless explicit removal |
| Type change | Compilation error (must specify migration) |

### Triggering Hot Reload

```erlang
%% In beamtalk_loader
handle_cast({hot_reload, Module, Binary}, State) ->
    %% Load new code
    code:load_binary(Module, "", Binary),

    %% Trigger code_change in all running instances
    %% (BEAM does this automatically when process makes qualified call)

    %% Optionally: force immediate migration via sys:change_code
    [sys:change_code(Pid, Module, undefined, [])
     || Pid <- find_actors_of_class(Module)],

    {noreply, State}.
```

### Limitations and Safety

**What works:**
- Adding fields with defaults
- Changing method implementations
- Adding new methods

**What's risky:**
- Changing field types (need explicit migration)
- Removing fields (data loss)
- Changing actor hierarchy

**Safety mechanism:** Compiler warns about potentially unsafe migrations and requires explicit opt-in.

---

## Future/Promise Implementation

Beamtalk is async-first: message sends return futures by default. This section describes the implementation.

### Design Choice: Lightweight Processes

Each future is a lightweight BEAM process. Why?

| Alternative | Pros | Cons |
|-------------|------|------|
| **Process per future** | Simple, isolated, GC'd naturally | Memory overhead (~2KB/process) |
| **Ref + registry** | Less memory | Complex tracking, no isolation |
| **ETS-based** | Fast lookup | Manual cleanup, no mailbox |

BEAM processes are cheap enough that process-per-future is the right default. Optimization can come later if needed.

### Future Process Implementation

```erlang
-module(beamtalk_future).

%% Spawn a new future
new() ->
    spawn(fun() -> pending([]) end).

%% Future states
pending(Waiters) ->
    receive
        {resolve, Value} ->
            %% Notify all waiters
            [Pid ! {future_resolved, self(), Value} || Pid <- Waiters],
            resolved(Value);
        {reject, Reason} ->
            [Pid ! {future_rejected, self(), Reason} || Pid <- Waiters],
            rejected(Reason);
        {await, Pid} ->
            %% Add to waiters list
            pending([Pid | Waiters]);
        {add_callback, resolved, Callback} ->
            pending([{callback, resolved, Callback} | Waiters]);
        {add_callback, rejected, Callback} ->
            pending([{callback, rejected, Callback} | Waiters])
    end.

resolved(Value) ->
    receive
        {await, Pid} ->
            Pid ! {future_resolved, self(), Value},
            resolved(Value);
        {add_callback, resolved, Callback} ->
            Callback(Value),
            resolved(Value);
        {add_callback, rejected, _} ->
            resolved(Value)  % Ignore reject callback
    end.

rejected(Reason) ->
    receive
        {await, Pid} ->
            Pid ! {future_rejected, self(), Reason},
            rejected(Reason);
        {add_callback, rejected, Callback} ->
            Callback(Reason),
            rejected(Reason);
        {add_callback, resolved, _} ->
            rejected(Reason)  % Ignore resolve callback
    end.
```

### Async Send Compilation

**Beamtalk:**
```
result := agent analyze: data
```

**Compiles to:**
```erlang
%% Create future
FuturePid = beamtalk_future:new(),

%% Send async message with future reference
gen_server:cast(AgentPid, {'analyze:', [Data], FuturePid}),

%% Bind future to variable
Result = FuturePid
```

### await Implementation

**Beamtalk:**
```
value := result await
```

**Compiles to:**
```erlang
%% Register as waiter
FuturePid ! {await, self()},

%% Block until resolved
Value = receive
    {future_resolved, FuturePid, V} -> V;
    {future_rejected, FuturePid, Reason} -> error(Reason)
after 30000 ->
    error(future_timeout)
end
```

### Continuation Callbacks

**Beamtalk:**
```
agent analyze: data
  whenResolved: [:value | self process: value]
  whenRejected: [:error | self handle: error]
```

**Compiles to:**
```erlang
FuturePid = beamtalk_future:new(),
gen_server:cast(AgentPid, {'analyze:', [Data], FuturePid}),

%% Register callbacks (non-blocking)
FuturePid ! {add_callback, resolved,
    fun(Value) -> dispatch('process:', [Value], self()) end},
FuturePid ! {add_callback, rejected,
    fun(Error) -> dispatch('handle:', [Error], self()) end}
```

### Async Pipe Implementation

**Beamtalk:**
```
data
  |>> agent1 process
  |>> agent2 validate
  |>> agent3 store
```

**Compiles to chained futures:**
```erlang
F1 = beamtalk_future:new(),
gen_server:cast(Agent1, {'process', [Data], F1}),

F2 = beamtalk_future:new(),
F1 ! {add_callback, resolved,
    fun(V1) -> gen_server:cast(Agent2, {'validate', [V1], F2}) end},

F3 = beamtalk_future:new(),
F2 ! {add_callback, resolved,
    fun(V2) -> gen_server:cast(Agent3, {'store', [V2], F3}) end},

F3  %% Return final future
```

### Future Cleanup

Futures are garbage collected when:
1. No references remain to the future pid
2. The resolved/rejected value has been delivered to all waiters

BEAM's per-process GC handles this naturally — no manual cleanup needed.

---

## Architecture: Next Steps

The following areas need detailed specification in future iterations:

### Supervision Tree Generation

How declarative supervision compiles to OTP supervisor specs:
- Child specification generation
- Restart strategies (one_for_one, one_for_all, rest_for_one)
- Dynamic vs static children
- Supervision across language boundaries (Beamtalk + Elixir)

### Error-Recovering Parser

Tooling-first parsing architecture:
- How partial/invalid code produces an AST
- Error node insertion and recovery strategies
- Preserving trivia (comments, whitespace) for formatting
- Incremental re-parsing on edit

### Distribution Model

Actors across BEAM nodes:
- Transparent remote message sending
- Location discovery and registration
- Failure detection across nodes
- State migration during node handoff

### Type System Architecture

If/when optional types are implemented:
- Where type checking fits in the pipeline
- Dialyzer spec generation from type annotations
- Type inference approach and limitations
- Gradual typing semantics

---

## References

- [Gleam Compiler Architecture](https://github.com/gleam-lang/gleam) — Similar Rust-to-BEAM approach
- [Erlang Code Loading](https://www.erlang.org/doc/man/code.html) — Hot code loading APIs
- [LSP Specification](https://microsoft.github.io/language-server-protocol/) — IDE protocol
- [Salsa](https://github.com/salsa-rs/salsa) — Incremental computation framework

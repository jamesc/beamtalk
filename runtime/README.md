# Beamtalk Runtime

This directory contains the Erlang runtime components for Beamtalk.

## Components

### beamtalk_actor.erl

The Actor runtime implementation. Every Beamtalk actor is a BEAM process running a `gen_server`.

Each actor maintains state in a map with:
- `__class__`: The actor's class name (atom)
- `__methods__`: A map from selector (atom) to method function
- User-defined state fields

Key features:
- **Message dispatch**: Routes messages to methods via `__methods__` map lookup
- **doesNotUnderstand**: Fallback handler for unknown messages (metaprogramming)
- **Sync and async**: Supports both `gen_server:call` (blocking) and `gen_server:cast` (with futures)
- **Hot reload**: Implements `code_change/3` for state migration during code updates
- **Helper functions**: `start_link/2` and `start_link/3` for creating supervised actors

See module documentation in `beamtalk_actor.erl` for the complete protocol and API.

### beamtalk.hrl

Shared record definitions for the Beamtalk runtime.

#### #beamtalk_object{} Record

Beamtalk object references use a record to bundle class metadata with the actor pid:

```erlang
-record(beamtalk_object, {
    class :: atom(),           % Class name (e.g., 'Counter')
    class_mod :: atom(),       % Class module (e.g., 'counter')
    pid :: pid()               % The actor process
}).
```

This design follows LFE Flavors' `#flavor-instance{}` pattern and enables:

- **Proper object semantics**: Object references carry their class identity
- **Reflection**: Access the class name via the record field
- **Type safety**: Distinguish object references from raw pids

Generated code creates these records in `spawn/0` and `spawn/1`:
```erlang
{'beamtalk_object', 'Counter', 'counter', Pid}
```

Message sends extract the pid using `element/2`:
```erlang
let Pid = call 'erlang':'element'(4, Obj)
in call 'gen_server':'cast'(Pid, {Selector, Args, Future})
```

See [beamtalk-object-model.md](../docs/beamtalk-object-model.md) Part 5 for the complete design.

### beamtalk_future.erl

The Future/Promise runtime implementation. Beamtalk is async-first: message sends return futures by default.

Each future is implemented as a lightweight BEAM process (~2KB) that can be in one of three states:
- **pending**: waiting for a value
- **resolved**: has a successful value  
- **rejected**: has an error

See module documentation in `beamtalk_future.erl` for the complete protocol and API.

## Building

To compile the runtime modules (requires Erlang/OTP installed):

### Using rebar3

```bash
cd runtime
rebar3 compile
```

## Testing

The tests use EUnit with rebar3 for automatic test discovery.

### Running all tests with rebar3

```bash
cd runtime
rebar3 eunit
```

This automatically discovers and runs all `*_tests.erl` modules.

### Interactive testing

```bash
cd runtime
rebar3 shell
```


## Integration with Beamtalk Compiler

Once BT-13 (erlc integration) is complete, the Rust compiler will:

1. Generate `.core` files from `.bt` source
2. Use the `compile.escript` to compile `.core` → `.beam`
3. Hot-load the `.beam` files into the running BEAM node
4. The runtime modules (`beamtalk_future`) will be available to generated code

## Design Rationale

From [beamtalk-architecture.md](../docs/beamtalk-architecture.md):

> Each future is a lightweight BEAM process. BEAM processes are cheap enough (~2KB) that process-per-future is the right default.

Alternatives considered:
- **Ref + registry**: Less memory but complex tracking, no isolation
- **ETS-based**: Fast lookup but manual cleanup, no mailbox

Process-per-future provides:
- Natural garbage collection (when no references remain)
- Message-based protocol (idiomatic BEAM)
- Fault isolation (future crashes don't affect callers)
- Simple implementation

## Future Work

- Performance benchmarks (futures/second, memory overhead)
- Optimization for common patterns (chaining, all/race combinators)
- Integration with OTP supervisor trees
- Distributed futures (cross-node await)

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
- **Helper functions**: `start_link/2`, `spawn_actor/2` for creating actors

See module documentation in `beamtalk_actor.erl` for the complete protocol and API.

### beamtalk_future.erl

The Future/Promise runtime implementation. Beamtalk is async-first: message sends return futures by default.

Each future is implemented as a lightweight BEAM process (~2KB) that can be in one of three states:
- **pending**: waiting for a value
- **resolved**: has a successful value  
- **rejected**: has an error

See module documentation in `beamtalk_future.erl` for the complete protocol and API.

## Building

To compile the runtime modules (requires Erlang/OTP installed):

```bash
cd runtime
erlc beamtalk_actor.erl beamtalk_future.erl
```

This produces `.beam` bytecode files.

## Testing

The tests use EUnit with rebar3 for automatic test discovery.

### Running all tests with rebar3

```bash
cd runtime
rebar3 eunit
```

This automatically discovers and runs all `*_tests.erl` modules.

### Running tests manually

```bash
cd runtime

# Compile runtime modules
erlc beamtalk_actor.erl beamtalk_future.erl

# Compile test modules
erlc -o test test/test_counter.erl test/test_proxy.erl test/test_throwing_actor.erl test/test_invalid_method_actor.erl test/test_throwing_dnu_actor.erl
erlc -o test test/beamtalk_future_tests.erl test/beamtalk_actor_tests.erl

# Run specific test module
erl -noshell -pa . -pa test -eval 'eunit:test(beamtalk_actor_tests, [verbose])' -s init stop

# Run all tests
erl -noshell -pa . -pa test -eval 'eunit:test([beamtalk_future_tests, beamtalk_actor_tests], [verbose])' -s init stop
```

### Interactive testing

```bash
cd runtime
erl -pa . -pa test
```

Then in the Erlang shell:

```erlang
c(beamtalk_actor).
c(beamtalk_future).
c("test/test_counter").
c("test/test_proxy").
c("test/beamtalk_actor_tests").
c("test/beamtalk_future_tests").
eunit:test([beamtalk_actor_tests, beamtalk_future_tests], [verbose]).
```

## Integration with Beamtalk Compiler

Once BT-13 (erlc integration) is complete, the Rust compiler will:

1. Generate `.core` files from `.bt` source
2. Use the `compile.erl` escript to compile `.core` → `.beam`
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

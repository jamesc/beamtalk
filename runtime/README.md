# Beamtalk Runtime

This directory contains the Erlang runtime components for Beamtalk.

## Components

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
erlc beamtalk_future.erl
```

This produces `beamtalk_future.beam` bytecode.

## Testing

The tests use EUnit. To run them:

```bash
cd runtime
erlc beamtalk_future.erl
erlc -pa . test/beamtalk_future_tests.erl
erl -noshell -pa . -eval 'eunit:test(beamtalk_future_tests, [verbose])' -s init stop
```

Or interactively:

```bash
cd runtime
erl -pa .
```

Then in the Erlang shell:

```erlang
c(beamtalk_future).
c("test/beamtalk_future_tests").
eunit:test(beamtalk_future_tests, [verbose]).
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

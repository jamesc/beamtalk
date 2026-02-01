# Beamtalk Test Fixtures

This directory contains Beamtalk source files used as test fixtures.

## Building Fixtures

The `counter.bt` fixture must be compiled to BEAM bytecode for use in runtime tests:

```bash
# From repository root
cd tests/fixtures
erlc build/counter.core
cp build/counter.beam ../../runtime/test/
```

Or use the helper script (if added):

```bash
./scripts/compile-test-fixtures.sh
```

## Fixtures

### counter.bt

A simple counter actor with the following methods:
- `increment` - increments value by 1, returns new value
- `decrement` - decrements value by 1, returns new value  
- `getValue` - returns current value

Used by `beamtalk_codegen_simulation_tests.erl` to test real compiled code generation,
including `spawn/0` and `spawn/1` that return `#beamtalk_object{}` records.

**Syntax Note:** The fixture uses the current parser syntax (`:=` for assignment, blocks for methods)
which differs from the planned `=>` method syntax documented in design docs.

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

### logging_counter.bt (BT-108)

Demonstrates super keyword for superclass method dispatch in inheritance hierarchy.

Inheritance: `Actor -> Counter -> LoggingCounter`

Methods:
- `increment` - increments logCount, calls `super increment`, returns value
- `getValue` - calls `super getValue` (tests super with different method)
- `getLogCount` - returns current logCount (new method added by subclass)
- `set:` - calls `super set:` with argument (tests super with keyword messages)

State variables:
- `value` - inherited from Counter
- `logCount` - tracks number of increment calls (new in LoggingCounter)

Used by `beamtalk_codegen_simulation_tests.erl` super keyword tests to verify:
- Super dispatch calls parent class methods
- State is maintained correctly across super calls
- Child can add new methods alongside overridden ones
- Super works with unary, keyword, and property access

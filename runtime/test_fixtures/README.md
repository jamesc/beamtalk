# Beamtalk Runtime Test Fixtures

This directory contains Beamtalk source files compiled for use in runtime unit tests.

## Overview

**Location:** `runtime/test_fixtures/` (BT-239 reorganization)  
**Purpose:** Pre-compiled BEAM bytecode for `beamtalk_codegen_simulation_tests.erl`  
**Build:** Automatically compiled by `compile.sh` via rebar3 pre-hook before eunit

## Directory Structure

```
runtime/test_fixtures/
├── compile.sh           # Compiles fixtures before tests
├── logging_counter.bt   # Super keyword test fixture
└── README.md           # This file
```

**Note:** `counter.bt` fixture consolidated to `tests/e2e/fixtures/counter.bt` (BT-239)

## Building Fixtures

Fixtures are compiled automatically by rebar3 before running tests:

```bash
# From repository root
just test-runtime  # Auto-compiles fixtures via pre-hook

# Or manually
./runtime/test_fixtures/compile.sh
```

The script:
1. Compiles `tests/e2e/fixtures/counter.bt` (unified fixture)
2. Compiles `runtime/test_fixtures/logging_counter.bt`
3. Copies resulting `.beam` files to `runtime/_build/*/test/`

## Fixtures

### counter.bt (Unified Fixture - BT-239)

**Source:** `tests/e2e/fixtures/counter.bt`  
**Syntax:** Modern class syntax (`Actor subclass: Counter`)

A simple counter actor with:
- `increment` - increments value by 1, returns new value
- `decrement` - decrements value by 1, returns new value  
- `getValue` - returns current value

Used by `beamtalk_codegen_simulation_tests.erl` to test real compiled code generation,
including `spawn/0` and `spawn/1` that return `#beamtalk_object{}` records.

**Consolidation:** Previously duplicated in `tests/fixtures/counter.bt`, now unified
with E2E fixture to reduce maintenance and confusion (BT-239).

### logging_counter.bt (BT-108)

**Source:** `runtime/test_fixtures/logging_counter.bt`  
**Purpose:** Super keyword testing

Demonstrates super keyword for superclass method dispatch in inheritance hierarchy.

Inheritance: `Object -> Counter -> LoggingCounter`

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

## References

- Runtime tests: `runtime/test/beamtalk_codegen_simulation_tests.erl`
- E2E fixtures: `tests/e2e/fixtures/`
- E2E test cases: `tests/e2e/cases/*.bt`

# Beamtalk Runtime Test Fixtures

This directory contains Beamtalk source files compiled for use in runtime unit tests.

## Overview

**Location:** `runtime/apps/beamtalk_runtime/test_fixtures/` (BT-239 reorganization, BT-287 umbrella)  
**Purpose:** Pre-compiled BEAM bytecode for `beamtalk_codegen_simulation_tests.erl`  
**Build:** Automatically compiled by `compile_fixtures.escript` via rebar3 pre-hook before eunit

## Directory Structure

```
runtime/apps/beamtalk_runtime/test_fixtures/
├── compile_fixtures.escript  # Compiles fixtures before tests (portable)
├── logging_counter.bt   # Super keyword test fixture
├── arithmetic_actor.bt  # Counter-shaped actor + a real divide: method (BT-3093)
├── rectangle_actor.bt   # Two-keyword message dispatch (BT-3093)
├── box_actor.bt         # Three-keyword message dispatch (BT-3093)
├── spawner_actor.bt     # Actor-spawns-actor from a compiled method (BT-3093)
├── shadow_actor.bt      # Param name shadows an instance var (BT-3093)
├── coordinate_actor.bt  # Multiple instance vars, one keyword message (BT-3093)
└── README.md           # This file
```

**Note:** `counter.bt` fixture consolidated to `tests/repl-protocol/fixtures/counter.bt` (BT-239)

## Building Fixtures

Fixtures are compiled automatically by rebar3 before running tests:

```bash
# From repository root
just test-runtime  # Auto-compiles fixtures via pre-hook

# Or manually
escript ./runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript
```

The script:
1. Compiles `tests/repl-protocol/fixtures/counter.bt` (unified fixture)
2. Compiles each `.bt` file listed in `compile_fixtures.escript`'s
   `LocalFixtures` list (currently `logging_counter`, `arithmetic_actor`,
   `rectangle_actor`, `box_actor`, `spawner_actor`, `shadow_actor`,
   `coordinate_actor`) to a module named `bt@<basename>`
3. Copies resulting `.beam` files to `runtime/_build/*/test/`

Add a new fixture by dropping a `<name>.bt` file in this directory and
appending its basename to `LocalFixtures` in `compile_fixtures.escript` —
no other wiring needed.

## Fixtures

### counter.bt (Unified Fixture - BT-239)

**Source:** `tests/repl-protocol/fixtures/counter.bt`  
**Syntax:** Modern class syntax (`Actor subclass: Counter`)

A simple counter actor with:
- `increment` - increments value by 1, returns new value
- `decrement` - decrements value by 1, returns new value  
- `getValue` - returns current value

Used by `beamtalk_codegen_simulation_tests.erl` to test real compiled code generation,
including `spawn/0` and `spawn/1` that return `#beamtalk_object{}` records.

**Consolidation:** Previously duplicated in `tests/fixtures/counter.bt`, now unified
with the REPL-protocol fixture to reduce maintenance and confusion (BT-239).

### logging_counter.bt (BT-108)

**Source:** `runtime/apps/beamtalk_runtime/test_fixtures/logging_counter.bt`  
**Purpose:** Super keyword testing

Demonstrates super keyword for superclass method dispatch in inheritance hierarchy.

Inheritance: `Object -> Counter -> LoggingCounter`

Methods:
- `increment` - increments logCount, calls `super increment`, returns value
- `getValue` - calls `super getValue` (tests super with different method)
- `getLogCount` - returns current logCount (new method added by subclass)

State variables:
- `value` - inherited from Counter
- `logCount` - tracks number of increment calls (new in LoggingCounter)

Used by `beamtalk_codegen_simulation_tests.erl` super keyword tests to verify:
- Super dispatch calls parent class methods
- State is maintained correctly across super calls
- Child can add new methods alongside overridden ones
- Super works with unary, keyword, and property access

### arithmetic_actor.bt, rectangle_actor.bt, box_actor.bt, spawner_actor.bt, shadow_actor.bt, coordinate_actor.bt (BT-3093)

Added migrating `beamtalk_codegen_simulation_tests.erl` off hand-written
"simulated compiler output" fixtures (state maps with a `'__methods__'` funs
table claiming to mirror compiled Beamtalk) onto real compiled dispatch,
per the Consistency-Test Disposition Rule
(`docs/development/architecture-principles.md` § 7) and the BT-239
precedent:

- `arithmetic_actor.bt` — a Counter-shaped actor (`value`, `increment`,
  `getValue`) plus a real `divide:` method, used for cascade/error-handling/
  actor-interaction/instance-var-persistence tests that need a spawnable
  actor beyond what the shared `counter.bt` exposes.
- `rectangle_actor.bt` / `box_actor.bt` — two- and three-keyword message
  dispatch (`width:height:`, `width:height:depth:`).
- `spawner_actor.bt` — spawns an `ArithmeticActor` from inside a compiled
  method body, proving actor-to-actor `ClassName spawn`.
- `shadow_actor.bt` — a method parameter shares a name with an instance
  variable, verifying the parameter shadows state.
- `coordinate_actor.bt` — multiple instance variables mutated by one
  two-keyword message.

`beamtalk_codegen_simulation_tests.erl`'s module doc and the
`counter_module_state/1` doc comment explain what stayed simulated and why
(the async future-cast protocol tested there has no compiled `.bt` source
construct that reaches it).

## References

- Runtime tests: `runtime/apps/beamtalk_runtime/test/beamtalk_codegen_simulation_tests.erl`
- REPL-protocol fixtures: `tests/repl-protocol/fixtures/`
- REPL-protocol test cases: `tests/repl-protocol/cases/*.btscript`

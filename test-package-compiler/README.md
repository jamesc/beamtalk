# Test Package Compiler

Snapshot tests for the beamtalk compiler. This crate validates compiler output across all compilation stages: lexing, parsing, and code generation.

## Overview

Tests are organized in the `cases/` directory. Each subdirectory with a `main.bt` file becomes a test case. The build script (`build.rs`) automatically discovers test cases and generates test functions for each stage of compilation.

## Test Structure

```
test-package-compiler/
├── cases/
│   └── hello_world/
│       └── main.bt           # Test source file
├── tests/
│   ├── compiler_tests.rs     # Test harness
│   └── snapshots/            # Snapshot files (auto-generated)
├── build.rs                  # Test discovery and generation
└── README.md                 # This file
```

## Adding a New Test Case

1. Create a new directory under `cases/` with a descriptive name:
   ```bash
   mkdir -p test-package-compiler/cases/my_test_case
   ```

2. Add a `main.bt` file with your beamtalk source code:
   ```bash
   cat > test-package-compiler/cases/my_test_case/main.bt <<'EOF'
   // Copyright 2026 James Casey
   // SPDX-License-Identifier: Apache-2.0

   // Your test code here
   counter := Counter new
   counter increment
   EOF
   ```

3. Run the tests to generate snapshots:
   ```bash
   cargo test -p test-package-compiler
   ```

4. Review and accept the generated snapshots:
   ```bash
   cargo insta review
   # Or automatically accept all:
   cargo insta accept
   ```

## Test Stages

Each test case generates two snapshot tests:

### 1. Lexer Test (`test_{case_name}_lexer`)
Validates the token stream produced by the lexer, including:
- Token kinds (identifiers, keywords, literals, operators)
- Source spans for error reporting
- Leading and trailing trivia (comments, whitespace)

### 2. Parser Test (`test_{case_name}_parser`)
Validates the Abstract Syntax Tree (AST) structure:
- Expression types and nesting
- Message send structure (unary/binary/keyword)
- Blocks and closures
- Error nodes (for error recovery tests)
- Diagnostics (parse errors and warnings)

### 3. Code Generation Test (Future)
Will validate the generated Core Erlang output.

## Snapshot Management

Snapshots are stored in `tests/snapshots/` and managed by [insta](https://insta.rs/).

### Review Changes
```bash
cargo insta review
```

### Accept All (use with caution)
```bash
cargo insta accept
```

### Reject All
```bash
cargo insta reject
```

### Re-run Failed Tests
```bash
cargo insta test
```

## Test Categories

Organize test cases by category:

### Basic Syntax
- `hello_world` - Simple message send
- `literals` - All literal types (numbers, strings, symbols)
- `assignments` - Variable assignment and compound assignment

### Message Precedence (Future)
- `unary_messages` - Object-oriented method calls
- `binary_messages` - Arithmetic and comparison
- `keyword_messages` - Multi-argument messages

### Control Flow (Future)
- `blocks` - Closures and block parameters
- `returns` - Early return from methods
- `cascades` - Multiple messages to same receiver

### Error Recovery (Future)
- `missing_token` - Parser continues after missing tokens
- `invalid_syntax` - Error nodes in AST
- `unterminated_string` - Lexer error recovery

### Advanced Features (Future)
- `pattern_matching` - Destructuring and guards
- `field_access` - Direct field access
- `async_messages` - Future-based async operations

## Architecture Requirements

From [AGENTS.md](../AGENTS.md):

### Tooling-First Approach
- Tests validate that parser produces valid AST even with syntax errors
- Error recovery must preserve enough structure for IDE features
- All AST nodes include precise source spans

### Performance Regression Tests (Future)
Track compilation times to ensure they stay within targets:
- Keystroke to diagnostics: <50ms
- Single-file incremental: <50ms
- Full file diagnostics: <100ms

## Threading-Mode Matrix (BT-3130 / ADR 0111)

[ADR 0111](../docs/ADR/0111-lowered-ir-verifier-for-state-threading.md) found
the codegen snapshot corpus too thin to gate its planned `ThreadedIr`
migration: of 318 snapshots, only 1 contained a `letrec` loop, 1 `$bt_nlr`,
1 `ClassVars1`, 1 `class_vars_shadow`, and 4 `StateAcc`. BT-3130 expanded the
corpus over the state-threading matrix:

```
{DirectParams, TupleAcc, Hybrid, StateAcc-fallback}
  × {Actor, ValueType, ClassMethod}
  × {NLR present/absent}
  × {class-var mutated/not}
```

Every *valid* cell is exercised by at least one fixture below (in addition
to the pre-existing `while_true_simple`, `class_methods`,
`actor_conditional_field_mutations`, `actor_enumeration_ops`,
`value_enumeration_ops`, and `stdlib_class_list` fixtures, which already
covered part of the matrix). Mode/reason labels were confirmed empirically
against `crates/beamtalk-core/src/codegen/core_erlang/control_flow/mod.rs`'s
`ThreadingPlan`/`StateAccFallbackReason` via `BEAMTALK_CODEGEN_DIAGNOSTICS=1`
(`CodegenOptions::with_codegen_diagnostics`), not inferred from source alone.

| Cell | Fixture(s) |
|---|---|
| DirectParams × Actor | `while_true_simple` (pre-existing) |
| DirectParams × ValueType | `letrec_direct_params_value_and_class` |
| DirectParams × ClassMethod | `letrec_direct_params_value_and_class` |
| Hybrid × Actor | `letrec_hybrid_actor` |
| TupleAcc × Actor | `foldl_tuple_acc_actor` (incl. early-exit ops: `detect:`, `anySatisfy:`, `takeWhile:`) |
| StateAcc-fallback × Actor (letrec) | `letrec_stateacc_fallbacks` (self-send, condition-state-effects, control-flow-mutations, nested-list-op-cross-scope reasons) |
| StateAcc-fallback × Actor (foldl) | `foldl_stateacc_fallbacks` (control-flow-mutations, destructure-as-last-expr reasons) |
| StateAcc (map-acc) × ValueType / ClassMethod (foldl) | `foldl_value_type_and_class_method` |
| NLR, matching-token × Actor | `nlr_matching_token_actor` |
| NLR, matching-token × ValueType | `nlr_matching_token_value_type` |
| NLR, matching-token × ClassMethod | `nlr_matching_token_class_method` |
| NLR, foreign-relay × Actor | `nlr_foreign_relay_actor` |
| NLR, foreign-relay × ClassMethod + class-var mutation + shadow-write (ADR 0110 shape) | `class_var_mutation_before_block_relay` |
| class-var mutation + shadow-write (single / sequential same-field / interleaved fields) | `class_var_single_mutation`, `class_var_sequential_mutations`, `class_var_multi_field_mutations` |
| Sequential mutations (State1→State2 / ClassVars1→ClassVars2) | `state_and_self_sequential_mutations`, `class_var_sequential_mutations` |
| Field mutation inside branch arms (`with_branch_context`) | `actor_conditional_field_mutations` (pre-existing) |

### Excluded cells (confirmed invalid or unsupported, not merely untested)

- **TupleAcc × ValueType** — `ThreadingPlan::select_tuple_acc` excludes
  `CodeGenContext::ValueType` explicitly (`control_flow/mod.rs:448-466`).
- **TupleAcc × ClassMethod** — empirically, class-method `do:`/`collect:`/
  `select:`/`inject:into:` bodies with mutations route through the shared
  `generate_list_do_body_with_threading` compat shim
  (`control_flow/mod.rs:2808`, used by `value_type_codegen`), which calls
  `ThreadingPlan::new(self, body, None)` — `allow_tuple_acc` is hardcoded
  `false` there. Confirmed via diagnostic
  `"StateAcc fallback — not a letrec loop"`. See
  `foldl_value_type_and_class_method`.
- **Hybrid × ValueType / ClassMethod** — `ThreadingPlan::select_hybrid_params`
  requires `context == CodeGenContext::Actor` (`control_flow/mod.rs:473-495`);
  value types have no actor `State` to extract fields from, and class
  methods route through the same fresh-map compat shim as ValueType.
- **Class-var mutation inside a conditional branch arm** (e.g.
  `flag ifTrue: [self.classVar := ...]` inside a `class` method) — attempted
  and confirmed **unsupported by the current compiler**: it raises a
  compile error ("Cannot assign to field '...' inside this block ... Field
  assignments only thread state back to the actor when the block is used
  directly with a control-flow construct ... — not when it's ... passed to
  a user-defined method"). This is a real limitation of the class-var
  assignment path (`generate_field_assignment`'s `in_class_method()` arm,
  `expressions.rs:547-620`), which does not route class-var writes through
  the same control-flow mutation-threading machinery instance fields use.
  Out of scope for BT-3130 (no production code changes); noted here as a
  known gap for a future issue.
- **Value-type field mutation (`Self`/`Self1`/`Self2`… threading, per
  `ValueTypeContext.self_version`/`next_self_var()`)** — not reachable
  through any legitimate `.bt` source: `Value subclass:` field assignment
  (`self.x := …`) is a compile error by design (immutability is enforced —
  see `docs/beamtalk-language-features.md` §"Object's Three Roles" / "Wrong
  Keyword Errors", and the `Value subclass:` row of the class-kind table:
  "Mutation: Immutable — methods return new instances"), and
  `Object subclass:` cannot declare instance data at all ("Object subclass
  `<Name>` cannot have instance data declarations"). Confirmed empirically:
  a fixture attempting `self.n := …` inside an `Object subclass:` with a
  `state:` field raised exactly that semantic error while codegen degraded
  to best-effort recovery, not a meaningful threading example — so this
  cell was dropped rather than pinned as a misleading snapshot. `Self`-
  versioning is therefore either dead in the in-scope
  `control_flow`/`threaded_expr.rs` surface, or lives entirely in
  `value_type_codegen.rs`'s constructor/initializer codegen, which ADR
  0111's own constraints section places out of scope for the `ThreadedIr`
  migration.
- **`InlineConditionalThreadedWrite` as the *labeled* StateAcc-fallback
  reason for Actor context** — reachable in principle
  (`diagnose_guard_failure`, `control_flow/mod.rs:534-555`), but for Actor
  context `needs_mutation_threading` (`mod.rs:2315-2318`) already counts
  any local-variable write inside a conditional as a "control-flow
  mutation", so `ControlFlowMutations` is always diagnosed first for Actor
  bodies with a local write inside `ifTrue:`/`ifFalse:`. Not forced as a
  separate fixture cell for that reason; `foldl_stateacc_fallbacks` and
  `letrec_stateacc_fallbacks` both exercise `ControlFlowMutations`, which is
  the reason that actually fires for this construct shape.

## Dependencies

- `beamtalk-core` - The compiler implementation
- `insta` - Snapshot testing framework
- `camino` - UTF-8 paths for cross-platform compatibility

## References

- [insta documentation](https://insta.rs/)
- [Gleam test harness](https://github.com/gleam-lang/gleam/tree/main/test-package-compiler)
- [AGENTS.md](../AGENTS.md) - Development guidelines

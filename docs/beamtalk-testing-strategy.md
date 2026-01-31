# Beamtalk Testing Strategy

This document describes the testing approach for the Beamtalk compiler and runtime.

## Overview

Beamtalk uses a multi-layered testing strategy covering both the Rust compiler and the Erlang runtime:

| Layer | Technology | Location | Purpose |
|-------|------------|----------|---------|
| Unit Tests | Rust `#[test]` | `crates/*/src/*.rs` | Test individual functions and modules |
| Snapshot Tests | insta | `test-package-compiler/` | Validate lexer, parser, and codegen output |
| Compilation Tests | erlc | `test-package-compiler/` | Verify generated Core Erlang compiles |
| Runtime Unit Tests | EUnit | `runtime/test/*_tests.erl` | Test Erlang runtime modules |
| Integration Tests | EUnit + daemon | `runtime/test/*_integration_tests.erl` | Test REPL ↔ daemon communication |
| E2E Tests (Erlang) | EUnit | `runtime/test/beamtalk_e2e_tests.erl` | Full Beamtalk → BEAM → execution |
| E2E Tests (Rust) | Rust + REPL | `tests/e2e/` | Language feature validation via REPL |

## Running Tests

### Quick Check (CI equivalent)

```bash
cargo build --all-targets
cargo clippy --all-targets -- -D warnings
cargo fmt --all -- --check
cargo test --all-targets
```

### Code Coverage

Generate coverage reports for both Rust and Erlang tests:

**Rust coverage:**
```bash
# Text output with summary
cargo llvm-cov --all-targets --workspace \
  -- --skip commands::build::tests::test_build_single_file \
     --skip commands::build::tests::test_build_multiple_files \
     --skip commands::run::tests::test_run_calls_build \
     --skip erlang_runtime_unit_tests

# HTML report (opens in browser)
cargo llvm-cov --all-targets --workspace --html --open \
  -- --skip commands::build::tests::test_build_single_file \
     --skip commands::build::tests::test_build_multiple_files \
     --skip commands::run::tests::test_run_calls_build \
     --skip erlang_runtime_unit_tests

# Cobertura XML format for CI integration
cargo llvm-cov --all-targets --workspace --cobertura --output-path coverage.cobertura.xml \
  -- --skip commands::build::tests::test_build_single_file \
     --skip commands::build::tests::test_build_multiple_files \
     --skip commands::run::tests::test_run_calls_build \
     --skip erlang_runtime_unit_tests
```

**Erlang coverage:**
```bash
cd runtime
rebar3 eunit --module=beamtalk_actor_tests,beamtalk_future_tests,beamtalk_repl_tests,beamtalk_e2e_tests --cover
rebar3 cover --verbose
# Generate Cobertura XML for CI
rebar3 covertool generate
```

Coverage reports are saved to:
- Rust HTML: `target/llvm-cov/html/index.html`
- Erlang HTML: `runtime/_build/test/cover/index.html`
- Rust Cobertura XML: `coverage.cobertura.xml`
- Erlang Cobertura XML: `runtime/_build/test/covertool/beamtalk_runtime.covertool.xml`

**CI Integration:**

Coverage metrics are automatically displayed in:
- **GitHub Actions Summary** - View in the "Summary" tab of any workflow run
- **PR Comments** - Sticky comment with coverage badges and details on all pull requests

No external services required - all coverage reporting is handled within GitHub Actions using `$GITHUB_STEP_SUMMARY` and PR comments.

**Coverage Thresholds:**

| Metric | Minimum | Target | Action if Below Minimum |
|--------|---------|--------|------------------------|
| Overall Line Coverage | 70% | 80% | Fail CI build |
| Branch Coverage | 80% | 90% | Fail CI build |
| Unit Test Coverage | 80% | 90% | Flag in PR review |

**Current Coverage (as of BT-136):**

| Language | Line Coverage | Notes |
|----------|--------------|-------|
| Rust | 81.98% | Overall workspace coverage |
| Erlang | 34% | Overall runtime coverage; some modules unused in tests |

**Target Coverage:**
- Rust unit tests: >90% line coverage
- Overall: >80% coverage for all test types

**Note:** Some Rust tests are skipped in coverage due to pre-existing failures unrelated to the test framework. See BT-136 for details.

### Erlang Runtime Tests

```bash
cd runtime
rebar3 eunit
```

### Individual Test Suites

```bash
# Just compiler snapshot tests
cargo test -p test-package-compiler

# Just core library unit tests
cargo test -p beamtalk-core

# Specific Erlang test module
cd runtime && rebar3 eunit --module=beamtalk_actor_tests
```

---

## Test Types

### 1. Rust Unit Tests

Standard Rust `#[test]` functions colocated with the code they test.

**Location:** `crates/*/src/*.rs` (in `#[cfg(test)] mod tests { ... }`)

**Count:** ~200 tests

**Example** ([erlang.rs](../crates/beamtalk-core/src/erlang.rs)):
```rust
#[test]
fn test_generate_literal_integer() {
    let mut generator = CoreErlangGenerator::new("test");
    let lit = Literal::Integer(42);
    let result = generator.generate_literal(&lit);
    assert!(result.is_ok());
    assert_eq!(generator.output, "42");
}
```

**What they test:**
- Lexer token generation
- Parser AST construction
- Code generation helpers
- Type conversions
- Error handling

---

### 2. Compiler Snapshot Tests

Snapshot tests validate the compiler's output at each stage. Changes are reviewed in PRs.

**Location:** `test-package-compiler/`

**Test cases:** `test-package-compiler/cases/*/main.bt`

**Snapshots:** `test-package-compiler/tests/snapshots/`

**Generated tests per case:**
| Test | Snapshot File | Purpose |
|------|---------------|---------|
| `test_{case}_lexer` | `*_lexer.snap` | Token stream from lexer |
| `test_{case}_parser` | `*_parser.snap` | AST structure from parser |
| `test_{case}_codegen` | `*_codegen.snap` | Generated Core Erlang |
| `test_{case}_compiles` | (none) | Verifies `erlc +from_core` succeeds |

**Adding a new test case:**
```bash
# 1. Create test directory
mkdir -p test-package-compiler/cases/my_feature

# 2. Add source file
cat > test-package-compiler/cases/my_feature/main.bt << 'EOF'
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Test description
myMethod := [ ^42 ]
EOF

# 3. Generate snapshots
cargo test -p test-package-compiler

# 4. Review and accept
cargo insta review
```

**Managing snapshots:**
```bash
cargo insta review    # Interactive review
cargo insta accept    # Accept all pending
cargo insta reject    # Reject all pending
```

---

### 3. Compilation Verification Tests

These tests verify that generated Core Erlang actually compiles with `erlc`.

**Location:** `test-package-compiler/tests/compiler_tests.rs` (`test_codegen_compiles`)

**Process:**
1. Parse Beamtalk source
2. Generate Core Erlang
3. Write to temp file
4. Run `erlc +from_core`
5. Assert compilation succeeds

**Skipping:** Tests are skipped gracefully if `erlc` is not available.

**Why this matters:** Snapshots can drift from actual erlc requirements. These tests catch syntax errors that snapshots miss.

---

### 4. Erlang Runtime Unit Tests

EUnit tests for the Erlang runtime modules.

**Location:** `runtime/test/`

| Test File | Tests |
|-----------|-------|
| `beamtalk_actor_tests.erl` | Actor lifecycle, message dispatch, doesNotUnderstand |
| `beamtalk_future_tests.erl` | Future creation, resolution, rejection, await |
| `beamtalk_repl_tests.erl` | REPL command parsing, expression evaluation |

**Running:**
```bash
cd runtime
rebar3 eunit --module=beamtalk_actor_tests
```

**Example** (`beamtalk_future_tests.erl`):
```erlang
resolve_sets_value_test() ->
    {ok, Future} = beamtalk_future:new(),
    ok = beamtalk_future:resolve(Future, 42),
    ?assertEqual(42, beamtalk_future:await(Future)).
```

**Test fixtures:** `test_counter.erl`, `test_throwing_actor.erl`, etc.

---

### 5. Integration Tests

Test the interaction between the Rust compiler daemon and Erlang runtime.

**Location:** `runtime/test/beamtalk_repl_integration_tests.erl`

**Requires:** Compiler daemon running (`beamtalk daemon start`)

**What they test:**
- TCP connection to daemon
- Expression compilation via daemon
- Hot code loading
- Error handling across Rust/Erlang boundary

**Running:**
```bash
# Terminal 1: Start daemon
./target/debug/beamtalk daemon start --foreground

# Terminal 2: Run tests
cd runtime
rebar3 eunit --module=beamtalk_repl_integration_tests
```

**CI runs these** with the daemon started in background mode.

---

### 6. End-to-End Tests (Erlang)

Full pipeline tests: Beamtalk source → Core Erlang → BEAM → execution.

**Location:** `runtime/test/beamtalk_e2e_tests.erl`

**What they test:**
- Complete compilation pipeline
- Actor spawning with `spawn/0` and `spawn/1`
- Method invocation (sync and async)
- State initialization and mutation
- Interaction between multiple actors

**Example:**
```erlang
spawn_with_args_initializes_state_test() ->
    %% Compile a counter actor with initial value via spawnWith:
    Counter = counter:spawn(#{value => 10}),
    %% Verify initial state was set
    ?assertEqual(10, gen_server:call(Counter, {getValue, []})).
```

---

### 7. End-to-End Tests (Rust)

Language feature validation via REPL TCP connection.

**Location:** `tests/e2e/`

**Test cases:** `tests/e2e/cases/*.bt`

**Test harness:** `crates/beamtalk-cli/tests/e2e.rs`

**What they test:**
- Complete compilation and execution via REPL
- Language features as users would experience them
- Arithmetic operators, blocks, closures
- Integration between compiler daemon and runtime

**Test file format:**
```smalltalk
// Test arithmetic
3 + 4
// => 7

// Test blocks
[:x | x + 1] value: 5
// => 6
```

**Running:**
```bash
# Run E2E tests only
cargo test --test e2e

# Run with verbose output
cargo test --test e2e -- --nocapture
```

**Adding a new test case:**
1. Create `tests/e2e/cases/my_feature.bt`
2. Add expressions with `// =>` expected results
3. Run `cargo test --test e2e`

**Error testing:**
```smalltalk
undefined_var
// => ERROR: Undefined variable
```

See [tests/e2e/README.md](../tests/e2e/README.md) for full documentation.

---

## CI Pipeline

The [CI workflow](../.github/workflows/ci.yml) runs on every PR:

```yaml
steps:
  - cargo build --all-targets          # Build everything
  - cargo clippy --all-targets -- -D warnings  # Lint (warnings = errors)
  - cargo fmt --all -- --check         # Format check
  - cargo test --all-targets           # All Rust tests + Erlang runtime
  - rebar3 eunit --module=...          # Unit tests
  - # Start daemon, then:
  - rebar3 eunit --module=beamtalk_repl_integration_tests  # Integration
```

---

## Test Organization Conventions

### Naming
- Rust tests: `test_descriptive_name` or `fn feature_behavior_context()`
- EUnit tests: `descriptive_name_test()` (EUnit auto-discovers `*_test` functions)
- Snapshot test cases: `snake_case` directory names

### File Structure
```
crates/beamtalk-core/src/
├── erlang.rs           # Code
└── erlang.rs           # Tests in same file (#[cfg(test)])

runtime/test/
├── beamtalk_actor_tests.erl      # Tests for beamtalk_actor.erl
├── test_counter.erl              # Test fixture actor
└── ...
```

### Test Fixtures
- Erlang: `test_*.erl` in `runtime/test/` for reusable actors
- Beamtalk: `test-package-compiler/cases/*/main.bt` for compiler test inputs

---

## Adding New Tests

### Adding a Rust Unit Test

Add to the existing `#[cfg(test)]` module in the source file:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn my_new_test() {
        // ...
    }
}
```

### Adding a Compiler Snapshot Test

1. Create `test-package-compiler/cases/my_feature/main.bt`
2. Run `cargo test -p test-package-compiler`
3. Review with `cargo insta review`

### Adding an Erlang Runtime Test

1. Add function to appropriate `*_tests.erl` file
2. Name it `descriptive_name_test()` (EUnit convention)
3. Run `cd runtime && rebar3 eunit`

### Adding an E2E Test (Erlang)

1. Add to `runtime/test/beamtalk_e2e_tests.erl`
2. May need to compile Beamtalk source first
3. Run `cd runtime && rebar3 eunit --module=beamtalk_e2e_tests`

### Adding an E2E Test (Rust/REPL)

1. Create or edit a `.bt` file in `tests/e2e/cases/`
2. Add expressions with `// => expected_result` annotations
3. Run `cargo test --test e2e`

Example test file:
```smalltalk
// Test my new feature
myExpression
// => expected_result
```

---

## Debugging Test Failures

### Rust tests
```bash
cargo test -- --nocapture              # Show println! output
cargo test test_name -- --nocapture    # Run specific test
RUST_BACKTRACE=1 cargo test            # Show backtraces
```

### Snapshot differences
```bash
cargo insta review                     # Interactive diff viewer
```

### Erlang tests
```bash
cd runtime
rebar3 eunit --module=module_name      # Run single module
rebar3 shell                           # Interactive debugging
```

### Integration test failures
```bash
# Check daemon is running
ps aux | grep beamtalk

# Check socket exists  
ls -la ~/.beamtalk/daemon.sock

# Run daemon in foreground to see output
./target/debug/beamtalk daemon start --foreground
```

---

## Performance Testing (Future)

From [AGENTS.md](../AGENTS.md), targets for tooling responsiveness:

| Operation | Target |
|-----------|--------|
| Keystroke to diagnostics | <50ms |
| Single-file incremental | <50ms |
| Full file diagnostics | <100ms |
| Project-wide find references | <500ms |

Performance regression tests are planned but not yet implemented.

---

## References

- [test-package-compiler/README.md](../test-package-compiler/README.md) - Snapshot test details
- [tests/e2e/README.md](../tests/e2e/README.md) - E2E test framework details
- [runtime/README.md](../runtime/README.md) - Erlang runtime test details
- [AGENTS.md](../AGENTS.md) - Development guidelines
- [insta documentation](https://insta.rs/) - Snapshot testing framework

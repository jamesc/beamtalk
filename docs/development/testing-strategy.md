# Beamtalk Testing Strategy

This document describes the testing approach for the Beamtalk compiler and runtime.

## Overview

Beamtalk uses a multi-layered testing strategy covering the Rust compiler, Erlang runtime, and language features:

| Layer | Technology | Location | Purpose |
|-------|------------|----------|---------|
| Unit Tests | Rust `#[test]` | `crates/*/src/*.rs` | Test individual functions and modules |
| Snapshot Tests | insta | `test-package-compiler/` | Validate lexer, parser, and codegen output |
| Compilation Tests | erlc | `test-package-compiler/` | Verify generated Core Erlang compiles |
| **Stdlib Tests** | **EUnit (compiled)** | **`stdlib/bootstrap-test/*.bt`** | **Bootstrap primitive validation (no REPL needed)** |
| **BUnit Tests** | **EUnit (TestCase)** | **`stdlib/test/*.bt`** | **Language feature tests via TestCase (`beamtalk test`)** |
| Runtime Unit Tests | EUnit | `runtime/apps/beamtalk_runtime/test/*_tests.erl` | Test Erlang runtime modules |
| Integration Tests | EUnit + daemon | `runtime/apps/beamtalk_runtime/test/*_integration_tests.erl` | Test REPL ↔ daemon communication |
| Codegen Simulation Tests | EUnit | `runtime/apps/beamtalk_runtime/test/beamtalk_codegen_simulation_tests.erl` | Simulate compiler output, test runtime behavior |
| E2E Tests (Rust) | Rust + REPL | `tests/e2e/` | REPL/workspace integration tests |

## Running Tests

### Quick Check (CI equivalent)

```bash
just ci                  # Build, lint, test, test-stdlib, test-e2e
```

Or individual steps:
```bash
cargo build --all-targets
cargo clippy --all-targets -- -D warnings
cargo fmt --all -- --check
cargo test --all-targets
just test-stdlib         # Bootstrap expression tests (fast, ~14s)
just test-bunit          # BUnit TestCase tests (~85 files)
just test-e2e            # REPL integration tests (slower, ~50s)
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
rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace --cover
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

**Example** ([erlang.rs](../../crates/beamtalk-core/src/erlang.rs)):
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

### 4. Stdlib Tests (Bootstrap Expression Tests)

Expression tests for bootstrap-critical primitives that TestCase transitively depends on. These must remain as expression tests because TestCase itself relies on these features working correctly.

**Location:** `stdlib/bootstrap-test/*.bt`

**Count:** ~11 test files

**Command:** `just test-stdlib`

**How it works:** The `beamtalk test-stdlib` command compiles each `.bt` file, parses `// =>` assertion comments, generates a thin EUnit wrapper, and runs via `eunit:test/1`. No REPL daemon is involved — tests compile and execute directly on BEAM.

**Test file format** (same `// =>` format as E2E):
```beamtalk
// Basic arithmetic
1 + 2
// => 3

5 negated
// => -5

// String operations
'hello' size
// => 5
```

**With fixtures** (`@load` directive, used in bootstrap tests only):
```beamtalk
// @load tests/e2e/fixtures/counter.bt

Counter spawn
// => _

// Wildcard _ means "runs but don't check result"
```

**Bootstrap files (DO NOT migrate to BUnit):**
`arithmetic.bt`, `booleans.bt`, `equality.bt`, `errors.bt`, `exceptions.bt`, `custom_exceptions.bt`, `erlang_exceptions.bt`, `integer_test.bt`, `float.bt`, `string_methods.bt`, `string_ops.bt`, `symbol.bt`, `literals.bt`, `value_types.bt`

Also kept as expression tests due to compile-time type check constraints:
`stack_frames.bt`, `class_hierarchy.bt`

**When to use stdlib expression tests:**
| Test needs... | Use stdlib test? |
|---|---|
| Bootstrap-critical primitives (arithmetic, strings, booleans) | ✅ Yes |
| Tests that TestCase depends on transitively | ✅ Yes |
| Tests that fail static type checks in BUnit (e.g. `Number subclasses`) | ✅ Yes |
| All other language feature tests | ❌ No — use BUnit tests |

**Adding a new stdlib test:**
1. Create `stdlib/bootstrap-test/my_feature.bt`
2. Add expressions with `// => expected_result` annotations
3. Run `just test-stdlib`

**Design:** See [ADR 0014](../ADR/0014-beamtalk-test-framework.md) for the full rationale behind compiled expression tests vs E2E tests.

---

### 4b. BUnit Tests (TestCase Classes)

SUnit-style test classes that subclass `TestCase`. The primary home for language feature tests — collections, closures, regex, actors, reflection, and more.

**Location:** `stdlib/test/*.bt` (project test directory)

**Count:** ~85 test files

**Command:** `just test-bunit` or `beamtalk test`

**How it works:** The `beamtalk test` command first pre-compiles all `.bt` files in the `fixtures/` subdirectory, making fixture classes available on the BEAM code path — similar to how all classes exist in a Smalltalk image. It then discovers `.bt` files containing `TestCase subclass:` definitions, compiles them through the normal pipeline, generates EUnit wrapper modules, and runs all test methods. Each test method starting with `test` is auto-discovered and run with a fresh instance. **Limitation:** currently only the first `TestCase` subclass in each `.bt` file is compiled (a warning is emitted if more are found), so put each test class in its own file.

**Test fixtures:** Place fixture classes in `stdlib/test/fixtures/`. All `.bt` files in this directory are automatically compiled and made available to all test files — no explicit loading needed. Just use the class name directly in your tests.

**Test file format:**
```beamtalk
// stdlib/test/counter_test.bt
// Counter class is available from stdlib/test/fixtures/counter.bt — no @load needed

TestCase subclass: CounterTest

  testInitialValue =>
    self assert: (Counter spawn getValue await) equals: 0

  testIncrement =>
    self assert: (Counter spawn increment await) equals: 1

  testMultipleIncrements =>
    | counter |
    counter := Counter spawn.
    3 timesRepeat: [counter increment await].
    self assert: (counter getValue await) equals: 3
```

**Lifecycle:** For each test method: create fresh instance → `setUp` → test method → `tearDown`

**Assertion methods:**

| Method | Description | Example |
|--------|-------------|---------|
| `assert:` | Assert condition is true | `self assert: (x > 0)` |
| `assert:equals:` | Assert two values equal | `self assert: result equals: 42` |
| `deny:` | Assert condition is false | `self deny: list isEmpty` |
| `should:raise:` | Assert block raises error | `self should: [1 / 0] raise: #badarith` |
| `fail:` | Unconditional failure | `self fail: 'not implemented'` |

**REPL integration:** TestCase classes can also be run interactively:
```text
> :load stdlib/test/counter_test.bt
> CounterTest runAll        // Run all tests in class
> CounterTest run: #testIncrement  // Run single test
```

**When to use BUnit tests:**
| Test needs... | Use BUnit? |
|---|---|
| Language features (collections, closures, regex, etc.) | ✅ Yes |
| Stateful test setup/teardown | ✅ Yes |
| Multiple assertions per test | ✅ Yes |
| Testing complex actor interactions | ✅ Yes |
| Bootstrap-critical primitives | ❌ No — use stdlib tests |
| REPL command testing | ❌ No — use E2E |

**Adding a BUnit test:**
1. Create `stdlib/test/my_feature_test.bt` with `TestCase subclass: MyFeatureTest`
2. Add test methods prefixed with `test`
3. Run `just test-bunit`

**Design:** See [ADR 0014](../ADR/0014-beamtalk-test-framework.md) Phase 2 for the full TestCase framework rationale.

---

### 5. Erlang Runtime Unit Tests

EUnit tests for the Erlang runtime modules.

**Location:** `runtime/apps/beamtalk_runtime/test/`

| Test File | Tests |
|-----------|-------|
| `beamtalk_actor_tests.erl` | Actor lifecycle, message dispatch, doesNotUnderstand |
| `beamtalk_future_tests.erl` | Future creation, resolution, rejection, await |
| `beamtalk_hot_reload_tests.erl` | Hot code reload, state migration |
| `beamtalk_codegen_simulation_tests.erl` | Codegen round-trip via EUnit simulation |

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

### 6. Integration Tests

Test the interaction between the Rust compiler daemon and Erlang runtime.

**Location:** `runtime/apps/beamtalk_runtime/test/beamtalk_repl_integration_tests.erl`

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

### 7. Codegen Simulation Tests

Tests runtime behavior using **real compiled Beamtalk code** and simulated patterns.

**Location:** `runtime/apps/beamtalk_runtime/test/beamtalk_codegen_simulation_tests.erl`

**What they test:**
- `spawn/0` and `spawn/1` tests use **real compiled `counter.bt`** (unified E2E fixture - BT-239)
  - Validates actual `#beamtalk_object{}` record generation
  - Tests `counter:spawn()` from compiled module
- Other tests use simulated state for complex scenarios
- Method invocation (sync and async)
- State initialization and mutation
- Interaction between multiple actors

**Test Fixtures:** Compiled automatically by rebar3 pre-hook
- Source: `tests/e2e/fixtures/counter.bt` (canonical implementation - BT-239)
- Compiled by: `runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript` (runs automatically)
- Output: `runtime/_build/*/test/bt@counter.beam`
- **No manual compilation needed** - hook runs before every `rebar3 eunit`

**Compilation Workflow:**
```
Developer runs: cargo test OR rebar3 eunit
  └─> cargo build (if needed) - creates ./target/debug/beamtalk
  └─> rebar3 pre-hook runs: escript runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript
      └─> Uses ./target/debug/beamtalk to compile tests/e2e/fixtures/counter.bt
      └─> Copies bt@counter.beam to runtime/_build/*/test/
  └─> Tests run with compiled fixtures available
```

**Note:** For true E2E tests with full compilation pipeline, see `tests/e2e/`.

**Example:**
```erlang
spawn_zero_uses_default_state_test() ->
    %% Uses real compiled counter module
    Object = counter:spawn(),
    ?assertMatch({beamtalk_object, 'Counter', counter, _Pid}, Object),
    
    %% Extract pid from #beamtalk_object{} record
    Pid = element(4, Object),
    
    %% Verify default value
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(0, Value).
```

---

### 8. End-to-End Tests (REPL Integration)

REPL and workspace integration tests that require a running REPL daemon.

**Location:** `tests/e2e/`

**Test cases:** `tests/e2e/cases/*.bt` (~23 files)

**Test harness:** `crates/beamtalk-cli/tests/e2e.rs`

**What they test:**
- Workspace bindings (Transcript, Beamtalk globals)
- REPL commands (`:load`, variable persistence)
- Actor auto-await behavior
- `ERROR:` assertion patterns
- Integration between compiler daemon and runtime

**When to use E2E tests:**
Only for tests that genuinely need the REPL daemon. Most language feature tests belong in `stdlib/test/*.bt` as BUnit tests (see section 4b).

**Test file format:**
```smalltalk
// Test workspace bindings
Transcript show: 'Hello'
// => nil

// Variable persistence across expressions
x := 42
// => 42

x + 1
// => 43
```

**Running:**
```bash
# Run E2E tests only
just test-e2e

# Or via cargo directly
cargo test --test e2e -- --ignored

# Run with verbose output
cargo test --test e2e -- --ignored --nocapture
```

**Adding a new E2E test case:**
1. Create `tests/e2e/cases/my_feature.bt`
2. Add expressions with `// =>` expected results
3. Run `just test-e2e`

**Note:** Before adding to E2E, consider whether the test needs the REPL. If it tests pure language features, add it to `stdlib/test/*.bt` as a BUnit test instead (see section 4b).

**Error testing:**
```smalltalk
undefined_var
// => ERROR: Undefined variable
```

See [tests/e2e/README.md](../../tests/e2e/README.md) for full documentation.

---

## CI Pipeline

The [CI workflow](../../.github/workflows/ci.yml) runs on every PR:

```bash
just ci
# Equivalent to:
#   just build           # Build Rust + Erlang
#   just lint            # Clippy + fmt-check + dialyzer
#   just test            # Rust unit tests + runtime EUnit
#   just test-stdlib     # Bootstrap expression tests (~14s)
#   just test-bunit      # BUnit TestCase tests (~85 files)
#   just test-e2e        # REPL integration tests (~50s)
```

### Testing Pyramid

The test suite follows a proper testing pyramid after [ADR 0014](../ADR/0014-beamtalk-test-framework.md):

```
            ╱╲
           ╱  ╲        E2E Tests (~36 files)
          ╱    ╲       REPL/workspace integration — slow (~50s)
         ╱──────╲
        ╱        ╲     BUnit Tests (~85 files)
       ╱          ╲    Language feature tests — fast (`just test-bunit`)
      ╱────────────╲
     ╱              ╲  Stdlib Tests (~11 files)
    ╱                ╲ Bootstrap expression tests — fast (~14s)
   ╱──────────────────╲
  ╱                    ╲ Rust + Erlang Unit Tests (~600+ tests)
 ╱                      ╲ Parser, codegen, runtime modules — fast (~10s)
╱────────────────────────╲
```

| Layer | Count | Speed | What it tests |
|-------|-------|-------|---------------|
| Rust unit tests | ~600 tests | ~5s | Parser, AST, codegen |
| Erlang unit tests | ~100 tests | ~3s | Runtime, primitives, object system |
| Compiler snapshots | ~51 cases | ~2s | Codegen output stability |
| **Stdlib tests** | **~11 files** | **~14s** | **Bootstrap primitives (expression tests)** |
| **BUnit tests** | **~85 files** | **—** | **Language features (TestCase classes)** |
| E2E tests | ~36 files | ~50s | REPL/workspace integration |

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

runtime/apps/beamtalk_runtime/test/
├── beamtalk_actor_tests.erl      # Tests for beamtalk_actor.erl
├── test_counter.erl              # Test fixture actor
└── ...
```

### Serial Test Locks

Tests that manipulate process-global state (environment variables, current working directory, shared filesystem state) must use named `serial_test` locks to prevent conflicts while allowing parallelism between non-conflicting tests.

**Lock naming guidelines:**

| Lock Name | Use When | Example |
|-----------|----------|---------|
| `erlang_runtime` | Running rebar3/erlc in runtime/ directory | Build/compile operations |
| `e2e` | Full E2E test with escript compilation | End-to-end language tests |
| `env_var` | Modifying environment variables | `std::env::set_var`, `std::env::remove_var` |
| `cwd` | Changing current working directory | `std::env::set_current_dir` |
| `daemon_lockfile` | Manipulating `~/.beamtalk/` directory | Daemon state management |

**Why named locks?**

Previously, all serialized tests used `#[serial_test::serial]`, which serialized *all* marked tests together. This reduced parallelism unnecessarily. For example, tests that manipulated environment variables would serialize with tests that changed the working directory, even though these operations don't conflict.

Named locks (e.g., `#[serial(env_var)]`) only serialize tests that actually conflict with each other. Tests in different groups can run in parallel, reducing CI time.

**Example:**

```rust
use serial_test::serial;

/// Uses `#[serial(env_var)]` because it modifies the `BEAMTALK_RUNTIME_DIR`
/// environment variable, which is process-global state.
#[test]
#[serial(env_var)]
fn test_env_modification() {
    unsafe { std::env::set_var("BEAMTALK_RUNTIME_DIR", "/tmp") };
    // ... test code ...
    unsafe { std::env::remove_var("BEAMTALK_RUNTIME_DIR") };
}

/// Uses `#[serial(cwd)]` because it changes the current working directory
/// (process-global state) using `std::env::set_current_dir`.
#[test]
#[serial(cwd)]
fn test_directory_change() {
    let original = std::env::current_dir().unwrap();
    std::env::set_current_dir("/tmp").unwrap();
    // ... test code ...
    std::env::set_current_dir(original).unwrap();
}

// These two tests can run in parallel because they use different locks
```

**Guidelines for adding new serial tests:**

1. **Always use a named lock** - Never use unnamed `#[serial]`
2. **Document why** - Add a doc comment explaining what global state is being manipulated
3. **Choose the right lock** - If manipulating a new type of global state, create a new lock name
4. **Keep locks focused** - Don't overload a lock name; create specific locks for specific conflicts

See [BT-115](https://linear.app/beamtalk/issue/BT-115) for the implementation details of the named lock system.

### Test Fixtures

**BUnit fixtures (Smalltalk image model):** Place fixture `.bt` files in `stdlib/test/fixtures/`. These are automatically compiled and available to all BUnit test files — no `@load` directives needed. Just use the fixture class name directly in your test methods. This mirrors the Smalltalk approach where all classes exist in the running image.

```text
stdlib/test/fixtures/
├── counter.bt            # Counter actor
├── typed_counter.bt      # Typed actor with Integer state
├── typed_account.bt      # Typed actor with Integer + String state
├── math_helper.bt        # Value type with recursion helpers
└── ...                   # ~46 fixture files
```

**Erlang fixtures:** `test_*.erl` in `runtime/apps/beamtalk_runtime/test/` for reusable actors.

**Compiler fixtures:** `test-package-compiler/cases/*/main.bt` for compiler test inputs.

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

### Adding a Codegen Simulation Test

1. Add to `runtime/apps/beamtalk_runtime/test/beamtalk_codegen_simulation_tests.erl`
2. Manually construct state as compiler would generate
3. Run `cd runtime && rebar3 eunit --module=beamtalk_codegen_simulation_tests`

### Adding a Stdlib Test (Bootstrap Primitives)

1. Create `stdlib/bootstrap-test/my_feature.bt`
2. Add expressions with `// => expected_result` annotations
3. Optionally use `// @load path/to/fixture.bt` for fixtures
4. Run `just test-stdlib`

Example test file:
```beamtalk
// Test my new feature
myExpression
// => expected_result

// Wildcard (run but don't check value)
sideEffectExpression
// => _
```

**Use this for:** Bootstrap-critical primitives only (arithmetic, booleans, equality, strings, errors, exceptions). Most new tests should use BUnit instead.

### Adding a BUnit Test (TestCase Classes)

1. Create `stdlib/test/my_feature_test.bt` with `TestCase subclass: MyFeatureTest`
2. If you need a helper class, add it to `stdlib/test/fixtures/` — it will be auto-compiled
3. Add test methods prefixed with `test` (auto-discovered)
4. Optionally add `setUp`/`tearDown` for lifecycle
5. Run `just test-bunit`

Example test file:
```beamtalk
// Fixture classes from stdlib/test/fixtures/ are automatically available
TestCase subclass: MyFeatureTest
  setUp =>
    self.thing := MyThing new

  testBasicBehavior =>
    self assert: (self.thing doSomething) equals: 42

  testErrorCase =>
    self should: [self.thing badMethod] raise: #does_not_understand
```

**Use this for:** Stateful tests with setup/teardown, complex scenarios with multiple assertions, actor interaction tests.

### Adding an E2E Test (REPL/Workspace Integration)

1. Create or edit a `.bt` file in `tests/e2e/cases/`
2. Add expressions with `// => expected_result` annotations
3. Run `just test-e2e`

**Use this for:** Workspace bindings, REPL commands, variable persistence, auto-await, `ERROR:` patterns.

Example test file:
```smalltalk
// Test workspace feature
Transcript show: 'hello'
// => nil
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

## Fuzzing (Parser Crash Safety)

Fuzzing tests the parser's robustness by feeding it random or mutated input to detect crashes, infinite loops, and excessive memory use.

**Technology:** cargo-fuzz (libFuzzer)

**Location:** `fuzz/fuzz_targets/parse_arbitrary.rs`

**Corpus:** `fuzz/corpus/parse_arbitrary/` (32 seed files from examples/ and tests/e2e/cases/)

### Running Locally

```bash
# Fuzz for 60 seconds (default)
just fuzz

# Fuzz for a specific duration
just fuzz 300  # 5 minutes

# Or use cargo directly
cargo +nightly fuzz run parse_arbitrary -- -max_total_time=60
```

**Requirements:**
- Rust nightly toolchain
- cargo-fuzz: `cargo install cargo-fuzz`

### What Fuzzing Tests

| Test Type | What It Catches |
|-----------|-----------------|
| **Crash safety** | Panics on unexpected token sequences |
| **Infinite loops** | Hangs during error recovery |
| **Stack overflow** | Deeply nested expressions causing stack exhaustion |
| **Out-of-memory** | Excessive memory allocation on malformed input |
| **Index bounds** | Array/buffer out-of-bounds access |

### CI Integration

Fuzzing runs nightly (not per-PR) via `.github/workflows/fuzz.yml`:
- Duration: 10 minutes per run
- Memory limit: 4GB RSS
- Artifacts uploaded on failure
- Auto-creates GitHub issues for crashes

**Why nightly?** Fuzzing is too slow for per-PR CI (minutes to hours). Nightly runs catch regressions without blocking development.

### Interpreting Results

**Success:** No artifacts produced, fuzzer completes normally
```
Done 17654 runs in 60 second(s)
```

**Crash (CRITICAL):** `crash-*` artifacts indicate parser panic
```
artifact_prefix='fuzz/artifacts/parse_arbitrary/'; Test unit written to crash-abc123
```
Action: Fix immediately, parser must never panic on user input.

**Timeout (WARNING):** `timeout-*` artifacts indicate infinite loop
```
SUMMARY: libFuzzer: timeout
```
Action: Investigate error recovery logic, add timeout limits.

**OOM (INFO):** `oom-*` artifacts indicate excessive memory use
```
SUMMARY: libFuzzer: out-of-memory
```
Action: Expected for extremely malformed input. Consider resource limits if frequent.

### Reproducing Failures

```bash
# Reproduce exact crash
cargo +nightly fuzz run parse_arbitrary fuzz/artifacts/parse_arbitrary/crash-abc123

# Minimize test case to smallest reproducer
cargo +nightly fuzz tmin parse_arbitrary fuzz/artifacts/parse_arbitrary/crash-abc123
```

### Adding Corpus Files

The corpus seeds fuzzing with realistic starting points. To add new files:

```bash
# Copy new .bt file to corpus
cp my_new_test.bt fuzz/corpus/parse_arbitrary/033_my_new_test.bt

# Fuzzer will use it as seed for mutation
just fuzz
```

**Keep corpus in sync:** When adding new `.bt` files to `examples/` or `tests/e2e/cases/`, also copy them to `fuzz/corpus/parse_arbitrary/` so the fuzzer can use them as mutation seeds.

**Corpus minimization:** `cargo +nightly fuzz cmin` rewrites the corpus directory in-place. Run it on a **temporary copy** to avoid deleting tracked seed files:
```bash
# Safe minimization (don't run cmin directly on tracked corpus)
cp -r fuzz/corpus/parse_arbitrary /tmp/corpus-backup
cargo +nightly fuzz cmin parse_arbitrary
# Review changes, restore any deleted seeds if needed
```

### Troubleshooting

**"cargo-fuzz not found":**
```bash
cargo install cargo-fuzz
```

**"nightly toolchain required":**
```bash
rustup toolchain install nightly
```

**"workspace errors":**
Ensure `fuzz` is in `workspace.exclude` in root `Cargo.toml`.

**Fuzzer runs too long:**
Use shorter duration for quick checks:
```bash
just fuzz 5  # 5 seconds
```

### References

- [Rust Fuzz Book](https://rust-fuzz.github.io/book/) - cargo-fuzz guide
- [ADR 0011](../ADR/0011-robustness-testing-layered-fuzzing.md) - Robustness testing strategy
- Epic: BT-362

---

## Property Testing (Nightly Extended)

Property tests use [proptest](https://proptest-rs.github.io/proptest/) to verify parser invariants over thousands of randomly generated inputs. Standard CI runs 512 cases per property (~0.4s). The nightly run extends this to 10,000 cases to catch rare edge cases.

**Location:** `crates/beamtalk-core/src/source_analysis/parser/property_tests.rs`

**Properties tested:**

| Property | What It Verifies |
|----------|-----------------|
| `parser_never_panics` | Arbitrary UTF-8 input never causes a panic |
| `parser_never_panics_near_valid` | Near-valid Beamtalk fragments don't panic |
| `diagnostic_spans_within_input` | All diagnostic spans have `end <= input.len()` |
| `error_nodes_produce_diagnostics` | Every `Expression::Error` node has diagnostics |
| `error_messages_are_user_facing` | No internal type names leak into error messages |

### Running Locally

```bash
# Run with default 512 cases (fast, ~0.4s)
cargo test -p beamtalk-core property_tests

# Run with extended cases (matches nightly)
PROPTEST_CASES=10000 cargo test -p beamtalk-core property_tests
```

### CI Integration

The extended proptest runs nightly alongside cargo-fuzz in the GitHub Actions workflow `.github/workflows/fuzz.yml`:
- Cases per property: 10,000 (vs 512 in standard CI)
- Schedule: 2 AM UTC daily (same as fuzzing)
- Can be triggered manually via `workflow_dispatch`
- Proptest automatically shrinks failures to minimal reproducing cases

**Why nightly?** 10,000 cases × 5 properties takes longer than is appropriate for per-PR CI. Nightly runs provide deeper exploration without slowing development.

### Interpreting Results

**Success:** All 5 properties pass with 10,000 cases each.

**Failure:** Proptest finds a failing input and shrinks it to the smallest reproducer. The shrunk case and a seed are printed in the test output. Example:
```text
proptest: Seed for failing test: 0x1234abcd...
proptest: Shrink failed: parser panicked on input "\x00\xff"
```

To reproduce a specific failure, use the seed from the output with `PROPTEST_REPLAY`:
```bash
PROPTEST_REPLAY="0x1234abcd..." cargo test -p beamtalk-core property_tests -- parser_never_panics
```

Proptest also persists failures in `proptest-regressions/` files, so they are automatically replayed on subsequent test runs.

### Standard CI vs Nightly

| | Standard CI | Nightly |
|---|---|---|
| Cases per property | 512 | 10,000 |
| Duration | ~0.4s | ~10s |
| Runs on | Every PR | Daily at 2 AM UTC |
| Trigger | Automatic | Schedule + manual |
| Configured via | `ProptestConfig` in source | `PROPTEST_CASES` env var |

### References

- [proptest documentation](https://proptest-rs.github.io/proptest/) - Property testing framework
- [ADR 0011](../ADR/0011-robustness-testing-layered-fuzzing.md) - Robustness testing strategy
- Epic: BT-362

---

## Performance Testing (Future)

From [AGENTS.md](../../AGENTS.md), targets for tooling responsiveness:

| Operation | Target |
|-----------|--------|
| Keystroke to diagnostics | <50ms |
| Single-file incremental | <50ms |
| Full file diagnostics | <100ms |
| Project-wide find references | <500ms |

Performance regression tests are planned but not yet implemented.

---

## References

- [ADR 0014: Beamtalk Test Framework](../ADR/0014-beamtalk-test-framework.md) - Architecture decision for the three-layer test strategy
- [test-package-compiler/README.md](../../test-package-compiler/README.md) - Snapshot test details
- [tests/e2e/README.md](../../tests/e2e/README.md) - E2E test framework details
- [runtime/README.md](../../runtime/README.md) - Erlang runtime test details
- [AGENTS.md](../../AGENTS.md) - Development guidelines
- [insta documentation](https://insta.rs/) - Snapshot testing framework

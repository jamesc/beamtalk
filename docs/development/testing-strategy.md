# Beamtalk Testing Strategy

This document describes the testing approach for the Beamtalk compiler and runtime.

## Overview

Beamtalk uses a multi-layered testing strategy covering the Rust compiler, Erlang runtime, and language features:

| Layer | Technology | Location | Purpose |
|-------|------------|----------|---------|
| Unit Tests | Rust `#[test]` | `crates/*/src/*.rs` | Test individual functions and modules |
| Snapshot Tests | insta | `test-package-compiler/` | Validate lexer, parser, and codegen output |
| Compilation Tests | erlc | `test-package-compiler/` | Verify generated Core Erlang compiles |
| **Stdlib Tests** | **EUnit (compiled)** | **`stdlib/bootstrap-test/*.btscript`** | **Bootstrap primitive validation (no REPL needed)** |
| **BUnit Tests** | **EUnit (TestCase)** | **`stdlib/test/*.bt`** | **Language feature tests via TestCase (`beamtalk test`)** |
| Runtime Unit Tests | EUnit | `runtime/apps/beamtalk_runtime/test/*_tests.erl` | Test Erlang runtime modules |
| Integration Tests | EUnit + daemon | `runtime/apps/beamtalk_runtime/test/*_integration_tests.erl` | Test REPL ↔ daemon communication |
| Codegen Simulation Tests | EUnit | `runtime/apps/beamtalk_runtime/test/beamtalk_codegen_simulation_tests.erl` | Simulate compiler output, test runtime behavior |
| REPL Protocol Tests (Rust) | Rust + REPL | `tests/repl-protocol/` | REPL TCP-protocol integration tests |
| Parity Tests | Rust + REPL/MCP/CLI/LSP | `tests/parity/` (cases) + `crates/beamtalk-parity-tests/` (harness) | Cross-surface equivalence checks (BT-2077) |

## Running Tests

### Quick Check (CI equivalent)

```bash
just ci                  # Build, lint, test, test-stdlib, test-repl-protocol
```

Or individual steps:
```bash
cargo build --all-targets
cargo clippy --all-targets -- -D warnings
cargo fmt --all -- --check
cargo test --all-targets
just test-stdlib         # Bootstrap expression tests (fast, ~14s)
just test-bunit          # BUnit TestCase tests (grows continuously — see "BUnit Tests" below for a count command)
just test-parity         # Cross-surface parity tests (BT-2077, ~30s)
just test-repl-protocol  # REPL TCP-protocol tests (slower, ~50s)
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
# Runs unit + E2E + stdlib suites under cover, merges, and generates XMLs
just coverage-all
```

**Elixir coverage (LiveView IDE, `editors/liveview`):**
```bash
just coverage-liveview
```
Uses Elixir's built-in `mix test --cover` (OTP's `:cover` — the same tool
family as `coverage-runtime`'s `rebar3 cover`, so no extra Hex dependency like
`excoveralls` is needed). `test_coverage` in `editors/liveview/mix.exs`
configures `ignore_modules` (currently just `BtAttachWeb.Layouts`, a pure
`embed_templates` boilerplate module with no logic) and the pass/fail
`summary: [threshold: ...]` floor — nested under `:summary`, since
`Mix.Tasks.Test.Coverage` only reads `:threshold` from there, not from the
top-level `test_coverage` options. The `liveview` CI job runs `mix test
--cover` directly, so PRs get the threshold enforced as a real gate; a
separate `coverage` job (push-to-`main` only) publishes the
`elixir-coverage.json` badge.

The threshold started at 55 (floor below the ~60% baseline measured in
BT-3288). After the WorkspaceLive decomposition and direct-test issues
(BT-3290's children, BT-3291–BT-3298) landed, total coverage measured
~82.7%; BT-3299 raised the floor to 78 — a few points of headroom below
that new baseline, not the measured number itself — mirroring the original
"floor below baseline" approach.

The Erlang coverage badge blends all four runtime apps —
`beamtalk_runtime`, `beamtalk_workspace`, `beamtalk_stdlib`, and
`beamtalk_compiler`. The `beamtalk_stdlib` figure reflects only the
hand-written FFI `.erl` modules: the `bt@*` compiled-Beamtalk modules
carry no Erlang abstract code (`erlc +from_core` emits empty abstract
forms), so `cover` cannot instrument them and they are auto-excluded
(BT-1672). They are exercised instead by the `.bt` BUnit suite.

#### Integration-shaped modules (BT-2389)

Some modules are only reached by a live external client, not by the
TCP `repl_protocol` E2E suite or plain unit tests:

- **`beamtalk_ws_handler`** — the Cowboy WebSocket handler for the REPL
  protocol (used by MCP/LSP/browser clients). Its callbacks (`init/2`,
  `websocket_init/1`, `websocket_handle/2`, `websocket_info/2`,
  `terminate/3`) are largely pure frame-builders over a `#ws_state{}`
  record. Rather than stand up a live WS client, they are driven
  **directly** by EUnit: the record is shared via
  `apps/beamtalk_workspace/include/beamtalk_ws_state.hrl`, and tests
  construct handler state + decode real protocol messages
  (`beamtalk_repl_protocol:decode/1`) to exercise each clause. The
  post-auth `create_session`/resume paths use a lightweight fixture that
  starts only `beamtalk_session_sup`. This is deterministic and needs no
  socket.
- **`beamtalk_build_worker`** — its pure compilation helpers
  (`compile_core_erlang/1`, `compile_core_file/2`, `compile_modules/2`,
  `handle_read_specs/1`) are unit-tested via `-ifdef(TEST)` exports; only
  the escript stdin loop (`main/0`/`compile_loop/0`) needs a real port.
- **`beamtalk_compiler_port` / `beamtalk_compiler_server`** — success
  paths are covered by live-port integration EUnit tests (they spawn the
  real Rust compiler port).

> **`--app` discovery gotcha:** `coverage-runtime` runs
> `rebar3 eunit --app=<app>`, which only auto-discovers a source module's
> `Module_tests` **companion**. A standalone `*_callbacks_tests` suite
> compiles fine but is silently skipped under `--app`, so its coverage
> never reaches the merged badge. Keep callback/unit tests for a source
> module in that module's `_tests` companion (e.g.
> `beamtalk_ws_handler_tests`).

**Genuinely unreachable (documented out-of-scope, not forced):** the
`create_session` `{error, Reason}` arm (the session supervisor's
`simple_one_for_one` child start does not fail deterministically), the
`actor_snapshot_frames/0` live-registry branch, the `compile_core_file/2`
beam-write-error arm, and root-only `permission_denied` / TOCTOU
`error:badarg` / `exit:{noproc}` defensive catches in `beamtalk_file`.

Coverage reports are saved to:
- Rust HTML: `target/llvm-cov/html/index.html`
- Erlang HTML: `runtime/_build/test/cover/index.html`
- Elixir HTML: `editors/liveview/cover/index.html`
- Rust Cobertura XML: `coverage.cobertura.xml`
- Erlang Cobertura XML: one per app under `runtime/_build/test/covertool/` (`beamtalk_runtime`, `beamtalk_workspace`, `beamtalk_stdlib`, `beamtalk_compiler`)

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

**Count:** grows continuously (thousands, the large majority in `beamtalk-core`) — get the current total with:
```bash
grep -r '#\[test\]' crates --include='*.rs' | wc -l
```

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

**Location:** `stdlib/bootstrap-test/*.btscript`

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
// @load tests/repl-protocol/fixtures/counter.bt
// (path is CWD-relative; the bootstrap suite resolves from the workspace root.)

Counter spawn
// => _

// Wildcard _ means "runs but don't check result"
```

**Bootstrap files (DO NOT migrate to BUnit):**
`arithmetic.btscript`, `blocks.btscript`, `booleans.btscript`, `equality.btscript`, `erlang_exceptions.btscript`, `errors.btscript`, `exceptions.btscript`, `float.btscript`, `literals.btscript`, `string_methods.btscript`, `scheme_symbol.btscript`

**When to use stdlib expression tests:**
| Test needs... | Use stdlib test? |
|---|---|
| Bootstrap-critical primitives (arithmetic, strings, booleans) | ✅ Yes |
| Tests that TestCase depends on transitively | ✅ Yes |
| Tests that fail static type checks in BUnit (e.g. `Number subclasses`) | ✅ Yes |
| All other language feature tests | ❌ No — use BUnit tests |

**Adding a new stdlib test:**
1. Create `stdlib/bootstrap-test/my_feature.btscript`
2. Add expressions with `// => expected_result` annotations
3. Run `just test-stdlib`

**Design:** See [ADR 0014](../ADR/0014-beamtalk-test-framework.md) for the full rationale behind compiled expression tests vs E2E tests.

---

### 4b. BUnit Tests (TestCase Classes)

SUnit-style test classes that subclass `TestCase`. The primary home for language feature tests — collections, closures, regex, actors, reflection, and more.

**Location:** `stdlib/test/*.bt` (project test directory)

**Count:** grows continuously (hundreds of files) — get the current total with:
```bash
find stdlib/test -maxdepth 1 -name '*.bt' | wc -l
```

**Command:** `just test-bunit` or `beamtalk test`

**How it works:** The `beamtalk test` command first pre-compiles all `.bt` files in the `fixtures/` subdirectory, making fixture classes available on the BEAM code path — similar to how all classes exist in a Smalltalk image. It then discovers `.bt` files containing `TestCase subclass:` definitions, compiles them through the normal pipeline, generates EUnit wrapper modules, and runs all test methods. Each test method starting with `test` is auto-discovered and run with a fresh instance. **Limitation:** currently only the first `TestCase` subclass in each `.bt` file is compiled (a warning is emitted if more are found), so put each test class in its own file.

**Test fixtures:** Place fixture classes in `stdlib/test/fixtures/`. All `.bt` files in this directory are automatically compiled and made available to all test files — no explicit loading needed. Just use the class name directly in your tests.

**Hazard — shared `Supervisor`/`DynamicSupervisor` fixtures:** `supervise` on a `Supervisor`/`DynamicSupervisor` subclass registers the running process under `{local, ClassName}` — one node-wide, name-registered singleton, not a per-test instance (`beamtalk_supervisor:startLink/1`). Since BUnit runs non-serial test classes concurrently, any two test classes that call `supervise`/`stop` on the *same* fixture class race: one test's teardown can `stop` the shared supervisor (and its children) out from under another test still using it, producing spurious "actor process has terminated" failures (BT-2729, BT-3379). If a test class touches a shared `Supervisor`/`DynamicSupervisor` fixture, declare `class serial -> Boolean => true` on it (see `SupervisorWhichTest` or `DynamicSupervisorDefaultsTest`) so it never runs concurrently with another class sharing that fixture.

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
- Source: `tests/repl-protocol/fixtures/counter.bt` (canonical implementation - BT-239)
- Compiled by: `runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript` (runs automatically)
- Output: `runtime/_build/*/test/bt@counter.beam`
- **No manual compilation needed** - hook runs before every `rebar3 eunit`

**Compilation Workflow:**
```
Developer runs: cargo test OR rebar3 eunit
  └─> cargo build (if needed) - creates ./target/debug/beamtalk
  └─> rebar3 pre-hook runs: escript runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript
      └─> Uses ./target/debug/beamtalk to compile tests/repl-protocol/fixtures/counter.bt
      └─> Copies bt@counter.beam to runtime/_build/*/test/
  └─> Tests run with compiled fixtures available
```

**Note:** For REPL TCP-protocol tests with full compilation pipeline, see `tests/repl-protocol/`.

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

### 8. REPL Protocol Tests

REPL TCP-protocol integration tests that require a running REPL daemon. (Previously called "E2E tests" — renamed in BT-2085 because they exercise one specific surface, not "end-to-end across surfaces". Cross-surface parity tests live under `tests/parity/`.)

**Location:** `tests/repl-protocol/`

**Test cases:** `tests/repl-protocol/cases/*.btscript` — grows continuously; get the current total with:
```bash
find tests/repl-protocol/cases -name '*.btscript' | wc -l
```

**Test harness:** `crates/beamtalk-cli/tests/repl_protocol.rs`

**What they test:**
- Workspace bindings (Transcript, Beamtalk globals)
- REPL commands (`:load`, variable persistence)
- Actor auto-await behavior
- `ERROR:` assertion patterns
- Integration between compiler daemon and runtime

**When to use REPL-protocol tests:**
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
# Run REPL protocol tests only
just test-repl-protocol

# Or via cargo directly
cargo test --test repl_protocol -- --ignored

# Run with verbose output
cargo test --test repl_protocol -- --ignored --nocapture
```

**Adding a new REPL-protocol test case:**
1. Create `tests/repl-protocol/cases/my_feature.btscript`
2. Add expressions with `// =>` expected results
3. Run `just test-repl-protocol`

**Note:** Before adding here, consider whether the test needs the REPL. If it tests pure language features, add it to `stdlib/test/*.bt` as a BUnit test instead (see section 4b).

**Error testing:**
```smalltalk
undefined_var
// => ERROR: Undefined variable
```

See [tests/repl-protocol/README.md](../../tests/repl-protocol/README.md) for full documentation.

### 9. Cross-surface Parity Tests (BT-2077)

Drives the same input through every public surface of the compiler stack
(REPL, MCP, CLI, LSP) and asserts the observable behaviour is equivalent.
Catches surface drift early — without this layer, a regression that affects
only the MCP `evaluate` tool would slip through both the REPL E2E suite and
the BUnit suite.

**Location:**
- Cases: `tests/parity/cases/*.parity.bt`
- Fixtures: `tests/parity/fixtures/`
- Harness crate: `crates/beamtalk-parity-tests/`

**What they test:**
- Literal evaluation (REPL + MCP) — same value
- Project loading (REPL + MCP + CLI) — same class set
- Lint (MCP + CLI) — same diagnostic count on a clean project
- BUnit test execution (REPL + MCP) — same pass/fail outcome
- Diagnostics on a broken file (CLI + MCP + LSP) — every surface flags it

**Case file format:**

```text
// @input
3 + 4
// @surfaces repl, mcp
// @expect 7
```

Three expectation directives are supported:

| Directive | Meaning |
|-----------|---------|
| `// @expect <text>` | Every surface must produce this normalized value |
| `// @expect-classes A, B, …` | Every surface must observe at least these class names |
| `// @expect-diagnostics N` | Every surface must report (≥) N diagnostics; `0` means exactly zero |

The harness recognises two placeholder tokens in the input:

* `<project>` — replaced with the staged temp copy of `tests/parity/fixtures/simple_project/`
* `<bad_file>` — replaced with the staged copy of `tests/parity/fixtures/diagnostic/BadSyntax.bt`

**Workspace pool:** all REPL and MCP cases share a single workspace started
once per harness run. The pattern is borrowed from `crates/beamtalk-mcp/src/client.rs::tests`.

**Running:**
```bash
just test-parity
```

**When to add a new case:**
- A new operation appears on more than one surface
- A bug fix corrected a divergence between surfaces — add a regression case
- A surface gets a new tool/command that maps to an existing REPL op

### 10. LiveView IDE (Cockpit) Tests

The Phoenix LiveView IDE under `editors/liveview` is tested in four layers,
gated by tags in `test/test_helper.exs`:

| Layer | Tag | Needs | Example |
|-------|-----|-------|---------|
| Pure / unit | *(none — bare `mix test`)* | nothing | `test/bt_attach/doc_format_test.exs` |
| LiveView integration | *(none)* | the `StubWorkspaceClient` stub | `test/bt_attach_web/workspace_doc_block_test.exs` |
| Workspace integration | `:workspace` | a live workspace node + `BT_WORKSPACE_COOKIE` | `test/bt_attach_web/workspace_live_test.exs` |
| Browser e2e | `:playwright` | a workspace node **and** Playwright/Chromium (`PHX_PLAYWRIGHT=1`) | `test/bt_attach_web/workspace_browser_test.exs` |

The bare `mix test` lane (the `liveview` CI job) runs the first two layers
against the stub, so the full LiveView render path is covered without a node;
the `:workspace` / `:playwright` lanes run in the `e2e` CI job.

**Known coverage note — doc-comment rendering (BT-2558):** the System Browser
doc block is covered at two seams — `beamtalk_repl_ops_browse_tests` asserts
`browse-method-source` carries `doc`/`signature` (from a `beamtalk_object_class`
fixture that populates `__doc__` directly), and `workspace_doc_block_test.exs`
asserts the LiveView renders that payload as escaped HTML (against a stub). The
compiler seam — that a `///` comment in `.bt` source actually flows through to
`__doc__` / `get_doc` at runtime — is exercised by the `help:` tests
(`beamtalk_repl_docs` / `beamtalk_interface_test.bt`), **not** through the
browser. There is intentionally no browser e2e walking source → codegen →
render (an earlier attempt was fragile against the mount-time class tree); if
doc-comment rendering regresses, check those three suites in that order.


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
#   just test-bunit      # BUnit TestCase tests (grows continuously — see "BUnit Tests" above for a count command)
#   just test-repl-protocol  # REPL TCP-protocol tests (~50s)
```

### Testing Pyramid

The test suite follows a proper testing pyramid after [ADR 0014](../ADR/0014-beamtalk-test-framework.md).
Layer counts grow continuously as the language and stdlib grow — each layer's
section above gives a command to derive its current count rather than a
number that goes stale. Shape and relative speed are stable and shown below:

```
            ╱╲
           ╱  ╲        E2E Tests (many files)
          ╱    ╲       REPL/workspace integration — slow (~50s)
         ╱──────╲
        ╱        ╲     BUnit Tests (most files, growing fastest)
       ╱          ╲    Language feature tests — fast (`just test-bunit`)
      ╱────────────╲
     ╱              ╲  Stdlib Tests (~11 files, fixed by design)
    ╱                ╲ Bootstrap expression tests — fast (~14s)
   ╱──────────────────╲
  ╱                    ╲ Rust + Erlang Unit Tests (most tests overall)
 ╱                      ╲ Parser, codegen, runtime modules — fast (~10s)
╱────────────────────────╲
```

| Layer | Count (derive with) | Speed | What it tests |
|-------|----------------------|-------|----------------|
| Rust unit tests | `grep -r '#\[test\]' crates --include='*.rs' \| wc -l` | ~5s | Parser, AST, codegen |
| Erlang unit tests | `grep -rE '_test\(\)\s*->\|_test_\(\)\s*->' runtime/apps --include='*_tests.erl' \| wc -l` (all 4 apps; "Erlang Runtime Unit Tests" below covers `beamtalk_runtime` only) | ~3s | Runtime, primitives, object system |
| Compiler snapshots | `find test-package-compiler/cases -mindepth 1 -maxdepth 1 -type d \| wc -l` cases (×4 generated tests each) | ~2s | Codegen output stability |
| **Stdlib tests** | **`find stdlib/bootstrap-test -name '*.btscript' \| wc -l` (~11 files, fixed by design)** | **~14s** | **Bootstrap primitives (expression tests)** |
| **BUnit tests** | **`find stdlib/test -maxdepth 1 -name '*.bt' \| wc -l`** | **—** | **Language features (TestCase classes)** |
| E2E tests | `find tests/repl-protocol/cases -name '*.btscript' \| wc -l` | ~50s | REPL/workspace integration |

### Cross-Repo Package Tests

The [cross-repo workflow](../../.github/workflows/cross-repo.yml) tests first-party packages that have been extracted to their own repositories (per [ADR 0073](../ADR/0073-package-distribution-and-discovery.md)). It builds the compiler from source, then checks out and tests each package against it.

**Runs on:** push to main + nightly schedule (7am UTC)

**Current packages tested:**
- [`beamtalk-http`](https://github.com/jamesc/beamtalk-http) — HTTP client and server

**Adding a new package:** Add a new job to `cross-repo.yml` following the `beamtalk-http` job as a template. Each job checks out the compiler, builds it, installs the binary, then checks out and tests the package.

**Network-dependent tests:** Package suites owned by other repos must not reach the public internet by default — a default `beamtalk test` run has to stay hermetic, or runner network noise reds this canary on an unrelated compiler change. `beamtalk-http`'s one live-network case (`HTTPTest>>testHttpsGetReturnsOkStatus`) is opt-in, gated behind the `BEAMTALK_HTTP_LIVE_NETWORK_TESTS` env var and skipped by default (BT-3191); the cross-repo job doesn't set it, so `Test beamtalk-http` is a plain `beamtalk test` with no retry wrapper.

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
└── ...                   # grows continuously; `find stdlib/test/fixtures -maxdepth 1 -name '*.bt' | wc -l`
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

1. Create `stdlib/bootstrap-test/my_feature.btscript`
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

### Adding a REPL Protocol Test (REPL/Workspace Integration)

1. Create or edit a `.bt` file in `tests/repl-protocol/cases/`
2. Add expressions with `// => expected_result` annotations
3. Run `just test-repl-protocol`

**Use this for:** Workspace bindings, REPL commands, variable persistence, auto-await, `ERROR:` patterns.

Example test file:
```smalltalk
// Test workspace feature
Transcript show: 'hello'
// => nil
```

### Cross-Platform Temp Paths

**Never hardcode `/tmp/` in tests.** This breaks on Windows where `/tmp` does not exist.

Use `File tempDirectory` to get the OS temp directory, then build paths from it:

```beamtalk
// In .btscript (e2e) or .bt (BUnit) tests:
tmp := File tempDirectory       // OS temp dir (e.g. /tmp or $TMPDIR on Unix, %TEMP% on Windows)
path := tmp ++ "/bt_my_test_file.txt"

// In Erlang tests:
TmpDir = beamtalk_file:'tempDirectory'(),
Path = <<TmpDir/binary, "/my_test_file.txt">>,
```

For BUnit tests (`stdlib/test/*.bt`), prefer relative paths under `target/bt-test-tmp/` when possible — these don't need cross-platform handling. Use `File tempDirectory` only when an absolute path is required.

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

## Metamorphic Testing (Semantics-Preserving Transforms)

Hand-written `// =>` assertions can only catch bugs someone thought to write
a literal for. The bug classes that escape them are the *silently wrong*
ones: the ADR 0110 NLR-relay bug returned the correct value while losing a
class-var mutation; state-threading bugs produce wrong values only under
specific mutation-in-control-flow shapes (BT-1226). Metamorphic testing
catches these without a reference implementation: apply a transformation
that must not change semantics, then assert the transformed program
evaluates to the same result as the original (BT-3117).

**Location:** `crates/beamtalk-cli/src/commands/test_metamorphic.rs`
(hidden dev command `beamtalk test-metamorphic`)

**Corpus:** every `// =>` expression in `stdlib/bootstrap-test/*.btscript` —
reuses `test_stdlib`'s exact parse → compile-to-Core-Erlang → `EUnit`
execution pipeline for both the original and the transformed variant, so
both run through the real compiler rather than a second hand-rolled
evaluator.

**Transforms** (each a probe into closure conversion, variable scoping, and
state threading):

| Transform | `expr` becomes |
|---|---|
| Block-wrap | `[expr] value` |
| Rename-locals | consistent alpha-rename of every name *bound* inside `expr` (block params, `:=` targets, match-pattern bindings) — free variables are left alone |
| Redundant-temp | `[tmp := expr. tmp] value` |

Each transform's output is unparsed and re-parsed before use (mirroring
`unparse`'s own `unparse_roundtrip_preserves_structure` property test) — a
transform that doesn't round-trip cleanly is skipped for that unit rather
than fed into the compiler. A unit shaped exactly `name := value` (a
`.btscript`-format cross-unit REPL-turn binding, detected by
`test_stdlib::extract_assignment_var`) is carried through unchanged instead
of transformed, so later units in the same file that reference `name` keep
seeing it — transforming that unit's outer shape would defeat the text-match
`extract_assignment_var` uses, which is a corpus-format artifact, not a real
semantics change.

### Running locally

```bash
just test-metamorphic                                  # full bootstrap-test corpus, ~5s
just test-metamorphic bootstrap-test/blocks.btscript    # single file
```

Runs in CI as part of the `test` job (`just test-bunit` → `just
test-metamorphic` → `just test-examples`) — cheap enough (~5s over the full
corpus) to run on every PR rather than nightly-only.

### Interpreting a failure

A failure means a semantics-preserving transform changed a unit's result —
report includes the transformed source, the transform name (in the file
label, e.g. `blocks [rename_locals]`), and both the expected and actual
values (`beamtalk_stdlib_test`'s usual `FAIL <location>` / `expected: ...` /
`got: ...` block). Treat it exactly like a `core_lint`/fuzz finding: it means
codegen or analysis is doing something shape-dependent that it shouldn't.

---

## Fuzzing (Parser & Compile Pipeline Crash Safety)

Fuzzing tests the compiler's robustness by feeding it random or mutated input to detect crashes, infinite loops, and excessive memory use. Two targets, at different pipeline depths (BT-3124):

| Target | Pipeline depth | Corpus |
|---|---|---|
| `parse_arbitrary` | lex → parse | `fuzz/corpus/parse_arbitrary/` (seed files copied from `examples/` and `tests/repl-protocol/cases/` — `find fuzz/corpus/parse_arbitrary -maxdepth 1 -type f \| wc -l` for the current count) |
| `compile_pipeline` | lex → parse → analyse → codegen | `stdlib/test/*.bt` + `tests/repl-protocol/cases/*.btscript`, referenced live as extra `cargo fuzz run` corpus dirs, plus `fuzz/corpus/compile_pipeline/` for fuzzer-discovered growth |

`compile_pipeline`'s seeds are *not* copied into the repo: `cargo fuzz run <target> [corpus_dir]...` accepts any number of corpus directories, so it points straight at `stdlib/test` and `tests/repl-protocol/cases` (see `Justfile`'s `fuzz`/`fuzz-corpus-lint` recipes and `.github/workflows/fuzz.yml`). Seeds this way always match the current test suite — no separate copy step to remember, no snapshot to go stale. `fuzz/corpus/compile_pipeline/` is fully gitignored and holds only what the fuzzer itself discovers (it's the first/writable dir in the list). `parse_arbitrary` predates this convention and still uses a committed snapshot; `compile_pipeline` (BT-3124) is the newer pattern and is the one to follow for any future fuzz target.

**Technology:** cargo-fuzz (libFuzzer)

`compile_pipeline` additionally asserts, whenever `generate_module` returns `Ok`, that the output is structurally valid Core Erlang — the same checks `core_erlang_validity_tests.rs`'s proptest suite runs, shared via `beamtalk_core::test_helpers::test_support::core_erlang_structural_issues` so the two never drift.

### Running Locally

```bash
# Fuzz both targets for 60 seconds each (default)
just fuzz

# Fuzz both targets for a specific duration each
just fuzz 300  # 5 minutes per target

# Or use cargo directly, one target at a time
cargo +nightly fuzz run parse_arbitrary -- -max_total_time=60
cargo +nightly fuzz run compile_pipeline fuzz/corpus/compile_pipeline stdlib/test tests/repl-protocol/cases -- -max_total_time=60
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

Fuzzing runs nightly (not per-PR) via `.github/workflows/fuzz.yml`, one job per target:
- Duration: 5 minutes per target per run (split from a single 10-minute `parse_arbitrary`-only run when `compile_pipeline` was added, BT-3124)
- Memory limit: 4GB RSS
- Artifacts uploaded on failure
- Auto-creates GitHub issues for crashes

**Why nightly?** Fuzzing is too slow for per-PR CI (minutes to hours). Nightly runs catch regressions without blocking development.

### Corpus-Through-BEAM Lint (`compile_pipeline` only)

`compile_pipeline`'s structural-validity check catches Core Erlang that's broken in ways beamtalk's own codegen can detect (unbalanced delimiters, missing `module`/`end`, Rust format-artifact leaks) — but not "codegen thinks this is fine, and it parses, but `erlc`/`core_lint` still rejects it" (e.g. an unbound variable — the BT-3115 bug class). Putting a real `erlc` compile in the libFuzzer hot loop would be far too slow, so this check runs as a separate nightly job instead:

1. `cargo run --release --example compile_pipeline_corpus -p beamtalk-core -- <out_dir> <corpus_dir>...` — runs every corpus file through the same lex/parse/analyse/codegen pipeline and writes each successful `Ok` output as a `.core` file.
2. `escript scripts/compile-pipeline-corpus-lint.escript <out_dir>` — batch-compiles every `.core` file with `compile:file/2` (`from_core`, `return_errors`, `return_warnings`, `clint`), the same options `beamtalk_build_worker.erl`/`beamtalk_compiler_server.erl` use in production. Prints an actionable per-file report (generated `.core` path, `erlc`/`core_lint` message) and exits non-zero on any failure.

Run both locally in one step:

```bash
just fuzz-corpus-lint
# Or against additional corpus dirs (e.g. fuzzer-grown corpus from a CI artifact):
just fuzz-corpus-lint "stdlib/test tests/repl-protocol/cases fuzz/corpus/compile_pipeline fuzz/artifacts/compile_pipeline"
```

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

**Keep corpus in sync:** When adding new `.bt` files to `examples/` or `tests/repl-protocol/cases/`, also copy them to `fuzz/corpus/parse_arbitrary/` so the fuzzer can use them as mutation seeds.

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

**"sanitizer is incompatible with statically linked libc":**
`cargo fuzz` defaults to `--target x86_64-unknown-linux-musl` (a fully static
binary) with AddressSanitizer enabled, and ASan can't instrument a
statically-linked libc. This has been confirmed to build and run fine on the
nightly CI runners (`.github/workflows/fuzz.yml`), so it's environment-specific
-- some sandboxed/restricted dev containers hit it locally. Work around it by
building against the dynamically-linked glibc target instead:
```bash
cargo +nightly fuzz run compile_pipeline --target x86_64-unknown-linux-gnu \
  fuzz/corpus/compile_pipeline stdlib/test tests/repl-protocol/cases -- -max_total_time=60
```
Or use `--sanitizer none` to keep the default musl target without ASan
instrumentation (still real coverage-guided libFuzzer mutation, just without
memory-sanitizer checks -- less relevant here since these targets are pure
safe Rust).

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

## Grammar-Driven Program Generator (BT-3116)

`near_valid_beamtalk()` in `core_erlang_validity_tests.rs` builds inputs from
a small hand-curated `FRAGMENTS` array plus truncation/concatenation —
useful for "never panics" robustness, but shallow: it can't reach nested
blocks with captures, `^` inside nested closures, or multi-statement bodies
threading local state, and shrinking only ever truncates a string rather
than simplifying a tree. `arb_program` generates *well-formed* programs as
typed AST values instead — `Object subclass:` with a single `run` method
whose body is built from a small grammar (literals, `true`/`false`/`nil`,
local/parameter references, unary/binary/keyword sends, `ifTrue:ifFalse:`,
self-invoking blocks, and a staged prelude-then-tail body that threads
freshly bound locals into later statements and occasionally returns early
via `^`). Because the values are real `ast::Module` trees, proptest's
built-in shrinking simplifies the tree structurally (fewer statements,
smaller sub-expressions) instead of chopping the string arbitrarily.

**Location:** `crate::test_helpers::test_support::arb_program` (`crates/beamtalk-core/src/test_helpers.rs`)

**Properties:** `crates/beamtalk-codegen/src/core_erlang_validity_tests.rs`, in a second `proptest!` block below the `FRAGMENTS`-based one (that block is kept as-is — it intentionally also covers ill-formed/truncated input this generator never produces):

| Property | What it verifies |
|---|---|
| `program_gen_round_trip` | A generated program's `unparse_module` output re-parses with zero error diagnostics |
| `program_gen_codegen_validity` | Whenever `generate_module` accepts a generated program, its output passes the same structural-validity checks (`core_erlang_structural_issues`) as the `FRAGMENTS`-based properties |

### Running Locally

```bash
cargo test -p beamtalk-core --lib core_erlang_validity_tests
# Extended run, matches nightly-style depth:
PROPTEST_CASES=3000 cargo test -p beamtalk-core --lib core_erlang_validity_tests --release -- --nocapture
```

### Scope (Tier 1 only)

The grammar currently covers expression-level shapes reachable from a single
method body: literals, sends, conditionals, blocks, and local-variable
threading. It does **not** generate class definitions with multiple methods,
actor state (`state:` declarations), field mutation, `whileTrue:`, or
collection literals — a second, feature-flagged tier covering those shapes
was considered but deliberately deferred (per the originating issue: "don't
gate on the generator issue; the bootstrap corpus is enough to start").
Extend `arb_expr`/`arb_body` in `test_support` when a new shape needs
generator coverage rather than adding a second generator elsewhere.

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

## Surface Parity

When adding or modifying operations across surfaces (CLI, REPL, MCP, LSP), consult the [Surface Parity Map](surface-parity.md) to ensure consistent coverage. Any operation not labelled `surface-specific` must produce equivalent output across all surfaces where it appears.

---

## References

- [Surface Parity Map](surface-parity.md) - Cross-surface operation coverage matrix
- [ADR 0014: Beamtalk Test Framework](../ADR/0014-beamtalk-test-framework.md) - Architecture decision for the three-layer test strategy
- [test-package-compiler/README.md](../../test-package-compiler/README.md) - Snapshot test details
- [tests/repl-protocol/README.md](../../tests/repl-protocol/README.md) - REPL protocol test framework details
- [runtime/README.md](../../runtime/README.md) - Erlang runtime test details
- [AGENTS.md](../../AGENTS.md) - Development guidelines
- [insta documentation](https://insta.rs/) - Snapshot testing framework

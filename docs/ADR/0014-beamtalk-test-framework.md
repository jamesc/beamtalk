# ADR 0014: Beamtalk Test Framework — Native Unit Tests and CLI Integration Tests

## Status
Proposed (2026-02-10)

## Context

### Problem Statement

Beamtalk currently has **40 E2E test files** (~3,400 lines) in `tests/e2e/cases/` that test language features. These tests work by:

1. Starting a REPL daemon (Erlang node + TCP server)
2. Sending each expression over TCP to the REPL
3. Reading back the result and comparing against `// => expected` comments
4. Running serially, one expression at a time

This approach has served us well for validating the full compilation pipeline, but it has serious limitations:

**Performance:** The full E2E suite takes ~90 seconds. Each expression requires a TCP round-trip through the REPL. As the language grows, this will become a bottleneck.

**Misclassification:** Most E2E tests are actually **unit tests for language features** (arithmetic, string operations, block semantics) — they don't need the REPL at all. They test that `1 + 2` equals `3`, not that the REPL can evaluate `1 + 2`.

**Missing real E2E coverage:** We have no tests for actual end-to-end workflows: `beamtalk build`, `beamtalk repl` session management, `beamtalk test`, workspace lifecycle, CLI argument parsing.

**No native test framework:** The language features doc (line 1049) describes a `TestCase subclass:` pattern but there's no implementation. Users can't write tests in Beamtalk itself.

### Current State

| Layer | Tool | Speed | What it tests |
|-------|------|-------|---------------|
| Rust unit tests | `cargo test` | Fast (~5s) | Parser, AST, codegen |
| Erlang unit tests | `rebar3 eunit` | Fast (~3s) | Runtime, primitives, object system |
| Compiler snapshot tests | `cargo test` | Fast (~2s) | 51 codegen snapshots |
| E2E tests | `just test-e2e` | **Slow (~90s)** | Language features via REPL |

The testing pyramid is inverted for language feature testing — everything goes through E2E when most could be fast compiled tests.

### Constraints

1. Tests must compile to BEAM bytecode (same pipeline as user code)
2. Must work with EUnit (Erlang's standard test framework) for CI integration
3. Must not require a running REPL daemon for pure language tests
4. Must preserve the existing E2E suite for REPL/workspace integration testing
5. Should feel idiomatic to Smalltalk developers (SUnit heritage)

## Decision

Implement a **two-phase test framework** with a phased rollout:

### Phase 1: Compiled Expression Tests (`beamtalk test`)

Reuse the existing `// =>` assertion format but compile test files directly to EUnit modules — no REPL needed.

**Test file format** (unchanged from current E2E):
```beamtalk
// test/integer_test.bt

// Basic arithmetic
1 + 2
// => 3

5 negated
// => -5

// String operations
'hello' size
// => 5

'hello' , ' world'
// => 'hello world'
```

**What changes:** The compiler parses `// =>` comments as test assertions at compile time and generates EUnit test functions:

```erlang
%% Generated from test/integer_test.bt
-module(integer_test).
-include_lib("eunit/include/eunit.hrl").

line_3_test() ->
    ?assertEqual(3, beamtalk_integer:dispatch(1, '+', [2])).

line_6_test() ->
    ?assertEqual(-5, beamtalk_integer:dispatch(5, negated, [])).

line_10_test() ->
    ?assertEqual(5, beamtalk_string:dispatch(<<"hello">>, size, [])).

line_13_test() ->
    ?assertEqual(<<"hello world">>, beamtalk_string:dispatch(<<"hello">>, ',', [<<" world">>])).
```

**CLI command:**
```bash
$ beamtalk test
Compiling 40 test files...
Running tests...

  integer_test: 12 tests, 12 passed ✓
  string_test: 8 tests, 8 passed ✓
  block_test: 15 tests, 15 passed ✓
  ...

40 files, 287 tests, 287 passed, 0 failed (1.2s)
```

**Key properties:**
- Same `// =>` format developers already know
- Compiles to EUnit — runs in ~1-2 seconds (vs ~90 seconds via REPL)
- No REPL daemon needed
- Existing test files work with minimal changes
- Tests with `@load` directives or workspace bindings remain E2E (need REPL)

**REPL session:**
```
> :test test/integer_test.bt
Compiling 1 test file...
12 tests, 12 passed ✓ (0.1s)
```

**Error output when a test fails:**
```
FAIL test/integer_test.bt:3
  Expression: 1 + 2
  Expected:   4
  Actual:     3
```

### Phase 2: SUnit-style TestCase Classes

Add a `TestCase` base class enabling idiomatic Smalltalk-style test classes:

```beamtalk
// test/counter_test.bt
@load tests/e2e/fixtures/counter.bt

Object subclass: CounterTest

  setUp =>
    self.counter := Counter spawn

  testIncrement =>
    self.counter increment await
    self assert: (self.counter getValue await) equals: 1

  testMultipleIncrements =>
    3 timesRepeat: [self.counter increment await]
    self assert: (self.counter getValue await) equals: 3

  testInitialValue =>
    self assert: (self.counter getValue await) equals: 0
```

**Key properties:**
- Methods starting with `test` are auto-discovered
- `setUp`/`tearDown` lifecycle methods run before/after each test
- Assertion methods: `assert:`, `assert:equals:`, `deny:`, `should:raise:`
- Compiles to EUnit like Phase 1
- `TestCase` is a stdlib class (written in Beamtalk with `@primitive` methods)

**REPL session:**
```
> :load test/counter_test.bt
> CounterTest runAll
3 tests, 3 passed ✓ (0.3s)

> CounterTest run: #testIncrement
1 test, 1 passed ✓ (0.1s)
```

**Error output:**
```
FAIL CounterTest >> testMultipleIncrements
  assert:equals: failed
  Expected: 4
  Actual:   3
  Location: test/counter_test.bt:12
```

### E2E Tests: Kept for Integration

The existing `tests/e2e/` suite continues to test REPL-specific behavior:

```beamtalk
// tests/e2e/cases/workspace_bindings.bt
// These NEED the REPL because they test workspace state

Transcript show: 'Hello from E2E'
// => nil

Transcript class
// => TranscriptStream
```

**Criteria for E2E vs compiled test:**
| Test needs... | Use |
|---|---|
| Pure language features (arithmetic, strings, blocks) | `beamtalk test` (compiled) |
| Actor spawning + messaging | `beamtalk test` with `@load` |
| Workspace bindings (Transcript, Beamtalk) | E2E (needs REPL) |
| REPL commands (`:load`, `:quit`, `:help`) | E2E (needs REPL) |
| CLI commands (`beamtalk build`, `beamtalk test`) | E2E / integration test |

## Prior Art

### SUnit (Pharo/Squeak Smalltalk)

The original xUnit framework. Tests are classes inheriting from `TestCase` with methods prefixed `test`. Supports `setUp`/`tearDown`, `assert:equals:`, `should:raise:`.

**What we adopt:** Class structure, naming conventions, assertion API.
**What we adapt:** No `poolDictionaries` or `classVariableNames`. BEAM process isolation replaces Smalltalk image-level isolation.

### EUnit (Erlang)

Erlang's built-in test framework. Test functions end in `_test` or `_test_`. Supports `?assertEqual`, `?assertMatch` macros, test generators, fixtures.

**What we adopt:** EUnit as the compilation target (Phase 1 and 2 both generate EUnit modules).
**What we adapt:** Beamtalk syntax instead of Erlang macros.

### ExUnit (Elixir)

Compile-time test generation via macros. `test "name" do ... end` blocks, `assert` macro, `setup`/`setup_all` callbacks. Data-driven test generation with `for`.

**What we adopt:** The idea of compile-time test generation (our compiler does what Elixir's macro system does).
**What we adapt:** Message-passing assertions instead of macro-based assertions.

### Gleam Testing

`gleam test` compiles test modules, runs each test in its own BEAM process. Simple `should.equal(actual, expected)` assertions. Process isolation for crash safety.

**What we adopt:** The `beamtalk test` CLI pattern, process isolation.
**What we adapt:** SUnit-style assertions instead of function-call assertions.

## User Impact

### Newcomer (from Python/JS)

Phase 1 is immediately accessible — the `// =>` format is like doctest in Python or inline assertions in tutorials. No new concepts to learn. Phase 2 introduces test classes, familiar from any xUnit framework.

### Smalltalk Developer

Phase 2 is exactly what they expect — SUnit is the canonical Smalltalk test framework. `TestCase subclass: MyTest` with `assert:equals:` is home territory. Phase 1 is a nice bonus for quick checks.

### Erlang/Elixir Developer

Both phases compile to EUnit, which they already know. `beamtalk test` works like `rebar3 eunit` or `mix test`. No new test infrastructure to learn at the BEAM level.

### Production Operator

Tests run fast (1-2s compiled vs 90s E2E). CI pipelines are faster. EUnit output integrates with existing CI tools. BEAM process isolation means crashed tests don't take down the suite.

## Steelman Analysis

### Alternative: Keep Pure E2E (Current Approach)

| Cohort | Their strongest argument |
|--------|------------------------|
| 🧑‍💻 **Newcomer** | "The current format is dead simple — I write an expression and the expected result. No classes, no imports, no boilerplate." |
| 🎩 **Smalltalk purist** | "In Smalltalk, we test in the live image. The REPL *is* the test environment. Compiling tests separately breaks the interactive-first promise." |
| ⚙️ **BEAM veteran** | "EUnit already works fine for Erlang. Why add another layer? Just keep testing through the REPL." |
| 🏭 **Operator** | "90 seconds is acceptable for a CI pipeline. Don't add complexity for marginal speed gains." |
| 🎨 **Language designer** | "The test format IS the language tutorial format. Keeping them the same means examples are always tested." |

**Rebuttal:** The speed problem will get worse as the language grows. 40 files at 90s means 200 files will take 7+ minutes. And we genuinely lack REPL/CLI integration tests because everything goes through the same slow path.

### Alternative: Phase 2 Only (SUnit-style from the Start)

| Cohort | Their strongest argument |
|--------|------------------------|
| 🧑‍💻 **Newcomer** | "One test framework to learn, not two. TestCase classes are universal." |
| 🎩 **Smalltalk purist** | "SUnit IS the Smalltalk way. Skipping it for a simpler format is a disservice to the language's heritage." |
| ⚙️ **BEAM veteran** | "Class-based tests map directly to EUnit fixtures. setUp/tearDown = EUnit setup. Clean." |
| 🏭 **Operator** | "One framework, one test command, one CI step. Simplicity." |
| 🎨 **Language designer** | "TestCase classes use the class system — testing the language WITH the language is dogfooding at its best." |

**Rebuttal:** Phase 2 requires the class system to be more mature (class-side methods, instantiation protocol). Phase 1 works today with zero language additions. Delivering Phase 1 first gives us fast tests immediately while Phase 2 develops.

### Tension Points

- **Newcomers** prefer Phase 1's simplicity; **Smalltalk purists** want Phase 2's heritage
- **BEAM veterans** are happy either way (both compile to EUnit)
- **Language designers** see Phase 1 as pragmatic and Phase 2 as aspirational
- The phased approach resolves most tension: deliver simplicity first, heritage second

## Alternatives Considered

### Alternative A: Optimize Current E2E Runner (Parallel Execution)

Run E2E tests in parallel REPL sessions to reduce wall-clock time.

**Why rejected:** Treats the symptom (speed) not the cause (architectural mismatch). Most tests don't need a REPL. Parallel REPL sessions add complexity and race condition risks.

### Alternative B: Pragma-Based Test Methods (`@test`)

```beamtalk
@test 'addition'
(1 + 2) assertEquals: 3
```

**Why rejected:** New pragma syntax for something that can be achieved with naming conventions (Phase 2) or existing comment syntax (Phase 1). Adds parser complexity without clear benefit over either phase.

### Alternative C: Property-Based Testing First

Focus on property-based testing (QuickCheck/PropEr style) instead of unit tests.

**Why rejected:** Property-based testing is valuable but requires a more mature type system and standard library. Better as a Phase 3 addition built on top of TestCase.

## Consequences

### Positive

- **50-100x faster** language feature tests (1-2s compiled vs 90s via REPL)
- **Correct testing pyramid** — unit tests are fast, E2E tests are focused
- **Native test framework** — users can write tests in Beamtalk itself
- **Dogfooding** — Phase 2 exercises the class system, proving it works
- **CI speed** — faster feedback loop for development
- **Tutorial compatibility** — Phase 1 test files ARE tutorials (same `// =>` format)

### Negative

- **Two test formats** — Phase 1 (`// =>`) and Phase 2 (TestCase classes) coexist
- **Compiler complexity** — assertion parsing and EUnit generation are new codegen paths
- **Migration effort** — moving 40 E2E files to compiled tests requires classifying which need REPL
- **Phase 2 dependency** — requires class instantiation protocol (ADR 0013) and setUp/tearDown lifecycle

### Neutral

- **EUnit dependency** — we already depend on EUnit for Erlang tests
- **Test file location** — need to decide convention (`test/`, `tests/`, alongside source)
- **Existing E2E tests preserved** — no breaking changes to current workflow

## Implementation

### Phase 1: Compiled Expression Tests

**Effort:** M (Medium) — ~250-350 lines across 5-7 files

| Component | Location | Description |
|-----------|----------|-------------|
| Assertion parser | `crates/beamtalk-core/src/source_analysis/parser/` | Parse `// =>` as `TestAssertion` AST nodes |
| EUnit codegen | `crates/beamtalk-core/src/codegen/core_erlang/` | Generate EUnit test functions from assertion pairs |
| `beamtalk test` CLI | `crates/beamtalk-cli/src/commands/test.rs` | Scan dir → compile → run EUnit → format output |
| Test classifier | `crates/beamtalk-cli/src/commands/test.rs` | Detect `@load` / workspace bindings → route to E2E |
| Output formatter | `crates/beamtalk-cli/src/commands/test.rs` | Parse EUnit output → user-friendly format |

**Affected layers:** Parser (Rust), Codegen (Rust), CLI (Rust), minimal Erlang glue.

### Phase 2: SUnit-style TestCase

**Effort:** XL (Extra Large) — ~650-750 lines across 8-12 files

| Component | Location | Description |
|-----------|----------|-------------|
| TestCase class | `lib/TestCase.bt` | Assertion methods, lifecycle hooks |
| TestCase runtime | `runtime/apps/beamtalk_runtime/src/` | `beamtalk_test_case.erl` — assertion primitives |
| Test discovery | `crates/beamtalk-cli/src/commands/test.rs` | Find TestCase subclasses, extract `test*` methods |
| EUnit bridge | `crates/beamtalk-core/src/codegen/core_erlang/` | Generate EUnit wrappers from TestCase methods |
| TestResult class | `lib/TestResult.bt` (optional) | Collect and report test results |

**Depends on:** ADR 0013 (class instantiation protocol — `new` for value objects), method introspection.

### Phase 3: Future Enhancements (Out of Scope)

- Property-based testing (`PropertyTest` class)
- Test coverage reporting
- Watch mode (`beamtalk test --watch`)
- IDE integration (LSP test discovery)

## Migration Path

### Moving E2E Tests to Compiled Tests

1. **Classify** each `tests/e2e/cases/*.bt` file:
   - No `@load`, no workspace bindings → move to `test/` (compiled)
   - Uses `@load` but no workspace bindings → move to `test/` with `@load` support
   - Uses workspace bindings → keep in `tests/e2e/` (needs REPL)

2. **Gradual migration** — move files one at a time, verify tests still pass

3. **Expected split:**
   - ~30 files → compiled tests (pure language features)
   - ~10 files → remain E2E (REPL/workspace integration)

### Test Directory Convention

```
test/                    # Compiled Beamtalk tests (Phase 1 + 2)
├── integer_test.bt      # Phase 1: expression tests
├── string_test.bt       # Phase 1: expression tests
├── counter_test.bt      # Phase 2: TestCase class
└── fixtures/
    └── counter.bt       # Shared test fixtures

tests/e2e/               # REPL integration tests (keep existing)
├── cases/
│   ├── workspace_bindings.bt
│   └── repl_commands.bt
└── fixtures/
    └── counter.bt
```

## References

- Language features doc: `docs/beamtalk-language-features.md` (lines 1049-1079)
- Testing strategy: `docs/development/testing-strategy.md`
- Architecture principles: `docs/development/architecture-principles.md` (testing pyramid)
- Related ADRs: ADR 0007 (stdlib compilation — reusable pattern), ADR 0013 (class instantiation — Phase 2 dependency)
- Prior art: [SUnit (Pharo)](https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Book%3A_Pharo_by_Example_5.0_(Ducasse_Zagidulin_Hess_and_Chloupis)/09%3A_SUnit), [EUnit (Erlang)](https://www.erlang.org/doc/apps/eunit/), [ExUnit (Elixir)](https://hexdocs.pm/ex_unit/ExUnit.html)

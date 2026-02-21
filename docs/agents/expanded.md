<!-- Full expanded AGENTS.md backup. Use this for detailed guidance. -->
# AGENTS.md - Beamtalk Development Guidelines (Expanded)

This document provides guidance for AI coding agents working on the beamtalk compiler and ecosystem.

## Repository Information

**Always use these values for GitHub API calls:**

| Property | Value |
|----------|-------|
| Owner | `jamesc` |
| Repository | `beamtalk` |
| Full name | `jamesc/beamtalk` |
| URL | `https://github.com/jamesc/beamtalk` |

Example `gh` CLI usage:
```bash
gh api repos/jamesc/beamtalk/pulls
gh pr create --repo jamesc/beamtalk
```

## Project Overview

Beamtalk is a Smalltalk/Newspeak-inspired programming language that compiles to the BEAM virtual machine. The compiler is written in Rust and generates Core Erlang, which is then compiled to BEAM bytecode via erlc.

**Key principle:** Beamtalk is an **interactive-first** language. The live environment and hot code reloading are core to the design, not afterthoughts.

**Important:** While heavily inspired by Smalltalk, Beamtalk makes pragmatic departures from Smalltalk syntax and semantics to work well with BEAM and modern development practices. We're "Smalltalk-like," not Smalltalk-compatible. See [beamtalk-syntax-rationale.md](../beamtalk-syntax-rationale.md) for specific differences, [beamtalk-principles.md](../beamtalk-principles.md) for design philosophy, and [beamtalk-language-features.md](../beamtalk-language-features.md) for full language specification.

---

## Syntax Verification - Preventing Hallucinations 🚨

**CRITICAL:** AI agents must **verify all Beamtalk syntax** before using it in code, tests, or examples. Do not invent or assume syntax patterns exist.

### Common Hallucination Patterns

| Pattern | Example | Reality |
|---------|---------|---------|
| **Inline class syntax** | `Counter := Actor [...]` | Only `Actor subclass: Counter` works |
| **Ruby/Python-style** | `Point.new(x: 3)` | Beamtalk uses `Point new: #{x => 3}` |
| **Compound assignment** | `self.value += 1` | Use `self.value := self.value + 1` (ADR 0001) |
| **Missing constraints** | `Counter new` on Actor | Error — actors use `spawn`, not `new` |
| **Sealed subclassing** | `Integer subclass: MyInt` | Primitives are sealed, cannot subclass |
| **Smalltalk verbatim** | `Object subclass: #Counter` | Beamtalk uses identifiers, not symbols |
| **Load syntax confusion** | `@load` in REPL | Use `:load` in REPL, `@load` only in test files |
| **Pragma confusion** | `@primitive blockValue` | Structural intrinsics use `@intrinsic`; `@primitive` is for quoted selectors only |

### Test File Rules

In test files (`stdlib/bootstrap-test/*.bt` and `tests/e2e/cases/*.bt`):
- **Every expression MUST have a `// =>` assertion** (even `// => _` for wildcard)
- **No assertion = no execution** — expressions are silently skipped
- **Missing assertions fail CI** (BT-249)
- **Pattern matching in assertions:** `_` within a pattern is a wildcard segment (e.g., `#Actor<Counter,_>`)
- `@load` is a test directive; `:load` is a REPL command — don't mix them up

**⚠️ Dangerous pattern — this looks fine but DOESN'T RUN:**
```beamtalk
x := 0
3 timesRepeat: [x := x + 1]   // No assertion → SKIPPED, never executes!
x
// => 3                        // Fails! x was never set
```

**Safe pattern — always add assertions:**
```beamtalk
x := 0
// => _
3 timesRepeat: [x := x + 1]
// => nil
x
// => 3
```

### Verification Before Using Syntax

Before using ANY Beamtalk syntax, verify it exists in at least one of:
1. **Language spec:** [docs/beamtalk-language-features.md](../docs/beamtalk-language-features.md)
2. **Examples:** `examples/*.bt`
3. **Tests:** `stdlib/bootstrap-test/*.bt`, `tests/e2e/cases/*.bt`
4. **Parser tests:** `crates/beamtalk-core/src/source_analysis/parser/`

**If it doesn't appear in any of these → it's likely hallucinated. Search the codebase or ask.**

---

## Domain-Driven Design (DDD)

The beamtalk architecture follows **Domain-Driven Design**. Use domain terms consistently (e.g., `CompletionProvider` not `completions`). The codebase has four bounded contexts: **Language Service** (IDE features), **Compilation** (lexer→codegen), **Runtime** (actors, OTP), and **REPL** (interactive dev).

All new modules should include a `//! **DDD Context:** ContextName` header. See [docs/beamtalk-ddd-model.md](../docs/beamtalk-ddd-model.md) for the full domain model.

---

## Development Architecture Principles

The beamtalk codebase follows strict architectural principles for code organization, error handling, testing, security, and dependencies. Full details in [docs/development/architecture-principles.md](../docs/development/architecture-principles.md).

**Core principles:**
1. **Layered Architecture** - Dependencies flow down only (core never depends on CLI)
2. **Error Recovery** - Return partial results + diagnostics (don't stop at first error)
3. **Testing Pyramid** - Unit 60-70%, Integration 20-30%, E2E 10%
4. **Security-First** - Input validation at boundaries, no unsafe without justification
5. **Minimal Dependencies** - Prefer std library, document why each dependency exists

**Critical rules:**

❌ **NEVER:**
- `beamtalk-core` importing `beamtalk-cli` or `beamtalk-lsp`
- Panic on user input (malformed source, invalid args, missing files)
- Add dependencies without security review and commit message justification
- Use `unwrap()` on user input
- **Use bare tuple errors in user-facing code** - Public API errors MUST use `#beamtalk_error{}` records (internal runtime helpers may use `{ok, Value} | {error, Reason}` if translated at boundaries)

✅ **ALWAYS:**
- Return `(Result, Vec<Diagnostic>)` or equivalent for user-facing operations
- Validate file paths and buffer boundaries
- Document unsafe code with `// SAFETY:` comment explaining invariants
- Run `cargo audit` before releases
- **Use structured errors at public API boundaries** - `beamtalk_error:new/with_selector/with_hint` for all user-facing error paths

See full guide: [docs/development/architecture-principles.md](../docs/development/architecture-principles.md)

---

## Error Handling - CRITICAL RULES

All **user-facing/public API errors** MUST use the structured `#beamtalk_error{}` system. Internal runtime helpers (in `runtime/**/*.erl`) may use bare `{ok, Value} | {error, Reason}` tuples, but these MUST be translated to `#beamtalk_error{}` at public API boundaries before reaching user code.

### Structured Error System

All errors use `#beamtalk_error{}` records defined in `runtime/include/beamtalk.hrl`:

```erlang
-record(beamtalk_error, {
    kind    :: atom(),              % does_not_understand | immutable_value | type_error | instantiation_error | ...
    class   :: atom(),              % 'Integer', 'Counter', 'Actor'  
    selector:: atom() | undefined,  % method that failed
    message :: binary(),            % human-readable explanation
    hint    :: binary() | undefined,% actionable suggestion
    details :: map()                % additional context
}).
```

### In Runtime Erlang Code

Use `beamtalk_error` module helpers:

```erlang
Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
Error1 = beamtalk_error:with_selector(Error0, 'foo'),
Error2 = beamtalk_error:with_hint(Error1, <<"Check spelling">>),
error(Error2)
```

### In Generated Core Erlang Code

Codegen MUST use `beamtalk_error` calls:

```erlang
%% ❌ WRONG - bare tuple (never do this!)
call 'erlang':'error'({'some_error', 'message'})

%% ✅ RIGHT - structured error
let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in
let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawn instead">>) in
call 'erlang':'error'(Error2)
```

### Error Kinds

| Kind | When | Example |
|------|------|---------|
| `does_not_understand` | Unknown method | `42 foo` |
| `immutable_value` | Mutation on primitive | `42 instVarAt:put:` |
| `type_error` | Wrong argument type | `"hello" + 42` |
| `arity_mismatch` | Wrong argument count | Missing/extra args |
| `instantiation_error` | Wrong instantiation | `Actor new` (use `spawn`) |
| `future_not_awaited` | Message to Future | `(future) size` |
| `timeout` | Operation timeout | Await exceeds deadline |

### Benefits of Structured Errors

1. **Consistent tooling** - Pattern match on kind/class/selector
2. **Better UX** - Actionable hints guide users
3. **Rich context** - Details map for debugging
4. **Future-proof** - Easy to add metadata without breaking changes

See full error taxonomy: [docs/internal/design-self-as-object.md](../docs/internal/design-self-as-object.md#38-error-handling-taxonomy)

---

## Logging - CRITICAL RULES

**Erlang runtime:** Use OTP logger **macros** (`?LOG_ERROR`, `?LOG_WARNING`, `?LOG_INFO`, `?LOG_DEBUG`), never `io:format` or `logger:error()` function calls. Macros auto-include caller MFA metadata. Every `.erl` file that logs must include `-include_lib("kernel/include/logger.hrl")`. Always use structured metadata maps, not format strings.

**Rust CLI:** Use the `tracing` crate (`trace!`, `debug!`, `info!`, `warn!`, `error!`). Use `println!` for user-facing output, tracing for diagnostics. Enable with `RUST_LOG=beamtalk_cli=debug`.

See [docs/development/erlang-guidelines.md](../docs/development/erlang-guidelines.md#logging-with-otp-logger) for full logging guidelines.

---

## User Experience & Developer Experience (DevEx) First

**A feature is not done until the user can interact with it in the REPL.** Building internal infrastructure is necessary but insufficient — always complete the loop to user-facing integration.

Before marking any feature complete, verify:
- [ ] Feature works in the REPL — can demonstrate in 1-2 lines
- [ ] Error messages are user-friendly and actionable (use `self` not `Self`)
- [ ] Examples in `examples/` or docs show real usage
- [ ] Failure modes tested and produce helpful output

🚩 **Red flags:** "Infrastructure is done" but no user sees it. "Tests pass" but no examples. "Error handling works" but messages are raw tuples.

---

## Scope Control

**Fix inline:** formatting/typos in files you're modifying, test failures from your changes, clippy warnings in your code, docs for features you just added.

**Create follow-up issue:** bugs in unrelated code, performance optimization opportunities, missing tests in other modules, refactoring not required for current issue.

🚩 **Scope creep red flags:** PR grows beyond 10 files, "just one more thing", acceptance criteria met but still coding.

---

## Debugging

See [docs/development/debugging.md](../docs/development/debugging.md) for step-by-step debugging guides covering compiler crashes, runtime errors, test failures, E2E failures, codegen debugging, and performance profiling.

**If stuck for >30 minutes:** Stop and ask. Summarize what you tried, share the error, ask for direction.

---

## Work Tracking

We use **Linear** for task management. Project prefix: `BT`

### Referencing Issues

Include the Linear issue ID in commits and PR titles:
```
git commit -m "Implement lexer tokens BT-123"
```

### Issue Lifecycle

| State | Meaning |
|-------|---------|
| **Backlog** | Idea captured, not yet specified |
| **Ready** | Fully specified with acceptance criteria; agent can pick up |
| **In Progress** | Actively being worked on |
| **In Review** | Code complete, needs human verification |
| **Done** | Merged and verified |

### Labels

We use several label groups to categorize issues:

#### Agent State

Tracks workflow status:

- `agent-ready` - Task is fully specified, agent can start immediately
- `needs-spec` - Requires human to clarify requirements before work begins
- `blocked` - Waiting on external dependency or another issue
- `human-review` - Agent completed work, needs human verification
- `done` - Issue is complete and closed

#### Item Area

Identifies which component of the codebase the issue affects:

| Label | Description | Key Directories |
|-------|-------------|----------------|
| `class-system` | Class definition, parsing, codegen, and runtime | `crates/beamtalk-core/src/ast.rs`, `crates/beamtalk-core/src/source_analysis/` |
| `stdlib` | Standard library: collections, primitives, strings (compiled from `stdlib/src/*.bt` via pragmas — see [ADR 0007](../docs/ADR/0007-compilable-stdlib-with-primitive-injection.md)) | `stdlib/src/` |
| `repl` | REPL backend and CLI interaction | `runtime/src/beamtalk_repl.erl`, `crates/beamtalk-cli/src/repl/` |
| `cli` | Command-line interface and build tooling | `crates/beamtalk-cli/` |
| `codegen` | Code generation to Core Erlang/BEAM | `crates/beamtalk-core/src/erlang.rs` |
| `runtime` | Erlang runtime: actors, futures, OTP integration | `runtime/src/` |
| `parser` | Lexer, parser, AST | `crates/beamtalk-core/src/source_analysis/`, `crates/beamtalk-core/src/ast.rs` |

#### Issue Type

Categorizes the kind of work:

- `Epic` - **Large initiatives that group related issues (5+ child issues)**
- `Feature` - A chunk of customer visible work
- `Bug` - Bugs, broken tests, broken code
- `Improvement` - Incremental work on top of a feature
- `Documentation` - Words that explain things
- `Infra` - Tools, CI, dev environment configuration
- `Language Feature` - New Beamtalk language syntax/semantics
- `Refactor` - Code cleanups, tech debt
- `Research` - Research projects, code spikes
- `Samples` - Code, examples, things to help devs get started

#### Item Size

T-shirt sizing for estimates: `S`, `M`, `L`, `XL`

**When creating issues:** Always set:
1. An **Agent State** label (`agent-ready` or `needs-spec`)
2. An **Item Area** label (what part of codebase)
3. An **Issue Type** label (what kind of work)
4. An **Item Size** label (how big)

### Epics

**Epics** group 5+ related issues for large initiatives. Use `Epic:` title prefix, `Epic` label, and size XL or L. Link child issues with "blocks" relationships. Include overview, goals, status summary, and child issue list in description.

Query Linear for epic status: `streamlinear-cli search "Epic:" --state "In Progress"`

### Writing Agent-Ready Issues

For an issue to be `agent-ready`, include:

1. **Context** - Why this work matters, background info
2. **Acceptance Criteria** - Specific, testable requirements (checkboxes)
3. **Files to Modify** - Explicit paths to relevant files
4. **Dependencies** - Other issues that must complete first
5. **References** - Links to specs, examples, or related code
6. **Blocking Relationships** - Use Linear's "blocks" relationship for dependencies

Example:
```
Title: Implement basic lexer token types

Context:
The lexer is the first phase of compilation. It needs to tokenize
Smalltalk-style message syntax including identifiers, numbers, and keywords.

Acceptance Criteria:
- [ ] Tokenize identifiers (letters, digits, underscores)
- [ ] Tokenize integers and floats
- [ ] Tokenize single and double quoted strings
- [ ] Tokenize message keywords ending in `:`
- [ ] Tokenize block delimiters `[` `]`
- [ ] All tokens include source span

Files to Modify:
- crates/beamtalk-core/src/source_analysis/token.rs
- crates/beamtalk-core/src/source_analysis/lexer.rs

Dependencies: None

References:
- See Gleam lexer: github.com/gleam-lang/gleam/blob/main/compiler-core/src/parse/lexer.rs
```

### Creating Issue Blocking Relationships

When creating issues with dependencies, **always** set up Linear's "blocks" relationships:

```typescript
// After creating issues BT-X and BT-Y, where BT-X blocks BT-Y:
mutation {
  issueRelationCreate(input: {
    issueId: "<BT-X issue ID>"
    relatedIssueId: "<BT-Y issue ID>"
    type: blocks
  }) {
    success
  }
}
```

**Rules:**
- If issue A must be completed before issue B can start, then A "blocks" B
- Always create blocking relationships when dependencies are mentioned
- Linear automatically shows blocked/blocking status in the UI
- Use GraphQL to create relationships after issue creation
- **Set agent-state label** when creating issues:
  - `agent-ready` if fully specified with all acceptance criteria
  - `needs-spec` if human clarification needed first

**Example:** For stdlib implementation issues:
- BT-21 (API definitions) blocks BT-32, BT-33, BT-34, BT-35, BT-36, BT-37
- BT-32 (block evaluation) blocks BT-35 (iteration uses blocks) and BT-37 (collections use blocks)
- All new issues marked with `agent-ready` label

---

## Architecture Decision Records (ADRs)

Significant design decisions are documented as ADRs in `docs/ADR/`. Create an ADR when a decision affects language design, core architecture, interoperability, or user-facing behavior.

**Format:** `docs/ADR/NNNN-kebab-case-title.md` with Status, Context, Decision, Consequences, References sections. Number sequentially, update `docs/ADR/README.md` index. See existing ADRs for format examples.

---

## Agent Skills

Agent skills are defined as personal skills in `~/.claude/skills/` (Claude Code) or `~/.copilot/skills/` (Copilot). They are not stored in the repository.

Available skills include: `pick-issue`, `done`, `resolve-pr`, `resolve-merge`, `create-issue`, `refresh-issue`, `update-issues`, `whats-next`, `draft-adr`, `plan-adr`, `plan-refactor`, `do-refactor`, `review-code`, `review-adr`.

---

## Repository Structure

```
runtime/
├── rebar.config            # Umbrella project config (ADR 0007, 0009)
├── apps/
│   ├── beamtalk_runtime/   # Core runtime OTP application (ADR 0009)
│   │   ├── src/            # Runtime source (primitives, object system, actors, futures)
│   │   ├── include/        # Header files (beamtalk.hrl)
│   │   ├── test/           # Runtime unit tests (*.erl)
│   │   ├── config/         # OTP sys.config
│   │   └── test_fixtures/  # Fixtures for runtime tests (BT-239)
│   │       ├── logging_counter.bt
│   │       ├── compile_fixtures.escript  # Auto-compiles fixtures via rebar3 pre-hook
│   │       └── README.md
│   ├── beamtalk_workspace/  # Workspace and interactive development OTP application (ADR 0009) — NEW
│   │   ├── src/            # REPL source (repl eval, server, workspace supervision, session management)
│   │   └── test/           # REPL unit tests (*.erl)
│   └── beamtalk_stdlib/    # Standard library OTP application (ADR 0007)
│       ├── src/            # Application metadata (beamtalk_stdlib.app.src)
│       └── ebin/           # Generated .beam files (Actor, Block, Integer, etc.) created by build-stdlib

tests/
├── stdlib/                # Compiled language feature tests (ADR 0014)
│   ├── arithmetic.bt      # ~32 test files, ~654 assertions
│   ├── blocks.bt          # Run via `just test-stdlib`
│   └── ...
└── e2e/
    ├── cases/             # REPL integration tests (~23 files)
    └── fixtures/          # Shared test fixtures
        └── counter.bt     # CANONICAL counter implementation (BT-239)

examples/
├── counter.bt             # Simple working example for REPL
├── hello.bt              # Minimal example
└── repl-tutorial.md      # Beginner tutorial (BT-239)
```

**Dependency direction (ADR 0009):**
```
beamtalk_workspace (interactive dev)
    ↓ depends on
beamtalk_runtime (core language runtime)
    ↓ depends on
beamtalk_stdlib (compiled stdlib)
```

### Test Organization - CRITICAL DISTINCTION

**IMPORTANT:** The test suite has multiple layers. Be precise about which tests you're referring to:

#### 1. Runtime Unit Tests
**Location:** `runtime/apps/beamtalk_runtime/test/*_tests.erl` (e.g., `beamtalk_actor_tests.erl`)
- Tests individual runtime modules in isolation
- Uses hand-written test fixtures (e.g., `test_counter.erl`)
- Calls `gen_server` protocol directly with raw pids
- Appropriate for testing low-level runtime behavior

#### 2. REPL Unit Tests
**Location:** `runtime/apps/beamtalk_workspace/test/*_tests.erl` (ADR 0009)
- Tests REPL evaluation, protocol, server, workspace management
- Includes session supervision and idle monitoring tests
- Moved from `beamtalk_runtime/test/` in BT-351

#### 3. Codegen Simulation Tests
**Location:** `runtime/apps/beamtalk_runtime/test/beamtalk_codegen_simulation_tests.erl`
- Tests using **real compiled Beamtalk code** from `tests/e2e/fixtures/counter.bt` (unified fixture - BT-239)
- The `spawn/0` and `spawn/1` tests use `counter:spawn()` from compiled module
- Other tests use simulated state structures for complex scenarios
- **Test fixtures compile automatically** via rebar3 pre-hook (no manual step needed)
- Fixtures: `logging_counter.bt` stored in `runtime/apps/beamtalk_runtime/test_fixtures/`, `counter.bt` sourced from E2E fixtures
- Compiled by `runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript` (rebar3 pre-hook)
- See `docs/development/testing-strategy.md` for compilation workflow details

#### 4. Stdlib Tests (Compiled Expression Tests) — ADR 0014
**Location:** `stdlib/bootstrap-test/*.bt` (~57 files)
- **Pure language feature tests** compiled directly to EUnit (no REPL needed)
- Uses same `// =>` assertion format as E2E tests
- Runs via `just test-stdlib` (fast, ~14s vs ~50s for E2E)
- Tests arithmetic, strings, blocks, closures, collections, object protocol, etc.
- Supports `@load` directives for fixture-dependent tests (actors, sealed classes)
- **Use this for any new test that doesn't need REPL/workspace features**

#### 5. BUnit Tests (TestCase Classes) — ADR 0014 Phase 2
**Location:** `stdlib/test/*.bt` (project test directory)
- **SUnit-style test classes** that subclass `TestCase`
- Methods starting with `test` are auto-discovered and run with fresh instances
- `setUp`/`tearDown` lifecycle methods for test fixtures
- Assertion methods: `assert:`, `assert:equals:`, `deny:`, `should:raise:`, `fail:`
- Runs via `beamtalk test` (compiles to EUnit, fast)
- Can also run interactively in REPL: `CounterTest runAll` or `CounterTest run: #testName`
- **Use this for stateful tests, complex actor interactions, multi-assertion scenarios**

#### 6. End-to-End Tests (REPL Integration)
**Location:** `tests/e2e/cases/*.bt` (~18 files)
- Require a running REPL daemon (started automatically by test harness)
- Test workspace bindings, REPL commands, variable persistence, auto-await
- Test `ERROR:` assertion patterns and `@load-error` directives
- Runs via `just test-e2e` (~50s)
- **Only use for tests that genuinely need the REPL**

**When choosing between stdlib, BUnit, and E2E tests:**
| Test needs... | Where |
|---|---|
| Pure language features (arithmetic, strings, blocks) | `stdlib/bootstrap-test/` |
| Collections (List, Dictionary, Set, Tuple) | `stdlib/bootstrap-test/` |
| Actor spawn + messaging (with `@load`) | `stdlib/bootstrap-test/` |
| Stateful tests with setUp/tearDown | `stdlib/test/*.bt` (BUnit) |
| Complex actor interactions, multiple assertions | `stdlib/test/*.bt` (BUnit) |
| Workspace bindings (Transcript, Beamtalk) | `tests/e2e/cases/` |
| REPL commands, variable persistence | `tests/e2e/cases/` |
| Auto-await, `ERROR:` assertions | `tests/e2e/cases/` |

#### Test Fixture Organization (BT-239)

**Runtime fixtures:** `runtime/apps/beamtalk_runtime/test_fixtures/`
- Colocated with runtime tests for better locality
- Compiled by `apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript` (rebar3 pre-hook)
- Currently: `logging_counter.bt` (super keyword tests)
- Note: `counter.bt` consolidated to E2E fixture

**E2E fixtures:** `tests/e2e/fixtures/`
- Used by E2E test cases
- `counter.bt` is the canonical Counter implementation
- Also used by runtime tests (unified fixture)

---

## Development Guidelines

Detailed coding standards are in `docs/development/`:

| Guide | Description |
|-------|-------------|
| [Rust Guidelines](../development/rust-guidelines.md) | Naming, traits, error handling, testing, compiler patterns |
| [Erlang Guidelines](../development/erlang-guidelines.md) | Code generation, OTP patterns, BEAM interop |
| [Common Tasks](../development/common-tasks.md) | Adding AST nodes, CLI commands, stdlib features |
| [Debugging](../development/debugging.md) | Step-by-step debugging for all failure types |
| [Language Features](../beamtalk-language-features.md) | Full Beamtalk syntax specification |

### CI Commands

```bash
just ci                      # Run all CI checks (build, lint, test, test-stdlib, test-integration, test-mcp, test-e2e)
just build                   # Build Rust + Erlang runtime
just test                    # Run fast tests (~10s)
just test-stdlib             # Compiled language feature tests (~14s)
just test-e2e                # Run E2E tests (~50s)
just fmt                     # Format all code
just clippy                  # Lints (warnings = errors)
just dialyzer                # Erlang type checking
```

### License Headers

All source files must include:
```text
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
```

### Beamtalk Style

- **Implicit returns**: Use `^` ONLY for early returns, never on last expression
- **No periods**: Newlines separate statements, not `.`
- **Comments**: Use `//` and `/* */`, not Smalltalk's `"..."`

### Code Generation — Document API Only

All Core Erlang codegen MUST use `Document` / `docvec!` API. Never use `format!()` or string concatenation. If you see existing string-based patterns, convert them.

### Clippy Discipline

Never suppress clippy warnings without a comment explaining why. Split long functions, remove dead code, fix return types. Goal: under 30 suppressions codebase-wide.

---

## File Conventions

| Extension | Description |
|-----------|-------------|
| `.bt` | Beamtalk source files |
| `.core` | Generated Core Erlang |
| `.beam` | Compiled BEAM bytecode |
| `.erl` | Erlang source (helpers, tests) |
| `.hrl` | Erlang header files |

---

## Resources

### Beamtalk Design
- [Design Principles](../beamtalk-principles.md) - Core philosophy guiding all decisions
- [Language Features](../beamtalk-language-features.md) - Planned syntax and features
- [Syntax Rationale](../beamtalk-syntax-rationale.md) - Why we keep/change Smalltalk conventions
- [Architecture](../beamtalk-architecture.md) - Compiler, runtime, and live development flow
- [Agent-Native Development](../beamtalk-agent-native-development.md) - AI agents as developers and live actor systems

### Rust Guidelines
- [Microsoft Rust Guidelines](https://microsoft.github.io/rust-guidelines/) - Pragmatic patterns for safety and maintainability
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/checklist.html) - Naming, traits, documentation standards
- [Rust Style Guide](https://doc.rust-lang.org/nightly/style-guide/) - Formatting conventions
- [Rust Design Patterns](https://rust-unofficial.github.io/patterns/) - Common patterns and idioms

### BEAM/Erlang
- [Core Erlang specification](https://www.it.uu.se/research/group/hipe/cerl/)
- [BEAM VM internals](https://blog.stenmans.org/theBeamBook/)

### Reference Implementations
- [Gleam compiler](https://github.com/gleam-lang/gleam) - Rust-to-BEAM reference
- [Newspeak language](https://newspeaklanguage.org/) - Module system inspiration
- [TypeScript compiler](https://github.com/microsoft/TypeScript/wiki/Architectural-Overview) - Tooling-first architecture

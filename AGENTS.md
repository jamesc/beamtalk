<!-- Short index for agents. For full details, see docs/agents/expanded.md -->
# AGENTS.md - Beamtalk Agent Quick Reference

This file is a compact one-page index for AI coding agents. For full, detailed guidance, examples, and workflows see `docs/agents/expanded.md`.

Essential facts

- **Repo values (use for API calls):** Owner: `jamesc`, Repo: `beamtalk`.
- **Must-read rules (inline):**
  - **Syntax verification:** Always verify Beamtalk syntax in `docs/beamtalk-language-features.md`, `examples/`, or `tests/` before using it.
  - **Structured errors:** Never emit bare tuple errors; use `#beamtalk_error{}` helpers in runtime and generated code.
  - **CI checklist:** Use `just ci` for full checks; quick commands: `just build`, `just test`, `just test-stdlib`, `just test-e2e`.

- **Agent shortcuts:** You may run `just`, `cargo`, `rustc`, `rustfmt`, and `git` without asking.

- **Issue workflow:** Create Linear issues with context and acceptance criteria; set `agent-state` and `item-area` labels and establish blocking relationships. See the expanded doc for exact label lists and GraphQL examples.

Links

- Full agent guidelines & examples: `docs/agents/expanded.md`
- Language spec: `docs/beamtalk-language-features.md`
- Development principles: `docs/development/architecture-principles.md`

If you want this shortened further (e.g., move even the three inline rules into the expanded doc), say "Make it minimal" and I'll apply the change.

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

This repository includes custom skills in `.github/skills/` that teach Copilot specialized workflows for this project. Skills are automatically loaded when relevant to your prompt.

### Available Skills

| Skill | Trigger | Description |
|-------|---------|-------------|
| **Coding Workflow** | | |
| `pick-issue` | `/pick-issue` | Pick up the next Linear issue from backlog |
| `done` | `/done` | Complete work, commit, push, and create PR |
| `resolve-pr` | `/resolve-pr` | Systematically address PR review comments |
| `resolve-merge` | `/resolve-merge` | Update main, merge into current branch, resolve conflicts |
| **Issue Management** | | |
| `create-issue` | "create issue" | Create Linear issues with blocking relationships |
| `refresh-issue` | `/refresh-issue BT-XX` | Refresh a Linear issue to align with current code state |
| `update-issues` | `/update-issues` | Find and update Linear issues with missing labels or metadata |
| `whats-next` | `/whats-next` | Get recommendations for what to work on next |
| **Architecture** | | |
| `draft-adr` | `/draft-adr` | Research a problem and draft an Architecture Decision Record |
| `plan-adr` | `/plan-adr` | Break an accepted ADR into implementation issues with an Epic |
| **Refactoring** | | |
| `plan-refactor` | `/plan-refactor` | Analyze repo and plan refactoring for code quality/maintainability |
| `do-refactor` | `/do-refactor` | Execute a refactoring epic: all issues on one branch, CI after each |
| **Review** | | |
| `review-code` | `/review-code` | 3-pass code review: diff, system, adversarial (different model) |
| `review-adr` | `/review-adr` | 3-pass ADR review: structural, reasoning, adversarial |

### Skill Locations

- **Project skills** (this repo): `.github/skills/<skill-name>/SKILL.md`
- **Personal skills** (all projects): `~/.copilot/skills/<skill-name>/SKILL.md`

### Creating New Skills

1. Create a directory: `.github/skills/<skill-name>/`
2. Add a `SKILL.md` file with YAML frontmatter:
   ```markdown
   ---
   name: skill-name
   description: When to use this skill. Be specific so Copilot knows when to activate it.
   ---

   # Skill Instructions

   Step-by-step instructions for Copilot to follow...
   ```

3. Optionally add scripts or resources to the skill directory

For more details, see [About Agent Skills](https://docs.github.com/en/copilot/concepts/agents/about-agent-skills).

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

⚠️ **IMPORTANT:** The test suite has multiple layers. Be precise about which tests you're referring to:

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
**Location:** `tests/stdlib/*.bt` (~57 files)
- **Pure language feature tests** compiled directly to EUnit (no REPL needed)
- Uses same `// =>` assertion format as E2E tests
- Runs via `just test-stdlib` (fast, ~14s vs ~50s for E2E)
- Tests arithmetic, strings, blocks, closures, collections, object protocol, etc.
- Supports `@load` directives for fixture-dependent tests (actors, sealed classes)
- **Use this for any new test that doesn't need REPL/workspace features**

#### 5. BUnit Tests (TestCase Classes) — ADR 0014 Phase 2
**Location:** `test/*.bt` (project test directory)
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
| Pure language features (arithmetic, strings, blocks) | `tests/stdlib/` |
| Collections (List, Dictionary, Set, Tuple) | `tests/stdlib/` |
| Actor spawn + messaging (with `@load`) | `tests/stdlib/` |
| Stateful tests with setUp/tearDown | `test/*.bt` (BUnit) |
| Complex actor interactions, multiple assertions | `test/*.bt` (BUnit) |
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
| [Rust Guidelines](docs/development/rust-guidelines.md) | Naming, traits, error handling, testing, compiler patterns |
| [Erlang Guidelines](docs/development/erlang-guidelines.md) | Code generation, OTP patterns, BEAM interop |
| [Common Tasks](docs/development/common-tasks.md) | Adding AST nodes, CLI commands, stdlib features |
| [Debugging](docs/development/debugging.md) | Step-by-step debugging for all failure types |
| [Language Features](docs/beamtalk-language-features.md) | Full Beamtalk syntax specification |

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
- [Design Principles](docs/beamtalk-principles.md) - Core philosophy guiding all decisions
- [Language Features](docs/beamtalk-language-features.md) - Planned syntax and features
- [Syntax Rationale](docs/beamtalk-syntax-rationale.md) - Why we keep/change Smalltalk conventions
- [Architecture](docs/beamtalk-architecture.md) - Compiler, runtime, and live development flow
- [Agent-Native Development](docs/beamtalk-agent-native-development.md) - AI agents as developers and live actor systems

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

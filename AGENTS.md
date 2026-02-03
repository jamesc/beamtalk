# AGENTS.md - Beamtalk Development Guidelines

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

**Key principle:** Beamtalk is an **interactive-first** language. The live environment and hot code reloading are core to the design, not afterthoughts. See [docs/beamtalk-principles.md](docs/beamtalk-principles.md) for full design philosophy and [docs/beamtalk-language-features.md](docs/beamtalk-language-features.md) for planned syntax and features.

---

## Domain Driven Design (DDD)

**CRITICAL:** The beamtalk architecture is **driven by Domain Driven Design principles**. Always consider DDD when creating new code or refactoring existing code.

### Core DDD Principles

1. **Ubiquitous Language** - Use domain terms consistently throughout the codebase
   - Module names, types, and functions should reflect the domain model
   - Example: `CompletionProvider`, `DiagnosticProvider`, `HoverProvider` (not `completions`, `diagnostics`, `hover`)
   - When the domain expert says "provider", the code should say "provider"

2. **Bounded Contexts** - The codebase is organized into distinct contexts
   - **Language Service Context** - IDE features (completions, diagnostics, hover)
   - **Compilation Context** - Lexer, parser, AST, semantic analysis, codegen
   - **Runtime Context** - Erlang runtime, actors, OTP integration
   - **REPL Context** - Interactive development, live coding

3. **Domain Services** - Stateless operations that don't naturally fit in entities
   - Example: `CompletionProvider::compute_completions()` is a domain service
   - Domain services are pure functions operating on domain objects (AST, Module)

4. **Value Objects** - Immutable objects defined by their attributes
   - Example: `Span`, `Position`, `Identifier`, `Token`
   - Use newtypes for clarity: `ModuleId`, `FunctionId`, `ByteOffset`, `LineNumber`

5. **Entities** - Objects with identity that can change over time
   - Example: `Module`, `Expression`, `Statement` in the AST
   - Identity matters more than attributes

### When to Apply DDD

**Always apply DDD when:**

1. **Creating new modules** - Name them after domain concepts
   ```rust
   // ✅ GOOD - Uses domain language
   pub mod completion_provider;
   pub mod diagnostic_provider;
   
   // ❌ BAD - Generic technical terms
   pub mod completions;
   pub mod diagnostics;
   ```

2. **Refactoring** - Align code with the domain model
   - If the code structure doesn't match the domain model, refactor it
   - Example: BT-199 renamed `completions` → `completion_provider` to match LSP terminology

3. **Adding features** - Consider which bounded context it belongs to
   - Language Service feature? → `crates/beamtalk-core/src/queries/`
   - Compilation feature? → `crates/beamtalk-core/src/parse/` or `src/analyse/`
   - Runtime feature? → `runtime/src/`

4. **Writing documentation** - Include DDD context annotations
   ```rust
   //! Completion provider for the language service.
   //!
   //! **DDD Context:** Language Service
   //!
   //! This domain service implements the `CompletionProvider` from the DDD model.
   ```

### DDD Resources

- **Primary reference:** [docs/beamtalk-ddd-model.md](docs/beamtalk-ddd-model.md) - Defines all bounded contexts and domain services
- **LSP alignment:** Language Service context aligns with LSP specification terminology
- **Review examples:** See BT-199 for a refactoring that improved DDD alignment

### Code Review Checklist

When reviewing code for DDD compliance:

- [ ] Module names use domain language (not generic technical terms)
- [ ] Types have clear bounded context (documented in module header)
- [ ] Functions use ubiquitous language from domain model
- [ ] Domain services are stateless and operate on domain objects
- [ ] Value objects are immutable
- [ ] Documentation includes DDD context annotations

---

## Development Architecture Principles

The beamtalk codebase follows strict architectural principles for code organization, error handling, testing, security, and dependencies. Full details in [docs/development/architecture-principles.md](docs/development/architecture-principles.md).

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

✅ **ALWAYS:**
- Return `(Result, Vec<Diagnostic>)` or equivalent for user-facing operations
- Validate file paths and buffer boundaries
- Document unsafe code with `// SAFETY:` comment explaining invariants
- Run `cargo audit` before releases

See full guide: [docs/development/architecture-principles.md](docs/development/architecture-principles.md)

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
| `class-system` | Class definition, parsing, codegen, and runtime | `crates/beamtalk-core/src/ast.rs`, `crates/beamtalk-core/src/parse/` |
| `stdlib` | Standard library: collections, primitives, strings | `lib/` |
| `repl` | REPL backend and CLI interaction | `runtime/src/beamtalk_repl.erl`, `crates/beamtalk-cli/src/repl/` |
| `cli` | Command-line interface and build tooling | `crates/beamtalk-cli/` |
| `codegen` | Code generation to Core Erlang/BEAM | `crates/beamtalk-core/src/erlang.rs` |
| `runtime` | Erlang runtime: actors, futures, OTP integration | `runtime/src/` |
| `parser` | Lexer, parser, AST | `crates/beamtalk-core/src/parse/`, `crates/beamtalk-core/src/ast.rs` |

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

**Epics** are large initiatives that group 5+ related issues. Use Epics to organize complex features that span multiple components or require sequential implementation.

#### Current Active Epics

| Epic ID | Name | Child Issues | Progress | Priority |
|---------|------|--------------|----------|----------|
| **BT-204** | Block Semantics and Control Flow | 13 issues | ~80% | High |
| **BT-205** | Standard Library Core Classes | 8 issues | ~60% | High |
| **BT-206** | REPL and Interactive Development | 14 issues | ~55% | High |
| **BT-207** | Actor Runtime and Messaging | 8 issues | ~50% | High |
| **BT-208** | Parser and Language Syntax | 8 issues | ~70% | Medium |
| **BT-209** | Code Quality and Infrastructure | 24 issues | ~75% | Medium |
| **BT-197** | DDD Architecture Refactoring | 4 issues | ~50% | Medium |
| **BT-162** | Self-as-Object and Reflection API | 10 issues | ~70% | High |
| **BT-140** | Full Semantic Analysis Framework | 10 issues | ~60% | Medium |
| **BT-185** | Persistent Workspace Management | TBD | ~10% | Urgent |
| **BT-210** | Advanced Language Features | 5 issues | ~10% | Low |

#### Epic Completed

| Epic ID | Name | Child Issues | Completion Date |
|---------|------|--------------|-----------------|
| **BT-134** | Improve Erlang Test Coverage | 5 issues | 2026-01-31 |
| **BT-128** | Improve Test Coverage to >80% | 6 issues | 2026-02-01 |

#### Creating Epics

When creating an Epic:

1. **Title prefix** - Always use `Epic:` prefix in title (e.g., `Epic: Standard Library Core Classes`)
2. **Use Epic label** - Always add `Epic` label to Epic issues
3. **Size as XL or L** - Epics are large by definition
4. **Write comprehensive description** with:
   - Overview of initiative
   - Goals (3-5 high-level outcomes)
   - Status summary (progress percentage, completed/in-progress/planned)
   - List of child issues with status indicators
   - References to design docs
   - Next steps
4. **Link child issues** - Use Linear's "blocks" relationship to connect Epic to child issues
5. **Track progress** - Update Epic description with progress as child issues complete
6. **Include completion criteria** - Checklist of what defines "done" for the Epic

**Example Epic Titles:**
```
Epic: Block Semantics and Control Flow
Epic: Standard Library Core Classes
Epic: REPL and Interactive Development
```

**Example Epic Structure:**
```markdown
## Overview
Brief description of the initiative and why it matters.

## Goals
1. Goal 1 - Specific outcome
2. Goal 2 - Specific outcome
3. Goal 3 - Specific outcome

## Status
**Progress:** ~60% complete (6/10 issues done)

**Completed:**
- ✅ BT-X: Issue description
- ✅ BT-Y: Issue description

**In Progress:**
- 🔄 BT-Z: Issue description

**Planned:**
- ⏳ BT-A: Issue description

## Child Issues
- BT-X - Issue title ✅
- BT-Y - Issue title ✅
- BT-Z - Issue title (In Progress)
- BT-A - Issue title

## References
- Design doc: `docs/...`
- Related Epic: BT-XXX

## Next Steps
1. Complete BT-Z (in progress)
2. Start BT-A
3. Write documentation
```

#### Epic Guidelines

**When to create an Epic:**
- Feature requires 5+ related issues
- Work spans multiple components (parser + codegen + runtime)
- Implementation requires sequential phases
- High-level initiative needs progress tracking

**When NOT to create an Epic:**
- Single feature with 1-3 issues (just use issue dependencies)
- Small refactoring with <5 files
- Bug fixes (even if touching multiple areas)

**Epic Maintenance:**
- Update progress percentage monthly or when child issues complete
- Close Epic when all child issues are Done
- Add new child issues as scope clarifies
- Keep "Current Active Epics" table in AGENTS.md up to date

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
- crates/beamtalk-core/src/parse/token.rs
- crates/beamtalk-core/src/parse/lexer.rs

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

We use **Architecture Decision Records (ADRs)** to document significant design and architectural decisions in `docs/adr/`.

### When to Create an ADR

Create an ADR for decisions that:

1. **Affect language design** - Syntax, semantics, operators, control flow
2. **Change core architecture** - Module organization, compilation pipeline, runtime behavior
3. **Impact interoperability** - Erlang/Elixir compatibility, BEAM alignment
4. **Alter user-facing behavior** - Breaking changes, API changes, feature removal
5. **Establish patterns** - Coding conventions, design patterns, best practices

**Examples requiring ADRs:**
- Adding/removing language features (e.g., pattern matching, string interpolation)
- Changing operator semantics (e.g., BT-188: equality operators)
- Architectural refactoring (e.g., DDD reorganization)
- Deprecating features (e.g., ADR 0001: no compound assignment)

**Examples NOT requiring ADRs:**
- Bug fixes (unless they change behavior significantly)
- Documentation updates
- Test additions
- Dependency updates
- Minor refactoring

### ADR Structure

Each ADR must include:

```markdown
# ADR NNNN: Descriptive Title

## Status
Proposed | Accepted | Deprecated | Superseded

## Context
Background, problem statement, why this decision is needed

## Decision
The decision made (clear, concise statement)

## Consequences
### Positive
- Benefits and advantages

### Negative
- Costs, trade-offs, risks

### Neutral
- Other impacts

## References
- Related issues (BT-XXX)
- Documentation links
- Prior discussions
```

### Creating ADRs

1. **Number sequentially:** Use next available number (0001, 0002, etc.)
2. **Title format:** `NNNN-kebab-case-title.md`
3. **One decision per ADR:** Keep focused—split complex decisions into multiple ADRs
4. **Document dependencies:** If ADR B depends on ADR A, state it explicitly
5. **Update the index:** Add your ADR to `docs/adr/README.md`

**Example workflow:**
```bash
# Create ADR file
vim docs/adr/0003-add-pattern-matching.md

# Update index
vim docs/adr/README.md

# Commit with reference
git commit -m "docs: add ADR 0003 - pattern matching syntax BT-XXX"
```

### ADR Best Practices

- **Write early:** Create ADR when decision is made, not months later
- **Be specific:** Include code examples, not just prose
- **Show trade-offs:** Document what you're giving up, not just what you gain
- **Link to issues:** Reference Linear issues (BT-XXX) for context
- **Update status:** Mark as Accepted/Deprecated when status changes
- **Supersede, don't delete:** If decision changes, create new ADR and mark old one as Superseded

### Current ADRs

See `docs/adr/README.md` for the complete list. Recent examples:
- **ADR 0001:** No compound assignment in Beamtalk (Smalltalk purity)
- **ADR 0002:** Use Erlang comparison operators directly (BEAM-first)

**Reference:** [ADR best practices](https://github.com/joelparkerhenderson/architecture-decision-record)

---

## Agent Skills

This repository includes custom skills in `.github/skills/` that teach Copilot specialized workflows for this project. Skills are automatically loaded when relevant to your prompt.

### Available Skills

| Skill | Trigger | Description |
|-------|---------|-------------|
| `next-issue` | `/next-issue` | Pick up the next Linear issue from backlog |
| `done` | `/done` | Complete work, commit, push, and create PR |
| `whats-next` | `/whats-next` | Get recommendations for what to work on next |
| `pr-resolve` | `/pr-resolve` | Systematically address PR review comments |
| `merge-resolve` | `/merge-resolve` | Update main, merge into current branch, resolve conflicts |
| `add-ast-node` | "add AST node" | Add a new AST node to the compiler |
| `add-cli-command` | "add CLI command" | Add a new command to the CLI |
| `debug-compilation` | "debug compilation" | Troubleshoot compiler issues |
| `create-issue` | "create issue" | Create Linear issues with blocking relationships |
| `update-issues` | `/update-issues` | Find and update Linear issues with missing labels or metadata |
| `merge-resolve` | `/merge-resolve` | Update main, merge into current branch, and resolve conflicts |

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
beamtalk/
├── crates/
│   ├── beamtalk-core/     # Lexer, parser, AST, type checking, codegen
│   ├── beamtalk-cli/      # Command-line interface
│   └── beamtalk-lsp/      # Language server (future)
├── lib/                    # Beamtalk standard library (.bt files)
├── runtime/
│   ├── src/               # Erlang runtime modules
│   └── test/              # Erlang unit tests (EUnit)
├── test-package-compiler/  # Snapshot test harness
├── tests/
│   └── e2e/cases/         # Real E2E tests (.bt source files)
├── docs/                   # Documentation
└── examples/               # Example beamtalk programs
```

### Test Organization - CRITICAL DISTINCTION

⚠️ **IMPORTANT:** The test suite has multiple layers. Be precise about which tests you're referring to:

#### 1. Runtime Unit Tests
**Location:** `runtime/test/*_tests.erl` (e.g., `beamtalk_actor_tests.erl`)
- Tests individual runtime modules in isolation
- Uses hand-written test fixtures (e.g., `test_counter.erl`)
- Calls `gen_server` protocol directly with raw pids
- Appropriate for testing low-level runtime behavior

#### 2. Codegen Simulation Tests  
**Location:** `runtime/test/beamtalk_codegen_simulation_tests.erl`
- Tests using **real compiled Beamtalk code** from `tests/fixtures/counter.bt`
- The `spawn/0` and `spawn/1` tests use `counter:spawn()` from compiled module
- Other tests use simulated state structures for complex scenarios
- **Test fixtures compile automatically** via rebar3 pre-hook (no manual step needed)
- See `docs/development/testing-strategy.md` for compilation workflow details

#### 3. Real End-to-End Tests
**Location:** `tests/e2e/cases/*.bt`
- Actual Beamtalk source files (`.bt` extension)
- Compiled by the real compiler (lexer → parser → codegen → erlc)
- Executed on BEAM and validated against expected results
- **These are the TRUE end-to-end tests**

**When discussing E2E tests, ALWAYS refer to `tests/e2e/cases/*.bt`, never `runtime/test/`.**

---

## Development Guidelines

Detailed coding standards and task guides are in `docs/development/`:

| Guide | Description |
|-------|-------------|
| [Rust Guidelines](docs/development/rust-guidelines.md) | Naming, traits, error handling, testing, compiler patterns |
| [Erlang Guidelines](docs/development/erlang-guidelines.md) | Code generation, OTP patterns, BEAM interop |
| [Common Tasks](docs/development/common-tasks.md) | Adding AST nodes, CLI commands, stdlib features |
| [Language Features](docs/beamtalk-language-features.md) | Full Beamtalk syntax specification |
| [Syntax Rationale](docs/beamtalk-syntax-rationale.md) | Why we keep/change Smalltalk conventions |

**Key rules (see full docs for details):**

### Static Verification (CI Commands)

```bash
cargo fmt --all                           # Format all crates
cargo fmt --all -- --check                # Verify formatting
cargo clippy --all-targets -- -D warnings # Lints (warnings = errors)
cargo test --all-targets                  # Run all tests
```

### License Headers

All source files must include Apache 2.0 header:
```rust
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
```

### Beamtalk Style (Critical)

- **Implicit returns**: Use `^` ONLY for early returns, never on last expression
- **No periods**: Newlines separate statements, not `.`
- **Comments**: Use `//` and `/* */`, not Smalltalk's `"..."`

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
- [IDE and Live Development](docs/beamtalk-ide.md) - Smalltalk-style integrated environment
- [BEAM Interop](docs/beamtalk-interop.md) - Erlang/Elixir integration specification
- [Feasibility Assessment](docs/beamtalk-feasibility.md) - Technical and market analysis
- [Agent Systems](docs/beamtalk-for-agents.md) - Multi-agent AI use cases

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

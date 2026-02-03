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

## Rust Development Best Practices

This section follows the [Microsoft Rust Guidelines](https://microsoft.github.io/rust-guidelines/) and [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/). When in doubt, consult these upstream sources.

### Upstream Guidelines

Follow these established guidelines:
- [Rust API Guidelines Checklist](https://rust-lang.github.io/api-guidelines/checklist.html) — naming, traits, documentation
- [Microsoft Rust Guidelines](https://microsoft.github.io/rust-guidelines/) — pragmatic patterns for safety and maintainability
- [Rust Style Guide](https://doc.rust-lang.org/nightly/style-guide/) — formatting conventions

### License Headers

All source code files must include the Apache 2.0 license header. Add this at the top of every new file:

**Rust files (`.rs`):**
```rust
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
```

**Erlang files (`.erl`, `.hrl`):**
```erlang
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
```

**Beamtalk files (`.bt`):**
```
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
```

### Static Verification

Configure lints in the workspace `Cargo.toml`:

```toml
[workspace.lints.rust]
missing_debug_implementations = "warn"
trivial_numeric_casts = "warn"
unsafe_op_in_unsafe_fn = "warn"
unused_lifetimes = "warn"

[workspace.lints.clippy]
cargo = { level = "warn", priority = -1 }
pedantic = { level = "warn", priority = -1 }
# Restriction lints (opt-in for quality)
clone_on_ref_ptr = "warn"
undocumented_unsafe_blocks = "warn"
```

**CRITICAL: Always run these exact commands (matching CI) before committing:**
```bash
# Step 1: Auto-fix formatting issues
cargo fmt --all

# Step 2: Verify formatting matches CI
cargo fmt --all -- --check

# Step 3: Run clippy with warnings as errors (MUST match CI)
cargo clippy --all-targets -- -D warnings

# Step 4: Run all tests
cargo test --all-targets
```

**Why these exact commands?**
- `cargo fmt --all` - Formats all crates in workspace (CI uses `--all`)
- `cargo fmt --all -- --check` - Verifies formatting without modifying files (CI check)
- `-D warnings` on clippy - CI treats warnings as errors
- `--all-targets` - Includes tests, benches, examples (matches CI)

**Common mistake:** Running `cargo fmt` without `--all` only formats the current crate, missing workspace members. Always use `cargo fmt --all`.

### Pre-Commit Hooks

**Problem:** CI format checks frequently fail because developers forget to run `cargo fmt --all` before committing.

**Solution:** Run the install script to automatically format code before each commit:
```bash
./scripts/install-git-hooks.sh
```

This installs a pre-commit hook that runs `cargo fmt --all` before every commit, ensuring all commits pass CI format checks.

Use `#[expect(...)]` instead of `#[allow(...)]` for lint overrides — it warns when the override becomes unnecessary:
```rust
#[expect(clippy::unused_async, reason = "will add I/O later")]
pub async fn ping() {}
```

### Naming Conventions

Follow [RFC 430](https://rust-lang.github.io/api-guidelines/naming.html#c-case):

| Item | Convention | Example |
|------|------------|---------|
| Types, Traits | `UpperCamelCase` | `TokenKind`, `Parser` |
| Functions, Methods | `snake_case` | `parse_expression` |
| Local variables | `snake_case` | `token_kind` |
| Constants | `SCREAMING_SNAKE_CASE` | `MAX_ERRORS` |
| Modules | `snake_case` | `code_gen` |

**Conversion methods** follow `as_`, `to_`, `into_` conventions:
- `as_*` — cheap, borrowed view (`&self → &T`)
- `to_*` — expensive conversion (`&self → T`)
- `into_*` — consuming conversion (`self → T`)

```rust
impl Token {
    fn as_str(&self) -> &str { ... }      // Cheap borrow
    fn to_string(&self) -> String { ... } // Allocates
    fn into_span(self) -> Span { ... }    // Consumes self
}
```

**Avoid weasel words** like `Manager`, `Service`, `Factory`. Be specific:
```rust
// Bad
struct TokenManager;

// Good
struct TokenStream;
struct Lexer;
```

### Common Trait Implementations

All public types should implement these traits where applicable:

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
```

| Trait | When to implement |
|-------|-------------------|
| `Debug` | **Always** for public types |
| `Clone` | If type can be cheaply cloned |
| `PartialEq`, `Eq` | If equality comparison makes sense |
| `Hash` | If type will be used in `HashMap`/`HashSet` |
| `Default` | If a sensible default exists |
| `Display` | If type is meant to be read by users |

**Sensitive data** should have a custom `Debug` that redacts secrets:
```rust
impl Debug for ApiKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ApiKey(...)")
    }
}
```

### Documentation Standards

**First sentence** is the summary — keep it under 15 words, one line:
```rust
/// Parses a message send expression from the token stream.
///
/// Extended description here...
pub fn parse_message_send(&mut self) -> Result<Expression, ParseError> { ... }
```

**Canonical sections** when applicable:
```rust
/// Brief summary (< 15 words).
///
/// Extended description explaining behavior.
///
/// # Examples
/// ```
/// let parser = Parser::new(source);
/// let expr = parser.parse_expression()?;
/// ```
///
/// # Errors
/// Returns `ParseError::UnexpectedToken` if...
///
/// # Panics
/// Panics if the parser is in an invalid state (programming error).
pub fn parse_expression(&mut self) -> Result<Expression, ParseError> { ... }
```

**Module documentation** with `//!` at the top of each module:
```rust
//! Lexical analysis for Beamtalk source code.
//!
//! This module converts source text into a stream of tokens.
//! The lexer handles error recovery for unterminated strings
//! and unknown characters.

pub struct Lexer { ... }
```

### Error Handling

**Panics vs Results:** Detected programming bugs are panics, not errors. External failures return `Result`.

```rust
// Programming bug — panic
fn get_token(&self, index: usize) -> &Token {
    &self.tokens[index]  // Panics on out-of-bounds (caller's fault)
}

// External failure — Result
fn parse_file(path: &Path) -> Result<Module, ParseError> {
    let source = std::fs::read_to_string(path)?;
    // ...
}
```

**Error types** should be structs with context, not enums:
```rust
#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
    backtrace: std::backtrace::Backtrace,
}

impl ParseError {
    pub fn is_unexpected_token(&self) -> bool {
        matches!(self.kind, ParseErrorKind::UnexpectedToken { .. })
    }

    pub fn span(&self) -> Span { self.span }
}

// Keep ErrorKind private for future flexibility
#[derive(Debug)]
pub(crate) enum ParseErrorKind {
    UnexpectedToken { expected: TokenKind, found: TokenKind },
    UnterminatedString,
    // ...
}
```

Use `thiserror` for error definitions, `miette` for user-facing diagnostics with source spans.

### Type Safety

**Use newtypes** to prevent type confusion:
```rust
// Bad — easy to mix up
fn get_symbol(module: u32, function: u32) -> Symbol;

// Good — compiler catches mistakes
pub struct ModuleId(u32);
pub struct FunctionId(u32);

fn get_symbol(module: ModuleId, function: FunctionId) -> Symbol;
```

**Avoid primitive obsession** — create domain types:
```rust
// Bad
fn set_offset(offset: usize);
fn set_line(line: usize);

// Good
pub struct ByteOffset(usize);
pub struct LineNumber(usize);

fn set_offset(offset: ByteOffset);
fn set_line(line: LineNumber);
```

**Builders** for types with 3+ optional parameters:
```rust
impl LexerBuilder {
    pub fn source(mut self, source: &str) -> Self { ... }
    pub fn file_id(mut self, id: FileId) -> Self { ... }
    pub fn error_limit(mut self, limit: usize) -> Self { ... }
    pub fn build(self) -> Lexer { ... }
}

// Usage
let lexer = Lexer::builder()
    .source(text)
    .file_id(file_id)
    .build();
```

### Code Style

- Use `rustfmt` with default settings; run `cargo fmt` before committing
- Prefer `ecow::EcoString` for AST string data (copy-on-write efficiency)
- Use `camino::Utf8PathBuf` instead of `std::path::PathBuf`
- Accept `impl AsRef<str>` or `impl AsRef<Path>` for flexibility

### Testing

**Test Requirements:**
- **All new or changed code MUST have tests** - flag missing tests in code review
- Unit tests go in the same file as the code (`#[cfg(test)] mod tests`)
- Use `insta` for snapshot testing of parser output and codegen
- **After compiler changes, ALWAYS refresh insta snapshots:**
  ```bash
  cargo insta test          # Run tests and check for snapshot changes
  cargo insta review        # Interactively review and accept/reject
  ```
- Integration tests in `test-package-compiler/cases/` directories
- Name test functions descriptively: `fn parse_message_send_with_multiple_keywords()`
- Use `?` in examples, not `.unwrap()` or `try!`

**Test Priorities:**
1. **Edge cases first:** Nulls, empty strings, empty collections, zero values
2. **Error conditions:** Invalid input, out-of-bounds, parse errors, IO failures
3. **Boundary conditions:** Min/max values, collection limits, buffer boundaries
4. **Happy path:** Normal operation (test last, not first)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_tokenizes_keyword_message() {
        let tokens = Lexer::new("array at: 1").collect::<Vec<_>>();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[1].kind, TokenKind::Keyword("at:".into()));
    }

    #[test]
    fn lexer_handles_empty_input() {
        let tokens = Lexer::new("").collect::<Vec<_>>();
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn lexer_reports_unterminated_string() {
        let result = Lexer::new("'unterminated").collect::<Result<Vec<_>, _>>();
        assert!(result.is_err());
    }
}
```

### Example-Driven Development Workflow

**When implementing new language features, ALWAYS follow this pattern:**

1. **Create a runnable example** in `examples/` showing the feature in use
2. **Create test fixtures** in `tests/e2e/fixtures/` if testing classes/actors
3. **Write E2E tests** in `tests/e2e/cases/` that load and use the fixtures
4. **Run E2E tests** to verify end-to-end functionality: `cargo test e2e_language_tests`
5. **Iterate** until all tests pass

**Why this matters:**
- Examples provide immediate validation that your feature actually works
- E2E tests catch integration issues that unit tests miss
- Test fixtures can be reused across multiple test files
- Users get working examples to learn from

#### Example: Adding ProtoObject (BT-176)

**Step 1: Create the API** (`lib/ProtoObject.bt`)
```beamtalk
ProtoObject
  class => // implemented by compiler
  doesNotUnderstand: selector args: arguments => // implemented by compiler
  == other => // implemented by compiler
  ~= other => (self == other) not
  perform: selector withArguments: arguments => // implemented by compiler
```

**Step 2: Create a fixture** (`tests/e2e/fixtures/simple_proxy.bt`)
```beamtalk
Actor subclass: SimpleProxy
  state: target = nil
  
  setTarget: newTarget => self.target := newTarget
  
  doesNotUnderstand: selector args: arguments =>
    self.target perform: selector withArguments: arguments
```

**Step 3: Write E2E tests** (`tests/e2e/cases/protoobject.bt`)
```beamtalk
// ALWAYS verify responses with // => assertions!
42 class
// => Integer

'hello' class
// => String

42 == 42
// => true
```

**Important:** Every testable expression MUST have a `// =>` assertion.
Expressions without `// =>` are skipped by the test runner.

**Step 4: Create runnable example** (`examples/protoobject_proxy.bt`)
```beamtalk
// Complete working example with:
// - How to run it in REPL
// - Expected output
// - Step-by-step explanation of what happens
// - BEAM mapping details
```

**Step 5: Test using @load directive** (`tests/e2e/cases/protoobject_actors.bt`)
```beamtalk
// @load tests/e2e/fixtures/counter.bt
// @load tests/e2e/fixtures/simple_proxy.bt

Counter spawn
SimpleProxy spawn
```

**Step 6: Run E2E tests and iterate**
```bash
cargo test e2e_language_tests
# Fix issues until: "220/220 tests passed"
```

#### Using @load for Stateful Tests

The E2E test infrastructure supports loading class definitions with the `@load` directive:

```beamtalk
// tests/e2e/cases/my_test.bt
// @load tests/e2e/fixtures/my_class.bt

instance := MyClass spawn
instance someMethod
// => expected_result
```

**How it works:**
1. Test parser extracts `// @load <path>` directives
2. REPL compiles each file via the compiler daemon
3. Classes are loaded into the REPL session
4. Test expressions can spawn and use those classes

**Best practices:**
- Put reusable classes in `tests/e2e/fixtures/`
- Keep examples in `examples/` for user learning
- Use `@load` for any test requiring actor/class definitions
- Run E2E tests after every compiler change
- **ALWAYS add `// =>` assertions** - Expressions without expected output are skipped!

#### E2E Response Verification Rules

**Rule 1: Every expression MUST have a `// =>` assertion**

```beamtalk
// ✅ GOOD - Verifies response
42 + 3
// => 45

// ❌ BAD - Expression is skipped, not tested!
42 + 3
```

**Rule 2: Test deterministic values, document non-deterministic ones**

```beamtalk
// ✅ GOOD - Primitives have deterministic output
42 class
// => Integer

'hello' == 'hello'
// => true

// ⚠️ NON-DETERMINISTIC - Can't match exact PID
// Instead, test what CAN be verified:
Counter spawn           // Just verify no error
c class                 // Can't test - 'c' wasn't assigned with => assertion
```

**Rule 3: For non-deterministic objects, test their METHODS**

```beamtalk
// Can't easily test spawn result, but CAN test methods:
x := 42
// => 42

x + 1  
// => 43

// For actors: Test the behavior, not the object identity
// Use runtime tests for full stateful actor testing
```

**Rule 4: Use runtime tests for complex actor scenarios**

E2E tests are best for:
- ✅ Primitive operations (arithmetic, strings, booleans)  
- ✅ Class loading and compilation
- ✅ Message syntax and parsing
- ⚠️ Limited for: Stateful actor interactions (use `runtime/test/*.erl` instead)

Example: `runtime/test/beamtalk_actor_tests.erl` has comprehensive tests for:
- doesNotUnderstand with proxies
- Concurrent message sends
- State mutations
- Future awaiting

**Summary:** Add `// =>` assertions for every deterministic result. For actor state tests, use runtime EUnit tests where you have full control.

### Code Coverage Standards

**Coverage Thresholds by Code Type:**

| Code Type | Overall Coverage | Branch Coverage | Notes |
|-----------|-----------------|-----------------|-------|
| **Critical logic** (auth, APIs, business rules) | 80-90% | 80-90% | High scrutiny required |
| **High-risk files** (security, data integrity) | 90%+ | 90%+ | Extra tests for edge cases |
| **Standard code** | 70-80% | 80%+ | Normal development |
| **Trivial code** (getters, setters, simple DTOs) | Ignore | Ignore | Use `#[cfg(not(tarpaulin_include))]` |

**Hard Thresholds (CI Failure):**
- Overall coverage drops below **70%**
- Branch coverage drops below **80%**
- Any PR that decreases overall coverage without justification

**CI Integration:**
- Always append coverage metrics to `$GITHUB_STEP_SUMMARY` for visibility in Actions UI
- Add coverage badge and summary to PR comments using `irongut/CodeCoverageSummary` action
- **Do not use external services** (Codecov, Coveralls) - keep metrics in GitHub Actions
- For README badges, use Shields.io endpoints with Gist or gh-pages JSON artifacts
- Generate Cobertura XML format for summary actions

**Example CI step:**
```yaml
- name: Add Coverage to Job Summary
  run: |
    echo "## 📊 Code Coverage Report" >> $GITHUB_STEP_SUMMARY
    cat code-coverage-results.txt >> $GITHUB_STEP_SUMMARY
    echo "**Badge:** ${{ steps.coverage.outputs.badge }}" >> $GITHUB_STEP_SUMMARY

- name: Fail if coverage below threshold
  run: |
    COVERAGE=$(grep -oE 'Line Rate: [0-9.]+' code-coverage-results.txt | grep -oE '[0-9.]+' || echo "0")
    if awk -v cov="$COVERAGE" 'BEGIN { exit (cov < 70.0) ? 0 : 1 }'; then
      echo "Coverage $COVERAGE% is below 70% threshold"
      exit 1
    fi
```

**README Badge Example:**
```markdown
![Coverage](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/user/id/raw/coverage.json)
```

**Reviewing PRs:**
- Check Actions summary tab for coverage metrics
- Flag any coverage drops in code review
- **Require tests for all new/changed code** - no exceptions
- Require coverage metrics visible in PR before merge
- Verify edge cases and error conditions are tested

### AI-Friendly Design

Following [M-DESIGN-FOR-AI](https://microsoft.github.io/rust-guidelines/guidelines/ai/):

1. **Idiomatic patterns** — Follow standard Rust conventions so AI training data applies
2. **Thorough docs** — Document all public items; AI agents rely on documentation
3. **Examples** — Provide runnable examples in doc comments
4. **Strong types** — Use newtypes and domain types; compiler errors guide AI
5. **Testable APIs** — Design for unit testing; AI agents iterate via test feedback
6. **Test coverage** — Good coverage enables AI refactoring with confidence

### Compiler-Specific Patterns

**Tooling-First Architecture (TypeScript Approach)**

The compiler is the language service. Design for IDE responsiveness first, batch compilation second.

```rust
// AST nodes MUST carry source location - no exceptions
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

// Preserve trivia (comments, whitespace) for formatting
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub leading_trivia: Vec<Trivia>,
    pub trailing_trivia: Vec<Trivia>,
}

// Error recovery - parser continues after errors
pub enum Expression {
    // ... valid expressions ...
    Error(ErrorNode),  // Placeholder for unparseable code
}

// Use newtypes for IDs to prevent mixing
pub struct ModuleId(u32);
pub struct FunctionId(u32);
```

**Query-Based Architecture**

Consider using `salsa` or similar for incremental computation:

```rust
// Queries are cached and invalidated incrementally
#[salsa::query_group(CompilerDatabase)]
pub trait Compiler {
    #[salsa::input]
    fn source_text(&self, file: FileId) -> Arc<String>;

    fn parse(&self, file: FileId) -> Arc<Module>;
    fn diagnostics(&self, file: FileId) -> Vec<Diagnostic>;
    fn completions(&self, file: FileId, position: Position) -> Vec<Completion>;
    fn hover(&self, file: FileId, position: Position) -> Option<HoverInfo>;
}
```

**Error Recovery Requirements**

- Parser MUST produce an AST even with syntax errors
- Mark error regions, continue parsing
- Lexer should handle unterminated strings, unknown characters gracefully
- Never panic on malformed input

**Performance Targets**

- Keystroke response: <50ms
- File save diagnostics: <100ms
- Project-wide find references: <500ms
- Full build: optimize for incremental, cold build secondary

### Dependencies

- Minimize dependencies; prefer std library when reasonable
- Pin major versions in `Cargo.toml`
- Audit new dependencies for security and maintenance status

---

## Erlang/OTP/BEAM Best Practices

### Generated Code Style

When generating Core Erlang or Erlang source:

- Use snake_case for function and variable names
- Prefix generated variables with underscore if unused: `_Var`
- Generate `-module(name).` and `-export([...]).` attributes
- Include `-compile([no_auto_import]).` to avoid import conflicts

### Core Erlang Generation

```erlang
%% Generated Core Erlang should be readable
module 'my_module' ['main'/0]
  attributes []

'main'/0 = fun () ->
    call 'erlang':'+'(1, 2)
```

- Use fully qualified calls: `'erlang':'+'` not `+`
- Generate unique variable names to avoid shadowing
- Preserve source locations in annotations where possible

### OTP Patterns

When generating code that interacts with OTP:

- Map beamtalk objects to `gen_server` when stateful
- Use supervision trees for fault tolerance
- Prefer `gen_statem` over `gen_fsm` (deprecated)
- Generate proper `-behaviour(gen_server).` declarations

### BEAM Interop

- Erlang atoms are lowercase; generate as `'atom_name'`
- Strings are lists in Erlang; use binaries (`<<"hello">>`) for efficiency
- Beamtalk blocks map to Erlang funs: `fun(X) -> X + 1 end`
- Handle Erlang exceptions: `try ... catch Class:Reason -> ... end`

### Testing Generated Code

- Use EUnit for generated Erlang tests
- Test with `erl -noshell -eval 'module:test(), halt().'`
- Verify OTP behaviors start/stop correctly
- Check for message queue buildup in process tests

---

## Smalltalk/Beamtalk Syntax Reference

For agents working on the parser or generating beamtalk code. See [docs/beamtalk-syntax-rationale.md](docs/beamtalk-syntax-rationale.md) for design decisions.

```
// Unary message
object message

// Binary message (standard math precedence)
3 + 4
2 + 3 * 4  // => 14

// Keyword message
array at: 1 put: 'hello'

// Cascade - multiple messages to same receiver
Transcript show: 'Hello'; cr; show: 'World'

// Blocks (closures)
[:x :y | x + y]

// Assignment
count := 0

// Return (implicit vs explicit)
// Use implicit return for last expression (no ^)
getValue => self.value

// Use ^ ONLY for early returns
max: other =>
  self > other ifTrue: [^self]   // early return needs ^
  other                           // last expression - no ^

// String interpolation (double quotes)
greeting := "Hello, {name}!"
```

### Message Precedence (high to low)
1. Unary messages: `3 factorial`
2. Binary messages: `3 + 4` (with standard math precedence)
3. Keyword messages: `array at: 1`

### Beamtalk Style Guidelines

**Critical style rules for writing Beamtalk code:**

#### 1. Implicit Returns (REQUIRED)

**Rule:** Use `^` ONLY for early returns. Never use `^` on the last line of a method or block.

```beamtalk
// ✅ CORRECT - Implicit return on last expression
isNil => false
getValue => self.value
add: x => self.count + x

// ❌ WRONG - Unnecessary ^ on final expression
isNil => ^false
getValue => ^self.value

// ✅ CORRECT - ^ used ONLY for early return
max: other =>
  self > other ifTrue: [^self]   // early return - needs ^
  other                           // last expression - no ^

// ✅ CORRECT - Blocks also use implicit returns
[:x | x * 2]          // returns x * 2
[self doSomething]    // returns result of doSomething
```

**Why:** Reduces noise. The `^` is distinctive and should only appear when returning early, making control flow immediately visible.

**Source:** See [docs/beamtalk-syntax-rationale.md](docs/beamtalk-syntax-rationale.md#return-value--value-keep-but-implicit-at-end)

#### 2. No Statement Terminators (REQUIRED)

**Rule:** Do NOT use `.` (period) to terminate statements. Newlines separate statements.

```beamtalk
// ✅ CORRECT - No periods
count := 0
count := count + 1
self doSomething

// ❌ WRONG - Smalltalk-style periods
count := 0.
count := count + 1.
self doSomething.

// ✅ CORRECT - Semicolons optional for multiple statements on one line
count := 0; count := count + 1
```

**Why:** Period-as-terminator feels archaic. Newlines naturally end statements in modern languages. Less visual noise.

**Source:** See [docs/beamtalk-syntax-rationale.md](docs/beamtalk-syntax-rationale.md#statement-terminator-required---optional-newlines-work)

#### 3. Comments

Use `//` for single-line and `/* */` for multi-line comments (not Smalltalk's `"..."`):

```beamtalk
// Single-line comment
count := 0  // inline comment

/*
  Multi-line comment
  for longer explanations
*/
```

#### 4. Standard Math Precedence

Beamtalk uses standard operator precedence (PEMDAS), not Smalltalk's strict left-to-right:

```beamtalk
2 + 3 * 4   // => 14 (not 20)
```

#### 5. License Headers

All `.bt` files must include Apache 2.0 license header:

```beamtalk
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
```

---

## Common Tasks

### Adding a New AST Node

1. Define type in `crates/beamtalk-core/src/ast.rs`
2. Add parsing in `crates/beamtalk-core/src/parse/`
3. Add type checking in `analyse.rs` if needed
4. Add Core Erlang generation in `erlang.rs`
5. Add snapshot tests in `test-package-compiler/cases/`

### Adding a CLI Command

1. Add command enum variant in `crates/beamtalk-cli/src/main.rs`
2. Implement handler function
3. Add integration test
4. Update `--help` documentation

### Adding a New Standard Library Feature

**Follow this example-driven workflow:**

1. **Define the API** in `lib/YourClass.bt`
   - Include comprehensive documentation
   - Mark compiler intrinsics with `// implemented by compiler`
   - Provide usage examples in comments

2. **Implement compiler support**
   - Add builtin handler in `crates/beamtalk-core/src/codegen/core_erlang/builtins.rs`
   - Integrate into dispatch chain in `message_dispatch.rs`
   - Follow the pattern of `try_generate_*_message` functions

3. **Create test fixtures** in `tests/e2e/fixtures/`
   - Create simple, reusable classes demonstrating the feature
   - Keep fixtures minimal and focused

4. **Write E2E tests** in `tests/e2e/cases/`
   - Test primitives first
   - Use `@load` directive to load fixtures for actor tests
   - Cover edge cases and error conditions

5. **Create runnable example** in `examples/`
   - Show how users would actually use the feature
   - Include step-by-step REPL session walkthrough
   - Explain what's happening under the hood
   - Document BEAM mapping and generated code

6. **Run E2E tests after EVERY change**
   ```bash
   cargo test e2e_language_tests
   # Must show: "X/X tests passed" (all pass)
   ```

7. **Update `lib/README.md`**
   - Add to class hierarchy or core classes section
   - Update BEAM mapping table
   - Update implementation status table

**Example from BT-176 (ProtoObject):**
- ✅ API: `lib/ProtoObject.bt` (154 lines)
- ✅ Compiler: `builtins.rs::try_generate_protoobject_message`
- ✅ Fixtures: `tests/e2e/fixtures/simple_proxy.bt`
- ✅ E2E Tests: `tests/e2e/cases/protoobject.bt`, `protoobject_actors.bt`
- ✅ Example: `examples/protoobject_proxy.bt` (194 lines with walkthrough)
- ✅ Docs: Updated `lib/README.md` with class hierarchy
- ✅ Result: All 224 E2E tests pass

### Debugging Compilation

1. Use `BEAMTALK_DEBUG=1` to print intermediate representations
2. Check generated `.core` files in `build/` directory
3. Use `erlc +debug_info` for BEAM debugging
4. Inspect BEAM files with `:beam_lib.chunks/2`

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

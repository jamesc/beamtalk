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

We use a label group called **Agent State** to track work status:

- `agent-ready` - Task is fully specified, agent can start immediately
- `needs-spec` - Requires human to clarify requirements before work begins
- `blocked` - Waiting on external dependency or another issue
- `human-review` - Agent completed work, needs human verification
- `done` - Issue is complete and closed

**When creating issues:** Always set the appropriate agent-state label based on whether the issue has all requirements specified (`agent-ready`) or needs clarification (`needs-spec`).

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
| `worktree-start` | `/worktree-start <branch>` | Guide for creating a new worktree |
| `worktree-stop` | `/worktree-stop <branch>` | Guide for removing a worktree |
| `add-ast-node` | "add AST node" | Add a new AST node to the compiler |
| `add-cli-command` | "add CLI command" | Add a new command to the CLI |
| `debug-compilation` | "debug compilation" | Troubleshoot compiler issues |
| `create-issue` | "create issue" | Create Linear issues with blocking relationships |

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

## Parallel Development with Worktrees

We use git worktrees to enable multiple parallel development sessions. Each worktree is an independent working directory with its own branch, allowing multiple Copilot agents to work simultaneously on different issues.

### Scripts

Helper scripts are in `scripts/`:

| Script | Purpose |
|--------|--------|
| `worktree-new.ps1` / `worktree-new.sh` | Create a worktree and start a devcontainer |
| `worktree-rm.ps1` / `worktree-rm.sh` | Clean up a worktree (handles container path fixups) |

### Creating a Worktree

**Windows:**
```powershell
.\scripts\worktree-new.ps1 BT-99-feature-name
```

**Linux/Mac:**
```bash
./scripts/worktree-new.sh BT-99-feature-name
```

This will:
1. Create a worktree at `../BT-99-feature-name/` (sibling to main repo)
2. Check out the branch (creating it from main if needed)
3. Start a devcontainer for that worktree
4. Connect you to a shell inside the container

### Removing a Worktree

**Windows:**
```powershell
.\scripts\worktree-rm.ps1 BT-99-feature-name
```

**Linux/Mac:**
```bash
./scripts/worktree-rm.sh BT-99-feature-name
```

This handles a special case: when a worktree has been used with devcontainers, the `.git` file gets modified to point to container paths (`/workspaces/...`). The script fixes this before removal.

### Why Worktrees?

- **Parallel work**: Multiple agents can work on different issues simultaneously
- **Isolation**: Each worktree has its own build artifacts, IDE state, etc.
- **No stashing**: Switch context without stashing uncommitted changes
- **Devcontainer support**: Each worktree can run in its own container

### Environment Setup

For devcontainer support, set this environment variable permanently:

**Windows:**
```powershell
setx BEAMTALK_MAIN_GIT_PATH "C:\Users\you\source\beamtalk\.git"
```

**Linux/Mac:**
```bash
echo 'export BEAMTALK_MAIN_GIT_PATH="$HOME/source/beamtalk/.git"' >> ~/.bashrc
```

---

## Repository Structure

```
beamtalk/
├── crates/
│   ├── beamtalk-core/     # Lexer, parser, AST, type checking, codegen
│   ├── beamtalk-cli/      # Command-line interface
│   └── beamtalk-lsp/      # Language server (future)
├── lib/                    # Beamtalk standard library (.bt files)
├── test-package-compiler/  # Snapshot test harness
├── docs/                   # Documentation
└── examples/               # Example beamtalk programs
```

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

Run before committing:
```bash
cargo fmt          # Format code
cargo clippy       # Lint checks
cargo test         # Run tests
```

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

- Unit tests go in the same file as the code (`#[cfg(test)] mod tests`)
- Use `insta` for snapshot testing of parser output and codegen
- Integration tests in `test-package-compiler/cases/` directories
- Name test functions descriptively: `fn parse_message_send_with_multiple_keywords()`
- Use `?` in examples, not `.unwrap()` or `try!`

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
}
```

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

// Return
^result

// String interpolation (double quotes)
greeting := "Hello, {name}!"
```

### Message Precedence (high to low)
1. Unary messages: `3 factorial`
2. Binary messages: `3 + 4` (with standard math precedence)
3. Keyword messages: `array at: 1`

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

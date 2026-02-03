# Rust Development Best Practices

This document follows the [Microsoft Rust Guidelines](https://microsoft.github.io/rust-guidelines/) and [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/). When in doubt, consult these upstream sources.

## Upstream Guidelines

Follow these established guidelines:
- [Rust API Guidelines Checklist](https://rust-lang.github.io/api-guidelines/checklist.html) — naming, traits, documentation
- [Microsoft Rust Guidelines](https://microsoft.github.io/rust-guidelines/) — pragmatic patterns for safety and maintainability
- [Rust Style Guide](https://doc.rust-lang.org/nightly/style-guide/) — formatting conventions

## License Headers

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

## Static Verification

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

Use `#[expect(...)]` instead of `#[allow(...)]` for lint overrides — it warns when the override becomes unnecessary:
```rust
#[expect(clippy::unused_async, reason = "will add I/O later")]
pub async fn ping() {}
```

## Naming Conventions

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

## Common Trait Implementations

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

## Documentation Standards

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

## Error Handling

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

## Type Safety

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

## Code Style

- Use `rustfmt` with default settings; run `cargo fmt` before committing
- Prefer `ecow::EcoString` for AST string data (copy-on-write efficiency)
- Use `camino::Utf8PathBuf` instead of `std::path::PathBuf`
- Accept `impl AsRef<str>` or `impl AsRef<Path>` for flexibility

## Testing

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

## Code Coverage Standards

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

## AI-Friendly Design

Following [M-DESIGN-FOR-AI](https://microsoft.github.io/rust-guidelines/guidelines/ai/):

1. **Idiomatic patterns** — Follow standard Rust conventions so AI training data applies
2. **Thorough docs** — Document all public items; AI agents rely on documentation
3. **Examples** — Provide runnable examples in doc comments
4. **Strong types** — Use newtypes and domain types; compiler errors guide AI
5. **Testable APIs** — Design for unit testing; AI agents iterate via test feedback
6. **Test coverage** — Good coverage enables AI refactoring with confidence

## Compiler-Specific Patterns

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

## Dependencies

- Minimize dependencies; prefer std library when reasonable
- Pin major versions in `Cargo.toml`
- Audit new dependencies for security and maintenance status

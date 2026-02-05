# Architecture Principles

This document defines the core development architecture principles for the beamtalk compiler and runtime. These principles guide code organization, error handling, testing strategy, security practices, and dependency management.

**Audience:** AI agents, contributors, and maintainers working on the beamtalk codebase.

---

## Table of Contents

1. [Layered Architecture & Dependency Rules](#1-layered-architecture--dependency-rules)
2. [Error Recovery Philosophy](#2-error-recovery-philosophy)
3. [Testing Pyramid](#3-testing-pyramid)
4. [Security-First Development](#4-security-first-development)
5. [Dependency Management Philosophy](#5-dependency-management-philosophy)

---

## 1. Layered Architecture & Dependency Rules

### Principle

The beamtalk codebase is organized into **layers with unidirectional dependencies**. Dependencies flow **downward only**—higher layers depend on lower layers, never the reverse.

### Layer Structure

```
┌─────────────────────────────────────┐
│ beamtalk-cli (binary)               │  ← User-facing CLI tool
│ beamtalk-lsp (binary, future)       │  ← IDE language server
├─────────────────────────────────────┤
│ beamtalk-core (library)             │  ← Compiler core (reusable)
│  ├─ queries/     (Language Service) │
│  ├─ parse/       (Lexer, Parser)    │
│  ├─ analyse/     (Semantic Analysis)│
│  └─ codegen/     (Core Erlang gen)  │
└─────────────────────────────────────┘
```

### Rules

**✅ ALLOWED:**
- `beamtalk-cli` depends on `beamtalk-core`
- `beamtalk-lsp` depends on `beamtalk-core`
- `queries` depends on `parse`, `analyse`, `codegen`
- `codegen` depends on `parse` (needs AST types)

**❌ FORBIDDEN:**
- `beamtalk-core` importing `beamtalk-cli`
- `parse` importing `codegen`
- `codegen` importing `queries`

### Rationale

1. **Reusability** - Core compiler logic is library, can be embedded elsewhere
2. **Testability** - Library crates have no CLI/UI dependencies, easier to test
3. **Clear boundaries** - Each layer has a well-defined responsibility
4. **LSP support** - Language server and CLI both consume the same core

### Examples

```rust
// ✅ GOOD - CLI depends on core
// crates/beamtalk-cli/src/main.rs
use beamtalk_core::parse::{lex, parse};
use beamtalk_core::codegen::generate_core_erlang;

// ❌ BAD - Core depends on CLI (NEVER DO THIS)
// crates/beamtalk-core/src/source_analysis/lexer.rs
use beamtalk_cli::repl::ReplContext; // ❌ WRONG!
```

### Enforcement

**Decision:** Document only (no automated enforcement)

**Rationale:** Solo developer, code review sufficient. Can add `cargo-deny` later if team grows.

**Action on violation:** Flag in code review, refactor immediately.

---

## 2. Error Recovery Philosophy

### Principle

**Compiler errors are not exceptional—they're expected user input.** The compiler should collect all errors, report them clearly, and provide partial results when possible.

### Core Concepts

1. **Never panic on user input** - Malformed source code, invalid arguments, missing files should return diagnostics, not crash
2. **Collect all errors** - Don't stop at the first error, find as many as possible in one pass
3. **Partial results with diagnostics** - Return a usable AST even if there are syntax errors
4. **Error messages are user-facing documentation** - Clear, actionable, with fix suggestions

### Error Handling Strategy (Hybrid Approach)

**For user input (source code, CLI args):**
```rust
// ✅ GOOD - Return partial results + diagnostics
pub fn parse(tokens: Vec<Token>) -> (Module, Vec<Diagnostic>)

pub fn compute_diagnostics(module: &Module, parse_diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic>
```

**For programmer errors (internal bugs, unreachable states):**
```rust
// ✅ GOOD - Use Result for recoverable errors
pub fn read_source_file(path: &Path) -> Result<String, std::io::Error>

// ✅ GOOD - Panic for unreachable states (bugs)
match token.kind {
    TokenKind::Identifier => { /* ... */ }
    TokenKind::Number => { /* ... */ }
    _ => unreachable!("parser bug: unexpected token in literal position"),
}
```

### Examples

```rust
// ✅ GOOD - Parser continues after errors
pub fn parse_expression(&mut self) -> Expression {
    match self.current_token() {
        TokenKind::Identifier => self.parse_identifier(),
        TokenKind::LeftParen => self.parse_parenthesized(),
        _ => {
            // Error recovery: return error node, continue parsing
            self.diagnostics.push(Diagnostic::error(
                "expected expression",
                self.current_span(),
            ));
            self.advance(); // Skip bad token
            Expression::Error(ErrorNode { span: self.current_span() })
        }
    }
}

// ❌ BAD - Stop at first error
pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
    match self.current_token() {
        TokenKind::Identifier => Ok(self.parse_identifier()),
        _ => Err(ParseError::ExpectedExpression), // ❌ Stops parsing
    }
}
```

### Error Message Quality

Error messages should:
- **Highlight the exact span** (byte-accurate)
- **Explain why it's invalid** (not just "syntax error")
- **Suggest a fix** (when possible)

Example:
```
error: cannot assign to field 'sum' inside a stored closure
  --> test.bt:12:21
   |
12 | myBlock := [:item | self.sum := self.sum + item]
   |                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = help: field assignments require immediate execution context
   = help: use control flow directly: items do: [:item | self.sum := self.sum + item]
```

### Rationale

1. **Better developer experience** - See all errors at once, not one at a time
2. **IDE support** - Partial AST enables completions even with syntax errors
3. **Robustness** - Compiler never crashes on bad input
4. **Incremental compilation** - Can continue working on other parts of the file

---

## 3. Testing Pyramid

### Principle

The beamtalk test suite follows the **testing pyramid** pattern: many fast unit tests at the base, fewer slower integration tests in the middle, and a small number of end-to-end tests at the top.

### Test Layers

```
        ╱ ╲  E2E Tests (tests/e2e/cases/*.bt)
       ╱   ╲  - Real Beamtalk → BEAM execution
      ╱ 10% ╲  - Slow, high confidence
     ╱───────╲  - User-facing scenarios
    ╱         ╲
   ╱   20-30%  ╲  Integration Tests (runtime/test/*.erl)
  ╱             ╲  - Multiple units working together
 ╱───────────────╲  - Codegen + runtime simulation
╱                 ╲
╰───────────────────╯ Unit Tests (#[cfg(test)] mod tests)
    60-70%          - Fast, isolated, high coverage
                    - Every public function tested
```

### Unit Tests

**Location:** `#[cfg(test)]` modules in the same file as the code

**Characteristics:**
- Fast (<1ms per test)
- Isolated (no file I/O, no network, no external processes)
- Focused on single function/module
- Cover edge cases and error conditions

**Coverage target:** 70-80% overall, 80-90% for critical paths

**Example:**
```rust
// crates/beamtalk-core/src/source_analysis/lexer.rs
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_identifier() {
        let tokens = lex("myVariable");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Identifier("myVariable".into()));
    }

    #[test]
    fn lex_empty_input() {
        let tokens = lex("");
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn lex_invalid_character() {
        let (tokens, diagnostics) = lex("@invalid");
        assert!(!diagnostics.is_empty());
    }
}
```

### Integration Tests

**Location:** `runtime/test/*_tests.erl` (EUnit tests in Erlang)

**Characteristics:**
- Medium speed (10-100ms per test)
- Test multiple units working together
- Codegen + runtime interaction
- Use compiled test fixtures

**Examples:**
- `beamtalk_actor_tests.erl` - Actor spawn, message passing, state
- `beamtalk_codegen_simulation_tests.erl` - Compiled `.bt` modules running in runtime

### End-to-End Tests

**Location:** `tests/e2e/cases/*.bt` (real Beamtalk source files)

**Characteristics:**
- Slow (100ms-1s per test)
- Full pipeline: source → lexer → parser → codegen → erlc → BEAM → execute
- Test user-facing scenarios
- Verify expected output with `// =>` assertions

**Example:**
```beamtalk
// tests/e2e/cases/arithmetic.bt
42 + 3
// => 45

2 * 3 + 4
// => 10
```

### Testing Best Practices

1. **Test edge cases first** - Nulls, empty strings, zero values, boundary conditions
2. **Test error conditions** - Invalid input, out-of-bounds, parse errors
3. **Prefer deterministic tests** - No random values, no timing dependencies
4. **Don't test implementation details** - Test behavior, not internal structure
5. **One assertion per test** - Makes failures clear
6. **Descriptive test names** - `test_parse_empty_block` not `test_parse_1`

### Test Priorities

**When adding new features:**
1. Write unit tests for individual functions
2. Write integration test for the feature end-to-end
3. Write E2E test for user-facing scenario (if applicable)

**When fixing bugs:**
1. Add regression test that reproduces the bug
2. Fix the bug
3. Verify test passes

---

## 4. Security-First Development

### Principle

**Treat all user input as untrusted.** The compiler processes arbitrary source code, so robust input validation and error handling are critical.

### Core Concepts

1. **Untrusted input = user source code** - Never assume well-formed input
2. **Input validation at boundaries** - Validate early, fail gracefully
3. **No unsafe code without justification** - Rust's safety guarantees are valuable
4. **Dependency audits** - Regularly check for vulnerabilities
5. **Fuzz testing** - Find crashes from malformed input

### Rules

**✅ ALWAYS:**
- Validate file paths (no directory traversal)
- Check buffer boundaries (no out-of-bounds access)
- Handle parse errors gracefully (no panics)
- Document unsafe code with safety invariants
- Run `cargo audit` before releases

**❌ NEVER:**
- Execute user-provided code without sandboxing
- Trust file extensions (validate content)
- Panic on malformed input
- Use `unwrap()` on user input
- Add dependencies without security review

### Examples

```rust
// ✅ GOOD - Validate and handle errors
pub fn read_source_file(path: &Utf8Path) -> Result<String, std::io::Error> {
    // Validate path is not trying to escape
    if path.as_str().contains("..") {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "path traversal not allowed",
        ));
    }
    std::fs::read_to_string(path)
}

// ❌ BAD - Unsafe without justification
pub fn get_token(&self, index: usize) -> &Token {
    unsafe { self.tokens.get_unchecked(index) } // ❌ Why unsafe? Justify!
}

// ✅ GOOD - Justified unsafe with documented invariants
pub fn get_token(&self, index: usize) -> &Token {
    // SAFETY: Caller guarantees index < self.tokens.len()
    // This is enforced by the parser state machine which never
    // advances past the end of the token stream.
    unsafe { self.tokens.get_unchecked(index) }
}
```

### Dependency Security

**Audit all new dependencies:**
```bash
# Check for known vulnerabilities
cargo audit

# Check licenses and security policies
cargo deny check
```

**Decision:** Manual fuzzing (set up infrastructure, run before releases)

**Rationale:** Fuzzing finds crashes from malformed input, but continuous fuzzing adds CI complexity. Manual runs before releases are sufficient for current maturity level.

### Security Checklist

When reviewing code:
- [ ] User input validated at boundaries
- [ ] No panics on malformed input
- [ ] File I/O errors handled gracefully
- [ ] Unsafe code documented with SAFETY comment
- [ ] Dependencies audited (run `cargo audit`)
- [ ] Buffer accesses bounds-checked

---

## 5. Dependency Management Philosophy

### Principle

**Minimize dependencies. Prefer the standard library. Document every dependency.**

### Core Concepts

1. **Prefer std library** - Only add dependencies when significantly beneficial
2. **No trivial dependencies** - Don't add a crate for `is_even` or `max`
3. **Vetted dependencies only** - High downloads, active maintenance, good reputation
4. **Pin major versions** - Use `"1.2"` not `"*"` or `"1"`
5. **Audit new dependencies** - Security, license, maintenance status
6. **Document why** - Commit message explains why dependency was added

### Decision Criteria

**Add a dependency when:**
- Reimplementing would be complex and error-prone (e.g., `miette` for diagnostics)
- Significantly improves developer experience (e.g., `insta` for snapshot tests)
- Provides critical functionality not in std (e.g., `tracing` for structured logging)
- Well-maintained, widely-used, aligns with Rust ecosystem standards

**Don't add a dependency when:**
- Functionality is trivial to implement (few lines of code)
- Only using 10% of the crate's features
- Unmaintained or low download count
- License incompatibility
- Increases compile time significantly for minor benefit

### Examples

```toml
# ✅ GOOD - Documented dependencies
[dependencies]
# Efficient copy-on-write strings for AST (reduces allocations)
ecow = "0.2"

# UTF-8 paths for cross-platform consistency (avoids encoding issues)
camino = "1.1"

# Rich error diagnostics with source spans (LSP-quality error messages)
miette = "7.6"

# Structured logging for debugging (better than println!)
tracing = "0.1"

# ❌ BAD - Undocumented or questionable dependencies
some-random-crate = "0.1"  # What does this do? Why?
is-even = "1.0"            # Trivial: use `n % 2 == 0`
unmaintained-thing = "2.0" # Last updated 3 years ago
```

### Dependency Review Process

**When adding a new dependency:**

1. **Check crates.io** - Download count, last updated, version
2. **Check GitHub** - Active development, issue response time, security policy
3. **Check license** - Apache-2.0/MIT compatible
4. **Run audit** - `cargo audit` to check for known vulnerabilities
5. **Document in commit** - Explain why the dependency is needed

**Example commit message:**
```
feat: add miette for rich diagnostics

Add miette crate for LSP-quality error messages with source spans,
fix suggestions, and beautiful terminal output. Miette is widely
used in the Rust ecosystem (2M+ downloads) and provides features
that would take weeks to implement ourselves.

Alternative considered: Implement custom diagnostic rendering.
Rejected because miette is well-tested and LSP-compatible.
```

### Keeping Dependencies Updated

**Regular maintenance:**
```bash
# Check for outdated dependencies
cargo outdated

# Update to latest compatible versions
cargo update

# Run tests after updating
cargo test --all-targets
```

**Before releases:**
```bash
# Security audit
cargo audit

# License and security policy check
cargo deny check
```

### Current Dependencies Rationale

**Core dependencies (beamtalk-core):**
- `ecow` - Copy-on-write strings, reduces AST memory usage
- `camino` - UTF-8 paths, avoids Windows encoding issues
- `miette` - Rich diagnostics, LSP-quality error messages
- `thiserror` - Error type derive macros, reduces boilerplate

**Development dependencies:**
- `insta` - Snapshot testing, perfect for codegen tests
- `tracing` - Structured logging, better than println debugging

**Runtime dependencies (Erlang side):**
- Minimal - Erlang/OTP standard library only

---

## Cross-References

- **Domain Driven Design:** [AGENTS.md - Domain Driven Design section](../../AGENTS.md#domain-driven-design-ddd)
- **DDD Model:** [docs/beamtalk-ddd-model.md](../beamtalk-ddd-model.md)
- **Testing Strategy:** [docs/development/testing-strategy.md](testing-strategy.md)
- **Rust Best Practices:** [AGENTS.md - Rust Development Best Practices](../../AGENTS.md#rust-development-best-practices)

---

## Questions or Clarifications?

If these principles are unclear or don't address a specific scenario, open a Linear issue with the `Documentation` label.

**Last updated:** 2026-02-03

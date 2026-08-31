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
6. [Duplication & the Shared-Leaf-Module Pattern](#6-duplication--the-shared-leaf-module-pattern)
7. [Consistency-Test Disposition Rule](#7-consistency-test-disposition-rule)

---

## 1. Layered Architecture & Dependency Rules

### Principle

The beamtalk codebase is organized into **layers with unidirectional dependencies**. Dependencies flow **downward only**—higher layers depend on lower layers, never the reverse.

### Layer Structure

```
┌─────────────────────────────────────┐
│ beamtalk-cli (binary)               │  ← User-facing CLI tool
│ beamtalk-lsp (binary)               │  ← IDE language server
│ beamtalk-mcp (binary)               │  ← Model Context Protocol server
│ beamtalk-compiler-port (binary)     │  ← Erlang compiler bridge
├─────────────────────────────────────┤
│ beamtalk-exec (library)             │  ← REPL/execution engine
│ beamtalk-workspace (library)        │  ← Workspace/session management
├─────────────────────────────────────┤
│ beamtalk-core (library)             │  ← Compiler core (reusable)
│  ├─ language_service/ (Lang Svc)    │
│  ├─ parse/       (Lexer, Parser)    │
│  ├─ analyse/     (Semantic Analysis)│
│  └─ codegen/     (Core Erlang gen)  │
└─────────────────────────────────────┘
```

### Rules

**✅ ALLOWED:**
- `beamtalk-cli` depends on `beamtalk-core`
- `beamtalk-lsp` depends on `beamtalk-core`
- `language_service` depends on `parse`, `analyse`, `codegen`
- `codegen` depends on `parse` (needs AST types)

**❌ FORBIDDEN:**
- `beamtalk-core` importing `beamtalk-cli`
- `parse` importing `codegen`
- `codegen` importing `language_service`

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

**Decision:** Document only (no automated enforcement) for the binary/library layer boundary above.

**Rationale:** Solo developer, code review sufficient. Can add `cargo-deny` later if team grows.

**Action on violation:** Flag in code review, refactor immediately.

**Exception — `beamtalk-core`'s own Compilation/Language Service boundary is
automatically enforced.** ADR 0117 found this diagram's `queries/ (Language
Service)` line had silently drifted out of sync with the code (a
`semantic_analysis → queries` production edge, plus an extensive `queries ⇄
language_service` cycle, existed with nothing to catch them) — exactly the
failure mode "document only" accepts the risk of. `just check-boundary`
(`crates/beamtalk-boundary-check`, BT-3339) now fails CI if a production
`use`/fully-qualified edge inside `beamtalk-core` crosses from the
Compilation bounded context (`ast`, `source_analysis`, `unparse`, `codegen`,
`semantic_analysis`, `compilation`) into Language Service (`language_service`,
`lint`); the reverse direction is unrestricted. `queries` and
`language_service` were originally two separate Rust modules for the one
Language Service DDD context; BT-3342 merged `queries` into
`language_service`, removing the `queries ⇄ language_service` cycle by
construction rather than by CI enforcement (that cycle was never something
`check-boundary` gated in the first place, since both sides were already
inside the same bounded context). This doesn't change the "document only"
decision for the coarser binary/library boundary above — only for this
specific, already-drifted-once edge. See
`docs/ADR/0117-beamtalk-core-crate-split.md`; the layer diagram above still
reflects that ADR's *aspirational* module names (`parse`/`analyse`) rather
than the real ones, corrected as later phases of that ADR land.

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
        ╱ ╲  REPL-Protocol Tests (tests/repl-protocol/cases/*.btscript)
       ╱   ╲  - Real Beamtalk → BEAM execution via REPL TCP
      ╱ 10% ╲  - Slow, high confidence
     ╱───────╲  - User-facing scenarios
    ╱         ╲
   ╱   20-30%  ╲  Integration Tests (runtime/apps/*/test/*.erl)
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

**Location:** `runtime/apps/*/test/*_tests.erl` (EUnit tests in Erlang)

**Characteristics:**
- Medium speed (10-100ms per test)
- Test multiple units working together
- Codegen + runtime interaction
- Use compiled test fixtures

**Examples:**
- `beamtalk_actor_tests.erl` - Actor spawn, message passing, state
- `beamtalk_codegen_simulation_tests.erl` - Compiled `.bt` modules running in runtime

### REPL-Protocol Tests

**Location:** `tests/repl-protocol/cases/*.btscript` (real Beamtalk source files)

**Characteristics:**
- Slow (100ms-1s per test)
- Full pipeline: source → lexer → parser → codegen → erlc → BEAM → execute
- Test user-facing scenarios
- Verify expected output with `// =>` assertions

**Example:**
```beamtalk
// tests/repl-protocol/cases/arithmetic.btscript
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

## 6. Duplication & the Shared-Leaf-Module Pattern

### Principle

**"Layer X can't depend on layer Y" is never a reason to duplicate a rule.** If two modules on different sides of a dependency edge need the same logic, extract it into a shared module that sits *below both of them*, and have both import it. Duplication should always be a deliberate, reviewed extraction decision — not a default reached for because importing looked awkward.

### The anti-pattern

The 2026-08 duplication audit found this comment in `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/declared_type.rs`:

```rust
/// Mirrors `type_resolver::split_generic_base` — duplicated here (rather
/// than imported) because `class_hierarchy` sits below `type_checker` in the
/// dependency graph and must not reach up into it.
fn split_generic_base(type_name: &str) -> (&str, Option<&str>) {
    ...
}
```

This reasoning is backwards. `class_hierarchy` sitting below `type_checker` is exactly why the shared logic should **not** live in `type_checker::type_resolver` — it should live somewhere both can reach: a small leaf module (or a free function in a module) that has no dependency on either `class_hierarchy` or `type_checker`, so both can import it downward. The "layering" observation correctly identifies *that* something is misplaced; it does not justify a second implementation.

### The fix pattern

1. Identify the lowest common layer both consumers can depend on (often a leaf module with no domain logic beyond the one function/table in question — string parsing, a shared constant table, a small pure algorithm).
2. Move the logic there (or create the module if none fits).
3. Have both original sites import it and delete the duplicate.
4. If a "mirrors"/"keep in sync" comment existed at either site, delete it — the comment's job is now done by the compiler (a single definition can't drift from itself).

### Rationale

1. **Drift is silent** — two implementations of the same rule will diverge under normal maintenance; nothing forces them to stay aligned except developer discipline, which does not scale across agents or contributors.
2. **The layering rule stays intact** — extraction respects "dependencies flow down only" (§1) rather than special-casing around it.
3. **Review is cheaper** — one implementation to read, one place to fix a bug.

### Enforcement

**Decision:** Document + code review (no automated enforcement for this class of duplication — it requires judgment about what counts as "the same rule").

**Action on violation:** Flag in code review (see `code-review`/`review-code` skill checklist); extract to a shared leaf module rather than approving a second implementation. See §7 for what happens to any existing "these two agree" test once the duplicate is deleted.

---

## 7. Consistency-Test Disposition Rule

### Principle

Tests that assert "these two independent implementations produce the same result" are a smell **unless** the two implementations are on opposite sides of a boundary you cannot delete (different language, different process, different deployable surface). This rule tells you, for any given pair-test, whether to keep it, convert it, or migrate it.

> **A consistency test across a boundary you cannot delete is enforcement — keep it and extend it. A consistency test between two copies you can delete is a smell — delete the copy; the test becomes an ordinary unit/golden test of the single implementation.**

### Applying the rule

| Situation | Disposition | Example |
|---|---|---|
| Two implementations in different languages, compiled/reviewed separately, that must agree on wire format or behavior | **Keep** — this is the permanent boundary the test exists to guard | `beamtalk-surface-drift` (CLI/REPL/MCP/LSP), `beamtalk-parity-tests` (cross-process parity), the Rust↔Erlang conformance fixtures from BT-3080/BT-3081/BT-3085/BT-3090 |
| Two Rust (or two Erlang) implementations of the same rule, previously duplicated, now being consolidated into one | **Convert** to a golden test (fixed input → fixed expected output) once the duplicate is deleted; delete the re-implementation the test used for comparison | `crates/beamtalk-workspace/tests/cross_crate_consistency.rs` — see below |
| A hand-written "simulated compiler output" fixture that stands in for real compiled output | **Migrate** the fixture onto compiled/generated output so drift fails at build time instead of needing a human to update the simulation | `beamtalk_codegen_simulation_tests.erl`'s simulated-state sections; precedent in BT-239 |

### Worked example: `cross_crate_consistency.rs`

Before BT-3091, `crates/beamtalk-workspace/tests/cross_crate_consistency.rs` re-implemented the (already-deleted) CLI and MCP workspace-ID hashing algorithms inline, purely so it could assert the shared `generate_workspace_id` helper matched them. Once the CLI/MCP duplicates were deleted, there was nothing left to be inconsistent *with* — the "matches the CLI algorithm" and "matches the MCP algorithm" tests were really just two copies of "hashes this known path to this known 12-hex string." The first conversion pass still hand-reimplemented SHA-256 inline to compute the expected value against a platform-dependent `std::env::temp_dir()` path — the same "two implementations that must be hand-kept in sync" smell, just moved from CLI/MCP-vs-shared to test-vs-shared. The fix: extract the pure hashing step (`hash_workspace_path_string`, no `canonicalize()`, no filesystem) out of `generate_workspace_id`, and golden-test *that* directly against a fixed literal input and a hardcoded expected output (`test_workspace_id_hash_is_stable_for_known_input`) — one production code path, zero re-implementation, and no per-OS variance. A separate, unpinned structural test (`test_generate_workspace_id_returns_well_formed_id_for_real_path`) still covers the `canonicalize()`-and-delegate wiring against a real path. This is the template: when a sibling consolidation issue deletes one side of a pair, revisit that pair's consistency test, and if computing the "expected" value still means re-deriving the algorithm by hand, extract the pure computation and golden-test that instead of the wrapper.

### Rationale

1. **Pair-tests across a deletable duplication hide the real bug** — they make the duplication look "safe" (tested!) instead of flagging it for removal.
2. **Golden tests are cheaper to maintain** — one code path, one set of fixtures, no risk of the "reference" implementation silently drifting out of sync with reality the way the thing under test can.
3. **Boundary tests earn their complexity** — when the two sides genuinely can't merge (different language runtimes, different processes), the dual-implementation test is the correct design, not a smell.

---

## Cross-References

- **Domain-Driven Design:** [AGENTS.md - Domain-Driven Design section](../../docs/agents/expanded.md#domain-driven-design-ddd)
- **Rust Best Practices:** [AGENTS.md - Rust Development Best Practices](../../docs/agents/expanded.md#development-architecture-principles)
- **Duplication & Drift Prevention (agent guidelines):** [docs/agents/expanded.md § Duplication & Drift Prevention](../../docs/agents/expanded.md#duplication--drift-prevention)

---

## Questions or Clarifications?

If these principles are unclear or don't address a specific scenario, open a Linear issue with the `Documentation` label.

**Last updated:** 2026-08-09

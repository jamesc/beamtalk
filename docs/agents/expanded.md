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

**Important:** While heavily inspired by Smalltalk, Beamtalk makes pragmatic departures from Smalltalk syntax and semantics to work well with BEAM and modern development practices. We're "Smalltalk-like," not Smalltalk-compatible. See [docs/beamtalk-syntax-rationale.md](../docs/beamtalk-syntax-rationale.md) for specific differences, [docs/beamtalk-principles.md](../docs/beamtalk-principles.md) for design philosophy, and [docs/beamtalk-language-features.md](../docs/beamtalk-language-features.md) for full language specification.

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

In test files (`tests/stdlib/*.bt` and `tests/e2e/cases/*.bt`):
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
3. **Tests:** `tests/stdlib/*.bt`, `tests/e2e/cases/*.bt`
4. **Parser tests:** `crates/beamtalk-core/src/source_analysis/parser/mod.rs`

**If it doesn't appear in any of these → it's likely hallucinated. Search the codebase or ask.**

---

## Domain Driven Design (DDD)

The beamtalk architecture follows **Domain Driven Design**. Use domain terms consistently (e.g., `CompletionProvider` not `completions`). The codebase has four bounded contexts: **Language Service** (IDE features), **Compilation** (lexer→codegen), **Runtime** (actors, OTP), and **REPL** (interactive dev).

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
- **Use bare tuple errors** - ALL errors MUST use `#beamtalk_error{}` records

✅ **ALWAYS:**
- Return `(Result, Vec<Diagnostic>)` or equivalent for user-facing operations
- Validate file paths and buffer boundaries
- Document unsafe code with `// SAFETY:` comment explaining invariants
- Run `cargo audit` before releases
- **Use structured errors** - `beamtalk_error:new/with_selector/with_hint` in all code

See full guide: [docs/development/architecture-principles.md](../docs/development/architecture-principles.md)

---

## Error Handling - CRITICAL RULES

**NO bare tuple errors EVER!** All errors in the beamtalk codebase MUST use the structured `#beamtalk_error{}` system.

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
| `stdlib` | Standard library: collections, primitives, strings (compiled from `lib/*.bt` via pragmas — see [ADR 0007](../docs/ADR/0007-compilable-stdlib-with-primitive-injection.md)) | `lib/` |
| `repl` | REPL backend and CLI interaction | `runtime/src/beamtalk_repl.erl`, `crates/beamtalk-cli/src/repl/` |
| `cli` | Command-line interface and build tooling | `crates/beamtalk-cli/` |
| `codegen` | Code generation to Core Erlang/BEAM | `crates/beamtalk-core/src/erlang.rs` |
| `runtime` | Erlang runtime: actors, futures, OTP integration | `runtime/src/` |
| `parser` | Lexer, parser, AST | `crates/beamtalk-core/src/source_analysis/`, `crates/beamtalk-core/src/ast.rs` |

... (remaining detailed guidance preserved in this expanded file)

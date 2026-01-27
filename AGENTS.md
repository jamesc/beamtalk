# AGENTS.md - Beamtalk Development Guidelines

This document provides guidance for AI coding agents working on the beamtalk compiler and ecosystem.

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

- `agent-ready` - Task is fully specified, agent can start immediately
- `needs-spec` - Requires human to clarify requirements before work begins
- `blocked` - Waiting on external dependency or another issue
- `human-review` - Agent completed work, needs human verification

### Writing Agent-Ready Issues

For an issue to be `agent-ready`, include:

1. **Context** - Why this work matters, background info
2. **Acceptance Criteria** - Specific, testable requirements (checkboxes)
3. **Files to Modify** - Explicit paths to relevant files
4. **Dependencies** - Other issues that must complete first
5. **References** - Links to specs, examples, or related code

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

### Code Style

- Use `rustfmt` with default settings; run `cargo fmt` before committing
- Use `clippy` at warn level: `cargo clippy -- -W clippy::all`
- Prefer `thiserror` for error types, `miette` for user-facing diagnostics
- Use `ecow::EcoString` for AST string data (copy-on-write efficiency)
- Use `camino::Utf8PathBuf` instead of `std::path::PathBuf`

### Error Handling

- Return `Result<T, Error>` from all fallible functions
- Use `?` operator for propagation; avoid `.unwrap()` except in tests
- Attach source spans to all errors for diagnostic rendering
- Group related errors; don't fail on first error when possible

### Testing

- Unit tests go in the same file as the code (`#[cfg(test)] mod tests`)
- Use `insta` for snapshot testing of parser output and codegen
- Integration tests in `test-package-compiler/cases/` directories
- Name test functions descriptively: `fn parse_message_send_with_multiple_keywords()`

### Compiler-Specific Patterns

```rust
// AST nodes should carry source location
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

// Use newtypes for IDs to prevent mixing
pub struct ModuleId(u32);
pub struct FunctionId(u32);

// Visitor pattern for AST traversal
pub trait AstVisitor {
    fn visit_expression(&mut self, expr: &Expression);
    fn visit_statement(&mut self, stmt: &Statement);
}
```

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

For agents working on the parser or generating beamtalk code:

```smalltalk
"Unary message"
object message

"Binary message"
3 + 4

"Keyword message"
array at: 1 put: 'hello'

"Cascade - multiple messages to same receiver"
Transcript show: 'Hello'; cr; show: 'World'

"Blocks (closures)"
[:x :y | x + y]

"Assignment"
count := 0

"Return"
^result
```

### Message Precedence (high to low)
1. Unary messages: `3 factorial`
2. Binary messages: `3 + 4`
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

- [Design Principles](docs/beamtalk-principles.md) - Core philosophy guiding all decisions
- [Core Erlang specification](https://www.it.uu.se/research/group/hipe/cerl/)
- [BEAM VM internals](https://blog.stenmans.org/theBeamBook/)
- [Gleam compiler](https://github.com/gleam-lang/gleam) - reference implementation
- [Newspeak language](https://newspeaklanguage.org/) - module system inspiration

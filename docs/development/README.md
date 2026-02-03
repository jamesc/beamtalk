# Development Documentation

This directory contains documentation for **contributors and AI agents** working on the Beamtalk compiler, runtime, and tooling.

**Audience:** Developers, maintainers, and AI coding agents contributing to the Beamtalk project.

---

## Quick Links

### Core Development Guides

| Guide | Description |
|-------|-------------|
| [Architecture Principles](architecture-principles.md) | Core development principles: layered architecture, error recovery, testing, security, dependencies |
| [Rust Guidelines](rust-guidelines.md) | Rust coding standards: naming, traits, error handling, testing, compiler patterns |
| [Erlang Guidelines](erlang-guidelines.md) | Erlang/OTP patterns: code generation, BEAM interop, testing |
| [Common Tasks](common-tasks.md) | Step-by-step guides: adding AST nodes, CLI commands, stdlib features |
| [Testing Strategy](testing-strategy.md) | Test organization, fixtures, E2E tests, coverage requirements |
| [AGENTS.md](../../AGENTS.md) | Complete guide for AI agents contributing to the codebase |

### Language Documentation

| Document | Description |
|----------|-------------|
| [Language Features](../beamtalk-language-features.md) | Full Beamtalk syntax specification |
| [Syntax Rationale](../beamtalk-syntax-rationale.md) | Design decisions: why we keep/change Smalltalk conventions |

### Related Documentation

| Document | Description |
|----------|-------------|
| [DDD Model](../beamtalk-ddd-model.md) | Domain-driven design: bounded contexts, ubiquitous language |
| [Architecture Overview](../beamtalk-architecture.md) | High-level compiler and runtime architecture |
| [Internal Docs](../internal/) | Implementation details, design documents, internals |

---

## Development Principles Summary

The Beamtalk codebase follows **5 core architectural principles**:

1. **Layered Architecture** - Dependencies flow down only (core never depends on CLI)
2. **Error Recovery** - Return partial results + diagnostics (don't stop at first error)
3. **Testing Pyramid** - Unit 60-70%, Integration 20-30%, E2E 10%
4. **Security-First** - Input validation at boundaries, no unsafe without justification
5. **Minimal Dependencies** - Prefer std library, document why each dependency exists

See [Architecture Principles](architecture-principles.md) for full details.

---

## Domain Driven Design

Beamtalk's architecture is driven by **Domain Driven Design (DDD)** principles:

- **Ubiquitous Language** - Code uses domain terms consistently (e.g., `CompletionProvider`, not `completions`)
- **Bounded Contexts** - Language Service, Compilation, Runtime, REPL
- **Domain Services** - Stateless operations on domain objects
- **Value Objects** - Immutable types like `Span`, `Position`, `Token`

See [DDD Model](../beamtalk-ddd-model.md) for the complete domain model.

---

## Testing Organization

Beamtalk has **3 layers of tests**:

1. **Unit Tests** (`#[cfg(test)]` in Rust files)
   - Fast, isolated, 60-70% of test suite
   - Test individual functions and modules

2. **Integration Tests** (`runtime/test/*.erl`)
   - Medium speed, 20-30% of test suite
   - Test codegen + runtime interaction

3. **E2E Tests** (`tests/e2e/cases/*.bt`)
   - Slow, 10% of test suite
   - Full pipeline: source → BEAM execution

See [Testing Strategy](testing-strategy.md) for detailed guidance.

---

## Getting Started

### For AI Agents

1. Read [AGENTS.md](../../AGENTS.md) - Complete guide for AI agents
2. Review [Architecture Principles](architecture-principles.md) - Core development rules
3. Check [DDD Model](../beamtalk-ddd-model.md) - Understand bounded contexts

### For Human Contributors

1. Read [Architecture Overview](../beamtalk-architecture.md) - High-level design
2. Review [Architecture Principles](architecture-principles.md) - Development standards
3. Check [Testing Strategy](testing-strategy.md) - How to write tests
4. Browse [Internal Docs](../internal/) - Implementation details

---

## Critical Rules

**❌ NEVER:**
- `beamtalk-core` importing `beamtalk-cli` or `beamtalk-lsp`
- Panic on user input (malformed source, invalid args)
- Add dependencies without justification in commit message
- Use `unwrap()` on user input

**✅ ALWAYS:**
- Return `(Result, Vec<Diagnostic>)` for user-facing operations
- Validate file paths and buffer boundaries
- Document unsafe code with `// SAFETY:` comments
- Run `cargo audit` before releases

---

## Documentation Structure

```
docs/
├── development/                 # ← YOU ARE HERE
│   ├── README.md               # This file
│   ├── architecture-principles.md
│   └── testing-strategy.md
│
├── beamtalk-*.md               # Language design & architecture
├── internal/                   # Implementation details
└── README.md                   # Overall docs index
```

---

## Questions or Feedback?

- **For architecture decisions:** Open Linear issue with `Documentation` label
- **For DDD clarifications:** Reference [beamtalk-ddd-model.md](../beamtalk-ddd-model.md)
- **For testing questions:** See [testing-strategy.md](testing-strategy.md)

**Last updated:** 2026-02-03

# Internal Documentation

This directory contains internal implementation documentation, design decisions, and technical planning documents. These are intended for developers working on the beamtalk compiler, not end users.

## Contents

### Architecture & Design

| Document | Description |
|----------|-------------|
| [Semantic Analysis](semantic-analysis.md) | Design for AST validation, name resolution, and block context analysis |
| [Parser Architecture](parser-architecture.md) | Parser design decisions, Pratt parsing integration, BT-109 research findings |
| [REPL Internals](repl-internals.md) | REPL architecture, session state, protocol details, and evaluation flow |

### Implementation Tracking

| Document | Description |
|----------|-------------|
| [Operator Implementation Status](operator-implementation-status.md) | Cross-reference of documented vs. implemented binary operators |
| [Coverage Analysis](coverage-analysis.md) | Core Erlang compilation verification test coverage analysis |

### Implementation Tracking

| Document | Description |
|----------|-------------|
| [Operator Implementation Status](OPERATOR_IMPLEMENTATION_STATUS.md) | Cross-reference of documented vs. implemented binary operators |
| [Coverage Analysis](COVERAGE_ANALYSIS.md) | Core Erlang compilation verification test coverage analysis |

## When to Add Documents Here

Add internal docs for:
- **Architecture decisions** - ADRs, design rationale for complex systems
- **Implementation plans** - Multi-phase feature implementations  
- **Technical debt tracking** - Known issues and planned refactors
- **Internal APIs** - Compiler internals not exposed to users

## Relationship to Linear

These documents complement Linear issues:
- **Linear issues** = discrete work items with acceptance criteria
- **Internal docs** = broader context, design rationale, cross-cutting concerns

Reference Linear issues from docs: `See [BT-90](https://linear.app/beamtalk/issue/BT-90)`

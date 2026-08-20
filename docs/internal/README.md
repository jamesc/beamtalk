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
| [Class-Keyed ETS Tables Investigation](class-keyed-ets-tables-investigation.md) | BT-2222 survey + decision on consolidating the six class-keyed ETS tables |

### Spike Findings

Validation spikes run ahead of an ADR's implementation phases — each records
what held, what broke, and what the following phase must change as a result.

| Document | Description |
|----------|-------------|
| [ADR 0105 Phase 0](adr-0105-phase0-spike-findings.md) | BT-2776: live image re-checking — signature capture, dependent lookup, batched port re-check |
| [ADR 0115 Phase 1](adr-0115-phase1-spike-findings.md) | BT-3216: xref receiver-type key — `InferredType` reachability, hierarchy-closure and `conforms_to/2` cost, default-fallback soundness |

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

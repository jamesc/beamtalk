# Internal Documentation

This directory contains internal implementation documentation, design decisions, and technical planning documents. These are intended for developers working on the beamtalk compiler, not end users.

## Contents

| Document | Description |
|----------|-------------|
| [Semantic Analysis](semantic-analysis.md) | Design for AST validation, name resolution, and block context analysis |

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

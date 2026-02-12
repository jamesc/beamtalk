# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records (ADRs) for Beamtalk.

ADRs document significant architectural and design decisions, including:
- Language design choices
- Implementation strategies
- Trade-offs and their rationale

## Format

Each ADR follows the structure in [TEMPLATE.md](TEMPLATE.md). Key sections:

- **Status:** Proposed, Accepted, Implemented, Deprecated, Superseded
- **Context:** Background, problem statement, constraints
- **Decision:** What was decided, with code and REPL examples
- **Prior Art:** How reference languages handle this
- **User Impact:** How this affects each user persona
- **Steelman Analysis:** Best argument for rejected alternatives from each user cohort
- **Alternatives Considered:** What else was evaluated and why it was rejected
- **Consequences:** Positive, negative, and neutral impacts
- **Implementation:** Affected components and rough phases
- **Migration Path:** How to migrate existing code (if applicable)
- **References:** Related issues, ADRs, documentation

## ADR List

| Number | Title | Status | Date |
|--------|-------|--------|------|
| [0001](0001-no-compound-assignment.md) | No Compound Assignment in Beamtalk | Implemented | 2026-02-08 |
| [0002](0002-use-erlang-comparison-operators.md) | Use Erlang Comparison Operators Directly | Accepted | 2026-02-03 |
| [0003](0003-core-erlang-vs-erlang-source.md) | Keep Core Erlang as Primary Code Generation Target | Implemented | 2026-02-08 |
| [0004](0004-persistent-workspace-management.md) | Persistent Workspace Management | Accepted | 2026-02-05 |
| [0005](0005-beam-object-model-pragmatic-hybrid.md) | BEAM Object Model - Pragmatic Hybrid Approach | Accepted | 2026-02-05 |
| [0006](0006-unified-method-dispatch.md) | Unified Method Dispatch with Hierarchy Walking | Accepted | 2026-02-05 |
| [0007](0007-compilable-stdlib-with-primitive-injection.md) | Compilable Standard Library with Primitive Injection | Accepted | 2026-02-06 |
| [0008](0008-doc-comments-and-api-documentation.md) | Doc Comments and API Documentation | Accepted | 2026-02-12 |
| [0009](0009-otp-application-structure.md) | OTP Application Structure — Split Workspace from Runtime | Implemented | 2026-02-07 |
| [0010](0010-global-objects-and-singleton-dispatch.md) | Global Objects and Singleton Dispatch | Accepted | 2026-02-07 |
| [0011](0011-robustness-testing-layered-fuzzing.md) | Robustness Testing — Layered Fuzzing and Error Quality | Proposed | 2026-02-07 |
| [0012](0012-list-literal-syntax.md) | Collection Literal Syntax and the `#` Data Literal System | Accepted | 2026-02-08 |
| [0013](0013-class-variables-class-methods-instantiation.md) | Class Variables, Class-Side Methods, and Instantiation Protocol | Proposed | 2026-02-09 |
| [0014](0014-beamtalk-test-framework.md) | Beamtalk Test Framework — Native Unit Tests and CLI Integration Tests | Accepted | 2026-02-09 |
| [0015](0015-repl-error-objects-and-exception-hierarchy.md) | Signal-Time Exception Objects and Error Class Hierarchy | Proposed | 2026-02-10 |
| [0016](0016-unified-stdlib-module-naming.md) | Unified Stdlib Packaging and Module Naming | Proposed | 2026-02-10 |
| [0018](0018-document-tree-codegen.md) | Document Tree Code Generation (Wadler-Lindig Pretty Printer) | Proposed | 2026-02-11 |
| [0019](0019-singleton-class-variables.md) | Singleton Access via Class Variables | Accepted | 2026-02-12 |
| [0020](0020-connection-security.md) | Connection Security — mTLS, Proxies, and Network Overlays | Proposed | 2026-02-12 |
| [0021](0021-streams-and-io-design.md) | Stream — Universal Data Interface | Proposed | 2026-02-12 |

## Creating New ADRs

1. **Number sequentially:** Use the next available number (0003, 0004, etc.)
2. **Use descriptive titles:** Make it clear what decision is being made
3. **Document thoroughly:** Include context, rationale, and consequences
4. **Link to issues:** Reference Linear issues (e.g., BT-XXX) where relevant
5. **Update this index:** Add your ADR to the table above

## References

- [ADR template and best practices](https://github.com/joelparkerhenderson/architecture-decision-record)
- [Why write ADRs](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)

# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records (ADRs) for Beamtalk.

ADRs document significant architectural and design decisions, including:
- Language design choices
- Implementation strategies
- Trade-offs and their rationale

## Format

Each ADR follows the structure in [TEMPLATE.md](TEMPLATE.md). Key sections:

- **Status:** Proposed, Accepted, Implemented, Deprecated, Superseded, Rejected
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
| [0002](0002-use-erlang-comparison-operators.md) | Use Erlang Comparison Operators Directly | Implemented | 2026-02-08 |
| [0003](0003-core-erlang-vs-erlang-source.md) | Keep Core Erlang as Primary Code Generation Target | Implemented | 2026-02-08 |
| [0004](0004-persistent-workspace-management.md) | Persistent Workspace Management | Implemented | 2026-02-15 |
| [0005](0005-beam-object-model-pragmatic-hybrid.md) | BEAM Object Model - Pragmatic Hybrid Approach | Implemented | 2026-02-08 |
| [0006](0006-unified-method-dispatch.md) | Unified Method Dispatch with Hierarchy Walking | Implemented | 2026-02-08 |
| [0007](0007-compilable-stdlib-with-primitive-injection.md) | Compilable Standard Library with Primitive Injection | Implemented | 2026-02-07 |
| [0008](0008-doc-comments-and-api-documentation.md) | Doc Comments and API Documentation | Implemented | 2026-02-15 |
| [0009](0009-otp-application-structure.md) | OTP Application Structure — Split Workspace from Runtime | Implemented | 2026-02-07 |
| [0010](0010-global-objects-and-singleton-dispatch.md) | Global Objects and Singleton Dispatch | Implemented | 2026-02-15 |
| [0011](0011-robustness-testing-layered-fuzzing.md) | Robustness Testing — Layered Fuzzing and Error Quality | Implemented | 2026-02-15 |
| [0012](0012-list-literal-syntax.md) | Collection Literal Syntax and the `#` Data Literal System | Implemented | 2026-02-15 |
| [0013](0013-class-variables-class-methods-instantiation.md) | Class Variables, Class-Side Methods, and Instantiation Protocol | Implemented | 2026-02-15 |
| [0014](0014-beamtalk-test-framework.md) | Beamtalk Test Framework — Native Unit Tests and CLI Integration Tests | Implemented | 2026-02-15 |
| [0015](0015-repl-error-objects-and-exception-hierarchy.md) | Signal-Time Exception Objects and Error Class Hierarchy | Implemented | 2026-02-15 |
| [0016](0016-unified-stdlib-module-naming.md) | Unified Stdlib Packaging and Module Naming | Implemented | 2026-02-10 |
| [0017](0017-browser-connectivity-to-running-workspaces.md) | Browser Connectivity to Running Workspaces | Implemented (Phases 0–2) | 2026-02-17 |
| [0018](0018-document-tree-codegen.md) | Document Tree Code Generation (Wadler-Lindig Pretty Printer) | Implemented | 2026-02-15 |
| [0019](0019-singleton-class-variables.md) | Singleton Access via Class Variables | Implemented | 2026-02-15 |
| [0020](0020-connection-security.md) | Connection Security — mTLS, Proxies, and Network Overlays | Implemented | 2026-02-17 |
| [0021](0021-streams-and-io-design.md) | Stream — Universal Data Interface | Implemented | 2026-02-15 |
| [0022](0022-embedded-compiler-via-otp-port.md) | Embedded Compiler via OTP Port (with NIF option) | Implemented | 2026-02-15 |
| [0023](0023-string-interpolation-and-binaries.md) | String Interpolation Syntax and Compilation | Implemented | 2026-02-17 |
| [0024](0024-static-first-live-augmented-ide-tooling.md) | Static-First, Live-Augmented IDE Tooling | Implemented (Phase 1) | 2026-02-17 |
| [0025](0025-gradual-typing-and-protocols.md) | Gradual Typing and Protocols | Implemented (Phases 1–2) | 2026-02-15 |
| [0026](0026-package-definition-and-project-manifest.md) | Package Definition and Project Manifest | Implemented | 2026-02-17 |
| [0027](0027-cross-platform-support.md) | Cross-Platform Support | Implemented | 2026-02-17 |
| [0028](0028-beam-interop-strategy.md) | BEAM Interop Strategy | Implemented | 2026-02-17 |
| [0029](0029-streaming-eval-output.md) | Streaming Eval Output | Implemented | 2026-02-18 |
| [0030](0030-cargo-dist-evaluation.md) | cargo-dist Evaluation for Release Packaging | Rejected | 2026-02-18 |
| [0031](0031-flat-namespace-for-v01.md) | Flat Namespace for v0.1 | Implemented | 2026-02-18 |
| [0032](0032-early-class-protocol.md) | Early Class Protocol — Behaviour and Class in Beamtalk Stdlib | Implemented | 2026-02-24 |
| [0033](0033-runtime-embedded-documentation.md) | Runtime-Embedded Documentation on Class and CompiledMethod | Implemented | 2026-02-24 |
| [0034](0034-stdlib-self-hosting-in-beamtalk.md) | Stdlib Self-Hosting — Moving Logic from Erlang to Beamtalk | Implemented | 2026-02-24 |
| [0035](0035-field-based-reflection-api.md) | Rename Instance Variable Reflection API from instVar to field | Implemented | 2026-02-24 |
| [0036](0036-full-metaclass-tower.md) | Full Metaclass Tower | Implemented | 2026-02-24 |
| [0037](0037-collection-class-hierarchy.md) | Collection Class Hierarchy — Shallow Abstract Layer with Minimal Primitive Surface | Implemented | 2026-02-24 |
| [0038](0038-subclass-classbuilder-protocol.md) | `subclass:` Grammar Desugaring to ClassBuilder Protocol | Accepted | 2026-02-24 |
| [0039](0039-syntax-pragmatism-vs-smalltalk.md) | Syntax Pragmatism vs Smalltalk | Implemented | 2026-02-24 |
| [0040](0040-workspace-native-repl-commands.md) | Workspace-Native REPL Commands, Facade/Dictionary Split, and Class-Based Reload | Accepted | 2026-02-24 |
| [0041](0041-universal-state-threading-block-protocol.md) | Universal State-Threading Block Protocol | Accepted | 2026-02-24 |
| [0042](0042-immutable-value-objects-actor-mutable-state.md) | Immutable Value Objects and Actor-Only Mutable State | Accepted | 2026-02-26 |
| [0043](0043-sync-by-default-actor-messaging.md) | Sync-by-Default Actor Messaging | Accepted | 2026-02-26 |
| [0044](0044-comments-as-first-class-ast-nodes.md) | Comments as First-Class AST Nodes | Accepted | 2026-02-28 |
| [0045](0045-repl-expression-completion-type-inference.md) | REPL Expression-Level Completion via Gradual Type Inference | Proposed | 2026-03-01 |

## Creating New ADRs

1. **Number sequentially:** Use the next available number (0003, 0004, etc.)
2. **Use descriptive titles:** Make it clear what decision is being made
3. **Document thoroughly:** Include context, rationale, and consequences
4. **Link to issues:** Reference Linear issues (e.g., BT-XXX) where relevant
5. **Update this index:** Add your ADR to the table above

## References

- [ADR template and best practices](https://github.com/joelparkerhenderson/architecture-decision-record)
- [Why write ADRs](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)

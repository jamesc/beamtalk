# beamtalk-codegen

The Beamtalk Code Generation bounded context: Core Erlang code generation
(`core_erlang`) built on top of `beamtalk-core`'s Compilation context (`ast`,
`semantic_analysis`, `source_analysis`, `unparse`).

Depends on `beamtalk-core` and `beamtalk-cerl-doc`, never the reverse — see
[`docs/development/architecture-principles.md`](../../docs/development/architecture-principles.md)
§1 and [ADR 0117](../../docs/ADR/0117-beamtalk-core-crate-split.md), Decision
step 5.

`beamtalk-cli`, `beamtalk-repl`, and `beamtalk-compiler-port` consume this
crate's `core_erlang::{generate_module, CodegenOptions, ...}` API rather than
reaching into `beamtalk-core` for it. `beamtalk-lsp`, `beamtalk-mcp`, and
`beamtalk-lint` have no dependency on this crate at all — they never need to
generate code, only analyze it.

ThreadedIr (`core_erlang::threaded_ir`) and its `verify()` invariants live
here — see `docs/development/debugging.md` § ThreadedIr verifier and
[ADR 0111](../../docs/ADR/0111-lowered-ir-verifier-for-state-threading.md).

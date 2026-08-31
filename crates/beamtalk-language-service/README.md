# beamtalk-language-service

The Beamtalk Language Service bounded context: the IDE-facing query API
(completions, hover, signature help, diagnostics, go-to-definition,
find-references, document symbols, folding ranges, code actions, call/type
hierarchy) built on top of `beamtalk-core`'s Compilation context (`ast`,
`source_analysis`, `semantic_analysis`, `unparse`, `compilation`).

Depends on `beamtalk-core`, never the reverse — see
[`docs/development/architecture-principles.md`](../../docs/development/architecture-principles.md)
§1 and [ADR 0117](../../docs/ADR/0117-beamtalk-core-crate-split.md), Decision
step 5.

`beamtalk-lsp`, `beamtalk-mcp`, `beamtalk-lint`, `beamtalk-cli`, and
`beamtalk-compiler-port` consume this crate's `LanguageService` /
`SimpleLanguageService` API and its `queries` submodule (the individual
query-provider implementations) rather than reaching into `beamtalk-core`
for them.

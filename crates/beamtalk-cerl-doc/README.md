# beamtalk-cerl-doc

Shared leaf crate: the Core Erlang `Document`/`leaf`/`docvec!` pretty-printing API
(ADR 0089 — Typed Document Leaves).

Provides the Wadler-Lindig `Document` tree, the typed-leaf constructors (`leaf::atom`,
`leaf::var`, `leaf::string_lit`, …), and the `docvec!` macro used to build Core Erlang
output declaratively, without either of its two consumers (`codegen`'s Core Erlang
generator and `unparse`'s Beamtalk-source pretty-printer) depending on the other for it —
the shared-leaf-module pattern from
[`docs/development/architecture-principles.md`](../../docs/development/architecture-principles.md) §6.
See [ADR 0117](../../docs/ADR/0117-beamtalk-core-crate-split.md), Decision step 4.

Also provides a `hash` module with a `fnv1a_64` FNV-1a 64-bit hash shared by
`beamtalk-codegen` (long-selector atom mangling) and `beamtalk-build` (spec-mapping
stamp). It lives here because this crate is the only zero-dependency shared leaf below
both consumers.

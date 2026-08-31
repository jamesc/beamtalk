# beamtalk-boundary-check

CI lint that enforces the bounded-context dependency direction
`architecture-principles.md` §1 documents for `beamtalk-core`: the
**Compilation** context (`ast`, `source_analysis`, `unparse`, `codegen`,
`semantic_analysis`, `compilation`) may never import from the
**Language Service** context (`language_service`, `lint`) in
production code. The reverse direction (Language Service consuming
Compilation) is expected and allowed. (`queries` used to be a separate
top-level module in this list too, until BT-3342 merged it into
`language_service` to remove the `queries <-> language_service` cycle by
construction.)

It parses every `.rs` file under `crates/beamtalk-core/src/<compilation
module>/` with `syn` and walks the AST for `use crate::...` imports and
fully-qualified `crate::...` path expressions, skipping anything gated by
`#[cfg(test)]` (directly, or inherited from an enclosing `#[cfg(test)]
mod`) or `#[test]`, plus whole files under a `tests/` directory or named
`*_test(s).rs` — Cargo permits cyclic dev-dependencies, so test-only edges
are not a violation. Any production edge into `language_service` or `lint`
fails the check unless explicitly allow-listed (with a `BT-NNNN` comment)
for a known, already-tracked violation.

Run via `just check-boundary`. Wired into `just ci`.

Tracked in BT-3339 (ADR 0117, Decision step 1).

# beamtalk-boundary-check

CI lint that enforces the bounded-context dependency direction
`architecture-principles.md` §1 documents for `beamtalk-core`: the
**Compilation** context (`ast`, `source_analysis`, `unparse`, `codegen`,
`semantic_analysis`, `compilation`) may never import from the
**Language Service** context (`queries`, `language_service`, `lint`) in
production code. The reverse direction (Language Service consuming
Compilation) is expected and allowed.

It parses every `.rs` file under `crates/beamtalk-core/src/<compilation
module>/` with `syn` and walks the AST for `use crate::...` imports and
fully-qualified `crate::...` path expressions, skipping anything gated by
`#[cfg(test)]` (directly, or inherited from an enclosing `#[cfg(test)]
mod`) or `#[test]`, plus whole files under a `tests/` directory or named
`*_test(s).rs` — Cargo permits cyclic dev-dependencies, so test-only edges
are not a violation. Any production edge into `queries`, `language_service`,
or `lint` fails the check unless explicitly allow-listed (with a `BT-NNNN`
comment) for a known, already-tracked violation.

As of BT-3361 (ADR 0117 Decision step 5), `queries` and `language_service`
no longer exist as directories under `beamtalk-core/src` at all — that whole
module tree moved into the standalone `beamtalk-language-service` crate
(joining `lint`, which BT-3340 moved into `beamtalk-lint` earlier) — so the
Compilation→Language-Service direction rule for those two is now also
cargo-enforced for any cross-crate edge. All three names stay in this
checker's `LANGUAGE_SERVICE_MODULES` list as regression guards: nothing in
`beamtalk-core` should ever declare a module with one of these names again.

Run via `just check-boundary`. Wired into `just ci`.

Tracked in BT-3339 (ADR 0117, Decision step 1); BT-3361 (ADR 0117, Decision
step 5) moved `queries`/`language_service` out of `beamtalk-core` entirely.

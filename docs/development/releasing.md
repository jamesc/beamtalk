# Versioning & Releases

## Single source of truth

`VERSION` at the repo root (e.g., `0.3.0`) is the only place a version is authored. Everything else derives from it at build time — **do not** hand-edit version strings in `.app.src` files, `build_stdlib.rs`, or `editors/liveview/mix.exs`.

How the version flows at build time:

- **Rust:** `build.rs` reads `VERSION` + git state → `BEAMTALK_VERSION` compile-time env var.
- **Erlang:** `.app.src` files use `{vsn, {cmd, "escript ../../../scripts/version.escript"}}`.
- **LiveView IDE (Elixir):** `editors/liveview/mix.exs` reads `VERSION` at compile time, so the `bt_attach` release/Docker tag tracks the toolchain version (BT-2514). It ships on its own release lane (BT-2512), not the core bundle.
- **Dev builds** (not on a git tag): version becomes `0.3.0-dev+<short sha>`.
- **Release builds** (on a git tag): version is the clean `0.3.0`.

The **Rust toolchain** is pinned in `rust-toolchain.toml`; update it there when upgrading Rust.

## Cutting a release

1. Update `VERSION` with the new version number.
2. Update `Cargo.toml` workspace `version` to match (required by Cargo).
3. Update `CHANGELOG.md` — add a new section following the existing format (Language, Standard Library, Runtime, Tooling, Internal subsections as needed).
4. Update version references in docs: `README.md`, `docs/beamtalk-tooling.md`, `docs/beamtalk-language-features.md`, `docs/repl-protocol.md`, `docs/beamtalk-ddd-model.md`, `crates/beamtalk-examples/corpus.json`.
5. Tag the release commit: `git tag v<version>`.

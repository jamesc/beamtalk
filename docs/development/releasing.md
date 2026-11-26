# Versioning & Releases

## Single source of truth

`VERSION` at the repo root (e.g., `0.3.0`) is the only place a version is authored. Everything else derives from it at build time — **do not** hand-edit version strings in `.app.src` files, `build_stdlib.rs`, or `editors/liveview/mix.exs`.

How the version flows at build time:

- **Rust:** `build.rs` reads `VERSION` + git state → `BEAMTALK_VERSION` compile-time env var.
- **Erlang:** `.app.src` files use `{vsn, {cmd, "escript ../../../scripts/version.escript"}}`.
- **LiveView IDE (Elixir):** `editors/liveview/mix.exs` reads `VERSION` at compile time, so the `bt_attach` release/Docker tag tracks the toolchain version (BT-2514). It ships on its own release lane (BT-2512), not the core bundle.
- **Build identity (BT-4812):** the same `build.rs` also emits `BEAMTALK_BUILD_ID`, the display string behind `--version`, the REPL banner, and the LSP/MCP server info. A tagged release prints `0.4.0`; edge and nightly builds print `0.4.0-edge+<sha> (<date>)` / `0.4.0-nightly+<sha> (<date>)` (`edge.yml` and `nightly.yml` set `BEAMTALK_BUILD_CHANNEL`); a local checkout prints `0.4.0-dev+<sha> (<date>)`, with `.dirty` after the sha when tracked files have uncommitted changes; a build with no git metadata prints the plain version. `BEAMTALK_BUILD_COMMIT` supplies the commit when there is no `.git`. It is display-only: compare `BEAMTALK_VERSION`, never `BEAMTALK_BUILD_ID`.
- **Dev builds** (not on a git tag): version becomes `0.3.0-dev+<short sha>`.
- **Release builds** (on a git tag): version is the clean `0.3.0`.

The **Rust toolchain** is pinned in `rust-toolchain.toml`; update it there when upgrading Rust.

## Cutting a release

1. Update `VERSION` with the new version number.
2. Update `Cargo.toml` workspace `version` to match (required by Cargo). Then refresh the committed lockfiles that pin the workspace crate versions: run `cargo update --workspace` in the repo root and `cargo update --workspace --manifest-path fuzz/Cargo.toml`, and commit `Cargo.lock` and `fuzz/Cargo.lock` (and `desktop/src-tauri/Cargo.lock` if it pins them). CI's `cargo tree --locked` checks fail on a stale lockfile (BT-5114).
3. Update `CHANGELOG.md` — add a new section following the existing format (Language, Standard Library, Runtime, Tooling, Internal subsections as needed).
4. Update version references in docs: `README.md`, `docs/beamtalk-tooling.md`, `docs/beamtalk-language-features.md`, `docs/repl-protocol.md`, `docs/beamtalk-ddd-model.md`.
5. Tag the release commit: `git tag v<version>`.

## uat dispatch payload (ADR 0154 § 3)

`edge.yml`, `nightly.yml` and `release.yml` each send a `repository_dispatch` to
`jamesc/beamtalk-uat` with `event-type: beamtalk-artifact`, authenticated with the
`UAT_DISPATCH_TOKEN` secret. The payload is built by
`scripts/ci/uat-dispatch-payload.sh` from the assets attached to the GitHub release:

```json
{ "channel": "edge|nightly|release", "version": "0.6.0-edge+c6b28b3",
  "tag": "edge|nightly|v0.6.0", "sha": "<commit>",
  "otp": { "min-major": 28, "max-major": 29 },
  "assets": { "beamtalk-…-linux-x86_64.tar.gz": "<sha256>", "…": "…" } }
```

| Field | Source |
|-------|--------|
| `channel` | the workflow: `edge`, `nightly` or `release` |
| `version` | `<VERSION>-edge+<short sha>` / `<VERSION>-nightly+<short sha>`; for a release the tag without `v` |
| `tag` | `edge`, `nightly`, or the release tag |
| `sha` | the commit the artifacts were built from (`GITHUB_SHA`) |
| `otp` | `min-major` / `max-major` read from `otp-support.toml` |
| `assets` | asset name → sha256 for every asset on the release except `.sha256` sidecars; for a tag this includes the LiveView IDE archives and the `.vsix` |

- **Timing.** `edge` and `nightly` dispatch after the rolling release is updated, and only from `main`. `release` dispatches in `notify-uat-artifact`, which waits for the VS Code and LiveView IDE lanes so the asset map is complete.
- **Runner images.** `release.yml`, `edge.yml`, `nightly.yml` and `liveview-release.yml` pin `ubuntu-24.04` and `macos-15` (not `*-latest`), because uat tests each target on the image that built it. Bumping an image is a reviewed change on both sides.
- **Legacy event.** `release.yml` still sends `beamtalk-release` (`version` and `tag` only) until uat accepts both types; a follow-up removes it.
- The payload is tested by `scripts/ci/test-uat-dispatch-payload.sh`.

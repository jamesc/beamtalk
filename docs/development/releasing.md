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
5. Tag the release commit and push the tag: `git tag v<version> && git push origin v<version>`. Do not create the GitHub release by hand: the tag push does it (see below).

## Release candidates: build-and-attach, then publish (ADR 0154 § 10)

A `v*` tag push runs `release.yml`, which **builds and attaches** but does not publish:

1. `prepare` creates the GitHub release for the tag as a **pre-release** (never `latest`). An existing pre-release is reused.
2. Every target is built from the tagged commit and attached to it: the four platform bundles and their `.sha256` files, the `.vsix` (`release-vscode.yml`), the LiveView IDE archives (`liveview-release.yml`) and the desktop installers (`desktop-release.yml`).
3. The IDE image is pushed as `ghcr.io/jamesc/beamtalk-ide:rc-<full sha>` (`liveview-docker.yml`). Its digest is recorded in the job summary and sent as `ide_image`.
4. `notify-uat` dispatches the payload below to beamtalk-uat.

Each channel workflow has two entry points, as inputs:

| Workflow | Build-and-attach (tag flow) | Publish (promotion) |
|----------|-----------------------------|---------------------|
| `release-vscode.yml` | `attach_to_release` + `tag` | `publish` + `tag`: publishes the `.vsix` attached to the release to the Marketplace, without rebuilding |
| `liveview-release.yml` | `attach` + `tag` | `publish` + `tag`: re-hashes the attached `beamtalk-ide-*` archives against their sidecars |
| `liveview-docker.yml` | `push_rc` (output `image`, pinned by digest) | `publish` + `image` + `version`: retags that digest as `<version>` and `latest`, without rebuilding |
| `desktop-release.yml` | `attach` + `tag` | `publish` + `tag`: re-hashes the attached `beamtalk-desktop-*` installers against their sidecars |

The archives and installers have no channel of their own: users get them when the release is marked latest.

**Who may publish.** Every publish half first runs `scripts/ci/publish-guard.sh`, which reads the repository variable `RELEASE_PUBLISH_MODE`:

- unset or `legacy` (today): any caller may publish. On a tag push, `release.yml`'s `interim-*` jobs publish every channel and mark the release latest once the four platform bundles are attached. This keeps the pre-BT-5313 behaviour until `promote.yml` exists.
- `promote`: only the `promote.yml` workflow on `main` may publish, and the `interim-*` jobs are skipped. `promote.yml` (BT-5315) runs the publish halves and marks the release latest after uat passes. Set the variable when it lands.

**Dry run.** `release.yml`'s `workflow_dispatch` takes an existing `tag` and runs the tag flow for that tag's commit, but never publishes, never moves `latest` and never dispatches uat; it still builds the uat payload and shows it in the job summary. It refuses a release that is no longer a pre-release. To test the workflow, push a test tag that does not start with `v` (so its push does not start the real flow), e.g. `0.0.0-test.1`, and dispatch with `discard: true`: afterwards it records the pre-release's assets in the summary, then deletes the pre-release, the tag and the `rc-<sha>` image.

## uat dispatch payload (ADR 0154 § 3)

`edge.yml`, `nightly.yml` and `release.yml` each send a `repository_dispatch` to
`jamesc/beamtalk-uat` with `event-type: beamtalk-artifact`, authenticated with the
`UAT_DISPATCH_TOKEN` secret. The payload is built by
`scripts/ci/uat-dispatch-payload.sh` from the assets attached to the GitHub release:

```json
{ "channel": "edge|nightly|release", "version": "0.6.0-edge+c6b28b3",
  "tag": "edge|nightly|v0.6.0", "sha": "<commit>",
  "otp": { "min-major": 28, "max-major": 29 },
  "assets": { "beamtalk-…-linux-x86_64.tar.gz": "<sha256>", "…": "…" },
  "ide_image": "ghcr.io/jamesc/beamtalk-ide@sha256:<digest>" }
```

| Field | Source |
|-------|--------|
| `channel` | the workflow: `edge`, `nightly` or `release` |
| `version` | `<VERSION>-edge+<short sha>` / `<VERSION>-nightly+<short sha>`; for a release the tag without `v` |
| `tag` | `edge`, `nightly`, or the release tag |
| `sha` | the commit the artifacts were built from (`GITHUB_SHA`) |
| `otp` | `min-major` / `max-major` read from `otp-support.toml` |
| `assets` | asset name → sha256 for every asset on the release except `.sha256` sidecars; for a tag this includes the LiveView IDE archives, the desktop installers and the `.vsix` |
| `ide_image` | tags only: the `rc-<sha>` IDE image pinned by digest (`IDE_IMAGE`); absent if that image was not pushed |

- **Timing.** `edge` and `nightly` dispatch after the rolling release is updated, and only from `main`. `release` dispatches from `notify-uat` on the tag push (not on `release: published`), after the VS Code, LiveView IDE, IDE image and desktop lanes, so the asset map is complete. It is gated on the four platform builds only: a failed lane contributes no assets but does not stop the dispatch. Each dispatch is its own job (`notify-uat`) so it can be re-run without rebuilding.
- **Runner images.** `release.yml`, `edge.yml`, `nightly.yml` and `liveview-release.yml` pin `ubuntu-24.04` and `macos-15` (not `*-latest`), because uat tests each target on the image that built it. Bumping an image is a reviewed change on both sides.
- **Legacy event.** `release.yml` no longer sends `beamtalk-release`: uat accepts `beamtalk-artifact` (BT-5303), and ADR 0154 § 3 drops the old type when `release.yml` switches to the pre-release flow (BT-5313).
- The payload is tested by `scripts/ci/test-uat-dispatch-payload.sh`; the publish guard and asset check by `scripts/ci/test-publish-guard.sh`.

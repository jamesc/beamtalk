# ADR 0030: cargo-dist Evaluation for Release Packaging

## Status

**Rejected** — cargo-dist is not viable for beamtalk's release packaging needs at this time.

## Context

### Problem

Beamtalk has a hand-written 566-line `release.yml` GitHub Actions workflow that builds platform-specific distribution tarballs/zips for 4 platforms (Linux x86_64, macOS x86_64, macOS ARM64, Windows x86_64) and bundles Erlang BEAM runtime files. This works but is custom CI to maintain.

[cargo-dist](https://github.com/axodotdev/cargo-dist) (v0.30.4, now branded as "dist") automates release packaging for Rust projects — generating CI workflows, building platform binaries, creating installers (shell scripts, Homebrew taps), checksums, and uploading to GitHub Releases.

### Current Distribution Layout

Beamtalk's distribution requires a specific layout that bundles 4 Rust binaries alongside 8 Erlang OTP applications:

```
dist/
  bin/
    beamtalk
    beamtalk-compiler-port
    beamtalk-lsp
    beamtalk-mcp
  lib/beamtalk/lib/
    beamtalk_runtime/ebin/*.beam, *.app
    beamtalk_workspace/ebin/*.beam, *.app
    beamtalk_compiler/ebin/*.beam, *.app
    beamtalk_stdlib/ebin/*.beam, *.app
    jsx/ebin/*.beam, *.app
    cowboy/ebin/*.beam, *.app
    cowlib/ebin/*.beam, *.app
    ranch/ebin/*.beam, *.app
```

The `lib/` directory is required at runtime — the CLI locates it relative to the binary location to load BEAM code paths.

### Current Workflow Structure

The existing `release.yml` has 7 jobs:
- 4 platform build jobs (Linux x86_64, macOS x86_64, macOS ARM64, Windows x86_64)
- 3 VS Code extension jobs (build-lsp, package-vscode, publish-vscode)

Each platform job installs Erlang/OTP 27.0 via `erlef/setup-beam@v1`, runs `rebar3 compile`, builds stdlib, and creates the full distribution.

## Evaluation

### What cargo-dist Handles Well

| Capability | Assessment |
|------------|------------|
| Multi-platform Rust binary builds | ✅ Excellent — native support for all 4 target platforms |
| GitHub Release creation | ✅ Automatic with changelog integration |
| Checksums (SHA-256) | ✅ Built-in, which our current workflow lacks |
| Shell/PowerShell install scripts | ✅ `curl \| sh` style installers |
| Homebrew tap generation | ✅ Built-in, which we don't have today |
| CI workflow generation | ✅ Generated 296-line workflow vs our 566-line hand-written one |
| Tag-based release trigger | ✅ Standard semver tag detection |

### What cargo-dist Cannot Handle

#### 1. Custom Directory Layout (Blocker) 🔴

cargo-dist archives contain **only** binaries in a flat structure. There is no supported mechanism to include a `lib/` directory tree alongside binaries in the release archive.

- The `include` config option copies files to the **root** of the archive, not into a subdirectory structure
- `extra-artifacts` are uploaded as **separate** release assets, not bundled inside the platform archive
- The `package-libraries` feature only handles Rust cdylib/cstaticlib, not arbitrary directory trees
- [Issue #934](https://github.com/axodotdev/cargo-dist/issues/934) tracks normalizing install directory structure to support `lib/` alongside `bin/`, but this is not yet implemented

Beamtalk requires `lib/beamtalk/lib/*/ebin/*.beam` to be co-located with binaries in a specific directory hierarchy. Without this, the CLI cannot find BEAM code paths at runtime.

#### 2. Erlang/OTP Build Dependency (Significant) 🟡

cargo-dist's `dependencies` system supports Apt, Chocolatey, and Homebrew for installing system packages. However:

- OTP 27.0 is **not available** in standard package manager repositories (Ubuntu Apt ships much older versions)
- The current workflow uses `erlef/setup-beam@v1` GitHub Action, which is purpose-built for Erlang CI
- `github-build-setup` can inject custom steps, but this couples us to GitHub Actions-specific YAML fragments and reduces the benefit of cargo-dist's abstraction

**Workaround exists** via `github-build-setup` to inject `erlef/setup-beam` before the build, but this is fragile and underdocumented for complex build chains.

#### 3. Multi-Step Build Process (Significant) 🟡

Beamtalk's build requires a specific sequence:
1. `cargo build --release` (Rust binaries)
2. `rebar3 compile` (Erlang runtime apps)
3. `beamtalk build-stdlib` (uses the just-built Rust binary to compile stdlib .bt → .beam)

cargo-dist's `build-command` replaces the standard Cargo build entirely (for non-Rust projects). For Rust projects with additional build steps, `extra-artifacts` provides a hook, but it runs **after** the Cargo build and its artifacts are uploaded **separately** — not merged into the platform archive.

There is no way to say "after Cargo builds, also run rebar3 and bundle the results into the same archive."

#### 4. VS Code Extension Pipeline (Not Supported) 🟡

The existing workflow includes 3 jobs for cross-compiling the LSP server, packaging platform-specific VS Code extensions (.vsix), and publishing to the marketplace. cargo-dist has no concept of VS Code extensions. These jobs would need to remain as custom CI regardless.

#### 5. Windows Build Complexity (Minor) 🟢

The existing Windows build has special handling (explicit bash shell, manual artifact collection, 7z packaging). cargo-dist handles Windows builds natively, but the Erlang bundling issues above apply equally on Windows.

### Generated vs Existing Workflow Comparison

| Aspect | cargo-dist Generated | Existing release.yml |
|--------|---------------------|---------------------|
| Lines | 296 | 566 |
| Jobs | 4 (plan, build-local, build-global, host) | 7 (4 platform + 3 VS Code) |
| Trigger | Tag push (`*.*.*`) | Release published / workflow_dispatch |
| Erlang support | ❌ None | ✅ OTP 27.0 via setup-beam |
| BEAM bundling | ❌ No mechanism | ✅ Full lib/ tree |
| Install scripts | ✅ shell + powershell | ❌ None |
| Checksums | ✅ SHA-256 | ❌ None |
| Homebrew | ✅ Available | ❌ None |
| VS Code ext | ❌ Not supported | ✅ Full pipeline |
| Archive format | tar.xz (unix), zip (win) | tar.gz (unix), zip (win) |

### Gains from Adopting cargo-dist

1. **Checksums** — SHA-256 for all archives (we lack this today)
2. **Install scripts** — `curl | sh` installer (we lack this today)
3. **Homebrew tap** — automated formula generation
4. **Less CI maintenance** — workflow regenerated on `dist init`
5. **Standardized release flow** — tag-based, with prerelease support

### Losses from Adopting cargo-dist

1. **Cannot bundle BEAM files in archives** — fundamental blocker
2. **VS Code extension pipeline** — must remain custom
3. **Erlang build steps** — must be injected via workarounds
4. **Release trigger flexibility** — loses workflow_dispatch trigger
5. **Archive format control** — defaults to tar.xz (minor)

## Decision

**Do not adopt cargo-dist** for beamtalk's release packaging at this time.

The fundamental blocker is that cargo-dist cannot include arbitrary directory trees (our `lib/beamtalk/lib/*/ebin/` structure) inside release archives. This is a core requirement for beamtalk — without bundled BEAM files, the distributed binary cannot function.

The gains (checksums, install scripts, Homebrew) are valuable but can be added to the existing workflow incrementally without cargo-dist.

## Alternatives Evaluated

### 1. GoReleaser (Most Promising) ⭐

[GoReleaser](https://goreleaser.com/) added Rust support in v2.5 via `cargo-zigbuild`. It's the most feature-rich option and **can handle beamtalk's requirements**.

**Why it could work:**
- **Custom archive layout**: `files` entries support `src`/`dst` mapping — can place BEAM files at `lib/beamtalk/lib/*/ebin/` inside the archive
- **Global `before` hooks**: Run `rebar3 compile` and `beamtalk build-stdlib` before archiving
- **Build hooks**: Pre/post build hooks for Erlang setup
- **Recursive directory inclusion**: Adding a directory path includes its full tree
- **Built-in features**: Checksums, Homebrew taps, shell install scripts, changelogs, Docker images, SBOMs

**Example `.goreleaser.yaml` sketch:**
```yaml
before:
  hooks:
    - rebar3 compile
    - ./target/release/beamtalk build-stdlib

builds:
  - builder: rust
    binary: beamtalk
    targets:
      - x86_64-unknown-linux-gnu
      - x86_64-apple-darwin
      - aarch64-apple-darwin
      - x86_64-pc-windows-msvc

archives:
  - format: tar.gz
    files:
      - src: runtime/_build/default/lib/beamtalk_runtime/ebin
        dst: lib/beamtalk/lib/beamtalk_runtime/ebin
      - src: runtime/_build/default/lib/beamtalk_workspace/ebin
        dst: lib/beamtalk/lib/beamtalk_workspace/ebin
      # ... remaining apps
      - src: runtime/apps/beamtalk_stdlib/ebin
        dst: lib/beamtalk/lib/beamtalk_stdlib/ebin
```

**Concerns:**
- Rust support is relatively new (v2.5, ~2024) — may have rough edges
- Some `src`/`dst` features may require GoReleaser Pro ($) for templated paths
- GoReleaser is a Go tool — adds a non-Rust dependency to CI
- Multiple binaries from a Cargo workspace need careful config (one `builds` entry per binary, or workspace-aware setup)
- Erlang/OTP 27 still needs GitHub Actions setup step (not handled by GoReleaser itself)
- VS Code extension pipeline would remain custom regardless

**Verdict:** Worth a deeper spike. If `src`/`dst` mapping works in the free version for directory trees, this could replace the existing workflow. Create a follow-up issue for a hands-on evaluation.

### 2. nFPM (Linux Package Formats Only)

[nFPM](https://nfpm.goreleaser.com/) (from GoReleaser team) creates `.deb`, `.rpm`, `.apk`, and tarballs with full control over directory layout via `contents` entries with `src`/`dst` and `type: tree`.

**Why it's interesting:**
- Perfect `src`/`dst` control with recursive directory support
- Can produce proper Linux packages (`.deb`, `.rpm`) in addition to tarballs
- Lightweight, single-purpose tool

**Why it's insufficient alone:**
- No macOS or Windows support (Linux package formats only)
- No CI workflow generation, checksums, install scripts, or GitHub Release integration
- Would need to be combined with other tools for a complete solution

**Verdict:** Useful as a complement for Linux package distribution, not a standalone replacement.

### 3. Earthly CI (Build Orchestration)

[Earthly](https://earthly.dev/) provides Dockerfile-meets-Makefile build definitions with cross-platform support. Good for multi-language projects (Rust + Erlang).

**Why it's interesting:**
- Reproducible, containerized builds
- Natural fit for Rust + Erlang multi-language builds
- Cross-platform via QEMU/buildx

**Why it doesn't fit:**
- Build orchestration tool, not a release packager — still need packaging logic
- Adds significant infrastructure complexity (Docker, QEMU)
- Doesn't provide installers, checksums, Homebrew, or GitHub Release integration
- Overkill for our needs — we already have working CI, we need better packaging

**Verdict:** Not the right tool for this problem. Solves build reproducibility, not release packaging.

### 4. Nix Flakes (Reproducible Builds)

Nix can build both Rust (`buildRustPackage`) and Erlang (`beamPackages`) with full reproducibility, including ERTS bundling.

**Why it's interesting:**
- Hermetic, reproducible builds for both Rust and BEAM
- Can produce self-contained release bundles
- Natural multi-language support

**Why it doesn't fit:**
- Steep learning curve, Nix-specific tooling
- Cross-compilation support is complex
- No GitHub Release integration, install scripts, or Homebrew generation
- Requires Nix on CI runners (additional setup)
- Better suited for deployment than distribution packaging

**Verdict:** Would be great for internal reproducibility but wrong abstraction for end-user distribution.

### 5. Incremental Improvements to Existing Workflow (Recommended for Now) ⭐

Rather than replacing the workflow, add the missing features identified in the cargo-dist evaluation:

| Feature | Approach | Effort |
|---------|----------|--------|
| **SHA-256 checksums** | Add `sha256sum` step after packaging, upload `.sha256` files | Small (~10 lines) |
| **Install script** | Create `scripts/install.sh` that detects OS/arch, downloads correct archive from GitHub Releases | Medium (~100 lines) |
| **Homebrew tap** | Create `jamesc/homebrew-beamtalk` repo, auto-update formula on release | Medium (new repo + ~30 lines of workflow) |

This approach:
- Preserves the working, well-understood workflow
- Adds the three most valuable features cargo-dist would have provided
- Has zero risk of breaking existing release process
- Can be done incrementally (one feature per PR)

## Recommended Follow-Up Actions

1. **Add checksums** to existing `release.yml` — generate SHA-256 for each archive and upload alongside (small change)
2. **Add install script** — create a standalone `install.sh` that downloads the correct platform archive from GitHub Releases
3. **Evaluate GoReleaser** — the most promising full replacement; create a spike issue to test `src`/`dst` archive mapping with beamtalk's `lib/` tree using the free version
4. **Consider Homebrew formula** — can be maintained independently as a tap repository
5. **Monitor cargo-dist** — [Issue #934](https://github.com/axodotdev/cargo-dist/issues/934) tracks `lib/` directory support; re-evaluate when this lands

## Consequences

- The existing `release.yml` workflow remains the source of truth for releases
- No `dist-workspace.toml` or `[profile.dist]` configuration is added
- Future improvements to the release pipeline should be made incrementally to the existing workflow

## References

- [cargo-dist documentation](https://axodotdev.github.io/cargo-dist/)
- [cargo-dist custom builds](https://axodotdev.github.io/cargo-dist/book/custom-builds.html)
- [cargo-dist CI customization](https://axodotdev.github.io/cargo-dist/book/ci/customizing.html)
- [cargo-dist config reference](https://axodotdev.github.io/cargo-dist/book/reference/config.html)
- [Issue #934: normalize install dir structure](https://github.com/axodotdev/cargo-dist/issues/934)
- Current workflow: `.github/workflows/release.yml` (566 lines, 7 jobs)

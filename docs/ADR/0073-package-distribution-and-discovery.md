# ADR 0073: Package Distribution and Discovery

## Status
Implemented (Phase 1, 2026-03-31). Phase 2 implemented in simplified form, 2026-07-26 — the design that shipped differs from the Phase 2 design this ADR originally specified; see "Amendment (2026-07-26)" below. The original Phase 2 design (hex-compatible static registry, PubGrub) is retained in the Decision section for reference but is **not** what was built — treat it as a possible future direction, not the current plan.

## Amendment (2026-07-26): Phase 2 Shipped in Simplified Form

The library registry epic (BT-2977) shipped a working package registry — BT-2978 (registry-dependency resolution), BT-2979 (`deps add`/`deps list`/`deps update` registry support), BT-2980 (`beamtalk version`/`beamtalk publish`), BT-2981 (user docs). It does not follow the Phase 2 design specified below. Found and recorded via BT-2995.

### What shipped vs. what this ADR specified

| This ADR specified (Phase 2, original) | What shipped |
| --- | --- |
| `[repos]` top-level section, shared with native hex deps (explicitly *not* a per-project registry URL) | `[registry] url` — a single URL, Beamtalk-package-only; native hex deps under `[native.dependencies]` are untouched by it |
| Hex-compatible static registry: protobuf index (`names`, `versions`, `packages/<name>`), signed, serving tarballs | A directory of TOML files (`packages/<name>.toml`) in a plain git repo (or a local directory for CI/air-gapped use) — no tarballs, no signing, no protobuf |
| Version ranges (`http = "~> 0.1"`) resolved by a **PubGrub** solver (`pubgrub` crate) | Exact `major.minor.patch` only (`yaml = "0.2.1"`) — no ranges, no solver of any kind |
| Distinct lockfile entry kind: `source = "registry"`, `registry = "beamtalk"`, `version`, `checksum = "sha256:..."` | The existing git `[[package]]` entry (`url`, `reference`, `sha` — the *commit* SHA) gains one optional `version` field recording the requested registry version; there is no separate entry kind and no package-content checksum |
| `beamtalk package build --tarball`, `beamtalk package registry build`, `beamtalk package retire` | `beamtalk publish` — tags the package's own repo and appends a `[[versions]]` block to the index; no tarball is ever built, nothing is retired/yanked |

A registry dependency resolves through the index into a `(git url, tag)` pair that then flows through the **existing git-dependency machinery unchanged** — cloned into `_build/deps/`, pinned to a commit SHA in `beamtalk.lock`, exactly like a hand-written `{ git = ..., tag = ... }` entry. The registry adds *indexing* (name/version → repo/tag) in front of Phase 1's git deps; it does not add a second distribution mechanism alongside them. See `crates/beamtalk-cli/src/commands/deps/registry.rs` (index resolution and lookup) and `crates/beamtalk-cli/src/commands/publish.rs` (`beamtalk publish`) for the implementation, and [`docs/beamtalk-packages.md` § Package Registry](../beamtalk-packages.md#package-registry) for the user-facing design and index format.

### Why the simpler design

- **The ecosystem still has ~1-2 packages and one publisher.** A PubGrub solver, protobuf index, and tarball signing pipeline solve problems (constraint conflicts across many independently-versioned packages, tamper-evident distribution at scale) that do not exist yet at this scale — the same "infrastructure proportional to actual need" reasoning this ADR already applies to the Phase 1 vs. Phase 2 boundary, applied one step further down.
- **Zero new distribution mechanism.** Phase 1 (git deps) already does the hard part — clone, pin, compile. The registry is *only* a lookup table (`(name, version) -> (git url, tag)`); it reuses the git-clone path Phase 1 built rather than introducing tarballs, a second fetch code path, or a second thing to keep working. This is a smaller step than "Phase 1 → Phase 2" as originally specified, not an implementation of that step.
- **No signing keypair or protobuf tooling to build or operate.** The original Phase 2 estimated ~600–1000 LOC of Rust (tarball creation, protobuf registry generation, RSA signing) plus registry hosting infrastructure. None of it is needed to answer "where do I find package X" with a git repo of TOML files.

### What this defers or leaves open

- **No integrity checksum on the fetched content.** The lockfile pins a git commit SHA, but nothing yet *enforces* that a fresh clone of a dependency actually lands on that recorded SHA before it is compiled and used — see BT-2992. The original Phase 2 design's `checksum = "sha256:..."` on a hex tarball would have closed this gap structurally (the tarball itself is hashed and verified); the shipped design has no equivalent yet. This is the most concrete cost of the simpler design and the most likely near-term follow-up.
- **Whether version ranges are ever supported is now an open question, not a settled plan.** The original Phase 2 committed to `~> 0.1`-style ranges via PubGrub. Nothing in the shipped design blocks introducing that later — a version-range string is a natural extension of the existing `name = "X.Y.Z"` manifest position — but it is speculative future work, not a phase already decided on. Track separately if/when it becomes worth doing.
- **Migration path to a hex-compatible registry: not dropped, but not planned.** The rest of "Phase 2: Hex-Compatible Registry" below (protobuf index, signing, tarballs, PubGrub) remains a *possible* future direction if the ecosystem grows enough to need version-range solving or tamper-evident distribution (many packages, external maintainers, native hex-dep version conflicts becoming frequent — see the "Native dep version conflicts" paragraph below, which is unaffected by this amendment and still describes the current `[native.dependencies]`/rebar3 behaviour). The migration would be additive, not a rewrite: the current TOML-index format already tolerates unknown keys, so a `checksum` or `yanked` field could be added to `packages/<name>.toml` without breaking older `beamtalk` binaries reading it, and a PubGrub-based resolver could be introduced behind the same `name = "X.Y.Z"` manifest syntax by generalising it to accept ranges. What is genuinely open is whether that migration is ever worth doing, not whether it is possible.
- **`beamtalk package retire`/`unretire` and native-registry sharing (`[repos]`) were not built.** A published version cannot currently be marked bad; the only recourse is publishing a new version and telling consumers not to use the old one out of band.

### Relationship to ADR 0070's deferred alternative

[ADR 0070](0070-package-namespaces-and-dependencies.md) deferred (not rejected) an "Alternative: Registry-Based Dependencies (Hex.pm)" — `json = "1.0"` resolving through hex.pm — pending "ecosystem maturity: published packages, version policies, a publishing workflow." What shipped here is a minimal instance of that alternative: it provides published packages (via `beamtalk publish`) and a publishing workflow, using the same `name = "X.Y.Z"` manifest syntax ADR 0070 sketched. It does **not** provide "version policies" in the sense ADR 0070 meant — there is no semver-range resolution, and the registry is self-hosted rather than hex.pm. ADR 0070's deferral is therefore only partially resolved: registry-based dependencies now exist, but the version-policy and hex.pm-hosting pieces it deferred remain deferred, tracked as open questions above rather than as a committed next phase.

## Context

Beamtalk's package infrastructure has matured through several ADRs:

- **ADR 0026** defines `beamtalk.toml` as the package manifest
- **ADR 0070** establishes dependency resolution (path and git sources), lockfiles, and the `beamtalk deps` CLI
- **ADR 0072** adds native Erlang sources and hex dependencies to packages, with rebar3 as the compilation backend

Today, Beamtalk packages can be authored, built, and consumed via **path** and **git** dependencies. The question is: what comes next for package distribution, and when?

Additionally, the tooling layer (MCP, LSP) needs a way to **discover API metadata** (classes, methods, documentation) from installed packages — not just from stdlib's bundled `corpus.json`. As packages are extracted from stdlib (starting with HTTP, per ADR 0072), MCP loses visibility into their APIs unless a discovery mechanism exists.

### What Needs Deciding

1. **Distribution strategy** — how packages move from monorepo path deps to standalone distributable packages, and the migration path toward a registry
2. **Config schema** — how users declare registry sources when the time comes
3. **API metadata for tooling** — how MCP/LSP discover class and method information from installed packages
4. **CLI surface** — packaging and eventual publish commands

### Constraints

- **Hex compatibility for native deps is non-negotiable.** Packages with `[native.dependencies]` already use rebar3 + hex.pm. The native dep pipeline must continue to work.
- **Beamtalk packages are not Erlang packages.** A Beamtalk package contains `.bt` source, native Erlang, and Beamtalk-specific metadata (class kinds, protocols). Hex's tarball format can carry this, but the metadata is richer than what hex natively understands.
- **BEAM ships source.** Convention across Erlang, Elixir, and Gleam: packages include source code, not just compiled artifacts. Consumers compile locally.
- **One-person project, pre-public.** The ecosystem has ~1 extractable package (HTTP). Infrastructure investment must be proportional to actual need.

## Decision

### 1. Three-Phase Distribution Strategy

Package distribution evolves through three phases, each triggered by actual need rather than speculative infrastructure:

#### Phase 1: Git Repositories with Tagged Releases (Now)

Packages extracted from the monorepo become standalone git repositories (e.g., `packages/http` → `jamesc/beamtalk-http`). Consumers use **git deps** — already supported by ADR 0070:

```toml
[dependencies]
http = { git = "https://github.com/jamesc/beamtalk-http", tag = "v0.1.0" }
```

GitHub Releases provide a landing page for each version with release notes. The git tag is what matters for resolution — the lockfile pins the exact commit SHA.

**This requires zero new infrastructure.** Git deps, lockfiles, and implicit fetch-on-build all work today. The only work is extracting `packages/http` into its own repo and tagging releases.

**What Phase 1 gives us:**
- Validates that packages are self-contained (no hidden monorepo dependencies)
- Tests the git fetch → compile → code path pipeline for standalone packages
- Versioned releases with immutable tags
- Works for any number of first-party packages

**What Phase 1 does not give us:**
- Version constraint solving (consumers pin exact versions)
- Package discoverability (you need to know the git URL)
- A package index for tooling
- Testing of the Phase 2 registry pipeline (tarball creation, integrity verification, signing, constraint solving are entirely different code paths)

**What Phase 1 costs:**
- CI setup in each extracted repo (must fetch/build the Beamtalk compiler to run tests)
- Compiler regressions that break extracted packages are not caught by the main repo's CI — requires cross-repo CI or periodic integration testing
- Test migration: BUnit/e2e tests that exercise extracted classes must move to the new repo or the main repo must add a git dep on the extracted package

#### Phase 2: Hex-Compatible Registry (When External Users Arrive) — original design, not what shipped

> **This section describes the original Phase 2 plan as first written. A simpler registry actually shipped in 2026-07 (BT-2977–2981) — see "Amendment (2026-07-26)" above for what was built and why. The design below is retained as a possible future direction, not the current plan.**

When the ecosystem has external contributors or enough packages that version constraint solving matters, add a **hex-compatible static registry**. This is a set of static files (protobuf index + tarballs) served from any HTTP endpoint — S3, GitHub Pages, or any static host.

**Tarball format** — hex-compatible, following the [hex tarball specification](https://github.com/hexpm/specifications/blob/main/package_tarball.md):

```text
{name}-{version}.tar
  VERSION              # "3" (hex tarball format version)
  metadata.config      # Erlang term file: package metadata
  CHECKSUM             # SHA-256 of contents
  contents.tar.gz
    beamtalk.toml                    # Package manifest
    src/*.bt                         # Beamtalk source files
    native/*.erl                     # Erlang FFI source files (if any)
    native/include/*.hrl             # Erlang headers (if any)
```

Beamtalk-specific files (`.bt` sources) live inside `contents.tar.gz`, which the hex tarball spec treats as opaque. Gleam uses the same approach to ship `.gleam` source in hex tarballs.

The `metadata.config` follows the hex tarball specification:

```erlang
{<<"name">>, <<"http">>}.
{<<"version">>, <<"0.1.0">>}.
{<<"description">>, <<"HTTP client and server for Beamtalk">>}.
{<<"app">>, <<"beamtalk_http">>}.
{<<"build_tools">>, [<<"beamtalk">>]}.
{<<"requirements">>, #{
    <<"gun">> => #{
        <<"requirement">> => <<"~> 2.1">>,
        <<"optional">> => false,
        <<"app">> => <<"gun">>
    }
}}.
```

Compiled `.beam` files are **not included** — consumers compile from source (BEAM convention).

**Registry structure:**

```text
registry/
  public_key                 # Signing verification key
  names                      # protobuf: package name list
  versions                   # protobuf: version metadata
  packages/<name>            # protobuf: per-package release info
  tarballs/<name>-<version>.tar
```

**Consumer config** — registry-based dependencies use a version string:

```toml
[dependencies]
http = "~> 0.1"

[repos.beamtalk]
url = "https://hex.beamtalk.dev"
public_key_path = "keys/beamtalk-hex.pub"
```

**Immutability policy:** Once a version is published, its tarball must not be modified or replaced. To address a bad release, use the retirement mechanism — never replace a published tarball.

**Package retirement:** `beamtalk package retire http 0.1.0 --reason security --message "Use 0.1.1"` marks a version as retired without removing the tarball. Retired versions are excluded from resolution but remain downloadable for existing lockfiles.

**Tarball and registry tooling — pure Rust.** Following Gleam's approach (which builds hex tarballs in ~200 lines of Rust in `compiler-cli/src/publish.rs`):

- **Tarball creation**: `tar` + `flate2` crates for tarball/gzip, `sha2` for CHECKSUM, `format!()` templates for `metadata.config` (~200 LOC)
- **Registry index generation**: `prost` for protobuf encoding, `rsa`/`ring` for RSA-SHA512 signing (~300-400 LOC)
- No Elixir, Mix, or Erlang toolchain dependency for publishing

```bash
beamtalk package build
# => Created _build/http-0.1.0.tar

beamtalk package registry build registry/ --name=beamtalk --private-key=key.pem
```

**Version constraint solving — PubGrub (Phase 2+).** Registry deps will require a constraint solver. Beamtalk will use the **PubGrub** algorithm via the `pubgrub` Rust crate — the same algorithm used by Gleam (Rust), Dart/pub, and Elixir/Mix (via `hex_solver`). Gleam's `compiler-core/src/dependency.rs` is the reference implementation. Implementation details deferred to the Phase 2 implementation issue.

**Native dep version conflicts.** `[native.dependencies]` are resolved by rebar3, which uses depth-first "first wins" — not PubGrub. If two Beamtalk packages declare conflicting hex dep versions (e.g., `gun = "~> 2.1"` vs `gun = "~> 2.0"`), rebar3 may silently pick one. The Beamtalk resolver has no visibility into rebar3's choices. Mitigation: `beamtalk build` should compare declared native dep versions across packages and warn on potential conflicts before invoking rebar3.

#### Phase 3: Public hex.pm (When APIs Stabilise)

When package APIs are stable enough to commit to semantic versioning, publish to hex.pm directly. hex.pm has no gatekeeping — Gleam, LFE, Clojerl, and Efene all publish there. Gleam started at v0.2 as a one-person project.

```toml
[dependencies]
http = "~> 0.1"              # resolves from hex.pm (default registry)
```

**This transition is not just a URL change.** It requires:
- A hex.pm account and organisation
- Verification that hex.pm accepts `build_tools = ["beamtalk"]` (Gleam precedent makes this low-risk)
- `beamtalk package publish` command with hex.pm API integration (via `hexpm` Rust crate, as Gleam does)
- The static registry may continue operating alongside hex.pm for private/internal packages

The tarball format is the same as Phase 2 — what changes is the resolution source and publishing mechanism.

**Why not hex.pm from the start?** hex.pm versions are immutable. Once you publish `http 0.1.0`, that version number is burned forever. With APIs still changing significantly between releases, we'd either burn through version numbers rapidly or publish packages that mislead consumers about stability. Phase 1 (git tags) and Phase 2 (static registry) let us iterate freely. Move to hex.pm once the package APIs are stable enough to commit to semantic versioning.

### 2. Config Schema

#### Per-project: `beamtalk.toml`

The three dependency forms, introduced progressively:

```toml
[dependencies]
# Path dep (existing, ADR 0070)
utils = { path = "../my-utils" }

# Git dep (existing, ADR 0070 — Phase 1 distribution)
http = { git = "https://github.com/jamesc/beamtalk-http", tag = "v0.1.0" }

# Registry dep (Phase 2/3 — version string = registry lookup)
http = "~> 0.1"
```

For native hex deps from a private registry:

```toml
[native.dependencies]
cowboy = "~> 2.12"                                          # public hex.pm
beamtalk_http_native = { version = "~> 0.1", repo = "beamtalk" }  # private repo
```

#### Registry declaration: `[repos]` — original design, not what shipped

> **Not implemented.** What shipped is a single `[registry] url` key that only affects Beamtalk-package registry deps — see "Amendment (2026-07-26)" above and [`docs/beamtalk-packages.md` § Package Registry](../beamtalk-packages.md#package-registry). `[native.dependencies]` and rebar3's hex repos are untouched by it.

Private hex repos are declared in a top-level `[repos]` section (Phase 2+). This section serves both Beamtalk package deps and native hex deps — the registry infrastructure is shared.

```toml
[repos.beamtalk]
url = "https://hex.beamtalk.dev"
public_key_path = "keys/beamtalk-hex.pub"      # relative to project root
```

This generates the corresponding `{hex, [{repos, [...]}]}` section in the rebar.config for rebar3.

**Why `[repos]` not `[native.repos]`?** The registry serves both Beamtalk packages and native hex deps. Putting it under `[native]` would imply it's only for Erlang dependencies.

**Why per-project, not global config?** Projects must be self-contained — cloning a repo must be enough to build it. Global config may be added later for auth tokens only.

#### Resolution priority — original design, not what shipped

> **Describes the original Phase 2 plan, not the shipped registry.** The shipped registry resolves exact versions only (no `~>` ranges) through a self-hosted git index (not hex.pm), and pins a git commit SHA in the lockfile (no checksum). See "Amendment (2026-07-26)" above and [`docs/beamtalk-packages.md` § Package Registry](../beamtalk-packages.md#package-registry) for the actual behaviour.

When resolving a Beamtalk package dependency:
1. **Path** — `{ path = "..." }` resolves to the local filesystem
2. **Git** — `{ git = "...", tag/branch/rev = "..." }` resolves from a git remote
3. **Registry** — `"~> 1.0"` resolves from the configured registry (default: hex.pm)

Path deps take absolute priority (development override). Git deps are pinned by lockfile with commit SHA. Registry deps are pinned by lockfile with version + checksum.

#### Lockfile format for registry deps — original design, not what shipped

> **Not implemented.** What shipped adds a single optional `version` field to the existing git `[[package]]` entry instead of a new entry kind — see "Amendment (2026-07-26)" above. There is no `source` field, no `registry` field, and no `checksum` field; `sha` remains the resolved git commit SHA, not a package-content hash.

ADR 0070's lockfile uses `[[package]]` entries with `url`, `reference`, and `sha` fields (designed for git sources). Registry deps (Phase 2+) add a new entry kind:

```toml
[[package]]
name = "http"
version = "0.1.0"
source = "registry"
registry = "beamtalk"          # or "hexpm" for public hex.pm
checksum = "sha256:abc123..."
```

The `source` field disambiguates from git-source entries.

### 3. API Metadata for Tooling

#### Generated on build, not shipped in tarballs

`beamtalk build` already generates `class_corpus.json` into `_build/dev/` for every compiled package (see `build.rs:647-686`). Currently this file contains: class `name`, `superclass`, `methods` (selector strings), `is_sealed`, `is_abstract`, and a `doc` field (currently always null). Richer metadata (class kind, method parameters, return types, doc comments) is a future enhancement — the corpus format is extensible.

Since ADR 0070 established implicit fetch-and-compile on build (the Cargo model), dependencies are always compiled before they can be used. The `class_corpus.json` is generated as a build artifact — there is no window where a dependency is resolved but not compiled.

API metadata is **not** included in the tarball. The tarball ships source; the corpus is derived data generated during compilation. This avoids two sources of truth and keeps tarballs minimal.

#### MCP discovery

MCP already discovers corpora from the `_build/` tree (implemented in `crates/beamtalk-mcp/src/server.rs`), including `_build/dev/` for the root package and `_build/deps/*/` for dependencies (with fallback search paths). After `beamtalk build`, each package's `class_corpus.json` is discovered from these existing locations. No new MCP code is needed.

**Visibility:** Visibility is enforced at corpus generation time — the build step that creates `class_corpus.json` includes only public classes and their public methods. MCP consumes the generated corpus as-is. When the `internal` class modifier lands (ADR 0071), internal classes are excluded from generated `class_corpus.json`.

#### Remote package search (future)

Searching a registry for packages by class name, method selector, or protocol conformance requires server-side metadata indexing. This is not possible with a static hex registry — it would require a custom registry with a search API or a separate search index (analogous to docs.rs for Rust). Deferred until the ecosystem warrants it.

### 4. CLI Surface — original design; `beamtalk publish` shipped in simplified form

> **What shipped:** `beamtalk publish` (BT-2980) — tags the current repo and appends an entry to the registry index, with no tarball step. `package build --tarball`, `package registry build`, and `package retire`/`unretire` below were not built (see "Amendment (2026-07-26)" above). `beamtalk version` (also BT-2980) is a manifest-version bump helper not originally listed here.

```bash
# Phase 1: Build the package (compile + test)
beamtalk package build
# => Builds the package, generates class_corpus.json

# Phase 2: Build a hex-compatible tarball for registry publishing
beamtalk package build --tarball
# => Created _build/http-0.1.0.tar

# Phase 2: Regenerate static registry index
beamtalk package registry build registry/ --name=beamtalk --private-key=key.pem

# Phase 2: Retire/unretire a version
beamtalk package retire http 0.1.0 --reason security --message "Use 0.1.1"
beamtalk package unretire http 0.1.0

# Phase 3: Publish directly to hex.pm
beamtalk package publish
```

**Future: reimplementation in Beamtalk.** Once the package ecosystem and HTTP package are stable, package operations could be reimplemented as native Beamtalk expressions using `hex_core` (pure Erlang) at runtime — e.g., `Package publish: "http"`, `Package add: "json"`. This would enable a fully dynamic workspace where packages can be resolved, fetched, and loaded without leaving the REPL. The Rust CLI remains the baseline; the Beamtalk-native version is a convenience layer on top.

## Prior Art

### Go

Go modules use git repositories as the distribution mechanism. `go.mod` declares dependencies with module paths (typically GitHub URLs) and version constraints. No separate registry — the module proxy (`proxy.golang.org`) caches and indexes git-hosted modules. This validates the "git repos first, registry layer later" approach.

**Adopted:** Git repositories as the Phase 1 distribution mechanism. Tags as version markers.

### Elixir / Hex.pm

The primary inspiration for Phase 2-3. Elixir publishes to hex.pm via `mix hex.publish`. The hex tarball format is well-specified. Private repos use `mix hex.repo add` with auth tokens. Hex.pm supports non-Elixir build tools — Gleam already publishes there with `build_tools = ["gleam"]`.

**Adopted:** Hex tarball format (Phase 2), version constraint syntax, package retirement mechanism.

### Gleam

Publishes to hex.pm using the same tarball format with `build_tools = ["gleam"]`. Critically, Gleam builds hex tarballs **in pure Rust** (~200 lines in `compiler-cli/src/publish.rs`) — it does not use hex_core, Mix, or any Erlang tooling for tarball creation. Uses the `hexpm` Rust crate for hex.pm API calls and `prost` for protobuf. Gleam started publishing to hex.pm at v0.2 (2019) as a one-person project, years before its 1.0 (2024).

**Adopted:** Pure Rust tarball creation, PubGrub resolver (`pubgrub` crate), source-in-tarball convention, `hexpm` crate for API interactions.

### Rust / Cargo

Crates.io is the single registry. `Cargo.toml` declares dependencies by version constraint. Crates can be yanked (removed from resolution but still downloadable).

**Adopted:** Implicit fetch on build (ADR 0070). Short-form version strings for registry deps. Yank/retire semantics.

### Smalltalk / Monticello

Smalltalk traditionally uses image-based distribution or Monticello/Metacello package specs. Metacello supports repository declarations pointing to HTTP servers.

**Rejected:** Image-based distribution doesn't fit the BEAM's file-based compilation model.

## User Impact

### Newcomer

**Phase 1:** Add a git dep to `beamtalk.toml`, run `beamtalk build`, use the classes. Simple but requires knowing the git URL.

**Phase 2-3:** Registry deps (`http = "~> 0.1"`) are the simplest form — just a name and version string.

### Package author

**Phase 1:** Push to GitHub, tag a release. Zero publishing infrastructure.

**Phase 2:** `beamtalk package build --tarball` creates a publishable artifact. Upload to static registry.

**Phase 3:** `beamtalk package publish` pushes directly to hex.pm.

### Erlang/BEAM developer

The hex tarball format (Phase 2+) is standard. Private repos work with rebar3's existing `{hex, [{repos, ...}]}` config. Native deps resolve from hex.pm throughout all phases.

### Tool author (MCP/LSP)

`class_corpus.json` is generated on build for every package, and MCP discovers it automatically from the `_build/` tree (`_build/dev/` for root, `_build/deps/*/` for dependencies). No changes needed across any phase.

## Steelman Analysis

### "Just use hex.pm from the start"

hex.pm has no gatekeeping — Gleam started at v0.2 as a one-person project. Early publishing forces you to get versioning right and is what grew Gleam's ecosystem from zero to 1,300+ packages. LFE, Clojerl, and Efene all publish there too.

However: hex.pm versions are immutable. Once you publish `http 0.1.0`, that version number is burned forever. With APIs still changing significantly, we'd burn through version numbers or mislead consumers about stability. Git tags and a static registry let us iterate freely first.

### "Build a custom registry, not hex-compatible"

A Beamtalk-specific registry could carry richer metadata natively (class kinds, protocol conformance, searchable method signatures) and implement features like class-level search.

However: hex compatibility is required for native deps anyway (rebar3 must resolve from the same registry), and the hex tarball format is extensible enough to carry Beamtalk-specific files inside `contents.tar.gz`.

### "Skip the git phase — build the static registry now"

Building the registry infrastructure now would dogfood the full publish/resolve/fetch/compile cycle and exercise the hex integration.

However: git deps already provide that cycle. `beamtalk build` fetches git deps, compiles them, and puts them on the code path — the same pipeline a registry dep would use. The only thing a static registry adds over git deps is version constraint solving, which isn't needed with <10 packages and one consumer (the main project). Build the registry when there's actual demand — external contributors or enough packages that manual version coordination breaks down.

### "Defer the whole thing — keep packages in the monorepo"

Path deps work. No extraction needed. Simpler build, simpler CI, single repo to manage.

However: path deps never test the real distribution pipeline. A package that works as a path dep in the monorepo may fail when consumed as a git dep (missing files, wrong paths, undeclared dependencies). Extracting to git repos forces the package to be self-contained and tests the consumer experience. The cost is low — it's just a new repo with a `beamtalk.toml`.

## Alternatives Considered

### Alternative A: Separate Beamtalk Package Registry

A custom registry server with Beamtalk-specific API for richer metadata queries.

**Rejected** because: it splits the ecosystem (native deps on hex.pm, Beamtalk packages elsewhere), requires building and maintaining registry infrastructure, and doesn't leverage existing hex tooling.

### Alternative B: Static Registry from Day One

Skip git deps, go straight to a hex-compatible static registry with tarball publishing.

**Rejected** because: it front-loads ~600 lines of Rust tooling (tarball creation + registry generation + signing), registry hosting infrastructure, and a signing keypair — all for ~1 package with one consumer. Git deps provide the same distribution capability with zero new code. Build the registry when the ecosystem needs version constraint solving.

### Alternative E: Publish from Monorepo (Cargo Workspace Model)

Keep packages in the monorepo (`packages/http/`) and add `beamtalk package build --tarball` to publish tarballs from subdirectories. No repo extraction needed. Cargo workspaces use this model — develop everything in one repo, publish individual crates to crates.io.

**Rejected for Phase 1** because: git deps already support `packages/http` as a path dep, which doesn't test the consumer experience. Publishing a tarball from the monorepo would test the tarball pipeline but requires building the Phase 2 tarball tooling — the same ~200 LOC of Rust we're deferring. Extracting to a standalone repo tests self-containedness (missing files, undeclared dependencies) without any new tooling. May revisit for Phase 2 — building tarballs from a monorepo workspace alongside standalone repos is not mutually exclusive.

### Alternative C: Global Config for Registry URLs

Store registry URLs in `~/.config/beamtalk/hex_repos.toml` instead of per-project.

**Rejected as primary mechanism** because projects must be self-contained. Global config may be added later for auth tokens only.

### Alternative D: BEAM Module Attributes Instead of `class_corpus.json`

Use `__beamtalk_meta/0` function exports as the sole source of API metadata. MCP would call these on loaded modules dynamically.

**Rejected** because: MCP runs as a Rust process reading JSON files from `_build/`, not connected to a running BEAM. `class_corpus.json` is the build-time materialisation of module metadata into a format MCP can read statically.

## Consequences

> The Positive/Negative/Neutral bullets below are as originally written for the three-phase design; bullets referring to the original Phase 2 (hex-compatible registry, `[repos]`, PubGrub) describe a design that was not built. See "Amendment (2026-07-26)" above for consequences of what actually shipped, summarised in the amendment-specific bullets appended to Negative and Neutral below.

### Positive
- Phase 1 requires zero new infrastructure — git deps work today
- Hex-compatible tarball format (Phase 2, original design — not what shipped) means rebar3, mix, and existing hex tooling work out of the box
- Three-phase strategy matches infrastructure investment to actual ecosystem size
- `class_corpus.json` generated on build gives MCP/LSP API visibility — no new infrastructure needed
- Per-project `[repos]` (original design — not what shipped; see Amendment) keeps projects self-contained and reproducible
- Gleam has proven hex.pm accepts non-Elixir build tools — Phase 3 path is validated
- Pure Rust tooling (Phase 2, original design — not what shipped) — no Elixir/Mix dependency for publishing
- **(Shipped, 2026-07-26)** The registry that actually shipped needed none of Phase 2's tooling investment — indexing is a lookup table in front of Phase 1's already-working git-dependency machinery

### Negative
- Phase 1 has no version constraint solving — consumers pin exact versions via git tags
- Phase 1 has no package discoverability — consumers need to know the git URL
- Phase 1 splits CI: compiler regressions that break extracted packages are not caught by the main repo's CI without cross-repo integration testing
- Phase 2 (original design — not what shipped) would have required ~600 lines of Rust tooling and registry hosting infrastructure
- Phase 3 depends on hex.pm accepting `build_tools = ["beamtalk"]` — third-party dependency, though low-risk
- Remote package search is not possible until a custom search index is built (deferred)
- **(Shipped, 2026-07-26)** No integrity checksum on fetched dependency content — the locked git commit SHA is not currently enforced on a fresh clone (BT-2992), a gap the original Phase 2 tarball checksum would have closed
- **(Shipped, 2026-07-26)** No version-range support of any kind — every registry dependency pins one exact version; whether ranges are ever added is an open question, not a planned phase
- **(Shipped, 2026-07-26)** A future move to a hex-compatible registry is a real lockfile migration, not a superset: today's lockfile records a commit SHA per registry dependency, not a package checksum, so adopting the original Phase 2 lockfile shape (`source`, `registry`, `checksum`) would be a third lockfile entry kind to support or migrate, alongside plain-git and today's registry-git entries

### Neutral
- Tarball format (Phase 2+, original design — not what shipped) is the same whether served from static registry or hex.pm
- The existing `corpus.json` (example corpus) and `class_corpus.json` (class API corpus) remain separate concerns
- Lockfile format extends additively with `source = "registry"` entries (Phase 2+, original design — not what shipped; see Amendment for the entry shape that actually shipped)

## Implementation

### Phase 1: Git Distribution (Now)
1. Extract `packages/http` → `jamesc/beamtalk-http` as standalone repo
2. Set up CI in the new repo (fetch Beamtalk compiler, run BUnit + EUnit tests)
3. Migrate tests that exercise HTTP classes to the new repo
4. Tag releases (`v0.1.0`, etc.)
5. Update consuming projects to use git deps
6. Verify full fetch/compile/test cycle works from a clean checkout
7. Add cross-repo CI: main repo periodically builds against latest tagged HTTP package

### Phase 2: Hex-Compatible Registry (When Needed) — original plan, not what shipped

> Steps 8–16 below were the original Phase 2 plan and were **not built**. What shipped instead (BT-2977–2981, 2026-07-26) is listed after this list — see "Amendment (2026-07-26)" above for the full comparison.

8. `beamtalk package build --tarball` — pure Rust tarball creation (reference: Gleam's `publish.rs`)
9. `beamtalk package registry build` — pure Rust static registry generation (`prost`, `rsa`/`ring`)
10. `[repos]` section in manifest parser
11. `{ version = "...", repo = "..." }` table form for `[native.dependencies]`
12. Generated `rebar.config` includes `{hex, [{repos, ...}]}` when private repos are configured
13. Version constraint solver — `pubgrub::DependencyProvider` (reference: Gleam's `dependency.rs`)
14. Lockfile extension with `source = "registry"` entries
15. `beamtalk package retire` / `beamtalk package unretire`
16. Set up static registry hosting

### Registry, as shipped (BT-2977–2981, 2026-07-26)

8'. `[registry] url` in the manifest parser, resolved with `BEAMTALK_REGISTRY` env → manifest → default priority (`crates/beamtalk-cli/src/manifest.rs`, `crates/beamtalk-cli/src/commands/deps/registry.rs`)
9'. Registry index resolution and lookup: a git-or-local-directory index of `packages/<name>.toml` files, `(name, exact version) -> (git url, tag)` (`registry.rs`)
10'. `deps add`/`deps list`/`deps update` extended to resolve and re-resolve registry dependencies through the index (BT-2979)
11'. `beamtalk.lock`'s existing git `[[package]]` entry gains an optional `version` field (`crates/beamtalk-cli/src/commands/deps/lockfile.rs`)
12'. `beamtalk version` (manifest version bump helper) and `beamtalk publish` (tag + push + append/create the index entry) (BT-2980, `crates/beamtalk-cli/src/commands/publish.rs`)
13'. User docs: `docs/beamtalk-packages.md` § Package Registry (BT-2981)

### Phase 3: hex.pm (When APIs Stabilise)
17. `beamtalk package publish` with hex.pm API integration (via `hexpm` Rust crate)
18. Registry deps in `[dependencies]` resolve from hex.pm by default

### Future: Beamtalk-Native Package Operations
19. Reimplement package operations as Beamtalk expressions using `hex_core` (pure Erlang)
20. `Package publish: "http"` — publish from the REPL
21. `Package add: "json"` — dynamically resolve, fetch, compile, and load without editing `beamtalk.toml`

### Affected Components

| Component | Phase | Changes |
|-----------|-------|---------|
| `packages/http` | 1 | Extract to `jamesc/beamtalk-http` |
| `crates/beamtalk-cli/src/commands/manifest.rs` | 2 (original plan — not built) | Parse `[repos]`, table-form native deps with `repo` field |
| `crates/beamtalk-cli/src/commands/build.rs` | 2 (original plan — not built) | Generate `{hex, [{repos, ...}]}` in rebar.config |
| `crates/beamtalk-cli/src/commands/package.rs` (new) | 2 (original plan — not built) | `package build --tarball`, `package registry build`, `package retire` |
| `crates/beamtalk-cli/src/commands/deps/` | 2 (original plan — not built) | PubGrub-based resolution (`pubgrub` crate), lockfile extension |
| `crates/beamtalk-mcp/src/server.rs` | — | Already handles dependency corpora — no changes needed |
| `crates/beamtalk-cli/src/manifest.rs` | 2, as shipped | `RegistryConfig` — parses `[registry] url` |
| `crates/beamtalk-cli/src/commands/deps/registry.rs` (new) | 2, as shipped | Registry index location resolution, clone/refresh, lookup |
| `crates/beamtalk-cli/src/commands/publish.rs` (new) | 2, as shipped | `beamtalk publish` — tag, push, update index entry |
| `crates/beamtalk-cli/src/commands/deps/lockfile.rs` | 2, as shipped | `LockEntry.registry_version` — additive `version` field on the existing git entry |

## Migration Path

Each phase adds new capabilities. The tooling is backwards-compatible, but **consumers of first-party packages must update their `beamtalk.toml`** when packages migrate between distribution phases:

- **Phase 1:** Consumers of extracted packages change from `http = { path = "../packages/http" }` to `http = { git = "...", tag = "v0.1.0" }`. Path deps for local development (via `beamtalk deps add --path`) remain available as overrides.
- **Phase 2 (original design — not what shipped):** Consumers change from git dep to registry dep: `http = "~> 0.1"`. Git deps continue to work for packages not yet on the registry.
- **Phase 3 (original design):** No consumer change — registry deps resolve from hex.pm instead of the static registry (URL change in `[repos]`, or removed entirely if hex.pm is the default).
- **Phase 2, as shipped:** Consumers change from git dep to registry dep with an *exact* version: `http = "0.1.0"` (no range syntax). Git deps continue to work for packages not yet on the registry. See "Amendment (2026-07-26)" above for what a later move to a hex-compatible registry (if ever pursued) would additionally require — chiefly a new lockfile entry kind, since today's registry entries are a git `[[package]]` entry, not a checksum-bearing tarball reference.

## Implementation Tracking

**Epic:** BT-1739 (Phase 1: package distribution)
**Issues:** BT-1738 (Justfile/--app), BT-1740 (extract HTTP repo), BT-1741 (monorepo git dep), BT-1742 (cross-repo CI)
**Status:** Implemented (Phase 1)

**Epic:** BT-2977 (library registry — Phase 2, shipped in simplified form; see "Amendment (2026-07-26)" above)
**Issues:** BT-2978 (registry-dependency resolution), BT-2979 (`deps add`/`list`/`update` registry support), BT-2980 (`beamtalk version`/`beamtalk publish`), BT-2981 (docs), BT-2995 (this amendment)
**Follow-up:** BT-2992 (enforce the locked SHA on a fresh dependency clone — the integrity gap noted above)
**Status:** Implemented (simplified Phase 2)

## References
- Related issues: BT-1721 (MCP discovery), BT-1727 (private hex repo setup), BT-1728 (repo field in native deps), BT-1729 (publish first-party packages)
- Related issues (registry, 2026-07): BT-2977 (epic), BT-2978, BT-2979, BT-2980, BT-2981, BT-2992, BT-2995
- Related ADRs: ADR 0026 (package manifest), ADR 0070 (package namespaces and dependencies — see its "Alternative: Registry-Based Dependencies" note pointing back here), ADR 0071 (class visibility), ADR 0072 (native Erlang in packages)
- Hex tarball specification (original Phase 2 design, not implemented): https://github.com/hexpm/specifications/blob/main/package_tarball.md
- Hex registry specification (original Phase 2 design, not implemented): https://github.com/hexpm/specifications/tree/main/registry
- `hex_core` (pure Erlang hex client, for future Beamtalk-native reimplementation): https://github.com/hexpm/hex_core
- `pubgrub` Rust crate (version constraint solver — not currently used; the shipped registry has no solver): https://crates.io/crates/pubgrub
- `hexpm` Rust crate (Gleam's hex.pm API client — not currently used): https://github.com/gleam-lang/hexpm-rust
- Gleam's tarball builder (reference implementation for the original, unbuilt tarball design): `gleam/compiler-cli/src/publish.rs`

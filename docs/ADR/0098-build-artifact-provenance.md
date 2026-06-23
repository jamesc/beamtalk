# ADR 0098: Build Artifact Provenance and Version-Based Staleness Invalidation

## Status
Proposed (2026-06-23)

## Context

### Problem

Stale compiled artifacts keep causing silent, confusing failures. The build
system decides what to reuse purely from **file mtimes and content equality**,
with **no record of which toolchain version produced an artifact**. Three
incidents trace to this single gap:

- **BT-2653** — the workspace served a stale generated `beamtalk_classes.hrl`
  and its `.beam`, producing a `spec for undefined function` cascade. Source and
  mtimes looked fine; nothing rebuilt.
- **beamtalk-http doc corruption** (root cause of the canceled BT-2651 / BT-2652)
  — `_build` artifacts compiled by an **older toolchain** carried mojibake and
  double-spaced docs. Source and mtimes were unchanged, so the dependency was
  never recompiled; the workspace served the frozen, buggy compiled docs.
- **Worktree stale BEAM** — `CLAUDE.md` instructs humans to run `just build`
  after entering a worktree precisely because stale artifacts cause false test
  failures. The build system can't detect this on its own.

mtime-based age checks cannot catch any of these: mtimes are unreliable across
git checkouts, worktrees, and clock skew, and they say nothing about the
*compiler version* that produced the bytes. The missing axis is **toolchain
provenance**.

### Current state

Everything that gates artifact reuse today is mtime- or content-based, and none
of it is version-aware:

| Mechanism | File | Reuse signal | Version-aware? |
|---|---|---|---|
| Pass-1 metadata cache | `build_cache.rs` | source-path → `mtime`; `CACHE_VERSION: u32`; manifest mtime | No |
| BEAM rebuild decision | `build.rs::detect_changes` | per-file `.bt` mtime vs `.beam` mtime | No |
| Dependency freshness | `deps/mod.rs::deps_are_fresh` | `beamtalk.toml` mtime vs `beamtalk.lock`; ebin presence | No |
| Workspace native rebuild | `beamtalk_repl_ops_load.erl` | `.erl`/`.bt` mtime + **generated-header content equality** (BT-2653) | No |
| Shared type-spec cache | `beam_compiler.rs` | **keyed by OTP version** (BT-2470) | **OTP only** |

The version information needed already exists — it is simply never written into
a `_build/` artifact:

- **Beamtalk version** is available at compile time as `env!("BEAMTALK_VERSION")`
  (`beamtalk-build/src/lib.rs` — `0.4.0` for release tags, `0.4.0-dev+<sha>`
  otherwise) and at runtime via OTP app `vsn`
  (`beamtalk_stdlib:version/0`, `beamtalk_package:version/1`).
- **OTP release** is discovered via `discover_otp_beam_files()`
  (`beam_compiler.rs`, `erlang:system_info(otp_release)`), already used to key the
  shared type-spec cache.
- **`__beamtalk_meta/0`** (codegen, `gen_server/methods.rs`) emits a per-module
  metadata map (`class`, `superclass`, `fields`, `method_info`, …) but carries
  **no provenance key**.

So we know the producing version at build time and can read the running version
at load time — we just never compare them.

### Constraints

- **Single source of truth.** Provenance must derive from `VERSION` (via
  `BEAMTALK_VERSION`) and OTP detection that already exist. No new version
  plumbing.
- **No needless rebuilds.** When provenance matches, the existing mtime fast
  paths must still short-circuit. Provenance is a *gate ahead of* mtime checks,
  not a replacement.
- **Proportional investment.** One-person, pre-public project (cf. ADR 0073). A
  lightweight stamp, not a content-addressed store.
- **Spans four layers.** CLI build, dependency resolution, codegen meta, and
  workspace load each consume artifacts and each must honour the same signal.

## Decision

Stamp every build scope with **toolchain provenance** and invalidate on
mismatch. Provenance — not mtime — is the authoritative staleness signal across
a toolchain change.

### 1. Provenance stamp

After a successful build, write a JSON stamp into the build scope:

```jsonc
// _build/dev/.beamtalk-stamp.json   (project)
// _build/deps/<name>/.beamtalk-stamp.json   (each dependency)
{
  "schema": 1,
  "beamtalk_version": "0.4.0-dev+a1b2c3d",  // BEAMTALK_VERSION, verbatim
  "otp_release": "27-15.0.1",               // full otp_release + ERTS, keyed
  "built_at": "2026-06-23T10:04:11Z"        // informational only
}
```

A new `BuildLayout::stamp_path()` / `dep_stamp_path(name)` owns these locations
(consistent with the rest of the layout API). `built_at` is diagnostic and
**never** an invalidation input — only `beamtalk_version` and the OTP version are.

The stamp is written **last**, after every artifact in the scope has landed, via
the same temp-write-then-rename discipline BT-2653 already uses for the generated
header — so a crash mid-build never leaves a stamp claiming freshness for
incomplete output. Concurrent builders are an **accepted race**: two processes
that both read a missing/stale stamp each rebuild and each write at the end;
last-write-wins still yields a *valid* stamp, so the only cost is duplicate work
— not worth a lock file at this project's scale.

The stamp lives under `_build/` (gitignored) and is therefore **build-local**: it
never travels with source, so a fresh clone or new worktree starts with no stamp
→ a provenance miss → a clean rebuild. Once this ships, the `CLAUDE.md` "run
`just build` first in a worktree" caveat can be retired.

Single-file builds (which use `<root>/build` rather than `_build/dev/` and carry
no dependencies) are **out of scope** for v1: they are throwaway, rebuilt per
invocation, and never serve a workspace.

### 2. Invalidation rule

Artifacts in a scope are reused **only if** the stamp's `beamtalk_version`
**and** OTP version equal the current toolchain's. On any mismatch — or a
missing, corrupt, or unrecognized-`schema` stamp — the scope is treated as fully
stale and rebuilt (the equivalent of `--force` for that scope), bypassing mtime
entirely.

```
read stamp
  ├─ missing / corrupt / unknown schema / version ≠ current → rebuild all (provenance miss)
  └─ version == current → fall through to existing mtime fast paths
```

A **project-scope** miss additionally discards the Pass-1 metadata cache
(`.beamtalk-pass1-cache.json`): its `ClassInfo` / class-index entries are
compiler-*derived* from the source AST and are just as version-sensitive as the
`.beam` output, so reusing them across a toolchain change would carry stale
metadata even after the bytecode is rebuilt.

This makes provenance a coarse gate in front of the fine-grained mtime logic:
within one toolchain version, builds are exactly as incremental as today; across
a toolchain change, the whole scope rebuilds once.

The signal is the **full `BEAMTALK_VERSION`**, including the `-dev+<sha>` suffix.
For *consumers* on a released binary this is just `0.4.0` and changes only on
upgrade. For *compiler developers* (this repo) it changes per commit, so an
artifact built by an older commit — exactly the beamtalk-http failure mode — is
detected and rebuilt. That cost is accepted (see Steelman).

The OTP signal is the **full `otp_release`** string (e.g. `27-15.0.1`), not just
the major. This matches the BT-2470 shared type-spec cache, which already keys on
the full version because minor OTP/ERTS releases can shift the primitive type
specs baked into compiled output. Keying both caches on the same granularity
keeps them from disagreeing about whether an upgrade is material.

### 3. Self-describing modules

Add `beamtalk_version` and `otp_release` to the `__beamtalk_meta/0` map so a
**loaded** module is self-describing without a side file:

```erlang
#{ class => 'Counter', superclass => 'Actor', kind => actor,
   beamtalk_version => <<"0.4.0-dev+a1b2c3d">>,   % NEW
   otp_release => <<"27-15.0.1">>,                 % NEW
   fields => [...], method_info => [...] }
```

The side stamp is the **primary gate** (O(1) per scope, readable before loading
anything). The meta keys are the **consumer-side check** the IDE/REPL uses to
validate already-loaded modules against the running toolchain.

A module compiled by an *older* toolchain has **no** provenance keys at all, so a
missing `beamtalk_version`/`otp_release` in the meta map is itself treated as a
provenance miss (stale → recompile), never as an error or a pass. This is
essential: the absent-key case *is* the older-toolchain failure mode this ADR
exists to catch (it is what bit beamtalk-http), so it must fail toward rebuild.

### 4. Provenance at each layer

- **`beamtalk build`** — read the project stamp at the top of
  `execute_build_passes`; on miss, set the existing `force`/`force_pass2` path so
  every module recompiles. Write the stamp in `post_process_package_artifacts`
  after a successful compile.
- **`beamtalk deps`** — `deps_are_fresh` reads each dependency's stamp; a
  version mismatch marks that dep stale and rebuilds it (or, if a rebuild isn't
  possible, fails with a clear *"dependency built by beamtalk X; run `beamtalk
  clean --deps`"* message). **This is the beamtalk-http fix.**
- **Workspace attach / project+dep load** (`beamtalk_repl_ops_load.erl`) —
  generalize the BT-2653 header-content force-rebuild into a provenance check:
  validate the project and each dep stamp (and/or loaded `__beamtalk_meta`) on
  attach; recompile stale scopes instead of silently serving them.
- **`beamtalk clean`** — unchanged. The default profile clean already removes
  `_build/dev/` (and thus the stamp); `--deps` removes dep stamps. Clean remains
  the manual escape hatch; provenance is the automatic version.

### Error example

```
$ beamtalk build
warning: dependency 'beamtalk-http' was compiled by beamtalk 0.3.2 (OTP 26);
         current toolchain is 0.4.0 (OTP 27) — recompiling.
   Compiling beamtalk-http v0.3.2
    Finished dev build in 4.1s
```

## Prior Art

- **Cargo** — `target/.fingerprint/` records the rustc version, host triple,
  features, and dep fingerprints; a rustc upgrade invalidates the cache
  regardless of source mtimes. Toolchain version is part of the validity key.
  This is the closest analog and the model we adopt.
- **Mix (Elixir)** — compile manifests store the Elixir/Erlang version
  (`manifest_vsn`) and force a full recompile when the compiler version changes.
- **Gleam** — the build manifest records the compiler version and invalidates
  the cache on a version change.
- **rebar3** — caches per-profile and records build metadata; recompiles across
  toolchain changes.
- **Go / Bazel / Buck** — content-addressed action caches where the toolchain is
  part of the action key. Maximally correct but heavyweight.

**Adopted:** the Cargo/Mix/Gleam principle — *toolchain version is part of the
cache validity key, not just input mtimes.* **Adapted:** a lightweight per-scope
JSON stamp rather than a fingerprint directory or content-addressed store,
proportional to project scale. **Rejected:** full content-addressed storage
(Bazel) — over-engineered for a pre-public, one-person project.

## User Impact

- **Newcomer** — builds "just work" after a toolchain upgrade. No mysterious
  `spec for undefined function` errors, no need to learn `beamtalk clean`.
- **Smalltalk developer** — the live workspace self-heals: attaching to a
  workspace with stale project/dep artifacts recompiles them instead of serving
  frozen bytes. The BT-2653 cascade no longer happens.
- **Erlang/BEAM developer** — familiar behaviour (rebar3/Mix recompile on
  toolchain change). Provenance is human-readable in the stamp and in
  `__beamtalk_meta`, aiding debugging of "which version produced this?".
- **Operator / CI** — deterministic rebuilds on toolchain upgrade, robust across
  fresh checkouts, worktrees, and clock skew where mtimes lie.

### Discoverability

The stamp is a plain JSON file at a predictable path; `__beamtalk_meta` exposes
the same data reflectively. Both answer "what built this?" without special
tooling.

## Steelman Analysis

- **"mtime is fine — just tell people to run `clean`."** For a one-person
  project this is tempting. But three separate incidents (BT-2651/2652/2653)
  prove the human-discipline approach fails *silently*: the symptom (mojibake
  docs, undefined-function cascade) is far removed from the cause (a stale
  artifact), so the human never thinks to clean. Provenance automates the one
  check a human reliably forgets, at trivial cost.
- **"Hash the inputs instead of the version."** Content hashing catches *source*
  changes but is blind to a toolchain change over *identical* source — which is
  exactly the beamtalk-http case (same `.bt`/`.erl`, older compiler, corrupt
  output). Version is the orthogonal axis content hashing misses.
- **Tension — compiler-dev rebuild cost vs. consumer correctness.** Including the
  `-dev+<sha>` suffix means *this repo's* developers get a full rebuild on every
  commit that changes the compiler. We accept this deliberately: in practice the
  stale-artifact failures are *frequent* and silent, while a recompile is loud,
  fast, and self-healing — so trading occasional rebuilds for never serving stale
  bytes is plainly worth it. The cost is also bounded: the shared OTP type-spec
  cache (BT-2470) survives `_build` wipes and absorbs the worst of it, and the
  per-file mtime layer still short-circuits within a commit. End users on released
  binaries never see this churn at all.

## Alternatives Considered

### mtime-only + manual `clean` (status quo)
Keep heuristics; rely on humans to clean. **Rejected** — empirically fails
silently (three incidents).

### Release-version-only key (drop the `-dev+<sha>` suffix)
Key only on `0.4.0`, ignoring git sha. Avoids dev rebuild churn. **Rejected** —
two different compiler commits share `0.4.0-dev`, so it would *not* invalidate
the beamtalk-http case, the very failure this ADR targets.

### Hash the compiler binary / port executable
Use a content hash of the toolchain itself. Robust, but expensive to compute on
every build and opaque to humans. The version string is cheap, already
available, and human-readable — sufficient for the failure modes observed.

### Content-addressed artifact store (Bazel-style)
Fully hermetic action keys. Maximally correct but disproportionate to project
scale and a large build-system rewrite. **Rejected** per ADR 0073 proportionality.

### `__beamtalk_meta` only (no side stamp)
Make modules self-describing and skip the stamp file. **Rejected as the sole
mechanism** — it requires loading every module to check provenance, whereas the
side stamp is an O(1) per-scope read *before* loading. We do **both**: stamp as
the primary gate, meta as the consumer-side validation.

## Consequences

### Positive
- Eliminates a recurring class of silent stale-artifact failures
  (BT-2651/2652/2653) at their common root.
- Self-healing across toolchain upgrades, worktrees, and CI checkouts.
- Removes reliance on human "remember to rebuild" discipline.
- Cheap: reuses existing version/OTP detection; one small JSON file per scope.
- Loaded modules become self-describing via `__beamtalk_meta`.

### Negative
- Compiler developers in this repo get a full rebuild whenever the commit sha
  changes (mitigated by the shared OTP type-spec cache and intra-commit mtime
  fast paths).
- Small additional stamp I/O per build scope.
- The stamp `schema` and the meta keys are new compatibility surfaces to version.
- The `__beamtalk_meta` map grows by two keys.

### Neutral
- `beamtalk clean` needs no change — it already removes the stamp with the
  profile/dep directories.
- OTP detection and `BEAMTALK_VERSION` already exist; this only *records* them.
- Adds accessors to `BuildLayout` and two keys to the meta map.

## Implementation

Affected components: CLI build (`build.rs`, `build_layout.rs`, `build_cache.rs`),
dependency resolution (`deps/mod.rs`, `deps/lockfile.rs`), codegen
(`gen_server/methods.rs`), workspace load
(`beamtalk_repl_ops_load.erl`).

- **Phase 1 — Project stamp.** Add `BuildLayout::stamp_path()`; write the stamp
  *last* (temp+rename) after a successful build; read it at the start of
  `execute_build_passes` and set `force`/`force_pass2` on mismatch. A miss also
  discards `.beamtalk-pass1-cache.json` (its `ClassInfo` is compiler-derived).
  Tests: version-mismatch → rebuild (incl. Pass-1 cache dropped); matching
  version → no needless rebuild.
- **Phase 2 — Dependency provenance.** Add `dep_stamp_path(name)`; write per-dep
  stamps on dep compile; teach `deps_are_fresh` to mark a dep stale on version
  mismatch (or fail with a `clean --deps` directive). Test: a dep-version-
  mismatch fixture reproducing the beamtalk-http stale-doc case and showing it
  self-heals.
- **Phase 3 — Self-describing modules.** Add `beamtalk_version` / `otp_release`
  to `build_meta_map_doc()` via the typed Document API (no `format!`). Audit the
  `__beamtalk_meta` readers whose snapshots / map-shape assertions may need
  updating: `beamtalk_object_class` (`read_meta/1`, `meta_to_methods/2`),
  `beamtalk_behaviour_intrinsics` (`meta_for_module/1`), `beamtalk_repl_ops_dev`,
  `beamtalk_repl_ops_browse`, and `beamtalk_compiler_server`.
- **Phase 4 — Workspace attach.** Generalize the BT-2653 header-content force-
  rebuild in `beamtalk_repl_ops_load.erl` into a provenance check over the
  project + dep stamps (and loaded `__beamtalk_meta`); recompile stale scopes on
  attach rather than serving them. Treat a **missing** provenance key (old
  toolchain) as a miss. Test: an old-toolchain fixture (no meta keys) recompiles
  on attach.

## Migration Path

No user action required. The first build after this ships finds no stamp,
performs one full rebuild, and writes the stamp; subsequent same-version builds
are incremental as before. Pre-existing `_build/` directories without a stamp are
treated as stale exactly once. `beamtalk clean` remains the manual escape hatch.

**Toolchain downgrade / schema skew.** Mixing toolchains in one `_build/` is
always safe in both directions. If a newer toolchain writes `schema: 2` and an
older binary then reads it, the older binary sees an unrecognized schema and
treats it as a miss (rebuild); the reverse (newer reads an older schema, or a
downgraded `beamtalk_version`) is the same version-mismatch miss. The failure
mode is always an extra rebuild, never silent reuse of foreign-toolchain bytes.

## References
- Related issues: BT-2673 (this ADR), BT-2653 (stale native build — partial
  precedent), BT-2651 / BT-2652 (beamtalk-http stale-`_build` doc corruption,
  canceled), BT-2470 (shared OTP-keyed type-spec cache), BT-2607 (parent epic;
  note: build/toolchain hygiene, not IDE UX)
- Related ADRs: ADR 0070 (package namespaces and dependencies), ADR 0072 (user
  Erlang sources in packages), ADR 0073 (package distribution and discovery),
  ADR 0075 (Erlang FFI type definitions — type-spec cache)
- Documentation: `VERSION`, `docs/development/releasing.md`,
  `docs/development/testing-strategy.md`

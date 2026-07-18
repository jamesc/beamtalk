# Design note: dependency protocol/alias wiring for real builds (BT-2910)

**Status:** Draft for review (answers BT-2910's `needs-spec` gap)
**Issue:** BT-2910 (parent epic BT-2893, ADR 0108)
**References:** ADR 0108 (named union type aliases), ADR 0071 (`internal` modifier),
ADR 0070 (package namespaces/deps), BT-2006 (protocol pre-loading precedent), BT-2898
(alias export/seeding/leakage check), BT-2928/BT-2932 (same-package cross-file alias
resolution)

## TL;DR

BT-2910's own description is partially stale — re-verified by reading the current code,
not by re-stating the issue. As of this audit:

- **Classes** are fully wired cross-package already, both `beamtalk build` and `beamtalk
  lint`, via `ResolvedDependency.class_infos`.
- **Aliases**: same-package cross-file resolution works (BT-2928/2932) in `beamtalk
  build` only. `beamtalk lint` has **zero** alias wiring — not even same-package.
  Cross-package (dependency) alias wiring doesn't exist on either surface.
- **Protocols**: same-package cross-file pre-loading only exists for the BUnit fixture
  compiler's test path. Real `beamtalk build`/`lint` pass `Vec::new()` unconditionally.
  Cross-package doesn't exist anywhere.
- **LSP**: already resolves cross-file aliases project-wide — including indexed
  dependency files under `_build/deps/*/src/` — via `ProjectIndex::cross_file_alias_infos_for`,
  but never populates `pre_loaded_protocols` at all. That alias path also does **no**
  package-boundary filtering today (returns every non-current-file alias
  unconditionally) — a latent `internal`-alias leak, noted below as an explicit
  non-goal of this issue.
- Protocols have **no `internal` modifier at the AST level** (`ProtocolDefinition`
  carries no such field) — so the "leakage check" the original issue worried about only
  applies to aliases. Protocol wiring is pure visibility plumbing, no filtering logic
  needed.

Consequence for sizing: this is **bigger** than the original framing on the lint side
(lint needs same-package *and* cross-package alias/protocol wiring, not just the
cross-package half) but **smaller** on the axis the `needs-spec` label was actually
gating — the extraction/registration APIs (`extract_alias_infos`, `add_pre_loaded`,
`AliasInfo.package`) already exist and already handle the internal/foreign-package
distinction correctly. This is wiring, not new design.

## Audit: how each piece of cross-file metadata reaches each surface today

| Data | build (same-pkg) | build (cross-pkg/dep) | lint (same-pkg) | lint (cross-pkg/dep) | LSP |
|---|---|---|---|---|---|
| `ClassInfo` | done | done (`dep.class_infos`) | done | done (`merge_dependency_class_infos`) | done (`ProjectIndex` preloads `_build/deps/*/src`) |
| `ProtocolInfo` | **missing** — `Vec::new()` always (`build.rs:798`) | **missing** — no field on `ResolvedDependency` | **missing** — not collected at all | **missing** — n/a, nothing to merge | **missing** — `pre_loaded_protocols` never populated |
| `AliasInfo` | done (`collect_project_alias_infos`, BT-2928) | **missing** — no field on `ResolvedDependency` (explicit comment at `build.rs:510`) | **missing** — not collected at all | **missing** — n/a | done via `ProjectIndex::cross_file_alias_infos_for` (no package filtering — separate follow-up) |

Key call sites, verified by reading the current source:

- `crates/beamtalk-cli/src/commands/deps/path.rs:27` `ResolvedDependency` — has
  `class_infos`, no `alias_infos`/`protocol_infos`.
- `crates/beamtalk-cli/src/commands/deps/mod.rs:246` `collect_fresh_deps` and
  `path.rs`'s two other construction sites (`:229`, `:319`) are the **only** places that
  build a `ResolvedDependency` — a single seam to extend once.
- `crates/beamtalk-cli/src/commands/deps/path.rs:615` `build_dep_class_index` is the
  per-dependency extraction helper `class_infos` goes through — natural home for sibling
  `alias`/`protocol` extraction, sharing the already-parsed file set rather than
  re-lexing per kind.
- `crates/beamtalk-cli/src/commands/build.rs:1676` `collect_project_alias_infos` —
  same-package-only, stamps `.package = Some(pkg_name)` on each `AliasInfo`. Needs a
  dependency-merging counterpart, the way `collect_all_class_infos` (`build.rs:1954`)
  already merges `source_class_infos` + `dep_class_infos`.
- `crates/beamtalk-cli/src/commands/lint.rs` calls `analyse_with_natives_and_extensions`
  (`semantic_analysis/mod.rs:408`), which has no protocol/alias parameters at all. The
  already-existing `analyse_with_natives_and_protocols_and_aliases`
  (`semantic_analysis/mod.rs:471`) is a drop-in replacement lint simply never adopted.
- `crates/beamtalk-cli/src/beam_compiler.rs:659` `ClassHierarchyContext` **already**
  carries `pre_loaded_protocols`/`pre_loaded_aliases` fields and forwards them into
  `ProjectDiagnosticContext` (~line 885) — no struct changes needed there; the fields are
  just fed empty/incomplete data today.
- `crates/beamtalk-core/src/semantic_analysis/alias_registry.rs:475`
  `AliasRegistry::add_pre_loaded` **already** implements the internal/foreign-package
  seeding-boundary filter correctly, keyed on `AliasInfo.package` vs `current_package` —
  reuse as-is; it just needs `AliasInfo.package` stamped per-dependency the way
  `collect_project_alias_infos` stamps it per-package today.
- `ProtocolInfo` (`protocol_registry.rs:49`) has no `package`/`is_internal` field —
  protocols have no internal-modifier concept at the AST level
  (`ast/class.rs:318` `ProtocolDefinition`), so the protocol half of this work is pure
  "collect and pass through," no filtering logic to design.

## Design

### 1. Extend the dependency-resolution seam once (`ResolvedDependency`)

Add two fields, populated identically to the existing `class_infos`:

```rust
// deps/path.rs
pub struct ResolvedDependency {
    ...
    pub class_infos: Vec<ClassInfo>,
    pub protocol_infos: Vec<ProtocolInfo>,   // new
    pub alias_infos: Vec<AliasInfo>,          // new — .package stamped to dep name
}
```

Split `build_dep_class_index` into three sibling extraction functions
(`build_dep_class_index`, `build_dep_protocol_infos`, `build_dep_alias_infos`) that share
the already-lexed/parsed `Module` list for the dependency's file set rather than
re-parsing three times. Each `AliasInfo` gets `.package = Some(dep_name.to_string())`
stamped exactly like `collect_project_alias_infos` does for same-package files — this is
what lets `AliasRegistry::add_pre_loaded`'s existing internal-modifier filter correctly
exclude a dependency's `internal type Foo = ...` while still seeding its public aliases.

All of `ResolvedDependency`'s construction sites (`deps/mod.rs:267` fresh-path,
`deps/path.rs:229`/`:319` compile paths) get the same addition. This is the single seam
both `build.rs` and `lint.rs` consume through (`ensure_deps_resolved`/`collect_fresh_deps`),
so one change reaches both surfaces.

### 2. `beamtalk build`: merge dependency protocol/alias infos, stop passing `Vec::new()`

- Add `collect_all_alias_infos(&[&source_aliases, &dep_aliases])`, mirroring
  `collect_all_class_infos` (`build.rs:1954`) — collision diagnostics are already handled
  by `AliasRegistry::add_pre_loaded`, so this is a plain merge, not a dedup pass.
- Add `collect_project_protocol_infos`, mirroring `collect_project_alias_infos`, for
  same-package cross-file protocols (doesn't exist today even same-package), then merge
  with `dep.protocol_infos`.
- `execute_build_passes`/`ClassIndexResult` gains `all_protocol_infos` alongside the
  existing `all_alias_infos`.
- `build.rs:798`'s `pre_loaded_protocols: Vec::new()` becomes
  `pre_loaded_protocols: index.all_protocol_infos.clone()`; `pre_loaded_aliases` gains
  the dependency-merged set instead of staying same-package-only.

### 3. `beamtalk lint`: add the missing same-package *and* cross-package wiring

- `parse_and_extract_class_infos` (`lint.rs:460`) gains alias/protocol extraction
  alongside its existing class extraction — same source files, same loop, avoiding a
  second parse pass.
- `merge_dependency_class_infos` (`lint.rs:556`) becomes `merge_dependency_infos`,
  extending `dep.protocol_infos`/`dep.alias_infos` alongside the existing
  `dep.class_infos.clone()` — free once step 1 populates those fields.
- `collect_diagnostics` (`lint.rs:40`) swaps its `analyse_with_natives_and_extensions`
  call for `analyse_with_natives_and_protocols_and_aliases` (already exists,
  `mod.rs:471`, currently unused by lint), threading the two new params through.

### 4. LSP: protocol wiring only (alias cross-file already works)

- `ProjectIndex` gains a `file_protocols: HashMap<Utf8PathBuf, Vec<ProtocolInfo>>`
  mirroring `file_aliases`, populated in `update_file`, plus a
  `cross_file_protocol_infos_for` mirroring `cross_file_alias_infos_for`.
- `language_service/mod.rs:1704`'s `ProjectDiagnosticContext` construction gains
  `pre_loaded_protocols: self.project_index.cross_file_protocol_infos_for(file)`.
- Aliases: no code change — already wired.
- **Explicitly out of scope / follow-up, not blocking this issue:**
  `cross_file_alias_infos_for`'s missing package-boundary filtering — a dependency's
  `internal` alias can currently leak into every LSP-preloaded file, since the LSP path
  never stamps or checks `AliasInfo.package`. This is a pre-existing, separate
  correctness gap from "wire the data through" at all; worth its own issue, doesn't
  block delivering protocol parity or fixing the CLI cross-package gap this issue targets.

### 5. Integration test (the concrete acceptance-criterion proof)

A real two-package path-dependency fixture (not `add_from_beam_meta`/`add_pre_loaded`
unit tests, per BT-2910's own AC), mirroring `deps/path.rs`'s existing
`path_dependency_resolves_classes` test pattern:

- Producer package exports `type Status = #ok | #error`, `internal type Secret =
  Integer`, and `Protocol define: Greetable`.
- Consumer package's `beamtalk build` **and** `beamtalk lint`:
  - resolve `Status` in a `:: Status` annotation (no diagnostic),
  - resolve `Greetable` in an `extending:`/conformance check,
  - still report `Secret` as unresolved (internal, correctly excluded).

## Sequencing

1. **Step 1** (`ResolvedDependency` + `build_dep_*` extraction) first — the shared seam
   everything else depends on, purely additive (new fields, no behavior change until
   consumed).
2. **Steps 2 and 3** (build.rs, lint.rs) can land in either order or together —
   independent consumers of step 1's data, mechanically identical (both just read
   `dep.protocol_infos`/`dep.alias_infos`). Recommend one PR for both.
3. **Step 4** (LSP protocol wiring) is independent of 2/3 — different crate boundary,
   can land separately.
4. **Step 5** (integration test) proves 1+2+3 together; extend for step 4 if bundled.

## Recommended issue split

Keep this as one `M`-sized issue covering steps 1–3 (the CLI-side ask BT-2910 was
actually filed for — remove `needs-spec`, add `agent-ready`), and file LSP protocol
wiring (step 4) as a separate `S` follow-up issue, matching this epic's existing
one-PR-per-narrow-issue pattern. The LSP alias package-filtering gap noted in step 4
should be filed as its own issue too, once someone decides it's worth prioritizing —
it's a correctness gap but not a regression, and not part of BT-2910's original ask.

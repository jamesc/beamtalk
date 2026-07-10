# Design note: project-scoped compilation WS1 + WS2 (BT-2795, BT-2796)

**Status:** Draft for review (answers the `needs-spec` checklists on BT-2795 and BT-2796)
**Issues:** BT-2795 (WS1: project-wide extension visibility), BT-2796 (WS2: project-wide
class hierarchy assembly), BT-2794 (ADR 0100 Rule 2, blocked on both)
**References:** ADR 0100 (open-world diagnostic policy), ADR 0066 (open class extensions),
ADR 0070 amendment (WS3: cross-package extension metadata), ADR 0024 (static-first,
live-augmented)

## TL;DR

The audit (WS2's first deliverable) shows the two workstreams are not the same size:

- **Class/hierarchy knowledge (WS2) is already effectively project-wide** on every
  production surface. `ClassInfo` carries full instance/class-side method surfaces
  (`class_info.rs:100,153,155`), and every orchestrator already injects cross-file
  `ClassInfo`s into per-file analysis. What WS2 is missing is not an assembly pass —
  it is an **explicit completeness contract**: the checker has no way to know whether
  the injected set is project-complete, so the `has_cross_file_parent` suppression
  must stay even where it is already inert.
- **Extension knowledge (WS1) is genuinely missing.** In production the
  `ExtensionIndex` is built from exactly one module, in-place, inside
  `analyse_full_with_natives` (`semantic_analysis/mod.rs:457-461`, path `"<current>"`).
  The multi-module builder (`collect_extensions`, `extension_index.rs:183`) and the
  conflict/shadowing detectors exist but are wired into no pipeline. A cross-file
  `String >> shoutLouder` is invisible at every call site outside its defining file —
  this is the source of the false `Dnu` hints ADR 0100 Rule 2 exists to remove.

Consequence for sequencing: WS2 shrinks to "plumb a completeness signal + guard
parse-error edge cases" (S/M, not L); WS1 is the substantive plumbing work (M). BT-2794
then becomes a small classifier change gated on the new signal.

## Audit: where knowledge is assembled today

Three independent orchestrators sit on shared `beamtalk-core` primitives; there is no
shared "assemble the project" function.

| Surface | File list | Class knowledge fed to per-file analysis | Extension knowledge |
|---|---|---|---|
| CLI `beamtalk build` | `build.rs:1417 find_source_files` → `FileWalker::source_files()`, alphabetical | Two-pass: Pass 1 `build_class_index` (`build.rs:373`) extracts all files' `ClassInfo`s (mtime-cached via `build_cache.rs`); Pass 2 injects the cross-file subset (`cross_file_class_infos`, `class_hierarchy/mod.rs:393`; `beam_compiler.rs:930`) via `add_from_beam_meta` (`mod.rs:342`) | **Current file only** |
| CLI `beamtalk lint` | `find_package_root` + whole-package parse (`lint.rs:108-117`, BT-2027), plus dep class infos and FFI type cache | Same injection path (`lint.rs:133-145`) | **Current file only** |
| LSP | `collect_preload_files` (`server.rs:2968`): all `src/`+`test/`+`_build/deps/*/src/`+stdlib at startup (5000-file budget); incremental `update_file` per edit | `ProjectIndex` (`project_index.rs:36`) keeps a merged hierarchy with per-file add/remove/update; diagnostics pass `cross_file_class_infos_for(file)` into the unified provider (`language_service/mod.rs:1403-1422`) | **Current file only** |
| compiler-port (REPL/hot reload, ADR 0022) | n/a — per-request | Serialized class hierarchy + `class_module_index` in the request (`compiler-port/main.rs:277,914,1027`) | **Current unit only** |
| `beamtalk check` | Stub (`main.rs:561-565`) — `lint` is the de-facto checker | — | — |

Other audit facts that shape the design:

- **Analysis rebuilds the hierarchy fresh per call.** All `analyse*` entry points funnel
  into `analyse_full_with_natives` (`semantic_analysis/mod.rs:394`): builtins preload
  (`with_builtins`, `mod.rs:211`, `OnceLock`-cached) + current AST + injected
  `pre_loaded_classes`. No entry point accepts a pre-built `ClassHierarchy`; the unified
  provider (`queries/diagnostic_provider.rs:83`, `ProjectDiagnosticContext` at `:45`)
  forwards `cross_file_classes`/`pre_loaded_protocols` the same way. Hierarchy assembly
  and diagnostic checking are the **same pass** today, with project breadth arriving as
  injected inputs — a separable "knowledge assembly" phase already exists de facto
  (CLI Pass 1, LSP preload/ProjectIndex).
- **Parse errors don't block assembly.** The parser error-recovers and returns a partial
  AST; `add_module_classes` (`mod.rs:603`) registers whatever classes were recovered;
  hierarchy construction is infallible (`mod.rs:99-101`). A file with syntax errors
  contributes partial class info rather than poisoning the project.
- **The suppression is nearly inert where injection is complete.** `has_cross_file_parent`
  (`hierarchy_queries.rs:204`) returns true only when a transitive superclass **name is
  absent from the hierarchy**. In a full CLI build/lint or post-preload LSP, intra-project
  parents are present, so the walk reaches a root and returns false — the receiver already
  classifies `ClosedComplete` (`receiver_knowledge.rs:84`) and unresolved-selector hints
  already fire. That is exactly why cross-file **extensions** produce false hints today:
  the class surface looks complete but extension methods were never registered. The
  suppression's remaining bite is (a) contexts *without* full injection (compiler-port,
  isolated single-file analysis, LSP mid-preload) and (b) genuinely unresolved parents.

## Design

### One shared seam, two assembly strategies (answers the shared WS1/WS2 question)

WS1 and WS2 do **not** share a new project-assembly pass; they share the **injection
seam**. Each surface keeps its existing assembly strategy — batch two-pass in the CLI,
incremental `ProjectIndex` merging in the LSP — and both feed a widened per-file analysis
context:

```rust
// queries/diagnostic_provider.rs
pub struct ProjectDiagnosticContext<'a> {
    // existing: options, cross_file_classes, pre_loaded_protocols, ...
    /// WS1: project-wide standalone extensions, excluding the current file
    /// (mirrors cross_file_classes' exclusion contract).
    pub cross_file_extensions: ExtensionIndex,
    /// WS2: how complete the injected knowledge is.
    pub knowledge_scope: KnowledgeScope,
}

pub enum KnowledgeScope {
    /// Default. Only the current module (+ whatever was injected) is known;
    /// today's conservative rules apply unchanged.
    ModuleOnly,
    /// The orchestrator walked the entire project: every project file's classes,
    /// extensions, and protocols are in the injected sets.
    ProjectComplete,
}
```

Threaded into `analyse_full_with_natives`, which registers `cross_file_extensions`
alongside the current module's own index (the existing "skip if already defined" rule in
`register_extensions`, `mod.rs:423`, makes ordering safe), and stamps `knowledge_scope`
onto the built `ClassHierarchy` so `classify_receiver` can consult it.

`KnowledgeScope` is the per-hierarchy realisation of ADR 0100's "feature flag [that]
gates the removal until the corresponding workstream is verified": no orchestrator claims
`ProjectComplete` until its assembly is verified, and any context that can't claim it
keeps today's behaviour automatically.

### WS1: extension collection per surface (pre-pass vs incremental)

**Both, matching each surface's existing architecture** — same answer the class metadata
already settled:

- **CLI build — batch pre-pass.** Extend Pass 1: while `build_class_index` extracts
  `ClassInfo`s per file, also run `ExtensionIndex::add_module(module, real_path)`.
  Extension entries join the per-file `build_cache` record (bump the cache format
  version) so incremental rebuilds stay mtime-driven. Pass 2 passes the index minus the
  current file's entries.
- **CLI lint — same walk.** `lint.rs` already parses the whole package for class infos
  (BT-2027); collect extensions in that same loop.
- **LSP — incremental.** `ProjectIndex` gains per-file extension tracking (mirroring its
  per-file class-name maps): `update_file`/`remove_file` maintain
  `extensions_by_file: HashMap<Utf8PathBuf, Vec<(ExtensionKey, ExtensionLocation)>>`,
  and `diagnostics()` passes a merged `cross_file_extensions_for(file)`. Mid-edit
  staleness gets the same semantics cross-file classes already have: 150 ms debounce +
  the BT-2027 self-healing republish after preload. No new mechanism.
- **Follow-up (optional, separate issue):** `detect_extension_conflicts` /
  `shadowing_diagnostics` (`compilation/extension_conflicts.rs:35,126`) are unit-tested
  but unwired; CLI Pass 1 is the natural home once a project-wide index exists there.

### WS2: completeness contract + parse-error guard

With injection already project-wide, WS2 delivers:

1. **`KnowledgeScope` plumbing** (above) and setting `ProjectComplete` from the three
   orchestrators that genuinely walk the project: CLI build Pass 2, CLI lint with a
   package root, LSP once initial preload has completed (before that: `ModuleOnly`).
2. **Per-file parse-error guard.** New failure mode introduced by trusting project
   completeness: an error-recovered file may *under-report* its classes' method
   surfaces, so a lifted suppression would emit false hints against a half-parsed
   parent. Guard per-receiver, not globally: Pass 1 / `ProjectIndex` mark a
   `surface_incomplete: bool` on `ClassInfo`s extracted from a file that had parse
   **errors**; `classify_receiver` downgrades to `Open` any receiver whose superclass
   chain contains a `surface_incomplete` class. A single broken file therefore degrades
   only its own classes (and their subclasses) to today's silence — it does not block
   assembly (construction is infallible) and does not disable hints project-wide.
   Rejected alternative: dropping the whole project to `ModuleOnly` on any parse error —
   too blunt; one broken file would kill every hint in the project.
3. **Cycle handling:** existing walks are already cycle-guarded
   (`hierarchy_queries.rs:204`); project-wide injection adds no new cycle risk. It does
   make *cross-file* inheritance cycles statically detectable for the first time — worth
   a diagnostic eventually, but a follow-up issue, not a WS2 requirement.

### BT-2794 (Rule 2) lands as a classifier change gated on scope

After WS1+WS2, `classify_receiver` (`receiver_knowledge.rs:84`) becomes:

```text
not in hierarchy                                → Dynamic
DNU override on the relevant side               → Open
scope == ModuleOnly && has_cross_file_parent    → Open   (today's rule, kept for
                                                          incomplete contexts)
any class in superclass chain surface_incomplete → Open  (parse-error guard, WS2)
scope == ProjectComplete && receiver class may
  carry unseen dependency extensions            → Open   (pre-WS3 guard, ADR 0100
                                                          Rule 1 third downgrade)
otherwise                                       → ClosedComplete
```

Under `ProjectComplete`, `has_cross_file_parent == true` can only mean a genuinely
unresolved parent (already surfaced as `UnresolvedClass`) — staying `Open` there is the
conservative choice and costs nothing. The suppression is thereby "removed" exactly as
ADR 0100 demands: per-receiver, only where knowledge is provably complete, with
`ModuleOnly` contexts (compiler-port, isolated files) automatically unaffected.

**Pre-WS3 guard, with one refinement to ADR 0100's wording.** ADR 0100 says only
"classes local to a package with no dependencies" reach `ClosedComplete` before WS3.
We propose the slightly sharper rule: **a class defined in the current package is
dependency-extension-free by construction** — a dependency cannot name (and so cannot
extend) its consumer's classes. So:

- receiver class defined in the current package → eligible for `ClosedComplete`
  regardless of the dependency list;
- receiver class defined elsewhere (builtin/stdlib/dep) **and** the package has ≥ 1
  dependency → `Open` until WS3 loads cross-package extension metadata.

This preserves the ADR's intent (never hint on a dep-contributed method that exists)
while keeping typo-hints on project-local receivers even in packages with dependencies.
If reviewers prefer the ADR's letter, drop the refinement — it is a one-line difference.
Note the visible effect either way: in a package **with** dependencies, today's hints on
stdlib receivers (e.g. `"hello" reverssed` in a compiled file) go silent until WS3.
Hints go down, not up — consistent with Rule 2 — but BT-2794's changelog must call it
out.

### Answers to the remaining checklist items

**WS1 — hot reload / REPL interaction (ADR 0024):** separate concerns, by design. The
static project index serves build/edit-time diagnostics only; the live image remains
authoritative at runtime, and runtime-registered extensions already reach tooling through
the live paths (ADR 0087 xref; runtime `register/5`). The compiler-port keeps
`ModuleOnly` scope, so REPL behaviour — including the ADR 0100 `"hello" reverssed` hint —
is unchanged. If REPL false hints on workspace-loaded cross-file extensions prove
annoying, the port's request schema can later carry extension metadata the same way it
carries the serialized hierarchy; that is a follow-up, not WS1.

**WS1 — file analysed in isolation** (`beamtalk lint somefile.bt` outside a package;
`beamtalk check` is still a stub): no package root → empty `cross_file_extensions`,
`knowledge_scope = ModuleOnly` → byte-for-byte today's behaviour, including the
cross-file-parent suppression. No regression possible.

**WS2 — assembly relative to checking:** stays a separable earlier phase (CLI Pass 1 /
LSP preload) feeding the same-pass per-file analysis. We deliberately do **not** move to
"build one shared `ClassHierarchy` object and pass it in": analysis mutates the hierarchy
per file (current module's classes, extensions, protocols, `propagate_class_kind`), so a
shared mutable hierarchy would need either cloning per file (what injection already
achieves, minus a large refactor) or interior mutability across files (ordering hazards).
Injection + a completeness flag gets the same knowledge with none of that risk.

## Suggested sequencing

1. **WS2 first (BT-2796, rescope L → M):** `KnowledgeScope` + `surface_incomplete`
   plumbing, orchestrators claim `ProjectComplete`, audit doc (this note). No
   user-visible diagnostic change yet.
2. **WS1 second (BT-2795, M):** extension collection + injection on all three surfaces;
   regression test that a same-project cross-file extension stops producing a false
   `Dnu` hint *at the resolution level* (the suppression is still in place, so assert
   via `register_extensions` effect / resolved method lookup, or land 3 immediately
   after and assert end-to-end there).
3. **BT-2794 last (S):** classifier change above, end-to-end regression tests (false
   hint gone; genuine typo on a fully-known local class still hints), ADR 0100 →
   Accepted, changelog note for `--warnings-as-errors` users per the ADR's migration
   section.

Steps 1–2 are independent enough to build in either order, but WS1's injected index has
nowhere to declare its completeness without WS2's flag, and neither changes user-visible
diagnostics until step 3 — so this order keeps every intermediate state shippable.

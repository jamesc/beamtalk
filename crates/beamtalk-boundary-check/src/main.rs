// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compilation -> Language Service dependency-direction checker (BT-3339).
//!
//! `docs/development/architecture-principles.md` §1 documents an aspirational
//! layering for `beamtalk-core`: the **Compilation** bounded context (`ast`,
//! `source_analysis`, `unparse`, `codegen`, `semantic_analysis`,
//! `compilation`) is consumed by the **Language Service** context
//! (`queries`, `language_service`, `lint`), never the other way round. ADR
//! 0117's review found that rule had silently gone stale — a
//! `semantic_analysis -> queries` production edge, plus an extensive
//! `queries <-> language_service` cycle, existed with nothing to catch them.
//! This binary is the fix: it parses every `.rs` file under each Compilation
//! module with `syn` (a real parser, not text/regex heuristics — Rust source
//! is full of `{}` inside string literals, which defeats brace-counting) and
//! fails when a production `use crate::<module>::...` import or
//! fully-qualified `crate::<module>::...` path reaches into `queries`,
//! `language_service`, or `lint`. That includes a `crate::<module>::...`
//! reference written inside a macro call (`format!`, `assert_eq!`, `vec!`,
//! ...) — macro bodies are opaque token streams to `syn`'s AST-level `Path`
//! visitor, so those are found by a separate raw-token scan, not missed.
//!
//! Two edges that don't literally start with `crate::` are also caught, so
//! they can't quietly reopen this exact hole (found in code review — see the
//! crate README):
//!
//! - **`super::(super::)*<module>::...`**: any chain of one or more leading
//!   `super` segments landing on a Language-Service module name is treated
//!   the same as a `crate::<module>::...` edge, without needing to compute
//!   how many `super`s a given file needs to actually reach the crate root —
//!   no module anywhere inside a Compilation-context subtree is ever itself
//!   named `queries`/`language_service`/`lint`, so a chain that resolves to
//!   one of those names (and compiles) can only mean the real top-level
//!   sibling module.
//! - **`crate::prelude::<Item>`** (and, equivalently, a
//!   `super::(super::)*prelude::<Item>` chain landing on that exact name):
//!   `beamtalk-core`'s `lib.rs` re-exports a mix of Compilation- and
//!   Language-Service-origin items through `pub mod prelude`. A reference
//!   through that shim is resolved back to the item's real origin module (by
//!   parsing `lib.rs`'s `prelude` block, not a hand-maintained list — see
//!   [`parse_module_reexport_aliases`]) before being classified; a
//!   `crate::prelude::...`/`super::(super::)*prelude::...` reference this
//!   checker can't resolve to one specific item (a glob import, or an item
//!   genuinely missing from `prelude`'s current re-export list) is *always*
//!   flagged, since it can't be proven safe. Treating a `super`-rooted match
//!   the same as a `crate`-rooted one is safe because [`run`] separately
//!   enforces, on every run (not by one-time manual audit), that no
//!   Compilation module ever nests a local `prelude` submodule of its own —
//!   see [`find_nested_prelude_mod`].
//!
//! Test-only edges are allowed (Cargo permits cyclic dev-dependencies): any
//! item gated by `#[cfg(test)]` (directly, or nested inside an enclosing
//! `#[cfg(test)] mod`), `#[test]`, or a multi-segment test-runner attribute
//! ending in `::test` (`#[tokio::test]`, `#[async_std::test]`, ...), and any
//! whole file under a `tests/` directory or named `test(s).rs` /
//! `*_test(s).rs` / `test_*.rs`, is skipped.
//!
//! Run via `just check-boundary` — the binary discovers the repo root by
//! walking up from `CARGO_MANIFEST_DIR`, so it can be invoked from any
//! working directory.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use proc_macro2::{Span, TokenStream, TokenTree};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::visit::Visit;
use syn::{Attribute, Item, ItemUse, Meta, Path as SynPath, UseTree};

/// `beamtalk-core`'s source root, relative to the repo root.
const CORE_SRC: &str = "crates/beamtalk-core/src";

/// The Compilation bounded context (CLAUDE.md § Architecture /
/// `architecture-principles.md` §1) — these modules may depend on each
/// other freely (that's internal cohesion, not a boundary violation; Rust
/// `mod`s inside one crate are not required to be acyclic), but never on
/// Language Service.
const COMPILATION_MODULES: &[&str] = &[
    "ast",
    "source_analysis",
    "unparse",
    "codegen",
    "semantic_analysis",
    "compilation",
];

/// The Language Service bounded context — the consumer. Compilation must
/// never import from these in production code. `lint` is listed here as a
/// regression guard, not an active target: BT-3340 (ADR 0117 Decision step
/// 2, landing after BT-3339 first wrote this list) extracted it out of
/// `beamtalk-core` entirely into the standalone `beamtalk-lint` crate — see
/// that crate's own module header (`crates/beamtalk-lint/src/lib.rs`) for
/// why the move was safe (it depended only on Compilation-context modules,
/// with no back-edges) — so no `crate::lint::...` edge can exist under
/// `CORE_SRC` any more. Keeping the name here costs nothing and means this
/// checker would immediately flag it if a `lint` module were ever
/// reintroduced inside `beamtalk-core` by mistake. BT-3353 is the issue that
/// corrected this comment once the module actually left.
const LANGUAGE_SERVICE_MODULES: &[&str] = &["queries", "language_service", "lint"];

/// Top-level `beamtalk-core/src` modules that belong to neither bounded
/// context this checker gates — `repl` (its own DDD Context: REPL) and
/// `project` (DDD Context: Build System, per their own module headers).
/// Listed explicitly (rather than treating "not Compilation, not Language
/// Service" as implicitly fine) so [`run`]'s directory-drift check can tell
/// "a module nobody classified yet" apart from "a module deliberately left
/// out of both lists" — a new top-level directory under `CORE_SRC` that
/// isn't in any of the three lists fails the check instead of silently
/// going unscanned.
const OTHER_MODULES: &[&str] = &["repl", "project"];

/// Known, already-tracked violations ADR 0117 Decision step 3 (BT-3342)
/// removes. Keyed by (file path relative to repo root, target module,
/// *exact* `crate::...` path text of the one tracked edge) — not just
/// (file, module), so that a new, unrelated edge from the same file into
/// the same module (e.g. a second `queries::` call added later) still fails
/// instead of silently matching this entry. Remove an entry here in the
/// *same* PR that removes the corresponding edge from the code — this list
/// exists to stop *new* edges from landing while these are still open, not
/// to launder them indefinitely. `run()` fails if an entry here no longer
/// matches a real edge, so it can't silently go stale either.
///
/// BT-3341 (the `queries::announce_sites_query::is_announce_selector` edge)
/// was fixed by moving `is_announce_selector` into the shared leaf module
/// `announce_selectors.rs`; its entry has been removed from this list.
const ALLOWLIST: &[(&str, &str, &str)] = &[];

/// True when `entry` (a single [`ALLOWLIST`] tuple: file, target module,
/// exact `crate::...` path text) covers the edge described by `file`,
/// `module`, and `path_text`. Scoped to the exact path text — not just
/// (file, module) — so an entry allow-listing one specific call site never
/// silently swallows a different, unrelated edge that happens to share the
/// same file and target module.
fn allowlist_matches(
    entry: &(&str, &str, &str),
    file: &str,
    module: &str,
    path_text: &str,
) -> bool {
    entry.0 == file && entry.1 == module && entry.2 == path_text
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => {
            println!("boundary-check: OK");
            ExitCode::SUCCESS
        }
        Err(errors) => {
            eprintln!(
                "boundary-check: {} dependency-direction error(s) detected\n",
                errors.len()
            );
            for e in &errors {
                eprintln!("  - {e}");
            }
            eprintln!(
                "\nCompilation ({}) must not import from Language Service ({}) in \
                 production code — see docs/development/architecture-principles.md §1 \
                 and docs/ADR/0117-beamtalk-core-crate-split.md. Move the shared behavior \
                 into a leaf module beneath both (pattern: synthetic_selectors.rs), or if \
                 this is a new instance of an already-tracked violation, add it to \
                 ALLOWLIST in crates/beamtalk-boundary-check/src/main.rs with a BT-NNNN \
                 comment.",
                COMPILATION_MODULES.join(", "),
                LANGUAGE_SERVICE_MODULES.join(", "),
            );
            ExitCode::FAILURE
        }
    }
}

/// Classifies one already-collected [`Edge`] (from a file at `rel`, inside
/// Compilation module `module`) against `LANGUAGE_SERVICE_MODULES` and
/// `ALLOWLIST`, resolving a `crate::prelude::...` reference first via
/// `prelude_reexports`. Returns `Some(violation message)` when the edge is a
/// violation; `None` when it's fine — either the (resolved) target isn't a
/// Language-Service module, or it's covered by an `ALLOWLIST` entry, in
/// which case that entry's key is inserted into `allowlist_matched` so
/// `run()`'s later "did every ALLOWLIST entry actually match something"
/// pass can tell it was used.
fn classify_edge(
    edge: &Edge,
    rel: &str,
    module: &str,
    prelude_reexports: &HashMap<String, String>,
    allowlist_matched: &mut HashSet<(String, String, String)>,
) -> Option<String> {
    let effective_module = match resolve_prelude_edge(edge, prelude_reexports) {
        None => edge.target_module.clone(),
        Some(Ok(resolved)) => resolved,
        Some(Err(())) => {
            return Some(format!(
                "{rel}:{}:{}: `{}` (in Compilation module `{module}`) references \
                 `crate::prelude::...` in a way this checker can't resolve to one specific \
                 underlying module (a glob/bare import, or an item not found in prelude's \
                 current re-export list) — resolve to the specific underlying module so this \
                 checker can verify it doesn't reach into Language Service, or update \
                 `prelude`'s re-export list / crates/beamtalk-boundary-check/src/main.rs if \
                 this checker's resolution logic is out of date",
                edge.line, edge.column, edge.path_text
            ));
        }
    };
    if !LANGUAGE_SERVICE_MODULES.contains(&effective_module.as_str()) {
        return None;
    }
    let allow_key = (
        rel.to_string(),
        effective_module.clone(),
        edge.path_text.clone(),
    );
    if ALLOWLIST
        .iter()
        .any(|e| allowlist_matches(e, &allow_key.0, &allow_key.1, &allow_key.2))
    {
        allowlist_matched.insert(allow_key);
        return None;
    }
    let via_prelude = effective_module != edge.target_module;
    Some(format!(
        "{rel}:{}:{}: `{}` (in Compilation module `{module}`) imports from Language Service \
         module `{}` via `{}`{}",
        edge.line,
        edge.column,
        module,
        effective_module,
        edge.path_text,
        if via_prelude {
            " (resolved through crate::prelude::...)"
        } else {
            ""
        }
    ))
}

fn run() -> Result<(), Vec<String>> {
    let repo_root = find_repo_root().map_err(|e| vec![e])?;
    let core_src = repo_root.join(CORE_SRC);
    let prelude_reexports = parse_module_reexport_aliases(&repo_root).map_err(|e| vec![e])?;

    let mut violations = Vec::new();
    violations.extend(check_module_list_drift(&core_src)?);
    let mut allowlist_matched: HashSet<(String, String, String)> = HashSet::new();
    let mut files_scanned = 0usize;

    for module in COMPILATION_MODULES {
        let module_dir = core_src.join(module);
        let mut files = Vec::new();
        collect_rs_files(&module_dir, &mut files).map_err(|e| vec![e])?;
        for file in files {
            let rel = file
                .strip_prefix(&repo_root)
                .unwrap_or(&file)
                .to_string_lossy()
                .replace('\\', "/");
            if is_test_only_file(&file) {
                continue;
            }
            let text = fs::read_to_string(&file)
                .map_err(|e| vec![format!("failed to read {}: {e}", file.display())])?;
            let parsed = syn::parse_file(&text).map_err(|e| {
                vec![format!(
                    "failed to parse {} as Rust source: {e} (is this file using syntax \
                     `syn` doesn't support yet? bump the `syn` dependency, or if that's \
                     not it, this check needs to learn about the new construct)",
                    rel
                )]
            })?;
            files_scanned += 1;

            if find_nested_prelude_mod(&parsed.items) {
                violations.push(format!(
                    "{rel}: declares a local `mod prelude` inside Compilation module \
                     `{module}` — this breaks the invariant `resolve_prelude_edge` and \
                     `module_index` rely on (see their doc comments) to treat every \
                     `super::(super::)*prelude::...` chain as unambiguously the crate-root \
                     `prelude` shim in lib.rs. Rename this submodule, or if it genuinely needs \
                     to be called `prelude`, teach the resolver to disambiguate before removing \
                     this guard."
                ));
            }

            let mut visitor = EdgeVisitor {
                test_depth: 0,
                found: Vec::new(),
            };
            visitor.visit_file(&parsed);

            for edge in visitor.found {
                if let Some(v) = classify_edge(
                    &edge,
                    &rel,
                    module,
                    &prelude_reexports,
                    &mut allowlist_matched,
                ) {
                    violations.push(v);
                }
            }
        }
    }

    // Sanity check, same rationale as beamtalk-surface-drift's zero-items
    // guards: if the scanner found nothing at all, the syn-based walk
    // itself is probably broken (wrong directory, parse silently no-op'd,
    // ...), not evidence the codebase is clean.
    if files_scanned == 0 {
        return Err(vec![format!(
            "scanned 0 files under {CORE_SRC}/<compilation module> — the module list or \
             directory layout may have changed. Update COMPILATION_MODULES in \
             crates/beamtalk-boundary-check/src/main.rs."
        )]);
    }

    // Every ALLOWLIST entry must correspond to a real, still-present edge —
    // otherwise the allowlist has gone stale (the violation was fixed, or
    // never existed under that path/text) and should be deleted.
    for (file, target, path_text) in ALLOWLIST {
        let key = (
            (*file).to_string(),
            (*target).to_string(),
            (*path_text).to_string(),
        );
        if !allowlist_matched.contains(&key) {
            violations.push(format!(
                "ALLOWLIST entry ({file}, {target}, {path_text}) in \
                 crates/beamtalk-boundary-check/src/main.rs no longer matches any edge — \
                 remove it (the violation it covered appears to be fixed, or its exact \
                 `crate::...` path text changed and the entry needs updating)."
            ));
        }
    }

    violations.sort();
    if violations.is_empty() {
        Ok(())
    } else {
        Err(violations)
    }
}

/// Walk upwards from `CARGO_MANIFEST_DIR` until we find a `VERSION` file.
fn find_repo_root() -> Result<PathBuf, String> {
    let manifest_dir = env_var("CARGO_MANIFEST_DIR")?;
    let mut dir: PathBuf = PathBuf::from(manifest_dir);
    loop {
        if dir.join("VERSION").exists() && dir.join("Cargo.toml").exists() {
            return Ok(dir);
        }
        if !dir.pop() {
            return Err("could not locate beamtalk repo root (no VERSION file found)".into());
        }
    }
}

fn env_var(name: &str) -> Result<String, String> {
    std::env::var(name).map_err(|_| format!("environment variable {name} not set"))
}

/// Lists every top-level directory directly under `core_src` and fails if
/// any of them isn't accounted for in [`COMPILATION_MODULES`],
/// [`LANGUAGE_SERVICE_MODULES`], or [`OTHER_MODULES`] — otherwise a new
/// module added to `beamtalk-core/src` (or one of these three lists falling
/// out of date some other way, e.g. a rename) would simply never be
/// scanned, with nothing here or in CI signaling the gap. This is the
/// module-list analogue of `run`'s `files_scanned == 0` guard: both exist so
/// this checker fails loudly on drift instead of silently checking less
/// than it claims to.
fn check_module_list_drift(core_src: &Path) -> Result<Vec<String>, Vec<String>> {
    let mut known: HashSet<&str> = HashSet::new();
    known.extend(COMPILATION_MODULES.iter().copied());
    known.extend(LANGUAGE_SERVICE_MODULES.iter().copied());
    known.extend(OTHER_MODULES.iter().copied());

    let entries = fs::read_dir(core_src)
        .map_err(|e| vec![format!("failed to read {}: {e}", core_src.display())])?;
    let mut unclassified = Vec::new();
    for entry in entries {
        let entry =
            entry.map_err(|e| vec![format!("dir entry error in {}: {e}", core_src.display())])?;
        let file_type = entry
            .file_type()
            .map_err(|e| vec![format!("failed to stat {}: {e}", entry.path().display())])?;
        if !file_type.is_dir() {
            continue;
        }
        let name = entry.file_name().to_string_lossy().into_owned();
        if !known.contains(name.as_str()) {
            unclassified.push(format!(
                "{}/{name} is a new top-level module not classified into COMPILATION_MODULES, \
                 LANGUAGE_SERVICE_MODULES, or OTHER_MODULES in \
                 crates/beamtalk-boundary-check/src/main.rs — add it to whichever bounded \
                 context it belongs to (see docs/beamtalk-ddd-model.md) so this checker \
                 actually scans it",
                core_src.display()
            ));
        }
    }
    Ok(unclassified)
}

/// Recursively searches `items` — a file's top-level items, or the body of a
/// nested `mod { ... }` block — for a `mod prelude` declaration at any
/// depth, in either form Rust allows: inline (`mod prelude { ... }`) or
/// pointing at a separate file (`mod prelude;`, `content` is `None` for that
/// form but the `Item::Mod` with `ident == "prelude"` still appears here).
///
/// This is the run-every-time invariant guard [`resolve_prelude_edge`] and
/// [`module_index`] depend on: [`run`] calls this for every parsed
/// Compilation-module file and fails the check the moment it finds a hit,
/// which is what makes it safe to treat a `super::(super::)*prelude::...`
/// chain as unambiguously *the* crate-root `prelude` shim — the same way
/// `queries`/`language_service`/`lint` are unambiguous by not existing
/// anywhere else in the Compilation-context subtree, except proven here on
/// every run instead of by one-time manual audit (`prelude` is common enough
/// re-export-convenience Rust style that a one-time audit alone would be
/// worth much less).
fn find_nested_prelude_mod(items: &[Item]) -> bool {
    for item in items {
        let Item::Mod(m) = item else { continue };
        if m.ident == "prelude" {
            return true;
        }
        if let Some((_, inner)) = &m.content {
            if find_nested_prelude_mod(inner) {
                return true;
            }
        }
    }
    false
}

/// Parses `<core_src>/lib.rs`'s `pub mod prelude { ... }` block and returns
/// a map from each re-exported item's externally-visible name to the real
/// module it comes from (e.g. `"LanguageService" -> "language_service"`).
/// Built by parsing `lib.rs` itself — not a hand-maintained mirror of
/// `prelude`'s contents — so it can't drift the way a duplicated list could
/// (CLAUDE.md § No duplicate implementations): if `prelude` gains or loses a
/// re-export, this map picks it up on the next run with no edit needed here.
///
/// Only items re-exported via `pub use crate::<module>::{...}` are
/// resolvable this way; a glob (`pub use crate::<module>::*`) or a
/// non-`crate::`-rooted re-export inside `prelude` contributes nothing to
/// the map, which is fine — [`resolve_prelude_edge`] treats an unresolvable
/// `crate::prelude::<name>` reference as always requiring attention rather
/// than silently assuming it's safe.
fn parse_module_reexport_aliases(repo_root: &Path) -> Result<HashMap<String, String>, String> {
    let lib_rs = repo_root.join(CORE_SRC).join("lib.rs");
    let text = fs::read_to_string(&lib_rs)
        .map_err(|e| format!("failed to read {}: {e}", lib_rs.display()))?;
    let parsed = syn::parse_file(&text)
        .map_err(|e| format!("failed to parse {} as Rust source: {e}", lib_rs.display()))?;

    let mut map = HashMap::new();
    for item in &parsed.items {
        let Item::Mod(m) = item else { continue };
        if m.ident != "prelude" {
            continue;
        }
        let Some((_, items)) = &m.content else {
            continue;
        };
        for inner in items {
            let Item::Use(u) = inner else { continue };
            let mut prefix: Vec<syn::Ident> = Vec::new();
            let mut leaves: Vec<(Vec<syn::Ident>, Span)> = Vec::new();
            walk_use_tree(&u.tree, &mut prefix, &mut leaves);
            for (segments, _span) in leaves {
                if segments.len() >= 2 && segments[0] == "crate" {
                    let module = segments[1].to_string();
                    let Some(exported_name) = segments.last() else {
                        continue;
                    };
                    map.insert(exported_name.to_string(), module);
                }
            }
        }
    }
    Ok(map)
}

/// Extracts the item name immediately following the `prelude` segment of an
/// [`Edge::path_text`] whose root is either `crate::prelude::` or a
/// `super::(super::)*prelude::` chain (an optional leading `use ` is
/// stripped first) — e.g.
/// `"crate::prelude::LanguageService::something"` -> `Some("LanguageService")`,
/// `"use crate::prelude::LanguageService"` -> `Some("LanguageService")`,
/// `"super::prelude::LanguageService"` -> `Some("LanguageService")`,
/// `"use super::super::prelude::LanguageService"` -> `Some("LanguageService")`.
/// Splitting on the literal `"::prelude::"` substring (rather than stripping
/// a fixed `crate::prelude::` prefix) is what makes both roots resolve the
/// same way without needing to know how many `super`s a given chain used.
/// `None` for a bare `crate::prelude`/`super::prelude`/`use crate::prelude`
/// reference (the module itself, or a glob leaf — see [`walk_use_tree`]'s
/// doc comment on how a glob's leaf omits the `*`), which
/// [`resolve_prelude_edge`] treats as unresolvable.
fn prelude_item_name(path_text: &str) -> Option<&str> {
    let rest = path_text.strip_prefix("use ").unwrap_or(path_text);
    let (_, after_prelude) = rest.split_once("::prelude::")?;
    after_prelude.split("::").next().filter(|s| !s.is_empty())
}

/// Resolves an [`Edge`] whose recorded `target_module` is `"prelude"` to the
/// real module it re-exports from. Returns `None` when `edge` doesn't target
/// `prelude` at all (the caller should use `edge.target_module` unchanged).
/// Returns `Some(Ok(module))` when a genuine `crate::prelude::...` **or**
/// `super::(super::)*prelude::...` reference is resolved via
/// `prelude_reexports`. Returns `Some(Err(()))` when it can't be resolved to
/// one specific module — a glob/bare `crate::prelude`/`super::prelude`
/// reference, or an item name not found in `prelude`'s current re-export
/// list — which `run()` always treats as a violation: letting an
/// unresolvable `prelude` reference through unflagged would silently reopen
/// the exact hole this checker exists to close (this gap, and the fix, came
/// out of code review — see the crate README).
///
/// A `super`-rooted match is deliberately resolved exactly the same as a
/// `crate`-rooted one — unlike `queries`/`language_service`/`lint`, `prelude`
/// isn't a name this function can independently prove unique across every
/// Compilation-context subtree, so this correctness *does* depend on no
/// Compilation module ever nesting a local `prelude` submodule of its own (a
/// `pub mod prelude { ... }` re-export-convenience submodule is otherwise
/// common Rust style). That precondition isn't just documented — [`run`]
/// checks it on every invocation via [`find_nested_prelude_mod`] and fails
/// loudly the moment it stops holding, rather than this function silently
/// assuming it forever. An earlier version of this function required a
/// `crate`-rooted path specifically to sidestep that risk without the
/// invariant check, which reopened a false negative: a `super::prelude::X`
/// reference from a depth-1 file (e.g. `ast/mod.rs`) is exactly as much
/// *the* crate-root `prelude` as `crate::prelude::X` is, and went unresolved
/// (silently treated as an ordinary, harmless edge) under that restriction
/// (found in code review — see the crate README).
fn resolve_prelude_edge(
    edge: &Edge,
    prelude_reexports: &HashMap<String, String>,
) -> Option<Result<String, ()>> {
    if edge.target_module != "prelude" {
        return None;
    }
    match prelude_item_name(&edge.path_text) {
        Some(name) => Some(prelude_reexports.get(name).cloned().ok_or(())),
        None => Some(Err(())),
    }
}

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if !dir.exists() {
        return Err(format!(
            "expected Compilation module directory {} does not exist — the module list in \
             crates/beamtalk-boundary-check/src/main.rs is out of date",
            dir.display()
        ));
    }
    let entries =
        fs::read_dir(dir).map_err(|e| format!("failed to read {}: {e}", dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|e| format!("dir entry error in {}: {e}", dir.display()))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .map_err(|e| format!("failed to stat {}: {e}", path.display()))?;
        if file_type.is_dir() {
            collect_rs_files(&path, out)?;
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
    Ok(())
}

/// A file is entirely test code — and so skipped outright rather than
/// parsed for production edges — when it lives under a `tests`/`test`
/// directory, or its own name marks it as a test module (`tests.rs`,
/// `test.rs`, `test_*.rs`, `*_test.rs`, `*_tests.rs`). This matches how
/// this codebase actually names its test files (verified against every
/// existing `crate::{queries,language_service,lint}::` reference inside the
/// Compilation modules as of BT-3339 landing — see the crate README);
/// individual `#[cfg(test)]`/`#[test]` items *inside* an otherwise-production
/// file are handled separately, by the AST visitor below.
fn is_test_only_file(path: &Path) -> bool {
    let in_test_dir = path.components().any(|c| {
        matches!(
            c,
            std::path::Component::Normal(s) if s == "tests" || s == "test"
        )
    });
    if in_test_dir {
        return true;
    }
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
    stem == "tests"
        || stem == "test"
        || stem.starts_with("test_")
        || stem.ends_with("_test")
        || stem.ends_with("_tests")
}

/// One production edge from a Compilation-context file into some module,
/// found by [`EdgeVisitor`]. Filtering to Language-Service targets happens
/// in `run()`.
struct Edge {
    target_module: String,
    path_text: String,
    line: usize,
    column: usize,
}

/// Walks a parsed file collecting every `crate::<module>::...` edge —
/// `use` imports (via a manual `UseTree` walk, since `syn`'s generic
/// visitor doesn't route those through `visit_path`) and fully-qualified
/// path expressions/types/patterns/macro paths (via `visit_path`, which
/// `syn`'s default implementation already routes every `Path`-typed AST
/// field through) — while skipping anything inside a `#[cfg(test)]` or
/// `#[test]`-gated item. Doc comments never appear here at all: `syn`
/// represents them as `#[doc = "..."]` string-literal attributes, not as
/// `Path`s, so an intra-doc link to `crate::queries::x` can't produce a
/// false positive.
struct EdgeVisitor {
    /// Greater than zero while inside an item gated by `#[cfg(test)]` /
    /// `#[test]` (nesting depth, so a test item inside another test item
    /// still un-gates correctly on the way back out).
    test_depth: usize,
    found: Vec<Edge>,
}

impl EdgeVisitor {
    fn record(&mut self, target_module: &str, path_text: String, span: Span) {
        if self.test_depth > 0 {
            return;
        }
        let start = span.start();
        self.found.push(Edge {
            target_module: target_module.to_string(),
            path_text,
            line: start.line,
            column: start.column + 1,
        });
    }

    /// Macro bodies (`format!(...)`, `assert_eq!(...)`, `vec![...]`, a
    /// custom `docvec!`, ...) are opaque `TokenStream`s to `syn` — none of
    /// their contents show up as `Path` AST nodes, so `visit_path` alone
    /// never sees a `crate::<module>::...`/`super::(super::)*<module>::...`
    /// reference written inside one. This walks the raw tokens (recursing
    /// into `(...)`/`[...]`/`{...}` groups, since a macro's own delimiters
    /// and any nested call like `assert_eq!(f(crate::queries::x()), 1)` are
    /// just more tokens) looking for an `ident ( :: ident )*` pattern rooted
    /// at `crate` or `super` and records each hit exactly like a real path
    /// edge.
    fn scan_token_stream(&mut self, tokens: TokenStream) {
        let toks: Vec<TokenTree> = tokens.into_iter().collect();
        let mut i = 0;
        while i < toks.len() {
            if let TokenTree::Ident(id) = &toks[i] {
                if id == "crate" || id == "super" {
                    if let Some((segments, next)) = parse_dotted_ident_path(&toks, i) {
                        let names: Vec<String> = segments.iter().map(|(n, _)| n.clone()).collect();
                        if let Some(idx) = module_index(&names) {
                            let module = names[idx].clone();
                            let path_text = names.join("::");
                            self.record(&module, path_text, segments[idx].1);
                            i = next;
                            continue;
                        }
                    }
                }
            }
            if let TokenTree::Group(g) = &toks[i] {
                self.scan_token_stream(g.stream());
            }
            i += 1;
        }
    }
}

/// If `names` starts with `crate` followed by at least one more segment,
/// returns `Some(1)` (the always-fixed index of the module name after a
/// `crate::` root). If `names` starts with one or more `super` segments
/// followed by at least one more segment, returns the index of that
/// segment (equal to the number of leading `super`s — `super::queries` is
/// index 1, `super::super::queries` is index 2, ...). Otherwise `None` (a
/// bare `crate`/`super` with nothing following, or neither root).
///
/// Correctness of treating a `super`-rooted match the same as a
/// `crate`-rooted one does *not* require knowing how many `super`s a given
/// file actually needs to reach the crate root: no module anywhere inside a
/// Compilation-context subtree is itself named `queries`, `language_service`,
/// or `lint` (verified at BT-3339 landing — see the crate README), so a
/// `super`-chain landing on one of those exact names can only be real Rust
/// code that compiles by genuinely reaching the true top-level sibling
/// module — there is no closer, shadowing target it could otherwise mean.
/// (This also means the function deliberately returns an index for chains
/// that stay *inside* the same Compilation module, e.g. `super::helper` —
/// harmless, since `run()` only acts on names that match a Language-Service
/// module.)
///
/// `prelude` is a different case: it isn't in [`LANGUAGE_SERVICE_MODULES`],
/// so a bare `module_index` match on it is never itself a violation — but
/// [`resolve_prelude_edge`] special-cases `target_module == "prelude"` to
/// resolve `crate::prelude::...` (and, equivalently, `super`-rooted)
/// re-exports, the same as this function treats `queries`/`language_service`/
/// `lint`. `prelude` isn't proven unique the way those three are by
/// construction (a `pub mod prelude { ... }` re-export-convenience submodule
/// is common Rust style) — instead [`run`] proves it on every run, via
/// [`find_nested_prelude_mod`], by checking no Compilation module actually
/// nests one. See [`resolve_prelude_edge`]'s doc comment.
fn module_index(names: &[String]) -> Option<usize> {
    if names.first().is_some_and(|s| s == "crate") {
        return if names.len() >= 2 { Some(1) } else { None };
    }
    let super_count = names.iter().take_while(|s| s.as_str() == "super").count();
    if super_count > 0 && names.len() > super_count {
        Some(super_count)
    } else {
        None
    }
}

impl<'ast> Visit<'ast> for EdgeVisitor {
    fn visit_item(&mut self, i: &'ast Item) {
        let gated = item_attrs(i).iter().any(is_test_gated);
        if gated {
            self.test_depth += 1;
            syn::visit::visit_item(self, i);
            self.test_depth -= 1;
        } else {
            syn::visit::visit_item(self, i);
        }
    }

    fn visit_impl_item_fn(&mut self, i: &'ast syn::ImplItemFn) {
        let gated = i.attrs.iter().any(is_test_gated);
        if gated {
            self.test_depth += 1;
            syn::visit::visit_impl_item_fn(self, i);
            self.test_depth -= 1;
        } else {
            syn::visit::visit_impl_item_fn(self, i);
        }
    }

    fn visit_item_use(&mut self, i: &'ast ItemUse) {
        // `visit_item` above has already accounted for `#[cfg(test)]` on
        // this `use` statement itself (Item::Use is covered by
        // `item_attrs`); `self.test_depth` reflects that by the time we get
        // here, so `record` still does the right thing for a directly-gated
        // `use`.
        let mut prefix: Vec<syn::Ident> = Vec::new();
        let mut leaves: Vec<(Vec<syn::Ident>, Span)> = Vec::new();
        walk_use_tree(&i.tree, &mut prefix, &mut leaves);
        for (segments, span) in leaves {
            let names: Vec<String> = segments
                .iter()
                .map(std::string::ToString::to_string)
                .collect();
            let Some(idx) = module_index(&names) else {
                continue;
            };
            let module = names[idx].clone();
            let path_text = format!("use {}", names.join("::"));
            self.record(&module, path_text, span);
        }
    }

    fn visit_path(&mut self, path: &'ast SynPath) {
        let starts_with_root = path
            .segments
            .first()
            .is_some_and(|s| s.ident == "crate" || s.ident == "super");
        if starts_with_root {
            let names: Vec<String> = path.segments.iter().map(|s| s.ident.to_string()).collect();
            if let Some(idx) = module_index(&names) {
                let module = names[idx].clone();
                let path_text = path_to_string(path);
                self.record(&module, path_text, path.segments[idx].ident.span());
            }
        }
        syn::visit::visit_path(self, path);
    }

    fn visit_macro(&mut self, mac: &'ast syn::Macro) {
        // `visit_macro`'s default impl already walks `mac.path` (covering a
        // macro invoked as `crate::some_module::some_macro!(...)`) through
        // `visit_path` above; this additionally scans the macro's argument
        // tokens, which the default impl does not descend into at all.
        self.scan_token_stream(mac.tokens.clone());
        syn::visit::visit_macro(self, mac);
    }
}

fn path_to_string(path: &SynPath) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

/// Parses an `ident ( :: ident )*` token pattern out of a raw macro token
/// stream, starting at `toks[start]` (checked by the caller to be an `Ident`
/// — `crate` or `super`, the two roots [`scan_token_stream`] cares about,
/// though this function itself doesn't care which). Returns every segment
/// name paired with its span (`toks[start]` itself included as the first
/// entry, mirroring [`module_index`]'s expectations) and the index of the
/// next unconsumed token. A trailing bare root with no `:: ident` after it
/// (there's no such usage in this codebase) still returns the one-element
/// segment list rather than `None` — failing closed here would just mean a
/// missed edge, not a false positive, so this is intentionally permissive
/// about what counts as "a `::` pair".
fn parse_dotted_ident_path(
    toks: &[TokenTree],
    start: usize,
) -> Option<(Vec<(String, Span)>, usize)> {
    let TokenTree::Ident(first) = toks.get(start)? else {
        return None;
    };
    let mut segments = vec![(first.to_string(), first.span())];
    let mut i = start + 1;
    while is_coloncolon(toks.get(i), toks.get(i + 1)) {
        let TokenTree::Ident(seg) = toks.get(i + 2)? else {
            break;
        };
        segments.push((seg.to_string(), seg.span()));
        i += 3;
    }
    Some((segments, i))
}

/// True when the two tokens are the `::` pair (two adjacent `:` `Punct`
/// tokens — `proc_macro2` always represents `::` this way, regardless of
/// whether the first is reported as `Joint` or `Alone` spacing).
fn is_coloncolon(a: Option<&TokenTree>, b: Option<&TokenTree>) -> bool {
    matches!(a, Some(TokenTree::Punct(p)) if p.as_char() == ':')
        && matches!(b, Some(TokenTree::Punct(p)) if p.as_char() == ':')
}

/// Recursively flattens a `UseTree` into (full segment path, span) leaves.
/// `UseTree::Group` (`use crate::{a::B, c::D}`) fans out into one leaf per
/// group member; `UseTree::Glob` (`use crate::queries::*`) contributes the
/// prefix itself as a leaf (there's no further ident to anchor a span on,
/// so the `*` token's span is used).
fn walk_use_tree(
    tree: &UseTree,
    prefix: &mut Vec<syn::Ident>,
    out: &mut Vec<(Vec<syn::Ident>, Span)>,
) {
    match tree {
        UseTree::Path(p) => {
            prefix.push(p.ident.clone());
            walk_use_tree(&p.tree, prefix, out);
            prefix.pop();
        }
        UseTree::Name(n) => {
            let mut full = prefix.clone();
            full.push(n.ident.clone());
            let span = n.ident.span();
            out.push((full, span));
        }
        UseTree::Rename(r) => {
            // The externally-visible name after `as` — not `r.ident` (the
            // original name), which callers resolving `prelude`
            // re-exports by their visible name (e.g.
            // `parse_module_reexport_aliases`) need to match what
            // production code actually writes at the use site.
            let mut full = prefix.clone();
            full.push(r.rename.clone());
            let span = r.rename.span();
            out.push((full, span));
        }
        UseTree::Glob(g) => {
            out.push((prefix.clone(), g.star_token.span));
        }
        UseTree::Group(group) => {
            for item in &group.items {
                walk_use_tree(item, prefix, out);
            }
        }
    }
}

/// Extracts the attribute list from every `Item` variant that carries one
/// (all of them except `Verbatim`, which `syn` uses for constructs it
/// can't otherwise represent).
fn item_attrs(item: &Item) -> &[Attribute] {
    match item {
        Item::Const(x) => &x.attrs,
        Item::Enum(x) => &x.attrs,
        Item::ExternCrate(x) => &x.attrs,
        Item::Fn(x) => &x.attrs,
        Item::ForeignMod(x) => &x.attrs,
        Item::Impl(x) => &x.attrs,
        Item::Macro(x) => &x.attrs,
        Item::Mod(x) => &x.attrs,
        Item::Static(x) => &x.attrs,
        Item::Struct(x) => &x.attrs,
        Item::Trait(x) => &x.attrs,
        Item::TraitAlias(x) => &x.attrs,
        Item::Type(x) => &x.attrs,
        Item::Union(x) => &x.attrs,
        Item::Use(x) => &x.attrs,
        _ => &[],
    }
}

/// True for `#[test]`, a multi-segment test-runner attribute ending in
/// `::test` (`#[tokio::test]`, `#[async_std::test]`, `#[actix_rt::test]`,
/// ...), or any `#[cfg(...)]` whose predicate mentions `test` as a bare,
/// non-negated term (`cfg(test)`, `cfg(any(test, ...))`,
/// `cfg(all(test, ...))`). `cfg(not(test))` deliberately does *not* count —
/// erring toward still scanning code is the safe direction (it can only
/// produce an over-strict false positive needing an allowlist entry, never
/// hide a real production edge).
fn is_test_gated(attr: &Attribute) -> bool {
    if is_test_attr_path(attr.path()) {
        return true;
    }
    if !attr.path().is_ident("cfg") {
        return false;
    }
    let Ok(meta) = attr.parse_args::<Meta>() else {
        return false;
    };
    cfg_predicate_mentions_test(&meta)
}

/// `#[test]` is a single-segment path, but `#[tokio::test]` and friends are
/// not — a bare `attr.path().is_ident("test")` only matches the former, so
/// this also accepts any path whose *last* segment is `test`.
fn is_test_attr_path(path: &SynPath) -> bool {
    path.segments.last().is_some_and(|s| s.ident == "test")
}

fn cfg_predicate_mentions_test(meta: &Meta) -> bool {
    match meta {
        Meta::Path(p) => p.is_ident("test"),
        Meta::NameValue(_) => false,
        Meta::List(list) => {
            if list.path.is_ident("not") {
                return false;
            }
            let Ok(inner) = list.parse_args_with(Punctuated::<Meta, Comma>::parse_terminated)
            else {
                return false;
            };
            inner.iter().any(cfg_predicate_mentions_test)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn edges_in(src: &str) -> Vec<(String, String, usize)> {
        let file = syn::parse_file(src).expect("test fixture must parse");
        let mut visitor = EdgeVisitor {
            test_depth: 0,
            found: Vec::new(),
        };
        visitor.visit_file(&file);
        visitor
            .found
            .into_iter()
            .map(|e| (e.target_module, e.path_text, e.line))
            .collect()
    }

    #[test]
    fn finds_plain_use_edge() {
        let edges = edges_in("use crate::queries::foo;\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "queries");
    }

    #[test]
    fn finds_grouped_use_edges() {
        let edges = edges_in("use crate::{queries::Foo, semantic_analysis::Bar, ast::Baz};\n");
        let modules: HashSet<_> = edges.iter().map(|e| e.0.as_str()).collect();
        assert!(modules.contains("queries"));
        assert!(modules.contains("semantic_analysis"));
        assert!(modules.contains("ast"));
    }

    #[test]
    fn finds_glob_use_edge() {
        let edges = edges_in("use crate::queries::*;\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "queries");
    }

    #[test]
    fn finds_fully_qualified_call_edge() {
        let edges = edges_in(
            "fn f() { let x = crate::queries::announce_sites_query::is_announce_selector(\"x\"); }\n",
        );
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "queries");
        assert_eq!(
            edges[0].1,
            "crate::queries::announce_sites_query::is_announce_selector"
        );
    }

    #[test]
    fn finds_edge_in_type_position() {
        let edges = edges_in("struct S { field: crate::language_service::Position }\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "language_service");
    }

    #[test]
    fn ignores_doc_comment_mentions() {
        // `syn` represents `///` / `//!` as `#[doc = "..."]` string-literal
        // attributes, never as a `Path` — so an intra-doc link to
        // `crate::queries::x` must not be flagged.
        let edges = edges_in("/// See [`crate::queries::foo`] for details.\nfn f() {}\n");
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn ignores_edge_inside_cfg_test_mod_block() {
        let edges =
            edges_in("#[cfg(test)]\nmod tests {\n    fn t() { crate::queries::foo(); }\n}\n");
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn ignores_edge_inside_test_fn() {
        let edges = edges_in("#[test]\nfn t() { crate::queries::foo(); }\n");
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn ignores_edge_inside_cfg_any_test_feature() {
        let edges = edges_in(
            "#[cfg(any(test, feature = \"test\"))]\npub mod test_support {\n    fn t() { crate::queries::foo(); }\n}\n",
        );
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn does_not_ignore_cfg_not_test() {
        // `not(test)` is the opposite predicate — erring toward *still*
        // scanning here is the safe direction.
        let edges = edges_in("#[cfg(not(test))]\nfn f() { crate::queries::foo(); }\n");
        assert_eq!(edges.len(), 1);
    }

    #[test]
    fn ignores_edge_inside_test_gated_impl_method() {
        let edges = edges_in(
            "struct S;\nimpl S {\n    #[cfg(test)]\n    fn t() { crate::lint::foo(); }\n}\n",
        );
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn production_edge_outside_any_test_gate_is_found() {
        let edges = edges_in(
            "fn prod() { crate::lint::check(); }\n#[cfg(test)]\nmod tests { fn t() { crate::lint::other(); } }\n",
        );
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].1, "crate::lint::check");
    }

    #[test]
    fn is_test_only_file_matches_repo_conventions() {
        assert!(is_test_only_file(Path::new("a/b/tests/c.rs")));
        assert!(is_test_only_file(Path::new("a/b/tests.rs")));
        assert!(is_test_only_file(Path::new("a/b/property_tests.rs")));
        assert!(is_test_only_file(Path::new(
            "a/b/method_category_corpus_tests.rs"
        )));
        assert!(is_test_only_file(Path::new("a/b/error_recovery_tests.rs")));
        assert!(is_test_only_file(Path::new("a/b/test_helpers_only.rs")));
        assert!(!is_test_only_file(Path::new("a/b/validation.rs")));
        assert!(!is_test_only_file(Path::new("a/b/mod.rs")));
    }

    #[test]
    fn finds_edge_inside_format_macro() {
        let edges = edges_in("fn f() { let _ = format!(\"{}\", crate::queries::foo()); }\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "queries");
        assert_eq!(edges[0].1, "crate::queries::foo");
    }

    #[test]
    fn finds_edge_inside_vec_macro() {
        let edges = edges_in("fn f() { let _ = vec![crate::lint::bar()]; }\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "lint");
    }

    #[test]
    fn finds_edge_inside_nested_macro_call() {
        // assert_eq!(g(crate::language_service::baz()), 1) — the edge is
        // inside a plain call nested *inside* the macro's own token group,
        // exercising the recursive descent into `TokenTree::Group`.
        let edges = edges_in("fn f() { assert_eq!(g(crate::language_service::baz()), 1); }\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "language_service");
    }

    #[test]
    fn ignores_macro_edge_inside_cfg_test_mod() {
        let edges = edges_in(
            "#[cfg(test)]\nmod tests {\n    fn t() { let _ = format!(\"{}\", crate::queries::foo()); }\n}\n",
        );
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn ignores_edge_inside_tokio_test_fn() {
        let edges = edges_in("#[tokio::test]\nasync fn t() { crate::queries::foo(); }\n");
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn finds_super_chain_use_edge() {
        let edges = edges_in("use super::super::queries::foo;\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "queries");
        assert_eq!(edges[0].1, "use super::super::queries::foo");
    }

    #[test]
    fn finds_super_chain_path_edge() {
        let edges = edges_in("fn f() { super::super::language_service::Position::new(); }\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "language_service");
        assert_eq!(edges[0].1, "super::super::language_service::Position::new");
    }

    #[test]
    fn finds_super_chain_edge_inside_macro() {
        let edges = edges_in("fn f() { let _ = format!(\"{}\", super::lint::check()); }\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "lint");
        assert_eq!(edges[0].1, "super::lint::check");
    }

    #[test]
    fn ignores_super_chain_edge_inside_cfg_test_mod() {
        let edges = edges_in(
            "#[cfg(test)]\nmod tests {\n    fn t() { super::super::queries::foo(); }\n}\n",
        );
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn single_super_targeting_a_plain_item_is_harmless() {
        // `super::helper` isn't a Language-Service module name, so it's
        // recorded (module_index doesn't know or care what it points at)
        // but filtered out downstream in `run()` — exercised here just to
        // confirm it doesn't panic and records the expected module name.
        let edges = edges_in("fn f() { super::helper(); }\n");
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "helper");
    }

    #[test]
    fn self_prefixed_path_is_not_recorded() {
        // `self::` is never treated as a root at all (never escapes the
        // current module), so this should record nothing.
        let edges = edges_in("fn f() { let x = self::local(); }\n");
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn bare_use_super_glob_is_not_recorded() {
        // `use super::*;` has no segment after the lone `super`, so
        // `module_index` correctly declines to treat it as a resolvable
        // edge (unlike `crate::prelude::*`, this isn't specially flagged —
        // no current usage of this pattern exists in the codebase either).
        let edges = edges_in("use super::*;\n");
        assert!(edges.is_empty(), "unexpected edges: {edges:?}");
    }

    #[test]
    fn module_index_handles_crate_and_super_roots() {
        let crate_names = vec!["crate".to_string(), "queries".to_string()];
        assert_eq!(module_index(&crate_names), Some(1));

        let bare_crate = vec!["crate".to_string()];
        assert_eq!(module_index(&bare_crate), None);

        let one_super = vec!["super".to_string(), "queries".to_string()];
        assert_eq!(module_index(&one_super), Some(1));

        let two_supers = vec![
            "super".to_string(),
            "super".to_string(),
            "queries".to_string(),
        ];
        assert_eq!(module_index(&two_supers), Some(2));

        let bare_super = vec!["super".to_string()];
        assert_eq!(module_index(&bare_super), None);

        let neither = vec!["self".to_string(), "queries".to_string()];
        assert_eq!(module_index(&neither), None);
    }

    #[test]
    fn prelude_item_name_extracts_from_use_and_path_forms() {
        assert_eq!(
            prelude_item_name("use crate::prelude::LanguageService"),
            Some("LanguageService")
        );
        assert_eq!(
            prelude_item_name("crate::prelude::LanguageService::new"),
            Some("LanguageService")
        );
        assert_eq!(prelude_item_name("use crate::prelude"), None);
        assert_eq!(prelude_item_name("crate::queries::foo"), None);
    }

    #[test]
    fn prelude_item_name_extracts_from_super_rooted_forms() {
        // A `super::(super::)*prelude::...` chain resolves the item name
        // exactly the same way a `crate::prelude::...` one does — see
        // `resolve_prelude_edge`'s doc comment on why that's safe.
        assert_eq!(
            prelude_item_name("super::prelude::LanguageService"),
            Some("LanguageService")
        );
        assert_eq!(
            prelude_item_name("super::super::prelude::LanguageService::new"),
            Some("LanguageService")
        );
        assert_eq!(
            prelude_item_name("use super::super::prelude::LanguageService"),
            Some("LanguageService")
        );
        assert_eq!(prelude_item_name("use super::prelude"), None);
        assert_eq!(prelude_item_name("super::helper"), None);
    }

    #[test]
    fn resolve_prelude_edge_returns_none_for_non_prelude_edge() {
        let edge = Edge {
            target_module: "queries".to_string(),
            path_text: "crate::queries::foo".to_string(),
            line: 1,
            column: 1,
        };
        let map = HashMap::new();
        assert!(resolve_prelude_edge(&edge, &map).is_none());
    }

    #[test]
    fn resolve_prelude_edge_resolves_super_rooted_reexport_same_as_crate_rooted() {
        // Regression test (should-still-catch direction): a
        // `super::prelude::X` reference from a depth-1 file (e.g.
        // `ast/mod.rs`) is exactly as much *the* crate-root `prelude` as
        // `crate::prelude::X` is, and must resolve — and be flagged when
        // `X` is Language-Service-origin — the same way. An earlier version
        // of `resolve_prelude_edge` required a `crate`-rooted path
        // specifically and silently let this case through unresolved (found
        // in code review).
        let edge = Edge {
            target_module: "prelude".to_string(),
            path_text: "super::prelude::LanguageService".to_string(),
            line: 1,
            column: 1,
        };
        let mut map = HashMap::new();
        map.insert(
            "LanguageService".to_string(),
            "language_service".to_string(),
        );
        assert_eq!(
            resolve_prelude_edge(&edge, &map),
            Some(Ok("language_service".to_string()))
        );
    }

    #[test]
    fn resolve_prelude_edge_resolves_use_multi_super_rooted_reexport() {
        let edge = Edge {
            target_module: "prelude".to_string(),
            path_text: "use super::super::prelude::LanguageService".to_string(),
            line: 1,
            column: 1,
        };
        let mut map = HashMap::new();
        map.insert(
            "LanguageService".to_string(),
            "language_service".to_string(),
        );
        assert_eq!(
            resolve_prelude_edge(&edge, &map),
            Some(Ok("language_service".to_string()))
        );
    }

    #[test]
    fn resolve_prelude_edge_flags_super_rooted_unknown_item_as_unresolvable() {
        // A `super::prelude::...` reference to an item this checker can't
        // find in prelude's current re-export list must still be flagged,
        // exactly like the crate-rooted case — it must never silently pass
        // through as harmless just because it's super-rooted.
        let edge = Edge {
            target_module: "prelude".to_string(),
            path_text: "super::prelude::SomethingNew".to_string(),
            line: 1,
            column: 1,
        };
        let map = HashMap::new();
        assert_eq!(resolve_prelude_edge(&edge, &map), Some(Err(())));
    }

    #[test]
    fn resolve_prelude_edge_resolves_known_reexport() {
        let edge = Edge {
            target_module: "prelude".to_string(),
            path_text: "crate::prelude::LanguageService::new".to_string(),
            line: 1,
            column: 1,
        };
        let mut map = HashMap::new();
        map.insert(
            "LanguageService".to_string(),
            "language_service".to_string(),
        );
        assert_eq!(
            resolve_prelude_edge(&edge, &map),
            Some(Ok("language_service".to_string()))
        );
    }

    #[test]
    fn resolve_prelude_edge_flags_unknown_item_as_unresolvable() {
        let edge = Edge {
            target_module: "prelude".to_string(),
            path_text: "crate::prelude::SomethingNew".to_string(),
            line: 1,
            column: 1,
        };
        let map = HashMap::new(); // "SomethingNew" not in the map
        assert_eq!(resolve_prelude_edge(&edge, &map), Some(Err(())));
    }

    #[test]
    fn resolve_prelude_edge_flags_glob_or_bare_reference_as_unresolvable() {
        let edge = Edge {
            target_module: "prelude".to_string(),
            path_text: "use crate::prelude".to_string(),
            line: 1,
            column: 1,
        };
        let map = HashMap::new();
        assert_eq!(resolve_prelude_edge(&edge, &map), Some(Err(())));
    }

    // `find_nested_prelude_mod` — the invariant guard `resolve_prelude_edge`
    // and `module_index` depend on to treat every `super`-rooted "prelude"
    // match as unambiguously the crate-root shim. Covers both directions:
    // it must actually catch a nested `prelude` submodule (should-still-catch),
    // and it must not flag ordinary, unrelated module structure
    // (shouldn't-false-positive).

    #[test]
    fn find_nested_prelude_mod_catches_inline_declaration() {
        let file = syn::parse_file("mod helper;\nmod prelude {\n    pub fn x() {}\n}\n")
            .expect("test fixture must parse");
        assert!(find_nested_prelude_mod(&file.items));
    }

    #[test]
    fn find_nested_prelude_mod_catches_file_backed_declaration() {
        // `mod prelude;` (pointing at a separate file) parses to an
        // `Item::Mod` with `content: None` — must still be caught, not just
        // the inline-body form.
        let file = syn::parse_file("mod helper;\nmod prelude;\n").expect("test fixture must parse");
        assert!(find_nested_prelude_mod(&file.items));
    }

    #[test]
    fn find_nested_prelude_mod_catches_nested_at_any_depth() {
        let file =
            syn::parse_file("mod outer {\n    mod inner {\n        mod prelude {}\n    }\n}\n")
                .expect("test fixture must parse");
        assert!(find_nested_prelude_mod(&file.items));
    }

    #[test]
    fn find_nested_prelude_mod_ignores_unrelated_modules() {
        // Shouldn't-false-positive direction: ordinary module structure with
        // no submodule literally named `prelude`, at any depth, must not be
        // flagged.
        let file = syn::parse_file(
            "mod helper;\nmod validation {\n    mod checks {\n        pub fn f() {}\n    }\n}\n",
        )
        .expect("test fixture must parse");
        assert!(!find_nested_prelude_mod(&file.items));
    }

    #[test]
    fn find_nested_prelude_mod_ignores_non_module_items_named_prelude() {
        // A function, const, or other item merely *named* `prelude` is not
        // a submodule and must not trip the guard — only `mod prelude`.
        let file = syn::parse_file("fn prelude() {}\nconst prelude: u8 = 0;\n")
            .expect("test fixture must parse");
        assert!(!find_nested_prelude_mod(&file.items));
    }

    #[test]
    fn parse_module_reexport_aliases_reads_lib_rs_prelude_block() {
        let dir = tempfile::tempdir().expect("tempdir");
        let core_src_dir = dir.path().join(CORE_SRC);
        fs::create_dir_all(&core_src_dir).expect("mkdir core_src");
        fs::write(
            core_src_dir.join("lib.rs"),
            "pub mod prelude {\n\
             \x20   pub use crate::ast::ClassDefinition;\n\
             \x20   pub use crate::language_service::{LanguageService, Position};\n\
             \x20   pub use crate::source_analysis::Span;\n\
             }\n",
        )
        .expect("write lib.rs");
        let map = parse_module_reexport_aliases(dir.path()).expect("parse ok");
        assert_eq!(map.get("ClassDefinition").map(String::as_str), Some("ast"));
        assert_eq!(
            map.get("LanguageService").map(String::as_str),
            Some("language_service")
        );
        assert_eq!(
            map.get("Position").map(String::as_str),
            Some("language_service")
        );
        assert_eq!(map.get("Span").map(String::as_str), Some("source_analysis"));
    }

    #[test]
    fn module_list_drift_clean_when_every_dir_is_classified() {
        let dir = tempfile::tempdir().expect("tempdir");
        for m in COMPILATION_MODULES
            .iter()
            .chain(LANGUAGE_SERVICE_MODULES)
            .chain(OTHER_MODULES)
        {
            fs::create_dir(dir.path().join(m)).expect("create module dir");
        }
        // A file alongside the module directories must not be mistaken for
        // an unclassified module (only directories are considered).
        fs::write(dir.path().join("lib.rs"), "").expect("write lib.rs");
        let drift = check_module_list_drift(dir.path()).expect("no I/O error");
        assert!(drift.is_empty(), "unexpected drift: {drift:?}");
    }

    #[test]
    fn module_list_drift_flags_unclassified_directory() {
        let dir = tempfile::tempdir().expect("tempdir");
        fs::create_dir(dir.path().join("ast")).expect("create ast dir");
        fs::create_dir(dir.path().join("ir")).expect("create unclassified dir");
        let drift = check_module_list_drift(dir.path()).expect("no I/O error");
        assert_eq!(drift.len(), 1);
        assert!(drift[0].contains("ir"), "unexpected message: {}", drift[0]);
    }

    #[test]
    fn allowlist_requires_exact_path_text_match() {
        let entry = (
            "crates/beamtalk-core/src/semantic_analysis/type_checker/validation.rs",
            "queries",
            "crate::queries::announce_sites_query::is_announce_selector",
        );
        assert!(allowlist_matches(&entry, entry.0, entry.1, entry.2));
        // Same file and target module, but a different call site — this
        // must NOT be covered by an entry scoped to one specific edge, or a
        // new unrelated violation could land silently in an already-
        // allow-listed file.
        assert!(!allowlist_matches(
            &entry,
            entry.0,
            entry.1,
            "crate::queries::hover_provider::unrelated_call"
        ));
    }
}

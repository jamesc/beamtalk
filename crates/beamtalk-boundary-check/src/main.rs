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

use std::collections::HashSet;
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
/// never import from these in production code. `lint`'s own module header
/// (`crates/beamtalk-core/src/lint/mod.rs`) says `DDD Context: Compilation`,
/// but that's stale documentation, not this list being wrong: `lint` is
/// consumed by `queries::diagnostic_provider` and nothing in Compilation
/// imports it (verified at BT-3339 landing — see the crate README), so
/// grouping it with the other Language-Service-consumed modules here matches
/// the actual dependency direction. Tracked as a doc fix in BT-3353.
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

/// Known, already-tracked violations ADR 0117 Decision step 3 (BT-3341,
/// BT-3342) removes. Keyed by (file path relative to repo root, target
/// module, *exact* `crate::...` path text of the one tracked edge) — not
/// just (file, module), so that a new, unrelated edge from the same file
/// into the same module (e.g. a second `queries::` call added later) still
/// fails instead of silently matching this entry. Remove an entry here in
/// the *same* PR that removes the corresponding edge from the code — this
/// list exists to stop *new* edges from landing while these two are still
/// open, not to launder them indefinitely. `run()` fails if an entry here no
/// longer matches a real edge, so it can't silently go stale either.
const ALLOWLIST: &[(&str, &str, &str)] = &[
    // BT-3341 (blocked by this check, BT-3339): the ADR 0103 process-
    // boundary sendability check reaches into
    // `queries::announce_sites_query::is_announce_selector` — a
    // three-string membership check, not a real query dependency. ADR 0117
    // Decision step 3 moves it into a shared leaf module below both
    // `semantic_analysis` and `queries` (pattern: `synthetic_selectors.rs`).
    (
        "crates/beamtalk-core/src/semantic_analysis/type_checker/validation.rs",
        "queries",
        "crate::queries::announce_sites_query::is_announce_selector",
    ),
];

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

fn run() -> Result<(), Vec<String>> {
    let repo_root = find_repo_root().map_err(|e| vec![e])?;
    let core_src = repo_root.join(CORE_SRC);

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

            let mut visitor = EdgeVisitor {
                test_depth: 0,
                found: Vec::new(),
            };
            visitor.visit_file(&parsed);

            for edge in visitor.found {
                if !LANGUAGE_SERVICE_MODULES.contains(&edge.target_module.as_str()) {
                    continue;
                }
                let allow_key = (
                    rel.clone(),
                    edge.target_module.clone(),
                    edge.path_text.clone(),
                );
                if ALLOWLIST
                    .iter()
                    .any(|e| allowlist_matches(e, &allow_key.0, &allow_key.1, &allow_key.2))
                {
                    allowlist_matched.insert(allow_key);
                    continue;
                }
                violations.push(format!(
                    "{rel}:{}:{}: `{}` (in Compilation module `{module}`) imports from \
                     Language Service module `{}` via `{}`",
                    edge.line, edge.column, module, edge.target_module, edge.path_text
                ));
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
    /// never sees a `crate::<module>::...` reference written inside one.
    /// This walks the raw tokens (recursing into `(...)`/`[...]`/`{...}`
    /// groups, since a macro's own delimiters and any nested call like
    /// `assert_eq!(f(crate::queries::x()), 1)` are just more tokens) looking
    /// for the `crate :: ident ( :: ident )*` pattern and records each hit
    /// exactly like a real path edge.
    fn scan_token_stream(&mut self, tokens: TokenStream) {
        let toks: Vec<TokenTree> = tokens.into_iter().collect();
        let mut i = 0;
        while i < toks.len() {
            if let TokenTree::Ident(id) = &toks[i] {
                if id == "crate" {
                    if let Some((module, path_text, span, next)) = parse_crate_path(&toks, i) {
                        self.record(&module, path_text, span);
                        i = next;
                        continue;
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
            if segments.len() >= 2 && segments[0] == "crate" {
                let module = segments[1].to_string();
                let path_text = format!(
                    "use crate::{}",
                    segments[1..]
                        .iter()
                        .map(std::string::ToString::to_string)
                        .collect::<Vec<_>>()
                        .join("::")
                );
                self.record(&module, path_text, span);
            }
        }
    }

    fn visit_path(&mut self, path: &'ast SynPath) {
        if path.segments.len() >= 2 && path.segments[0].ident == "crate" {
            let module = path.segments[1].ident.to_string();
            let path_text = path_to_string(path);
            self.record(&module, path_text, path.segments[1].ident.span());
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

/// Parses a `crate :: ident ( :: ident )*` token pattern out of a raw macro
/// token stream, starting at `toks[start]` (which must be the `crate`
/// identifier — checked by the caller). Returns the module name (the first
/// segment after `crate`), the full dotted path text (`crate::a::b::c`),
/// the span of that first segment (for diagnostics — matches how
/// [`EdgeVisitor::visit_path`] reports a real `Path` edge), and the index of
/// the next unconsumed token. Returns `None` if `crate` isn't followed by at
/// least one `:: ident` (a bare `crate` token some other way — there's no
/// such usage in this codebase, but failing closed here would just mean a
/// missed edge, not a false positive, so this is intentionally permissive
/// about what counts as "a `::` pair").
fn parse_crate_path(toks: &[TokenTree], start: usize) -> Option<(String, String, Span, usize)> {
    let mut i = start + 1;
    let mut segments: Vec<String> = Vec::new();
    let mut first_span: Option<Span> = None;
    while is_coloncolon(toks.get(i), toks.get(i + 1)) {
        let TokenTree::Ident(seg) = toks.get(i + 2)? else {
            break;
        };
        if first_span.is_none() {
            first_span = Some(seg.span());
        }
        segments.push(seg.to_string());
        i += 3;
    }
    let module = segments.first()?.clone();
    let path_text = format!("crate::{}", segments.join("::"));
    Some((module, path_text, first_span?, i))
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
            let mut full = prefix.clone();
            full.push(r.ident.clone());
            let span = r.ident.span();
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

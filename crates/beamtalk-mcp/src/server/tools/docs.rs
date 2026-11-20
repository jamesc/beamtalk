// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Docs/search tool family: `docs`, `search_examples`, `search_classes`,
//! `list_classes`, `list_packages`, `package_classes`, and `describe` — the
//! read-only tools that help an agent discover what a class or package
//! offers before touching it. `search_examples`/`search_classes` work
//! entirely offline against the bundled example corpus; the rest query the
//! live REPL.

use rmcp::{
    handler::server::wrapper::Parameters,
    model::{CallToolResult, ContentBlock},
    tool, tool_router,
};
use sha2::{Digest, Sha256};

use crate::server::params::{
    DocsParams, ListClassesParams, PackageClassesParams, SearchClassesParams, SearchExamplesParams,
};
use crate::server::{
    BeamtalkMcp, InternalErr, MCP_OUTPUT_MODE, ToolTimer, check_response, pretty_json,
};
use crate::server::{validate_class_name, validate_erlang_module_name, validate_selector};

use beamtalk_core::build_layout::BuildLayout;
use beamtalk_package::dependency_classes::dependency_layout;
use beamtalk_repl_protocol::format as fmt;
use camino::{Utf8Path, Utf8PathBuf};

use crate::client::ReplClient;

/// Discover package corpus files from the working directory's project.
///
/// Looks for `corpus.json` and `class_corpus.json` in the root package's
/// `_build/dev/` and in each dependency's checkout (`_build/dev/` first, then
/// the checkout root). Dependencies are located by
/// [`dependency_layout`] — path dependencies at their declared path, git and
/// registry ones under `_build/deps/<name>/` — the same walk `lint` uses.
///
/// Returns `(example_corpora, class_corpora)` loaded from disk.
fn discover_package_corpora() -> (
    Vec<beamtalk_examples::Corpus>,
    Vec<beamtalk_examples::ClassCorpus>,
) {
    let Ok(cwd) = std::env::current_dir() else {
        return (Vec::new(), Vec::new());
    };
    let root = beamtalk_project::discover_project_root(&cwd);
    match Utf8Path::from_path(&root) {
        Some(root) => discover_package_corpora_in(root),
        None => (Vec::new(), Vec::new()),
    }
}

/// The checkout roots of `root`'s dependencies: the declared graph
/// ([`dependency_layout`]), or — when the manifest can't be read, so nothing
/// is known about what it declares — every `_build/deps/*` checkout.
fn dependency_roots(root: &Utf8Path) -> Vec<Utf8PathBuf> {
    let layout = dependency_layout(root);
    if !layout.manifest_unreadable {
        return layout.found.into_iter().map(|dep| dep.root).collect();
    }
    let mut roots: Vec<Utf8PathBuf> = BuildLayout::new(root)
        .deps_dir()
        .read_dir_utf8()
        .into_iter()
        .flatten()
        .flatten()
        .map(|entry| entry.path().to_path_buf())
        .filter(|path| path.is_dir())
        .collect();
    roots.sort_unstable();
    roots
}

/// [`discover_package_corpora`] for an explicit project `root`.
fn discover_package_corpora_in(
    root: &Utf8Path,
) -> (
    Vec<beamtalk_examples::Corpus>,
    Vec<beamtalk_examples::ClassCorpus>,
) {
    let mut example_corpora = Vec::new();
    let mut class_corpora = Vec::new();

    // Root package corpus
    let dev_dir = BuildLayout::new(root).profile_dir();
    if let Some(corpus) = beamtalk_examples::load_corpus_from_file(
        dev_dir.as_std_path().join("corpus.json").as_path(),
    ) {
        tracing::debug!(path = %dev_dir, "Loaded root package corpus");
        example_corpora.push(corpus);
    }
    if let Some(corpus) = beamtalk_examples::load_class_corpus_from_file(
        dev_dir.as_std_path().join("class_corpus.json").as_path(),
    ) {
        tracing::debug!(path = %dev_dir, "Loaded root package class corpus");
        class_corpora.push(corpus);
    }

    // Dependency corpora. A dependency builds its corpus in its own
    // `_build/dev/` during compilation, but the corpus is also placed
    // alongside `ebin` in the checkout, so check both.
    for dep_root in dependency_roots(root) {
        let search_dirs = [BuildLayout::new(&dep_root).profile_dir(), dep_root];
        if let Some((dir, corpus)) = search_dirs.iter().find_map(|dir| {
            beamtalk_examples::load_corpus_from_file(dir.join("corpus.json").as_std_path())
                .map(|corpus| (dir, corpus))
        }) {
            tracing::debug!(path = %dir, "Loaded dependency corpus");
            example_corpora.push(corpus);
        }
        if let Some((dir, corpus)) = search_dirs.iter().find_map(|dir| {
            beamtalk_examples::load_class_corpus_from_file(
                dir.join("class_corpus.json").as_std_path(),
            )
            .map(|corpus| (dir, corpus))
        }) {
            tracing::debug!(path = %dir, "Loaded dependency class corpus");
            class_corpora.push(corpus);
        }
    }

    (example_corpora, class_corpora)
}

/// Locate `class`'s on-disk `.bt` source via a `nav-symbols` round
/// trip, then compute its divider-grouped method categories locally.
///
/// The `nav-symbols` op is the same one the LSP's `documentSymbol`
/// runtime-delegate path already sends (`crates/beamtalk-lsp/src/runtime.rs`)
/// — reused here purely to resolve `class` -> source file path; the
/// categorization itself runs in-process against `beamtalk-core`'s
/// `source_analysis::categorize_methods_in_source`, the same function the
/// static AST-walker `documentSymbol` path calls — no
/// reimplementation, no second port round trip. Returns `None` (never an
/// error) whenever nothing is computable: the class isn't in the live
/// registry, it has no source file, the file can't be read, or the class
/// can't be found/categorized in it.
///
/// Requests `scope: "user"` — only classes with a backing `.bt` file, per
/// `RequestBuilder::nav_symbols`'s doc — rather than `"all"`: a class with
/// no source file could never yield a `source_file` below regardless, so
/// the narrower scope is both the correct filter and a smaller reply to
/// pull over the wire on an image with many loaded classes.
async fn doc_method_categories(client: &ReplClient, class: &str) -> Option<serde_json::Value> {
    let response = client.nav_symbols(Some("user")).await.ok()?;
    if response.is_error() {
        return None;
    }
    let payload: beamtalk_language_service::NavSymbolsResponse =
        serde_json::from_value(response.value?).ok()?;
    let source_file = payload
        .classes
        .into_iter()
        .find(|c| c.name == class)?
        .source_file?;
    // File I/O + parsing is blocking/CPU-bound work — run it off the Tokio
    // worker thread, same as the `lint`/`diagnostic_summary` tools' own
    // `spawn_blocking` wrapping around comparable offline analysis.
    let class_owned = class.to_string();
    tokio::task::spawn_blocking(move || compute_doc_method_categories(&source_file, &class_owned))
        .await
        .ok()
        .flatten()
}

/// Pure helper behind [`doc_method_categories`]: read `source_file` and
/// categorize `class`'s methods by its `// === Name ===` section dividers,
/// returning a JSON value shaped
/// `{"class": ..., "categories": [{"name": ..|null, "methods": [{"selector",
/// "side"}]}]}` — or `None` if the file can't be read or `class` can't be
/// found/categorized in it. Split out from [`doc_method_categories`] so it
/// can be unit-tested against a fixture file with no live REPL connection,
/// matching this module's other offline-testable helpers (e.g.
/// `run_lint_structured`).
pub(crate) fn compute_doc_method_categories(
    source_file: &str,
    class: &str,
) -> Option<serde_json::Value> {
    use beamtalk_core::source_analysis::{MethodSide, categorize_methods_in_source};

    let source = std::fs::read_to_string(source_file).ok()?;
    let (result, _diagnostics) = categorize_methods_in_source(&source, class);
    let categories = result.ok()?;
    let categories_json: Vec<serde_json::Value> = categories
        .iter()
        .map(|category| {
            let methods: Vec<serde_json::Value> = category
                .methods
                .iter()
                .map(|method| {
                    let side = match method.side {
                        MethodSide::Instance => "instance",
                        MethodSide::Class => "class",
                    };
                    serde_json::json!({"selector": method.selector, "side": side})
                })
                .collect();
            serde_json::json!({"name": category.name, "methods": methods})
        })
        .collect();
    Some(serde_json::json!({"class": class, "categories": categories_json}))
}

#[tool_router(router = docs_tool_router, vis = "pub(crate)")]
impl BeamtalkMcp {
    /// Get documentation for a Beamtalk class or Erlang module.
    #[tool(
        description = "Get documentation for a Beamtalk class or Erlang module. Provide either 'class' (Beamtalk) or 'erlang_module' (Erlang FFI), and optionally a method/function selector.",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn docs(
        &self,
        Parameters(params): Parameters<DocsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start_quiet("docs");
        tracing::debug!(tool = "docs", class = ?params.class, erlang_module = ?params.erlang_module, selector = ?params.selector, "tool invoked");

        let expr = match (&params.class, &params.erlang_module) {
            (Some(class), None) => {
                validate_class_name(class)?;
                match params.selector.as_deref() {
                    Some(sel) => {
                        let sel = sel.strip_prefix('#').unwrap_or(sel);
                        validate_selector(sel)?;
                        format!("Beamtalk help: {class} selector: #{sel}")
                    }
                    None => format!("Beamtalk help: {class}"),
                }
            }
            (None, Some(module)) => {
                validate_erlang_module_name(module)?;
                match params.selector.as_deref() {
                    Some(sel) => {
                        let sel = sel.strip_prefix('#').unwrap_or(sel);
                        validate_selector(sel)?;
                        format!("Beamtalk erlangHelp: \"{module}\" selector: #{sel}")
                    }
                    None => format!("Beamtalk erlangHelp: \"{module}\""),
                }
            }
            (Some(_), Some(_)) => {
                return Err(rmcp::ErrorData::invalid_params(
                    "Provide either 'class' or 'erlang_module', not both.",
                    None,
                ));
            }
            (None, None) => {
                return Err(rmcp::ErrorData::invalid_params(
                    "Provide either 'class' (Beamtalk class) or 'erlang_module' (Erlang module).",
                    None,
                ));
            }
        };

        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .internal_err()?;

        check_response!(response, "No documentation found");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "No documentation available".to_string()
            } else {
                v
            }
        };

        let mut call_result = CallToolResult::default();
        call_result.content = vec![ContentBlock::text(text)];

        // For a whole-class lookup (no per-selector filter), also
        // attach structured, divider-grouped method-category data —
        // "structured data, not just REPL text formatting" per the surface-
        // parity contract, since `docs`'s text content above is the exact
        // same rendered string the REPL's `:help` prints. Best-effort: a
        // purely runtime-loaded class (no `.bt` source file), a class with
        // no `// === Name ===` dividers, or any lookup failure along the
        // way just leaves `structured_content` unset — never an error.
        if let (Some(class), None) = (&params.class, &params.selector) {
            call_result.structured_content = doc_method_categories(&self.client, class).await;
        }

        timer.mark_ok();
        Ok(call_result)
    }

    /// Search the bundled example corpus for Beamtalk code examples.
    #[tool(
        description = "Search for Beamtalk code examples by keyword or topic. Returns matching examples with source code, explanation, and tags. Use this to find idiomatic patterns, syntax examples, and working code before writing .bt files. Works offline — no REPL connection needed.",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn search_examples(
        &self,
        Parameters(params): Parameters<SearchExamplesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start_quiet("search_examples");
        tracing::debug!(tool = "search_examples", limit = ?params.limit, "tool invoked");
        let start = std::time::Instant::now();

        // Aggregate package corpora with the bundled corpus.
        let bundled = beamtalk_examples::bundled_corpus();
        let (pkg_corpora, _) = discover_package_corpora();
        let merged;
        let corpus_ref = if pkg_corpora.is_empty() {
            bundled.corpus()
        } else {
            merged = beamtalk_examples::merge_corpora(bundled.corpus(), &pkg_corpora);
            &merged
        };
        let results = beamtalk_examples::search_in(corpus_ref, &params.query, params.limit);
        let duration_us = start.elapsed().as_micros();

        let result_count = results.len();
        let top_score = results.first().map_or(0, |r| r.score);

        // Telemetry: hash the query for counting unique queries without exposing content.
        let hash_bytes = Sha256::digest(params.query.as_bytes());
        let query_hash = hash_bytes
            .iter()
            .fold(String::with_capacity(64), |mut acc, b| {
                use std::fmt::Write as _;
                let _ = write!(acc, "{b:02x}");
                acc
            });

        tracing::info!(
            query_hash = %query_hash,
            result_count = result_count,
            top_score = top_score,
            duration_us = duration_us,
            "search_examples"
        );
        tracing::debug!(query = %params.query, "search_examples query");

        if results.is_empty() {
            timer.mark_ok();
            // The bundled corpus is generated at build time (BT-4614); say so
            // rather than reporting "no matches" against an empty corpus.
            if let beamtalk_examples::BundledCorpus::Missing { path } = bundled {
                let location = path.as_ref().map_or_else(
                    || "this installation".to_string(),
                    |p| p.display().to_string(),
                );
                return Ok(CallToolResult::success(vec![ContentBlock::text(format!(
                    "No examples found: the bundled example corpus is missing ({location}). \
                     It is generated at build time — run `just build` in a dev checkout, or \
                     reinstall beamtalk."
                ))]));
            }
            return Ok(CallToolResult::success(vec![ContentBlock::text(
                "No examples found for that query. Try different keywords — e.g. 'closures', 'actor state', 'collections'.",
            )]));
        }

        let text = results
            .iter()
            .map(|r| {
                format!(
                    "## {} (score: {})\n**Category:** {} | **Tags:** {}\n\n```beamtalk\n{}\n```\n\n{}\n",
                    r.entry.title,
                    r.score,
                    r.entry.category,
                    r.entry.tags.join(", "),
                    r.entry.source,
                    r.entry.explanation,
                )
            })
            .collect::<Vec<_>>()
            .join("\n---\n\n");

        timer.ok_text(text)
    }

    /// Search for Beamtalk classes by keyword or concept.
    #[tool(
        description = "Search for Beamtalk classes by keyword, concept, or method name. Returns matching classes with their superclass, description, and key methods. Use this to discover which class provides a capability before using 'docs' for full details. Works offline — no REPL connection needed.",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn search_classes(
        &self,
        Parameters(params): Parameters<SearchClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start_quiet("search_classes");
        tracing::debug!(tool = "search_classes", limit = ?params.limit, "tool invoked");
        let start = std::time::Instant::now();

        // Aggregate package class corpora with the bundled corpus.
        let (_, pkg_class_corpora) = discover_package_corpora();
        let merged;
        let corpus_ref = if pkg_class_corpora.is_empty() {
            &*beamtalk_examples::class_corpus::CLASS_CORPUS
        } else {
            merged = beamtalk_examples::merge_class_corpora(
                &beamtalk_examples::class_corpus::CLASS_CORPUS,
                &pkg_class_corpora,
            );
            &merged
        };
        let results = beamtalk_examples::search_classes_in(corpus_ref, &params.query, params.limit);
        let duration_us = start.elapsed().as_micros();

        let result_count = results.len();
        let top_score = results.first().map_or(0, |r| r.score);

        let hash_bytes = Sha256::digest(params.query.as_bytes());
        let query_hash = hash_bytes
            .iter()
            .fold(String::with_capacity(64), |mut acc, b| {
                use std::fmt::Write as _;
                let _ = write!(acc, "{b:02x}");
                acc
            });

        tracing::info!(
            query_hash = %query_hash,
            result_count = result_count,
            top_score = top_score,
            duration_us = duration_us,
            "search_classes"
        );
        tracing::debug!(
            query_hash = %query_hash,
            query_len = params.query.len(),
            "search_classes query"
        );

        if results.is_empty() {
            return timer.ok_text("No classes found for that query. Try different keywords — e.g. 'http', 'collection', 'file', 'actor', 'subprocess'.",);
        }

        let text = results
            .iter()
            .map(|r| {
                let sealed = if r.entry.is_sealed { " (sealed)" } else { "" };
                let abstract_ = if r.entry.is_abstract {
                    " (abstract)"
                } else {
                    ""
                };
                let doc = r
                    .entry
                    .doc
                    .as_deref()
                    .unwrap_or("No description available.");
                let methods_display = if r.entry.methods.is_empty() {
                    "  (no methods)".to_string()
                } else {
                    r.entry
                        .methods
                        .iter()
                        .take(15)
                        .map(|m| format!("  {m}"))
                        .collect::<Vec<_>>()
                        .join("\n")
                };
                let more = if r.entry.methods.len() > 15 {
                    format!("\n  ... and {} more", r.entry.methods.len() - 15)
                } else {
                    String::new()
                };
                format!(
                    "## {}{}{} < {} (score: {})\n{}\n\n**Methods:**\n{}{}\n",
                    r.entry.name,
                    sealed,
                    abstract_,
                    r.entry.superclass,
                    r.score,
                    doc,
                    methods_display,
                    more,
                )
            })
            .collect::<Vec<_>>()
            .join("\n---\n\n");

        timer.ok_text(text)
    }

    /// List all available Beamtalk classes with one-line descriptions.
    #[tool(
        description = "List all available Beamtalk classes with one-line descriptions. Optionally filter by superclass (e.g. 'Value', 'Actor') or scope ('stdlib' for built-in classes, 'user' for user-defined).",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn list_classes(
        &self,
        Parameters(params): Parameters<ListClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start_quiet("list_classes");
        tracing::debug!(tool = "list_classes", filter = ?params.filter, "tool invoked");
        let response = self
            .client
            .list_classes(params.filter.as_deref())
            .await
            .internal_err()?;

        check_response!(response, "Failed to list classes");

        let classes = response.class_list.unwrap_or_default();
        let text = fmt::format_class_list(&classes, MCP_OUTPUT_MODE);

        timer.ok_text(text)
    }

    /// List all loaded Beamtalk packages with metadata.
    #[tool(
        description = "List all loaded Beamtalk packages with their versions, class counts, and dependencies. Returns package metadata from the runtime.",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn list_packages(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start("list_packages");

        // Get list of package names via Package all
        let names_response = self
            .client
            .evaluate_with_options("Package all", false)
            .await
            .internal_err()?;

        check_response!(names_response, "Failed to list packages");

        let value = names_response.value_string();
        if value.is_empty() || value == "nil" {
            return timer.ok_text("No packages loaded");
        }

        // For each package, get detailed info via a single Beamtalk expression
        let detail_response = self
            .client
            .evaluate_with_options(
                "Package all collect: [:name | \
                    pkg := Package named: name. \
                    name ++ \" v\" ++ (pkg at: #version) ++ \
                    \" (\" ++ (pkg at: #classes) size printString ++ \" classes)\"\
                ]",
                false,
            )
            .await
            .internal_err()?;

        let text = if detail_response.is_error() {
            // Fall back to just listing names
            value
        } else {
            detail_response.value_string()
        };

        timer.ok_text(text)
    }

    /// List all classes belonging to a named package.
    #[tool(
        description = "List all classes belonging to a named Beamtalk package (e.g. 'stdlib'). Returns the class names as a list.",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn package_classes(
        &self,
        Parameters(params): Parameters<PackageClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start_quiet("package_classes");
        let pkg = &params.package;
        tracing::debug!(tool = "package_classes", package = %pkg, "tool invoked");

        // Validate package name — no code injection
        if pkg.is_empty()
            || !pkg
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
        {
            return Err(rmcp::ErrorData::invalid_params(
                format!(
                    "Invalid package name: '{pkg}'. Must contain only alphanumeric characters, hyphens, or underscores."
                ),
                None,
            ));
        }

        let expr = format!("Package classes: \"{pkg}\"");
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .internal_err()?;

        check_response!(response, "Failed to get package classes");

        let text = response.value_string();
        if text.is_empty() || text == "#()" || text == "nil" {
            return timer.ok_text(format!(
                "No classes found in package '{pkg}' (package may not be loaded)"
            ));
        }

        timer.ok_text(text)
    }

    /// Discover supported REPL operations and protocol version.
    #[tool(
        description = "Discover supported REPL operations and protocol version. Returns the list of available ops with their parameters, and version information.",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn describe(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start("describe");
        let response = self.client.describe().await.internal_err()?;

        check_response!(response, "Describe failed");

        let mut parts = Vec::new();

        if let Some(ops) = response.ops {
            parts.push(ContentBlock::text(format!(
                "Supported operations:\n{}",
                pretty_json(&ops)
            )));
        }
        if let Some(versions) = response.versions {
            parts.push(ContentBlock::text(format!(
                "Versions: {}",
                pretty_json(&versions)
            )));
        }

        if parts.is_empty() {
            parts.push(ContentBlock::text("No describe information available"));
        }

        timer.ok_parts(parts)
    }
}

#[cfg(test)]
mod corpus_discovery_tests {
    use super::*;
    use std::fs;

    fn write(path: &Utf8Path, text: &str) {
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    fn corpus_json(id: &str) -> String {
        format!(
            r#"{{"entries":[{{"id":"{id}","title":"t","category":"c","tags":[],"source":"s","explanation":"e"}}]}}"#
        )
    }

    fn class_corpus_json(name: &str) -> String {
        format!(
            r#"[{{"name":"{name}","superclass":"Object","doc":null,"methods":[],"is_sealed":false,"is_abstract":false}}]"#
        )
    }

    fn write_package(root: &Utf8Path, name: &str, deps: &str) {
        write(
            &root.join("beamtalk.toml"),
            &format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n\n[dependencies]\n{deps}"),
        );
    }

    fn ids(corpora: &[beamtalk_examples::Corpus]) -> Vec<&str> {
        corpora
            .iter()
            .flat_map(|c| c.entries.iter().map(|e| e.id.as_str()))
            .collect()
    }

    #[test]
    fn finds_root_git_and_path_dependency_corpora() {
        let tmp = tempfile::tempdir().unwrap();
        let base = Utf8Path::from_path(tmp.path()).unwrap();
        let app = base.join("app");
        write_package(
            &app,
            "app",
            "fetched = { git = \"https://example.test/fetched\", tag = \"v1\" }\nsibling = { path = \"../sibling\" }\n",
        );
        write(
            &BuildLayout::new(&app).profile_dir().join("corpus.json"),
            &corpus_json("root"),
        );
        // A git dependency: checked out under `_build/deps/<name>/`, corpus in its `_build/dev/`.
        let fetched = BuildLayout::new(&app).dep_checkout_dir("fetched");
        write_package(&fetched, "fetched", "");
        write(
            &BuildLayout::new(&fetched).profile_dir().join("corpus.json"),
            &corpus_json("fetched"),
        );
        // A path dependency lives outside `_build/deps/`, corpus beside its manifest.
        let sibling = base.join("sibling");
        write_package(&sibling, "sibling", "");
        write(&sibling.join("corpus.json"), &corpus_json("sibling"));
        write(
            &sibling.join("class_corpus.json"),
            &class_corpus_json("SiblingClass"),
        );

        let (examples, classes) = discover_package_corpora_in(&app);

        assert_eq!(ids(&examples), ["root", "fetched", "sibling"]);
        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].entries[0].name, "SiblingClass");
    }

    #[test]
    fn unreadable_manifest_falls_back_to_fetched_checkouts() {
        let tmp = tempfile::tempdir().unwrap();
        let app = Utf8Path::from_path(tmp.path()).unwrap();
        write(&app.join("beamtalk.toml"), "{ not toml");
        let fetched = BuildLayout::new(app).dep_checkout_dir("fetched");
        write(&fetched.join("corpus.json"), &corpus_json("fetched"));

        let (examples, _) = discover_package_corpora_in(app);

        assert_eq!(ids(&examples), ["fetched"]);
    }

    #[test]
    fn ignores_checkouts_no_manifest_declares() {
        let tmp = tempfile::tempdir().unwrap();
        let app = Utf8Path::from_path(tmp.path()).unwrap();
        write_package(app, "app", "");
        let orphan = BuildLayout::new(app).dep_checkout_dir("removed");
        write_package(&orphan, "removed", "");
        write(&orphan.join("corpus.json"), &corpus_json("removed"));

        let (examples, classes) = discover_package_corpora_in(app);

        assert!(examples.is_empty());
        assert!(classes.is_empty());
    }
}

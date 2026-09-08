// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Document-symbol, folding-range, and workspace-symbol handlers.

use super::super::Backend;
use super::super::convert::{path_to_uri, span_to_folding_range, span_to_range, to_lsp_symbol};
use super::super::nav::{
    class_source_is_stdlib, runtime_class_to_document_symbol, runtime_class_to_workspace_symbol,
};
use std::collections::HashSet;
use std::path::PathBuf;

use beamtalk_language_service::{DocumentSymbolKind, LanguageService, NavSymbolClass};
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    DocumentSymbolParams, DocumentSymbolResponse, FoldingRange, FoldingRangeParams,
    SymbolInformation, SymbolKind, Url, WorkspaceSymbolParams,
};

impl Backend {
    /// Returns the document symbol outline (classes, methods, fields).
    ///
    /// Dispatches through [`Backend::delegate_nav_symbols`] as the
    /// per-file dispatcher: when `delegateToRuntime` is on and a workspace
    /// is attached, the runtime answers via `nav-symbols` (the live class
    /// registry — picks up REPL-loaded and live-edited classes the AST
    /// walker can't see) and we filter to classes whose `source_file`
    /// resolves to the requested URI's path. Otherwise we fall back to
    /// the in-process AST walker.
    ///
    /// **Bypasses the runtime path for buffers the runtime can't answer
    /// correctly**. The runtime path needs a stable
    /// `source_file` correspondence and the latest source bytes — neither
    /// holds for:
    /// * `untitled:` URIs — no `source_file` exists in the class registry
    ///   (the buffer has never been saved), so a `scope = "user"` query
    ///   filters every class out and returns an empty outline.
    /// * `beamtalk-stdlib:` URIs — virtual stdlib documents. `scope =
    ///   "user"` filters out stdlib classes by construction, so the outline
    ///   would be empty here too.
    /// * Dirty (unsaved) `file://` documents — `did_change` updates the
    ///   in-memory AST cache but never re-pushes source to the runtime, so
    ///   the runtime's reply reflects stale (last-saved / last-compiled)
    ///   bytes. The AST path uses the latest in-memory copy.
    ///
    /// For any of those, we drop straight into the AST fallback rather
    /// than asking the runtime.
    pub(in crate::server) async fn handle_document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        // AST fallback.
        // Used when the flag is off, the runtime is unreachable, the
        // runtime returns an error, or the request is for a buffer the
        // runtime can't answer correctly (see the doc comment above).
        let ast_fallback = || -> Vec<tower_lsp::lsp_types::DocumentSymbol> {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(src) = svc.file_source(&path) else {
                return Vec::new();
            };
            let symbols = svc.document_symbols(&path);
            symbols
                .into_iter()
                .map(|s| to_lsp_symbol(s, &src))
                .collect()
        };

        // Runtime path needs (a) a `file://` URI so `source_file`
        // correspondence is meaningful, and (b) a clean buffer so the
        // runtime's view of the source matches the editor's. If either
        // fails, take the AST path directly.
        let runtime_path_ok = uri.scheme() == "file" && !self.is_dirty(&path);
        if !runtime_path_ok {
            let lsp_symbols = ast_fallback();
            return if lsp_symbols.is_empty() {
                Ok(None)
            } else {
                Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
            };
        }

        let target_path = path.clone();
        let to_lsp = move |classes: Vec<NavSymbolClass>,
                           roots: &[PathBuf]|
              -> Vec<tower_lsp::lsp_types::DocumentSymbol> {
            classes
                .into_iter()
                .filter_map(|c| runtime_class_to_document_symbol(c, &target_path, roots))
                .collect()
        };

        // `scope = "user"` — `document_symbol` is per-file, so only
        // source-backed classes are reachable here. Reduces the wire
        // payload and avoids touching stdlib's huge class set.
        let lsp_symbols = self
            .delegate_nav_symbols(Some("user"), to_lsp, ast_fallback)
            .await;

        if lsp_symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
        }
    }

    /// Returns folding ranges for a file: one per `// === Name ===` section
    /// divider category, plus one per class body and one per
    /// method body, instance- and class-side.
    ///
    /// AST-only — unlike `document_symbol`, there is no runtime-delegation
    /// path here (see `docs/development/surface-parity.md`'s `nav-symbols`
    /// row, which documents this outline nesting as AST-only for the
    /// same reason: folding ranges are inter-method file structure, not a
    /// property surfaced by a loaded class with no source in hand).
    ///
    /// Per the LSP folding spec's provider-registration model,
    /// registering `folding_range_provider` at all opts every `.bt` file
    /// out of VS Code's default indentation-based folding strategy once
    /// `editor.foldingStrategy` is `"auto"` (the default) — there is no
    /// per-region merge/fallback, and a proposed `disablesIndentation`
    /// provider opt-out flag exists upstream (microsoft/vscode#265661) but
    /// has not shipped. A divider-only provider would therefore have
    /// silently regressed every divider-less file's
    /// per-class/per-method fold arrows the moment this capability was
    /// registered. To avoid that, `compute_folding_ranges` also emits a
    /// class-body range and one range per method body for **every** class,
    /// independent of whether it uses dividers — the indentation-equivalent
    /// fold points VS Code's built-in strategy would otherwise have
    /// offered. A class with dividers gets both the divider-category ranges
    /// and these body ranges (nested folding: class -> category -> method);
    /// a divider-less class gets only the body ranges, but still folds at
    /// every class/method exactly as indentation folding used to. A
    /// single-line class or method contributes no range of its own (nothing
    /// to collapse), matching indentation folding's own behavior.
    ///
    /// Returning `Ok(None)` (JSON `null`) rather than `Ok(Some(vec![]))` is
    /// load-bearing, not a style choice, for the one case that still
    /// produces zero ranges — a file with no classes, or none with anything
    /// multi-line to fold: `null` is how a `textDocument/foldingRange`
    /// response tells VS Code "this provider has nothing to say," which is
    /// what lets the editor fall back to its own indentation-based folding
    /// for that request. An empty array is a *real, if vacuous, answer* and
    /// editors are not obligated to fall back on one. Do not simplify away
    /// the `is_empty()` branch below.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::folding_range signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_folding_range(
        &self,
        params: FoldingRangeParams,
    ) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let ranges = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(src) = svc.file_source(&path) else {
                return Ok(None);
            };
            svc.folding_ranges(&path)
                .into_iter()
                .map(|span| span_to_folding_range(span, &src))
                .collect::<Vec<_>>()
        };

        if ranges.is_empty() {
            Ok(None)
        } else {
            Ok(Some(ranges))
        }
    }

    /// Returns workspace-wide class symbols matching the query.
    ///
    /// Dispatches through [`Backend::delegate_nav_symbols`]:
    /// * when `delegateToRuntime` is on and a workspace is attached, the
    ///   runtime answers via `nav-symbols` against the live class
    ///   registry — the headline win is that REPL-loaded classes with
    ///   no source file appear here (impossible with the AST/glob path
    ///   which only sees indexed `.bt` files on disk). Classes without
    ///   a `source_file` are attached to the first workspace root with
    ///   a zero-width range at (0, 0); the symbol detail carries
    ///   `(no source file)` so editors render them visibly distinct.
    /// * Otherwise — flag off, runtime unreachable, runtime error — the
    ///   fallback iterates every indexed user file and emits one
    ///   `SymbolInformation` per top-level class whose name contains the
    ///   query string (case-insensitive, substring match). Stdlib files
    ///   are excluded so the result mirrors the MCP `list_classes`
    ///   "user" scope.
    ///
    /// An empty query returns every user class; this matches MCP
    /// behaviour and the editor's "Ctrl-T with empty filter" UX.
    pub(in crate::server) async fn handle_symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query_lower = params.query.to_ascii_lowercase();

        // AST fallback path, so cold-file mode stays byte-for-byte identical
        // regardless of the runtime-delegate flag.
        let query_for_fallback = query_lower.clone();
        let ast_fallback = || -> Vec<SymbolInformation> {
            let svc = self.service.lock().expect("service lock poisoned");
            let mut out: Vec<SymbolInformation> = Vec::new();
            let files: Vec<Utf8PathBuf> = svc
                .project_index()
                .indexed_files()
                .into_iter()
                .filter(|f| !svc.project_index().is_stdlib_file(f))
                .cloned()
                .collect();
            for file in files {
                let Some(source) = svc.file_source(&file) else {
                    continue;
                };
                let symbols = svc.document_symbols(&file);
                let Some(uri) = path_to_uri(&file) else {
                    continue;
                };
                for sym in symbols {
                    if !matches!(sym.kind, DocumentSymbolKind::Class) {
                        continue;
                    }
                    let name = sym
                        .name
                        .as_str()
                        .strip_suffix(" (class)")
                        .unwrap_or(sym.name.as_str())
                        .to_string();
                    if !query_for_fallback.is_empty()
                        && !name.to_ascii_lowercase().contains(&query_for_fallback)
                    {
                        continue;
                    }
                    let range = span_to_range(sym.span, &source);
                    #[expect(
                        deprecated,
                        reason = "LSP SymbolInformation requires deprecated field"
                    )]
                    let info = SymbolInformation {
                        name,
                        kind: SymbolKind::CLASS,
                        tags: None,
                        deprecated: None,
                        location: tower_lsp::lsp_types::Location {
                            uri: uri.clone(),
                            range,
                        },
                        container_name: None,
                    };
                    out.push(info);
                }
            }
            out
        };

        // Workspace-root URI fallback used when the runtime reports a
        // class with no `source_file`. Editors that get this row see
        // the symbol but a zero-width range; clicking opens the project
        // root (or stays put if the editor declines the navigation).
        let workspace_root_uri = {
            let roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            roots.first().and_then(|p| Url::from_file_path(p).ok())
        };
        // Stdlib paths to exclude from the runtime path — mirrors the
        // AST fallback at line ~2126 which skips `is_stdlib_file`. The
        // runtime's `scope=all` returns every loaded class including
        // stdlib (`Integer`, `String`, ...); without this filter the
        // editor's Ctrl-T picker would balloon with stdlib classes that
        // were not in the cold-file result.
        let stdlib_paths: HashSet<Utf8PathBuf> = {
            let guard = self
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            guard.clone()
        };

        let to_lsp =
            move |classes: Vec<NavSymbolClass>, roots: &[PathBuf]| -> Vec<SymbolInformation> {
                classes
                    .iter()
                    .filter(|c| !class_source_is_stdlib(c, roots, &stdlib_paths))
                    .filter_map(|c| {
                        runtime_class_to_workspace_symbol(
                            c,
                            &query_lower,
                            roots,
                            workspace_root_uri.as_ref(),
                        )
                    })
                    .collect()
            };

        // `scope = "all"` — workspace/symbol's headline win is showing
        // source-less classes (REPL-loaded, dynamically built). Letting
        // the runtime return its full class list and filtering on the
        // LSP side keeps the query string locally-applied for
        // case-insensitivity parity with the AST path.
        let out = self
            .delegate_nav_symbols(Some("all"), to_lsp, ast_fallback)
            .await;

        if out.is_empty() {
            Ok(None)
        } else {
            Ok(Some(out))
        }
    }
}

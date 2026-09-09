// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Go-to-definition, find-references, and go-to-implementation handlers.

use super::super::Backend;
use super::super::convert::{
    offset_to_position, path_to_stdlib_uri, path_to_uri, span_to_range, to_bt_position,
};
use super::super::nav::{
    ast_declarations_for_cursor, merge_locations, runtime_site_to_lsp_location,
};
use std::collections::HashSet;

use beamtalk_core::source_analysis::Span;
use beamtalk_language_service::LanguageService;
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, MessageType, ReferenceParams, Url,
};

impl Backend {
    /// Navigates to the definition of the symbol at the cursor.
    ///
    /// Returns a `beamtalk-stdlib:///ClassName.bt` virtual URI for stdlib definitions,
    /// or a `file://` URI for user-defined symbols.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::goto_definition signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        // Hold svc lock only for the definition lookup; release before checking stdlib_paths.
        let (resolved, native_delegate, ffi_call) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let location = svc.goto_definition(&path, pos);
            if let Some(loc) = location {
                let delegate_info = svc.check_native_delegate(&loc);
                let target_source = svc.file_source(&loc.file);
                let resolved = target_source.map(|src| {
                    let range = span_to_range(loc.span, &src);
                    (loc.file.clone(), range)
                });
                (resolved, delegate_info, None)
            } else {
                // No Beamtalk definition found — check if cursor is on an FFI call.
                let ffi = svc.check_ffi_call(&path, pos);
                (None, None, ffi)
            }
        };

        // If this is a native delegate method, try to navigate to the backing .erl file.
        if let Some(delegate_info) = native_delegate {
            if let Some(erl_path) = self.find_erlang_source_file(&delegate_info.backing_module) {
                if let Ok(target_uri) = Url::from_file_path(&erl_path) {
                    // Resolve the matching `handle_call` clause line so the jump
                    // lands on the same clause the System Browser's native-source
                    // jump does. If the `.erl` is unreadable or the
                    // selector has no `handle_call` clause (e.g. a delegate that
                    // replies from `handle_info`), fall back to the file top.
                    let range = std::fs::read_to_string(&erl_path)
                        .ok()
                        .and_then(|content| {
                            beamtalk_language_service::queries::definition_provider::handle_call_clause_line(
                                &content,
                                &delegate_info.selector,
                            )
                        })
                        .map_or_else(tower_lsp::lsp_types::Range::default, |line| {
                            // Clause lines are 1-based; LSP lines are 0-based.
                            let lsp_line = line.saturating_sub(1);
                            tower_lsp::lsp_types::Range {
                                start: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                                end: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                            }
                        });
                    return Ok(Some(GotoDefinitionResponse::Scalar(
                        tower_lsp::lsp_types::Location {
                            uri: target_uri,
                            range,
                        },
                    )));
                }
            }
            // Fall through to .bt location if .erl file not found.
        }

        // If this is an Erlang FFI call, try to navigate to the .erl source file.
        if let Some(ffi_info) = ffi_call {
            if let Some(erl_path) = self.find_erlang_source_file(&ffi_info.module_name) {
                if let Ok(target_uri) = Url::from_file_path(&erl_path) {
                    // Use the function's source line from .beam abstract_code if available.
                    // LSP lines are 0-based; abstract_code lines are 1-based.
                    let range = ffi_info
                        .line
                        .map(|line| {
                            let lsp_line = line.saturating_sub(1);
                            tower_lsp::lsp_types::Range {
                                start: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                                end: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                            }
                        })
                        .unwrap_or_default();
                    return Ok(Some(GotoDefinitionResponse::Scalar(
                        tower_lsp::lsp_types::Location {
                            uri: target_uri,
                            range,
                        },
                    )));
                }
            }
        }

        Ok(resolved.and_then(|(file, range)| {
            let is_stdlib = {
                let stdlib_paths = self
                    .stdlib_paths
                    .lock()
                    .expect("stdlib_paths lock poisoned");
                stdlib_paths.contains(&file)
            };
            let target_uri = if is_stdlib {
                path_to_stdlib_uri(&file)?
            } else {
                path_to_uri(&file)?
            };
            Some(GotoDefinitionResponse::Scalar(
                tower_lsp::lsp_types::Location {
                    uri: target_uri,
                    range,
                },
            ))
        }))
    }

    /// Finds all references to the symbol at the cursor.
    ///
    /// Dispatches through [`Backend::delegate_nav_query`], the
    /// runtime-attached navigation seam, and closes the **declaration-merge
    /// gap** a pure senders-of lookup would otherwise leave open:
    ///
    /// * `senders_of/1` (the runtime backing for selectors) returns call
    ///   sites only — it never returns the method-definition headers.
    /// * `references_to/1` (the runtime backing for classes) returns use
    ///   sites only — it never returns the class-declaration name span.
    ///
    /// The AST walker has always returned both definitions and call sites
    /// merged into one list. So when the LSP `context.includeDeclaration`
    /// flag is `true` (the LSP default) and the runtime path is active,
    /// we overlay the AST-known declaration sites onto the runtime result.
    /// When `includeDeclaration` is `false`, we strip declarations out of
    /// the AST result so the cold-file path obeys the flag too.
    ///
    /// Behaviour matrix:
    ///
    /// | cursor on | `include_declaration` | result |
    /// |-----------|-----------------------|--------|
    /// | selector  | `true`  | runtime `senders` + AST/runtime declarations |
    /// | selector  | `false` | runtime `senders` only |
    /// | class     | `true`  | runtime `references` + AST class-decl sites |
    /// | class     | `false` | runtime `references` only |
    /// | local id  | any      | AST walker (locals have no declaration overlay — `include_declaration` is a no-op for them) |
    pub(in crate::server) async fn handle_references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };
        let include_declaration = params.context.include_declaration;

        // Compute everything that depends on `svc` up front so the lock is
        // released before any async runtime call (delegate_nav_query awaits
        // a runtime round-trip when the flag is on).
        let (pos, runtime_query, incomplete_coverage_warning) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position.position, &source);
            let runtime_query = if self.delegate_to_runtime() {
                svc.references_query_at(&path, pos)
            } else {
                None
            };
            // ADR 0108 Phase 8: find-references coverage for a
            // type alias is explicitly scoped to files compiled into the
            // current build graph — an uncompiled or never-opened file
            // contributes no reference edges until it's compiled. When
            // workspace preload hasn't finished (`!is_project_complete()`),
            // a short or empty result must not silently read as exhaustive:
            // a caller trusting it could delete an alias a not-yet-compiled
            // file still uses. Surface that via `window/showMessage` below
            // rather than staying silent.
            //
            // a plain `alias_name_at` check alone misses the case
            // where the *alias's own declaring file* hasn't been indexed yet
            // — `Foo` in `policy :: Foo` doesn't look like a known alias if
            // `type Foo = ...` hasn't been compiled, even though the cursor
            // position proves it can only be a class/protocol/alias
            // reference. `has_incomplete_reference_coverage_at` closes that
            // gap (and the equivalent one for not-yet-indexed class names)
            // without needing the name to resolve first, in a single AST
            // walk of the cursor position.
            let incomplete_coverage_warning =
                !svc.is_project_complete() && svc.has_incomplete_reference_coverage_at(&path, pos);
            (pos, runtime_query, incomplete_coverage_warning)
        };

        if incomplete_coverage_warning {
            self.client
                .show_message(
                    MessageType::WARNING,
                    "Find-references for this name may be incomplete: not all workspace files \
                     have been compiled yet, so references in uncompiled files aren't included \
                     in this result.",
                )
                .await;
        }

        let backend_self = self;
        let path_for_ast = path.clone();
        // Cold-file fallback. Builds an LSP location list from the AST
        // walker. The walker always returns definitions+calls; strip
        // declarations out when `include_declaration` is false so the
        // cold-file path mirrors the runtime-path behaviour.
        let ast_fallback = || -> Vec<tower_lsp::lsp_types::Location> {
            let svc = backend_self.service.lock().expect("service lock poisoned");
            let refs = svc.find_references(&path_for_ast, pos);
            // When the caller wants senders/uses only, subtract the AST
            // declaration set for this cursor from the merged AST results.
            let decl_set: HashSet<(Utf8PathBuf, Span)> = if include_declaration {
                HashSet::new()
            } else {
                ast_declarations_for_cursor(&svc, &path_for_ast, pos)
                    .into_iter()
                    .map(|loc| (loc.file, loc.span))
                    .collect()
            };
            refs.into_iter()
                .filter(|loc| {
                    include_declaration || !decl_set.contains(&(loc.file.clone(), loc.span))
                })
                .filter_map(|loc| {
                    let source = svc.file_source(&loc.file)?;
                    let range = span_to_range(loc.span, &source);
                    Some(tower_lsp::lsp_types::Location {
                        uri: path_to_uri(&loc.file)?,
                        range,
                    })
                })
                .collect()
        };

        let locations = match runtime_query {
            Some(query) => {
                let mut sites = self
                    .delegate_nav_query(query.clone(), runtime_site_to_lsp_location, ast_fallback)
                    .await;
                if include_declaration {
                    let decl_locs = self.declaration_sites_for_query(&query).await;
                    merge_locations(&mut sites, decl_locs);
                }
                sites
            }
            None => {
                // Cursor isn't on a selector or class name (local
                // identifier, parameter, etc.) — the runtime can't answer
                // this, so go straight to the AST path. `ast_fallback`
                // already honours `include_declaration`.
                ast_fallback()
            }
        };

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    /// Finds every class that implements the selector under the
    /// cursor (`textDocument/implementation`).
    ///
    /// When `delegateToRuntime` is on and a runtime is attached, classifies
    /// the cursor with [`SimpleLanguageService::implementors_query_at`] and
    /// forwards an `ImplementorsOf` [`NavQuery`] through
    /// [`Backend::delegate_nav_query`]; the runtime answers via
    /// `beamtalk_xref` (one site per implementing class, instance- and
    /// class-side both reported) and includes ADR-0066 extension methods
    /// the AST walker can't see.
    ///
    /// Otherwise (flag off, no workspace, runtime error), falls back to the
    /// in-process AST walker via [`SimpleLanguageService::find_implementors`].
    ///
    /// Returns `None` (LSP "no result") when the cursor isn't on a selector
    /// or when no class defines it. LSP technically distinguishes `null`
    /// from `[]` on the wire, but most editors treat both as "no jump
    /// target"; collapsing both cases to `None` keeps the handler simple
    /// and matches the existing `goto_definition` shape.
    pub(in crate::server) async fn handle_goto_implementation(
        &self,
        params: tower_lsp::lsp_types::request::GotoImplementationParams,
    ) -> Result<Option<tower_lsp::lsp_types::request::GotoImplementationResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        // Compute everything that depends on `svc` up front so the lock is
        // released before any async runtime call (delegate_nav_query awaits
        // a runtime round-trip when the flag is on). Mirrors the `references`
        // handler structure.
        let (pos, runtime_query) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let runtime_query = if self.delegate_to_runtime() {
                svc.implementors_query_at(&path, pos)
            } else {
                None
            };
            (pos, runtime_query)
        };

        let backend_self = self;
        let path_for_ast = path.clone();
        let ast_fallback = || -> Vec<tower_lsp::lsp_types::Location> {
            let svc = backend_self.service.lock().expect("service lock poisoned");
            // Resolve the selector locally so the AST fallback works when
            // the runtime flag is off (in which case `runtime_query` above
            // is `None` and the same `implementors_query_at` classification
            // is needed here). Reusing the classifier keeps cold-file and
            // runtime-attached modes in lockstep.
            let Some(query) = svc.implementors_query_at(&path_for_ast, pos) else {
                return Vec::new();
            };
            let Some(selector) = query.selector() else {
                return Vec::new();
            };
            let impls = svc.find_implementors(selector);
            impls
                .into_iter()
                .filter_map(|loc| {
                    let source = svc.file_source(&loc.file)?;
                    // Collapse to a zero-width range at the start of the
                    // method-header line, matching `runtime_site_to_lsp_location`
                    // so goto-impl selection looks identical in cold-file and
                    // runtime-attached modes.
                    let start = offset_to_position(loc.span.start() as usize, &source);
                    let line_start = tower_lsp::lsp_types::Position::new(start.line, 0);
                    let range = tower_lsp::lsp_types::Range {
                        start: line_start,
                        end: line_start,
                    };
                    Some(tower_lsp::lsp_types::Location {
                        uri: path_to_uri(&loc.file)?,
                        range,
                    })
                })
                .collect()
        };

        let locations = if let Some(query) = runtime_query {
            self.delegate_nav_query(query, runtime_site_to_lsp_location, ast_fallback)
                .await
        } else {
            // Cursor isn't on a selector — `implementorsOf:` has no answer
            // for non-selector tokens. Skip the runtime hop and run the
            // fallback (which also returns empty for the same reason).
            ast_fallback()
        };

        if locations.is_empty() {
            Ok(None)
        } else {
            // `GotoImplementationResponse` is an alias for `GotoDefinitionResponse`
            // in lsp-types; using the underlying type avoids the
            // `request::` path everywhere we construct the response.
            Ok(Some(GotoDefinitionResponse::Array(locations)))
        }
    }
}

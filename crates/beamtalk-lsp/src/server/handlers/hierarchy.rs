// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Call-hierarchy and type-hierarchy handlers.

use super::super::Backend;
use super::super::convert::{offset_to_position, path_to_uri, span_to_range, to_bt_position};
use super::super::nav::{
    SerializedCallTarget, collect_hierarchy_items, format_class_detail, outgoing_calls_for_body,
    target_to_lsp_item, type_hierarchy_item,
};
use std::path::PathBuf;

use beamtalk_language_service::{NavQuery, NavSite, nav_site_to_location};
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    Position, Range, SymbolKind, TypeHierarchyItem, TypeHierarchyPrepareParams,
    TypeHierarchySubtypesParams, TypeHierarchySupertypesParams,
};

impl Backend {
    /// `textDocument/prepareCallHierarchy` — resolve the cursor to
    /// a method-level `CallHierarchyItem` the editor can pass back to
    /// `callHierarchy/{incomingCalls,outgoingCalls}`.
    ///
    /// Cold-file classification only — `SimpleLanguageService::call_hierarchy_prepare_at`
    /// walks the open document's AST. When the hit lands on a method-
    /// definition header we record the enclosing class and class-side flag
    /// in `data` (via [`SerializedCallTarget`]) so outgoing calls can
    /// reliably locate the body to walk; call-site hits omit those fields
    /// because the receiver class is dynamic.
    ///
    /// Returns `None` (`null` to the editor) when the cursor is on a local
    /// identifier, whitespace, or any non-selector shape — VS Code falls
    /// back to no call hierarchy in that case.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::prepare_call_hierarchy signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(&uri) else {
            return Ok(None);
        };

        let item = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let Some(target) = svc.call_hierarchy_prepare_at(&path, pos) else {
                return Ok(None);
            };
            target_to_lsp_item(&target, &uri, &source)
        };

        match item {
            Some(item) => Ok(Some(vec![item])),
            None => Ok(None),
        }
    }

    /// `textDocument/prepareTypeHierarchy` — resolves the class
    /// under the cursor to a single [`TypeHierarchyItem`] the editor can
    /// then pass to [`Self::supertypes`] / [`Self::subtypes`].
    ///
    /// Cold-file only: type-hierarchy classification (selectors vs. class
    /// names vs. locals) lives in [`SimpleLanguageService::type_hierarchy_prepare_at`].
    /// The query channel (`nav-query`) is deliberately kept locked to the
    /// three navigation kinds it ships today (`senders` / `implementors` /
    /// `references`); the class-hierarchy data needed here is structural
    /// (parent edges in `ClassHierarchy`), so the AST-walker answer is
    /// strictly more complete than a runtime query would be in cold-file
    /// mode. The runtime-attached path falls back to the same AST walker
    /// when the flag is off, matching the "outgoing calls" handler's choice.
    ///
    /// Returns `None` when the cursor is not on a known class name (a
    /// selector, a local identifier, whitespace, ...). LSP's
    /// `prepareTypeHierarchy` semantics use `null` to mean "no item" — the
    /// editor then suppresses the supertypes/subtypes follow-up.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::prepare_type_hierarchy signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let prepared = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let Some((class_name, declaration)) = svc.type_hierarchy_prepare_at(&path, pos) else {
                return Ok(None);
            };
            // The clicked-on file's URI is the authoritative fallback when
            // the class itself has no indexed declaration (built-in or
            // runtime-only class). Tests rely on this — `prepareTypeHierarchy`
            // on `Object` in a user file returns an item the editor can
            // still annotate, even though `Object`'s source isn't in the
            // workspace.
            type_hierarchy_item(class_name.as_str(), declaration.as_ref(), &svc, uri)
        };

        Ok(prepared.map(|item| vec![item]))
    }

    /// `callHierarchy/incomingCalls` — who calls this method.
    ///
    /// Decodes the [`SerializedCallTarget`] from the item's `data` field
    /// to recover the selector, then dispatches through the existing
    /// `nav-query` `SendersOf` channel via [`Backend::delegate_nav_query`].
    /// The runtime path uses `beamtalk_xref:senders_of/1` (live, sees
    /// extension methods + `ChangeLog` patches); the AST fallback uses
    /// the in-process `references_provider::find_selector_references`
    /// walker — the same path used by `textDocument/references`.
    ///
    /// Each sender site becomes one `CallHierarchyIncomingCall`. Sites with
    /// no backing source file (stdlib without `source_file` metadata,
    /// dynamic / bootstrap classes) are dropped — the editor cannot
    /// navigate to them anyway.
    pub(in crate::server) async fn handle_incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        let Some(serialized) = SerializedCallTarget::from_item(&params.item) else {
            return Ok(None);
        };
        let selector = serialized.selector.clone();

        let backend_self = self;
        let display_name = serialized.selector.clone();
        let ast_fallback = move || -> Vec<CallHierarchyIncomingCall> {
            let svc = backend_self.service.lock().expect("service lock poisoned");
            // Sender sites only — `find_selector_send_sites_across_files`
            // intentionally excludes method-definition headers (see its
            // docstring) so the editor doesn't list the method itself as
            // an incoming call. This mirrors the runtime-attached path,
            // where `nav-query` `senders` returns send sites only.
            let refs = svc.find_selector_send_sites_across_files(&selector);
            refs.into_iter()
                .filter_map(|loc| {
                    let source = svc.file_source(&loc.file)?;
                    let range = span_to_range(loc.span, &source);
                    let uri = path_to_uri(&loc.file)?;
                    let from = CallHierarchyItem {
                        name: display_name.clone(),
                        kind: SymbolKind::METHOD,
                        tags: None,
                        detail: None,
                        uri,
                        range,
                        selection_range: range,
                        data: None,
                    };
                    Some(CallHierarchyIncomingCall {
                        from,
                        from_ranges: vec![range],
                    })
                })
                .collect()
        };

        let query = NavQuery::SendersOf(serialized.selector.clone().into());
        let to_lsp =
            move |site: &NavSite, roots: &[PathBuf]| -> Option<CallHierarchyIncomingCall> {
                let resolved = nav_site_to_location(site, roots)?;
                let uri = path_to_uri(&resolved.file)?;
                let line = resolved.line.checked_sub(1)?;
                let range = Range {
                    start: Position::new(line, 0),
                    end: Position::new(line, 0),
                };
                // Name the item after the *containing* method (the caller),
                // because the editor renders this as "X calls our method".
                let from = CallHierarchyItem {
                    name: site.method.to_string(),
                    kind: SymbolKind::METHOD,
                    tags: None,
                    detail: Some(format_class_detail(&site.class, site.class_side)),
                    uri,
                    range,
                    selection_range: range,
                    data: None,
                };
                Some(CallHierarchyIncomingCall {
                    from,
                    from_ranges: vec![range],
                })
            };

        let calls = self.delegate_nav_query(query, to_lsp, ast_fallback).await;
        if calls.is_empty() {
            Ok(None)
        } else {
            Ok(Some(calls))
        }
    }

    /// `callHierarchy/outgoingCalls` — what does this method call.
    ///
    /// Walks the method body's AST in-process via
    /// [`find_all_sends_in_source`]: the body source slice comes from the
    /// item's recorded file + range (the file the editor opened, the range
    /// covering the method definition). One [`CallHierarchyOutgoingCall`]
    /// per send is emitted, with the selector as the item's name and the
    /// 1-based line (translated to LSP 0-based) as the call range.
    ///
    /// Erlang FFI sends (`Erlang foo` / `(Erlang foo) bar:`) are skipped —
    /// these are Erlang function invocations through the `ErlangModule`
    /// DNU bridge, not Beamtalk message sends, so showing them as
    /// outgoing calls would shadow Beamtalk selectors with Erlang
    /// module/function names. Mirrors the same exclusion in the stdlib
    /// `unusedSelectors` query.
    ///
    /// Returns `None` when:
    /// * The item is a call-site target (no enclosing method body to walk)
    /// * The file isn't open / has no cached source
    /// * The body slice contains no sends
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::outgoing_calls signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        let Some(serialized) = SerializedCallTarget::from_item(&params.item) else {
            return Ok(None);
        };
        // A call-site hit (no class context) has no body to walk.
        if serialized.class_name.is_none() {
            return Ok(None);
        }
        let Ok(path) = Utf8PathBuf::try_from(PathBuf::from(serialized.file.as_str())) else {
            return Ok(None);
        };

        let calls = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let Some(body_slice) =
                source.get(serialized.range_start as usize..serialized.range_end as usize)
            else {
                return Ok(None);
            };
            // Re-derive the LSP line offset for the body slice so per-hit
            // ranges land at the right absolute line in the file. Method
            // definitions start at column 0 in practice (the unparser
            // emits them that way), so a column anchor is not needed —
            // the LSP item's range carries the precise span.
            let body_start_pos = offset_to_position(serialized.range_start as usize, &source);
            let Some(uri) = path_to_uri(&path) else {
                return Ok(None);
            };
            outgoing_calls_for_body(body_slice, body_start_pos, &uri)
        };

        if calls.is_empty() {
            Ok(None)
        } else {
            Ok(Some(calls))
        }
    }

    /// `typeHierarchy/supertypes` — answers "what does this class
    /// inherit from, transitively?" via [`SimpleLanguageService::supertypes_of`]
    /// (which delegates to [`ClassHierarchy::superclass_chain`]).
    ///
    /// Returns one [`TypeHierarchyItem`] per ancestor, in order from
    /// nearest parent to root. Names whose declaration site isn't indexed
    /// (built-in classes the language service hasn't loaded) still appear
    /// in the list — they carry a synthetic zero-range and the URI of the
    /// originating item, so editors can show the name even if they can't
    /// navigate to it. See [`type_hierarchy_item_for_undeclared`] for the
    /// rationale.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::supertypes signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let parent_uri = params.item.uri.clone();
        let class_name = params.item.name.clone();
        let items = {
            let svc = self.service.lock().expect("service lock poisoned");
            collect_hierarchy_items(svc.supertypes_of(class_name.as_str()), &svc, &parent_uri)
        };
        Ok(Some(items))
    }

    /// `typeHierarchy/subtypes` — answers "who inherits from this
    /// class, transitively?" via [`SimpleLanguageService::subtypes_of`]
    /// (which delegates to [`ClassHierarchy::all_subclasses`]).
    ///
    /// Order is the BFS order of `all_subclasses` — direct children first,
    /// then grandchildren, etc. Names whose declaration site isn't indexed
    /// are surfaced with [`type_hierarchy_item_for_undeclared`], same as
    /// supertypes.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::subtypes signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let parent_uri = params.item.uri.clone();
        let class_name = params.item.name.clone();
        let items = {
            let svc = self.service.lock().expect("service lock poisoned");
            collect_hierarchy_items(svc.subtypes_of(class_name.as_str()), &svc, &parent_uri)
        };
        Ok(Some(items))
    }
}

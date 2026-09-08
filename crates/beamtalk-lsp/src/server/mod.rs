// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! LSP server implementation.
//!
//! **DDD Context:** Language Service
//!
//! Delegates all IDE operations to `SimpleLanguageService` + `ProjectIndex`.
//! Maps between LSP protocol types and beamtalk language service types.
//!
//! `Backend`'s own support methods live in [`core`]; the `LanguageServer`
//! trait impl below is dispatch only — larger handler bodies are inherent
//! `impl Backend` methods grouped by capability under [`handlers`], and the
//! reload listener, flush listener, command, config, hover, and conversion
//! helpers each have their own submodule.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use serde::{Deserialize, Serialize};

use crate::runtime::RuntimeClient;

use beamtalk_language_service::{CompletionKind, LanguageService, SimpleLanguageService};
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification as LspNotification;
use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentFormattingParams, DocumentRangeFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, Documentation, ExecuteCommandParams,
    FoldingRange, FoldingRangeParams, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverContents, HoverParams, InitializeParams, InitializeResult, InitializedParams,
    MarkupContent, MarkupKind, MessageType, ParameterInformation, ParameterLabel, Range,
    ReferenceParams, SignatureHelp, SignatureHelpParams, SignatureInformation, SymbolInformation,
    TextEdit, TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams,
    TypeHierarchySupertypesParams, Url, WorkspaceEdit, WorkspaceSymbolParams,
};
use tower_lsp::{Client, LanguageServer};

mod commands;
mod config;
mod convert;
mod core;
mod flush;
mod handlers;
mod hover;
mod nav;
mod reload;

pub(in crate::server) use commands::build_command_expression;
pub(in crate::server) use config::PreloadConfig;
pub(in crate::server) use convert::{
    offset_to_position, position_to_offset, span_to_range, to_bt_position,
};
pub(in crate::server) use hover::{format_hover_documentation, stdlib_hover_policy_note};

// The rest of this file's own code never names these — they exist purely so
// `tests/`'s `use super::*;` chain (through `tests/mod.rs`'s own
// `pub use super::*;`) keeps seeing the same flat namespace the pre-split
// `server.rs` gave every test module. Each glob overlaps an explicit import
// above (e.g. `commands::build_command_expression`); Rust resolves that in
// the explicit import's favor without a conflict.
#[cfg(test)]
pub(in crate::server) use crate::runtime::FlushFileKind;
#[cfg(test)]
pub(in crate::server) use beamtalk_core::source_analysis::{Severity, Span};
#[cfg(test)]
pub(in crate::server) use beamtalk_core::unparse::format_source;
#[cfg(test)]
pub(in crate::server) use beamtalk_language_service::{CallHierarchyTarget, DocumentSymbolKind};
#[cfg(test)]
pub(in crate::server) use commands::*;
#[cfg(test)]
pub(in crate::server) use config::*;
#[cfg(test)]
pub(in crate::server) use convert::*;
#[cfg(test)]
pub(in crate::server) use ecow::EcoString;
#[cfg(test)]
pub(in crate::server) use flush::*;
#[cfg(test)]
pub(in crate::server) use hover::*;
#[cfg(test)]
pub(in crate::server) use nav::*;
#[cfg(test)]
pub(in crate::server) use reload::*;
#[cfg(test)]
pub(in crate::server) use std::path::Path;
#[cfg(test)]
pub(in crate::server) use tower_lsp::lsp_types::{
    DiagnosticSeverity, DocumentChangeOperation, DocumentChanges, ImplementationProviderCapability,
    OneOf, Position, ResourceOp, SymbolKind,
};

/// Live reload-induced diagnostics (ADR 0105 Phase 1), double-keyed
/// by document URI and then by `(owner class name, changed class name)`.
/// Both levels of keying are load-bearing, not cosmetic:
///
/// * **Owner**: Beamtalk supports multiple classes defined in one `.bt` file
///   (e.g. `behaviour.bt`, `workspace_interface.bt`), so two different caller
///   classes' reload-induced diagnostics can legitimately share a URI. A
///   flat `HashMap<Url, Vec<Diagnostic>>` would have one owner's `put`/clear
///   silently clobber a sibling owner's diagnostics in the same file.
/// * **Changed class**: a single caller can be broken by two *independently
///   reloading* classes (`Dashboard` calls both `Counter>>getCount` and
///   `Widget>>size`; each can go stale and get fixed on its own schedule).
///   Keying only by owner would have a later reload's `insert`/`remove`
///   clobber an earlier, still-valid finding from a *different* changed
///   class — mirrors `beamtalk_workspace_findings_store`'s `origin_key()`
///   (`{Owner, ChangedClass}`) server-side.
type ReloadDiagnosticsByUriAndOrigin =
    HashMap<Url, HashMap<(String, String), Vec<tower_lsp::lsp_types::Diagnostic>>>;

/// Params for the `beamtalk-lsp/fetchContent` custom request.
#[derive(Deserialize)]
pub struct FetchContentParams {
    /// A `beamtalk-stdlib:///ClassName.bt` URI identifying the file to fetch.
    pub uri: String,
}

/// Response for the `beamtalk-lsp/fetchContent` custom request.
#[derive(Debug, Serialize)]
pub struct FetchContentResult {
    /// The source content of the requested file.
    pub content: String,
}

/// Params for the `beamtalk-lsp/documentMoved` custom notification (ADR 0114
/// LSP follow-up) — see [`DocumentMoved`] for why it exists.
/// `#[serde(rename_all = "camelCase")]` matches every other LSP payload
/// (e.g. `lsp_types::RenameFile`'s `oldUri`/`newUri`), even though this type
/// isn't itself part of the upstream `lsp_types` crate.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DocumentMovedParams {
    /// The `file://` URI the class's declaration lived at before the
    /// `renameTo:`/`moveClass:to:` flush moved it. Already gone from disk by
    /// the time this notification is sent — see [`apply_rename_class_move`].
    pub old_uri: Url,
    /// The `file://` URI the class's declaration now lives at.
    pub new_uri: Url,
}

/// A custom server-to-client LSP notification (ADR 0114 LSP follow-up):
/// `beamtalk-lsp/documentMoved`, `{oldUri, newUri}`.
///
/// **Why this exists:** a `renameTo:`/`moveClass:to:` flush's
/// `workspace/applyEdit` used to include a typed `RenameFile` resource
/// operation (`old_uri -> new_uri`), so an open editor tab would follow the
/// move. By the time that edit reaches the client, though, the runtime has
/// already renamed the file on disk and unlinked `old_uri` — and VS Code's
/// own `RenameOperation.perform()` treats "target already exists, source
/// already gone" as "nothing to do" and silently skips the move step
/// entirely (no error, but no editor-state retargeting either). See
/// docs/ADR/0114-class-and-method-rename.md's LSP section for the
/// resulting design.
///
/// This notification is this project's first-party VS Code extension's
/// replacement mechanism: it doesn't ask the client to perform a filesystem
/// rename at all (there is nothing left to rename by the time it fires),
/// just tells a listening client which editor URI to retarget. The
/// extension (`editors/vscode/src/extension.ts`'s `handleDocumentMoved`,
/// mirroring the existing `beamtalk-lsp/fetchContent` custom-request
/// precedent used by `StdlibContentProvider`) closes any open tab at
/// `old_uri` and reopens `new_uri` in its place. Any other LSP client
/// ignores an unrecognised notification per the LSP spec, so it sees the
/// documented degraded (no-crash, no-retarget) outcome — the same one the
/// dropped `RenameFile` op silently produced in VS Code, just without the
/// misleading implication that a real filesystem rename request was made.
enum DocumentMoved {}

impl LspNotification for DocumentMoved {
    type Params = DocumentMovedParams;

    const METHOD: &'static str = "beamtalk-lsp/documentMoved";
}

/// LSP backend wrapping `SimpleLanguageService`.
pub struct Backend {
    /// LSP client handle for sending notifications and responses.
    client: Client,
    /// The underlying language service, protected by a mutex for concurrent
    /// access. `Arc`-wrapped (ADR 0105 Phase 1) so the detached
    /// reload-check listener task (spawned once, outlives any single
    /// request handler's `&self` borrow) can recompute *static* diagnostics
    /// and merge them with reload-induced ones when it republishes — the
    /// same reason [`Self::versions`] and [`Self::nav_cache`] are
    /// `Arc`-wrapped. `self.service.lock()` is unaffected: `Arc<Mutex<T>>`
    /// derefs to `Mutex<T>`.
    service: Arc<Mutex<SimpleLanguageService>>,
    /// Last known LSP document version by file path.
    versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>>,
    /// Per-path generation, bumped only by `did_open` (never by `did_change`)
    /// and read by `did_close` to detect a same-path reopen racing its own
    /// (I/O-bearing) disk re-index. Deliberately *not* `versions`: an
    /// ordinary `did_change` also bumps that map's value, and a version
    /// snapshot/recheck against it can't tell "this path was reopened —
    /// back off" apart from "this path was merely edited while its close was
    /// in flight — proceed, the edit is moot, we're closing regardless". A
    /// dedicated counter that only a genuine reopen touches makes that
    /// distinction unambiguous. See `did_close`'s doc for the full race.
    open_generation: Mutex<HashMap<Utf8PathBuf, u64>>,
    /// Source for `open_generation`'s values — monotonic and global (not
    /// per-path) is sufficient: `did_close` only ever compares one path's
    /// before/after value against itself, never across paths.
    next_open_generation: std::sync::atomic::AtomicU64,
    /// Paths of documents that have received `didChange` notifications since
    /// their last `didSave` / `didOpen`. The editor's in-memory copy is the
    /// source of truth for these — the on-disk bytes are stale and so is the
    /// runtime's class registry (which only sees compiled / flushed source).
    /// Used by [`Backend::document_symbol`] to bypass the runtime path when
    /// answering the outline for an unsaved buffer.
    dirty_files: Mutex<HashSet<Utf8PathBuf>>,
    /// Monotonic generation counter used to debounce `didChange` diagnostics per URI.
    diagnostic_generation: Mutex<HashMap<Url, u64>>,
    /// Deferred preload config captured at initialize and consumed after handshake.
    preload_config: Mutex<Option<PreloadConfig>>,
    /// Paths of stdlib files loaded during preload; used to emit `beamtalk-stdlib://` URIs.
    stdlib_paths: Mutex<HashSet<Utf8PathBuf>>,
    /// Workspace roots discovered at initialization; used for native delegate `.erl` lookup.
    workspace_roots: Mutex<Vec<PathBuf>>,
    /// Cached OTP lib directory (e.g., `/usr/lib/erlang/lib`); resolved once at startup.
    otp_lib_dir: Mutex<Option<PathBuf>>,
    /// ADR 0082 Phase 3: lazy-attached WebSocket client to the
    /// running workspace, used for `workspace/executeCommand` dispatch and
    /// `flush_completed` push subscription. Lives behind a `tokio` mutex so
    /// async `executeCommand` handlers can attach on demand without blocking
    /// the LSP service thread.
    runtime: tokio::sync::Mutex<Option<RuntimeClient>>,
    /// ADR 0082 Phase 3: handle to the flush-event listener task
    /// that consumes `FlushEvent`s from the runtime client and emits
    /// `workspace/applyEdit` per touched file.
    flush_listener: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// handle to the class-changed-event listener task that
    /// consumes `ClassChangedEvent`s and invalidates [`Self::nav_cache`].
    class_changed_listener: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// feature flag — when true and a runtime is attached, the
    /// LSP delegates navigation queries (find-references / implementation
    /// / call-hierarchy / type-hierarchy) to the workspace via
    /// `nav-query`. When false (the default), all nav queries use the
    /// in-process AST walker. Read once from the `initialize` params'
    /// `initializationOptions.delegateToRuntime` field and never mutated
    /// after that — atomic for cheap concurrent reads from nav handlers.
    delegate_to_runtime: std::sync::atomic::AtomicBool,
    /// per-query cache of runtime-attached nav results, keyed by
    /// query string. Invalidated wholesale on every `ClassChangedEvent`
    /// (coarse but correct for the foundation issue — per-method children
    /// can refine to per-class buckets if needed).
    ///
    /// Shared `Arc` so the class-changed listener task can hold a clone
    /// without keeping the `Backend` itself alive (the listener is aborted
    /// on `Drop` via `class_changed_listener`'s `JoinHandle`).
    nav_cache: Arc<std::sync::Mutex<NavCache>>,
    /// ADR 0105 Phase 1: handle to the reload-check-event listener
    /// task that consumes `ReloadCheckEvent`s and publishes/clears
    /// reload-induced diagnostics.
    reload_check_listener: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// ADR 0105 Phase 1: currently-live reload-induced diagnostics,
    /// keyed by document URI and then by owner class name (see
    /// [`ReloadDiagnosticsByUriAndOrigin`] for why the owner keying matters).
    /// Populated/replaced wholesale per owner by [`reload_check_listener`]
    /// (clearing-by-replacement — an owner with no current findings has its
    /// entry removed, not left stale), and merged into every
    /// [`Backend::publish_diagnostics`] call so a subsequent
    /// `didChange`-triggered republish doesn't silently drop them.
    reload_diagnostics: Arc<std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>>,
}

/// Coarse cache for runtime-attached navigation results.
///
/// The foundation issue uses a single generation counter — every
/// `ClassChangedEvent` increments it, and cache entries are tagged with
/// the generation they were computed under. A reader treats any entry
/// whose generation predates the current one as stale.
///
/// Per-method children can keep the same shape and add
/// payloads keyed by `(NavQuery, generation)` if they want memoisation;
/// the foundation issue ships the invariant (any class change ⇒ caches
/// are stale) without committing to a memoisation policy.
#[derive(Debug, Default)]
pub(crate) struct NavCache {
    /// Monotonic generation. Starts at 0; bumped on every class change.
    generation: u64,
}

impl NavCache {
    /// Read the current generation. Cache consumers stash this with each
    /// cached entry and compare on read.
    ///
    /// Foundation issue exposes the API but does not consume it directly;
    /// per-method children call this when they cache
    /// runtime results. Allowed-dead so the foundation PR doesn't have
    /// to ship a consumer.
    #[allow(dead_code, reason = "per-method children consume this API")]
    pub(crate) fn generation(&self) -> u64 {
        self.generation
    }

    /// Bump the generation — invalidates every cached entry.
    pub(crate) fn invalidate(&mut self) {
        self.generation = self.generation.saturating_add(1);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.handle_initialize(params).await
    }

    async fn initialized(&self, params: InitializedParams) {
        self.handle_initialized(params).await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.handle_shutdown().await
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.handle_did_open(params).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.handle_did_change(params).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.handle_did_close(params).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.handle_did_save(params).await;
    }

    /// Returns completion items for the cursor position.
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let items: Vec<CompletionItem> = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position.position, &source);
            svc.completions(&path, pos)
                .into_iter()
                .map(|c| CompletionItem {
                    label: c.label.to_string(),
                    kind: Some(match c.kind {
                        CompletionKind::Function => CompletionItemKind::FUNCTION,
                        CompletionKind::Variable => CompletionItemKind::VARIABLE,
                        CompletionKind::Class => CompletionItemKind::CLASS,
                        CompletionKind::Module => CompletionItemKind::MODULE,
                        CompletionKind::Keyword => CompletionItemKind::KEYWORD,
                        CompletionKind::Field => CompletionItemKind::FIELD,
                    }),
                    detail: c.detail.map(|d| d.to_string()),
                    documentation: c.documentation.map(|d| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: d.to_string(),
                        })
                    }),
                    ..Default::default()
                })
                .collect()
        };

        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    /// Returns hover information (type/docs) for the symbol at the cursor.
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let source = svc.file_source(&path);
        let Some(src) = source.as_deref() else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position_params.position, src);
        let hover = svc.hover(&path, pos);

        Ok(hover.map(|h| {
            let mut value = h.contents.to_string();
            if let Some(doc) = h.documentation {
                value.push_str("\n\n");
                value.push_str(&format_hover_documentation(&doc));
            }
            if let Some(stdlib_note) = stdlib_hover_policy_note(&svc, &value) {
                value.push_str("\n\n");
                value.push_str(&stdlib_note);
            }
            Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: Some(span_to_range(h.span, src)),
            }
        }))
    }

    /// Returns signature help for the method being called at the cursor.
    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let Some(source) = svc.file_source(&path) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position_params.position, &source);
        let help = svc.signature_help(&path, pos);

        Ok(help.map(|h| {
            let signatures = h
                .signatures
                .into_iter()
                .map(|sig| {
                    let parameters = Some(
                        sig.parameters
                            .into_iter()
                            .map(|p| ParameterInformation {
                                label: ParameterLabel::Simple(p.label.to_string()),
                                documentation: p.documentation.map(|d| {
                                    Documentation::MarkupContent(MarkupContent {
                                        kind: MarkupKind::Markdown,
                                        value: d.to_string(),
                                    })
                                }),
                            })
                            .collect(),
                    );
                    SignatureInformation {
                        label: sig.label.to_string(),
                        documentation: sig.documentation.map(|d| {
                            Documentation::MarkupContent(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: d.to_string(),
                            })
                        }),
                        parameters,
                        active_parameter: None,
                    }
                })
                .collect();
            SignatureHelp {
                signatures,
                active_signature: Some(h.active_signature),
                active_parameter: Some(h.active_parameter),
            }
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        self.handle_goto_definition(params).await
    }

    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::Location>>> {
        self.handle_references(params).await
    }

    async fn goto_implementation(
        &self,
        params: tower_lsp::lsp_types::request::GotoImplementationParams,
    ) -> Result<Option<tower_lsp::lsp_types::request::GotoImplementationResponse>> {
        self.handle_goto_implementation(params).await
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        self.handle_prepare_call_hierarchy(params).await
    }

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        self.handle_prepare_type_hierarchy(params).await
    }

    async fn incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        self.handle_incoming_calls(params).await
    }

    async fn outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        self.handle_outgoing_calls(params).await
    }

    async fn supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        self.handle_supertypes(params).await
    }

    async fn subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        self.handle_subtypes(params).await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        self.handle_document_symbol(params).await
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        self.handle_folding_range(params).await
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        self.handle_symbol(params).await
    }

    /// Formats the entire document using the Beamtalk unparser.
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        Ok(self.format_document(&params.text_document.uri))
    }

    /// Formats the document for a selected range.
    ///
    /// Beamtalk formatting is a whole-file operation (the unparser works on the
    /// full module AST), so this method formats the entire document and returns
    /// a single edit even when only a range is selected. `VSCode` accepts
    /// whole-document edits from `rangeFormatting` without issue.
    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        Ok(self.format_document(&params.text_document.uri))
    }

    /// Returns code actions available at the requested range.
    ///
    /// Currently surfaces "Add annotation: -> `ClassName`" quick-fixes for
    /// unannotated methods whose return type the `TypeChecker` can infer
    /// (ADR 0045 Phase 1b).
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };
        // Stdlib files are read-only; no code actions.
        if uri.scheme() == "beamtalk-stdlib" {
            return Ok(None);
        }

        let (source, actions) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let range = &params.range;
            let start_offset = position_to_offset(range.start, &source) as u32;
            let end_offset = position_to_offset(range.end, &source) as u32;
            let actions = svc.code_actions(&path, start_offset, end_offset);
            (source, actions)
        };

        if actions.is_empty() {
            return Ok(Some(vec![]));
        }

        let lsp_actions: CodeActionResponse = actions
            .into_iter()
            .map(|action| {
                let insert_pos = offset_to_position(action.insert_at as usize, &source);
                let edit_range = Range {
                    start: insert_pos,
                    end: insert_pos,
                };
                let text_edit = TextEdit {
                    range: edit_range,
                    new_text: action.new_text.to_string(),
                };
                let mut changes = HashMap::new();
                changes.insert(uri.clone(), vec![text_edit]);
                let lsp_action = CodeAction {
                    title: action.title.to_string(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    }),
                    is_preferred: Some(false),
                    ..Default::default()
                };
                CodeActionOrCommand::CodeAction(lsp_action)
            })
            .collect();

        Ok(Some(lsp_actions))
    }

    /// ADR 0082 Phase 3: dispatch `workspace/executeCommand`.
    ///
    /// Each LSP command compiles to a Beamtalk expression submitted via the
    /// existing `evaluate` REPL op on the attached workspace. Per ADR 0082
    /// the language is the API — there are no new workspace-side ops. This
    /// handler:
    ///
    /// 1. Looks up the command and builds the Beamtalk expression (see
    ///    [`build_command_expression`]).
    /// 2. Attaches to a running workspace lazily if not already attached (see
    ///    [`Backend::ensure_runtime_attached`]).
    /// 3. Submits the expression and returns the result value to the editor.
    ///
    /// A missing workspace (`beamtalk run .` not active) surfaces a friendly
    /// `MessageType::WARNING` log to the client and an `internal_error`
    /// JSON-RPC response so the editor's progress UI ends cleanly. Structured
    /// runtime errors (`#beamtalk_error{}`) come back through `ReplResponse`
    /// and are forwarded as the command result so editors can surface the
    /// detail to the user.
    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let expr = match build_command_expression(&params.command, &params.arguments) {
            Ok(expr) => expr,
            Err(msg) => {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("executeCommand {}: {msg}", params.command),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::invalid_params(msg));
            }
        };

        let runtime = match self.ensure_runtime_attached().await {
            Ok(client) => client,
            Err(e) => {
                let detail = format!(
                    "Beamtalk LSP could not reach a running workspace for executeCommand `{}`. \
                     Start `beamtalk run .` (or `beamtalk repl`) against this project and retry. \
                     ({e})",
                    params.command
                );
                self.client.log_message(MessageType::WARNING, &detail).await;
                let mut err = tower_lsp::jsonrpc::Error::internal_error();
                err.message = detail.into();
                return Err(err);
            }
        };

        match runtime.evaluate(&expr).await {
            Ok(response) => {
                if response.is_error() {
                    let msg = response
                        .error_message()
                        .unwrap_or("workspace evaluator returned an error");
                    self.client
                        .log_message(
                            MessageType::WARNING,
                            format!("executeCommand {}: {msg}", params.command),
                        )
                        .await;
                    // Forward structured Beamtalk evaluator errors as the
                    // command result so editors can surface the rich payload
                    // (matches the docstring above). Reserve JSON-RPC errors
                    // for transport/parameter failures.
                    let value = response.value.clone().unwrap_or_else(|| {
                        serde_json::json!({
                            "error": msg,
                            "status": response.status,
                        })
                    });
                    return Ok(Some(value));
                }
                let pretty = response.value_string();
                let value = response.value.unwrap_or(serde_json::Value::Null);
                if !pretty.is_empty() {
                    self.client
                        .log_message(
                            MessageType::INFO,
                            format!("executeCommand {}: {pretty}", params.command),
                        )
                        .await;
                }
                Ok(Some(value))
            }
            Err(e) => {
                let detail = format!("executeCommand {}: runtime error: {e}", params.command);
                self.client.log_message(MessageType::WARNING, &detail).await;
                let mut err = tower_lsp::jsonrpc::Error::internal_error();
                err.message = detail.into();
                Err(err)
            }
        }
    }
}

#[cfg(test)]
mod tests;

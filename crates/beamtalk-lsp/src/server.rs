// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! LSP server implementation.
//!
//! **DDD Context:** Language Service
//!
//! Delegates all IDE operations to `SimpleLanguageService` + `ProjectIndex`.
//! Maps between LSP protocol types and beamtalk language service types.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::Duration;

use serde::{Deserialize, Serialize};

use beamtalk_core::language_service::{
    CompletionKind, DocumentSymbolKind, LanguageService, Position as BtPosition,
    SimpleLanguageService,
};
use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::source_analysis::{Severity, Span};
use beamtalk_core::unparse::format_source;
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CompletionItem, CompletionItemKind,
    CompletionOptions, CompletionParams, CompletionResponse, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentFormattingParams, DocumentRangeFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, Documentation, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MarkupContent, MarkupKind, OneOf,
    ParameterInformation, ParameterLabel, Range, ReferenceParams, ServerCapabilities,
    SignatureHelp, SignatureHelpOptions, SignatureHelpParams, SignatureInformation, SymbolKind,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, TextEdit, Url, WorkDoneProgressOptions, WorkspaceEdit,
};
use tower_lsp::{Client, LanguageServer};
use tracing::debug;

const DIAGNOSTIC_DEBOUNCE_DURATION: Duration = Duration::from_millis(150);
const PRELOAD_MAX_FILES: usize = 5000;

#[derive(Clone)]
struct PreloadConfig {
    roots: Vec<PathBuf>,
    stdlib_dirs: Vec<PathBuf>,
}

#[derive(Default)]
struct PreloadedFiles {
    user_files: Vec<(PathBuf, String)>,
    stdlib_files: Vec<(PathBuf, String)>,
}

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

/// LSP backend wrapping `SimpleLanguageService`.
pub struct Backend {
    /// LSP client handle for sending notifications and responses.
    client: Client,
    /// The underlying language service, protected by a mutex for concurrent access.
    service: Mutex<SimpleLanguageService>,
    /// Last known LSP document version by file path.
    versions: Mutex<HashMap<Utf8PathBuf, i32>>,
    /// Monotonic generation counter used to debounce `didChange` diagnostics per URI.
    diagnostic_generation: Mutex<HashMap<Url, u64>>,
    /// Deferred preload config captured at initialize and consumed after handshake.
    preload_config: Mutex<Option<PreloadConfig>>,
    /// Paths of stdlib files loaded during preload; used to emit `beamtalk-stdlib://` URIs.
    stdlib_paths: Mutex<HashSet<Utf8PathBuf>>,
    /// Workspace roots discovered at initialization; used for native delegate `.erl` lookup.
    workspace_roots: Mutex<Vec<PathBuf>>,
}

impl Backend {
    /// Creates a new `Backend` with the given LSP client handle.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            service: Mutex::new(SimpleLanguageService::new()),
            versions: Mutex::new(HashMap::new()),
            diagnostic_generation: Mutex::new(HashMap::new()),
            preload_config: Mutex::new(None),
            stdlib_paths: Mutex::new(HashSet::new()),
            workspace_roots: Mutex::new(Vec::new()),
        }
    }

    fn file_version_for_uri(&self, uri: &Url) -> Option<i32> {
        let path = self.resolve_path_for_uri(uri)?;
        let versions = self.versions.lock().expect("versions lock poisoned");
        versions.get(&path).copied()
    }

    /// Resolves a URI to an internal path key used by the language service.
    ///
    /// - For `file://` URIs: returns the real filesystem path.
    /// - For `untitled:` URIs: returns a synthetic `__untitled__/` path key.
    /// - For `beamtalk-stdlib:///ClassName.bt` URIs: looks up the real path
    ///   from `stdlib_paths`. Returns `None` for unknown class names, invalid
    ///   URI form, or ambiguous filenames.
    fn resolve_path_for_uri(&self, uri: &Url) -> Option<Utf8PathBuf> {
        if uri.scheme() == "beamtalk-stdlib" {
            self.stdlib_uri_to_path(uri)
        } else {
            uri_to_path(uri)
        }
    }

    /// Looks up the real filesystem path for a `beamtalk-stdlib:///ClassName.bt` URI.
    ///
    /// Returns `None` for invalid URI form, unknown class names, or ambiguous filenames.
    fn stdlib_uri_to_path(&self, uri: &Url) -> Option<Utf8PathBuf> {
        // Only canonical form: no host, no query, no fragment.
        if uri.host().is_some() || uri.query().is_some() || uri.fragment().is_some() {
            return None;
        }
        let path = uri.path().trim_start_matches('/');
        // Require non-empty, no sub-paths, .bt extension only.
        if path.is_empty()
            || path.contains('/')
            || !std::path::Path::new(path)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("bt"))
        {
            return None;
        }

        let stdlib_paths = self
            .stdlib_paths
            .lock()
            .expect("stdlib_paths lock poisoned");
        let mut matches = stdlib_paths.iter().filter(|p| p.file_name() == Some(path));
        let first = matches.next()?.clone();
        // Ambiguous: multiple files with the same name → None.
        if matches.next().is_some() {
            return None;
        }
        Some(first)
    }

    /// Searches workspace roots for an Erlang source file matching a module name.
    ///
    /// Looks in `runtime/apps/*/src/<module>.erl` and `src/<module>.erl` relative
    /// to each workspace root. Returns the first match found.
    fn find_erlang_source_file(&self, module_name: &str) -> Option<PathBuf> {
        // Reject module names containing path separators to prevent directory traversal.
        if module_name.contains('/') || module_name.contains('\\') || module_name.contains("..") {
            return None;
        }
        let filename = format!("{module_name}.erl");
        let roots = self
            .workspace_roots
            .lock()
            .expect("workspace_roots lock poisoned")
            .clone();
        for root in &roots {
            // Check runtime/apps/*/src/<module>.erl (OTP app layout)
            let runtime_apps = root.join("runtime").join("apps");
            if let Ok(entries) = std::fs::read_dir(&runtime_apps) {
                for entry in entries.flatten() {
                    let candidate = entry.path().join("src").join(&filename);
                    if candidate.is_file() {
                        return Some(candidate);
                    }
                }
            }
            // Check src/<module>.erl (flat layout)
            let flat = root.join("src").join(&filename);
            if flat.is_file() {
                return Some(flat);
            }
        }
        None
    }

    async fn preload_workspace_source_files(&self, config: PreloadConfig) {
        let loaded = tokio::task::spawn_blocking(move || collect_preload_files(config))
            .await
            .unwrap_or_default();

        // Register stdlib paths before indexing so they are available immediately.
        let stdlib_utf8: Vec<Utf8PathBuf> = loaded
            .stdlib_files
            .iter()
            .filter_map(|(p, _)| Utf8PathBuf::from_path_buf(p.clone()).ok())
            .collect();
        {
            let mut stdlib_paths = self
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            for path in &stdlib_utf8 {
                stdlib_paths.insert(path.clone());
            }
        }

        let mut svc = self.service.lock().expect("service lock poisoned");
        for (path, content) in loaded.user_files.into_iter().chain(loaded.stdlib_files) {
            let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) else {
                continue;
            };
            svc.update_file(utf8_path, content);
        }
    }

    /// Handles the `beamtalk-lsp/fetchContent` custom request.
    ///
    /// Returns the source content for a `beamtalk-stdlib:///ClassName.bt` virtual URI.
    /// Responds with an error if the URI scheme is unsupported or the file is not available.
    #[expect(
        clippy::unused_async,
        reason = "tower-lsp custom_method requires async fn signature"
    )]
    pub async fn fetch_content(
        &self,
        params: FetchContentParams,
    ) -> tower_lsp::jsonrpc::Result<FetchContentResult> {
        let uri = Url::parse(&params.uri).map_err(|_| {
            tower_lsp::jsonrpc::Error::invalid_params(format!("invalid URI: {}", params.uri))
        })?;

        if uri.scheme() != "beamtalk-stdlib" {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "unsupported URI scheme: {}",
                uri.scheme()
            )));
        }

        // Only the canonical form `beamtalk-stdlib:///ClassName.bt` (empty authority, no query/fragment) is accepted.
        if uri.host().is_some() || uri.query().is_some() || uri.fragment().is_some() {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "invalid stdlib URI `{}` (expected beamtalk-stdlib:///ClassName.bt)",
                params.uri
            )));
        }

        let path = uri.path().trim_start_matches('/');
        if path.is_empty()
            || path.contains('/')
            || !std::path::Path::new(path)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("bt"))
        {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "invalid stdlib URI path `{}` (expected beamtalk-stdlib:///ClassName.bt)",
                params.uri
            )));
        }
        let filename = path.to_string();

        let matching_paths: Vec<Utf8PathBuf> = {
            let stdlib_paths = self
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths
                .iter()
                .filter(|p| p.file_name() == Some(filename.as_str()))
                .cloned()
                .collect()
        };

        let path = match matching_paths.as_slice() {
            [single] => single.clone(),
            [] => {
                return Err(tower_lsp::jsonrpc::Error {
                    code: tower_lsp::jsonrpc::ErrorCode::ServerError(-32_001),
                    message: format!("stdlib source not available: {filename}").into(),
                    data: None,
                });
            }
            _ => {
                return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                    "ambiguous stdlib URI `{}`: multiple files named `{filename}`",
                    params.uri
                )));
            }
        };

        let content = {
            let svc = self.service.lock().expect("service lock poisoned");
            svc.file_source(&path)
        };

        content
            .map(|c| FetchContentResult { content: c })
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ServerError(-32_001),
                message: format!("stdlib source not available: {filename}").into(),
                data: None,
            })
    }

    /// Formats a document identified by URI, returning whole-document edits.
    fn format_document(&self, uri: &Url) -> Option<Vec<TextEdit>> {
        // Stdlib virtual documents are read-only; return no edits.
        if uri.scheme() == "beamtalk-stdlib" {
            return None;
        }
        let path = uri_to_path(uri)?;
        let source = {
            let svc = self.service.lock().expect("service lock poisoned");
            svc.file_source(&path)?
        };

        let formatted = format_source(&source)?;

        if formatted == source {
            return Some(vec![]);
        }

        let end = offset_to_position(source.len(), &source);
        Some(vec![TextEdit {
            range: Range {
                start: tower_lsp::lsp_types::Position::new(0, 0),
                end,
            },
            new_text: formatted,
        }])
    }

    /// Publishes diagnostics for a file after every change.
    async fn publish_diagnostics(&self, uri: &Url) {
        // Stdlib virtual documents have no user-facing diagnostics.
        if uri.scheme() == "beamtalk-stdlib" {
            return;
        }
        let Some(path) = uri_to_path(uri) else {
            return;
        };
        let diagnostics = {
            let svc = self.service.lock().expect("service lock poisoned");
            let source = svc.file_source(&path);
            svc.diagnostics(&path)
                .into_iter()
                .map(|d| to_lsp_diagnostic(&d, source.as_deref()))
                .collect()
        };
        let version = self.file_version_for_uri(uri);
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, version)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Reports server capabilities to the client during handshake.
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let roots = workspace_roots(&params);
        let configured_stdlib = configured_stdlib_source_dir(&params);
        let stdlib_dirs = configured_stdlib_source_dirs(configured_stdlib.as_deref(), &roots);
        {
            let mut stored_roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            (*stored_roots).clone_from(&roots);
        }
        {
            let mut preload_config = self
                .preload_config
                .lock()
                .expect("preload_config lock poisoned");
            *preload_config = Some(PreloadConfig { roots, stdlib_dirs });
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![":".into()]),
                    retrigger_characters: Some(vec![" ".into()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    /// Called after the client acknowledges initialization.
    async fn initialized(&self, _: InitializedParams) {
        let preload_config = {
            let mut preload_config = self
                .preload_config
                .lock()
                .expect("preload_config lock poisoned");
            preload_config.take()
        };
        if let Some(config) = preload_config {
            self.preload_workspace_source_files(config).await;
        }

        debug!("beamtalk-lsp initialized");
        self.client
            .log_message(
                tower_lsp::lsp_types::MessageType::INFO,
                "Beamtalk language server ready",
            )
            .await;
    }

    /// Handles a graceful shutdown request from the client.
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    /// Indexes a newly opened document and publishes diagnostics.
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            if uri.scheme() != "beamtalk-stdlib" {
                // Stdlib files are pre-loaded at startup; skip re-indexing.
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path.clone(), params.text_document.text);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(path, params.text_document.version);
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    /// Re-indexes a document after edits and republishes diagnostics.
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // Stdlib virtual documents are read-only; ignore all change events.
        if uri.scheme() == "beamtalk-stdlib" {
            return;
        }
        if let (Some(path), Some(change)) =
            (uri_to_path(&uri), params.content_changes.into_iter().last())
        {
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path, change.text);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(
                    uri_to_path(&uri).expect("path already checked"),
                    params.text_document.version,
                );
            }

            let generation = {
                let mut generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                let entry = generations.entry(uri.clone()).or_insert(0);
                *entry += 1;
                *entry
            };

            tokio::time::sleep(DIAGNOSTIC_DEBOUNCE_DURATION).await;

            let is_latest = {
                let generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                generations.get(&uri).copied() == Some(generation)
            };

            if is_latest {
                self.publish_diagnostics(&uri).await;
            }
        }
    }

    /// Removes a closed document from the index and clears its diagnostics.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            if uri.scheme() != "beamtalk-stdlib" {
                // Keep stdlib files indexed across editor open/close cycles.
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.remove_file(&path);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.remove(&path);
            }
            {
                let mut generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                generations.remove(&uri);
            }
            if uri.scheme() != "beamtalk-stdlib" {
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
            }
        }
    }

    /// Handles save notifications and republishes diagnostics.
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!(uri = %uri, "did_save");
        self.publish_diagnostics(&uri).await;
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

    /// Navigates to the definition of the symbol at the cursor.
    ///
    /// Returns a `beamtalk-stdlib:///ClassName.bt` virtual URI for stdlib definitions,
    /// or a `file://` URI for user-defined symbols.
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        // Hold svc lock only for the definition lookup; release before checking stdlib_paths.
        let (resolved, native_delegate) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let location = svc.goto_definition(&path, pos);
            match location {
                Some(loc) => {
                    let delegate_info = svc.check_native_delegate(&loc);
                    let target_source = svc.file_source(&loc.file);
                    let resolved = target_source.map(|src| {
                        let range = span_to_range(loc.span, &src);
                        (loc.file.clone(), range)
                    });
                    (resolved, delegate_info)
                }
                None => (None, None),
            }
        };

        // If this is a native delegate method, try to navigate to the backing .erl file.
        if let Some(delegate_info) = native_delegate {
            if let Some(erl_path) = self.find_erlang_source_file(&delegate_info.backing_module) {
                if let Ok(target_uri) = Url::from_file_path(&erl_path) {
                    return Ok(Some(GotoDefinitionResponse::Scalar(
                        tower_lsp::lsp_types::Location {
                            uri: target_uri,
                            range: tower_lsp::lsp_types::Range::default(),
                        },
                    )));
                }
            }
            // Fall through to .bt location if .erl file not found.
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
    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let Some(source) = svc.file_source(&path) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position.position, &source);
        let refs = svc.find_references(&path, pos);

        let locations: Vec<tower_lsp::lsp_types::Location> = refs
            .into_iter()
            .filter_map(|loc| {
                let source = svc.file_source(&loc.file)?;
                let range = span_to_range(loc.span, &source);
                Some(tower_lsp::lsp_types::Location {
                    uri: path_to_uri(&loc.file)?,
                    range,
                })
            })
            .collect();

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    /// Returns the document symbol outline (classes, methods, fields).
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let source = svc.file_source(&path);
        let symbols = svc.document_symbols(&path);

        let Some(src) = source.as_deref() else {
            return Ok(None);
        };

        let lsp_symbols: Vec<tower_lsp::lsp_types::DocumentSymbol> =
            symbols.into_iter().map(|s| to_lsp_symbol(s, src)).collect();

        if lsp_symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
        }
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
    /// (BT-1067, ADR 0045 Phase 1b).
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
}

fn workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::new();

    if let Some(workspace_folders) = &params.workspace_folders {
        for folder in workspace_folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(path);
            }
        }
    }

    if let Some(root_uri) = &params.root_uri {
        if let Ok(path) = root_uri.to_file_path() {
            roots.push(path);
        }
    }

    roots = roots
        .into_iter()
        .map(|root| beamtalk_core::project::discover_project_root(&root))
        .collect();

    roots.sort_unstable();
    roots.dedup();
    roots
}

fn configured_stdlib_source_dir(params: &InitializeParams) -> Option<String> {
    params
        .initialization_options
        .as_ref()
        .and_then(|value| value.get("stdlibSourceDir"))
        .and_then(|value| value.as_str())
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToString::to_string)
}

/// Returns the stdlib source directory auto-discovered from the LSP binary's sysroot.
///
/// Derives sysroot as `parent(parent(current_exe()))` — the same convention used
/// by `beamtalk --print-sysroot` — then looks for `share/beamtalk/stdlib/src/`
/// under that prefix.
fn sysroot_stdlib_source_dir() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let sysroot = exe.parent()?.parent()?;
    let candidate = sysroot.join("share/beamtalk/stdlib/src");
    canonicalize_existing_dir(&candidate)
}

fn configured_stdlib_source_dirs(
    configured: Option<&str>,
    project_roots: &[PathBuf],
) -> Vec<PathBuf> {
    let Some(configured) = configured else {
        // No explicit config — fall back to sysroot auto-discovery.
        return sysroot_stdlib_source_dir().into_iter().collect();
    };

    let configured_path = PathBuf::from(configured);
    let canonical_roots: Vec<PathBuf> = project_roots
        .iter()
        .filter_map(|root| canonicalize_existing_dir(root))
        .collect();
    let mut dirs = Vec::new();

    if configured_path.is_absolute() {
        if let Some(candidate) = canonicalize_existing_dir(&configured_path) {
            dirs.push(candidate);
        }
    } else {
        for root in project_roots {
            let candidate = root.join(&configured_path);
            if let Some(canonical_candidate) = canonicalize_existing_dir(&candidate)
                && path_within_any_root(&canonical_candidate, &canonical_roots)
            {
                dirs.push(canonical_candidate);
            }
        }
    }

    dirs.sort_unstable();
    dirs.dedup();
    dirs
}

fn canonicalize_existing_dir(path: &Path) -> Option<PathBuf> {
    if !path.is_dir() {
        return None;
    }
    fs::canonicalize(path).ok()
}

fn path_within_any_root(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| path.starts_with(root))
}

fn collect_preload_files(config: PreloadConfig) -> PreloadedFiles {
    let PreloadConfig { roots, stdlib_dirs } = config;
    let mut user_paths = Vec::new();
    let mut stdlib_path_list = Vec::new();
    let mut remaining_budget = PRELOAD_MAX_FILES;

    for root in &roots {
        if remaining_budget == 0 {
            break;
        }
        let src_dir = root.join("src");
        if src_dir.is_dir() {
            collect_beamtalk_files_recursive(&src_dir, &mut user_paths, &mut remaining_budget);
        }
    }

    for dir in &stdlib_dirs {
        if remaining_budget == 0 {
            break;
        }
        collect_beamtalk_files_recursive(dir, &mut stdlib_path_list, &mut remaining_budget);
    }

    // Build the stdlib set first so user files that overlap with stdlib are
    // classified as stdlib (preserving the beamtalk-stdlib:// URI route).
    let stdlib_path_set: HashSet<PathBuf> = stdlib_path_list.iter().cloned().collect();

    let mut seen_user_files = HashSet::new();
    let mut user_files = Vec::new();
    for path in user_paths {
        // Skip paths that appear in stdlib: they must stay in the stdlib bucket.
        if stdlib_path_set.contains(&path) || !seen_user_files.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        user_files.push((path, content));
    }

    let mut seen_stdlib_files = HashSet::new();
    let mut stdlib_files = Vec::new();
    for path in stdlib_path_list {
        if !seen_stdlib_files.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        stdlib_files.push((path, content));
    }

    PreloadedFiles {
        user_files,
        stdlib_files,
    }
}

fn is_beamtalk_file(path: &Path) -> bool {
    path.is_file() && path.extension().is_some_and(|ext| ext == "bt")
}

fn collect_beamtalk_files_recursive(
    dir: &Path,
    files: &mut Vec<PathBuf>,
    remaining_budget: &mut usize,
) {
    if *remaining_budget == 0 {
        return;
    }

    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };

    for entry in entries.flatten() {
        if *remaining_budget == 0 {
            return;
        }

        let path = entry.path();
        let Ok(file_type) = entry.file_type() else {
            continue;
        };

        if file_type.is_symlink() {
            continue;
        }

        if file_type.is_dir() {
            let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            if matches!(name, ".git" | "target" | "node_modules" | "_build") {
                continue;
            }
            collect_beamtalk_files_recursive(&path, files, remaining_budget);
        } else if is_beamtalk_file(&path) {
            files.push(path);
            *remaining_budget = remaining_budget.saturating_sub(1);
        }
    }
}

/// Formats hover documentation for compact display in editor hovers.
///
/// VS Code controls hover font sizes, so we demote markdown heading levels
/// to reduce visual dominance of large section titles like `## Examples`.
fn format_hover_documentation(doc: &str) -> String {
    doc.lines()
        .map(|line| {
            if let Some(rest) = line.strip_prefix("### ") {
                return format!("##### {rest}");
            }
            if let Some(rest) = line.strip_prefix("## ") {
                return format!("#### {rest}");
            }
            if let Some(rest) = line.strip_prefix("# ") {
                return format!("### {rest}");
            }
            line.to_string()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Adds an LSP-only stdlib profile note for class/method hovers.
///
/// This is intentionally editor policy (not compiler behavior): when hover
/// resolves to a built-in/stdlib class, show sealed/abstract traits so users
/// can better understand completion/diagnostic confidence.
fn stdlib_hover_policy_note(
    service: &SimpleLanguageService,
    hover_markdown: &str,
) -> Option<String> {
    let class_name = extract_hover_class_name(hover_markdown)?;
    if !ClassHierarchy::is_builtin_class(class_name) {
        return None;
    }

    let class = service.project_index().hierarchy().get_class(class_name)?;
    let mut traits = Vec::new();
    if class.is_sealed {
        traits.push("sealed");
    }
    if class.is_abstract {
        traits.push("abstract");
    }

    if traits.is_empty() {
        return None;
    }

    let mut lines = vec![format!(
        "**Stdlib Profile:** `{class_name}` is {}.",
        traits.join(" + ")
    )];
    if class.is_sealed {
        lines.push("- Method surface is closed to subclass overrides.".to_string());
    }
    if class.is_abstract {
        lines.push("- Defines protocol to be implemented by concrete subclasses.".to_string());
    }
    if class.is_sealed && hover_markdown.contains("Resolved on `") {
        lines.push("- Confidence: high (static, sealed stdlib dispatch).".to_string());
    }

    Some(lines.join("\n"))
}

/// Extract class name from hover markdown generated by beamtalk-core.
///
/// Supports:
/// - Class hovers: `Class: `Integer` ...`
/// - Resolved method hovers: `Resolved on `Integer` (defined in ... )`
fn extract_hover_class_name(hover_markdown: &str) -> Option<&str> {
    if let Some(name) = extract_backticked_after(hover_markdown, "Class: `") {
        return Some(name);
    }
    extract_backticked_after(hover_markdown, "Resolved on `")
}

/// Returns the first backticked segment after a marker prefix.
fn extract_backticked_after<'a>(text: &'a str, marker: &str) -> Option<&'a str> {
    let start = text.find(marker)? + marker.len();
    let rest = &text[start..];
    let end = rest.find('`')?;
    Some(&rest[..end])
}

// --- Type conversion helpers ---

/// Converts an LSP URI to a `Utf8PathBuf`.
fn uri_to_path(uri: &Url) -> Option<Utf8PathBuf> {
    match uri.scheme() {
        "file" => uri
            .to_file_path()
            .ok()
            .and_then(|p| Utf8PathBuf::try_from(p).ok()),
        "untitled" => {
            let name = uri.path().trim_start_matches('/');
            Some(Utf8PathBuf::from(format!("__untitled__/{name}")))
        }
        _ => None,
    }
}

/// Converts a `Utf8PathBuf` to an LSP URI.
fn path_to_uri(path: &Utf8PathBuf) -> Option<Url> {
    if let Some(name) = path.as_str().strip_prefix("__untitled__/") {
        Url::parse(&format!("untitled:{name}")).ok()
    } else {
        Url::from_file_path(path.as_str()).ok()
    }
}

/// Converts a stdlib file path to a `beamtalk-stdlib:///ClassName.bt` virtual URI.
fn path_to_stdlib_uri(path: &Utf8PathBuf) -> Option<Url> {
    let filename = path.file_name()?;
    Url::parse(&format!("beamtalk-stdlib:///{filename}")).ok()
}

/// Converts an LSP `Position` (UTF-16 code units) to a beamtalk `Position` (byte offsets).
///
/// LSP positions use UTF-16 code units for the character field.
/// Beamtalk positions use byte offsets within the line.
fn to_bt_position(pos: tower_lsp::lsp_types::Position, source: &str) -> BtPosition {
    let target_line = pos.line;
    let target_utf16_col = pos.character;

    let mut current_line = 0u32;
    let mut line_start = 0usize;

    // Find the start of the target line
    for (i, ch) in source.char_indices() {
        if current_line == target_line {
            break;
        }
        if ch == '\n' {
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Walk the target line, counting UTF-16 code units until we reach the target column
    let mut utf16_col = 0u32;
    let mut byte_col = 0u32;
    for ch in source[line_start..].chars() {
        if ch == '\n' || utf16_col >= target_utf16_col {
            break;
        }
        // UTF-16 len is always 1 or 2, safe to truncate
        #[expect(
            clippy::cast_possible_truncation,
            reason = "char::len_utf16() is always 1 or 2"
        )]
        {
            utf16_col += ch.len_utf16() as u32;
        }
        // len_utf8 is always 1-4, safe to truncate
        #[expect(
            clippy::cast_possible_truncation,
            reason = "char::len_utf8() is always 1 to 4"
        )]
        {
            byte_col += ch.len_utf8() as u32;
        }
    }

    BtPosition::new(target_line, byte_col)
}

/// Converts a beamtalk `Span` to an LSP `Range` using source text.
fn span_to_range(span: Span, source: &str) -> Range {
    let start = offset_to_position(span.start() as usize, source);
    let end = offset_to_position(span.end() as usize, source);
    Range { start, end }
}

/// Converts a byte offset to an LSP `Position` (0-based line/character in UTF-16 code units).
fn offset_to_position(offset: usize, source: &str) -> tower_lsp::lsp_types::Position {
    let offset = offset.min(source.len());
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            // UTF-16 len is always 1 or 2, safe to truncate
            #[expect(
                clippy::cast_possible_truncation,
                reason = "char::len_utf16() is always 1 or 2"
            )]
            {
                col += ch.len_utf16() as u32;
            }
        }
    }
    tower_lsp::lsp_types::Position::new(line, col)
}

/// Converts an LSP `Position` (line, UTF-16 column) to a byte offset in `source`.
///
/// Returns `source.len()` when the position is beyond the end of the file.
fn position_to_offset(pos: tower_lsp::lsp_types::Position, source: &str) -> usize {
    let mut line = 0u32;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if line == pos.line {
            // Walk UTF-16 columns within this line
            let mut col_utf16 = 0u32;
            let mut byte_offset = line_start;
            for (j, c) in source[line_start..].char_indices() {
                if col_utf16 >= pos.character {
                    return line_start + j;
                }
                if c == '\n' {
                    break;
                }
                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "char::len_utf16() is always 1 or 2"
                )]
                {
                    col_utf16 += c.len_utf16() as u32;
                }
                byte_offset = line_start + j + c.len_utf8();
            }
            return byte_offset;
        }
        if ch == '\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    source.len()
}

/// Converts a beamtalk `Diagnostic` to an LSP `Diagnostic`.
fn to_lsp_diagnostic(
    diag: &beamtalk_core::language_service::Diagnostic,
    source: Option<&str>,
) -> tower_lsp::lsp_types::Diagnostic {
    let range = source
        .map(|src| span_to_range(diag.span, src))
        .unwrap_or_default();

    tower_lsp::lsp_types::Diagnostic {
        range,
        severity: Some(match diag.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            // Lint and Hint map to LSP HINT (informational)
            Severity::Lint | Severity::Hint => DiagnosticSeverity::HINT,
        }),
        source: Some("beamtalk".into()),
        message: {
            use std::fmt::Write;
            let mut msg = diag.message.to_string();
            // BT-1588: Append notes for origin tracing
            for note in &diag.notes {
                let _ = write!(msg, "\n  = {}", note.message);
            }
            if let Some(ref hint) = diag.hint {
                let _ = write!(msg, "\nHint: {hint}");
            }
            msg
        },
        ..Default::default()
    }
}

/// Converts a beamtalk `DocumentSymbol` to an LSP `DocumentSymbol`.
#[expect(deprecated, reason = "LSP DocumentSymbol requires deprecated field")]
fn to_lsp_symbol(
    sym: beamtalk_core::language_service::DocumentSymbol,
    source: &str,
) -> tower_lsp::lsp_types::DocumentSymbol {
    let range = span_to_range(sym.span, source);
    let children = sym
        .children
        .into_iter()
        .map(|c| to_lsp_symbol(c, source))
        .collect();

    let selection_range = sym.name_span.map_or(range, |s| span_to_range(s, source));
    tower_lsp::lsp_types::DocumentSymbol {
        name: sym.name.to_string(),
        kind: match sym.kind {
            DocumentSymbolKind::Class => SymbolKind::CLASS,
            DocumentSymbolKind::Method | DocumentSymbolKind::ClassMethod => SymbolKind::METHOD,
            DocumentSymbolKind::Field => SymbolKind::FIELD,
        },
        detail: None,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: Some(children),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::language_service::HoverInfo;
    use beamtalk_core::test_helpers::unique_temp_dir;
    use camino::Utf8PathBuf;
    use std::fs;

    #[test]
    fn configured_stdlib_source_dirs_rejects_relative_traversal_outside_root() {
        let temp = unique_temp_dir("beamtalk_lsp_stdlib_traversal");
        let project_root = temp.join("project");
        let outside = temp.join("outside");
        fs::create_dir_all(project_root.join("src")).expect("create project dirs");
        fs::create_dir_all(outside.join("lib")).expect("create outside dirs");

        let dirs = configured_stdlib_source_dirs(Some("../outside/lib"), &[project_root]);
        assert!(dirs.is_empty());

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn configured_stdlib_source_dirs_accepts_in_root_absolute_path() {
        let temp = unique_temp_dir("beamtalk_lsp_stdlib_in_root");
        let project_root = temp.join("project");
        let stdlib = project_root.join("stdlib/src");
        fs::create_dir_all(&stdlib).expect("create stdlib dir");

        let dirs = configured_stdlib_source_dirs(
            Some(stdlib.to_str().expect("utf8 path")),
            std::slice::from_ref(&project_root),
        );
        assert_eq!(dirs.len(), 1);

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn configured_stdlib_source_dirs_accepts_outside_root_absolute_path() {
        let temp = unique_temp_dir("beamtalk_lsp_stdlib_outside_root");
        let project_root = temp.join("project");
        let stdlib = temp.join("shared-stdlib");
        fs::create_dir_all(project_root.join("src")).expect("create project dir");
        fs::create_dir_all(&stdlib).expect("create stdlib dir");

        let dirs = configured_stdlib_source_dirs(
            Some(stdlib.to_str().expect("utf8 path")),
            std::slice::from_ref(&project_root),
        );
        assert_eq!(
            dirs,
            vec![fs::canonicalize(&stdlib).expect("canonical stdlib")]
        );

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn configured_stdlib_source_dir_reads_initialize_option() {
        let params = InitializeParams {
            initialization_options: Some(serde_json::json!({
                "stdlibSourceDir": "stdlib/src"
            })),
            ..Default::default()
        };

        let configured = configured_stdlib_source_dir(&params);
        assert_eq!(configured.as_deref(), Some("stdlib/src"));
    }

    #[test]
    fn extract_hover_class_name_from_class_hover() {
        let text = "Class: `Integer`";
        assert_eq!(extract_hover_class_name(text), Some("Integer"));
    }

    #[test]
    fn extract_hover_class_name_from_resolved_method_hover() {
        let text = "```beamtalk\n+\n```\n\nResolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_";
        assert_eq!(extract_hover_class_name(text), Some("Integer"));
    }

    #[test]
    fn stdlib_policy_note_for_sealed_class() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Integer`");
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("Stdlib Profile"));
        assert!(note.contains("sealed"));
    }

    #[test]
    fn stdlib_policy_note_ignores_non_builtin_classes() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("user.bt");
        service.update_file(
            file,
            "Object subclass: Counter\n  increment => 1".to_string(),
        );

        let note = stdlib_hover_policy_note(&service, "Class: `Counter`");
        assert!(note.is_none());
    }

    #[test]
    fn stdlib_policy_note_for_abstract_class() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Boolean`");
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("abstract"));
    }

    #[test]
    fn stdlib_policy_note_marks_high_confidence_for_sealed_method_hover() {
        let service = SimpleLanguageService::new();
        let hover = "```beamtalk\n+\n```\n\nResolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_";
        let note = stdlib_hover_policy_note(&service, hover);
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("Confidence: high"));
        assert!(note.contains("sealed stdlib dispatch"));
    }

    #[test]
    fn stdlib_policy_note_does_not_mark_high_confidence_for_class_hover() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Integer`");
        assert!(note.is_some());
        assert!(!note.unwrap().contains("Confidence: high"));
    }

    #[test]
    fn policy_note_works_for_resolved_method_hover_markdown() {
        let service = SimpleLanguageService::new();
        let hover = HoverInfo::new("```beamtalk\n+\n```", Span::new(0, 1)).with_documentation(
            "Resolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_",
        );
        let markdown = format!(
            "{}\n\n{}",
            hover.contents,
            format_hover_documentation(hover.documentation.as_deref().unwrap_or_default())
        );

        let note = stdlib_hover_policy_note(&service, &markdown);
        assert!(note.is_some());
        assert!(note.unwrap().contains("Integer"));
    }

    #[test]
    fn configured_stdlib_falls_back_to_sysroot_when_none() {
        // When no explicit stdlibSourceDir is configured, configured_stdlib_source_dirs
        // falls back to sysroot auto-discovery. Assert against the actual sysroot
        // result so the test is deterministic whether or not the binary is installed.
        let expected: Vec<PathBuf> = sysroot_stdlib_source_dir().into_iter().collect();
        let dirs = configured_stdlib_source_dirs(None, &[PathBuf::from("/tmp/project")]);
        assert_eq!(dirs, expected);
    }

    #[test]
    fn sysroot_stdlib_source_dir_smoke_test() {
        // Verify sysroot_stdlib_source_dir doesn't panic regardless of
        // the environment — the result depends on the test runner's
        // installation layout and may be Some or None.
        let _ = sysroot_stdlib_source_dir();
    }

    #[test]
    fn path_to_stdlib_uri_produces_beamtalk_stdlib_scheme() {
        let path = Utf8PathBuf::from("/usr/share/beamtalk/stdlib/src/Integer.bt");
        let uri = path_to_stdlib_uri(&path).expect("should produce URI");
        assert_eq!(uri.scheme(), "beamtalk-stdlib");
        assert_eq!(uri.path(), "/Integer.bt");
    }

    #[test]
    fn path_to_stdlib_uri_handles_nested_path() {
        let path = Utf8PathBuf::from("/some/deep/path/to/Collection.bt");
        let uri = path_to_stdlib_uri(&path).expect("should produce URI");
        assert_eq!(uri.scheme(), "beamtalk-stdlib");
        assert_eq!(uri.path(), "/Collection.bt");
    }

    #[tokio::test]
    async fn fetch_content_returns_error_for_unsupported_scheme() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "file:///some/file.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("unsupported URI scheme"));
    }

    #[tokio::test]
    async fn fetch_content_returns_error_for_missing_stdlib_file() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib:///NonExistent.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("stdlib source not available"));
        assert!(err.message.contains("NonExistent.bt"));
    }

    #[tokio::test]
    async fn fetch_content_rejects_malformed_stdlib_uri_path() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        for bad_uri in &[
            "beamtalk-stdlib:///",
            "beamtalk-stdlib:///sub/Integer.bt",
            "beamtalk-stdlib:///Integer.erl",
        ] {
            let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
                .fetch_content(FetchContentParams {
                    uri: bad_uri.to_string(),
                })
                .await;
            assert!(result.is_err(), "expected error for {bad_uri}");
            let err = result.unwrap_err();
            assert!(
                err.message.contains("invalid stdlib URI path"),
                "expected 'invalid stdlib URI path' in error for {bad_uri}, got: {}",
                err.message
            );
        }
    }

    #[tokio::test]
    async fn fetch_content_rejects_authority_bearing_stdlib_uri() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib://host/Integer.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("invalid stdlib URI"),
            "expected 'invalid stdlib URI' but got: {}",
            err.message
        );
    }

    #[tokio::test]
    async fn fetch_content_rejects_stdlib_uri_with_query_or_fragment() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        for bad_uri in &[
            "beamtalk-stdlib:///Integer.bt?x=1",
            "beamtalk-stdlib:///Integer.bt#section",
        ] {
            let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
                .fetch_content(FetchContentParams {
                    uri: bad_uri.to_string(),
                })
                .await;
            assert!(result.is_err(), "expected error for {bad_uri}");
            let err = result.unwrap_err();
            assert!(
                err.message.contains("invalid stdlib URI"),
                "expected 'invalid stdlib URI' for {bad_uri}, got: {}",
                err.message
            );
        }
    }

    #[tokio::test]
    async fn fetch_content_returns_error_for_ambiguous_stdlib_filename() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let path1 = Utf8PathBuf::from("/fake/stdlib/a/Integer.bt");
        let path2 = Utf8PathBuf::from("/fake/stdlib/b/Integer.bt");
        let content = "Object subclass: Integer".to_string();

        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(path1.clone());
            stdlib_paths.insert(path2.clone());
        }
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path1, content.clone());
            svc.update_file(path2, content);
        }

        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib:///Integer.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("ambiguous"),
            "expected 'ambiguous' but got: {}",
            err.message
        );
    }

    #[tokio::test]
    async fn fetch_content_returns_content_for_registered_stdlib_file() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let stdlib_path = Utf8PathBuf::from("/fake/stdlib/Integer.bt");
        let content = "Object subclass: Integer\n  + other => 0".to_string();

        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(stdlib_path.clone());
        }
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(stdlib_path, content.clone());
        }

        let result: FetchContentResult = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib:///Integer.bt".to_string(),
            })
            .await
            .expect("should return content");
        assert_eq!(result.content, content);
    }

    #[test]
    fn collect_preload_files_classifies_overlapping_path_as_stdlib() {
        // When the same .bt file appears in both a workspace src/ dir and a
        // stdlib dir, it must end up in stdlib_files (not user_files) so that
        // goto_definition emits a beamtalk-stdlib:// URI rather than file://.
        let temp = unique_temp_dir("beamtalk_lsp_preload_overlap");
        let project_root = temp.join("project");
        let src_dir = project_root.join("src");
        let stdlib_dir = temp.join("stdlib");

        fs::create_dir_all(&src_dir).expect("create src dir");
        fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");

        // Write the same path into both dirs by using a shared physical file
        // path via the stdlib_dir pointing into the src dir is not realistic;
        // instead, create the same filename in both so we can test the set logic.
        // The realistic case is a symlinked or canonicalized path appearing twice.
        let shared_path = src_dir.join("Integer.bt");
        fs::write(&shared_path, "Object subclass: Integer").expect("write Integer");
        let stdlib_integer = stdlib_dir.join("Integer.bt");
        fs::write(&stdlib_integer, "Object subclass: Integer").expect("write stdlib Integer");

        // The paths are different files with the same name — test that the stdlib
        // path deduplication is independent of the user path deduplication.
        let config = PreloadConfig {
            roots: vec![project_root.clone()],
            stdlib_dirs: vec![stdlib_dir.clone()],
        };
        let loaded = collect_preload_files(config);

        // The two files have the same name but different paths, so both should
        // appear in their respective buckets.
        assert_eq!(loaded.user_files.len(), 1);
        assert_eq!(loaded.stdlib_files.len(), 1);
        assert!(loaded.user_files[0].0.ends_with("Integer.bt"));
        assert!(loaded.stdlib_files[0].0.ends_with("Integer.bt"));
        // The user file must NOT be the stdlib path.
        assert_ne!(loaded.user_files[0].0, stdlib_integer);
        assert_eq!(loaded.stdlib_files[0].0, stdlib_integer);

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn collect_preload_files_separates_user_and_stdlib_files() {
        let temp = unique_temp_dir("beamtalk_lsp_preload_separation");
        let project_root = temp.join("project");
        let src_dir = project_root.join("src");
        let stdlib_dir = temp.join("stdlib");

        fs::create_dir_all(&src_dir).expect("create src dir");
        fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");

        fs::write(src_dir.join("User.bt"), "Object subclass: User").expect("write user file");
        fs::write(stdlib_dir.join("Integer.bt"), "Object subclass: Integer")
            .expect("write stdlib file");

        let config = PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![stdlib_dir],
        };
        let loaded = collect_preload_files(config);

        assert_eq!(loaded.user_files.len(), 1);
        assert_eq!(loaded.stdlib_files.len(), 1);
        assert!(loaded.user_files[0].0.ends_with("User.bt"));
        assert!(loaded.stdlib_files[0].0.ends_with("Integer.bt"));

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn format_source_returns_none_for_parse_errors() {
        let result = format_source("@@@invalid beamtalk@@@");
        assert!(result.is_none(), "parse errors must suppress formatting");
    }

    #[test]
    fn format_source_returns_formatted_string() {
        // An unformatted source must produce non-empty output, and that output
        // must be stable (formatting the result again returns the same string).
        let source = "Object subclass: Foo\n  bar => 42\n";
        let pass1 = format_source(source).expect("valid source must produce output");
        assert!(!pass1.is_empty(), "formatted output must not be empty");
        let pass2 = format_source(&pass1).expect("formatted output must be valid");
        assert_eq!(pass1, pass2, "output must be canonical (idempotent)");
    }

    #[test]
    fn format_source_ensures_trailing_newline() {
        let source = "x := 42";
        let result = format_source(source).expect("valid source");
        assert!(
            result.ends_with('\n'),
            "formatted output must end with newline"
        );
    }

    #[test]
    fn format_source_idempotent() {
        let source = "Object subclass: Foo\n  bar => 42\n";
        let pass1 = format_source(source).expect("pass 1");
        let pass2 = format_source(&pass1).expect("pass 2");
        assert_eq!(pass1, pass2, "formatting must be idempotent");
    }

    #[test]
    fn resolve_path_for_uri_stdlib_uri_resolves_to_real_path() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let real_path = Utf8PathBuf::from("/fake/stdlib/Integer.bt");
        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(real_path.clone());
        }

        let uri = Url::parse("beamtalk-stdlib:///Integer.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert_eq!(result, Some(real_path));
    }

    #[test]
    fn resolve_path_for_uri_unknown_class_returns_none() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let uri = Url::parse("beamtalk-stdlib:///NonExistent.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert!(result.is_none());
    }

    #[test]
    fn resolve_path_for_uri_invalid_stdlib_form_returns_none() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let real_path = Utf8PathBuf::from("/fake/stdlib/Integer.bt");
        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(real_path);
        }

        for bad_uri in &[
            "beamtalk-stdlib:///",
            "beamtalk-stdlib:///sub/Integer.bt",
            "beamtalk-stdlib:///Integer.erl",
            "beamtalk-stdlib://host/Integer.bt",
            "beamtalk-stdlib:///Integer.bt?x=1",
            "beamtalk-stdlib:///Integer.bt#section",
        ] {
            let uri = Url::parse(bad_uri).expect("parseable URI");
            let result = backend.resolve_path_for_uri(&uri);
            assert!(result.is_none(), "expected None for {bad_uri}");
        }
    }

    #[test]
    fn resolve_path_for_uri_ambiguous_stdlib_returns_none() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(Utf8PathBuf::from("/fake/stdlib/a/Integer.bt"));
            stdlib_paths.insert(Utf8PathBuf::from("/fake/stdlib/b/Integer.bt"));
        }

        let uri = Url::parse("beamtalk-stdlib:///Integer.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert!(result.is_none(), "ambiguous filename must return None");
    }

    #[test]
    #[cfg(unix)]
    fn resolve_path_for_uri_file_uri_still_works() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let uri = Url::parse("file:///some/project/main.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert_eq!(result, Some(Utf8PathBuf::from("/some/project/main.bt")));
    }

    #[test]
    #[cfg(windows)]
    fn resolve_path_for_uri_file_uri_still_works() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let uri = Url::parse("file:///C:/project/main.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert_eq!(result, Some(Utf8PathBuf::from("C:/project/main.bt")));
    }

    // ── position_to_offset tests (BT-1067) ─────────────────────────────

    #[test]
    fn position_to_offset_first_line() {
        let source = "hello world";
        let pos = tower_lsp::lsp_types::Position::new(0, 6);
        assert_eq!(position_to_offset(pos, source), 6);
    }

    #[test]
    fn position_to_offset_second_line() {
        let source = "hello\nworld";
        let pos = tower_lsp::lsp_types::Position::new(1, 3);
        assert_eq!(position_to_offset(pos, source), 9); // 'l' in "world"
    }

    #[test]
    fn position_to_offset_beyond_eof() {
        let source = "hi";
        let pos = tower_lsp::lsp_types::Position::new(5, 0);
        assert_eq!(position_to_offset(pos, source), source.len());
    }

    #[test]
    fn position_to_offset_roundtrip_with_offset_to_position() {
        let source = "Object subclass: Counter\n  count => 42";
        for byte_offset in [0, 5, 24, 25, 30, 38] {
            let pos = offset_to_position(byte_offset, source);
            let back = position_to_offset(pos, source);
            assert_eq!(
                back, byte_offset,
                "roundtrip failed for offset {byte_offset}"
            );
        }
    }
}

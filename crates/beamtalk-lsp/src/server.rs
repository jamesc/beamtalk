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

use beamtalk_core::language_service::{
    CompletionKind, DocumentSymbolKind, LanguageService, Position as BtPosition,
    SimpleLanguageService,
};
use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::source_analysis::{Severity, Span};
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbolParams,
    DocumentSymbolResponse, Documentation, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, MarkupContent, MarkupKind, OneOf, ParameterInformation, ParameterLabel,
    Range, ReferenceParams, ServerCapabilities, SignatureHelp, SignatureHelpOptions,
    SignatureHelpParams, SignatureInformation, SymbolKind, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Url,
    WorkDoneProgressOptions,
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
        }
    }

    fn file_version_for_uri(&self, uri: &Url) -> Option<i32> {
        let path = uri_to_path(uri)?;
        let versions = self.versions.lock().expect("versions lock poisoned");
        versions.get(&path).copied()
    }

    async fn preload_workspace_source_files(&self, config: PreloadConfig) {
        let loaded_files = tokio::task::spawn_blocking(move || collect_preload_files(config))
            .await
            .unwrap_or_default();

        let mut svc = self.service.lock().expect("service lock poisoned");
        for (path, content) in loaded_files {
            let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) else {
                continue;
            };
            svc.update_file(utf8_path, content);
        }
    }

    /// Publishes diagnostics for a file after every change.
    async fn publish_diagnostics(&self, uri: &Url) {
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
        if let Some(path) = uri_to_path(&uri) {
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path, params.text_document.text);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(
                    uri_to_path(&uri).expect("path already checked"),
                    params.text_document.version,
                );
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    /// Re-indexes a document after edits and republishes diagnostics.
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
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
        if let Some(path) = uri_to_path(&uri) {
            {
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
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
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
        let Some(path) = uri_to_path(uri) else {
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
        let Some(path) = uri_to_path(uri) else {
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
        let Some(path) = uri_to_path(uri) else {
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
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = uri_to_path(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let Some(source) = svc.file_source(&path) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position_params.position, &source);
        let location = svc.goto_definition(&path, pos);

        Ok(location.and_then(|loc| {
            let target_source = svc.file_source(&loc.file)?;
            let range = span_to_range(loc.span, &target_source);
            Some(GotoDefinitionResponse::Scalar(
                tower_lsp::lsp_types::Location {
                    uri: path_to_uri(&loc.file)?,
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
        let Some(path) = uri_to_path(uri) else {
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
        let Some(path) = uri_to_path(uri) else {
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

fn configured_stdlib_source_dirs(
    configured: Option<&str>,
    project_roots: &[PathBuf],
) -> Vec<PathBuf> {
    let Some(configured) = configured else {
        return Vec::new();
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

fn collect_preload_files(config: PreloadConfig) -> Vec<(PathBuf, String)> {
    let PreloadConfig { roots, stdlib_dirs } = config;
    let mut files_to_index = Vec::new();
    let mut seen_files = HashSet::new();
    let mut remaining_budget = PRELOAD_MAX_FILES;

    for root in &roots {
        if remaining_budget == 0 {
            break;
        }
        let src_dir = root.join("src");
        if src_dir.is_dir() {
            collect_beamtalk_files_recursive(&src_dir, &mut files_to_index, &mut remaining_budget);
        }
    }

    for dir in &stdlib_dirs {
        if remaining_budget == 0 {
            break;
        }
        collect_beamtalk_files_recursive(dir, &mut files_to_index, &mut remaining_budget);
    }

    let mut loaded_files = Vec::new();
    for path in files_to_index {
        if !seen_files.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        loaded_files.push((path, content));
    }

    loaded_files
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
    if class.is_sealed && hover_markdown.contains("Method: `") {
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
            Severity::Hint => DiagnosticSeverity::HINT,
        }),
        source: Some("beamtalk".into()),
        message: if let Some(ref hint) = diag.hint {
            format!("{}\nHint: {hint}", diag.message)
        } else {
            diag.message.to_string()
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
        selection_range: range,
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
        let text = "Class: `Integer` (module `integer`)";
        assert_eq!(extract_hover_class_name(text), Some("Integer"));
    }

    #[test]
    fn extract_hover_class_name_from_resolved_method_hover() {
        let text =
            "Method: `+` (instance-side, arity: 1)\n\nResolved on `Integer` (defined in `Integer`)";
        assert_eq!(extract_hover_class_name(text), Some("Integer"));
    }

    #[test]
    fn stdlib_policy_note_for_sealed_class() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Integer` (module `integer`)");
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

        let note = stdlib_hover_policy_note(&service, "Class: `Counter` (module `counter`)");
        assert!(note.is_none());
    }

    #[test]
    fn stdlib_policy_note_for_abstract_class() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Boolean` (module `boolean`)");
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("abstract"));
    }

    #[test]
    fn stdlib_policy_note_marks_high_confidence_for_sealed_method_hover() {
        let service = SimpleLanguageService::new();
        let hover =
            "Method: `+` (instance-side, arity: 1)\n\nResolved on `Integer` (defined in `Integer`)";
        let note = stdlib_hover_policy_note(&service, hover);
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("Confidence: high"));
        assert!(note.contains("sealed stdlib dispatch"));
    }

    #[test]
    fn stdlib_policy_note_does_not_mark_high_confidence_for_class_hover() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Integer` (module `integer`)");
        assert!(note.is_some());
        assert!(!note.unwrap().contains("Confidence: high"));
    }

    #[test]
    fn policy_note_works_for_resolved_method_hover_markdown() {
        let service = SimpleLanguageService::new();
        let hover = HoverInfo::new("Method: `+` (instance-side, arity: 1)", Span::new(0, 1))
            .with_documentation("Resolved on `Integer` (defined in `Integer`)");
        let markdown = format!(
            "{}\n\n{}",
            hover.contents,
            format_hover_documentation(hover.documentation.as_deref().unwrap_or_default())
        );

        let note = stdlib_hover_policy_note(&service, &markdown);
        assert!(note.is_some());
        assert!(note.unwrap().contains("Integer"));
    }
}

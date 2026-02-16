// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! LSP server implementation.
//!
//! **DDD Context:** Language Service
//!
//! Delegates all IDE operations to `SimpleLanguageService` + `ProjectIndex`.
//! Maps between LSP protocol types and beamtalk language service types.

use std::collections::HashMap;
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
    InitializedParams, MarkupContent, MarkupKind, OneOf, Range, ReferenceParams,
    ServerCapabilities, SymbolKind, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Url,
};
use tower_lsp::{Client, LanguageServer};
use tracing::debug;

const DIAGNOSTIC_DEBOUNCE_MS: u64 = 150;

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
}

impl Backend {
    /// Creates a new `Backend` with the given LSP client handle.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            service: Mutex::new(SimpleLanguageService::new()),
            versions: Mutex::new(HashMap::new()),
            diagnostic_generation: Mutex::new(HashMap::new()),
        }
    }

    fn file_version_for_uri(&self, uri: &Url) -> Option<i32> {
        let path = uri_to_path(uri)?;
        let versions = self.versions.lock().expect("versions lock poisoned");
        versions.get(&path).copied()
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
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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

            tokio::time::sleep(Duration::from_millis(DIAGNOSTIC_DEBOUNCE_MS)).await;

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
    use camino::Utf8PathBuf;

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

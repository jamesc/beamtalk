// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! LSP server implementation.
//!
//! **DDD Context:** Language Service
//!
//! Delegates all IDE operations to `SimpleLanguageService` + `ProjectIndex`.
//! Maps between LSP protocol types and beamtalk language service types.

use std::sync::Mutex;

use beamtalk_core::language_service::{
    CompletionKind, DocumentSymbolKind, LanguageService, Position as BtPosition,
    SimpleLanguageService,
};
use beamtalk_core::source_analysis::{Severity, Span};
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DocumentSymbolParams, DocumentSymbolResponse, Documentation,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, MarkupContent,
    MarkupKind, OneOf, Range, ReferenceParams, ServerCapabilities, SymbolKind,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer};
use tracing::debug;

/// LSP backend wrapping `SimpleLanguageService`.
pub struct Backend {
    client: Client,
    service: Mutex<SimpleLanguageService>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            service: Mutex::new(SimpleLanguageService::new()),
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
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
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

    async fn initialized(&self, _: InitializedParams) {
        debug!("beamtalk-lsp initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = uri_to_path(&uri) {
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path, params.text_document.text);
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let (Some(path), Some(change)) =
            (uri_to_path(&uri), params.content_changes.into_iter().last())
        {
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path, change.text);
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = uri_to_path(&uri) {
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.remove_file(&path);
            }
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = uri_to_path(uri) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position.position);

        let items: Vec<CompletionItem> = {
            let svc = self.service.lock().expect("service lock poisoned");
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = uri_to_path(uri) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position_params.position);

        let svc = self.service.lock().expect("service lock poisoned");
        let source = svc.file_source(&path);
        let hover = svc.hover(&path, pos);

        Ok(hover.map(|h| {
            let mut value = h.contents.to_string();
            if let Some(doc) = h.documentation {
                value.push_str("\n\n");
                value.push_str(&doc);
            }
            Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: source.as_deref().map(|src| span_to_range(h.span, src)),
            }
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = uri_to_path(uri) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position_params.position);

        let svc = self.service.lock().expect("service lock poisoned");
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

    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = uri_to_path(uri) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position.position);

        let svc = self.service.lock().expect("service lock poisoned");
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

// --- Type conversion helpers ---

/// Converts an LSP URI to a `Utf8PathBuf`.
fn uri_to_path(uri: &Url) -> Option<Utf8PathBuf> {
    uri.to_file_path()
        .ok()
        .and_then(|p| Utf8PathBuf::try_from(p).ok())
}

/// Converts a `Utf8PathBuf` to an LSP URI.
fn path_to_uri(path: &Utf8PathBuf) -> Option<Url> {
    Url::from_file_path(path.as_str()).ok()
}

/// Converts an LSP `Position` to a beamtalk `Position`.
fn to_bt_position(pos: tower_lsp::lsp_types::Position) -> BtPosition {
    BtPosition::new(pos.line, pos.character)
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
        message: diag.message.to_string(),
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

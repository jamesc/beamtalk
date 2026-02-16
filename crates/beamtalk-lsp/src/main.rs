// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk Language Server Protocol server.
//!
//! **DDD Context:** Language Service
//!
//! This binary exposes the `SimpleLanguageService` + `ProjectIndex` from
//! `beamtalk-core` over the Language Server Protocol using `tower-lsp`.

/// LSP server backend implementation.
mod server;

use tower_lsp::{LspService, Server};
use tracing_subscriber::EnvFilter;

/// Entry point for the beamtalk language server.
///
/// Initialises tracing, creates the LSP service, and serves over stdin/stdout.
#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        // LSP is consumed by editors/tools, so logs should be plain text (no ANSI escapes).
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(server::Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

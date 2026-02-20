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

use clap::{ArgAction, Parser};
use tower_lsp::{LspService, Server};
use tracing_subscriber::{self, EnvFilter};

/// Entry point for the beamtalk language server.
///
/// Initialises tracing, creates the LSP service, and serves over stdin/stdout.
#[derive(Debug, Parser)]
#[command(name = "beamtalk-lsp", about = "Beamtalk Language Server")]
struct Cli {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    let default_directive = directive_for_verbosity(cli.verbose);

    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(default_directive)),
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

fn directive_for_verbosity(v: u8) -> &'static str {
    // Target must match the crate's Rust module path (`beamtalk_lsp`).
    // `beamtalk=…` only matches `beamtalk::*`, not `beamtalk_lsp`.
    // Also include beamtalk_core for compiler/analysis diagnostics, and
    // tower_lsp at warn for protocol-level errors.
    match v {
        0 => "beamtalk_lsp=info,beamtalk_core=info,tower_lsp=warn",
        1 => "beamtalk_lsp=debug,beamtalk_core=debug,tower_lsp=info",
        _ => "beamtalk_lsp=trace,beamtalk_core=trace,tower_lsp=debug",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn directive_defaults() {
        assert_eq!(
            directive_for_verbosity(0),
            "beamtalk_lsp=info,beamtalk_core=info,tower_lsp=warn"
        );
        assert_eq!(
            directive_for_verbosity(1),
            "beamtalk_lsp=debug,beamtalk_core=debug,tower_lsp=info"
        );
        assert_eq!(
            directive_for_verbosity(2),
            "beamtalk_lsp=trace,beamtalk_core=trace,tower_lsp=debug"
        );
    }
}

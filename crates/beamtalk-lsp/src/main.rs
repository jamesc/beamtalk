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

/// WebSocket client to a running workspace (ADR 0082 Phase 3, BT-2289).
mod runtime;

use clap::{ArgAction, Parser};
use tower_lsp::{ClientSocket, LspService, Server};
use tracing_subscriber::{self, EnvFilter};

/// Command-line arguments.
///
/// `long_about = None` keeps `--help` showing the one-line `about` rather than
/// this doc comment — clap promotes a struct doc comment to the program
/// description otherwise, which would leak an implementation note into
/// user-facing help.
#[derive(Debug, Parser)]
#[command(
    name = "beamtalk-lsp",
    about = "Beamtalk Language Server",
    long_about = None
)]
struct Cli {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,
}

/// Entry point for the beamtalk language server.
///
/// Initialises tracing, creates the LSP service, and serves over stdin/stdout.
#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    init_tracing(cli.verbose);

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = build_service();
    Server::new(stdin, stdout, socket).serve(service).await;
}

/// Build the LSP service plus its client socket, registering every custom
/// (non-standard-LSP) method the Beamtalk server answers.
///
/// Split out of [`main`] so tests can assert the custom-method registration
/// without taking over the process's stdin/stdout.
fn build_service() -> (LspService<server::Backend>, ClientSocket) {
    LspService::build(server::Backend::new)
        .custom_method("beamtalk-lsp/fetchContent", server::Backend::fetch_content)
        .finish()
}

/// Install the process-wide tracing subscriber.
///
/// Not unit-tested directly — `init()` installs a global subscriber, which can
/// only happen once per process. The filter it depends on is tested via
/// [`tracing_filter`].
fn init_tracing(verbose: u8) {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_filter(verbose))
        // LSP is consumed by editors/tools, so logs should be plain text (no ANSI escapes).
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();
}

/// Resolve the log filter: an explicit `RUST_LOG` wins, otherwise the
/// verbosity-derived default from [`directive_for_verbosity`].
fn tracing_filter(verbose: u8) -> EnvFilter {
    EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(directive_for_verbosity(verbose)))
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
    use clap::CommandFactory;
    use tower::{Service, ServiceExt};

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

    /// Verbosity saturates at trace: `-vvv` and beyond must not panic or fall
    /// through to a different directive than `-vv`.
    #[test]
    fn directive_saturates_above_two() {
        let trace = directive_for_verbosity(2);
        assert_eq!(directive_for_verbosity(3), trace);
        assert_eq!(directive_for_verbosity(u8::MAX), trace);
    }

    /// clap's own structural sanity check for the derived `Cli` — catches
    /// conflicting short/long flags and malformed `#[arg]` attributes.
    #[test]
    fn cli_definition_is_well_formed() {
        Cli::command().debug_assert();
        assert_eq!(Cli::command().get_name(), "beamtalk-lsp");
    }

    #[test]
    fn cli_defaults_to_zero_verbosity() {
        let cli = Cli::try_parse_from(["beamtalk-lsp"]).expect("no args parses");
        assert_eq!(cli.verbose, 0);
    }

    #[test]
    fn cli_counts_repeated_verbose_flags() {
        for (args, expected) in [
            (vec!["beamtalk-lsp", "-v"], 1u8),
            (vec!["beamtalk-lsp", "-vv"], 2),
            (vec!["beamtalk-lsp", "-v", "-v", "-v"], 3),
            (vec!["beamtalk-lsp", "--verbose"], 1),
            (vec!["beamtalk-lsp", "--verbose", "--verbose"], 2),
        ] {
            let cli = Cli::try_parse_from(&args).unwrap_or_else(|e| panic!("{args:?}: {e}"));
            assert_eq!(cli.verbose, expected, "for {args:?}");
        }
    }

    #[test]
    fn cli_rejects_unknown_arguments() {
        let err = Cli::try_parse_from(["beamtalk-lsp", "--nope"]).expect_err("unknown flag");
        assert_eq!(err.kind(), clap::error::ErrorKind::UnknownArgument);
    }

    #[test]
    fn cli_rejects_positional_arguments() {
        // The server speaks LSP over stdio only — it takes no file operands,
        // so a stray path must be an error rather than silently ignored.
        let err = Cli::try_parse_from(["beamtalk-lsp", "src/counter.bt"]).expect_err("positional");
        assert_eq!(
            err.kind(),
            clap::error::ErrorKind::UnknownArgument,
            "unexpected error: {err}"
        );
    }

    #[test]
    fn cli_help_documents_the_verbose_flag() {
        let err = Cli::try_parse_from(["beamtalk-lsp", "--help"]).expect_err("help exits early");
        assert_eq!(err.kind(), clap::error::ErrorKind::DisplayHelp);
        let help = err.to_string();
        assert!(help.contains("Beamtalk Language Server"), "{help}");
        assert!(help.contains("--verbose"), "{help}");
    }

    /// With no `RUST_LOG` set the filter must come from the verbosity
    /// directive; with one set, `RUST_LOG` must win. The test reads the
    /// ambient env rather than mutating it (`set_var` is process-global and
    /// unsound under a multi-threaded test runner).
    ///
    /// `EnvFilter`'s `Display` does not preserve directive order, so compare
    /// as sets.
    #[test]
    fn tracing_filter_prefers_rust_log_then_falls_back_to_verbosity() {
        fn directives(filter: &str) -> Vec<&str> {
            let mut parts: Vec<&str> = filter.split(',').collect();
            parts.sort_unstable();
            parts
        }

        let filter = tracing_filter(0).to_string();
        match std::env::var("RUST_LOG") {
            Err(_) => assert_eq!(
                directives(&filter),
                directives(directive_for_verbosity(0)),
                "no RUST_LOG: the verbosity default applies"
            ),
            Ok(rust_log) => assert_eq!(
                directives(&filter),
                directives(&EnvFilter::new(rust_log).to_string()),
                "RUST_LOG must win over the verbosity default"
            ),
        }
    }

    /// `build_service` must register `beamtalk-lsp/fetchContent`; a bare
    /// `LspService::new` (no `custom_method`) must not. Driving both through
    /// the `tower::Service` impl is the only way to see the difference —
    /// `Backend::fetch_content` is callable either way, it's the *dispatch
    /// table* that `build_service` populates.
    #[tokio::test]
    async fn build_service_registers_fetch_content_custom_method() {
        async fn call_fetch_content(
            service: &mut LspService<server::Backend>,
        ) -> tower_lsp::jsonrpc::Response {
            let init = tower_lsp::jsonrpc::Request::build("initialize")
                .params(serde_json::json!({"capabilities": {}}))
                .id(1)
                .finish();
            service
                .ready()
                .await
                .expect("ready")
                .call(init)
                .await
                .expect("initialize")
                .expect("initialize response");

            let request = tower_lsp::jsonrpc::Request::build("beamtalk-lsp/fetchContent")
                .params(serde_json::json!({"uri": "http://example.com/Counter.bt"}))
                .id(2)
                .finish();
            service
                .ready()
                .await
                .expect("ready")
                .call(request)
                .await
                .expect("call")
                .expect("response")
        }

        let (mut registered, _socket) = build_service();
        let response = call_fetch_content(&mut registered).await;
        let (_id, result) = response.into_parts();
        let err = result.expect_err("unsupported scheme is rejected by the handler");
        assert_eq!(
            err.code,
            tower_lsp::jsonrpc::ErrorCode::InvalidParams,
            "the registered handler ran (and rejected the scheme): {err:?}"
        );

        let (mut bare, _bare_socket) = LspService::new(server::Backend::new);
        let response = call_fetch_content(&mut bare).await;
        let (_id, result) = response.into_parts();
        let err = result.expect_err("no such method on a bare service");
        assert_eq!(
            err.code,
            tower_lsp::jsonrpc::ErrorCode::MethodNotFound,
            "bare LspService must not know the custom method: {err:?}"
        );
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP Port binary for the Beamtalk compiler (ADR 0022).
//!
//! **DDD Context:** Compilation (Anti-Corruption Layer boundary)
//!
//! Reads ETF-encoded requests from stdin ({packet, 4} framing),
//! calls beamtalk-core compile functions, and writes ETF-encoded
//! responses to stdout.
//!
//! Supports commands: `compile_expression`, `compile`, `diagnostics`, `version`.

mod decode;
mod diagnostics;
mod handlers;
mod registry;
mod respond;

use std::io;

use beamtalk_etf::{self as etf, map_get};
use clap::{ArgAction, Parser};
use tracing_subscriber::{self, EnvFilter};

use eetf::Term;

use handlers::{
    handle_build_class_module_index_in_source, handle_categorize_methods,
    handle_class_state_field_defaults, handle_compile, handle_compile_expression,
    handle_compile_expression_trace, handle_compile_method, handle_diagnostics,
    handle_find_all_sends_in_source, handle_find_announce_sites_in_source,
    handle_find_definition_selector_spans, handle_find_ffi_sites_in_source,
    handle_find_field_readers_in_source, handle_find_field_writers_in_source,
    handle_find_references_to_in_source, handle_find_selector_send_spans,
    handle_find_senders_in_source, handle_reindent_method_source, handle_resolve_class_span,
    handle_resolve_completion_type, handle_resolve_method_span, handle_version,
};
use respond::error_response;

/// Handle a single request and return a response Term.
fn handle_request(request_term: &Term) -> Term {
    let Term::Map(map) = request_term else {
        return error_response(&["Request must be a map".to_string()]);
    };

    // Extract command atom
    let command = match map_get(map, "command") {
        Some(Term::Atom(a)) => a.name.as_str(),
        _ => return error_response(&["Missing or invalid 'command' field".to_string()]),
    };

    // BT-3095: this match arm's string literals are the Rust half of the
    // compiler-port wire vocabulary; `beamtalk_compiler.erl` (fanning out
    // through `beamtalk_compiler_server`/`beamtalk_compiler_port`) is the
    // Erlang half, sending each `command => <atom>` from separate call
    // sites. The two lists cannot literally share code (different
    // languages/processes either side of the OTP port), so a command
    // added to one side and not the other is a silent drift whose only
    // symptom is a runtime "Unknown command" error wherever it's invoked
    // (BT-3078 drift audit; BT-3091 flagged this pair for evaluation).
    // A shared corpus fixture
    // (`runtime/apps/beamtalk_compiler/test/fixtures/compiler_port_command_vocabulary_corpus.json`)
    // pins both sides to the same 19-command list end-to-end: the Rust test
    // below dispatches each corpus command through `handle_request` and
    // checks it isn't the catch-all arm, while
    // `beamtalk_compiler_tests:command_vocabulary_corpus_is_recognized_test/0`
    // drives the real compiled binary through `beamtalk_compiler`'s public
    // API (one command per corpus entry) and asserts each one dispatches
    // successfully — so a command missing on either side fails a build-time
    // test instead of surfacing only at runtime.
    match command {
        "compile_expression" => handle_compile_expression(map),
        "compile_expression_trace" => handle_compile_expression_trace(map),
        "compile" => handle_compile(map),
        "compile_method" => handle_compile_method(map),
        "diagnostics" => handle_diagnostics(map),
        "version" => handle_version(),
        "resolve_completion_type" => handle_resolve_completion_type(map),
        "find_senders_in_source" => handle_find_senders_in_source(map),
        "find_all_sends_in_source" => handle_find_all_sends_in_source(map),
        "find_references_to_in_source" => handle_find_references_to_in_source(map),
        "find_field_readers_in_source" => handle_find_field_readers_in_source(map),
        "find_field_writers_in_source" => handle_find_field_writers_in_source(map),
        "find_ffi_sites_in_source" => handle_find_ffi_sites_in_source(map),
        "find_announce_sites_in_source" => handle_find_announce_sites_in_source(map),
        "resolve_method_span" => handle_resolve_method_span(map),
        "reindent_method_source" => handle_reindent_method_source(map),
        "resolve_class_span" => handle_resolve_class_span(map),
        "find_selector_send_spans" => handle_find_selector_send_spans(map),
        "find_definition_selector_spans" => handle_find_definition_selector_spans(map),
        "categorize_methods" => handle_categorize_methods(map),
        "class_state_field_defaults" => handle_class_state_field_defaults(map),
        "build_class_module_index_in_source" => handle_build_class_module_index_in_source(map),
        _ => error_response(&[format!("Unknown command: {command}")]),
    }
}

#[derive(Debug, Parser)]
#[command(name = "beamtalk-compiler-port", about = "Beamtalk compiler port")]
struct Cli {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,
}

/// Match the CLI's 8 MB stack so deeply-nested source files don't overflow
/// the default Windows 1 MB thread stack.
const STACK_SIZE: usize = 8 * 1024 * 1024;

fn main() {
    // Spawn the real entry point on a thread with a larger stack.
    // On Windows the default is 1 MB, which overflows on non-trivial
    // Beamtalk source files. Linux defaults to 8 MB so it rarely hits
    // this, but the explicit size makes behaviour consistent everywhere.
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .expect("failed to spawn main thread")
        .join()
        .expect("main thread panicked");
}

fn run() {
    let cli = Cli::parse();

    // Only initialize tracing when explicitly requested.
    // The compiler port is spawned by the Erlang runtime without args, so
    // default (verbose=0) must produce no stderr output to avoid interfering
    // with the OTP port protocol.
    let has_rust_log = std::env::var("RUST_LOG").is_ok();
    if has_rust_log || cli.verbose > 0 {
        let env_filter = if has_rust_log {
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("warn"))
        } else {
            EnvFilter::new(directive_for_verbosity(cli.verbose))
        };
        let _ = tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .with_writer(std::io::stderr)
            .with_ansi(false)
            .try_init();
    }

    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

    loop {
        // Read next request
        let packet = match etf::read_packet(&mut stdin) {
            Ok(Some(data)) => data,
            Ok(None) => break, // EOF — port closed
            Err(e) => {
                eprintln!("Failed to read packet: {e}");
                break;
            }
        };

        // Decode ETF
        let term = match Term::decode(io::Cursor::new(&packet)) {
            Ok(t) => t,
            Err(e) => {
                // Send error response for decode failures
                let response = error_response(&[format!("ETF decode error: {e}")]);
                let mut buf = Vec::new();
                if response.encode(&mut buf).is_ok() {
                    let _ = etf::write_packet(&mut stdout, &buf);
                }
                continue;
            }
        };

        // Handle the request
        let response = handle_request(&term);

        // Encode and send response
        let mut buf = Vec::new();
        match response.encode(&mut buf) {
            Ok(()) => {
                if let Err(e) = etf::write_packet(&mut stdout, &buf) {
                    eprintln!("Failed to write response: {e}");
                    break;
                }
            }
            Err(e) => {
                eprintln!("Failed to encode response: {e}");
                break;
            }
        }
    }
}

fn directive_for_verbosity(v: u8) -> &'static str {
    // Target must match Rust module paths (`beamtalk_compiler_port`, `beamtalk_core`).
    // `beamtalk=…` only matches `beamtalk::*`, not `beamtalk_compiler_port`.
    match v {
        0 => "beamtalk_compiler_port=info,beamtalk_core=info",
        1 => "beamtalk_compiler_port=debug,beamtalk_core=debug",
        _ => "beamtalk_compiler_port=trace,beamtalk_core=trace",
    }
}

#[cfg(test)]
mod tests;

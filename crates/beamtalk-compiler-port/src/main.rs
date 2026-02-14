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

use std::io::{self, Read, Write};

use eetf::{Atom, Binary, List, Map, Term};

/// Read a {packet, 4} framed message from stdin.
fn read_packet(stdin: &mut impl Read) -> io::Result<Option<Vec<u8>>> {
    let mut len_buf = [0u8; 4];
    match stdin.read_exact(&mut len_buf) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }
    let len = u32::from_be_bytes(len_buf) as usize;
    // Guard against unreasonably large packets (>64 MiB)
    if len > 64 * 1024 * 1024 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("packet too large: {len} bytes"),
        ));
    }
    let mut buf = vec![0u8; len];
    stdin.read_exact(&mut buf)?;
    Ok(Some(buf))
}

/// Write a `{packet, 4}` framed message to stdout.
fn write_packet(stdout: &mut impl Write, data: &[u8]) -> io::Result<()> {
    let len = u32::try_from(data.len())
        .expect("packet too large for {packet, 4} framing")
        .to_be_bytes();
    stdout.write_all(&len)?;
    stdout.write_all(data)?;
    stdout.flush()
}

/// Helper to create an atom Term.
fn atom(name: &str) -> Term {
    Term::from(Atom::from(name))
}

/// Helper to create a binary Term from a string.
fn binary(s: &str) -> Term {
    Term::from(Binary::from(s.as_bytes()))
}

/// Extract a binary string value from a Term.
fn term_to_string(term: &Term) -> Option<String> {
    match term {
        Term::Binary(b) => String::from_utf8(b.bytes.clone()).ok(),
        // Erlang may send short strings as ByteList
        Term::ByteList(bl) => String::from_utf8(bl.bytes.clone()).ok(),
        _ => None,
    }
}

/// Extract a list of strings from a Term (for `known_vars`).
fn term_to_string_list(term: &Term) -> Option<Vec<String>> {
    match term {
        Term::List(list) => {
            let mut result = Vec::new();
            for elem in &list.elements {
                result.push(term_to_string(elem)?);
            }
            Some(result)
        }
        _ => None,
    }
}

/// Look up a key (atom) in an ETF map.
fn map_get<'a>(map: &'a Map, key: &str) -> Option<&'a Term> {
    map.map.get(&atom(key))
}

/// Extract a boolean value from a Term.
fn term_to_bool(term: &Term) -> Option<bool> {
    match term {
        Term::Atom(a) => match a.name.as_str() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Build a response map for a successful `compile_expression`.
fn ok_response(core_erlang: &str, warnings: &[String]) -> Term {
    let warning_terms: Vec<Term> = warnings.iter().map(|w| binary(w)).collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("warnings"), Term::from(List::from(warning_terms))),
    ]))
}

/// Build a response map for a successful `compile` (file compilation).
fn compile_ok_response(
    core_erlang: &str,
    module_name: &str,
    classes: &[(String, String)],
    warnings: &[String],
) -> Term {
    let warning_terms: Vec<Term> = warnings.iter().map(|w| binary(w)).collect();
    let class_terms: Vec<Term> = classes
        .iter()
        .map(|(name, superclass)| {
            Term::from(Map::from([
                (atom("name"), binary(name)),
                (atom("superclass"), binary(superclass)),
            ]))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (atom("classes"), Term::from(List::from(class_terms))),
        (atom("warnings"), Term::from(List::from(warning_terms))),
    ]))
}

/// Build a response map for a successful `diagnostics` query.
fn diagnostics_ok_response(diagnostics: &[DiagInfo]) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| {
            Term::from(Map::from([
                (atom("message"), binary(&d.message)),
                (atom("severity"), binary(&d.severity)),
                (
                    atom("start"),
                    Term::from(eetf::FixInteger::from(
                        i32::try_from(d.start).unwrap_or(i32::MAX),
                    )),
                ),
                (
                    atom("end"),
                    Term::from(eetf::FixInteger::from(
                        i32::try_from(d.end).unwrap_or(i32::MAX),
                    )),
                ),
            ]))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}

/// Build a response map for a compilation error.
fn error_response(diagnostics: &[String]) -> Term {
    let diag_terms: Vec<Term> = diagnostics.iter().map(|d| binary(d)).collect();
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}

/// Structured diagnostic info.
struct DiagInfo {
    message: String,
    severity: String,
    start: u32,
    end: u32,
}

/// Separate diagnostics into errors and warnings, returning structured info.
fn partition_diagnostics(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> (Vec<DiagInfo>, Vec<DiagInfo>) {
    let errors = diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: "error".to_string(),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    let warnings = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
            )
        })
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: "warning".to_string(),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    (errors, warnings)
}

/// Handle a single `compile_expression` request.
fn handle_compile_expression(request: &Map) -> Term {
    // Extract required fields
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let Some(module_name) = map_get(request, "module").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'module' field".to_string()]);
    };

    let known_vars = map_get(request, "known_vars")
        .and_then(term_to_string_list)
        .unwrap_or_default();

    // Parse the expression
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis with known REPL variables
    let known_var_refs: Vec<&str> = known_vars.iter().map(String::as_str).collect();
    let mut all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars(
            &module,
            parse_diagnostics,
            &known_var_refs,
        );

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions::default();
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    // Separate errors and warnings
    let errors: Vec<String> = all_diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .map(|d| d.message.to_string())
        .collect();

    let warnings: Vec<String> = all_diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
            )
        })
        .map(|d| d.message.to_string())
        .collect();

    if !errors.is_empty() {
        return error_response(&errors);
    }

    if module.expressions.is_empty() {
        return error_response(&["No expressions to compile".to_string()]);
    }

    // Generate Core Erlang for the first expression
    let expression = &module.expressions[0];
    match beamtalk_core::erlang::generate_repl_expression(expression, &module_name) {
        Ok(code) => ok_response(&code, &warnings),
        Err(e) => error_response(&[format!("Code generation failed: {e}")]),
    }
}

/// Handle a `compile` request (file/class compilation).
fn handle_compile(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let stdlib_mode = map_get(request, "stdlib_mode")
        .and_then(term_to_bool)
        .unwrap_or(false);

    let workspace_mode = map_get(request, "workspace_mode")
        .and_then(term_to_bool)
        .unwrap_or(true);

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis
    let mut all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars(
            &module,
            parse_diagnostics,
            &[],
        );

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode,
        allow_primitives: false,
        workspace_mode: false,
    };
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    let (errors, warnings) = partition_diagnostics(&all_diagnostics);

    if !errors.is_empty() {
        let error_msgs: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
        return error_response(&error_msgs);
    }

    // Derive module name from first class in AST (ADR 0016)
    let base_name = if let Some(first_class) = module.classes.first() {
        beamtalk_core::erlang::to_module_name(&first_class.name.name)
    } else {
        // No class definition — use a fallback
        return error_response(&["No class definition found in source".to_string()]);
    };

    let module_name = if stdlib_mode {
        format!("bt@stdlib@{base_name}")
    } else {
        format!("bt@{base_name}")
    };

    // Extract class info
    let classes: Vec<(String, String)> = module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();

    // Generate Core Erlang
    let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();
    match beamtalk_core::erlang::generate_with_workspace_and_source(
        &module,
        &module_name,
        workspace_mode,
        Some(&source),
    ) {
        Ok(code) => compile_ok_response(&code, &module_name, &classes, &warning_msgs),
        Err(e) => error_response(&[format!("Code generation failed: {e}")]),
    }
}

/// Handle a `diagnostics` request (syntax/semantic check only).
fn handle_diagnostics(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis
    let all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars(
            &module,
            parse_diagnostics,
            &[],
        );

    let all_diags: Vec<DiagInfo> = all_diagnostics
        .iter()
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::source_analysis::Severity::Error => "error".to_string(),
                beamtalk_core::source_analysis::Severity::Warning => "warning".to_string(),
            },
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();

    diagnostics_ok_response(&all_diags)
}

/// Handle a `version` request.
fn handle_version() -> Term {
    let version = env!("CARGO_PKG_VERSION");
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("version"), binary(version)),
    ]))
}

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

    match command {
        "compile_expression" => handle_compile_expression(map),
        "compile" => handle_compile(map),
        "diagnostics" => handle_diagnostics(map),
        "version" => handle_version(),
        _ => error_response(&[format!("Unknown command: {command}")]),
    }
}

fn main() {
    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

    loop {
        // Read next request
        let packet = match read_packet(&mut stdin) {
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
                    let _ = write_packet(&mut stdout, &buf);
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
                if let Err(e) = write_packet(&mut stdout, &buf) {
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

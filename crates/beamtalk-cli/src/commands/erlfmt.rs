// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `erlfmt` integration for formatting native Erlang (`.erl`) files.
//!
//! Builds and caches erlfmt as a rebar3 dependency on first use, then invokes
//! it via `erl` for formatting operations. Falls back gracefully when `rebar3`
//! is not available.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result};
use std::process::Command;
use tracing::warn;

/// Result of an erlfmt invocation.
#[derive(Debug, Default)]
pub struct ErlfmtResult {
    /// Files that were formatted (write mode) or would be reformatted (check mode).
    pub changed_files: Vec<Utf8PathBuf>,
    /// Files that could not be formatted (e.g. parse errors).
    pub error_files: Vec<Utf8PathBuf>,
}

/// Subdirectory under `_build/dev/` where erlfmt is compiled.
const ERLFMT_TOOL_DIR: &str = "tools/erlfmt_build";
/// Path to compiled erlfmt beams relative to the tool directory.
const ERLFMT_EBIN_REL: &str = "_build/default/lib/erlfmt/ebin";

/// Run erlfmt on the given `.erl` files.
///
/// In check mode, reports which files would change without modifying them.
/// In write mode, formats files in place.
///
/// Returns `Ok(result)` with changed/error file lists, or gracefully
/// warns and returns an empty result if erlfmt is unavailable.
pub fn run_erlfmt(
    files: &[Utf8PathBuf],
    check_only: bool,
    project_root: &Utf8Path,
) -> Result<ErlfmtResult> {
    if files.is_empty() {
        return Ok(ErlfmtResult::default());
    }

    let build_dir = project_root.join("_build").join("dev");
    let tool_dir = build_dir.join(ERLFMT_TOOL_DIR);
    let ebin_dir = tool_dir.join(ERLFMT_EBIN_REL);

    // Ensure erlfmt beams are available.
    if !ebin_dir.is_dir() {
        if let Err(e) = build_erlfmt(&tool_dir) {
            warn!("could not build erlfmt: {e}");
            eprintln!(
                "warning: erlfmt not available; skipping .erl formatting \
                 (install rebar3 to enable)"
            );
            return Ok(ErlfmtResult::default());
        }
    }

    invoke_erlfmt(&ebin_dir, files, check_only)
}

/// Build erlfmt using rebar3 and cache the compiled beams.
fn build_erlfmt(tool_dir: &Utf8Path) -> Result<()> {
    // Check rebar3 is available.
    if Command::new("rebar3").arg("--version").output().is_err() {
        miette::bail!("rebar3 not found on PATH");
    }

    std::fs::create_dir_all(tool_dir).into_diagnostic()?;

    // Write minimal rebar.config with erlfmt as a dependency.
    let rebar_config = tool_dir.join("rebar.config");
    std::fs::write(
        rebar_config.as_std_path(),
        "{deps, [{erlfmt, \"1.8.0\"}]}.\n",
    )
    .into_diagnostic()?;

    // Write minimal app src so rebar3 is happy.
    let src_dir = tool_dir.join("src");
    std::fs::create_dir_all(&src_dir).into_diagnostic()?;
    std::fs::write(
        src_dir.join("erlfmt_tool.app.src").as_std_path(),
        "{application, erlfmt_tool, [{vsn, \"0.0.1\"}]}.\n",
    )
    .into_diagnostic()?;

    eprintln!("Building erlfmt (first time only)...");

    let output = Command::new("rebar3")
        .arg("compile")
        .current_dir(tool_dir.as_std_path())
        .output()
        .into_diagnostic()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!("rebar3 compile failed: {stderr}");
    }

    let ebin_dir = tool_dir.join(ERLFMT_EBIN_REL);
    if !ebin_dir.is_dir() {
        miette::bail!("erlfmt ebin not found after build at {ebin_dir}");
    }

    eprintln!("erlfmt compiled and cached");
    Ok(())
}

/// Invoke erlfmt on the given files via `erl`.
///
/// Uses `erlfmt:format_file/2` for each file since erlfmt doesn't have a
/// standalone CLI binary. The compiled beams are loaded via `-pa`.
fn invoke_erlfmt(
    ebin_dir: &Utf8Path,
    files: &[Utf8PathBuf],
    check_only: bool,
) -> Result<ErlfmtResult> {
    let mut result = ErlfmtResult::default();

    for file in files {
        let action = if check_only { "check" } else { "write" };
        // Escape the file path for embedding in an Erlang string literal.
        // Must handle: backslash, double-quote, and control characters.
        let escaped_file = escape_for_erlang_string(file.as_str());
        // Build an Erlang expression that formats a single file.
        // erlfmt:format_file/2 returns {ok, Code, Warnings} | {skip, Reason} | {error, Error}
        // For check mode we compare the formatted output with the original.
        let eval_expr = if check_only {
            format!(
                "case erlfmt:format_file(\"{escaped_file}\", [{{print_width, 100}}]) of \
                    {{ok, Formatted, _}} -> \
                        {{ok, Original}} = file:read_file(\"{escaped_file}\"), \
                        case iolist_to_binary(Formatted) =:= Original of \
                            true -> halt(0); \
                            false -> halt(1) \
                        end; \
                    {{skip, _}} -> halt(0); \
                    {{error, _}} -> halt(2) \
                end."
            )
        } else {
            format!(
                "case erlfmt:format_file(\"{escaped_file}\", [{{print_width, 100}}]) of \
                    {{ok, Formatted, _}} -> \
                        file:write_file(\"{escaped_file}\", Formatted), halt(0); \
                    {{skip, _}} -> halt(0); \
                    {{error, _}} -> halt(2) \
                end."
            )
        };

        let output = Command::new("erl")
            .arg("-noshell")
            .arg("-pa")
            .arg(ebin_dir.as_str())
            .arg("-eval")
            .arg(&eval_expr)
            .output()
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to run erlfmt on '{}': {e}", file))?;

        match output.status.code() {
            Some(0) => {
                // File is already formatted (check) or was formatted (write).
            }
            Some(1) if check_only => {
                // File needs formatting.
                result.changed_files.push(file.clone());
            }
            _ => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                if !stderr.is_empty() {
                    eprintln!(
                        "warning: erlfmt {action} error on '{}': {}",
                        file,
                        stderr.trim()
                    );
                }
                result.error_files.push(file.clone());
            }
        }
    }

    Ok(result)
}

/// Escape a string for embedding in an Erlang string literal.
///
/// Handles backslashes, double-quotes, and control characters that the
/// Erlang parser would otherwise interpret.
fn escape_for_erlang_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c => out.push(c),
        }
    }
    out
}

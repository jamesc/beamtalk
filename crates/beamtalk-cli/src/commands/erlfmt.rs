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

use super::erlang_eval::{self, ErlangEval};

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
        let eval_expr = ErlangEval::new(file.as_str()).build(check_only);

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
            Some(code) if code == i32::from(erlang_eval::exit_codes::SUCCESS) => {
                // File is already formatted (check) or was formatted (write).
            }
            Some(code)
                if check_only && code == i32::from(erlang_eval::exit_codes::NEEDS_FORMAT) =>
            {
                // File needs formatting. (Distinct from VM-crash exit 1.)
                result.changed_files.push(file.clone());
            }
            Some(code) => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                let stdout = String::from_utf8_lossy(&output.stdout);
                let detail = if !stderr.is_empty() {
                    stderr.trim().to_string()
                } else if !stdout.is_empty() {
                    stdout.trim().to_string()
                } else {
                    format!("exit code {code}")
                };
                eprintln!("warning: erlfmt {action} error on '{file}': {detail}");
                result.error_files.push(file.clone());
            }
            None => {
                // Process was killed by a signal (no exit code).
                let stderr = String::from_utf8_lossy(&output.stderr);
                let detail = if stderr.is_empty() {
                    "process terminated by signal".to_string()
                } else {
                    stderr.trim().to_string()
                };
                eprintln!("warning: erlfmt {action} crashed on '{file}': {detail}");
                result.error_files.push(file.clone());
            }
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write as _;

    /// Write `content` to a temp `.erl` file and return (dir, path).
    fn write_temp_erl(name: &str, content: &str) -> (tempfile::TempDir, Utf8PathBuf) {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join(name);
        let mut f = std::fs::File::create(&path).expect("create temp file");
        f.write_all(content.as_bytes()).expect("write temp file");
        let utf8_path = Utf8PathBuf::from_path_buf(path).expect("utf8 path");
        (dir, utf8_path)
    }

    /// Return the project root (two levels up from the crate manifest directory).
    fn project_root() -> Utf8PathBuf {
        let manifest_dir = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        manifest_dir
            .parent()
            .and_then(|p| p.parent())
            .expect("project root")
            .to_owned()
    }

    #[test]
    fn escape_for_erlang_string_handles_special_chars() {
        use erlang_eval::escape_for_erlang_string;
        assert_eq!(escape_for_erlang_string(r"hello"), "hello");
        assert_eq!(escape_for_erlang_string(r#"a"b"#), r#"a\"b"#);
        assert_eq!(escape_for_erlang_string("a\\b"), "a\\\\b");
        assert_eq!(escape_for_erlang_string("a\nb"), "a\\nb");
    }

    /// erlfmt must correctly format and write back a `.erl` file containing
    /// non-ASCII Unicode characters (e.g. arrows, em-dashes) in comments.
    ///
    /// This is an integration test requiring `erl` and `rebar3` on PATH.
    /// Erlfmt beams are bootstrapped automatically if not already built.
    #[test]
    #[ignore = "requires erl and rebar3 on PATH"]
    fn erlfmt_write_preserves_unicode_in_comments() {
        let root = project_root();

        // U+2192 RIGHTWARDS ARROW, U+2014 EM DASH
        let source = "-module(unicode_test).\n\n\
                       %% Dispatch: request \u{2192} handler \u{2014} see docs.\n\n\
                       -export([hello/0]).\n\n\
                       hello() -> ok.\n";

        let (_dir, path) = write_temp_erl("unicode_test.erl", source);
        let files = vec![path.clone()];

        // Format in write mode (bootstraps erlfmt if needed).
        let result = run_erlfmt(&files, false, &root).expect("run_erlfmt write");
        assert!(
            result.error_files.is_empty(),
            "erlfmt should not report errors for Unicode file; errors: {:?}",
            result.error_files
        );

        // Read back and verify Unicode chars survived.
        let written = std::fs::read_to_string(path.as_std_path()).expect("read formatted file");
        assert!(
            written.contains('\u{2192}'),
            "formatted output must preserve U+2192 RIGHTWARDS ARROW; got: {written:?}"
        );
        assert!(
            written.contains('\u{2014}'),
            "formatted output must preserve U+2014 EM DASH; got: {written:?}"
        );

        // Check mode on the already-formatted file must report no changes.
        let check_result = run_erlfmt(&files, true, &root).expect("run_erlfmt check");
        assert!(
            check_result.changed_files.is_empty(),
            "fmt-check after fmt must report 0 changed files; got: {:?}",
            check_result.changed_files
        );
        assert!(
            check_result.error_files.is_empty(),
            "fmt-check must not report errors; got: {:?}",
            check_result.error_files
        );
    }

    /// When the Erlang subprocess crashes (e.g. nonexistent ebin path),
    /// `invoke_erlfmt` must report the file in `error_files`, not silently
    /// succeed.
    #[test]
    #[ignore = "requires erl on PATH"]
    fn erlfmt_surfaces_errors_on_subprocess_failure() {
        let bad_ebin = Utf8PathBuf::from("/nonexistent/ebin/path");
        let (_dir, path) = write_temp_erl("ok.erl", "-module(ok).\n");
        let files = vec![path.clone()];

        let result = invoke_erlfmt(&bad_ebin, &files, false).expect("invoke_erlfmt");
        assert!(
            !result.error_files.is_empty(),
            "erlfmt with bad ebin path must report file as error"
        );
    }
}

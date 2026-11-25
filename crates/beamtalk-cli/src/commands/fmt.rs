// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk fmt` and `beamtalk fmt-check` — format source files.
//!
//! **DDD Context:** Language Service — Formatting
//!
//! `beamtalk fmt <path>...` parses each `.bt` file, runs the unparser, and
//! writes the formatted output back in-place. Files that are already formatted
//! are left unchanged.
//!
//! `.btscript` files use **identity formatting**: only parse errors are
//! reported; the file content is never rewritten. This preserves the
//! `// => X` assertion comments that the btscript test runner expects on
//! separate lines directly after each expression.
//!
//! Native `.erl` files in `native/` and `native/test/` are formatted via
//! `erlfmt`. If erlfmt is unavailable, `.erl` files are skipped
//! with a warning.
//!
//! `beamtalk fmt-check <path>...` performs the same parse/unparse pass but
//! prints a unified diff for every file that would change and exits non-zero
//! if any files need reformatting or could not be verified. No files are
//! modified.

use std::collections::HashSet;

use crate::commands::build::collect_formattable_files_from_dir;
use crate::commands::erlfmt;
use crate::commands::util::write_atomic;
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_core::unparse::{FormatError, format_source};
use beamtalk_package::lint_pipeline::find_package_root;
use camino::Utf8PathBuf;
use miette::{IntoDiagnostic, Result};
use similar::TextDiff;

/// Collected files from path arguments, split by type.
struct CollectedFiles {
    bt_files: Vec<Utf8PathBuf>,
    erl_files: Vec<Utf8PathBuf>,
    project_root: Option<Utf8PathBuf>,
}

/// Discover formattable files from the given paths.
fn collect_fmt_files(paths: &[String]) -> Result<CollectedFiles> {
    let mut seen = HashSet::new();
    let mut bt_files = Vec::new();
    let mut erl_seen = HashSet::new();
    let mut erl_files = Vec::new();
    let mut project_root = None;

    for path in paths {
        let source_path = Utf8PathBuf::from(path);

        if source_path.is_file() {
            match source_path.extension() {
                Some("bt" | "btscript") => {
                    if seen.insert(source_path.clone()) {
                        bt_files.push(source_path);
                    }
                }
                Some("erl") => {
                    if erl_seen.insert(source_path.clone()) {
                        erl_files.push(source_path);
                    }
                }
                _ => {
                    miette::bail!(
                        "File '{}' is not a .bt, .btscript, or .erl source file",
                        path
                    );
                }
            }
        } else if source_path.is_dir() {
            for file in collect_formattable_files_from_dir(&source_path)? {
                if seen.insert(file.clone()) {
                    bt_files.push(file);
                }
            }
            // Collect native .erl files from native/ directory.
            let root = find_package_root(&source_path).unwrap_or_else(|| source_path.clone());
            let native_dir = root.join("native");
            if native_dir.is_dir() {
                match FileWalker::native_erl_files().walk(&native_dir) {
                    Ok(files) => {
                        for file in files {
                            if erl_seen.insert(file.clone()) {
                                erl_files.push(file);
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("warning: failed to scan native directory: {e}");
                    }
                }
            }
            project_root = Some(root);
        } else {
            miette::bail!("Path '{}' does not exist", path);
        }
    }

    if bt_files.is_empty() && erl_files.is_empty() {
        miette::bail!("No .bt, .btscript, or .erl source files found");
    }

    Ok(CollectedFiles {
        bt_files,
        erl_files,
        project_root,
    })
}

/// Format (or check formatting of) the given paths.
///
/// When `check_only` is `true` the command prints a unified diff for every
/// file that is not already formatted and exits non-zero if any files would
/// change or could not be verified (e.g. due to parse errors). When
/// `check_only` is `false` the command writes formatted output back to each
/// file that has changed; files with parse errors are skipped with a warning.
pub fn run_fmt(paths: &[String], check_only: bool) -> Result<()> {
    run_fmt_with(paths, check_only, format_source)
}

/// [`run_fmt`] with the `.bt` formatter supplied by the caller, so tests can
/// drive the refusal path (BT-5183) without depending on a live formatter bug.
fn run_fmt_with(
    paths: &[String],
    check_only: bool,
    format: fn(&str) -> std::result::Result<String, FormatError>,
) -> Result<()> {
    let collected = collect_fmt_files(paths)?;

    let mut changed_files: Vec<Utf8PathBuf> = Vec::new();
    let mut skipped_files: Vec<Utf8PathBuf> = Vec::new();
    // Files the formatter refused because its output would have lost
    // comments, respelled literals or changed the program (BT-5183).
    let mut refused_files: Vec<Utf8PathBuf> = Vec::new();
    let mut erlfmt_error_files: Vec<Utf8PathBuf> = Vec::new();

    for file in &collected.bt_files {
        let original = std::fs::read_to_string(file.as_std_path())
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", file))?;

        // .btscript files use identity formatting — the `// => X` assertion
        // comments must remain on separate lines for the btscript test runner
        // We only verify that the file parses without errors;
        // if it's valid, leave the file unchanged without running the unparser.
        if file.extension() == Some("btscript") {
            let tokens = lex_with_eof(&original);
            let (_, diags) = parse(tokens);
            let has_errors = diags.iter().any(|d| d.severity == Severity::Error);
            if has_errors {
                eprintln!("warning: skipping '{file}' (has parse errors)");
                skipped_files.push(file.clone());
            }
            continue;
        }

        // For .bt files, run the canonical format pipeline. It refuses a
        // source with parse errors — formatting a broken file could corrupt
        // it (the unparser converts Error nodes to comments) — and any result
        // that fails its round-trip check (a formatter bug), so a lossy
        // rewrite never reaches the disk.
        let formatted = match format(&original) {
            Ok(formatted) => formatted,
            Err(FormatError::ParseErrors) => {
                eprintln!("warning: skipping '{file}' (has parse errors)");
                skipped_files.push(file.clone());
                continue;
            }
            Err(e) => {
                eprintln!("error: not formatting '{file}': {e}");
                refused_files.push(file.clone());
                continue;
            }
        };

        if formatted == original {
            continue;
        }

        changed_files.push(file.clone());

        if check_only {
            print_unified_diff(file.as_str(), &original, &formatted);
        } else {
            // Atomic: an interrupted run must not leave a truncated source file.
            write_atomic(file, &formatted, ".beamtalk-fmt.")
                .into_diagnostic()
                .map_err(|e| miette::miette!("Failed to write '{}': {e}", file))?;
        }
    }

    // Format native .erl files via erlfmt.
    if !collected.erl_files.is_empty() {
        let root = collected.project_root.unwrap_or_else(|| {
            collected
                .erl_files
                .first()
                .and_then(|f| find_package_root(f))
                .unwrap_or_else(|| Utf8PathBuf::from("."))
        });

        match erlfmt::run_erlfmt(&collected.erl_files, check_only, &root) {
            Ok(result) => {
                changed_files.extend(result.changed_files);
                erlfmt_error_files.extend(result.error_files);
            }
            Err(e) => {
                eprintln!("warning: erlfmt failed: {e}");
            }
        }
    }

    // A refusal is a formatter bug and fails the run in both modes; in check
    // mode it joins the rest of the summary rather than hiding it.
    let refused_summary = (!refused_files.is_empty()).then(|| {
        let count = refused_files.len();
        let plural = if count == 1 { "" } else { "s" };
        format!(
            "{count} file{plural} could not be formatted without losing source (formatter bug; files left unchanged)"
        )
    });

    if check_only {
        let mut parts: Vec<String> = Vec::new();
        parts.extend(refused_summary);
        if !changed_files.is_empty() {
            let count = changed_files.len();
            let plural = if count == 1 { "" } else { "s" };
            parts.push(format!("{count} file{plural} would be reformatted"));
        }
        if !skipped_files.is_empty() {
            let count = skipped_files.len();
            let plural = if count == 1 { "" } else { "s" };
            parts.push(format!(
                "{count} file{plural} could not be checked (parse errors)"
            ));
        }
        if !erlfmt_error_files.is_empty() {
            let count = erlfmt_error_files.len();
            let plural = if count == 1 { "" } else { "s" };
            parts.push(format!(
                "{count} .erl file{plural} could not be checked (erlfmt errors)"
            ));
        }
        if !parts.is_empty() {
            miette::bail!("{}", parts.join("; "));
        }
    } else if let Some(summary) = refused_summary {
        miette::bail!("{summary}");
    }

    Ok(())
}

/// Print a unified diff between `original` and `formatted` for the given file
/// path. Output goes to stdout so it can be captured and piped.
fn print_unified_diff(path: &str, original: &str, formatted: &str) {
    let diff = TextDiff::from_lines(original, formatted);
    print!(
        "{}",
        diff.unified_diff()
            .header(&format!("a/{path}"), &format!("b/{path}"))
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write as _;

    /// Write `content` to a temp `.bt` file and return (dir, path).
    fn write_temp_bt(content: &str) -> (tempfile::TempDir, Utf8PathBuf) {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("test.bt");
        let mut f = std::fs::File::create(&path).expect("create temp file");
        f.write_all(content.as_bytes()).expect("write temp file");
        let utf8_path = Utf8PathBuf::from_path_buf(path).expect("utf8 path");
        (dir, utf8_path)
    }

    /// Write `content` to a temp `.btscript` file and return (dir, path).
    fn write_temp_btscript(content: &str) -> (tempfile::TempDir, Utf8PathBuf) {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("test.btscript");
        let mut f = std::fs::File::create(&path).expect("create temp file");
        f.write_all(content.as_bytes()).expect("write temp file");
        let utf8_path = Utf8PathBuf::from_path_buf(path).expect("utf8 path");
        (dir, utf8_path)
    }

    /// Helper to run `run_fmt` on a single path.
    fn run_fmt_single(path: &str, check_only: bool) -> Result<()> {
        run_fmt(&[path.to_string()], check_only)
    }

    /// A formatter that always reports a lost comment, standing in for a
    /// printer bug the round-trip check catches.
    fn lossy_formatter(_: &str) -> std::result::Result<String, FormatError> {
        Err(FormatError::CommentsChanged {
            missing: vec!["// c".to_string()],
            added: Vec::new(),
        })
    }

    /// BT-5183: when the formatter's round-trip check refuses a file, `fmt`
    /// leaves it byte-for-byte unchanged and exits non-zero, in both write and
    /// check mode, rather than skipping it silently.
    #[test]
    fn fmt_refused_by_round_trip_check_leaves_file_and_fails() {
        let source = "x foo: 1 // c\n  bar: 2\n";
        for check_only in [false, true] {
            let (_dir, path) = write_temp_bt(source);
            let result = run_fmt_with(&[path.to_string()], check_only, lossy_formatter);
            let err = result.expect_err("a refused file must fail the run");
            assert!(
                err.to_string()
                    .contains("could not be formatted without losing source"),
                "unexpected error: {err}"
            );
            let after = std::fs::read_to_string(path.as_std_path()).expect("read");
            assert_eq!(after, source, "a refused file must not be rewritten");
        }
    }

    /// In check mode a refusal joins the summary instead of hiding the other
    /// problems (here, a file with parse errors).
    #[test]
    fn fmt_check_reports_refusals_alongside_other_problems() {
        let dir = tempfile::tempdir().expect("temp dir");
        std::fs::write(dir.path().join("a.bt"), "x := 1\n").expect("write");
        std::fs::write(dir.path().join("b.bt"), "@@@ broken\n").expect("write");
        let dir_path = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).expect("utf8");
        let refuse_valid = |source: &str| {
            if source.starts_with("@@@") {
                Err(FormatError::ParseErrors)
            } else {
                lossy_formatter(source)
            }
        };
        let err = run_fmt_with(&[dir_path.to_string()], true, refuse_valid).expect_err("must fail");
        let text = err.to_string();
        assert!(text.contains("1 file could not be formatted"), "{text}");
        assert!(
            text.contains("could not be checked (parse errors)"),
            "{text}"
        );
    }

    #[test]
    fn fmt_check_already_formatted_exits_zero() {
        // An already-formatted file should not trigger a non-zero exit.
        // Format once to get canonical form, then check — must exit 0.
        let source = "Object subclass: Foo\n  bar => 42\n";
        let (_dir, path) = write_temp_bt(source);

        // Obtain the canonical form.
        run_fmt_single(path.as_str(), false).expect("fmt pass 1");
        let canonical = std::fs::read_to_string(path.as_std_path()).expect("read");

        // Write canonical form and check — should be a no-op.
        let (_dir2, path2) = write_temp_bt(&canonical);
        let result = run_fmt_single(path2.as_str(), true);
        assert!(
            result.is_ok(),
            "fmt-check should exit 0 on already-formatted file"
        );
    }

    /// BT-5017: the rewrite is staged and renamed, so an interrupted run cannot
    /// leave a truncated source. An in-place write kept the file's inode; a
    /// rename gives it a new one. No staging file is left behind, and a
    /// rewritten script keeps its mode.
    #[cfg(unix)]
    #[test]
    fn fmt_rewrites_by_rename_and_keeps_mode() {
        use std::os::unix::fs::{MetadataExt, PermissionsExt};
        let (dir, path) = write_temp_bt("Object subclass: Foo\n    bar   =>   42\n");
        std::fs::set_permissions(path.as_std_path(), std::fs::Permissions::from_mode(0o750))
            .unwrap();
        let before = std::fs::metadata(path.as_std_path()).unwrap().ino();
        run_fmt_single(path.as_str(), false).expect("fmt");
        let meta = std::fs::metadata(path.as_std_path()).unwrap();
        assert_ne!(
            meta.ino(),
            before,
            "fmt must replace the file, not truncate it"
        );
        assert_eq!(meta.permissions().mode() & 0o777, 0o750);
        let names: Vec<_> = std::fs::read_dir(dir.path()).unwrap().collect();
        assert_eq!(names.len(), 1, "staging file left behind: {names:?}");
    }

    #[test]
    fn fmt_idempotent() {
        // fmt(fmt(source)) == fmt(source) for a class definition.
        let source = "Actor subclass: Counter\n  state: value = 0\n\n  getValue => self.value\n";
        let (_dir, path) = write_temp_bt(source);

        // First format pass.
        run_fmt_single(path.as_str(), false).expect("fmt pass 1");
        let pass1 = std::fs::read_to_string(path.as_std_path()).expect("read pass1");

        // Second format pass.
        let (_dir2, path2) = write_temp_bt(&pass1);
        run_fmt_single(path2.as_str(), false).expect("fmt pass 2");
        let pass2 = std::fs::read_to_string(path2.as_std_path()).expect("read pass2");

        assert_eq!(pass1, pass2, "unparser output must be idempotent");
    }

    #[test]
    fn fmt_preserves_line_comments() {
        // Comments attached to AST nodes must survive the round-trip.
        let source = "// a module comment\nx := 42\n";
        let (_dir, path) = write_temp_bt(source);
        run_fmt_single(path.as_str(), false).expect("fmt");
        let result = std::fs::read_to_string(path.as_std_path()).expect("read");
        assert!(
            result.contains("// a module comment"),
            "line comment must be preserved; got: {result:?}"
        );
    }

    #[test]
    fn fmt_check_unformatted_exits_nonzero() {
        // A source that already has the canonical form plus an extra trailing
        // blank line is non-canonical: fmt-check must exit non-zero.
        let source = "x := 42\n";
        let (_dir, path) = write_temp_bt(source);
        run_fmt_single(path.as_str(), false).expect("fmt");
        let canonical = std::fs::read_to_string(path.as_std_path()).expect("read");

        // Append an extra blank line — the unparser will strip it.
        let non_canonical = format!("{canonical}\n");
        let (_dir2, path2) = write_temp_bt(&non_canonical);

        let result = run_fmt_single(path2.as_str(), true);
        assert!(
            result.is_err(),
            "fmt-check must exit non-zero for unformatted file"
        );
    }

    #[test]
    fn fmt_check_nonzero_message_mentions_file_count() {
        let source = "x := 42\n";
        let (_dir, path) = write_temp_bt(source);
        run_fmt_single(path.as_str(), false).expect("fmt");
        let canonical = std::fs::read_to_string(path.as_std_path()).expect("read");

        let non_canonical = format!("{canonical}\n");
        let (_dir2, path2) = write_temp_bt(&non_canonical);
        let err = run_fmt_single(path2.as_str(), true).unwrap_err();
        let msg = format!("{err}");
        assert!(
            msg.contains("file"),
            "error message must mention 'file'; got: {msg:?}"
        );
    }

    #[test]
    fn fmt_deduplicates_overlapping_paths() {
        // Passing the same path twice must not format the file twice.
        let source = "x := 42\n";
        let (_dir, path) = write_temp_bt(source);
        run_fmt_single(path.as_str(), false).expect("fmt");
        let canonical = std::fs::read_to_string(path.as_std_path()).expect("read");

        // Non-canonical version: appended blank line.
        let non_canonical = format!("{canonical}\n");
        let (_dir2, path2) = write_temp_bt(&non_canonical);
        let path_str = path2.as_str().to_string();

        // Pass the same path twice.
        let result = run_fmt(&[path_str.clone(), path_str], true);
        // The error message should say 1 file (not 2).
        let msg = format!("{}", result.unwrap_err());
        assert!(
            msg.contains("1 file"),
            "duplicate path should count as 1 file; got: {msg:?}"
        );
    }

    #[test]
    fn fmt_check_exits_nonzero_on_parse_error() {
        // A file with a parse error must cause fmt-check to exit non-zero.
        // The error message should mention "could not be checked".
        let bad_source = "@@@invalid beamtalk source@@@\n";
        let (_dir, path) = write_temp_bt(bad_source);
        let result = run_fmt_single(path.as_str(), true);
        let err = result.unwrap_err();
        let msg = format!("{err}");
        assert!(
            msg.contains("could not be checked"),
            "fmt-check must fail on parse errors; got: {msg:?}"
        );
    }

    // --- .btscript file support ---

    #[test]
    fn fmt_accepts_btscript_file() {
        // beamtalk fmt must accept .btscript files without rejecting them.
        let source = "3 + 4\n// => 7\n";
        let (_dir, path) = write_temp_btscript(source);
        let result = run_fmt_single(path.as_str(), false);
        assert!(
            result.is_ok(),
            "fmt must accept .btscript files; got: {result:?}"
        );
    }

    #[test]
    fn fmt_rejects_non_bt_btscript_file() {
        // A .txt file must still be rejected with a helpful error.
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("test.txt");
        std::fs::write(&path, "hello\n").expect("write");
        let utf8_path = Utf8PathBuf::from_path_buf(path).expect("utf8 path");
        let result = run_fmt_single(utf8_path.as_str(), false);
        assert!(result.is_err(), "fmt must reject .txt files");
        let msg = format!("{}", result.unwrap_err());
        assert!(
            msg.contains(".bt") || msg.contains("btscript"),
            "error must mention .bt/.btscript; got: {msg:?}"
        );
    }

    #[test]
    fn fmt_btscript_assertion_comments_preserved_idempotent() {
        // .btscript files use identity formatting — the file is never rewritten,
        // only checked for parse errors. Both passes must leave the file unchanged
        // and pass1 must equal the original source.
        let source = "3 + 4\n// => 7\n\n10 - 3\n// => 7\n";
        let (_dir, path) = write_temp_btscript(source);

        // Format once — file must not change (identity formatting).
        run_fmt_single(path.as_str(), false).expect("fmt pass 1");
        let pass1 = std::fs::read_to_string(path.as_std_path()).expect("read pass1");
        assert_eq!(
            pass1, source,
            "first fmt pass must not change .btscript file content"
        );

        // Format again — must also not change.
        let (_dir2, path2) = write_temp_btscript(&pass1);
        run_fmt_single(path2.as_str(), false).expect("fmt pass 2");
        let pass2 = std::fs::read_to_string(path2.as_std_path()).expect("read pass2");
        assert_eq!(
            pass1, pass2,
            "formatter must be idempotent on .btscript files"
        );
        assert!(
            pass1.contains("3 + 4\n// => 7"),
            "assertion comment must remain directly after expression (no extra blank line); got: {pass1:?}"
        );
    }

    #[test]
    fn fmt_btscript_directory_scan_includes_btscript_files() {
        // When pointing fmt at a directory, .btscript files must be included in
        // the scan. To make inclusion observable: give the .btscript file a parse
        // error and run fmt-check — if .btscript files are scanned, fmt-check
        // must report a "could not be checked" failure; if they are silently
        // ignored, fmt-check would succeed (false pass).
        let dir = tempfile::tempdir().expect("temp dir");
        let bt_path = dir.path().join("foo.bt");
        let btscript_path = dir.path().join("bar.btscript");
        // Valid .bt file — must not affect the outcome.
        std::fs::write(&bt_path, "x := 1\n").expect("write bt");
        // Intentionally invalid .btscript — triggers a parse error if scanned.
        std::fs::write(&btscript_path, "@@@invalid syntax@@@\n").expect("write btscript");

        let dir_str = Utf8PathBuf::from_path_buf(dir.path().to_path_buf())
            .expect("utf8 path")
            .to_string();
        let result = run_fmt(&[dir_str], true);
        let err_msg = format!("{}", result.unwrap_err());
        assert!(
            err_msg.contains("could not be checked"),
            "fmt-check must report parse error from .btscript file; got: {err_msg:?}"
        );
    }
}

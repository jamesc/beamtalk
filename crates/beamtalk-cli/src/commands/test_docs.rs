// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Markdown doctest runner: extract ` ```beamtalk ` code blocks and run them as btscript tests.
//!
//! **DDD Context:** CLI / Test System
//!
//! Finds `.md` files, extracts all ` ```beamtalk ` fenced code blocks (concatenated in order so
//! variable state is preserved across blocks), normalises inline `// =>` assertions to
//! next-line form, then hands the resulting btscript content to the same `EUnit` pipeline
//! used by `test_stdlib`.

use super::test_stdlib::{
    CompiledTestFile, compile_erl_files, compile_single_test_file, report_results,
    run_all_eunit_tests,
};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::{info, instrument, warn};

// ──────────────────────────────────────────────────────────────────────────
// Markdown extraction
// ──────────────────────────────────────────────────────────────────────────

/// Return `true` if the markdown content contains at least one ` ```beamtalk ` fence.
pub(crate) fn has_beamtalk_blocks(content: &str) -> bool {
    content.lines().any(|l| {
        let t = l.trim();
        t == "```beamtalk" || t == "``` beamtalk"
    })
}

/// Extract ` ```beamtalk ` code blocks that contain `// =>` assertions.
///
/// Only blocks that contain at least one `// =>` marker are included — blocks
/// without assertions (class definition examples) are silently
/// skipped. Qualifying blocks are concatenated in document order, separated by
/// a blank line so the parser treats them as independent top-level expressions
/// while still sharing variable scope.
pub(crate) fn extract_btscript_from_markdown(content: &str) -> String {
    let mut result = String::new();
    let mut in_block = false;
    let mut current_block = String::new();

    for line in content.lines() {
        let trimmed = line.trim();
        if !in_block && (trimmed == "```beamtalk" || trimmed == "``` beamtalk") {
            in_block = true;
            current_block.clear();
        } else if in_block && trimmed.starts_with("```") {
            in_block = false;
            // Only keep blocks that contain at least one assertion.
            if current_block.contains("// =>") {
                result.push_str(&current_block);
                result.push('\n'); // blank line between blocks
            }
            current_block.clear();
        } else if in_block {
            current_block.push_str(line);
            current_block.push('\n');
        }
    }

    result
}

/// Pre-process btscript content so that inline `// =>` assertions become next-line assertions.
///
/// The parser in `test_stdlib` only understands the next-line form:
/// ```text
/// expr
/// // => value
/// ```
///
/// Markdown code blocks often use the inline form for readability:
/// ```text
/// expr  // => value
/// ```
///
/// This function converts inline assertions by splitting such lines into two:
/// ```text
/// expr
/// // => value
/// ```
///
/// Lines that are already standalone `// => …` markers (or comments without `=>`) are left
/// unchanged.
pub(crate) fn normalize_inline_assertions(content: &str) -> String {
    let mut out = String::with_capacity(content.len() + 64);

    for line in content.lines() {
        // Detect an inline assertion: the line must contain `// =>` but must NOT start with `//`
        // (which would be a standalone comment/assertion line already).
        let trimmed = line.trim();
        if !trimmed.starts_with("//") {
            if let Some(pos) = line.find("// =>") {
                // Split into expression part and assertion part.
                let expr_part = line[..pos].trim_end();
                let assertion_part = line[pos..].trim_start(); // "// => value"
                if !expr_part.is_empty() {
                    out.push_str(expr_part);
                    out.push('\n');
                    out.push_str(assertion_part);
                    out.push('\n');
                    continue;
                }
            }
        }
        out.push_str(line);
        out.push('\n');
    }

    out
}

// ──────────────────────────────────────────────────────────────────────────
// File discovery
// ──────────────────────────────────────────────────────────────────────────

/// Find all `.md` files under `path` (file or directory, non-recursive walk).
///
/// If `path` is a single `.md` file, returns just that file.
/// If `path` is a directory, returns all `.md` files directly inside it (sorted).
fn find_md_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    if path.is_file() {
        if path.extension() == Some("md") {
            return Ok(vec![path.to_owned()]);
        }
        miette::bail!("Expected a .md file, got '{path}'");
    }

    let mut files = Vec::new();

    for entry in fs::read_dir(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{path}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let entry_path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", path))?;

        if entry_path.is_file() && entry_path.extension() == Some("md") {
            files.push(entry_path);
        }
    }

    files.sort();
    Ok(files)
}

// ──────────────────────────────────────────────────────────────────────────
// Main entry point
// ──────────────────────────────────────────────────────────────────────────

/// Extract beamtalk code blocks from `.md` files and write synthetic `.btscript` files.
///
/// Returns `(md_path, btscript_path)` pairs. Files with no blocks are silently skipped.
fn extract_to_btscript_files(
    md_files: &[Utf8PathBuf],
    btscript_dir: &Utf8Path,
) -> Result<Vec<(Utf8PathBuf, Utf8PathBuf)>> {
    let mut pairs: Vec<(Utf8PathBuf, Utf8PathBuf)> = Vec::new();
    for md_file in md_files {
        let content = fs::read_to_string(md_file)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read '{md_file}'"))?;
        let extracted = extract_btscript_from_markdown(&content);
        if extracted.trim().is_empty() {
            if has_beamtalk_blocks(&content) {
                warn!(
                    "'{md_file}' has beamtalk code blocks but none contain `// =>` assertions — skipped"
                );
            }
            continue;
        }
        let normalised = normalize_inline_assertions(&extracted);
        let stem = md_file
            .file_stem()
            .ok_or_else(|| miette::miette!("Markdown file has no stem: {md_file}"))?;
        // Sanitise stem for use as an Erlang module fragment.
        let safe_stem: String = stem
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        let btscript_path = btscript_dir.join(format!("{safe_stem}.btscript"));
        fs::write(&btscript_path, &normalised)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write btscript for '{md_file}'"))?;
        pairs.push((md_file.clone(), btscript_path));
    }
    Ok(pairs)
}

/// Run doctests extracted from Markdown files.
///
/// For each `.md` file found in `path`:
/// 1. Extract all ` ```beamtalk ` code blocks (concatenated, preserving state).
/// 2. Normalise inline `// =>` assertions to next-line form.
/// 3. Write the result to a temporary `.btscript` file.
/// 4. Compile and run through the same `EUnit` pipeline as `test_stdlib`.
///
/// Files with no ` ```beamtalk ` blocks are skipped silently.
#[instrument(skip_all)]
pub fn run_tests(path: &str, no_warnings: bool, quiet: bool, verbose: bool) -> Result<()> {
    info!("Starting doctest run");

    let test_path = Utf8PathBuf::from(path);
    if !test_path.exists() {
        miette::bail!("Path '{}' not found", test_path);
    }

    let md_files = find_md_files(&test_path)?;
    if md_files.is_empty() {
        println!("No .md files found in '{test_path}'");
        return Ok(());
    }

    // Create temporary build directory
    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory")?;
    let build_dir = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    // Separate temp dir for synthetic .btscript files (so they have clean names)
    let btscript_dir = build_dir.join("btscript");
    fs::create_dir_all(btscript_dir.as_std_path())
        .into_diagnostic()
        .wrap_err("Failed to create btscript staging directory")?;

    let btscript_files = extract_to_btscript_files(&md_files, &btscript_dir)?;

    if btscript_files.is_empty() {
        println!("No ```beamtalk code blocks found in '{test_path}'");
        return Ok(());
    }

    if !quiet {
        println!(
            "Compiling doctests from {} file(s)...",
            btscript_files.len()
        );
    }

    // Phase 1: Compile all synthetic .btscript files
    let mut compiled_files: Vec<CompiledTestFile> = Vec::new();
    let mut all_core_files: Vec<Utf8PathBuf> = Vec::new();
    let mut all_erl_files: Vec<Utf8PathBuf> = Vec::new();
    let mut all_fixture_modules: Vec<String> = Vec::new();

    for (md_path, btscript_path) in &btscript_files {
        let result = compile_single_test_file(btscript_path, &build_dir, no_warnings)
            .wrap_err_with(|| format!("Failed to compile doctests from '{md_path}'"))?;

        all_core_files.extend(result.core_files);
        all_erl_files.push(result.erl_file);
        all_fixture_modules.extend(result.fixture_modules);
        compiled_files.push(CompiledTestFile {
            source_file: md_path.clone(),
            module_name: result.test_module_name,
            assertion_count: result.test_count,
        });
    }

    all_fixture_modules.sort();
    all_fixture_modules.dedup();

    // Phase 2: Batch compile all .core → .beam
    if !all_core_files.is_empty() {
        let compiler = crate::beam_compiler::BeamCompiler::new(build_dir.clone());
        compiler
            .compile_batch(&all_core_files)
            .wrap_err("Failed to batch-compile doctest expression modules to BEAM")?;
    }

    // Phase 3: Batch compile all EUnit .erl → .beam
    compile_erl_files(&all_erl_files, &build_dir)?;

    // Phase 4: Run all EUnit test modules in a single BEAM process
    let test_module_names: Vec<&str> = compiled_files
        .iter()
        .map(|f| f.module_name.as_str())
        .collect();

    let total_tests: usize = compiled_files.iter().map(|f| f.assertion_count).sum();

    let eunit_result = run_all_eunit_tests(
        &test_module_names,
        &all_fixture_modules,
        &build_dir,
        verbose,
    )?;

    // Phase 5: Report results
    report_results(
        &compiled_files,
        &eunit_result,
        btscript_files.len(),
        total_tests,
        quiet,
    )
}

// ──────────────────────────────────────────────────────────────────────────
// Unit tests
// ──────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── extract_btscript_from_markdown ─────────────────────────────────────

    #[test]
    fn test_extract_empty_markdown() {
        assert_eq!(extract_btscript_from_markdown(""), "");
    }

    #[test]
    fn test_extract_no_beamtalk_blocks() {
        let md = "# Hello\n\n```rust\nlet x = 1;\n```\n";
        assert_eq!(extract_btscript_from_markdown(md), "");
    }

    #[test]
    fn test_extract_single_block() {
        let md = "Some text.\n\n```beamtalk\n1 + 2\n// => 3\n```\n\nMore text.\n";
        let expected = "1 + 2\n// => 3\n\n";
        assert_eq!(extract_btscript_from_markdown(md), expected);
    }

    #[test]
    fn test_extract_multiple_blocks_concatenated() {
        let md = "```beamtalk\nx := 10\n// => 10\n```\n\n```beamtalk\nx + 1\n// => 11\n```\n";
        let result = extract_btscript_from_markdown(md);
        // Both blocks should appear with a blank line separator
        assert!(result.contains("x := 10"));
        assert!(result.contains("x + 1"));
        // Blank line between blocks
        assert!(result.contains("\n\n"));
    }

    #[test]
    fn test_extract_block_with_space_variant() {
        let md = "``` beamtalk\n3 + 4\n// => 7\n```\n";
        let result = extract_btscript_from_markdown(md);
        assert!(result.contains("3 + 4"));
    }

    #[test]
    fn test_extract_ignores_other_fenced_blocks() {
        let md = "```erlang\n-module(foo).\n```\n\n```beamtalk\n1 + 1\n// => 2\n```\n";
        let result = extract_btscript_from_markdown(md);
        assert!(!result.contains("-module(foo)"));
        assert!(result.contains("1 + 1"));
    }

    #[test]
    fn test_extract_skips_blocks_without_assertions() {
        // BUnit-style blocks (no // =>) are silently skipped
        let md = "```beamtalk\nTestCase subclass: Foo\n  testBar => self assert: 1 equals: 1\n```\n\n```beamtalk\n3 + 4  // => 7\n```\n";
        let result = extract_btscript_from_markdown(md);
        assert!(
            !result.contains("TestCase subclass"),
            "BUnit block should be skipped"
        );
        assert!(result.contains("3 + 4"), "doctest block should be kept");
    }

    // ── normalize_inline_assertions ────────────────────────────────────────

    #[test]
    fn test_normalize_no_inline_assertions() {
        let content = "1 + 2\n// => 3\n";
        assert_eq!(normalize_inline_assertions(content), content);
    }

    #[test]
    fn test_normalize_inline_assertion() {
        let content = "3 + 4  // => 7\n";
        let result = normalize_inline_assertions(content);
        assert_eq!(result, "3 + 4\n// => 7\n");
    }

    #[test]
    fn test_normalize_multiple_inline_assertions() {
        let content = "1 + 1  // => 2\n2 + 2  // => 4\n";
        let result = normalize_inline_assertions(content);
        assert_eq!(result, "1 + 1\n// => 2\n2 + 2\n// => 4\n");
    }

    #[test]
    fn test_normalize_standalone_comment_untouched() {
        let content = "// This is a comment\n1 + 2\n// => 3\n";
        assert_eq!(normalize_inline_assertions(content), content);
    }

    #[test]
    fn test_normalize_does_not_split_standalone_assertion_line() {
        // A line that already IS `// => value` (no expression before it)
        let content = "// => 42\n";
        assert_eq!(normalize_inline_assertions(content), content);
    }

    #[test]
    fn test_normalize_mixed_content() {
        let content = "x := 5  // => 5\n// A comment\nx + 1\n// => 6\n";
        let result = normalize_inline_assertions(content);
        assert_eq!(result, "x := 5\n// => 5\n// A comment\nx + 1\n// => 6\n");
    }
}

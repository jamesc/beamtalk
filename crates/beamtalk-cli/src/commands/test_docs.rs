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
//!
//! ## Fixture support
//!
//! Markdown chapters can declare companion `.bt` fixture files with an HTML comment:
//!
//! ```markdown
//! <!-- btfixture: fixtures/point.bt -->
//! ```
//!
//! The runner resolves the path relative to the markdown file, injects a `// @load` directive
//! into the generated btscript, and compiles the fixture before running assertions.
//! Fixture files that contain `TestCase subclass:` are also run as `BUnit` test suites.

use super::test_stdlib::{
    CompiledTestFile, compile_erl_files, compile_single_test_file, report_results,
    run_all_eunit_tests,
};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write;
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
/// Find the byte position of `// =>` in `line`, skipping occurrences inside
/// double-quoted string literals.
///
/// Returns `None` if no unquoted `// =>` is present.
fn find_assertion_marker_pos(line: &str) -> Option<usize> {
    let bytes = line.as_bytes();
    let len = bytes.len();
    let marker = b"// =>";
    let marker_len = marker.len();
    let mut i = 0;
    let mut in_string = false;

    while i < len {
        if in_string {
            if bytes[i] == b'\\' {
                // Skip escaped character (e.g. \" inside a string)
                i += 2;
                continue;
            } else if bytes[i] == b'"' {
                in_string = false;
            }
            i += 1;
        } else if bytes[i] == b'"' {
            in_string = true;
            i += 1;
        } else if i + marker_len <= len && &bytes[i..i + marker_len] == marker {
            return Some(i);
        } else {
            i += 1;
        }
    }
    None
}

pub(crate) fn normalize_inline_assertions(content: &str) -> String {
    let mut out = String::with_capacity(content.len() + 64);

    for line in content.lines() {
        // Detect an inline assertion: the line must contain `// =>` outside
        // string literals and must NOT start with `//` (standalone comment).
        let trimmed = line.trim();
        if !trimmed.starts_with("//") {
            if let Some(pos) = find_assertion_marker_pos(line) {
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
// Fixture support
// ──────────────────────────────────────────────────────────────────────────

/// Extract `<!-- btfixture: rel/path.bt -->` declarations from markdown content.
///
/// Returns paths as written (relative to the markdown file's directory).
fn extract_fixture_paths(content: &str) -> Vec<String> {
    content
        .lines()
        .filter_map(|line| {
            line.trim()
                .strip_prefix("<!-- btfixture:")
                .and_then(|r| r.strip_suffix("-->"))
                .map(|p| p.trim().to_string())
                .filter(|p| !p.is_empty())
        })
        .collect()
}

/// Returns `true` if Beamtalk source defines at least one `TestCase subclass:`.
fn has_bunit_tests(content: &str) -> bool {
    content.contains("TestCase subclass:")
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

/// A `BUnit` fixture file discovered via `<!-- btfixture: ... -->`.
struct BunitFixture {
    /// The markdown file that declared this fixture.
    source_md: Utf8PathBuf,
    /// Absolute path to the `.bt` fixture file.
    fixture_path: Utf8PathBuf,
}

/// Results of extracting btscript files from markdown.
struct ExtractionResult {
    /// `(md_path, btscript_path)` for files that had runnable inline assertions.
    pairs: Vec<(Utf8PathBuf, Utf8PathBuf)>,
    /// Count of files that had beamtalk fences but all lacked `// =>` markers.
    skipped_with_blocks: usize,
    /// `BUnit` fixture modules that should be added to the `EUnit` test run
    /// in addition to the inline-assertion modules.
    bunit_fixtures: Vec<BunitFixture>,
}

/// Extract beamtalk code blocks from `.md` files and write synthetic `.btscript` files.
///
/// Fixture declarations (`<!-- btfixture: path.bt -->`) are resolved relative to each
/// markdown file and injected as `// @load` directives in the generated btscript.
/// Fixture files containing `TestCase subclass:` are tracked as `BUnit` test modules
/// so the caller can include them in the `EUnit` run.
fn extract_to_btscript_files(
    md_files: &[Utf8PathBuf],
    btscript_dir: &Utf8Path,
) -> Result<ExtractionResult> {
    let mut pairs: Vec<(Utf8PathBuf, Utf8PathBuf)> = Vec::new();
    let mut skipped_with_blocks = 0usize;
    let mut bunit_fixtures: Vec<BunitFixture> = Vec::new();

    for md_file in md_files {
        let content = fs::read_to_string(md_file)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read '{md_file}'"))?;

        // Resolve fixture declarations to absolute `// @load` directives.
        let fixture_rel_paths = extract_fixture_paths(&content);
        let md_dir = md_file.parent().unwrap_or(Utf8Path::new("."));

        let mut load_preamble = String::new();
        for rel_path in &fixture_rel_paths {
            let fixture_path = md_dir.join(rel_path);
            if !fixture_path.exists() {
                miette::bail!(
                    "btfixture '{rel_path}' declared in '{md_file}' not found \
                     (resolved to '{fixture_path}')"
                );
            }
            // Canonicalise so the path works regardless of CWD at compile time.
            let abs_path = fixture_path
                .canonicalize_utf8()
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!("Failed to canonicalise fixture path '{fixture_path}'")
                })?;
            let _ = writeln!(load_preamble, "// @load {abs_path}");

            // If the fixture contains BUnit tests, record the module name so it
            // gets added to the EUnit run (not just loaded).
            let fixture_src = fs::read_to_string(&abs_path)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read fixture '{abs_path}'"))?;
            if has_bunit_tests(&fixture_src) {
                bunit_fixtures.push(BunitFixture {
                    source_md: md_file.clone(),
                    fixture_path: abs_path.clone(),
                });
            }
        }

        let extracted = extract_btscript_from_markdown(&content);

        // Combine load preamble with extracted inline assertions.
        let combined = if load_preamble.is_empty() {
            extracted
        } else {
            format!("{load_preamble}\n{extracted}")
        };

        if combined.trim().is_empty() {
            if has_beamtalk_blocks(&content) {
                warn!(
                    "'{md_file}' has beamtalk code blocks but none contain `// =>` assertions — skipped"
                );
                skipped_with_blocks += 1;
            }
            continue;
        }
        let normalised = normalize_inline_assertions(&combined);
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
        // Prefix with a zero-padded index to prevent collisions when two distinct
        // filenames sanitise to the same safe_stem (e.g. "01-foo.md" and "01_foo.md"
        // both become "01_foo"). The `doc` prefix ensures the stem (and thus the
        // derived Erlang module name) always starts with a letter, which is required
        // by the Erlang parser.
        let unique_stem = format!("doc{:03}_{safe_stem}", pairs.len());
        let btscript_path = btscript_dir.join(format!("{unique_stem}.btscript"));
        fs::write(&btscript_path, &normalised)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write btscript for '{md_file}'"))?;
        pairs.push((md_file.clone(), btscript_path));
    }

    bunit_fixtures.sort_by(|a, b| a.fixture_path.cmp(&b.fixture_path));
    bunit_fixtures.dedup_by(|a, b| a.fixture_path == b.fixture_path);

    Ok(ExtractionResult {
        pairs,
        skipped_with_blocks,
        bunit_fixtures,
    })
}

/// Generate `EUnit` wrappers for `BUnit` fixture files so they can be run by `EUnit`.
///
/// `BUnit` `.bt` files compile to Beamtalk class modules (via `compile_fixture`),
/// but `EUnit` needs a wrapper that creates instances and dispatches test methods.
/// This function discovers `TestCase` subclasses in each fixture, generates the
/// corresponding `.erl` wrappers, and registers them for compilation and execution.
fn generate_bunit_wrappers(
    bunit_fixtures: &[BunitFixture],
    build_dir: &Utf8Path,
    all_erl_files: &mut Vec<Utf8PathBuf>,
    compiled_files: &mut Vec<CompiledTestFile>,
) -> Result<()> {
    for fixture in bunit_fixtures {
        let (test_classes, _loads) = super::test::discover_test_classes(&fixture.fixture_path)
            .wrap_err_with(|| {
                format!(
                    "Failed to discover test classes in '{}'",
                    fixture.fixture_path
                )
            })?;

        for test_class in &test_classes {
            let eunit_module = format!("{}_tests", test_class.module_name);
            let erl_source =
                super::test::generate_eunit_wrapper(test_class, fixture.fixture_path.as_str());
            let erl_file = build_dir.join(format!("{eunit_module}.erl"));
            fs::write(&erl_file, &erl_source)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to write EUnit wrapper for '{eunit_module}'"))?;
            all_erl_files.push(erl_file);
            compiled_files.push(CompiledTestFile {
                source_file: fixture.source_md.clone(),
                module_name: eunit_module,
                assertion_count: test_class.test_methods.len(),
            });
        }
    }
    Ok(())
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

    let extraction = extract_to_btscript_files(&md_files, &btscript_dir)?;
    let btscript_files = extraction.pairs;
    let skipped_with_blocks = extraction.skipped_with_blocks;
    let bunit_fixtures = extraction.bunit_fixtures;

    if btscript_files.is_empty() && bunit_fixtures.is_empty() {
        if skipped_with_blocks > 0 {
            miette::bail!(
                "No runnable doctests found in '{test_path}': {skipped_with_blocks} file(s) had \
                 beamtalk code blocks but none contained `// =>` assertions. \
                 Add `// => value` to expression lines or use `beamtalk test` for BUnit tests."
            );
        }
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

    // Phase 1b: Generate EUnit wrappers for BUnit fixture files.
    generate_bunit_wrappers(
        &bunit_fixtures,
        &build_dir,
        &mut all_erl_files,
        &mut compiled_files,
    )?;

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
    // Include both inline-assertion modules and BUnit fixture modules.
    // BUnit fixtures are already in compiled_files, so just collect all module names.
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
    let file_count = btscript_files.len() + bunit_fixtures.len();
    report_results(
        &compiled_files,
        &eunit_result,
        file_count,
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

    #[test]
    fn test_normalize_does_not_split_marker_inside_string_literal() {
        // "// =>" inside a double-quoted string is NOT an assertion marker.
        let content = "Transcript show: \"literal // => text\"\n";
        assert_eq!(normalize_inline_assertions(content), content);
    }

    #[test]
    fn test_normalize_splits_real_assertion_after_string_literal() {
        // A real assertion after a string expression is still split correctly.
        let content = "\"hello\" size  // => 5\n";
        let result = normalize_inline_assertions(content);
        assert_eq!(result, "\"hello\" size\n// => 5\n");
    }

    #[test]
    fn test_normalize_marker_in_string_with_real_assertion() {
        // String literal contains "// =>" AND there's a real assertion after.
        // The string content should win — only the outside marker splits.
        let content = "x greet: \"hi // => bye\"  // => ok\n";
        let result = normalize_inline_assertions(content);
        assert_eq!(result, "x greet: \"hi // => bye\"\n// => ok\n");
    }

    // ── find_assertion_marker_pos ──────────────────────────────────────────

    #[test]
    fn test_find_marker_not_in_string() {
        assert_eq!(find_assertion_marker_pos("x + 1  // => 2"), Some(7));
    }

    #[test]
    fn test_find_marker_inside_string_returns_none() {
        assert_eq!(
            find_assertion_marker_pos("Transcript show: \"literal // => text\""),
            None
        );
    }

    #[test]
    fn test_find_marker_after_string() {
        // Marker appears after a closed string literal — should be found.
        let line = "\"hello\" size  // => 5";
        let pos = find_assertion_marker_pos(line).expect("marker should be found");
        assert!(pos > 0);
        assert_eq!(&line[pos..], "// => 5");
    }

    #[test]
    fn test_find_marker_escaped_quote_does_not_end_string() {
        // "\\"" — escaped quote inside string, marker is still inside string.
        let line = "x := \"say \\\"// => nope\\\"\"";
        assert_eq!(find_assertion_marker_pos(line), None);
    }
}

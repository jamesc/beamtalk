// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `BUnit` / `EUnit` execution and output parsing for `beamtalk test`.
//!
//! **DDD Context:** CLI / Test System
//!
//! Phase 3 of the test pipeline: launches the BEAM test runner for the
//! compiled `BUnit` modules and the native `EUnit` modules, then parses the
//! runner's JSON / `EUnit` output into result structs that the reporting phase
//! in the parent module renders. Moved out of `commands/test.rs` (BT-4700);
//! behaviour is unchanged.

use super::{TestPipeline, util};
use beamtalk_codegen::core_erlang::escape_atom_chars;
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::{debug, info, warn};

// ──────────────────────────────────────────────────────────────────────────
// BUnit runner
// ──────────────────────────────────────────────────────────────────────────

/// Result from running `BUnit` tests via `beamtalk_test_runner:run_all/1`.
///
/// Holds the parsed JSON result containing aggregated test metrics and per-test details.
#[derive(Debug)]
pub(super) struct BunitResult {
    /// Total number of tests across all classes.
    pub(super) total: usize,
    /// Number of passing tests.
    pub(super) passed: usize,
    /// Number of failing tests.
    pub(super) failed: usize,
    /// Number of skipped tests.
    #[allow(dead_code)] // populated for future skip-reason display
    pub(super) skipped: usize,
    /// Total duration in seconds.
    #[allow(dead_code)] // populated for future per-class duration display
    pub(super) duration: f64,
    /// Per-test result details (name, status, optional error).
    pub(super) tests: Vec<TestResultDetail>,
}

#[derive(Debug)]
pub(super) struct TestResultDetail {
    pub(super) name: String,
    pub(super) class: Option<String>,
    pub(super) status: String, // "pass", "fail", "skip"
    pub(super) error: Option<String>,
}

/// Parsed per-module `EUnit` output sections, split by `NATIVE_EUNIT_DONE` markers.
pub(super) struct EunitModuleSections {
    /// Per-module results: `(module_name, passed, failed, skipped)`.
    pub(super) modules: Vec<(String, usize, usize, usize)>,
    /// Total passing tests across all modules.
    pub(super) total_passed: usize,
    /// Total failing tests across all modules.
    pub(super) total_failed: usize,
    /// Total skipped tests across all modules.
    pub(super) total_skipped: usize,
    /// Captured `EUnit` output from failing modules.
    pub(super) eunit_output: String,
}

/// Result from running native `EUnit` tests.
#[derive(Debug, Default)]
pub(super) struct NativeEunitResult {
    /// Total number of native `EUnit` tests (passed + failed + skipped).
    pub(super) total: usize,
    /// Number of passing tests.
    pub(super) passed: usize,
    /// Number of failing tests.
    pub(super) failed: usize,
    /// Number of skipped tests.
    #[allow(dead_code)] // populated for future skip-reason display
    pub(super) skipped: usize,
    /// Per-module results: `(module_name, passed, failed, skipped)`.
    pub(super) modules: Vec<(String, usize, usize, usize)>,
    /// Captured `EUnit` output (failures, errors).
    pub(super) output: String,
}

/// Combined test results from `BUnit` and native `EUnit` runs.
#[derive(Debug)]
pub(super) struct TestResults {
    pub(super) bunit: Option<BunitResult>,
    pub(super) native: Option<NativeEunitResult>,
}

/// Phase 3: Run all tests (`BUnit` + native `EUnit`).
///
/// Runs `BUnit` tests via `beamtalk_test_runner:run_all(Jobs)` if any `BUnit`
/// tests were compiled, then runs native `EUnit` test modules if any were
/// discovered in `native/test/`.
pub(super) fn execute_tests(pipeline: &TestPipeline) -> Result<TestResults> {
    println!("Running tests...\n");

    let bunit = if pipeline.compiled_tests.is_empty() && pipeline.compiled_doc_tests.is_empty() {
        None
    } else {
        Some(run_bunit_tests(pipeline)?)
    };

    let native = if pipeline.native_test_modules.is_empty() {
        None
    } else {
        Some(run_native_eunit_tests(pipeline)?)
    };

    Ok(TestResults { bunit, native })
}

/// Run tests via `beamtalk_test_runner:run_all(Jobs)` in a single BEAM process.
///
/// Writes the ordered list of package/fixture/test module names to a file in
/// the pipeline's build directory and returns a short `-eval` fragment that
/// loads them via `file:consult/1` + `ensure_loaded_or_warn/1`.
///
/// Windows' `CreateProcess` caps the full command line at 32,767 characters.
/// Enumerating every module as an individual `ensure_loaded_or_warn('mod')`
/// call directly in the `-eval` string (as this used to do) blew past that
/// limit once the `stdlib` package's `package_modules` list grew into the
/// hundreds, failing with `os error 206` on Windows CI only — Unix's much
/// larger `ARG_MAX` never showed the problem. Reading the list from a file
/// keeps the `-eval` argument a constant, small size regardless of module
/// count.
fn build_load_command(pipeline: &TestPipeline) -> Result<String> {
    // Use ensure_loaded_or_warn for consistent on_load failure reporting.
    // Order matters: packages must load (and register their classes) before
    // fixtures and test classes that depend on them.
    let modules = pipeline
        .package_modules
        .iter()
        .map(String::as_str)
        .chain(pipeline.all_fixture_modules.iter().map(String::as_str))
        .chain(
            pipeline
                .compiled_tests
                .iter()
                .map(|t| t.test_class.module_name.as_str()),
        );

    // escape_atom_chars mirrors the Core Erlang codegen path (leaf::atom):
    // module names can contain characters (e.g. `'`, `\`) that would
    // otherwise break the quoted-atom terms below and fail `file:consult/1`.
    let mut contents = String::new();
    for m in modules {
        contents.push('\'');
        contents.push_str(&escape_atom_chars(m));
        contents.push_str("'.\n");
    }

    if contents.is_empty() {
        return Ok(String::new());
    }

    let list_path = pipeline.build_dir.join("bunit_load_modules.terms");
    fs::write(&list_path, contents)
        .into_diagnostic()
        .wrap_err("Failed to write BUnit module load list")?;
    let list_path_str = util::to_forward_slash(list_path.as_str());

    Ok(format!(
        "{{ok, LoadModules}} = file:consult(\"{list_path_str}\"), \
         lists:foreach(fun beamtalk_test_runner:ensure_loaded_or_warn/1, LoadModules), "
    ))
}

/// Build a BEAM command (`erl -noshell`) with `-pa` paths for the build dir,
/// package ebin dirs, and runtime paths, then appends the `-eval` command.
///
/// Shared by both `BUnit` and native `EUnit` test runners.
fn build_beam_test_command(
    pipeline: &TestPipeline,
    pa_args: &[std::ffi::OsString],
    eval_cmd: &str,
) -> std::process::Command {
    let mut cmd = beamtalk_workspace::erl_command();
    cmd.arg("-noshell")
        .arg("-pa")
        .arg(util::to_forward_slash(pipeline.build_dir.as_str()));

    // Add all package ebin directories to code path
    for ebin_dir in &pipeline.package_ebin_dirs {
        cmd.arg("-pa")
            .arg(util::to_forward_slash(ebin_dir.as_str()));
    }

    for arg in pa_args {
        cmd.arg(arg);
    }

    cmd.arg("-eval").arg(eval_cmd);
    cmd
}

/// Parse `BUnit` runner JSON output into a `BunitResult`.
///
/// Extracts the JSON line from stdout (skipping Erlang error/info reports),
/// deserializes the summary fields (`total`, `passed`, `failed`, `skipped`,
/// `duration`) and the `tests` array into structured results.
fn parse_bunit_result_json(stdout: &str, stderr: &str) -> Result<BunitResult> {
    // Extract JSON from stdout -- skip any Erlang error/info reports that may precede it.
    // The JSON line starts with `{"` (a JSON object key). Using `{"` instead of just `{`
    // avoids matching Erlang error tuples like `{error, {beamtalk_error,...}}` which also
    // start with `{` but are not valid JSON.
    let json_line = stdout
        .lines()
        .find(|line| line.starts_with("{\""))
        .ok_or_else(|| {
            let combined = format!("{stdout}\n{stderr}");
            miette::miette!("No JSON output from BUnit runner:\n{combined}")
        })?;

    // Parse JSON result
    let result_json: serde_json::Value = serde_json::from_str(json_line)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to parse BUnit JSON result: {json_line}"))?;

    // Deserialize into BunitResult
    let parse_usize = |key: &str| -> Result<usize> {
        result_json[key]
            .as_u64()
            .map(|v| usize::try_from(v).unwrap_or(usize::MAX))
            .ok_or_else(|| miette::miette!("Missing '{key}' in BUnit result"))
    };
    let total = parse_usize("total")?;
    let passed = parse_usize("passed")?;
    let failed = parse_usize("failed")?;
    let skipped = parse_usize("skipped")?;
    let duration = result_json["duration"]
        .as_f64()
        .ok_or_else(|| miette::miette!("Missing 'duration' in BUnit result"))?;

    let tests_array = result_json["tests"]
        .as_array()
        .ok_or_else(|| miette::miette!("Missing 'tests' array in BUnit result"))?;

    let tests = tests_array
        .iter()
        .map(|t| {
            let name = t["name"].as_str().unwrap_or("unknown").to_string();
            let class = t["class"].as_str().map(String::from);
            let status = t["status"].as_str().unwrap_or("unknown").to_string();
            let error = t["error"].as_str().map(String::from);
            TestResultDetail {
                name,
                class,
                status,
                error,
            }
        })
        .collect();

    Ok(BunitResult {
        total,
        passed,
        failed,
        skipped,
        duration,
        tests,
    })
}

/// Build cover instrumentation preamble/epilogue for the `BUnit` eval command.
///
/// Gated on the `BUNIT_COVER` env var (mirrors `STDLIB_COVER` in
/// `test_stdlib.rs`). Instruments all four Beamtalk app ebins that carry
/// Erlang abstract code; the compiled `bt@*` stdlib modules have no abstract
/// code and are silently skipped by `cover:compile_beam_directory/1`. The
/// result is exported to `_build/test/cover/bunit.coverdata` so the
/// `coverage-all` wildcard merge folds it into the badge.
fn bunit_cover_fragments(
    beam_paths: &beamtalk_cli::repl_startup::BeamPaths,
    runtime_dir: &std::path::Path,
) -> (String, String) {
    if std::env::var("BUNIT_COVER").is_err() {
        return (String::new(), String::new());
    }

    let cover_dir = runtime_dir.join("_build/test/cover");
    let _ = std::fs::create_dir_all(&cover_dir);
    let cover_export_path = cover_dir.join("bunit.coverdata");
    info!(
        "BUnit cover mode enabled, will export to {}",
        cover_export_path.display()
    );

    let preamble = format!(
        "cover:start(), \
         cover:compile_beam_directory(\"{runtime_ebin}\"), \
         cover:compile_beam_directory(\"{workspace_ebin}\"), \
         cover:compile_beam_directory(\"{compiler_ebin}\"), \
         cover:compile_beam_directory(\"{stdlib_ebin}\"), ",
        runtime_ebin = beam_paths.runtime_ebin.display(),
        workspace_ebin = beam_paths.workspace_ebin.display(),
        compiler_ebin = beam_paths.compiler_ebin.display(),
        stdlib_ebin = beam_paths.stdlib_erlang_ebin.display(),
    );
    let epilogue = format!(
        "cover:export(\"{export}\"), cover:stop(), ",
        export = cover_export_path.display(),
    );
    (preamble, epilogue)
}

/// Calls the `BUnit` runner directly, bypassing `EUnit` wrappers. The runner
/// discovers test classes, executes them with the specified concurrency level,
/// and returns aggregated results as JSON.
fn run_bunit_tests(pipeline: &TestPipeline) -> Result<BunitResult> {
    debug!("Running BUnit tests with jobs={}", pipeline.jobs);

    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()
        .wrap_err("Cannot find Erlang runtime directory")?;
    let beam_paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let pa_args = beamtalk_cli::repl_startup::beam_pa_args(&beam_paths);

    let load_cmd = build_load_command(pipeline)?;

    // ADR 0072: Start hex dep OTP applications before tests
    let hex_deps_start_cmd =
        beamtalk_cli::repl_startup::hex_deps_start_fragment(&pipeline.hex_dep_names);

    let root_package_start = pipeline.root_package_fragment();

    // BUNIT_COVER: instrument the runtime under Erlang `cover` so the BUnit
    // suite (which drives beamtalk_test_case, beamtalk_test_runner, and the
    // full dispatch/object/class machinery via real .bt TestCase classes)
    // contributes to the Erlang coverage badge — mirroring the existing
    // STDLIB_COVER / E2E_COVER passes. The exported bunit.coverdata is picked
    // up by `just coverage-all`'s wildcard merge.
    let (cover_preamble, cover_epilogue) = bunit_cover_fragments(&beam_paths, &runtime_dir);

    // `beamtalk test` runs no workspace supervisor, so nothing registers the
    // workspace callback. Register `beamtalk_repl_eval` when its code is loadable so
    // the runtime's pure classification/compile calls answer as they did before they
    // went through `beamtalk_workspace_callback`. This leaves a registered callback
    // with no recorded capabilities, a combination only `beamtalk test` produces; a
    // workspace supervisor started and stopped by a test restores it when it stops.
    let eval_cmd = format!(
        "{cover_preamble}\
         {{ok, _}} = application:ensure_all_started(beamtalk_stdlib), \
         application:set_env(beamtalk_stdlib, full_traces, {full_traces}), \
         ok = beamtalk_workspace_callback:register_if_loaded(beamtalk_repl_eval), \
         {root_package_start}\
         {hex_deps_start_cmd}\
         {load_cmd}\
         Result = beamtalk_test_runner:run_all({jobs}), \
         JSON = beamtalk_test_runner:result_to_json(Result), \
         io:format(\"~s~n\", [JSON]), \
         {cover_epilogue}\
         init:stop(case beamtalk_test_runner:result_has_passed(Result) of true -> 0; _ -> 1 end).",
        jobs = pipeline.jobs,
        full_traces = pipeline.full_traces
    );

    let mut cmd = build_beam_test_command(pipeline, &pa_args, &eval_cmd);

    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run BUnit tests")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    debug!("BUnit stdout: {}", stdout);
    debug!("BUnit stderr: {}", stderr);

    parse_bunit_result_json(&stdout, &stderr)
}

/// Parse `EUnit`'s summary line from verbose output to extract test counts.
///
/// `EUnit` outputs one of two summary formats:
/// - `"  N tests passed."` — all passed
/// - `"  Failed: F.  Skipped: S.  Passed: P."` — mixed results
///
/// Returns `(passed, failed, skipped)`. Falls back to `(0, 0, 0)` if no summary found.
fn parse_eunit_summary(output: &str) -> (usize, usize, usize) {
    for line in output.lines().rev() {
        let trimmed = line.trim();

        // "All N tests passed.", "N tests passed.", or "Test passed." (single test)
        if trimmed.ends_with("tests passed.") || trimmed == "Test passed." {
            if trimmed == "Test passed." {
                return (1, 0, 0);
            }
            if let Some(n_str) = trimmed
                .strip_suffix(" tests passed.")
                .and_then(|s| s.strip_prefix("All ").or(Some(s)))
                && let Ok(n) = n_str.parse::<usize>()
            {
                return (n, 0, 0);
            }
        }

        // "Failed: F.  Skipped: S.  Passed: P."
        if trimmed.starts_with("Failed:") && trimmed.contains("Passed:") {
            let mut passed = 0;
            let mut failed = 0;
            let mut skipped = 0;
            for segment in trimmed.split('.') {
                let seg = segment.trim();
                if let Some(val) = seg.strip_prefix("Failed:") {
                    failed = val.trim().parse().unwrap_or(0);
                } else if let Some(val) = seg.strip_prefix("Passed:") {
                    passed = val.trim().parse().unwrap_or(0);
                } else if let Some(val) = seg.strip_prefix("Skipped:") {
                    skipped = val.trim().parse().unwrap_or(0);
                }
            }
            return (passed, failed, skipped);
        }
    }
    (0, 0, 0)
}

/// Parse `EUnit` verbose output into per-module results using `NATIVE_EUNIT_DONE` markers.
///
/// Splits stdout by marker lines to attribute each `EUnit` summary to the correct
/// module. Falls back to counting modules as all-passed or all-failed if no
/// markers are found (e.g. `EUnit` crashed before emitting any output).
///
fn parse_eunit_module_sections(
    stdout: &str,
    success: bool,
    native_test_modules: &[String],
) -> EunitModuleSections {
    let mut modules = Vec::new();
    let mut total_passed: usize = 0;
    let mut total_failed: usize = 0;
    let mut total_skipped: usize = 0;
    let mut eunit_output = String::new();

    // Split output into per-module sections using the marker lines
    let mut current_section = String::new();
    let mut module_idx = 0;
    for line in stdout.lines() {
        if let Some(module_name) = line.strip_prefix("NATIVE_EUNIT_DONE:") {
            // Parse the section for this module's EUnit summary
            let (passed, mut failed, skipped) = parse_eunit_summary(&current_section);

            // If EUnit crashed before printing a summary (e.g. {error, Reason}),
            // parse_eunit_summary returns (0, 0, 0). Detect this by checking the
            // exit status -- if the process failed and no tests were counted, treat
            // the module as having 1 failure.
            if passed == 0 && failed == 0 && skipped == 0 && !success {
                // Check if the section contains EUnit error indicators
                if current_section.contains("*failed*")
                    || current_section.contains("Error in")
                    || current_section.contains("{error,")
                {
                    failed = 1;
                }
            }

            modules.push((module_name.to_string(), passed, failed, skipped));
            total_passed += passed;
            total_failed += failed;
            total_skipped += skipped;

            if failed > 0 {
                eunit_output.push_str(&current_section);
            }

            current_section.clear();
            module_idx += 1;
        } else {
            current_section.push_str(line);
            current_section.push('\n');
        }
    }

    // If the stream ended with accumulated output but no DONE marker (e.g. BEAM crash),
    // flush the last module's section as a failure so results aren't silently dropped.
    if !current_section.is_empty() && module_idx > 0 && module_idx < native_test_modules.len() {
        let module_name = &native_test_modules[module_idx];
        warn!(
            module = %module_name,
            "BEAM output ended without NATIVE_EUNIT_DONE marker — reporting partial results"
        );

        let (passed, mut failed, skipped) = parse_eunit_summary(&current_section);
        // If the BEAM process failed (non-zero exit), ensure at least 1 failure
        // is reported — even if a partial summary showed "All tests passed" for
        // this module before the crash.
        if failed == 0 && !success {
            failed = 1;
        }

        eunit_output.push_str(&current_section);
        modules.push((module_name.clone(), passed, failed, skipped));
        total_passed += passed;
        total_failed += failed;
        total_skipped += skipped;
    }

    // If no markers found at all, fall back to counting test modules as all-passed or all-failed
    if module_idx == 0 && !native_test_modules.is_empty() {
        warn!("No NATIVE_EUNIT_DONE markers found in EUnit output");
        for module_name in native_test_modules {
            if success {
                modules.push((module_name.clone(), 1, 0, 0));
                total_passed += 1;
            } else {
                modules.push((module_name.clone(), 0, 1, 0));
                total_failed += 1;
            }
        }
        eunit_output = stdout.to_string();
    }

    EunitModuleSections {
        modules,
        total_passed,
        total_failed,
        total_skipped,
        eunit_output,
    }
}

/// Run native `EUnit` test modules in a BEAM process.
///
/// Executes `eunit:test/2` for each discovered native test module and
/// collects per-module pass/fail counts by parsing `EUnit`'s verbose
/// summary output.
fn run_native_eunit_tests(pipeline: &TestPipeline) -> Result<NativeEunitResult> {
    debug!(
        count = pipeline.native_test_modules.len(),
        "Running native EUnit tests"
    );

    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()
        .wrap_err("Cannot find Erlang runtime directory")?;
    let beam_paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let pa_args = beamtalk_cli::repl_startup::beam_pa_args(&beam_paths);

    // ADR 0072: Start hex dep OTP applications before tests
    let hex_deps_start_cmd =
        beamtalk_cli::repl_startup::hex_deps_start_fragment(&pipeline.hex_dep_names);

    let root_package_start = pipeline.root_package_fragment();

    // Build a list of module atoms for EUnit
    let module_list = pipeline
        .native_test_modules
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    // Inline Erlang: run EUnit per module with `verbose` so per-test
    // pass/fail output is printed to stdout. After each module completes,
    // emit a `NATIVE_EUNIT_DONE:<module>` marker line. The Rust side
    // parses EUnit's own summary lines to get exact pass/fail counts
    // (e.g. "  2 tests passed." or "  Failed: 1.  Skipped: 0.  Passed: 2.")
    // rather than relying on the coarse ok/error return value.
    // The workspace callback is registered here for the same reason as in
    // `run_bunit_tests`.
    let eval_cmd = format!(
        "{{ok, _}} = application:ensure_all_started(beamtalk_stdlib), \
         ok = beamtalk_workspace_callback:register_if_loaded(beamtalk_repl_eval), \
         {root_package_start}\
         {hex_deps_start_cmd}\
         Mods = [{module_list}], \
         HasFailed = lists:foldl(fun(M, Acc) -> \
             Res = eunit:test(M, [verbose]), \
             io:format(\"NATIVE_EUNIT_DONE:~s~n\", [M]), \
             case Res of ok -> Acc; _ -> true end \
         end, false, Mods), \
         init:stop(case HasFailed of true -> 1; false -> 0 end)."
    );

    let mut cmd = build_beam_test_command(pipeline, &pa_args, &eval_cmd);

    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run native EUnit tests")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    debug!("Native EUnit stdout: {}", stdout);
    debug!("Native EUnit stderr: {}", stderr);

    let sections = parse_eunit_module_sections(
        &stdout,
        output.status.success(),
        &pipeline.native_test_modules,
    );

    Ok(NativeEunitResult {
        total: sections.total_passed + sections.total_failed + sections.total_skipped,
        passed: sections.total_passed,
        failed: sections.total_failed,
        skipped: sections.total_skipped,
        modules: sections.modules,
        output: sections.eunit_output,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_eunit_summary_all_passed() {
        let output = "======================== EUnit ========================\n\
                      module 'foo_tests'\n\
                        foo_tests: bar_test...ok\n\
                        foo_tests: baz_test...ok\n\
                        [done in 0.006 s]\n\
                      =======================================================\n\
                        All 2 tests passed.\n";
        assert_eq!(parse_eunit_summary(output), (2, 0, 0));
    }

    #[test]
    fn test_parse_eunit_summary_single_test_passed() {
        let output = "  Test passed.\n";
        assert_eq!(parse_eunit_summary(output), (1, 0, 0));
    }

    #[test]
    fn test_parse_eunit_summary_mixed_results() {
        let output = "=======================================================\n\
                        Failed: 1.  Skipped: 0.  Passed: 2.\n";
        assert_eq!(parse_eunit_summary(output), (2, 1, 0));
    }

    #[test]
    fn test_parse_eunit_summary_with_skipped() {
        let output = "=======================================================\n\
                        Failed: 1.  Skipped: 3.  Passed: 5.\n";
        assert_eq!(parse_eunit_summary(output), (5, 1, 3));
    }

    #[test]
    fn test_parse_eunit_summary_no_summary() {
        assert_eq!(parse_eunit_summary("some random output\n"), (0, 0, 0));
    }

    #[test]
    fn test_parse_eunit_summary_61_passed() {
        let output = "  All 61 tests passed.\n";
        assert_eq!(parse_eunit_summary(output), (61, 0, 0));
    }

    #[test]
    fn test_parse_eunit_module_sections_normal() {
        let stdout = "module 'foo_tests'\n\
                      foo_tests: bar_test...ok\n\
                        All 2 tests passed.\n\
                      NATIVE_EUNIT_DONE:foo_tests\n\
                      module 'baz_tests'\n\
                      baz_tests: qux_test...ok\n\
                        Test passed.\n\
                      NATIVE_EUNIT_DONE:baz_tests\n";
        let modules = vec!["foo_tests".to_string(), "baz_tests".to_string()];
        let sections = parse_eunit_module_sections(stdout, true, &modules);
        assert_eq!(sections.modules.len(), 2);
        assert_eq!(sections.modules[0], ("foo_tests".to_string(), 2, 0, 0));
        assert_eq!(sections.modules[1], ("baz_tests".to_string(), 1, 0, 0));
        assert_eq!(sections.total_passed, 3);
        assert_eq!(sections.total_failed, 0);
    }

    #[test]
    fn test_parse_eunit_module_sections_truncated_crash() {
        // Simulate BEAM crash: first module completes, second module's output
        // is cut off without a NATIVE_EUNIT_DONE marker.
        let stdout = "module 'foo_tests'\n\
                        All 2 tests passed.\n\
                      NATIVE_EUNIT_DONE:foo_tests\n\
                      module 'bar_tests'\n\
                      bar_tests: crash_test...{error,beam_crashed}\n";
        let modules = vec!["foo_tests".to_string(), "bar_tests".to_string()];
        let sections = parse_eunit_module_sections(stdout, false, &modules);
        assert_eq!(
            sections.modules.len(),
            2,
            "should include the truncated module"
        );
        assert_eq!(sections.modules[0], ("foo_tests".to_string(), 2, 0, 0));
        // The second module has no summary and process failed → 1 failure
        assert_eq!(sections.modules[1], ("bar_tests".to_string(), 0, 1, 0));
        assert_eq!(sections.total_passed, 2);
        assert_eq!(sections.total_failed, 1);
        assert!(
            !sections.eunit_output.is_empty(),
            "truncated module output should be captured"
        );
    }

    #[test]
    fn test_parse_eunit_module_sections_truncated_with_partial_summary() {
        // BEAM crash after printing a partial EUnit summary for the last module
        let stdout = "NATIVE_EUNIT_DONE:foo_tests\n\
                      module 'bar_tests'\n\
                        Failed: 1.  Skipped: 0.  Passed: 3.\n";
        let modules = vec!["foo_tests".to_string(), "bar_tests".to_string()];
        let sections = parse_eunit_module_sections(stdout, false, &modules);
        assert_eq!(sections.modules.len(), 2);
        // First module had empty section with DONE marker → (0, 0, 0)
        assert_eq!(sections.modules[0], ("foo_tests".to_string(), 0, 0, 0));
        // Second module has a parsed summary and was flushed by the crash handler
        assert_eq!(sections.modules[1], ("bar_tests".to_string(), 3, 1, 0));
    }

    #[test]
    fn test_parse_eunit_module_sections_no_markers() {
        let stdout = "some output\n";
        let modules = vec!["foo_tests".to_string()];
        let sections = parse_eunit_module_sections(stdout, false, &modules);
        assert_eq!(sections.modules.len(), 1);
        assert_eq!(sections.modules[0], ("foo_tests".to_string(), 0, 1, 0));
    }

    #[test]
    fn test_parse_bunit_result_json_with_class_field() {
        let json = r#"{"total":3,"passed":2,"failed":1,"skipped":0,"duration":0.5,"tests":[{"name":"testAdd","class":"MathTest","status":"pass"},{"name":"testSub","class":"MathTest","status":"fail","error":"expected 1 got 2"},{"name":"testConcat","class":"StringTest","status":"pass"}]}"#;
        let result = parse_bunit_result_json(json, "").unwrap();
        assert_eq!(result.total, 3);
        assert_eq!(result.passed, 2);
        assert_eq!(result.failed, 1);
        assert_eq!(result.tests.len(), 3);
        // Class field is parsed
        assert_eq!(result.tests[0].class.as_deref(), Some("MathTest"));
        assert_eq!(result.tests[0].name, "testAdd");
        assert_eq!(result.tests[2].class.as_deref(), Some("StringTest"));
        // Error field is parsed on failures
        assert_eq!(result.tests[1].error.as_deref(), Some("expected 1 got 2"));
    }

    #[test]
    fn test_parse_bunit_result_json_without_class_field() {
        // Older runners may omit the class field — name-only fallback
        let json = r#"{"total":1,"passed":1,"failed":0,"skipped":0,"duration":0.1,"tests":[{"name":"testFoo","status":"pass"}]}"#;
        let result = parse_bunit_result_json(json, "").unwrap();
        assert_eq!(result.tests[0].class, None);
        assert_eq!(result.tests[0].name, "testFoo");
    }
}

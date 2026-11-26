// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk check` — run the semantic checker over a package without emitting
//! bytecode.
//!
//! This command runs the same Pass 1 class/protocol/alias index construction
//! and the same per-file analysis pipeline
//! ([`crate::beam_compiler::analyse_source_with_bindings`]) as `beamtalk
//! build`, so annotation/type errors surface with identical diagnostics —
//! then stops before Core Erlang codegen. In particular it never writes
//! `.core`/`.beam` files for Beamtalk sources, never runs the post-codegen verifier, and never produces OTP
//! packaging artifacts (`.app` file, corpus files).
//!
//! Dependency resolution is shared with `build` (stale dependencies are
//! built for their metadata, exactly as a build would do), and the reused
//! environment setup creates the otherwise-empty build directory. The
//! package's own `native/*.erl` is compiled first (BT-5342, as `build` does),
//! so its `-spec`s type FFI calls identically on a clean tree and after a
//! build; a passing check still does not guarantee `beamtalk build` succeeds.
//!
//! **DDD Context:** Compilation (CLI surface for the semantic checker).

use miette::Result;
use tracing::{debug, error, info, instrument};

use crate::beam_compiler::{analyse_source_with_bindings, diagnostics_are_errors};
use crate::commands::build::{
    Pass2Inputs, build_class_index, compile_native_sources, compute_file_module_pairs,
    new_compile_context, pass2_inputs, prepare_options, resolve_and_validate_dependencies,
    setup_build_environment, warn_if_otp_out_of_window,
};

/// Check Beamtalk source files for errors without emitting bytecode.
///
/// Runs Pass 1 indexing plus the full semantic-analysis diagnostic pipeline
/// over every discovered source file (always a fresh full check — no
/// incremental cache), prints each file's diagnostics exactly as `build`
/// would, and fails when any file contains error-severity diagnostics.
#[allow(clippy::too_many_lines)] // orchestration function mirroring `build`'s phases — split adds indirection, not clarity
#[instrument(skip_all, fields(path = %path))]
pub fn run_check(path: &str, options: &beamtalk_core::CompilerOptions) -> Result<()> {
    info!("Starting check");

    crate::commands::build_stamp::require_supported_otp()?;
    warn_if_otp_out_of_window();

    let env = setup_build_environment(path)?;

    // Same `CompilerOptions` shaping as `build`, by the same function.
    let options = prepare_options(&env, path, options);
    let options = &options;

    let dep_ctx = resolve_and_validate_dependencies(&env, options)?;
    // Fresh Pass 1 (`force = true`) and no diagnostics-cache replay: check
    // is specified as an always-cold full gate, so it deliberately bypasses
    // the incremental machinery (`provenance_requires_rebuild`,
    // diagnostics-cache replay) - every file is analysed from scratch.
    let mut index = build_class_index(&env, &dep_ctx, options, true)?;

    // BT-5342: compile the package's own `native/*.erl` first, exactly as
    // `build` does after Pass 1, so its `-spec`s are visible to Pass 2 on a
    // clean tree. Without this the FFI type registry reads only already-built
    // artifacts and every call into the package's own native module is
    // inferred as Dynamic, making the result depend on whether `_build`
    // exists. Only native code is compiled: no `.core`/`.beam` for `.bt`.
    let _native_result = compile_native_sources(&env, &dep_ctx, &index.class_module_index)?;

    // Same Pass 2 inputs as `build`: project-known Erlang modules for the
    // `unresolved_ffi` check, plus the ADR 0075 FFI type registry (read from
    // the `.beam` files/caches, now including the package's own natives).
    let Pass2Inputs {
        options: pass2_options,
        stub_diagnostics,
        native_type_registry,
        ..
    } = pass2_inputs(&env, &dep_ctx, options);
    let options = &pass2_options;

    let file_module_pairs = compute_file_module_pairs(&env)?;

    let mut cached_asts = std::mem::take(&mut index.cached_asts);
    let check_ctx = new_compile_context(&env, &index, native_type_registry);

    // A broken `stubs/` file yields error-severity diagnostics (stub files
    // are parsed too) — fail on those instead of merely counting them in
    // the summary as `build` does. A wrong stub silently skews FFI typing
    // for every file, so a checker gate must not exit 0 here.
    let stub_failed =
        diagnostics_are_errors(&stub_diagnostics, options, &check_ctx.diagnostics_overrides);
    if stub_failed {
        error!("Check failed for stub declarations");
    }
    // Seed with stubs/ diagnostics (skipped signatures, version drift),
    // as `build` does.
    let mut all_check_diags: Vec<beamtalk_core::source_analysis::Diagnostic> = stub_diagnostics;
    let mut failed_files = 0usize;

    // Unlike `build` (which stops at the first failing file), check analyses
    // every file so one run reports the whole error surface.
    for (file, _, _) in &file_module_pairs {
        debug!("Checking {file}");
        let cached = cached_asts.remove(file);
        let (_source, _module, file_diags, _analysis) =
            match analyse_source_with_bindings(file, options, &check_ctx, cached) {
                Ok(analysed) => analysed,
                Err(e) => {
                    // I/O failure (parse problems are diagnostics, not
                    // errors): record the file as failed and keep checking
                    // the rest, so one unreadable file doesn't hide the
                    // whole package's error surface. The provision sink
                    // flushes at the end of the loop regardless.
                    error!("Failed to check '{file}': {e:#}");
                    failed_files += 1;
                    continue;
                }
            };
        if diagnostics_are_errors(&file_diags, options, &check_ctx.diagnostics_overrides) {
            error!("Check failed for '{file}'");
            failed_files += 1;
        }
        all_check_diags.extend(file_diags);
    }

    check_ctx
        .provision_sink
        .flush(&check_ctx.hierarchy.pre_loaded_protocol_sources, options);

    let all_check_diags = crate::diagnostic::dedupe_provision_diagnostics(all_check_diags);
    let diagnostic_summary = beamtalk_core::source_analysis::DiagnosticSummary::from_diagnostics(
        &all_check_diags,
        file_module_pairs.len(),
    );

    // Print diagnostic summary at end of the check, matching `build`.
    if !options.suppress_warnings && !diagnostic_summary.is_empty() {
        eprintln!();
        eprintln!("{diagnostic_summary}");
    }

    if stub_failed {
        miette::bail!("check failed: stub declarations contain errors");
    }
    if failed_files > 0 {
        miette::bail!(
            "check failed: {failed_files} of {} file(s) contain errors",
            file_module_pairs.len()
        );
    }

    eprintln!("Checked {} file(s) — no errors", file_module_pairs.len());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::{Utf8Path, Utf8PathBuf};
    use tempfile::TempDir;

    fn test_project_with(filename: &str, content: &str) -> (TempDir, Utf8PathBuf) {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = project.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(src.join(filename), content).unwrap();
        (temp, project)
    }

    /// Files with the given extensions anywhere under `dir`, via the shared
    /// [`beamtalk_core::file_walker::FileWalker`] (recursive, ignores no
    /// paths — build outputs under ignored `_build/` must be visible too).
    fn files_with_extensions(dir: &Utf8Path, extensions: &[&str]) -> Vec<Utf8PathBuf> {
        beamtalk_core::file_walker::FileWalker::new()
            .extensions(extensions)
            .walk(dir)
            .unwrap()
    }

    #[test]
    fn check_valid_project_succeeds_without_emitting_bytecode() {
        let (_temp, project) = test_project_with("main.bt", "main := [42].");

        run_check(project.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect("check should succeed on valid sources");

        // No bytecode artifacts (`.core`/`.beam`) anywhere under the project.
        let emitted = files_with_extensions(&project, &["core", "beam"]);
        assert!(
            emitted.is_empty(),
            "check must not emit bytecode, found: {emitted:?}"
        );
    }

    #[test]
    fn check_syntax_error_fails_like_build() {
        let (_temp, project) = test_project_with("main.bt", "test := [1 + ].");

        let err = run_check(project.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect_err("check should fail on syntax error");
        assert!(
            format!("{err:?}").contains("check failed"),
            "unexpected error: {err:?}"
        );

        // Build's analysis fails on the same fixture (before any BEAM stage,
        // so no escript is needed).
        let options = beamtalk_core::CompilerOptions::default();
        let result = crate::beam_compiler::compile_source(
            &project.join("src").join("main.bt"),
            "main",
            &project.join("main.core"),
            &options,
        );
        assert!(
            result.is_err(),
            "build analysis should fail on the same fixture"
        );
    }

    #[test]
    fn check_type_error_fails_like_build() {
        // Undefined variable: an error-severity checker diagnostic, not a
        // parse error.
        let (_temp, project) = test_project_with("main.bt", "main := [nosuchvar].");

        let err = run_check(project.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect_err("check should fail on type error");
        assert!(
            format!("{err:?}").contains("check failed"),
            "unexpected error: {err:?}"
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = crate::beam_compiler::compile_source(
            &project.join("src").join("main.bt"),
            "main",
            &project.join("main.core"),
            &options,
        );
        assert!(
            result.is_err(),
            "build analysis should fail on the same fixture"
        );
    }

    #[test]
    fn check_manifest_package_exercises_pass1_cached_asts() {
        // A manifest-backed two-file package: Pass 1 runs (unlike
        // manifest-less checks) and feeds cached ASTs plus cross-file
        // `ClassInfo` into every file's analysis, so the inherited
        // `getValue` send resolves instead of reporting DNU.
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = project.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(
            project.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(
            src.join("counter.bt"),
            "Actor subclass: Counter\n  state: count = 0\n  getValue => self.count\n  increment => self.count := self.count + 1\n",
        )
        .unwrap();
        std::fs::write(
            src.join("inheriting_counter.bt"),
            "Counter subclass: InheritingCounter\n  getDoubled => self getValue * 2\n",
        )
        .unwrap();

        run_check(project.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect("check should succeed with cross-file knowledge");

        let emitted = files_with_extensions(&project, &["core", "beam"]);
        assert!(
            emitted.is_empty(),
            "check must not emit bytecode, found: {emitted:?}"
        );
    }

    #[test]
    fn check_hint_does_not_fail() {
        // A DNU hint is not error-severity: check reports it but succeeds,
        // exactly like `build` without `--warnings-as-errors`.
        let (_temp, project) = test_project_with("main.bt", "\"hello\" frobnicate");

        run_check(project.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect("check should succeed on hint-only diagnostics");
    }

    #[test]
    fn check_reports_every_failing_file() {
        // Unlike `build` (which stops at the first failing file), one check
        // run reports the whole error surface: both bad files are counted.
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = project.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(src.join("bad1.bt"), "test := [1 + ].").unwrap();
        std::fs::write(src.join("bad2.bt"), "main := [nosuchvar].").unwrap();

        let err = run_check(project.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect_err("check should fail");
        assert!(
            format!("{err:?}").contains("2 of 2 file(s) contain errors"),
            "unexpected error: {err:?}"
        );

        // `build` fails on the same package too (at the first bad file, so
        // no escript is needed).
        let result = crate::commands::build::build(
            project.as_str(),
            &beamtalk_core::CompilerOptions::default(),
            true,
        );
        assert!(result.is_err(), "build should fail on the same package");
    }

    #[test]
    fn check_single_file_path() {
        // `beamtalk check path/to/file.bt` checks just that file.
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = project.join("src");
        std::fs::create_dir_all(&src).unwrap();
        let file = src.join("main.bt");
        std::fs::write(&file, "main := [42].").unwrap();

        run_check(file.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect("check should succeed on a valid single file");

        let emitted = files_with_extensions(&project, &["core", "beam"]);
        assert!(
            emitted.is_empty(),
            "check must not emit bytecode, found: {emitted:?}"
        );
    }

    #[test]
    fn check_broken_stub_file_fails() {
        // A syntax-broken `stubs/` file yields error-severity diagnostics:
        // check must fail rather than exit 0 with errors in its summary.
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = project.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::create_dir_all(project.join("stubs")).unwrap();
        std::fs::write(
            project.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(src.join("main.bt"), "main := [42].").unwrap();
        std::fs::write(
            project.join("stubs").join("lists.bt"),
            "class Foo { @@@@invalid syntax }",
        )
        .unwrap();

        let err = run_check(project.as_str(), &beamtalk_core::CompilerOptions::default())
            .expect_err("check should fail on broken stub");
        assert!(
            format!("{err:?}").contains("stub declarations contain errors"),
            "unexpected error: {err:?}"
        );
    }

    // ── Dependency guard (ADR 0070 WS3, BT-4608) ──────────────────────────
    //
    // A consumer package with a path dependency. `[diagnostics]` escalates
    // `dnu` and `type` to errors so whether `check` passes shows whether the
    // dependency guard lifted. ADR 0152 removed cross-file extension methods,
    // so a dependency contributes classes only.

    /// A sealed producer class with a class method, and no
    /// `validateHeader:value:` (the BT-4526 repro shape).
    const PRODUCER_CODEC: &str =
        "sealed Object subclass: Codec\n  class encode: x :: Integer -> Integer => x\n";

    /// `deps::test_support::consumer_with_fresh_path_dependency` with `dnu` and `type`
    /// escalated to errors, so `check` fails exactly when one is reported.
    fn consumer_with_dependency(
        temp: &TempDir,
        producer_files: &[(&str, &str)],
        consumer_files: &[(&str, &str)],
    ) -> Utf8PathBuf {
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        crate::commands::deps::test_support::consumer_with_fresh_path_dependency(
            &root,
            producer_files,
            "[diagnostics]\ndnu = \"error\"\ntype = \"error\"\n",
            consumer_files,
        )
    }

    #[test]
    fn missing_selector_is_reported_in_a_package_with_dependencies() {
        // Before WS3 the guard kept every receiver `Open` in a package with
        // dependencies, so this passed.
        let temp = TempDir::new().unwrap();
        let consumer = consumer_with_dependency(
            &temp,
            &[("codec.bt", PRODUCER_CODEC)],
            &[("main.bt", "Object subclass: Main\n  run => 42 whisper\n")],
        );
        let err = run_check(
            consumer.as_str(),
            &beamtalk_core::CompilerOptions::default(),
        )
        .expect_err("an undefined selector must be reported as does-not-understand");
        assert!(format!("{err:?}").contains("check failed"), "{err:?}");
    }

    #[test]
    fn removed_class_side_selector_on_dependency_class_is_reported() {
        // The BT-4526 repro: `HPACK validateHeader:value:` after the
        // dependency removed it.
        let temp = TempDir::new().unwrap();
        let consumer = consumer_with_dependency(
            &temp,
            &[("codec.bt", PRODUCER_CODEC)],
            &[(
                "main.bt",
                "Object subclass: Main\n  run => Codec validateHeader: 1 value: 2\n",
            )],
        );
        run_check(
            consumer.as_str(),
            &beamtalk_core::CompilerOptions::default(),
        )
        .expect_err("the removed class method must be reported");
    }
}

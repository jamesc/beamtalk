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
//! `.core`/`.beam` files, never compiles the package's own native Erlang
//! sources, never runs the post-codegen verifier, and never produces OTP
//! packaging artifacts (`.app` file, corpus files).
//!
//! Dependency resolution is shared with `build` (stale dependencies are
//! built for their metadata, exactly as a build would do), and the reused
//! environment setup creates the otherwise-empty build directory. FFI and
//! native-module checks see only already-built artifacts, so a passing
//! check does not guarantee `beamtalk build` succeeds.
//!
//! **DDD Context:** Compilation (CLI surface for the semantic checker).

use camino::Utf8Path;
use miette::Result;
use tracing::{debug, error, info, instrument};

use crate::beam_compiler::{
    ClassHierarchyContext, CompileContext, analyse_source_with_bindings, diagnostics_are_errors,
};
use crate::commands::OutputFormat;
use crate::commands::build::{
    build_class_index, compute_file_module_pairs, extract_type_specs, package_identity,
    resolve_and_validate_dependencies, resolve_package_type_registry, setup_build_environment,
    warn_if_otp_out_of_window,
};

/// Check Beamtalk source files for errors without emitting bytecode.
///
/// Runs Pass 1 indexing plus the full semantic-analysis diagnostic pipeline
/// over every discovered source file (always a fresh full check — no
/// incremental cache), prints each file's diagnostics exactly as `build`
/// would, and fails when any file contains error-severity diagnostics.
#[allow(clippy::too_many_lines)] // orchestration function mirroring `build`'s phases — split adds indirection, not clarity
#[instrument(skip_all, fields(path = %path))]
pub fn run_check(path: &str) -> Result<()> {
    info!("Starting check");

    crate::commands::build_stamp::require_supported_otp()?;
    warn_if_otp_out_of_window();

    let env = setup_build_environment(path)?;

    // Same `CompilerOptions` shaping as `build`: a package directory check
    // walks every project source file (Pass 1) before any per-file analysis
    // runs (Pass 2), so the injected cross-file knowledge is
    // project-complete. See `build` for the full rationale of each line.
    let mut options = beamtalk_core::CompilerOptions::default();
    let scope =
        crate::commands::manifest::PackageCheckerScope::for_manifest(env.full_manifest.as_ref());
    options.dependency_extensions_unknown = scope.has_package_dependencies;
    if Utf8Path::new(path).is_dir() {
        options.knowledge_scope = scope.knowledge_scope;
    }
    options.current_package =
        package_identity(env.pkg_manifest(), options.stdlib_mode).map(str::to_owned);
    let options = &options;

    let dep_ctx = resolve_and_validate_dependencies(&env, options)?;
    // Fresh Pass 1 (`force = true`) and no diagnostics-cache replay: check
    // is specified as an always-cold full gate, so it deliberately bypasses
    // the incremental machinery (`provenance_requires_rebuild`,
    // diagnostics-cache replay) — every file is analysed from scratch.
    let index = build_class_index(&env, &dep_ctx, options, true)?;

    // Same Pass 2 context as `build`: project-known Erlang modules for the
    // `unresolved_ffi` check, plus the ADR 0075 FFI type registry (read from
    // existing `.beam` files/caches — never compiled here).
    let pass2_options = beamtalk_core::CompilerOptions {
        known_erlang_modules: beamtalk_core::ffi_type_specs::collect_project_known_erlang_modules(
            &env.project_root,
        ),
        ..options.clone()
    };
    let options = &pass2_options;

    let auto_extract_registry = extract_type_specs(
        &env.layout,
        env.pkg_manifest().is_some(),
        options.stdlib_mode,
    );
    let resolved_registry = resolve_package_type_registry(
        &env.project_root,
        auto_extract_registry,
        &dep_ctx.resolved_deps,
        Some(OutputFormat::Text),
    );
    let stub_diagnostics: Vec<_> = resolved_registry.diagnostics().cloned().collect();
    let native_type_registry = resolved_registry.registry;
    let native_type_registry = native_type_registry.map(std::sync::Arc::new);

    let file_module_pairs = compute_file_module_pairs(&env)?;

    let strict_deps = env
        .full_manifest
        .as_ref()
        .is_some_and(|m| m.package.strict_deps);
    let diagnostics_overrides = env
        .full_manifest
        .as_ref()
        .map(|m| m.diagnostics.clone())
        .unwrap_or_default();

    let registry_ref = if index.dep_registry.is_empty() {
        None
    } else {
        Some(&index.dep_registry)
    };
    let check_ctx = CompileContext {
        hierarchy: ClassHierarchyContext {
            class_module_index: index.class_module_index.clone(),
            class_superclass_index: index.class_superclass_index.clone(),
            pre_loaded_classes: index.all_class_infos.clone(),
            pre_loaded_protocols: index.all_protocol_infos.clone(),
            pre_loaded_protocol_defs: index.all_protocol_defs.clone(),
            pre_loaded_protocol_sources: index.all_protocol_sources.clone(),
            pre_loaded_aliases: index.all_alias_infos.clone(),
            extension_index: index.extension_index.clone(),
            dependency_extensions_complete: index.dependency_extensions_complete,
        },
        dep_registry: registry_ref,
        strict_deps,
        native_type_registry,
        diagnostics_overrides,
        // ADR 0127 §3: a provision shared by several users is reported once,
        // against its protocol's file, after every file has been checked.
        provision_sink: crate::beam_compiler::ProvisionSink::collecting(),
    };

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
    let mut cached_asts = index.cached_asts;
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
    use camino::Utf8PathBuf;
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

        run_check(project.as_str()).expect("check should succeed on valid sources");

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

        let err = run_check(project.as_str()).expect_err("check should fail on syntax error");
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

        let err = run_check(project.as_str()).expect_err("check should fail on type error");
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

        run_check(project.as_str()).expect("check should succeed with cross-file knowledge");

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

        run_check(project.as_str()).expect("check should succeed on hint-only diagnostics");
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

        let err = run_check(project.as_str()).expect_err("check should fail");
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

        run_check(file.as_str()).expect("check should succeed on a valid single file");

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

        let err = run_check(project.as_str()).expect_err("check should fail on broken stub");
        assert!(
            format!("{err:?}").contains("stub declarations contain errors"),
            "unexpected error: {err:?}"
        );
    }

    // ── ADR 0070 WS3 (BT-4608): dependency extensions ─────────────────────
    //
    // A consumer package with a path dependency. `[diagnostics]` escalates
    // `dnu` and `type` to errors so whether `check` passes shows whether the
    // pre-WS3 dependency guard lifted: before WS3 every receiver in a package
    // with dependencies was `Open`, so no DNU or conformance diagnostic could
    // fire at all.

    /// The producer's extensions: on `Object`, on a stdlib class instance-
    /// and class-side, and class-side on its own sealed class.
    const PRODUCER_EXTENSIONS: &str = "Object >> shout -> Integer => 1\n\
         Integer >> doubled -> Integer => self * 2\n\
         Integer class >> fromShout -> Integer => 2\n\
         Codec class >> make -> Integer => 3\n";

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

    fn producer_with_extensions() -> Vec<(&'static str, &'static str)> {
        vec![
            ("codec.bt", PRODUCER_CODEC),
            ("ext.bt", PRODUCER_EXTENSIONS),
        ]
    }

    #[test]
    fn dependency_extensions_resolve_with_no_false_dnu() {
        let temp = TempDir::new().unwrap();
        let consumer = consumer_with_dependency(
            &temp,
            &producer_with_extensions(),
            &[(
                "main.bt",
                "Object subclass: Main\n  run => 42 shout + 42 doubled + Integer fromShout + Codec make\n",
            )],
        );
        run_check(consumer.as_str())
            .expect("selectors the dependency's extensions define must not be reported");
    }

    #[test]
    fn missing_selector_is_reported_once_dependency_extensions_are_loaded() {
        // Before WS3 the guard kept every receiver `Open` in a package with
        // dependencies, so this passed.
        let temp = TempDir::new().unwrap();
        let consumer = consumer_with_dependency(
            &temp,
            &producer_with_extensions(),
            &[("main.bt", "Object subclass: Main\n  run => 42 whisper\n")],
        );
        let err = run_check(consumer.as_str())
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
            &producer_with_extensions(),
            &[(
                "main.bt",
                "Object subclass: Main\n  run => Codec validateHeader: 1 value: 2\n",
            )],
        );
        run_check(consumer.as_str()).expect_err("the removed class method must be reported");
    }

    #[test]
    fn removing_the_dependency_extension_brings_the_dnu_back() {
        let temp = TempDir::new().unwrap();
        let consumer = consumer_with_dependency(
            &temp,
            &[("codec.bt", PRODUCER_CODEC)],
            &[("main.bt", "Object subclass: Main\n  run => 42 shout\n")],
        );
        run_check(consumer.as_str())
            .expect_err("without the producer's extension, `shout` is not understood");
    }

    #[test]
    fn dependency_extension_satisfies_protocol_conformance() {
        let consumer_files = [
            (
                "shouter.bt",
                "Protocol define: Shouter\n  shout -> Integer\n",
            ),
            (
                "main.bt",
                "Object subclass: Main\n  \
                 use: s :: Shouter -> Integer => s shout\n  \
                 run => self use: 42\n",
            ),
        ];

        let temp = TempDir::new().unwrap();
        let consumer =
            consumer_with_dependency(&temp, &producer_with_extensions(), &consumer_files);
        run_check(consumer.as_str())
            .expect("Integer conforms to Shouter through the dependency's `Object >> shout`");

        let temp = TempDir::new().unwrap();
        let consumer =
            consumer_with_dependency(&temp, &[("codec.bt", PRODUCER_CODEC)], &consumer_files);
        run_check(consumer.as_str())
            .expect_err("without the extension, Integer does not conform to Shouter");
    }
}

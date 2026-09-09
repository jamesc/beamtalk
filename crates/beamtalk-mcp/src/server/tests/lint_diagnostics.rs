// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint and diagnostic-summary tests: near-miss dividers, module-name mismatches, stub-file suppression, native-declaration locations, and the shared FFI/native-type-registry fixtures they exercise.

use super::*;
use camino::Utf8PathBuf;

// --- run_lint_structured ---

#[test]
fn run_lint_structured_nonexistent_path() {
    let result = run_lint_structured("/nonexistent/path/that/does/not/exist");
    assert_eq!(result.total, 1);
    assert!(result.errors.len() == 1);
    assert!(result.warnings.is_empty());
    assert!(result.errors[0].message.contains("does not exist"));
}

#[test]
fn run_lint_structured_non_bt_file() {
    // Use a temp file so the test is portable across platforms.
    let temp = tempfile::TempDir::new().unwrap();
    let path = Utf8PathBuf::from_path_buf(temp.path().join("non_bt.txt"))
        .expect("temp dir should be UTF-8");
    std::fs::write(path.as_std_path(), "not beamtalk").unwrap();
    let result = run_lint_structured(path.as_str());
    assert_eq!(result.total, 1);
    assert!(result.errors.len() == 1);
    assert!(result.errors[0].message.contains(".bt file"));
}

#[test]
fn run_lint_structured_includes_dnu_diagnostics() {
    // BT-1587: MCP lint must include DNU diagnostics from semantic analysis,
    // matching CLI `beamtalk lint` behavior.
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("dnu_test.bt");
    std::fs::write(
        &file,
        r#"Object subclass: DnuTest

  class demo =>
    s := "hello"
    val := s sqrt
    val
"#,
    )
    .unwrap();
    let result = run_lint_structured(file.to_str().unwrap());
    let has_dnu = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .any(|d| d.message.contains("does not understand"));
    assert!(
        has_dnu,
        "MCP lint should report DNU diagnostics from semantic analysis, got: {result:?}",
    );
}

#[test]
fn run_lint_structured_expect_type_suppresses_dnu() {
    // BT-1587: @expect type should suppress DNU diagnostics in MCP lint,
    // just as it does in CLI lint.
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("expect_test.bt");
    std::fs::write(
        &file,
        r#"Object subclass: ExpectTest

  class demo =>
    s := "hello"
    @expect type
    val := s sqrt
    val
"#,
    )
    .unwrap();
    let result = run_lint_structured(file.to_str().unwrap());
    let has_dnu = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .any(|d| d.message.contains("does not understand"));
    assert!(
        !has_dnu,
        "@expect type should suppress DNU in MCP lint, got: {result:?}",
    );
}

// ── near-miss `// === Name ===` divider (BT-3240/BT-3257) ──────────────
//
// These exercise the real `lint`/`diagnostic_summary` entry points
// (`run_lint_structured`, `run_module_analysis`), not
// `near_miss_divider::scan_source` directly — that's already covered by
// that module's own `scan_source_locates_the_near_miss_comment_line_precisely`
// test.

#[test]
fn run_lint_structured_near_miss_divider_span_points_at_comment_line() {
    // BT-3240/BT-3257: before `source` was threaded through
    // `run_module_analysis`, MCP `lint` reached this check through the
    // AST-based `NearMissDividerPass`, whose `Comment::span` is actually
    // `bar`'s token span (line 3), not the comment's own line (line 2).
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("near_miss_test.bt");
    std::fs::write(
        &file,
        "Object subclass: Foo\n  // === Section ====\n  bar => 1\n",
    )
    .unwrap();
    let result = run_lint_structured(file.to_str().unwrap());
    let near_misses: Vec<_> = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .filter(|d| d.message.contains("section divider"))
        .collect();
    assert_eq!(
        near_misses.len(),
        1,
        "expected exactly one near-miss-divider diagnostic: {result:?}"
    );
    assert_eq!(
        near_misses[0].line,
        Some(2),
        "span should point at the comment's own line (2), not `bar`'s line (3): {result:?}"
    );
}

#[test]
fn run_lint_structured_multiple_near_miss_dividers_get_distinct_correctly_attributed_lines() {
    // Two near-misses in one file must not get their lines mixed up.
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("multi_near_miss_test.bt");
    std::fs::write(
        &file,
        "Object subclass: Foo\n  // === First ====\n  bar => 1\n\n  // == Second ==\n  baz => 2\n",
    )
    .unwrap();
    let result = run_lint_structured(file.to_str().unwrap());
    let mut lines: Vec<u32> = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .filter(|d| d.message.contains("section divider"))
        .filter_map(|d| d.line)
        .collect();
    lines.sort_unstable();
    assert_eq!(
        lines,
        vec![2, 5],
        "each near-miss should be attributed to its own comment line, not the other's: {result:?}"
    );
}

/// BT-3257: `compute_diagnostic_summary`'s public JSON output only
/// exposes aggregate severity/category counts, not individual
/// diagnostic spans — so span accuracy can't be asserted through its
/// return value directly. This instead calls `run_module_analysis`,
/// the exact shared function `compute_diagnostic_summary`'s Pass 2 loop
/// invokes per file (see the call site a few lines below in this
/// module), and inspects the diagnostic it produces before that
/// information is aggregated away — proving MCP `diagnostic_summary`'s
/// code path carries the same accurate, comment-line span as MCP
/// `lint` and the LSP, not just that it doesn't crash.
#[test]
fn run_module_analysis_near_miss_divider_span_points_at_comment_line() {
    let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    let initial_diags: Vec<_> = parse_diags
        .into_iter()
        .filter(|d| d.severity == Severity::Lint)
        .collect();
    let (diags, _) = run_module_analysis(
        &module,
        source,
        &[],
        initial_diags,
        false,
        None,
        None,
        false,
        None,
    );
    let near_misses: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("section divider"))
        .collect();
    assert_eq!(
        near_misses.len(),
        1,
        "expected exactly one near-miss-divider diagnostic: {diags:?}"
    );
    assert_eq!(
        near_misses[0].span.line_number(source),
        2,
        "span should point at the comment's own line (2), not `bar`'s line (3): {diags:?}"
    );
}

/// BT-3431: MCP `lint`/`diagnostic_summary` must report the same
/// file-name/class-name mismatch `beamtalk build`/`beamtalk lint`/the
/// LSP do — before this fix, `run_module_analysis` never called
/// `check_class_file_name_agreement` at all, so this surface silently
/// reported clean regardless of `file_stem`.
#[test]
fn run_module_analysis_reports_mismatched_file_name() {
    let source = "Value subclass: ExduraEvent";
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    let (diags, _) = run_module_analysis(
        &module,
        source,
        &[],
        parse_diags,
        false,
        None,
        None,
        false,
        Some("event"),
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("does not match declared class")),
        "mismatched file name should be reported: {diags:?}"
    );
}

/// BT-3431 negative control.
#[test]
fn run_module_analysis_does_not_report_matching_file_name() {
    let source = "Value subclass: ExduraEvent";
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    let (diags, _) = run_module_analysis(
        &module,
        source,
        &[],
        parse_diags,
        false,
        None,
        None,
        false,
        Some("exdura_event"),
    );
    assert!(
        !diags
            .iter()
            .any(|d| d.message.contains("does not match declared class")),
        "matching file name should not be reported: {diags:?}"
    );
}

/// BT-3398 regression, analogous to
/// `beamtalk_language_service::project_index::tests::is_stub_file_true_for_file_under_a_root_stubs_dir`:
/// `run_module_analysis`'s `is_stub_file` argument must actually reach
/// `AnalysisContext::is_stub_file` — verified here by calling it directly
/// with `is_stub_file: false` (the value every call site used
/// unconditionally before this fix) on a module containing a `declare
/// native:` block and confirming `check_native_declaration_location`
/// still runs (would reject it if this test's own module lived in
/// `src/`), then with `is_stub_file: true` (what a real `stubs/` call
/// site now derives) and confirming it no longer would.
///
/// At the time this test was written, `check_native_declaration_location`'s
/// diagnostic had no `DiagnosticCategory` (a separate, pre-existing gap
/// shared by `beamtalk lint`'s own `collect_diagnostics` — filed and
/// fixed as BT-3404), so `run_module_analysis`'s `category.is_some()`
/// filter dropped it from the *returned* diagnostics regardless of
/// `is_stub_file`. This test therefore asserts on `analyse_full`'s
/// pre-filter diagnostics — built with the identical `AnalysisContext`
/// construction `run_module_analysis` uses — rather than
/// `run_module_analysis`'s own return value, so it actually exercises the
/// `is_stub_file` wiring instead of vacuously passing either way. Now that
/// BT-3404 has given the diagnostic a category, the *returned* diagnostics
/// carry it too — see
/// `run_module_analysis_reports_native_declaration_location_error` below.
#[test]
fn run_module_analysis_is_stub_file_suppresses_native_declaration_location_error() {
    let source = "declare native: lists\n";
    let tokens = lex_with_eof(source);
    let (module, _parse_diags) = parse(tokens);

    let has_location_error = |is_stub_file: bool| {
        let analysis_ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
            .with_is_stub_file(is_stub_file);
        let result = beamtalk_core::semantic_analysis::analyse_full(&module, analysis_ctx);
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("only valid in stubs/ directory"))
    };

    assert!(
        has_location_error(false),
        "declare native: outside stubs/ should still be rejected"
    );
    assert!(
        !has_location_error(true),
        "declare native: inside stubs/ should not be rejected"
    );
}

/// BT-3404 regression: `check_native_declaration_location`'s diagnostic
/// now carries a `DiagnosticCategory`
/// (`NativeDeclarationLocation`), so `run_module_analysis`'s
/// `category.is_some()` filter no longer silently drops it from the
/// diagnostics MCP `lint`/`diagnostic_summary` actually return — unlike
/// before this fix, where the previous test had to reach past
/// `run_module_analysis` into `analyse_full`'s pre-filter diagnostics to
/// observe the check running at all.
#[test]
fn run_module_analysis_reports_native_declaration_location_error() {
    let source = "declare native: lists\n";
    let tokens = lex_with_eof(source);
    let (module, _parse_diags) = parse(tokens);

    let (diags, _) = run_module_analysis(
        &module,
        source,
        &[],
        Vec::new(),
        false,
        None,
        None,
        false,
        None,
    );

    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("only valid in stubs/ directory")),
        "declare native: outside stubs/ should be reported by run_module_analysis, \
             not silently dropped: {diags:?}"
    );
}

/// BT-3398 end-to-end (MCP-level) regression, per the issue's acceptance
/// criteria: opening a legitimate `stubs/lists.bt` via the MCP `lint`
/// tool must not report a false "only valid in stubs/ directory" error.
/// The previous test asserts the `is_stub_file` wiring actually
/// discriminates stub vs. non-stub at the `AnalysisContext` level (the
/// only level that can observe it, per that test's doc on the
/// category-filter gap); this one pins the MCP-surface behaviour the
/// issue is actually about.
#[test]
fn run_lint_structured_stub_file_declare_native_no_location_error() {
    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"stub-test\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    let stubs_dir = dir.join("stubs");
    std::fs::create_dir_all(&stubs_dir).unwrap();
    let stub_file = stubs_dir.join("lists.bt");
    std::fs::write(&stub_file, "declare native: lists\n").unwrap();

    let stub_result = run_lint_structured(stub_file.to_str().unwrap());
    let stub_location_errors: Vec<_> = stub_result
        .errors
        .iter()
        .filter(|d| d.message.contains("only valid in stubs/ directory"))
        .collect();
    assert!(
        stub_location_errors.is_empty(),
        "a legitimate stubs/lists.bt should not report a native-declaration \
             location error via MCP lint, got: {stub_result:?}"
    );
}

/// BT-2858: `build_native_type_registry` extracts live from OTP `.beam`
/// files for a manifest-backed project with no prior `beamtalk build` —
/// analogous to `commands::lint`'s
/// `lint_extracts_type_specs_live_on_cold_cache_bt_2851` in the CLI
/// binary. Before this fix, MCP `lint`/`diagnostic_summary` had no way to
/// obtain a registry at all (`run_module_analysis` always passed `None`).
#[test]
fn build_native_type_registry_extracts_live_on_cold_cache() {
    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    std::fs::create_dir_all(dir.join("src")).unwrap();
    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    // No `_build/` directory exists yet — the cold-cache case.
    assert!(!dir.join("_build").exists());

    let Some(registry) = build_native_type_registry(dir.join("src").to_str().unwrap()) else {
        // OTP `.beam` discovery is environment-dependent (e.g. a sandbox
        // with no OTP install on disk); skip rather than false-fail.
        eprintln!(
            "skipping build_native_type_registry_extracts_live_on_cold_cache: \
                 no OTP .beam files discovered in this environment"
        );
        return;
    };
    assert!(
        registry.lookup("lists", "reverse", 1).is_some(),
        "live extraction with no prior build must still find lists:reverse/1"
    );
    // The extractor writes the same cache a `beamtalk build`/`beamtalk
    // lint` run would, so a subsequent call reads it back instead of
    // re-extracting.
    assert!(dir.join("_build").join("type_cache").exists());
}

/// BT-2858: MCP `lint` must see the same FFI argument-type registry
/// `beamtalk lint`/`beamtalk build` do, so a well-specced `(Erlang m) f:`
/// call does not fall back to `Dynamic(UntypedFfi)` and trip the BT-1914
/// "Dynamic in typed class" warning — mirrors
/// `commands::lint`'s `ffi_call_with_registry_does_not_warn_dynamic_in_typed_class`.
/// Before this fix, `run_module_analysis` always analysed with `None`,
/// so this warning fired unconditionally regardless of whether the
/// runtime's `.beam` files carried a real `-spec`.
#[test]
fn run_lint_structured_ffi_call_does_not_warn_dynamic_in_typed_class() {
    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    std::fs::create_dir_all(dir.join("src")).unwrap();
    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    let file = dir.join("src").join("ffi_test.bt");
    std::fs::write(
            &file,
            "sealed typed Value subclass: FfiTest\n\n  check -> Dynamic =>\n    Erlang lists reverse: (1 to: 3) asArray\n",
        )
        .unwrap();

    if build_native_type_registry(file.to_str().unwrap()).is_none() {
        eprintln!(
            "skipping run_lint_structured_ffi_call_does_not_warn_dynamic_in_typed_class: \
                 no OTP .beam files discovered in this environment"
        );
        return;
    }

    let result = run_lint_structured(file.to_str().unwrap());
    let untyped_ffi: Vec<_> = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .filter(|d| d.message.contains("untyped FFI"))
        .collect();
    assert!(
        untyped_ffi.is_empty(),
        "with a live registry, MCP lint must not warn untyped FFI; got: {untyped_ffi:?}"
    );
}

/// BT-2052: MCP lint must resolve cross-file classes from the full package
/// source set (src/ + test/). Without this, `@expect type` annotations that
/// suppress diagnostics referencing classes from other files in the same
/// package are falsely reported as stale.
#[test]
fn run_lint_structured_cross_file_classes() {
    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    let src_dir = dir.join("src");
    let test_dir = dir.join("test");
    std::fs::create_dir_all(&src_dir).unwrap();
    std::fs::create_dir_all(&test_dir).unwrap();

    // Create a beamtalk.toml so find_package_root works.
    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"cross-test\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    // Define an Actor in src/ — Actor subclasses are known types.
    std::fs::write(
        src_dir.join("my_actor.bt"),
        "Actor subclass: MyActor\n  run => 42\n",
    )
    .unwrap();

    // A test file that uses `@expect all` on `MyActor new` — the `new`
    // message on an Actor produces an instantiation_error diagnostic that
    // the @expect suppresses. Without cross-file class info, the @expect
    // would be reported as stale.
    std::fs::write(
        test_dir.join("my_actor_test.bt"),
        "Object subclass: MyActorTest\n\n  class run =>\n    @expect all\n    MyActor new\n",
    )
    .unwrap();

    // Lint only the test file, but cross-file resolution should still see
    // MyActor from src/.
    let test_file = test_dir.join("my_actor_test.bt");
    let result = run_lint_structured(test_file.to_str().unwrap());

    let stale = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .any(|d| d.message.contains("stale @expect"));
    assert!(
        !stale,
        "MCP lint with cross-file classes should not report @expect as stale, got: {result:?}",
    );
}

/// Write a fixture project whose `src/` declares an `internal` class in
/// one file and leaks it through a public method's signature in a
/// *sibling* file, mirroring `docs/beamtalk-language-features.md`'s
/// TokenBuffer/Parser example (and the CLI's `cli_build.rs` regression
/// for the same fixture, BT-2920). Returns the fixture's `TempDir` (keep
/// it alive for the duration of the test) and the path to
/// `src/parser.bt`.
fn write_cross_file_visibility_leak_fixture() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir).unwrap();

    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src_dir.join("token_buffer.bt"),
        "internal Object subclass: TokenBuffer\n  data => nil\n",
    )
    .unwrap();
    let parser_file = src_dir.join("parser.bt");
    std::fs::write(
        &parser_file,
        "Object subclass: Parser\n  tokenize: input :: String -> TokenBuffer => nil\n",
    )
    .unwrap();

    (temp, parser_file)
}

/// Regression for BT-2921: `current_package` was never threaded into
/// `run_module_analysis`'s `CompilerOptions`, so `check_class_visibility`
/// (E0401/E0402) silently emitted zero diagnostics for MCP `lint`, unlike
/// `beamtalk build`/`beamtalk lint` after BT-2920.
#[test]
fn run_lint_structured_reports_e0402_for_cross_file_internal_class_leak() {
    let (_temp, parser_file) = write_cross_file_visibility_leak_fixture();
    let result = run_lint_structured(parser_file.to_str().unwrap());

    let leaked = result.warnings.iter().chain(result.errors.iter()).any(|d| {
        d.message
            .contains("Internal class 'TokenBuffer' appears in public signature")
    });
    assert!(
        leaked,
        "MCP lint should report E0402 for the cross-file internal class leak, got: {result:?}",
    );
}

/// Same as `run_lint_structured_reports_e0402_for_cross_file_internal_class_leak`
/// but for the `diagnostic_summary` tool (BT-2921).
#[test]
fn compute_diagnostic_summary_reports_e0402_for_cross_file_internal_class_leak() {
    let (_temp, parser_file) = write_cross_file_visibility_leak_fixture();
    let result = compute_diagnostic_summary(parser_file.to_str().unwrap());

    let visibility_total = result["totals_by_category"]["Visibility"]["total"]
        .as_u64()
        .unwrap_or(0);
    assert!(
        visibility_total > 0,
        "diagnostic_summary should report the E0402 visibility leak, got: {result:?}",
    );
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Extension-method integration with the type checker: DNU
//! suppression, return-type flow, and class-side extensions.

use super::*;

// --- Extension method integration with type checker ---

#[test]
fn extension_method_suppresses_dnu_in_analyse_pipeline() {
    // Extension method `Integer >> factorial` defined in same file.
    // `42 factorial` should NOT produce a DNU warning.
    let src = r"
        Integer >> factorial => 1.
        42 factorial.
    ";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Extension method 'factorial' should be visible to type checker, got: {dnu:?}"
    );
}

#[test]
fn extension_method_return_type_flows_through_pipeline() {
    // Extension `String >> shout -> String => ...`
    // `"hello" shout size` should resolve: shout returns String, size is on String.
    let src = r#"
        String >> shout -> String => "HELLO".
        "hello" shout size.
    "#;
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Return type from annotated extension should propagate, got: {dnu:?}"
    );
}

#[test]
fn extension_method_unannotated_no_false_errors() {
    // Extension `Integer >> fancy => ...` with no return type.
    // `42 fancy nonExistent` should NOT warn because fancy returns Dynamic.
    let src = r"
        Integer >> fancy => 1.
        42 fancy nonExistent.
    ";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Unannotated extension returns Dynamic — no false type errors, got: {dnu:?}"
    );
}

#[test]
fn missing_method_still_warns_with_extensions() {
    // Even with extensions registered, truly missing methods should still warn.
    let src = r"
        Integer >> factorial => 1.
        42 totallyBogus.
    ";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu.len(),
        1,
        "Missing method should still produce DNU warning"
    );
    assert!(dnu[0].message.contains("totallyBogus"));
}

#[test]
fn extension_double_colon_return_type_flows_through_pipeline() {
    // Extension `Integer >> double :: -> Integer => self * 2`
    // `42 double + 1` should not produce a DNU warning because
    // `double` returns `Integer`, and `Integer` understands `+`.
    let src = r"
        Integer >> double :: -> Integer => self * 2.
        42 double + 1.
    ";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Return type from :: -> annotated extension should propagate, got: {dnu:?}"
    );
}

#[test]
fn class_side_extension_suppresses_dnu_in_pipeline() {
    // Extension `String class >> fromJson: s :: String -> String => ...`
    // `String fromJson: "{}"` should NOT produce a DNU warning.
    let src = r#"
        String class >> fromJson: s :: String -> String => s.
        String fromJson: "{}".
    "#;
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Class-side extension should suppress DNU, got: {dnu:?}"
    );
}

// ---- unguarded `late` read in a cross-file `Foo >> terminate:` (BT-4863) ----
//
// A same-file `Foo >> terminate:` is folded into `Foo`'s body before the
// check runs; only an extension in a *different* file than the class reaches
// the standalone-method path of `check_unguarded_late_reads_in_lifecycle_hooks`.

const LATE_ACTOR: &str = "typed Actor subclass: CodexClient\n  late state: proc :: Subprocess\n";

fn cross_file_unguarded(ext_src: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let ctx = AnalysisContext::default().with_pre_loaded_classes(declared_by("app", LATE_ACTOR));
    analyse_full(&parse_bt(ext_src), ctx)
        .diagnostics
        .into_iter()
        .filter(|d| {
            d.category == Some(crate::source_analysis::DiagnosticCategory::UnguardedLateRead)
        })
        .collect()
}

#[test]
fn cross_file_extension_terminate_with_unguarded_late_read_warns() {
    let d = cross_file_unguarded("CodexClient >> terminate: reason => self.proc close\n");
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(d[0].message.contains("`proc`"), "{}", d[0].message);
}

#[test]
fn cross_file_extension_terminate_with_guarded_late_read_is_silent() {
    let d = cross_file_unguarded(
        "CodexClient >> terminate: reason =>\n  (self hasField: #proc)\n    ifTrue: [self.proc close]\n  nil\n",
    );
    assert!(d.is_empty(), "{d:?}");
}

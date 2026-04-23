// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! on:do: block-param inference and ifNotNil: narrowing (BT-2045, BT-2046).

use super::common::*;

// --- BT-2045: on:do: block parameter inference ---

/// `on: Exception do: [:e | e message]` should infer `e :: Exception`,
/// so `e message` resolves to `String` with no Dynamic or DNU warnings.
#[test]
fn bt2045_on_do_infers_block_param_from_exception_class() {
    let source = "
typed Object subclass: Repro
  handleIt -> String =>
    [^42]
      on: Exception
      do: [:e | e message]
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "on:do: handler param should be typed as Exception, not Dynamic; got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "e message should resolve — Exception defines `message`; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// Subclass of Exception: `on: Error do: [:e | e message]` should
/// infer `e :: Error` and its inherited `message` method should resolve.
#[test]
fn bt2045_on_do_infers_block_param_from_exception_subclass() {
    let source = "
typed Object subclass: Repro
  handleIt -> String =>
    [^42]
      on: Error
      do: [:e | e message]
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "on:do: handler param should be typed as Error, not Dynamic; got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "e message should resolve — Error inherits `message` from Exception; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// Non-class-reference first argument to `on:do:` should not crash;
/// the handler param falls back to `Dynamic(UnannotatedParam)`.
#[test]
fn bt2045_on_do_non_class_ref_falls_back_to_dynamic() {
    let source = "
typed Object subclass: Repro
  handleIt: exClass :: Object -> Nil =>
    [^42]
      on: exClass
      do: [:e | e message]
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // With a variable (not class reference), we expect Dynamic — which is
    // correct because we can't statically determine the exception class.
    // The test just verifies we don't crash.
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("inferred as Dynamic") && d.message.contains("unannotated parameter")
        })
        .collect();
    assert!(
        !dynamic_warnings.is_empty(),
        "variable first arg should leave handler param as Dynamic(UnannotatedParam)"
    );
}

// --- BT-2046: ifNotNil: block parameter narrowing ---
//
// When the receiver of `ifNotNil: [:x | ...]` is typed `T | Nil`, the block
// parameter `:x` should be typed `T` inside the block (non-nil branch). Dual
// of the receiver-side `isNil ifFalse:` narrowing shipped in BT-2048.

/// Minimal repro from the issue: `self.snapshot` typed `ReplaySnapshot | Nil`,
/// block param `snap` should be `ReplaySnapshot`, so `snap workflowId` resolves.
#[test]
fn bt2046_if_not_nil_narrows_block_param_on_union_field() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  use -> String =>
    ^self.snapshot
      ifNotNil: [:snap | snap workflowId]
      ifNil: ["none"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "snap workflowId should resolve — snap narrowed to ReplaySnapshot; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    // The key signal: no "unannotated parameter" Dynamic warning for snap. If
    // the narrowing is missing, the block param `snap` stays Dynamic and emits
    // a "unannotated parameter" warning; if the block param is narrowed,
    // `snap workflowId` resolves to String and no such warning fires.
    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "ifNotNil: block param `snap` should narrow to ReplaySnapshot, not stay Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Non-nullable receiver: `ifNotNil: [:x | ...]` over plain `T` should still
/// type `x` as `T` (no regression — previously it was `Dynamic`).
#[test]
fn bt2046_if_not_nil_passes_through_non_nullable_receiver() {
    let source = r#"
typed Object subclass: Widget
  name -> String =>
    [^"widget"]

typed Object subclass: Repro
  field: widget :: Widget = nil

  use -> String =>
    self.widget ifNotNil: [:w |
      ^w name
    ]
    ^"none"
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "w name should resolve — w typed as Widget; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "block param `w` should be typed Widget, not Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// `ifNil: [..] ifNotNil: [:x | ..]` — the `ifNotNil:` block param (second
/// argument) should get the non-nil narrowing.
#[test]
fn bt2046_if_nil_if_not_nil_narrows_second_block_param() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  use -> String =>
    ^self.snapshot
      ifNil: ["none"]
      ifNotNil: [:snap | snap workflowId]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "snap workflowId should resolve — snap narrowed to ReplaySnapshot; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "block param `snap` should be narrowed to ReplaySnapshot, not Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// `ifNotNil: [:x | ..] ifNil: [..]` — the `ifNotNil:` block param (first
/// argument) should get the non-nil narrowing.
#[test]
fn bt2046_if_not_nil_if_nil_narrows_first_block_param() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  use -> String =>
    ^self.snapshot
      ifNotNil: [:snap | snap workflowId]
      ifNil: ["none"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "snap workflowId should resolve — snap narrowed to ReplaySnapshot; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "block param `snap` should be narrowed to ReplaySnapshot, not Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Zero-arity `ifNotNil: [ ... ]` (no block param) should not crash and should
/// still type-check normally.
#[test]
fn bt2046_if_not_nil_zero_arity_block_does_not_crash() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  use -> String =>
    self.snapshot ifNotNil: ["present"]
    ^"done"
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.severity == crate::source_analysis::Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "zero-arity ifNotNil: block should type-check cleanly; got errors: {:?}",
        errors.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// Local variable with union annotation: `x :: Widget | Nil` should narrow
/// the block param to `Widget` inside `ifNotNil: [:w | ...]`.
#[test]
fn bt2046_if_not_nil_narrows_local_variable_union() {
    let source = r#"
typed Object subclass: Widget
  name -> String =>
    [^"widget"]

typed Object subclass: Repro
  tryIt: x :: Widget | Nil -> String =>
    ^x
      ifNotNil: [:w | w name]
      ifNil: ["none"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "w name should resolve — w narrowed to Widget from Widget|Nil; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "block param `w` should narrow to Widget, not stay Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `String>>collect:` and `Actor>>onExit:` declare parameterized `Block(...)`
//! callback types instead of bare `Block` (BT-2826).
//!
//! Before this fix, both methods declared their callback as bare `Block`
//! (no type params). In a `typed` class, that left the block's parameter
//! `Dynamic(UnannotatedParam)`, forcing callers to add `@expect type` plus a
//! rebind to recover a concrete param type. With `Block(String, String)` on
//! `String>>collect:` and `Block(Object, Object)` on `Actor>>onExit:`, the
//! block parameter should now resolve to a concrete (non-Dynamic) type with
//! no `@expect` workaround needed (BT-2826).

use super::common::*;

/// `"abc" collect: [:c | c uppercase]` — the block param `c` should be
/// inferred as `String` (grapheme cluster), so `c uppercase` resolves with
/// no DNU and no "unannotated parameter" Dynamic warning.
#[test]
fn bt2826_string_collect_infers_string_block_param() {
    let source = r#"
typed Object subclass: Repro
  shout -> String =>
    ^"abc" collect: [:c | c uppercase]
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
        "c uppercase should resolve — c inferred as String; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "block param `c` should be typed String (from Block(String, String)), \
         not Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// `worker onExit: [:reason | ...]` — the block param `reason` should be
/// inferred as `Object` (the best available bound for an OTP exit reason),
/// so no "unannotated parameter" Dynamic warning fires and no `@expect type`
/// workaround is needed (BT-2826).
#[test]
fn bt2826_actor_on_exit_infers_object_block_param() {
    let source = r"
typed Actor subclass: Worker

typed Object subclass: Repro
  watch: w :: Worker -> Symbol =>
    ^w onExit: [:reason | reason printString]
";
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
        "reason printString should resolve — reason inferred as Object; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "block param `reason` should be typed Object (from Block(Object, Object)), \
         not Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

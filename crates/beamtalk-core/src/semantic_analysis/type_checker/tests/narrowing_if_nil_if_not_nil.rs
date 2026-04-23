// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ifNil:ifNotNil: branch-union return types (BT-2047).

use super::common::*;

// ── BT-2047: `ifNil:ifNotNil:` / `ifNotNil:ifNil:` return-type unification ──
//
// The whole `receiver ifNil: [a] ifNotNil: [:x | b]` expression should type as
// `typeof(a) | typeof(b)` (deduplicated) instead of `Dynamic`. BT-2046 already
// inferred both branches as `Block(..., R)` with narrowed params — this test
// family locks in that the outer send's return type is the branch union.

/// Assert the module's `type_map` contains a Union whose members' class names
/// match `expected` (order-insensitive). Returns the matching type on success.
fn find_union_in_type_map<'a>(
    type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
    expected: &[&str],
) -> Option<&'a InferredType> {
    type_map.iter().find_map(|(_, ty)| {
        if let InferredType::Union { members, .. } = ty {
            let names: std::collections::BTreeSet<String> = members
                .iter()
                .filter_map(|m| m.as_known().map(std::string::ToString::to_string))
                .collect();
            let want: std::collections::BTreeSet<String> =
                expected.iter().map(|s| (*s).to_string()).collect();
            if names == want {
                return Some(ty);
            }
        }
        None
    })
}

/// Walk the module AST to find the first `MessageSend` whose selector name
/// matches `selector_name`, then look up its inferred type in `type_map`.
/// Used by BT-2047 tests to assert the exact type of the `ifNil:ifNotNil:`
/// expression — checking diagnostics alone isn't enough because
/// `check_return_type` bails on `Union` / `Dynamic` inferred bodies.
fn find_send_inferred_ty<'a>(
    module: &crate::ast::Module,
    type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
    selector_name: &str,
) -> Option<&'a InferredType> {
    fn walk_expr<'a>(
        expr: &crate::ast::Expression,
        type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
        selector_name: &str,
    ) -> Option<&'a InferredType> {
        use crate::ast::{Expression, MessageSelector};
        match expr {
            Expression::MessageSend {
                selector,
                span,
                receiver,
                arguments,
                ..
            } => {
                let this_sel = match selector {
                    MessageSelector::Unary(s) | MessageSelector::Binary(s) => s.to_string(),
                    MessageSelector::Keyword(parts) => {
                        parts.iter().map(|p| p.keyword.as_str()).collect::<String>()
                    }
                };
                if this_sel == selector_name {
                    return type_map.get(*span);
                }
                if let Some(t) = walk_expr(receiver, type_map, selector_name) {
                    return Some(t);
                }
                for a in arguments {
                    if let Some(t) = walk_expr(a, type_map, selector_name) {
                        return Some(t);
                    }
                }
                None
            }
            Expression::Return { value, .. }
            | Expression::Parenthesized {
                expression: value, ..
            }
            | Expression::Assignment { value, .. } => walk_expr(value, type_map, selector_name),
            Expression::Block(block) => block
                .body
                .iter()
                .find_map(|s| walk_expr(&s.expression, type_map, selector_name)),
            _ => None,
        }
    }
    for class in &module.classes {
        for method in &class.methods {
            for stmt in &method.body {
                if let Some(t) = walk_expr(&stmt.expression, type_map, selector_name) {
                    return Some(t);
                }
            }
        }
    }
    None
}

/// Minimal repro from the BT-2047 issue: `self.snapshot ifNil: [42] ifNotNil:
/// [:snap | "got one"]` should type as `Integer | String`, not Dynamic.
#[test]
fn bt2047_if_nil_if_not_nil_returns_branch_union() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go =>
    self.snapshot
      ifNil: [42]
      ifNotNil: [:snap | "got one"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "String"]).is_some(),
        "expected `Integer | String` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .filter_map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// Reversed keyword order: `ifNotNil: [:x | a] ifNil: [b]` yields the same
/// union (acceptance criterion #2).
#[test]
fn bt2047_if_not_nil_if_nil_returns_branch_union() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go =>
    self.snapshot
      ifNotNil: [:snap | "got one"]
      ifNil: [42]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "String"]).is_some(),
        "expected `Integer | String` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .filter_map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// When both branches produce the same type, `union_of` dedups to that type
/// (no spurious `String | String`).
#[test]
fn bt2047_same_branch_types_deduplicate() {
    let source = r#"
typed Object subclass: ReplaySnapshot

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go -> String =>
    ^self.snapshot
      ifNil: ["none"]
      ifNotNil: [:snap | "some"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Assert the inferred type of the `ifNil:ifNotNil:` send directly —
    // "no Type diagnostics" alone would pass even if the expression fell
    // back to `Dynamic` (which skips return-type mismatch checks).
    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:ifNotNil:")
        .expect("no ifNil:ifNotNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "both branches typed String — expression should dedup to Known(\"String\"), got: {send_ty:?}",
    );
}

/// Early-returning branch: `ifNil: [^42] ifNotNil: [:x | "ok"]` should type
/// as `String` alone (the non-diverging branch), NOT `Integer | String`. The
/// `^` exits the method before the expression value is observed, so the
/// diverging branch contributes `Never` to the union (AC #3).
#[test]
fn bt2047_diverging_if_nil_branch_contributes_only_other_arm() {
    let source = r#"
typed Object subclass: ReplaySnapshot

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go -> String =>
    ^self.snapshot
      ifNil: [^"early"]
      ifNotNil: [:snap | "got one"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:ifNotNil:")
        .expect("no ifNil:ifNotNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "diverging ifNil: branch should project to Never — expression should be Known(\"String\"), got: {send_ty:?}",
    );
}

/// Key test: the divergent branch has a DIFFERENT type than the surviving
/// branch. `ifNil: [^42]` (Integer) with `ifNotNil: [:x | "ok"]` (String) —
/// without the `Never` projection, the expression would type as
/// `Integer | String` and mismatch the method's declared `-> String`. With
/// it, only the non-diverging branch contributes.
#[test]
fn bt2047_diverging_branch_with_distinct_type_projects_to_never() {
    let source = r#"
typed Object subclass: ReplaySnapshot

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go -> String =>
    ^self.snapshot
      ifNil: [^42]
      ifNotNil: [:snap | "got one"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The key assertion: the send's inferred type is Known("String"), not
    // `Integer | String`. Without the `Never` projection, the union would
    // include Integer and the return type mismatch would surface; with it,
    // the diverging branch drops out.
    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:ifNotNil:")
        .expect("no ifNil:ifNotNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "diverging `ifNil: [^42]` with distinct `ifNotNil:` type should project to \
         Known(\"String\"), not `Integer | String`; got: {send_ty:?}",
    );
}

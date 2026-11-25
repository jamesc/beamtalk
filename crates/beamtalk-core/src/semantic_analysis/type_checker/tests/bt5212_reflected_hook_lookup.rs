// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5212 (ADR 0116 comparison addendum, Phase 3): a number-on-the-left send
//! `n <op> x` whose argument class declares the reflected hook
//! (`plusFromNumber:`, `lessThanFromNumber:`, ...) is not a type error, and is
//! typed from the hook's declared return type. Without the hook the warning
//! stays and its hint names the selector to implement.

use super::common::*;

const OPERATORS: [(&str, &str, &str); 8] = [
    ("+", "plusFromNumber:", "Money"),
    ("-", "minusFromNumber:", "Money"),
    ("*", "timesFromNumber:", "Money"),
    ("/", "divFromNumber:", "Money"),
    ("<", "lessThanFromNumber:", "Boolean"),
    ("<=", "lessOrEqualFromNumber:", "Boolean"),
    (">", "greaterThanFromNumber:", "Boolean"),
    (">=", "greaterOrEqualFromNumber:", "Boolean"),
];

fn source(op: &str, hook: Option<(&str, &str)>, arg_decl: &str) -> String {
    let hook_def = hook
        .map(|(sel, ret)| format!("  {sel} n :: Number -> {ret} => self\n"))
        .unwrap_or_default();
    // `self` is a valid body only for a Money return; Boolean hooks answer true.
    let hook_def = hook_def.replace("-> Boolean => self", "-> Boolean => true");
    format!(
        "typed Value subclass: Money\n{hook_def}\n\
         typed Value subclass: Probe\n  run{arg_decl} =>\n    5 {op} m\n"
    )
}

fn operand_diags(src: &str) -> Vec<Diagnostic> {
    diags_parse_ok(src)
        .into_iter()
        .filter(|d| d.message.contains("expects a"))
        .collect()
}

#[test]
fn hook_present_is_silent_for_every_operator() {
    for (op, sel, ret) in OPERATORS {
        let src = source(op, Some((sel, ret)), ": m :: Money");
        let d = operand_diags(&src);
        assert!(d.is_empty(), "`{op}` with `{sel}`: {d:?}");
    }
}

#[test]
fn hook_present_types_the_send_from_the_hook_return() {
    for (op, sel, ret) in OPERATORS {
        let src = source(op, Some((sel, ret)), ": m :: Money");
        let (types, _) = return_types_and_diags(&src);
        let ty = types.get(&("Probe".into(), "run:".into(), false));
        assert_eq!(
            ty.and_then(InferredType::as_known)
                .map(ecow::EcoString::as_str),
            Some(ret),
            "`{op}`: {ty:?}"
        );
    }
}

#[test]
fn hook_missing_warns_and_names_the_selector() {
    for (op, sel, _) in OPERATORS {
        let src = source(op, None, ": m :: Money");
        let d = operand_diags(&src);
        assert_eq!(d.len(), 1, "`{op}`: {d:?}");
        let hint = d[0].hint.as_deref().unwrap_or_default();
        assert!(
            hint.contains(sel),
            "`{op}` hint should name `{sel}`: {hint}"
        );
        assert!(hint.contains("does_not_understand"), "`{op}`: {hint}");
        assert_eq!(d[0].severity, crate::source_analysis::Severity::Warning);
    }
}

#[test]
fn untyped_argument_is_silent() {
    for (op, _, _) in OPERATORS {
        let src = source(op, None, ": m");
        assert!(operand_diags(&src).is_empty(), "`{op}`");
    }
}

#[test]
fn hook_parameter_is_checked_against_the_receiver() {
    let src = "typed Value subclass: Vec\n  plusFromNumber: n :: String -> Vec => self\n\n\
               typed Value subclass: Probe\n  run: v :: Vec =>\n    5 + v\n";
    let msgs: Vec<String> = diags_parse_ok(src)
        .iter()
        .map(|d| d.message.to_string())
        .filter(|m| !m.starts_with("Missing return type"))
        .collect();
    assert_eq!(msgs.len(), 1, "{msgs:?}");
    assert!(
        msgs[0].contains("plusFromNumber:") && msgs[0].contains("String"),
        "{msgs:?}"
    );
}

fn run_type(src: &str) -> (Option<InferredType>, Vec<Diagnostic>) {
    let (types, diags) = return_types_and_diags(src);
    (
        types.get(&("Probe".into(), "run:".into(), false)).cloned(),
        // Annotation-completeness lints are not what these tests are about.
        diags
            .into_iter()
            .filter(|d| !d.message.starts_with("Missing return type"))
            .collect(),
    )
}

#[test]
fn hook_without_return_annotation_is_not_typed_as_the_number_result() {
    let src = "typed Value subclass: Money\n  plusFromNumber: n :: Number => self\n\n\
               typed Value subclass: Probe\n  run: m :: Money =>\n    5 + m\n";
    let (ty, diags) = run_type(src);
    assert!(diags.is_empty(), "{diags:?}");
    assert!(
        !matches!(
            ty.as_ref()
                .and_then(InferredType::as_known)
                .map(|s| s.as_str()),
            Some("Integer")
        ),
        "{ty:?}"
    );
}

#[test]
fn inherited_hook_and_float_receiver_are_found() {
    let src = "typed Value subclass: Base\n  plusFromNumber: n :: Number -> Base => self\n\n\
               Base subclass: Money\n\n\
               typed Value subclass: Probe\n  run: m :: Money =>\n    5.5 + m\n";
    let (ty, diags) = run_type(src);
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        ty.as_ref()
            .and_then(InferredType::as_known)
            .map(|s| s.as_str()),
        Some("Base")
    );
}

#[test]
fn numeric_argument_does_not_take_the_hook_path() {
    let src = "typed Value subclass: Probe\n  run: m :: Float =>\n    5 + m\n";
    let (_, diags) = run_type(src);
    assert!(diags.is_empty(), "{diags:?}");
}

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
    let diags = diags_parse_ok(src);
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("plusFromNumber:") || d.message.contains("String")),
        "{:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

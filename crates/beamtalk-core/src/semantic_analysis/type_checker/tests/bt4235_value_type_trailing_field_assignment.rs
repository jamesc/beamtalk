// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4235: on codegen's value-type body path a trailing `self.x := v`
//! answers the updated `self` (BT-833), with or without statement-level
//! parentheses (BT-4092), so the method's inferred return type is the
//! receiver type, not `v`'s. The path is decided by the shared
//! `ClassHierarchy::compiles_as_actor` (class bodies) and
//! `ClassHierarchy::extension_target_is_actor` (standalone extensions).
//! Actor methods and class-side methods still infer the assigned value's
//! type.

use super::common::*;

/// Parses `source` and returns the inferred method return types plus the
/// type checker's diagnostics.
fn check_source(
    source: &str,
) -> (
    std::collections::HashMap<MethodReturnKey, InferredType>,
    Vec<crate::source_analysis::Diagnostic>,
) {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let diagnostics = checker.take_diagnostics();
    (checker.take_method_return_types(), diagnostics)
}

fn return_type<'a>(
    types: &'a std::collections::HashMap<MethodReturnKey, InferredType>,
    class: &str,
    selector: &str,
    is_class_method: bool,
) -> Option<&'a InferredType> {
    types.get(&(class.into(), selector.into(), is_class_method))
}

fn dnu_messages(diagnostics: &[crate::source_analysis::Diagnostic]) -> Vec<&str> {
    diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .map(|d| d.message.as_str())
        .collect()
}

/// A value type whose `bump` ends with `last`, plus a caller that sends to
/// `bump`'s answer.
fn value_type_source(last: &str) -> String {
    format!(
        "Value subclass: Probe\n  field: m = 0\n  mValue => self.m\n  bump =>\n    t := 1\n    {last}\n  useIt =>\n    r := self bump\n    r mValue\n"
    )
}

/// The issue's repro: a bare trailing field assignment in a value-type
/// method answers `Probe`, so a send to the answer draws no DNU hint.
#[test]
fn bare_trailing_field_assignment_in_value_type_infers_receiver_type() {
    let (types, diagnostics) = check_source(&value_type_source("self.m := 5"));
    assert_eq!(
        return_type(&types, "Probe", "bump", false),
        Some(&InferredType::known("Probe"))
    );
    assert!(dnu_messages(&diagnostics).is_empty(), "{diagnostics:?}");
}

/// Statement-level parentheses change nothing (BT-4092), at any depth and in
/// a single-statement method.
#[test]
fn parenthesised_trailing_field_assignment_in_value_type_infers_receiver_type() {
    for last in ["(self.m := 5)", "((self.m := self.m + 1))"] {
        let (types, diagnostics) = check_source(&value_type_source(last));
        assert_eq!(
            return_type(&types, "Probe", "bump", false),
            Some(&InferredType::known("Probe")),
            "`{last}`"
        );
        assert!(
            dnu_messages(&diagnostics).is_empty(),
            "`{last}`: {diagnostics:?}"
        );
    }
    let (types, _) =
        check_source("Value subclass: Probe\n  field: m = 0\n  bump => (self.m := 5)\n");
    assert_eq!(
        return_type(&types, "Probe", "bump", false),
        Some(&InferredType::known("Probe"))
    );
}

/// Only the LAST statement counts, and operand-position parentheses keep the
/// assigned value: neither answers `self`.
#[test]
fn non_trailing_or_operand_field_assignment_keeps_its_value_type() {
    for last in ["self.m := 5\n    7", "(self.m := 5) + 1"] {
        let (types, _) = check_source(&value_type_source(last));
        assert_eq!(
            return_type(&types, "Probe", "bump", false),
            Some(&InferredType::known("Integer")),
            "`{last}`"
        );
    }
}

/// An actor method's trailing field assignment answers the assigned value,
/// with or without parentheses: unchanged.
#[test]
fn trailing_field_assignment_in_actor_infers_assigned_value_type() {
    for last in ["self.m := 5", "(self.m := 5)"] {
        let src = format!("Actor subclass: ProbeActor\n  state: m = 0\n  bump =>\n    {last}\n");
        let (types, _) = check_source(&src);
        assert_eq!(
            return_type(&types, "ProbeActor", "bump", false),
            Some(&InferredType::known("Integer")),
            "`{last}`"
        );
    }
}

/// A class-side method of a value type writes a class variable and answers
/// the assigned value: unchanged.
#[test]
fn trailing_class_variable_assignment_infers_assigned_value_type() {
    for last in ["self.m := 5", "(self.m := 5)"] {
        let src = format!(
            "Object subclass: ProbeClassSide\n  classState: m = 0\n  class bump =>\n    {last}\n"
        );
        let (types, _) = check_source(&src);
        assert_eq!(
            return_type(&types, "ProbeClassSide", "bump", true),
            Some(&InferredType::known("Integer")),
            "`{last}`"
        );
    }
}

/// An instance-side extension on a value type lowers through the value-type
/// body path, so it answers the receiver too; one on an actor target, or on
/// the class side, answers the assigned value. (A send to the extension's
/// answer is not checked here: `ClassHierarchy::build` alone does not
/// register extension selectors.)
#[test]
fn trailing_field_assignment_in_extension_follows_extension_target_rule() {
    for last in ["self.m := 5", "(self.m := 5)"] {
        let src = format!("Value subclass: Probe\n  field: m = 0\n\nProbe >> bump => {last}\n");
        let (types, _) = check_source(&src);
        assert_eq!(
            return_type(&types, "Probe", "bump", false),
            Some(&InferredType::known("Probe")),
            "value-type extension `{last}`"
        );

        let src =
            format!("Actor subclass: ProbeActor\n  state: m = 0\n\nProbeActor >> bump => {last}\n");
        let (types, _) = check_source(&src);
        assert_eq!(
            return_type(&types, "ProbeActor", "bump", false),
            Some(&InferredType::known("Integer")),
            "actor extension `{last}`"
        );

        let src = format!(
            "Object subclass: ProbeClassSide\n  classState: m = 0\n\nProbeClassSide class >> bump => {last}\n"
        );
        let (types, _) = check_source(&src);
        assert_eq!(
            return_type(&types, "ProbeClassSide", "bump", true),
            Some(&InferredType::known("Integer")),
            "class-side extension `{last}`"
        );
    }
}

/// The other side of the fix: a value-type method that declares a
/// non-receiver return type and ends in a field write now draws a
/// return-type mismatch, since it answers `self`. Declaring the receiver
/// type (`Self` or the class) stays silent.
#[test]
fn declared_return_type_is_checked_against_the_receiver() {
    let mismatches = |ret: &str| {
        let src =
            format!("Value subclass: Probe\n  field: m = 0\n  bump -> {ret} => self.m := 5\n");
        let (_, diagnostics) = check_source(&src);
        diagnostics
            .into_iter()
            .filter(|d| d.message.contains("declares return type"))
            .map(|d| d.message)
            .collect::<Vec<_>>()
    };
    let integer = mismatches("Integer");
    assert_eq!(integer.len(), 1, "{integer:?}");
    assert!(integer[0].contains("Probe"), "{integer:?}");
    for ret in ["Self", "Probe"] {
        assert!(
            mismatches(ret).is_empty(),
            "`-> {ret}`: {:?}",
            mismatches(ret)
        );
    }
}

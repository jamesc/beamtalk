// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Non-overridable operator declaration checks (BT-2997).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Per ADR 0002, Beamtalk's four equality operators — `=:=`, `=/=`, `==` and
//! `/=` — are *not* message sends. Codegen lowers each one straight to the
//! corresponding Erlang BIF (`operators.rs`, `generate_binary_op`) with no
//! class lookup and no runtime dispatch guard, unlike the arithmetic
//! (`+ - * /`) and ordering (`< > <= >=`) operators, which BT-2709/BT-2710
//! made dispatchable so value types can overload them.
//!
//! The consequence is that a class-level `=:=` method **never runs**. It parses,
//! typechecks, appears in method listings, and is compiled into the class
//! module — but no expression can reach it. `Duration` shipped exactly such a
//! pair of methods and its tests passed only because Duration canonicalises to
//! a single `millis` field, so raw structural equality happened to agree with
//! the override that was never called.
//!
//! This is not a limitation worth lifting. The keyed containers decide identity
//! inside the VM, below anything the language can dispatch: `Dictionary` is
//! backed by Erlang maps (keys compare with `=:=`), `Set` by `ordsets`
//! (elements compare by term order, i.e. `==`). So do `lists:member/2`, `ets`,
//! and receive-pattern matching. An override the compiler honoured would still
//! be invisible to all of them, producing `a =:= b` while a `Set` holding both
//! still reports size 2. Dispatch would also make equality asymmetric (it keys
//! on the left receiver alone) and cost a guard on the language's hottest
//! operator.
//!
//! So the operators stay primitive and this validator makes the dead
//! declaration impossible to write, pointing authors at `equals:` — the
//! overridable, representation-independent equality declared on `Object`.
//!
//! ## What stays legal
//!
//! A body that is a single `@primitive` / `@intrinsic` pragma is *not* an
//! override: it is how the stdlib declares the built-in lowering itself, so
//! that `=:=` shows up in completions and `respondsTo:` with a class-specific
//! signature (`proto_object.bt:50`, `integer.bt:99`, …). Codegen ignores those
//! bodies for the same reason it ignores every other one. They are exempt.

use crate::ast::{Expression, MessageSelector, MethodDefinition, Module};
use crate::source_analysis::{Diagnostic, Span};

/// The operators codegen lowers directly to Erlang BIFs, with no message
/// dispatch, and which therefore cannot be overridden per-class (ADR 0002).
///
/// Deliberately excludes `+ - * /` and `< > <= >=`: those *are* dispatchable
/// (BT-2709/BT-2710), which is why value types like `Duration` and `DateTime`
/// can and do override them.
const NON_OVERRIDABLE_OPERATORS: &[&str] = &["=:=", "=/=", "==", "/="];

/// BT-2997: Reject method declarations for operators that codegen never
/// dispatches, so they cannot be written as silently-dead code.
///
/// Covers instance- and class-side methods on every class in the module, plus
/// standalone (Tonel-style) `MyClass >> =:= other => …` definitions.
pub(crate) fn check_non_overridable_operator_methods(
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            check_method(method, diagnostics);
        }
    }

    for standalone in &module.method_definitions {
        check_method(&standalone.method, diagnostics);
    }
}

/// Emits a diagnostic if `method` declares a non-dispatchable operator with a
/// real (non-pragma) body.
fn check_method(method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>) {
    let MessageSelector::Binary(op) = &method.selector else {
        return;
    };
    if !NON_OVERRIDABLE_OPERATORS.contains(&op.as_str()) || is_primitive_declaration(method) {
        return;
    }

    diagnostics.push(
        Diagnostic::error(
            format!(
                "`{op}` cannot be overridden — this method can never be called. \
                 Per ADR 0002 the compiler lowers `{op}` directly to Erlang's `{op}` \
                 operator, with no method dispatch"
            ),
            operator_span(method),
        )
        .with_hint(
            "Rename it to `equals:` (declared on `Object`, defaulting to `=:=`), or to a \
             domain-specific name such as `sameInstant:`. Note that `Dictionary` and \
             `Set` decide key identity in the VM (Erlang maps and `ordsets`), so neither \
             an operator nor an `equals:` override can affect them — a class that needs \
             content-based membership must normalise its representation."
                .to_string(),
        ),
    );
}

/// Whether `method`'s body is a single `@primitive` / `@intrinsic` pragma.
///
/// Such a declaration documents the built-in lowering rather than attempting to
/// replace it, so it is exempt (see the module docs).
fn is_primitive_declaration(method: &MethodDefinition) -> bool {
    matches!(
        method.body.as_slice(),
        [stmt] if matches!(stmt.expression, Expression::Primitive { .. })
    )
}

/// The span to anchor the diagnostic on: the declaration head only.
///
/// `method.span` covers the whole declaration including the body, which for a
/// multi-line body underlines far more than the mistake. Narrow it to the first
/// line so the caret sits on the selector.
fn operator_span(method: &MethodDefinition) -> Span {
    method.body.first().map_or(method.span, |first| {
        Span::new(method.span.start(), first.expression.span().start())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_support::parse_bt;

    fn diagnostics_for(source: &str) -> Vec<Diagnostic> {
        let module = parse_bt(source);
        let mut diagnostics = Vec::new();
        check_non_overridable_operator_methods(&module, &mut diagnostics);
        diagnostics
    }

    #[test]
    fn rejects_strict_equality_override() {
        let diagnostics = diagnostics_for(
            "Value subclass: Money\n  \
             state: cents :: Integer = 0\n  \
             =:= other :: Money -> Boolean => self.cents =:= other.cents\n",
        );
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert!(
            diagnostics[0]
                .message
                .contains("`=:=` cannot be overridden"),
            "{}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .is_some_and(|h| h.contains("equals:")),
            "hint should point at `equals:`"
        );
    }

    #[test]
    fn rejects_all_four_equality_operators() {
        for op in ["=:=", "=/=", "==", "/="] {
            let source = format!(
                "Value subclass: Money\n  \
                 state: cents :: Integer = 0\n  \
                 {op} other :: Money -> Boolean => true\n"
            );
            let diagnostics = diagnostics_for(&source);
            assert_eq!(diagnostics.len(), 1, "expected `{op}` to be rejected");
        }
    }

    #[test]
    fn allows_primitive_and_intrinsic_declarations() {
        // How the stdlib declares the built-in lowering (ProtoObject, Integer, …).
        let diagnostics = diagnostics_for(
            "Value subclass: Money\n  \
             =:= other :: Money -> Boolean => @primitive \"=:=\"\n  \
             =/= other :: Money -> Boolean => @intrinsic \"=/=\"\n",
        );
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }

    #[test]
    fn allows_dispatchable_operators() {
        // `+ - * /` and `< > <= >=` are message-dispatched (BT-2709/2710), so
        // overriding them is the supported way to write a value type.
        let diagnostics = diagnostics_for(
            "Value subclass: Money\n  \
             state: cents :: Integer = 0\n  \
             + other :: Money -> Money => Money cents: self.cents + other.cents\n  \
             < other :: Money -> Boolean => self.cents < other.cents\n",
        );
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }

    #[test]
    fn allows_named_equality_methods() {
        let diagnostics = diagnostics_for(
            "Value subclass: Money\n  \
             state: cents :: Integer = 0\n  \
             equals: other :: Money -> Boolean => self.cents =:= other.cents\n",
        );
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }

    #[test]
    fn rejects_standalone_method_definition() {
        let diagnostics = diagnostics_for(
            "Value subclass: Money\n  \
             state: cents :: Integer = 0\n\n\
             Money >> =:= other :: Money -> Boolean => self.cents =:= other.cents\n",
        );
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    }
}

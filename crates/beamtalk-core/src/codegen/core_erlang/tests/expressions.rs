// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for expression-level Core Erlang code generation.
//!
//! Covers literal codegen (integers, floats, symbols, strings, maps), binary
//! operators, variable naming, string interpolation, type erasure, and
//! state-variable field access patterns.

use super::*;

#[test]
fn test_generate_literal_integer() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::Integer(42);
    let doc = generator.generate_literal(&lit).unwrap();
    assert_eq!(doc.to_pretty_string(), "42");
}

#[test]
fn test_generate_literal_float() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::Float(2.5);
    let doc = generator.generate_literal(&lit).unwrap();
    assert_eq!(doc.to_pretty_string(), "2.5");
}

#[test]
fn test_generate_literal_float_whole_number() {
    // BT-1562: Whole-number floats must retain decimal point in Core Erlang
    // so that Erlang distinguishes them from integers (5.0 ≠ 5).
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::Float(5.0);
    let doc = generator.generate_literal(&lit).unwrap();
    assert_eq!(doc.to_pretty_string(), "5.0");
}

#[test]
fn test_generate_literal_symbol() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::Symbol("ok".into());
    let doc = generator.generate_literal(&lit).unwrap();
    assert_eq!(doc.to_pretty_string(), "'ok'");
}

#[test]
fn test_generate_literal_string() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::String("hello".into());
    let doc = generator.generate_literal(&lit).unwrap();
    // Core Erlang binary syntax: #{segment, ...}#
    // Each segment is #<charcode>(8,1,'integer',['unsigned'|['big']])
    assert_eq!(
        doc.to_pretty_string(),
        "#\
{#<104>(8,1,'integer',['unsigned'|['big']]),\
#<101>(8,1,'integer',['unsigned'|['big']]),\
#<108>(8,1,'integer',['unsigned'|['big']]),\
#<108>(8,1,'integer',['unsigned'|['big']]),\
#<111>(8,1,'integer',['unsigned'|['big']])}#",
        "String 'hello' should generate correct Core Erlang binary literal"
    );
}

#[test]
fn test_generate_binary_op_addition() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(4), Span::new(4, 5))];
    let doc = generator.generate_binary_op("+", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    // Binary op codegen emits plain operands with no maybe_await wrapping (ADR-0043).
    assert_eq!(output, "call 'erlang':'+'(3, 4)");
}

#[test]
fn test_generate_binary_op_no_wrap_for_literals() {
    // Binary op codegen never wraps operands with maybe_await (ADR-0043).
    let mut generator = CoreErlangGenerator::new("test");
    for (op, l, r) in [
        (
            "+",
            Expression::Literal(Literal::Integer(1), Span::new(0, 1)),
            Expression::Literal(Literal::Integer(2), Span::new(2, 3)),
        ),
        (
            "<",
            Expression::Literal(Literal::Integer(0), Span::new(0, 1)),
            Expression::Literal(Literal::Float(1.0), Span::new(2, 4)),
        ),
    ] {
        let doc = generator.generate_binary_op(op, &l, &[r]).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            !output.contains("maybe_await"),
            "literal operands should not be wrapped in maybe_await; got: {output}"
        );
    }
}

#[test]
fn test_generate_binary_op_no_wrap_for_identifiers() {
    use crate::ast::Identifier;
    // Since ADR-0043 all actor sends are sync — no expression produces a future.
    // Binary op codegen must NOT wrap any operand with maybe_await.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Identifier(Identifier {
        name: "x".into(),
        span: Span::new(0, 1),
    });
    let right = vec![Expression::Literal(Literal::Integer(1), Span::new(4, 5))];
    let doc = generator.generate_binary_op("+", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        !output.contains("maybe_await"),
        "no operand should be wrapped in maybe_await; got: {output}"
    );
    assert!(
        output.contains("call 'erlang':'+'("),
        "should still generate erlang:'+' call; got: {output}"
    );
}

// BT-2709: arithmetic operators are dispatchable messages. The bare-BIF fast
// path must hold for statically-numeric receivers (literals, `self` in
// Integer/Float, `:: Number`-family params); everything else gets a runtime
// `is_number` guard that falls back to message dispatch.

#[test]
fn test_arith_bare_bif_for_numeric_literal_receiver() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(4), Span::new(4, 5))];
    let output = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert_eq!(output, "call 'erlang':'+'(3, 4)");
    assert!(
        !output.contains("is_number"),
        "numeric literal receiver must skip the guard; got: {output}"
    );
}

#[test]
fn test_arith_bare_bif_for_self_in_numeric_class() {
    use crate::ast::Identifier;
    // ADR 0116/BT-3263: `self <op> other` in Integer/Float stays a bare BIF
    // with no `try` only when the right operand is ALSO statically numeric —
    // here `other :: Number`, so `receiver_is_statically_numeric(right)` is
    // `true` and the always-bare path never engages the coercion mechanism.
    for class in ["Integer", "Float"] {
        let mut generator = CoreErlangGenerator::new("test");
        generator.set_class_identity(Some(util::ClassIdentity::new(class)));
        generator
            .current_method_param_types
            .insert("other".to_string(), "Number".to_string());
        let left = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
        let right = vec![Expression::Identifier(Identifier::new(
            "other",
            Span::new(7, 12),
        ))];
        let output = generator
            .generate_binary_op("*", &left, &right)
            .unwrap()
            .to_pretty_string();
        assert!(
            output.contains("call 'erlang':'*'(")
                && !output.contains("is_number")
                && !output.contains("try "),
            "`self * other` in {class} with other :: Number must be a bare BIF; got: {output}"
        );
    }
}

#[test]
fn test_arith_coercion_try_for_self_in_numeric_class_unknown_right() {
    use crate::ast::Identifier;
    // ADR 0116/BT-3263: `self * x` in a numeric class with an UNTYPED right
    // operand must wrap the bare BIF in the number-on-the-left coercion
    // try/catch — a statically-numeric left operand no longer means "skip all
    // guards" once the right operand's type is genuinely unknown (`5 *
    // aVector`).
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Integer")));
    let left = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
    let right = vec![Expression::Identifier(Identifier::new(
        "Other",
        Span::new(7, 12),
    ))];
    let output = generator
        .generate_binary_op("*", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("try call 'erlang':'*'("),
        "unknown right operand must wrap the bare BIF in try/catch; got: {output}"
    );
    assert!(
        output.contains("call 'erlang':'is_number'("),
        "coercion try must re-check is_number on the right operand; got: {output}"
    );
    assert!(
        output.contains("call 'beamtalk_message_dispatch':'send_number_coercion'(")
            && output.contains("'timesFromNumber:'"),
        "non-numeric right operand must dispatch to timesFromNumber:; got: {output}"
    );
}

#[test]
fn test_arith_bare_bif_for_numeric_param() {
    use crate::ast::Identifier;
    for ty in ["Integer", "Float", "Number"] {
        let mut generator = CoreErlangGenerator::new("test");
        generator
            .current_method_param_types
            .insert("other".to_string(), ty.to_string());
        let left = Expression::Identifier(Identifier::new("other", Span::new(0, 5)));
        let right = vec![Expression::Literal(Literal::Integer(1), Span::new(8, 9))];
        let output = generator
            .generate_binary_op("-", &left, &right)
            .unwrap()
            .to_pretty_string();
        assert!(
            output.contains("call 'erlang':'-'(") && !output.contains("is_number"),
            "`other - 1` with other :: {ty} must be a bare BIF; got: {output}"
        );
    }
}

#[test]
fn test_arith_guard_for_unknown_receiver_numeric_right_no_try() {
    use crate::ast::Identifier;
    // ADR 0116/BT-3263: the right operand is statically numeric (`:: Number`)
    // here, so the guard's is_number-true branch stays a bare BIF — the
    // coercion try/catch never engages because the right operand's type is
    // already known at compile time.
    let mut generator = CoreErlangGenerator::new("test");
    generator
        .current_method_param_types
        .insert("y".to_string(), "Number".to_string());
    let left = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let right = vec![Expression::Identifier(Identifier::new(
        "y",
        Span::new(4, 5),
    ))];
    let output = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    // Runtime guard: is_number → BIF, else dispatch (no try/catch, no bare BIF).
    assert!(
        output.contains("call 'erlang':'is_number'("),
        "unknown receiver must emit an is_number guard; got: {output}"
    );
    assert!(
        output.contains("call 'beamtalk_message_dispatch':'send'(") && output.contains("'+'"),
        "guard must dispatch '+' as a message on the false arm; got: {output}"
    );
    assert!(
        !output.contains("try "),
        "statically-numeric right operand must not enter the coercion try; got: {output}"
    );
}

#[test]
fn test_arith_guard_for_unknown_receiver_and_unknown_right_uses_coercion_try() {
    use crate::ast::Identifier;
    // ADR 0116/BT-3263: both operands are genuinely unknown here, so the
    // guard's is_number-true (bare BIF) branch must itself become the
    // number-on-the-left coercion try/catch — a numeric-at-runtime `x` could
    // still meet a non-numeric `y`.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let right = vec![Expression::Identifier(Identifier::new(
        "y",
        Span::new(4, 5),
    ))];
    let output = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("call 'erlang':'is_number'("),
        "unknown receiver must emit an is_number guard; got: {output}"
    );
    assert!(
        output.contains("call 'beamtalk_message_dispatch':'send'(") && output.contains("'+'"),
        "guard must dispatch '+' as a message on the false arm; got: {output}"
    );
    assert!(
        output.contains("try call 'erlang':'+'("),
        "unknown right operand must wrap the guard's bare-BIF branch in try/catch; got: {output}"
    );
    assert!(
        output.contains("call 'beamtalk_message_dispatch':'send_number_coercion'(")
            && output.contains("'plusFromNumber:'"),
        "coercion try must dispatch to plusFromNumber: when right isn't a number; got: {output}"
    );
}

#[test]
fn test_arith_guard_for_self_in_non_numeric_class() {
    use crate::ast::Identifier;
    // A user value-type overloading `+`: `self + other` must NOT be a bare BIF.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Money")));
    let left = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
    let right = vec![Expression::Identifier(Identifier::new(
        "Other",
        Span::new(7, 12),
    ))];
    let output = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("call 'erlang':'is_number'("),
        "`self + x` in a non-numeric class must be guarded; got: {output}"
    );
}

// ADR 0116/BT-3263: number-on-the-left coercion. `5 + aVector` — a
// statically-numeric left operand and a right operand whose type is
// genuinely unknown — must wrap the bare arithmetic BIF in a badarith-
// catching try/catch that dispatches to the right operand's
// `<verb>FromNumber:` reflected method instead of crashing.

#[test]
fn test_number_coercion_try_for_literal_left_unknown_right() {
    use crate::ast::Identifier;
    // The always-bare path: left is a numeric literal (no guard at all today),
    // right is an untyped identifier — genuinely unknown at compile time.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let right = vec![Expression::Identifier(Identifier::new(
        "aVector",
        Span::new(4, 11),
    ))];
    let output = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("try call 'erlang':'+'("),
        "numeric literal left with unknown right must wrap in try/catch; got: {output}"
    );
    assert!(
        output.contains("of <") && output.contains("> -> "),
        "try must use the mandatory `of <Var> -> Var` clause; got: {output}"
    );
    assert!(
        output.contains("catch <") && output.contains("case_clause"),
        "catch must bind Type/Error/Stack and include the BT-3163 case_clause fallback; got: {output}"
    );
    assert!(
        output.contains("call 'erlang':'is_number'("),
        "badarith arm must re-check is_number on the right operand; got: {output}"
    );
    assert!(
        output.contains("primop 'raw_raise'("),
        "a genuinely numeric badarith (e.g. div-by-zero) must re-raise unchanged; got: {output}"
    );
    assert!(
        output.contains("call 'beamtalk_message_dispatch':'send_number_coercion'(")
            && output.contains("'plusFromNumber:'")
            && output.contains(", '+')"),
        "non-numeric right must dispatch via send_number_coercion/4 with the '+' op symbol; got: {output}"
    );
}

#[test]
fn test_number_coercion_bare_bif_unaffected_for_total_plus_delta() {
    use crate::ast::Identifier;
    // ADR 0116 § Consequences, Positive: `total + delta`-shaped call sites —
    // numeric literal left, `:: Number`-typed right — must emit byte-for-byte
    // the same bare BIF as before this ADR, with no try at all.
    let mut generator = CoreErlangGenerator::new("test");
    generator
        .current_method_param_types
        .insert("delta".to_string(), "Integer".to_string());
    let left = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
    let right = vec![Expression::Identifier(Identifier::new(
        "delta",
        Span::new(4, 9),
    ))];
    let output = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        !output.contains("try "),
        "a statically-numeric right operand must never enter the coercion try; got: {output}"
    );
    assert!(
        output.starts_with("call 'erlang':'+'("),
        "must be the exact bare-BIF shape with no wrapping; got: {output}"
    );
}

#[test]
fn test_number_coercion_selector_per_operator() {
    use crate::ast::Identifier;
    // New helper mapping `+ - * /` -> `plusFromNumber:` / `minusFromNumber:` /
    // `timesFromNumber:` / `divFromNumber:`.
    for (op, selector) in [
        ("+", "plusFromNumber:"),
        ("-", "minusFromNumber:"),
        ("*", "timesFromNumber:"),
        ("/", "divFromNumber:"),
    ] {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
        let right = vec![Expression::Identifier(Identifier::new(
            "aVector",
            Span::new(4, 11),
        ))];
        let output = generator
            .generate_binary_op(op, &left, &right)
            .unwrap()
            .to_pretty_string();
        let expected_selector_atom = format!("'{selector}'");
        assert!(
            output.contains("call 'beamtalk_message_dispatch':'send_number_coercion'(")
                && output.contains(&expected_selector_atom),
            "operator {op} must dispatch to {selector}; got: {output}"
        );
    }
}

#[test]
fn test_number_coercion_does_not_double_evaluate_right_operand() {
    use crate::ast::Identifier;
    // A non-trivial right operand (a message send) must be evaluated exactly
    // once — referencing it a second time inside the catch handler would
    // silently duplicate a side effect.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let right_expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new(
            "factory",
            Span::new(4, 11),
        ))),
        selector: MessageSelector::Unary("makeVector".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(4, 24),
    };
    let output = generator
        .generate_binary_op("+", &left, &[right_expr])
        .unwrap()
        .to_pretty_string();
    let dispatch_call_count = output.matches("'makeVector'").count();
    assert_eq!(
        dispatch_call_count, 1,
        "the right operand's message send must be evaluated exactly once; got: {output}"
    );
}

#[test]
fn test_number_coercion_try_catch_compiles_through_erlc() {
    use crate::ast::Identifier;
    // ADR 0116 § Implementation de-risking spike: the exact try/catch shape
    // must round-trip through `erlc`, not just look right as a string —
    // Core Erlang's mandatory `of <Var> -> Var` clause and mandatory `when`
    // guard on every case clause are both syntax errors if omitted, and
    // `erlc` is the only real check for that.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let right = vec![Expression::Identifier(Identifier::new(
        "aVector",
        Span::new(4, 11),
    ))];
    let body = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    // The generated expression reads `aVector` via `maps:get('aVector',
    // State)` (identifiers resolve through the workspace `State` map in this
    // generator context) — bind a `State` so the module is self-contained.
    let core_erlang = format!(
        "module 'coercion_try' ['f'/0]\n    attributes []\n'f'/0 =\n    ( fun () ->\n\t  let State = ~{{}}~ in\n\t  {body}\n      -| [] )\nend\n"
    );
    crate::test_helpers::assert_compiles_through_erlc("coercion_try", &core_erlang);
}

#[test]
fn test_number_coercion_untyped_self_field_right_operand_stays_bare() {
    // Known, tracked gap (BT-3266, filed from BT-3263 code review): the
    // right-operand gate reuses `receiver_is_statically_numeric` verbatim —
    // per ADR 0116's own "Trigger condition, refined" spec ("a numeric/
    // untyped field — the exact same rule already applied to the left
    // operand") — so an UNTYPED `self.field` on the right is treated as
    // statically numeric and skips the new try/catch entirely, exactly like
    // an untyped left-operand field already does (BT-2709's deliberate
    // `self.count := self.count + 1` performance trade-off). This means
    // `self.total + self.extra` with `extra` untyped still raw-crashes on
    // badarith at runtime if `extra` isn't a number, rather than dispatching
    // to `plusFromNumber:` — pinned down here as documented, current
    // behavior, not asserted as correct; BT-3266 tracks whether to close it.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let right = self_field_access("extra");
    let output = generator
        .generate_binary_op("+", &left, &[right])
        .unwrap()
        .to_pretty_string();
    assert!(
        !output.contains("try ") && !output.contains("send_number_coercion"),
        "BT-3266: an untyped self.field right operand currently stays on the \
         bare-BIF path (inherits BT-2709's left-operand leniency) — got: {output}"
    );
}

#[test]
fn test_number_coercion_untyped_self_fields_both_sides_stay_bare() {
    // BT-3266 (ADR 0116 addendum, decision: accept the gap): this issue's
    // own literal example, `self.total + self.extra` with both fields
    // untyped. `receiver_is_statically_numeric` trusts an untyped
    // `self.<field>` on either side of the operator identically, so this
    // stays a bare `erlang:'+'` — no guard, no try/catch — exactly like the
    // single-field-on-the-right case above. Pinned down as documented,
    // accepted behavior, not asserted as correct.
    let mut generator = CoreErlangGenerator::new("test");
    let left = self_field_access("total");
    let right = self_field_access("extra");
    let output = generator
        .generate_binary_op("+", &left, &[right])
        .unwrap()
        .to_pretty_string();
    assert!(
        !output.contains("try ") && !output.contains("send_number_coercion"),
        "BT-3266: self.total + self.extra (both untyped) stays on the bare-BIF \
         path on both operands — got: {output}"
    );
    assert!(
        output.contains("call 'erlang':'+'("),
        "expected a bare erlang:'+' call; got: {output}"
    );
}

// BT-2710: comparison operators (`< > <= >=`) are dispatchable messages. The
// bare-BIF fast path holds for statically-comparable receivers (numeric / char
// / string literals, `self` in Integer/Float/Character/String, and
// Integer/Float/Number/Character/String params); everything else gets a runtime
// `is_object` guard (NOT `is_number`) that dispatches on objects and keeps the
// bare comparison BIF for primitives.

#[test]
fn test_comparison_bare_bif_for_literal_receivers() {
    // Numeric, character, and string literals all keep the bare comparison BIF.
    for (left, erl) in [
        (
            Expression::Literal(Literal::Integer(3), Span::new(0, 1)),
            "<",
        ),
        (
            Expression::Literal(Literal::Character('a'), Span::new(0, 2)),
            "<",
        ),
        (
            Expression::Literal(Literal::String("x".into()), Span::new(0, 3)),
            "<",
        ),
    ] {
        let mut generator = CoreErlangGenerator::new("test");
        let right = vec![Expression::Literal(Literal::Integer(4), Span::new(5, 6))];
        let output = generator
            .generate_binary_op("<", &left, &right)
            .unwrap()
            .to_pretty_string();
        assert!(
            output.contains(&format!("call 'erlang':'{erl}'(")) && !output.contains("is_object"),
            "literal receiver must skip the comparison guard; got: {output}"
        );
    }
}

#[test]
fn test_comparison_op_maps_to_erlang_le() {
    // `<=` maps to Erlang `=<` on the bare fast path.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(4), Span::new(5, 6))];
    let output = generator
        .generate_binary_op("<=", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert_eq!(output, "call 'erlang':'=<'(3, 4)");
}

#[test]
fn test_comparison_bare_bif_for_self_in_comparable_class() {
    use crate::ast::Identifier;
    for class in ["Integer", "Float", "Character", "String"] {
        let mut generator = CoreErlangGenerator::new("test");
        generator.set_class_identity(Some(util::ClassIdentity::new(class)));
        let left = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
        let right = vec![Expression::Identifier(Identifier::new(
            "Other",
            Span::new(7, 12),
        ))];
        let output = generator
            .generate_binary_op(">", &left, &right)
            .unwrap()
            .to_pretty_string();
        assert!(
            output.contains("call 'erlang':'>'(") && !output.contains("is_object"),
            "`self > x` in {class} must be a bare BIF; got: {output}"
        );
    }
}

#[test]
fn test_comparison_bare_bif_for_comparable_param() {
    use crate::ast::Identifier;
    for ty in ["Integer", "Float", "Number", "Character", "String"] {
        let mut generator = CoreErlangGenerator::new("test");
        generator
            .current_method_param_types
            .insert("other".to_string(), ty.to_string());
        let left = Expression::Identifier(Identifier::new("other", Span::new(0, 5)));
        let right = vec![Expression::Literal(Literal::Integer(1), Span::new(8, 9))];
        let output = generator
            .generate_binary_op(">=", &left, &right)
            .unwrap()
            .to_pretty_string();
        assert!(
            output.contains("call 'erlang':'>='(") && !output.contains("is_object"),
            "`other >= 1` with other :: {ty} must be a bare BIF; got: {output}"
        );
    }
}

#[test]
fn test_comparison_guard_for_unknown_receiver() {
    use crate::ast::Identifier;
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let right = vec![Expression::Identifier(Identifier::new(
        "y",
        Span::new(4, 5),
    ))];
    let output = generator
        .generate_binary_op("<", &left, &right)
        .unwrap()
        .to_pretty_string();
    // Inverted guard (BT-2710): is_object → dispatch, else bare comparison BIF.
    assert!(
        output.contains("call 'beamtalk_primitive':'is_object'("),
        "unknown comparison receiver must emit an is_object guard; got: {output}"
    );
    assert!(
        !output.contains("is_number"),
        "comparison guard must NOT use is_number; got: {output}"
    );
    assert!(
        output.contains("call 'beamtalk_message_dispatch':'send'(") && output.contains("'<'"),
        "guard must dispatch '<' as a message on the object arm; got: {output}"
    );
    assert!(
        output.contains("call 'erlang':'<'("),
        "guard must keep a bare comparison BIF on the primitive arm; got: {output}"
    );
    assert!(
        !output.contains("try "),
        "guard must not use try/catch; got: {output}"
    );
}

#[test]
fn test_comparison_guard_for_self_in_non_comparable_class() {
    use crate::ast::Identifier;
    // A user value-type overloading `<`: `self < other` must NOT be a bare BIF.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Money")));
    let left = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
    let right = vec![Expression::Identifier(Identifier::new(
        "Other",
        Span::new(7, 12),
    ))];
    let output = generator
        .generate_binary_op("<", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("call 'beamtalk_primitive':'is_object'("),
        "`self < x` in a non-comparable class must be guarded; got: {output}"
    );
}

// BT-2710 follow-up: `self.<field>` operator fast path is type-aware. A field
// with an explicit non-primitive (object) declared type routes through the
// runtime guard so it dispatches (silent-wrong-result fix for comparison;
// badarith fix for arithmetic); numeric/primitive and untyped fields stay bare.

/// Builds a `self.<field>` access expression for the field fast-path tests.
fn self_field_access(field: &str) -> Expression {
    use crate::ast::Identifier;
    let end = 5 + u32::try_from(field.len()).expect("short field name");
    Expression::FieldAccess {
        receiver: Box::new(Expression::Identifier(Identifier::new(
            "self",
            Span::new(0, 4),
        ))),
        field: Identifier::new(field, Span::new(5, end)),
        span: Span::new(0, end),
    }
}

#[test]
fn test_comparison_object_typed_field_is_guarded() {
    use crate::ast::Identifier;
    // `self.lo < y` where `lo :: Money` must dispatch (is_object guard), not
    // silently term-order the tagged map.
    let mut generator = CoreErlangGenerator::new("test");
    generator
        .current_class_field_types
        .insert("lo".to_string(), "Money".to_string());
    let left = self_field_access("lo");
    let right = vec![Expression::Identifier(Identifier::new(
        "y",
        Span::new(10, 11),
    ))];
    let output = generator
        .generate_binary_op("<", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("call 'beamtalk_primitive':'is_object'("),
        "object-typed self.<field> comparison must be guarded; got: {output}"
    );
}

#[test]
fn test_comparison_primitive_and_untyped_field_stay_bare() {
    // Integer-typed and untyped fields keep the bare comparison BIF (status quo;
    // preserves counter/accumulator hot paths and their state-threading).
    for (field_name, ty) in [("count", Some("Integer")), ("x", None)] {
        let mut generator = CoreErlangGenerator::new("test");
        if let Some(t) = ty {
            generator
                .current_class_field_types
                .insert(field_name.to_string(), t.to_string());
        }
        let left = self_field_access(field_name);
        let right = vec![Expression::Literal(Literal::Integer(0), Span::new(10, 11))];
        let output = generator
            .generate_binary_op("<", &left, &right)
            .unwrap()
            .to_pretty_string();
        assert!(
            output.contains("call 'erlang':'<'(") && !output.contains("is_object"),
            "field {field_name} ({ty:?}) comparison must stay bare; got: {output}"
        );
    }
}

#[test]
fn test_arith_object_typed_field_is_guarded() {
    use crate::ast::Identifier;
    // Arithmetic analog: `self.lo + y` where `lo :: Money` routes through the
    // is_number guard so it dispatches instead of badarith-ing.
    let mut generator = CoreErlangGenerator::new("test");
    generator
        .current_class_field_types
        .insert("lo".to_string(), "Money".to_string());
    let left = self_field_access("lo");
    let right = vec![Expression::Identifier(Identifier::new(
        "y",
        Span::new(10, 11),
    ))];
    let output = generator
        .generate_binary_op("+", &left, &right)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("call 'erlang':'is_number'("),
        "object-typed self.<field> arithmetic must be guarded; got: {output}"
    );
}

#[test]
fn test_arith_numeric_and_untyped_field_stay_bare() {
    // Numeric-typed and untyped fields keep the bare arithmetic BIF — no
    // regression to the `self.count := self.count + 1` counter pattern.
    for (field_name, ty) in [("count", Some("Integer")), ("x", None)] {
        let mut generator = CoreErlangGenerator::new("test");
        if let Some(t) = ty {
            generator
                .current_class_field_types
                .insert(field_name.to_string(), t.to_string());
        }
        let left = self_field_access(field_name);
        let right = vec![Expression::Literal(Literal::Integer(1), Span::new(10, 11))];
        let output = generator
            .generate_binary_op("+", &left, &right)
            .unwrap()
            .to_pretty_string();
        assert!(
            output.contains("call 'erlang':'+'(") && !output.contains("is_number"),
            "field {field_name} ({ty:?}) arithmetic must stay bare; got: {output}"
        );
    }
}

#[test]
fn test_generate_power_op_no_wrap() {
    use crate::ast::Identifier;
    let mut generator = CoreErlangGenerator::new("test");
    // Literal operands: no maybe_await wrapping.
    let left = Expression::Literal(Literal::Integer(2), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(8), Span::new(4, 5))];
    let doc = generator.generate_binary_op("**", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        !output.contains("maybe_await"),
        "literal operands in ** should not be wrapped; got: {output}"
    );

    // Non-literal operands: also no maybe_await (ADR-0043: all sends are sync).
    let left_var = Expression::Identifier(Identifier {
        name: "base".into(),
        span: Span::new(0, 4),
    });
    let right_lit = vec![Expression::Literal(Literal::Integer(2), Span::new(7, 8))];
    let doc2 = generator
        .generate_binary_op("**", &left_var, &right_lit)
        .unwrap();
    let output2 = doc2.to_pretty_string();
    assert!(
        !output2.contains("maybe_await"),
        "no operand in ** should be wrapped with maybe_await; got: {output2}"
    );
}

#[test]
fn test_generate_concat_op_no_wrap() {
    use crate::ast::Identifier;
    let mut generator = CoreErlangGenerator::new("test");
    // String literal ++ string literal: no maybe_await wrapping.
    let left = Expression::Literal(Literal::String("hello".into()), Span::new(0, 7));
    let right = vec![Expression::Literal(
        Literal::String(" world".into()),
        Span::new(11, 19),
    )];
    let doc = generator.generate_binary_op("++", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        !output.contains("maybe_await"),
        "literal string operands in ++ should not be wrapped; got: {output}"
    );

    // Non-literal right operand: also no maybe_await (ADR-0043: all sends are sync).
    let left_lit = Expression::Literal(Literal::String("hi".into()), Span::new(0, 4));
    let right_var = vec![Expression::Identifier(Identifier {
        name: "suffix".into(),
        span: Span::new(8, 14),
    })];
    let doc2 = generator
        .generate_binary_op("++", &left_lit, &right_var)
        .unwrap();
    let output2 = doc2.to_pretty_string();
    assert!(
        !output2.contains("maybe_await"),
        "no operand in ++ should be wrapped with maybe_await; got: {output2}"
    );
}

#[test]
fn test_generate_string_concatenation_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::String("Hello".into()), Span::new(0, 7));
    let right = vec![Expression::Literal(
        Literal::String(" World".into()),
        Span::new(11, 19),
    )];
    let doc = generator.generate_binary_op("++", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("iolist_to_binary"),
        "Should use iolist_to_binary for string concatenation. Got: {output}",
    );
    assert!(
        output.contains("binary_to_list"),
        "Should convert binaries to lists first. Got: {output}",
    );
}

#[test]
fn test_generate_strict_equality_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
    let right = vec![Expression::Literal(Literal::Integer(42), Span::new(6, 8))];
    let doc = generator.generate_binary_op("=:=", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'erlang':'=:='"),
        "Should use strict equality =:=. Got: {output}",
    );
}

#[test]
fn test_generate_loose_inequality_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
    let right = vec![Expression::Literal(Literal::Integer(99), Span::new(7, 9))];
    let doc = generator.generate_binary_op("/=", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'erlang':'/='"),
        "Should use loose inequality /= (negation of ==). Got: {output}",
    );
}

#[test]
fn test_generate_loose_equality_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(5), Span::new(6, 7))];
    let doc = generator.generate_binary_op("==", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'erlang':'=='"),
        "Should use loose equality ==. Got: {output}",
    );
    // Literal operands are provably non-future: no maybe_await wrapping needed.
    assert_eq!(output, "call 'erlang':'=='(5, 5)");
}

// ── Binary-operator error paths ────────────────────────────────────────────

#[test]
fn test_binary_op_wrong_argument_count_returns_error() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let result = generator.generate_binary_op("+", &left, &[]);
    assert!(
        result.is_err(),
        "generate_binary_op with 0 arguments must return an error"
    );
}

#[test]
fn test_binary_op_unsupported_operator_returns_error() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(2), Span::new(4, 5))];
    let result = generator.generate_binary_op("@", &left, &right);
    assert!(
        result.is_err(),
        "generate_binary_op with unsupported operator '@' must return an error"
    );
}

// ── Concat-op dispatch branches ────────────────────────────────────────────

#[test]
fn test_concat_op_list_literal_uses_list_append() {
    // Left operand is a list literal → is_list = true → should emit erlang:'++'.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(
        Literal::List(vec![Literal::Integer(1), Literal::Integer(2)]),
        Span::new(0, 6),
    );
    let right = vec![Expression::Literal(
        Literal::List(vec![Literal::Integer(3)]),
        Span::new(10, 13),
    )];
    let doc = generator.generate_binary_op("++", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'erlang':'++'"),
        "List ++ should use 'erlang':'++', got: {output}"
    );
    assert!(
        !output.contains("iolist_to_binary"),
        "List ++ must not use iolist_to_binary, got: {output}"
    );
}

#[test]
fn test_concat_op_identifier_left_uses_runtime_dispatch() {
    // Left operand is an identifier (type unknown at compile time) →
    // should emit a runtime is_list check that dispatches to either
    // erlang:'++' or iolist_to_binary depending on the runtime value.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Identifier(Identifier {
        name: "xs".into(),
        span: Span::new(0, 2),
    });
    let right = vec![Expression::Identifier(Identifier {
        name: "ys".into(),
        span: Span::new(6, 8),
    })];
    let doc = generator.generate_binary_op("++", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("is_list"),
        "Non-literal ++ should emit a runtime is_list type check, got: {output}"
    );
    assert!(
        output.contains("'erlang':'++'"),
        "Runtime dispatch must include the list path 'erlang':'++', got: {output}"
    );
    assert!(
        output.contains("iolist_to_binary"),
        "Runtime dispatch must include the binary path iolist_to_binary, got: {output}"
    );
}

#[test]
fn test_fresh_var_generation() {
    let mut generator = CoreErlangGenerator::new("test");
    let var1 = generator.fresh_var("temp");
    let var2 = generator.fresh_var("temp");
    assert_ne!(var1, var2);
    assert!(var1.starts_with("_temp"));
    assert!(var2.starts_with("_temp"));
}

#[test]
fn test_class_name_derived_from_module() {
    let generator = CoreErlangGenerator::new("my_counter_actor");
    assert_eq!(generator.class_name(), "MyCounterActor");

    let generator = CoreErlangGenerator::new("simple");
    assert_eq!(generator.class_name(), "Simple");
}

#[test]
fn test_class_name_from_identity_overrides_module() {
    let mut generator = CoreErlangGenerator::new("bt@stdlib@string");
    generator.set_class_identity(Some(util::ClassIdentity::new("String")));
    assert_eq!(generator.class_name(), "String");
}

#[test]
fn test_class_name_to_module_name() {
    // Single word
    assert_eq!(util::to_module_name("Counter"), "counter");

    // Multi-word CamelCase
    assert_eq!(util::to_module_name("MyCounterActor"), "my_counter_actor");

    // With acronyms
    assert_eq!(util::to_module_name("HTTPRouter"), "httprouter");

    // Mixed case
    assert_eq!(util::to_module_name("HTTPSConnection"), "httpsconnection");
}

#[test]
fn test_generated_core_erlang_compiles() {
    // Test a self-contained Core Erlang module to verify syntax is valid
    // This specifically tests that:
    // 1. Empty map syntax ~{}~ compiles correctly
    // 2. The overall Core Erlang structure is valid
    // Full gen_server integration is tested in integration tests
    let core_erlang = r"module 'test_module' ['get_methods'/0, 'simple_fun'/0]
  attributes []

'get_methods'/0 = fun () ->
    ~{}~

'simple_fun'/0 = fun () ->
    42

end
";

    crate::test_helpers::assert_compiles_through_erlc("test_module", core_erlang);
}

#[test]
fn test_string_literal_core_erlang_compiles() {
    // Test that string literals compile correctly through the full pipeline
    // This tests the new binary syntax: #{#<value>(8,1,'integer',['unsigned'|['big']]),...}#
    let core_erlang = r"module 'test_string' ['get_greeting'/0]
  attributes []

'get_greeting'/0 = fun () ->
    #{#<104>(8,1,'integer',['unsigned'|['big']]),#<105>(8,1,'integer',['unsigned'|['big']])}#

end
";

    crate::test_helpers::assert_compiles_through_erlc("test_string", core_erlang);
}

#[test]
fn test_temp_vars_dont_shadow_user_identifiers() {
    let mut generator = CoreErlangGenerator::new("test");

    // First, generate a whileTrue: which creates internal _Loop, _Cond, _Body temps
    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::Identifier(Identifier::new(
            "false",
            Span::new(1, 6),
        )))],
        Span::new(0, 7),
    );
    let body_block = Block::new(vec![], vec![], Span::new(8, 10));

    let receiver = Expression::Block(condition_block);
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(7, 17))]);
    let arguments = vec![Expression::Block(body_block)];

    let _doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();

    // generate_message_send registers temp vars as a side effect;
    // the document output is discarded since we only need the scope state.

    // Now push a scope with a user variable named "Loop"
    generator.push_scope();
    let user_loop_var = generator.fresh_var("Loop");

    // Access the "Loop" identifier - it should resolve to the user's binding
    let loop_id = Identifier::new("Loop", Span::new(0, 4));
    let doc = generator.generate_identifier(&loop_id).unwrap();
    let output = doc.to_pretty_string();

    // The identifier should resolve to the user's variable, not an internal temp
    assert!(
        output.contains(&user_loop_var),
        "User identifier 'Loop' should resolve to user's binding {user_loop_var}, got: {output}"
    );

    generator.pop_scope();
}

#[test]
fn test_generate_empty_map_literal() {
    let mut generator = CoreErlangGenerator::new("test");
    let pairs = vec![];
    let doc = generator.generate_map_literal(&pairs).unwrap();
    assert_eq!(doc.to_pretty_string().trim(), "~{}~");
}

#[test]
fn test_generate_map_literal_with_atoms() {
    let mut generator = CoreErlangGenerator::new("test");

    let pairs = vec![
        MapPair::new(
            Expression::Literal(Literal::Symbol("name".into()), Span::new(2, 7)),
            Expression::Literal(Literal::String("Alice".into()), Span::new(11, 18)),
            Span::new(2, 18),
        ),
        MapPair::new(
            Expression::Literal(Literal::Symbol("age".into()), Span::new(20, 24)),
            Expression::Literal(Literal::Integer(30), Span::new(28, 30)),
            Span::new(20, 30),
        ),
    ];

    let doc = generator.generate_map_literal(&pairs).unwrap();
    let output = doc.to_pretty_string();
    // Symbols become atoms in Core Erlang
    assert!(
        output.contains("'name'"),
        "Output should contain 'name': {output}",
    );
    // Strings are represented as binaries with character codes
    assert!(
        output.contains("#<65>"),
        "Output should contain character code for 'A': {output}",
    );
    assert!(
        output.contains("'age'"),
        "Output should contain 'age': {output}",
    );
    assert!(output.contains("30"), "Output should contain 30: {output}",);
}

#[test]
fn test_generate_map_literal_compiles() {
    // BT-3340: uses the shared `codegen()` class-method helper (workspace
    // mode) rather than `generate_repl_expression` — map-literal Core
    // Erlang representation doesn't depend on REPL vs class-method context,
    // and this keeps the test (which needs the `#[cfg(test)]`-only
    // `assert_compiles_through_erlc`, a dev-dependency-gated helper not
    // reachable from the standalone `beamtalk-repl` crate) inside
    // `beamtalk-core` rather than needing a crate boundary crossing.
    let code = codegen("Object subclass: T\n  m => #{#key => \"value\"}");

    // Verify the generated Core Erlang contains the map literal syntax
    assert!(
        code.contains("~{"),
        "Should contain Core Erlang map syntax ~{{"
    );
    // Symbols become atoms in Core Erlang
    assert!(code.contains("'key'"));
    // Strings are represented as binaries with character codes
    assert!(
        code.contains("#<"),
        "String should be represented as binary"
    );

    // Verify the generated code compiles through erlc. Module name must
    // match `codegen()`'s hardcoded module name ("test"), not an arbitrary
    // label — `erlc` rejects a mismatch between the `-module` attribute and
    // the source file name it derives from this argument.
    crate::test_helpers::assert_compiles_through_erlc("test", &code);
}

#[test]
fn test_dictionary_at_on_identifier() {
    // BT-296: Dictionary methods now go through runtime dispatch
    // BT-430: person at: #name -> beamtalk_message_dispatch:send(person, 'at:', ['name'])
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();
    generator.bind_var("person", "Person");

    let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("at:", Span::new(7, 10))]);
    let arguments = vec![Expression::Literal(
        Literal::Symbol("name".into()),
        Span::new(11, 16),
    )];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    // BT-430: Unified dispatch
    assert!(
        output.contains("beamtalk_message_dispatch") && output.contains("'at:'"),
        "Should generate unified dispatch for at:. Got: {output}"
    );
}

#[test]
fn test_dictionary_at_put_on_identifier() {
    // BT-296: Dictionary methods now go through runtime dispatch
    // BT-430: person at: #age put: 31 -> beamtalk_message_dispatch:send(person, 'at:put:', ['age', 31])
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();
    generator.bind_var("person", "Person");

    let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("at:", Span::new(7, 10)),
        KeywordPart::new("put:", Span::new(14, 18)),
    ]);
    let arguments = vec![
        Expression::Literal(Literal::Symbol("age".into()), Span::new(11, 14)),
        Expression::Literal(Literal::Integer(31), Span::new(19, 21)),
    ];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("beamtalk_message_dispatch") && output.contains("'at:put:'"),
        "Should generate unified dispatch for at:put:. Got: {output}"
    );
}

#[test]
fn test_dictionary_size_on_identifier() {
    // BT-296: Dictionary methods now go through runtime dispatch
    // BT-430: person size -> beamtalk_message_dispatch:send(person, 'size', [])
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();
    generator.bind_var("person", "Person");

    let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
    let selector = MessageSelector::Unary("size".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("beamtalk_message_dispatch") && output.contains("'size'"),
        "Should generate unified dispatch for size. Got: {output}"
    );
}

#[test]
fn test_as_type_erasure() {
    // `x asType: Integer` should erase to just the receiver `x`
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("asType:", Span::new(2, 9))]);
    let arguments = vec![Expression::ClassReference {
        name: Identifier::new("Integer", Span::new(10, 17)),
        span: Span::new(10, 17),
        package: None,
    }];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    // asType: should be erased — no dispatch, no beamtalk_message_dispatch
    assert!(
        !output.contains("beamtalk_message_dispatch"),
        "asType: should be erased, not dispatched. Got: {output}"
    );
    assert!(
        !output.contains("asType"),
        "asType: should not appear in generated code. Got: {output}"
    );
    // The output should just be the variable reference
    assert!(
        output.contains('x') || output.contains("_x"),
        "asType: should generate only the receiver expression. Got: {output}"
    );
}

#[test]
fn test_binary_pattern_integer_segment() {
    // <<version:8>> — single integer segment, default signedness/endianness
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("version", Span::new(0, 7))),
        size: Some(Box::new(Expression::Literal(
            Literal::Integer(8),
            Span::new(8, 9),
        ))),
        segment_type: Some(BinarySegmentType::Integer),
        signedness: None,
        endianness: None,
        unit: None,
        span: Span::new(0, 9),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    // Must contain: #<Version>(8,1,'integer',['unsigned'|['big']])
    assert!(
        output.contains("#<"),
        "Expected #< in binary segment, got: {output}"
    );
    assert!(
        output.contains("'integer'"),
        "Expected 'integer' type, got: {output}"
    );
    assert!(
        output.contains("'unsigned'"),
        "Expected 'unsigned' signedness, got: {output}"
    );
    assert!(
        output.contains("'big'"),
        "Expected 'big' endianness, got: {output}"
    );
}

#[test]
fn test_binary_pattern_binary_segment() {
    // <<rest/binary>> — binary/rest segment with 'all' size and unit 8
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("rest", Span::new(0, 4))),
        size: None,
        segment_type: Some(BinarySegmentType::Binary),
        signedness: None,
        endianness: None,
        unit: None,
        span: Span::new(0, 4),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'all'"),
        "Expected 'all' size, got: {output}"
    );
    assert!(
        output.contains("'binary'"),
        "Expected 'binary' type, got: {output}"
    );
}

#[test]
fn test_binary_pattern_utf8_segment() {
    // <<codepoint/utf8>> — UTF-8 segment with 'undefined' size and unit
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("cp", Span::new(0, 2))),
        size: None,
        segment_type: Some(BinarySegmentType::Utf8),
        signedness: None,
        endianness: None,
        unit: None,
        span: Span::new(0, 2),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'undefined'"),
        "UTF-8 segment should have 'undefined' size, got: {output}"
    );
    assert!(
        output.contains("'utf8'"),
        "Expected 'utf8' type, got: {output}"
    );
}

#[test]
fn test_binary_pattern_signed_little_segment() {
    // <<val:16/signed-little>> — signed little-endian integer
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("val", Span::new(0, 3))),
        size: Some(Box::new(Expression::Literal(
            Literal::Integer(16),
            Span::new(4, 6),
        ))),
        segment_type: Some(BinarySegmentType::Integer),
        signedness: Some(BinarySignedness::Signed),
        endianness: Some(BinaryEndianness::Little),
        unit: None,
        span: Span::new(0, 6),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'signed'"),
        "Expected 'signed', got: {output}"
    );
    assert!(
        output.contains("'little'"),
        "Expected 'little', got: {output}"
    );
}

#[test]
fn test_binary_pattern_whole_pattern() {
    // <<version:8, rest/binary>> — full binary pattern
    let mut generator = CoreErlangGenerator::new("test");
    let pattern = Pattern::Binary {
        segments: vec![
            BinarySegment {
                value: Pattern::Variable(Identifier::new("version", Span::new(0, 7))),
                size: Some(Box::new(Expression::Literal(
                    Literal::Integer(8),
                    Span::new(8, 9),
                ))),
                segment_type: Some(BinarySegmentType::Integer),
                signedness: None,
                endianness: None,
                unit: None,
                span: Span::new(0, 9),
            },
            BinarySegment {
                value: Pattern::Variable(Identifier::new("rest", Span::new(11, 15))),
                size: None,
                segment_type: Some(BinarySegmentType::Binary),
                signedness: None,
                endianness: None,
                unit: None,
                span: Span::new(11, 15),
            },
        ],
        span: Span::new(0, 17),
    };
    let doc = generator.generate_pattern(&pattern).unwrap();
    let output = doc.to_pretty_string();
    // Should wrap in #{...}#
    assert!(
        output.starts_with("#{"),
        "Expected #{{ prefix, got: {output}"
    );
    assert!(output.ends_with("}#"), "Expected }}# suffix, got: {output}");
    // Should contain both segments
    assert!(
        output.contains("'integer'"),
        "Expected integer segment, got: {output}"
    );
    assert!(
        output.contains("'binary'"),
        "Expected binary segment, got: {output}"
    );
}

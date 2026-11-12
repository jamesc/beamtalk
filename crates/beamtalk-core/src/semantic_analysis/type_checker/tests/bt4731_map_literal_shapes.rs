// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4731): map literals whose keys are all symbol literals infer
//! closed shapes; every other literal stays `Dictionary(K, V)`.

use super::common::*;
use crate::ast::MapPair;

fn pair(key: Expression, value: Expression) -> MapPair {
    MapPair {
        key,
        value,
        span: span(),
    }
}

fn map(pairs: Vec<MapPair>) -> Expression {
    Expression::MapLiteral {
        pairs,
        span: span(),
    }
}

fn infer(expr: &Expression) -> InferredType {
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    checker.infer_expr(expr, &hierarchy, &mut env, false)
}

fn field(key: &str, value: InferredType) -> ShapeFieldType {
    ShapeFieldType {
        key: key.into(),
        value,
        optional: false,
    }
}

#[test]
fn symbol_keyed_literal_infers_a_closed_shape() {
    let ty = infer(&map(vec![
        pair(symbol_lit("body"), str_lit("{}")),
        pair(symbol_lit("timeout"), int_lit(5000)),
    ]));
    assert_eq!(
        ty,
        InferredType::shape(
            vec![
                field("body", InferredType::known("String")),
                field("timeout", InferredType::known("Integer")),
            ],
            false
        )
    );
    assert_eq!(
        ty.display_for_diagnostic().unwrap(),
        "#{#body => String, #timeout => Integer}"
    );
}

#[test]
fn nested_literals_infer_nested_shapes() {
    let ty = infer(&map(vec![pair(
        symbol_lit("inner"),
        map(vec![pair(symbol_lit("n"), int_lit(1))]),
    )]));
    let inner = InferredType::shape(vec![field("n", InferredType::known("Integer"))], false);
    assert_eq!(ty, InferredType::shape(vec![field("inner", inner)], false));
}

#[test]
fn empty_literal_stays_the_gradual_dictionary() {
    let d = InferredType::Dynamic(DynamicReason::Unknown);
    assert_eq!(
        infer(&map(vec![])),
        InferredType::known_with_args("Dictionary", vec![d.clone(), d])
    );
}

#[test]
fn accumulator_idiom_keeps_inferring_cleanly() {
    let msgs = warnings(
        "Object subclass: Probe\n  \
           build: ks :: List(Symbol) -> Dictionary =>\n    \
             ks inject: #{} into: [:acc :k | acc at: k put: 1]\n",
    );
    assert!(msgs.is_empty(), "got: {msgs:#?}");
}

#[test]
fn other_literals_stay_dictionary() {
    let integer = InferredType::known("Integer");
    let string = InferredType::known("String");
    // Non-symbol key, a mix of symbol and non-symbol keys, a computed key,
    // and a repeated symbol key.
    let cases = [
        map(vec![pair(int_lit(1), str_lit("a"))]),
        map(vec![
            pair(symbol_lit("a"), int_lit(1)),
            pair(str_lit("b"), int_lit(2)),
        ]),
        map(vec![pair(var("k"), int_lit(1))]),
        map(vec![
            pair(symbol_lit("a"), int_lit(1)),
            pair(symbol_lit("a"), str_lit("x")),
        ]),
    ];
    for expr in &cases {
        let ty = infer(expr);
        assert!(
            matches!(&ty, InferredType::Known { class_name, .. } if class_name == "Dictionary"),
            "expected Dictionary, got {ty:?}"
        );
    }
    let ty = infer(&cases[0]);
    assert_eq!(
        ty,
        InferredType::known_with_args("Dictionary", vec![integer, string])
    );
}

/// The `Dictionary(K, V)` the literal inferred before shapes: the separate
/// joins of the key and value types.
fn legacy_dictionary(pairs: &[(&str, InferredType)]) -> InferredType {
    let keys: Vec<_> = pairs
        .iter()
        .map(|(k, _)| InferredType::known(format!("#{k}")))
        .collect();
    let values: Vec<_> = pairs.iter().map(|(_, v)| v.clone()).collect();
    InferredType::known_with_args(
        "Dictionary",
        vec![
            InferredType::union_of(&keys),
            InferredType::union_of(&values),
        ],
    )
}

#[test]
fn dictionary_view_equals_the_pre_shape_inference() {
    let int = || InferredType::known("Integer");
    let string = || InferredType::known("String");
    let samples: Vec<(Expression, Vec<(&str, InferredType)>)> = vec![
        (
            map(vec![pair(symbol_lit("a"), int_lit(1))]),
            vec![("a", int())],
        ),
        (
            map(vec![
                pair(symbol_lit("a"), int_lit(1)),
                pair(symbol_lit("b"), str_lit("x")),
            ]),
            vec![("a", int()), ("b", string())],
        ),
        (
            map(vec![
                pair(symbol_lit("a"), int_lit(1)),
                pair(symbol_lit("b"), int_lit(2)),
            ]),
            vec![("a", int()), ("b", int())],
        ),
    ];
    for (expr, legacy) in &samples {
        let ty = infer(expr);
        assert_eq!(
            ty.dictionary_view().expect("a shape"),
            legacy_dictionary(legacy),
            "{ty:?}"
        );
    }
}

fn warnings(source: &str) -> Vec<(String, String)> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| matches!(d.category, Some(DiagnosticCategory::Type)))
        .map(|d| {
            (
                d.message.to_string(),
                d.hint.map(|h| h.to_string()).unwrap_or_default(),
            )
        })
        .collect()
}

#[test]
fn matching_literal_is_accepted_directly_and_through_a_local() {
    let msgs = warnings(
        "Object subclass: Probe\n  \
           take: o :: #{#port => Integer, #host => String} => o\n  \
           direct => self take: #{#port => 1, #host => \"h\"}\n  \
           viaLocal =>\n    opts := #{#port => 1, #host => \"h\"}\n    self take: opts\n",
    );
    assert!(msgs.is_empty(), "got: {msgs:#?}");
}

#[test]
fn missing_key_in_literal_is_named() {
    let msgs = warnings(
        "Object subclass: Probe\n  \
           take: o :: #{#port => Integer, #host => String} => o\n  \
           direct => self take: #{#port => 1}\n",
    );
    assert_eq!(msgs.len(), 1, "got: {msgs:#?}");
    let (m, h) = &msgs[0];
    assert!(
        m.contains("#host") || h.contains("`#host`"),
        "key not named: {m} / {h}"
    );
}

#[test]
fn wrongly_typed_value_via_unannotated_local_is_named() {
    let msgs = warnings(
        "Object subclass: Probe\n  \
           take: o :: #{#port => Integer} => o\n  \
           viaLocal =>\n    opts := #{#port => \"x\"}\n    self take: opts\n",
    );
    assert_eq!(msgs.len(), 1, "got: {msgs:#?}");
    assert!(msgs[0].1.contains("`#port`"), "{:?}", msgs[0]);
}

#[test]
fn unexpected_key_in_closed_target_suggests_the_near_miss() {
    let msgs = warnings(
        "Object subclass: Probe\n  \
           take: o :: #{?#timeout => Integer} => o\n  \
           direct => self take: #{#timeot => 1}\n",
    );
    assert_eq!(msgs.len(), 1, "got: {msgs:#?}");
    assert!(
        msgs[0].1.contains("did you mean `#timeout`"),
        "{:?}",
        msgs[0]
    );
}

#[test]
fn literal_is_checked_against_annotated_local_field_default_and_return() {
    let msgs = warnings(
        "Value subclass: Probe\n  \
           field: cfg :: #{#port => Integer} = #{#port => \"x\"}\n  \
           local =>\n    o :: #{#port => Integer} := #{#host => 1}\n    o\n  \
           ret -> #{#port => Integer} => #{#port => \"x\"}\n",
    );
    assert_eq!(msgs.len(), 3, "got: {msgs:#?}");
    // Each one names the offending key.
    let keyed = |k: &str| msgs.iter().filter(|(_, h)| h.contains(k)).count();
    assert_eq!(keyed("`#port`"), 3, "{msgs:#?}");
    assert_eq!(keyed("missing required key `#port`"), 1, "{msgs:#?}");
}

#[test]
fn literal_flows_into_dictionary_slots_via_its_view() {
    let msgs = warnings(
        "Object subclass: Probe\n  \
           take: d :: Dictionary(Symbol, Integer) => d\n  \
           takeAny: d :: Dictionary => d\n  \
           ok => self take: #{#a => 1, #b => 2}\n  \
           any => self takeAny: #{#a => 1, #b => \"x\"}\n  \
           empty => self take: #{}\n",
    );
    assert!(msgs.is_empty(), "got: {msgs:#?}");
}

#[test]
fn build_then_pass_idiom_is_silent_now_keyed_ops_are_shape_aware() {
    // BT-4732 made `at:put:` on a shape answer a shape, so a value derived
    // from a literal still satisfies a shape slot.
    let msgs = warnings(
        "Object subclass: Probe\n  \
           take: o :: #{#port => Integer, #host => Integer} => o\n  \
           go =>\n    o := #{#port => 1}\n    o := o at: #host put: 2\n    self take: o\n",
    );
    assert!(msgs.is_empty(), "got: {msgs:#?}");
}

#[test]
fn annotated_local_accepts_a_literal_by_structure() {
    let msgs = warnings(
        "Object subclass: Probe\n  \
           bare =>\n    d :: Dictionary := #{#a => 1}\n    d\n  \
           typed =>\n    d :: Dictionary(Symbol, Integer) := #{#a => 1}\n    d\n  \
           open =>\n    d :: #{#a => Integer, ...} := #{#a => 1, #b => \"x\"}\n    d\n  \
           optional =>\n    d :: #{#a => Integer, ?#b => String} := #{#a => 1}\n    d\n",
    );
    assert!(msgs.is_empty(), "got: {msgs:#?}");
}

#[test]
fn annotated_local_is_the_narrowing_cast_into_a_shape() {
    // ADR 0147 § `Dictionary` to shape: a typed `Dictionary` or `Object`
    // value is asserted into a shape by annotating a local.
    let msgs = warnings(
        "Object subclass: Probe\n  \
           fromDict: d :: Dictionary(Symbol, Object) =>\n    x :: #{#a => Integer} := d\n    x\n  \
           fromObject: o :: Object =>\n    x :: #{#a => Integer} := o\n    x\n",
    );
    assert!(msgs.is_empty(), "got: {msgs:#?}");
}

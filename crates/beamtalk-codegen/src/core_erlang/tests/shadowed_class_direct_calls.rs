// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4271: a send to a class *reference* never pins a compile-time module
//! when the name is shadowed (declared by two or more packages in scope,
//! BT-4151's `shadowed_class_names`). Such a name is one last-wins runtime
//! class, so the compile-time package's module is not necessarily the one a
//! dynamic send reaches. Covered here: `class sealed` class-method direct
//! calls (ADR 0129 Phase 0b), keyword constructors and nullary `new`
//! (BT-4099). Each negative has an unshadowed positive control so it cannot
//! pass vacuously.

use super::super::method_frame::DIRECT_CLASS_SELF_PARAM;
use super::super::{CodegenOptions, CoreErlangGenerator, DirectCallClassInfo, generate_module};
use beamtalk_core::ast::{
    Expression, Identifier, KeywordPart, Literal, MessageSelector, MethodKind,
};
use beamtalk_core::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
use beamtalk_core::source_analysis::Span;

/// A sealed, stateless `Parser` declared by `package` with one
/// `class sealed parse:` class method: what `compute_direct_call_eligible`
/// makes direct-call eligible.
fn sealed_parser(package: Option<&str>) -> ClassInfo {
    ClassInfo {
        name: "Parser".into(),
        superclass: Some("Object".into()),
        is_sealed: true,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: package.map(Into::into),
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        surface_incomplete: false,
        is_module_less: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        state_kinds: std::collections::HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![MethodInfo {
            selector: "parse:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Parser".into(),
            is_sealed: true,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![None],
            doc: None,
            origin: None,
            primitive: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }
}

/// Compiles a `Caller` that sends `Parser parse: 1` against `ambient` (the
/// cross-file/dependency declarations a batch build hands codegen).
fn caller_with_scope(ambient: Vec<ClassInfo>) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt -> Object =>\n",
        "    Parser parse: 1\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    generate_module(
        &module,
        CodegenOptions::new("bt@app@caller").with_class_hierarchy(ambient),
    )
    .expect("codegen should succeed")
}

const DIRECT_PARSE: &str = ":'class_parse:'(";
const CLASS_SEND: &str = "'beamtalk_object_class':'class_send'";

#[test]
fn unshadowed_class_sealed_method_send_direct_calls() {
    // Positive control: one declaration of `Parser` in scope.
    let code = caller_with_scope(vec![sealed_parser(Some("json"))]);
    assert!(
        code.contains(DIRECT_PARSE),
        "an unshadowed `class sealed` class method should direct-call. Got:\n{code}"
    );
}

#[test]
fn class_sealed_method_send_to_class_exported_by_two_packages_stays_a_class_send() {
    let code = caller_with_scope(vec![
        sealed_parser(Some("json")),
        sealed_parser(Some("xml")),
    ]);
    assert!(
        !code.contains(DIRECT_PARSE),
        "a shadowed class's `class sealed` method must not pin either package's module. Got:\n{code}"
    );
    assert!(
        code.contains(CLASS_SEND),
        "the shadowed class-method send should be a dynamic class send. Got:\n{code}"
    );
}

#[test]
fn class_sealed_method_send_to_class_shadowing_a_package_less_class_stays_a_class_send() {
    // A workspace/single-file class collides with a package's class just
    // like two packages' classes do.
    let code = caller_with_scope(vec![sealed_parser(None), sealed_parser(Some("json"))]);
    assert!(
        !code.contains(DIRECT_PARSE) && code.contains(CLASS_SEND),
        "a class name shared with a package-less class must stay a class send. Got:\n{code}"
    );
}

// --- constructors (BT-4099) ---------------------------------------------
//
// The verdict is planted directly (as `tests/dispatch.rs` does for the
// unshadowed constructor tests) so these exercise the class-reference gate
// itself, independent of how a hierarchy derives constructor eligibility.

fn generator_with_direct_pt(shadowed: bool) -> CoreErlangGenerator {
    let mut generator = CoreErlangGenerator::new("bt@test@main");
    generator.direct_call_eligible.insert(
        "Pt".to_string(),
        DirectCallClassInfo {
            module_name: "bt@test@pt".into(),
            selectors: std::iter::once("x:y:".to_string()).collect(),
            direct_new0: true,
            direct_new1: true,
        },
    );
    if shadowed {
        generator.set_shadowed_class_names(std::iter::once("Pt".into()).collect());
    }
    generator
}

fn pt_reference() -> Expression {
    Expression::ClassReference {
        name: Identifier::new("Pt", Span::new(0, 2)),
        package: None,
        span: Span::new(0, 2),
    }
}

fn send_new(shadowed: bool) -> String {
    let mut generator = generator_with_direct_pt(shadowed);
    generator
        .generate_message_send(&pt_reference(), &MessageSelector::Unary("new".into()), &[])
        .expect("codegen should succeed")
        .to_pretty_string()
}

fn send_keyword_constructor(shadowed: bool) -> String {
    let mut generator = generator_with_direct_pt(shadowed);
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("x:", Span::new(3, 5)),
        KeywordPart::new("y:", Span::new(9, 11)),
    ]);
    let arguments = vec![
        Expression::Literal(Literal::Integer(1), Span::new(6, 7)),
        Expression::Literal(Literal::Integer(2), Span::new(12, 13)),
    ];
    generator
        .generate_message_send(&pt_reference(), &selector, &arguments)
        .expect("codegen should succeed")
        .to_pretty_string()
}

#[test]
fn nullary_new_on_shadowed_class_stays_a_class_send() {
    let control = send_new(false);
    assert!(
        control.contains("call 'bt@test@pt':'new'()"),
        "control: unshadowed `Pt new` should call the builder directly. Got:\n{control}"
    );
    let output = send_new(true);
    assert!(
        !output.contains(":'new'()") && output.contains(CLASS_SEND),
        "shadowed `Pt new` must stay a class send. Got:\n{output}"
    );
}

#[test]
fn keyword_constructor_on_shadowed_class_stays_a_class_send() {
    let control = send_keyword_constructor(false);
    assert!(
        control.contains("call 'bt@test@pt':'class_x:y:'("),
        "control: unshadowed `Pt x:y:` should direct-call. Got:\n{control}"
    );
    let output = send_keyword_constructor(true);
    assert!(
        !output.contains("'class_x:y:'(") && output.contains(CLASS_SEND),
        "shadowed `Pt x:y:` must stay a class send. Got:\n{output}"
    );
}

#[test]
fn qualified_reference_to_a_shadowed_class_stays_a_class_send() {
    // Class-method sends dispatch on the bare name whatever the qualifier
    // (ADR 0070 Phase 2), so `json@Parser parse:` is gated like `Parser`.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt -> Object =>\n",
        "    json@Parser parse: 1\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@app@caller").with_class_hierarchy(vec![
            sealed_parser(Some("json")),
            sealed_parser(Some("xml")),
        ]),
    )
    .expect("codegen should succeed");
    assert!(
        !code.contains(DIRECT_PARSE) && code.contains(CLASS_SEND),
        "a qualified send to a shadowed class must stay a class send. Got:\n{code}"
    );
}

#[test]
fn own_class_sibling_call_is_unaffected_by_shadowing() {
    // A self-send between two direct-called class methods of the class's own
    // module forwards the raw incoming class-self (`DIRECT_CLASS_SELF_PARAM`,
    // possibly `nil`; BT-4029), shadowed or not: it describes how this
    // module's own functions are entered, which shadowing does not change.
    // Forwarding the gated `ClassSelf` instead would read a variable the
    // direct-called method never bound.
    //
    // `probe` is an instance method, so its `Parser helper: 1` is a class
    // *reference* send that goes through the shadow gate: it proves `Parser`
    // really is shadowed in the rival compile (and not in the control).
    const SRC: &str = concat!(
        "sealed Object subclass: Parser\n",
        "  class sealed parse: x => self helper: x\n",
        "  class sealed helper: x => x\n",
        "  probe => Parser helper: 1\n",
    );
    let compile = |ambient: Vec<ClassInfo>| {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(SRC);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        generate_module(
            &module,
            CodegenOptions::new("bt@parser").with_class_hierarchy(ambient),
        )
        .expect("codegen should succeed")
    };
    let forwarded = format!("'class_helper:'({DIRECT_CLASS_SELF_PARAM}, ");
    let probe_direct = "call 'bt@parser':'class_helper:'('nil', 1)";

    // Control: no rival, so the class-reference send in `probe` direct-calls.
    let control = compile(vec![]);
    assert!(
        control.contains(probe_direct),
        "control: an unshadowed class-reference send should direct-call. Got:\n{control}"
    );
    assert!(
        control.contains(&forwarded),
        "control: the sibling call should forward `{DIRECT_CLASS_SELF_PARAM}`. Got:\n{control}"
    );

    // Rival: a dependency also declares `Parser`.
    let code = compile(vec![sealed_parser(Some("json"))]);
    assert!(
        !code.contains(probe_direct) && code.contains(CLASS_SEND),
        "precondition: `Parser` must be shadowed, so `probe`'s class-reference send \
         stays a class send. Got:\n{code}"
    );
    assert!(
        code.contains(&forwarded),
        "the own-class sibling call must still forward `{DIRECT_CLASS_SELF_PARAM}` under \
         shadowing. Got:\n{code}"
    );
}

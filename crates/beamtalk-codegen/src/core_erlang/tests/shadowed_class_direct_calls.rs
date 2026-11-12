// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4271: a send to a class *reference* never pins a compile-time module
//! when the name is shadowed (declared by two or more packages in scope,
//! BT-4151's `shadowed_class_names`). Such a name is one last-wins runtime
//! class, so the compile-time package's module is not necessarily the one a
//! dynamic send reaches. Covered here: guarded class-method direct calls
//! (ADR 0142 §2), keyword constructors and nullary `new`
//! (BT-4099). The spawn family and the ADR 0135 supervisor start keep their
//! caller-side spawn but resolve the module at call time (BT-4278). Each
//! negative has an unshadowed positive control so it cannot pass vacuously.

use super::super::{CodegenOptions, CoreErlangGenerator, DirectCallClassInfo, generate_module};
use beamtalk_core::ast::{
    Expression, Identifier, KeywordPart, Literal, MessageSelector, MethodKind,
};
use beamtalk_core::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
use beamtalk_core::source_analysis::Span;

/// A stateless `Parser` declared by `package` with one `parse:` class method:
/// what `compute_direct_call_eligible` makes direct-call eligible.
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
        in_caller_new1: false,
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
            constant_value: None,
            answers_receiver: false,
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
    // A class-side self-send to a class method of the class's own module
    // passes the method's own `ClassSelf` behind the `class_self_direct_ok/4`
    // guard (BT-4301), shadowed or not: it describes how this module's own
    // functions are entered, which shadowing does not change.
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
    let forwarded = "'class_helper:'(ClassSelf, ";
    // BT-4316/BT-4442: the guarded direct call matches the call target's key
    // in the class's static call map.
    let probe_direct = "<~{{'bt@parser', 'class_helper:', 'Parser class'} := ";

    // Control: no rival, so the class-reference send in `probe` direct-calls.
    let control = compile(vec![]);
    assert!(
        control.contains(probe_direct),
        "control: an unshadowed class-reference send should direct-call. Got:\n{control}"
    );
    assert!(
        control.contains(forwarded),
        "control: the sibling call should pass `ClassSelf`. Got:\n{control}"
    );

    // Rival: a dependency also declares `Parser`.
    let code = compile(vec![sealed_parser(Some("json"))]);
    assert!(
        !code.contains(probe_direct) && code.contains(CLASS_SEND),
        "precondition: `Parser` must be shadowed, so `probe`'s class-reference send \
         stays a class send. Got:\n{code}"
    );
    assert!(
        code.contains(forwarded),
        "the own-class sibling call must still pass `ClassSelf` under shadowing. Got:\n{code}"
    );
}

// --- spawn family and supervisor start (BT-4278) -------------------------
//
// These keep their caller-side spawn (a dynamic `class_send` would start the
// process in the class `gen_server`, with no monitored-spawn clause), so a
// shadowed unqualified reference resolves the module at call time instead of
// pinning the compile-time one.

// BT-4404: the lookup also names the call shape it was lowered to, so the
// runtime can raise a structured error when the winning class is a
// different kind.
const RUNTIME_MODULE_LOOKUP: &str =
    "call 'beamtalk_class_instantiation':'resolve_spawn_module_or_raise'(";

fn counter_reference(package: Option<&str>) -> Expression {
    Expression::ClassReference {
        name: Identifier::new("Counter", Span::new(0, 7)),
        package: package.map(|p| Identifier::new(p, Span::new(0, 0))),
        span: Span::new(0, 7),
    }
}

/// `Counter <selector>` (one integer argument for a keyword selector) on a
/// fresh generator, `Counter` shadowed or not, a supervisor class or not.
fn send_spawn(selector: &str, shadowed: bool, package: Option<&str>, supervisor: bool) -> String {
    let mut generator = CoreErlangGenerator::new("bt@test@main");
    if shadowed {
        generator.set_shadowed_class_names(std::iter::once("Counter".into()).collect());
    }
    if supervisor {
        generator.supervisor_classes.insert("Counter".to_string());
    }
    let (selector, arguments) = if selector.ends_with(':') {
        (
            MessageSelector::Keyword(vec![KeywordPart::new(selector, Span::new(8, 20))]),
            vec![Expression::Literal(Literal::Integer(1), Span::new(21, 22))],
        )
    } else {
        (MessageSelector::Unary(selector.into()), vec![])
    };
    generator
        .generate_message_send(&counter_reference(package), &selector, &arguments)
        .expect("codegen should succeed")
        .to_pretty_string()
}

#[test]
fn spawn_family_on_shadowed_class_resolves_module_at_runtime() {
    for (selector, function) in [
        ("spawn", "'spawn'"),
        ("spawnWith:", "'spawn'"),
        ("spawnMonitored", "'spawn_monitored'"),
        ("spawnMonitoredWith:", "'spawn_monitored'"),
    ] {
        let control = send_spawn(selector, false, None, false);
        assert!(
            control.contains(&format!("call 'bt@test@counter':{function}(")),
            "control: unshadowed `Counter {selector}` should call the compile-time module. \
             Got:\n{control}"
        );
        assert!(!control.contains(RUNTIME_MODULE_LOOKUP), "Got:\n{control}");

        let output = send_spawn(selector, true, None, false);
        assert!(
            output.contains(&format!(
                "{RUNTIME_MODULE_LOOKUP}'Counter', '{selector}', 'actor')"
            )),
            "shadowed `Counter {selector}` must resolve its module at runtime. Got:\n{output}"
        );
        assert!(
            !output.contains("'bt@test@counter'"),
            "shadowed `Counter {selector}` must not pin the compile-time module. Got:\n{output}"
        );
        assert!(
            output.contains(&format!(":{function}(")) && !output.contains(CLASS_SEND),
            "shadowed `Counter {selector}` must still spawn in the caller. Got:\n{output}"
        );
    }
}

#[test]
fn qualified_spawn_on_shadowed_class_keeps_its_named_module() {
    // `json@Counter spawn` names its package's module explicitly.
    let output = send_spawn("spawn", true, Some("json"), false);
    assert!(
        output.contains("call 'bt@json@counter':'spawn'(")
            && !output.contains(RUNTIME_MODULE_LOOKUP),
        "a package-qualified spawn keeps its named module. Got:\n{output}"
    );
}

#[test]
fn supervisor_start_on_shadowed_class_resolves_module_at_runtime() {
    let control = send_spawn("spawn", false, None, true);
    assert!(
        control.contains(
            "call 'beamtalk_supervisor':'class_start'('Counter', 'bt@test@counter', 'spawn', [])"
        ),
        "control: unshadowed supervisor start passes the compile-time module. Got:\n{control}"
    );
    let output = send_spawn("spawnWith:", true, None, true);
    assert!(
        output.contains(&format!(
            "call 'beamtalk_supervisor':'class_start'('Counter', \
             {RUNTIME_MODULE_LOOKUP}'Counter', 'spawnWith:', 'supervisor'), 'spawnWith:', ["
        )),
        "shadowed supervisor start must resolve its module at runtime. Got:\n{output}"
    );
    assert!(!output.contains("'bt@test@counter'"), "Got:\n{output}");
}

#[test]
fn shadowed_spawn_family_module_compiles_through_erlc() {
    // End to end: `Counter` declared by two packages, so every spawn-family
    // send in `Caller` resolves the module at call time — and the emitted
    // module is valid Core Erlang.
    let counter = |package: &str| ClassInfo {
        name: "Counter".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        class_methods: vec![],
        ..sealed_parser(Some(package))
    };
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  a => Counter spawn\n",
        "  b => Counter spawnWith: #{}\n",
        "  c => Counter spawnMonitored\n",
        "  d => Counter spawnMonitoredWith: #{}\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("caller").with_class_hierarchy(vec![counter("json"), counter("xml")]),
    )
    .expect("codegen should succeed");
    assert_eq!(
        code.matches(RUNTIME_MODULE_LOOKUP).count(),
        4,
        "every spawn-family send should resolve its module at runtime. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("caller", &code);
}

#[test]
fn class_self_module_lookup_keeps_the_unchecked_two_arity_helper() {
    // BT-4404 shares one lookup builder between the `ClassSelf` intrinsics
    // and class-reference spawns; only the latter may carry the kind check.
    let doc = CoreErlangGenerator::class_self_module_doc("spawn").to_pretty_string();
    assert!(
        doc.starts_with("call 'beamtalk_class_instantiation':'resolve_module_or_raise'(")
            && doc.ends_with(", 'spawn')")
            && !doc.contains(RUNTIME_MODULE_LOOKUP),
        "the ClassSelf lookup must stay resolve_module_or_raise/2. Got:\n{doc}"
    );
}

// --- spawn family on a known non-spawnable class (BT-4443) -----------------
//
// A Value or Object class has no `spawn/0,1`, so a pinned direct call would
// fail with a raw `undef`. When the compiler can prove the class is not
// spawnable (fully-known chain reaching neither `Actor` nor a supervisor
// base), an unqualified reference resolves its module at call time through
// the same kind-checked lookup as a shadowed one, which raises a structured
// `does_not_understand` — and the spawn stays in the caller. An unknown
// class or an incomplete chain proves nothing and keeps the pinned call.

/// `Caller` with one spawn-family send per method to `<prefix>Target`,
/// compiled against `target` (named `Target`).
fn spawn_sends_to(target: ClassInfo, prefix: &str) -> String {
    let source = format!(
        "Object subclass: Caller\n  a => {prefix}Target spawn\n  \
         b => {prefix}Target spawnWith: #{{}}\n  c => {prefix}Target spawnMonitored\n  \
         d => {prefix}Target spawnMonitoredWith: #{{}}\n"
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    generate_module(
        &module,
        CodegenOptions::new("caller").with_class_hierarchy(vec![target]),
    )
    .expect("codegen should succeed")
}

fn target_class(superclass: &str, is_value: bool) -> ClassInfo {
    ClassInfo {
        name: "Target".into(),
        superclass: Some(superclass.into()),
        is_sealed: false,
        is_value,
        class_methods: vec![],
        ..sealed_parser(None)
    }
}

const SPAWN_SELECTORS: [&str; 4] = [
    "spawn",
    "spawnWith:",
    "spawnMonitored",
    "spawnMonitoredWith:",
];

#[test]
fn spawn_family_on_known_non_spawnable_class_resolves_module_with_kind_check() {
    for (superclass, is_value) in [("Value", true), ("Object", false)] {
        let code = spawn_sends_to(target_class(superclass, is_value), "");
        for selector in SPAWN_SELECTORS {
            assert!(
                code.contains(&format!(
                    "{RUNTIME_MODULE_LOOKUP}'Target', '{selector}', 'actor')"
                )),
                "`Target {selector}` on a {superclass} class must resolve its module \
                 with the kind check. Got:\n{code}"
            );
        }
        assert!(
            !code.contains("'bt@target'") && !code.contains(CLASS_SEND),
            "a {superclass} class must neither pin its module nor leave the caller. \
             Got:\n{code}"
        );
    }
}

#[test]
fn spawn_family_on_actor_or_unprovable_class_keeps_the_pinned_module() {
    // Positive control (an actor) and an incomplete chain (an ancestor not in
    // scope), which proves nothing about the class's kind.
    for superclass in ["Actor", "NotInScopeBase"] {
        let code = spawn_sends_to(target_class(superclass, false), "");
        assert!(
            code.contains("call 'bt@target':'spawn'(")
                && code.contains("call 'bt@target':'spawn_monitored'(")
                && !code.contains(RUNTIME_MODULE_LOOKUP),
            "`Target` under {superclass} must keep its pinned spawn call. Got:\n{code}"
        );
    }
}

#[test]
fn qualified_spawn_on_known_non_spawnable_class_keeps_its_named_module() {
    // The hierarchy is keyed by bare name, so it proves nothing about the
    // class a package-qualified reference names.
    let code = spawn_sends_to(target_class("Value", true), "json@");
    assert!(
        code.contains("call 'bt@json@target':'spawn'(") && !code.contains(RUNTIME_MODULE_LOOKUP),
        "a package-qualified spawn keeps its named module. Got:\n{code}"
    );
}

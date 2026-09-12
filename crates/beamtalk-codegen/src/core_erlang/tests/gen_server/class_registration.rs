// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Empty-module and class-registration codegen: `register_class`,
//! per-class `__beamtalk_meta`, multi-class registration order, and
//! short-circuiting registration on the first class-definition error.

use super::*;

#[test]
fn test_generate_empty_module() {
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let result = generate(&module);
    assert!(result.is_ok());
    let code = result.unwrap();
    assert!(code.contains("module 'bt_module'"));
    assert!(code.contains("attributes ['behaviour' = ['gen_server']]"));
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "comprehensive test covering all registration metadata"
)]
fn test_class_registration_generation() {
    // Test that class definitions generate registration code
    use beamtalk_core::ast::{
        ClassDefinition, DeclaredKeyword, Identifier, MethodDefinition, MethodKind,
        StateDeclaration,
    };
    use beamtalk_core::source_analysis::Span;

    // Create a Counter class with instance variables and methods
    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 7)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("value", Span::new(0, 5)),
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
            type_annotation: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 10),
        }],
        methods: vec![
            MethodDefinition {
                selector: MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            },
            MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 50),
    };

    let module = Module {
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // Check that on_load attribute is present
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute. Got:\n{code}"
    );

    // Check that register_class/0 is exported
    assert!(
        code.contains("'register_class'/0"),
        "Should export register_class/0. Got:\n{code}"
    );

    // Check that register_class/0 function exists
    assert!(
        code.contains("'register_class'/0 = fun () ->"),
        "Should generate register_class function. Got:\n{code}"
    );

    // Check that it calls beamtalk_class_builder:register
    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState0)"),
        "Should call beamtalk_class_builder:register. Got:\n{code}"
    );

    // Check ClassBuilder state fields
    assert!(
        code.contains("'className' => 'Counter'"),
        "Should include className in builder state. Got:\n{code}"
    );
    assert!(
        code.contains("'moduleName' => 'counter'"),
        "Should include moduleName in builder state. Got:\n{code}"
    );
    assert!(
        code.contains("'superclassRef' => 'Actor'"),
        "Should include superclassRef in builder state. Got:\n{code}"
    );

    // Check beamtalk_class module attribute for dependency sorting
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}]"),
        "Should include beamtalk_class attribute with class and superclass. Got:\n{code}"
    );

    // methodSpecs, fieldSpecs, classMethods removed from BuilderState.
    // Methods and fields now live in meta map.
    assert!(
        code.contains("'meta' => ~{"),
        "Should include meta map in builder state. Got:\n{code}"
    );
    // Check method_info contains instance methods with arity
    assert!(
        code.contains("'method_info' => ~{"),
        "Should include method_info in meta map. Got:\n{code}"
    );
    assert!(
        code.contains("'class_method_info' => ~{"),
        "Should include class_method_info in meta map. Got:\n{code}"
    );
    // Check fields in meta
    assert!(
        code.contains("'fields' => ['value']"),
        "Should include fields in meta map. Got:\n{code}"
    );

    // modifiers removed from BuilderState; is_sealed/is_abstract now in meta map
    assert!(
        code.contains("'is_sealed' => 'false'"),
        "Should include is_sealed in meta map. Got:\n{code}"
    );

    // Check function returns ok
    assert!(code.contains("'ok'"), "Should return 'ok'. Got:\n{code}");

    // catch clause must re-raise, not silently swallow errors
    assert!(
        code.contains("catch <CatchType, CatchError, CatchStack> -> primop 'raw_raise'(CatchType, CatchError, CatchStack)"),
        "register_class/0 catch clause must re-raise via primop 'raw_raise' (BT-998). Got:\n{code}"
    );

    // every generated class module must export method_table/0 and
    // has_method/1 — these are the reflection accessors that runtime dispatch
    // (beamtalk_class_dispatch, method_table lookups, DNU chain walk) relies
    // on. The classifier at dispatch_codegen.rs:is_class_auto_export_selector
    // must stay aligned with this export set — see its unit test for the
    // reverse direction.
    //
    // Scope the assertions to the module header export list so we verify the
    // API surface, not just a substring match that could pick up function
    // definitions or other mentions.
    let header_exports = extract_module_exports(&code);
    assert!(
        header_exports.contains("'method_table'/0"),
        "Generated class module must export method_table/0 in header. Got header:\n{header_exports}\n\nFull code:\n{code}"
    );
    assert!(
        header_exports.contains("'has_method'/1"),
        "Generated class module must export has_method/1 in header. Got header:\n{header_exports}"
    );
    // superclass/0 and class_name/0 must always be in the header. class_name/0
    // is still reachable via `is_class_auto_export_selector`'s self-send
    // classifier. superclass/0 is part of the same uniform
    // auto-export set every class module emits, but no live caller for it
    // exists anywhere in the runtime: the gen_server's own
    // `superclass` reply reads `#class_state.superclass` directly rather than
    // calling this export, non-self-send dispatch resolves `superclass`
    // through the Class/Behaviour chain to
    // `beamtalk_behaviour_intrinsics:classSuperclass/1` (ADR 0032 Phase 2),
    // and self-sent `superclass` now routes through
    // `class_self_send_reflective_primitive` to the same intrinsic. This
    // assertion exists to keep the export set stable/uniform across class
    // modules regardless — not because anything currently calls it.
    assert!(
        header_exports.contains("'superclass'/0"),
        "Generated class module must export superclass/0 in header. Got header:\n{header_exports}"
    );
    assert!(
        header_exports.contains("'class_name'/0"),
        "Generated class module must export class_name/0 in header. Got header:\n{header_exports}"
    );
    // The old mistaken auto-export `methods/0` must NOT appear — the
    // classifier no longer includes it and no codegen site emits it.
    assert!(
        !header_exports.contains("'methods'/0"),
        "Generated class module must NOT export methods/0 in header — removed after BT-2007. Got header:\n{header_exports}"
    );
}

#[test]
fn test_class_state_emits_class_fields_in_meta() {
    // `classState:` declarations must be reflected into __beamtalk_meta/0
    // as a `class_fields` key, alongside the instance-side `fields` key.
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n",
        "  classState: total = 0\n",
        "  classState: label = \"unset\"\n\n",
        "  class total => self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // Instance-side fields unchanged.
    assert!(
        code.contains("'fields' => ['value']"),
        "Should include instance fields in meta map. Got:\n{code}"
    );
    // Class-side fields emitted in declaration order.
    assert!(
        code.contains("'class_fields' => ['total', 'label']"),
        "Should include class_fields (class variables) in meta map. Got:\n{code}"
    );
}

#[test]
fn test_no_class_state_emits_empty_class_fields() {
    // a class with no `classState:` declarations emits an empty
    // class_fields list so the runtime intrinsic always finds the key.
    let src = concat!("Actor subclass: Counter\n", "  state: value = 0\n");
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    assert!(
        code.contains("'class_fields' => []"),
        "Class with no class state should emit empty class_fields. Got:\n{code}"
    );
}

#[test]
fn test_no_class_registration_for_empty_module() {
    // Modules without class definitions should not have on_load or register_class
    let module = Module::new(vec![], Span::new(0, 0));
    let code = generate_module(&module, CodegenOptions::new("empty_module"))
        .expect("codegen should succeed");

    // Should NOT have on_load attribute
    assert!(
        !code.contains("'on_load'"),
        "Module without classes should not have on_load. Got:\n{code}"
    );

    // Should NOT export register_class/0
    assert!(
        !code.contains("'register_class'/0"),
        "Module without classes should not export register_class. Got:\n{code}"
    );

    // Should NOT have beamtalk_class attribute
    assert!(
        !code.contains("'beamtalk_class'"),
        "Module without classes should not have beamtalk_class attribute. Got:\n{code}"
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn test_multiple_classes_registration() {
    // Test that modules with multiple classes register all of them
    use beamtalk_core::ast::{ClassDefinition, DeclaredKeyword, Identifier, StateDeclaration};
    use beamtalk_core::source_analysis::Span;

    fn make_actor_class(
        name: &str,
        name_len: u32,
        field: &str,
        field_len: u32,
        span_end: u32,
    ) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            superclass_package: None,
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state: vec![StateDeclaration {
                name: Identifier::new(field, Span::new(0, field_len)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: Span::new(0, 10),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            handle_scope: None,
            span: Span::new(0, span_end),
        }
    }

    let module = Module {
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        classes: vec![
            make_actor_class("Counter", 7, "value", 5, 20),
            make_actor_class("Logger", 6, "messages", 8, 30),
        ],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("multi_actors"))
        .expect("codegen should succeed");

    // Should have on_load attribute
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute for multiple classes. Got:\n{code}"
    );

    // Should register both classes via ClassBuilder
    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState0)"),
        "Should register Counter via ClassBuilder. Got:\n{code}"
    );
    assert!(
        code.contains("'className' => 'Counter'"),
        "Should include Counter metadata. Got:\n{code}"
    );
    // fieldSpecs removed from BuilderState; fields now in meta map
    assert!(
        code.contains("'fields' => ['value']"),
        "Should include Counter fields in meta. Got:\n{code}"
    );

    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState1)"),
        "Should register Logger via ClassBuilder. Got:\n{code}"
    );
    assert!(
        code.contains("'className' => 'Logger'"),
        "Should include Logger metadata. Got:\n{code}"
    );
    assert!(
        code.contains("'fields' => ['messages']"),
        "Should include Logger fields in meta. Got:\n{code}"
    );

    // Should use let-binding chain to sequence registrations
    assert!(
        code.contains("let _BuilderState0 = ~{"),
        "Should have first BuilderState binding. Got:\n{code}"
    );
    assert!(
        code.contains("let _Reg0 = case"),
        "Should have first registration with _Reg0. Got:\n{code}"
    );
    assert!(
        code.contains("let _BuilderState1 = ~{"),
        "Should have second BuilderState binding. Got:\n{code}"
    );
    assert!(
        code.contains("let _Reg1 = case"),
        "Should chain second registration with _Reg1. Got:\n{code}"
    );

    // Final result propagates last _Reg.
    assert!(
        code.contains("in _Reg1"),
        "Should propagate last _Reg result after all registrations. Got:\n{code}"
    );

    // Short-circuit: earlier error must propagate before executing later classes.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should short-circuit on _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error. Got:\n{code}"
    );

    // Check beamtalk_class attribute lists both classes
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}, {'Logger', 'Actor'}]"),
        "Should include beamtalk_class attribute with both classes. Got:\n{code}"
    );
}

#[test]
fn test_multi_class_early_error_short_circuits() {
    // When an earlier class (not the last) returns {error, ...} from
    // update_class (e.g. stdlib_shadowing), the error must propagate — the
    // subsequent class registrations must not mask it with 'ok'.
    //
    // We verify this by checking the generated code structure: each _RegN
    // (except the last) must be wrapped in a case that short-circuits on error.
    use beamtalk_core::ast::{ClassDefinition, DeclaredKeyword, Identifier, StateDeclaration};
    use beamtalk_core::source_analysis::Span;

    fn make_class(name: &str, name_len: u32, span_end: u32) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            superclass_package: None,
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state: vec![StateDeclaration {
                name: Identifier::new("x", Span::new(0, 1)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: Span::new(0, 5),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            handle_scope: None,
            span: Span::new(0, span_end),
        }
    }

    // Two classes: ShadowA (index 0), ValidB (index 1, last).
    // ValidB is fine; ShadowA would be the one shadowing stdlib.
    // The fix must ensure that if _Reg0 is {error, ...}, we never reach _Reg1.
    let module = Module {
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        classes: vec![make_class("ShadowA", 7, 20), make_class("ValidB", 6, 30)],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("multi_shadow"))
        .expect("codegen should succeed");

    // First class must be wrapped in a short-circuit case check.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should wrap _Reg0 in a short-circuit case. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error before executing later classes. Got:\n{code}"
    );

    // The last class's result is returned directly (no further wrapping needed).
    assert!(
        code.contains("in _Reg1"),
        "Should use _Reg1 as the final result. Got:\n{code}"
    );

    // The second class must NOT be wrapped in its own short-circuit case
    // (it is the last, so its result flows out directly).
    assert!(
        !code.contains("in case _Reg1 of"),
        "Last _Reg should not be wrapped in a short-circuit case. Got:\n{code}"
    );
}

#[test]
fn test_three_class_short_circuit_nesting() {
    // Verify nesting correctness for N=3 classes.
    // Short-circuit cases are added for indices 0 and 1 (all except the last).
    // The last class (index 2) is returned directly with no extra wrapping.
    use beamtalk_core::ast::{ClassDefinition, DeclaredKeyword, Identifier, StateDeclaration};
    use beamtalk_core::source_analysis::Span;

    fn make_class(name: &str, name_len: u32) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            superclass_package: None,
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state: vec![StateDeclaration {
                name: Identifier::new("x", Span::new(0, 1)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: Span::new(0, 5),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            handle_scope: None,
            span: Span::new(0, 20),
        }
    }

    let module = Module {
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        classes: vec![make_class("A", 1), make_class("B", 1), make_class("C", 1)],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 60),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("three_classes"))
        .expect("codegen should succeed");

    // Classes 0 and 1 (non-last) must have short-circuit case wrappers.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should short-circuit on _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("in case _Reg1 of"),
        "Should short-circuit on _Reg1 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr1}> when 'true' -> {'error', _RegErr1}"),
        "Should propagate _Reg1 error. Got:\n{code}"
    );

    // Class 2 (last) must be returned directly — no extra case wrapping.
    assert!(
        code.contains("in _Reg2"),
        "Should use _Reg2 as final result. Got:\n{code}"
    );
    assert!(
        !code.contains("in case _Reg2 of"),
        "Last _Reg should not be wrapped in a short-circuit case. Got:\n{code}"
    );
}

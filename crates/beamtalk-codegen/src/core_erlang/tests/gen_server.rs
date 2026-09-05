// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for `gen_server` module and class-level Core Erlang code generation.
//!
//! Covers empty-module codegen, REPL workspace module generation, class
//! registration, class methods, value-subclass auto-accessors, class
//! hierarchy actor/value classification, and type-annotation writeback.

use super::*;

/// Extract a Core Erlang function body from generated code by cutting at the
/// next function header rather than relying on blank-line formatting.
///
/// Given a `marker` like `"'has_method'/1 = fun"`, returns the text from
/// that marker to the next top-level function definition (`'name'/N = fun`).
fn extract_core_fn<'a>(code: &'a str, marker: &str) -> Option<&'a str> {
    let start = code.find(marker)?;
    let body = &code[start + marker.len()..];
    // Scan for the next Core Erlang function header: a line starting with
    // `'<name>'/<digits> = fun`.  Track byte offset via cumulative line lengths
    // to avoid ambiguous substring matching.
    let mut offset = 0;
    for (i, line) in body.split('\n').enumerate() {
        if i == 0 {
            offset += line.len() + 1;
            continue;
        }
        let trimmed = line.trim_start();
        if trimmed.starts_with('\'') && trimmed.contains("'/") && trimmed.contains("= fun") {
            return Some(&body[..offset]);
        }
        offset += line.len() + 1;
    }
    Some(body)
}

/// Extract the module-header export list from generated Core Erlang.
///
/// A Core Erlang module header looks like:
///   module 'Name' ['export1'/0, 'export2'/1, ...]
///     attributes [...]
///
/// Returns the bracketed export list as a string (without the surrounding
/// brackets), or an empty string if no header is found. Used by tests that
/// want to assert on the exported API surface without false-positive matches
/// against function definitions deeper in the module body.
fn extract_module_exports(code: &str) -> String {
    let Some(module_start) = code.find("module '") else {
        return String::new();
    };
    let after_module = &code[module_start..];
    let Some(bracket_open) = after_module.find('[') else {
        return String::new();
    };
    let Some(bracket_close) = after_module[bracket_open..].find(']') else {
        return String::new();
    };
    after_module[bracket_open + 1..bracket_open + bracket_close].to_string()
}

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
    // BT-218: Test that class definitions generate registration code
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

    // BT-837: Check that it calls beamtalk_class_builder:register
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

    // BT-745: Check beamtalk_class module attribute for dependency sorting
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}]"),
        "Should include beamtalk_class attribute with class and superclass. Got:\n{code}"
    );

    // BT-1078: methodSpecs, fieldSpecs, classMethods removed from BuilderState.
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

    // BT-1078: modifiers removed from BuilderState; is_sealed/is_abstract now in meta map
    assert!(
        code.contains("'is_sealed' => 'false'"),
        "Should include is_sealed in meta map. Got:\n{code}"
    );

    // Check function returns ok
    assert!(code.contains("'ok'"), "Should return 'ok'. Got:\n{code}");

    // BT-998: catch clause must re-raise, not silently swallow errors
    assert!(
        code.contains("catch <CatchType, CatchError, CatchStack> -> primop 'raw_raise'(CatchType, CatchError, CatchStack)"),
        "register_class/0 catch clause must re-raise via primop 'raw_raise' (BT-998). Got:\n{code}"
    );

    // BT-2029: every generated class module must export method_table/0 and
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
    // classifier (BT-2007). superclass/0 is part of the same uniform
    // auto-export set every class module emits, but BT-3057's audit found no
    // live caller for it anywhere in the runtime: the gen_server's own
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
    // The old mistaken auto-export `methods/0` must NOT appear — it was
    // removed from the classifier after BT-2007 and no codegen site emits it.
    assert!(
        !header_exports.contains("'methods'/0"),
        "Generated class module must NOT export methods/0 in header — removed after BT-2007. Got header:\n{header_exports}"
    );
}

#[test]
fn test_class_state_emits_class_fields_in_meta() {
    // BT-2238: `classState:` declarations must be reflected into __beamtalk_meta/0
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
    // BT-2238: a class with no `classState:` declarations emits an empty
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
    // BT-218: Modules without class definitions should not have on_load or register_class
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

    // BT-745: Should NOT have beamtalk_class attribute
    assert!(
        !code.contains("'beamtalk_class'"),
        "Module without classes should not have beamtalk_class attribute. Got:\n{code}"
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn test_multiple_classes_registration() {
    // BT-218: Test that modules with multiple classes register all of them
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

    // BT-837: Should register both classes via ClassBuilder
    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState0)"),
        "Should register Counter via ClassBuilder. Got:\n{code}"
    );
    assert!(
        code.contains("'className' => 'Counter'"),
        "Should include Counter metadata. Got:\n{code}"
    );
    // BT-1078: fieldSpecs removed from BuilderState; fields now in meta map
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

    // BT-738: Final result propagates last _Reg.
    assert!(
        code.contains("in _Reg1"),
        "Should propagate last _Reg result after all registrations. Got:\n{code}"
    );

    // BT-749: Short-circuit: earlier error must propagate before executing later classes.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should short-circuit on _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error. Got:\n{code}"
    );

    // BT-745: Check beamtalk_class attribute lists both classes
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}, {'Logger', 'Actor'}]"),
        "Should include beamtalk_class attribute with both classes. Got:\n{code}"
    );
}

#[test]
fn test_multi_class_early_error_short_circuits() {
    // BT-749: When an earlier class (not the last) returns {error, ...} from
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

    // BT-749: First class must be wrapped in a short-circuit case check.
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
    // BT-749: Verify nesting correctness for N=3 classes.
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

    // BT-749: Classes 0 and 1 (non-last) must have short-circuit case wrappers.
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

#[test]
fn test_is_actor_class_direct_actor_subclass() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_object_subclass_is_value_type() {
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Object", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();
    assert!(!CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_multi_level_inheritance() {
    // LoggingCounter extends Counter extends Actor
    // Should still be detected as actor
    let counter = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let logging_counter = ClassDefinition {
        name: Identifier::new("LoggingCounter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    // Module with both classes; first class is LoggingCounter
    let module = Module {
        classes: vec![counter, logging_counter.clone()],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();

    // Test with LoggingCounter as the first class
    let module_lc = Module {
        classes: vec![logging_counter],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    // Build hierarchy from full module so Counter is known
    assert!(CoreErlangGenerator::is_actor_class(&module_lc, &hierarchy));
}

#[test]
fn test_is_actor_class_no_classes_defaults_to_actor() {
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_unknown_superclass_defaults_to_actor() {
    // LoggingCounter extends Counter, but Counter is NOT in this module.
    // Hierarchy chain is incomplete; should default to actor (backward compat).
    let class = ClassDefinition {
        name: Identifier::new("LoggingCounter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_collection_subclass_is_value_type() {
    // Collection extends Value (built-in), so subclasses are value types.
    let class = ClassDefinition {
        name: Identifier::new("MyList", Span::new(0, 0)),
        superclass: Some(Identifier::new("Collection", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Collection subclass should be value type (chain reaches Value)"
    );
}

#[test]
fn test_is_actor_class_integer_subclass_is_value_type() {
    // Integer is a sealed built-in extending Object — subclass should be value type.
    // (Sealed enforcement is separate; codegen should still route correctly.)
    let class = ClassDefinition {
        name: Identifier::new("MyInt", Span::new(0, 0)),
        superclass: Some(Identifier::new("Integer", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Integer subclass should be value type (chain reaches Object)"
    );
}

#[test]
fn test_is_actor_class_root_class_is_value_type() {
    // Root class (superclass: None → "none") should be value type, not actor.
    let class = ClassDefinition {
        name: Identifier::new("ProtoObject", Span::new(0, 0)),
        superclass: None,
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: true,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
            .0
            .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Root class (nil superclass) should be value type"
    );
}

#[test]
fn test_actor_value_classification_consistent_regardless_of_exception_grandchild_declaration_order()
{
    // BT-3086: MyBaseError is a grandchild of Exception (Exception -> Error -> MyBaseError);
    // MySpecificError extends MyBaseError. Neither analysis (`resolve_class_kind`) nor codegen
    // (`is_actor_class`) should ever classify these as actors, and the answer must not depend
    // on which order the two classes are declared in — `add_module_classes` registers every
    // class in a module before any chain is walked (Pass 1), so within-module declaration
    // order must never change the result.
    let base_declared_first =
        "Error subclass: MyBaseError\n\nMyBaseError subclass: MySpecificError\n";
    let child_declared_first =
        "MyBaseError subclass: MySpecificError\n\nError subclass: MyBaseError\n";

    for src in [base_declared_first, child_declared_first] {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
        let hierarchy =
            beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
                .0
                .unwrap();

        // Analysis: both classes resolve to Object — Exception's hierarchy is neither
        // Actor nor Value.
        for name in ["MyBaseError", "MySpecificError"] {
            assert_eq!(
                hierarchy.resolve_class_kind(name),
                ClassKind::Object,
                "{name} should resolve to ClassKind::Object regardless of declaration order. src:\n{src}"
            );
        }

        // Codegen: routing each class's own single-class module through `is_actor_class`
        // must agree with analysis — neither routes to actor (gen_server) codegen.
        for class in &module.classes {
            let single_class_module = Module {
                classes: vec![class.clone()],
                method_definitions: Vec::new(),
                protocols: Vec::new(),
                type_aliases: Vec::new(),
                native_declarations: Vec::new(),
                expressions: vec![],
                span: Span::new(0, 0),
                file_leading_comments: vec![],
                file_trailing_comments: Vec::new(),
            };
            assert!(
                !CoreErlangGenerator::is_actor_class(&single_class_module, &hierarchy),
                "{} should route to value-type codegen, not actor. src:\n{src}",
                class.name.name
            );
        }
    }
}

#[test]
fn test_generate_with_bindings_compiles_value_type() {
    // Test that generate_with_bindings produces valid output for a value type
    let class = ClassDefinition::new(
        Identifier::new("Point", Span::new(0, 0)),
        Identifier::new("Object", Span::new(0, 0)),
        vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 0),
        }],
        vec![],
        Span::new(0, 0),
    );
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let bindings = primitive_bindings::PrimitiveBindingTable::new();
    let result = generate_module(
        &module,
        CodegenOptions::new("point").with_bindings(bindings),
    );
    assert!(result.is_ok());
    let code = result.unwrap();
    assert!(code.contains("module 'point'"));
}

#[test]
fn test_class_method_rejects_field_access() {
    // BT-426: Class methods should reject instance field access
    let src = "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("test_class_field").with_workspace_mode(true),
    );
    assert!(
        result.is_err(),
        "Should reject field access in class method"
    );
    let err = format!("{}", result.unwrap_err());
    assert!(
        err.contains("cannot access instance field"),
        "Error should mention field access. Got: {err}"
    );
}

#[test]
fn test_class_method_rejects_field_assignment() {
    // BT-426: Class methods should reject instance field mutation
    let src = "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value := 42";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("test_class_assign").with_workspace_mode(true),
    );
    assert!(
        result.is_err(),
        "Should reject field assignment in class method"
    );
    let err = format!("{}", result.unwrap_err());
    assert!(
        err.contains("cannot assign to instance field"),
        "Error should mention field assignment. Got: {err}"
    );
}

#[test]
fn test_value_subclass_auto_getter_exported() {
    // BT-923: `Value subclass:` auto-generates getter functions for each slot.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    assert!(result.is_ok(), "Codegen should succeed for Value subclass:");
    let code = result.unwrap();
    // Getter exports: 'x'/1 and 'y'/1
    assert!(
        code.contains("'x'/1"),
        "Should export getter 'x'/1. Got:\n{code}"
    );
    assert!(
        code.contains("'y'/1"),
        "Should export getter 'y'/1. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_getter_function() {
    // BT-923: Getter body uses maps:get to read the slot from Self.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'x'/1 = fun (Self) ->"),
        "Should generate x/1 getter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('x', Self)"),
        "x getter should use maps:get. Got:\n{code}"
    );
    assert!(
        code.contains("'y'/1 = fun (Self) ->"),
        "Should generate y/1 getter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_setter_exported() {
    // BT-923: `Value subclass:` auto-generates with*: functional setters.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'withX:'/2"),
        "Should export withX:/2. Got:\n{code}"
    );
    assert!(
        code.contains("'withY:'/2"),
        "Should export withY:/2. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_setter_function() {
    // BT-923: with*: setter body uses maps:put to return an updated map.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'withX:'/2 = fun (Self, NewVal) ->"),
        "Should generate withX:/2 setter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'put'('x', NewVal, Self)"),
        "withX: setter should use maps:put. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_keyword_constructor_exported() {
    // BT-923: `Value subclass:` auto-generates an all-fields keyword constructor.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    // Keyword constructor selector for x, y → 'class_x:y:'/4
    assert!(
        code.contains("'class_x:y:'/4"),
        "Should export 'class_x:y:'/4 keyword constructor. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_keyword_constructor_function() {
    // BT-923: Keyword constructor body creates a tagged map with all slots.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'class_x:y:'/4 = fun (_ClassSelf, _ClassVars, SlotArg0, SlotArg1) ->"),
        "Should generate keyword constructor function. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class' => 'Point'"),
        "Keyword constructor should set $beamtalk_class. Got:\n{code}"
    );
    assert!(
        code.contains("'x' => SlotArg0"),
        "Keyword constructor should set x from SlotArg0. Got:\n{code}"
    );
    assert!(
        code.contains("'y' => SlotArg1"),
        "Keyword constructor should set y from SlotArg1. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_dispatch_routes_getter() {
    // BT-923: dispatch/3 must route getter selectors to auto-generated functions.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'bt@point':'x'(Self)"),
        "dispatch/3 should route 'x' to getter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_dispatch_routes_setter() {
    // BT-923: dispatch/3 must route with*: selectors to auto-generated functions.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'bt@point':'withX:'(Self, DispArg0)"),
        "dispatch/3 should route 'withX:' to setter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_has_method_includes_auto_methods() {
    // BT-923: has_method/1 must report true for auto-generated selectors.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'x'"),
        "has_method/1 should list 'x' getter. Got:\n{code}"
    );
    assert!(
        code.contains("'withX:'"),
        "has_method/1 should list 'withX:' setter. Got:\n{code}"
    );
}

/// BT-2734: Extracts the body between a `<key> => ~{` marker and its closing
/// `}~`. Map entry values are Core Erlang binary literals (`#{...}#`), which
/// never contain the `}~` map terminator, so the first `}~` after the marker is
/// the map close.
fn map_body<'a>(code: &'a str, key: &str) -> &'a str {
    let start = code
        .find(key)
        .unwrap_or_else(|| panic!("no `{key}` in:\n{code}"));
    let after = &code[start + key.len()..];
    let end = after
        .find("}~")
        .unwrap_or_else(|| panic!("no map close after `{key}`"));
    &after[..end]
}

#[test]
fn test_value_subclass_synthetic_accessor_metadata_injected() {
    // BT-2734: auto-generated getters / setters / keyword constructor gain
    // `__signature__` + `__doc__` entries in the builder-state selector maps, so
    // every reflective surface can resolve their docs uniformly.
    let module = make_value_subclass_point();
    let code = generate_module(&module, CodegenOptions::new("bt@point")).unwrap();

    let sigs = map_body(&code, "'methodSignatures' => ~{");
    assert!(
        sigs.contains("'x' =>") && sigs.contains("'y' =>"),
        "instance signatures should carry synthetic getters. Got:\n{sigs}"
    );
    assert!(
        sigs.contains("'withX:' =>") && sigs.contains("'withY:' =>"),
        "instance signatures should carry synthetic setters. Got:\n{sigs}"
    );

    let docs = map_body(&code, "'methodDocs' => ~{");
    assert!(
        docs.contains("'x' =>") && docs.contains("'withX:' =>"),
        "instance docs should carry synthetic accessor docs. Got:\n{docs}"
    );

    let class_sigs = map_body(&code, "'classMethodSignatures' => ~{");
    assert!(
        class_sigs.contains("'x:y:' =>"),
        "class-side signatures should carry the keyword constructor. Got:\n{class_sigs}"
    );
    let class_docs = map_body(&code, "'classMethodDocs' => ~{");
    assert!(
        class_docs.contains("'x:y:' =>"),
        "class-side docs should carry the keyword constructor doc. Got:\n{class_docs}"
    );
}

#[test]
fn test_value_subclass_synthetic_accessor_metadata_gated_to_value_kind() {
    // BT-2734: only `Value subclass:` classes get synthetic accessor metadata.
    // An `Object subclass:` with the same slot must not synthesize `withX:`.
    let mut module = make_value_subclass_point();
    module.classes[0].class_kind = ClassKind::Object;
    module.classes[0].superclass = Some(Identifier::new("Object", Span::new(0, 0)));
    let code = generate_module(&module, CodegenOptions::new("bt@widget")).unwrap();
    let sigs = map_body(&code, "'methodSignatures' => ~{");
    assert!(
        !sigs.contains("'withX:' =>"),
        "object subclass should not synthesize accessor signatures. Got:\n{sigs}"
    );
}

#[test]
fn test_object_subclass_no_auto_getters() {
    // BT-923: `Object subclass:` (ClassKind::Object) must NOT generate auto-getters.
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Object", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 0),
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
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    // Object subclass should NOT have auto-getter 'x'/1
    assert!(
        !code.contains("'x'/1 = fun (Self) ->"),
        "Object subclass should not generate auto-getter. Got:\n{code}"
    );
    // And should not have withX:/2
    assert!(
        !code.contains("'withX:'/2"),
        "Object subclass should not generate auto-setter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_user_defined_overrides_auto() {
    // BT-923: User-defined methods suppress the corresponding auto-generated method.
    let x_method = MethodDefinition {
        selector: MessageSelector::Unary("x".into()),
        parameters: vec![],
        return_type: None,
        body: vec![bare(Expression::Literal(
            Literal::Integer(99),
            Span::new(0, 0),
        ))],
        kind: MethodKind::Primary,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let class = ClassDefinition {
        name: Identifier::new("MyVal", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 0),
        }],
        methods: vec![x_method],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@my_val"));
    let code = result.unwrap();
    // The auto-getter would produce: call 'maps':'get'('x', Self)
    // When user defines 'x', that body should NOT appear — the user's body (99) wins.
    assert!(
        !code.contains("call 'maps':'get'('x', Self)"),
        "Auto-getter body should be suppressed when user defines 'x'. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_no_slots_no_keyword_constructor() {
    // BT-923: A Value subclass with no slots produces no keyword constructor.
    let class = ClassDefinition {
        name: Identifier::new("Empty", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@empty"));
    let code = result.unwrap();
    // A class with no slots has no keyword constructor selector, so no 'class_X:'/N pattern.
    // Scan all lines for the pattern: contains 'class_' AND contains ':'/  (selector with colon)
    let has_keyword_ctor = code
        .lines()
        .any(|line| line.contains("'class_") && line.contains(":/"));
    assert!(
        !has_keyword_ctor,
        "No keyword constructor should be generated for empty Value subclass. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_class_method_slot_send_routes_to_constructor() {
    // BT-996: `ClassName slot: value` inside a class method of the same class must
    // route to the auto-generated class-side keyword constructor, not the instance getter.
    //
    // Equivalent Beamtalk:
    //   Value subclass: SchemeSymbol
    //     state: symName = ""
    //     class withName: n => SchemeSymbol symName: n
    //
    // The generated `class_withName:/3` body should call `class_symName:` (constructor),
    // NOT `symName` (instance getter).
    let class = ClassDefinition {
        name: Identifier::new("SchemeSymbol", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("symName", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(
                Literal::String("".into()),
                Span::new(0, 0),
            )),
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 0),
        }],
        methods: vec![],
        class_methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "withName:",
                Span::new(0, 0),
            )]),
            parameters: vec![ParameterDefinition::new(Identifier::new(
                "n",
                Span::new(0, 0),
            ))],
            body: vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::ClassReference {
                    name: Identifier::new("SchemeSymbol", Span::new(0, 0)),
                    span: Span::new(0, 0),
                    package: None,
                }),
                selector: MessageSelector::Keyword(vec![KeywordPart::new(
                    "symName:",
                    Span::new(0, 0),
                )]),
                arguments: vec![Expression::Identifier(Identifier::new(
                    "n",
                    Span::new(0, 0),
                ))],
                is_cast: false,
                span: Span::new(0, 0),
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: true,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@scheme_symbol"));
    let code = result.unwrap();

    // Must call the class-side keyword constructor from within class_withName:
    assert!(
        code.contains("call 'bt@scheme_symbol':'class_symName:'(ClassSelf, ClassVars,"),
        "class_withName: should dispatch to class_symName: constructor. Got:\n{code}"
    );
    // The class_withName: body must not call the instance getter (symName/1) passing n as self.
    // (Note: `symName` legitimately appears in dispatch/3 for the instance getter arm — correct.)
    assert!(
        !code.contains("call 'bt@scheme_symbol':'symName'(ClassSelf")
            && !code.contains("call 'bt@scheme_symbol':'symName'(_n"),
        "class_withName: body must not call instance getter symName/1. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_writeback_inferred_return_type_appears_in_method_return_types() {
    // BT-1005: A user-defined Actor class method with no explicit return-type
    // annotation should have its inferred return type written back into the AST
    // before codegen, so the emitted BEAM module contains it in method_return_types.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  getValue => value
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // The writeback pass should have populated return_type in meta.method_info
    // with 'Integer' (inferred from the state variable type).
    // BT-1078: return types now live in meta.method_info, not methodReturnTypes.
    assert!(
        code.contains(
            "'getValue' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false', 'visibility' => 'public'}~"
        ),
        "meta.method_info should contain inferred return type for unannotated getValue. Got:\n{code}"
    );
}

#[test]
fn test_bt3249_method_source_omits_inferred_return_type_annotation() {
    // BT-3249: `getValue` has no explicit `-> Type` annotation in source —
    // return-type writeback infers `Integer` and (correctly) records it in
    // meta.method_info for chain-based REPL completion. But the *browsable*
    // `methodSource` text (what the cockpit/System Browser displays, and what
    // the ChangeLog's `disk_differs`/`body_delta` comparisons diff against)
    // must stay byte-for-byte what the user wrote — no `-> Integer` leaking
    // in from writeback. Without this fix, a save -> revert -> re-save of an
    // unchanged buffer recorded a spurious ChangeLog entry whose only diff
    // was this inferred annotation (root-caused by `extract_method_source`
    // unparsing the post-writeback AST while the ChangeLog's own
    // `source_ref` is unparsed pre-writeback).
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  getValue => value
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // Inference still ran and is still recorded for meta/REPL completion.
    assert!(
        code.contains("'return_type' => 'Integer'"),
        "inferred return type should still be recorded in meta.method_info. Got:\n{code}"
    );

    // `methodSource` bakes as a Core Erlang binary literal (per-byte
    // segments, not a plain string) — compare the whole `'methodSource' =>
    // ~{'getValue' => #{...}#}~` entry against the same byte-segment
    // encoding codegen itself produces for the exact bare (unannotated)
    // source. Scoped to just this entry (rather than a bare "-> Integer"
    // search over the whole module) since `methodSignatures` legitimately
    // keeps showing the inferred type for `:help` — only the browsable,
    // ChangeLog-diffed `methodSource` must drop it.
    let expected_method_source_entry = format!(
        "'methodSource' => ~{{'getValue' => #{{{}}}#}}~",
        beamtalk_cerl_doc::binary::binary_byte_segments("getValue => value")
    );
    assert!(
        code.contains(&expected_method_source_entry),
        "methodSource for getValue should round-trip the exact on-disk \
         (unannotated) source, with no inferred `-> Integer` leaking in. Got:\n{code}"
    );
}

#[test]
fn test_bt2524_generated_callbacks_notify_state_change_substrate() {
    // BT-2524: a compiled actor's generated handle_call/handle_cast must call
    // beamtalk_actor:notify_state_change/2 after committing new state, so a
    // *watched* actor's state writes push {object_changed,…} to the live
    // Inspector. The runtime beamtalk_actor dispatch path does this via
    // log_dispatch_complete/5; compiled actors run their own callbacks and would
    // otherwise never publish (the changed field would never flash).
    let src = "
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // BT-2717: handle_call strips codegen-internal `__local__` threading temps from
    // the committed state, then notifies + persists the cleaned state.
    assert!(
        code.contains("let CleanNewState = call 'beamtalk_actor':'strip_local_temps'(NewState) in"),
        "handle_call must strip __local__ threading temps before persist/notify. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_actor':'notify_state_change'(State, CleanNewState)"),
        "handle_call must notify the per-object change substrate with the cleaned \
         state after committing. Got:\n{code}"
    );
    // handle_cast (fire-and-forget) commits CastNewState; same strip + hook.
    assert!(
        code.contains(
            "let CleanCastNewState = call 'beamtalk_actor':'strip_local_temps'(CastNewState) in"
        ),
        "handle_cast must strip __local__ threading temps before persist/notify. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_actor':'notify_state_change'(State, CleanCastNewState)"),
        "handle_cast must notify the per-object change substrate with the cleaned \
         state after committing. Got:\n{code}"
    );
}

#[test]
fn test_bt2717_handle_continue_strips_local_temps_from_init_state() {
    // BT-2717: handle_continue is an outermost state-commit boundary (it persists
    // the post-initialize state). An `initialize` that threads an outer local must
    // not leave a `__local__` temp in the actor's first committed state, so the
    // post-initialize path strips it before the {'noreply', …} reply — the same
    // clean-up handle_call/handle_cast apply.
    let src = "
Actor subclass: Counter
  state: value = 0
  initialize => self.value := 1
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    assert!(
        code.contains(
            "let InitCleanState = call 'beamtalk_actor':'strip_local_temps'(InitNewState) in"
        ),
        "handle_continue must strip __local__ threading temps from the committed \
         post-initialize state. Got:\n{code}"
    );
    assert!(
        code.contains("{'noreply', InitCleanState}"),
        "handle_continue must reply with the cleaned post-initialize state. Got:\n{code}"
    );
}

#[test]
fn test_bt2717_handle_info_strips_local_temps_for_server_subclass() {
    // BT-2717: a Server subclass's handle_info is an outermost state-commit boundary
    // too — a `handleInfo:` that threads an outer local through a control-flow desugar
    // must not persist `__local__` temps into the committed gen_server state.
    let src = "
Server subclass: TickServer
  state: count = 0
  handleInfo: msg =>
    msg == #tick ifTrue: [self.count := self.count + 1]
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("tick_server"))
        .expect("codegen should succeed")
        .code;

    // Sanity: this is the Server-subclass handle_info (dispatches handleInfo:), not
    // the plain Actor delegate stub.
    assert!(
        code.contains("'safe_dispatch'('handleInfo:', [Msg], State)"),
        "expected a Server-subclass handle_info dispatching handleInfo:. Got:\n{code}"
    );
    assert!(
        code.contains(
            "let CleanInfoNewState = call 'beamtalk_actor':'strip_local_temps'(NewState) in"
        ),
        "handle_info must strip __local__ threading temps before committing the \
         post-handleInfo: state. Got:\n{code}"
    );
    assert!(
        code.contains("{'noreply', CleanInfoNewState}"),
        "handle_info must commit the cleaned state. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_explicit_annotation_not_overwritten_by_writeback() {
    // BT-1005: An explicitly annotated method must NOT be changed by the writeback pass.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  getValue -> Integer => value
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // Explicit annotation takes precedence — still appears correctly in meta.method_info.
    assert!(
        code.contains(
            "'getValue' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false', 'visibility' => 'public'}~"
        ),
        "Explicitly annotated method should appear in meta.method_info. Got:\n{code}"
    );
    // BT-3249: a genuine user-written annotation must still round-trip
    // untouched into the browsable `methodSource` text (only inference-
    // written ones get stripped). `methodSource` bakes as a Core Erlang
    // binary literal (per-byte segments, not a plain string), so compare
    // against the same byte-segment encoding codegen itself produces.
    let expected_signature_bytes =
        beamtalk_cerl_doc::binary::binary_byte_segments("getValue -> Integer =>");
    assert!(
        code.contains(&expected_signature_bytes),
        "explicit user-written return-type annotation must survive in \
         methodSource. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_literal_return_type_inferred_by_writeback() {
    // BT-1005: A method returning an integer literal should have Integer inferred
    // and written back even when the class has no typed state.
    let src = "
Actor subclass: Greeter
  answer => 42
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("greeter"))
        .expect("codegen should succeed")
        .code;

    assert!(
        code.contains(
            "'answer' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false', 'visibility' => 'public'}~"
        ),
        "meta.method_info should contain inferred Integer for literal-returning method. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_standalone_method_writeback_infers_return_type() {
    // BT-1005: Tonel-style standalone method definitions (Counter >> getValue => ...)
    // must also have their return types inferred and written back.
    // This exercises the module.method_definitions loop in infer_method_return_types.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0

Counter >> getValue => value
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    assert!(
        code.contains(
            "'getValue' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false', 'visibility' => 'public'}~"
        ),
        "meta.method_info should contain inferred Integer for standalone getValue. Got:\n{code}"
    );
}

#[test]
fn test_bt3367_sealed_class_does_not_mark_unsealed_class_method_as_sealed() {
    // BT-3367: a class-level `sealed` must not leak into an individual class
    // method's own `is_sealed` bit in __beamtalk_meta/0 — only a method itself
    // declared `class sealed` should report `is_sealed => true`. This is the
    // producer side of the bug: the REPL recovers an already-loaded project
    // class's method info from exactly this serialized meta map, and
    // compute_direct_call_eligible's Gate 5 (mod.rs) relies on a false
    // `is_sealed` here to route a self-constructing factory method (like
    // `make` below) through the safe gen_server dispatch instead of a direct
    // call with a hard-coded nil `ClassSelf`.
    let src = "
sealed Value subclass: SealedFactory
  class make => SealedFactory new
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("sealed_factory"))
        .expect("codegen should succeed")
        .code;

    assert!(
        code.contains(
            "'make' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'SealedFactory', 'is_sealed' => 'false', 'visibility' => 'public'}~"
        ),
        "an unsealed class method of a sealed class must serialize is_sealed => false. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_untyped_param_does_not_shadow_state_field_type() {
    // BT-1005: An untyped parameter with the same name as a state field must NOT
    // cause the method's return type to be inferred as the state field's type.
    // The untyped param should be Dynamic, so the method's inferred return type
    // is also Dynamic and no writeback annotation is emitted.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  add: value => value
";
    // `add: value` has an untyped param named `value` that shadows the `value`
    // state field. The return type should be Dynamic (not Integer).
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // `add:` must NOT appear in method_return_types with Integer inferred from
    // the state field — it should be absent (Dynamic = no entry).
    assert!(
        !code.contains("'add:' => 'Integer'"),
        "Untyped param `value` must not be mis-inferred as state field Integer. Got:\n{code}"
    );
}

#[test]
fn generate_module_with_pre_class_hierarchy_does_not_panic() {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
    use std::collections::HashMap;

    let src = "Object subclass: MyService\n  greet => \"hello\"";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let pre_class = ClassInfo {
        surface_incomplete: false,
        name: ecow::EcoString::from("Helper"),
        superclass: Some(ecow::EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    let result = generate_module(
        &module,
        CodegenOptions::new("bt@my_service")
            .with_workspace_mode(true)
            .with_class_hierarchy(vec![pre_class]),
    );
    assert!(result.is_ok(), "generate_module should succeed: {result:?}");
}

/// BT-2728: Builds a `ClassInfo` for a foreign target class named `PriceBand`
/// with a single state field `lo` carrying the given declared type. Used by the
/// extension-method field-type threading tests.
fn price_band_class_info_with_lo_type(
    lo_type: Option<&str>,
) -> beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo {
    use beamtalk_core::semantic_analysis::class_hierarchy::{ClassInfo, DeclaredType};
    use std::collections::HashMap;

    let mut state_types = HashMap::new();
    if let Some(ty) = lo_type {
        state_types.insert(ecow::EcoString::from("lo"), DeclaredType::parse(ty));
    }
    ClassInfo {
        surface_incomplete: false,
        name: ecow::EcoString::from("PriceBand"),
        superclass: Some(ecow::EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![ecow::EcoString::from("lo")],
        state_types,
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }
}

#[test]
fn test_bt2728_extension_object_typed_field_dispatches() {
    // BT-2728: An extension method comparing an object-typed `self.<field>` must
    // route through the runtime guard so it dispatches to the field type's
    // operator — same as an in-class method. The target class (`PriceBand`) is
    // foreign (declared elsewhere); its `lo :: Money` field type is resolved
    // from the class hierarchy threaded into extension codegen.
    let src = "PriceBand >> below: other => self.lo < other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(Some("Money"))]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'beamtalk_primitive':'is_object'("),
        "object-typed self.<field> comparison in an extension must be guarded (dispatch); got:\n{code}"
    );
}

#[test]
fn test_bt2728_extension_object_typed_field_arithmetic_dispatches() {
    // BT-2728: The arithmetic guard (`is_number`) follows a parallel path to the
    // comparison guard and shares the same `set_extension_target_field_types`
    // fix. An extension method doing arithmetic on an object-typed `self.<field>`
    // must route through the `is_number` guard so `self.lo + other` dispatches to
    // the field type's `+` instead of `badarith`-ing.
    let src = "PriceBand >> plus: other => self.lo + other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(Some("Money"))]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'erlang':'is_number'("),
        "object-typed self.<field> arithmetic in an extension must be guarded (dispatch); got:\n{code}"
    );
}

#[test]
fn test_bt2728_extension_untyped_field_stays_bare() {
    // BT-2728: An untyped `self.<field>` in an extension keeps the bare BIF (no
    // regression) — the guard/dispatch path is only taken for object-typed
    // fields.
    let src = "PriceBand >> below: other => self.lo < other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(None)]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'erlang':'<'(") && !code.contains("is_object"),
        "untyped self.<field> comparison in an extension must stay bare; got:\n{code}"
    );
}

#[test]
fn test_bt2728_extension_primitive_field_stays_bare() {
    // BT-2728: A primitive-typed (`Integer`) `self.<field>` in an extension keeps
    // the bare comparison BIF — parity with in-class primitive fields.
    let src = "PriceBand >> below: other => self.lo < other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(Some("Integer"))]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'erlang':'<'(") && !code.contains("is_object"),
        "primitive-typed self.<field> comparison in an extension must stay bare; got:\n{code}"
    );
}

#[test]
fn test_value_subclass_typed_fields_emit_type_alias() {
    // BT-1156: Value subclass with typed state: declarations emits '-type t()' attribute.
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![
            StateDeclaration {
                name: Identifier::new("x", Span::new(0, 0)),
                type_annotation: Some(TypeAnnotation::simple("Integer", Span::new(0, 0))),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: Span::new(0, 0),
            },
            StateDeclaration {
                name: Identifier::new("y", Span::new(0, 0)),
                type_annotation: Some(TypeAnnotation::simple("Integer", Span::new(0, 0))),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: Span::new(0, 0),
            },
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'type' ="),
        "Should emit 'type' attribute. Got:\n{code}"
    );
    assert!(
        code.contains("'map_field_exact'"),
        "Type alias fields should use map_field_exact. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class'"),
        "Type alias should include $beamtalk_class tag. Got:\n{code}"
    );
    assert!(
        code.contains("'Point'"),
        "Type alias should include class name atom. Got:\n{code}"
    );
    assert!(
        code.contains("'integer'"),
        "Typed Integer fields should map to integer(). Got:\n{code}"
    );
    assert!(
        code.contains("'export_type' = [{'t', 0}]"),
        "Should emit export_type([t/0]) so other modules can reference Point:t(). Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_untyped_fields_still_emit_type_alias() {
    // BT-1156: Value subclass with untyped state: declarations also emits '-type t()'
    // using any() for untyped fields.
    let module = make_value_subclass_point(); // x and y have no type annotations
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'type' ="),
        "Should emit 'type' attribute for untyped fields too. Got:\n{code}"
    );
    assert!(
        code.contains("'any'"),
        "Untyped fields should use any(). Got:\n{code}"
    );
    assert!(
        code.contains("'export_type' = [{'t', 0}]"),
        "Should emit export_type([t/0]) so other modules can reference Point:t(). Got:\n{code}"
    );
}

#[test]
fn test_actor_class_method_alias_param_emits_user_type_and_named_type() {
    // BT-2909: wiring the compile's `AliasRegistry` into `actor_codegen.rs`'s
    // `generate_class_specs` call site must make an alias-typed annotation
    // emit a `user_type` reference — and the module must also declare the
    // matching named `-type` in its own attribute list (an `erlc` compile
    // error otherwise). Actor *instance* methods don't get standalone specs
    // (BT-1944 — they're dispatch clauses inside `safe_dispatch/3`), so this
    // exercises the class-side method spec path, the only spec surface a
    // full `gen_server` actor module has.
    let src = "
type RestartStrategy = #temporary | #transient | #permanent

Actor subclass: Supervisor
  class defaultStrategy: policy :: RestartStrategy => policy
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");
    let code = generate_module(&module, CodegenOptions::new("bt@supervisor"))
        .expect("codegen should succeed");

    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "class method param typed with the alias should emit a user_type reference. Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "module must declare the matching named -type for the alias. Got:\n{code}"
    );
    assert!(
        code.contains(
            "{'type', 0, 'union', [{'atom', 0, 'temporary'}, {'atom', 0, 'transient'}, \
             {'atom', 0, 'permanent'}]}"
        ),
        "named -type declaration must expand the alias's RHS. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_field_alias_emits_user_type_and_named_type() {
    // BT-2909: same wiring check as the actor test above, but for
    // `value_type_codegen.rs`'s `generate_type_alias`/`generate_class_specs`
    // call sites — a Value subclass's `state:` field typed with an alias
    // must reference the alias's named `-type` from inside the class's own
    // `-type t()` map alias (BT-1156), with the named `-type` declared
    // alongside it in the same module.
    let src = "
type RestartStrategy = #temporary | #transient | #permanent

Value subclass: Child
  state: strategy :: RestartStrategy = #temporary
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");
    let code =
        generate_module(&module, CodegenOptions::new("bt@child")).expect("codegen should succeed");

    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "state field typed with the alias should emit a user_type reference. Got:\n{code}"
    );
    assert!(
        code.contains("'export_type' = [{'t', 0}]"),
        "Value subclass's own -type t() alias (BT-1156) must be unaffected. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_cross_module_alias_reference_emits_user_type() {
    // BT-2932: same wiring check as
    // `test_value_subclass_field_alias_emits_user_type_and_named_type`
    // above, but the alias is declared in a *different* compiled module —
    // threaded in via `CodegenOptions::with_pre_loaded_aliases`, mirroring
    // how the CLI build pipeline populates it from
    // `ClassHierarchyContext::pre_loaded_aliases` — instead of this
    // module's own `type_aliases`.
    let alias_src = "type RestartStrategy = #temporary | #transient | #permanent";
    let alias_tokens = beamtalk_core::source_analysis::lex_with_eof(alias_src);
    let (alias_module, alias_diags) = beamtalk_core::source_analysis::parse(alias_tokens);
    assert!(
        alias_diags.is_empty(),
        "alias-declaring module parse should succeed: {alias_diags:?}"
    );
    let pre_loaded_aliases =
        beamtalk_core::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module);

    // No `type RestartStrategy = ...` in this module — only a `state:`
    // field referencing the name declared elsewhere.
    let src = "
Value subclass: Child
  state: strategy :: RestartStrategy = #temporary
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@child_cross_module").with_pre_loaded_aliases(pre_loaded_aliases),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "state field typed with a cross-module alias should emit a user_type reference. \
         Got:\n{code}"
    );
    assert!(
        code.contains("'export_type' = [{'t', 0}]"),
        "Value subclass's own -type t() alias (BT-1156) must be unaffected. Got:\n{code}"
    );
}

#[test]
fn test_module_without_type_aliases_is_unaffected_by_alias_wiring() {
    // BT-2909 acceptance criterion: confirm generated Core Erlang for
    // message dispatch/field access is unaffected for modules with no
    // `type_aliases` — `generate_alias_type_attrs` returns an empty `Vec`
    // for an empty registry, so no `'type'` attribute for aliases (and no
    // spurious `user_type` reference) should appear anywhere.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  class from: start :: Integer => start
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");
    let code = generate_module(&module, CodegenOptions::new("bt@counter"))
        .expect("codegen should succeed");

    assert!(
        !code.contains("user_type"),
        "a module with no type_aliases must never emit a user_type reference. Got:\n{code}"
    );
}

#[test]
fn test_cross_module_alias_reference_emits_user_type_via_pre_loaded_aliases() {
    // BT-2932: an alias declared in one compiled module — simulated here by
    // extracting `AliasInfo`s from a standalone `type X = ...` module via
    // `AliasRegistry::extract_alias_infos`, the same mechanism the CLI build
    // pipeline uses to populate `ClassHierarchyContext::pre_loaded_aliases`
    // — and referenced in a method annotation in a *different* module must
    // still emit a `user_type` reference. Before this issue,
    // `actor_codegen.rs`'s `generate_class_specs` call site only ever saw
    // `AliasRegistry::from_module_declarations(module)` — the referencing
    // module's own (here, empty) `type_aliases` — so this exact case fell
    // through to `any()` (see the negative-control test below).
    let alias_src = "type RestartStrategy = #temporary | #transient | #permanent";
    let alias_tokens = beamtalk_core::source_analysis::lex_with_eof(alias_src);
    let (alias_module, alias_diags) = beamtalk_core::source_analysis::parse(alias_tokens);
    assert!(
        alias_diags.is_empty(),
        "alias-declaring module parse should succeed: {alias_diags:?}"
    );
    let pre_loaded_aliases =
        beamtalk_core::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module);
    assert_eq!(
        pre_loaded_aliases.len(),
        1,
        "sanity: exactly one pre-loaded alias extracted"
    );

    // The consuming module declares no `type RestartStrategy = ...` of its
    // own — only a method annotation referencing the name declared in the
    // other (pre-loaded) module.
    let src = "
Actor subclass: Supervisor
  class defaultStrategy: policy :: RestartStrategy => policy
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@supervisor_cross_module")
            .with_pre_loaded_aliases(pre_loaded_aliases),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "class method param typed with a cross-module alias should emit a user_type reference. \
         Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "module must declare the matching named -type for the cross-module alias. Got:\n{code}"
    );
}

#[test]
fn test_cross_module_alias_reference_compiles_through_erlc() {
    // BT-2932 (review follow-up): the sibling same-module case is guarded
    // through erlc by `test_alias_annotated_actor_module_compiles_through_erlc`
    // (BT-2909) — this exercises the cross-module case (alias declared in
    // one module, referenced via `pre_loaded_aliases` from another) the same
    // way, so a `-type`/`user_type` pairing bug here would fail to compile
    // rather than only fail a string assertion.
    let alias_src = "type RestartStrategy = #temporary | #transient | #permanent";
    let alias_tokens = beamtalk_core::source_analysis::lex_with_eof(alias_src);
    let (alias_module, alias_diags) = beamtalk_core::source_analysis::parse(alias_tokens);
    assert!(
        alias_diags.is_empty(),
        "alias-declaring module parse should succeed: {alias_diags:?}"
    );
    let pre_loaded_aliases =
        beamtalk_core::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module);

    let src = "
Actor subclass: Supervisor
  class defaultStrategy: policy :: RestartStrategy => policy
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");

    let code = generate_module(
        &module,
        CodegenOptions::new("bt_cross_module_alias_erlc_check")
            .with_pre_loaded_aliases(pre_loaded_aliases),
    )
    .expect("codegen should succeed");

    assert_compiles_through_erlc("bt_cross_module_alias_erlc_check", &code);
}

#[test]
fn test_cross_module_alias_reference_without_pre_loaded_aliases_falls_back_to_any() {
    // BT-2932 negative control: the same module, compiled without
    // `with_pre_loaded_aliases`, reproduces the pre-fix gap this issue
    // closes — since the module has no local `type_aliases` of its own,
    // `RestartStrategy` is an unresolved name and the annotation falls
    // through to `any()` rather than a spurious `user_type` reference.
    let src = "
Actor subclass: Supervisor
  class defaultStrategy: policy :: RestartStrategy => policy
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@supervisor_cross_module_neg"),
    )
    .expect("codegen should succeed");

    assert!(
        !code.contains("user_type"),
        "without pre-loaded aliases, an unresolved cross-module alias name must fall back to \
         any(), not user_type. Got:\n{code}"
    );
}

#[test]
fn test_unused_pre_loaded_alias_gets_no_type_declaration() {
    // BT-2940: `generate_alias_type_attrs` used to emit a `-type` for every
    // name in the pre-loaded `AliasRegistry` (BT-2932), regardless of
    // whether this module's own specs referenced it — for a project with
    // `A` aliases and `M` modules, every module's attribute list grew by
    // `A` entries rather than just what it used. Two aliases are pre-loaded
    // here; the consuming module references only one of them, so only that
    // one's `-type` declaration (and `user_type` reference) may appear.
    let alias_src = "
type RestartStrategy = #temporary | #transient | #permanent
type Timeout = Integer
";
    let alias_tokens = beamtalk_core::source_analysis::lex_with_eof(alias_src);
    let (alias_module, alias_diags) = beamtalk_core::source_analysis::parse(alias_tokens);
    assert!(
        alias_diags.is_empty(),
        "alias-declaring module parse should succeed: {alias_diags:?}"
    );
    let pre_loaded_aliases =
        beamtalk_core::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module);
    assert_eq!(
        pre_loaded_aliases.len(),
        2,
        "sanity: both pre-loaded aliases extracted"
    );

    let src = "
Actor subclass: Supervisor
  class defaultStrategy: policy :: RestartStrategy => policy
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@supervisor_unused_alias_scale")
            .with_pre_loaded_aliases(pre_loaded_aliases),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "the referenced alias should still emit a user_type reference. Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "the referenced alias's named -type must be declared. Got:\n{code}"
    );
    assert!(
        !code.contains("{'timeout',"),
        "an unreferenced pre-loaded alias must not get a -type declaration. Got:\n{code}"
    );
}

#[test]
fn test_unused_pre_loaded_alias_gets_no_type_declaration_for_value_state_field() {
    // BT-2940 sibling of `test_unused_pre_loaded_alias_gets_no_type_declaration`
    // for `value_type_codegen.rs`'s `generate_type_alias` call site — a
    // Value subclass's `state:` field is the other (besides method specs)
    // path that can mark an alias referenced; it must be scoped just as
    // precisely as the method-spec path above.
    let alias_src = "
type RestartStrategy = #temporary | #transient | #permanent
type Timeout = Integer
";
    let alias_tokens = beamtalk_core::source_analysis::lex_with_eof(alias_src);
    let (alias_module, alias_diags) = beamtalk_core::source_analysis::parse(alias_tokens);
    assert!(
        alias_diags.is_empty(),
        "alias-declaring module parse should succeed: {alias_diags:?}"
    );
    let pre_loaded_aliases =
        beamtalk_core::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module);
    assert_eq!(
        pre_loaded_aliases.len(),
        2,
        "sanity: both pre-loaded aliases extracted"
    );

    let src = "
Value subclass: Child
  state: strategy :: RestartStrategy = #temporary
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@child_unused_alias_scale")
            .with_pre_loaded_aliases(pre_loaded_aliases),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "the referenced alias should still emit a user_type reference. Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "the referenced alias's named -type must be declared. Got:\n{code}"
    );
    assert!(
        !code.contains("{'timeout',"),
        "an unreferenced pre-loaded alias must not get a -type declaration. Got:\n{code}"
    );
}

#[test]
fn test_alias_annotated_actor_module_compiles_through_erlc() {
    // BT-2909: the correctness trap this issue exists to close — a
    // `-spec`/`-type` referencing an undeclared local type is a hard `erlc`
    // compile error, not just a Dialyzer warning. This exercises the full
    // `generate_module` pipeline end-to-end through `erlc` (mirroring
    // `test_generated_core_erlang_compiles`/`test_while_true_compiles_through_erlc`)
    // to catch that failure mode directly rather than via string assertions
    // alone: if `Some(registry)` were ever wired into the spec-generating
    // calls without also emitting the matching named `-type` declaration,
    // this test would fail to compile through `erlc`.
    let src = "
type RestartStrategy = #temporary | #transient | #permanent

Actor subclass: Supervisor
  class defaultStrategy: policy :: RestartStrategy => policy
";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse should succeed: {diags:?}");
    let code = generate_module(&module, CodegenOptions::new("bt_alias_erlc_check"))
        .expect("codegen should succeed");

    assert_compiles_through_erlc("bt_alias_erlc_check", &code);
}

#[test]
fn test_class_method_local_var_assignment_of_self_class_method() {
    // BT-1201: class method `x := self classMethod` must NOT produce `in  in`.
    // Previously generated invalid Core Erlang:
    //   let X = let _CMR = call ... in let ClassVars1 = ... in let _Unwrapped = ... in  in X
    let src = "Object subclass: Broken\n  class a =>\n    x := self b.\n    x\n\n  class b => 42";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@broken").with_workspace_mode(true),
    );
    assert!(result.is_ok(), "Codegen should succeed. Got: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
    assert!(
        code.contains("'class_a'/2"),
        "Should generate class_a/2 function. Got:\n{code}"
    );
    assert!(
        code.contains("'class_b'/2"),
        "Should generate class_b/2 function. Got:\n{code}"
    );
}

#[test]
fn test_class_method_local_var_after_class_var_mutation() {
    // BT-1201 follow-up (reviewer feedback): a class var mutation (`self.cv := expr`) preceding
    // a local var assignment (`x := plainExpr`) must NOT incorrectly treat the local var RHS as
    // a class-var-producing expression. Any stale producer state left over from the field
    // assignment must not leak into processing the local var's RHS.
    //
    // Pattern: class a => self.cv := 1. x := self b. x
    // Without the clear, x would be bound to the field-assignment's result var, not `self b`.
    let src = "Object subclass: CVThenLocal\n  class cv = 0\n  class a =>\n    self.cv := 1.\n    x := self b.\n    x\n\n  class b => 99";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvthenlocal").with_workspace_mode(true),
    );
    assert!(result.is_ok(), "Codegen should succeed. Got: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_in_while_loop_body_compiles_and_threads_class_vars() {
    // BT-3150/BT-3168: a self-send to a same-class class method (`self bump`)
    // used as a bare statement inside a `whileTrue:` loop body previously
    // produced a `core_parse_error` — a doubled `in in` around the
    // self-send's `class_var_result` tuple-unwrapping, from
    // `emit_class_var_result_unwrap`'s open let-chain being re-wrapped by the
    // loop body's naive `let _ = <expr> in` statement sequencing. Fixing only
    // the syntax (so it compiles) was tried and rejected at the time (BT-3140/
    // BT-3150): the mutation was silently discarded by the time the loop
    // finished, because `ClassVarsN` was never threaded through the loop's
    // recursive tail call the way `StateAcc` was — rejected at compile time
    // instead. BT-3168 (ADR 0111 Addendum 9) closes that gap: `ClassVars`
    // now threads through the loop's own recursive tail call as an extra fun
    // parameter, so this compiles AND correctly accumulates. See
    // `stdlib/test/loop_class_var_mutation_test.bt`'s
    // `testSelfSendInWhileLoopAccumulates` for the
    // runtime-behavior pin (this test only pins the codegen shape).
    let src = "Value subclass: Driver\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aBlock over: aList =>\n    i := 1\n    [i <= aList size] whileTrue: [\n      self bump\n      aBlock value: (aList at: i)\n      i := i + 1\n    ]\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver").with_workspace_mode(true),
    );
    let code = result
        .unwrap_or_else(|e| panic!("self bump inside a whileTrue: body must compile. Got: {e:?}"));
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
    assert!(
        code.contains("fun (StateAcc, ClassVars)"),
        "The whileTrue: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', StateAcc, ClassVars}"),
        "The whileTrue: exit arm must carry ClassVars through. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_in_to_do_loop_body_compiles_and_threads_class_vars() {
    // BT-3150 review nit / BT-3168: `to:do:`/`to:by:do:` compile through the
    // same `generate_counted_stateful_loop`/`BodyKind::Letrec` path as
    // `timesRepeat:` (see `control_flow/mod.rs`'s `generate_counted_stateful_loop`
    // doc comment), so the `ClassVars`-threading fix must cover this
    // construct too, not just `whileTrue:`/`timesRepeat:`.
    let src = "Value subclass: DriverToDo\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: n =>\n    total := 0\n    1 to: n do: [:i | self bump. total := total + i]\n    total";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivertodo").with_workspace_mode(true),
    );
    let code = result
        .unwrap_or_else(|e| panic!("self bump inside a to:do: body must compile. Got: {e:?}"));
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
    assert!(
        code.contains(", StateAcc, ClassVars) ->"),
        "The to:do: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_as_local_var_assignment_rhs_in_while_loop_compiles() {
    // BT-3150 review follow-up: the `ClassMethodSelfSendInThreadedLoopBody`
    // guard only fires when a class-method self-send is itself the top-level
    // statement expression (`self bump` as a bare statement) — it doesn't walk
    // into `Expression::Assignment`, so `x := self bump` inside the same
    // `whileTrue:` body skipped the guard entirely and fell into
    // `try_generate_block_local_plain_let`, which used the non-open-scope-aware
    // `expression_doc` and wrapped it in `let X = <open chain> in`, reproducing
    // the exact doubled-`in` `core_parse_error` this PR exists to prevent (just
    // reached via assignment instead of a bare statement).
    //
    // Deliberately NOT rejected the way a bare self-send statement is: unlike a
    // discarded statement, `x`'s value here is genuinely captured and used
    // within the same iteration (`result := result + x`) — the same
    // "self-send return value matters" shape that made blanket-rejecting
    // `Foldl*` bodies wrong (see `test_class_method_self_send_as_collect_transform_still_compiles`).
    // So this is fixed as a compile bug (thread the self-send's class-var
    // mutation ahead of the assignment's own compile, mirroring BT-1397's fix
    // for the same shape inside blocks generally), not folded into the reject
    // list. `self.runs` not accumulating across
    // iterations is the same pre-existing, tracked `Letrec` limitation as
    // always (this test only pins that it compiles and runs without crashing).
    let src = "Value subclass: DriverAssign\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aList =>\n    i := 1\n    result := 0\n    [i <= aList size] whileTrue: [\n      x := self bump\n      result := result + x\n      i := i + 1\n    ]\n    result";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverassign").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "x := self bump inside a whileTrue: body must compile. Got: {result:?}"
    );
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
}

#[test]
fn test_do_assigned_to_discarded_local_in_direct_params_loop_still_emits_foldl() {
    // BT-3150 review follow-up: `try_generate_block_local_plain_let`'s
    // producer-aware fix (above) initially discarded `val_doc` entirely in
    // the direct-params-loop "no single value" arm instead of emitting it
    // first (unlike the ordinary-value arm right above it). That arm fires
    // for a mutation-threaded `do:` nested inside a direct-params outer loop
    // (BT-1329/
    // BT-3053, see `test_do_nested_in_direct_params_loop` in
    // `control_flow/list_ops/tests.rs` for the bare-statement variant this
    // adapts) — there, `val_doc` isn't just "a value", it's the entire
    // generated `lists:foldl` call. Assigning such a `do:`'s result to a
    // discarded local var (`_y := items do: [...]`) inside a direct-params
    // loop silently dropped the nested loop from the generated code — no
    // crash, just the nested `do:` (and any mutation it made, like `seen`
    // below) never executing. A regression from the prior behavior (a loud
    // `core_parse_error` for this same shape) to silently wrong code, so this
    // pins that the `lists:foldl` call — and the loop it drives — survives.
    //
    // Confirmed via manual `beamtalk build` toggling of the fix (not just
    // reasoning about it) that this exact shape reproduces the drop with the
    // bug present and is fixed by it — several other plausible-looking
    // shapes (e.g. `_y := ...` as a `timesRepeat:` body's only/last
    // statement, or as a `class` method's `timesRepeat:` rather than an
    // `Actor` method's `to:do:`) turned out NOT to reach this code path at
    // all, so this test's shape matters and shouldn't be casually
    // "simplified".
    let src = "Actor subclass: CtrNested\n  state: x = 0\n  run: items =>\n    count := 0\n    seen := 0\n    1 to: 3 do: [:i |\n      _y := items do: [:item | seen := seen + 1]\n      count := count + 1\n    ]\n    count";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@ctrnested").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "do: assigned to a discarded local var inside a direct-params loop must compile. \
         Got: {result:?}"
    );
    let code = result.unwrap();
    assert!(
        code.contains("'lists':'foldl'"),
        "The nested do:'s lists:foldl call must not be dropped. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_alongside_local_in_times_repeat_body_compiles() {
    // BT-3150/BT-3168: the same gap reached via `timesRepeat:` instead of
    // `whileTrue:`, with a co-occurring local-variable mutation — a bare
    // self-send-only `timesRepeat:` body doesn't reach the state-threaded
    // loop codegen path at all (see
    // `test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading`
    // below), so this pins the shape that actually reaches it: a loop that
    // legitimately needs local threading (an accumulator) with a class-method
    // self-send alongside it. Now compiles and threads ClassVars correctly
    // (BT-3168, ADR 0111 Addendum 9) instead of being rejected.
    let src = "Value subclass: Driver5\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: n =>\n    total := 0\n    n timesRepeat: [\n      self bump\n      total := total + 1\n    ]\n    total";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver5").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!(
            "self bump alongside a local mutation inside a timesRepeat: body must compile. \
             Got: {e:?}"
        )
    });
    assert!(
        code.contains(", StateAcc, ClassVars) ->"),
        "The timesRepeat: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
}

#[test]
fn test_non_mutating_class_method_self_send_in_loop_body_also_compiles() {
    // BT-3150/BT-3168: every same-class class-method self-send routes
    // through the same `{class_var_result, ...}` unwrap convention
    // regardless of whether the callee actually touches class state — the
    // caller can't know that statically (the callee may be overridden, or
    // defined later in the file). `ClassVars` threading (BT-3168) works
    // unconditionally for the same reason: it doesn't need to know whether
    // the self-send actually mutates anything, only that the callee's return
    // convention always carries a (possibly-unchanged) `ClassVars` value.
    let src = "Value subclass: Driver7\n  class helper: x => x * 2\n  class countedRun: aBlock over: aList =>\n    i := 1\n    [i <= aList size] whileTrue: [\n      self helper: i\n      aBlock value: (aList at: i)\n      i := i + 1\n    ]\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver7").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "self helper: (a pure, non-class-var-mutating self-send) inside a whileTrue: body \
         must compile. Got: {result:?}"
    );
}

#[test]
fn test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading() {
    // BT-3150 (contrast case): a `timesRepeat:` body with ONLY a class-method
    // self-send and no other local-variable mutation never needs state
    // threading (`needs_mutation_threading`, BT-1346) — it compiles as an
    // ordinary block passed to the runtime `timesRepeat:` helper, never
    // reaching `generate_threaded_loop_body`/the BT-3150 gap at all. Pinned
    // here as the boundary of this fix's scope, mirroring BT-3140's analogous
    // bare-field-write contrast test.
    //
    // BT-3151: that "ordinary block" path is exactly `generate_block`'s
    // generic fallback, which is where BT-3151's own guard lives — so this
    // bare, mutation-losing self-send is now a compile error instead of
    // silently compiling. Updated from `result.is_ok()` (BT-3150-era) to
    // match.
    let src = "Value subclass: Driver4\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: n =>\n    n timesRepeat: [\n      self bump\n      self bump\n    ]\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver4").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A timesRepeat: body with only self-sends (no local var) skips the \
         threaded-loop codegen path, but the mutating self-send must now be \
         caught by BT-3151's unthreaded-block guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_after_loop_still_compiles() {
    // BT-3150 (contrast case): the workaround recommended by
    // `ClassMethodSelfSendInThreadedLoopBody`'s error message — accumulate a
    // local count inside the loop, then make the self-send once after the
    // loop, at the class method's own top frame (the already-proven ADR
    // 0110/BT-412 shape) — must keep compiling.
    //
    // BT-3151: this is deliberately a single top-frame self-send, not
    // `count timesRepeat: [self bump]` — repeating the self-send N times
    // still requires wrapping it in a block, which is exactly the shape
    // BT-3151's guard now (correctly) rejects. See
    // `test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading`
    // for that case.
    let src = "Value subclass: Driver8\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aBlock over: aList =>\n    i := 1\n    count := 0\n    [i <= aList size] whileTrue: [\n      count := count + 1\n      aBlock value: (aList at: i)\n      i := i + 1\n    ]\n    self bump\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver8").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "A class-method self-send after (not inside) the loop body must still \
         compile. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_alongside_local_in_do_body_survives_via_class_vars_threading() {
    // BT-3150 review follow-up / BT-3169: the `Letrec` (whileTrue:/timesRepeat:)
    // guard used to leave an identical class-var-mutation-loss gap open for
    // `Foldl*` bodies (do:/collect:/select:/inject:into:/...) — `ThreadingPlan`
    // threaded only `threaded_locals` (user `:=` locals) through a fold's
    // accumulator, never `ClassVars`, so a class-method self-send inside a
    // `do:` block with a co-occurring local mutation (which is what actually
    // routes it through `generate_threaded_loop_body_inner` in the first
    // place) lost its class-var mutation exactly like the `whileTrue:` case
    // — confirmed empirically (pre-fix): `runs` stayed at 0 across all 3 list
    // elements instead of accumulating.
    //
    // BT-3169 closes this: the fold's accumulator becomes a `{ClassVars,
    // StateAcc}` 2-tuple whenever the body threads `ClassVars` (ADR 0111
    // Addendum 9, Question 6), so the mutation now survives the loop and is
    // visible in the method's own `{'class_var_result', Result, ClassVarsN}`
    // return. Confirmed both by direct `erl` execution against the compiled
    // `.beam` (`runs` correctly ends at 3, not 0 — see BT-3169's own PR
    // description) and, structurally, here: the compiled `class_countedRun:`
    // fun's accumulator parameter and the post-`lists:foldl` extraction both
    // reference a *versioned* `ClassVarsN` name (`N > 0`), never the bare,
    // unmutated `ClassVars` the pre-fix compiler silently discarded into.
    let src = "Value subclass: DriverDo\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aList =>\n    total := 0\n    aList do: [:x | self bump. total := total + x]\n    total";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverdo").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!("A class-method self-send inside a do: body must compile. Got: {e:?}")
    });
    let func = extract_core_fn(&code, "'class_countedRun:'/3 = fun")
        .expect("expected a class_countedRun:/3 function in the generated code");
    // The fold's own accumulator parameter must be a raw {ClassVars, StateAcc}
    // tuple (unwrapped via two `erlang:element/2` calls), not the bare
    // literal `StateAcc` the pre-fix compiler emitted.
    assert!(
        func.contains("call 'erlang':'element'(1,") && func.contains("call 'erlang':'element'(2,"),
        "the fold fun's accumulator must be unwrapped from a {{ClassVars, StateAcc}} \
         tuple via two element/2 calls. Got:\n{func}"
    );
    // The method's own final class-var-result reply must reference a
    // *versioned* ClassVars name (ClassVars1, ClassVars2, ...) — proof the
    // self-send's mutation, threaded through the fold, reached the method's
    // own top-level return, not the bare (unmutated, version-0) `ClassVars`
    // parameter the pre-fix bug silently returned instead.
    let reply_idx = func
        .rfind("{'class_var_result',")
        .expect("expected a final {'class_var_result', ...} reply");
    let reply_tail = &func[reply_idx..];
    assert!(
        !reply_tail.trim_end_matches(')').ends_with(", ClassVars}"),
        "the method's own final class_var_result reply must thread the \
         self-send's mutated (versioned) ClassVars forward, not the bare, \
         unmutated ClassVars parameter — this is the exact BT-3151 silent-loss \
         shape BT-3169 closes. Got:\n{reply_tail}"
    );
}

#[test]
fn test_class_method_self_send_as_select_predicate_alongside_local_survives_via_class_vars_threading()
 {
    // BT-3150 review follow-up / BT-3169: an earlier version of this fix
    // blanket-rejected a class-method self-send in ANY `BodyKind::Foldl*`
    // body, including `select:`'s predicate position — but that broke a
    // real, existing stdlib fixture (`test/fixtures/class_method_block.bt`)
    // that uses pure (non-mutating) self-sends as the value feeding
    // `collect:`/`sort:`/`inject:into:` (see
    // `test_class_method_self_send_as_collect_transform_still_compiles`
    // below). Unlike `Letrec`, `select:`'s predicate result is NOT discarded
    // — it structurally IS the fold's output — so rejecting every self-send
    // there has a real false-positive cost. The `Letrec`-only compile-time
    // guard was never widened to cover this shape.
    //
    // BT-3169 instead makes this shape correct rather than rejecting it: the
    // fold's accumulator threads `ClassVars` through a `{ClassVars, StateAcc}`
    // 2-tuple, so a class-var mutation performed by `check:` — hypothetically
    // — would now survive rather than being silently lost. This fixture's
    // own `check:` is pure (no class var declared at all), so the test below
    // checks the *threading machinery* is in place — the fold fun's
    // accumulator unwrap and the assignment's own final ClassVars rebind —
    // not a specific mutated value, mirroring `class_method_block.bt`'s
    // deliberately-pure self-send shapes this fixture is modeled on.
    let src = "Value subclass: DriverSelect2\n  class check: x => x > 0\n  class positives: aList =>\n    seen := 0\n    result := aList select: [:x | seen := seen + 1. self check: x]\n    result";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverselect2").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!(
            "A class-method self-send used as select:'s predicate value, alongside a \
             co-occurring local mutation, must still compile. Got: {e:?}"
        )
    });
    let func = extract_core_fn(&code, "'class_positives:'/3 = fun")
        .expect("expected a class_positives:/3 function in the generated code");
    // The fold's own accumulator parameter must be a raw {ClassVars, AccSt}
    // tuple (unwrapped via two `erlang:element/2` calls before the
    // pre-existing {AccList, StateAcc} unpack), not the bare AccSt the
    // pre-fix compiler emitted.
    assert!(
        func.contains("call 'erlang':'element'(1,") && func.contains("call 'erlang':'element'(2,"),
        "the fold fun's accumulator must be unwrapped from a {{ClassVars, AccSt}} \
         tuple via two element/2 calls. Got:\n{func}"
    );
    // A fresh ClassVars name must be minted after the fold (from the raw,
    // still-wrapped `lists:foldl` result — `_RawFoldCV<N>`, per
    // `ThreadingPlan::foldl_call_doc`) to receive the threaded-through value
    // — proof the fold's own `{ClassVars, ...}` accumulator wrap and
    // post-`foldl` unwrap are both wired up for this predicate-position
    // self-send.
    assert!(
        func.contains("'erlang':'element'(1, _RawFoldCV"),
        "expected a post-fold ClassVars unwrap (element(1, _RawFoldCV...)) \
         rebinding the threaded-through value. Got:\n{func}"
    );
}

#[test]
fn test_class_method_self_send_as_collect_transform_still_compiles() {
    // BT-3150 review follow-up: pins the exact pattern from the real stdlib
    // fixture (`test/fixtures/class_method_block.bt`) that an earlier,
    // over-broad version of this fix accidentally broke in CI — a pure
    // (non-mutating) self-send used as `collect:`'s per-item transform,
    // alongside a co-occurring local mutation that routes the body through
    // `generate_threaded_loop_body_inner`. Must keep compiling.
    let src = "Object subclass: ClassMethodBlockLike\n  class double: x => x * 2\n  class doubleAllCounting: items =>\n    seen := 0\n    items collect: [:item | seen := seen + 1. self double: item]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@classmethodblocklike").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "A pure class-method self-send used as collect:'s transform, alongside a \
         co-occurring local mutation, must still compile. Got: {result:?}"
    );
}

#[test]
fn test_bare_class_method_self_send_in_select_body_skips_loop_threading() {
    // BT-3150 review follow-up (contrast case): a `select:`/`collect:`/`do:`
    // block with ONLY a class-method self-send and no other local-variable
    // mutation never needs state threading — mirroring
    // `test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading`
    // for `Letrec`, it compiles as an ordinary block (never reaching
    // `generate_threaded_loop_body`), routing instead through
    // `generate_block`'s generic fallback.
    //
    // BT-3151: this is the exact repro from that issue — confirmed
    // empirically (before the fix) that this shape silently loses the
    // class-var mutation (`runs` stayed 0). Now caught at compile time by
    // BT-3151's guard in `generate_block`. Updated from `result.is_ok()`
    // (BT-3150-era, when this was still a documented open gap) to match.
    let src = "Value subclass: DriverSelect\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x > 0\n  class positives: aList =>\n    aList select: [:x | self check: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverselect").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A select: body with only a class-method self-send (no local var) skips the \
         threaded-loop codegen path, but the mutating self-send must now be caught \
         by BT-3151's unthreaded-block guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_block_compiles_when_class_has_no_class_vars() {
    // BT-3151 follow-up: BT-3151's unthreaded-block guard can't see a
    // self-send's target selector when it isn't locally defined on the
    // current class (e.g. inherited from a superclass in a different file —
    // `compute_class_var_mutating_selectors` only has this class's own
    // `class_methods` to analyze), so it conservatively treats any such
    // self-send as unsafe. That conservatism is unsound as a blanket rule:
    // `stdlib/src/Subprocess.bt` self-sends `spawnWith:` (inherited from
    // `Actor`, `stdlib/src/Actor.bt`) from inside a `tryDo:` block, and
    // `just build` failed on it once this guard landed.
    //
    // The fix: gate the whole check on the class actually declaring class
    // variables (`class_var_names`). With none, there is no classState a
    // self-send could possibly lose — an inherited method's body is fixed
    // at the *superclass's* compile time and can only reference class vars
    // declared there or above, never ones a subclass adds later. This class
    // has no `classState:`, so a self-send to `spawnWith:` — not locally
    // defined here, standing in for the real inherited-from-`Actor` case —
    // must still compile inside a bare `select:` block, hitting exactly the
    // "isn't defined locally in this class" conservative-fallback branch
    // this guard would otherwise trip.
    let src = "Value subclass: NoClassVarsDriver\n  class doubled: aList =>\n    aList select: [:x | self spawnWith: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@noclassvarsdriver").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "A self-send inside a bare block must not be rejected when the enclosing \
         class has no class variables at all — there is no classState mutation to \
         lose. Got: {result:?}"
    );
}

#[test]
fn test_class_method_mutating_self_send_as_second_cascade_message_in_block_is_compile_error() {
    // BT-3151 review follow-up: a cascade's 2nd+ message is sent to the same
    // shared receiver as the first (cascade semantics evaluate the receiver
    // once), but `analyze_expression`'s `Expression::Cascade` arm only ever
    // checked later messages for `is_self_field_value_send` — it never
    // recorded a self-send to `self_send_selectors`/`has_self_sends` for
    // them, unlike the `MessageSend` arm's handling of the cascade's first
    // message (folded into `receiver` by the parser). A mutating self-send
    // hidden behind an earlier *pure* cascade message inside a bare block
    // (`self pureLog: x; check: x`) was therefore invisible to
    // `check_no_unsafe_class_method_self_sends`, silently compiling and
    // losing the mutation — reproducing the exact bug this guard exists to
    // close, just one cascade message later.
    let src = "Value subclass: DriverCascade\n  classState: runs = 0\n  class pureLog: x => x\n  class check: x => self.runs := self.runs + 1. x > 0\n  class positives: aList =>\n    aList select: [:x | self pureLog: x; check: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivercascade").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send as the 2nd+ message of a cascade inside a bare block \
         must be caught by BT-3151's guard just like a standalone self-send. \
         Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_erlang_interop_block_is_compile_error() {
    // BT-3151 review follow-up: a block argument crossing the Erlang interop
    // boundary in a direct `(Erlang mod) fn: arg` call
    // (`generate_direct_erlang_call`'s keyword branch, `dispatch_codegen.rs`)
    // routes through the same `generate_erlang_interop_wrapper` →
    // `generate_block` mechanism as a `select:`/`do:` argument — same
    // process, same lossy in-process self-send optimization. Must be caught
    // the same way.
    let src = "Value subclass: DriverErlangInterop\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class run: aList =>\n    (Erlang lists) foreach: [:x | self bump] over: aList\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivererlanginterop").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a block crossing the Erlang interop boundary \
         must be caught by BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_any_satisfy_block_is_compile_error() {
    // BT-3151 review follow-up: `anySatisfy:`/`allSatisfy:` (and every other
    // sibling list-op in `control_flow/list_ops/` with the same
    // `block_needs_mutation_threading`-gated "fall through to a bare/BIF
    // call" shape — `detect:ifNone:`, `count:`, `flatMap:`, `takeWhile:`,
    // `dropWhile:`, `partition:`, `groupBy:`, `sort:`, plus the
    // `eachWithIndex:`/`do:separatedBy:` desugar fallbacks) were left
    // unguarded by this PR's first push even though they're structurally
    // identical to `select:`/`do:`/`collect:`. Now share the guard via
    // `check_bare_list_op_block_self_sends`. This pins the exact repro from
    // the review comment.
    let src = "Value subclass: DriverAnySatisfy\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x > 0\n  class positives: aList =>\n    aList anySatisfy: [:x | self check: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driveranysatisfy").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside an anySatisfy: block must be caught by \
         BT-3151's guard, matching every other bare-block list-op call site. \
         Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_sort_block_is_compile_error() {
    // BT-3151 review follow-up: pins `sort:` (a 2-arg comparator block) as
    // another sibling covered by `check_bare_list_op_block_self_sends` — see
    // `test_class_method_self_send_in_any_satisfy_block_is_compile_error`'s
    // comment for the full list.
    let src = "Value subclass: DriverSort\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class sorted: aList =>\n    aList sort: [:a :b | (self check: a) < (self check: b)]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driversort").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a sort: comparator block must be caught by \
         BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_each_with_index_block_is_compile_error() {
    // BT-3151 review follow-up: `eachWithIndex:` desugars to `inject:into:`
    // (`try_generate_each_with_index`, `enumeration_ops.rs`) only when the
    // user block needs mutation threading; a bare self-send-only block falls
    // through to `Collection.bt`'s own self-hosted `eachWithIndex:` — a
    // same-process, in-process call, same as every other list-op call site.
    let src = "Value subclass: DriverEachWithIndex\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class run: aList =>\n    aList eachWithIndex: [:item :i | self check: item]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivereachwithindex").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside an eachWithIndex: block must be caught by \
         BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_do_separated_by_block_is_compile_error() {
    // BT-3151 review follow-up: `do:separatedBy:`'s desugar
    // (`try_generate_do_separated_by`, `enumeration_ops.rs`) has the same
    // bare-block fallthrough shape as `eachWithIndex:` above — checks both
    // the element and separator blocks.
    let src = "Value subclass: DriverDoSeparatedBy\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class run: aList =>\n    aList do: [:x | x] separatedBy: [self check: 0]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverdoseparatedby").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a do:separatedBy: separator block must be caught \
         by BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_detect_if_none_block_alongside_mutating_predicate_is_compile_error()
 {
    // BT-3151 review follow-up: when `detect:ifNone:`'s predicate needs
    // mutation threading (a co-occurring local-var mutation), execution
    // routes through `generate_list_detect_if_none_with_mutations`, which
    // compiles `if_none` independently via `expression_doc` →
    // `generate_block` — a bare, unthreaded block regardless of what the
    // predicate does. A mutating self-send hidden only in `if_none` (not
    // the predicate) must still be caught.
    let src = "Value subclass: DriverDetectIfNone\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class run: aList =>\n    seen := 0\n    aList detect: [:x | seen := seen + 1. x > 1000] ifNone: [self check: 0]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverdetectifnone").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside detect:ifNone:'s ifNone block must be caught by \
         BT-3151's guard even when the predicate needs mutation threading. \
         Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_pure_inject_into_block_is_compile_error() {
    // BT-3151 follow-up: `generate_list_inject`'s BT-1327 pure-block fast
    // path calls `generate_block_body` directly (to emit an inline
    // `lists:foldl` with zero wrapper overhead), bypassing
    // `generate_block`'s own BT-3151 self-send check entirely. Before this
    // was wired up (`check_no_unsafe_class_method_self_sends`, called from
    // `generate_list_inject` too), this didn't just silently lose the
    // mutation — it crashed erlc with malformed Core Erlang (`unbound
    // variable 'ClassVars1'`), confirmed empirically. Must now be a clean
    // compile-time error, matching every other bare-block call site.
    let src = "Value subclass: DriverInject\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class sumChecked: aList =>\n    aList inject: 0 into: [:acc :x | acc + (self check: x)]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverinject").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside inject:into:'s pure-block fast path must be \
         caught by BT-3151's guard, not reach codegen and crash erlc. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_while_condition_block_is_compile_error() {
    // BT-3151 follow-up: a `whileTrue:`/`whileFalse:` loop's *condition*
    // block is structurally the same kind of bare, unthreaded block as a
    // `select:`/`inject:into:` argument — `generate_while_loop` (and its
    // direct-params/hybrid-params variants) calls `generate_block_body` on
    // the condition directly, bypassing `generate_block`'s check the same
    // way `generate_list_inject`'s fast path did. A mutating self-send in
    // the condition silently loses its mutation on every iteration; must be
    // a compile-time error instead.
    let src = "Value subclass: DriverCond\n  classState: runs = 0\n  class shouldContinue: n => self.runs := self.runs + 1. self.runs < n\n  class run: n =>\n    i := 0\n    [self shouldContinue: n] whileTrue: [i := i + 1]\n    i";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivercond").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a whileTrue: condition block must be caught by \
         BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_var_mutation_emits_shadow_write() {
    // ADR 0110 (BT-3032/BT-3037): a top-frame class-var mutation in a class
    // method must write the just-updated ClassVars map into the
    // '$bt_class_vars_shadow' process-dictionary key, immediately after the
    // maps:put threading, so a foreign NLR relayed out of the method can
    // recover the mutation (read + erased by invoke_class_method/7, BT-3036).
    let src = "Object subclass: ShadowCounter\n  classState: runs = 0\n\n  class bump =>\n    self.runs := self.runs + 1\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@shadowcounter").with_workspace_mode(true),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains(
            "call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1)"
        ),
        "class-var mutation should emit the ADR 0110 (BT-3039) class-keyed shadow write. Got:\n{code}"
    );
}

#[test]
fn test_class_var_mutation_in_while_loop_body_compiles_and_threads_class_vars() {
    // BT-3140/BT-3168: a class-var mutation made directly inside a
    // whileTrue: loop body previously couldn't thread through
    // `generate_field_assignment_open`'s generic State/StateAcc mechanism —
    // it silently wrote into the loop's own scratch StateAcc map instead of
    // ClassVars, losing the mutation on both normal return and a foreign NLR
    // escape (confirmed empirically via a throwaway BUnit driver/probe
    // fixture, mirroring fixtures/collection_driver.bt/collection_probe.bt's
    // shape with the mutation moved inside the loop), so it was rejected at
    // compile time instead, mirroring BT-2792's FieldAssignmentInUnsupportedBlock
    // for the analogous "can't thread this state" shape. BT-3168 (ADR 0111
    // Addendum 9) closes the gap: `ClassVars` now threads through the loop's
    // own recursive tail call as an extra fun parameter, tagged with the
    // loop's real frame and a real ADR 0110 shadow write each iteration. See
    // `stdlib/test/loop_class_var_mutation_test.bt`'s
    // `testFieldAssignmentInWhileLoopAccumulates` for the
    // runtime-behavior pin (this test only pins the codegen shape).
    let src = "Object subclass: LoopShadowCounter\n  classState: runs = 0\n\n  class countUpTo: n =>\n    i := 0\n    [i < n] whileTrue: [\n      self.runs := self.runs + 1\n      i := i + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@loopshadowcounter").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!("self.runs := ... inside a whileTrue: body must compile. Got: {e:?}")
    });
    assert!(
        code.contains("fun (StateAcc, ClassVars)"),
        "The whileTrue: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
    assert!(
        code.contains(
            "call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1)"
        ),
        "the loop-body mutation must still emit the ADR 0110 shadow write. Got:\n{code}"
    );
}

#[test]
fn test_bare_class_var_mutation_in_times_repeat_body_hits_existing_stored_closure_guard() {
    // BT-3140: `needs_mutation_threading` (BT-1346) deliberately excludes bare
    // field writes/self-sends from triggering StateAcc threading in a class
    // method — a `timesRepeat:` body with ONLY a class-var write and no other
    // mutation never reaches `generate_threaded_loop_body`/
    // `generate_field_assignment_open` at all; it falls through to the
    // generic block path and is already caught by BT-2792's
    // `FieldAssignmentInUnsupportedBlock` (`validate_stored_closure`). Pinned
    // here as the contrast case to the co-occurring-local-mutation shape
    // below, which DOES reach the BT-3140 gap.
    let src = "Object subclass: TimesRepeatShadowCounter\n  classState: runs = 0\n\n  class bumpN: n =>\n    n timesRepeat: [self.runs := self.runs + 1]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@timesrepeatshadowcounter").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "Expected the pre-existing FieldAssignmentInUnsupportedBlock guard for a \
         bare class-var mutation (no co-occurring local/self-send mutation) inside \
         a timesRepeat: body. Got: {result:?}"
    );
}

#[test]
fn test_class_var_mutation_alongside_local_in_times_repeat_body_compiles() {
    // BT-3140/BT-3168: once a `timesRepeat:` body ALSO has a local-variable
    // mutation (or self-send), `needs_mutation_threading` fires for that
    // reason and the body IS routed through `generate_threaded_loop_body` —
    // this is the shape that actually matters: a loop that legitimately
    // needs local threading (an accumulator, a counter) with a class-var
    // write alongside it. Now compiles and threads ClassVars correctly
    // (BT-3168, ADR 0111 Addendum 9) instead of being rejected.
    let src = "Object subclass: TimesRepeatShadowCounter2\n  classState: runs = 0\n\n  class bumpN: n =>\n    seen := 0\n    n timesRepeat: [\n      self.runs := self.runs + 1\n      seen := seen + 1\n    ]\n    seen";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@timesrepeatshadowcounter2").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!(
            "self.runs := ... alongside a local mutation inside a timesRepeat: body must \
             compile. Got: {e:?}"
        )
    });
    assert!(
        code.contains(", StateAcc, ClassVars) ->"),
        "The timesRepeat: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
}

#[test]
fn test_class_var_mutation_before_loop_still_emits_shadow_write() {
    // BT-3140 (contrast case): a class-var mutation BEFORE a whileTrue: loop
    // (top frame, block_depth == 0, not inside the loop's threaded body) is
    // the already-proven ADR 0110 shape and must keep compiling + emitting
    // the shadow write — the BT-3140 rejection is scoped to mutations
    // literally inside the loop body, not merely a method that also has one.
    let src = "Object subclass: LoopShadowCounterOk\n  classState: runs = 0\n\n  class bumpThenLoop: n =>\n    self.runs := self.runs + 1\n    i := 0\n    [i < n] whileTrue: [i := i + 1]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@loopshadowcounterok").with_workspace_mode(true),
    )
    .expect("mutation before the loop, at top frame, must still compile");
    assert!(
        code.contains("$bt_class_vars_shadow"),
        "top-frame class-var mutation before the loop must still emit the \
         ADR 0110 shadow write. Got:\n{code}"
    );
}

#[test]
fn test_nested_letrec_direct_field_mutation_in_inner_loop_is_compile_error() {
    // BT-3172 (BT-3168 follow-up): a `Letrec` loop nested inside another
    // `Letrec` loop, where the INNER loop directly mutates a class var, but
    // the OUTER loop's own top-level statements (`j := 0`, the inner
    // `whileTrue:` send, `i := i + 1`) have no bare class-var mutation of
    // their own — so `loop_body_threads_class_vars` correctly returns
    // `false` for the outer loop (per its own narrow, top-level-only
    // design), meaning the outer loop's fun/tail call never threads
    // `ClassVars` at all. Before this fix, this compiled successfully but
    // silently discarded every inner-loop mutation (confirmed empirically:
    // both the method's own return and a later fresh read of the class var
    // returned `0` instead of `9` for `nestedBump: 3`). Now rejected at
    // compile time instead — see
    // `CodeGenError::ClassVarMutationLostAcrossNestedLoop`'s doc comment.
    let src = "Object subclass: NestedLoopShadowCounter\n  classState: runs = 0\n\n  class nestedBump: n =>\n    i := 0\n    [i < n] whileTrue: [\n      j := 0\n      [j < n] whileTrue: [\n        self.runs := self.runs + 1\n        j := j + 1\n      ]\n      i := i + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedloopshadowcounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "class variable 'runs'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a class-var write inside a \
             loop nested inside another loop. Got: {other:?}"
        ),
    }
}

#[test]
fn test_nested_letrec_self_send_mutation_in_inner_loop_is_compile_error() {
    // BT-3172: the same gap via a same-class self-send (BT-3150's shape)
    // inside the inner loop instead of a direct field write — the inner
    // loop's own `loop_body_threads_class_vars` matches `is_class_method_self_send`,
    // not `is_class_var_assignment`, but the outer loop's discard is
    // identical either way.
    let src = "Object subclass: NestedLoopSelfSendCounter\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedBumpViaSelfSend: n =>\n    i := 0\n    [i < n] whileTrue: [\n      j := 0\n      [j < n] whileTrue: [\n        self bump\n        j := j + 1\n      ]\n      i := i + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedloopselfsendcounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "'self bump'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a self-send inside a loop \
             nested inside another loop. Got: {other:?}"
        ),
    }
}

#[test]
fn test_nested_timesrepeat_class_var_mutation_in_inner_loop_is_compile_error() {
    // BT-3172: the same gap via `timesRepeat:`/`to:do:` nesting, not just
    // `whileTrue:` — `nested_letrec_loop_body` must recognize all four
    // Letrec-shaped loop selectors, not just `whileTrue:`/`whileFalse:`. The
    // outer body needs its own co-occurring local-variable mutation
    // (`seen := seen + 1`) so the OUTER `timesRepeat:` itself reaches
    // `generate_threaded_loop_body` (mirroring `bumpTimes:` in
    // `loop_class_var_mutation.bt`) instead of being caught earlier, for an
    // unrelated reason, by the generic `FieldAssignmentInUnsupportedBlock`
    // block-validator guard that a bare-class-var-only body (no co-occurring
    // local mutation) hits regardless of nesting.
    let src = "Object subclass: NestedCountedLoopCounter\n  classState: runs = 0\n\n  class nestedBumpTimes: n =>\n    seen := 0\n    n timesRepeat: [\n      n timesRepeat: [\n        self.runs := self.runs + 1\n      ]\n      seen := seen + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedcountedloopcounter").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { .. })
        ),
        "Expected ClassVarMutationLostAcrossNestedLoop for a class-var write inside a \
         timesRepeat: nested inside another timesRepeat:. Got: {result:?}"
    );
}

#[test]
fn test_instance_field_mutation_does_not_emit_shadow_write() {
    // ADR 0110: the shadow write is scoped to class-var mutations in class
    // methods — ordinary actor state threading must not gain a pdict write.
    let src = "Actor subclass: PlainCounter\n  state: count = 0\n\n  bump => self.count := self.count + 1";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@plaincounter").with_workspace_mode(true),
    )
    .expect("codegen should succeed");
    assert!(
        !code.contains("$bt_class_vars_shadow"),
        "instance field mutation must not emit the class-var shadow write. Got:\n{code}"
    );
}

#[test]
fn test_bt1213_block_value_with_captured_mutation_actor() {
    // BT-1213: [count := count + 1] value in actor context
    // Parse from source to get a realistic AST
    // Build AST manually: Object subclass: BT1213Actor
    //   testIt => count := 0. [count := count + 1] value. count
    let s = Span::new(0, 0);
    let count_id = || Expression::Identifier(Identifier::new("count", s));

    // count := count + 1
    let add_expr = Expression::MessageSend {
        receiver: Box::new(count_id()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![Expression::Literal(Literal::Integer(1), s)],
        is_cast: false,
        span: s,
    };
    let assign = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(add_expr),
        type_annotation: None,
        span: s,
    };

    // [count := count + 1] value
    let block = Block::new(vec![], vec![bare(assign)], s);
    let block_value = Expression::MessageSend {
        receiver: Box::new(Expression::Block(block)),
        selector: MessageSelector::Unary("value".into()),
        arguments: vec![],
        is_cast: false,
        span: s,
    };

    // count := 0
    let init_count = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(Expression::Literal(Literal::Integer(0), s)),
        type_annotation: None,
        span: s,
    };

    let method = MethodDefinition::new(
        MessageSelector::Unary("testIt".into()),
        vec![],
        vec![bare(init_count), bare(block_value), bare(count_id())],
        s,
    );

    let class = ClassDefinition {
        name: Identifier::new("BT1213Actor", s),
        superclass: Some(Identifier::new("Actor", s)),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![method],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("bt@bt1213_actor"))
        .expect("codegen should work");

    // Actor codegen should thread count through StateAcc
    assert!(
        code.contains("__local__count"),
        "Should thread count through StateAcc. Got:\n{code}"
    );
}

// ─── Native Facade (ADR 0056) ───────────────────────────────────────────────

/// Build a Module for `Actor subclass: TestNative native: test_backing_mod`
/// with two delegate methods.
fn make_native_actor_module() -> Module {
    let self_expr = || Expression::Identifier(Identifier::new("self", Span::new(0, 0)));
    let delegate_send = || {
        bare(Expression::MessageSend {
            receiver: Box::new(self_expr()),
            selector: MessageSelector::Unary("delegate".into()),
            arguments: vec![],
            is_cast: false,
            span: Span::new(0, 0),
        })
    };

    let class = ClassDefinition {
        name: Identifier::new("TestNative", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![
            MethodDefinition {
                selector: MessageSelector::Unary("doWork".into()),
                parameters: vec![],
                body: vec![delegate_send()],
                kind: MethodKind::Primary,
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
            MethodDefinition {
                selector: MessageSelector::Keyword(vec![KeywordPart::new(
                    "process:",
                    Span::new(0, 0),
                )]),
                parameters: vec![ParameterDefinition::new(Identifier::new(
                    "data",
                    Span::new(0, 0),
                ))],
                body: vec![delegate_send()],
                kind: MethodKind::Primary,
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: Some(Identifier::new("test_backing_mod", Span::new(0, 0))),
        handle_scope: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_native_facade_spawn_calls_backing_module() {
    // ADR 0056: spawn/1 should call BackingModule:start_link, not gen_server:start_link
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'test_backing_mod':'start_link'(Config)"),
        "spawn/1 should call backing module's start_link. Got:\n{code}"
    );
    // Should NOT contain gen_server:start_link (that's for regular actors)
    assert!(
        !code.contains("'gen_server':'start_link'"),
        "Native facade should not use gen_server:start_link. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_wraps_beamtalk_object() {
    // ADR 0056: spawn result is wrapped as #beamtalk_object{} record
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("{'beamtalk_object', 'TestNative', 'bt@test_native', Pid}"),
        "spawn should wrap result as beamtalk_object. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_0_delegates_to_spawn_1() {
    // ADR 0056: spawn/0 calls spawn/1 with empty map
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'spawn'/0 = fun () ->"),
        "Should have spawn/0. Got:\n{code}"
    );
    assert!(
        code.contains("'bt@test_native':'spawn'(~{}~)"),
        "spawn/0 should call spawn/1 with empty map. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_has_method_includes_all_selectors() {
    // ADR 0056: has_method/1 returns true for all declared selectors
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract the has_method/1 function body to avoid matching selectors in method_info/meta
    let has_method_fn =
        extract_core_fn(&code, "'has_method'/1 = fun").expect("has_method/1 not found");
    assert!(
        has_method_fn.contains("'doWork'"),
        "has_method/1 body should include 'doWork'. Got:\n{has_method_fn}"
    );
    assert!(
        has_method_fn.contains("'process:'"),
        "has_method/1 body should include 'process:'. Got:\n{has_method_fn}"
    );
}

#[test]
fn test_native_facade_meta_includes_native_flag() {
    // ADR 0056: __beamtalk_meta/0 includes native => true and backing_module
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract the __beamtalk_meta/0 function body to avoid matching keys in BuilderState.meta
    let meta_fn =
        extract_core_fn(&code, "'__beamtalk_meta'/0 = fun").expect("__beamtalk_meta/0 not found");
    assert!(
        meta_fn.contains("'native' => 'true'"),
        "__beamtalk_meta/0 body should include native => true. Got:\n{meta_fn}"
    );
    assert!(
        meta_fn.contains("'backing_module' => 'test_backing_mod'"),
        "__beamtalk_meta/0 body should include backing_module. Got:\n{meta_fn}"
    );
}

#[test]
fn test_meta_superclass_is_single_quoted_atom() {
    // BT-2328: the leaf-constructor migration must emit the meta-map superclass as a
    // single-quoted atom (`'superclass' => 'Actor'`). A stray leading quote ahead of
    // leaf::atom produced `''Actor'`, which desyncs Core Erlang atom quoting.
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    let meta_fn =
        extract_core_fn(&code, "'__beamtalk_meta'/0 = fun").expect("__beamtalk_meta/0 not found");
    assert!(
        meta_fn.contains("'superclass' => 'Actor'"),
        "__beamtalk_meta/0 body should include 'superclass' => 'Actor'. Got:\n{meta_fn}"
    );
    assert!(
        !meta_fn.contains("=> ''"),
        "__beamtalk_meta/0 must not emit doubled-quote atoms (e.g. ''Actor'). Got:\n{meta_fn}"
    );
}

#[test]
fn test_native_facade_no_gen_server_behaviour() {
    // ADR 0056: Native facade does not declare gen_server behaviour
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        !code.contains("'behaviour' = ['gen_server']"),
        "Native facade should not declare gen_server behaviour. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_no_gen_server_callbacks() {
    // ADR 0056: Native facade should not have init/1, handle_cast/2, etc.
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        !code.contains("'init'/1"),
        "Native facade should not have init/1. Got:\n{code}"
    );
    assert!(
        !code.contains("'handle_cast'/2"),
        "Native facade should not have handle_cast/2. Got:\n{code}"
    );
    assert!(
        !code.contains("'handle_call'/3"),
        "Native facade should not have handle_call/3. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_register_class_includes_meta() {
    // ADR 0056: register_class/0 should include native meta in BuilderState
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract the register_class/0 function body
    let register_fn =
        extract_core_fn(&code, "'register_class'/0 = fun").expect("register_class/0 not found");
    assert!(
        register_fn.contains("'beamtalk_class_builder':'register'"),
        "register_class/0 should call beamtalk_class_builder:register. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'isConstructible' => 'false'"),
        "BuilderState should mark native actors as not constructible. Got:\n{register_fn}"
    );
    // BuilderState.meta should contain native-specific keys
    assert!(
        register_fn.contains("'native' => 'true'"),
        "BuilderState.meta should include native => true. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'backing_module' => 'test_backing_mod'"),
        "BuilderState.meta should include backing_module. Got:\n{register_fn}"
    );
}

/// BT-2385: `native:` facade `register_class/0` bakes a `methodXref` list into
/// its `BuilderState`, exactly like the standard `register_class/0` path. Before
/// this fix native classes (e.g. `Subprocess`, `TranscriptStream`) loaded with no
/// baked `method_xref`, so they were absent from `beamtalk_xref` and every
/// navigation query source-scanned them via the miss-policy fallback.
#[test]
fn test_native_facade_register_class_bakes_method_xref() {
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    let register_fn =
        extract_core_fn(&code, "'register_class'/0 = fun").expect("register_class/0 not found");
    // The methodXref field is present and a list (not a `~{ }~` map).
    assert!(
        register_fn.contains("'methodXref' => ["),
        "native register_class/0 should bake a methodXref list. Got:\n{register_fn}"
    );
    // The instance methods `doWork` and `process:` are recorded, instance-side,
    // and tagged indexed (they carry analysable Beamtalk source).
    assert!(
        register_fn.contains("'selector' => 'doWork'"),
        "doWork xref entry missing. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'selector' => 'process:'"),
        "process: xref entry missing. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'class_side' => 'false'"),
        "instance-side entries should carry 'class_side' => 'false'. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'source_status' => 'indexed'"),
        "native instance-method rows should be tagged indexed. Got:\n{register_fn}"
    );
}

#[test]
fn test_native_facade_spawn_error_raises_instantiation_error() {
    // ADR 0056: spawn failure should raise instantiation_error with reason in details
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'instantiation_error'"),
        "Should raise instantiation_error on spawn failure. Got:\n{code}"
    );
    assert!(
        code.contains("'reason' => Reason"),
        "Should include reason in error details. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_handles_ignore() {
    // BT-1337: spawn/1 should handle `ignore` from start_link (init/1 returned ignore)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("<'ignore'> when 'true' ->"),
        "spawn/1 should have an 'ignore' match arm. Got:\n{code}"
    );
    assert!(
        code.contains("'reason' => 'ignore'"),
        "ignore case should set reason => 'ignore' in details. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_wraps_crash_in_try_catch() {
    // BT-1337: spawn/1 should wrap start_link in try-catch for crash handling
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("let StartResult = try call"),
        "spawn/1 should wrap start_link in try-catch. Got:\n{code}"
    );
    assert!(
        code.contains("of _StartOk -> _StartOk"),
        "try-catch should have of clause for success passthrough. Got:\n{code}"
    );
    assert!(
        code.contains("{'__bt_spawn_crash', SpawnCrashReason}"),
        "catch arm should wrap crash reason in __bt_spawn_crash tuple. Got:\n{code}"
    );
    assert!(
        code.contains("<{'__bt_spawn_crash', SpawnCrashReason}> when 'true' ->"),
        "case should match __bt_spawn_crash tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'reason' => SpawnCrashReason"),
        "crash case should include SpawnCrashReason in details. Got:\n{code}"
    );
}

/// Build a native actor with class methods and class variables for richer tests.
fn make_native_actor_with_class_methods() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("TestNativeRich", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("status".into()),
            parameters: vec![],
            body: vec![bare(Expression::Identifier(Identifier::new(
                "self",
                Span::new(0, 0),
            )))],
            kind: MethodKind::Primary,
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        class_methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![KeywordPart::new("connect:", Span::new(0, 0))]),
            parameters: vec![ParameterDefinition::new(Identifier::new(
                "config",
                Span::new(0, 0),
            ))],
            body: vec![bare(Expression::Identifier(Identifier::new(
                "config",
                Span::new(0, 0),
            )))],
            kind: MethodKind::Primary,
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: true,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        class_variables: vec![StateDeclaration {
            name: Identifier::new("current", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 0),
        }],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: Some("A test native actor with class methods.".to_string()),
        backing_module: Some(Identifier::new("test_rich_backing", Span::new(0, 0))),
        handle_scope: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_native_facade_class_methods_exported() {
    // ADR 0056: Class methods on native actors compile normally
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert!(
        code.contains("'class_connect:'/3"),
        "Should export class method 'class_connect:'/3. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_class_method_alias_param_emits_user_type_and_named_type() {
    // BT-2909: `gen_server/native_facade.rs`'s `generate_class_specs` call
    // site must resolve alias-typed annotations to `user_type` references,
    // with the module also declaring the matching named `-type` in the
    // same attribute list (an `erlc` compile error otherwise). Native
    // facade modules use the same `is_value_type: false` spec path as
    // regular actors (BT-1944 — instance methods don't get standalone
    // specs), so this uses the class-side `connect:` method.
    let mut module = make_native_actor_with_class_methods();
    module.type_aliases.push(TypeAliasDefinition {
        name: Identifier::new("RestartStrategy", Span::new(0, 0)),
        annotation: TypeAnnotation::union(
            vec![
                TypeAnnotation::singleton("temporary", Span::new(0, 0)),
                TypeAnnotation::singleton("transient", Span::new(0, 0)),
                TypeAnnotation::singleton("permanent", Span::new(0, 0)),
            ],
            Span::new(0, 0),
        ),
        is_internal: false,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    });
    module.classes[0].class_methods[0].parameters[0].type_annotation =
        Some(TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)));

    let code = generate_module(&module, CodegenOptions::new("bt@test_rich"))
        .expect("codegen should succeed");
    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "class method param typed with the alias should emit a user_type reference. Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "module must declare the matching named -type for the alias. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_cross_module_alias_reference_emits_user_type() {
    // BT-2932: same wiring check as
    // `test_native_facade_class_method_alias_param_emits_user_type_and_named_type`
    // above, but the alias is declared in a *different* compiled module —
    // threaded in via `CodegenOptions::with_pre_loaded_aliases` — instead of
    // this module's own `type_aliases`.
    let strategy_alias = TypeAliasDefinition {
        name: Identifier::new("RestartStrategy", Span::new(0, 0)),
        annotation: TypeAnnotation::union(
            vec![
                TypeAnnotation::singleton("temporary", Span::new(0, 0)),
                TypeAnnotation::singleton("transient", Span::new(0, 0)),
                TypeAnnotation::singleton("permanent", Span::new(0, 0)),
            ],
            Span::new(0, 0),
        ),
        is_internal: false,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let pre_loaded_aliases = vec![
        beamtalk_core::semantic_analysis::alias_registry::AliasInfo::from_definition(
            &strategy_alias,
        ),
    ];

    // No `type_aliases` of its own — the module only references the name.
    let mut module = make_native_actor_with_class_methods();
    module.classes[0].class_methods[0].parameters[0].type_annotation =
        Some(TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)));

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@test_rich_cross_module")
            .with_pre_loaded_aliases(pre_loaded_aliases),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "class method param typed with a cross-module alias should emit a user_type reference. \
         Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "module must declare the matching named -type for the cross-module alias. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_class_variables_in_builder_state() {
    // ADR 0056: classState: should appear in BuilderState
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert!(
        code.contains("'classState' => ~{'current' =>"),
        "BuilderState should include class variables. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_doc_comments_in_builder_state() {
    // Doc comments should propagate to BuilderState
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert!(
        code.contains("'classDoc' =>"),
        "BuilderState should include classDoc. Got:\n{code}"
    );
    // classDoc should not be 'none' since we set a doc comment
    assert!(
        !code.contains("'classDoc' => 'none'"),
        "classDoc should not be 'none' when doc comment is set. Got:\n{code}"
    );
}

// ===========================================================================
// BT-1210: Dispatch functions for self delegate methods
// ===========================================================================

#[test]
fn test_native_facade_dispatch_exported() {
    // BT-1210: Dispatch functions for self delegate methods must be exported
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'dispatch_doWork'/1"),
        "dispatch_doWork/1 should be exported. Got:\n{code}"
    );
    assert!(
        code.contains("'dispatch_process:'/2"),
        "dispatch_process:/2 should be exported. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_dispatch_extracts_pid() {
    // BT-1210: Dispatch functions extract pid from Self via element(4, Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract dispatch function body (starts with "= fun")
    let dispatch_dowork = code
        .split("'dispatch_doWork'/1 = fun")
        .nth(1)
        .expect("dispatch_doWork function body should exist");
    assert!(
        dispatch_dowork.contains("call 'erlang':'element'(4, Self)"),
        "dispatch should extract pid via element(4, Self). Got:\n{dispatch_dowork}"
    );
}

#[test]
fn test_native_facade_dispatch_calls_sync_send() {
    // BT-1210: Dispatch functions call beamtalk_actor:sync_send/3
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'beamtalk_actor':'sync_send'(Pid, 'doWork', [])"),
        "dispatch_doWork should call sync_send with empty args. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'sync_send'(Pid, 'process:', [Data])"),
        "dispatch_process: should call sync_send with [Data] args. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_dispatch_unary_arity() {
    // BT-1210: Unary self delegate dispatch has arity 1 (just Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'dispatch_doWork'/1 = fun (Self) ->"),
        "Unary dispatch should take only Self. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_dispatch_keyword_arity() {
    // BT-1210: Keyword self delegate dispatch has arity = params + 1 (for Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'dispatch_process:'/2 = fun (Data, Self) ->"),
        "Keyword dispatch should take params then Self. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_no_dispatch_for_beamtalk_body() {
    // BT-1210: Methods with full Beamtalk bodies should NOT get dispatch functions
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    // status => self (not self delegate) should not have a dispatch function
    assert!(
        !code.contains("'dispatch_status'"),
        "Non-delegate method should NOT get a dispatch function. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_in_block() {
    // BT-1397: Class method self-send inside a block should produce valid Core Erlang.
    // Previously, the open-scope `let ... in ` from the self-send was not closed,
    // resulting in `syntax error before: ']'` from the Core Erlang parser.
    let src = r"Object subclass: Foo
  class compare: a with: b => a < b
  class sortItems: items =>
    items sort: [:a :b | self compare: a with: b]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@foo").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "Class method with self-send in block should compile. Got: {:?}",
        result.err()
    );
    let code = result.unwrap();
    // The block body should call class_compare:with: directly and close with the result var.
    // Before BT-1397, the open-scope `let ... in` was left unclosed, producing a parse error.
    assert!(
        code.contains("'class_compare:with:'"),
        "Should call class_compare:with: directly. Got:\n{code}"
    );
    // Verify the block closes properly: the result var should appear before `])`
    // (closing the argument list), not after it.
    let fun_idx = code.find("fun (").expect("Should contain fun");
    let block_code = &code[fun_idx..];
    assert!(
        !block_code.contains("in ])"),
        "Block should not have unclosed scope before `])`. Got:\n{block_code}"
    );
}

#[test]
fn test_class_method_self_send_in_block_local_assignment() {
    // BT-1397: Local assignment with class method self-send as RHS inside a block.
    // The open-scope from the self-send must be emitted before the let binding.
    let src = r"Object subclass: Bar
  class double: x => x * 2
  class compare: a with: b => a < b
  class doubleAndSort: items =>
    items sort: [:a :b |
      da := self double: a
      db := self double: b
      self compare: da with: db
    ]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@bar").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "Block with local := class-method-self-send should compile. Got: {:?}",
        result.err()
    );
    let code = result.unwrap();
    // Verify: local assignment `da := self double: a` should emit the open scope
    // THEN bind Da, not wrap the open scope in `let Da = ... in  in`.
    assert!(
        !code.contains("in  in"),
        "Should not have double `in` from unclosed open scope. Got:\n{code}"
    );
}

/// BT-1610: A module with only Protocol definitions (no classes) should still
/// generate `register_class/0` that registers the protocols.
#[test]
fn protocol_only_module_generates_register_class() {
    let module = Module {
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("Displayable", Span::new(0, 0)),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = generate_module(&module, CodegenOptions::new("bt@proto_only"));
    assert!(
        result.is_ok(),
        "Protocol-only module should compile. Got: {:?}",
        result.err()
    );
    let code = result.unwrap();

    // Should have register_class/0 in exports
    assert!(
        code.contains("'register_class'/0"),
        "Should export register_class/0. Got:\n{code}"
    );

    // Should have on_load attribute
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute. Got:\n{code}"
    );

    // Should call beamtalk_protocol_registry:register_protocol
    assert!(
        code.contains("'beamtalk_protocol_registry':'register_protocol'"),
        "Should call register_protocol. Got:\n{code}"
    );

    // Should reference the Displayable protocol name
    assert!(
        code.contains("'Displayable'"),
        "Should reference protocol name. Got:\n{code}"
    );

    // BT-1611: Should include required_class_methods key
    assert!(
        code.contains("'required_class_methods'"),
        "Should include required_class_methods key. Got:\n{code}"
    );

    // Should NOT have class builder calls (no classes)
    assert!(
        !code.contains("'beamtalk_class_builder':'register'"),
        "Should not call class_builder:register. Got:\n{code}"
    );
}

#[test]
#[allow(clippy::similar_names)]
fn test_bt_1944_typed_param_does_not_change_actor_codegen() {
    // BT-1944: Type annotations on method params should be erasable — they
    // must NOT change the generated Core Erlang dispatch/body code for actors.
    // Uses a multi-keyword method matching the original reproducer:
    // `executeActivity:selector:args:timeout:` with `:: Integer | Nil` on last param.
    let untyped_src = concat!(
        "Actor subclass: TestActor\n",
        "  state: count = 0\n",
        "  executeActivity: act selector: sel args: a timeout: t =>\n",
        "    self.count := self.count + 1\n",
        "    t\n",
    );
    let typed_src = concat!(
        "Actor subclass: TestActor\n",
        "  state: count = 0\n",
        "  executeActivity: act selector: sel args: a timeout: t :: Integer | Nil =>\n",
        "    self.count := self.count + 1\n",
        "    t\n",
    );

    let tokens_u = beamtalk_core::source_analysis::lex_with_eof(untyped_src);
    let (module_u, _) = beamtalk_core::source_analysis::parse(tokens_u);
    let code_u = generate_module(
        &module_u,
        CodegenOptions::new("test_actor").with_workspace_mode(true),
    )
    .expect("untyped should compile");

    let tokens_t = beamtalk_core::source_analysis::lex_with_eof(typed_src);
    let (module_t, _) = beamtalk_core::source_analysis::parse(tokens_t);
    let code_t = generate_module(
        &module_t,
        CodegenOptions::new("test_actor").with_workspace_mode(true),
    )
    .expect("typed should compile");

    // Strip metadata lines that naturally differ (source text, param types).
    // Everything else — dispatch, body, exports — must be identical.
    let strip_metadata = |code: &str| -> String {
        code.lines()
            .filter(|line| {
                !line.contains("'param_types'")
                    && !line.contains("'methodSource'")
                    && !line.contains("'methodSignatures'")
                    // ADR 0087 Phase 2 (BT-2298): the typed param adds a class
                    // reference in its type annotation, which methodXref records.
                    // That is metadata, not dispatch/body, so strip it too.
                    && !line.contains("'methodXref'")
            })
            .collect::<Vec<_>>()
            .join("\n")
    };

    let code_u_stripped = strip_metadata(&code_u);
    let code_t_stripped = strip_metadata(&code_t);

    assert_eq!(
        code_u_stripped, code_t_stripped,
        "BT-1944: Typed param changed dispatch/body code (beyond metadata)"
    );

    // Actor instance methods should NOT generate spec attributes — methods are
    // dispatch clauses inside safe_dispatch/3, not standalone functions.
    assert!(
        !code_t.contains("'spec' ="),
        "BT-1944: Actor instance method should NOT generate spec attribute"
    );
}

/// ADR 0087 Phase 2 (BT-2298): `register_class/0` bakes a `methodXref` field
/// into the `BuilderState` map. Each entry records the method's defining line,
/// the selectors it sends (with receiver kind), and class references — all with
/// `source_status => indexed`.
#[test]
fn test_method_xref_baked_into_register_class() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: count = 0\n\n",
        "  increment =>\n",
        "    self.count := self.count + 1\n\n",
        "  class default -> Counter =>\n",
        "    Counter new\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // The methodXref field is present and a list (not a `~{ }~` map).
    assert!(
        code.contains("'methodXref' => ["),
        "Should bake a methodXref list. Got:\n{code}"
    );
    // The instance method `increment` is recorded, instance-side, indexed.
    assert!(
        code.contains("'selector' => 'increment'"),
        "increment entry missing. Got:\n{code}"
    );
    assert!(
        code.contains("'class_side' => 'false'"),
        "instance-side entry should carry 'class_side' => 'false'. Got:\n{code}"
    );
    // The `+` send inside `increment` is recorded with a self receiver kind
    // (it is sent to `self.count`, an `other` receiver — the field access).
    assert!(
        code.contains("'selector' => '+'"),
        "the `+` send should be recorded. Got:\n{code}"
    );
    assert!(
        code.contains("'recv_kind' =>"),
        "sends should carry a recv_kind. Got:\n{code}"
    );
    // The class-side method `default` references `Counter` (return type + body).
    assert!(
        code.contains("'class_side' => 'true'"),
        "class-side entry should carry 'class_side' => 'true'. Got:\n{code}"
    );
    assert!(
        code.contains("'class' => 'Counter'"),
        "the Counter reference should be recorded. Got:\n{code}"
    );
    // The user-authored rows are `indexed`.
    assert!(
        code.contains("'source_status' => 'indexed'"),
        "rows should be tagged indexed. Got:\n{code}"
    );
    // BT-3073: `Counter` no longer carries synthetic class-side rows for
    // `new`/`new:`/`spawn`/`spawnWith:` — BT-3071/BT-3072 lifted those bodies
    // into real, source-backed class methods on `Actor` itself
    // (`stdlib/src/Actor.bt`), so a subclass like `Counter` genuinely
    // *inherits* them rather than *defining* them, and its own methodXref
    // carries no row for them at all (the honest Smalltalk answer — see
    // BT-2614, which introduced the now-removed rows). Bound the methodXref
    // payload to the next class-info field (`'classState'`) so the assertions
    // below cannot be satisfied by unrelated parts of the generated module.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];

    // The optional synthetic_origin key is omitted for the user-authored indexed
    // rows. Scope the check to the `increment` row (an indexed user method) so it
    // is not tripped by unrelated nested rows. The increment row runs from its
    // `'selector' => 'increment'` key up to the start of the next xref row — NOT
    // the first nested `}~`, which would truncate the slice mid-row inside the
    // `sends` list (BT-2622).
    let inc_pos = mx_seg
        .find("'selector' => 'increment'")
        .expect("increment row present");
    let inc_after = &mx_seg[inc_pos..];
    // A `}~, ~{` sequence is the *row* separator in the methodXref list — it only
    // appears between top-level rows, never inside one (nested `sends`/`references`
    // maps close with `}~]`, not `}~, ~{`). Bounding here keeps the slice to the
    // single increment row instead of truncating at the first nested `}~`.
    let inc_row = match inc_after.find("}~, ~{") {
        Some(end) => &inc_after[..end],
        None => inc_after,
    };
    assert!(
        !inc_row.contains("synthetic_origin"),
        "synthetic_origin must be omitted for the indexed increment row. Got:\n{inc_row}"
    );
    // BT-3073: `new`/`new:`/`spawn`/`spawnWith:` are inherited from `Actor`,
    // not defined by `Counter` — no top-level row for them, synthetic or
    // otherwise. Match on the `class_side` + `selector` pair (not just
    // `'selector' => '<sel>'` in isolation) so a legitimate nested `sends`
    // entry — e.g. the `default` class method's own `Counter new` send,
    // which also carries a `'selector' => 'new'` key — is not a false
    // positive: only top-level methodXref rows carry `class_side`.
    for sel in ["new", "new:", "spawn", "spawnWith:"] {
        assert!(
            !mx_seg.contains(&format!("'class_side' => 'true', 'selector' => '{sel}'")),
            "`{sel}` is inherited from Actor and must not appear as a Counter class-side row. Got:\n{mx_seg}"
        );
    }
}

/// ADR 0087 Phase 6 (BT-2304): compiler-generated auto-accessors for a
/// `Value subclass:` class ride the `method_xref` write path with
/// `source_status => synthetic` and a derived `synthetic_origin` line pointing
/// at the generating slot declaration. They are included by default — the
/// documented parity exception that makes `implementorsOf:` on an auto-accessor
/// non-empty.
#[test]
fn test_method_xref_emits_synthetic_accessors_for_value_class() {
    let src = concat!(
        "Value subclass: Point\n",
        "  state: x :: Integer = 0\n",
        "  state: y :: Integer = 0\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("point")).expect("codegen should succeed");

    // Scope the assertions to the `methodXref` payload so they cannot be
    // satisfied by unrelated parts of the generated module (exports, dispatch,
    // method signatures, etc.). The payload runs from `'methodXref' => [` up to
    // the next class-info field, `'classState'`.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];

    // Synthetic getter rows for both slots, tagged synthetic.
    assert!(
        mx_seg.contains("'source_status' => 'synthetic'"),
        "auto-accessors should be tagged synthetic. Got:\n{mx_seg}"
    );
    // Getter selectors `x` and `y` are both present as synthetic rows.
    assert!(
        mx_seg.contains("'selector' => 'x'"),
        "getter `x` synthetic row missing. Got:\n{mx_seg}"
    );
    assert!(
        mx_seg.contains("'selector' => 'y'"),
        "getter `y` synthetic row missing. Got:\n{mx_seg}"
    );
    // The `with*:` setter selectors are emitted.
    assert!(
        mx_seg.contains("'selector' => 'withX:'") && mx_seg.contains("'selector' => 'withY:'"),
        "setter rows `withX:` / `withY:` missing. Got:\n{mx_seg}"
    );
    // Every synthetic row carries a derived synthetic_origin line.
    assert!(
        mx_seg.contains("'synthetic_origin' =>"),
        "synthetic rows must carry synthetic_origin. Got:\n{mx_seg}"
    );
    // Accessors delegate to runtime map primitives — no Beamtalk sends.
    assert!(
        mx_seg.contains("'sends' => []"),
        "synthetic accessors should have empty sends. Got:\n{mx_seg}"
    );
    // The slot's declared type `Integer` is recorded as a reference on the
    // accessor (return/param type), like a hand-written typed accessor.
    assert!(
        mx_seg.contains("'class' => 'Integer'"),
        "slot type `Integer` should be a reference on the accessor. Got:\n{mx_seg}"
    );
}

/// ADR 0087 Phase 6 (BT-2304): an `Object subclass:` (not a value class) gets no
/// auto-accessors, so no synthetic rows are emitted.
#[test]
fn test_method_xref_no_synthetic_rows_for_object_class() {
    let src = concat!(
        "Object subclass: Plain\n",
        "  state: count = 0\n\n",
        "  bump =>\n    count := count + 1\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("plain")).expect("codegen should succeed");

    assert!(
        !code.contains("'source_status' => 'synthetic'"),
        "non-value classes must not emit synthetic accessor rows. Got:\n{code}"
    );
}

/// ADR 0087 Phase 6 (BT-2304): a user-defined accessor suppresses the synthetic
/// one for that slot — `compute_auto_slot_methods` already excludes hand-defined
/// selectors, so the synthetic emission must not double-emit. The hand-written
/// `x` getter is `indexed`, and there is no synthetic `x` row.
#[test]
fn test_method_xref_user_accessor_suppresses_synthetic() {
    let src = concat!(
        "Value subclass: Point\n",
        "  state: x :: Integer = 0\n\n",
        "  x =>\n    self.x\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("point")).expect("codegen should succeed");

    // The hand-written `x` getter is an indexed row; the synthetic getter for
    // `x` is suppressed. The `withX:` setter is still synthetic.
    //
    // Scope to the `methodXref` payload (between `'methodXref' => [` and the
    // next class-info field `'classState'`) so a global `'withX:'` substring in
    // exports/dispatch cannot satisfy the assertion.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];

    // Exactly one synthetic row survives.
    let synthetic_count = mx_seg.matches("'source_status' => 'synthetic'").count();
    assert_eq!(
        synthetic_count, 1,
        "only the `withX:` setter should be synthetic (user defined `x`). Got:\n{mx_seg}"
    );

    // Prove that sole synthetic row is the `withX:` setter, not some other
    // selector: isolate the row containing the synthetic marker (each row is a
    // `~{ ... }~` map) and check its `selector`.
    let synth_marker = mx_seg
        .find("'source_status' => 'synthetic'")
        .expect("synthetic marker present");
    // Each row map opens with `~{'class_side' =>`; nested `~{...}~` reference
    // maps do not, so anchor on the row prefix to isolate the owning row.
    let row_start = mx_seg[..synth_marker]
        .rfind("~{'class_side' =>")
        .expect("synthetic row opens with ~{'class_side' =>");
    let row = &mx_seg[row_start..synth_marker];
    assert!(
        row.contains("'selector' => 'withX:'"),
        "the surviving synthetic row must be the `withX:` setter. Got:\n{row}"
    );
}

/// ADR 0087 Phase 2 (BT-2298): a send to a selector longer than the 255-byte
/// Erlang atom limit (e.g. a 20-keyword auto-constructor) must be dropped from
/// the xref `sends` list — emitting it as an atom would fail `core_scan` at
/// BEAM-compile time. The generated Core Erlang must still be well-formed.
#[test]
fn test_method_xref_drops_oversized_selectors() {
    // Build a keyword send whose concatenated selector exceeds 255 bytes.
    use std::fmt::Write as _;
    let mut send_parts = String::new();
    for i in 0..40 {
        write!(send_parts, " longKeywordPartNumber{i}: x{i}").unwrap();
    }
    let src = format!(
        concat!(
            "Actor subclass: BigSend\n",
            "  state: count = 0\n\n",
            "  run: target =>\n    target{}\n"
        ),
        send_parts
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("big_send")).expect("codegen should succeed");

    // The methodXref field is still emitted, and the oversized selector is
    // dropped from it: the single method's `sends` list is empty.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];
    assert!(
        !mx_seg.contains("longKeywordPartNumber"),
        "oversized selector must be dropped from methodXref. Got:\n{mx_seg}"
    );
    assert!(
        mx_seg.contains("'sends' => []"),
        "the only send was oversized, so sends should be empty. Got:\n{mx_seg}"
    );
}

// ── BT-2499: initialize chain codegen coverage ────────────────────────────

/// BT-1417/BT-1541: When an Actor defines an `initialize` method, `init/1`
/// must NOT call it inline. Instead it emits a `__skip_initialize__` guard
/// and returns `{'ok', CleanState1, {'continue', 'initialize'}}` so OTP
/// invokes `handle_continue/2` after the message loop starts, avoiding
/// deadlock on self-sends from within initialize.
#[test]
fn test_actor_with_initialize_defers_to_handle_continue() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n\n",
        "  initialize =>\n",
        "    self.value := 10\n\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // init/1 must contain the __skip_initialize__ guard (BT-1541) so that when
    // a subclass calls this as a parent state-builder, initialize is not
    // dispatched a second time.
    assert!(
        code.contains("'__skip_initialize__'"),
        "init/1 must guard against double-dispatch with __skip_initialize__. Got:\n{code}"
    );

    // The non-helper branch must return {ok, State, {continue, initialize}} to
    // hand off to handle_continue.
    assert!(
        code.contains("{'continue', 'initialize'}"),
        "init/1 must return {{continue, initialize}} to defer initialize dispatch. Got:\n{code}"
    );

    // The CleanState variants strip the flag from state before returning.
    assert!(
        code.contains("'__skip_initialize__', FinalState"),
        "init/1 must strip __skip_initialize__ flag from FinalState. Got:\n{code}"
    );
}

/// BT-1951 (ADR 0078): When an Actor defines `initialize`, `handle_continue/2`
/// must build a pdict-stash + `safe_dispatch` loop so each class in the
/// initialize chain gets a chance to run. Verifies the pdict stash/restore,
/// the `safe_dispatch` call, and the final `noreply` return.
#[test]
fn test_handle_continue_dispatches_initialize_chain() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n\n",
        "  initialize =>\n",
        "    self.value := 10\n\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // The callback function must be present.
    assert!(
        code.contains("'handle_continue'/2 = fun (Continue, State) ->"),
        "handle_continue/2 must be generated. Got:\n{code}"
    );

    // The <'initialize'> pattern dispatches the chain.
    assert!(
        code.contains("<'initialize'> when 'true' ->"),
        "handle_continue/2 must match on 'initialize' continuation. Got:\n{code}"
    );

    // BT-1325: pdict stash/restore brackets every safe_dispatch call to
    // preserve re-entrant self-send semantics inside initialize.
    assert!(
        code.contains("'$bt_actor_state'"),
        "handle_continue/2 must stash/restore $bt_actor_state for re-entrant sends. Got:\n{code}"
    );

    // The chain dispatches initialize via safe_dispatch on the class module.
    assert!(
        code.contains("'safe_dispatch'('initialize',"),
        "handle_continue/2 must dispatch 'initialize' via safe_dispatch. Got:\n{code}"
    );

    // On success (the reply arm), the outer result is a noreply continuation.
    assert!(
        code.contains("'noreply'"),
        "handle_continue/2 must return noreply on successful initialize. Got:\n{code}"
    );
}

/// BT-1417: When a class inherits from a user-defined Actor (not directly
/// from `Actor`), `init/1` must call the parent's `init/1` to accumulate
/// inherited state, then merge the child's own fields on top, and propagate
/// any `{error, Reason}` the parent returns.
///
/// BT-2768: This is also the cross-file inherited-state regression coverage.
/// The parent (`Counter`) is compiled in a *separate* module — its AST is absent
/// here — yet the child correctly pulls the parent's state via `bt@counter:init/1`.
/// This is why the old AST-only `collect_inherited_fields` was removed: the
/// super-init chain already handles cross-file / stdlib / package parents.
#[test]
fn test_init_parent_actor_subclass_calls_parent_init() {
    // LoggingCounter extends Counter (itself an Actor subclass).
    // Compiling only LoggingCounter — Counter's AST is absent, but the
    // superclass name "Counter" != "Actor" triggers the parent-init path.
    let src = concat!(
        "Counter subclass: LoggingCounter\n",
        "  state: logCount = 0\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("logging_counter"))
        .expect("codegen should succeed");

    // init/1 must delegate to the parent module's init/1.
    assert!(
        code.contains("'bt@counter':'init'("),
        "init/1 must call parent bt@counter:init/1. Got:\n{code}"
    );

    // The parent's returned state is bound and then merged with child fields.
    assert!(
        code.contains("ParentState"),
        "init/1 must bind parent state as ParentState. Got:\n{code}"
    );
    assert!(
        code.contains("ChildFields"),
        "init/1 must create ChildFields map for child-only state. Got:\n{code}"
    );
    assert!(
        code.contains("MergedState"),
        "init/1 must merge parent and child state into MergedState. Got:\n{code}"
    );
    assert!(
        code.contains("FinalState"),
        "init/1 must produce FinalState (MergedState + InitArgs overrides). Got:\n{code}"
    );

    // Parent init errors must be propagated, not swallowed.
    assert!(
        code.contains("{'error', Reason}"),
        "init/1 must propagate parent {{error, Reason}} without modification. Got:\n{code}"
    );

    // The child's own state field must appear in ChildFields.
    assert!(
        code.contains("'logCount'"),
        "ChildFields must include the child's own logCount state field. Got:\n{code}"
    );
}

// ── Type-annotation codegen coverage ─────────────────────────────────────────
//
// Target: gen_server/callbacks.rs — is_nilable_type Union branch,
// type_annotation_display Singleton/Generic/FalseOr/SelfType/SelfClass/ClassOf
// variants, user_defined_initialize_chain fallback when class_hierarchy is None,
// and inherited_typed_no_default_fields fallback.
// (Function names rather than line numbers so these references don't drift as
// callbacks.rs evolves.)
//
// Strategy:
// - Tests 1-6: generate_module with actor having one typed-no-default field per
//   TypeAnnotation variant; coverage comes from the hierarchy path in
//   inherited_typed_no_default_fields that calls is_nilable_type and
//   type_annotation_display.
// - Tests 7-15: direct CoreErlangGenerator unit tests (class_hierarchy = None)
//   to exercise the no-hierarchy fallback paths in both functions.

/// Shared helper: a single-class Actor Module with one typed-no-default state field.
fn make_actor_typed_no_default(field_name: &str, ty: TypeAnnotation) -> Module {
    let s = Span::new(0, 0);
    let class = ClassDefinition {
        name: Identifier::new("TestActor", s),
        superclass: Some(Identifier::new("Actor", s)),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new(field_name, s),
            type_annotation: Some(ty),
            default_value: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: s,
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
        span: s,
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s,
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_actor_typed_union_nil_field_is_nilable() {
    // is_nilable_type Union branch: Union([Integer, Nil]) is nilable.
    // The field is excluded from typed-no-default so no initialize continuation emitted.
    let s = Span::new(0, 0);
    let union_nil = TypeAnnotation::union(
        vec![
            TypeAnnotation::simple("Integer", s),
            TypeAnnotation::simple("Nil", s),
        ],
        s,
    );
    let module = make_actor_typed_no_default("optValue", union_nil);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("{'continue', 'initialize'}"),
        "Nil-union field is nilable so no initialize continuation is needed. Got:\n{code}"
    );
    // Positive guard (symmetric with test 2): a nilable field must NOT emit the
    // typed-no-default validation. Without this, the negative assertion above
    // would pass trivially if init generation broke for any unrelated reason.
    assert!(
        !code.contains("'uninitialized_state_error'"),
        "Nilable Union field should not trigger typed-no-default validation. Got:\n{code}"
    );
    assert!(
        code.contains("'init'"),
        "init/1 callback should still be generated for the actor. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_union_non_nil_field_triggers_validation() {
    // is_nilable_type Union branch: Union([Integer, String]) is not nilable → included.
    // type_annotation_display Union branch also exercised.
    let s = Span::new(0, 0);
    let union_no_nil = TypeAnnotation::union(
        vec![
            TypeAnnotation::simple("Integer", s),
            TypeAnnotation::simple("String", s),
        ],
        s,
    );
    let module = make_actor_typed_no_default("combo", union_no_nil);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Non-nil Union field should trigger typed-no-default validation. Got:\n{code}"
    );
    // BT-2717: the typed-no-default field-check path must also strip __local__
    // threading temps from the committed post-initialize state — the `let
    // InitCleanState = …` binding is emitted before the nested field-check case,
    // and the success arm replies with it.
    assert!(
        code.contains(
            "let InitCleanState = call 'beamtalk_actor':'strip_local_temps'(InitNewState) in"
        ),
        "typed-no-default post-init path must strip __local__ temps. Got:\n{code}"
    );
    assert!(
        code.contains("{'noreply', InitCleanState}"),
        "typed-no-default post-init success arm must reply with the cleaned state. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_singleton_field_triggers_validation() {
    // type_annotation_display Singleton branch.
    let s = Span::new(0, 0);
    let singleton = TypeAnnotation::Singleton {
        name: "ok".into(),
        span: s,
    };
    let module = make_actor_typed_no_default("status", singleton);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Singleton-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_generic_field_triggers_validation() {
    // type_annotation_display Generic branch.
    let s = Span::new(0, 0);
    let generic = TypeAnnotation::generic(
        Identifier::new("Collection", s),
        vec![TypeAnnotation::simple("Integer", s)],
        s,
    );
    let module = make_actor_typed_no_default("items", generic);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Generic-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_false_or_field_triggers_validation() {
    // type_annotation_display FalseOr branch.
    let s = Span::new(0, 0);
    let false_or = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", s), s);
    let module = make_actor_typed_no_default("result", false_or);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "FalseOr-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_class_of_field_triggers_validation() {
    // type_annotation_display ClassOf branch.
    let s = Span::new(0, 0);
    let class_of = TypeAnnotation::ClassOf {
        class_name: Identifier::new("Actor", s),
        span: s,
    };
    let module = make_actor_typed_no_default("actorClass", class_of);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "ClassOf-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_user_defined_initialize_chain_fallback_with_initialize() {
    // user_defined_initialize_chain fallback: class_hierarchy is
    // None, actor defines initialize → fallback returns chain containing the leaf.
    let src = concat!(
        "Actor subclass: TestActor\n",
        "  state: value = 0\n\n",
        "  initialize =>\n",
        "    self.value := 42\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let chain = generator.user_defined_initialize_chain(&module, "TestActor");
    assert_eq!(
        chain.len(),
        1,
        "Fallback should return one entry for the initialize method"
    );
    assert_eq!(
        chain[0].class_name, "TestActor",
        "Chain entry should name the leaf class"
    );
}

#[test]
fn test_user_defined_initialize_chain_fallback_without_initialize() {
    // user_defined_initialize_chain fallback: class_hierarchy is
    // None, no initialize method → fallback returns empty chain.
    let src = concat!(
        "Actor subclass: TestActor\n",
        "  state: value = 0\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let chain = generator.user_defined_initialize_chain(&module, "TestActor");
    assert!(
        chain.is_empty(),
        "No initialize method should produce an empty fallback chain"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_union_nil_excluded() {
    // inherited_typed_no_default_fields fallback: Union([Integer, Nil])
    // is nilable → field excluded from the typed-no-default list.
    let s = Span::new(0, 0);
    let union_nil = TypeAnnotation::union(
        vec![
            TypeAnnotation::simple("Integer", s),
            TypeAnnotation::simple("Nil", s),
        ],
        s,
    );
    let module = make_actor_typed_no_default("optValue", union_nil);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert!(
        fields.is_empty(),
        "Nil-union field is nilable so it should be excluded from typed-no-default"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_singleton_type_display() {
    // inherited_typed_no_default_fields fallback: Singleton type annotation →
    // type_annotation_display returns "#ok" → field included with the correct
    // display name.
    let s = Span::new(0, 0);
    let singleton = TypeAnnotation::Singleton {
        name: "ok".into(),
        span: s,
    };
    let module = make_actor_typed_no_default("status", singleton);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "Singleton field should appear in typed-no-default"
    );
    assert_eq!(fields[0].field_name, "status");
    assert_eq!(
        fields[0].type_name, "#ok",
        "Singleton display should be '#ok'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_self_type_display() {
    // type_annotation_display SelfType branch.
    let s = Span::new(0, 0);
    let self_type = TypeAnnotation::SelfType { span: s };
    let module = make_actor_typed_no_default("selfRef", self_type);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "SelfType field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Self",
        "SelfType display should be 'Self'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_self_class_display() {
    // type_annotation_display SelfClass branch.
    let s = Span::new(0, 0);
    let self_class = TypeAnnotation::SelfClass { span: s };
    let module = make_actor_typed_no_default("classRef", self_class);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "SelfClass field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Self class",
        "SelfClass display should be 'Self class'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_generic_type_display() {
    // type_annotation_display Generic branch: directly assert the fallback
    // display string (the generate_module test only checks it indirectly via
    // emitted Core Erlang).
    let s = Span::new(0, 0);
    let generic = TypeAnnotation::generic(
        Identifier::new("Collection", s),
        vec![TypeAnnotation::simple("Integer", s)],
        s,
    );
    let module = make_actor_typed_no_default("items", generic);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "Generic field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Collection(Integer)",
        "Generic display should be 'Collection(Integer)'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_class_of_type_display() {
    // type_annotation_display ClassOf branch: directly assert the fallback
    // display string (the generate_module test only checks it indirectly via
    // emitted Core Erlang).
    let s = Span::new(0, 0);
    let class_of = TypeAnnotation::ClassOf {
        class_name: Identifier::new("Actor", s),
        span: s,
    };
    let module = make_actor_typed_no_default("actorClass", class_of);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "ClassOf field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Actor class",
        "ClassOf display should be 'Actor class'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_false_or_type_display() {
    // type_annotation_display FalseOr branch: directly assert the fallback
    // display string (the generate_module test only checks it indirectly via
    // emitted Core Erlang).
    let s = Span::new(0, 0);
    let false_or = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", s), s);
    let module = make_actor_typed_no_default("result", false_or);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "FalseOr field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Integer | False",
        "FalseOr display should be 'Integer | False'"
    );
}

#[test]
fn test_bt_2720_native_object_instance_delegate_lowers_to_native_call() {
    // ADR 0101 / BT-2720: an instance-side `self delegate` on a `native:`
    // Object lowers through beamtalk_erlang_proxy:native_call/4, prepending
    // Self and carrying {Class, Sel} context.
    let src = concat!(
        "Object subclass: Stream native: beamtalk_stream\n",
        "  select: predicate :: Block -> Object => self delegate\n",
        "  asList -> Object => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains(
            "call 'beamtalk_erlang_proxy':'native_call'('beamtalk_stream', 'select', [Self, "
        ),
        "select: should lower to native_call('beamtalk_stream', 'select', [Self, Pred], ...). Got:\n{code}"
    );
    assert!(
        code.contains("{'Stream', 'select:'}"),
        "native_call should carry {{Class, Sel}} = {{'Stream', 'select:'}}. Got:\n{code}"
    );
    // Unary delegate: asList -> native_call(..., 'asList', [Self], {'Stream', 'asList'})
    assert!(
        code.contains("call 'beamtalk_erlang_proxy':'native_call'('beamtalk_stream', 'asList', [Self], {'Stream', 'asList'})"),
        "asList should lower to native_call('beamtalk_stream', 'asList', [Self], {{'Stream', 'asList'}}). Got:\n{code}"
    );
    // Must NOT emit a bare module:fn call for the delegate body.
    assert!(
        !code.contains("call 'beamtalk_stream':'select'"),
        "native: delegate must route through the proxy, not a bare beamtalk_stream:select. Got:\n{code}"
    );
}

#[test]
fn test_bt_2720_native_object_class_delegate_omits_self() {
    // ADR 0101 / BT-2720: a class-side `self delegate` omits self from the arg
    // list (class methods are not instances).
    let src = concat!(
        "Object subclass: Stream native: beamtalk_stream\n",
        "  class from: start :: Integer -> Object => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_erlang_proxy':'native_call'('beamtalk_stream', 'from', [")
            && code.contains("], {'Stream', 'from:'})"),
        "class from: should lower to native_call('beamtalk_stream', 'from', [Start], {{'Stream', 'from:'}}). Got:\n{code}"
    );
    // The class-side arg list must omit ClassSelf / ClassVars (class methods
    // are not instances).
    assert!(
        !code.contains("'native_call'('beamtalk_stream', 'from', [ClassSelf"),
        "class-side native_call must omit ClassSelf from the arg list. Got:\n{code}"
    );
}

/// BT-nightly: When an Actor subclass has BOTH an intermediate parent (`has_parent_init=true`)
/// AND its own `initialize` method (`has_initialize=true`), `init/1` must call the parent's
/// init, merge state, and then use the `__skip_initialize__` guard to defer initialize
/// dispatch to `handle_continue` — not call it inline.
#[test]
fn test_actor_with_parent_init_and_initialize_defers_to_handle_continue() {
    let src = concat!(
        "Counter subclass: LoggingInitCounter\n",
        "  state: logCount = 0\n\n",
        "  initialize =>\n",
        "    self.logCount := 0\n\n",
        "  increment =>\n",
        "    self.logCount := self.logCount + 1\n",
        "    super increment\n\n",
        "  getLogCount => self.logCount\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("logging_init_counter"))
        .expect("codegen should succeed");

    // init/1 must delegate to the parent module's init/1.
    assert!(
        code.contains("'bt@counter':'init'("),
        "init/1 must call parent bt@counter:init/1. Got:\n{code}"
    );

    // Parent state must be merged with child fields.
    assert!(
        code.contains("ParentState"),
        "init/1 must bind parent state as ParentState. Got:\n{code}"
    );
    assert!(
        code.contains("FinalState"),
        "init/1 must produce FinalState. Got:\n{code}"
    );

    // Because initialize is defined, init/1 must use the __skip_initialize__ guard
    // and defer to handle_continue — NOT call initialize inline.
    assert!(
        code.contains("'__skip_initialize__'"),
        "init/1 must guard initialize dispatch with __skip_initialize__. Got:\n{code}"
    );
    assert!(
        code.contains("{'continue', 'initialize'}"),
        "init/1 must return {{continue, initialize}} to defer initialize. Got:\n{code}"
    );

    // handle_continue must exist and dispatch initialize.
    assert!(
        code.contains("'handle_continue'/2"),
        "Module must export handle_continue/2. Got:\n{code}"
    );
    assert!(
        code.contains("'safe_dispatch'('initialize'"),
        "handle_continue must dispatch initialize via safe_dispatch. Got:\n{code}"
    );
}

// ── Cross-file ancestor ClassInfo path ───────────────────────────────────────
//
// Target: gen_server/callbacks.rs — `is_nilable_type_name` and the ClassInfo
// branch of `inherited_typed_no_default_fields` (the `else if let Some(info)
// = hierarchy.get_class(&name)` arm).  Reached only when an ancestor class is
// absent from the current module's AST but present in the pre-loaded
// ClassHierarchy (BEAM metadata / cross-file compilation).

/// Exercises `is_nilable_type_name()` via the `ClassInfo` path in
/// `inherited_typed_no_default_fields()`.
///
/// A cross-file ancestor is injected via `CodegenOptions::with_class_hierarchy`.
/// Its typed-no-default fields exercise every branch of `is_nilable_type_name`:
///
/// - `nilField :: Nil`          → `type_name == "Nil"` returns true → excluded
/// - `nilUnionField :: Integer | Nil` → union `split(" | ").any(…)` → excluded
/// - `reqField :: Integer`       → neither branch → included → validation fires
///
/// The validation output for `reqField` confirms the `ClassInfo` loop ran.
/// The absence of `nilField` / `nilUnionField` in the output confirms the
/// nilability guards work correctly.
#[test]
fn test_cross_file_ancestor_nil_typed_fields_excluded_from_validation() {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
    use std::collections::HashMap;

    let ancestor = ClassInfo {
        surface_incomplete: false,
        name: ecow::EcoString::from("BaseActor"),
        superclass: Some(ecow::EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![
            ecow::EcoString::from("nilField"),
            ecow::EcoString::from("nilUnionField"),
            ecow::EcoString::from("reqField"),
        ],
        state_types: {
            let mut m = HashMap::new();
            m.insert(
                ecow::EcoString::from("nilField"),
                beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::parse("Nil"),
            );
            m.insert(
                ecow::EcoString::from("nilUnionField"),
                beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::parse(
                    "Integer | Nil",
                ),
            );
            m.insert(
                ecow::EcoString::from("reqField"),
                beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::parse("Integer"),
            );
            m
        },
        state_has_default: {
            let mut m = HashMap::new();
            m.insert(ecow::EcoString::from("nilField"), false);
            m.insert(ecow::EcoString::from("nilUnionField"), false);
            m.insert(ecow::EcoString::from("reqField"), false);
            m
        },
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    // LogChild extends cross-file BaseActor; only LogChild's AST is present.
    let src = "BaseActor subclass: LogChild\n  logCount = 0\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let result = generate_module(
        &module,
        CodegenOptions::new("bt@log_child").with_class_hierarchy(vec![ancestor]),
    );
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();

    // reqField :: Integer is not nilable → typed-no-default validation must fire.
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Non-nilable cross-file ancestor field must trigger typed-no-default validation. Got:\n{code}"
    );
    assert!(
        code.contains("'reqField'"),
        "Non-nilable field 'reqField' must appear in the validation error hint. Got:\n{code}"
    );

    // nilField :: Nil — excluded by `type_name == "Nil"` branch of is_nilable_type_name.
    assert!(
        !code.contains("'nilField'"),
        "Nil-typed field must be excluded by is_nilable_type_name. Got:\n{code}"
    );

    // nilUnionField :: Integer | Nil — excluded by the union-split branch.
    assert!(
        !code.contains("'nilUnionField'"),
        "Integer|Nil union field must be excluded by is_nilable_type_name. Got:\n{code}"
    );

    // Because BaseActor ≠ Actor/Object (has_parent_init=true) AND reqField is a
    // typed-no-default field (has_initialize=true via chain_has_typed_no_default),
    // init/1 must call the parent and defer to handle_continue.
    assert!(
        code.contains("'bt@base_actor':'init'("),
        "init/1 must call parent bt@base_actor:init/1 for has_parent_init path. Got:\n{code}"
    );
    assert!(
        code.contains("{'continue', 'initialize'}"),
        "init/1 must return {{continue, initialize}} to defer post-initialize check. Got:\n{code}"
    );
}

// ── code_change / terminate codegen coverage ─────────────────────────────────
//
// Target: gen_server/callbacks.rs generate_code_change (lines 1583-1599) and
// generate_terminate (lines 1618-1675) — zero coverage in the 2026-07-20 CI run.
//
// Strategy: exercise both functions via generate_module on a minimal Actor class
// and via generate() on the plain-module path, asserting on the key fragments
// that each function is responsible for emitting.

#[test]
fn test_code_change_delegates_to_beamtalk_hot_reload() {
    // generate_code_change must emit 'code_change'/3 that delegates entirely to
    // beamtalk_hot_reload:code_change/3 for OTP hot-code-reload state migration.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("'code_change'/3"),
        "Module must export code_change/3. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_hot_reload':'code_change'(OldVsn, State, Extra)"),
        "code_change/3 must delegate to beamtalk_hot_reload:code_change/3. Got:\n{code}"
    );
}

#[test]
fn test_terminate_lifecycle_stop_telemetry() {
    // generate_terminate must emit lifecycle-stop telemetry (BT-1638) via
    // beamtalk_actor:maybe_execute_telemetry with the 'stop' event path.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("'terminate'/2"),
        "Module must export terminate/2. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'maybe_execute_telemetry'("),
        "terminate/2 must emit lifecycle telemetry. Got:\n{code}"
    );
    assert!(
        code.contains("'lifecycle', 'stop']"),
        "terminate/2 telemetry must include the lifecycle 'stop' event name. Got:\n{code}"
    );
}

#[test]
fn test_terminate_uses_class_name_for_telemetry_metadata() {
    // BT-1642: terminate/2 telemetry 'class' metadata must use the clean Beamtalk
    // class name (e.g. 'EventStore'), not the compiled module name (e.g.
    // 'bt@event_store'). This matches how dispatch traces report class names.
    let src = "Actor subclass: EventStore\n  state: count = 0\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("bt@event_store"))
        .expect("codegen should succeed");
    assert!(
        code.contains("'class' => 'EventStore'"),
        "terminate/2 must use class name 'EventStore' in telemetry metadata. Got:\n{code}"
    );
}

#[test]
fn test_terminate_wraps_dispatch_in_try_catch() {
    // generate_terminate must wrap the 'terminate:' method dispatch in try-catch
    // so that user exceptions cannot prevent OTP gen_server shutdown (BT-29).
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("let _TermDisp = try call"),
        "terminate: dispatch must be the body of the try expression. Got:\n{code}"
    );
    assert!(
        code.contains("catch <_TermT, _TermE, _TermS> -> 'ok'"),
        "terminate/2 catch clause must swallow all exceptions and return ok. Got:\n{code}"
    );
    assert!(
        code.contains("'terminate:'"),
        "terminate/2 must dispatch the 'terminate:' method. Got:\n{code}"
    );
}

#[test]
fn test_terminate_calls_make_self_before_dispatch() {
    // terminate/2 must build a self-object via beamtalk_actor:make_self before
    // calling dispatch so the Beamtalk object is available to terminate: handlers.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("call 'beamtalk_actor':'make_self'(State)"),
        "terminate/2 must call beamtalk_actor:make_self/1 to build the self-object. Got:\n{code}"
    );
    assert!(
        code.contains("in 'ok'"),
        "terminate/2 must end with 'ok' as its return value. Got:\n{code}"
    );
}

#[test]
fn test_terminate_plain_module_uses_module_name_as_class_label() {
    // When the module has no explicit class definition, generate_terminate falls
    // back to the module name as the class label in telemetry metadata.
    // generate() uses module name 'bt_module'.
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let result = generate(&module);
    assert!(
        result.is_ok(),
        "codegen should succeed for plain module: {result:?}"
    );
    let code = result.unwrap();
    assert!(
        code.contains("'terminate'/2"),
        "Plain module must still export terminate/2. Got:\n{code}"
    );
    assert!(
        code.contains("'class' => 'bt_module'"),
        "Plain module terminate/2 must use module name 'bt_module' as class label. Got:\n{code}"
    );
}

// ── Direct unit tests for dispatch.rs codegen helpers ───────────────────────
//
// These call `generate_class_name_function`, `generate_has_method`, and
// `generate_safe_dispatch` directly (without going through the full
// `generate_module` pipeline) to pin their exact Core Erlang output and to
// reach the macro-expanded lines that full-pipeline tests miss.

#[test]
fn test_generate_class_name_function_derives_from_module_name() {
    // CoreErlangGenerator::class_name() converts the module name from
    // snake_case → CamelCase when no explicit class identity is set.
    let generator = CoreErlangGenerator::new("my_counter");
    let module = Module::new(vec![], Span::new(0, 0));
    // _module arg is unused by production code; only self.module_name matters
    let doc = generator.generate_class_name_function(&module).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'class_name'/0 = fun () -> 'MyCounter'"),
        "class_name/0 should return CamelCase atom from module name. Got: {output}"
    );
}

#[test]
fn test_generate_class_name_function_single_word_module() {
    // Single-word module name: "counter" → "Counter".
    let generator = CoreErlangGenerator::new("counter");
    let module = Module::new(vec![], Span::new(0, 0));
    // _module arg is unused by production code; only self.module_name matters
    let doc = generator.generate_class_name_function(&module).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'class_name'/0 = fun () -> 'Counter'"),
        "class_name/0 should return 'Counter' for module 'counter'. Got: {output}"
    );
}

#[test]
fn test_generate_has_method_empty_module_produces_empty_member_list() {
    // An empty module has no methods; has_method/1 should always return false
    // (member of an empty list is always false).
    let generator = CoreErlangGenerator::new("counter");
    let module = Module::new(vec![], Span::new(0, 0));
    let doc = generator.generate_has_method(&module).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'has_method'/1 = fun (Selector) ->"),
        "Should generate has_method/1 header. Got: {output}"
    );
    assert!(
        output.contains("call 'lists':'member'(Selector, [])"),
        "Empty module should yield empty member list. Got: {output}"
    );
}

#[test]
fn test_generate_has_method_lists_primary_class_methods() {
    // A module with an Actor class should list all primary methods in has_method/1.
    use beamtalk_core::ast::{ClassDefinition, MethodDefinition, MethodKind};

    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![
            MethodDefinition {
                selector: MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(0),
                    Span::new(0, 0),
                ))],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
            MethodDefinition {
                selector: MessageSelector::Keyword(vec![KeywordPart::new(
                    "setValue:",
                    Span::new(0, 0),
                )]),
                parameters: vec![ParameterDefinition {
                    name: Identifier::new("value", Span::new(0, 0)),
                    type_annotation: None,
                }],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(0),
                    Span::new(0, 0),
                ))],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
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
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'increment'"),
        "has_method/1 should list 'increment'. Got: {output}"
    );
    assert!(
        output.contains("'setValue:'"),
        "has_method/1 should list 'setValue:'. Got: {output}"
    );
    assert!(
        output.contains("call 'lists':'member'(Selector, ["),
        "has_method/1 should call lists:member on the method list. Got: {output}"
    );
}

#[test]
fn test_generate_has_method_from_expression_based_module() {
    // Script/workspace modules use top-level `name := [block]` assignments as
    // methods; has_method/1 must include those names.
    let src = "increment := [self.value + 1]. getValue := [self.value]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'increment'"),
        "has_method/1 should include 'increment' from script method. Got: {output}"
    );
    assert!(
        output.contains("'getValue'"),
        "has_method/1 should include 'getValue' from script method. Got: {output}"
    );
}

#[test]
fn test_generate_safe_dispatch_structure() {
    // safe_dispatch/3 must wrap dispatch/4 in a try/catch that returns the
    // stacktrace on failure (BT-1822) and calls beamtalk_actor:make_self/1 first
    // (BT-161). The generated call must reference the module's own dispatch fn.
    let mut generator = CoreErlangGenerator::new("my_counter");
    let doc = generator.generate_safe_dispatch().unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("'safe_dispatch'/3 = fun (Selector, Args, State) ->"),
        "Should generate safe_dispatch/3 header. Got: {output}"
    );
    // BT-161: Self must be constructed via make_self before dispatch
    assert!(
        output.contains("call 'beamtalk_actor':'make_self'(State)"),
        "Should construct Self via make_self/1. Got: {output}"
    );
    // The try must call the module's own dispatch function
    assert!(
        output.contains("'my_counter':'dispatch'(Selector, Args, Self, State)"),
        "Should dispatch to my_counter:dispatch/4. Got: {output}"
    );
    // The try/catch structure
    assert!(
        output.contains("try call"),
        "Should use try/catch for error isolation. Got: {output}"
    );
    assert!(
        output.contains("of Result -> Result"),
        "Happy path should pass Result through. Got: {output}"
    );
    // BT-1822: stacktrace captured and returned in error tuple
    assert!(
        output.contains("catch <Type, Error, Stacktrace>"),
        "Should catch with stacktrace variable. Got: {output}"
    );
    assert!(
        output.contains("{'error', {Type, Error, Stacktrace}, State}"),
        "Should return error tuple containing the stacktrace. Got: {output}"
    );
}

#[test]
fn test_script_module_keyword_method_dispatch_destructures_args() {
    // A script/workspace module with a multi-parameter block (keyword method)
    // must generate a dispatch clause that:
    //   1. Matches the selector atom in case Selector of
    //   2. Matches Args as a list of named variables (Args destructuring)
    //   3. Falls back to 'bad_arity' on arg count mismatch
    //
    // This exercises generate_legacy_method_clause with non-empty param_vars
    // (line 329) and build_dispatch_clause's Args-case branch.
    let src = "add := [:a :b | a + b]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    let dispatch_body =
        extract_core_fn(&code, "'dispatch'/4 = fun").expect("should have dispatch/4");
    assert!(
        dispatch_body.contains("<'add'>"),
        "dispatch/4 should have 'add' case arm. Got:\n{dispatch_body}"
    );
    // Args must be destructured into a list pattern when params are present
    assert!(
        dispatch_body.contains("case Args of"),
        "Keyword-style method must destructure Args. Got:\n{dispatch_body}"
    );
    assert!(
        dispatch_body.contains("<["),
        "Args case should pattern-match into a list. Got:\n{dispatch_body}"
    );
    assert!(
        dispatch_body.contains("'bad_arity'"),
        "Should fall back to 'bad_arity' on arity mismatch. Got:\n{dispatch_body}"
    );
}

#[test]
fn test_method_table_with_script_methods_includes_arity() {
    // Script/workspace modules emit method_table entries for every
    // `name := [block]` binding, with the block arity as the value.
    let src = "unary := [42]. binary := [:a :b | a + b]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    let table_body =
        extract_core_fn(&code, "'method_table'/0 = fun").expect("should have method_table/0");
    assert!(
        table_body.contains("'unary' => 0"),
        "method_table should list 'unary' with arity 0. Got:\n{table_body}"
    );
    assert!(
        table_body.contains("'binary' => 2"),
        "method_table should list 'binary' with arity 2. Got:\n{table_body}"
    );
}

// ─── BT-2998: bare `new` on an opaque `native:` class ────────────────────────

#[test]
fn test_bt_2998_native_class_without_fields_raises_on_new() {
    // A `native:` class keeps its state in the shape its backing module
    // defines, so `basicNew`'s `~{'$beamtalk_class' => 'X'}~` is a hollow
    // instance every later method call trips over. Refuse it up front.
    let src = concat!(
        "Value subclass: DateTime native: beamtalk_datetime\n",
        "  class sealed now -> DateTime => self delegate\n",
        "  class sealed monotonicNow -> Integer => self delegate\n",
        "  class sealed fromString: str :: String -> DateTime => self delegate\n",
        "  year -> Integer => self delegate\n",
    );
    let code = super::codegen(src);

    assert!(
        code.contains("call 'beamtalk_error':'new'('instantiation_error', 'DateTime')"),
        "bare new on an opaque native class must raise instantiation_error. Got:\n{code}"
    );
    assert!(
        !code.contains("~{'$beamtalk_class' => 'DateTime'}~"),
        "must not still build the hollow tagged map. Got:\n{code}"
    );
    // (The hint text itself is a Core Erlang binary literal, so it is asserted
    // on in `value_type_codegen`'s unit tests rather than the emitted code.)
    // `new:` merges over `new`, so it is just as hollow and refuses too.
    assert!(
        code.contains("'new'/1 = fun (_InitArgs) ->"),
        "new/1 must also refuse rather than merge over a hollow default. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_error':'with_selector'(Error0, 'new:')"),
        "the new/1 refusal must name selector 'new:'. Got:\n{code}"
    );
}

#[test]
fn test_bt_2998_native_class_with_declared_fields_still_builds_default_instance() {
    // A `native:` class that *does* declare fields has a real default
    // instance (`Package`, `SupervisionNode`), so `basicNew` stays correct.
    let src = concat!(
        "Value subclass: Package native: beamtalk_package\n",
        "  field: name = nil\n",
        "  class sealed named: n :: String -> Package => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains("'$beamtalk_class' => 'Package'"),
        "field-carrying native class must still build its default map. Got:\n{code}"
    );
    assert!(
        !code.contains("'instantiation_error', 'Package'"),
        "field-carrying native class must not refuse new. Got:\n{code}"
    );
}

#[test]
fn test_bt_2998_native_class_with_own_class_new_keeps_it() {
    // `Random`/`Queue` declare a working zero-arg `new`; `new/0` must keep
    // delegating to it rather than being replaced by the refusal.
    let src = concat!(
        "Value subclass: Random native: beamtalk_random\n",
        "  class sealed new -> Random => self delegate\n",
    );
    let code = super::codegen(src);
    let new_body = extract_core_fn(&code, "'new'/0 = fun").expect("should have new/0");
    assert!(
        new_body.contains("'class_new'('undefined', 'undefined')"),
        "new/0 must delegate to the declared class method. Got:\n{new_body}"
    );
    assert!(
        !new_body.contains("instantiation_error"),
        "a class with its own `new` must not get the refusal. Got:\n{new_body}"
    );
}

#[test]
fn test_bt_2998_non_native_value_class_unaffected() {
    // Plain value types build their instance from field defaults as before.
    let src = concat!(
        "Value subclass: Point\n",
        "  field: x = 0\n",
        "  field: y = 0\n",
    );
    let code = super::codegen(src);
    assert!(
        !code.contains("'instantiation_error', 'Point'"),
        "non-native value class must still be constructible. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class' => 'Point'"),
        "non-native value class must build its default map. Got:\n{code}"
    );
}

#[test]
fn test_bt_2998_opaque_native_class_registers_as_non_constructible() {
    // BT-877's compile-time `isConstructible` flag must agree with the
    // now-raising `new/0`, instead of leaving the runtime to discover it.
    let src = concat!(
        "Value subclass: Uuid native: beamtalk_uuid\n",
        "  class sealed v4 -> Uuid => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains("'isConstructible' => 'false'"),
        "opaque native class must register isConstructible => false. Got:\n{code}"
    );
}

// ── Foreign cross-class extension codegen (gen_server/extensions.rs) ──────

/// BT-2250: A unary foreign extension on a stdlib value class generates a
/// `beamtalk_extensions:register/5` call with a 2-arity fun.
///
/// The target class (`String`) is not declared in this module, so the
/// standalone method is foreign and must be registered at load time.
#[test]
fn test_foreign_extension_unary_emits_register_with_2arity_fun() {
    let src = "String >> shout => self uppercase ++ \"!\"\n";
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_extensions':'register'"),
        "foreign extension should emit beamtalk_extensions:register. Got:\n{code}"
    );
    assert!(
        code.contains("'String'"),
        "foreign extension should register under the bare class name 'String'. Got:\n{code}"
    );
    assert!(
        code.contains("'shout'"),
        "foreign extension should register the selector atom. Got:\n{code}"
    );
    // Value-type targets use a 2-arity fun (ExtArgs + Self).
    assert!(
        code.contains("fun (_ExtArgs, Self)"),
        "value-type foreign extension fun must be 2-arity. Got:\n{code}"
    );
    assert!(
        !code.contains("fun (_ExtArgs, Self, State)"),
        "value-type foreign extension must NOT use the 3-arity actor fun shape. Got:\n{code}"
    );
}

/// BT-2250: A keyword foreign extension generates `_ExtArgs` list unpacking
/// for each declared parameter.
#[test]
fn test_foreign_extension_keyword_unpacks_ext_args() {
    let src = "String >> wrapWith: edge => edge ++ self ++ edge\n";
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_extensions':'register'"),
        "keyword foreign extension should emit register. Got:\n{code}"
    );
    assert!(
        code.contains("_ExtArgs"),
        "keyword foreign extension should reference _ExtArgs for parameter unpacking. Got:\n{code}"
    );
    // The first (and only) parameter is bound via erlang:hd(_ExtArgs).
    assert!(
        code.contains("'erlang':'hd'"),
        "first keyword parameter must be bound via erlang:hd(_ExtArgs). Got:\n{code}"
    );
}

/// BT-2250: A class-side foreign extension (`Target class >> sel`) registers
/// under the metaclass tag `'Target class'` (with a space), not the bare class
/// name. This is the established tag convention for metaclass registration.
#[test]
fn test_foreign_extension_class_side_uses_metaclass_tag() {
    let src = "String class >> banner => \"=== banner ===\"\n";
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_extensions':'register'"),
        "class-side foreign extension should emit register. Got:\n{code}"
    );
    // Metaclass tag uses a space: 'String class' (not 'Stringclass' or 'String').
    assert!(
        code.contains("'String class'"),
        "class-side extension must use the metaclass tag 'String class'. Got:\n{code}"
    );
    assert!(
        code.contains("'banner'"),
        "class-side extension should register the selector atom. Got:\n{code}"
    );
}

/// BT-2250: A self-extension (target class declared in the same module) is
/// folded into the host class module and must NOT emit a
/// `beamtalk_extensions:register` call.
#[test]
fn test_self_extension_not_registered_via_beamtalk_extensions() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n",
        "  get => value\n",
        "\n",
        "Counter >> getDouble => value * 2\n",
    );
    let code = super::codegen(src);
    assert!(
        !code.contains("call 'beamtalk_extensions':'register'"),
        "self-extension (Counter >> in the same module as Counter) must NOT emit \
         beamtalk_extensions:register. Got:\n{code}"
    );
}

#[test]
fn test_nested_foldl_self_send_in_inner_do_is_compile_error() {
    // BT-3172 audit (acceptance criteria bullet 3): the same silent-loss gap
    // reachable via `Foldl*`-in-`Foldl*` nesting (`do:`-in-`do:`), not just
    // `Letrec`-in-`Letrec`. Confirmed empirically to be WORSE than silent
    // loss before this fix: the outer `do:`'s own `plan.threads_class_vars`
    // comes back `true` (Foldl's gate is `body_analysis.has_self_sends`,
    // which — unlike `loop_body_threads_class_vars` — recurses into the
    // nested `do:`'s own block and finds `self bump`), so the outer fold
    // expects to build its own `{ClassVars, StateAcc}` accumulator wrap —
    // but the inner `do:`'s own `next_class_var()` mint (from
    // `ThreadingPlan::foldl_call_doc`) permanently advances the generator's
    // single, unscoped class-var-name counter without that name ever being
    // surfaced back to the outer scope, so the outer wrap references a name
    // that was only ever bound inside the inner `do:`'s own (already
    // exited) fold closure — an `erlc` "unbound variable" compile crash,
    // not silent data loss, but broken all the same. Rejected at compile
    // time instead — see `CodeGenError::ClassVarMutationLostAcrossNestedLoop`'s
    // doc comment.
    let src = "Value subclass: NestedFoldClassVarMutation\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedDo: aList =>\n    outerSeen := 0\n    aList\n      do: [:x |\n        total := 0\n        aList\n          do: [:y |\n            self bump\n            total := total + 1\n          ]\n        outerSeen := outerSeen + 1\n      ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedfoldclassvarmutation").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "'self bump'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a self-send inside a do: \
             nested inside another do:. Got: {other:?}"
        ),
    }
}

#[test]
fn test_nested_letrec_self_send_buried_in_conditional_compiles() {
    // BT-3172 review follow-up: a same-class self-send buried inside an
    // `ifTrue:` conditional (NOT a bare top-level statement) within an
    // inner `whileTrue:` that's itself nested inside an outer `whileTrue:`
    // must NOT be rejected. `Letrec`'s own real `threads_class_vars` gate
    // (`loop_body_threads_class_vars`) is narrowly top-level-only by
    // design — recursing into a conditional buried inside a `Letrec` body
    // is exactly the shape that predicate was narrowed to exclude (the
    // `class_var_subexpr.bt` `tickInLoopConditional` regression documented
    // on `loop_body_threads_class_vars` itself), and it's also the shape
    // `class_var_subexpr_test.bt`'s
    // `testTickInLoopConditionalCompilesAndRuns` already pins as
    // accepted, silently-non-threading behavior at a single loop level
    // (BT-2308, out of BT-3172's scope). The inner loop was never going to
    // attempt `ClassVars` threading for this self-send in the first place,
    // so nothing is "lost" here for the outer loop to fail to recover —
    // rejecting only the nested-loop variant of this exact same shape
    // would be an inconsistent new restriction. Mirrors
    // `tickInLoopConditional` one loop level deeper.
    let src = "Object subclass: NestedCondSelfSend\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class run: n =>\n    j := 0\n    [j < n] whileTrue: [\n      i := 0\n      [i < n] whileTrue: [\n        (i >= 0) ifTrue: [self bump]\n        i := i + 1\n      ]\n      j := j + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedcondselfsend").with_workspace_mode(true),
    );
    result.unwrap_or_else(|e| {
        panic!(
            "A self-send buried inside a conditional (not a bare top-level statement) \
             inside a Letrec loop nested inside another Letrec loop must not be rejected \
             by ClassVarMutationLostAcrossNestedLoop — Letrec's own real threading gate \
             never attempts to thread it in the first place. Got: {e:?}"
        )
    });
}

#[test]
fn test_nested_foldl_self_send_buried_in_conditional_is_compile_error() {
    // BT-3172 review follow-up (contrast case): the same "self-send buried
    // in a conditional, not a bare top-level statement" shape as the
    // Letrec test above, but inside a `Foldl*` (`do:`) body instead —
    // `Foldl*`'s own real `threads_class_vars` gate
    // (`!Actor && in_class_method() && body_analysis.has_self_sends`) IS
    // genuinely recursive (unlike Letrec's), so this shape must still be
    // rejected when nested inside another loop.
    let src = "Value subclass: NestedFoldCondSelfSend\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedDo: aList =>\n    outerSeen := 0\n    aList\n      do: [:x |\n        total := 0\n        aList\n          do: [:y |\n            (y >= 0) ifTrue: [self bump]\n            total := total + 1\n          ]\n        outerSeen := outerSeen + 1\n      ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedfoldcondselfsend").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { .. })
        ),
        "Expected ClassVarMutationLostAcrossNestedLoop for a self-send buried in a \
         conditional inside a do: nested inside another do: — Foldl*'s own real \
         threading gate is recursive, unlike Letrec's. Got: {result:?}"
    );
}

#[test]
fn test_nested_detect_self_send_in_inner_detect_is_compile_error() {
    // BT-3172 review follow-up: `nested_loop_or_fold_body` must also cover
    // the predicate-based `Foldl*` shapes (`detect:`/`count:`/`takeWhile:`/
    // `dropWhile:`/`partition:`/`groupBy:`), not just `do:`/`collect:`/
    // `select:`/`reject:`/`anySatisfy:`/`allSatisfy:`/`inject:into:` —
    // `ThreadingPlan::new_impl`'s `threads_class_vars` gate
    // (`!Actor && in_class_method() && body_analysis.has_self_sends`)
    // applies uniformly to every non-`Letrec` `BodyKind`, so a class-var
    // self-send nested inside `detect:`, itself nested inside another
    // `detect:`, is exactly as vulnerable to the silent-loss/`erlc`-crash
    // bug as the `do:`-in-`do:` shape pinned above.
    let src = "Value subclass: NestedDetectClassVarMutation\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedDetect: aList =>\n    outerSeen := 0\n    aList\n      detect: [:x |\n        total := 0\n        aList\n          detect: [:y |\n            self bump\n            total := total + 1\n            true\n          ]\n        outerSeen := outerSeen + 1\n        true\n      ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nesteddetectclassvarmutation").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "'self bump'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a self-send inside a detect: \
             nested inside another detect:. Got: {other:?}"
        ),
    }
}

#[test]
fn test_mixed_letrec_nested_in_foldl_is_compile_error() {
    // BT-3172 audit (acceptance criteria bullet 3): mixed nesting — a
    // `Letrec` (`whileTrue:`) loop with a direct class-var field write,
    // nested inside a `Foldl*` (`do:`) body — hits the same gap. The inner
    // `whileTrue:`'s own `ThreadingPlan::threads_class_vars` (Letrec's
    // narrow, top-level-only `loop_body_threads_class_vars` gate) is `true`
    // for its own bare `self.runs := self.runs + 1`, but nothing in the
    // outer `Foldl*` machinery (which only knows how to unpack its OWN
    // `{ClassVars, StateAcc}` accumulator shape, not a nested `Letrec`
    // loop's extra tail-call `ClassVars` fun parameter) surfaces that
    // mutation back out.
    let src = "Object subclass: MixedLetrecFoldCounter\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class mixedBumpUpTo: n =>\n    seen := 0\n    #(1) do: [:x |\n      i := 0\n      [i < n] whileTrue: [\n        self.runs := self.runs + 1\n        i := i + 1\n      ]\n      seen := seen + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@mixedletrecfoldcounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "class variable 'runs'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a whileTrue: (with a direct \
             class-var write) nested inside a do:. Got: {other:?}"
        ),
    }
}

#[test]
fn test_mixed_foldl_nested_in_letrec_is_compile_error() {
    // BT-3172 audit (acceptance criteria bullet 3): the reverse mixed
    // nesting — a `Foldl*` (`do:`) body with a direct class-var field
    // write, nested inside a `Letrec` (`whileTrue:`) loop. The inner `do:`
    // never reaches Foldl's own `ClassVars` threading at all here (a bare
    // field write inside a `Foldl*` body is unconditionally rejected by
    // `reject_class_var_field_assignment` regardless of nesting — see
    // `nested_loop_lost_class_var_mutation`'s doc comment), so this pins
    // that the OUTER `Letrec`'s own top-level dispatch catches the shape
    // before ever generating the inner `do:` at all.
    let src = "Value subclass: MixedFoldLetrecCounter\n  classState: runs = 0\n\n  class mixedBumpAll: aList =>\n    outerSeen := 0\n    [outerSeen < 2] whileTrue: [\n      aList do: [:x |\n        self.runs := self.runs + 1\n      ]\n      outerSeen := outerSeen + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@mixedfoldletreccounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "class variable 'runs'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a do: (with a direct class-var \
             write) nested inside a whileTrue:. Got: {other:?}"
        ),
    }
}

#[test]
fn test_class_builder_cascade_in_second_field_assignment_does_not_corrupt_state_version() {
    // BT-3289: a `classBuilder … addClassMethod:body:`/`classMethods:` cascade
    // lowers its block via `generate_class_method_fun_from_block`, which resets
    // the instance-`State` version counter (`reset_state_version`) to give the
    // class-method fun its own fresh count — but nothing saved/restored the
    // ENCLOSING method's counter around that reset. When such a cascade sits in
    // the value of a field-assignment that is not the method's first (i.e. an
    // earlier statement already minted a `State` version), the reset rewinds
    // the enclosing counter, and the enclosing assignment's own next-minted
    // version collides with the earlier one — an ADR-0111 `NonLinearVersion`
    // ThreadedIr-verify violation (`producers: 2, consumers: 1`), which used to
    // hard-panic via `report_threaded_ir_verify_errors`'s `debug_assert!` (this
    // crate's dev/test profile builds with debug_assertions on).
    //
    // This requires an Actor with real `state:` elsewhere in the SAME compiled
    // module: instance field-assignment only threads through this
    // version-counted `State`/`maps:put` convention when the module needs
    // gen_server/actor semantics — otherwise fields thread through a separate
    // `Self`-counted convention `generate_class_method_fun_from_block` never
    // touches. Beamtalk's CLI enforces one class per `.bt` file and the REPL
    // compiles one expression per turn, so this exact shape is unreachable
    // through either surface — only a caller building a multi-class `Module`
    // directly (as `generate_module` allows, and as fuzzing does) can hit it.
    // That's exactly how the nightly `compile_pipeline` fuzz target found it: a
    // `CrossOver` mutation spliced fragments of an Actor fixture and
    // `class_builder_incremental_test.bt` into one fuzz input.
    let src = "Actor subclass: SlwActor\n  state: value = 41\n\nTestCase subclass: FooTest\n  field: cls = nil\n  field: parentCls = nil\n\n  testX =>\n    self.parentCls := 1\n    self.cls := Object classBuilder name: #Child;\n      superclass: Object;\n      addClassMethod: #greeting body: [:self | \"child\"];\n      register\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diags.is_empty(),
        "Reproducer must parse cleanly. Got diagnostics: {diags:?}"
    );

    let result = generate_module(&module, CodegenOptions::new("bt3289_state_version_repro"));

    assert!(
        result.is_ok(),
        "generate_module must not fail/panic on a second field-assignment \
         whose value contains a classBuilder addClassMethod:body: cascade. Got: {result:?}"
    );
}

#[test]
fn test_nested_class_builder_cascade_does_not_corrupt_current_method_params() {
    // BT-3300: the same unguarded-reset shape as BT-3289's `state_version` leak
    // above, for `current_method_params`/`current_method_param_types` instead.
    // `generate_class_method_fun_from_block` unconditionally clears both at its
    // start to give the class-method fun its own fresh parameter list — but
    // neither is captured in `SavedClassMethodCtx`, so a builder cascade nested
    // INSIDE another builder class-method fun's own body clobbers the outer
    // fun's parameter list for any later statement in that same body that
    // reads `current_method_params` (e.g. the `erlangApply`/`erlangModuleLookup`
    // FFI intrinsics, or primitive-BIF codegen).
    //
    // Here the outer `greeting:` class-method fun has one parameter (`a`). Its
    // body first runs an inner `classBuilder … addClassMethod:body:` cascade
    // (building an unrelated `Inner` class), then ends with `@intrinsic
    // erlangApply`, which reads `current_method_params` to find the selector
    // argument to forward. Before the fix, the inner cascade's clear() wiped
    // out the outer fun's `a` parameter, so `erlangApply` fell back to a
    // hardcoded `"Selector"` var name that was never bound in this scope —
    // `beamtalk_erlang_proxy:dispatch(Selector, Arguments, Self)` — which
    // would fail `erlc` with an unbound-variable error rather than a clean
    // Rust-side panic (so, unlike BT-3289, this isn't caught by the ADR-0111
    // ThreadedIr verifier). Same reachability caveat as BT-3289: unreachable
    // through the CLI (one class per `.bt` file) or the REPL (one expression
    // per turn) — only direct `generate_module` library use, as fuzzing does,
    // can construct this AST shape.
    let src = "Actor subclass: SlwActor\n  state: value = 41\n\nTestCase subclass: FooTest\n  field: cls = nil\n  field: parentCls = nil\n\n  testX =>\n    self.parentCls := 1\n    self.cls := Object classBuilder name: #Outer;\n      superclass: Object;\n      addClassMethod: #greeting: body: [:self :a |\n        Object classBuilder name: #Inner;\n          superclass: Object;\n          addClassMethod: #answer body: [:self2 | 42];\n          register\n        @intrinsic erlangApply\n      ];\n      register\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diags.is_empty(),
        "Reproducer must parse cleanly. Got diagnostics: {diags:?}"
    );

    let result = generate_module(&module, CodegenOptions::new("bt3300_params_repro"));
    let code = result.expect("generate_module must not fail on a nested classBuilder cascade");

    let marker = "'beamtalk_erlang_proxy':'dispatch'(";
    let call_pos = code
        .find(marker)
        .expect("expected an erlangApply-generated dispatch call in the output");
    let after = &code[call_pos + marker.len()..];
    assert!(
        !after.starts_with("Selector,"),
        "current_method_params leaked across the nested classBuilder cascade: the \
         outer `greeting:` fun's own `a` parameter should have been threaded into \
         the erlangApply call, not the hardcoded \"Selector\" fallback (which is \
         unbound in this scope and would fail erlc). Generated code:\n{code}"
    );
}

// ── Abstract actor codegen (BT-105, BT-403) ──────────────────────────────────
//
// abstract Actor subclass generates:
//  - spawn/0 and spawn/1 as instantiation_error stubs (not safe_spawn calls)
//  - minimal gen_server callback stubs (noreply/reply-nil, no initialize chain)
//  - simple safe_dispatch pass-through (no try-catch error isolation)
//
// None of these paths were exercised by the parse-based codegen() helper before
// these tests were added; only AST-constructed module tests touched is_abstract.

#[test]
fn test_abstract_actor_spawn_raises_instantiation_error() {
    // BT-105: abstract classes generate spawn/0 as an error stub, not safe_spawn.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'instantiation_error'"),
        "abstract spawn/0 must raise instantiation_error. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_error':'with_hint'("),
        "abstract spawn/0 must call with_hint (hint text included). Got:\n{code}"
    );
    assert!(
        !code.contains("'beamtalk_actor':'safe_spawn'"),
        "abstract spawn/0 must NOT call beamtalk_actor:safe_spawn. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_spawn_1_is_error_stub() {
    // BT-105: abstract classes generate spawn/1 as an error stub too.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'spawn'/1 = fun (_InitArgs) ->"),
        "abstract spawn/1 should be declared as fun (_InitArgs). Got:\n{code}"
    );
    assert!(
        code.contains("'spawnWith:'"),
        "abstract spawn/1 error stub must set the 'spawnWith:' selector on the error. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_has_stub_handle_continue() {
    // BT-403: abstract classes emit a minimal handle_continue/2 stub with no
    // initialize dispatch chain — the chain would be unreachable anyway since
    // abstract actors can never be instantiated.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'handle_continue'/2 = fun (_Continue, State) -> {'noreply', State}"),
        "abstract actor handle_continue/2 must be a noreply stub. Got:\n{code}"
    );
    assert!(
        !code.contains("'initialize'"),
        "abstract actor must not emit an initialize dispatch in handle_continue. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_has_stub_handle_cast_and_call() {
    // BT-403: abstract actor handle_cast/2 and handle_call/3 are stubs.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'handle_cast'/2 = fun (_Msg, State) -> {'noreply', State}"),
        "abstract actor handle_cast/2 must be a noreply stub. Got:\n{code}"
    );
    assert!(
        code.contains("'handle_call'/3 = fun (_Msg, _From, State) -> {'reply', 'nil', State}"),
        "abstract actor handle_call/3 must be a reply-nil stub. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_safe_dispatch_is_simple_passthrough() {
    // BT-403: abstract actor safe_dispatch/3 is a plain call to dispatch/4, not
    // the try-catch error-isolation wrapper used by concrete actors.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'safe_dispatch'/3"),
        "abstract actor must still export safe_dispatch/3. Got:\n{code}"
    );
    assert!(
        !code.contains("catch <Type, Error, Stacktrace>"),
        "abstract actor safe_dispatch must NOT have a try-catch (uses simple passthrough). Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_exports_include_spawn_and_has_method() {
    // Abstract actors still export spawn/0, spawn/1 and has_method/1 so
    // reflection works; the spawn bodies raise errors rather than starting a process.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    let exports = extract_module_exports(&code);
    assert!(
        exports.contains("'spawn'/0"),
        "abstract actor must still export spawn/0. Exports:\n{exports}"
    );
    assert!(
        exports.contains("'spawn'/1"),
        "abstract actor must still export spawn/1. Exports:\n{exports}"
    );
    assert!(
        exports.contains("'has_method'/1"),
        "abstract actor must still export has_method/1. Exports:\n{exports}"
    );
}

#[test]
fn test_abstract_actor_with_class_method_exports_class_method() {
    // Actors with class-side methods must export them. This exercises the
    // build_class_method_export_doc path for an abstract actor with a class method.
    let src = concat!(
        "abstract Actor subclass: AbstractShape\n",
        "  area => 0\n",
        "\n",
        "  class create => self spawn\n",
    );
    let code = codegen(src);
    let exports = extract_module_exports(&code);
    assert!(
        exports.contains("'class_create'/"),
        "abstract actor class method 'create' must be exported as 'class_create'/N. Exports:\n{exports}"
    );
    assert!(
        code.contains("'class_create'/"),
        "abstract actor's class method must appear in the generated code. Got:\n{code}"
    );
}

#[test]
fn bt3382_self_dispatch_receiver_of_conditional_threads_state_and_compiles_through_erlc() {
    // BT-3382: `(self recordOnce: which) ifTrue:ifFalse:` — the self-send is
    // the RECEIVER of the conditional, not a block-body statement, so the
    // `_with_mutations` branch generators' block-mutation scan never even
    // sees it (neither block body itself contains a mutation). Confirms the
    // receiver's own mutation is threaded into the branches' base state AND
    // the generated code is real, erlc-valid Core Erlang (not just
    // parseable).
    let src = "Actor subclass: MutProbe\n  state: timestamps = 0\n\n  triggerDirectly: which =>\n    (self recordOnce: which)\n      ifTrue: [1]\n      ifFalse: [2].\n    self.timestamps\n\n  internal recordOnce: which =>\n    self.timestamps := self.timestamps + 1.\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3382_self_dispatch_receiver_of_conditional")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3382_self_dispatch_receiver_of_conditional", &code);
}

#[test]
fn bt3392_self_dispatch_nested_in_binary_op_operand_threads_state_and_compiles_through_erlc() {
    // BT-3392: `1 + (self bumpCount)` inside an `ifTrue:` block body — the
    // self-send is a binary-op operand nested inside the block's own (only)
    // statement, neither the block's top-level statement (C11/C12b, already
    // correct) nor the conditional's receiver (BT-3382, already fixed).
    // Confirms the self-send's mutation is threaded via a real `Bind` AND
    // the generated code is real, erlc-valid Core Erlang.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [\n      1 + (self bumpCount)\n    ] ifFalse: [\n      0\n    ].\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3392_self_dispatch_nested_in_binary_op_operand")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3392_self_dispatch_nested_in_binary_op_operand", &code);
}

#[test]
fn bt3392_binary_op_hoist_does_not_reorder_past_a_non_self_send_operand() {
    // BT-3392 code review finding: `(self.items at: idx) + (self
    // bumpCount)` — the left operand is a message send but NOT a self-send,
    // so `hoist_self_sends_for_binary_op` must not treat it as safe to
    // hoist past. Confirms the self-dispatch for `bumpCount` is compiled
    // in its ordinary (non-hoisted) position — i.e. as part of
    // `expression_doc`'s normal left-to-right compilation of the whole
    // statement — rather than pulled out ahead of `at:`'s evaluation. The
    // generated code must still be real, erlc-valid Core Erlang.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [\n      (self.items at: 1) + (self bumpCount)\n    ] ifFalse: [\n      0\n    ].\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3392_binary_op_hoist_order_safety").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc("bt3392_binary_op_hoist_order_safety", &code);
}

#[test]
fn adr0118_order_unsafe_self_send_in_binary_op_now_threads_with_no_warning() {
    // BT-3399/ADR 0118 phase 2a (BT-3417): same order-unsafe shape as the
    // test above — `(self.items at: idx) + (self bumpCount)`, non-last
    // inside an `ifTrue:` block reached through `generate_conditional_branch_inline`'s
    // C12 catch-all — but this test used to be named
    // `bt3399_order_unsafe_self_send_in_binary_op_emits_warning_and_still_compiles`
    // and asserted the *fallback's* old behavior: the un-hoisted self-send's
    // mutation silently dropped, with a compile-time warning naming it. ADR
    // 0118's universal sequencing rule (`sequence_children`, reached here
    // via C12's `thread_ahead`) makes that drop unrepresentable: the
    // non-self-send `(self.items at: 1)` operand is bound to a temp AHEAD
    // of `bumpCount`'s dispatch (preserving `at:`'s own evaluation-order
    // guarantee), and `bumpCount`'s mutation threads through a real `Bind`
    // instead. So `bumpCount`'s `NewState` is now genuinely extracted via
    // `element(2, ...)`, and the BT-3399 warning no longer fires for this
    // shape. Mirrors the stdlib regression coverage at
    // `stdlib/test/actor_conditional_mutations_test.bt`'s
    // `testSelfSendAsBinaryOpArgumentInBoundsThreadsMutation`.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [\n      (self.items at: 1) + (self bumpCount)\n    ] ifFalse: [\n      0\n    ].\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("adr0118_order_unsafe_self_send_now_threads").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc(
        "adr0118_order_unsafe_self_send_now_threads",
        &generated.code,
    );
    assert!(
        generated.code.contains("erlang':'element'(2, "),
        "bumpCount's new state must now be extracted (threaded), not discarded. Got:\n{}",
        generated.code
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("bumpCount") && w.message.contains("silently dropped")),
        "the BT-3399 dropped-mutation warning must no longer fire for this shape \
         now that it threads. Got: {:?}",
        generated.warnings
    );
}

#[test]
fn bt3396_self_dispatch_nested_in_conditional_receiver_and_threads_state_and_compiles_through_erlc()
{
    // BT-3396 shape 1: `((self recordOnce: which) and: [true]) ifTrue:ifFalse:`
    // — the conditional's receiver is an `and:` send whose OWN receiver is
    // the self-send. Neither block mutates, and the receiver is not itself
    // a self-send (BT-3382's check), so only the widened
    // `conditional_receiver_needs_threading` probe makes this conditional
    // inline; `compile_conditional_receiver` then threads the nested
    // dispatch ahead of the `and:` send. The generated code must be real,
    // erlc-valid Core Erlang.
    let src = "Actor subclass: MutProbe\n  state: timestamps = 0\n\n  triggerDirectly: which =>\n    ((self recordOnce: which) and: [true])\n      ifTrue: [1]\n      ifFalse: [2].\n    self.timestamps\n\n  internal recordOnce: which =>\n    self.timestamps := self.timestamps + 1.\n    which\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_self_dispatch_nested_in_conditional_receiver_and")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the nested self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('recordOnce:'").count(),
        1,
        "the hoisted self-send must be dispatched exactly once (hoist, then substitute). Got:\n{code}"
    );
    assert_compiles_through_erlc(
        "bt3396_self_dispatch_nested_in_conditional_receiver_and",
        &code,
    );
}

#[test]
fn bt3396_self_dispatch_as_keyword_argument_in_method_body_threads_state_and_compiles_through_erlc()
{
    // BT-3396 shape 2: `#(10, 20, 30) at: (self bumpCount)` as a top-level
    // method-body statement (`BodyExprKind::Pure`, not inside any
    // conditional) — the self-send is an argument to an arbitrary non-self
    // keyword send. The method-body `Pure` arm must hoist it as a real
    // ROOT-frame `Bind` ahead of the statement.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    #(10, 20, 30) at: (self bumpCount).\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_self_dispatch_as_keyword_argument").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('bumpCount'").count(),
        1,
        "the hoisted self-send must be dispatched exactly once. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3396_self_dispatch_as_keyword_argument", &code);
}

#[test]
fn bt3396_self_dispatch_in_field_assignment_rhs_snapshots_prior_field_read_and_compiles_through_erlc()
 {
    // BT-3396 shape 3 + evaluation order: `self.count := self.count + (self
    // bumpCount)` — the self-send is a sub-expression of a field
    // assignment's RHS (the `lower_field_assignment_bind`/`FieldAssignment`
    // `source_version` hazard BT-3382's reverted prototype hit), AND the
    // `self.count` read precedes it in evaluation order. The read must be
    // bound BEFORE the dispatch runs, so it keeps its source-order
    // (pre-bump) value.
    //
    // ADR 0118 phase 1a (BT-3415): the sequencing rule binds the preceding
    // `self.count` read to a `_TmpN` temp (it is compiled against the
    // pre-dispatch `State` and bound ahead of the dispatch's `Bind`) — the
    // planner's `FieldSnap` snapshot is the same rule applied to one node
    // kind, and is no longer what this position goes through.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    self.count := self.count + (self bumpCount).\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_self_dispatch_in_field_assignment_rhs")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let read_at = code.find("let _Tmp").unwrap_or_else(|| {
        panic!("the preceding self.count read must be temp-bound. Got:\n{code}")
    });
    let dispatch_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("the nested self-send must be dispatched. Got:\n{code}"));
    assert!(
        read_at < dispatch_at,
        "the field-read temp must be bound BEFORE the dispatch runs. Got:\n{code}"
    );
    assert!(
        code[read_at..dispatch_at].contains("call 'maps':'get'('count', State)"),
        "the temp must hold the PRE-dispatch read (against `State`, not `State1`). Got:\n{code}"
    );
    assert!(
        !code.contains("FieldSnap"),
        "the Actor-body FieldAssignment arm no longer goes through the planner's snapshot. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('bumpCount'").count(),
        1,
        "the sequenced self-send must be dispatched exactly once. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'put'('count', _Val"),
        "the field write must still land as the real Put Bind. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3396_self_dispatch_in_field_assignment_rhs", &code);
}

#[test]
fn bt3396_self_dispatch_after_order_unsafe_operand_is_sequenced_behind_a_temp() {
    // BT-3392/BT-3396 refused to hoist a self-send past a non-self,
    // non-effect-free operand (`printString` may raise) and left
    // `bumpCount` in its natural, state-dropping position (BT-3399).
    //
    // ADR 0118 phase 1a (BT-3415), §Decision 3: in method-body position the
    // sequencing rule binds the earlier operand to a `_TmpN` temp FIRST,
    // then runs the dispatch + real `State` `Bind`, then the `++` on the
    // temp and the dispatch result — evaluation order preserved by
    // construction (`printString` still raises before `bumpCount` runs)
    // AND the mutation threaded. `HoistAction::Dropped` is unreachable
    // from this position now.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: x =>\n    (x printString) ++ (self bumpCount) printString.\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3396_order_unsafe_operand_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let code = &generated.code;
    let temp_at = code
        .find("let _Tmp")
        .unwrap_or_else(|| panic!("the earlier `x printString` must be temp-bound. Got:\n{code}"));
    let dispatch_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("the self-send must be dispatched. Got:\n{code}"));
    let bind_at = code
        .find("let State1 = call 'erlang':'element'(2, _SD")
        .unwrap_or_else(|| panic!("the dispatch's NewState must be threaded. Got:\n{code}"));
    assert!(
        temp_at < dispatch_at && dispatch_at < bind_at,
        "order must be: temp for `x printString`, then the dispatch, then its State Bind. Got:\n{code}"
    );
    assert!(
        code[temp_at..dispatch_at].contains("'printString'"),
        "the temp must hold the `x printString` send itself. Got:\n{code}"
    );
    assert!(
        code.contains("{'reply', _Result, State1}"),
        "the reply must carry the post-dispatch state. Got:\n{code}"
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("silently dropped")),
        "no BT-3399 drop warning in a sequenced position. Got: {:?}",
        generated.warnings
    );
    assert_compiles_through_erlc("bt3396_order_unsafe_operand_sequenced", code);
}

#[test]
fn bt3415_binary_operand_self_send_after_raising_operand_is_sequenced_in_method_body() {
    // ADR 0118 phase 1a (BT-3415) acceptance shape: `(items at: idx) +
    // (self bump)` as an Actor method-body statement compiles to
    // `let _Tmp = <at:> in <dispatch> in let State1 = element(2, _SD) in
    // _Tmp + element(1, _SD)` — `at:` raises first (it is bound before the
    // dispatch runs), and when it does not raise `bump`'s state is
    // threaded into the reply. The BT-3399 "Dropped" case is not
    // reachable from method-body position any more.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(10, 20, 30)\n\n  pick: idx =>\n    (self.items at: idx) + (self bump)\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3415_binary_operand_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let code = &generated.code;
    let temp_at = code
        .find("let _Tmp")
        .unwrap_or_else(|| panic!("`items at: idx` must be temp-bound. Got:\n{code}"));
    let dispatch_at = code
        .find("'safe_dispatch'('bump'")
        .unwrap_or_else(|| panic!("`bump` must be dispatched. Got:\n{code}"));
    let bind_at = code
        .find("let State1 = call 'erlang':'element'(2, _SD")
        .unwrap_or_else(|| panic!("`bump`'s NewState must be threaded. Got:\n{code}"));
    assert!(
        temp_at < dispatch_at && dispatch_at < bind_at,
        "order must be: temp for `at:`, then the dispatch, then its State Bind. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('bump'").count(),
        1,
        "the sequenced self-send is dispatched exactly once. Got:\n{code}"
    );
    assert!(
        code.contains("{'reply', _Result, State1}"),
        "the method must reply with the post-dispatch state. Got:\n{code}"
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("silently dropped")),
        "no BT-3399 drop warning: the sequencing rule makes `Dropped` unreachable here. Got: {:?}",
        generated.warnings
    );
    assert_compiles_through_erlc("bt3415_binary_operand_sequenced", code);
}

#[test]
fn bt3415_ffi_receiver_is_not_sequenced_but_its_self_send_argument_is() {
    // Adversarial review finding on #3717: `Erlang lists reverse: (self
    // bump)` — the FFI receiver is consumed STRUCTURALLY by
    // `try_handle_erlang_interop` (`erlang_module_of_receiver` turns it into
    // a module atom; it is never compiled through `generate_expression`), so
    // the sequencing rule must leave it alone or `finish_precompiled_scope`
    // reports the never-substituted registration as an internal error and
    // the whole module fails to compile. The argument's self-send is still
    // sequenced and its state threaded.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go =>\n    Erlang lists reverse: (self bump)\n\n  goParens =>\n    (Erlang lists) reverse: (self bump)\n\n  goTwo =>\n    Erlang lists seq: 1 to: (self bump)\n\n  goNested =>\n    self record: (Erlang lists reverse: (self bump))\n\n  internal record: x => x\n\n  internal bump =>\n    self.count := self.count + 1\n    #(1, 2)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_ffi_receiver_not_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("an FFI send with a self-send argument must compile. Got: {e:?}"));
    assert_eq!(
        code.matches("'safe_dispatch'('bump'").count(),
        4,
        "each of the four methods dispatches `bump` exactly once. Got:\n{code}"
    );
    assert!(
        code.contains("'direct_call'('lists', 'reverse', [call 'erlang':'element'(1, _SD"),
        "the direct FFI call must receive the sequenced dispatch result. Got:\n{code}"
    );
    assert!(
        !code.contains("let _Tmp"),
        "an FFI receiver is never temp-bound. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_ffi_receiver_not_sequenced", &code);
}

#[test]
fn bt3415_early_return_reply_state_threads_the_conditionals_own_mutation() {
    // Adversarial review finding on #3717, superseded by ADR 0118 phase 4
    // (BT-3420): `^ 1 + ((self flagTrue) ifTrue: [1] ifFalse: [2])` — before
    // BT-3420, the conditional receiver's dispatch chain minted `State1`
    // INSIDE the conditional's own closed document
    // (`compile_conditional_receiver`'s open let-chain), invisible to the
    // `^` arm's `current_state_var()` read, so the reply fell back to the
    // stale pre-conditional `State` — `flagTrue`'s mutation compiled and
    // ran, but the method's own reply (and any state read afterward)
    // couldn't see it. BT-3420 makes the mutation-threaded `ifTrue:ifFalse:`
    // a real `ThreadedValue` producer whose prelude — including the
    // receiver's own hoisted `flagTrue` dispatch — splices into the `^`
    // arm's `single_sequenced_child` sequencing, so the reply now correctly
    // carries the prelude's own final version: `State1` from the
    // receiver's hoisted `flagTrue` dispatch, then `State2` from
    // `control_flow_tuple_to_threaded_value`'s own wrap of the
    // `ifTrue:ifFalse:` construct's `{Value, NewState}` tuple (matching
    // `bt3415_early_return_reply_state_follows_the_prelude_when_there_is_one`'s
    // shape) — instead of the discarding `State`.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go: i =>\n    ^ 1 + ((self flagTrue) ifTrue: [1] ifFalse: [2])\n    0\n\n  internal flagTrue =>\n    self.count := self.count + 1\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_early_return_post_prelude_state").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("{'reply', _ReturnValue, State2}"),
        "the reply must carry the prelude's final State2 (flagTrue's own \
         mutation, threaded through the conditional receiver, then the \
         conditional's own wrap), not the stale pre-conditional State. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_early_return_post_prelude_state", &code);
}

#[test]
fn bt3415_early_return_reply_state_follows_the_prelude_when_there_is_one() {
    // The positive counterpart: `^ (self.items at: 1) + (self bump)` has a
    // real prelude (the `bump` dispatch + `State1` Bind), and the reply
    // must carry THAT version.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(10, 20)\n\n  go =>\n    ^ (self.items at: 1) + (self bump)\n    0\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_early_return_prelude_state").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("{'reply', _ReturnValue, State1}"),
        "the reply must carry the prelude's State1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_early_return_prelude_state", &code);
}

#[test]
fn bt3416_thread_ahead_no_longer_warns_once_the_interpolation_segment_threads() {
    // ADR 0118 phase 1b (BT-3416) superseded the BT-3415-era pin below
    // (`bt3415_thread_ahead_keeps_the_bt3399_warning_for_a_dropped_only_plan`):
    // a `thread_ahead` consumer (here `FieldAssignment`) whose RHS is a
    // `StringInterpolation` with an order-unsafe self-send in a LATER
    // segment — `"{self.items size}-{self bump}"` — used to run the
    // planner, which could not safely hoist `bump` ahead of the first
    // segment's `displayString` dispatch and so dropped the mutation with
    // the BT-3399 warning. `threaded_string_interpolation` now moves
    // BOTH segments' `let`-chains into the RHS's prelude, in order, so
    // `bump` dispatches (after the first segment's `displayString` call,
    // preserving evaluation order) and the warning is gone — the same fix
    // as the BUnit matrix's `interpolationBinaryOpSelfSend` row, exercised
    // here from a `FieldAssignment` RHS instead of a bare statement.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(1)\n  state: label = \"\"\n\n  go =>\n    self.label := \"{self.items size}-{self bump}\"\n    self.count\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3416_thread_ahead_no_longer_warns").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("bump") && w.message.contains("silently dropped")),
        "bump's mutation now threads — the BT-3399 warning must be gone. Got: {:?}",
        generated.warnings
    );
    assert!(
        generated.code.contains("erlang':'element'(2, _SD"),
        "bump's dispatch must thread its NewState. Got:\n{}",
        generated.code
    );
    assert_compiles_through_erlc("bt3416_thread_ahead_no_longer_warns", &generated.code);
}

#[test]
fn bt3416_self_send_nested_in_a_cast_sends_receiver_still_threads() {
    // Review finding on #3718: `sequenced_send_children` treats an
    // `is_cast` send (`X!`) as opaque (never a "covered send", per its own
    // doc comment) since it isn't compiled through `try_handle_self_dispatch`
    // like an ordinary send — but `threaded_expression`'s "Pure default"
    // fallback (after this ADR's phase-1b cases were added) no longer ran
    // the planner there, so a self-send nested in a CAST send's receiver
    // — `(self next) process!` — silently lost its mutation: `subexpr_
    // needs_prelude`'s own tail probe still finds it via `hoist_plan_walk`
    // (which, unlike `sequenced_send_children`, does NOT special-case
    // `is_cast` and walks into the receiver regardless), but nothing
    // produced a prelude for it. `threaded_expression` restores the
    // planner as the explicit last-resort fallback, so this shape is
    // exactly what `hoist_nested_self_sends` already handled before ADR
    // 0118 phase 1b and continues to.
    //
    // `!` only ever terminates a whole body statement (the parser marks
    // `body.last_mut()`'s expression `is_cast`), never a nested
    // sub-expression, so the shape is pinned as its own top-level
    // statement rather than nested inside a literal.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go =>\n    (self next) process!\n    self.count\n\n  internal next =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3416_cast_send_receiver_self_send_threads")
            .with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("'safe_dispatch'('next'"),
        "the cast's receiver self-send must still dispatch. Got:\n{code}"
    );
    assert!(
        code.contains("erlang':'element'(2, _SD"),
        "the nested self-send's NewState must thread into the method body. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3416_cast_send_receiver_self_send_threads", &code);
}

#[test]
fn bt3418_field_assign_rhs_in_loop_body_threads_nested_self_send() {
    // ADR 0118 phase 2b (BT-3418): `self.count := self.count + (self
    // bump)` as a `do:` loop-body statement — the field-assignment RHS
    // path inside `generate_threaded_loop_body_inner`. Before this phase
    // the nested self-send's mutation was silently dropped (no hoist ran
    // for this position at all); `thread_ahead` now sequences it ahead of
    // `generate_field_assignment_open`'s own compile of the RHS. Per
    // iteration: `count` reads BEFORE `bump` runs (evaluation order), so
    // `1, 2` bumps `count` to `1, 2` while `self.count` is reassigned to
    // `0+1=1`, then `1+2=3`.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(1, 2)\n\n  go =>\n    self.items do: [:x | self.count := self.count + (self bump)]\n    self.count\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3418_field_assign_rhs_in_loop_body").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("'safe_dispatch'('bump'"),
        "the nested self-send must dispatch. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3418_field_assign_rhs_in_loop_body", &code);
}

#[test]
fn bt3418_local_assign_rhs_in_loop_body_threads_nested_self_send_with_no_warning() {
    // ADR 0118 phase 2b (BT-3418): `y := 1 + (self bump)` as a `do:`
    // loop-body statement — `generate_local_var_assignment_in_loop`'s own
    // RHS. This is the exact shape the BT-3399 warning used to fire for
    // (an order-unsafe self-send binary-op operand silently dropped);
    // `thread_ahead` now sequences it ahead of the RHS's own compile via
    // the universal sequencing rule, so the mutation threads and the
    // warning is gone.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(1, 2)\n\n  go =>\n    y := 0.\n    self.items do: [:x | y := 1 + (self bump)].\n    y\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3418_local_assign_rhs_in_loop_body").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        generated.code.contains("erlang':'element'(2, _SD"),
        "bump's NewState must be extracted (threaded), not discarded. Got:\n{}",
        generated.code
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("bump") && w.message.contains("silently dropped")),
        "the BT-3399 dropped-mutation warning must no longer fire for this shape \
         now that it threads. Got: {:?}",
        generated.warnings
    );
    assert_compiles_through_erlc("bt3418_local_assign_rhs_in_loop_body", &generated.code);
}

#[test]
fn bt3415_registering_the_same_subexpression_twice_is_never_silent() {
    // Adversarial review finding on #3717: a second registration of one
    // node would let the inner scope's finish remove the entry out from
    // under the outer scope, whose consulted-exactly check then passes
    // vacuously — a double dispatch with no error. Pinned as a hard
    // internal error in every build profile (a `codegen_warnings`
    // diagnostic would be discarded by the CLI's build path).
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go => self bump\n\n  internal bump => self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let class = module.classes.first().expect("one class");
    let expr = &class.methods[0]
        .body
        .first()
        .expect("one statement")
        .expression;
    let mut generator = CoreErlangGenerator::new("bt3415_double_registration");
    let mut scope = super::super::PrecompiledScope::new();
    generator
        .register_precompiled_subexpr(&mut scope, expr, Document::Str("'a'"), false)
        .expect("first registration succeeds");
    let err = generator
        .register_precompiled_subexpr(&mut scope, expr, Document::Str("'b'"), false)
        .expect_err("second registration of the same node must fail");
    assert!(
        format!("{err:?}").contains("registered twice"),
        "expected the duplicate-registration internal error, got {err:?}"
    );
    generator
        .finish_precompiled_scope(scope)
        .expect_err("the scope still holds the never-consulted first entry");
}

#[test]
fn bt3415_self_send_argument_of_self_send_sequences_args_before_dispatch() {
    // `self record: (self bumpCount)` — the producer sequences its own
    // arguments (ADR 0118 §Decision 2): `bumpCount`'s dispatch + Bind
    // precede `record:`'s, whose argument list references the pure
    // `element(1, _SD)` result, and both states thread (`State1`,
    // `State2`).
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: log = #()\n\n  go =>\n    self record: (self bumpCount)\n\n  internal record: n =>\n    self.log := self.log ++ #(n)\n    n\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_self_send_arg_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let bump_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("`bumpCount` must be dispatched. Got:\n{code}"));
    let record_at = code
        .find("'safe_dispatch'('record:'")
        .unwrap_or_else(|| panic!("`record:` must be dispatched. Got:\n{code}"));
    assert!(
        bump_at < record_at,
        "the argument's dispatch must precede the outer dispatch. Got:\n{code}"
    );
    assert!(
        code.contains("let State1 = call 'erlang':'element'(2, _SD")
            && code.contains("let State2 = call 'erlang':'element'(2, _SD"),
        "both dispatches must thread their NewState. Got:\n{code}"
    );
    assert!(
        code.contains("{'reply', call 'erlang':'element'(1, _SD"),
        "the reply reads the outer dispatch's pure result. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_self_send_arg_sequenced", &code);
}

#[test]
fn bt3416_self_dispatch_in_later_interpolation_segment_now_threads_after_earlier_segment() {
    // BT-3396 found that `generate_string_interpolation` dispatches
    // `displayString` on each segment's value right after evaluating it,
    // before the next segment runs — a message send that may raise — so
    // hoisting a LATER segment's self-send ahead of an EARLIER segment's
    // `displayString` dispatch would reorder evaluation; the then-current
    // planner's fix was to leave it un-hoisted (dropping the mutation,
    // with a warning). ADR 0118 phase 1b (BT-3416) replaces that with the
    // sequencing rule: `threaded_string_interpolation` moves every
    // segment's `let`-chain up to and including the LAST one that needs
    // threading into the prelude, in order — so in `"{x}-{self
    // bumpCount}"` the mutation now threads AND `x`'s `displayString`
    // dispatch still runs first (see `threaded_string_interpolation`'s
    // doc comment). A self-send in the FIRST segment
    // (`"n={self bumpCount}"`) has nothing before it and was already
    // threaded before this phase.
    let later = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: x =>\n    \"{x}-{self bumpCount}\".\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(later);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3416_later_interpolation_segment_threaded")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, _SD"),
        "a self-send in a later interpolation segment must now thread its NewState. Got:\n{code}"
    );
    let x_display_at = code
        .find("'displayString'")
        .unwrap_or_else(|| panic!("`x`'s displayString dispatch must appear. Got:\n{code}"));
    let bump_dispatch_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("`bumpCount` must be dispatched. Got:\n{code}"));
    assert!(
        x_display_at < bump_dispatch_at,
        "the first segment's displayString dispatch must still precede the later segment's self-send dispatch. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3416_later_interpolation_segment_threaded", &code);

    let first = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    \"n={self bumpCount}\".\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(first);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_first_interpolation_segment_hoisted").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, _SD"),
        "a self-send in the first interpolation segment has nothing before it and must still be hoisted. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3396_first_interpolation_segment_hoisted", &code);
}

// BT-3414 (ADR 0118 phase 0): three shapes from the ADR's 47-shape self-send
// position probe (§Context) PANICKED the ThreadedIr verifier (rather than
// merely crashing at runtime or silently dropping a mutation) before ADR
// 0118 phase 3 (BT-3419). A debug-build verifier panic
// (`report_threaded_ir_verify_errors`'s `debug_assert!`, control_flow/mod.rs)
// aborts the WHOLE test-binary invocation, so — unlike every other row in
// the same probe — the two BT-3419 closes cannot live in a BUnit `.bt`
// fixture (see stdlib/test/fixtures/self_send_position_counter.bt's header
// comment); they are pinned here. The third (`bt3414_bare_and_inside_if_true_branch_inside_do_body`,
// below) is a different shape — a bare-receiver `and:` inside an `ifTrue:`
// inside a `do:` body — still open for a later phase (ADR 0118's own
// "Out of Scope" note for phase 3: "Inline-threaded control flow in
// expression position").

#[test]
fn bt3414_self_send_in_and_receiver_inside_while_true_condition_now_compiles_and_threads_state() {
    // `[i := i + 1. (self bumpCount) > 0 and: [i < 3]] whileTrue: [nil]` —
    // a self-send as the RECEIVER of an inline-threaded `and:`, itself the
    // whileTrue: CONDITION block's last expression. Before ADR 0118 phase 3
    // (BT-3419), `generate_while_true`'s mode selection only inspected the
    // BODY's own mutations (trivially none — `[nil]`), so this fell to the
    // simple (non-threading) codegen path, which compiled the condition as
    // a genuine stateful Tier-2 closure and panicked the verifier
    // (`UnboundVersion`). Now: `generate_while_true` also checks the
    // condition (`condition_has_state_effects`), routing this into the
    // mutation-threading path, and every iteration's `bumpCount` dispatch
    // correctly advances the actor's `count` field.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    i := 0\n    [\n      i := i + 1\n      (self bumpCount) > 0 and: [i < 3]\n    ] whileTrue: [nil]\n    i\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3414_and_receiver_self_send_in_while_condition")
            .with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc("bt3414_and_receiver_self_send_in_while_condition", &code);
}

#[test]
fn bt3414_self_send_as_and_receiver_alone_inside_while_true_condition_now_compiles_and_threads_state()
 {
    // `[i := i + 1. (self flagTrue) and: [i < 3]] whileTrue: [nil]` — same
    // shape as above with a bare self-send (no binary-op wrapper) as the
    // `and:` receiver. Also closed by ADR 0118 phase 3 (BT-3419).
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    i := 0\n    [\n      i := i + 1\n      (self flagTrue) and: [i < 3]\n    ] whileTrue: [nil]\n    i\n\n  internal flagTrue =>\n    self.count := self.count + 1\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3414_bare_and_receiver_self_send_in_while_condition")
            .with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc(
        "bt3414_bare_and_receiver_self_send_in_while_condition",
        &code,
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn threading_gates_agree_on_fixture_set() {
    // BT-3414 (ADR 0118 phase 0): "does this conditional-shaped construct
    // need the inline mutation-threading path" has historically been
    // answered by several overlapping predicates — `control_flow_has_mutations`
    // (gen_server/methods.rs, used by the statement-level C11/C12 dispatch
    // to decide whether a `Match`/conditional send needs threaded lowering),
    // `conditional_receiver_needs_threading` (util.rs, one disjunct of the
    // same question — renamed from `contains_hoistable_self_send` by ADR
    // 0118 phase 2b/BT-3418, which also moved it out of
    // `control_flow/conditionals.rs`), and the gate behind `ifTrue:`,
    // `ifFalse:`, `ifTrue:ifFalse:`, `ifNotNil:`, and `and:`/`or:`
    // (intrinsics.rs, collapsed by this same issue into
    // `conditional_needs_mutation_threading` /
    // `and_or_needs_mutation_threading`) — under a "must stay in sync"
    // comment (gen_server/methods.rs) with no enforcing test until now.
    //
    // For a fixture set of real parsed `and:`/`or:`/`ifTrue:ifFalse:` sends,
    // assert every predicate that claims to answer this question agrees.
    struct Case {
        name: &'static str,
        snippet: &'static str,
        expect_needs_threading: bool,
    }

    let cases = [
        Case {
            name: "and_no_mutation_no_self_send",
            snippet: "true and: [false]",
            expect_needs_threading: false,
        },
        Case {
            name: "and_block_mutates_field",
            snippet: "true and: [self.count := self.count + 1. true]",
            expect_needs_threading: true,
        },
        Case {
            name: "and_receiver_is_hoistable_self_send",
            snippet: "(self bumpCount) and: [true]",
            expect_needs_threading: true,
        },
        Case {
            name: "or_no_mutation_no_self_send",
            snippet: "false or: [true]",
            expect_needs_threading: false,
        },
        Case {
            name: "or_block_mutates_field",
            snippet: "false or: [self.count := self.count + 1. true]",
            expect_needs_threading: true,
        },
        Case {
            name: "or_receiver_is_hoistable_self_send",
            snippet: "(self bumpCount) or: [false]",
            expect_needs_threading: true,
        },
    ];

    for case in cases {
        let src = format!(
            "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    {}\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n",
            case.snippet
        );
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
        let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(
            diags.is_empty(),
            "case {:?}: fixture must parse cleanly. Got: {diags:?}",
            case.name
        );
        let class = module.classes.first().expect("one class");
        let method = class
            .methods
            .iter()
            .find(|m| matches!(&m.selector, MessageSelector::Unary(s) if s.as_str() == "triggerDirectly"))
            .expect("triggerDirectly method");
        let expr = &method
            .body
            .first()
            .expect("triggerDirectly has one statement")
            .expression;
        let Expression::MessageSend {
            receiver,
            arguments,
            ..
        } = expr
        else {
            panic!("case {:?}: expected a MessageSend, got {expr:?}", case.name);
        };
        let Expression::Block(block) = &arguments[0] else {
            panic!("case {:?}: expected a block argument", case.name);
        };

        let generator = CoreErlangGenerator::new("bt3414_gate_agreement");

        let and_or = generator.and_or_needs_mutation_threading(receiver, block);
        assert_eq!(
            and_or, case.expect_needs_threading,
            "case {:?}: and_or_needs_mutation_threading disagreed",
            case.name
        );

        let collapsed = generator.conditional_needs_mutation_threading(receiver, &[block]);
        assert_eq!(
            collapsed, case.expect_needs_threading,
            "case {:?}: conditional_needs_mutation_threading disagreed",
            case.name
        );

        let needs_threading = generator.conditional_receiver_needs_threading(receiver);
        assert!(
            !needs_threading || case.expect_needs_threading,
            "case {:?}: conditional_receiver_needs_threading(receiver) was true but the case did \
             not expect threading — a self-send needing threading in the receiver must always \
             force threading",
            case.name
        );

        let control_flow = generator.control_flow_has_mutations(expr);
        assert_eq!(
            control_flow, case.expect_needs_threading,
            "case {:?}: control_flow_has_mutations disagreed",
            case.name
        );
    }

    // `ifTrue:ifFalse:` exercises `conditional_needs_mutation_threading`'s
    // two-block path (both `true_block` and `false_block` are checked).
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [self.count := self.count + 1] ifFalse: [nil]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diags.is_empty(),
        "ifTrue:ifFalse: fixture must parse cleanly. Got: {diags:?}"
    );
    let class = module.classes.first().expect("one class");
    let method = class
        .methods
        .iter()
        .find(|m| {
            matches!(&m.selector, MessageSelector::Keyword(parts) if parts.len() == 1 && parts[0].keyword == "triggerDirectly:")
        })
        .expect("triggerDirectly: method");
    let expr = &method.body.first().expect("one statement").expression;
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr
    else {
        panic!("expected a MessageSend, got {expr:?}");
    };
    let (Expression::Block(true_block), Expression::Block(false_block)) =
        (&arguments[0], &arguments[1])
    else {
        panic!("expected two block arguments");
    };
    let generator = CoreErlangGenerator::new("bt3414_gate_agreement_if_true_if_false");
    assert!(
        generator.conditional_needs_mutation_threading(receiver, &[true_block, false_block]),
        "ifTrue:ifFalse: with a mutating true-branch must need threading"
    );
    assert!(
        generator.control_flow_has_mutations(expr),
        "control_flow_has_mutations must agree that this ifTrue:ifFalse: needs threading"
    );
}

#[test]
#[should_panic(expected = "ThreadedIr verify")]
#[cfg(debug_assertions)]
fn bt3414_bare_and_inside_if_true_branch_inside_do_body_panics_verifier() {
    // `items do: [:x | x > 0 ifTrue: [(self flagTrue) and: [true]] ifFalse:
    // [nil]]` — a bare-receiver `and:` (self-send as its receiver) inside an
    // `ifTrue:` branch, itself inside a `do:` loop body. The conditional
    // branch's own ThreadedIr frame and the enclosing loop body's frame both
    // end up producing a Bind for the same version: `NonLinearVersion`.
    // Confirmed still panicking after ADR 0118 phase 2b (BT-3418, loop-body
    // consumers): this statement routes through
    // `generate_threaded_loop_body_inner`'s separate `control_flow_has_mutations`
    // branch (an inline conditional with mutations, not any of phase 2b's
    // three consumers), so neither phase touches it. Left open for a later
    // phase.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    #(1) do: [:x |\n      x > 0\n        ifTrue: [(self flagTrue) and: [true]]\n        ifFalse: [nil]\n    ]\n    self.count\n\n  internal flagTrue =>\n    self.count := self.count + 1\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let _ = generate_module(
        &module,
        CodegenOptions::new("bt3414_bare_and_in_if_true_inside_do_body").with_workspace_mode(true),
    );
}

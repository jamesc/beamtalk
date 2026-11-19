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
    assert_has(
        "test_generate_empty_module",
        &code,
        &[
            "module 'bt_module'",
            "attributes ['behaviour' = ['gen_server']]",
        ],
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "comprehensive test covering all registration metadata"
)]
fn test_class_registration_generation() {
    // Test that class definitions generate registration code
    use beamtalk_core::ast::{ClassDefinition, Identifier, MethodDefinition, StateDeclaration};
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
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
            slot_kind: beamtalk_core::ast::SlotKind::default(),
            ..StateDeclaration::new(Identifier::new("value", Span::new(0, 5)), Span::new(0, 10))
        }],
        methods: vec![
            MethodDefinition {
                pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
                literal_comments: beamtalk_core::ast::LiteralComments::default(),
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                ..MethodDefinition::test_at("increment", Span::new(0, 10))
            },
            MethodDefinition {
                pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
                literal_comments: beamtalk_core::ast::LiteralComments::default(),
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                ..MethodDefinition::test_at("getValue", Span::new(0, 10))
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 50),
    };

    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        span: Span::new(0, 50),
        ..empty_module()
    };

    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // Check that on_load attribute is present
    assert_has(
        "Should have on_load attribute",
        &code,
        &["'on_load' = [{'register_class', 0}]"],
    );

    // Check that register_class/0 is exported
    assert_has(
        "Should export register_class/0",
        &code,
        &["'register_class'/0"],
    );

    // Check that register_class/0 function exists
    assert_has(
        "Should generate register_class function",
        &code,
        &["'register_class'/0 = fun () ->"],
    );

    // Check that it calls beamtalk_class_builder:register
    assert_has(
        "Should call beamtalk_class_builder:register",
        &code,
        &["call 'beamtalk_class_builder':'register'(_BuilderState0)"],
    );

    // Check ClassBuilder state fields
    assert_has(
        "Should include className in builder state",
        &code,
        &["'className' => 'Counter'"],
    );
    assert_has(
        "Should include moduleName in builder state",
        &code,
        &["'moduleName' => 'counter'"],
    );
    assert_has(
        "Should include superclassRef in builder state",
        &code,
        &["'superclassRef' => 'Actor'"],
    );

    // Check beamtalk_class module attribute for dependency sorting
    assert_has(
        "Should include beamtalk_class attribute with class and superclass",
        &code,
        &["'beamtalk_class' = [{'Counter', 'Actor'}]"],
    );

    // methodSpecs, fieldSpecs, classMethods removed from BuilderState.
    // Methods and fields now live in meta map.
    assert_has(
        "Should include meta map in builder state",
        &code,
        &["'meta' => ~{"],
    );
    // Check method_info contains instance methods with arity
    assert_has(
        "Should include method_info in meta map",
        &code,
        &["'method_info' => ~{"],
    );
    assert_has(
        "Should include class_method_info in meta map",
        &code,
        &["'class_method_info' => ~{"],
    );
    // Check fields in meta
    assert_has(
        "Should include fields in meta map",
        &code,
        &["'fields' => ['value']"],
    );

    // modifiers removed from BuilderState; is_sealed/is_abstract now in meta map
    assert_has(
        "Should include is_sealed in meta map",
        &code,
        &["'is_sealed' => 'false'"],
    );

    // Check function returns ok
    assert!(code.contains("'ok'"), "Should return 'ok'. Got:\n{code}");

    // catch clause must re-raise, not silently swallow errors
    assert_has(
        "register_class/0 catch clause must re-raise via primop 'raw_raise' (BT-998)",
        &code,
        &[
            "catch <CatchType, CatchError, CatchStack> -> primop 'raw_raise'(CatchType, CatchError, CatchStack)",
        ],
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
    assert_has(
        "Generated class module must export has_method/1 in header. Got header",
        &header_exports,
        &["'has_method'/1"],
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
    // and self-sent `superclass` reaches the same intrinsic through
    // `class_self_send/3`'s class-object protocol fallthrough (BT-4252). This
    // assertion exists to keep the export set stable/uniform across class
    // modules regardless — not because anything currently calls it.
    assert_has(
        "Generated class module must export superclass/0 in header. Got header",
        &header_exports,
        &["'superclass'/0"],
    );
    assert_has(
        "Generated class module must export class_name/0 in header. Got header",
        &header_exports,
        &["'class_name'/0"],
    );
    // The old mistaken auto-export `methods/0` must NOT appear — the
    // classifier no longer includes it and no codegen site emits it.
    assert_lacks(
        "Generated class module must NOT export methods/0 in header — removed after BT-2007. Got header",
        &header_exports,
        &["'methods'/0"],
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
    let code = codegen_named(src, "counter");

    // Instance-side fields unchanged.
    assert_has(
        "Should include instance fields in meta map",
        &code,
        &["'fields' => ['value']"],
    );
    // Class-side fields emitted in declaration order.
    assert_has(
        "Should include class_fields (class variables) in meta map",
        &code,
        &["'class_fields' => ['total', 'label']"],
    );
}

#[test]
fn test_no_class_state_emits_empty_class_fields() {
    // a class with no `classState:` declarations emits an empty
    // class_fields list so the runtime intrinsic always finds the key.
    let src = concat!("Actor subclass: Counter\n", "  state: value = 0\n");
    let code = codegen_named(src, "counter");

    assert_has(
        "Class with no class state should emit empty class_fields",
        &code,
        &["'class_fields' => []"],
    );
}

#[test]
fn test_late_state_and_classstate_emit_field_kinds_in_meta() {
    // ADR 0124 §1/B5a: a `late state:`/`late classState:` declaration must
    // be reflected into __beamtalk_meta/0 as 'late' entries in the
    // 'field_kinds' (instance) and 'class_field_kinds' (class-side) maps,
    // alongside every ordinary declaration's 'eager' entry.
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late state: proc :: Subprocess\n",
        "  state: id :: Integer = 0\n",
        "  late classState: current :: CodexClient\n",
        "  classState: total :: Integer = 0\n\n",
        "  launch => self.proc\n",
    );
    let code = codegen_named(src, "codex_client");

    assert_has(
        "Should include instance field_kinds (late proc, eager id) in meta map",
        &code,
        &["'field_kinds' => ~{'proc' => 'late', 'id' => 'eager'}~"],
    );
    assert_has(
        "Should include class_field_kinds (late current, eager total) in meta map",
        &code,
        &["'class_field_kinds' => ~{'current' => 'late', 'total' => 'eager'}~"],
    );
}

#[test]
fn test_late_state_omitted_from_init_state_literal() {
    // BT-3548 (ADR 0124 §2/B2): a defaultless `late state:` field's key
    // must be entirely absent from init/1's `DefaultState` literal — not
    // present as `'proc' => 'nil'` — so it starts absent from the actor's
    // state map. An ordinary eager field alongside it (`id`) still gets its
    // default emitted as usual.
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late state: proc :: Subprocess\n",
        "  state: id :: Integer = 0\n\n",
        "  launch => self.proc\n",
    );
    let code = codegen_named(src, "codex_client");

    let default_state = code
        .split("let DefaultState = ~{")
        .nth(1)
        .expect("init/1 must emit a DefaultState map literal")
        .split("}~")
        .next()
        .expect("DefaultState map literal must close with }~");

    assert_lacks(
        "Unassigned `late` slot 'proc' must be absent from DefaultState entirely. Got DefaultState body",
        default_state,
        &["'proc'"],
    );
    assert_has(
        "Eager field 'id' must still get its default in DefaultState. Got DefaultState body",
        default_state,
        &["'id' => 0"],
    );
}

#[test]
fn test_late_classstate_omitted_from_classstate_map() {
    // BT-3548 (ADR 0124 §2/B2): a defaultless `late classState:` slot is
    // likewise omitted from the class-side `'classState'` map in
    // `_BuilderState` — the class-side counterpart of the instance-side
    // omission above.
    let src = concat!(
        "typed Actor subclass: CodexClient\n",
        "  late classState: current :: CodexClient\n",
        "  classState: total :: Integer = 0\n\n",
        "  class launch => self.current\n",
    );
    let code = codegen_named(src, "codex_client");

    let class_state = code
        .split("'classState' => ~{")
        .nth(1)
        .expect("_BuilderState must emit a 'classState' map literal")
        .split("}~")
        .next()
        .expect("classState map literal must close with }~");

    assert_lacks(
        "Unassigned `late classState:` slot 'current' must be absent from the classState map entirely. Got classState body",
        class_state,
        &["'current'"],
    );
    assert_has(
        "Eager class var 'total' must still get its default in the classState map. Got classState body",
        class_state,
        &["'total' => 0"],
    );
}

#[test]
fn test_no_late_fields_emit_empty_field_kinds_when_no_state() {
    // A class with no state/classState declarations still emits the
    // 'field_kinds'/'class_field_kinds' keys (empty maps), matching
    // 'class_fields' emitting `[]` in test_no_class_state_emits_empty_class_fields
    // — the runtime intrinsic always finds the key.
    let src = "Actor subclass: Empty\n  noop => nil\n";
    let code = codegen_named(src, "empty");

    assert_has(
        "Should emit empty field_kinds map",
        &code,
        &["'field_kinds' => ~{}~"],
    );
    assert_has(
        "Should emit empty class_field_kinds map",
        &code,
        &["'class_field_kinds' => ~{}~"],
    );
}

#[test]
fn test_no_class_registration_for_empty_module() {
    // Modules without class definitions should not have on_load or register_class
    let module = Module::new(vec![], Span::new(0, 0));
    let code = generate_module(&module, CodegenOptions::new("empty_module"))
        .expect("codegen should succeed");

    // Should NOT have on_load attribute
    assert_lacks(
        "Module without classes should not have on_load",
        &code,
        &["'on_load'"],
    );

    // Should NOT export register_class/0
    assert_lacks(
        "Module without classes should not export register_class",
        &code,
        &["'register_class'/0"],
    );

    // Should NOT have beamtalk_class attribute
    assert_lacks(
        "Module without classes should not have beamtalk_class attribute",
        &code,
        &["'beamtalk_class'"],
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn test_multiple_classes_registration() {
    // Test that modules with multiple classes register all of them
    use beamtalk_core::ast::{ClassDefinition, Identifier, StateDeclaration};
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
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                slot_kind: beamtalk_core::ast::SlotKind::default(),
                ..StateDeclaration::new(
                    Identifier::new(field, Span::new(0, field_len)),
                    Span::new(0, 10),
                )
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            uses: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            handle_scope: None,
            shape_version: None,
            span: Span::new(0, span_end),
        }
    }

    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![
            make_actor_class("Counter", 7, "value", 5, 20),
            make_actor_class("Logger", 6, "messages", 8, 30),
        ],
        span: Span::new(0, 50),
        ..empty_module()
    };

    let code = generate_module(&module, CodegenOptions::new("multi_actors"))
        .expect("codegen should succeed");

    // Should have on_load attribute
    assert_has(
        "Should have on_load attribute for multiple classes",
        &code,
        &["'on_load' = [{'register_class', 0}]"],
    );

    // Should register both classes via ClassBuilder
    assert_has(
        "Should register Counter via ClassBuilder",
        &code,
        &["call 'beamtalk_class_builder':'register'(_BuilderState0)"],
    );
    assert_has(
        "Should include Counter metadata",
        &code,
        &["'className' => 'Counter'"],
    );
    // fieldSpecs removed from BuilderState; fields now in meta map
    assert_has(
        "Should include Counter fields in meta",
        &code,
        &["'fields' => ['value']"],
    );

    assert_has(
        "Should register Logger via ClassBuilder",
        &code,
        &["call 'beamtalk_class_builder':'register'(_BuilderState1)"],
    );
    assert_has(
        "Should include Logger metadata",
        &code,
        &["'className' => 'Logger'"],
    );
    assert_has(
        "Should include Logger fields in meta",
        &code,
        &["'fields' => ['messages']"],
    );

    // Should use let-binding chain to sequence registrations
    assert_has(
        "Should have first BuilderState binding",
        &code,
        &["let _BuilderState0 = ~{"],
    );
    assert_has(
        "Should have first registration with _Reg0",
        &code,
        &["let _Reg0 = case"],
    );
    assert_has(
        "Should have second BuilderState binding",
        &code,
        &["let _BuilderState1 = ~{"],
    );
    assert_has(
        "Should chain second registration with _Reg1",
        &code,
        &["let _Reg1 = case"],
    );

    // Final result propagates last _Reg.
    assert_has(
        "Should propagate last _Reg result after all registrations",
        &code,
        &["in _Reg1"],
    );

    // Short-circuit: earlier error must propagate before executing later classes.
    assert_has(
        "Should short-circuit on _Reg0 error",
        &code,
        &["in case _Reg0 of"],
    );
    assert_has(
        "Should propagate _Reg0 error",
        &code,
        &["<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"],
    );

    // Check beamtalk_class attribute lists both classes
    assert_has(
        "Should include beamtalk_class attribute with both classes",
        &code,
        &["'beamtalk_class' = [{'Counter', 'Actor'}, {'Logger', 'Actor'}]"],
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
    use beamtalk_core::ast::{ClassDefinition, Identifier, StateDeclaration};
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
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                slot_kind: beamtalk_core::ast::SlotKind::default(),
                ..StateDeclaration::new(Identifier::new("x", Span::new(0, 1)), Span::new(0, 5))
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            uses: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            handle_scope: None,
            shape_version: None,
            span: Span::new(0, span_end),
        }
    }

    // Two classes: ShadowA (index 0), ValidB (index 1, last).
    // ValidB is fine; ShadowA would be the one shadowing stdlib.
    // The fix must ensure that if _Reg0 is {error, ...}, we never reach _Reg1.
    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![make_class("ShadowA", 7, 20), make_class("ValidB", 6, 30)],
        span: Span::new(0, 50),
        ..empty_module()
    };

    let code = generate_module(&module, CodegenOptions::new("multi_shadow"))
        .expect("codegen should succeed");

    // First class must be wrapped in a short-circuit case check.
    assert_has(
        "Should wrap _Reg0 in a short-circuit case",
        &code,
        &["in case _Reg0 of"],
    );
    assert_has(
        "Should propagate _Reg0 error before executing later classes",
        &code,
        &["<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"],
    );

    // The last class's result is returned directly (no further wrapping needed).
    assert_has("Should use _Reg1 as the final result", &code, &["in _Reg1"]);

    // The second class must NOT be wrapped in its own short-circuit case
    // (it is the last, so its result flows out directly).
    assert_lacks(
        "Last _Reg should not be wrapped in a short-circuit case",
        &code,
        &["in case _Reg1 of"],
    );
}

#[test]
fn test_three_class_short_circuit_nesting() {
    // Verify nesting correctness for N=3 classes.
    // Short-circuit cases are added for indices 0 and 1 (all except the last).
    // The last class (index 2) is returned directly with no extra wrapping.
    use beamtalk_core::ast::{ClassDefinition, Identifier, StateDeclaration};
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
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                slot_kind: beamtalk_core::ast::SlotKind::default(),
                ..StateDeclaration::new(Identifier::new("x", Span::new(0, 1)), Span::new(0, 5))
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            uses: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            handle_scope: None,
            shape_version: None,
            span: Span::new(0, 20),
        }
    }

    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![make_class("A", 1), make_class("B", 1), make_class("C", 1)],
        span: Span::new(0, 60),
        ..empty_module()
    };

    let code = generate_module(&module, CodegenOptions::new("three_classes"))
        .expect("codegen should succeed");

    // Classes 0 and 1 (non-last) must have short-circuit case wrappers.
    assert_has(
        "Should short-circuit on _Reg0 error",
        &code,
        &["in case _Reg0 of"],
    );
    assert_has(
        "Should propagate _Reg0 error",
        &code,
        &["<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"],
    );
    assert_has(
        "Should short-circuit on _Reg1 error",
        &code,
        &["in case _Reg1 of"],
    );
    assert_has(
        "Should propagate _Reg1 error",
        &code,
        &["<{'error', _RegErr1}> when 'true' -> {'error', _RegErr1}"],
    );

    // Class 2 (last) must be returned directly — no extra case wrapping.
    assert_has("Should use _Reg2 as final result", &code, &["in _Reg2"]);
    assert_lacks(
        "Last _Reg should not be wrapped in a short-circuit case",
        &code,
        &["in case _Reg2 of"],
    );
}

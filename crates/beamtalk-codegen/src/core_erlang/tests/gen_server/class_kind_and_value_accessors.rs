// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor/value class-kind classification (`is_actor_class`) and
//! value-subclass auto-accessor codegen: getters, setters, keyword
//! constructors, `has_method`, synthetic accessor metadata, and
//! method/state-field return-type writeback.

use super::*;

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
    // MyBaseError is a grandchild of Exception (Exception -> Error -> MyBaseError);
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
    // Class methods should reject instance field access
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
    // Class methods should reject instance field mutation
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
    // `Value subclass:` auto-generates getter functions for each slot.
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
    // Getter body uses maps:get to read the slot from Self.
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
    // `Value subclass:` auto-generates with*: functional setters.
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
    // with*: setter body uses maps:put to return an updated map.
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
    // `Value subclass:` auto-generates an all-fields keyword constructor.
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
    // Keyword constructor body creates a tagged map with all slots.
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
    // dispatch/3 must route getter selectors to auto-generated functions.
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
    // dispatch/3 must route with*: selectors to auto-generated functions.
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
    // has_method/1 must report true for auto-generated selectors.
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

/// Extracts the body between a `<key> => ~{` marker and its closing
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
    // auto-generated getters / setters / keyword constructor gain
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
    // only `Value subclass:` classes get synthetic accessor metadata.
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
    // `Object subclass:` (ClassKind::Object) must NOT generate auto-getters.
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
    // User-defined methods suppress the corresponding auto-generated method.
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
    // A Value subclass with no slots produces no keyword constructor.
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
    // `ClassName slot: value` inside a class method of the same class must
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
    // A user-defined Actor class method with no explicit return-type
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
    // return types now live in meta.method_info, not methodReturnTypes.
    assert!(
        code.contains(
            "'getValue' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false', 'visibility' => 'public'}~"
        ),
        "meta.method_info should contain inferred return type for unannotated getValue. Got:\n{code}"
    );
}

#[test]
fn test_bt3249_method_source_omits_inferred_return_type_annotation() {
    // `getValue` has no explicit `-> Type` annotation in source —
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
    // a compiled actor's generated handle_call/handle_cast must call
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

    // handle_call strips codegen-internal `__local__` threading temps from
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
    // handle_continue is an outermost state-commit boundary (it persists
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
    // a Server subclass's handle_info is an outermost state-commit boundary
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
    // An explicitly annotated method must NOT be changed by the writeback pass.
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
    // a genuine user-written annotation must still round-trip
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
    // A method returning an integer literal should have Integer inferred
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
    // Tonel-style standalone method definitions (Counter >> getValue => ...)
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
    // a class-level `sealed` must not leak into an individual class
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
    // An untyped parameter with the same name as a state field must NOT
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

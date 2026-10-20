// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for supervisor module Core Erlang code generation (ADR 0059 Phase 3).

use super::*;

/// Build a minimal `Supervisor subclass: WebApp` module with `supervisor_kind = Static`.
fn make_static_supervisor_module() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("WebApp", Span::new(0, 0)),
        superclass: Some(Identifier::new("Supervisor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: Some(SupervisorKind::Static),
        state: vec![],
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
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    }
}

/// Build a minimal `DynamicSupervisor subclass: WorkerPool` module.
fn make_dynamic_supervisor_module() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("WorkerPool", Span::new(0, 0)),
        superclass: Some(Identifier::new("DynamicSupervisor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: Some(SupervisorKind::Dynamic),
        state: vec![],
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
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    }
}

#[test]
fn test_static_supervisor_has_supervisor_behaviour() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'behaviour' = ['supervisor']"),
        "Static supervisor must have supervisor behaviour. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_no_gen_server_behaviour() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        !code.contains("'behaviour' = ['gen_server']"),
        "Static supervisor must NOT have gen_server behaviour. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_exports_start_link() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'start_link'/0"),
        "Static supervisor must export start_link/0. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_exports_init() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'init'/1"),
        "Static supervisor must export init/1. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_start_link_calls_supervisor_start_link() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("call 'supervisor':'start_link'"),
        "start_link/0 must call supervisor:start_link. Got:\n{code}"
    );
    assert!(
        code.contains("{'local', 'bt@webapp'}"),
        "start_link/0 must register locally under module name. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_init_delegates_to_static_init() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    // init/1 must delegate to beamtalk_supervisor:static_init/3 to avoid a gen_server
    // deadlock: the class gen_server is blocked waiting for supervisor:start_link to return,
    // so calling class_send from init/1 would deadlock. ADR 0135: the start args are
    // forwarded so the configuration methods can read `self args`.
    // Match the exact call shape to verify argument order (module, class, args).
    assert!(
        code.contains("'init'/1 = fun (Args) ->"),
        "init/1 must bind its start args. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_supervisor':'static_init'('bt@webapp', 'WebApp', Args)"),
        "init/1 must delegate to beamtalk_supervisor:static_init/3 with module, class and args. Got:\n{code}"
    );
    // Must NOT contain direct class_send calls — those would deadlock.
    assert!(
        !code.contains("beamtalk_object_class':'class_send'"),
        "init/1 must NOT call class_send directly (causes deadlock). Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_has_on_load_register_class() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Supervisor module must have on_load register_class. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_has_register_class_function() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'register_class'/0"),
        "Supervisor module must export register_class/0. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_has_superclass_function() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'superclass'/0"),
        "Supervisor module must export superclass/0. Got:\n{code}"
    );
    assert!(
        code.contains("'Supervisor'"),
        "superclass/0 must return 'Supervisor'. Got:\n{code}"
    );
}

#[test]
fn test_dynamic_supervisor_has_supervisor_behaviour() {
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    assert!(
        code.contains("'behaviour' = ['supervisor']"),
        "Dynamic supervisor must have supervisor behaviour. Got:\n{code}"
    );
}

#[test]
fn test_dynamic_supervisor_init_delegates_to_dynamic_init() {
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    // simple_one_for_one is set inside beamtalk_supervisor:dynamic_init/3 (runtime),
    // not in the generated init/1. Verify the delegation with correct argument order.
    assert!(
        code.contains(
            "call 'beamtalk_supervisor':'dynamic_init'('bt@workerpool', 'WorkerPool', Args)"
        ),
        "Dynamic supervisor init/1 must delegate to beamtalk_supervisor:dynamic_init/3 with module, class and args. Got:\n{code}"
    );
}

#[test]
fn test_supervisor_start_link_forms() {
    // ADR 0135: start_link/0 (what `supervise` runs) registers under the module
    // name with `nil` start args; start_link/1 starts an anonymous instance with
    // the given args, so a parent can pass args.
    for (module, name) in [
        (make_static_supervisor_module(), "bt@webapp"),
        (make_dynamic_supervisor_module(), "bt@workerpool"),
    ] {
        let code =
            generate_module(&module, CodegenOptions::new(name)).expect("codegen should succeed");
        assert!(
            code.contains("'start_link'/0, 'start_link'/1, 'init'/1"),
            "must export start_link/0,1. Got:\n{code}"
        );
        assert!(
            code.contains(&format!(
                "call 'supervisor':'start_link'({{'local', '{name}'}}, '{name}', 'nil')"
            )),
            "start_link/0 must pass nil start args. Got:\n{code}"
        );
        assert!(
            code.contains(&format!(
                "'start_link'/1 = fun (Args) ->\n    call 'supervisor':'start_link'('{name}', Args)"
            )),
            "start_link/1 must start an anonymous instance with Args. Got:\n{code}"
        );
    }
}

#[test]
fn test_dynamic_supervisor_init_does_not_fetch_children() {
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    // DynamicSupervisor does not call 'children' — that's only for static supervisors
    // (though 'children' may appear in other contexts like the selector atom in class_send args)
    // The key is that 'children' as a selector should NOT appear in the dynamic init/1.
    // We check that childClass IS present and simple_one_for_one IS used instead.
    assert!(
        !code.contains("'children'"),
        "Dynamic supervisor init/1 must NOT call 'children'. Got:\n{code}"
    );
}

#[test]
fn test_dynamic_supervisor_does_not_emit_class_side_child_class() {
    // ADR 0135 §2: the child class is per instance (read by dynamic_init/3 and
    // stored in the pool's process dictionary); there is no class-side
    // `childClass/0` for the runtime to call any more.
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    assert!(
        !code.contains("'childClass'/0"),
        "Dynamic supervisor must not export/define 'childClass'/0. Got:\n{code}"
    );
}

#[test]
fn test_supervisor_instance_methods_are_compiled_with_dispatch() {
    // ADR 0135 §2: configuration is instance-side. The supervisor module must
    // compile the class's instance methods and export the dispatch entry points
    // the runtime's instance dispatch probes; unknown selectors delegate to the
    // stdlib Supervisor base, and has_method_local/1 answers only for the
    // class's own selectors (so inherited ones keep the base as their owner).
    let mut module = make_static_supervisor_module();
    module.classes[0].methods.push(MethodDefinition {
        selector: MessageSelector::Unary("childSpecs".into()),
        parameters: vec![],
        body: vec![bare(Expression::Literal(
            Literal::List(vec![]),
            Span::new(0, 0),
        ))],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: Span::new(0, 0),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    });
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    for export in [
        "'childSpecs'/1",
        "'dispatch'/3",
        "'dispatch'/4",
        "'has_method'/1",
        "'has_method_local'/1",
    ] {
        assert!(code.contains(export), "must export {export}. Got:\n{code}");
    }
    assert!(
        code.contains(
            "<'childSpecs'> when 'true' ->\n            call 'bt@webapp':'childSpecs'(Self)"
        ),
        "dispatch/3 must route childSpecs to its function. Got:\n{code}"
    );
    assert!(
        code.contains("call 'bt@stdlib@supervisor':'dispatch'(Selector, Args, Self)")
            && code.contains("call 'beamtalk_extensions':'lookup'('WebApp', Selector)"),
        "dispatch/3 must delegate other selectors to the Supervisor base. Got:\n{code}"
    );
    let local = code
        .split("'has_method_local'/1 = fun")
        .nth(1)
        .expect("has_method_local/1 must be defined");
    assert!(
        local.contains("['childSpecs']") && local.contains("<'false'> when 'true' -> 'false'"),
        "has_method_local/1 must answer only the class's own selectors. Got:\n{code}"
    );
}

#[test]
fn test_is_actor_class_returns_true_for_supervisor_subclass() {
    // Verify that supervisor subclasses route through generate_actor_module
    // (which then delegates to supervisor_codegen).
    let module = make_static_supervisor_module();
    let (hierarchy, _) =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module);
    let hierarchy = hierarchy.unwrap();
    assert!(
        CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Supervisor subclass must be routed through generate_actor_module. Got false."
    );
}

#[test]
fn test_is_actor_class_returns_true_for_dynamic_supervisor_subclass() {
    let module = make_dynamic_supervisor_module();
    let (hierarchy, _) =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module);
    let hierarchy = hierarchy.unwrap();
    assert!(
        CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "DynamicSupervisor subclass must be routed through generate_actor_module. Got false."
    );
}

#[test]
fn test_static_supervisor_with_user_class_method_exports_it() {
    // A concrete WebApp with a user class method (here `class children`, which the
    // validator rejects since ADR 0135, but codegen still compiles) exports it.
    let children_method = MethodDefinition {
        selector: MessageSelector::Unary("children".into()),
        parameters: vec![],
        body: vec![bare(Expression::Literal(
            Literal::List(vec![]),
            Span::new(0, 0),
        ))],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: Span::new(0, 0),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };
    let class = ClassDefinition {
        name: Identifier::new("WebApp", Span::new(0, 0)),
        superclass: Some(Identifier::new("Supervisor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: Some(SupervisorKind::Static),
        state: vec![],
        methods: vec![],
        class_methods: vec![children_method],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'class_children'/1"),
        "Static supervisor with user class method must export class_children/1. Got:\n{code}"
    );
}

#[test]
fn test_static_supervisor_class_method_alias_param_emits_user_type_and_named_type() {
    // `supervisor_codegen.rs`'s `generate_class_specs` call site
    // must resolve alias-typed annotations to `user_type` references,
    // with the module also declaring the matching named `-type` in the
    // same attribute list (an `erlc` compile error otherwise).
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
    let strategy_method = MethodDefinition {
        selector: MessageSelector::Keyword(vec![KeywordPart::new(
            "defaultStrategy:",
            Span::new(0, 0),
        )]),
        parameters: vec![ParameterDefinition::with_type(
            Identifier::new("policy", Span::new(0, 0)),
            TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)),
        )],
        body: vec![bare(Expression::Identifier(Identifier::new(
            "policy",
            Span::new(0, 0),
        )))],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: Span::new(0, 0),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };
    let class = ClassDefinition {
        name: Identifier::new("WebApp", Span::new(0, 0)),
        superclass: Some(Identifier::new("Supervisor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: Some(SupervisorKind::Static),
        state: vec![],
        methods: vec![],
        class_methods: vec![strategy_method],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: vec![strategy_alias],
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
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
fn test_static_supervisor_cross_module_alias_reference_emits_user_type() {
    // same wiring check as
    // `test_static_supervisor_class_method_alias_param_emits_user_type_and_named_type`
    // above, but the alias is declared in a *different* module — passed via
    // `CodegenOptions::with_pre_loaded_aliases` instead of this module's own
    // `type_aliases` — the cross-module case `supervisor_codegen.rs`'s
    // `generate_static_supervisor` couldn't resolve before this issue, since
    // it only ever built `AliasRegistry::from_module_declarations(module)`.
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
    let strategy_method = MethodDefinition {
        selector: MessageSelector::Keyword(vec![KeywordPart::new(
            "defaultStrategy:",
            Span::new(0, 0),
        )]),
        parameters: vec![ParameterDefinition::with_type(
            Identifier::new("policy", Span::new(0, 0)),
            TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)),
        )],
        body: vec![bare(Expression::Identifier(Identifier::new(
            "policy",
            Span::new(0, 0),
        )))],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: Span::new(0, 0),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };
    let class = ClassDefinition {
        name: Identifier::new("WebApp", Span::new(0, 0)),
        superclass: Some(Identifier::new("Supervisor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: Some(SupervisorKind::Static),
        state: vec![],
        methods: vec![],
        class_methods: vec![strategy_method],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 0),
    };
    // No `type_aliases` of its own — the module only references the name.
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@webapp_cross_module").with_pre_loaded_aliases(pre_loaded_aliases),
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
fn test_dynamic_supervisor_cross_module_alias_reference_emits_user_type() {
    // same wiring check as
    // `test_static_supervisor_cross_module_alias_reference_emits_user_type`
    // above, but for `supervisor_codegen.rs`'s *second* call site,
    // `generate_dynamic_supervisor` (`DynamicSupervisor subclass:`) — the
    // two call sites are separate copies of the same
    // `AliasRegistry`-consuming code, so both need coverage.
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
    let strategy_method = MethodDefinition {
        selector: MessageSelector::Keyword(vec![KeywordPart::new(
            "defaultStrategy:",
            Span::new(0, 0),
        )]),
        parameters: vec![ParameterDefinition::with_type(
            Identifier::new("policy", Span::new(0, 0)),
            TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)),
        )],
        body: vec![bare(Expression::Identifier(Identifier::new(
            "policy",
            Span::new(0, 0),
        )))],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: Span::new(0, 0),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };
    let class = ClassDefinition {
        name: Identifier::new("WorkerPool", Span::new(0, 0)),
        superclass: Some(Identifier::new("DynamicSupervisor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: Some(SupervisorKind::Dynamic),
        state: vec![],
        methods: vec![],
        class_methods: vec![strategy_method],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 0),
    };
    // No `type_aliases` of its own — the module only references the name.
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@workerpool_cross_module")
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
fn test_dynamic_supervisor_with_user_class_method_exports_it() {
    // A `DynamicSupervisor subclass: WorkerPool` with a user-defined class
    // method (e.g. `class maxConcurrency => ...`) must export it — this is the
    // same invariant as the static case (see
    // `test_static_supervisor_with_user_class_method_exports_it`).
    let max_concurrency_method = MethodDefinition {
        selector: MessageSelector::Unary("maxConcurrency".into()),
        parameters: vec![],
        body: vec![bare(Expression::Literal(
            Literal::Integer(10),
            Span::new(0, 0),
        ))],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: Span::new(0, 0),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };
    let class = ClassDefinition {
        name: Identifier::new("WorkerPool", Span::new(0, 0)),
        superclass: Some(Identifier::new("DynamicSupervisor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: Some(SupervisorKind::Dynamic),
        state: vec![],
        methods: vec![],
        class_methods: vec![max_concurrency_method],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    // Pin the export in the module header specifically, not just anywhere in the
    // output (the function definition also contains `'class_maxConcurrency'/1`).
    let header_exports = code
        .split_once("module 'bt@workerpool' [")
        .expect("generated code must contain a module header")
        .1
        .split_once(']')
        .expect("module header export list must be closed")
        .0;
    assert!(
        header_exports.contains("'class_maxConcurrency'/1"),
        "Dynamic supervisor with user class method must export class_maxConcurrency/1 \
         in the module header. Got header exports:\n{header_exports}"
    );
}

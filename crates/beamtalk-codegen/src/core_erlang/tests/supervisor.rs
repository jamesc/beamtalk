// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for supervisor module Core Erlang code generation (ADR 0059 Phase 3).

use super::*;

/// Build a minimal `Supervisor subclass: WebApp` module with `supervisor_kind = Static`.
pub(super) fn make_static_supervisor_module() -> Module {
    let class = ClassDefinition {
        supervisor_kind: Some(SupervisorKind::Static),
        ..ClassDefinition::test("WebApp", Some("Supervisor"), ClassKind::Object)
    };
    Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    }
}

/// Build a minimal `DynamicSupervisor subclass: WorkerPool` module.
fn make_dynamic_supervisor_module() -> Module {
    let class = ClassDefinition {
        supervisor_kind: Some(SupervisorKind::Dynamic),
        ..ClassDefinition::test("WorkerPool", Some("DynamicSupervisor"), ClassKind::Object)
    };
    Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    }
}

#[test]
fn test_static_supervisor_has_supervisor_behaviour() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "Static supervisor must have supervisor behaviour",
        &code,
        &["'behaviour' = ['supervisor']"],
    );
}

#[test]
fn test_static_supervisor_no_gen_server_behaviour() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_lacks(
        "Static supervisor must NOT have gen_server behaviour",
        &code,
        &["'behaviour' = ['gen_server']"],
    );
}

#[test]
fn test_static_supervisor_exports_start_link() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "Static supervisor must export start_link/0",
        &code,
        &["'start_link'/0"],
    );
}

#[test]
fn test_static_supervisor_exports_init() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has("Static supervisor must export init/1", &code, &["'init'/1"]);
}

#[test]
fn test_static_supervisor_start_link_calls_supervisor_start_link() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "start_link/0 must call supervisor:start_link",
        &code,
        &["call 'supervisor':'start_link'"],
    );
    assert_has(
        "start_link/0 must register locally under module name",
        &code,
        &["{'local', 'bt@webapp'}"],
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
    assert_has(
        "init/1 must bind its start args",
        &code,
        &["'init'/1 = fun (Args) ->"],
    );
    assert_has(
        "init/1 must delegate to beamtalk_supervisor:static_init/3 with module, class and args",
        &code,
        &["call 'beamtalk_supervisor':'static_init'('bt@webapp', 'WebApp', Args)"],
    );
    // Must NOT contain direct class_send calls — those would deadlock.
    assert_lacks(
        "init/1 must NOT call class_send directly (causes deadlock)",
        &code,
        &["beamtalk_object_class':'class_send'"],
    );
}

#[test]
fn test_static_supervisor_has_on_load_register_class() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "Supervisor module must have on_load register_class",
        &code,
        &["'on_load' = [{'register_class', 0}]"],
    );
}

#[test]
fn test_static_supervisor_has_register_class_function() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "Supervisor module must export register_class/0",
        &code,
        &["'register_class'/0"],
    );
}

#[test]
fn test_static_supervisor_has_superclass_function() {
    let module = make_static_supervisor_module();
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "Supervisor module must export superclass/0",
        &code,
        &["'superclass'/0"],
    );
    assert_has(
        "superclass/0 must return 'Supervisor'",
        &code,
        &["'Supervisor'"],
    );
}

#[test]
fn test_dynamic_supervisor_has_supervisor_behaviour() {
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    assert_has(
        "Dynamic supervisor must have supervisor behaviour",
        &code,
        &["'behaviour' = ['supervisor']"],
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
        assert_has(
            "must export start_link/0,1,2",
            &code,
            &["'start_link'/0, 'start_link'/1, 'start_link'/2, 'init'/1"],
        );
        assert!(
            code.contains(&format!(
                "'start_link'/2 = fun (Name, Args) ->\n    call 'supervisor':'start_link'({{'local', Name}}, '{name}', Args)"
            )),
            "start_link/2 must register the instance under Name (ADR 0135 §3). Got:\n{code}"
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
    assert_lacks(
        "Dynamic supervisor init/1 must NOT call 'children'",
        &code,
        &["'children'"],
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
    assert_lacks(
        "Dynamic supervisor must not export/define 'childClass'/0",
        &code,
        &["'childClass'/0"],
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
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        body: vec![bare(Expression::Literal(
            Literal::List(vec![]),
            Span::new(0, 0),
        ))],
        ..MethodDefinition::test("childSpecs")
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
            && !code.contains("beamtalk_extensions"),
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
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        body: vec![bare(Expression::Literal(
            Literal::List(vec![]),
            Span::new(0, 0),
        ))],
        ..MethodDefinition::test("children")
    };
    let class = ClassDefinition {
        supervisor_kind: Some(SupervisorKind::Static),
        class_methods: vec![children_method],
        ..ClassDefinition::test("WebApp", Some("Supervisor"), ClassKind::Object)
    };
    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    };
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "Static supervisor with user class method must export class_children/1",
        &code,
        &["'class_children'/1"],
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
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        ..MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("defaultStrategy:", Span::new(0, 0))]),
            vec![ParameterDefinition::with_type(
                Identifier::new("policy", Span::new(0, 0)),
                TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)),
            )],
            vec![bare(Expression::Identifier(Identifier::new(
                "policy",
                Span::new(0, 0),
            )))],
            Span::new(0, 0),
        )
    };
    let class = ClassDefinition {
        supervisor_kind: Some(SupervisorKind::Static),
        class_methods: vec![strategy_method],
        ..ClassDefinition::test("WebApp", Some("Supervisor"), ClassKind::Object)
    };
    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        type_aliases: vec![strategy_alias],
        ..empty_module()
    };
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert_has(
        "class method param typed with the alias should emit a user_type reference",
        &code,
        &["{'user_type', 0, 'restart_strategy', []}"],
    );
    assert_has(
        "module must declare the matching named -type for the alias",
        &code,
        &["'restart_strategy'"],
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
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        ..MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("defaultStrategy:", Span::new(0, 0))]),
            vec![ParameterDefinition::with_type(
                Identifier::new("policy", Span::new(0, 0)),
                TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)),
            )],
            vec![bare(Expression::Identifier(Identifier::new(
                "policy",
                Span::new(0, 0),
            )))],
            Span::new(0, 0),
        )
    };
    let class = ClassDefinition {
        supervisor_kind: Some(SupervisorKind::Static),
        class_methods: vec![strategy_method],
        ..ClassDefinition::test("WebApp", Some("Supervisor"), ClassKind::Object)
    };
    // No `type_aliases` of its own — the module only references the name.
    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
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
    assert_has(
        "module must declare the matching named -type for the cross-module alias",
        &code,
        &["'restart_strategy'"],
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
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        ..MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("defaultStrategy:", Span::new(0, 0))]),
            vec![ParameterDefinition::with_type(
                Identifier::new("policy", Span::new(0, 0)),
                TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)),
            )],
            vec![bare(Expression::Identifier(Identifier::new(
                "policy",
                Span::new(0, 0),
            )))],
            Span::new(0, 0),
        )
    };
    let class = ClassDefinition {
        supervisor_kind: Some(SupervisorKind::Dynamic),
        class_methods: vec![strategy_method],
        ..ClassDefinition::test("WorkerPool", Some("DynamicSupervisor"), ClassKind::Object)
    };
    // No `type_aliases` of its own — the module only references the name.
    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
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
    assert_has(
        "module must declare the matching named -type for the cross-module alias",
        &code,
        &["'restart_strategy'"],
    );
}

#[test]
fn test_dynamic_supervisor_with_user_class_method_exports_it() {
    // A `DynamicSupervisor subclass: WorkerPool` with a user-defined class
    // method (e.g. `class maxConcurrency => ...`) must export it — this is the
    // same invariant as the static case (see
    // `test_static_supervisor_with_user_class_method_exports_it`).
    let max_concurrency_method = MethodDefinition {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        body: vec![bare(Expression::Literal(
            Literal::Integer(10),
            Span::new(0, 0),
        ))],
        ..MethodDefinition::test("maxConcurrency")
    };
    let class = ClassDefinition {
        supervisor_kind: Some(SupervisorKind::Dynamic),
        class_methods: vec![max_concurrency_method],
        ..ClassDefinition::test("WorkerPool", Some("DynamicSupervisor"), ClassKind::Object)
    };
    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
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

#[test]
fn test_class_ref_spawn_family_on_supervisor_lowers_to_class_start() {
    // ADR 0135 §1: `ClassRef spawn…` on a supervisor class starts the tree in
    // the caller through the runtime start helper, never the actor spawn
    // intrinsic (a supervisor module has no `spawn/0,1`) nor class dispatch.
    let src = "Object subclass: Foo\n  a => Leaf spawn\n  b => Leaf spawnWith: #{#n => 1}\n  c => Leaf spawnAs: #leaf\n  d => Leaf spawnWith: 1 as: #leaf\n  e => Pool spawn\n\nSupervisor subclass: Leaf\n  childSpecs => #()\n\nDynamicSupervisor subclass: Pool\n  childClass => Leaf\n";
    let code = codegen_named(src, "foo");
    for selector in ["'spawn'", "'spawnWith:'", "'spawnAs:'", "'spawnWith:as:'"] {
        assert!(
            code.contains(&format!(
                "call 'beamtalk_supervisor':'class_start'('Leaf', 'bt@leaf', {selector}, ["
            )),
            "Leaf {selector} must lower to class_start. Got:\n{code}"
        );
    }
    assert_has(
        "a DynamicSupervisor receiver takes the same carve-out",
        &code,
        &["call 'beamtalk_supervisor':'class_start'('Pool', 'bt@pool', 'spawn', [])"],
    );
    assert_lacks(
        "must not call the actor spawn intrinsic on a supervisor module",
        &code,
        &["'bt@leaf':'spawn'"],
    );
}

#[test]
fn test_spawn_monitored_on_supervisor_lowers_to_class_start() {
    // BT-4104: supervisor modules have no spawn_monitored/0,1, so the
    // monitored selectors on a supervisor receiver (class reference or
    // `self` in a class method) reach class_start/4, which raises a clear
    // unsupported error.
    let src = "Object subclass: Foo\n  a => Leaf spawnMonitored\n  b => Leaf spawnMonitoredWith: #{#n => 1}\n\nSupervisor subclass: Leaf\n  childSpecs => #()\n";
    let code = codegen_named(src, "foo");
    for selector in ["'spawnMonitored', []", "'spawnMonitoredWith:', ["] {
        assert!(
            code.contains(&format!(
                "call 'beamtalk_supervisor':'class_start'('Leaf', 'bt@leaf', {selector}"
            )),
            "Leaf {selector} must lower to class_start. Got:\n{code}"
        );
    }
    assert_lacks(
        "must not call a spawn_monitored export on a supervisor module",
        &code,
        &["'spawn_monitored'"],
    );

    let src =
        "Supervisor subclass: Leaf\n  class one => self spawnMonitored\n  childSpecs => #()\n";
    let code = codegen_named(src, "bt@leaf");
    assert_has(
        "self spawnMonitored in a supervisor class method must lower to class_start",
        &code,
        &["'spawnMonitored', []"],
    );
    assert_lacks(
        "must not use the actor class_self_spawn_monitored helper",
        &code,
        &["class_self_spawn_monitored"],
    );
}

#[test]
fn test_spawn_monitored_on_actor_keeps_actor_intrinsics() {
    // BT-4104: an actor receiver calls the module's own spawn_monitored/0,1
    // from the sender; `self spawnMonitored` in a class method lowers to
    // class_self_spawn_monitored.
    let src = "Object subclass: Foo\n  a => Counter spawnMonitored\n  b => Counter spawnMonitoredWith: #{#n => 1}\n\nActor subclass: Counter\n  state: n = 0\n";
    let code = codegen_named(src, "foo");
    assert_has("Got", &code, &["call 'bt@counter':'spawn_monitored'()"]);
    assert_has("Got", &code, &["call 'bt@counter':'spawn_monitored'(~{"]);
    assert!(!code.contains("'class_start'"), "Got:\n{code}");

    let src = "Actor subclass: Counter\n  state: n = 0\n  class make => self spawnMonitored\n  class makeWith: m => self spawnMonitoredWith: m\n";
    let code = codegen_named(src, "bt@counter");
    assert_eq!(
        code.matches("call 'beamtalk_class_instantiation':'class_self_spawn_monitored'(")
            .count(),
        2,
        "self spawnMonitored/spawnMonitoredWith: must lower to class_self_spawn_monitored. Got:\n{code}"
    );
    assert!(
        code.contains("'resolve_is_abstract_or_raise'(") && code.contains("'spawnMonitoredWith:')"),
        "the abstract check must name the monitored selector. Got:\n{code}"
    );
}

#[test]
fn test_class_ref_spawn_on_actor_keeps_actor_intrinsic() {
    let src =
        "Object subclass: Foo\n  a => Counter spawn\n\nActor subclass: Counter\n  state: n = 0\n";
    let code = codegen_named(src, "foo");
    assert_has(
        "an actor receiver keeps the spawn intrinsic",
        &code,
        &["call 'bt@counter':'spawn'()"],
    );
    assert!(!code.contains("'class_start'"), "Got:\n{code}");
}

#[test]
fn test_self_spawn_family_in_supervisor_class_method_lowers_to_class_start() {
    // ADR 0135 §1: `self spawnWith:` in a class method of a supervisor class
    // starts an instance of the receiving class (resolved from `ClassSelf`)
    // through the runtime start helper, not `class_self_spawn`.
    let src = "Supervisor subclass: Leaf\n  class make: n => self spawnWith: n\n  class one => self spawn\n  childSpecs => #()\n";
    let code = codegen_named(src, "bt@leaf");
    assert!(
        code.contains(
            "call 'beamtalk_supervisor':'class_start'(call 'beamtalk_primitive':'class_name_from_tag'("
        ),
        "self spawn… must lower to class_start with the receiver's class. Got:\n{code}"
    );
    assert!(code.contains("'spawnWith:', ["), "Got:\n{code}");
    assert_lacks(
        "must not use the actor class_self_spawn helper",
        &code,
        &["class_self_spawn"],
    );
}

#[test]
fn test_spawn_monitored_async_on_supervisor_lowers_to_class_start() {
    // BT-4477: like `spawnMonitored`, the asynchronous pair has no supervisor
    // counterpart; a supervisor receiver reaches class_start/4's unsupported
    // error instead of a `spawn_monitored_async` export it does not have.
    let src = "Object subclass: Foo\n  a => Leaf spawnMonitoredAsync\n  b => Leaf spawnMonitoredAsyncWith: #{#n => 1}\n\nSupervisor subclass: Leaf\n  childSpecs => #()\n";
    let code = codegen_named(src, "foo");
    for selector in ["'spawnMonitoredAsync', []", "'spawnMonitoredAsyncWith:', ["] {
        assert!(
            code.contains(&format!(
                "call 'beamtalk_supervisor':'class_start'('Leaf', 'bt@leaf', {selector}"
            )),
            "Leaf {selector} must lower to class_start. Got:\n{code}"
        );
    }
    assert_lacks(
        "must not call a spawn_monitored_async export on a supervisor module",
        &code,
        &["'spawn_monitored_async'"],
    );
}

#[test]
fn test_spawn_monitored_async_on_actor_keeps_actor_intrinsics() {
    // BT-4477: an actor receiver calls the module's own
    // spawn_monitored_async/0,1 from the sender; `self spawnMonitoredAsync` in
    // a class method lowers to class_self_spawn_monitored_async.
    let src = "Object subclass: Foo\n  a => Counter spawnMonitoredAsync\n  b => Counter spawnMonitoredAsyncWith: #{#n => 1}\n\nActor subclass: Counter\n  state: n = 0\n";
    let code = codegen_named(src, "foo");
    assert_has(
        "Got",
        &code,
        &["call 'bt@counter':'spawn_monitored_async'()"],
    );
    assert_has(
        "Got",
        &code,
        &["call 'bt@counter':'spawn_monitored_async'(~{"],
    );
    assert!(!code.contains("'class_start'"), "Got:\n{code}");

    let src = "Actor subclass: Counter\n  state: n = 0\n  class make => self spawnMonitoredAsync\n  class makeWith: m => self spawnMonitoredAsyncWith: m\n";
    let code = codegen_named(src, "bt@counter");
    assert_eq!(
        code.matches("call 'beamtalk_class_instantiation':'class_self_spawn_monitored_async'(")
            .count(),
        2,
        "self spawnMonitoredAsync… must lower to class_self_spawn_monitored_async. Got:\n{code}"
    );
}

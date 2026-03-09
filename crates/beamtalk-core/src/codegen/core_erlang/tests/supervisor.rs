// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for supervisor module Core Erlang code generation (BT-1220, ADR 0059 Phase 3).

use super::*;

/// Build a minimal `Supervisor subclass: WebApp` module with `supervisor_kind = Static`.
fn make_static_supervisor_module() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("WebApp", Span::new(0, 0)),
        superclass: Some(Identifier::new("Supervisor", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: Some(SupervisorKind::Static),
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: vec![],
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
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: Some(SupervisorKind::Dynamic),
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: vec![],
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
    // init/1 must delegate to beamtalk_supervisor:static_init/2 to avoid a gen_server
    // deadlock: the class gen_server is blocked waiting for supervisor:start_link to return,
    // so calling class_send from init/1 would deadlock.
    // The call must pass both the module atom and the class name atom so the runtime
    // can call class module functions directly (bypassing the gen_server) via ETS hierarchy walk.
    assert!(
        code.contains("beamtalk_supervisor':'static_init'"),
        "init/1 must delegate to beamtalk_supervisor:static_init/2. Got:\n{code}"
    );
    assert!(
        code.contains("'bt@webapp'"),
        "init/1 must pass module atom to static_init. Got:\n{code}"
    );
    assert!(
        code.contains("'WebApp'"),
        "init/1 must pass class name atom to static_init. Got:\n{code}"
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
fn test_dynamic_supervisor_init_uses_simple_one_for_one() {
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    // simple_one_for_one is set inside beamtalk_supervisor:dynamic_init/2 (runtime),
    // not in the generated init/1. Verify the delegation is correct.
    assert!(
        code.contains("beamtalk_supervisor':'dynamic_init'"),
        "Dynamic supervisor init/1 must delegate to beamtalk_supervisor:dynamic_init/2. Got:\n{code}"
    );
}

#[test]
fn test_dynamic_supervisor_init_fetches_child_class() {
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    assert!(
        code.contains("'childClass'"),
        "Dynamic supervisor init/1 must call 'childClass'. Got:\n{code}"
    );
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
fn test_dynamic_supervisor_exports_and_defines_child_class() {
    // beamtalk_supervisor:startChild/1,2 calls SupMod:'childClass'() directly at runtime.
    // Without this export, startChild will fail with 'undef'.
    let module = make_dynamic_supervisor_module();
    let code = generate_module(&module, CodegenOptions::new("bt@workerpool"))
        .expect("codegen should succeed");
    assert!(
        code.contains("'childClass'/0"),
        "Dynamic supervisor must export/define 'childClass'/0 (called by beamtalk_supervisor:startChild). Got:\n{code}"
    );
}

#[test]
fn test_is_actor_class_returns_true_for_supervisor_subclass() {
    // Verify that supervisor subclasses route through generate_actor_module
    // (which then delegates to supervisor_codegen).
    let module = make_static_supervisor_module();
    let (hierarchy, _) = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module);
    let hierarchy = hierarchy.unwrap();
    assert!(
        CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Supervisor subclass must be routed through generate_actor_module. Got false."
    );
}

#[test]
fn test_is_actor_class_returns_true_for_dynamic_supervisor_subclass() {
    let module = make_dynamic_supervisor_module();
    let (hierarchy, _) = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module);
    let hierarchy = hierarchy.unwrap();
    assert!(
        CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "DynamicSupervisor subclass must be routed through generate_actor_module. Got false."
    );
}

#[test]
fn test_static_supervisor_with_user_class_method_exports_it() {
    // A concrete WebApp with `class children => ...` should have class_children/2 exported.
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
        span: Span::new(0, 0),
        doc_comment: None,
        comments: CommentAttachment::default(),
    };
    let class = ClassDefinition {
        name: Identifier::new("WebApp", Span::new(0, 0)),
        superclass: Some(Identifier::new("Supervisor", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: Some(SupervisorKind::Static),
        state: vec![],
        methods: vec![],
        class_methods: vec![children_method],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };
    let code =
        generate_module(&module, CodegenOptions::new("bt@webapp")).expect("codegen should succeed");
    assert!(
        code.contains("'class_children'/2"),
        "Static supervisor with user class method must export class_children/2. Got:\n{code}"
    );
}

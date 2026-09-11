// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type-alias emission for value/actor class fields, including
//! cross-module alias references and pre-loaded-alias fallback.

use super::*;

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

#[test]
fn test_value_subclass_typed_fields_emit_type_alias() {
    // Value subclass with typed state: declarations emits '-type t()' attribute.
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
    // Value subclass with untyped state: declarations also emits '-type t()'
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
    // wiring the compile's `AliasRegistry` into `actor_codegen.rs`'s
    // `generate_class_specs` call site must make an alias-typed annotation
    // emit a `user_type` reference — and the module must also declare the
    // matching named `-type` in its own attribute list (an `erlc` compile
    // error otherwise). Actor *instance* methods don't get standalone specs
    // (— they're dispatch clauses inside `safe_dispatch/3`), so this
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
    // same wiring check as the actor test above, but for
    // `value_type_codegen.rs`'s `generate_type_alias`/`generate_class_specs`
    // call sites — a Value subclass's `state:` field typed with an alias
    // must reference the alias's named `-type` from inside the class's own
    // `-type t()` map alias, with the named `-type` declared
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
    // same wiring check as
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
    // Confirm generated Core Erlang for
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
    // an alias declared in one compiled module — simulated here by
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
    // review follow-up: the sibling same-module case is guarded
    // through erlc by `test_alias_annotated_actor_module_compiles_through_erlc`
    // — this exercises the cross-module case (alias declared in
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
    // Negative control: the same module, compiled without
    // `with_pre_loaded_aliases`, reproduces the gap that motivates
    // cross-module alias wiring — since the module has no local `type_aliases` of its own,
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
    // `generate_alias_type_attrs` used to emit a `-type` for every
    // name in the pre-loaded `AliasRegistry`, regardless of
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
    // Sibling of `test_unused_pre_loaded_alias_gets_no_type_declaration`
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
    // the correctness trap this issue exists to close — a
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

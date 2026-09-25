// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3596: `init/1` traps exits (`erlang:process_flag(trap_exit, true)`)
//! only for actors whose effective `terminate:` is overridden somewhere in
//! the class hierarchy, and `handle_info/2` intercepts a non-parent
//! `'EXIT'` before dispatch — see `gen_server::callbacks::generate_init_function`
//! / `generate_handle_info` and their `class_or_ancestor_overrides_terminate`
//! helper. The no-hierarchy AST-only fallback for that helper is unit-tested
//! directly in `gen_server::callbacks::tests`; this module covers the full
//! `generate_module` codegen output.

use super::*;

/// An actor that never overrides `terminate:` must NOT trap exits — its
/// `init/1` is unchanged (byte-identical) from before BT-3596.
#[test]
fn test_actor_without_terminate_override_does_not_trap_exit() {
    let src = concat!(
        "Actor subclass: PlainCounter\n",
        "  state: value = 0\n\n",
        "  increment => self.value := self.value + 1\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("plain_counter"))
        .expect("codegen should succeed");

    assert!(
        !code.contains("process_flag"),
        "init/1 for an actor without a terminate: override must not call \
         process_flag at all. Got:\n{code}"
    );
    assert!(
        !code.contains("trap_exit"),
        "init/1 for an actor without a terminate: override must not \
         reference trap_exit. Got:\n{code}"
    );
}

/// An actor that overrides `terminate:` must trap exits: `init/1`'s
/// non-helper (`__skip_initialize__` == false) branch sets
/// `erlang:process_flag(trap_exit, true)` before returning.
#[test]
fn test_actor_with_terminate_override_traps_exit_in_init() {
    let src = concat!(
        "Actor subclass: CleanupActor\n",
        "  state: value = 0\n\n",
        "  terminate: _reason => nil\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("cleanup_actor"))
        .expect("codegen should succeed");

    let init_body =
        extract_core_fn(&code, "'init'/1 = fun").expect("init/1 must be present in generated code");
    assert!(
        init_body.contains("call 'erlang':'process_flag'('trap_exit', 'true')"),
        "init/1 for an actor overriding terminate: must trap exits. Got:\n{init_body}"
    );
}

/// A subclass that does NOT itself override `terminate:`, but whose
/// ancestor does, must still trap exits — `class_or_ancestor_overrides_terminate`
/// walks the full hierarchy, not just the leaf class's own AST.
///
/// The ancestor is injected via `CodegenOptions::with_class_hierarchy` (a
/// `ClassInfo` snapshot), mirroring the existing cross-file-ancestor test
/// pattern in `actor_init_and_field_validation.rs`.
#[test]
fn test_actor_inheriting_terminate_override_from_cross_file_ancestor_traps_exit() {
    use beamtalk_core::ast::MethodKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use std::collections::HashMap;

    let ancestor = ClassInfo {
        surface_incomplete: false,
        name: ecow::EcoString::from("BaseCleanupActor"),
        superclass: Some(ecow::EcoString::from("Actor")),
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
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![MethodInfo {
            selector: "terminate:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "BaseCleanupActor".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![None],
            doc: None,

            origin: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    // LeafActor extends the cross-file BaseCleanupActor and does not define
    // its own terminate: — only BaseCleanupActor's AST is absent, its
    // ClassInfo is supplied via with_class_hierarchy.
    let src = concat!(
        "BaseCleanupActor subclass: LeafActor\n",
        "  state: value = 0\n\n",
        "  increment => self.value := self.value + 1\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("leaf_actor").with_class_hierarchy(vec![ancestor]),
    )
    .expect("codegen should succeed");

    let init_body =
        extract_core_fn(&code, "'init'/1 = fun").expect("init/1 must be present in generated code");
    assert!(
        init_body.contains("call 'erlang':'process_flag'('trap_exit', 'true')"),
        "init/1 for a subclass inheriting a terminate: override from an \
         ancestor must also trap exits. Got:\n{init_body}"
    );
}

/// A plain Actor subclass's `handle_info/2` (the default ignore-all
/// delegate) is unchanged: it still just calls `beamtalk_actor:handle_info/2`.
/// The `'EXIT'` interception for this path lives entirely in that shared
/// runtime function (`beamtalk_actor:handle_linked_exit/2`), not in codegen,
/// so a plain actor's generated `handle_info/2` is untouched by BT-3596.
#[test]
fn test_plain_actor_handle_info_still_delegates_to_runtime() {
    let src = "Actor subclass: PlainCounter\n  state: value = 0\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("plain_counter"))
        .expect("codegen should succeed");

    let info_body = extract_core_fn(&code, "'handle_info'/2 = fun")
        .expect("handle_info/2 must be present in generated code");
    assert!(
        info_body.contains("call 'beamtalk_actor':'handle_info'(Msg, State)"),
        "plain-Actor handle_info/2 must delegate to beamtalk_actor:handle_info/2. Got:\n{info_body}"
    );
}

/// A Server subclass's `handle_info/2` must intercept a linked `'EXIT'` via
/// `beamtalk_actor:handle_linked_exit/2` *before* dispatching `handleInfo:` —
/// falling through to the existing dispatch body only on `'pass'`.
#[test]
fn test_server_subclass_handle_info_intercepts_linked_exit_before_dispatch() {
    let src = concat!(
        "Server subclass: TickServer\n",
        "  state: count = 0\n\n",
        "  handleInfo: msg =>\n",
        "    msg == #tick ifTrue: [self.count := self.count + 1]\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("tick_server"))
        .expect("codegen should succeed");

    let info_body = extract_core_fn(&code, "'handle_info'/2 = fun")
        .expect("handle_info/2 must be present in generated code");

    assert!(
        info_body.contains("call 'beamtalk_actor':'handle_linked_exit'(Msg, State)"),
        "Server-subclass handle_info/2 must check handle_linked_exit/2 first. Got:\n{info_body}"
    );
    // The 'pass' arm must still contain the original handleInfo: dispatch.
    let pass_idx = info_body
        .find("<'pass'>")
        .expect("must have a 'pass' arm falling through to the original dispatch");
    let dispatch_idx = info_body
        .find("'safe_dispatch'('handleInfo:', [Msg], State)")
        .expect("original handleInfo: dispatch must still be present");
    assert!(
        pass_idx < dispatch_idx,
        "the handleInfo: dispatch must be nested inside the 'pass' arm, after \
         the handle_linked_exit check. Got:\n{info_body}"
    );
}

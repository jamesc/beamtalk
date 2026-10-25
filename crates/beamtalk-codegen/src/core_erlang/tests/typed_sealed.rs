// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4069: sends to a receiver typed as a sealed Value class compile to a
//! guarded direct `Module:dispatch/3` call instead of a dynamic
//! `beamtalk_message_dispatch:send/3`.
//!
//! Positive tests (sealed receivers take the direct path) fail until the
//! `typed_sealed` handler lands; negative tests (open/untyped/union,
//! raw-representation builtins, exception kin, `Character` literals) lock in
//! today's dynamic behavior so the new handler cannot steal those sends.

use super::codegen;

// ---------------------------------------------------------------------------
// Positive: sealed receivers take the guarded direct path
// ---------------------------------------------------------------------------

#[test]
fn sealed_value_typed_param_sends_direct_with_tag_guard() {
    // NOTE: `generate_module` compiles only the module's first class — the
    // Caller (holding the send under test) comes first, the callee second.
    // The hierarchy and type inference still see both.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    // Direct call to the class module with the literal selector. The
    // module's own `dispatch/3` definition also mentions
    // `bt@sealed_widget':'dispatch'` (its `perform:` arm), but always with a
    // variable selector — the `('x'` literal pins this to the send site.
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call the class module. Got:\n{code}"
    );
    // Tag guard on '$beamtalk_class' with a dynamic-send fallback.
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "guard mismatch should fall back to the dynamic send. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_object_typed_param_sends_direct() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: c :: SealedCounter -> Integer =>\n",
        "    c answer\n\n",
        "sealed Object subclass: SealedCounter\n",
        "  answer -> Integer => 42\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_counter':'dispatch'('answer'"),
        "sealed Object-kind receiver should direct-call too (same tagged-map representation). Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

// ---------------------------------------------------------------------------
// Negative: everything else stays dynamic
// ---------------------------------------------------------------------------

#[test]
fn sealed_value_keyword_send_binds_args_once() {
    // Keyword sends thread arguments into both the direct and fallback
    // branches from a single let-binding (no double evaluation).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Object =>\n",
        "    w withX: 3\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('withX:', ["),
        "sealed keyword send should direct-call with args. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_value_typed_param_sends_direct_class_side() {
    // Class methods send to values too (every vs-elixir row is class-side):
    // instance `dispatch/3` on a value is pure, so no class-process hazard.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  class useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call from class side too. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_value_typed_param_sends_direct_actor_side() {
    // Actor methods send to values too: a direct `dispatch/3` on a value
    // executes in-caller exactly like the dynamic send (values never hop
    // processes), and the emission must be verifier-clean — this test runs
    // with debug assertions on, so a `report_threaded_ir_verify_errors`
    // finding would panic rather than pass.
    let code = codegen(concat!(
        "Actor subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call from actor side too. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn open_value_typed_param_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: OpenWidget -> Integer =>\n",
        "    w x\n\n",
        "Value subclass: OpenWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !code.contains("call 'bt@open_widget':'dispatch'('x'"),
        "open (subclassable) receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "open receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn untyped_param_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w =>\n",
        "    w ping\n",
    ));
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "untyped receiver send should stay dynamic. Got:\n{code}"
    );
}

#[test]
fn union_typed_param_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: Widget | Gadget =>\n",
        "    w ping\n\n",
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Gadget\n",
        "  ping => 1\n",
    ));
    assert!(
        !code.contains("'dispatch'('ping'"),
        "union receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "union receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn integer_typed_param_stays_dynamic() {
    // `Integer` instances are raw Erlang integers, never tagged maps: the
    // tag guard could never match, so emitting it would only pessimize.
    // (`frobnicate` is intentionally undefined — DNU only matters at
    // runtime; codegen shape is what this asserts.)
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: n :: Integer -> Object =>\n",
        "    n frobnicate\n",
    ));
    assert!(
        !code.contains("call 'bt@stdlib@integer':'dispatch'('frobnicate'"),
        "raw-term receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "raw-term receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn dictionary_typed_param_stays_dynamic() {
    // `Dictionary` instances are untagged maps: the tag guard could never
    // match. (`at:`/`at:put:` are dict intrinsics claimed before dispatch
    // selection, so this uses an unclaimed selector.)
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: d :: Dictionary(Symbol, Integer) -> Object =>\n",
        "    d frobnicate\n",
    ));
    assert!(
        !code.contains("call 'bt@stdlib@dictionary':'dispatch'('frobnicate'"),
        "untagged-map receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "untagged-map receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn sealed_exception_subclass_stays_dynamic() {
    // Dynamic sends to exceptions route via `bt@stdlib@exception:dispatch`,
    // not the leaf module — a direct leaf call would diverge.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: e :: SealedBoom -> Object =>\n",
        "    e frobnicate\n\n",
        "sealed typed Exception subclass: SealedBoom\n",
        "  answer -> Integer => 42\n",
    ));
    assert!(
        !code.contains("call 'bt@sealed_boom':'dispatch'('frobnicate'"),
        "exception-kin receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "exception-kin send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn non_tagged_exclusions_are_sealed_builtin_values() {
    // Every `NON_TAGGED_MAP_CLASSES` entry must still be a sealed builtin of
    // Value- or Object-kind: an entry that stopped being sealed is dead (the
    // sealed check excludes it first) and must be removed, not carried.
    use beamtalk_core::ast::ClassKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
    let module = beamtalk_core::ast::Module::new(
        Vec::new(),
        beamtalk_core::source_analysis::Span::new(0, 0),
    );
    let (hierarchy_result, _) = ClassHierarchy::build(&module);
    let hierarchy = hierarchy_result.expect("builtin hierarchy should build");
    for name in crate::core_erlang::dispatch_codegen::NON_TAGGED_MAP_CLASSES {
        let info = hierarchy
            .get_class(name)
            .unwrap_or_else(|| panic!("{name} should still be a builtin class"));
        assert!(
            info.is_sealed,
            "{name} is no longer sealed — remove it from NON_TAGGED_MAP_CLASSES"
        );
        assert!(
            matches!(
                hierarchy.resolve_class_kind(name),
                ClassKind::Value | ClassKind::Object
            ),
            "{name} is no longer Value/Object-kind — re-check its representation"
        );
    }
}

#[test]
fn character_literal_keeps_character_dispatch() {
    // `$A` recurses through `HANDLERS` (including the new trailing entry)
    // before falling back to `generate_character_typed_dispatch`: the new
    // handler must not steal it (`Character` is a raw integer at runtime).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  m -> Object =>\n",
        "    $A uppercase\n",
    ));
    assert!(
        code.contains("call 'bt@stdlib@character':'dispatch'('uppercase'"),
        "Character literal should still reach the Character module. Got:\n{code}"
    );
    assert!(
        !code.contains("beamtalk_message_dispatch':'send'("),
        "Character literal send should not go dynamic. Got:\n{code}"
    );
}

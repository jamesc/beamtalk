// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! chaining a block-taking send onto a `Union`-typed receiver must
//! push declared block-param types down from the union's members, not fall
//! through to `Dynamic(UnannotatedParam)`.
//!
//! `Dictionary(K, V)>>at:ifAbsent:` is typed `Block(T) -> V | T`.
//! Whenever the fallback block's inferred type differs from `V` (e.g. an
//! empty collection literal `#()`, or `List new`), the send's result becomes
//! a genuine `InferredType::Union { V, T }`. Before this fix, chaining a
//! further block-taking message onto that union receiver — e.g. `(dict at:
//! name ifAbsent: [#()]) select: [:t | ...]` — inferred the chained block's
//! parameter as `Dynamic(DynamicReason::UnannotatedParam)`, a reason the
//! BT-1914 "Dynamic in typed class" lint does not filter, forcing a spurious
//! warning and an explicit intermediate type-annotated local as a workaround.
//!
//! The fix (`TypeChecker::resolve_union_block_param_types`) resolves the
//! declared `Block(...)` param type from each union member's method
//! signature and merges them: a `Dynamic` contribution (e.g. from the
//! `List(Dynamic)` produced by an empty-literal `T`) never blocks agreement
//! with a concrete type from another member — only two differing *concrete*
//! types do.

use super::common::*;

/// Finds the first `select:` message send in `class_name`'s named method and
/// returns its block argument's `InferredType` from `type_map`.
///
/// Reads the checker's own `TypeMap` for the block's span (rather than only
/// asserting on diagnostics) so the test distinguishes "block param resolved
/// to the concrete element type" from "block param stayed Dynamic" directly
/// — mirroring `bt2864_union_block_arg_type_inference`'s `block_type_in_method`.
fn select_block_type<'a>(
    module: &Module,
    type_map: &'a TypeMap,
    class_name: &str,
    selector: &str,
) -> &'a InferredType {
    fn find_select_block(expr: &Expression) -> Option<&Block> {
        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                if selector.name() == "select:" {
                    if let Some(Expression::Block(block)) = arguments.first() {
                        return Some(block);
                    }
                }
                find_select_block(receiver).or_else(|| arguments.iter().find_map(find_select_block))
            }
            _ => None,
        }
    }

    let class = module
        .classes
        .iter()
        .find(|c| c.name.name == class_name)
        .unwrap_or_else(|| panic!("class {class_name} not found"));
    let method = class
        .methods
        .iter()
        .find(|m| m.selector.name() == selector)
        .unwrap_or_else(|| panic!("method {selector} not found on {class_name}"));
    let block = method
        .body
        .iter()
        .find_map(|stmt| find_select_block(&stmt.expression))
        .unwrap_or_else(|| panic!("no select: block literal found in {class_name}>>{selector}"));
    type_map
        .get(block.span)
        .unwrap_or_else(|| panic!("no type_map entry for block at {:?}", block.span))
}

/// Asserts `ty` is `Block(param, ...)` whose first type arg is the concrete
/// `expected_param` class — not `Dynamic`.
fn assert_block_param_is(ty: &InferredType, expected_param: &str, context: &str) {
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = ty
    else {
        panic!("{context}: expected a Known Block type, got: {ty:?}");
    };
    assert_eq!(class_name.as_str(), "Block", "{context}: got {ty:?}");
    let param_ty = type_args
        .first()
        .unwrap_or_else(|| panic!("{context}: Block type has no type_args: {ty:?}"));
    assert_eq!(
        param_ty.as_known().map(ecow::EcoString::as_str),
        Some(expected_param),
        "{context}: block param should infer as {expected_param}, got: {param_ty:?}"
    );
}

fn dynamic_in_typed_class_warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect()
}

fn build_hierarchy(module: &Module) -> ClassHierarchy {
    let mut hierarchy = ClassHierarchy::with_builtins();
    let user_hierarchy = ClassHierarchy::build(module).0.unwrap();
    hierarchy.merge(&user_hierarchy);
    hierarchy
}

/// AC: `(dict at: k ifAbsent: [#()]) select: [:t | ...]` infers `t ::
/// RestartEntry` (the dictionary's element type), not `Dynamic` — no false
/// "Dynamic in typed class" diagnostic. This is the exact repro shape from
/// the issue (`beamtalk-watcher`'s `Dictionary(Symbol, List(RestartEntry))`
/// fixture).
#[test]
fn union_receiver_empty_literal_fallback_resolves_chained_block_param() {
    let source = r"
typed Object subclass: RestartEntry
  label => 'entry'

typed Actor subclass: Watcher
  state: restartTimestamps :: Dictionary(Symbol, List(RestartEntry)) = #{}

  recentFor: name :: Symbol -> List(RestartEntry) =>
    (self.restartTimestamps at: name ifAbsent: [#()]) select: [:t | t notNil]
";
    let module = parse_source(source);
    let hierarchy = build_hierarchy(&module);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let ty = select_block_type(&module, checker.type_map(), "Watcher", "recentFor:");
    assert_block_param_is(
        ty,
        "RestartEntry",
        "BT-3463 (empty-literal fallback, chained select:)",
    );

    let warnings = dynamic_in_typed_class_warnings(checker.diagnostics());
    assert!(
        warnings.is_empty(),
        "chained select: block param should not fire the Dynamic-in-typed-class \
         warning; got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// AC: the bug isn't specific to empty-literal fallbacks — `ifAbsent: [List
/// new]` (any fallback expression whose type differs from `V`) reproduces
/// the identical union-receiver shape and must resolve the same way.
#[test]
fn union_receiver_non_empty_literal_fallback_resolves_chained_block_param() {
    let source = r"
typed Object subclass: RestartEntry
  label => 'entry'

typed Actor subclass: Watcher
  state: restartTimestamps :: Dictionary(Symbol, List(RestartEntry)) = #{}

  recentFor: name :: Symbol -> List(RestartEntry) =>
    (self.restartTimestamps at: name ifAbsent: [List new]) select: [:t | t notNil]
";
    let module = parse_source(source);
    let hierarchy = build_hierarchy(&module);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let ty = select_block_type(&module, checker.type_map(), "Watcher", "recentFor:");
    assert_block_param_is(
        ty,
        "RestartEntry",
        "BT-3463 (`List new` fallback, chained select:)",
    );

    let warnings = dynamic_in_typed_class_warnings(checker.diagnostics());
    assert!(
        warnings.is_empty(),
        "chained select: block param should not fire the Dynamic-in-typed-class \
         warning; got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// Regression guard: `dict at: k ifAbsent: [nil]`, used directly
/// (not chained onto a further block-taking send), must still type-check as
/// `V | Nil` with no false diagnostic — the union-receiver push-down must
/// not disturb the already-correct nil-fallback path.
#[test]
fn union_receiver_nil_fallback_unchained_still_typechecks() {
    let source = r"
typed Object subclass: RestartEntry
  label => 'entry'

typed Actor subclass: Watcher
  state: restartTimestamps :: Dictionary(Symbol, List(RestartEntry)) = #{}

  firstFor: name :: Symbol -> List(RestartEntry) | Nil =>
    self.restartTimestamps at: name ifAbsent: [nil]
";
    let module = parse_source(source);
    let hierarchy = build_hierarchy(&module);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings = dynamic_in_typed_class_warnings(checker.diagnostics());
    assert!(
        warnings.is_empty(),
        "unchained nil-fallback should not fire the Dynamic-in-typed-class warning; \
         got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
    assert!(
        checker
            .diagnostics()
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Type)
                || !d.message.contains("declares return type")),
        "firstFor: should type-check against its declared `List(RestartEntry) | Nil` \
         return type, got: {:?}",
        checker.diagnostics()
    );
}

/// Regression guard (review finding on this PR): a message sent to a
/// `Union`-typed receiver with *no block argument at all* (e.g. `includes:`)
/// must not double-infer its non-block arguments. Before this fix,
/// `resolve_union_block_param_types` only signalled a contribution when a
/// block argument resolved, so a receiver/selector pair with zero block
/// arguments always fell through to `infer_args_with_dynamic_block_params`,
/// which re-ran `infer_expr` on every argument already inferred once inside
/// `resolve_union_block_param_types` — double-emitting any diagnostic that
/// argument's inference produces (here, the "Dynamic in typed class" warning
/// on the unannotated `flag` parameter).
#[test]
fn union_receiver_non_block_send_does_not_double_infer_arguments() {
    let source = r"
typed Object subclass: RestartEntry
  label => 'entry'

typed Actor subclass: Watcher
  state: restartTimestamps :: Dictionary(Symbol, List(RestartEntry)) = #{}

  hasEntry: name :: Symbol flag: flag -> Boolean =>
    (self.restartTimestamps at: name ifAbsent: [#()]) includes: flag
";
    let module = parse_source(source);
    let hierarchy = build_hierarchy(&module);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings = dynamic_in_typed_class_warnings(checker.diagnostics());
    assert_eq!(
        warnings.len(),
        1,
        "the unannotated `flag` argument should fire the Dynamic-in-typed-class \
         warning exactly once (not zero, not twice from double-inference); got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// Regression guard (review Suggestion on this PR): the DNU-override check in
/// `resolve_union_block_param_types` must branch on `is_class_side_send`
/// (`has_class_dnu_override` vs `has_instance_dnu_override`) — a union member
/// with only an *instance*-side `doesNotUnderstand:args:` override must still
/// contribute its *class*-side method's block-param type for a class-side
/// send, not be wrongly treated as uncertain.
///
/// Unit-tests `resolve_union_block_param_types` directly (see its doc
/// comment for why: a `Union` receiver combined with a class-side send isn't
/// reachable through today's other type-checker features, since a
/// class-side send's receiver is always a `ClassReference` or `self` —
/// both single, non-`Union` types).
///
/// Design: the two union members' `spawn:` class methods declare
/// *disagreeing* concrete block-param types (`Block(String, String)` vs
/// `Block(Integer, Integer)`). If both are correctly consulted, the merge
/// conservatively falls back to `Dynamic` (two differing concrete types).
/// If `WithInstanceOverride` were wrongly skipped (the pre-fix bug — its
/// *instance*-side override incorrectly suppressed its *class*-side
/// contribution), only `Plain` would contribute and the result would be the
/// single concrete type `Integer` instead — so this test would have failed
/// before the fix.
#[test]
#[allow(clippy::too_many_lines)] // test fixture — length is proportional to ClassInfo fields
fn union_class_side_send_instance_only_dnu_override_still_contributes() {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use std::collections::HashMap;

    let mut hierarchy = ClassHierarchy::with_builtins();
    let with_instance_override = ClassInfo {
        surface_incomplete: false,
        name: eco_string("WithInstanceOverride"),
        superclass: Some(eco_string("Object")),
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
        methods: vec![MethodInfo {
            selector: eco_string("doesNotUnderstand:args:"),
            arity: 2,
            kind: MethodKind::Primary,
            defined_in: eco_string("WithInstanceOverride"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![None, None],
            doc: None,
        }],
        class_methods: vec![MethodInfo {
            selector: eco_string("spawn:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("WithInstanceOverride"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(DeclaredType::parse("String")),
            param_types: vec![Some(DeclaredType::parse("Block(String, String)"))],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    let plain = ClassInfo {
        surface_incomplete: false,
        name: eco_string("Plain"),
        superclass: Some(eco_string("Object")),
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
        class_methods: vec![MethodInfo {
            selector: eco_string("spawn:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("Plain"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(DeclaredType::parse("Integer")),
            param_types: vec![Some(DeclaredType::parse("Block(Integer, Integer)"))],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![with_instance_override, plain]);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let members = vec![
        InferredType::known("WithInstanceOverride"),
        InferredType::known("Plain"),
    ];
    let block_arg = Expression::Block(Block::new(
        vec![crate::ast::BlockParameter::new("x", span())],
        vec![bare(var("x"))],
        span(),
    ));
    let arguments = vec![block_arg];

    let arg_types = checker.resolve_union_block_param_types(
        &members, &arguments, "spawn:", &hierarchy, &mut env, false,
        true, // is_class_side_send
    );

    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = &arg_types[0]
    else {
        panic!("expected a Known Block type, got: {:?}", arg_types[0]);
    };
    assert_eq!(class_name.as_str(), "Block", "got: {:?}", arg_types[0]);
    let param_ty = type_args
        .first()
        .unwrap_or_else(|| panic!("Block type has no type_args: {:?}", arg_types[0]));
    assert!(
        matches!(param_ty, InferredType::Dynamic(_)),
        "both union members' class-side `spawn:` methods should be consulted for a \
         class-side send — `WithInstanceOverride`'s instance-only DNU override must \
         not suppress its contribution — so the disagreeing param types (String vs \
         Integer) should conservatively merge to Dynamic; got: {param_ty:?} (if this \
         is `Integer`, `WithInstanceOverride` was wrongly skipped)"
    );
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Validation checks for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module contains class-aware validation checks that run after the
//! main analysis pipeline. These checks use the `ClassHierarchy` to validate:
//! - Abstract class instantiation (BT-105)
//! - Actor `new` vs `spawn` usage (BT-563)
//! - Field name validation in `new:` maps (BT-563)
//! - Class variable access (BT-563)
//! - Empty method bodies (BT-631)
//! - Value type `-> Nil` return annotations (BT-1052)

use crate::ast::{
    Block, Expression, Identifier, MessageSelector, MethodDefinition, Module, Pattern,
};
use crate::ast_walker::{walk_expression, walk_module};
use crate::semantic_analysis::block_context::{classify_block, is_collection_hof_selector};
use crate::semantic_analysis::{BlockContext, ClassHierarchy};
use std::collections::HashSet;
#[cfg(test)]
use crate::source_analysis::lex_with_eof;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

/// BT-105: Check for attempts to instantiate abstract classes.
///
/// Walks all expressions looking for `MessageSend` where the receiver is an
/// identifier matching an abstract class and the selector is an instantiation
/// method (`spawn`, `spawnWith:`, `new`, or `new:`).
pub(crate) fn check_abstract_instantiation(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_abstract_instantiation);
}

/// Returns true if the selector name is an instantiation method (spawn, new, etc.)
fn is_instantiation_selector(name: &str) -> bool {
    matches!(name, "spawn" | "spawnWith:" | "new" | "new:")
}

/// Visitor for abstract class instantiation checks (BT-105).
///
/// Called by `walk_expression` on each expression node; the walker handles
/// recursive traversal, so this function only inspects the current node.
fn visit_abstract_instantiation(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            span,
            ..
        } => {
            let receiver_name = match receiver.as_ref() {
                Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
                Expression::ClassReference { name, .. } => Some(name.name.as_str()),
                _ => None,
            };

            if let Some(name) = receiver_name {
                let selector_name = selector.name();

                if is_instantiation_selector(&selector_name) && hierarchy.is_abstract(name) {
                    diagnostics.push(Diagnostic::error(
                        format!("Cannot instantiate abstract class `{name}`. Subclass it first.",),
                        *span,
                    ));
                }
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            let receiver_name = match receiver.as_ref() {
                Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
                Expression::ClassReference { name, .. } => Some(name.name.as_str()),
                _ => None,
            };
            if let Some(name) = receiver_name {
                for msg in messages {
                    let sel = msg.selector.name();
                    if is_instantiation_selector(&sel) && hierarchy.is_abstract(name) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "Cannot instantiate abstract class `{name}`. Subclass it first.",
                            ),
                            msg.span,
                        ));
                    }
                }
            }
        }
        _ => {}
    }
}

// ── BT-563: Class-aware diagnostics ──────────────────────────────────────────

/// Extracts a class name from a receiver expression (`Identifier` or `ClassReference`).
fn receiver_class_name(receiver: &Expression) -> Option<&str> {
    match receiver {
        Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
        Expression::ClassReference { name, .. } => Some(name.name.as_str()),
        _ => None,
    }
}

/// Walks all expressions in a module, calling `visitor` on each node (pre-order).
///
/// This is a thin adapter over [`walk_module`] that threads the `hierarchy`
/// and `diagnostics` through a closure, allowing callers to pass a simple
/// `fn(&Expression, &ClassHierarchy, &mut Vec<Diagnostic>)` visitor.
fn walk_module_with_hierarchy(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
    visitor: fn(&Expression, &ClassHierarchy, &mut Vec<Diagnostic>),
) {
    walk_module(module, &mut |expr| visitor(expr, hierarchy, diagnostics));
}

/// BT-563: Warn when Actor subclasses use `new` or `new:` instead of `spawn`.
pub(crate) fn check_actor_new_usage(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_actor_new);
}

fn visit_actor_new(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        selector,
        span,
        ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            let sel = selector.name();
            if (sel == "new" || sel == "new:")
                && class_name != "Actor"
                && hierarchy.is_actor_subclass(class_name)
            {
                diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Actor subclass `{class_name}` should use `spawn` instead of `{sel}`"
                        ),
                        *span,
                    )
                    .with_hint("Use spawn instead of new for Actor subclasses"),
                );
            }
        }
    }
    // Also check cascade messages
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            for msg in messages {
                let sel = msg.selector.name();
                if (sel == "new" || sel == "new:")
                    && class_name != "Actor"
                    && hierarchy.is_actor_subclass(class_name)
                {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Actor subclass `{class_name}` should use `spawn` instead of `{sel}`"
                            ),
                            msg.span,
                        )
                        .with_hint("Use spawn instead of new for Actor subclasses"),
                    );
                }
            }
        }
    }
}

/// BT-563: Validate field names in `ClassName new: #{field => value}`.
pub(crate) fn check_new_field_names(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_new_field_names);
}

fn visit_new_field_names(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            let sel = selector.name();
            if sel == "new:" || sel == "spawn:" {
                if let Some(Expression::MapLiteral { pairs, .. }) = arguments.first() {
                    let declared_state = hierarchy.all_state(class_name);
                    if !declared_state.is_empty() {
                        validate_map_field_names(pairs, class_name, &declared_state, diagnostics);
                    }
                }
            }
        }
    }
    // Also check cascade messages
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            for msg in messages {
                let sel = msg.selector.name();
                if sel == "new:" || sel == "spawn:" {
                    if let Some(Expression::MapLiteral { pairs, .. }) = msg.arguments.first() {
                        let declared_state = hierarchy.all_state(class_name);
                        if !declared_state.is_empty() {
                            validate_map_field_names(
                                pairs,
                                class_name,
                                &declared_state,
                                diagnostics,
                            );
                        }
                    }
                }
            }
        }
    }
}

/// Checks that symbol keys in a map literal match declared state fields.
fn validate_map_field_names(
    pairs: &[crate::ast::MapPair],
    class_name: &str,
    declared_state: &[EcoString],
    diagnostics: &mut Vec<Diagnostic>,
) {
    for pair in pairs {
        if let Expression::Literal(crate::ast::Literal::Symbol(sym), sym_span) = &pair.key {
            if !declared_state.iter().any(|s| s.as_str() == sym.as_str()) {
                let fields: Vec<&str> = declared_state.iter().map(EcoString::as_str).collect();
                diagnostics.push(
                    Diagnostic::warning(
                        format!("Unknown field `{sym}` for class `{class_name}`"),
                        *sym_span,
                    )
                    .with_hint(format!("Declared fields: {}", fields.join(", "))),
                );
            }
        }
    }
}

/// BT-563: Warn on access to undeclared class variables.
pub(crate) fn check_class_variable_access(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_classvar_access);
}

fn visit_classvar_access(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        span,
        ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            let sel = selector.name();
            if sel == "classState:" {
                if let Some(Expression::Literal(crate::ast::Literal::Symbol(var_name), _)) =
                    arguments.first()
                {
                    let class_vars = hierarchy.class_variable_names(class_name);
                    if hierarchy.has_class(class_name)
                        && !class_vars.iter().any(|cv| cv.as_str() == var_name.as_str())
                    {
                        let hint = if class_vars.is_empty() {
                            format!("`{class_name}` has no declared class variables")
                        } else {
                            let vars: Vec<&str> =
                                class_vars.iter().map(EcoString::as_str).collect();
                            format!("Declared class variables: {}", vars.join(", "))
                        };
                        diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "Undefined class variable `{var_name}` on class `{class_name}`"
                                ),
                                *span,
                            )
                            .with_hint(hint),
                        );
                    }
                }
            }
        }
    }
    // Also check cascade messages
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(class_name) = receiver_class_name(receiver) {
            for msg in messages {
                let sel = msg.selector.name();
                if sel == "classState:" {
                    if let Some(Expression::Literal(crate::ast::Literal::Symbol(var_name), _)) =
                        msg.arguments.first()
                    {
                        let class_vars = hierarchy.class_variable_names(class_name);
                        if hierarchy.has_class(class_name)
                            && !class_vars.iter().any(|cv| cv.as_str() == var_name.as_str())
                        {
                            let hint = if class_vars.is_empty() {
                                format!("`{class_name}` has no declared class variables")
                            } else {
                                let vars: Vec<&str> =
                                    class_vars.iter().map(EcoString::as_str).collect();
                                format!("Declared class variables: {}", vars.join(", "))
                            };
                            diagnostics.push(
                                Diagnostic::warning(
                                    format!(
                                        "Undefined class variable `{var_name}` on class `{class_name}`"
                                    ),
                                    msg.span,
                                )
                                .with_hint(hint),
                            );
                        }
                    }
                }
            }
        }
    }
}

/// BT-738: Warn when a user-defined class name matches a stdlib built-in class name.
///
/// Stdlib class names are protected at runtime (via `update_class` in the runtime).
/// This compile-time warning surfaces the conflict earlier, before the BEAM module
/// even loads. Uses `ClassHierarchy::is_runtime_protected_class` which covers
/// only generated stdlib classes (from `lib/*.bt`) that have the `bt@stdlib@`
/// module prefix and are actually protected at runtime.
///
/// Since BT-813, `Future` and `FileHandle` are also stdlib classes with
/// `bt@stdlib@` prefixed modules, so they trigger shadowing warnings too.
///
/// This must NOT be called during stdlib compilation (`stdlib_mode = true`).
/// Call it alongside `validate_primitives`, guarded by `!options.stdlib_mode`.
pub fn check_stdlib_name_shadowing(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        let name = class.name.name.as_str();
        if ClassHierarchy::is_runtime_protected_class(name) {
            diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Class name `{name}` conflicts with a stdlib class. \
                         Loading will fail because stdlib class names are protected."
                    ),
                    class.name.span,
                )
                .with_hint(format!(
                    "Choose a different name. `{name}` is a protected stdlib class name."
                )),
            );
        }
        // BT-1041: `Self` is reserved as a return type keyword
        if name == "Self" {
            diagnostics.push(
                Diagnostic::error(
                    "`Self` is reserved as a return type keyword and cannot be used as a class name"
                        .to_string(),
                    class.name.span,
                )
                .with_category(DiagnosticCategory::Type),
            );
        }
    }
}

// ── BT-914: Value type slot assignment validation ──────────────────────────────

/// BT-914: Reject `self.slot :=` in Value type methods; warn when it bypasses
/// an overridden `withSlot:` method in Actor methods.
///
/// Value types (ADR 0042) are immutable — direct slot assignment is a compile
/// error. Actors allow direct slot assignment but warn when a custom `with*:`
/// method exists in the hierarchy and would be bypassed.
pub(crate) fn check_value_slot_assignment(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        let is_value = hierarchy.is_value_subclass(class_name);

        // Check instance methods only — classState: slots on class methods are
        // separate and not subject to value-type immutability rules.
        for method in &class.methods {
            let method_selector = method.selector.name();
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |e| {
                    check_slot_assignment_at(
                        e,
                        class_name,
                        &method_selector,
                        is_value,
                        hierarchy,
                        diagnostics,
                    );
                });
            }
        }
    }

    // Also check standalone method definitions (Tonel-style: `Counter >> setX: v => ...`).
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue; // Class-side methods are not subject to value-type rules.
        }
        let class_name = standalone.class_name.name.as_str();
        let is_value = hierarchy.is_value_subclass(class_name);
        let method_selector = standalone.method.selector.name();
        for stmt in &standalone.method.body {
            walk_expression(&stmt.expression, &mut |e| {
                check_slot_assignment_at(
                    e,
                    class_name,
                    &method_selector,
                    is_value,
                    hierarchy,
                    diagnostics,
                );
            });
        }
    }
}

/// Capitalise the first character of a string (used to build `with<Slot>:` selectors).
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().to_string() + chars.as_str(),
    }
}

/// Checks a single `self.slot := value` node and reports diagnostics.
///
/// Reports an error for value-type classes and a warning for actor classes
/// that have an overriding `withSlot:` method in the hierarchy.
fn report_self_slot_assignment(
    slot_name: &str,
    span: crate::source_analysis::Span,
    class_name: &str,
    method_selector: &str,
    is_value: bool,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let with_selector = format!("with{}:", capitalize_first(slot_name));
    if is_value {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Cannot assign to slot `{slot_name}` on value type \
                     — use `self {with_selector} newValue` to create a new instance"
                ),
                span,
            )
            .with_hint(format!(
                "Value types are immutable. \
                 Use `self {with_selector}` to return a new instance with the updated slot."
            )),
        );
    } else if method_selector != with_selector
        && hierarchy.find_method(class_name, &with_selector).is_some()
    {
        // Actor: warn only when we are NOT inside the withSlot: method
        // itself, and the method exists in the hierarchy (user-defined).
        diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Direct slot assignment `self.{slot_name} :=` bypasses \
                     the `{with_selector}` method defined in the class hierarchy"
                ),
                span,
            )
            .with_hint(format!(
                "Consider using `self {with_selector} newValue` \
                 to go through the `{with_selector}` method."
            )),
        );
    }
}

/// Collects child expression references from an expression node.
fn child_expressions(expr: &Expression) -> Vec<&Expression> {
    match expr {
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            let mut children = vec![receiver.as_ref()];
            children.extend(arguments.iter());
            children
        }
        Expression::Block(block) => block.body.iter().map(|s| &s.expression).collect(),
        Expression::Assignment { target, value, .. } => vec![target.as_ref(), value.as_ref()],
        Expression::Return { value, .. } => vec![value.as_ref()],
        Expression::Cascade {
            receiver, messages, ..
        } => {
            let mut children = vec![receiver.as_ref()];
            for msg in messages {
                children.extend(msg.arguments.iter());
            }
            children
        }
        Expression::Parenthesized { expression, .. } => vec![expression.as_ref()],
        Expression::FieldAccess { receiver, .. } => vec![receiver.as_ref()],
        Expression::Match { value, arms, .. } => {
            let mut children = vec![value.as_ref()];
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    children.push(guard);
                }
                children.push(&arm.body);
            }
            children
        }
        Expression::MapLiteral { pairs, .. } => {
            pairs.iter().flat_map(|p| [&p.key, &p.value]).collect()
        }
        Expression::ListLiteral { elements, tail, .. } => {
            let mut children: Vec<&Expression> = elements.iter().collect();
            if let Some(t) = tail {
                children.push(t.as_ref());
            }
            children
        }
        Expression::ArrayLiteral { elements, .. } => elements.iter().collect(),
        Expression::StringInterpolation { segments, .. } => segments
            .iter()
            .filter_map(|seg| {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    Some(e)
                } else {
                    None
                }
            })
            .collect(),
        _ => vec![],
    }
}

/// Checks a single expression node for `self.slot :=` patterns.
///
/// Called via [`walk_expression`] from [`check_value_slot_assignment`]; the
/// walker handles recursive traversal, so this function only inspects the
/// current node.
fn check_slot_assignment_at(
    expr: &Expression,
    class_name: &str,
    method_selector: &str,
    is_value: bool,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::Assignment { target, span, .. } = expr {
        if let Expression::FieldAccess {
            receiver, field, ..
        } = target.as_ref()
        {
            if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self") {
                report_self_slot_assignment(
                    field.name.as_str(),
                    *span,
                    class_name,
                    method_selector,
                    is_value,
                    hierarchy,
                    diagnostics,
                );
            }
        }
    }
}

// ── BT-950: Redundant assignment ─────────────────────────────────────────────

/// BT-950: Warn when the RHS of an assignment is the same identifier as the LHS.
///
/// Detects `x := x` where both sides are the same plain identifier binding.
/// This has no effect at runtime and usually indicates a copy-paste error or
/// leftover code.
pub(crate) fn check_redundant_assignment(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        if let Expression::Assignment {
            target,
            value,
            span,
            ..
        } = expr
        {
            if let (
                Expression::Identifier(Identifier { name: lhs, .. }),
                Expression::Identifier(Identifier { name: rhs, .. }),
            ) = (target.as_ref(), value.as_ref())
            {
                if lhs == rhs {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!("Redundant assignment: `{lhs} := {lhs}` has no effect"),
                            *span,
                        )
                        .with_hint("Remove this assignment or assign a different value."),
                    );
                }
            }
        }
    });
}

// ── BT-953: Self capture in collection HOF blocks ─────────────────────────────

/// BT-953: Warn when `self` is referenced inside a literal block passed to a
/// collection higher-order method (collect:, do:, select:, reject:, inject:into:,
/// detect:, detect:ifNone:) inside an **Actor** class method.
///
/// Only `Actor subclass:` uses the `calling_self` / `gen_server` dispatch mechanism.
/// `Object subclass:` and `Value subclass:` use direct synchronous Erlang function
/// calls, so self-sends inside collection blocks are safe for those class kinds.
///
/// Example: `items collect: [:x | self process: x]`  ← deadlock risk (Actor only)
pub(crate) fn check_self_capture_in_actor_block(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Precompute the set of selectors whose block arguments run in a separate
    // BEAM process, so lookups are O(1) per expression node (BT-1312).
    let spawn_selectors = hierarchy.spawns_block_selectors();

    for class in &module.classes {
        if !hierarchy.is_actor_subclass(class.name.name.as_str()) {
            continue;
        }
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |e| {
                    check_self_capture_at(e, &spawn_selectors, diagnostics);
                });
            }
        }
    }

    // Also cover Tonel-style standalone method definitions for Actor classes.
    for standalone in &module.method_definitions {
        if !hierarchy.is_actor_subclass(standalone.class_name.name.as_str()) {
            continue;
        }
        for stmt in &standalone.method.body {
            walk_expression(&stmt.expression, &mut |e| {
                check_self_capture_at(e, &spawn_selectors, diagnostics);
            });
        }
    }
}

/// Returns `true` if `expr` is a bare `self` identifier, unwrapping any
/// surrounding parentheses.
fn is_self_receiver(expr: &Expression) -> bool {
    match expr {
        Expression::Identifier(Identifier { name, .. }) => name == "self",
        Expression::Parenthesized { expression, .. } => is_self_receiver(expression),
        _ => false,
    }
}

/// Searches an expression tree for a `MessageSend` or `Cascade` whose
/// immediate receiver is `self`.
///
/// Only these forms go through the `calling_self` / `gen_server` dispatch and
/// can deadlock when called from inside a collection HOF block.
/// `FieldAccess { receiver: self }` (`self.field`) compiles to `maps:get` /
/// `maps:put` — a direct in-process map operation — and is safe.
///
/// Block arguments of methods whose selectors are in `spawn_selectors` are
/// skipped: they run in a separate BEAM process and self-sends there are
/// always safe (BT-1312).
fn find_self_message_send(
    expr: &Expression,
    spawn_selectors: &HashSet<EcoString>,
) -> Option<Span> {
    match expr {
        Expression::MessageSend { receiver, span, .. }
        | Expression::Cascade { receiver, span, .. } => {
            if is_self_receiver(receiver) {
                return Some(*span);
            }
        }
        _ => {}
    }
    // Methods with `spawns_block: true` run their block in a separate process —
    // skip the block argument so we don't flag safe self-sends (BT-1301/BT-1312).
    // The receiver is NOT skipped: `(self getTimer) after:do:` still warns
    // because `self getTimer` is a real calling_self dispatch in the HOF.
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = expr
    {
        if spawn_selectors.contains(selector.name().as_str()) {
            return std::iter::once(receiver.as_ref())
                .chain(
                    arguments
                        .iter()
                        .filter(|a| !matches!(a, Expression::Block(_))),
                )
                .find_map(|e| find_self_message_send(e, spawn_selectors));
        }
    }
    child_expressions(expr)
        .into_iter()
        .find_map(|e| find_self_message_send(e, spawn_selectors))
}

/// Searches a block's body for a message send or cascade directly to `self`.
fn find_self_reference_in_block(
    block: &Block,
    spawn_selectors: &HashSet<EcoString>,
) -> Option<Span> {
    block
        .body
        .iter()
        .find_map(|s| find_self_message_send(&s.expression, spawn_selectors))
}

/// Checks a single expression node for the self-capture pattern.
///
/// Called by [`check_self_capture_in_actor_block`]; the
/// walker handles recursive traversal, so this function only inspects the
/// current node.
fn check_self_capture_at(
    expr: &Expression,
    spawn_selectors: &HashSet<EcoString>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        selector,
        arguments,
        span,
        ..
    } = expr
    {
        let selector_str = selector.name();
        for (i, arg) in arguments.iter().enumerate() {
            if !is_collection_hof_selector(&selector_str, i) {
                continue;
            }
            // Use classify_block to confirm this is a literal block in a control-flow
            // position (not a block variable). This is the production wiring of the
            // block_context infrastructure (BT-953).
            let ctx = classify_block(arg.span(), expr, false);
            if !matches!(ctx, BlockContext::ControlFlow) {
                continue;
            }
            if let Expression::Block(block) = arg {
                if find_self_reference_in_block(block, spawn_selectors).is_some() {
                    diagnostics.push(
                        Diagnostic::hint(
                            format!(
                                "`self` capture in block passed to `{selector_str}` may deadlock"
                            ),
                            *span,
                        )
                        .with_hint(
                            "Sending `self` from within a collection block re-enters the \
                             `calling_self` dispatch and can deadlock. \
                             Inline the logic or bind the result to a local variable before \
                             entering the block.",
                        )
                        .with_category(DiagnosticCategory::SelfCapture),
                    );
                }
            }
        }
    }
}

// ── BT-955: Literal boolean condition ────────────────────────────────────────

/// BT-955: Warn when a boolean conditional message is sent to a literal boolean receiver.
///
/// When `ifTrue:`, `ifFalse:`, or `ifTrue:ifFalse:` is sent to a literal `true`
/// or `false`, one branch is statically unreachable or the conditional is
/// entirely redundant.
///
/// Examples:
/// - `true ifTrue: [42]`           → condition always true (branch always taken)
/// - `false ifFalse: [42]`         → condition always false (branch always taken)
/// - `true ifFalse: [42]`          → condition always true (`ifFalse:` unreachable)
/// - `false ifTrue: [42]`          → condition always false (`ifTrue:` unreachable)
/// - `true ifTrue: [1] ifFalse: [2]`  → `ifFalse:` branch is dead code
/// - `false ifTrue: [1] ifFalse: [2]` → `ifTrue:` branch is dead code
pub(crate) fn check_literal_boolean_condition(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        check_literal_boolean_condition_at(expr, diagnostics);
    });
}

/// Returns `true` if the selector is a boolean conditional message.
fn is_boolean_conditional_selector(sel: &str) -> bool {
    matches!(sel, "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:")
}

/// Returns a hint string describing the unreachable or redundant branch.
fn dead_branch_hint(is_true: bool, selector: &str) -> &'static str {
    match (is_true, selector) {
        (true, "ifTrue:") | (false, "ifFalse:") => {
            "The branch is always taken. Remove the conditional and use the branch body directly."
        }
        (true, "ifFalse:") | (false, "ifTrue:") => "This branch is never executed. Remove it.",
        (true, "ifTrue:ifFalse:") => {
            "The `ifFalse:` branch is never executed. Simplify to the `ifTrue:` block."
        }
        (false, "ifTrue:ifFalse:") => {
            "The `ifTrue:` branch is never executed. Simplify to the `ifFalse:` block."
        }
        _ => "Remove the unreachable branch.",
    }
}

/// Checks a single expression node for literal boolean conditional patterns.
///
/// Called by [`walk_module`] via [`check_literal_boolean_condition`]; the walker
/// handles recursive traversal, so this function only inspects the current node.
fn check_literal_boolean_condition_at(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    if let Expression::MessageSend {
        receiver,
        selector,
        span,
        ..
    } = expr
    {
        let literal_val = match receiver.as_ref() {
            Expression::Identifier(Identifier { name, .. }) if name == "true" => Some(true),
            Expression::Identifier(Identifier { name, .. }) if name == "false" => Some(false),
            _ => None,
        };
        if let Some(is_true) = literal_val {
            let selector_str = selector.name();
            if is_boolean_conditional_selector(&selector_str) {
                let literal_name = if is_true { "true" } else { "false" };
                diagnostics.push(
                    Diagnostic::warning(format!("Condition is always `{literal_name}`"), *span)
                        .with_hint(dead_branch_hint(is_true, &selector_str)),
                );
            }
        }
    }
    // Also check cascade messages — `true ifTrue: [1]; ifFalse: [2]` has a literal
    // boolean receiver with each cascade message going to the same receiver.
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        let literal_val = match receiver.as_ref() {
            Expression::Identifier(Identifier { name, .. }) if name == "true" => Some(true),
            Expression::Identifier(Identifier { name, .. }) if name == "false" => Some(false),
            _ => None,
        };
        if let Some(is_true) = literal_val {
            let literal_name = if is_true { "true" } else { "false" };
            for msg in messages {
                let selector_str = msg.selector.name();
                if is_boolean_conditional_selector(&selector_str) {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!("Condition is always `{literal_name}`"),
                            msg.span,
                        )
                        .with_hint(dead_branch_hint(is_true, &selector_str)),
                    );
                }
            }
        }
    }
}

/// BT-859: Error on empty method bodies.
///
/// Methods declared with `=>` but no body expressions are a compile error.
/// Use `self notImplemented` for work-in-progress stubs, or
/// `self subclassResponsibility` for abstract interface contracts.
pub(crate) fn check_empty_method_bodies(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if method.body.is_empty() {
                let selector = method.selector.name();
                diagnostics.push(
                    Diagnostic::error(format!("Method `{selector}` has an empty body"), method.span)
                        .with_category(DiagnosticCategory::EmptyBody)
                        .with_hint("Use `self notImplemented` for stubs, or `self subclassResponsibility` for abstract methods"),
                );
            }
        }
    }
    for standalone in &module.method_definitions {
        if standalone.method.body.is_empty() {
            let selector = standalone.method.selector.name();
            diagnostics.push(
                Diagnostic::error(
                    format!("Method `{selector}` has an empty body"),
                    standalone.method.span,
                )
                .with_category(DiagnosticCategory::EmptyBody)
                .with_hint("Use `self notImplemented` for stubs, or `self subclassResponsibility` for abstract methods"),
            );
        }
    }
}

// ── BT-951: Effect-free statement detection ───────────────────────────────────

/// Returns `true` if the expression is pure (no observable side effects).
///
/// Only a conservative subset is classified as pure: literals, variable reads,
/// class references, parenthesized pure expressions, and binary sends with a
/// known arithmetic or comparison operator applied to pure operands.
fn is_effect_free(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_, _)
        | Expression::Identifier(_)
        | Expression::ClassReference { .. } => true,
        Expression::MapLiteral { pairs, .. } => pairs
            .iter()
            .all(|p| is_effect_free(&p.key) && is_effect_free(&p.value)),
        Expression::ArrayLiteral { elements, .. } => elements.iter().all(is_effect_free),
        Expression::ListLiteral { elements, tail, .. } => {
            elements.iter().all(is_effect_free) && tail.as_ref().is_none_or(|t| is_effect_free(t))
        }
        Expression::Parenthesized { expression, .. } => is_effect_free(expression),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            ..
        } => {
            if *is_cast {
                return false;
            }
            match selector {
                crate::ast::MessageSelector::Binary(op) => {
                    is_pure_binary_op(op)
                        && is_effect_free(receiver)
                        && arguments.iter().all(is_effect_free)
                }
                _ => false,
            }
        }
        _ => false,
    }
}

/// Returns `true` if a binary operator is a known pure arithmetic/comparison op.
fn is_pure_binary_op(op: &str) -> bool {
    matches!(
        op,
        "+" | "-"
            | "*"
            | "/"
            | "//"
            | "\\\\"
            | "**"
            | "<"
            | ">"
            | "<="
            | ">="
            | "="
            | "~~"
            | "&"
            | "|"
            | "^"
            | ">>"
            | "<<"
    )
}

/// Returns a short description of the expression kind for diagnostic messages.
fn effect_free_label(expr: &Expression) -> &'static str {
    match expr {
        Expression::Literal(_, _) => "literal",
        Expression::Identifier(_) => "variable reference",
        Expression::ClassReference { .. } => "class reference",
        Expression::MapLiteral { .. } => "map literal",
        Expression::ArrayLiteral { .. } => "array literal",
        Expression::ListLiteral { .. } => "list literal",
        _ => "pure expression",
    }
}

/// Walk an expression to find nested sequences (blocks) that may contain
/// effect-free non-last statements.
fn walk_expr_for_effect_free(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Block(block) => {
            check_seq_for_effect_free(&block.body, diagnostics);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            walk_expr_for_effect_free(receiver, diagnostics);
            for arg in arguments {
                walk_expr_for_effect_free(arg, diagnostics);
            }
        }
        Expression::Assignment { target, value, .. } => {
            walk_expr_for_effect_free(target, diagnostics);
            walk_expr_for_effect_free(value, diagnostics);
        }
        Expression::Return { value, .. } => {
            walk_expr_for_effect_free(value, diagnostics);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            walk_expr_for_effect_free(receiver, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    walk_expr_for_effect_free(arg, diagnostics);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            walk_expr_for_effect_free(expression, diagnostics);
        }
        Expression::FieldAccess { receiver, .. } => {
            walk_expr_for_effect_free(receiver, diagnostics);
        }
        Expression::Match { value, arms, .. } => {
            walk_expr_for_effect_free(value, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expr_for_effect_free(guard, diagnostics);
                }
                walk_expr_for_effect_free(&arm.body, diagnostics);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_expr_for_effect_free(&pair.key, diagnostics);
                walk_expr_for_effect_free(&pair.value, diagnostics);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_expr_for_effect_free(elem, diagnostics);
            }
            if let Some(t) = tail {
                walk_expr_for_effect_free(t, diagnostics);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                walk_expr_for_effect_free(elem, diagnostics);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    walk_expr_for_effect_free(e, diagnostics);
                }
            }
        }
        _ => {}
    }
}

/// Check a sequence of expressions: warn on any non-last expression that is
/// effect-free, then recurse into all expressions for nested sequences.
fn check_seq_for_effect_free(
    exprs: &[crate::ast::ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let len = exprs.len();
    for (i, stmt) in exprs.iter().enumerate() {
        let expr = &stmt.expression;
        let is_last = i == len - 1;
        if !is_last && is_effect_free(expr) {
            let label = effect_free_label(expr);
            diagnostics.push(
                Diagnostic::lint(format!("this {label} has no effect"), expr.span()).with_hint(
                    "Remove this expression, or assign its value to a variable if needed.",
                ),
            );
        }
        walk_expr_for_effect_free(expr, diagnostics);
    }
}

/// BT-951: Warn (as a lint) when a statement is an effect-free expression
/// whose value is silently discarded.
///
/// Checks method bodies, standalone method bodies, and (by default)
/// module-level expressions (`module.expressions`). Pass
/// `skip_module_expression_lint = true` to suppress the module-level check —
/// bootstrap-test compilation uses this because those files intentionally use
/// top-level expressions as test assertions paired with `// =>` comments.
///
/// Effect-free expressions include literals, variable references, and pure
/// binary arithmetic / comparison expressions composed from pure sub-expressions.
///
/// Uses `Severity::Lint` so the warning is suppressed during normal compilation
/// and only surfaces when running `beamtalk lint` or in the REPL.
pub(crate) fn check_effect_free_statements(
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
    skip_module_expression_lint: bool,
) {
    // BT-979: Check module-level expressions unless the caller opts out.
    if !skip_module_expression_lint {
        check_seq_for_effect_free(&module.expressions, diagnostics);
    }
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            check_seq_for_effect_free(&method.body, diagnostics);
        }
    }
    for standalone in &module.method_definitions {
        check_seq_for_effect_free(&standalone.method.body, diagnostics);
    }
}

/// BT-919: Error when `!` (cast) is used on a statically-known value type.
///
/// Value types are not actors — they do not have a mailbox and cannot receive
/// asynchronous messages. Using `!` on a value type receiver is always wrong.
pub(crate) fn check_cast_on_value_type(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_with_hierarchy(module, hierarchy, diagnostics, visit_cast_on_value_type);
}

fn visit_cast_on_value_type(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        receiver,
        is_cast,
        span,
        ..
    } = expr
    {
        if !is_cast {
            return;
        }
        if let Some(class_name) = receiver_class_name(receiver) {
            if hierarchy.has_class(class_name) && hierarchy.is_value_subclass(class_name) {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "Cannot use ! (cast) on value type `{class_name}`. Value types are not actors."
                    ),
                    *span,
                ));
            }
        }
    }
}

// ── BT-1052: Value type `-> Nil` return annotation ────────────────────────────

/// BT-1052: Error when an instance method on a Value type has an explicit `-> Nil`
/// return type annotation.
///
/// Value types (ADR 0042) are immutable transformations — methods should return
/// new values, not perform side effects. A `-> Nil` return type signals a
/// side-effecting void method, which contradicts value-type semantics.
///
/// Class-side methods and `subclassResponsibility` placeholders are exempt.
pub(crate) fn check_value_nil_return(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        if !hierarchy.is_value_subclass(class_name) {
            continue;
        }
        // Only check instance methods — class_methods are exempt.
        for method in &class.methods {
            check_method_nil_return(method, class_name, diagnostics);
        }
    }
    // Also check standalone method definitions (Tonel-style).
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue;
        }
        let class_name = standalone.class_name.name.as_str();
        if !hierarchy.is_value_subclass(class_name) {
            continue;
        }
        check_method_nil_return(&standalone.method, class_name, diagnostics);
    }
}

/// Returns `true` if the method body contains a `self subclassResponsibility`
/// call anywhere (including inside `^ self subclassResponsibility`, cascades,
/// or other wrapped forms), marking it as an abstract placeholder that should
/// be exempt from the value `-> Nil` lint.
fn is_subclass_responsibility(method: &MethodDefinition) -> bool {
    let mut found = false;
    for stmt in &method.body {
        walk_expression(&stmt.expression, &mut |e| {
            if found {
                return;
            }
            if matches!(
                e,
                Expression::MessageSend {
                    receiver,
                    selector: MessageSelector::Unary(sel),
                    ..
                } if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
                    && sel.as_str() == "subclassResponsibility"
            ) {
                found = true;
            }
        });
    }
    found
}

/// Check a single method for `-> Nil` on a value type and push an error diagnostic.
fn check_method_nil_return(
    method: &MethodDefinition,
    class_name: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(return_type) = &method.return_type else {
        return;
    };
    let crate::ast::TypeAnnotation::Simple(id) = return_type else {
        return;
    };
    if id.name.as_str() != "Nil" {
        return;
    }
    // Exempt abstract placeholder methods.
    if is_subclass_responsibility(method) {
        return;
    }
    // Exempt iteration/callback methods that accept a Block parameter.
    // These methods delegate side-effects to the block rather than performing
    // them directly on the Value — consistent with Value-type semantics.
    let has_block_param = method.parameters.iter().any(|p| {
        matches!(&p.type_annotation, Some(crate::ast::TypeAnnotation::Simple(id)) if id.name.as_str() == "Block")
    });
    if has_block_param {
        return;
    }
    let selector = method.selector.name();
    diagnostics.push(
        Diagnostic::error(
            format!(
                "method `{selector}` returns Nil on a Value type \
                 — Value types should not have side-effecting void methods; \
                 consider `Object subclass:` instead"
            ),
            method.span,
        )
        .with_hint(format!(
            "Value types (ADR 0042) are immutable transformations. \
             Move `{class_name}` to inherit from `Object` if it needs void methods."
        )),
    );
}

/// BT-1218: Validate that `class supervisionPolicy` override returns a valid symbol.
///
/// When a class defines `class supervisionPolicy -> Symbol => #value`, checks that
/// `#value` is one of `#permanent`, `#transient`, or `#temporary`. Only validates
/// when the method body is a single symbol literal (static check).
///
/// Checks both inline class-side methods (`class supervisionPolicy => ...` in the
/// class body) and standalone class-side method definitions
/// (`MyActor class >> supervisionPolicy => ...`).
pub(crate) fn check_supervision_policy_override(
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Collect all class-side supervisionPolicy method bodies to validate.
    // Includes both inline (class.class_methods) and standalone method definitions.
    let mut methods_to_check: Vec<&MethodDefinition> = Vec::new();

    for class in &module.classes {
        for method in &class.class_methods {
            if method.selector.name() == "supervisionPolicy" {
                methods_to_check.push(method);
            }
        }
    }
    for standalone in &module.method_definitions {
        if standalone.is_class_method && standalone.method.selector.name() == "supervisionPolicy" {
            methods_to_check.push(&standalone.method);
        }
    }

    for method in methods_to_check {
        if method.body.len() != 1 {
            continue;
        }
        let Expression::Literal(crate::ast::Literal::Symbol(sym), _) = &method.body[0].expression
        else {
            continue;
        };
        if !matches!(sym.as_str(), "permanent" | "transient" | "temporary") {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "supervisionPolicy must return #permanent, #transient, or #temporary; \
                         got #{sym}"
                    ),
                    method.body[0].expression.span(),
                )
                .with_hint("Change the return value to one of: #permanent, #transient, #temporary"),
            );
        }
    }
}

/// BT-1218: Warn when an Actor subclass in a static `children` list/array literal
/// has no explicit `supervisionPolicy` override.
///
/// The default policy is `#temporary` (not restarted on crash). Developers often
/// want `#permanent` for long-lived actors. The warning nudges them to be explicit.
///
/// Checks both inline class-side methods (`class children => ...` in the class body)
/// and standalone class-side method definitions (`MySup class >> children => ...`).
/// Recognises both `#(Worker)` list literals and `#[Worker]` array literals.
pub(crate) fn check_children_supervision_policy(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        // Look for `class children` methods in Supervisor subclasses
        if !hierarchy.is_supervisor_subclass(class_name) {
            continue;
        }
        // Check inline class-side methods
        for method in &class.class_methods {
            if method.selector.name() != "children" {
                continue;
            }
            for stmt in &method.body {
                check_children_literal_for_policy(&stmt.expression, hierarchy, diagnostics);
            }
        }
        // Check standalone class-side method definitions
        for standalone in &module.method_definitions {
            if !standalone.is_class_method {
                continue;
            }
            if standalone.class_name.name.as_str() != class_name {
                continue;
            }
            if standalone.method.selector.name() != "children" {
                continue;
            }
            for stmt in &standalone.method.body {
                check_children_literal_for_policy(&stmt.expression, hierarchy, diagnostics);
            }
        }
    }
}

fn check_children_literal_for_policy(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Supervisor.children should return a List (#() syntax), but also handle
    // Array literals (#[] syntax) defensively.
    let elements: &[Expression] = match expr {
        Expression::ListLiteral { elements, .. } | Expression::ArrayLiteral { elements, .. } => {
            elements
        }
        _ => return,
    };
    for element in elements {
        let (class_name, span) = match element {
            Expression::ClassReference { name, span } => (name.name.as_str(), *span),
            Expression::Identifier(id) => (id.name.as_str(), id.span),
            _ => continue,
        };
        // Only warn for Actor subclasses visible in the hierarchy
        if !hierarchy.is_actor_subclass(class_name) {
            continue;
        }
        // Check if the class has an explicit supervisionPolicy class method
        let has_explicit_policy = hierarchy.get_class(class_name).is_some_and(|info| {
            info.class_methods
                .iter()
                .any(|m| m.selector == "supervisionPolicy")
        });
        if !has_explicit_policy {
            diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Actor subclass `{class_name}` has no explicit `class supervisionPolicy` \
                         override; default is `#temporary` (not restarted on crash)"
                    ),
                    span,
                )
                .with_hint(
                    "Add `class supervisionPolicy -> Symbol => #permanent` (or `#transient`) \
                     to declare the restart behaviour explicitly",
                ),
            );
        }
    }
}

// ── BT-1299: Match exhaustiveness for sealed types ────────────────────────────

/// BT-1299: Error when a `match:` on a sealed type omits a known constructor
/// variant and has no wildcard arm.
///
/// Only applies to stdlib sealed types with a known complete variant set
/// (Phase 1: `Result`). Wildcard `_` suppresses the check.
///
/// # Design: pattern-based, not type-based
///
/// Beamtalk is dynamically typed — there is no resolved scrutinee type available
/// at compile time. The check is therefore keyed on the *constructor patterns in
/// the arms*, not on the static type of the matched expression:
///
/// - If any arm contains a `Result ok:` or `Result error:` constructor pattern,
///   the programmer has asserted that the value can be a `Result`, and the check
///   requires full coverage (or a wildcard `_` escape hatch).
/// - If the scrutinee happens to be a non-`Result` value, using `Result`
///   constructor patterns on it is already a programmer error (the patterns will
///   never match at runtime). The exhaustiveness error is an additional signal
///   that coverage is incomplete; the wildcard `_` arm is the correct way to opt
///   out of the check.
pub(crate) fn check_match_exhaustiveness(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        visit_match_exhaustiveness(expr, diagnostics);
    });
}

/// Returns all constructor selectors for a sealed stdlib type, or `None` if the
/// type is not a known sealed type (exhaustiveness check does not apply).
fn sealed_type_all_constructors(class: &str) -> Option<&'static [&'static str]> {
    match class {
        "Result" => Some(&["ok:", "error:"]),
        _ => None,
    }
}

/// Visitor for match exhaustiveness (BT-1299).
fn visit_match_exhaustiveness(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    let Expression::Match { arms, span, .. } = expr else {
        return;
    };

    // An unguarded wildcard arm suppresses exhaustiveness checking.
    // A guarded wildcard (`_ when: [cond] -> body`) is conditional and does
    // NOT guarantee coverage of all remaining cases.
    if arms
        .iter()
        .any(|arm| arm.guard.is_none() && matches!(arm.pattern, Pattern::Wildcard(_)))
    {
        return;
    }

    // Collect covered constructor selectors by class name.
    // Only unguarded arms count: a guarded constructor arm (`Result ok: v when: [v > 0] -> ...`)
    // is conditional and does not guarantee coverage of that variant.
    let mut covered: std::collections::HashMap<&str, Vec<String>> =
        std::collections::HashMap::new();
    for arm in arms {
        if arm.guard.is_some() {
            continue;
        }
        if let Pattern::Constructor {
            class, keywords, ..
        } = &arm.pattern
        {
            let selector: String = keywords.iter().map(|(kw, _)| kw.name.as_str()).collect();
            covered
                .entry(class.name.as_str())
                .or_default()
                .push(selector);
        }
    }

    // For each sealed class with constructor arms, check that all variants are covered.
    for (class_name, used_selectors) in &covered {
        let Some(all_selectors) = sealed_type_all_constructors(class_name) else {
            // Not a known sealed type — codegen already emits an error for unknown classes.
            continue;
        };

        let missing: Vec<&str> = all_selectors
            .iter()
            .copied()
            .filter(|sel| !used_selectors.iter().any(|u| u == *sel))
            .collect();

        if !missing.is_empty() {
            let missing_str = missing.join(", ");
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Non-exhaustive match: on sealed type `{class_name}` — \
                         missing arm{}: {missing_str}.",
                        if missing.len() == 1 { "" } else { "s" }
                    ),
                    *span,
                )
                .with_hint(
                    "Add the missing arm(s), or add a wildcard `_ ->` arm to handle \
                     remaining cases."
                        .to_string(),
                ),
            );
        }
    }
}

// ── BT-1207: Native actor validation (ADR 0056) ──────────────────────────────

/// BT-1207: Error if a `native:` class declares `state:` fields.
///
/// Native actors delegate to a backing Erlang `gen_server` — state is owned by
/// that Erlang module, not by the Beamtalk class. Declaring `state:` fields
/// would create an impossible situation where the actor has Beamtalk-managed
/// state alongside Erlang-managed state.
pub(crate) fn check_native_state_fields(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        if class.backing_module.is_some() && !class.state.is_empty() {
            let class_name = &class.name.name;
            let module_name = class
                .backing_module
                .as_ref()
                .map_or("?", |id| id.name.as_str());
            for field in &class.state {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Native actor `{class_name}` cannot declare state fields — \
                             state is owned by the backing gen_server `{module_name}`"
                        ),
                        field.name.span,
                    )
                    .with_hint(
                        "Remove the state: declaration. Manage state in the Erlang module instead."
                            .to_string(),
                    ),
                );
            }
        }
    }
}

/// BT-1207: Warn if a `self delegate` method has no return type annotation.
///
/// `self delegate` methods forward to Erlang — the compiler cannot infer the
/// return type. Without an annotation, callers have no type information and
/// the type checker cannot validate usage sites.
pub(crate) fn check_native_delegate_return_type(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Check inline class methods.
    for class in &module.classes {
        // Only check classes that have a backing module (native: classes).
        // On non-native classes, `self delegate` is a regular message send to
        // Actor.delegate (which raises at runtime), so no special warning needed.
        if class.backing_module.is_none() {
            continue;
        }

        for method in &class.methods {
            if method.is_self_delegate() && method.return_type.is_none() {
                emit_delegate_return_type_warning(&method.selector, method.span, diagnostics);
            }
        }
    }

    // Check standalone (Tonel-style) method definitions: `MyActor >> selector => self delegate`.
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue;
        }
        let class_name = standalone.class_name.name.as_str();
        if hierarchy.is_native(class_name)
            && standalone.method.is_self_delegate()
            && standalone.method.return_type.is_none()
        {
            emit_delegate_return_type_warning(
                &standalone.method.selector,
                standalone.method.span,
                diagnostics,
            );
        }
    }
}

/// Emit the warning diagnostic for a `self delegate` method missing a return type.
fn emit_delegate_return_type_warning(
    selector: &MessageSelector,
    span: crate::source_analysis::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let selector_name = selector.name();
    diagnostics.push(
        Diagnostic::warning(
            format!(
                "Method `{selector_name}` uses `self delegate` without a return type annotation"
            ),
            span,
        )
        .with_hint("Add a return type: `selector -> ReturnType => self delegate`".to_string()),
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::Severity;
    use crate::source_analysis::parse;

    /// Future is a runtime-only built-in class (BT-1057 removed the stub).
    /// User-defined `Future` classes should trigger a stdlib shadowing warning.
    #[test]
    fn future_class_triggers_shadowing_warning() {
        let tokens = lex_with_eof("Object subclass: Future\n  value => 1");
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(module.classes.len(), 1);
        let mut diagnostics = Vec::new();
        check_stdlib_name_shadowing(&module, &mut diagnostics);
        assert!(
            !diagnostics.is_empty() && diagnostics[0].severity == Severity::Warning,
            "Expected warning for Future shadowing, got: {diagnostics:?}"
        );
    }

    /// BT-738: A class named `Integer` (generated stdlib) SHOULD trigger the warning.
    #[test]
    fn stdlib_class_triggers_shadowing_warning() {
        let tokens = lex_with_eof("Object subclass: Integer\n  value => 1");
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(module.classes.len(), 1);
        let mut diagnostics = Vec::new();
        check_stdlib_name_shadowing(&module, &mut diagnostics);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("Integer"));
    }

    /// BT-1041: A class named `Self` should be rejected.
    #[test]
    fn self_as_class_name_is_error() {
        let tokens = lex_with_eof("Object subclass: Self\n  value => 1");
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_stdlib_name_shadowing(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected error for class named Self, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("reserved"));
    }

    // ── BT-914: Value type slot assignment tests ──────────────────────────────

    /// `self.slot :=` inside a Value subclass method is a compile error.
    #[test]
    fn value_subclass_slot_assign_is_error() {
        let src = "Value subclass: Point\n  state: x = 0\n  setX: v => self.x := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for slot assignment in value type, got: {diagnostics:?}"
        );
        assert_eq!(
            diagnostics[0].severity,
            Severity::Error,
            "Expected error severity, got: {:?}",
            diagnostics[0].severity
        );
        assert!(
            diagnostics[0].message.contains("value type"),
            "Expected 'value type' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `self.slot :=` inside an Actor subclass method is allowed (no diagnostic).
    #[test]
    fn actor_subclass_slot_assign_is_allowed() {
        let src = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for actor slot assignment, got: {diagnostics:?}"
        );
    }

    /// `self.slot :=` in an Actor method warns when `withSlot:` is overridden in the hierarchy.
    #[test]
    fn actor_slot_assign_warns_when_with_selector_overridden() {
        // The class defines both `withValue:` (custom override) and a method that
        // directly assigns `self.value :=` — the validator should warn.
        let src = "Actor subclass: BankAccount\n  state: value = 0\n  withValue: v => v\n  setDirectly: v => self.value := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 bypass warning, got: {diagnostics:?}"
        );
        assert_eq!(
            diagnostics[0].severity,
            Severity::Warning,
            "Expected warning severity"
        );
        assert!(
            diagnostics[0].message.contains("bypasses"),
            "Expected 'bypasses' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Inside `withValue:` itself, `self.value :=` is NOT warned (it's the implementation).
    #[test]
    fn no_bypass_warning_inside_with_selector_method() {
        let src =
            "Actor subclass: BankAccount\n  state: value = 0\n  withValue: v => self.value := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning inside withValue: implementation, got: {diagnostics:?}"
        );
    }

    /// `self.slot :=` in an Object subclass (neither Value nor Actor) is allowed without warning.
    #[test]
    fn object_subclass_slot_assign_no_diagnostic() {
        let src = "Object subclass: Plain\n  state: x = 0\n  setX: v => self.x := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for Object subclass slot assignment, got: {diagnostics:?}"
        );
    }

    /// `self.slot :=` nested inside a block within a Value subclass method is still an error.
    /// `self.slot :=` nested inside a block within a Value subclass method is still an error.
    #[test]
    fn value_slot_assign_inside_block_is_error() {
        let src = "Value subclass: Point\n  state: x = 0\n  setX: v => [self.x := v] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for slot assignment inside block in value type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    /// Standalone method definition (`Point >> setX: v => self.x := v`) on a Value subclass
    /// must be caught by the `module.method_definitions` path.
    #[test]
    fn value_standalone_method_slot_assign_is_error() {
        // class declared separately; standalone method adds a mutating method to the value type
        let src = "Value subclass: Point\n  state: x = 0\nPoint >> setX: v => self.x := v";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(
            module.method_definitions.len(),
            1,
            "Expected 1 standalone method definition"
        );
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_slot_assignment(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for standalone method slot assignment on value type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    // BT-919: Cast (!) on value type tests

    #[test]
    fn cast_on_value_type_is_error() {
        // `Point` is a Value subclass, so `Point foo!` should produce an error
        let src = "Value subclass: Point\n  state: x = 0\n  doStuff => Point foo!";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_cast_on_value_type(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for cast on value type, got: {diagnostics:?}"
        );
        assert!(
            diagnostics[0]
                .message
                .contains("Cannot use ! (cast) on value type"),
            "Expected value-type cast error, got: {}",
            diagnostics[0].message
        );
    }

    // ── BT-951: Effect-free statement tests ──────────────────────────────────

    /// A lone literal in a method body is its return value — no lint warning.
    #[test]
    fn single_literal_in_method_no_lint() {
        let src = "Object subclass: Foo\n  bar => 42";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert!(
            diagnostics.is_empty(),
            "Expected no lint for single literal return value, got: {diagnostics:?}"
        );
    }

    /// A literal appearing as a non-last statement should produce a lint.
    #[test]
    fn literal_non_last_statement_emits_lint() {
        let src = "Object subclass: Foo\n  bar =>\n    42\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded literal, got: {diagnostics:?}"
        );
        assert_eq!(
            diagnostics[0].severity,
            Severity::Lint,
            "Expected Lint severity"
        );
        assert!(
            diagnostics[0].message.contains("literal"),
            "Expected 'literal' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// A string literal as a non-last statement should produce a lint.
    #[test]
    fn string_literal_non_last_emits_lint() {
        let src = "Object subclass: Foo\n  bar =>\n    \"hello\"\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded string literal, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
    }

    /// `x + y` as a non-last statement (pure arithmetic) should produce a lint.
    #[test]
    fn pure_binary_expr_non_last_emits_lint() {
        let src = "Object subclass: Foo\n  bar: x and: y =>\n    x + y\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for pure binary expression, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
        assert!(
            diagnostics[0].message.contains("pure expression"),
            "Got: {}",
            diagnostics[0].message
        );
    }

    /// A message send with side effects (keyword send) should NOT produce a lint.
    #[test]
    fn effectful_send_no_lint() {
        let src = "Object subclass: Foo\n  bar =>\n    self doSomething\n    self doOtherThing";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert!(
            diagnostics.is_empty(),
            "Expected no lint for effectful sends, got: {diagnostics:?}"
        );
    }

    /// A literal inside a block in non-last position should produce a lint.
    #[test]
    fn literal_in_block_non_last_emits_lint() {
        let src = "Object subclass: Foo\n  bar => [42. self doSomething] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded literal in block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
    }

    /// Multiple effect-free statements produce one lint each.
    #[test]
    fn multiple_effect_free_stmts_emit_multiple_lints() {
        let src = "Object subclass: Foo\n  bar =>\n    42\n    \"hello\"\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            2,
            "Expected 2 lints for two discarded literals, got: {diagnostics:?}"
        );
    }

    /// BT-979: Module-level expressions ARE linted by default.
    #[test]
    fn module_level_effect_free_linted_by_default() {
        let src = "42.\nself doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded module-level literal, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
    }

    /// BT-979: Module-level expressions NOT linted when opt-out flag is set.
    #[test]
    fn module_level_effect_free_skipped_with_flag() {
        let src = "42.\nself doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, true);
        assert!(
            diagnostics.is_empty(),
            "Expected no lint when skip_module_expression_lint is true, got: {diagnostics:?}"
        );
    }

    /// Standalone method definition: non-last literal triggers lint.
    #[test]
    fn standalone_method_effect_free_emits_lint() {
        let src = "Object subclass: Foo\nFoo >> bar =>\n  42\n  self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(
            module.method_definitions.len(),
            1,
            "Expected 1 standalone method"
        );
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded literal in standalone method, got: {diagnostics:?}"
        );
    }

    #[test]
    fn cast_on_actor_type_is_ok() {
        // Actor subclass should NOT produce an error for cast
        let src = "Actor subclass: Worker\n  doStuff => Worker foo!";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_cast_on_value_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors for cast on actor type, got: {diagnostics:?}"
        );
    }

    // ── BT-950: Redundant assignment tests ───────────────────────────────────

    /// `x := x` at the top level emits a warning.
    #[test]
    fn redundant_assignment_top_level_warns() {
        let src = "x := 1.\nx := x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("Redundant assignment"),
            "Expected 'Redundant assignment' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0].message.contains("x := x"),
            "Expected 'x := x' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `x := y` (different names) does NOT warn.
    #[test]
    fn non_redundant_assignment_no_warn() {
        let src = "x := 1.\ny := 2.\nx := y";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-redundant assignment, got: {diagnostics:?}"
        );
    }

    /// `x := x` inside a method body (using a parameter) emits a warning.
    #[test]
    fn redundant_assignment_in_method_warns() {
        let src = "Object subclass: Foo\n  withX: x => x := x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment in method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// `x := x` nested inside a block emits a warning.
    #[test]
    fn redundant_assignment_in_block_warns() {
        let src = "Object subclass: Foo\n  withX: x => [x := x] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment inside block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// `self.x := self.x` (field access, not plain identifiers) does NOT trigger
    /// the redundant-assignment check (that's a separate BT-914 concern).
    #[test]
    fn field_access_assignment_not_flagged_as_redundant() {
        let src = "Object subclass: Foo\n  state: x = 0\n  noOp => self.x := self.x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no redundant-assignment warning for field access, got: {diagnostics:?}"
        );
    }

    /// `x := x` inside a standalone method definition warns.
    #[test]
    fn redundant_assignment_in_standalone_method_warns() {
        let src = "Object subclass: Foo\n  value => 1\nFoo >> withX: x => x := x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment in standalone method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    // ── BT-953: Self capture in collection HOF blocks ─────────────────────────

    /// `self` inside a `collect:` block emits a hint.
    #[test]
    fn self_capture_in_collect_block_hints() {
        let src =
            "Actor subclass: Processor\n  process: items => items collect: [:x | self handle: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in collect:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
        assert!(
            diagnostics[0].message.contains("collect:"),
            "Expected 'collect:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `self` inside an `inject:into:` block emits a hint.
    #[test]
    fn self_capture_in_inject_into_block_hints() {
        let src = "Actor subclass: Processor\n  run: items => items inject: 0 into: [:acc :x | self transform: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in inject:into:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
        assert!(diagnostics[0].message.contains("inject:into:"));
    }

    /// No `self` in the block — no hint.
    #[test]
    fn no_self_in_collect_block_no_hint() {
        let src = "Actor subclass: Processor\n  process: items => items collect: [:x | x * 2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints when self is not in block, got: {diagnostics:?}"
        );
    }

    /// `self` in an `ifTrue:` block is safe — no hint.
    #[test]
    fn self_in_if_true_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => (x > 0) ifTrue: [self doWork]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for self in ifTrue: block, got: {diagnostics:?}"
        );
    }

    /// `Object subclass:` does NOT use `calling_self` dispatch — no hint emitted.
    #[test]
    fn self_capture_in_object_subclass_no_hint() {
        let src = "Object subclass: Formatter\n  format: rows => rows collect: [:row | self formatRow: row]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for Object subclass: (no calling_self dispatch), got: {diagnostics:?}"
        );
    }

    /// `Value subclass:` uses synchronous pure-function dispatch — no hint emitted.
    #[test]
    fn self_capture_in_value_subclass_no_hint() {
        let src =
            "Value subclass: Point\n  scale: factor => #(1, 2) collect: [:c | self transform: c]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for Value subclass: (synchronous dispatch), got: {diagnostics:?}"
        );
    }

    /// `self` inside a `do:` block in an Actor class emits a hint.
    #[test]
    fn self_capture_in_do_block_hints() {
        let src = "Actor subclass: Runner\n  run: items => items do: [:x | self process: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in do:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// `self` inside a map literal within a collect: block in an Actor class still triggers.
    #[test]
    fn self_capture_nested_in_map_literal_hints() {
        let src =
            "Actor subclass: Foo\n  run: items => items collect: [:x | #{#key => self value}]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self nested in map literal, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// A block variable (not a literal block) passed to collect: does NOT trigger
    /// the hint — we only flag literal blocks.
    #[test]
    fn block_variable_in_collect_no_hint() {
        let src = "Object subclass: Foo\n  run: items with: blk => items collect: blk";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for block variable (not literal), got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) Actor method with self in collect: emits a hint.
    #[test]
    fn self_capture_in_standalone_actor_method_hints() {
        let src = "Actor subclass: Processor\nProcessor >> process: items => items collect: [:x | self handle: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in standalone Actor method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// Standalone (Tonel-style) Object method with self in collect: — no hint.
    #[test]
    fn self_capture_in_standalone_object_method_no_hint() {
        let src = "Object subclass: Formatter\nFormatter >> format: rows => rows collect: [:row | self formatRow: row]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for standalone Object subclass: method, got: {diagnostics:?}"
        );
    }

    /// Transitive Actor subclass: class inheriting from an Actor also gets the hint.
    #[test]
    fn self_capture_in_transitive_actor_subclass_hints() {
        let src = "Actor subclass: BaseActor\nBaseActor subclass: SpecificActor\n  process: items => items collect: [:x | self handle: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for transitive Actor subclass, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    // ── Field access / assignment in collection blocks (false-positive tests) ──

    /// `self.field` read inside collect: is a maps:get, not a `gen_server` call — no hint.
    #[test]
    fn self_field_read_in_collect_no_hint() {
        let src =
            "Actor subclass: Counter\n  sumItems: items => items collect: [:x | x + self.total]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self.field read (maps:get, not gen_server), got: {diagnostics:?}"
        );
    }

    /// `self.field :=` write inside inject:into: is a maps:put, not a `gen_server` call — no hint.
    #[test]
    fn self_field_write_in_inject_no_hint() {
        let src = "Actor subclass: Counter\n  accumulate: items =>\n    items inject: 0 into: [:acc :val |\n      self.total := self.total + val\n      acc + val]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self.field := (maps:put, not gen_server), got: {diagnostics:?}"
        );
    }

    /// Sending a message to a field value (`self.cache at: x`) inside a block is safe —
    /// the receiver of the send is `self.cache` (a maps:get result), not `self`.
    #[test]
    fn message_send_to_self_field_no_hint() {
        let src =
            "Actor subclass: Repo\n  process: items => items collect: [:x | self.cache at: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for message sent to self.field value (not self), got: {diagnostics:?}"
        );
    }

    /// `self someMessage` (actual message send to self) inside collect: still fires.
    #[test]
    fn self_message_send_in_collect_still_hints() {
        let src =
            "Actor subclass: Counter\n  process: items => items collect: [:x | self transform: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for actual self message send in collect:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    // ── BT-1301: Timer block self-capture false-positive tests ───────────────

    /// `Timer after:do:` with `self` in the block — no hint (separate process).
    #[test]
    fn self_in_timer_after_do_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => Timer after: 0 do: [self doWork]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self in Timer after:do: block (separate process), got: {diagnostics:?}"
        );
    }

    /// `Timer every:do:` with `self` in the block — no hint (separate process).
    #[test]
    fn self_in_timer_every_do_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => Timer every: 100 do: [self tick]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self in Timer every:do: block (separate process), got: {diagnostics:?}"
        );
    }

    /// `Timer after:do:` block nested inside a collection HOF — no hint (Timer
    /// runs in a separate process even when scheduled from a HOF iteration).
    #[test]
    fn self_in_timer_block_nested_in_hof_no_hint() {
        let src = "Actor subclass: Worker\n  process: items => items do: [:x | Timer after: 0 do: [self handle: x]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self in Timer block inside do: (Timer runs separately), got: {diagnostics:?}"
        );
    }

    /// Cast (`!`) variant: `Timer after:do:` with `self someMethod!` — no hint.
    #[test]
    fn self_cast_in_timer_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => Timer after: 0 do: [self doWork!]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self cast in Timer after:do: block, got: {diagnostics:?}"
        );
    }

    /// A direct `self` call inside a collection HOF still fires even when a Timer
    /// is also present — only the Timer block is safe, not sibling direct self-sends.
    #[test]
    fn direct_self_in_hof_with_timer_still_hints() {
        let src = "Actor subclass: Worker\n  process: items => items do: [:x | self log: x. Timer after: 0 do: [self handle: x]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for direct self send in do: block alongside Timer, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// `self` in the *receiver* of a timer call inside a HOF still warns —
    /// e.g. `(self getTimer) after: 0 do: [block]` — the receiver goes through
    /// `calling_self` dispatch in the HOF and is NOT safe.
    #[test]
    fn self_in_timer_receiver_inside_hof_still_hints() {
        let src = "Actor subclass: Worker\n  process: items => items do: [:x | (self getTimer) after: 0 do: [x doSomething]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self in Timer receiver inside do: block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    // ── BT-955: Literal boolean condition tests ───────────────────────────────

    /// `true ifTrue: [42]` — condition always true, branch always taken.
    #[test]
    fn literal_true_if_true_warns() {
        let src = "true ifTrue: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for true ifTrue:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `true`"),
            "Expected 'always `true`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `false ifFalse: [42]` — condition always false, branch always taken.
    #[test]
    fn literal_false_if_false_warns() {
        let src = "false ifFalse: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for false ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `false`"),
            "Expected 'always `false`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `true ifFalse: [42]` — condition always true, `ifFalse:` branch unreachable.
    #[test]
    fn literal_true_if_false_warns() {
        let src = "true ifFalse: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for true ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `true`"),
            "Expected 'always `true`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `false ifTrue: [42]` — condition always false, `ifTrue:` branch unreachable.
    #[test]
    fn literal_false_if_true_warns() {
        let src = "false ifTrue: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for false ifTrue:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `false`"),
            "Expected 'always `false`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `true ifTrue: [1] ifFalse: [2]` — `ifFalse:` branch is dead code.
    #[test]
    fn literal_true_if_true_if_false_warns() {
        let src = "true ifTrue: [1] ifFalse: [2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for true ifTrue:ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `true`"),
            "Expected 'always `true`' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .is_some_and(|h| h.contains("ifFalse:")),
            "Expected hint to mention `ifFalse:`, got: {:?}",
            diagnostics[0].hint
        );
    }

    /// `false ifTrue: [1] ifFalse: [2]` — `ifTrue:` branch is dead code.
    #[test]
    fn literal_false_if_true_if_false_warns() {
        let src = "false ifTrue: [1] ifFalse: [2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for false ifTrue:ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `false`"),
            "Expected 'always `false`' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .is_some_and(|h| h.contains("ifTrue:")),
            "Expected hint to mention `ifTrue:`, got: {:?}",
            diagnostics[0].hint
        );
    }

    /// A non-literal receiver does NOT trigger the warning.
    #[test]
    fn non_literal_receiver_no_warn() {
        let src = "x := true.\nx ifTrue: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-literal receiver, got: {diagnostics:?}"
        );
    }

    /// A non-boolean conditional selector does NOT trigger the warning.
    #[test]
    fn non_conditional_selector_no_warn() {
        let src = "true printString";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-conditional selector, got: {diagnostics:?}"
        );
    }

    /// Warning fires inside a method body.
    #[test]
    fn literal_bool_condition_in_method_warns() {
        let src = "Object subclass: Foo\n  run => true ifFalse: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning inside method body, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// Warning fires inside a nested block.
    #[test]
    fn literal_bool_condition_in_block_warns() {
        let src = "Object subclass: Foo\n  run => [false ifTrue: [1]] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning inside nested block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// Message with literal boolean receiver should warn (receiver of first message before cascade).
    /// `true ifTrue: [1]; ifFalse: [2]` — warns for the first message's receiver.
    #[test]
    fn literal_bool_cascade_warns_for_each_message() {
        let src = "true ifTrue: [1]; ifFalse: [2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for cascade starting with literal true, got: {diagnostics:?}"
        );
        assert!(diagnostics.iter().all(|d| d.severity == Severity::Warning));
        assert!(
            diagnostics
                .iter()
                .all(|d| d.message.contains("always `true`")),
            "Expected all messages to say 'always `true`', got: {diagnostics:?}"
        );
    }

    // ── BT-1052: Value type `-> Nil` return annotation tests ─────────────────

    /// Value instance method with `-> Nil` is an error.
    #[test]
    fn value_instance_method_nil_return_is_error() {
        let src = "Value subclass: MyVal\n  doSomething -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for -> Nil on value type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("doSomething"),
            "Expected selector in message, got: {:?}",
            diagnostics[0].message
        );
    }

    /// Value class-side method with `-> Nil` is exempt.
    #[test]
    fn value_class_method_nil_return_is_exempt() {
        let src = "Value subclass: MyVal\n  class create -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for class-side -> Nil, got: {diagnostics:?}"
        );
    }

    /// Object instance method with `-> Nil` is not flagged.
    #[test]
    fn object_instance_method_nil_return_is_exempt() {
        let src = "Object subclass: MyObj\n  doSomething -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for Object -> Nil, got: {diagnostics:?}"
        );
    }

    /// Actor instance method with `-> Nil` is not flagged.
    #[test]
    fn actor_instance_method_nil_return_is_exempt() {
        let src = "Actor subclass: MyActor\n  doSomething -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for Actor -> Nil, got: {diagnostics:?}"
        );
    }

    /// Value instance method with a Block parameter and `-> Nil` is exempt (iteration pattern).
    #[test]
    fn value_block_param_nil_return_is_exempt() {
        let src = "Value subclass: MyVal\n  each: block :: Block -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for Block-param -> Nil (iteration pattern), got: {diagnostics:?}"
        );
    }

    /// Value instance method WITHOUT a Block parameter and `-> Nil` is still an error.
    #[test]
    fn value_no_block_param_nil_return_is_error() {
        let src = "Value subclass: MyVal\n  sideEffect -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected error for -> Nil without Block param, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    /// Value instance method with `self subclassResponsibility` is exempt.
    #[test]
    fn value_subclass_responsibility_nil_return_is_exempt() {
        let src = "Value subclass: MyVal\n  doSomething -> Nil => self subclassResponsibility";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for subclassResponsibility placeholder, got: {diagnostics:?}"
        );
    }

    /// `^ self subclassResponsibility` (early-return form) is also exempt.
    #[test]
    fn value_subclass_responsibility_early_return_is_exempt() {
        let src = "Value subclass: MyVal\n  doSomething -> Nil => ^ self subclassResponsibility";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for ^ self subclassResponsibility, got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) Value method with `-> Nil` is also an error.
    #[test]
    fn standalone_value_method_nil_return_is_error() {
        let src = "Value subclass: MyVal\n  doSomething => nil\nMyVal >> doVoid -> Nil => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_value_nil_return(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for standalone -> Nil on value type, got: {diagnostics:?}"
        );
        assert!(
            diagnostics[0].message.contains("doVoid"),
            "Expected selector in message, got: {:?}",
            diagnostics[0].message
        );
    }

    // ── BT-1218: supervisionPolicy override validation ────────────────────────

    /// Valid `#permanent` override — no diagnostics.
    #[test]
    fn supervision_policy_permanent_is_valid() {
        let src = "Actor subclass: MyActor\n  class supervisionPolicy -> Symbol => #permanent\n  go => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for #permanent, got: {diagnostics:?}"
        );
    }

    /// Valid `#transient` override — no diagnostics.
    #[test]
    fn supervision_policy_transient_is_valid() {
        let src = "Actor subclass: MyActor\n  class supervisionPolicy -> Symbol => #transient\n  go => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for #transient, got: {diagnostics:?}"
        );
    }

    /// Valid `#temporary` override — no diagnostics.
    #[test]
    fn supervision_policy_temporary_is_valid() {
        let src = "Actor subclass: MyActor\n  class supervisionPolicy -> Symbol => #temporary\n  go => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for #temporary, got: {diagnostics:?}"
        );
    }

    /// Invalid symbol `#restart` — error diagnostic.
    #[test]
    fn supervision_policy_invalid_symbol_is_error() {
        let src =
            "Actor subclass: MyActor\n  class supervisionPolicy -> Symbol => #restart\n  go => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for #restart, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("restart"),
            "Expected 'restart' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Multi-expression body is skipped (no static check possible).
    #[test]
    fn supervision_policy_multi_expr_body_is_skipped() {
        let src = "Actor subclass: MyActor\n  class supervisionPolicy -> Symbol =>\n    x := #invalid\n    x\n  go => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for multi-expr body, got: {diagnostics:?}"
        );
    }

    // ── BT-1218: children supervision policy warning ──────────────────────────

    /// Actor subclass in children array with no policy override → warning.
    #[test]
    fn children_actor_without_policy_warns() {
        let src = "Actor subclass: Worker\n  go => nil\nSupervisor subclass: MySup\n  class children => #(Worker)";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_children_supervision_policy(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for Worker with no policy, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("Worker"),
            "Expected 'Worker' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Actor subclass with explicit policy override → no warning.
    #[test]
    fn children_actor_with_policy_no_warning() {
        let src = "Actor subclass: Worker\n  class supervisionPolicy -> Symbol => #permanent\n  go => nil\nSupervisor subclass: MySup\n  class children => #(Worker)";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_children_supervision_policy(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning for Worker with #permanent policy, got: {diagnostics:?}"
        );
    }

    // ── BT-1299: Match exhaustiveness for sealed types ────────────────────────

    /// Missing `error:` arm without wildcard → compile error.
    #[test]
    fn match_exhaustiveness_result_missing_error_arm_is_error() {
        let src = "(Result ok: 42) match: [Result ok: v -> [v + 1]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for missing error: arm, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("error:"),
            "Expected 'error:' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0].message.contains("Result"),
            "Expected 'Result' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Missing `ok:` arm without wildcard → compile error.
    #[test]
    fn match_exhaustiveness_result_missing_ok_arm_is_error() {
        let src = "(Result error: #x) match: [Result error: e -> [e]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for missing ok: arm, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("ok:"),
            "Expected 'ok:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Both arms present → no error.
    #[test]
    fn match_exhaustiveness_result_both_arms_no_error() {
        let src = "r match: [Result ok: v -> [v]; Result error: e -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error when both Result arms present, got: {diagnostics:?}"
        );
    }

    /// Wildcard arm suppresses exhaustiveness check.
    #[test]
    fn match_exhaustiveness_wildcard_suppresses_check() {
        let src = "r match: [Result ok: v -> [v]; _ -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error when wildcard present, got: {diagnostics:?}"
        );
    }

    /// Non-sealed type match (no constructor patterns) → no error.
    #[test]
    fn match_exhaustiveness_non_constructor_match_no_error() {
        let src = "x match: [1 -> [#one]; 2 -> [#two]; _ -> [#other]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for non-constructor match, got: {diagnostics:?}"
        );
    }

    /// Non-Result scrutinee with a Result constructor arm still triggers the check.
    ///
    /// Using `Result ok:` on a non-Result value is itself a programmer error
    /// (the pattern will never match at runtime). The exhaustiveness error fires
    /// as an additional signal; the programmer should add a wildcard `_` arm
    /// or cover all Result variants.
    ///
    /// This test pins the pattern-based (not type-based) boundary: the check is
    /// keyed on which constructor patterns appear in the arms, not on the static
    /// type of the scrutinee expression.
    #[test]
    fn match_exhaustiveness_non_result_scrutinee_with_result_arm_is_error() {
        let src = "42 match: [Result ok: v -> v]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error when Result constructor arm appears with non-Result scrutinee, \
             got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("error:"),
            "Expected 'error:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Exhaustiveness check fires inside a method body.
    #[test]
    fn match_exhaustiveness_fires_inside_method() {
        let src = "Object subclass: Foo\n  run: r => r match: [Result ok: v -> [v]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error inside method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    /// Guarded wildcard does NOT suppress exhaustiveness — the guard means it
    /// won't always match, so missing variants are still uncovered.
    #[test]
    fn match_exhaustiveness_guarded_wildcard_does_not_suppress() {
        let src = "r match: [Result ok: v -> [v]; _ when: [true] -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error: guarded wildcard should not suppress exhaustiveness, \
             got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("error:"),
            "Expected 'error:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Guarded constructor arm does NOT count as coverage — the guard means
    /// it won't match when the guard is false.
    #[test]
    fn match_exhaustiveness_guarded_constructor_arm_not_counted() {
        let src = "r match: [Result ok: v when: [v > 0] -> [v]; Result error: e -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error: guarded ok: arm should not count as full ok: coverage, \
             got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("ok:"),
            "Expected 'ok:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Unguarded wildcard still suppresses exhaustiveness.
    #[test]
    fn match_exhaustiveness_unguarded_wildcard_still_suppresses() {
        let src = "r match: [Result ok: v -> [v]; _ -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error: unguarded wildcard should suppress check, got: {diagnostics:?}"
        );
    }

    /// Unguarded constructor arm still counts as coverage.
    #[test]
    fn match_exhaustiveness_unguarded_constructor_arm_counts() {
        let src = "r match: [Result ok: v -> [v]; Result error: _ -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error: both unguarded arms present, got: {diagnostics:?}"
        );
    }

    // ── BT-1207: Native actor validation tests ────────────────────────────────

    /// Native actor with state: field → compile error.
    #[test]
    fn native_actor_state_field_is_error() {
        let src = "Actor subclass: MyActor native: my_mod\n  state: count = 0\n  increment => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for state: on native actor, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0]
                .message
                .contains("cannot declare state fields"),
            "Expected 'cannot declare state fields' in message, got: {}",
            diagnostics[0].message
        );
        assert!(diagnostics[0].message.contains("my_mod"));
    }

    /// Native actor with multiple state: fields → one error per field.
    #[test]
    fn native_actor_multiple_state_fields_multiple_errors() {
        let src = "Actor subclass: MyActor native: my_mod\n  state: x = 0\n  state: y = 0\n  get => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            2,
            "Expected 2 errors (one per state field), got: {diagnostics:?}"
        );
    }

    /// Non-native actor with state: fields → no error.
    #[test]
    fn non_native_actor_state_fields_ok() {
        let src = "Actor subclass: Counter\n  state: count = 0\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors for non-native actor, got: {diagnostics:?}"
        );
    }

    /// classState: is allowed on native actors — no error.
    #[test]
    fn native_actor_class_state_ok() {
        let src = "Actor subclass: MyActor native: my_mod\n  classState: instances = 0\n  get => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_native_state_fields(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "classState: should be allowed on native actors, got: {diagnostics:?}"
        );
    }

    /// self delegate method without return type → warning on native actor.
    #[test]
    fn native_delegate_missing_return_type_warning() {
        let src = "Actor subclass: MyActor native: my_mod\n  getValue => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for missing return type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(diagnostics[0].message.contains("return type annotation"));
    }

    /// self delegate method with return type → no warning.
    #[test]
    fn native_delegate_with_return_type_ok() {
        let src = "Actor subclass: MyActor native: my_mod\n  getValue -> Integer => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning when return type is present, got: {diagnostics:?}"
        );
    }

    /// self delegate on non-native class → no warning (it's a regular message send).
    #[test]
    fn non_native_delegate_no_warning() {
        let src = "Actor subclass: Counter\n  state: count = 0\n  getValue => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning for non-native class, got: {diagnostics:?}"
        );
    }

    /// Non-delegate method body on native class → no warning.
    #[test]
    fn native_non_delegate_method_no_warning() {
        let src = "Actor subclass: MyActor native: my_mod\n  version => 1";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning for non-delegate method, got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) self delegate without return type on native class → warning.
    #[test]
    fn native_standalone_delegate_missing_return_type_warning() {
        let src = "Actor subclass: MyActor native: my_mod\n  get -> Integer => self delegate\nMyActor >> getValue => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for standalone method missing return type, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("getValue"));
    }

    /// Standalone self delegate with return type on native class → no warning.
    #[test]
    fn native_standalone_delegate_with_return_type_ok() {
        let src = "Actor subclass: MyActor native: my_mod\n  get -> Integer => self delegate\nMyActor >> getValue -> Integer => self delegate";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_native_delegate_return_type(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warning when standalone method has return type, got: {diagnostics:?}"
        );
    }
}

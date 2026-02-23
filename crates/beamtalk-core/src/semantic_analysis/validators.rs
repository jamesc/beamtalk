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

use crate::ast::{Expression, Identifier, Module};
use crate::semantic_analysis::ClassHierarchy;
#[cfg(test)]
use crate::source_analysis::lex_with_eof;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
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
    walk_module_expressions(module, hierarchy, diagnostics, visit_abstract_instantiation);
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

/// Walks all expressions in a module (top-level + class methods),
/// calling `visitor` on each expression.
fn walk_module_expressions(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
    visitor: fn(&Expression, &ClassHierarchy, &mut Vec<Diagnostic>),
) {
    for expr in &module.expressions {
        walk_expression(expr, hierarchy, diagnostics, visitor);
    }
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for expr in &method.body {
                walk_expression(expr, hierarchy, diagnostics, visitor);
            }
        }
    }
    for standalone in &module.method_definitions {
        for expr in &standalone.method.body {
            walk_expression(expr, hierarchy, diagnostics, visitor);
        }
    }
}

/// Recursively walks an expression tree, calling `visitor` on each node
/// before recursing into children.
fn walk_expression(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
    visitor: fn(&Expression, &ClassHierarchy, &mut Vec<Diagnostic>),
) {
    visitor(expr, hierarchy, diagnostics);
    match expr {
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            walk_expression(receiver, hierarchy, diagnostics, visitor);
            for arg in arguments {
                walk_expression(arg, hierarchy, diagnostics, visitor);
            }
        }
        Expression::Block(block) => {
            for e in &block.body {
                walk_expression(e, hierarchy, diagnostics, visitor);
            }
        }
        Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
            walk_expression(value, hierarchy, diagnostics, visitor);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            walk_expression(receiver, hierarchy, diagnostics, visitor);
            for msg in messages {
                for arg in &msg.arguments {
                    walk_expression(arg, hierarchy, diagnostics, visitor);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            walk_expression(expression, hierarchy, diagnostics, visitor);
        }
        Expression::FieldAccess { receiver, .. } => {
            walk_expression(receiver, hierarchy, diagnostics, visitor);
        }
        Expression::Match { value, arms, .. } => {
            walk_expression(value, hierarchy, diagnostics, visitor);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expression(guard, hierarchy, diagnostics, visitor);
                }
                walk_expression(&arm.body, hierarchy, diagnostics, visitor);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_expression(&pair.key, hierarchy, diagnostics, visitor);
                walk_expression(&pair.value, hierarchy, diagnostics, visitor);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_expression(elem, hierarchy, diagnostics, visitor);
            }
            if let Some(t) = tail {
                walk_expression(t, hierarchy, diagnostics, visitor);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    walk_expression(e, hierarchy, diagnostics, visitor);
                }
            }
        }
        _ => {}
    }
}

/// BT-563: Warn when Actor subclasses use `new` or `new:` instead of `spawn`.
pub(crate) fn check_actor_new_usage(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module_expressions(module, hierarchy, diagnostics, visit_actor_new);
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
                let mut diag = Diagnostic::warning(
                    format!("Actor subclass `{class_name}` should use `spawn` instead of `{sel}`"),
                    *span,
                );
                diag.hint = Some("Use spawn instead of new for Actor subclasses".into());
                diagnostics.push(diag);
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
                    let mut diag = Diagnostic::warning(
                        format!(
                            "Actor subclass `{class_name}` should use `spawn` instead of `{sel}`"
                        ),
                        msg.span,
                    );
                    diag.hint = Some("Use spawn instead of new for Actor subclasses".into());
                    diagnostics.push(diag);
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
    walk_module_expressions(module, hierarchy, diagnostics, visit_new_field_names);
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
                let mut diag = Diagnostic::warning(
                    format!("Unknown field `{sym}` for class `{class_name}`"),
                    *sym_span,
                );
                let fields: Vec<&str> = declared_state.iter().map(EcoString::as_str).collect();
                diag.hint = Some(format!("Declared fields: {}", fields.join(", ")).into());
                diagnostics.push(diag);
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
    walk_module_expressions(module, hierarchy, diagnostics, visit_classvar_access);
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
                        let mut diag = Diagnostic::warning(
                            format!(
                                "Undefined class variable `{var_name}` on class `{class_name}`"
                            ),
                            *span,
                        );
                        if class_vars.is_empty() {
                            diag.hint = Some(
                                format!("`{class_name}` has no declared class variables").into(),
                            );
                        } else {
                            let vars: Vec<&str> =
                                class_vars.iter().map(EcoString::as_str).collect();
                            diag.hint = Some(
                                format!("Declared class variables: {}", vars.join(", ")).into(),
                            );
                        }
                        diagnostics.push(diag);
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
                            let mut diag = Diagnostic::warning(
                                format!(
                                    "Undefined class variable `{var_name}` on class `{class_name}`"
                                ),
                                msg.span,
                            );
                            if class_vars.is_empty() {
                                diag.hint = Some(
                                    format!("`{class_name}` has no declared class variables")
                                        .into(),
                                );
                            } else {
                                let vars: Vec<&str> =
                                    class_vars.iter().map(EcoString::as_str).collect();
                                diag.hint = Some(
                                    format!("Declared class variables: {}", vars.join(", ")).into(),
                                );
                            }
                            diagnostics.push(diag);
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
/// Runtime-only built-ins like `Future` are intentionally excluded because
/// they lack the `bt@stdlib@` prefix and loading actually succeeds (BT-750).
///
/// This must NOT be called during stdlib compilation (`stdlib_mode = true`).
/// Call it alongside `validate_primitives`, guarded by `!options.stdlib_mode`.
pub fn check_stdlib_name_shadowing(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        let name = class.name.name.as_str();
        if ClassHierarchy::is_runtime_protected_class(name) {
            let mut diag = Diagnostic::warning(
                format!(
                    "Class name `{name}` conflicts with a stdlib class. \
                     Loading will fail because stdlib class names are protected."
                ),
                class.name.span,
            );
            diag.hint = Some(
                format!("Choose a different name. `{name}` is a protected stdlib class name.")
                    .into(),
            );
            diagnostics.push(diag);
        }
    }
}

/// BT-631: Warn about empty method bodies.
///
/// Methods declared with `=>` but no body expressions are likely incomplete.
/// The codegen returns `self` for these, but users should be aware.
pub(crate) fn check_empty_method_bodies(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if method.body.is_empty() {
                let selector = method.selector.name();
                let mut diag = Diagnostic::warning(
                    format!("Method `{selector}` has an empty body and will return `self`"),
                    method.span,
                )
                .with_category(DiagnosticCategory::EmptyBody);
                diag.hint =
                    Some("Add an expression after `=>`, or remove the method if unneeded".into());
                diagnostics.push(diag);
            }
        }
    }
    for standalone in &module.method_definitions {
        if standalone.method.body.is_empty() {
            let selector = standalone.method.selector.name();
            let mut diag = Diagnostic::warning(
                format!("Method `{selector}` has an empty body and will return `self`"),
                standalone.method.span,
            )
            .with_category(DiagnosticCategory::EmptyBody);
            diag.hint =
                Some("Add an expression after `=>`, or remove the method if unneeded".into());
            diagnostics.push(diag);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Severity;
    use crate::source_analysis::parse;

    /// Future is now a real stdlib class (stdlib/src/Future.bt exists).
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
}

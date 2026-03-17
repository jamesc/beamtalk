// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Supervision policy validators.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators for OTP supervision policies:
//! - Supervision policy override validation (BT-1218)
//! - Children supervision policy warnings (BT-1218)

use crate::ast::{Expression, MethodDefinition, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::Diagnostic;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::Severity;
    use crate::source_analysis::lex_with_eof;
    use crate::source_analysis::parse;

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
}

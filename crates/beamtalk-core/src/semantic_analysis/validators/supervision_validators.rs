// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Supervision policy validators.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators for OTP supervision policies:
//! - Supervision policy override validation
//! - Children supervision policy warnings
//! - Removed class-side supervisor configuration (ADR 0135 §2)

use crate::ast::{Expression, MethodDefinition, Module};
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

/// Validate that `class supervisionPolicy` override returns a valid symbol.
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

/// Class-side supervisor configuration selectors that ADR 0135 §2 moved to the
/// instance side, with the fix-it hint for each.
///
/// The runtime reads configuration from the supervisor instance being started,
/// so a class-side definition of one of these would be silently ignored. That
/// is why defining one is a compile error rather than a warning.
const REMOVED_CLASS_SIDE_SUPERVISOR_CONFIG: &[(&str, &str)] = &[
    (
        "children",
        "`class children` is no longer read; define instance-side `childSpecs` \
         (e.g. `childSpecs => #(Worker)`)",
    ),
    (
        "initialize:",
        "`class initialize: sup` is now instance-side `initialize`; use `self` for `sup` \
         (e.g. `initialize => (self which: Worker) unwrap ...`)",
    ),
    (
        "childClass",
        "`class childClass` is no longer read; define it on the instance side (drop `class`): \
         `childClass => Worker`",
    ),
    (
        "strategy",
        "`class strategy` is no longer read; define it on the instance side (drop `class`): \
         `strategy => #oneForAll`",
    ),
    (
        "maxRestarts",
        "`class maxRestarts` is no longer read; define it on the instance side (drop `class`): \
         `maxRestarts => 5`",
    ),
    (
        "restartWindow",
        "`class restartWindow` is no longer read; define it on the instance side (drop `class`): \
         `restartWindow => 30`",
    ),
    (
        "maxChildren",
        "`class maxChildren` is no longer read; define it on the instance side (drop `class`): \
         `maxChildren => 100`",
    ),
    (
        "childShutdown",
        "`class childShutdown` is no longer read; define it on the instance side (drop `class`): \
         `childShutdown => 15000`",
    ),
];

/// Render `beamtalk_supervisor_config.hrl`: the removed class-side supervisor
/// configuration selectors as an Erlang list, so the runtime's ADR 0084
/// override warning (`beamtalk_object_class:put_class_method/4`) uses the
/// same list as [`check_class_side_supervisor_config`] instead of a copy.
/// Written by `beamtalk build-stdlib`; `just check-generated-builtins` fails
/// when the checked-in file is stale.
#[must_use]
pub fn removed_supervisor_config_erlang_header() -> String {
    let selectors = REMOVED_CLASS_SIDE_SUPERVISOR_CONFIG
        .iter()
        .map(|(sel, _)| format!("    '{sel}'"))
        .collect::<Vec<_>>()
        .join(",\n");
    format!(
        "%% AUTO-GENERATED by `beamtalk build-stdlib` from \
         crates/beamtalk-core/src/semantic_analysis/validators/supervision_validators.rs \
         — do not edit manually.\n\
         %% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         \n\
         %% Class-side supervisor configuration selectors removed by ADR 0135 §2\n\
         %% (configuration and `initialize` are instance-side). The compiler rejects\n\
         %% them; the runtime warns when an ADR 0084 override installs one.\n\
         \n\
         -ifndef(BEAMTALK_SUPERVISOR_CONFIG_HRL).\n\
         -define(BEAMTALK_SUPERVISOR_CONFIG_HRL, true).\n\
         \n\
         -define(BT_REMOVED_CLASS_SIDE_SUPERVISOR_CONFIG, [\n{selectors}\n]).\n\
         \n\
         -endif.\n"
    )
}

/// Reject class-side supervisor configuration (ADR 0135 §2).
///
/// Supervisor configuration (`childSpecs`, `strategy`, `maxRestarts`,
/// `restartWindow`; `childClass`, `maxChildren`, `childShutdown`) and the
/// `initialize` hook are instance methods, read from the supervisor being
/// started. A `Supervisor` or `DynamicSupervisor` subclass that still defines
/// one of the old class-side selectors (`class children`, `class strategy`,
/// …, `class initialize:`) gets an error with the fix, never a silent no-op.
///
/// Checks both inline class-side methods and standalone class-side method
/// definitions (`MySup class >> strategy => ...`). The stdlib base classes
/// themselves are exempt.
pub(crate) fn check_class_side_supervisor_config(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let is_supervisor_class = |name: &str| {
        name != "Supervisor"
            && name != "DynamicSupervisor"
            && (hierarchy.is_supervisor_subclass(name)
                || hierarchy.is_dynamic_supervisor_subclass(name))
    };
    let check_class_side = |method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>| {
        let selector = method.selector.name();
        if let Some((_, hint)) = REMOVED_CLASS_SIDE_SUPERVISOR_CONFIG
            .iter()
            .find(|(sel, _)| *sel == selector.as_str())
        {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "class-side `{selector}` is no longer supported on supervisors: \
                         configuration and `initialize` are instance-side (ADR 0135)"
                    ),
                    method.span,
                )
                .with_hint(*hint),
            );
        }
    };
    for class in &module.classes {
        if !is_supervisor_class(class.name.name.as_str()) {
            continue;
        }
        for method in &class.class_methods {
            check_class_side(method, diagnostics);
        }
        for method in &class.methods {
            check_instance_side_migration(method, diagnostics);
        }
    }
    for standalone in &module.method_definitions {
        if !is_supervisor_class(standalone.class_name.name.as_str()) {
            continue;
        }
        if standalone.is_class_method {
            check_class_side(&standalone.method, diagnostics);
        } else {
            check_instance_side_migration(&standalone.method, diagnostics);
        }
    }
}

/// Catch the two half-migrations of class-side configuration that would
/// otherwise compile silently (ADR 0135 §2): dropping `class` from
/// `class initialize: sup` gives an instance-side `initialize:` that is never
/// called, and dropping it from `class children` overrides the live-children
/// inspection method `children` instead of defining `childSpecs`.
fn check_instance_side_migration(method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>) {
    match method.selector.name().as_str() {
        "initialize:" => diagnostics.push(
            Diagnostic::error(
                "instance-side `initialize:` is never called on a supervisor: \
                 the post-start hook is unary `initialize` (ADR 0135)",
                method.span,
            )
            .with_hint("Rename it to `initialize =>` and use `self` for the supervisor argument"),
        ),
        "children" => diagnostics.push(
            Diagnostic::warning(
                "`children` overrides the supervisor's live-children inspection method; \
                 the child list is configured by `childSpecs` (ADR 0135)",
                method.span,
            )
            .with_hint(
                "Rename it to `childSpecs` (`class children` became instance-side `childSpecs`)",
            ),
        ),
        _ => {}
    }
}

/// Warn when an Actor subclass in a static `childSpecs` list/array literal
/// has no explicit `supervisionPolicy` override.
///
/// The default policy is `#temporary` (not restarted on crash). Developers often
/// want `#permanent` for long-lived actors. The warning nudges them to be explicit.
///
/// Checks both inline instance-side methods (`childSpecs => ...` in the class body)
/// and standalone instance-side method definitions (`MySup >> childSpecs => ...`).
/// Recognises both `#(Worker)` list literals and `#[Worker]` array literals.
pub(crate) fn check_children_supervision_policy(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        // Look for `childSpecs` methods in Supervisor subclasses
        if !hierarchy.is_supervisor_subclass(class_name) {
            continue;
        }
        // Check inline instance-side methods
        for method in &class.methods {
            if method.selector.name() != "childSpecs" {
                continue;
            }
            for stmt in &method.body {
                check_children_literal_for_policy(&stmt.expression, hierarchy, diagnostics);
            }
        }
        // Check standalone instance-side method definitions
        for standalone in &module.method_definitions {
            if standalone.is_class_method {
                continue;
            }
            if standalone.class_name.name.as_str() != class_name {
                continue;
            }
            if standalone.method.selector.name() != "childSpecs" {
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
            Expression::ClassReference { name, span, .. } => (name.name.as_str(), *span),
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
                )
                .with_category(DiagnosticCategory::Type),
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

    // ── supervisionPolicy override validation ──────────────────────────────────

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

    // ── children supervision policy warning ────────────────────────────────────

    /// Actor subclass in children array with no policy override → warning.
    #[test]
    fn children_actor_without_policy_warns() {
        let src = "Actor subclass: Worker\n  go => nil\nSupervisor subclass: MySup\n  childSpecs => #(Worker)";
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
        let src = "Actor subclass: Worker\n  class supervisionPolicy -> Symbol => #permanent\n  go => nil\nSupervisor subclass: MySup\n  childSpecs => #(Worker)";
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

    // ── Standalone class-side method definitions ───────────────────────────────

    /// Standalone class-side `supervisionPolicy` override (`MyActor class >> ...`)
    /// with a valid symbol is collected and validated just like the inline form.
    #[test]
    fn standalone_supervision_policy_valid_symbol_no_diagnostics() {
        let src = "Actor subclass: MyActor\n  go => nil\nMyActor class >> supervisionPolicy -> Symbol => #permanent";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for standalone #permanent, got: {diagnostics:?}"
        );
    }

    /// Standalone class-side `supervisionPolicy` override with an invalid symbol
    /// is collected and flagged (mirrors the inline-form error path).
    #[test]
    fn standalone_supervision_policy_invalid_symbol_is_error() {
        let src = "Actor subclass: MyActor\n  go => nil\nMyActor class >> supervisionPolicy -> Symbol => #restart";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for standalone #restart, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    /// A single-expression body that isn't a symbol literal (e.g. an integer)
    /// is skipped — the static check only applies to symbol-literal bodies.
    #[test]
    fn supervision_policy_non_symbol_single_expr_body_is_skipped() {
        let src = "Actor subclass: MyActor\n  class supervisionPolicy -> Symbol => 42\n  go => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_supervision_policy_override(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for non-symbol single-expr body, got: {diagnostics:?}"
        );
    }

    /// An inline instance-side method whose selector isn't `childSpecs` is
    /// skipped (the `continue` branch of the selector-name guard).
    #[test]
    fn non_children_class_method_is_skipped() {
        let src = "Supervisor subclass: MySup\n  other => 1";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_children_supervision_policy(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics when no `childSpecs` method is present, got: {diagnostics:?}"
        );
    }

    /// Standalone instance-side `childSpecs` method definition (`MySup >> ...`)
    /// is collected and validated just like the inline form. Also exercises
    /// the three `continue` guards in the standalone loop: a class-side
    /// standalone method (not instance-side), an instance-side standalone
    /// method on an unrelated class, and an instance-side standalone method
    /// with a different selector — none of which should contribute a diagnostic.
    #[test]
    fn standalone_children_method_actor_without_policy_warns() {
        let src = "Actor subclass: Worker\n  go => nil\nObject subclass: Other\n  go2 => nil\nSupervisor subclass: MySup\n  go3 => nil\nMySup class >> classHelper => nil\nOther >> childSpecs => #(Worker)\nMySup >> notChildSpecs => #(Worker)\nMySup >> childSpecs => #(Worker)";
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
            "Expected 1 warning for standalone children method, got: {diagnostics:?}"
        );
        assert!(
            diagnostics[0].message.contains("Worker"),
            "Expected 'Worker' in message, got: {}",
            diagnostics[0].message
        );
    }

    // ── check_children_literal_for_policy edge cases ────────────────────────────

    /// `childSpecs` returning a non-list/array expression is a no-op
    /// (the static check only handles list/array literals).
    #[test]
    fn children_non_literal_body_is_ignored() {
        let src = "Supervisor subclass: MySup\n  childSpecs => nil";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_children_supervision_policy(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for a non-list children body, got: {diagnostics:?}"
        );
    }

    /// Children list elements that aren't recognised (a non-actor identifier,
    /// a non-actor class reference, and a plain literal) are all skipped
    /// without warnings; only the real, policy-less Actor subclass warns.
    #[test]
    fn children_list_mixed_element_kinds() {
        let src = "Actor subclass: Worker\n  go => nil\nObject subclass: PlainClass\n  go => nil\nSupervisor subclass: MySup\n  childSpecs => #(1, worker, PlainClass, Worker)";
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
            "Expected exactly 1 warning (only the real Actor subclass), got: {diagnostics:?}"
        );
        assert!(
            diagnostics[0].message.contains("Worker"),
            "Expected 'Worker' in message, got: {}",
            diagnostics[0].message
        );
    }

    // ── removed class-side supervisor configuration (ADR 0135 §2) ──────────────

    fn class_side_config_diagnostics(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_class_side_supervisor_config(&module, &hierarchy, &mut diagnostics);
        diagnostics
    }

    /// Each removed class-side selector is a compile error with its fix-it,
    /// on a static supervisor, a dynamic supervisor, and as a standalone
    /// class-side definition.
    #[test]
    fn removed_class_side_config_selectors_are_errors() {
        let cases: &[(&str, &str, &str)] = &[
            (
                "class children => #()",
                "children",
                "define instance-side `childSpecs`",
            ),
            (
                "class initialize: sup => nil",
                "initialize:",
                "is now instance-side `initialize`; use `self` for `sup`",
            ),
            ("class childClass => Object", "childClass", "drop `class`"),
            ("class strategy => #oneForAll", "strategy", "drop `class`"),
            ("class maxRestarts => 3", "maxRestarts", "drop `class`"),
            ("class restartWindow => 30", "restartWindow", "drop `class`"),
            ("class maxChildren => 5", "maxChildren", "drop `class`"),
            (
                "class childShutdown => 1000",
                "childShutdown",
                "drop `class`",
            ),
        ];
        for (method, selector, hint) in cases {
            for header in [
                "Supervisor subclass: MySup",
                "DynamicSupervisor subclass: MySup",
            ] {
                let src = format!("{header}\n  {method}");
                let diagnostics = class_side_config_diagnostics(&src);
                assert_eq!(diagnostics.len(), 1, "{src}: {diagnostics:?}");
                assert_eq!(diagnostics[0].severity, Severity::Error, "{src}");
                assert!(
                    diagnostics[0].message.contains(&format!("`{selector}`")),
                    "{src}: {}",
                    diagnostics[0].message
                );
                let got_hint = diagnostics[0].hint.as_deref().unwrap_or_default();
                assert!(got_hint.contains(hint), "{src}: hint was {got_hint}");
            }
            let standalone = format!(
                "Supervisor subclass: MySup\n  childSpecs => #()\nMySup {}",
                method.replacen("class ", "class >> ", 1)
            );
            let diagnostics = class_side_config_diagnostics(&standalone);
            assert_eq!(diagnostics.len(), 1, "{standalone}: {diagnostics:?}");
            assert_eq!(diagnostics[0].severity, Severity::Error, "{standalone}");
        }
    }

    /// Half-migrated instance-side forms are caught: `initialize:` (never
    /// called) is an error and `children` (shadows live inspection) a warning.
    #[test]
    fn half_migrated_instance_side_forms_are_flagged() {
        let diagnostics = class_side_config_diagnostics(
            "Supervisor subclass: MySup\n  initialize: sup => nil\n  children => #()",
        );
        assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("`initialize:`"));
        assert_eq!(diagnostics[1].severity, Severity::Warning);
        assert!(
            diagnostics[1]
                .hint
                .as_deref()
                .unwrap_or_default()
                .contains("childSpecs")
        );
        let standalone = class_side_config_diagnostics(
            "Supervisor subclass: MySup\n  childSpecs => #()\nMySup >> initialize: sup => nil",
        );
        assert_eq!(standalone.len(), 1, "{standalone:?}");
    }

    /// The generated Erlang header lists exactly the selectors the compiler
    /// rejects, so the runtime's override warning cannot drift from them.
    #[test]
    fn removed_config_header_lists_every_rejected_selector() {
        let header = removed_supervisor_config_erlang_header();
        for (selector, _) in REMOVED_CLASS_SIDE_SUPERVISOR_CONFIG {
            assert!(header.contains(&format!("    '{selector}'")), "{header}");
        }
        assert_eq!(
            header.matches("    '").count(),
            REMOVED_CLASS_SIDE_SUPERVISOR_CONFIG.len(),
            "{header}"
        );
    }

    /// `isAlive` and `stop` are sealed on Supervisor and DynamicSupervisor
    /// (BT-3984): the runtime answers them on a supervisor handle without a
    /// method lookup, so an override would never run and is rejected instead.
    #[test]
    fn supervisor_is_alive_and_stop_overrides_are_errors() {
        for superclass in ["Supervisor", "DynamicSupervisor"] {
            for selector in ["isAlive", "stop"] {
                let src = format!("{superclass} subclass: MySup\n  {selector} => nil");
                let tokens = lex_with_eof(&src);
                let (module, parse_diags) = parse(tokens);
                assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
                let (_, diagnostics) = ClassHierarchy::build(&module);
                assert_eq!(diagnostics.len(), 1, "{src}: {diagnostics:?}");
                let diag = &diagnostics[0];
                assert_eq!(diag.severity, Severity::Error, "{src}");
                assert_eq!(diag.category, Some(DiagnosticCategory::Inheritance));
                assert_eq!(
                    diag.message,
                    format!("Cannot override sealed method `{selector}` from class `{superclass}`"),
                    "{src}"
                );
            }
        }
    }

    /// Instance-side configuration, `supervisionPolicy`, other class methods,
    /// and the same class-side selectors on non-supervisor classes are fine.
    #[test]
    fn instance_side_config_and_non_supervisors_are_not_flagged() {
        let src = "Supervisor subclass: MySup\n  strategy => #oneForAll\n  childSpecs => #()\n  initialize => nil\n  class supervisionPolicy => #permanent\n  class helper => 1\nObject subclass: Plain\n  class strategy => #oneForAll\n  class children => #()\nActor subclass: Act\n  class initialize: x => nil";
        let diagnostics = class_side_config_diagnostics(src);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }
}

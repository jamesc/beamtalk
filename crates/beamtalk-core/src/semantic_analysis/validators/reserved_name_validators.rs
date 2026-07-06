// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Reserved internal-namespace name validation (BT-2718).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Beamtalk's codegen reserves a `__`-prefixed map-key namespace inside
//! actor/object state: `__methods__` and `__class_mod__` (dispatch metadata),
//! and `__local__<name>` control-flow threading temporaries (BT-2717). The
//! runtime's `beamtalk_actor:strip_local_temps/1` deletes every
//! `__local__`-prefixed key from committed state on each dispatch, and
//! `changed_state_keys/2` excludes the same prefix from watch notifications.
//!
//! Nothing at the language level guards this namespace — the lexer admits
//! leading-underscore identifiers. A user declaration whose name lands in it
//! (e.g. `state: __local__balance :: Integer = 0`) would be stored under the
//! bare atom `'__local__balance'` and then **silently stripped from persisted
//! state on every commit** — silent data loss. This validator rejects such
//! names at compile time so the collision is impossible by construction.

use crate::ast::{Expression, Identifier, Module};
use crate::ast_walker::walk_module;
use crate::semantic_analysis::extract_pattern_bindings;
use crate::source_analysis::{Diagnostic, Span};

/// The reserved-namespace prefix. Codegen-internal state keys
/// (`__local__<name>`, `__methods__`, `__class_mod__`) all begin with a double
/// underscore; reserving the whole prefix — rather than an explicit blocklist —
/// keeps future internal keys safe by construction.
const RESERVED_PREFIX: &str = "__";

/// Returns `true` if `name` falls in the compiler's reserved internal
/// state-key namespace.
///
/// A single leading underscore (`_unused`) is the conventional "intentionally
/// ignored" marker and stays allowed — only the double-underscore prefix is
/// reserved.
fn is_reserved_internal_name(name: &str) -> bool {
    name.starts_with(RESERVED_PREFIX)
}

/// BT-2718: Reject user-chosen names that collide with the compiler's reserved
/// internal state-key namespace (the `__` prefix).
///
/// Covers every site where a user names a binding that could be packed into
/// actor/object state or a control-flow-threaded local: instance fields, class
/// variables, method parameters, block parameters, and local `:=` assignment
/// targets (including destructuring binds).
pub(crate) fn check_reserved_internal_names(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        // Instance fields (`state:` / `field:`).
        for field in &class.state {
            check_declared_name(&field.name, "field", diagnostics);
        }
        // Class variables (`classState:`).
        for var in &class.class_variables {
            check_declared_name(&var.name, "class variable", diagnostics);
        }
        // Method parameters (instance- and class-side).
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for param in &method.parameters {
                check_declared_name(&param.name, "parameter", diagnostics);
            }
        }
    }

    // Standalone (Tonel-style) method parameters: `MyClass >> foo: __x => …`.
    for standalone in &module.method_definitions {
        for param in &standalone.method.parameters {
            check_declared_name(&param.name, "parameter", diagnostics);
        }
    }

    // Block parameters and local `:=` assignment targets, everywhere a body
    // appears (module-level expressions, class methods, standalone methods).
    walk_module(module, &mut |expr| match expr {
        Expression::Block(block) => {
            for param in &block.parameters {
                check_raw_name(
                    param.name.as_str(),
                    param.span,
                    "block parameter",
                    diagnostics,
                );
            }
        }
        Expression::Assignment { target, .. } => {
            if let Expression::Identifier(id) = target.as_ref() {
                check_declared_name(id, "local variable", diagnostics);
            }
        }
        Expression::DestructureAssignment { pattern, .. } => {
            let (bindings, _) = extract_pattern_bindings(pattern);
            for id in &bindings {
                check_declared_name(id, "local variable", diagnostics);
            }
        }
        _ => {}
    });
}

/// Emits a diagnostic if `id`'s name is in the reserved namespace.
fn check_declared_name(id: &Identifier, kind: &str, diagnostics: &mut Vec<Diagnostic>) {
    check_raw_name(id.name.as_str(), id.span, kind, diagnostics);
}

/// Emits a diagnostic if `name` is in the reserved namespace, anchored at `span`.
fn check_raw_name(name: &str, span: Span, kind: &str, diagnostics: &mut Vec<Diagnostic>) {
    if is_reserved_internal_name(name) {
        // The hazard depends on where the name appears. Fields and class
        // variables land as keys in the actor/object state map; parameters,
        // block parameters, and locals become codegen-generated variables.
        // Emit only the paragraph relevant to this declaration kind.
        let hint = match kind {
            "field" | "class variable" => {
                "Rename it. A field or class variable beginning with `__` (double \
                 underscore) becomes a reserved state-map key: `__local__`-prefixed keys \
                 are silently stripped from persisted state on every dispatch commit, and \
                 a `__methods__` or `__class_mod__` collision would overwrite the \
                 runtime's dispatch metadata and corrupt method lookup."
            }
            "parameter" | "block parameter" | "local variable" => {
                "Rename it. A parameter, block parameter, or local beginning with `__` \
                 (double underscore) collides with the names the compiler generates for \
                 control-flow temporaries."
            }
            // Defensive fallback for any future declaration kind: describe both
            // hazards rather than silently mis-routing to one of the specific
            // paragraphs above.
            _ => {
                "Rename it. Identifiers beginning with `__` (double underscore) are \
                 reserved for the compiler — they collide with reserved state-map keys \
                 or with generated control-flow temporary names."
            }
        };
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "{kind} `{name}` uses the reserved `__` prefix, which the compiler \
                     uses internally for state keys and control-flow temporaries"
                ),
                span,
            )
            .with_hint(hint.to_string()),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_support::parse_bt;

    fn diagnostics_for(source: &str) -> Vec<Diagnostic> {
        let module = parse_bt(source);
        let mut diagnostics = Vec::new();
        check_reserved_internal_names(&module, &mut diagnostics);
        diagnostics
    }

    #[test]
    fn reserved_prefix_helper() {
        assert!(is_reserved_internal_name("__local__balance"));
        assert!(is_reserved_internal_name("__methods__"));
        assert!(is_reserved_internal_name("__class_mod__"));
        // Single underscore is the "ignore me" convention — allowed.
        assert!(!is_reserved_internal_name("_unused"));
        assert!(!is_reserved_internal_name("balance"));
    }

    #[test]
    fn rejects_reserved_field_name() {
        let diagnostics =
            diagnostics_for("Actor subclass: Counter\n  state: __local__balance :: Integer = 0\n");
        assert_eq!(
            diagnostics.len(),
            1,
            "expected one diagnostic: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("__local__balance"));
        assert!(diagnostics[0].message.contains("field"));
    }

    #[test]
    fn rejects_reserved_class_variable_name() {
        let diagnostics =
            diagnostics_for("Object subclass: Config\n  classState: __methods__ = 0\n");
        assert_eq!(
            diagnostics.len(),
            1,
            "expected one diagnostic: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("__methods__"));
        assert!(diagnostics[0].message.contains("class variable"));
    }

    #[test]
    fn rejects_reserved_parameter_name() {
        let diagnostics =
            diagnostics_for("Object subclass: Widget\n  set: __class_mod__ => ^__class_mod__\n");
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("parameter") && d.message.contains("__class_mod__")),
            "expected a parameter diagnostic: {diagnostics:?}"
        );
    }

    #[test]
    fn rejects_reserved_local_variable_target() {
        let diagnostics = diagnostics_for(
            "Object subclass: Widget\n  run => \n    __local__tmp := 1\n    ^__local__tmp\n",
        );
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("local variable") && d.message.contains("__local__tmp")),
            "expected a local-variable diagnostic: {diagnostics:?}"
        );
    }

    #[test]
    fn rejects_reserved_block_parameter() {
        let diagnostics = diagnostics_for(
            "Object subclass: Widget\n  run => [:__methods__ | __methods__ + 1] value: 1\n",
        );
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("block parameter") && d.message.contains("__methods__")),
            "expected a block-parameter diagnostic: {diagnostics:?}"
        );
    }

    #[test]
    fn allows_single_underscore_and_ordinary_names() {
        let diagnostics = diagnostics_for(
            "Actor subclass: Counter\n  state: balance :: Integer = 0\n  \
             add: _ignored => \n    total := balance\n    ^total\n",
        );
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {diagnostics:?}"
        );
    }

    #[test]
    fn field_hint_describes_state_map_path() {
        let diagnostics =
            diagnostics_for("Actor subclass: Counter\n  state: __local__balance :: Integer = 0\n");
        let hint = diagnostics[0]
            .hint
            .as_ref()
            .expect("field diagnostic should carry a hint");
        assert!(
            hint.contains("state-map key"),
            "field hint should describe the state-map path: {hint}"
        );
        assert!(
            !hint.contains("control-flow temporaries"),
            "field hint should not describe the control-flow path: {hint}"
        );
    }

    #[test]
    fn parameter_hint_describes_control_flow_path() {
        let diagnostics =
            diagnostics_for("Object subclass: Widget\n  set: __class_mod__ => ^__class_mod__\n");
        let param = diagnostics
            .iter()
            .find(|d| d.message.contains("parameter"))
            .expect("expected a parameter diagnostic");
        let hint = param
            .hint
            .as_ref()
            .expect("parameter diagnostic should carry a hint");
        assert!(
            hint.contains("control-flow temporaries"),
            "parameter hint should describe the control-flow path: {hint}"
        );
        assert!(
            !hint.contains("state-map key"),
            "parameter hint should not describe the state-map path: {hint}"
        );
    }
}

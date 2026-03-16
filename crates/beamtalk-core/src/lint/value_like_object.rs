// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when an `Object subclass:` declares state fields and only has
//! getter/setter methods, suggesting it should be a `Value subclass:` instead.
//!
//! **DDD Context:** Compilation
//!
//! ```text
//! // Bad — Object subclass with only state + getters/setters
//! Object subclass: Point
//!   state: x = 0
//!   state: y = 0
//!   x => self.x
//!   y => self.y
//!   withX: aValue => self.x := aValue. self
//!   withY: aValue => self.y := aValue. self
//!
//! // Good — use Value subclass, which auto-generates these
//! Value subclass: Point
//!   state: x = 0
//!   state: y = 0
//! ```

use std::collections::HashSet;

use crate::ast::{ClassDefinition, ClassKind, Expression, Identifier, MethodDefinition, Module};
use crate::lint::LintPass;
use crate::source_analysis::Diagnostic;

/// Lint pass that flags `Object subclass:` classes that look like value types.
///
/// A class is flagged when:
/// - It is an `Object` subclass (not Actor or Value)
/// - It declares at least one `state:` field
/// - It has at least one instance method
/// - All instance methods are either getters (`self.field`) or `withX:` setters
///   (`self.field := arg. self`)
pub(crate) struct ValueLikeObjectPass;

impl LintPass for ValueLikeObjectPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for class in &module.classes {
            check_class(class, diagnostics);
        }
    }
}

fn check_class(class: &ClassDefinition, diagnostics: &mut Vec<Diagnostic>) {
    // Only check Object subclasses with state fields
    if class.class_kind != ClassKind::Object || class.state.is_empty() {
        return;
    }

    // Must have at least one instance method to be interesting
    if class.methods.is_empty() {
        return;
    }

    let field_names: HashSet<&str> = class.state.iter().map(|s| s.name.name.as_str()).collect();

    // Check that every instance method is a getter or withX: setter
    for method in &class.methods {
        if !is_getter(method, &field_names) && !is_with_setter(method, &field_names) {
            return;
        }
    }

    let class_name = &class.name.name;
    diagnostics.push(
        Diagnostic::lint(
            format!(
                "class `{class_name}` only has state fields and getters/setters — \
                 consider using `Value subclass:` instead of `Object subclass:`"
            ),
            class.span,
        )
        .with_hint(
            "`Value subclass:` auto-generates getters, `withX:` setters, \
             `initWith:` constructors, and value equality",
        ),
    );
}

/// Returns `true` if `method` is a unary getter that returns `self.field`
/// where `field` is one of the class's state fields.
fn is_getter(method: &MethodDefinition, field_names: &HashSet<&str>) -> bool {
    // Must be a unary method (no parameters)
    if !method.parameters.is_empty() || method.body.len() != 1 {
        return false;
    }

    // Body must be a single `self.field` expression
    is_self_field_access(&method.body[0].expression, field_names)
}

/// Returns `true` if `method` is a `withField:` setter whose body is
/// `self.field := param. self`.
fn is_with_setter(method: &MethodDefinition, field_names: &HashSet<&str>) -> bool {
    // Must be a keyword method with exactly one parameter and two statements
    if method.parameters.len() != 1 || method.body.len() != 2 {
        return false;
    }

    // Selector must match the `withField:` naming convention (e.g. `withX:`, `withName:`)
    let selector_name = method.selector.name();
    if !selector_name.starts_with("with") {
        return false;
    }
    // The character after "with" must be uppercase (e.g. `withX:`, not `withdraw:`)
    let after_with = &selector_name[4..];
    if after_with.is_empty() || !after_with.starts_with(|c: char| c.is_ascii_uppercase()) {
        return false;
    }

    let param_name = method.parameters[0].name.name.as_str();

    // First statement: self.field := param
    let is_field_assign = matches!(
        &method.body[0].expression,
        Expression::Assignment { target, value, .. }
            if is_self_field_access(target, field_names)
            && matches!(value.as_ref(), Expression::Identifier(Identifier { name, .. }) if name.as_str() == param_name)
    );

    if !is_field_assign {
        return false;
    }

    // Second statement: self
    matches!(
        &method.body[1].expression,
        Expression::Identifier(Identifier { name, .. }) if name.as_str() == "self"
    )
}

/// Returns `true` if `expr` is `self.field` where `field` is in `field_names`.
fn is_self_field_access(expr: &Expression, field_names: &HashSet<&str>) -> bool {
    matches!(
        expr,
        Expression::FieldAccess { receiver, field, .. }
            if matches!(receiver.as_ref(), Expression::Identifier(Identifier { name, .. }) if name.as_str() == "self")
            && field_names.contains(field.name.as_str())
    )
}

#[cfg(test)]
mod tests {
    use crate::lint::run_lint_passes;
    use crate::source_analysis::{DiagnosticCategory, Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        run_lint_passes(&module)
    }

    fn value_like_lints(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
        lint(source)
            .into_iter()
            .filter(|d| d.message.contains("Value subclass:"))
            .collect()
    }

    // ── True positives ──────────────────────────────────────────────────

    #[test]
    fn object_with_state_and_getters_flagged() {
        let diags = value_like_lints(
            "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  x => self.x\n  y => self.y\n",
        );
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(diags[0].message.contains("Point"));
    }

    #[test]
    fn object_with_getters_and_with_setters_flagged() {
        let diags = value_like_lints(
            "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  x => self.x\n  y => self.y\n  withX: v => self.x := v. self\n  withY: v => self.y := v. self\n",
        );
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
    }

    #[test]
    fn lint_has_hint_and_category() {
        let diags = value_like_lints("Object subclass: Foo\n  state: x = 0\n  x => self.x\n");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::Lint));
        assert!(diags[0].hint.is_some(), "expected a hint");
        assert!(
            diags[0].hint.as_ref().unwrap().contains("auto-generates"),
            "hint: {:?}",
            diags[0].hint
        );
    }

    // ── True negatives ──────────────────────────────────────────────────

    #[test]
    fn object_with_real_method_not_flagged() {
        let diags = value_like_lints(
            "Object subclass: Calc\n  state: x = 0\n  x => self.x\n  double => self.x * 2\n",
        );
        assert!(
            diags.is_empty(),
            "class with non-getter method should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn actor_subclass_not_flagged() {
        let diags = value_like_lints(
            "Actor subclass: Counter\n  state: count = 0\n  count => self.count\n",
        );
        assert!(
            diags.is_empty(),
            "Actor subclass should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn value_subclass_not_flagged() {
        let diags = value_like_lints("Value subclass: Point\n  state: x = 0\n  state: y = 0\n");
        assert!(
            diags.is_empty(),
            "Value subclass should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn object_without_state_not_flagged() {
        let diags = value_like_lints("Object subclass: Utils\n  helper => 42\n");
        assert!(
            diags.is_empty(),
            "Object without state should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn object_with_state_but_no_methods_not_flagged() {
        let diags = value_like_lints("Object subclass: Empty\n  state: x = 0\n");
        assert!(
            diags.is_empty(),
            "Object with state but no methods should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn object_with_message_send_in_method_not_flagged() {
        // Method that does a message send (side effect) should not be flagged
        let diags = value_like_lints(
            "Object subclass: Logger\n  state: name = \"\"\n  name => self.name\n  log => Transcript show: self.name\n",
        );
        assert!(
            diags.is_empty(),
            "Object with side-effect method should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn non_with_setter_not_flagged() {
        // A method like `reset: v => self.x := v. self` has setter-like body
        // but doesn't follow the `withX:` naming convention
        let diags = value_like_lints(
            "Object subclass: Foo\n  state: x = 0\n  x => self.x\n  reset: v => self.x := v. self\n",
        );
        assert!(
            diags.is_empty(),
            "non-withX: setter should not be treated as a value setter, got: {diags:?}"
        );
    }

    #[test]
    fn object_with_setter_that_does_more_not_flagged() {
        // A withX: setter that does extra work beyond `self.x := arg. self`
        let diags = value_like_lints(
            "Object subclass: Foo\n  state: x = 0\n  x => self.x\n  withX: v => self.x := v. self.x\n",
        );
        assert!(
            diags.is_empty(),
            "setter returning self.x instead of self should not match, got: {diags:?}"
        );
    }
}

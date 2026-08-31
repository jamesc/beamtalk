// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive pragma validation for Beamtalk.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates `@primitive` usage according to ADR 0007:
//! - Restricts `@primitive` to standard library code by default
//! - Validates structural intrinsic names against the known registry
//! - Supports `--allow-primitives` escape hatch for advanced FFI use

use crate::CompilerOptions;
use crate::ast::{Expression, Module};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

/// Known structural intrinsic names (ADR 0007).
///
/// These are the unquoted intrinsic names that require custom code generation.
/// Quoted selectors (e.g., `@primitive \"+\"`) are always valid — they delegate
/// to runtime dispatch modules.
const STRUCTURAL_INTRINSICS: &[&str] = &[
    // Object lifecycle
    "basicNew",
    "basicNewWith",
    // BT-3072: `actorSpawn` / `actorSpawnWith` retired — `class sealed spawn`
    // / `spawnWith:` now have real FFI bodies (`(Erlang beamtalk_actor)
    // doSpawn: self`) instead of `@intrinsic` markers, so these names are no
    // longer used anywhere and are intentionally not in this registry.
    // BT-3074: `actorNewError` / `actorNewWithArgsError` retired the same
    // way — `class sealed new` / `new:` now send `Exception
    // signalKind:class:selector:hint:` instead of an `@intrinsic` marker.
    // Reflection
    "classOf",
    "doesNotUnderstand",
    "dynamicSend",
    "dynamicSendWithArgs",
    "respondsTo",
    "fieldNames",
    "fieldAt",
    "fieldAtPut",
    // Object protocol
    "printString",
    "hash",
    // Control flow
    "conditional",
    "conditionalTrue",
    "conditionalFalse",
    "shortCircuitAnd",
    "shortCircuitOr",
    "booleanNot",
    "blockValue",
    "blockValue1",
    "blockValue2",
    "blockValue3",
    "blockValueWithArguments",
    "whileTrue",
    "whileFalse",
    "repeat",
    // Iteration
    // Note: timesRepeat/toDo/toByDo are no longer intrinsics (BT-1054)
    "listDo",
    "listCollect",
    "listSelect",
    "listReject",
    "listInjectInto",
    // Async
    "futureAwait",
    "futureAwaitTimeout",
    "futureAwaitForever",
    // Exception handling
    "onDo",
    "ensure",
    // Error signaling
    "error",
    // BEAM interop (ADR 0028)
    "erlangModuleLookup",
    "erlangApply",
    // ClassBuilder (ADR 0038)
    "classBuilderRegister",
];

/// Validates all `@primitive` usages in a module.
///
/// Returns diagnostics for invalid primitive usage:
/// - Error if `@primitive` appears outside stdlib (unless `--allow-primitives`)
/// - Warning if `@primitive` used with `--allow-primitives` outside stdlib
/// - Error for unknown structural intrinsic names
pub fn validate_primitives(module: &Module, options: &CompilerOptions) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let is_stdlib = is_stdlib_module(options);

    // Check top-level expressions
    for stmt in &module.expressions {
        validate_expr(&stmt.expression, is_stdlib, options, &mut diagnostics);
    }

    // Check class methods and state default values
    for class in &module.classes {
        for state in &class.state {
            if let Some(ref default_expr) = state.default_value {
                validate_expr(default_expr, is_stdlib, options, &mut diagnostics);
            }
        }
        for method in &class.methods {
            for stmt in &method.body {
                validate_expr(&stmt.expression, is_stdlib, options, &mut diagnostics);
            }
        }
    }

    diagnostics
}

/// Determines if the current module is part of the standard library.
fn is_stdlib_module(options: &CompilerOptions) -> bool {
    options.stdlib_mode
}

/// Recursively validates an expression for primitive usage.
fn validate_expr(
    expr: &Expression,
    is_stdlib: bool,
    options: &CompilerOptions,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::Primitive {
            name,
            is_quoted,
            span,
            ..
        } => {
            // Check stdlib restriction
            validate_stdlib_restriction(is_stdlib, options, *span, diagnostics);

            // Check intrinsic name validity (only for unquoted/structural intrinsics)
            if !is_quoted {
                validate_intrinsic_name(name, *span, diagnostics);
            }
        }

        // Recurse into subexpressions
        Expression::Assignment { target, value, .. } => {
            validate_expr(target, is_stdlib, options, diagnostics);
            validate_expr(value, is_stdlib, options, diagnostics);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            validate_expr(receiver, is_stdlib, options, diagnostics);
            for arg in arguments {
                validate_expr(arg, is_stdlib, options, diagnostics);
            }
        }
        Expression::Block(block) => {
            for body_stmt in &block.body {
                validate_expr(&body_stmt.expression, is_stdlib, options, diagnostics);
            }
        }
        Expression::Return { value, .. }
        | Expression::Parenthesized {
            expression: value, ..
        }
        | Expression::DestructureAssignment { value, .. } => {
            validate_expr(value, is_stdlib, options, diagnostics);
        }
        Expression::FieldAccess { receiver, .. } => {
            validate_expr(receiver, is_stdlib, options, diagnostics);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            validate_expr(receiver, is_stdlib, options, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    validate_expr(arg, is_stdlib, options, diagnostics);
                }
            }
        }
        Expression::Match { value, arms, .. } => {
            validate_expr(value, is_stdlib, options, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    validate_expr(guard, is_stdlib, options, diagnostics);
                }
                validate_expr(&arm.body, is_stdlib, options, diagnostics);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                validate_expr(&pair.key, is_stdlib, options, diagnostics);
                validate_expr(&pair.value, is_stdlib, options, diagnostics);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                validate_expr(elem, is_stdlib, options, diagnostics);
            }
            if let Some(t) = tail {
                validate_expr(t, is_stdlib, options, diagnostics);
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                validate_expr(elem, is_stdlib, options, diagnostics);
            }
        }

        // Leaf expressions — no primitives to validate
        Expression::Literal(..)
        | Expression::Identifier(_)
        | Expression::ClassReference { .. }
        | Expression::Super(_)
        | Expression::Error { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. } => {}

        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(expr) = segment {
                    validate_expr(expr, is_stdlib, options, diagnostics);
                }
            }
        }
    }
}

/// Validates that `@primitive` is allowed in the current compilation context.
fn validate_stdlib_restriction(
    is_stdlib: bool,
    options: &CompilerOptions,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if is_stdlib {
        return;
    }

    if options.allow_primitives {
        // Escape hatch: warning instead of error
        diagnostics.push(
            Diagnostic::warning(
                "Using primitives outside stdlib — ensure you understand safety implications",
                span,
            )
            .with_hint("Primitives bypass normal dispatch — prefer Erlang FFI unless implementing a core stdlib binding")
            .with_category(DiagnosticCategory::Type),
        );
    } else {
        // Default: hard error
        diagnostics.push(
            Diagnostic::error(
                "Primitives can only be declared in the standard library",
                span,
            )
            .with_hint("Use --allow-primitives flag only if implementing FFI bindings"),
        );
    }
}

/// Validates that an unquoted intrinsic name is in the structural intrinsic registry.
fn validate_intrinsic_name(name: &str, span: Span, diagnostics: &mut Vec<Diagnostic>) {
    if !STRUCTURAL_INTRINSICS.contains(&name) {
        let known = STRUCTURAL_INTRINSICS.join(", ");
        diagnostics.push(
            Diagnostic::error(format!("Unknown intrinsic '{name}'"), span)
                .with_hint(format!("Known intrinsics: {known}")),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};
    use crate::test_helpers::test_support::parse_bt;

    /// Wraps a `@primitive` expression in a class method body for parser acceptance.
    fn stdlib_method(primitive: &str) -> String {
        format!("Object subclass: T\n  m => {primitive}")
    }

    #[test]
    fn primitive_in_stdlib_mode_no_error() {
        let module = parse_bt(&stdlib_method("@primitive \"+\""));
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(
            diags.is_empty(),
            "Expected no diagnostics in stdlib mode, got: {diags:?}"
        );
    }

    #[test]
    fn primitive_in_user_code_error() {
        let module = parse_bt(&stdlib_method("@primitive \"+\""));
        let options = CompilerOptions::default();
        let diags = validate_primitives(&module, &options);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Primitives can only be declared"));
        assert!(
            diags[0]
                .hint
                .as_ref()
                .unwrap()
                .contains("--allow-primitives")
        );
    }

    #[test]
    fn primitive_with_allow_primitives_warning() {
        let module = parse_bt(&stdlib_method("@primitive \"+\""));
        let options = CompilerOptions {
            allow_primitives: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Using primitives outside stdlib"));
        assert_eq!(diags[0].severity, crate::source_analysis::Severity::Warning);
    }

    #[test]
    fn unknown_structural_intrinsic_error() {
        let module = parse_bt(&stdlib_method("@primitive unknownFoo"));
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Unknown intrinsic 'unknownFoo'"));
        assert!(diags[0].hint.as_ref().unwrap().contains("basicNew"));
    }

    #[test]
    fn known_structural_intrinsic_no_error() {
        let module = parse_bt(&stdlib_method("@primitive basicNew"));
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(
            diags.is_empty(),
            "Expected no diagnostics for known intrinsic, got: {diags:?}"
        );
    }

    #[test]
    fn quoted_selector_always_accepted() {
        // Quoted selectors are runtime-dispatch, no intrinsic name validation
        let module = parse_bt(&stdlib_method("@primitive \"anyRandomName\""));
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(diags.is_empty());
    }

    #[test]
    fn primitive_in_class_method_validated() {
        let source = "Object subclass: MyInt\n  + other => @primitive \"+\"";
        let module = parse_bt(source);
        let options = CompilerOptions::default();
        let diags = validate_primitives(&module, &options);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Primitives can only be declared"));
    }

    #[test]
    fn primitive_in_class_method_stdlib_ok() {
        let source = "Object subclass: MyInt\n  + other => @primitive \"+\"";
        let module = parse_bt(source);
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(diags.is_empty());
    }

    #[test]
    fn multiple_primitives_multiple_errors() {
        let source = "Object subclass: T\n  m => @primitive \"+\". @primitive unknownFoo";
        let module = parse_bt(source);
        let options = CompilerOptions::default();
        let diags = validate_primitives(&module, &options);
        // At least 2 errors: one for stdlib restriction on '+', one for stdlib + unknown on unknownFoo
        assert!(
            diags.len() >= 2,
            "Expected multiple diagnostics, got: {diags:?}"
        );
    }

    // BT-3347: the tests below exercise `validate_expr`'s recursive match arms —
    // previously only the top-level "primitive as the entire statement" shape
    // was tested, so every arm that *recurses into a subexpression* (rather
    // than being the primitive itself) was untested. Each `@primitive "+"`
    // here uses a quoted selector, so it always contributes exactly one
    // "Primitives can only be declared" diagnostic (no intrinsic-name check).

    #[test]
    fn primitive_in_assignment_value_validated() {
        let source = "Object subclass: T\n  m =>\n    x := @primitive \"+\"";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert!(diags[0].message.contains("Primitives can only be declared"));
    }

    #[test]
    fn primitive_in_message_send_receiver_and_argument_validated() {
        let source = "Object subclass: T\n  m => @primitive \"+\" foo: @primitive \"+\"";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(
            diags.len(),
            2,
            "expected one diagnostic each for the receiver and the argument, got: {diags:?}"
        );
    }

    #[test]
    fn primitive_in_block_body_validated() {
        // Assignment{value: Block} — exercises both the Assignment and Block arms.
        let source = "Object subclass: T\n  m =>\n    blk := [@primitive \"+\"]";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_return_value_validated() {
        let source = "Object subclass: T\n  m =>\n    ^@primitive \"+\".\n    99";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_parenthesized_expr_validated() {
        let source = "Object subclass: T\n  m => (@primitive \"+\")";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_destructure_assignment_value_validated() {
        let source = "Object subclass: T\n  m =>\n    #(a, b) := @primitive \"+\"";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn field_access_expression_does_not_interfere_with_primitive_validation() {
        // Also exercises the `state.default_value` iteration in `validate_primitives`
        // with an actual (non-primitive) default, which no prior test reached.
        let source =
            "Object subclass: T\n  state: x = 0\n  m =>\n    self.x.\n    @primitive \"+\"";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(
            diags.len(),
            1,
            "field access itself should not produce a diagnostic, got: {diags:?}"
        );
    }

    #[test]
    fn primitive_in_cascade_message_argument_validated() {
        let source = "Object subclass: T\n  m => 1 foo: 2; bar: @primitive \"+\"";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_match_value_guard_and_arm_body_validated() {
        // Covers both the `Some(guard)` and (via the second, guard-less arm) the
        // implicit "no guard" path, plus the arm-body recursion.
        let source = "Object subclass: T\n  m => x match: [n when: [@primitive \"+\"] -> @primitive \"+\"; _ -> 0]";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 2, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_map_literal_key_and_value_validated() {
        let source = "Object subclass: T\n  m => #{@primitive \"+\" => @primitive \"+\"}";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 2, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_list_literal_elements_and_cons_tail_validated() {
        let source = "Object subclass: T\n  m =>\n    #(@primitive \"+\" | @primitive \"+\").\n    #(@primitive \"+\")";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 3, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_array_literal_elements_validated() {
        let source = "Object subclass: T\n  m => #[@primitive \"+\"]";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_string_interpolation_segment_validated() {
        let source = "Object subclass: T\n  m => \"a{@primitive \"+\"}a\"";
        let module = parse_bt(source);
        let diags = validate_primitives(&module, &CompilerOptions::default());
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn primitive_in_state_default_validated() {
        // @primitive in a state default value is caught by the parser (not in method body),
        // so no Expression::Primitive reaches semantic analysis — verify no false positives
        let source = "Object subclass: MyObj\n  state: x = @primitive \"bad\"";
        let tokens = lex_with_eof(source);
        let (module, parser_diags) = parse(tokens);
        // Parser should catch this
        assert!(
            !parser_diags.is_empty(),
            "Expected parser error for @primitive in state default"
        );
        assert!(
            parser_diags[0]
                .message
                .contains("@primitive can only appear inside a method body")
        );
        // Semantic validator sees no primitives
        let options = CompilerOptions::default();
        let diags = validate_primitives(&module, &options);
        assert!(
            diags.is_empty(),
            "No semantic diagnostics expected (parser already caught it)"
        );
    }
}

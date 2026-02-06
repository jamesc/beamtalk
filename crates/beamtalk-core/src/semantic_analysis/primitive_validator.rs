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

use crate::ast::{Expression, Module};
use crate::source_analysis::{Diagnostic, Span};
use crate::CompilerOptions;

/// Known structural intrinsic names (ADR 0007).
///
/// These are the unquoted intrinsic names that require custom code generation.
/// Quoted selectors (e.g., `@primitive '+'`) are always valid — they delegate
/// to runtime dispatch modules.
const STRUCTURAL_INTRINSICS: &[&str] = &[
    // Object lifecycle
    "basicNew",
    "basicNewWith",
    "actorSpawn",
    "actorSpawnWith",
    // Reflection
    "classOf",
    "doesNotUnderstand",
    "dynamicSend",
    "respondsTo",
    "instVarNames",
    "instVarAt",
    "instVarAtPut",
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
    "whileTrue",
    "whileFalse",
    "repeat",
    // Iteration
    "timesRepeat",
    "toDo",
    "toByDo",
    "listDo",
    "listCollect",
    "listSelect",
    "listReject",
    "listInjectInto",
    // Async
    "futureAwait",
    "futureAwaitTimeout",
    "futureAwaitForever",
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
    for expr in &module.expressions {
        validate_expr(expr, is_stdlib, options, &mut diagnostics);
    }

    // Check class methods
    for class in &module.classes {
        for method in &class.methods {
            for expr in &method.body {
                validate_expr(expr, is_stdlib, options, &mut diagnostics);
            }
        }
    }

    diagnostics
}

/// Determines if the current module is part of the standard library.
fn is_stdlib_module(options: &CompilerOptions) -> bool {
    // Priority 1: Explicit --stdlib-mode flag
    if options.stdlib_mode {
        return true;
    }

    // Priority 2: Source path heuristic (lib/ prefix)
    if let Some(ref path) = options.source_path {
        if path.starts_with("lib/") || path.starts_with("lib\\") {
            return true;
        }
    }

    false
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
            for body_expr in &block.body {
                validate_expr(body_expr, is_stdlib, options, diagnostics);
            }
        }
        Expression::Return { value, .. } | Expression::Parenthesized { expression: value, .. } => {
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
        Expression::Pipe { value, target, .. } => {
            validate_expr(value, is_stdlib, options, diagnostics);
            validate_expr(target, is_stdlib, options, diagnostics);
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

        // Leaf expressions — no primitives to validate
        Expression::Literal(..)
        | Expression::Identifier(_)
        | Expression::ClassReference { .. }
        | Expression::Super(_)
        | Expression::Error { .. } => {}
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
        diagnostics.push(Diagnostic::warning(
            "Using primitives outside stdlib — ensure you understand safety implications",
            span,
        ));
    } else {
        // Default: hard error
        let mut diag = Diagnostic::error(
            "Primitives can only be declared in the standard library",
            span,
        );
        diag.hint = Some(
            "Use --allow-primitives flag only if implementing FFI bindings".into(),
        );
        diagnostics.push(diag);
    }
}

/// Validates that an unquoted intrinsic name is in the structural intrinsic registry.
fn validate_intrinsic_name(name: &str, span: Span, diagnostics: &mut Vec<Diagnostic>) {
    if !STRUCTURAL_INTRINSICS.contains(&name) {
        let known = STRUCTURAL_INTRINSICS.join(", ");
        let mut diag = Diagnostic::error(
            format!("Unknown intrinsic '{name}'"),
            span,
        );
        diag.hint = Some(format!("Known intrinsics: {known}").into());
        diagnostics.push(diag);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_module(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        module
    }

    #[test]
    fn primitive_in_stdlib_mode_no_error() {
        let module = parse_module("@primitive '+'");
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(diags.is_empty(), "Expected no diagnostics in stdlib mode, got: {diags:?}");
    }

    #[test]
    fn primitive_in_stdlib_path_no_error() {
        let module = parse_module("@primitive '+'");
        let options = CompilerOptions {
            source_path: Some("lib/Integer.bt".to_string()),
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(diags.is_empty(), "Expected no diagnostics for lib/ path, got: {diags:?}");
    }

    #[test]
    fn primitive_in_user_code_error() {
        let module = parse_module("@primitive '+'");
        let options = CompilerOptions::default();
        let diags = validate_primitives(&module, &options);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Primitives can only be declared"));
        assert!(diags[0].hint.as_ref().unwrap().contains("--allow-primitives"));
    }

    #[test]
    fn primitive_with_allow_primitives_warning() {
        let module = parse_module("@primitive '+'");
        let options = CompilerOptions {
            allow_primitives: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Using primitives outside stdlib"));
        assert_eq!(
            diags[0].severity,
            crate::source_analysis::Severity::Warning
        );
    }

    #[test]
    fn unknown_structural_intrinsic_error() {
        let module = parse_module("@primitive unknownFoo");
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
        let module = parse_module("@primitive basicNew");
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(diags.is_empty(), "Expected no diagnostics for known intrinsic, got: {diags:?}");
    }

    #[test]
    fn quoted_selector_always_accepted() {
        // Quoted selectors are runtime-dispatch, no intrinsic name validation
        let module = parse_module("@primitive 'anyRandomName'");
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(diags.is_empty());
    }

    #[test]
    fn primitive_in_class_method_validated() {
        let source = "Object subclass: MyInt\n  + other => @primitive '+'";
        let module = parse_module(source);
        let options = CompilerOptions::default();
        let diags = validate_primitives(&module, &options);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Primitives can only be declared"));
    }

    #[test]
    fn primitive_in_class_method_stdlib_ok() {
        let source = "Object subclass: MyInt\n  + other => @primitive '+'";
        let module = parse_module(source);
        let options = CompilerOptions {
            stdlib_mode: true,
            ..Default::default()
        };
        let diags = validate_primitives(&module, &options);
        assert!(diags.is_empty());
    }

    #[test]
    fn multiple_primitives_multiple_errors() {
        let source = "@primitive '+'. @primitive unknownFoo";
        let module = parse_module(source);
        let options = CompilerOptions::default();
        let diags = validate_primitives(&module, &options);
        // At least 2 errors: one for stdlib restriction on '+', one for stdlib + unknown on unknownFoo
        assert!(diags.len() >= 2, "Expected multiple diagnostics, got: {diags:?}");
    }
}

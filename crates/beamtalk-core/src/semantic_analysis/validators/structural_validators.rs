// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compile-time structural validation (BT-1726).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators that check structural consistency of class references, FFI
//! module names, and Erlang function arities at compile time:
//! - Unresolved class references — classes not in the hierarchy
//! - Unresolved FFI modules — `Erlang <module>` with unknown module
//! - Arity mismatches — known Erlang functions called with wrong argument count

use crate::ast::{Expression, ExpressionStatement, MessageSelector, Module};
use crate::ast_walker::{walk_expression, walk_module};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

// ── Unresolved class references ──────────────────────────────────────────────

/// BT-1726: Warn on class references that don't exist in the hierarchy.
///
/// Walks all expressions looking for `ClassReference` nodes whose name is
/// not found in the `ClassHierarchy` or `ProtocolRegistry`. Skips built-in
/// names (`Erlang`, `Self`, `Nil`, `True`, `False`), type parameters of
/// the enclosing class, protocol names, and REPL workspace bindings passed
/// via `known_vars`.
pub(crate) fn check_unresolved_classes(
    module: &Module,
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
    known_vars: &[&str],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let empty_params: Vec<&str> = Vec::new();

    // Module-level expressions: no type parameters in scope.
    walk_stmts(
        &module.expressions,
        hierarchy,
        protocol_registry,
        &empty_params,
        known_vars,
        diagnostics,
    );

    // Per-class: only that class's type parameters are in scope.
    for class in &module.classes {
        let class_type_params: Vec<&str> = class
            .type_params
            .iter()
            .map(|tp| tp.name.name.as_str())
            .collect();
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            walk_stmts(
                &method.body,
                hierarchy,
                protocol_registry,
                &class_type_params,
                known_vars,
                diagnostics,
            );
        }
    }

    // Standalone method definitions: no type parameters in scope.
    for standalone in &module.method_definitions {
        walk_stmts(
            &standalone.method.body,
            hierarchy,
            protocol_registry,
            &empty_params,
            known_vars,
            diagnostics,
        );
    }
}

/// Walk a slice of expression statements, checking each for unresolved class references.
fn walk_stmts(
    stmts: &[ExpressionStatement],
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
    type_param_names: &[&str],
    known_vars: &[&str],
    diagnostics: &mut Vec<Diagnostic>,
) {
    for stmt in stmts {
        walk_expression(&stmt.expression, &mut |expr| {
            visit_unresolved_class(
                expr,
                hierarchy,
                protocol_registry,
                type_param_names,
                known_vars,
                diagnostics,
            );
        });
    }
}

/// Names that should never trigger unresolved-class warnings.
///
/// - `Erlang`: The FFI proxy class — always valid, handled by DNU.
/// - `Self`: Refers to the enclosing class.
/// - `Nil`, `True`, `False`: Literal types used in annotations.
const BUILTIN_CLASS_NAMES: &[&str] = &["Erlang", "Self", "Nil", "True", "False"];

fn visit_unresolved_class(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
    type_param_names: &[&str],
    known_vars: &[&str],
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::ClassReference { name, span, .. } = expr {
        let class_name = name.name.as_str();

        // Skip builtins, type parameters, known REPL bindings, classes, and protocols.
        // Known vars covers REPL workspace bindings like `Workspace` and `Transcript`
        // which are capitalized variables, not class references.
        // Protocols are first-class objects (BT-1928) and valid class references.
        if BUILTIN_CLASS_NAMES.contains(&class_name)
            || type_param_names.contains(&class_name)
            || known_vars.contains(&class_name)
            || hierarchy.has_class(class_name)
            || protocol_registry.has_protocol(class_name)
        {
            return;
        }

        diagnostics.push(
            Diagnostic::warning(
                format!("Unresolved class `{class_name}`"),
                *span,
            )
            .with_hint(
                "This class is not defined in the current compilation unit or standard library. \
                 Suppress with @expect unresolved_class if it exists at runtime.",
            )
            .with_category(DiagnosticCategory::UnresolvedClass),
        );
    }
}

// ── Workspace binding shadows class ─────────────────────────────────────────

/// BT-1759: Warn when a workspace binding (REPL variable) shadows a class name.
///
/// In the REPL, workspace bindings like `Workspace` and `Transcript` are
/// injected as known variables. If a class with the same name also exists in
/// the hierarchy, the binding silently shadows the class, which can confuse
/// users. This check emits a warning for each such collision.
pub(crate) fn check_workspace_shadows(
    hierarchy: &ClassHierarchy,
    known_vars: &[&str],
    diagnostics: &mut Vec<Diagnostic>,
) {
    for &var_name in known_vars {
        if hierarchy.has_class(var_name) {
            diagnostics.push(
                Diagnostic::warning(
                    format!("Workspace binding `{var_name}` shadows class `{var_name}`"),
                    crate::source_analysis::Span::new(0, 0),
                )
                .with_hint(format!(
                    "The REPL workspace binding `{var_name}` hides the class with the same name. \
                     References to `{var_name}` will resolve to the binding, not the class.",
                ))
                .with_category(DiagnosticCategory::ShadowedClass),
            );
        }
    }
}

// ── Unresolved FFI modules ───────────────────────────────────────────────────

/// Known OTP/Erlang modules that are always available on the BEAM.
///
/// This is intentionally generous to avoid false positives. Modules not in
/// this list trigger a warning, not an error.
const KNOWN_OTP_MODULES: &[&str] = &[
    "application",
    "atomics",
    "base64",
    "binary",
    "calendar",
    "code",
    "counters",
    "crypto",
    "dict",
    "digraph",
    "ets",
    "erl_syntax",
    "erlang",
    "error_logger",
    "file",
    "filelib",
    "filename",
    "gb_sets",
    "gb_trees",
    "gen_event",
    "gen_server",
    "gen_statem",
    "gen_tcp",
    "gen_udp",
    "httpc",
    "inet",
    "io",
    "io_lib",
    "json",
    "lists",
    "logger",
    "maps",
    "math",
    "orddict",
    "ordsets",
    "os",
    "persistent_term",
    "proc_lib",
    "proplists",
    "queue",
    "rand",
    "re",
    "sets",
    "ssl",
    "string",
    "supervisor",
    "sys",
    "timer",
    "unicode",
    "uri_string",
    "zlib",
];

/// BT-1726: Warn on unresolved Erlang FFI module references.
///
/// Detects the `Erlang <module> <selector>` FFI call pattern where
/// `<module>` is used as a unary message to `Erlang`.
pub(crate) fn check_unresolved_ffi_modules(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    // Collect Erlang source files in the project (their module names are valid).
    // We don't have access to the filesystem here, so we only check the static list.

    walk_module(module, &mut |expr| {
        visit_unresolved_ffi(expr, diagnostics);
    });
}

/// Class-protocol selectors that are NOT Erlang module names.
///
/// Mirrors `CLASS_PROTOCOL_SELECTORS` in codegen so that `Erlang class`,
/// `Erlang new`, etc. are not treated as FFI module lookups.
const CLASS_PROTOCOL_SELECTORS: &[&str] = &[
    "new",
    "spawn",
    "class",
    "methods",
    "superclass",
    "subclasses",
    "allSubclasses",
    "class_name",
    "module_name",
    "printString",
];

/// Visitor for Erlang FFI module references in expressions.
///
/// Matches `Erlang <module>` — a `MessageSend` whose receiver is
/// `ClassReference("Erlang")` with a `Unary` selector naming the module.
/// This catches both standalone proxy construction (`Erlang typo_mod`)
/// and calls like `Erlang typo_mod reverse: xs` (the inner send).
fn visit_unresolved_ffi(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(module_name),
        span,
        ..
    } = expr
    {
        if let Expression::ClassReference { name, .. } = receiver.as_ref() {
            // Skip class-protocol selectors (class, new, methods, etc.)
            if name.name == "Erlang"
                && !CLASS_PROTOCOL_SELECTORS.contains(&module_name.as_str())
                && !is_known_erlang_module(module_name.as_str())
            {
                diagnostics.push(
                    Diagnostic::warning(
                        format!("Unknown Erlang module `{module_name}` in FFI call"),
                        *span,
                    )
                    .with_hint(
                        "This module is not in the known OTP module list. \
                         Suppress with @expect unresolved_ffi if the module exists at runtime.",
                    )
                    .with_category(DiagnosticCategory::UnresolvedFfi),
                );
            }
        }
    }
}

fn is_known_erlang_module(name: &str) -> bool {
    KNOWN_OTP_MODULES.contains(&name) || name.starts_with("beamtalk_")
}

// ── FFI arity mismatches ─────────────────────────────────────────────────────

/// Known Erlang function arities: `(module, function, arity)`.
///
/// This is a curated list of commonly-used OTP functions where arity
/// mismatches are a frequent source of bugs when using the Beamtalk FFI.
const KNOWN_ARITIES: &[(&str, &str, usize)] = &[
    // lists
    ("lists", "append", 1),
    ("lists", "append", 2),
    ("lists", "filter", 2),
    ("lists", "flatten", 1),
    ("lists", "foldl", 3),
    ("lists", "foldr", 3),
    ("lists", "foreach", 2),
    ("lists", "keyfind", 3),
    ("lists", "keystore", 4),
    ("lists", "last", 1),
    ("lists", "map", 2),
    ("lists", "member", 2),
    ("lists", "nth", 2),
    ("lists", "reverse", 1),
    ("lists", "reverse", 2),
    ("lists", "seq", 2),
    ("lists", "seq", 3),
    ("lists", "sort", 1),
    ("lists", "sort", 2),
    ("lists", "zip", 2),
    // maps
    ("maps", "find", 2),
    ("maps", "fold", 3),
    ("maps", "from_list", 1),
    ("maps", "get", 2),
    ("maps", "get", 3),
    ("maps", "is_key", 2),
    ("maps", "keys", 1),
    ("maps", "map", 2),
    ("maps", "merge", 2),
    ("maps", "new", 0),
    ("maps", "put", 3),
    ("maps", "remove", 2),
    ("maps", "size", 1),
    ("maps", "to_list", 1),
    ("maps", "update", 3),
    ("maps", "values", 1),
    // erlang
    ("erlang", "atom_to_binary", 1),
    ("erlang", "atom_to_list", 1),
    ("erlang", "binary_to_atom", 1),
    ("erlang", "binary_to_list", 1),
    ("erlang", "element", 2),
    ("erlang", "hd", 1),
    ("erlang", "integer_to_binary", 1),
    ("erlang", "is_atom", 1),
    ("erlang", "is_binary", 1),
    ("erlang", "is_integer", 1),
    ("erlang", "is_list", 1),
    ("erlang", "is_map", 1),
    ("erlang", "length", 1),
    ("erlang", "list_to_binary", 1),
    ("erlang", "list_to_tuple", 1),
    ("erlang", "make_ref", 0),
    ("erlang", "self", 0),
    ("erlang", "size", 1),
    ("erlang", "spawn", 1),
    ("erlang", "spawn", 3),
    ("erlang", "throw", 1),
    ("erlang", "tl", 1),
    ("erlang", "tuple_to_list", 1),
    // io
    ("io", "format", 1),
    ("io", "format", 2),
    // timer
    ("timer", "sleep", 1),
    // string
    ("string", "join", 2),
    ("string", "split", 2),
    ("string", "trim", 1),
    // file
    ("file", "read_file", 1),
    ("file", "write_file", 2),
    ("file", "write_file", 3),
    // os
    ("os", "cmd", 1),
    ("os", "getenv", 1),
    ("os", "type", 0),
    // math
    ("math", "sqrt", 1),
    ("math", "pow", 2),
    ("math", "pi", 0),
    // re
    ("re", "compile", 1),
    ("re", "compile", 2),
    ("re", "run", 2),
    ("re", "run", 3),
    // ets
    ("ets", "insert", 2),
    ("ets", "lookup", 2),
    ("ets", "new", 2),
    ("ets", "delete", 1),
    ("ets", "delete", 2),
    // rand
    ("rand", "uniform", 0),
    ("rand", "uniform", 1),
    // json
    ("json", "encode", 1),
    ("json", "decode", 1),
];

/// BT-1726: Warn when a known Erlang function is called with the wrong arity.
///
/// Only checks functions in the `KNOWN_ARITIES` table — unknown functions
/// are silently accepted (the unresolved-FFI check handles unknown modules).
pub(crate) fn check_ffi_arity(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        visit_ffi_arity(expr, diagnostics);
    });
}

/// Unwraps `Parenthesized` wrappers to get the inner expression.
///
/// `(Erlang lists)` parses as `Parenthesized { expression: MessageSend { ... } }`.
/// This strips the parentheses so pattern matching sees the send underneath.
fn unwrap_parens(mut expr: &Expression) -> &Expression {
    while let Expression::Parenthesized { expression, .. } = expr {
        expr = expression;
    }
    expr
}

/// Extracts the Erlang module name from an FFI proxy expression.
///
/// Matches `Erlang <module>` or `(Erlang <module>)` — a `MessageSend`
/// whose receiver is `ClassReference("Erlang")` with a `Unary` selector.
fn extract_erlang_module(expr: &Expression) -> Option<&str> {
    let expr = unwrap_parens(expr);
    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(module_name),
        ..
    } = expr
    {
        if let Expression::ClassReference { name, .. } = receiver.as_ref() {
            if name.name == "Erlang" {
                return Some(module_name.as_str());
            }
        }
    }
    None
}

/// Derives the Erlang function name from a Beamtalk selector.
///
/// Keyword selectors like `seq:to:` become the first keyword part (`seq`).
/// Unary selectors use the name directly. Binary selectors don't map to
/// Erlang functions (they're Beamtalk operators).
fn erlang_function_name(selector: &MessageSelector) -> Option<String> {
    match selector {
        MessageSelector::Unary(name) => Some(name.to_string()),
        MessageSelector::Keyword(parts) => parts
            .first()
            .map(|kp| kp.keyword.trim_end_matches(':').to_string()),
        MessageSelector::Binary(_) => None,
    }
}

/// Computes the Erlang arity for a message send.
///
/// For direct Erlang FFI calls, the arity is the number of Beamtalk arguments
/// for keyword messages (e.g. `seq: 1 to: 10` has arity 2), and 0 for unary
/// messages (they lower to zero-argument Erlang function calls).
fn erlang_arity(selector: &MessageSelector, argument_count: usize) -> usize {
    match selector {
        MessageSelector::Unary(_) => 0,
        _ => argument_count,
    }
}

fn visit_ffi_arity(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        span,
        ..
    } = expr
    {
        if let Some(module_name) = extract_erlang_module(receiver) {
            if let Some(function_name) = erlang_function_name(selector) {
                let arity = erlang_arity(selector, arguments.len());

                // Check if this function has known arities.
                let known: Vec<usize> = KNOWN_ARITIES
                    .iter()
                    .filter(|(m, f, _)| *m == module_name && *f == function_name)
                    .map(|(_, _, a)| *a)
                    .collect();

                if !known.is_empty() && !known.contains(&arity) {
                    let expected = if known.len() == 1 {
                        format!("{}", known[0])
                    } else {
                        let strs: Vec<String> = known.iter().map(ToString::to_string).collect();
                        strs.join(" or ")
                    };

                    diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Arity mismatch: `{module_name}:{function_name}/{arity}` — \
                                 expected arity {expected}"
                            ),
                            *span,
                        )
                        .with_hint(format!(
                            "The Erlang function `{module_name}:{function_name}` \
                             expects {expected} argument(s), but {arity} provided. \
                             Suppress with @expect arity_mismatch.",
                        ))
                        .with_category(DiagnosticCategory::ArityMismatch),
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ClassDefinition, CommentAttachment, ExpressionStatement, Identifier, KeywordPart,
        MethodDefinition, MethodKind, TypeParamDecl,
    };
    use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
    use crate::source_analysis::Span;

    fn test_span() -> Span {
        Span::new(0, 10)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: test_span(),
        }
    }

    fn kwpart(keyword: &str) -> KeywordPart {
        KeywordPart {
            keyword: keyword.into(),
            span: test_span(),
        }
    }

    fn class_ref(name: &str) -> Expression {
        Expression::ClassReference {
            name: ident(name),
            package: None,
            span: test_span(),
        }
    }

    fn empty_module_with_exprs(exprs: Vec<Expression>) -> Module {
        Module::new(
            exprs.into_iter().map(ExpressionStatement::bare).collect(),
            test_span(),
        )
    }

    // ── Unresolved class tests ──────────────────────────────────────────────

    #[test]
    fn test_unresolved_class_warns_on_unknown() {
        let module = empty_module_with_exprs(vec![class_ref("NonExistent")]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_unresolved_classes(
            &module,
            &hierarchy,
            &ProtocolRegistry::new(),
            &[],
            &mut diags,
        );

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Unresolved class `NonExistent`"));
        assert_eq!(diags[0].category, Some(DiagnosticCategory::UnresolvedClass));
    }

    #[test]
    fn test_unresolved_class_skips_builtins() {
        let module = empty_module_with_exprs(vec![
            class_ref("Erlang"),
            class_ref("Self"),
            class_ref("Nil"),
            class_ref("True"),
            class_ref("False"),
        ]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_unresolved_classes(
            &module,
            &hierarchy,
            &ProtocolRegistry::new(),
            &[],
            &mut diags,
        );

        assert!(diags.is_empty(), "Builtins should not trigger warnings");
    }

    #[test]
    fn test_unresolved_class_skips_known_classes() {
        // Object, Integer, String, etc. are builtins in the hierarchy
        let module = empty_module_with_exprs(vec![class_ref("Object"), class_ref("Integer")]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_unresolved_classes(
            &module,
            &hierarchy,
            &ProtocolRegistry::new(),
            &[],
            &mut diags,
        );

        assert!(
            diags.is_empty(),
            "Known classes should not trigger warnings"
        );
    }

    #[test]
    fn test_unresolved_class_skips_known_vars() {
        // REPL workspace bindings like Workspace and Transcript are capitalized
        // variables, not class references — they should not trigger warnings.
        let module = empty_module_with_exprs(vec![class_ref("Workspace"), class_ref("Transcript")]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_unresolved_classes(
            &module,
            &hierarchy,
            &ProtocolRegistry::new(),
            &["Workspace", "Transcript"],
            &mut diags,
        );

        assert!(
            diags.is_empty(),
            "Known REPL variables should not trigger unresolved class warnings"
        );
    }

    #[test]
    fn test_unresolved_class_skips_protocols() {
        // Protocol names like Printable are first-class class objects (BT-1928)
        // and should not trigger unresolved class warnings.
        let module = empty_module_with_exprs(vec![class_ref("Printable")]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut registry = ProtocolRegistry::new();
        registry.register_test_protocol(crate::semantic_analysis::ProtocolInfo {
            name: "Printable".into(),
            type_params: vec![],
            type_param_bounds: vec![],
            extending: None,
            methods: vec![],
            class_methods: vec![],
            span: test_span(),
        });
        let mut diags = Vec::new();

        check_unresolved_classes(&module, &hierarchy, &registry, &[], &mut diags);

        assert!(
            diags.is_empty(),
            "Protocol names should not trigger unresolved class warnings"
        );
    }

    // ── FFI module tests ────────────────────────────────────────────────────

    #[test]
    fn test_known_otp_modules_accepted() {
        assert!(is_known_erlang_module("lists"));
        assert!(is_known_erlang_module("maps"));
        assert!(is_known_erlang_module("erlang"));
        assert!(is_known_erlang_module("io"));
    }

    #[test]
    fn test_unknown_module_not_accepted() {
        assert!(!is_known_erlang_module("my_custom_module"));
        assert!(!is_known_erlang_module("nonexistent"));
    }

    #[test]
    fn test_ffi_module_warns_on_unknown() {
        // Build: Erlang custom_mod reverse: x
        // AST: MessageSend(receiver=MessageSend(receiver=ClassRef("Erlang"), sel=Unary("custom_mod")), sel=Keyword(["reverse"]), args=[x])
        let inner_send = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("custom_mod".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let outer_send = Expression::MessageSend {
            receiver: Box::new(inner_send),
            selector: MessageSelector::Keyword(vec![kwpart("reverse:")]),
            arguments: vec![Expression::Identifier(ident("x"))],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![outer_send]);
        let mut diags = Vec::new();

        check_unresolved_ffi_modules(&module, &mut diags);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("custom_mod"));
        assert_eq!(diags[0].category, Some(DiagnosticCategory::UnresolvedFfi));
    }

    #[test]
    fn test_ffi_module_accepts_known() {
        let inner_send = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("lists".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let outer_send = Expression::MessageSend {
            receiver: Box::new(inner_send),
            selector: MessageSelector::Keyword(vec![kwpart("reverse:")]),
            arguments: vec![Expression::Identifier(ident("x"))],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![outer_send]);
        let mut diags = Vec::new();

        check_unresolved_ffi_modules(&module, &mut diags);

        assert!(diags.is_empty(), "Known OTP modules should not warn");
    }

    // ── Arity mismatch tests ────────────────────────────────────────────────

    #[test]
    fn test_arity_mismatch_warns_on_wrong_count() {
        // Erlang lists flatten: a extra: b — arity 2, but lists:flatten is arity 1 only
        let inner_send = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("lists".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let outer_send = Expression::MessageSend {
            receiver: Box::new(inner_send),
            selector: MessageSelector::Keyword(vec![kwpart("flatten:"), kwpart("extra:")]),
            arguments: vec![
                Expression::Identifier(ident("a")),
                Expression::Identifier(ident("b")),
            ],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![outer_send]);
        let mut diags = Vec::new();

        check_ffi_arity(&module, &mut diags);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("lists:flatten/2"));
        assert!(diags[0].message.contains("expected arity 1"));
        assert_eq!(diags[0].category, Some(DiagnosticCategory::ArityMismatch));
    }

    #[test]
    fn test_arity_match_no_warning() {
        // Erlang lists reverse: x — arity 1, matches lists:reverse/1
        let inner_send = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("lists".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let outer_send = Expression::MessageSend {
            receiver: Box::new(inner_send),
            selector: MessageSelector::Keyword(vec![kwpart("reverse:")]),
            arguments: vec![Expression::Identifier(ident("x"))],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![outer_send]);
        let mut diags = Vec::new();

        check_ffi_arity(&module, &mut diags);

        assert!(diags.is_empty(), "Correct arity should not warn");
    }

    #[test]
    fn test_arity_unknown_function_no_warning() {
        // Erlang lists some_unknown_function: x — not in known arities table
        let inner_send = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("lists".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let outer_send = Expression::MessageSend {
            receiver: Box::new(inner_send),
            selector: MessageSelector::Keyword(vec![kwpart("some_unknown_function:")]),
            arguments: vec![Expression::Identifier(ident("x"))],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![outer_send]);
        let mut diags = Vec::new();

        check_ffi_arity(&module, &mut diags);

        assert!(
            diags.is_empty(),
            "Unknown functions should not trigger arity warnings"
        );
    }

    #[test]
    fn test_ffi_standalone_proxy_warns() {
        // `Erlang typo_mod` as a standalone expression should warn
        let proxy = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("typo_mod".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![proxy]);
        let mut diags = Vec::new();

        check_unresolved_ffi_modules(&module, &mut diags);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("typo_mod"));
    }

    #[test]
    fn test_arity_check_with_parenthesized_receiver() {
        // (Erlang lists) flatten: a extra: b — arity 2, but lists:flatten is arity 1
        let inner_send = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("lists".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let parens = Expression::Parenthesized {
            expression: Box::new(inner_send),
            span: test_span(),
        };
        let outer_send = Expression::MessageSend {
            receiver: Box::new(parens),
            selector: MessageSelector::Keyword(vec![kwpart("flatten:"), kwpart("extra:")]),
            arguments: vec![
                Expression::Identifier(ident("a")),
                Expression::Identifier(ident("b")),
            ],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![outer_send]);
        let mut diags = Vec::new();

        check_ffi_arity(&module, &mut diags);

        assert_eq!(
            diags.len(),
            1,
            "Parenthesized receiver should not bypass arity check"
        );
        assert!(diags[0].message.contains("lists:flatten/2"));
    }

    fn make_method(body_expr: Expression) -> MethodDefinition {
        MethodDefinition {
            selector: MessageSelector::Unary("test".into()),
            parameters: vec![],
            body: vec![ExpressionStatement::bare(body_expr)],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }
    }

    #[test]
    fn test_unresolved_class_skips_type_params_in_class() {
        // A class Container(T) with a method referencing T should not warn.
        let mut module = empty_module_with_exprs(vec![]);
        let mut class_def = ClassDefinition::new(
            ident("Container"),
            ident("Object"),
            vec![],
            vec![],
            test_span(),
        );
        class_def.type_params.push(TypeParamDecl {
            name: ident("T"),
            bound: None,
            span: test_span(),
        });
        class_def.methods.push(make_method(class_ref("T")));
        module.classes.push(class_def);

        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_unresolved_classes(
            &module,
            &hierarchy,
            &ProtocolRegistry::new(),
            &[],
            &mut diags,
        );

        assert!(
            diags.is_empty(),
            "Type parameters should not trigger unresolved class warnings within their class"
        );
    }

    #[test]
    fn test_unresolved_class_warns_type_param_outside_class() {
        // T referenced at module level should warn even if a class defines T as a type param.
        let mut module = empty_module_with_exprs(vec![class_ref("T")]);
        let mut class_def = ClassDefinition::new(
            ident("Container"),
            ident("Object"),
            vec![],
            vec![],
            test_span(),
        );
        class_def.type_params.push(TypeParamDecl {
            name: ident("T"),
            bound: None,
            span: test_span(),
        });
        module.classes.push(class_def);

        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_unresolved_classes(
            &module,
            &hierarchy,
            &ProtocolRegistry::new(),
            &[],
            &mut diags,
        );

        assert_eq!(
            diags.len(),
            1,
            "Type param T used outside its class should warn"
        );
    }

    #[test]
    fn test_arity_multi_arity_display() {
        // Erlang lists reverse: a extra: b extra2: c — arity 3
        // lists:reverse has arities 1 and 2, so message should say "1 or 2"
        let inner_send = Expression::MessageSend {
            receiver: Box::new(class_ref("Erlang")),
            selector: MessageSelector::Unary("lists".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };
        let outer_send = Expression::MessageSend {
            receiver: Box::new(inner_send),
            selector: MessageSelector::Keyword(vec![
                kwpart("reverse:"),
                kwpart("extra:"),
                kwpart("extra2:"),
            ]),
            arguments: vec![
                Expression::Identifier(ident("a")),
                Expression::Identifier(ident("b")),
                Expression::Identifier(ident("c")),
            ],
            is_cast: false,
            span: test_span(),
        };
        let module = empty_module_with_exprs(vec![outer_send]);
        let mut diags = Vec::new();

        check_ffi_arity(&module, &mut diags);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("lists:reverse/3"));
        assert!(diags[0].message.contains("1 or 2"));
    }

    #[test]
    fn test_beamtalk_prefix_accepted() {
        assert!(is_known_erlang_module("beamtalk_extensions"));
        assert!(is_known_erlang_module("beamtalk_runtime"));
        assert!(!is_known_erlang_module("my_custom_module"));
    }

    // ── Workspace shadow tests ─────────────────────────────────────────────

    #[test]
    fn test_workspace_shadow_warns_when_binding_matches_class() {
        // "Object" is a built-in class in the hierarchy, so a workspace binding
        // named "Object" should trigger a shadowing warning.
        let module = empty_module_with_exprs(vec![]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_workspace_shadows(&hierarchy, &["Object"], &mut diags);

        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("Workspace binding `Object` shadows class `Object`")
        );
        assert_eq!(diags[0].category, Some(DiagnosticCategory::ShadowedClass));
    }

    #[test]
    fn test_workspace_shadow_no_warning_for_non_class() {
        // "myVar" is not a class, so no shadow warning.
        let module = empty_module_with_exprs(vec![]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_workspace_shadows(&hierarchy, &["myVar"], &mut diags);

        assert!(
            diags.is_empty(),
            "Non-class bindings should not trigger shadow warnings"
        );
    }

    #[test]
    fn test_workspace_shadow_multiple_collisions() {
        // Both "Object" and "Integer" are built-in classes.
        let module = empty_module_with_exprs(vec![]);
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        let hierarchy = hierarchy.unwrap();
        let mut diags = Vec::new();

        check_workspace_shadows(&hierarchy, &["Object", "Integer", "notAClass"], &mut diags);

        assert_eq!(diags.len(), 2, "Should warn for Object and Integer only");
    }
}

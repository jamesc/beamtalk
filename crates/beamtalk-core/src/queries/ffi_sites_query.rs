// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! FFI call-site query — find Erlang FFI invocations of a named function
//! within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation ffiSitesFor:` (BT-2211). Where `sendersOf:` answers
//! "who calls this Beamtalk method?", this query answers "who calls this Erlang
//! function through the FFI bridge?". Given the source text of a single compiled
//! method (as returned by `CompiledMethod source`) and a target Erlang
//! `module`/`function` (optionally constrained to a specific `arity`), it
//! returns the 1-based line numbers, relative to the method source, at which a
//! matching FFI call appears.
//!
//! # What counts as an FFI call site
//!
//! There is no dedicated `ErlangCall` AST node. An FFI call such as
//! `(Erlang lists) reverse: xs` parses as a nested [`Expression::MessageSend`]
//! whose root receiver is the `Erlang` class reference. The detection mirrors
//! the existing FFI machinery in
//! [`crate::semantic_analysis::validators::structural_validators`] (BT-1726):
//!
//! - The **module** is recovered from the receiver: `Erlang <module>` (or
//!   `(Erlang <module>)`) is a `MessageSend` whose receiver is
//!   `ClassReference("Erlang")` with a `Unary` selector naming the module.
//! - The **function** is derived from the outer selector: a `Unary` selector
//!   uses its name directly; a `Keyword` selector uses its first keyword part
//!   (`seq:to:` → `seq`). This matches how codegen lowers the call
//!   (`generate_direct_erlang_call`).
//! - The **arity** is the Beamtalk argument count for keyword/binary sends, and
//!   `0` for unary sends (they lower to zero-argument Erlang calls).
//!
//! When the caller supplies `arity = Some(n)`, only call sites with exactly `n`
//! arguments match; `arity = None` matches the function at any arity.
//!
//! Class-protocol selectors such as `Erlang class` / `Erlang new` are NOT FFI
//! module lookups (they dispatch to the class protocol), so they are never
//! reported — the [`CLASS_PROTOCOL_SELECTORS`] guard mirrors codegen and the
//! validators.
//!
//! # Parsing strategy
//!
//! Reuses the synthetic-class-header trick from
//! [`crate::queries::senders_query`]: a bare method definition is not a valid
//! top-level form, so the input is wrapped in
//! `Object subclass: __SyntheticFfiScope\n` before lexing/parsing. Line numbers
//! are translated back to input-source space by subtracting the prefix line
//! count.
//!
//! # Error handling
//!
//! Parse errors are tolerated: any sub-trees that parsed successfully still
//! contribute results. A completely unparseable source returns an empty list.
//! Callers treat "no sites found" identically to "could not parse".

use super::selector_span;
use crate::ast::{Expression, MessageSelector, Pattern, StringSegment};
use crate::source_analysis::{Span, lex_with_eof, parse};

/// Number of newlines in the synthetic class header that wraps the input.
/// Used to translate line numbers from wrapped-source to input-source space.
const PREFIX_LINES: u32 = 1;

/// The synthetic class header used to make a bare method definition parseable.
/// The single newline at the end is counted by [`PREFIX_LINES`]. The class
/// name uses a leading underscore so it cannot collide with a real class.
const SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticFfiScope\n";

/// Class-protocol selectors that are NOT Erlang module names.
///
/// Mirrors `CLASS_PROTOCOL_SELECTORS` in codegen
/// (`dispatch_codegen::try_handle_erlang_interop`) and the FFI validators, so
/// `Erlang class`, `Erlang new`, etc. are not treated as FFI module lookups.
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

/// Find the 1-based line numbers within `method_source` where the Erlang FFI
/// function `module`:`function` is invoked.
///
/// Walks the parsed AST for [`Expression::MessageSend`] nodes that resolve
/// through the `Erlang` FFI bridge to a function whose module and function name
/// match `module`/`function`. When `arity` is `Some(n)`, only call sites with
/// exactly `n` arguments match; when `None`, the function matches at any arity.
///
/// Each matching call site produces one entry; multiple matching calls on the
/// same line produce multiple entries, preserving the pre-order walk.
///
/// Returns an empty vector if no sites are found or the source cannot be parsed
/// at all.
#[must_use]
pub fn find_ffi_sites_in_source(
    method_source: &str,
    module: &str,
    function: &str,
    arity: Option<usize>,
) -> Vec<u32> {
    let wrapped = format!("{SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (parsed, _diags) = parse(tokens);

    let target = FfiTarget {
        module,
        function,
        arity,
    };

    let mut wrapped_lines = Vec::new();

    crate::ast_walker::for_each_expr_seq(&parsed, |seq| {
        for stmt in seq {
            collect_ffi_sites(&stmt.expression, &target, &wrapped, &mut wrapped_lines);
        }
    });

    // Translate from wrapped-source line numbers back to input-source space.
    wrapped_lines
        .into_iter()
        .map(|line| line.saturating_sub(PREFIX_LINES).max(1))
        .collect()
}

/// The Erlang function the walker is matching against.
struct FfiTarget<'a> {
    /// Erlang module name (e.g. `lists`).
    module: &'a str,
    /// Erlang function name (e.g. `reverse`).
    function: &'a str,
    /// When `Some(n)`, only call sites with exactly `n` arguments match.
    arity: Option<usize>,
}

/// Strip `Parenthesized` wrappers so a `(Erlang lists)` receiver is matched the
/// same as a bare `Erlang lists`.
fn unwrap_parens(mut expr: &Expression) -> &Expression {
    while let Expression::Parenthesized { expression, .. } = expr {
        expr = expression;
    }
    expr
}

/// Recover the Erlang module name from an FFI proxy receiver.
///
/// Matches `Erlang <module>` or `(Erlang <module>)` — a `MessageSend` whose
/// receiver is `ClassReference("Erlang")` with a `Unary` selector. Returns the
/// module name, or `None` if the receiver is not an FFI module proxy. Class-
/// protocol selectors (`Erlang class`, …) are rejected so they are not treated
/// as module lookups.
fn extract_erlang_module(expr: &Expression) -> Option<&str> {
    let expr = unwrap_parens(expr);
    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(module_name),
        ..
    } = expr
    {
        if let Expression::ClassReference { name, package, .. } = receiver.as_ref() {
            if package.is_none()
                && name.name == "Erlang"
                && !CLASS_PROTOCOL_SELECTORS.contains(&module_name.as_str())
            {
                return Some(module_name.as_str());
            }
        }
    }
    None
}

/// Derive the Erlang function name from a Beamtalk selector.
///
/// Keyword selectors like `seq:to:` use the first keyword part (`seq`). Unary
/// selectors use the name directly. Binary selectors do not map to Erlang
/// function calls (they are Beamtalk operators), so they yield `None`. Mirrors
/// `structural_validators::erlang_function_name` and the codegen lowering.
fn erlang_function_name(selector: &MessageSelector) -> Option<String> {
    match selector {
        MessageSelector::Unary(name) => Some(name.to_string()),
        MessageSelector::Keyword(parts) => parts
            .first()
            .map(|kp| kp.keyword.trim_end_matches(':').to_string()),
        MessageSelector::Binary(_) => None,
    }
}

/// Compute the Erlang arity for a message send.
///
/// Unary FFI sends lower to zero-argument Erlang calls; keyword/binary sends use
/// the Beamtalk argument count (`seq: 1 to: 10` → arity 2). Mirrors
/// `structural_validators::erlang_arity`.
fn erlang_arity(selector: &MessageSelector, argument_count: usize) -> usize {
    match selector {
        MessageSelector::Unary(_) => 0,
        _ => argument_count,
    }
}

/// Determine the line number to report for a matching FFI call site. Uses the
/// span of the selector keywords when available (so the line points at the
/// function selector rather than the `Erlang` receiver, for multi-line calls),
/// falling back to the enclosing message-send span otherwise.
fn ffi_site_line(selector: &MessageSelector, fallback: Span, source: &str) -> u32 {
    selector_span(selector)
        .unwrap_or(fallback)
        .line_number(source)
}

/// Record a call site if `module` + `selector` (+ arity) match `target`.
///
/// `module` is the resolved Erlang module of the receiver (`None` when the
/// receiver is not an FFI proxy). Shared by the `MessageSend` and `Cascade`
/// arms so both match identically.
#[allow(clippy::too_many_arguments)]
fn record_ffi_match(
    module: Option<&str>,
    selector: &MessageSelector,
    arg_count: usize,
    span: Span,
    target: &FfiTarget,
    source: &str,
    lines: &mut Vec<u32>,
) {
    let Some(module) = module else { return };
    if module != target.module {
        return;
    }
    let Some(function) = erlang_function_name(selector) else {
        return;
    };
    if function != target.function {
        return;
    }
    let arity = erlang_arity(selector, arg_count);
    if target.arity.is_none_or(|wanted| wanted == arity) {
        lines.push(ffi_site_line(selector, span, source));
    }
}

/// Recursively collect line numbers of FFI call sites matching `target`.
// Exhaustive recursive AST walker: one arm per `Expression` variant, so the
// length is inherent (mirrors `collect_access_lines` in the sibling queries).
#[allow(clippy::too_many_lines)]
fn collect_ffi_sites(expr: &Expression, target: &FfiTarget, source: &str, lines: &mut Vec<u32>) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            // Check whether this send is itself a matching FFI call.
            record_ffi_match(
                extract_erlang_module(receiver),
                selector,
                arguments.len(),
                *span,
                target,
                source,
                lines,
            );
            // Recurse into the receiver and arguments so nested FFI calls (and
            // FFI calls inside argument expressions) are still found.
            collect_ffi_sites(receiver, target, source, lines);
            for arg in arguments {
                collect_ffi_sites(arg, target, source, lines);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            // The first message is part of `receiver` (parsed as a full send),
            // caught by recursing into it below. The remaining cascade messages
            // are sent to the SAME receiver as the first message — i.e. that
            // first send's own receiver (e.g. `(Erlang lists)` in
            // `(Erlang lists) reverse: xs; sort: ys`), so resolve the shared
            // module from there and match each cascade message against it.
            collect_ffi_sites(receiver, target, source, lines);
            let shared_module = match unwrap_parens(receiver) {
                Expression::MessageSend {
                    receiver: inner, ..
                } => extract_erlang_module(inner),
                other => extract_erlang_module(other),
            };
            for msg in messages {
                record_ffi_match(
                    shared_module,
                    &msg.selector,
                    msg.arguments.len(),
                    msg.span,
                    target,
                    source,
                    lines,
                );
                for arg in &msg.arguments {
                    collect_ffi_sites(arg, target, source, lines);
                }
            }
        }
        Expression::Assignment {
            target: t, value, ..
        } => {
            collect_ffi_sites(t, target, source, lines);
            collect_ffi_sites(value, target, source, lines);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_ffi_sites(value, target, source, lines);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_ffi_sites(&stmt.expression, target, source, lines);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_ffi_sites(expression, target, source, lines);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_ffi_sites(receiver, target, source, lines);
        }
        Expression::Match { value, arms, .. } => {
            collect_ffi_sites(value, target, source, lines);
            for arm in arms {
                collect_pattern_ffi_sites(&arm.pattern, target, source, lines);
                if let Some(guard) = &arm.guard {
                    collect_ffi_sites(guard, target, source, lines);
                }
                collect_ffi_sites(&arm.body, target, source, lines);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_ffi_sites(inner, target, source, lines);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_ffi_sites(element, target, source, lines);
            }
            if let Some(tail_expr) = tail {
                collect_ffi_sites(tail_expr, target, source, lines);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_ffi_sites(element, target, source, lines);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_ffi_sites(&pair.key, target, source, lines);
                collect_ffi_sites(&pair.value, target, source, lines);
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

/// Recursively collect FFI call sites inside a [`Pattern`].
///
/// Patterns carry no FFI calls today — the only expression slot is a
/// `BinarySegment::size`, which the parser restricts to an integer literal or a
/// bare identifier. The walker is defensive (mirroring the sibling queries): it
/// traverses container patterns and the binary-segment size expression so
/// loosening the parser later does not silently regress this query.
fn collect_pattern_ffi_sites(
    pattern: &Pattern,
    target: &FfiTarget,
    source: &str,
    lines: &mut Vec<u32>,
) {
    match pattern {
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_ffi_sites(&segment.value, target, source, lines);
                if let Some(size) = &segment.size {
                    collect_ffi_sites(size, target, source, lines);
                }
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_ffi_sites(element, target, source, lines);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_ffi_sites(element, target, source, lines);
            }
            if let Some(rest_pattern) = rest {
                collect_pattern_ffi_sites(rest_pattern, target, source, lines);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_ffi_sites(element, target, source, lines);
            }
            if let Some(tail_pattern) = tail {
                collect_pattern_ffi_sites(tail_pattern, target, source, lines);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_ffi_sites(&pair.value, target, source, lines);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_selector, inner) in keywords {
                collect_pattern_ffi_sites(inner, target, source, lines);
            }
        }
        Pattern::Wildcard(..)
        | Pattern::Literal(..)
        | Pattern::Variable(..)
        | Pattern::Nil(..)
        | Pattern::Type { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_keyword_ffi_call() {
        // `(Erlang lists) reverse: xs` — module `lists`, function `reverse`,
        // arity 1, on line 1.
        let lines = find_ffi_sites_in_source(
            "rev: xs => (Erlang lists) reverse: xs",
            "lists",
            "reverse",
            None,
        );
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_unparenthesized_ffi_call() {
        // `Erlang lists reverse: xs` (no parens) resolves the same way.
        let lines = find_ffi_sites_in_source(
            "rev: xs => Erlang lists reverse: xs",
            "lists",
            "reverse",
            None,
        );
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_unary_ffi_call_arity_zero() {
        // `Erlang erlang make_ref` — unary FFI call lowers to arity 0.
        let lines =
            find_ffi_sites_in_source("ref => Erlang erlang make_ref", "erlang", "make_ref", None);
        assert_eq!(lines, vec![1]);
        // Explicit arity 0 also matches.
        let lines0 = find_ffi_sites_in_source(
            "ref => Erlang erlang make_ref",
            "erlang",
            "make_ref",
            Some(0),
        );
        assert_eq!(lines0, vec![1]);
    }

    #[test]
    fn multi_keyword_function_name_is_first_keyword() {
        // `(Erlang lists) seq: 1 to: 10` — function `seq`, arity 2.
        let src = "range => (Erlang lists) seq: 1 to: 10";
        let lines = find_ffi_sites_in_source(src, "lists", "seq", None);
        assert_eq!(lines, vec![1]);
        let lines2 = find_ffi_sites_in_source(src, "lists", "seq", Some(2));
        assert_eq!(lines2, vec![1]);
    }

    #[test]
    fn arity_filter_narrows_matches() {
        // `(Erlang lists) reverse: xs` has arity 1; a query for arity 2 must
        // NOT match, while arity 1 does.
        let src = "rev: xs => (Erlang lists) reverse: xs";
        assert_eq!(
            find_ffi_sites_in_source(src, "lists", "reverse", Some(1)),
            vec![1]
        );
        assert!(find_ffi_sites_in_source(src, "lists", "reverse", Some(2)).is_empty());
    }

    #[test]
    fn does_not_match_other_module() {
        // A call to `lists:reverse` must not match a query for `maps:reverse`.
        let src = "rev: xs => (Erlang lists) reverse: xs";
        assert!(find_ffi_sites_in_source(src, "maps", "reverse", None).is_empty());
    }

    #[test]
    fn does_not_match_other_function() {
        let src = "rev: xs => (Erlang lists) reverse: xs";
        assert!(find_ffi_sites_in_source(src, "lists", "sort", None).is_empty());
    }

    #[test]
    fn does_not_match_beamtalk_send() {
        // A plain Beamtalk send `xs reverse` is not an FFI call.
        let src = "rev: xs => xs reverse";
        assert!(find_ffi_sites_in_source(src, "lists", "reverse", None).is_empty());
    }

    #[test]
    fn class_protocol_selector_is_not_an_ffi_call() {
        // `Erlang new` / `Erlang class` are class-protocol sends, not module
        // lookups, so a query for module `new`/`class` must not match.
        let src = "make => Erlang new";
        assert!(find_ffi_sites_in_source(src, "new", "anything", None).is_empty());
    }

    #[test]
    fn finds_ffi_call_inside_block() {
        // `[:x | (Erlang lists) reverse: x] value: xs` — the FFI call is on
        // line 2.
        let src = "rev: xs =>\n  [:x | (Erlang lists) reverse: x] value: xs";
        let lines = find_ffi_sites_in_source(src, "lists", "reverse", None);
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_ffi_call_in_argument_position() {
        // `Transcript show: ((Erlang lists) reverse: xs)` — the FFI call is an
        // argument; it must still be found.
        let src = "report: xs => Transcript show: ((Erlang lists) reverse: xs)";
        let lines = find_ffi_sites_in_source(src, "lists", "reverse", None);
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_multiple_calls_in_source_order() {
        let src = "go: xs =>\n  (Erlang lists) reverse: xs\n  (Erlang lists) reverse: xs\n  (Erlang lists) reverse: xs";
        let lines = find_ffi_sites_in_source(src, "lists", "reverse", None);
        assert_eq!(lines, vec![2, 3, 4]);
    }

    #[test]
    fn finds_call_in_class_method_with_return_type() {
        // Class-side methods carry a return-type arrow in the signature. The
        // walker must still find the FFI call in the body.
        let src = "make -> List =>\n  (Erlang lists) reverse: #(1, 2, 3)";
        let lines = find_ffi_sites_in_source(src, "lists", "reverse", None);
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn returns_empty_for_untouched_function() {
        let lines = find_ffi_sites_in_source("greet => self name", "lists", "reverse", None);
        assert!(lines.is_empty());
    }

    #[test]
    fn handles_unparseable_source_without_panicking() {
        let lines = find_ffi_sites_in_source(")@!", "lists", "reverse", None);
        assert!(lines.is_empty());
    }

    #[test]
    fn finds_chained_ffi_in_assignment_value() {
        // `result := (Erlang lists) reverse: xs` — the FFI call is in the
        // assignment's value position.
        let src = "rev: xs =>\n  result := (Erlang lists) reverse: xs\n  result";
        let lines = find_ffi_sites_in_source(src, "lists", "reverse", None);
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_ffi_in_cascade_message() {
        // A cascade sends every message to the same Erlang module receiver, so
        // both the first send (`reverse:`, line 3) and the cascade message
        // (`sort:`, line 4) are FFI call sites on `lists`.
        let src = "go: xs =>\n  (Erlang lists)\n    reverse: xs;\n    sort: xs";
        assert_eq!(
            find_ffi_sites_in_source(src, "lists", "reverse", None),
            vec![3]
        );
        assert_eq!(
            find_ffi_sites_in_source(src, "lists", "sort", None),
            vec![4]
        );
        // Arity filtering still applies to cascade messages (`sort: xs` is arity 1).
        assert_eq!(
            find_ffi_sites_in_source(src, "lists", "sort", Some(1)),
            vec![4]
        );
        assert!(find_ffi_sites_in_source(src, "lists", "sort", Some(2)).is_empty());
    }
}

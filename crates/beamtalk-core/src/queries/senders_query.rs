// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Senders query — find call sites of a selector within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation sendersOf:` (BT-2190, moved to `SystemNavigation`
//! in BT-2214). Given the source text of a single
//! compiled method (as returned by `CompiledMethod source`) and a target
//! selector name, returns the 1-based line numbers, relative to the method
//! source, at which a `MessageSend` or `Cascade` message with that selector
//! appears.
//!
//! # Parsing strategy
//!
//! `CompiledMethod source` returns a bare method definition — the signature
//! and body, but no class header. The parser does not accept a method
//! definition at the top level, so this module wraps the input in a synthetic
//! class definition (`Object subclass: __SyntheticSendersScope\n` plus the
//! method source) before invoking the existing lexer/parser. Line numbers
//! are then translated back to the input's coordinate space by subtracting
//! the prefix line count.
//!
//! # Error handling
//!
//! Parse errors are tolerated: any sub-trees that parsed successfully still
//! contribute results. A completely unparseable source returns an empty list.
//! Callers treat "no senders found" identically to "could not parse".

use crate::ast::{Expression, Pattern, StringSegment};
use crate::source_analysis::{Span, lex_with_eof, parse};

/// Number of newlines in the synthetic class header that wraps the input.
/// Used to translate line numbers from wrapped-source to input-source space.
const PREFIX_LINES: u32 = 1;

/// The synthetic class header used to make a bare method definition parseable.
/// The single newline at the end is counted by [`PREFIX_LINES`].
const SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticSendersScope\n";

/// Find the 1-based line numbers within `method_source` where messages with
/// `selector_name` are sent.
///
/// Walks the parsed AST for [`Expression::MessageSend`] and
/// [`Expression::Cascade`] nodes whose selector matches `selector_name`. Each
/// matching send produces one entry. Multiple sends in the same line produce
/// multiple entries (one per occurrence), preserving source order.
///
/// Returns an empty vector if no senders are found or the source cannot be
/// parsed at all.
#[must_use]
pub fn find_senders_in_source(method_source: &str, selector_name: &str) -> Vec<u32> {
    let wrapped = format!("{SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut wrapped_lines = Vec::new();

    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for stmt in &method.body {
                collect_send_lines(
                    &stmt.expression,
                    selector_name,
                    &wrapped,
                    &mut wrapped_lines,
                );
            }
        }
    }

    // Sources that look like top-level expressions, or that parsed as
    // standalone `Class >> selector => body` definitions, are walked as
    // fallbacks so partial-parse cases still contribute senders.
    for stmt in &module.expressions {
        collect_send_lines(
            &stmt.expression,
            selector_name,
            &wrapped,
            &mut wrapped_lines,
        );
    }
    for smd in &module.method_definitions {
        for stmt in &smd.method.body {
            collect_send_lines(
                &stmt.expression,
                selector_name,
                &wrapped,
                &mut wrapped_lines,
            );
        }
    }

    // Translate from wrapped-source line numbers back to input-source space.
    wrapped_lines
        .into_iter()
        .map(|line| line.saturating_sub(PREFIX_LINES).max(1))
        .collect()
}

/// Recursively collect line numbers of message sends matching `selector_name`.
fn collect_send_lines(expr: &Expression, selector_name: &str, source: &str, lines: &mut Vec<u32>) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            if selector.name() == selector_name {
                lines.push(selector_line(selector, *span, source));
            }
            collect_send_lines(receiver, selector_name, source, lines);
            for arg in arguments {
                collect_send_lines(arg, selector_name, source, lines);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_send_lines(receiver, selector_name, source, lines);
            for msg in messages {
                if msg.selector.name() == selector_name {
                    lines.push(selector_line(&msg.selector, msg.span, source));
                }
                for arg in &msg.arguments {
                    collect_send_lines(arg, selector_name, source, lines);
                }
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_send_lines(target, selector_name, source, lines);
            collect_send_lines(value, selector_name, source, lines);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_send_lines(value, selector_name, source, lines);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_send_lines(&stmt.expression, selector_name, source, lines);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_send_lines(expression, selector_name, source, lines);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_send_lines(receiver, selector_name, source, lines);
        }
        Expression::Match { value, arms, .. } => {
            collect_send_lines(value, selector_name, source, lines);
            for arm in arms {
                collect_pattern_send_lines(&arm.pattern, selector_name, source, lines);
                if let Some(guard) = &arm.guard {
                    collect_send_lines(guard, selector_name, source, lines);
                }
                collect_send_lines(&arm.body, selector_name, source, lines);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_send_lines(inner, selector_name, source, lines);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_send_lines(element, selector_name, source, lines);
            }
            if let Some(tail_expr) = tail {
                collect_send_lines(tail_expr, selector_name, source, lines);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_send_lines(element, selector_name, source, lines);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_send_lines(&pair.key, selector_name, source, lines);
                collect_send_lines(&pair.value, selector_name, source, lines);
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

/// Recursively collect line numbers of message sends inside a [`Pattern`].
///
/// Patterns are themselves name-binding/literal-matching constructs and carry
/// no message sends today — `BinarySegment::size` is the only expression slot
/// in the [`Pattern`] enum, and the parser currently restricts it to an
/// integer literal or a bare identifier (see
/// `parse_binary_segment_size`). The walker is defensive: it traverses
/// container patterns (Tuple/Array/List/Map/Constructor) and the binary
/// segment size expression so that loosening the parser later does not
/// silently regress `sendersOf:`. Constructor-pattern selector keywords (e.g.
/// the `ok:` in `Result ok: v`) are *not* sends and are skipped here.
fn collect_pattern_send_lines(
    pattern: &Pattern,
    selector_name: &str,
    source: &str,
    lines: &mut Vec<u32>,
) {
    match pattern {
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_send_lines(&segment.value, selector_name, source, lines);
                if let Some(size) = &segment.size {
                    collect_send_lines(size, selector_name, source, lines);
                }
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_send_lines(element, selector_name, source, lines);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_send_lines(element, selector_name, source, lines);
            }
            if let Some(rest_pattern) = rest {
                collect_pattern_send_lines(rest_pattern, selector_name, source, lines);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_send_lines(element, selector_name, source, lines);
            }
            if let Some(tail_pattern) = tail {
                collect_pattern_send_lines(tail_pattern, selector_name, source, lines);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_send_lines(&pair.value, selector_name, source, lines);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_selector, inner) in keywords {
                collect_pattern_send_lines(inner, selector_name, source, lines);
            }
        }
        Pattern::Wildcard(..) | Pattern::Literal(..) | Pattern::Variable(..) => {}
    }
}

/// Determine the line number to report for a matching send. Uses the span of
/// the selector keywords when available (so the line points at the selector
/// token rather than the receiver, for multi-line sends) and falls back to
/// the enclosing message-send span otherwise.
fn selector_line(selector: &crate::ast::MessageSelector, fallback: Span, source: &str) -> u32 {
    selector_span(selector)
        .unwrap_or(fallback)
        .line_number(source)
}

fn selector_span(selector: &crate::ast::MessageSelector) -> Option<Span> {
    match selector {
        crate::ast::MessageSelector::Keyword(parts) if !parts.is_empty() => {
            let first = parts.first().unwrap().span;
            let last = parts.last().unwrap().span;
            Some(first.merge(last))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_single_unary_send() {
        let lines = find_senders_in_source("greet => self name asUppercase", "asUppercase");
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_keyword_send() {
        let src = "at: index put: value => self.items at: index put: value";
        let lines = find_senders_in_source(src, "at:put:");
        // One `at:put:` send in the body, on the same line as the signature.
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_binary_send() {
        let src = "double: n => n * 2\nincrementBy: n => self.value := self.value + n";
        // First method sends `*`, second sends `+`.
        let plus = find_senders_in_source(src, "+");
        assert_eq!(plus, vec![2]);
        let times = find_senders_in_source(src, "*");
        assert_eq!(times, vec![1]);
    }

    #[test]
    fn finds_multiple_sends_in_same_method() {
        // Three `printString` sends in one method body.
        let src = "report =>\n  a printString\n  b printString\n  c printString";
        let lines = find_senders_in_source(src, "printString");
        assert_eq!(lines, vec![2, 3, 4]);
    }

    #[test]
    fn finds_send_inside_block() {
        let src = "shout =>\n  [:x | x asString] value: 42";
        let lines = find_senders_in_source(src, "asString");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_in_cascade() {
        let src = "report =>\n  Transcript\n    show: \"a\";\n    show: \"b\"";
        let lines = find_senders_in_source(src, "show:");
        // Two `show:` sends on lines 3 and 4 of the cascade.
        assert_eq!(lines.len(), 2);
        assert_eq!(lines, vec![3, 4]);
    }

    #[test]
    fn returns_empty_when_no_senders() {
        let lines = find_senders_in_source("greet => self name", "absentSelector");
        assert!(lines.is_empty());
    }

    /// BT-2195: Class-side methods carry a return-type arrow in their
    /// signature (e.g. `default -> SystemNavigation =>`). The senders walker
    /// must still find sends inside the body — exercises the source shape
    /// produced by the unparser for class-side definitions.
    #[test]
    fn bt2195_finds_send_in_class_method_with_return_type() {
        let src = "default -> SystemNavigation =>\n  self new";
        let lines = find_senders_in_source(src, "new");
        assert_eq!(
            lines,
            vec![2],
            "expected to find `new` on line 2, got {lines:?}"
        );
    }

    /// BT-2195: The unparser emits doc comments (`///`) and the `@expect`
    /// directive before the body. The senders walker must still recurse into
    /// the body and find sends.
    #[test]
    fn bt2195_finds_send_in_class_method_with_doc_and_expect() {
        let src = "/// doc line\ndefault -> SystemNavigation =>\n  @expect dnu\n  self new";
        let lines = find_senders_in_source(src, "new");
        assert!(
            !lines.is_empty(),
            "expected `new` to be found, got {lines:?}"
        );
    }

    #[test]
    fn handles_unparseable_source_without_panicking() {
        // Garbage source — parser produces diagnostics but does not crash.
        let lines = find_senders_in_source(")@!", "anything");
        assert!(lines.is_empty());
    }

    #[test]
    fn constructor_pattern_keyword_is_not_a_send() {
        // `ok:` in `Result ok: v` is a constructor-pattern selector keyword,
        // not a message send. A query for `ok:` should not report it. The
        // body's `ok:` *send* still counts.
        let src = "wrap: x =>\n  x match: [\n    Result ok: v -> Result ok: v;\n    _ -> nil]";
        let lines = find_senders_in_source(src, "ok:");
        assert_eq!(lines, vec![3]);
    }

    #[test]
    fn finds_send_with_leading_doc_comment() {
        let src = "/// Doubles the receiver.\ndouble => self * 2";
        let lines = find_senders_in_source(src, "*");
        // Send appears on line 2 of the input source.
        assert_eq!(lines, vec![2]);
    }

    // BT-2196: scanning extension method source (registered via
    // `beamtalk_extensions:register/5`) should produce hits the same way
    // class-body methods do. This is the canonical shape passed by
    // `SystemNavigation collectExtensionSendersFor:into:` when iterating
    // the extension sources table.
    #[test]
    fn finds_send_in_extension_style_source() {
        let lines = find_senders_in_source("describe => self asString", "asString");
        assert_eq!(lines, vec![1]);
    }
}

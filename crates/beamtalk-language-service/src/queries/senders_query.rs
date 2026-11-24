// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Senders query — find call sites of a selector within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation sendersOf:`. Given the source text of a single
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

use super::selector_line;
use beamtalk_core::ast_walker::walk_sends;
use beamtalk_core::source_analysis::{lex_with_eof, parse};

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

    beamtalk_core::ast_walker::for_each_expr_seq(&module, |seq| {
        for stmt in seq {
            walk_sends(&stmt.expression, &mut |send| {
                if send.selector.name() == selector_name {
                    wrapped_lines.push(selector_line(send.selector, send.span, &wrapped));
                }
            });
        }
    });
    // `walk_sends` reports a cascade's messages together, ahead of the sends
    // nested in their receiver and arguments; restore source order.
    wrapped_lines.sort_unstable();

    // Translate from wrapped-source line numbers back to input-source space.
    wrapped_lines
        .into_iter()
        .map(|line| line.saturating_sub(PREFIX_LINES).max(1))
        .collect()
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
    fn cascade_sends_are_reported_in_source_order() {
        // A cascade's messages are reported together by the walker, ahead of the
        // send nested in the first message's argument; lines stay in source order.
        let src = "run =>\n  self log: (self fmt)\n    ; other: 1\n    ; fmt";
        // `fmt` is sent inside the first argument (line 2) and as the last cascade
        // message (line 4).
        assert_eq!(find_senders_in_source(src, "fmt"), vec![2, 4]);
        assert_eq!(find_senders_in_source(src, "other:"), vec![3]);
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

    /// Class-side methods carry a return-type arrow in their
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

    /// The unparser emits doc comments (`///`) and the `@expect`
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

    // Scanning extension method source (registered via
    // `beamtalk_extensions:register/5`) should produce hits the same way
    // class-body methods do. This is the canonical shape passed by
    // `SystemNavigation collectExtensionSendersFor:into:` when iterating
    // the extension sources table.
    #[test]
    fn finds_send_in_extension_style_source() {
        let lines = find_senders_in_source("describe => self asString", "asString");
        assert_eq!(lines, vec![1]);
    }

    // -----------------------------------------------------------------------
    // Expression walker: coverage for arms not reached by the tests above
    // -----------------------------------------------------------------------

    #[test]
    fn finds_send_inside_return_expression() {
        // `^ x asString` is `Expression::Return { value: MessageSend{asString} }`.
        // The walker must recurse into the return value.
        let src = "bail: x =>\n  x = 0 ifTrue: [^ x asString]\n  x printString";
        let lines = find_senders_in_source(src, "asString");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_inside_destructure_assignment_value() {
        // `{a, b} := self items` — the send is in the *value* position of a
        // DestructureAssignment and must still be collected.
        let src = "unpack =>\n  {a, b} := self items\n  a + b";
        let lines = find_senders_in_source(src, "items");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_in_parenthesized_expression() {
        // `(self name)` is `Expression::Parenthesized`. The walker must recurse
        // into the inner expression.
        let src = "describe =>\n  (self name) asUppercase";
        let lines = find_senders_in_source(src, "name");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_in_match_guard() {
        // `x when: [x printString = "0"]` — the guard block is walked by
        // `collect_send_lines`, exercising the `arm.guard` branch.
        let src = "classify: n =>\n  n match: [x when: [x printString = \"0\"] -> x; _ -> nil]";
        let lines = find_senders_in_source(src, "printString");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_in_string_interpolation() {
        // `"Hello, {name asUppercase}!"` is `Expression::StringInterpolation`.
        // The walker must recurse into each interpolation segment.
        let src = "greet: name =>\n  \"Hello, {name asUppercase}!\"";
        let lines = find_senders_in_source(src, "asUppercase");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_in_list_literal_element_and_tail() {
        // `#(x size | self name)` — `x size` is the element and `self name` is
        // the cons tail. Both the element loop and the tail branch are exercised.
        let src = "build: x =>\n  #(x size | self name)";
        let size_lines = find_senders_in_source(src, "size");
        assert_eq!(size_lines, vec![2]);
        let name_lines = find_senders_in_source(src, "name");
        assert_eq!(name_lines, vec![2]);
    }

    #[test]
    fn finds_send_in_array_literal() {
        // `#[self name, 42]` is `Expression::ArrayLiteral`. The walker must
        // recurse into each element.
        let src = "wrap =>\n  #[self name, 42]";
        let lines = find_senders_in_source(src, "name");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_in_tuple_literal() {
        // `{self name, 42}` is `Expression::TupleLiteral`. The walker must
        // recurse into each element.
        let src = "pair =>\n  {self name, 42}";
        let lines = find_senders_in_source(src, "name");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_send_in_map_literal() {
        // `#{#key => self name}` is `Expression::MapLiteral`. The walker must
        // recurse into both key and value expressions.
        let src = "info =>\n  #{#key => self name}";
        let lines = find_senders_in_source(src, "name");
        assert_eq!(lines, vec![2]);
    }

    // -----------------------------------------------------------------------
    // Pattern walker: coverage for arms not reached by the tests above
    // (Pattern::Constructor is already covered by
    // `constructor_pattern_keyword_is_not_a_send`.)
    // -----------------------------------------------------------------------

    #[test]
    fn match_binary_pattern_body_send_found() {
        // `<<v:8, _/binary>>` is `Pattern::Binary`. The pattern walker must
        // traverse the segments without panicking, and the arm body send must
        // be collected.
        let src = "unpack: bytes =>\n  bytes match: [<<v:8, _/binary>> -> v printString; _ -> nil]";
        let lines = find_senders_in_source(src, "printString");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn match_tuple_pattern_body_send_found() {
        // `{a, b}` is `Pattern::Tuple`. The pattern walker traverses the
        // elements (Variable patterns, no sends), and the arm body send is
        // still collected.
        let src = "check: r =>\n  r match: [{a, b} -> a asString; _ -> nil]";
        let lines = find_senders_in_source(src, "asString");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn match_array_pattern_with_rest_body_send_found() {
        // `#[h, ...rest]` is `Pattern::Array` with a rest element. Both the
        // element loop and the rest-pattern branch are exercised; the arm body
        // send is still collected.
        let src = "get: xs =>\n  xs match: [#[h, ...rest] -> h asString; _ -> nil]";
        let lines = find_senders_in_source(src, "asString");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn match_map_pattern_body_send_found() {
        // `#{#name => n}` is `Pattern::Map`. The pattern walker traverses the
        // value bindings, and the arm body send is collected.
        let src = "get: m =>\n  m match: [#{#name => n} -> n asString; _ -> nil]";
        let lines = find_senders_in_source(src, "asString");
        assert_eq!(lines, vec![2]);
    }
}

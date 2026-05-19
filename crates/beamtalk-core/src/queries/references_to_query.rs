// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! References-to query — find class-reference sites within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation referencesTo:` (BT-2203). Where `sendersOf:` answers
//! "who calls this method?", `referencesTo:` answers "who mentions this class?".
//! Given the source text of a single compiled method (as returned by
//! `CompiledMethod source`) and a target class name, returns the 1-based line
//! numbers, relative to the method source, at which an
//! [`Expression::ClassReference`] node with that name appears.
//!
//! # Parsing strategy
//!
//! Reuses the synthetic-class-header trick from
//! [`crate::queries::senders_query`]: a bare method definition is not a valid
//! top-level form, so the input is wrapped in
//! `Object subclass: __SyntheticReferencesScope\n` before lexing/parsing.
//! Line numbers are translated back to input-source space by subtracting the
//! prefix line count. The synthetic header references
//! `__SyntheticReferencesScope` — a name that cannot collide with a user or
//! stdlib class — so the wrapper itself never contributes a false-positive
//! reference, regardless of which class the caller is asking about.
//!
//! # Error handling
//!
//! Parse errors are tolerated: any sub-trees that parsed successfully still
//! contribute results. A completely unparseable source returns an empty list.
//! Callers treat "no references found" identically to "could not parse".

use crate::ast::{Expression, Pattern, StringSegment, TypeAnnotation};
use crate::source_analysis::{lex_with_eof, parse};

/// Number of newlines in the synthetic class header that wraps the input.
/// Used to translate line numbers from wrapped-source to input-source space.
const PREFIX_LINES: u32 = 1;

/// The synthetic class header used to make a bare method definition parseable.
/// The single newline at the end is counted by [`PREFIX_LINES`]. The class
/// name uses a leading underscore so it cannot collide with a real class.
const SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticReferencesScope\n";

/// Find the 1-based line numbers within `method_source` where a class
/// reference with name `class_name` appears.
///
/// Walks the parsed AST for [`Expression::ClassReference`] nodes whose `name`
/// matches `class_name`. Each matching reference produces one entry. Multiple
/// references in the same line produce multiple entries (one per occurrence),
/// preserving source order.
///
/// Also walks [`TypeAnnotation`] nodes attached to parameter / state / return
/// types so a class mentioned only in a type signature (e.g.
/// `param :: Counter -> Integer =>`) is still found.
///
/// Returns an empty vector if no references are found or the source cannot be
/// parsed at all.
#[must_use]
pub fn find_references_to_in_source(method_source: &str, class_name: &str) -> Vec<u32> {
    let wrapped = format!("{SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut wrapped_lines = Vec::new();

    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for param in &method.parameters {
                if let Some(ann) = &param.type_annotation {
                    collect_type_lines(ann, class_name, &wrapped, &mut wrapped_lines);
                }
            }
            if let Some(ret) = &method.return_type {
                collect_type_lines(ret, class_name, &wrapped, &mut wrapped_lines);
            }
            for stmt in &method.body {
                collect_reference_lines(&stmt.expression, class_name, &wrapped, &mut wrapped_lines);
            }
        }
    }

    // Sources that look like top-level expressions, or that parsed as
    // standalone `Class >> selector => body` definitions, are walked as
    // fallbacks so partial-parse cases still contribute references.
    for stmt in &module.expressions {
        collect_reference_lines(&stmt.expression, class_name, &wrapped, &mut wrapped_lines);
    }
    for smd in &module.method_definitions {
        for param in &smd.method.parameters {
            if let Some(ann) = &param.type_annotation {
                collect_type_lines(ann, class_name, &wrapped, &mut wrapped_lines);
            }
        }
        if let Some(ret) = &smd.method.return_type {
            collect_type_lines(ret, class_name, &wrapped, &mut wrapped_lines);
        }
        for stmt in &smd.method.body {
            collect_reference_lines(&stmt.expression, class_name, &wrapped, &mut wrapped_lines);
        }
    }

    // Translate from wrapped-source line numbers back to input-source space.
    // Drop any line that falls inside the synthetic wrapper header (line
    // numbers <= PREFIX_LINES). Using `.max(1)` here would silently collapse
    // wrapper-region matches onto line 1 of the user's source, masking them
    // as a real reference at the top of the method.
    wrapped_lines
        .into_iter()
        .filter(|&line| line > PREFIX_LINES)
        .map(|line| line - PREFIX_LINES)
        .collect()
}

/// Recursively collect line numbers of class references matching `class_name`.
fn collect_reference_lines(
    expr: &Expression,
    class_name: &str,
    source: &str,
    lines: &mut Vec<u32>,
) {
    match expr {
        Expression::ClassReference { name, span, .. } => {
            if name.name.as_str() == class_name {
                lines.push(span.line_number(source));
            }
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_reference_lines(receiver, class_name, source, lines);
            for arg in arguments {
                collect_reference_lines(arg, class_name, source, lines);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_reference_lines(receiver, class_name, source, lines);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_reference_lines(arg, class_name, source, lines);
                }
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_reference_lines(target, class_name, source, lines);
            collect_reference_lines(value, class_name, source, lines);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_reference_lines(value, class_name, source, lines);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_reference_lines(&stmt.expression, class_name, source, lines);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_reference_lines(expression, class_name, source, lines);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_reference_lines(receiver, class_name, source, lines);
        }
        Expression::Match { value, arms, .. } => {
            collect_reference_lines(value, class_name, source, lines);
            for arm in arms {
                collect_pattern_reference_lines(&arm.pattern, class_name, source, lines);
                if let Some(guard) = &arm.guard {
                    collect_reference_lines(guard, class_name, source, lines);
                }
                collect_reference_lines(&arm.body, class_name, source, lines);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_reference_lines(inner, class_name, source, lines);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_reference_lines(element, class_name, source, lines);
            }
            if let Some(tail_expr) = tail {
                collect_reference_lines(tail_expr, class_name, source, lines);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_reference_lines(element, class_name, source, lines);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_reference_lines(&pair.key, class_name, source, lines);
                collect_reference_lines(&pair.value, class_name, source, lines);
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

/// Recursively collect line numbers of class references inside a [`Pattern`].
///
/// `Constructor` patterns carry an explicit class name (e.g. `Result ok: v`);
/// container patterns recurse into their nested patterns so a class mentioned
/// only inside a nested constructor (e.g. `Result ok: (Wrapper value: v)`) is
/// still reported. `BinarySegment::size` is a full expression and is walked
/// via `collect_reference_lines`.
fn collect_pattern_reference_lines(
    pattern: &Pattern,
    class_name: &str,
    source: &str,
    lines: &mut Vec<u32>,
) {
    match pattern {
        Pattern::Constructor {
            class, keywords, ..
        } => {
            if class.name.as_str() == class_name {
                lines.push(class.span.line_number(source));
            }
            for (_selector, inner) in keywords {
                collect_pattern_reference_lines(inner, class_name, source, lines);
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_reference_lines(element, class_name, source, lines);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_reference_lines(element, class_name, source, lines);
            }
            if let Some(rest_pattern) = rest {
                collect_pattern_reference_lines(rest_pattern, class_name, source, lines);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_reference_lines(element, class_name, source, lines);
            }
            if let Some(tail_pattern) = tail {
                collect_pattern_reference_lines(tail_pattern, class_name, source, lines);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_reference_lines(&pair.value, class_name, source, lines);
            }
        }
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_reference_lines(&segment.value, class_name, source, lines);
                if let Some(size) = &segment.size {
                    collect_reference_lines(size, class_name, source, lines);
                }
            }
        }
        Pattern::Wildcard(..) | Pattern::Literal(..) | Pattern::Variable(..) => {}
    }
}

/// Collect class references inside a [`TypeAnnotation`]. Class names appear as
/// the head of `Simple`/`Generic`/`ClassOf` types — recurse into parameters,
/// unions, and false-or wrappers so types like `List(Counter)` and
/// `Counter | False` are covered.
fn collect_type_lines(
    annotation: &TypeAnnotation,
    class_name: &str,
    source: &str,
    lines: &mut Vec<u32>,
) {
    match annotation {
        TypeAnnotation::Simple(id) => {
            if id.name.as_str() == class_name {
                lines.push(id.span.line_number(source));
            }
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            if base.name.as_str() == class_name {
                lines.push(base.span.line_number(source));
            }
            for param in parameters {
                collect_type_lines(param, class_name, source, lines);
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for ty in types {
                collect_type_lines(ty, class_name, source, lines);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            collect_type_lines(inner, class_name, source, lines);
        }
        TypeAnnotation::ClassOf {
            class_name: class_id,
            ..
        } => {
            if class_id.name.as_str() == class_name {
                lines.push(class_id.span.line_number(source));
            }
        }
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_class_reference_in_send() {
        // `Integer new` — the receiver is a ClassReference.
        let lines = find_references_to_in_source("make => Integer new", "Integer");
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_class_reference_as_argument() {
        let src = "check: x => x isKindOf: Counter";
        let lines = find_references_to_in_source(src, "Counter");
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_multiple_references_in_one_method() {
        let src = "buildPair =>\n  a := Counter new\n  b := Counter new\n  Counter reset";
        let lines = find_references_to_in_source(src, "Counter");
        assert_eq!(lines, vec![2, 3, 4]);
    }

    #[test]
    fn finds_reference_inside_block() {
        let src = "spawn =>\n  [:x | Counter new value: x] value: 42";
        let lines = find_references_to_in_source(src, "Counter");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_reference_in_cascade_argument() {
        let src = "log =>\n  Transcript\n    show: Counter new printString;\n    show: \"!\"";
        let lines = find_references_to_in_source(src, "Counter");
        // Counter appears once on line 3 (the cascade's first show: argument).
        // Transcript on line 2 is a different class.
        assert_eq!(lines, vec![3]);
    }

    #[test]
    fn finds_reference_in_list_literal() {
        let src = "classes => #(Integer, Float, String)";
        let lines = find_references_to_in_source(src, "Float");
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_reference_in_map_literal() {
        let src = "kinds => #{#int => Integer, #str => String}";
        let lines = find_references_to_in_source(src, "String");
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn returns_empty_when_no_references() {
        let lines = find_references_to_in_source("greet => self name", "Counter");
        assert!(lines.is_empty());
    }

    #[test]
    fn handles_unparseable_source_without_panicking() {
        let lines = find_references_to_in_source(")@!", "Counter");
        assert!(lines.is_empty());
    }

    #[test]
    fn does_not_match_lowercase_identifier_of_same_name() {
        // A local variable named `counter` is parsed as an Identifier, not a
        // ClassReference, so it must not match a query for `Counter`.
        let src = "use => counter := 1\n  counter + 1";
        let lines = find_references_to_in_source(src, "counter");
        assert!(lines.is_empty());
    }

    #[test]
    fn finds_reference_in_return_type_annotation() {
        let src = "make -> Counter => Counter new";
        // Counter appears twice on line 1: once in the return type, once in
        // the body. Both should be reported.
        let lines = find_references_to_in_source(src, "Counter");
        assert_eq!(lines.len(), 2);
        assert_eq!(lines, vec![1, 1]);
    }

    #[test]
    fn finds_reference_in_parameter_type_annotation() {
        let src = "use: x :: Counter -> Integer =>\n  x value";
        let lines = find_references_to_in_source(src, "Counter");
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn finds_reference_in_generic_type_parameter() {
        let src = "items -> List(Counter) =>\n  List new";
        let lines = find_references_to_in_source(src, "Counter");
        assert_eq!(lines, vec![1]);
    }

    #[test]
    fn does_not_match_synthetic_wrapper_class() {
        // Querying for the synthetic wrapper class name must return nothing —
        // the wrapper is an implementation detail, not a real reference.
        let lines = find_references_to_in_source("noop => self", "__SyntheticReferencesScope");
        assert!(lines.is_empty());
    }

    #[test]
    fn finds_reference_with_leading_doc_comment() {
        let src = "/// Builds an Integer.\nmake => Integer new";
        let lines = find_references_to_in_source(src, "Integer");
        assert_eq!(lines, vec![2]);
    }

    #[test]
    fn finds_reference_in_match_arm_pattern() {
        // The only mention of `Result` is inside a match-arm constructor
        // pattern. Without walking patterns this would be missed.
        let src = "classify: x =>\n  x match: [\n    Result ok: v -> v;\n    _ -> nil]";
        let lines = find_references_to_in_source(src, "Result");
        assert_eq!(lines, vec![3]);
    }

    #[test]
    fn finds_reference_in_nested_match_arm_pattern() {
        // Class appears only inside a nested constructor pattern.
        let src =
            "unwrap: x =>\n  x match: [\n    Result ok: (Wrapper value: v) -> v;\n    _ -> nil]";
        let lines = find_references_to_in_source(src, "Wrapper");
        assert_eq!(lines, vec![3]);
    }

    #[test]
    fn does_not_match_object_in_synthetic_header() {
        // The synthetic wrapper begins with `Object subclass: __Synthetic...`.
        // `Object` is part of the class definition header (not a method body),
        // so it must not contribute a false-positive reference when the caller
        // queries for `Object`.
        let lines = find_references_to_in_source("noop => self", "Object");
        assert!(lines.is_empty());
    }
}

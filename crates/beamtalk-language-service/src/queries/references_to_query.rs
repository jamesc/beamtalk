// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! References-to query — find class-reference sites within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation referencesTo:`. Where `sendersOf:` answers
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
//!
//! # Implementation note
//!
//! [`ReferenceHit`] and [`find_all_references_in_source`] (the unfiltered
//! all-references walker used by xref codegen) live in
//! [`beamtalk_core::method_source_walker`], a shared leaf below both this Language
//! Service context and Code Generation. They are re-exported here so
//! Language Service callers can keep their existing import paths.

pub use beamtalk_core::method_source_walker::{ReferenceHit, find_all_references_in_source};

use beamtalk_core::ast::{Expression, Pattern, TypeAnnotation};
use beamtalk_core::ast_walker::{WalkNode, walk_expression_and_patterns};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

const PREFIX_LINES: u32 = 1;
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

    // The module is walked class methods first, then top-level expressions, and
    // a match arm's patterns are reported ahead of its scrutinee; restore source
    // order.
    wrapped_lines.sort_unstable();
    wrapped_lines
        .into_iter()
        .filter(|&line| line > PREFIX_LINES)
        .map(|line| line - PREFIX_LINES)
        .collect()
}

/// Collect line numbers of class references matching `class_name` in `expr`.
///
/// A closure over [`walk_expression_and_patterns`]: a `ClassReference`
/// expression, a `Constructor` pattern's class (e.g. `Result ok: v`) and a
/// `Type` pattern's class are the leaf cases.
fn collect_reference_lines(
    expr: &Expression,
    class_name: &str,
    source: &str,
    lines: &mut Vec<u32>,
) {
    walk_expression_and_patterns(expr, &mut |node| match node {
        WalkNode::Expr(Expression::ClassReference { name, span, .. }) => {
            if name.name.as_str() == class_name {
                lines.push(span.line_number(source));
            }
        }
        WalkNode::Pattern(Pattern::Constructor { class, .. } | Pattern::Type { class, .. }) => {
            if class.name.as_str() == class_name {
                lines.push(class.span.line_number(source));
            }
        }
        WalkNode::Expr(_) | WalkNode::Pattern(_) => {}
    });
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
        TypeAnnotation::Difference { base, excluded, .. } => {
            collect_type_lines(base, class_name, source, lines);
            collect_type_lines(excluded, class_name, source, lines);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            collect_type_lines(left, class_name, source, lines);
            collect_type_lines(right, class_name, source, lines);
        }
        TypeAnnotation::ClassOf {
            class_name: class_id,
            ..
        } => {
            if class_id.name.as_str() == class_name {
                lines.push(class_id.span.line_number(source));
            }
        }
        TypeAnnotation::Shape { fields, .. } => {
            for field in fields {
                collect_type_lines(&field.value, class_name, source, lines);
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

    // BT-4908: one table for the `find_references_to_in_source` cases.
    // Rows are `(name, source, class, expected lines)`.
    const CASES: &[(&str, &str, &str, &[u32])] = &[
        // `Integer new` — the receiver is a ClassReference.
        (
            "finds_class_reference_in_send",
            "make => Integer new",
            "Integer",
            &[1],
        ),
        (
            "finds_class_reference_as_argument",
            "check: x => x isKindOf: Counter",
            "Counter",
            &[1],
        ),
        (
            "finds_multiple_references_in_one_method",
            "buildPair =>\n  a := Counter new\n  b := Counter new\n  Counter reset",
            "Counter",
            &[2, 3, 4],
        ),
        (
            "finds_reference_inside_block",
            "spawn =>\n  [:x | Counter new value: x] value: 42",
            "Counter",
            &[2],
        ),
        // Counter appears once on line 3 (the cascade's first show: argument).
        // Transcript on line 2 is a different class.
        (
            "finds_reference_in_cascade_argument",
            "log =>\n  Transcript\n    show: Counter new printString;\n    show: \"!\"",
            "Counter",
            &[3],
        ),
        (
            "finds_reference_in_list_literal",
            "classes => #(Integer, Float, String)",
            "Float",
            &[1],
        ),
        (
            "finds_reference_in_map_literal",
            "kinds => #{#int => Integer, #str => String}",
            "String",
            &[1],
        ),
        (
            "returns_empty_when_no_references",
            "greet => self name",
            "Counter",
            &[],
        ),
        (
            "handles_unparseable_source_without_panicking",
            ")@!",
            "Counter",
            &[],
        ),
        // A local variable named `counter` is parsed as an Identifier, not a
        // ClassReference, so it must not match a query for `Counter`.
        (
            "does_not_match_lowercase_identifier_of_same_name",
            "use => counter := 1\n  counter + 1",
            "counter",
            &[],
        ),
        // Counter appears twice on line 1: once in the return type, once in
        // the body. Both should be reported.
        (
            "finds_reference_in_return_type_annotation",
            "make -> Counter => Counter new",
            "Counter",
            &[1, 1],
        ),
        (
            "finds_reference_in_parameter_type_annotation",
            "use: x :: Counter -> Integer =>\n  x value",
            "Counter",
            &[1],
        ),
        (
            "finds_reference_in_generic_type_parameter",
            "items -> List(Counter) =>\n  List new",
            "Counter",
            &[1],
        ),
        // Querying for the synthetic wrapper class name must return nothing —
        // the wrapper is an implementation detail, not a real reference.
        (
            "does_not_match_synthetic_wrapper_class",
            "noop => self",
            "__SyntheticReferencesScope",
            &[],
        ),
        (
            "finds_reference_with_leading_doc_comment",
            "/// Builds an Integer.\nmake => Integer new",
            "Integer",
            &[2],
        ),
        // The only mention of `Result` is inside a match-arm constructor
        // pattern. Without walking patterns this would be missed.
        (
            "finds_reference_in_match_arm_pattern",
            "classify: x =>\n  x match: [\n    Result ok: v -> v;\n    _ -> nil]",
            "Result",
            &[3],
        ),
        // The synthetic wrapper begins with `Object subclass: __Synthetic...`.
        // `Object` is part of the class definition header (not a method body),
        // so it must not contribute a false-positive reference when the caller
        // queries for `Object`.
        (
            "does_not_match_object_in_synthetic_header",
            "noop => self",
            "Object",
            &[],
        ),
        (
            "finds_reference_in_destructure_assignment",
            "make =>\n  {a, b} := Counter new",
            "Counter",
            &[2],
        ),
        (
            "finds_reference_in_early_return",
            "get: flag =>\n  flag ifTrue: [^ Counter new]\n  nil",
            "Counter",
            &[2],
        ),
        // The FieldAccess branch recurses into the receiver; Counter is found
        // in a later statement on the same method body walk.
        (
            "walks_through_field_access_to_find_reference",
            "get =>\n  x := self.count\n  Counter new",
            "Counter",
            &[3],
        ),
        (
            "finds_reference_in_match_arm_guard",
            "classify: x =>\n  x match: [\n    v when: [v isKindOf: Counter] -> true;\n    _ -> false]",
            "Counter",
            &[3],
        ),
        (
            "finds_reference_in_string_interpolation",
            "describe =>\n  \"count: {Counter new}\"",
            "Counter",
            &[2],
        ),
        (
            "finds_reference_in_binary_construction",
            "encode =>\n  <<(Counter new):8>>",
            "Counter",
            &[2],
        ),
        (
            "finds_reference_in_list_literal_tail",
            "cons =>\n  #(1 | Counter new)",
            "Counter",
            &[2],
        ),
        (
            "finds_reference_in_array_literal",
            "arr =>\n  #[Counter new, 1]",
            "Counter",
            &[2],
        ),
        (
            "finds_reference_in_tuple_literal",
            "tup =>\n  {Counter new, 1}",
            "Counter",
            &[2],
        ),
        // Pattern::Tuple containing a Constructor pattern referencing Result.
        (
            "finds_reference_in_match_pattern_tuple",
            "classify: x =>\n  x match: [\n    {Result ok: v, 1} -> v;\n    _ -> nil]",
            "Result",
            &[3],
        ),
        // Pattern::Array containing a Constructor pattern referencing Result.
        (
            "finds_reference_in_match_pattern_array",
            "classify: x =>\n  x match: [\n    #[Result ok: v] -> v;\n    _ -> nil]",
            "Result",
            &[3],
        ),
        // Pattern::Map branch is exercised; Counter is found in the match value.
        (
            "walks_through_map_pattern_finds_reference_in_value",
            "classify =>\n  (Counter new) match: [\n    #{#key => v} -> v;\n    _ -> nil]",
            "Counter",
            &[2],
        ),
        // Pattern::Binary branch is exercised; Counter is found in the match value.
        (
            "walks_through_binary_pattern_finds_reference_in_value",
            "classify =>\n  (Counter new) match: [\n    <<_:8>> -> nil;\n    _ -> nil]",
            "Counter",
            &[2],
        ),
        // Pattern::Type carries a class name and is reported when it matches.
        (
            "finds_reference_in_match_pattern_type",
            "classify: x =>\n  x match: [\n    v :: Counter -> v;\n    _ -> nil]",
            "Counter",
            &[3],
        ),
        (
            "finds_reference_in_union_type_annotation",
            "use: x :: Counter | Integer -> Boolean =>\n  true",
            "Counter",
            &[1],
        ),
        // The existing generic-parameter test covers `List(Counter)`.
        // This test covers the base (`Counter`) in `Counter(Integer)`.
        (
            "finds_reference_in_generic_base_type_annotation",
            "use: x :: Counter(Integer) -> Boolean =>\n  true",
            "Counter",
            &[1],
        ),
        (
            "finds_reference_in_difference_type_annotation",
            "use: x :: Counter \\ Integer -> Boolean =>\n  true",
            "Counter",
            &[1],
        ),
        (
            "finds_reference_in_intersection_type_annotation",
            "use: x :: Counter & Integer -> Boolean =>\n  true",
            "Counter",
            &[1],
        ),
        (
            "finds_reference_in_class_of_type_annotation",
            "use: cls :: Counter class -> Boolean =>\n  true",
            "Counter",
            &[1],
        ),
        // `Counter >> selector` — the walker processes param types, return
        // type, and body; Counter appears in param type (line 1), return type
        // (line 1), and the body (line 2).
        (
            "finds_reference_in_standalone_method_definition",
            "Counter >> increment: x :: Counter -> Counter =>\n  Counter new",
            "Counter",
            &[1, 1, 2],
        ),
    ];

    #[test]
    fn find_references_to_in_source_cases() {
        for (name, src, class, expected) in CASES {
            assert_eq!(
                find_references_to_in_source(src, class),
                *expected,
                "[{name}] {src:?}"
            );
        }
    }

    #[test]
    fn finds_reference_in_nested_match_arm_pattern() {
        // Class appears only inside a nested constructor pattern.
        let src =
            "unwrap: x =>\n  x match: [\n    Result ok: (Wrapper value: v) -> v;\n    _ -> nil]";
        let lines = find_references_to_in_source(src, "Wrapper");
        assert_eq!(lines, vec![3]);
    }

    /// Class-side method sources (as unparsed for `class_method_source`)
    /// carry the same shape as instance methods — no `class ` prefix is emitted
    /// by `unparse_method`. The references walker must still find class
    /// references in their bodies and signatures.
    #[test]
    fn bt2195_finds_reference_in_class_method_source() {
        let src = "/// doc\ndefault -> SystemNavigation =>\n  @expect dnu\n  self new";
        let lines = find_references_to_in_source(src, "SystemNavigation");
        // The return-type annotation on line 2 references SystemNavigation.
        assert!(
            !lines.is_empty(),
            "expected at least one reference, got {lines:?}"
        );
    }

    // ADR 0087 Phase 2: find_all_references_in_source collects every
    // class reference in one pass for the xref codegen.

    #[test]
    fn all_refs_collects_multiple_distinct_classes() {
        let src = "build =>\n  a := Counter new\n  b := Integer new";
        let hits = find_all_references_in_source(src);
        let names: Vec<&str> = hits.iter().map(|h| h.class.as_str()).collect();
        assert!(names.contains(&"Counter"), "got {hits:?}");
        assert!(names.contains(&"Integer"), "got {hits:?}");
        let counter = hits.iter().find(|h| h.class == "Counter").unwrap();
        assert_eq!(counter.line, 2);
        let integer = hits.iter().find(|h| h.class == "Integer").unwrap();
        assert_eq!(integer.line, 3);
    }

    #[test]
    fn all_refs_collects_type_annotation_references() {
        let src = "use: x :: Counter -> Integer =>\n  x value";
        let hits = find_all_references_in_source(src);
        let names: Vec<&str> = hits.iter().map(|h| h.class.as_str()).collect();
        assert!(names.contains(&"Counter"), "got {hits:?}");
        assert!(names.contains(&"Integer"), "got {hits:?}");
    }

    #[test]
    fn all_refs_does_not_leak_synthetic_wrapper() {
        // The wrapper header `Object subclass: __SyntheticReferencesScope` must
        // never appear in the all-references output.
        let hits = find_all_references_in_source("noop => self");
        let names: Vec<&str> = hits.iter().map(|h| h.class.as_str()).collect();
        assert!(
            !names.contains(&"Object"),
            "wrapper Object leaked: {hits:?}"
        );
        assert!(
            !names.contains(&"__SyntheticReferencesScope"),
            "wrapper class leaked: {hits:?}"
        );
    }

    #[test]
    fn all_refs_parity_with_filtered_query() {
        // For any class queried via the filtered API, the all-references walker
        // must report the same line numbers (one ReferenceHit per occurrence).
        let src = "buildPair =>\n  a := Counter new\n  b := Counter new\n  Counter reset";
        let filtered = find_references_to_in_source(src, "Counter");
        let all: Vec<u32> = find_all_references_in_source(src)
            .into_iter()
            .filter(|h| h.class == "Counter")
            .map(|h| h.line)
            .collect();
        assert_eq!(filtered, all, "filtered vs all-references line mismatch");
    }

    #[test]
    fn all_refs_empty_when_no_references() {
        let hits = find_all_references_in_source("greet => self name");
        assert!(hits.is_empty(), "got {hits:?}");
    }
}

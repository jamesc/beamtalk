// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for `type Name = ...` type alias declarations (ADR 0108, Phase 1, BT-2894).
use super::*;

// ==========================================================================
// Valid declarations
// ==========================================================================

#[test]
fn parse_simple_type_alias() {
    let module = parse_ok("type Port = Integer");
    assert_eq!(module.type_aliases.len(), 1);
    let alias = &module.type_aliases[0];
    assert_eq!(alias.name.name, "Port");
    assert!(matches!(&alias.annotation, TypeAnnotation::Simple(id) if id.name == "Integer"));
}

#[test]
fn parse_singleton_union_type_alias() {
    let module = parse_ok("type RestartStrategy = #temporary | #transient | #permanent");
    assert_eq!(module.type_aliases.len(), 1);
    let alias = &module.type_aliases[0];
    assert_eq!(alias.name.name, "RestartStrategy");
    let TypeAnnotation::Union { types, .. } = &alias.annotation else {
        panic!("expected union, got {:?}", alias.annotation);
    };
    assert_eq!(types.len(), 3);
    for (ty, expected) in types.iter().zip(["temporary", "transient", "permanent"]) {
        match ty {
            TypeAnnotation::Singleton { name, .. } => assert_eq!(name.as_str(), expected),
            other => panic!("expected singleton #{expected}, got {other:?}"),
        }
    }
}

#[test]
fn parse_plain_union_type_alias() {
    let module = parse_ok("type JsonKey = String | Symbol");
    let alias = &module.type_aliases[0];
    assert_eq!(alias.name.name, "JsonKey");
    let TypeAnnotation::Union { types, .. } = &alias.annotation else {
        panic!("expected union, got {:?}", alias.annotation);
    };
    assert_eq!(types.len(), 2);
    assert!(matches!(&types[0], TypeAnnotation::Simple(id) if id.name == "String"));
    assert!(matches!(&types[1], TypeAnnotation::Simple(id) if id.name == "Symbol"));
}

#[test]
fn parse_generic_type_alias() {
    let module = parse_ok("type IntList = List(Integer)");
    let alias = &module.type_aliases[0];
    assert_eq!(alias.name.name, "IntList");
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = &alias.annotation
    else {
        panic!("expected generic, got {:?}", alias.annotation);
    };
    assert_eq!(base.name, "List");
    assert_eq!(parameters.len(), 1);
    assert!(matches!(&parameters[0], TypeAnnotation::Simple(id) if id.name == "Integer"));
}

#[test]
fn parse_difference_type_alias() {
    let module = parse_ok("type PublicTag = Symbol \\ (#reserved | #internal)");
    let alias = &module.type_aliases[0];
    assert_eq!(alias.name.name, "PublicTag");
    let TypeAnnotation::Difference { base, excluded, .. } = &alias.annotation else {
        panic!("expected difference, got {:?}", alias.annotation);
    };
    assert!(matches!(base.as_ref(), TypeAnnotation::Simple(id) if id.name == "Symbol"));
    assert!(matches!(excluded.as_ref(), TypeAnnotation::Union { .. }));
}

#[test]
fn parse_intersection_type_alias() {
    let module = parse_ok("type Both = Printable & Comparable");
    let alias = &module.type_aliases[0];
    assert_eq!(alias.name.name, "Both");
    let TypeAnnotation::Intersection { left, right, .. } = &alias.annotation else {
        panic!("expected intersection, got {:?}", alias.annotation);
    };
    assert!(matches!(left.as_ref(), TypeAnnotation::Simple(id) if id.name == "Printable"));
    assert!(matches!(right.as_ref(), TypeAnnotation::Simple(id) if id.name == "Comparable"));
}

#[test]
fn parse_type_alias_doc_comment() {
    let module = parse_ok(
        "/// How a supervised child restarts after exit.\ntype RestartStrategy = #temporary | #transient | #permanent",
    );
    let alias = &module.type_aliases[0];
    assert_eq!(
        alias.doc_comment.as_deref(),
        Some("How a supervised child restarts after exit.")
    );
}

#[test]
fn parse_multiple_type_aliases() {
    let module = parse_ok("type Port = Integer\ntype Timeout = Integer | #infinity");
    assert_eq!(module.type_aliases.len(), 2);
    assert_eq!(module.type_aliases[0].name.name, "Port");
    assert_eq!(module.type_aliases[1].name.name, "Timeout");
}

#[test]
fn parse_type_alias_before_class_definition() {
    // A `type` declaration must terminate correctly and not be swallowed by
    // a following (or preceding) class definition's body.
    let module = parse_ok(
        "type Direction = #north | #south\n\nActor subclass: Compass\n  heading => #north",
    );
    assert_eq!(module.type_aliases.len(), 1);
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].name.name, "Compass");
}

#[test]
fn parse_type_alias_after_class_definition() {
    let module = parse_ok(
        "Actor subclass: Compass\n  heading => #north\n\ntype Direction = #north | #south",
    );
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.type_aliases.len(), 1);
    assert_eq!(module.type_aliases[0].name.name, "Direction");
}

// ==========================================================================
// `internal` modifier (ADR 0071, ADR 0108 Phase 5, BT-2898)
// ==========================================================================

#[test]
fn parse_internal_type_alias() {
    let module = parse_ok("internal type ParserState = Integer | String");
    assert_eq!(module.type_aliases.len(), 1);
    let alias = &module.type_aliases[0];
    assert_eq!(alias.name.name, "ParserState");
    assert!(alias.is_internal);
}

#[test]
fn parse_public_type_alias_is_not_internal() {
    let module = parse_ok("type Port = Integer");
    assert!(!module.type_aliases[0].is_internal);
}

#[test]
fn parse_internal_type_alias_with_doc_comment() {
    // Leading trivia (doc comment) must still attach correctly with the
    // `internal` modifier in between it and `type`.
    let module = parse_ok(
        "/// Package-private parser state representation.\ninternal type ParserState = Integer",
    );
    let alias = &module.type_aliases[0];
    assert!(alias.is_internal);
    assert_eq!(
        alias.doc_comment.as_deref(),
        Some("Package-private parser state representation.")
    );
}

#[test]
fn parse_multiple_type_aliases_mixed_internal_and_public() {
    let module = parse_ok("internal type Priv = Integer\ntype Pub = Priv | String");
    assert_eq!(module.type_aliases.len(), 2);
    assert!(module.type_aliases[0].is_internal);
    assert_eq!(module.type_aliases[0].name.name, "Priv");
    assert!(!module.type_aliases[1].is_internal);
    assert_eq!(module.type_aliases[1].name.name, "Pub");
}

#[test]
fn internal_as_bare_identifier_is_not_a_type_alias() {
    // `internal` remains a legal ordinary identifier when not immediately
    // followed by the `type <Uppercase> =` shape — mirrors the existing
    // `type_as_assignment_target_is_not_a_type_alias` disambiguation tests
    // below for the bare `type` keyword.
    let module = parse_ok("internal := 5");
    assert!(module.type_aliases.is_empty());
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        module.expressions[0].expression,
        Expression::Assignment { .. }
    ));
}

#[test]
fn internal_followed_by_non_type_identifier_is_not_a_type_alias() {
    let module = parse_ok("internal printString");
    assert!(module.type_aliases.is_empty());
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        module.expressions[0].expression,
        Expression::MessageSend { .. }
    ));
}

// ==========================================================================
// Single-letter name rejection (ADR 0068's implicit type parameters)
// ==========================================================================

#[test]
fn single_letter_type_alias_name_is_rejected() {
    let diagnostics = parse_err("type T = Integer | Nil");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("single-letter type names are reserved")),
        "expected single-letter rejection diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn multi_letter_type_alias_name_is_accepted() {
    // Sanity check: the single-letter check must not over-fire on ordinary names.
    let module = parse_ok("type OptionalInt = Integer | Nil");
    assert_eq!(module.type_aliases.len(), 1);
    assert_eq!(module.type_aliases[0].name.name, "OptionalInt");
}

// ==========================================================================
// Malformed declarations
// ==========================================================================

#[test]
fn type_alias_missing_rhs_is_reported() {
    let diagnostics = parse_err("type Foo = ");
    assert!(
        diagnostics.iter().any(|d| d.severity == Severity::Error),
        "expected an error diagnostic for a missing RHS, got: {diagnostics:?}"
    );
}

#[test]
fn type_alias_invalid_rhs_is_reported() {
    let diagnostics = parse_err("type Foo = 123");
    assert!(
        diagnostics.iter().any(|d| d.severity == Severity::Error),
        "expected an error diagnostic for a non-type RHS, got: {diagnostics:?}"
    );
}

#[test]
fn parametric_type_alias_is_not_recognized_yet() {
    // ADR 0108 Consequences: `type Name(...) = ...` (parametric aliases) is
    // deliberately deferred and reserved as a parse error, v1. The `(` right
    // after the name means `is_at_type_alias_definition`'s lookahead (which
    // requires a bare `=` immediately after the name) does not match, so this
    // falls through to ordinary expression parsing rather than being treated
    // as a type alias. This test only pins that the fallback is graceful
    // (parses to *something*, does not panic) — not that the resulting
    // diagnostic is polished, which is out of scope until parametric aliases
    // are implemented.
    let module = parse_ok_or_err_but_not_panic("type Foo(T) = Integer");
    assert!(module.type_aliases.is_empty());
}

/// Parses `source` without asserting success or failure — used only to pin
/// that malformed input the parser doesn't yet have a dedicated diagnostic
/// for (e.g. parametric type aliases) is handled gracefully rather than
/// panicking.
fn parse_ok_or_err_but_not_panic(source: &str) -> Module {
    let tokens = lex_with_eof(source);
    let (module, _diagnostics) = parse(tokens);
    module
}

// ==========================================================================
// `type` remains an ordinary identifier everywhere else
// ==========================================================================

#[test]
fn type_as_assignment_target_is_not_a_type_alias() {
    let module = parse_ok("type := 5");
    assert!(module.type_aliases.is_empty());
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        module.expressions[0].expression,
        Expression::Assignment { .. }
    ));
}

#[test]
fn type_as_unary_message_receiver_is_not_a_type_alias() {
    let module = parse_ok("type printString");
    assert!(module.type_aliases.is_empty());
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        module.expressions[0].expression,
        Expression::MessageSend { .. }
    ));
}

#[test]
fn type_as_lowercase_unary_receiver_is_not_a_type_alias() {
    // `type` followed by a lowercase identifier never matches the
    // `type <Uppercase> =` lookahead — falls through to an ordinary unary
    // message send, same as `type printString` above.
    let module = parse_ok("type size");
    assert!(module.type_aliases.is_empty());
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        module.expressions[0].expression,
        Expression::MessageSend { .. }
    ));
}

#[test]
fn type_as_binary_expression_inside_deep_method_body_does_not_truncate() {
    // Regression test for a bug found in review: `is_at_type_alias_definition()`
    // was checked in `parse_method_body`'s loop guard (and `is_at_member_boundary`)
    // with no indentation guard, unlike the sibling `is_at_method_definition()`
    // check in the same function (BT-1294). A deeply indented line starting
    // with `type <Uppercase> =` — e.g. `type` used as an ordinary variable,
    // sent the unary message `Port` — token-matches the type-alias lookahead
    // and would falsely end the body early, treating the line as a
    // declaration boundary instead of an in-body statement. `type` must
    // remain a legal identifier everywhere but top-level declaration
    // position, per the ADR.
    //
    // `=` is not a valid Beamtalk binary operator (reserved; use `=:=`/`==`),
    // so this snippet is not otherwise-valid code and does still produce a
    // diagnostic — the fix under test is specifically that `type Port` is
    // parsed as part of the method body (a second body statement) rather
    // than never being attempted at all because the loop exited first.
    let tokens = lex_with_eof(
        "typed Object subclass: Foo\n\
         \x20\x20bar =>\n\
         \x20\x20\x20\x20self value.\n\
         \x20\x20\x20\x20\x20\x20type Port = 8080",
    );
    let (module, _diagnostics) = parse(tokens);
    assert!(module.type_aliases.is_empty());
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);
    let body = &class.methods[0].body;
    assert_eq!(
        body.len(),
        2,
        "the deeply-indented `type Port = ...` line must be parsed as part of \
         the method body, not treated as a type-alias declaration boundary \
         that ends the body early: {body:?}"
    );
    assert!(matches!(
        &body[1].expression,
        Expression::MessageSend { selector: MessageSelector::Unary(s), .. } if s == "Port"
    ));
}

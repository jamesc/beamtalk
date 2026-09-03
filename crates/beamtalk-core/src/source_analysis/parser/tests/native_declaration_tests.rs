// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for `declare native: <module>` type declarations (ADR 0075, Phase 2, BT-1846).

use super::*;

// ==========================================================================
// Valid declarations
// ==========================================================================

#[test]
fn parse_empty_native_declaration() {
    let module = parse_ok("declare native: lists");
    assert_eq!(module.native_declarations.len(), 1);
    let decl = &module.native_declarations[0];
    assert_eq!(decl.module.name, "lists");
    assert!(decl.method_signatures.is_empty());
}

#[test]
fn parse_native_declaration_with_unary_signature() {
    let module = parse_ok("declare native: lists\n  reverse: list :: List(T) -> List(T)\n");
    let decl = &module.native_declarations[0];
    assert_eq!(decl.module.name, "lists");
    assert_eq!(decl.method_signatures.len(), 1);

    let sig = &decl.method_signatures[0];
    let MessageSelector::Keyword(keywords) = &sig.selector else {
        panic!("expected keyword selector, got {:?}", sig.selector);
    };
    assert_eq!(keywords.len(), 1);
    assert_eq!(keywords[0].keyword.as_str(), "reverse:");
    assert_eq!(sig.parameters.len(), 1);
    assert_eq!(sig.parameters[0].name.name, "list");

    let TypeAnnotation::Generic {
        base, parameters, ..
    } = sig.parameters[0].type_annotation.as_ref().unwrap()
    else {
        panic!(
            "expected generic param type, got {:?}",
            sig.parameters[0].type_annotation
        );
    };
    assert_eq!(base.name, "List");
    assert_eq!(parameters.len(), 1);

    let TypeAnnotation::Generic {
        base: ret_base,
        parameters: ret_params,
        ..
    } = sig.return_type.as_ref().unwrap()
    else {
        panic!("expected generic return type, got {:?}", sig.return_type);
    };
    assert_eq!(ret_base.name, "List");
    assert_eq!(ret_params.len(), 1);
}

#[test]
fn parse_native_declaration_with_multiple_signatures() {
    let module = parse_ok(
        "declare native: lists\n  \
         reverse: list :: List(T) -> List(T)\n  \
         seq: from :: Integer to: to :: Integer -> List(Integer)\n  \
         member: elem :: T in: list :: List(T) -> Boolean\n",
    );
    let decl = &module.native_declarations[0];
    assert_eq!(decl.method_signatures.len(), 3);

    let seq = &decl.method_signatures[1];
    let MessageSelector::Keyword(keywords) = &seq.selector else {
        panic!("expected keyword selector");
    };
    assert_eq!(keywords.len(), 2);
    assert_eq!(keywords[0].keyword.as_str(), "seq:");
    assert_eq!(keywords[1].keyword.as_str(), "to:");
    assert_eq!(seq.parameters.len(), 2);
}

#[test]
fn parse_native_declaration_unary_signature() {
    let module = parse_ok("declare native: erlang\n  node -> Symbol\n");
    let decl = &module.native_declarations[0];
    assert_eq!(decl.method_signatures.len(), 1);
    let sig = &decl.method_signatures[0];
    assert!(matches!(&sig.selector, MessageSelector::Unary(name) if name == "node"));
    assert!(sig.parameters.is_empty());
    assert!(matches!(
        sig.return_type.as_ref().unwrap(),
        TypeAnnotation::Simple(id) if id.name == "Symbol"
    ));
}

#[test]
fn native_declaration_signatures_have_no_body() {
    // Signatures never get an `=>` implementation — bodies are illegal.
    let module = parse_ok("declare native: lists\n  reverse: list -> List\n");
    let decl = &module.native_declarations[0];
    assert_eq!(decl.method_signatures.len(), 1);
}

#[test]
fn native_declaration_doc_comment_attaches() {
    let module = parse_ok(
        "/// Type declarations for Erlang module `lists`.\n\
         declare native: lists\n  \
         /// Reverse a list.\n  \
         reverse: list :: List(T) -> List(T)\n",
    );
    let decl = &module.native_declarations[0];
    assert_eq!(
        decl.doc_comment.as_deref(),
        Some("Type declarations for Erlang module `lists`.")
    );
    assert_eq!(
        decl.method_signatures[0].doc_comment.as_deref(),
        Some("Reverse a list.")
    );
}

#[test]
fn multiple_native_declarations_in_one_file() {
    let module = parse_ok(
        "declare native: lists\n  reverse: list -> List\n\n\
         declare native: maps\n  keys: map -> List\n",
    );
    assert_eq!(module.native_declarations.len(), 2);
    assert_eq!(module.native_declarations[0].module.name, "lists");
    assert_eq!(module.native_declarations[1].module.name, "maps");
}

#[test]
fn native_declaration_stops_before_following_class_definition() {
    let module = parse_ok(
        "declare native: lists\n  reverse: list -> List\n\n\
         Object subclass: Counter\n  state: count :: Integer = 0\n",
    );
    assert_eq!(module.native_declarations.len(), 1);
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].name.name, "Counter");
}

#[test]
fn native_declaration_stops_before_following_protocol_definition() {
    let module = parse_ok(
        "declare native: lists\n  reverse: list -> List\n\n\
         Protocol define: Sortable\n  sortKey -> Object\n",
    );
    assert_eq!(module.native_declarations.len(), 1);
    assert_eq!(module.protocols.len(), 1);
}

// ==========================================================================
// Ambiguity guard: `declare` remains a legal identifier everywhere else
// ==========================================================================

#[test]
fn declare_as_ordinary_identifier_outside_declaration_position() {
    // `declare` with no following `native:` keyword parses as a normal
    // message send, not a native declaration.
    let module = parse_ok("declare printString");
    assert!(module.native_declarations.is_empty());
    assert_eq!(module.expressions.len(), 1);
}

#[test]
fn native_keyword_send_inside_method_body_is_not_swallowed() {
    // `declare native: value` as an ordinary keyword message send to a
    // variable named `declare`, inside a real method body, must not be
    // misparsed as a new top-level native declaration truncating the body.
    let module = parse_ok(
        "Object subclass: Registry\n  \
         register =>\n    \
         declare := Object new.\n    \
         declare native: 5.\n    \
         ^declare\n",
    );
    assert_eq!(module.classes.len(), 1);
    assert!(module.native_declarations.is_empty());
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 3);
}

// ==========================================================================
// Invalid declarations
// ==========================================================================

#[test]
fn declare_followed_by_other_keyword_is_an_ordinary_message_send() {
    // `declare` not immediately followed by `native:` never matches the
    // native-declaration predicate — it's just a keyword message send.
    let module = parse_ok("declare foo: lists");
    assert!(module.native_declarations.is_empty());
    assert_eq!(module.expressions.len(), 1);
}

#[test]
fn native_declaration_missing_module_name_errors() {
    let diagnostics = parse_err("declare native:");
    assert!(!diagnostics.is_empty(), "expected a parse error");
}

#[test]
fn native_declaration_signature_with_fat_arrow_errors() {
    // `=>` bodies are illegal in native declarations, same as protocols.
    let diagnostics = parse_err("declare native: lists\n  reverse: list => list\n");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("cannot have implementations")),
        "expected a 'cannot have implementations' error, got: {diagnostics:?}"
    );
}

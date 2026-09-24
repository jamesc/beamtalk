// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for ADR 0127 (Traits — Protocols with Provided Methods) syntax:
//! provided methods in `Protocol define:` bodies and `uses:` lines in class
//! bodies. Syntax only (BT-3587) — the flattening pass that gives these
//! forms meaning is BT-3588.
use super::*;

// ==========================================================================
// Protocol define: — provided methods (ADR 0127 §1)
// ==========================================================================

#[test]
fn parse_protocol_provided_instance_method() {
    let module = parse_ok(
        "Protocol define: Comparable
  < other :: Self -> Boolean
  > other :: Self -> Boolean => other < self",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.method_signatures.len(), 1, "`<` stays required");
    assert_eq!(proto.method_signatures[0].selector.name(), "<");
    assert_eq!(proto.provided_methods.len(), 1, "`>` is provided");
    assert_eq!(proto.provided_methods[0].selector.name(), ">");
    assert!(!proto.provided_methods[0].body.is_empty());
}

#[test]
fn parse_protocol_provided_method_keyword_selector() {
    let module = parse_ok(
        "Protocol define: Comparable
  < other :: Self -> Boolean
  between: min :: Self and: max :: Self -> Boolean =>
    (self >= min) and: [self <= max]",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.provided_methods.len(), 1);
    assert_eq!(proto.provided_methods[0].selector.name(), "between:and:");
    assert_eq!(proto.provided_methods[0].parameters.len(), 2);
}

#[test]
fn parse_protocol_provided_method_unary_with_no_required_signatures() {
    // A protocol made entirely of provided methods (no requirements at all)
    // still parses — ADR 0127 doesn't require at least one required
    // selector.
    let module = parse_ok(
        "Protocol define: Greeter
  greeting -> String => \"hello\"",
    );
    let proto = &module.protocols[0];
    assert!(proto.method_signatures.is_empty());
    assert_eq!(proto.provided_methods.len(), 1);
    assert_eq!(proto.provided_methods[0].selector.name(), "greeting");
}

#[test]
fn parse_protocol_class_side_requirement_unaffected() {
    // Class-side *requirements* (no `=>`) are unchanged by this ADR.
    let module = parse_ok(
        "Protocol define: Creatable
  class create -> Self",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.class_method_signatures.len(), 1);
    assert!(proto.provided_methods.is_empty());
}

#[test]
fn parse_protocol_class_side_provision_is_not_yet_supported() {
    // ADR 0127 §1, §13: a class-side *provision* (`class sel … =>`) is a
    // parse error in v1 — there is nowhere in the AST for it to live yet.
    let diagnostics = parse_err(
        "Protocol define: Creatable
  class create -> Self => Self new",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("class-side provided methods are not yet supported")),
        "Expected a class-side-provision error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_protocol_class_side_provision_not_collected_anywhere() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Protocol define: Creatable
  class create -> Self => Self new",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let proto = &module.protocols[0];
    assert!(proto.class_method_signatures.is_empty());
    assert!(proto.provided_methods.is_empty());
}

#[test]
fn parse_protocol_uses_line_is_not_yet_supported() {
    // ADR 0127 §Status 8, §13: a protocol composing another protocol is
    // post-v1.
    let diagnostics = parse_err(
        "Protocol define: Sortable
  uses: Comparable
  sortKey -> Object",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("a protocol using another protocol is not yet supported")),
        "Expected a protocol-uses-protocol error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_protocol_uses_line_does_not_swallow_following_signature() {
    // The reserved `uses:` line inside a protocol body is an error, but
    // parsing must recover and keep going — the required signature after it
    // still parses.
    let tokens = crate::source_analysis::lex_with_eof(
        "Protocol define: Sortable
  uses: Comparable
  sortKey -> Object",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let proto = &module.protocols[0];
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "sortKey");
}

#[test]
fn parse_protocol_method_literally_named_uses_is_a_provided_method() {
    // ADR 0127 §2: `uses:` is reserved only at the start of a line with no
    // `=>` — a method *named* `uses:` has a body and is an ordinary
    // provided method, not the reserved composition clause.
    let module = parse_ok(
        "Protocol define: Weird
  uses: aProtocol => aProtocol name",
    );
    let proto = &module.protocols[0];
    assert!(
        proto
            .provided_methods
            .iter()
            .any(|m| m.selector.name() == "uses:")
    );
}

#[test]
fn parse_protocol_excluding_keyword_still_usable_as_selector() {
    // ADR 0127 §2: only `uses:` is reserved inside a protocol body;
    // `excluding:`/`overriding:` stay usable as ordinary selectors there.
    let module = parse_ok(
        "Protocol define: SetLike
  excluding: item :: Object -> Self",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "excluding:");
}

// ==========================================================================
// `uses:` lines in a class body (ADR 0127 §2)
// ==========================================================================

#[test]
fn parse_class_uses_line_basic() {
    let tokens = crate::source_analysis::lex_with_eof(
        "sealed typed Value subclass: DateTime
  uses: Comparable
  field: seconds :: Integer = 0",
    );
    let (module, diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses.len(), 1);
    assert_eq!(class.uses[0].protocol.name, "Comparable");
    assert!(class.uses[0].package.is_none());
    assert!(class.uses[0].type_args.is_empty());
    assert!(class.uses[0].excluding.is_empty());
    assert!(class.uses[0].overriding.is_empty());
    // Still a state declaration after it, proving `uses:` doesn't swallow
    // the rest of the body.
    assert_eq!(class.state.len(), 1);
    // BT-3587's placeholder: flattening isn't implemented yet.
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("protocol composition is not yet supported")),
        "Expected the not-yet-supported placeholder error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_class_uses_line_package_qualified() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: LenientParser
  uses: json@Parser",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses.len(), 1);
    assert_eq!(class.uses[0].protocol.name, "Parser");
    assert_eq!(class.uses[0].package.as_ref().unwrap().name, "json");
}

#[test]
fn parse_class_uses_line_with_type_args() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Actor subclass: WorkerPool
  uses: Enumerable(Worker)",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses.len(), 1);
    assert_eq!(class.uses[0].type_args.len(), 1);
    assert_eq!(class.uses[0].type_args[0].type_name(), "Worker");
}

#[test]
fn parse_class_uses_line_with_excluding() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: Report
  uses: Describable excluding: #(#printString)",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses[0].excluding.len(), 1);
    assert_eq!(class.uses[0].excluding[0].name, "printString");
    assert!(class.uses[0].overriding.is_empty());
}

#[test]
fn parse_class_uses_line_with_overriding() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: AuditRecord
  uses: Describable overriding: #(#printString)",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses[0].overriding.len(), 1);
    assert_eq!(class.uses[0].overriding[0].name, "printString");
}

#[test]
fn parse_class_uses_line_with_excluding_and_overriding() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: Report
  uses: Labelled excluding: #(#printString, #summary) overriding: #(#hash)",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses[0].excluding.len(), 2);
    assert_eq!(class.uses[0].excluding[0].name, "printString");
    assert_eq!(class.uses[0].excluding[1].name, "summary");
    assert_eq!(class.uses[0].overriding.len(), 1);
    assert_eq!(class.uses[0].overriding[0].name, "hash");
}

#[test]
fn parse_class_uses_line_excluding_keyword_selector() {
    // `excluding:`'s selector list can carry keyword selectors too.
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: Pair
  uses: Enumerable excluding: #(#at:put:)",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses[0].excluding[0].name, "at:put:");
}

#[test]
fn parse_class_multiple_uses_lines() {
    // One trait per `uses:` line; several traits are several lines.
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: Report
  uses: Labelled
  uses: Describable",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses.len(), 2);
    assert_eq!(class.uses[0].protocol.name, "Labelled");
    assert_eq!(class.uses[1].protocol.name, "Describable");
}

#[test]
fn parse_class_uses_line_aliasing_is_not_yet_supported() {
    let diagnostics = parse_err(
        "Value subclass: Report
  uses: Describable aliasing: #{#describeString => #printString}",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("'aliasing:' is not yet supported")),
        "Expected an aliasing-not-yet-supported error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_class_uses_line_aliasing_recovers_for_following_declarations() {
    // Parse recovery: consuming the `#{...}` dict literal after `aliasing:`
    // must not swallow the state declaration that follows.
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: Report
  uses: Describable aliasing: #{#describeString => #printString}
  field: title :: String = \"\"",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(class.uses.len(), 1);
    assert_eq!(class.state.len(), 1);
    assert_eq!(class.state[0].name.name, "title");
}

#[test]
fn parse_class_uses_after_state_is_misplaced_error() {
    let diagnostics = parse_err(
        "Value subclass: Report
  field: title :: String = \"\"
  uses: Describable",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("'uses:' lines must come before state and method declarations")),
        "Expected a misplaced-uses: error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_class_uses_after_state_is_not_added_to_uses() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: Report
  field: title :: String = \"\"
  uses: Describable",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert!(
        class.uses.is_empty(),
        "a misplaced uses: line should not compose"
    );
}

#[test]
fn parse_class_uses_after_method_is_misplaced_error() {
    let diagnostics = parse_err(
        "Value subclass: Report
  printString -> String => \"Report\"
  uses: Describable",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("'uses:' lines must come before state and method declarations")),
        "Expected a misplaced-uses: error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_class_method_literally_named_uses_is_an_ordinary_method() {
    // Symmetric with the protocol-body disambiguation: `uses: x => body` is
    // a method literally named `uses:`, not the composition clause.
    let module = parse_ok(
        "Value subclass: Weird
  uses: aProtocol => aProtocol name",
    );
    let class = &module.classes[0];
    assert!(class.uses.is_empty());
    assert!(class.methods.iter().any(|m| m.selector.name() == "uses:"));
}

#[test]
fn parse_class_unknown_keyword_is_error_not_silent_end_of_body() {
    // ADR 0127 §13: an unrecognized keyword line in a class body is a
    // targeted error, replacing the old silent-end-of-body behavior
    // (`parser/declarations.rs:808-809` before this change).
    let diagnostics = parse_err(
        "Object subclass: Foo
  bogus: 1
  size => 0",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("unexpected 'bogus:' in class body")),
        "Expected an unexpected-keyword error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_class_unknown_keyword_recovers_and_parses_following_method() {
    // The key behavioral fix: everything after the bad keyword line used to
    // go unparsed with no diagnostic at all. It must now still parse.
    let tokens = crate::source_analysis::lex_with_eof(
        "Object subclass: Foo
  bogus: 1
  size => 0",
    );
    let (module, _diagnostics) = crate::source_analysis::parse(tokens);
    let class = &module.classes[0];
    assert_eq!(
        class.methods.len(),
        1,
        "the method after the bad keyword line must still be parsed"
    );
    assert_eq!(class.methods[0].selector.name(), "size");
}

#[test]
fn parse_class_uses_excluding_non_symbol_element_is_error() {
    let diagnostics = parse_err(
        "Value subclass: Report
  uses: Describable excluding: #(printString)",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("expected a symbol selector")),
        "Expected a not-a-symbol error, got: {diagnostics:?}"
    );
}

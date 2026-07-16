// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block-literal callback params now infer their arg type when the declared
//! param type is a Union containing a `Block(...)` arm (BT-2864).
//!
//! Before this fix (BT-2864), propagating an expected type into a
//! block-literal argument only recognised a bare `Block(...)` param type. A
//! param typed as a Union whose arms include a `Block(...)` (e.g.
//! `Block(HTTPRequest, HTTPResponse) | HTTPHandler | HTTPRouter`, matching
//! `HTTPServer>>start:handler:` from the issue) fell back to typing every
//! block param as `Dynamic`, requiring a manual `@expect type` annotation to
//! narrow it, even though exactly one union arm was an unambiguous
//! `Block(...)` signature.

use super::common::*;

/// Finds the first `Expression::Block` literal in `class_name`'s named
/// method and returns its `InferredType` from `type_map`.
///
/// Diagnostics alone can't reliably distinguish "block param resolved to
/// the expected class" from "block param stayed Dynamic": a Dynamic
/// receiver never fires DNU (Dynamic suppresses that check entirely), and
/// this codebase's BT-1914 "Dynamic in typed class" warning only fires for
/// specific expression shapes — not for a Dynamic value used as a
/// message-send receiver nested inside a block body. Reading the checker's
/// own `TypeMap` for the block's span is the direct, shape-independent way
/// to assert what it actually inferred.
fn block_type_in_method<'a>(
    module: &Module,
    type_map: &'a TypeMap,
    class_name: &str,
    selector: &str,
) -> &'a InferredType {
    let class = module
        .classes
        .iter()
        .find(|c| c.name.name == class_name)
        .unwrap_or_else(|| panic!("class {class_name} not found"));
    let method = class
        .methods
        .iter()
        .find(|m| m.selector.name() == selector)
        .unwrap_or_else(|| panic!("method {selector} not found on {class_name}"));
    let block = method
        .body
        .iter()
        .find_map(|stmt| find_block(&stmt.expression))
        .unwrap_or_else(|| panic!("no block literal found in {class_name}>>{selector}"));
    type_map
        .get(block.span)
        .unwrap_or_else(|| panic!("no type_map entry for block at {:?}", block.span))
}

/// Recursively searches an expression tree for the first block literal.
/// Covers the shapes these fixtures use: a top-level keyword message send
/// whose arguments include a block literal.
fn find_block(expr: &Expression) -> Option<&Block> {
    match expr {
        Expression::Block(block) => Some(block),
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => find_block(receiver).or_else(|| arguments.iter().find_map(find_block)),
        _ => None,
    }
}

/// Asserts `ty` is `Block(HTTPRequest, ...)` — the block's first (and only)
/// parameter resolved to the concrete `HTTPRequest` class, not `Dynamic`.
fn assert_block_param_is_http_request(ty: &InferredType, context: &str) {
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = ty
    else {
        panic!("{context}: expected a Known Block type, got: {ty:?}");
    };
    assert_eq!(class_name.as_str(), "Block", "{context}: got {ty:?}");
    let param_ty = type_args
        .first()
        .unwrap_or_else(|| panic!("{context}: Block type has no type_args: {ty:?}"));
    assert_eq!(
        param_ty.as_known().map(ecow::EcoString::as_str),
        Some("HTTPRequest"),
        "{context}: block param should infer as HTTPRequest (from the Union's \
         Block(...) arm), got: {param_ty:?}"
    );
}

/// AC: a block literal passed inline at the call site to a Union-typed param
/// gets its parameters typed from the Union's `Block(...)` arm — this is
/// the exact repro shape from the issue (`HTTPServer start:handler:`).
#[test]
fn union_typed_block_param_inferred_from_single_block_arm_inline() {
    let source = "\
typed Object subclass: HTTPRequest\n\
  respond -> HTTPRequest => self\n\
typed Object subclass: HTTPHandler\n\
typed Object subclass: HTTPRouter\n\
typed Object subclass: HTTPServer\n\
  start: port :: Integer handler: handler :: Block(HTTPRequest, HTTPRequest) | HTTPHandler | HTTPRouter -> HTTPServer => self\n\
typed Object subclass: App\n\
  m: server :: HTTPServer -> HTTPServer => server start: 8080 handler: [:req | req respond]\n";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let ty = block_type_in_method(&module, checker.type_map(), "App", "m:");
    assert_block_param_is_http_request(ty, "BT-2864 (inline block, Union param)");
}

/// Regression guard: a bare (non-Union) `Block(...)` param still works
/// unchanged (the pre-existing BT-2158 behaviour `find_block_arm` must
/// preserve).
#[test]
fn bare_block_param_still_inferred_no_union() {
    let source = "\
typed Object subclass: HTTPRequest\n\
  respond -> HTTPRequest => self\n\
typed Object subclass: HTTPServer\n\
  start: port :: Integer handler: handler :: Block(HTTPRequest, HTTPRequest) -> HTTPServer => self\n\
typed Object subclass: App\n\
  m: server :: HTTPServer -> HTTPServer => server start: 8080 handler: [:req | req respond]\n";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let ty = block_type_in_method(&module, checker.type_map(), "App", "m:");
    assert_block_param_is_http_request(ty, "bare Block(...) param (no Union, BT-2158 baseline)");
}

/// Ambiguity guard: `find_block_arm` returns `None` for a Union with *two*
/// `Block(...)` arms rather than guessing which signature applies (unit
/// test on the helper directly — `tests` is a descendant module of
/// `type_checker`, so private items are visible here).
#[test]
fn find_block_arm_none_for_ambiguous_union() {
    assert_eq!(
        TypeChecker::find_block_arm("Block(HTTPRequest, HTTPRequest) | Block(Integer, Integer)"),
        None,
        "two Block(...) arms is ambiguous — must not pick either"
    );
}

/// `find_block_arm` finds the single `Block(...)` arm of a Union, matching
/// the issue's exact repro shape.
#[test]
fn find_block_arm_finds_single_arm_in_union() {
    assert_eq!(
        TypeChecker::find_block_arm("Block(HTTPRequest, HTTPResponse) | HTTPHandler | HTTPRouter"),
        Some("Block(HTTPRequest, HTTPResponse)")
    );
}

/// `find_block_arm` still matches a bare (non-Union) `Block(...)` type.
#[test]
fn find_block_arm_matches_bare_block() {
    assert_eq!(
        TypeChecker::find_block_arm("Block(Integer, Integer)"),
        Some("Block(Integer, Integer)")
    );
}

/// `find_block_arm` returns `None` when no arm is a `Block(...)` type.
#[test]
fn find_block_arm_none_when_no_block_arm() {
    assert_eq!(
        TypeChecker::find_block_arm("HTTPHandler | HTTPRouter"),
        None
    );
    assert_eq!(TypeChecker::find_block_arm("String"), None);
}

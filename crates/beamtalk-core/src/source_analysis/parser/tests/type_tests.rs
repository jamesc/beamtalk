// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for generic type annotations, protocol definitions, package-qualified
//! definitions, and superclass type arguments.
use super::*;

// ==========================================================================
// Generic type annotation parsing (ADR 0068, BT-1568)
// ==========================================================================

#[test]
fn parse_generic_class_definition_single_param() {
    let module = parse_ok(
        "Value subclass: Stack(E)
  state: items = #[]",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Stack");
    assert_eq!(class.type_params.len(), 1);
    assert_eq!(class.type_params[0].name.name, "E");
}

#[test]
fn parse_generic_class_definition_two_params() {
    let module = parse_ok(
        "sealed Value subclass: Result(T, E)
  state: okValue = nil
  state: errReason = nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Result");
    assert!(class.is_sealed);
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "T");
    assert_eq!(class.type_params[1].name.name, "E");
    assert_eq!(class.state.len(), 2);
}

#[test]
fn parse_generic_class_definition_no_params() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Counter");
    assert!(class.type_params.is_empty());
}

// --- ADR 0068 Phase 2d: Bounded type parameters ---

#[test]
fn parse_bounded_type_param_single() {
    let module = parse_ok(
        "Actor subclass: Logger(T :: Printable)
  log: item :: T => item asString",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Logger");
    assert_eq!(class.type_params.len(), 1);
    assert_eq!(class.type_params[0].name.name, "T");
    assert!(class.type_params[0].bound.is_some());
    assert_eq!(
        class.type_params[0].bound.as_ref().unwrap().name,
        "Printable"
    );
}

#[test]
fn parse_bounded_type_param_mixed() {
    // T is bounded, E is unbounded
    let module = parse_ok(
        "Value subclass: Container(T :: Printable, E)
  state: item :: T = nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "T");
    assert!(class.type_params[0].bound.is_some());
    assert_eq!(
        class.type_params[0].bound.as_ref().unwrap().name,
        "Printable"
    );
    assert_eq!(class.type_params[1].name.name, "E");
    assert!(class.type_params[1].bound.is_none());
}

#[test]
fn parse_bounded_type_param_all_bounded() {
    let module = parse_ok(
        "Value subclass: SortedMap(K :: Comparable, V :: Printable)
  state: items = #[]",
    );
    let class = &module.classes[0];
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "K");
    assert_eq!(
        class.type_params[0].bound.as_ref().unwrap().name,
        "Comparable"
    );
    assert_eq!(class.type_params[1].name.name, "V");
    assert_eq!(
        class.type_params[1].bound.as_ref().unwrap().name,
        "Printable"
    );
}

#[test]
fn parse_bounded_type_param_protocol() {
    // Protocol with bounded type params
    let module = parse_ok(
        "Protocol define: Mapper(T :: Printable)
  map: block :: Block(T, Object) -> Object",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.type_params.len(), 1);
    assert_eq!(proto.type_params[0].name.name, "T");
    assert!(proto.type_params[0].bound.is_some());
    assert_eq!(
        proto.type_params[0].bound.as_ref().unwrap().name,
        "Printable"
    );
}

#[test]
fn parse_generic_type_annotation_single() {
    // `Array(Integer)` in a type annotation
    let module = parse_ok(
        "Actor subclass: Foo
  state: items :: Array(Integer) = #[]",
    );
    let class = &module.classes[0];
    let state = &class.state[0];
    let ty = state.type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    {
        assert_eq!(base.name, "Array");
        assert_eq!(parameters.len(), 1);
        assert!(matches!(&parameters[0], TypeAnnotation::Simple(id) if id.name == "Integer"));
    } else {
        panic!("Expected Generic type annotation, got: {ty:?}");
    }
}

#[test]
fn parse_generic_type_annotation_result() {
    // `Result(T, E)` in a type annotation
    let module = parse_ok(
        "Actor subclass: Foo
  state: result :: Result(String, IOError) = nil",
    );
    let ty = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    {
        assert_eq!(base.name, "Result");
        assert_eq!(parameters.len(), 2);
        assert!(matches!(&parameters[0], TypeAnnotation::Simple(id) if id.name == "String"));
        assert!(matches!(&parameters[1], TypeAnnotation::Simple(id) if id.name == "IOError"));
    } else {
        panic!("Expected Generic type annotation, got: {ty:?}");
    }
}

#[test]
fn parse_generic_type_annotation_nested() {
    // `Block(T, Result(R, E))` — nested generic type
    let module = parse_ok(
        "Actor subclass: Foo
  state: handler :: Block(T, Result(R, E)) = nil",
    );
    let ty = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base,
        parameters: params,
        ..
    } = ty
    {
        assert_eq!(base.name, "Block");
        assert_eq!(params.len(), 2);
        assert!(matches!(&params[0], TypeAnnotation::Simple(id) if id.name == "T"));
        if let TypeAnnotation::Generic {
            base: inner_base,
            parameters: inner_params,
            ..
        } = &params[1]
        {
            assert_eq!(inner_base.name, "Result");
            assert_eq!(inner_params.len(), 2);
            assert!(matches!(&inner_params[0], TypeAnnotation::Simple(id) if id.name == "R"));
            assert!(matches!(&inner_params[1], TypeAnnotation::Simple(id) if id.name == "E"));
        } else {
            panic!("Expected nested Generic, got: {:?}", params[1]);
        }
    } else {
        panic!("Expected Generic type annotation, got: {ty:?}");
    }
}

#[test]
fn parse_generic_type_in_method_return_type() {
    // Method with `-> Result(Integer, Error)` return type
    let module = parse_ok(
        "Actor subclass: Foo
  compute -> Result(Integer, Error) => nil",
    );
    let method = &module.classes[0].methods[0];
    let ret_ty = method.return_type.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ret_ty
    {
        assert_eq!(base.name, "Result");
        assert_eq!(parameters.len(), 2);
    } else {
        panic!("Expected Generic return type, got: {ret_ty:?}");
    }
}

#[test]
fn parse_generic_type_in_method_param() {
    // Method with `block :: Block(T, R)` parameter type
    let module = parse_ok(
        "Actor subclass: Foo
  map: block :: Block(T, R) -> Result(R, E) => nil",
    );
    let method = &module.classes[0].methods[0];
    let param_ty = method.parameters[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = param_ty
    {
        assert_eq!(base.name, "Block");
        assert_eq!(parameters.len(), 2);
    } else {
        panic!("Expected Generic param type, got: {param_ty:?}");
    }
    let ret_ty = method.return_type.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ret_ty
    {
        assert_eq!(base.name, "Result");
        assert_eq!(parameters.len(), 2);
    } else {
        panic!("Expected Generic return type, got: {ret_ty:?}");
    }
}

#[test]
fn parse_generic_type_name_uses_parentheses() {
    // `type_name()` should produce `Collection(Integer)` not `Collection<Integer>`
    let ty = TypeAnnotation::generic(
        Identifier::new("Collection", Span::new(0, 10)),
        vec![TypeAnnotation::simple("Integer", Span::new(11, 18))],
        Span::new(0, 19),
    );
    assert_eq!(ty.type_name(), "Collection(Integer)");

    let ty2 = TypeAnnotation::generic(
        Identifier::new("Result", Span::new(0, 6)),
        vec![
            TypeAnnotation::simple("String", Span::new(7, 13)),
            TypeAnnotation::simple("Error", Span::new(15, 20)),
        ],
        Span::new(0, 21),
    );
    assert_eq!(ty2.type_name(), "Result(String, Error)");
}

#[test]
fn parse_generic_class_with_native_module() {
    // Generic class with native: backing module
    let module = parse_ok(
        "Actor subclass: Cache(K, V) native: beamtalk_cache
  getValue: key :: K -> V => nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Cache");
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "K");
    assert_eq!(class.type_params[1].name.name, "V");
    assert!(class.backing_module.is_some());
    assert_eq!(
        class.backing_module.as_ref().unwrap().name,
        "beamtalk_cache"
    );
}

#[test]
fn parse_generic_union_with_generic_member() {
    // `Result(Integer, Error) | nil` — union with a generic member
    let module = parse_ok(
        "Actor subclass: Foo
  state: result :: Result(Integer, Error) | nil = nil",
    );
    let ty = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Union { types, .. } = ty {
        assert_eq!(types.len(), 2);
        assert!(
            matches!(&types[0], TypeAnnotation::Generic { base, parameters, .. }
            if base.name == "Result" && parameters.len() == 2)
        );
    } else {
        panic!("Expected Union type, got: {ty:?}");
    }
}

// ========================================================================
// Protocol Definition Tests (ADR 0068, Phase 2a — BT-1578)
// ========================================================================

#[test]
fn parse_protocol_printable() {
    let module = parse_ok(
        "Protocol define: Printable
  /// Return a human-readable string representation.
  asString -> String",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Printable");
    assert!(proto.type_params.is_empty());
    assert!(proto.extending.is_none());
    assert_eq!(proto.method_signatures.len(), 1);

    let sig = &proto.method_signatures[0];
    assert_eq!(sig.selector.name(), "asString");
    assert!(sig.parameters.is_empty());
    let ret_ty = sig.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::Simple(id) if id.name == "String"));
    assert!(sig.doc_comment.is_some());
}

#[test]
fn parse_protocol_comparable() {
    let module = parse_ok(
        "Protocol define: Comparable
  < other :: Self -> Boolean
  > other :: Self -> Boolean
  <= other :: Self -> Boolean
  >= other :: Self -> Boolean",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Comparable");
    assert_eq!(proto.method_signatures.len(), 4);

    // Check first signature: `< other :: Self -> Boolean`
    let sig = &proto.method_signatures[0];
    assert!(matches!(&sig.selector, MessageSelector::Binary(op) if op == "<"));
    assert_eq!(sig.parameters.len(), 1);
    assert_eq!(sig.parameters[0].name.name, "other");
    let param_ty = sig.parameters[0].type_annotation.as_ref().unwrap();
    assert!(matches!(param_ty, TypeAnnotation::SelfType { .. }));
    let ret_ty = sig.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::Simple(id) if id.name == "Boolean"));
}

#[test]
fn parse_protocol_generic_collection() {
    let module = parse_ok(
        "Protocol define: Collection(E)
  /// The number of elements in this collection.
  size -> Integer
  /// Iterate over each element.
  do: block :: Block(E, Object)
  /// Transform each element.
  collect: block :: Block(E, Object) -> Self
  /// Return elements matching the predicate.
  select: block :: Block(E, Boolean) -> Self",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Collection");
    assert_eq!(proto.type_params.len(), 1);
    assert_eq!(proto.type_params[0].name.name, "E");
    assert_eq!(proto.method_signatures.len(), 4);

    // Check `size -> Integer`
    let sig_size = &proto.method_signatures[0];
    assert_eq!(sig_size.selector.name(), "size");
    assert!(sig_size.parameters.is_empty());
    assert!(matches!(
        sig_size.return_type.as_ref().unwrap(),
        TypeAnnotation::Simple(id) if id.name == "Integer"
    ));

    // Check `do: block :: Block(E, Object)`
    let sig_do = &proto.method_signatures[1];
    assert_eq!(sig_do.selector.name(), "do:");
    assert_eq!(sig_do.parameters.len(), 1);
    let param_ty = sig_do.parameters[0].type_annotation.as_ref().unwrap();
    assert!(
        matches!(param_ty, TypeAnnotation::Generic { base, parameters, .. }
        if base.name == "Block" && parameters.len() == 2)
    );
    assert!(sig_do.return_type.is_none());

    // Check `collect: block :: Block(E, Object) -> Self`
    let sig_collect = &proto.method_signatures[2];
    assert_eq!(sig_collect.selector.name(), "collect:");
    let ret_ty = sig_collect.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::SelfType { .. }));
}

#[test]
fn parse_protocol_extending() {
    let module = parse_ok(
        "Protocol define: Sortable
  extending: Comparable
  /// The key used for sort ordering.
  sortKey -> Object",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Sortable");
    assert!(proto.type_params.is_empty());
    let extending = proto.extending.as_ref().unwrap();
    assert_eq!(extending.name, "Comparable");
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "sortKey");
}

#[test]
fn parse_protocol_no_methods() {
    // A protocol with no method signatures (just the definition)
    let module = parse_ok("Protocol define: Marker");
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Marker");
    assert!(proto.method_signatures.is_empty());
}

#[test]
fn parse_protocol_multiple_type_params() {
    let module = parse_ok(
        "Protocol define: Mapping(K, V)
  at: key :: K -> V
  put: key :: K value: val :: V",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Mapping");
    assert_eq!(proto.type_params.len(), 2);
    assert_eq!(proto.type_params[0].name.name, "K");
    assert_eq!(proto.type_params[1].name.name, "V");
    assert_eq!(proto.method_signatures.len(), 2);

    // Check `at: key :: K -> V`
    let sig_at = &proto.method_signatures[0];
    assert_eq!(sig_at.selector.name(), "at:");
    assert_eq!(sig_at.parameters.len(), 1);
    let ret_ty = sig_at.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::Simple(id) if id.name == "V"));

    // Check `put: key :: K value: val :: V`
    let sig_put = &proto.method_signatures[1];
    assert_eq!(sig_put.selector.name(), "put:value:");
    assert_eq!(sig_put.parameters.len(), 2);
}

#[test]
fn parse_protocol_followed_by_class() {
    // Protocol definition followed by a class definition
    let module = parse_ok(
        "Protocol define: Printable
  asString -> String

Object subclass: Foo
  toString => \"hello\"",
    );
    assert_eq!(module.protocols.len(), 1);
    assert_eq!(module.protocols[0].name.name, "Printable");
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].name.name, "Foo");
}

#[test]
fn parse_multiple_protocols() {
    let module = parse_ok(
        "Protocol define: Printable
  asString -> String

Protocol define: Comparable
  < other :: Self -> Boolean",
    );
    assert_eq!(module.protocols.len(), 2);
    assert_eq!(module.protocols[0].name.name, "Printable");
    assert_eq!(module.protocols[1].name.name, "Comparable");
}

#[test]
fn parse_protocol_with_impl_body_error() {
    // Protocol method signatures should NOT have `=>`
    let diagnostics = parse_err(
        "Protocol define: Bad
  asString => \"hello\"",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Protocol method signatures cannot have implementations")),
        "Expected error about protocol implementations, got: {diagnostics:?}"
    );
}

#[test]
fn parse_protocol_unary_no_return_type() {
    // Unary method without a return type
    let module = parse_ok(
        "Protocol define: Hashable
  hash",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "hash");
    assert!(proto.method_signatures[0].return_type.is_none());
}

#[test]
fn parse_protocol_identifier_in_expression_context() {
    // `Protocol` used as a variable name should not trigger protocol parsing
    let module = parse_ok("x := Protocol");
    assert!(module.protocols.is_empty());
    assert_eq!(module.expressions.len(), 1);
}

#[test]
fn parse_protocol_class_method_unary() {
    // BT-1611: Protocol with a class method requirement
    let module = parse_ok(
        "Protocol define: Serializable
  asString -> String
  class fromString: aString :: String -> Self",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Serializable");

    // Instance methods
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "asString");

    // Class methods
    assert_eq!(proto.class_method_signatures.len(), 1);
    let class_sig = &proto.class_method_signatures[0];
    assert_eq!(class_sig.selector.name(), "fromString:");
    assert_eq!(class_sig.parameters.len(), 1);
    assert_eq!(class_sig.parameters[0].name.name, "aString");
    let param_ty = class_sig.parameters[0].type_annotation.as_ref().unwrap();
    assert!(matches!(param_ty, TypeAnnotation::Simple(id) if id.name == "String"));
    let ret_ty = class_sig.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::SelfType { .. }));
}

#[test]
fn parse_protocol_class_method_only() {
    // BT-1611: Protocol with only class methods
    let module = parse_ok(
        "Protocol define: Factory
  class create -> Self",
    );
    let proto = &module.protocols[0];
    assert!(proto.method_signatures.is_empty());
    assert_eq!(proto.class_method_signatures.len(), 1);
    assert_eq!(proto.class_method_signatures[0].selector.name(), "create");
}

#[test]
fn parse_protocol_mixed_instance_and_class_methods() {
    // BT-1611: Protocol with interleaved instance and class method signatures
    let module = parse_ok(
        "Protocol define: Configurable
  config -> Map
  class default -> Self
  reset -> Object",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.method_signatures.len(), 2); // config and reset
    assert_eq!(proto.method_signatures[0].selector.name(), "config");
    assert_eq!(proto.method_signatures[1].selector.name(), "reset");
    assert_eq!(proto.class_method_signatures.len(), 1);
    assert_eq!(proto.class_method_signatures[0].selector.name(), "default");
}

#[test]
fn parse_protocol_class_definition_named_protocol() {
    // A class named `Protocol` should parse as a class, not a protocol
    // because the keyword after `Protocol` is `subclass:`, not `define:`
    let module = parse_ok(
        "Object subclass: Protocol
  doSomething => nil",
    );
    assert!(module.protocols.is_empty());
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].name.name, "Protocol");
}

// ---- BT-1577: Superclass type argument parsing ----

#[test]
fn parse_superclass_type_args_single_param() {
    // Collection(E) subclass: Array(E)
    let module = parse_ok(
        "Collection(E) subclass: Array(E)
  append: item => nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Array");
    assert_eq!(class.superclass.as_ref().unwrap().name, "Collection");
    assert_eq!(class.type_params.len(), 1);
    assert_eq!(class.type_params[0].name.name, "E");
    assert_eq!(class.superclass_type_args.len(), 1);
    match &class.superclass_type_args[0] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "E"),
        other => panic!("Expected Simple(E), got: {other:?}"),
    }
}

#[test]
fn parse_superclass_type_args_concrete_type() {
    // Collection(Integer) subclass: IntArray
    let module = parse_ok(
        "Collection(Integer) subclass: IntArray
  append: item => nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "IntArray");
    assert!(class.type_params.is_empty());
    assert_eq!(class.superclass_type_args.len(), 1);
    match &class.superclass_type_args[0] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "Integer"),
        other => panic!("Expected Simple(Integer), got: {other:?}"),
    }
}

#[test]
fn parse_superclass_type_args_two_params() {
    // Mapping(K, V) subclass: SortedMap(K, V)
    let module = parse_ok(
        "Mapping(K, V) subclass: SortedMap(K, V)
  size => 0",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "SortedMap");
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.superclass_type_args.len(), 2);
    match &class.superclass_type_args[0] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "K"),
        other => panic!("Expected Simple(K), got: {other:?}"),
    }
    match &class.superclass_type_args[1] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "V"),
        other => panic!("Expected Simple(V), got: {other:?}"),
    }
}

#[test]
fn parse_superclass_no_type_args() {
    // Actor subclass: Counter — no superclass type args
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0",
    );
    let class = &module.classes[0];
    assert!(class.superclass_type_args.is_empty());
}

// === Protocol define: is_input_complete tests ===

#[test]
fn incomplete_protocol_define_no_methods() {
    // "Protocol define: Greetable" alone is incomplete — waiting for method sigs
    assert!(!is_input_complete("Protocol define: Greetable"));
}

#[test]
fn incomplete_protocol_define_with_method() {
    // Protocol definitions are always incomplete — the REPL needs a blank line
    // to know the definition is finished (protocols can have multiple methods).
    assert!(!is_input_complete(
        "Protocol define: Greetable\n  greet -> String"
    ));
}

#[test]
fn incomplete_protocol_define_with_multiple_methods() {
    // Still incomplete even with multiple methods — needs blank line to submit
    assert!(!is_input_complete(
        "Protocol define: Serializable\n  serialize -> String\n  deserialize: data :: String -> Self"
    ));
}

#[test]
fn incomplete_protocol_define_extending_only() {
    assert!(!is_input_complete(
        "Protocol define: PrettyPrintable\n  extending: Greetable"
    ));
}

#[test]
fn incomplete_protocol_define_extending_with_method() {
    assert!(!is_input_complete(
        "Protocol define: PrettyPrintable\n  extending: Greetable\n  prettyPrint -> String"
    ));
}

#[test]
fn non_protocol_define_is_complete() {
    // "define:" on a non-Protocol receiver should not trigger the protocol check
    assert!(is_input_complete("myObj define: something"));
}

// === needs_blank_line_to_complete tests ===

#[test]
fn blank_line_submit_protocol_define() {
    // Protocol definitions need a blank line to complete
    assert!(needs_blank_line_to_complete("Protocol define: Greetable"));
    assert!(needs_blank_line_to_complete(
        "Protocol define: Greetable\n  greet -> String"
    ));
    assert!(needs_blank_line_to_complete(
        "Protocol define: Serializable\n  serialize -> String\n  deserialize: data :: String -> Self"
    ));
}

#[test]
fn blank_line_submit_class_definition_no_methods() {
    // Class header without methods — needs blank line
    assert!(needs_blank_line_to_complete("Actor subclass: Counter"));
    assert!(needs_blank_line_to_complete(
        "Actor subclass: Counter\n  state: value = 0"
    ));
}

#[test]
fn blank_line_submit_class_definition_with_state_map() {
    // Map `=>` in state initializer should not count as a method arrow
    assert!(needs_blank_line_to_complete(
        "Object subclass: Config\n  state: opts = #{verbose => true}"
    ));
}

#[test]
fn no_blank_line_submit_for_complete_input() {
    // Already complete — blank line not needed
    assert!(!needs_blank_line_to_complete("3 + 4"));
    assert!(!needs_blank_line_to_complete("x := 42"));
    assert!(!needs_blank_line_to_complete(""));
}

#[test]
fn no_blank_line_submit_for_unclosed_delimiters() {
    // Unclosed delimiters — blank line should NOT force-submit
    assert!(!needs_blank_line_to_complete("[:x | x * 2"));
    assert!(!needs_blank_line_to_complete("#{name =>"));
    assert!(!needs_blank_line_to_complete("#(1, 2"));
    assert!(!needs_blank_line_to_complete("{1, 2"));
}

#[test]
fn no_blank_line_submit_for_trailing_operators() {
    // Trailing operators — user is mid-expression
    assert!(!needs_blank_line_to_complete("x := 1 +"));
    assert!(!needs_blank_line_to_complete("array at:"));
    assert!(!needs_blank_line_to_complete("x :="));
}

#[test]
fn no_blank_line_submit_non_protocol_define() {
    // "define:" on a non-Protocol receiver should not trigger
    assert!(!needs_blank_line_to_complete("myObj define: something"));
}

#[test]
fn blank_line_submit_class_with_method_is_already_complete() {
    // Class with at least one method is already complete via is_input_complete,
    // so needs_blank_line_to_complete returns false
    assert!(!needs_blank_line_to_complete(
        "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1"
    ));
}

// === Package-qualified class references (ADR 0070) ===

#[test]
fn parse_qualified_class_reference() {
    // `json@Parser` should parse as a ClassReference with package = Some("json")
    let module = parse_ok("json@Parser");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::ClassReference {
        name,
        package,
        span,
        ..
    } = &module.expressions[0].expression
    {
        assert_eq!(name.name.as_str(), "Parser");
        let pkg = package.as_ref().expect("expected package qualifier");
        assert_eq!(pkg.name.as_str(), "json");
        // Span should cover the full `json@Parser`
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 11);
    } else {
        panic!(
            "Expected ClassReference, got {:?}",
            module.expressions[0].expression
        );
    }
}

#[test]
fn parse_qualified_class_reference_message_send() {
    // `json@Parser parse: x` — qualified class ref as message receiver
    let module = parse_ok("json@Parser parse: x");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        // Receiver should be a qualified ClassReference
        if let Expression::ClassReference { name, package, .. } = &**receiver {
            assert_eq!(name.name.as_str(), "Parser");
            assert_eq!(
                package.as_ref().expect("expected package").name.as_str(),
                "json"
            );
        } else {
            panic!("Expected ClassReference receiver, got {receiver:?}");
        }
        assert_eq!(parts.len(), 1);
        assert_eq!(parts[0].keyword.as_str(), "parse:");
        assert_eq!(arguments.len(), 1);
    } else {
        panic!("Expected keyword message send");
    }
}

#[test]
fn parse_qualified_class_reference_unary_message() {
    // `json@Parser new` — qualified class ref with unary message
    let module = parse_ok("json@Parser new");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(name),
        ..
    } = &module.expressions[0].expression
    {
        if let Expression::ClassReference {
            name: class_name,
            package,
            ..
        } = &**receiver
        {
            assert_eq!(class_name.name.as_str(), "Parser");
            assert_eq!(
                package.as_ref().expect("expected package").name.as_str(),
                "json"
            );
        } else {
            panic!("Expected ClassReference receiver");
        }
        assert_eq!(name.as_str(), "new");
    } else {
        panic!("Expected unary message send");
    }
}

#[test]
fn parse_unqualified_class_reference_has_no_package() {
    // `Parser` — plain class reference should have package = None
    let module = parse_ok("Parser");
    if let Expression::ClassReference { package, .. } = &module.expressions[0].expression {
        assert!(
            package.is_none(),
            "plain class reference should have no package"
        );
    } else {
        panic!("Expected ClassReference");
    }
}

#[test]
fn parse_qualified_class_mixed_with_directive() {
    // Directives on a previous line should not interfere with qualified class refs
    let module = parse_ok("json@Parser");
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        &module.expressions[0].expression,
        Expression::ClassReference { .. }
    ));
}

#[test]
fn parse_at_parser_no_package_error_recovery() {
    // `@Parser` — missing package name. This is lexed as an error token.
    let diagnostics = parse_err("@Parser");
    // Should produce at least one diagnostic (from the error token)
    assert!(
        !diagnostics.is_empty(),
        "expected error for @Parser without package"
    );
}

#[test]
fn parse_qualified_class_reference_spans() {
    // Verify span precision for `json@Parser`
    let module = parse_ok("json@Parser");
    if let Expression::ClassReference {
        name,
        package,
        span,
        ..
    } = &module.expressions[0].expression
    {
        // Package span: 0..4
        let pkg = package.as_ref().unwrap();
        assert_eq!(pkg.span.start(), 0);
        assert_eq!(pkg.span.end(), 4);
        // Class name span: 5..11
        assert_eq!(name.span.start(), 5);
        assert_eq!(name.span.end(), 11);
        // Full span: 0..11
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 11);
    } else {
        panic!("Expected ClassReference");
    }
}

#[test]
fn parse_qualified_lowercase_after_at_is_error() {
    // `json@parser` — lowercase after `@` is not a valid class name
    let diagnostics = parse_err("json@parser");
    assert!(
        !diagnostics.is_empty(),
        "expected error for lowercase class name after @"
    );
}

// ========================================================================
// Package-qualified standalone method definitions (ADR 0070 Phase 2, BT-1651)
// ========================================================================

#[test]
fn parse_qualified_standalone_method_definition() {
    // `json@Parser >> lenient => 42` — cross-package extension method
    let module = parse_ok("json@Parser >> lenient => 42");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Parser");
    let pkg = method_def
        .package
        .as_ref()
        .expect("expected package qualifier");
    assert_eq!(pkg.name.as_str(), "json");
    assert!(!method_def.is_class_method);
}

#[test]
fn parse_qualified_standalone_class_method() {
    // `json@Parser class >> fromString: s => 42` — cross-package class-side extension
    let module = parse_ok("json@Parser class >> fromString: s => 42");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Parser");
    assert_eq!(
        method_def
            .package
            .as_ref()
            .expect("expected package")
            .name
            .as_str(),
        "json"
    );
    assert!(method_def.is_class_method);
}

#[test]
fn parse_unqualified_standalone_method_has_no_package() {
    // `Counter >> increment => 42` — no package qualifier
    let module = parse_ok("Counter >> increment => 42");
    assert_eq!(module.method_definitions.len(), 1);
    assert!(
        module.method_definitions[0].package.is_none(),
        "unqualified standalone method should have package = None"
    );
}

// ========================================================================
// Package-qualified superclass in class definitions (ADR 0070 Phase 2, BT-1651)
// ========================================================================

#[test]
fn parse_qualified_superclass() {
    // `json@Parser subclass: LenientParser` — subclassing from another package
    let module = parse_ok("json@Parser subclass: LenientParser\n  parse => 42");
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.name.name.as_str(), "LenientParser");
    let superclass = class.superclass.as_ref().expect("expected superclass");
    assert_eq!(superclass.name.as_str(), "Parser");
    let pkg = class
        .superclass_package
        .as_ref()
        .expect("expected superclass_package");
    assert_eq!(pkg.name.as_str(), "json");
}

#[test]
fn parse_unqualified_superclass_has_no_package() {
    // `Object subclass: Foo` — no package qualifier on superclass
    let module = parse_ok("Object subclass: Foo\n  bar => 42");
    assert_eq!(module.classes.len(), 1);
    assert!(
        module.classes[0].superclass_package.is_none(),
        "unqualified superclass should have superclass_package = None"
    );
}

#[test]
fn parse_self_class_return_type() {
    // BT-1952: `Self class` metatype annotation
    let module = parse_ok(
        "Object subclass: Foo
  class -> Self class => @primitive \"class\"",
    );
    assert_eq!(module.classes.len(), 1);
    let methods = &module.classes[0].methods;
    assert_eq!(methods.len(), 1);
    let ret_ty = methods[0].return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::SelfClass { .. }));
}

#[test]
fn parse_self_class_type_name() {
    // BT-1952: type_name() returns "Self class"
    let ann = TypeAnnotation::SelfClass {
        span: crate::source_analysis::Span::new(0, 0),
    };
    assert_eq!(ann.type_name().as_str(), "Self class");
}

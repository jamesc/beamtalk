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

#[test]
fn parse_self_class_union_return_type() {
    // BT-1952: `-> Self class | Nil` should be recognized as a method definition
    let module = parse_ok(
        "Object subclass: Bar
  maybeClass -> Self class | Nil => nil",
    );
    assert_eq!(module.classes.len(), 1);
    let methods = &module.classes[0].methods;
    assert_eq!(methods.len(), 1);
    let ret_ty = methods[0].return_type.as_ref().unwrap();
    assert!(
        matches!(ret_ty, TypeAnnotation::Union { types, .. } if types.len() == 2),
        "Expected Union type, got: {ret_ty:?}"
    );
}

// ---- BT-2034: <ClassName> class metatype annotations ----

#[test]
fn parse_class_of_field_annotation() {
    // BT-2034: `Actor class | Nil` field annotation parses as Union(ClassOf, Nil)
    let module = parse_ok(
        "typed Value subclass: Spec
  field: cls :: Actor class | Nil = nil",
    );
    assert_eq!(module.classes.len(), 1);
    let state = &module.classes[0].state;
    assert_eq!(state.len(), 1);
    let ann = state[0].type_annotation.as_ref().unwrap();
    let TypeAnnotation::Union { types, .. } = ann else {
        panic!("Expected Union, got {ann:?}");
    };
    assert_eq!(types.len(), 2);
    assert!(
        matches!(&types[0], TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {:?}",
        types[0]
    );
}

#[test]
fn parse_class_of_return_type() {
    // BT-2034: `-> Actor class` return type parses as ClassOf
    let module = parse_ok(
        "Object subclass: Foo
  actorClass -> Actor class => self actorClass",
    );
    assert_eq!(module.classes.len(), 1);
    let methods = &module.classes[0].methods;
    assert_eq!(methods.len(), 1);
    let ret_ty = methods[0].return_type.as_ref().unwrap();
    assert!(
        matches!(ret_ty, TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {ret_ty:?}"
    );
}

#[test]
fn parse_class_of_type_name() {
    // BT-2034: type_name() for ClassOf returns "<Name> class"
    let ann = TypeAnnotation::ClassOf {
        class_name: crate::ast::Identifier::new("Actor", crate::source_analysis::Span::new(0, 5)),
        span: crate::source_analysis::Span::new(0, 11),
    };
    assert_eq!(ann.type_name().as_str(), "Actor class");
}

#[test]
fn parse_trailing_class_on_next_line_is_separate_method() {
    // BT-2034: `-> Foo\nclass bar => ...` must not consume `class` into the
    // return type as a metatype — it starts a class-method definition on the
    // next line.
    let module = parse_ok(
        "Object subclass: Bar
  foo -> Integer => 1
  class bar -> Integer => 2",
    );
    let cls = &module.classes[0];
    assert_eq!(cls.methods.len(), 1);
    assert_eq!(cls.class_methods.len(), 1);
    assert_eq!(cls.methods[0].selector.name(), "foo");
    assert_eq!(cls.class_methods[0].selector.name(), "bar");
    // And `foo`'s return type is a plain Simple(Integer), not ClassOf
    let ret_ty = cls.methods[0].return_type.as_ref().unwrap();
    assert!(
        matches!(ret_ty, TypeAnnotation::Simple(id) if id.name == "Integer"),
        "Expected Simple(Integer), got {ret_ty:?}"
    );
}

#[test]
fn parse_trailing_self_class_on_next_line_is_separate_method() {
    // BT-2034: the same-line `class` guard must also apply to `Self` so that
    // `-> Self\nclass bar => ...` parses as `Self` followed by a class-method
    // definition on the next line (not `Self class`).
    let module = parse_ok(
        "Object subclass: Bar
  foo -> Self => self
  class bar -> Integer => 2",
    );
    let cls = &module.classes[0];
    assert_eq!(cls.methods.len(), 1);
    assert_eq!(cls.class_methods.len(), 1);
    let ret_ty = cls.methods[0].return_type.as_ref().unwrap();
    assert!(
        matches!(ret_ty, TypeAnnotation::SelfType { .. }),
        "Expected SelfType, got {ret_ty:?}"
    );
}

#[test]
fn parse_binary_method_with_class_metatype_param() {
    // BT-2034: `+ other :: Actor class => ...` must be recognized as a
    // binary method definition — the `::` lookahead needs to skip the
    // `class` metatype suffix for method detection to succeed.
    let module = parse_ok(
        "Object subclass: Registry
  + other :: Actor class => other",
    );
    let cls = &module.classes[0];
    assert_eq!(cls.methods.len(), 1);
    assert_eq!(cls.methods[0].selector.name(), "+");
    let param_ty = cls.methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    assert!(
        matches!(param_ty, TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {param_ty:?}"
    );
}

// ==========================================================================
// Singleton type annotations (`#foo`) — subtypes of `Symbol` (ADR 0068, BT-2627)
// ==========================================================================

/// Helper: assert a `TypeAnnotation` is the singleton `#name`.
fn assert_singleton(ty: &TypeAnnotation, name: &str) {
    match ty {
        TypeAnnotation::Singleton { name: n, .. } => assert_eq!(n.as_str(), name),
        other => panic!("expected singleton #{name}, got {other:?}"),
    }
}

#[test]
fn parse_bare_singleton_param() {
    let module = parse_ok(
        "Object subclass: D
  m: x :: #infinity => x",
    );
    let param = &module.classes[0].methods[0].parameters[0];
    assert_singleton(param.type_annotation.as_ref().unwrap(), "infinity");
}

#[test]
fn parse_singleton_union_param() {
    let module = parse_ok(
        "Object subclass: D
  m: x :: Integer | #infinity => x",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    let TypeAnnotation::Union { types, .. } = ty else {
        panic!("expected union, got {ty:?}");
    };
    assert_eq!(types.len(), 2);
    assert!(matches!(&types[0], TypeAnnotation::Simple(id) if id.name == "Integer"));
    assert_singleton(&types[1], "infinity");
}

#[test]
fn parse_multi_singleton_union_param() {
    let module = parse_ok(
        "Object subclass: D
  restart: policy :: #temporary | #transient | #permanent => policy",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    let TypeAnnotation::Union { types, .. } = ty else {
        panic!("expected union, got {ty:?}");
    };
    assert_eq!(types.len(), 3);
    assert_singleton(&types[0], "temporary");
    assert_singleton(&types[1], "transient");
    assert_singleton(&types[2], "permanent");
}

#[test]
fn parse_singleton_union_return_type() {
    let module = parse_ok(
        "Object subclass: D
  m -> Integer | #infinity => 1",
    );
    let ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let TypeAnnotation::Union { types, .. } = ty else {
        panic!("expected union return type, got {ty:?}");
    };
    assert_eq!(types.len(), 2);
    assert_singleton(&types[1], "infinity");
}

#[test]
fn parse_singleton_union_field() {
    let module = parse_ok(
        "Object subclass: D
  field: timeout :: Integer | #infinity = 1",
    );
    let ty = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    let TypeAnnotation::Union { types, .. } = ty else {
        panic!("expected union field type, got {ty:?}");
    };
    assert_singleton(&types[1], "infinity");
}

#[test]
fn parse_singleton_in_binary_method_param() {
    let module = parse_ok(
        "Object subclass: D
  + other :: Integer | #infinity => other",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    assert!(matches!(ty, TypeAnnotation::Union { .. }));
}

#[test]
fn parse_singleton_inside_generic_param() {
    let module = parse_ok(
        "Object subclass: D
  m: xs :: List(#infinity) => xs",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    else {
        panic!("expected generic, got {ty:?}");
    };
    assert_eq!(base.name, "List");
    assert_singleton(&parameters[0], "infinity");
}

#[test]
fn parse_singleton_does_not_consume_class_suffix() {
    // BT-2627 review: `#foo` is a single token with no metatype surface, so the
    // signature lookahead must not consume a trailing `class` (which would
    // diverge from `parse_single_type_annotation` and orphan the `class`). A
    // valid singleton union still parses cleanly...
    let module = parse_ok(
        "Object subclass: D
  m: x :: Integer | #infinity => x",
    );
    assert!(matches!(
        module.classes[0].methods[0].parameters[0].type_annotation,
        Some(TypeAnnotation::Union { .. })
    ));

    // ...and the bare singleton resolves to exactly the `Singleton`, leaving the
    // following token untouched (here a real next statement).
    let module2 = parse_ok(
        "Object subclass: D
  m: x :: #infinity => x",
    );
    assert_singleton(
        module2.classes[0].methods[0].parameters[0]
            .type_annotation
            .as_ref()
            .unwrap(),
        "infinity",
    );
}

// ========================================================================
// Class metatype inside generic type arguments (BT-2630)
// ========================================================================

#[test]
fn parse_class_metatype_inside_generic_type() {
    // BT-2630: `List(Actor class)` must parse as a Generic whose argument is ClassOf(Actor)
    let module = parse_ok(
        "Object subclass: Foo
  m: x :: List(Actor class) => x",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    else {
        panic!("expected generic, got {ty:?}");
    };
    assert_eq!(base.name, "List");
    assert_eq!(parameters.len(), 1);
    assert!(
        matches!(&parameters[0], TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {:?}",
        parameters[0]
    );
}

#[test]
fn parse_class_metatype_inside_nested_generic() {
    // BT-2630: `Map(String, Actor class)` must parse with ClassOf(Actor) as second param
    let module = parse_ok(
        "Object subclass: Foo
  m: x :: Map(String, Actor class) => x",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    else {
        panic!("expected generic, got {ty:?}");
    };
    assert_eq!(base.name, "Map");
    assert_eq!(parameters.len(), 2);
    assert!(
        matches!(&parameters[0], TypeAnnotation::Simple(id) if id.name == "String"),
        "Expected Simple(String), got {:?}",
        parameters[0]
    );
    assert!(
        matches!(&parameters[1], TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {:?}",
        parameters[1]
    );
}

#[test]
fn parse_class_metatype_generic_in_return_type() {
    // BT-2630: `-> List(Actor class)` return type must be recognized
    let module = parse_ok(
        "Object subclass: Foo
  getActors -> List(Actor class) => nil",
    );
    let method = &module.classes[0].methods[0];
    let ret_ty = method.return_type.as_ref().unwrap();
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = ret_ty
    else {
        panic!("expected generic return type, got {ret_ty:?}");
    };
    assert_eq!(base.name, "List");
    assert_eq!(parameters.len(), 1);
    assert!(
        matches!(&parameters[0], TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {:?}",
        parameters[0]
    );
}

#[test]
fn parse_class_metatype_in_generic_union_arg() {
    // BT-2630: `List(Actor class | Nil)` — metatype as the *first* union member
    // inside a generic type arg. Exercises the first-param path in
    // `skip_paren_type_params` (the `skip_type_name_with_metatype` call at the
    // "First type param" block). The trailing `Nil` is a plain identifier.
    let module = parse_ok(
        "Object subclass: Foo
  m: x :: List(Actor class | Nil) => x",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    else {
        panic!("expected generic, got {ty:?}");
    };
    assert_eq!(base.name, "List");
    assert_eq!(parameters.len(), 1);
    let TypeAnnotation::Union { types, .. } = &parameters[0] else {
        panic!("expected union arg, got {:?}", parameters[0]);
    };
    assert_eq!(types.len(), 2);
    assert!(
        matches!(&types[0], TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {:?}",
        types[0]
    );
    assert!(
        matches!(&types[1], TypeAnnotation::Simple(id) if id.name == "Nil"),
        "Expected Simple(Nil), got {:?}",
        types[1]
    );
}

#[test]
fn parse_class_metatype_as_later_union_member_in_generic_arg() {
    // BT-2630: `List(Foo | Actor class)` — metatype as a *later* union member
    // inside a generic type arg. This is the path the pipe-union loop in
    // `skip_paren_type_params` must handle: the lookahead has to consume the
    // trailing `class` on `Actor` in lock-step with `parse_single_type_annotation`.
    let module = parse_ok(
        "Object subclass: Foo
  m: x :: List(Foo | Actor class) => x",
    );
    let ty = module.classes[0].methods[0].parameters[0]
        .type_annotation
        .as_ref()
        .unwrap();
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    else {
        panic!("expected generic, got {ty:?}");
    };
    assert_eq!(base.name, "List");
    assert_eq!(parameters.len(), 1);
    let TypeAnnotation::Union { types, .. } = &parameters[0] else {
        panic!("expected union arg, got {:?}", parameters[0]);
    };
    assert_eq!(types.len(), 2);
    assert!(
        matches!(&types[0], TypeAnnotation::Simple(id) if id.name == "Foo"),
        "Expected Simple(Foo), got {:?}",
        types[0]
    );
    assert!(
        matches!(&types[1], TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"),
        "Expected ClassOf(Actor), got {:?}",
        types[1]
    );
}

// ==========================================================================
// Difference type annotations (`\`) — ADR 0102 §1/§3, BT-2742
// ==========================================================================

#[test]
fn parse_difference_return_type() {
    // `Symbol \ #foo` parses as Difference { base: Symbol, excluded: #foo }.
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> Symbol \\ #foo => #bar",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let TypeAnnotation::Difference { base, excluded, .. } = ret_ty else {
        panic!("expected Difference, got {ret_ty:?}");
    };
    assert!(matches!(base.as_ref(), TypeAnnotation::Simple(id) if id.name == "Symbol"));
    assert!(matches!(excluded.as_ref(), TypeAnnotation::Singleton { name, .. } if name == "foo"));
}

#[test]
fn parse_difference_state_field() {
    // `field: x :: Symbol \ #foo = nil` in a typed class.
    let module = parse_ok(
        "typed Value subclass: Spec
  field: tag :: Symbol \\ #foo = #bar",
    );
    let ann = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    assert!(
        matches!(ann, TypeAnnotation::Difference { .. }),
        "expected Difference, got {ann:?}"
    );
}

#[test]
fn parse_difference_binds_tighter_than_union() {
    // ADR 0102 §3: `Integer | Symbol \ #foo` parses as
    // `Integer | (Symbol \ #foo)` — `\` binds tighter than `|`.
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> Integer | Symbol \\ #foo => 1",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let TypeAnnotation::Union { types, .. } = ret_ty else {
        panic!("expected Union, got {ret_ty:?}");
    };
    assert_eq!(types.len(), 2, "union has two members: {types:?}");
    assert!(
        matches!(&types[0], TypeAnnotation::Simple(id) if id.name == "Integer"),
        "first member is Integer, got {:?}",
        types[0]
    );
    let TypeAnnotation::Difference { base, excluded, .. } = &types[1] else {
        panic!("second member must be a Difference, got {:?}", types[1]);
    };
    assert!(matches!(base.as_ref(), TypeAnnotation::Simple(id) if id.name == "Symbol"));
    assert!(matches!(excluded.as_ref(), TypeAnnotation::Singleton { name, .. } if name == "foo"));
}

#[test]
fn parse_difference_is_left_associative() {
    // ADR 0102 §3: `Symbol \ #a \ #b` parses as `(Symbol \ #a) \ #b`.
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> Symbol \\ #a \\ #b => #c",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let TypeAnnotation::Difference { base, excluded, .. } = ret_ty else {
        panic!("expected outer Difference, got {ret_ty:?}");
    };
    // Outer excluded is #b; the outer base is the inner `Symbol \ #a`.
    assert!(matches!(excluded.as_ref(), TypeAnnotation::Singleton { name, .. } if name == "b"));
    let TypeAnnotation::Difference {
        base: inner_base,
        excluded: inner_excluded,
        ..
    } = base.as_ref()
    else {
        panic!("expected inner Difference in base, got {base:?}");
    };
    assert!(matches!(inner_base.as_ref(), TypeAnnotation::Simple(id) if id.name == "Symbol"));
    assert!(
        matches!(inner_excluded.as_ref(), TypeAnnotation::Singleton { name, .. } if name == "a")
    );
}

#[test]
fn parse_difference_type_name_round_trips() {
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> Symbol \\ #foo => #bar",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    assert_eq!(ret_ty.type_name().as_str(), "Symbol \\ #foo");
}

#[test]
fn parse_double_backslash_in_type_position_is_typo_error() {
    // The lexer greedily merges `\\` into the modulo selector. In type position
    // that is almost certainly a typo for the difference operator `\`, so we
    // emit a targeted diagnostic pointing at `\`.
    let diagnostics = parse_err(
        "Object subclass: Foo
  narrow -> Symbol \\\\ #foo => #bar",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("did you mean") && d.message.contains('\\')),
        "expected a `did you mean \\` diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn value_position_backslash_is_unchanged() {
    // The `\` difference operator is special-cased *only* in type-annotation
    // position (BT-2742). In a value expression `\` remains an ordinary binary
    // selector with no binding power, so `x \ x` parses exactly as it did
    // before this change — an "expected expression" error, never a silent
    // type-difference. The method header `combine: x =>` is still detected,
    // proving the type-chain lookahead did not leak into value position.
    let diagnostics = parse_err(
        "Object subclass: Foo
  combine: x => x \\ x",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("expected expression")),
        "value-position `\\` must parse as before (unchanged), got: {diagnostics:?}"
    );
}

// ==========================================================================
// Intersection type annotations (`&`) — ADR 0068 §Protocol Composition,
// ADR 0102 §1/§3, BT-2743
// ==========================================================================

#[test]
fn parse_intersection_return_type() {
    // `Collection(Object) & Comparable` parses as
    // Intersection { left: Generic(Collection, [Object]), right: Comparable }.
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> Collection(Object) & Comparable => self",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let TypeAnnotation::Intersection { left, right, .. } = ret_ty else {
        panic!("expected Intersection, got {ret_ty:?}");
    };
    assert!(
        matches!(left.as_ref(), TypeAnnotation::Generic { base, .. } if base.name == "Collection")
    );
    assert!(matches!(right.as_ref(), TypeAnnotation::Simple(id) if id.name == "Comparable"));
}

#[test]
fn parse_intersection_state_field() {
    // `field: tag :: Printable & Comparable = nil` in a typed class.
    let module = parse_ok(
        "typed Value subclass: Spec
  field: tag :: Printable & Comparable = nil",
    );
    let ann = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    assert!(
        matches!(ann, TypeAnnotation::Intersection { .. }),
        "expected Intersection, got {ann:?}"
    );
}

#[test]
fn parse_intersection_binds_tighter_than_union() {
    // ADR 0102 §3: `Integer | Printable & Comparable` parses as
    // `Integer | (Printable & Comparable)` — `&` binds tighter than `|`.
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> Integer | Printable & Comparable => 1",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let TypeAnnotation::Union { types, .. } = ret_ty else {
        panic!("expected Union, got {ret_ty:?}");
    };
    assert_eq!(types.len(), 2, "union has two members: {types:?}");
    assert!(
        matches!(&types[0], TypeAnnotation::Simple(id) if id.name == "Integer"),
        "first member is Integer, got {:?}",
        types[0]
    );
    let TypeAnnotation::Intersection { left, right, .. } = &types[1] else {
        panic!("second member must be an Intersection, got {:?}", types[1]);
    };
    assert!(matches!(left.as_ref(), TypeAnnotation::Simple(id) if id.name == "Printable"));
    assert!(matches!(right.as_ref(), TypeAnnotation::Simple(id) if id.name == "Comparable"));
}

#[test]
fn parse_intersection_is_left_associative() {
    // ADR 0102 §3: `A & B & C` parses as `(A & B) & C`.
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> A & B & C => self",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let TypeAnnotation::Intersection { left, right, .. } = ret_ty else {
        panic!("expected outer Intersection, got {ret_ty:?}");
    };
    // Outer right is C; the outer left is the inner `A & B`.
    assert!(matches!(right.as_ref(), TypeAnnotation::Simple(id) if id.name == "C"));
    let TypeAnnotation::Intersection {
        left: inner_left,
        right: inner_right,
        ..
    } = left.as_ref()
    else {
        panic!("expected inner Intersection in left, got {left:?}");
    };
    assert!(matches!(inner_left.as_ref(), TypeAnnotation::Simple(id) if id.name == "A"));
    assert!(matches!(inner_right.as_ref(), TypeAnnotation::Simple(id) if id.name == "B"));
}

#[test]
fn parse_intersection_type_name_round_trips() {
    let module = parse_ok(
        "Object subclass: Foo
  narrow -> Printable & Comparable => self",
    );
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    assert_eq!(ret_ty.type_name().as_str(), "Printable & Comparable");
}

#[test]
fn parse_mixed_intersection_and_difference_without_parens_is_error() {
    // ADR 0102 §3: mixing `&` and `\` in the same chain without explicit
    // grouping is a deliberate parse error — `(A & B) \ C` and `A & (B \ C)`
    // differ and neither reading is obviously dominant.
    let diagnostics = parse_err(
        "Object subclass: Foo
  narrow -> A & B \\ #c => self",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("Cannot mix") && d.message.contains('&')),
        "expected a mixed-operator diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn parse_mixed_difference_and_intersection_without_parens_is_error() {
    // Same rule, opposite order: `A \ #b & C` also requires parens.
    let diagnostics = parse_err(
        "Object subclass: Foo
  narrow -> A \\ #b & C => self",
    );
    assert!(
        diagnostics.iter().any(|d| d.message.contains("Cannot mix")),
        "expected a mixed-operator diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn value_position_ampersand_is_unchanged() {
    // The `&` intersection operator is special-cased *only* in type-annotation
    // position (BT-2743), mirroring `\` (BT-2742). In a value expression `&`
    // has no binding power (like `\`), so `x & x` parses exactly as it did
    // before this change — an "expected expression" error, never a silent
    // type-intersection. The method header `combine: x =>` is still detected,
    // proving the type-chain lookahead did not leak into value position.
    let diagnostics = parse_err(
        "Object subclass: Foo
  combine: x => x & x",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("expected expression")),
        "value-position `&` must parse as before (unchanged), got: {diagnostics:?}"
    );
}

// ==========================================================================
// Grouping parentheses in type-annotation position — BT-2760, ADR 0102 §3
// ==========================================================================

/// Parses `ty` as the return type of a method and returns the annotation.
fn parse_return_type(ty: &str) -> TypeAnnotation {
    let module = parse_ok(&format!("Object subclass: Foo\n  narrow -> {ty} => self"));
    module.classes[0].methods[0]
        .return_type
        .clone()
        .expect("method must have a return type")
}

/// Asserts that unparsing (`type_name`) is a fixed point: parsing `ty`,
/// printing it, and re-parsing the printed form yields the same printed
/// form again. Combined with the shape assertions in the individual tests,
/// this checks that `type_name` re-derives grouping parens exactly where
/// precedence requires them.
fn assert_type_name_round_trips(ty: &str) {
    let parsed = parse_return_type(ty);
    let printed = parsed.type_name();
    let reparsed = parse_return_type(&printed);
    assert_eq!(
        reparsed.type_name(),
        printed,
        "type_name must be a fixed point for `{ty}`"
    );
}

#[test]
fn parse_grouped_simple_type_is_transparent() {
    // `(Integer)` parses as plain `Integer` — no extra AST node, only the
    // span widens to cover the parens.
    let ret_ty = parse_return_type("(Integer)");
    assert!(
        matches!(&ret_ty, TypeAnnotation::Simple(id) if id.name == "Integer"),
        "expected transparent Simple, got {ret_ty:?}"
    );
}

#[test]
fn parse_grouped_type_span_covers_parens() {
    let source = "Object subclass: Foo\n  narrow -> (A & B) => self";
    let module = parse_ok(source);
    let ret_ty = module.classes[0].methods[0].return_type.as_ref().unwrap();
    let span = ret_ty.span();
    assert_eq!(
        &source[span.start() as usize..span.end() as usize],
        "(A & B)",
        "group span must cover the parens"
    );
}

#[test]
fn parse_grouped_intersection_then_difference() {
    // `(A & B) \ #c` — the previously unwritable mixed form (BT-2760):
    // Difference { base: Intersection(A, B), excluded: #c }.
    let ret_ty = parse_return_type("(A & B) \\ #c");
    let TypeAnnotation::Difference { base, excluded, .. } = &ret_ty else {
        panic!("expected Difference, got {ret_ty:?}");
    };
    let TypeAnnotation::Intersection { left, right, .. } = base.as_ref() else {
        panic!("expected Intersection base, got {base:?}");
    };
    assert!(matches!(left.as_ref(), TypeAnnotation::Simple(id) if id.name == "A"));
    assert!(matches!(right.as_ref(), TypeAnnotation::Simple(id) if id.name == "B"));
    assert!(matches!(excluded.as_ref(), TypeAnnotation::Singleton { name, .. } if name == "c"));
}

#[test]
fn parse_intersection_with_grouped_difference() {
    // `A & (B \ #c)` — the other mixed reading, now writable explicitly:
    // Intersection { left: A, right: Difference(B, #c) }.
    let ret_ty = parse_return_type("A & (B \\ #c)");
    let TypeAnnotation::Intersection { left, right, .. } = &ret_ty else {
        panic!("expected Intersection, got {ret_ty:?}");
    };
    assert!(matches!(left.as_ref(), TypeAnnotation::Simple(id) if id.name == "A"));
    let TypeAnnotation::Difference { base, excluded, .. } = right.as_ref() else {
        panic!("expected Difference right, got {right:?}");
    };
    assert!(matches!(base.as_ref(), TypeAnnotation::Simple(id) if id.name == "B"));
    assert!(matches!(excluded.as_ref(), TypeAnnotation::Singleton { name, .. } if name == "c"));
}

#[test]
fn parse_difference_with_grouped_union_excluded() {
    // `Symbol \ (#a | #b)` — ADR 0102 §3's own example form:
    // Difference { base: Symbol, excluded: Union(#a, #b) }.
    let ret_ty = parse_return_type("Symbol \\ (#a | #b)");
    let TypeAnnotation::Difference { base, excluded, .. } = &ret_ty else {
        panic!("expected Difference, got {ret_ty:?}");
    };
    assert!(matches!(base.as_ref(), TypeAnnotation::Simple(id) if id.name == "Symbol"));
    let TypeAnnotation::Union { types, .. } = excluded.as_ref() else {
        panic!("expected Union excluded, got {excluded:?}");
    };
    assert_eq!(types.len(), 2);
    assert!(matches!(&types[0], TypeAnnotation::Singleton { name, .. } if name == "a"));
    assert!(matches!(&types[1], TypeAnnotation::Singleton { name, .. } if name == "b"));
}

#[test]
fn parse_union_with_grouped_difference_member() {
    // `Integer | (Symbol \ #foo)` — the group is redundant (same parse as the
    // bare form) but must still be accepted.
    let ret_ty = parse_return_type("Integer | (Symbol \\ #foo)");
    let TypeAnnotation::Union { types, .. } = &ret_ty else {
        panic!("expected Union, got {ret_ty:?}");
    };
    assert_eq!(types.len(), 2);
    assert!(matches!(&types[0], TypeAnnotation::Simple(id) if id.name == "Integer"));
    assert!(
        matches!(&types[1], TypeAnnotation::Difference { .. }),
        "second member must be a Difference, got {:?}",
        types[1]
    );
}

#[test]
fn parse_nested_groups() {
    // Groups nest: `((A & B)) \ #c` parses identically to `(A & B) \ #c`.
    let ret_ty = parse_return_type("((A & B)) \\ #c");
    let TypeAnnotation::Difference { base, .. } = &ret_ty else {
        panic!("expected Difference, got {ret_ty:?}");
    };
    assert!(
        matches!(base.as_ref(), TypeAnnotation::Intersection { .. }),
        "expected Intersection base, got {base:?}"
    );
}

#[test]
fn parse_grouped_union_member_splices_into_enclosing_union() {
    // Unions are n-ary and associative, so `(A | B) | C` splices into a flat
    // three-member union — keeping the AST canonical and `type_name`
    // round-trippable.
    let ret_ty = parse_return_type("(A | B) | C");
    let TypeAnnotation::Union { types, .. } = &ret_ty else {
        panic!("expected Union, got {ret_ty:?}");
    };
    assert_eq!(types.len(), 3, "union must be flat: {types:?}");
    assert!(matches!(&types[0], TypeAnnotation::Simple(id) if id.name == "A"));
    assert!(matches!(&types[1], TypeAnnotation::Simple(id) if id.name == "B"));
    assert!(matches!(&types[2], TypeAnnotation::Simple(id) if id.name == "C"));
}

#[test]
fn parse_grouped_right_nested_difference() {
    // `Symbol \ (#a \ #b)` — right-nested same-operator group; distinct from
    // the left-associative bare chain `Symbol \ #a \ #b`.
    let ret_ty = parse_return_type("Symbol \\ (#a \\ #b)");
    let TypeAnnotation::Difference { base, excluded, .. } = &ret_ty else {
        panic!("expected Difference, got {ret_ty:?}");
    };
    assert!(matches!(base.as_ref(), TypeAnnotation::Simple(id) if id.name == "Symbol"));
    assert!(
        matches!(excluded.as_ref(), TypeAnnotation::Difference { .. }),
        "expected right-nested Difference, got {excluded:?}"
    );
}

#[test]
fn parse_grouped_type_in_param_annotation() {
    // Grouped type on a keyword-method parameter: the method-header
    // lookahead (`skip_double_colon_type`) must recognise the group.
    let module = parse_ok(
        "Object subclass: Foo
  narrow: x :: (A & B) \\ #c => x",
    );
    let method = &module.classes[0].methods[0];
    let ann = method.parameters[0].type_annotation.as_ref().unwrap();
    assert!(
        matches!(ann, TypeAnnotation::Difference { .. }),
        "expected Difference param annotation, got {ann:?}"
    );
}

#[test]
fn parse_grouped_type_in_state_annotation() {
    let module = parse_ok(
        "typed Value subclass: Spec
  field: tag :: Symbol \\ (#a | #b) = nil",
    );
    let ann = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    assert!(
        matches!(ann, TypeAnnotation::Difference { .. }),
        "expected Difference field annotation, got {ann:?}"
    );
}

#[test]
fn parse_grouped_type_in_generic_argument() {
    // A group inside a generic argument list: `List((A & B) \ #c)`.
    let ret_ty = parse_return_type("List((A & B) \\ #c)");
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = &ret_ty
    else {
        panic!("expected Generic, got {ret_ty:?}");
    };
    assert_eq!(base.name, "List");
    assert_eq!(parameters.len(), 1);
    assert!(
        matches!(&parameters[0], TypeAnnotation::Difference { .. }),
        "expected Difference type argument, got {:?}",
        parameters[0]
    );
}

#[test]
fn parse_unparenthesised_mixed_still_errors() {
    // The bare mixed chain remains a deliberate parse error (ADR 0102 §3) —
    // grouping parens are the escape hatch, not a precedence change.
    let diagnostics = parse_err(
        "Object subclass: Foo
  narrow -> A & B \\ #c => self",
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("Cannot mix") && d.message.contains("parenthesise")),
        "expected the mixed-operator diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn parse_unclosed_group_errors() {
    // A state declaration parses its `::` annotation unconditionally (no
    // method-header lookahead), so the unclosed group reaches the annotation
    // parser and gets the targeted diagnostic. (In a method header an
    // unclosed group makes the lookahead reject the line as a header
    // entirely, so it falls back to expression-parse errors instead.)
    let diagnostics = parse_err(
        "Object subclass: Foo
  state: x :: (A | B = 1",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Expected ')' to close grouping parentheses")),
        "expected an unclosed-group diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn grouped_type_name_round_trips() {
    // `type_name` re-derives parens exactly where precedence requires them,
    // so printing and re-parsing is a fixed point.
    assert_type_name_round_trips("(A & B) \\ #c");
    assert_type_name_round_trips("A & (B \\ #c)");
    assert_type_name_round_trips("Symbol \\ (#a | #b)");
    assert_type_name_round_trips("Integer | (Symbol \\ #foo)");
    assert_type_name_round_trips("Symbol \\ (#a \\ #b)");
    assert_type_name_round_trips("A & (B & C)");
    assert_type_name_round_trips("(A | B) | C");
    assert_type_name_round_trips("(Integer)");
    assert_type_name_round_trips("List((A & B) \\ #c)");
}

#[test]
fn grouped_type_name_prints_required_parens_only() {
    // Groups that changed the parse keep their parens; redundant groups
    // collapse away.
    assert_eq!(
        parse_return_type("(A & B) \\ #c").type_name().as_str(),
        "(A & B) \\ #c"
    );
    assert_eq!(
        parse_return_type("Symbol \\ (#a | #b)")
            .type_name()
            .as_str(),
        "Symbol \\ (#a | #b)"
    );
    assert_eq!(
        parse_return_type("Symbol \\ (#a \\ #b)")
            .type_name()
            .as_str(),
        "Symbol \\ (#a \\ #b)"
    );
    // Redundant grouping collapses: `(Integer)` prints as `Integer`, and a
    // grouped tighter-binding member needs no parens under `|`.
    assert_eq!(
        parse_return_type("(Integer)").type_name().as_str(),
        "Integer"
    );
    assert_eq!(
        parse_return_type("Integer | (Symbol \\ #foo)")
            .type_name()
            .as_str(),
        "Integer | Symbol \\ #foo"
    );
}

#[test]
fn value_expression_parens_unchanged_by_type_grouping() {
    // Guard: `(...)` in a value expression still parses as a parenthesized
    // expression — the type-position grouping (BT-2760) must not leak.
    let module = parse_ok(
        "Object subclass: Foo
  combine: x => (x + 1) * 2",
    );
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "combine:");
    assert_eq!(method.body.len(), 1);
}

#[test]
fn parse_doc_examples_for_grouped_types() {
    // The exact forms documented in docs/beamtalk-language-features.md
    // (§Difference and Intersection Types) must parse.
    parse_ok(
        "Object subclass: Foo
  run =>
    tag :: Symbol \\ (#a | #b) := #c
    tag",
    );
    parse_ok(
        "Object subclass: Foo
  withSentinel: ms :: Integer | (Symbol \\ #infinity) => ms",
    );
    parse_ok(
        "Object subclass: Foo
  narrow -> (A & B) \\ #c => self",
    );
    parse_ok(
        "Object subclass: Foo
  narrow -> A & (B \\ #c) => self",
    );
}

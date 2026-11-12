// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//! Tests for class hierarchy construction, method resolution, and inheritance rules.
use super::builtins::builtin_method;
use super::*;
use crate::ast::{
    ClassDefinition, ClassKind, CommentAttachment, DeclaredKeyword, Identifier, MethodDefinition,
    ParameterDefinition, StateDeclaration, TypeAnnotation,
};
use crate::semantic_analysis::test_helpers::test_span;
use crate::source_analysis::Span;
use std::collections::BTreeSet;
// --- Value object method tests ---
#[test]
fn method_info_can_override_non_sealed() {
    let child = builtin_method("printString", 0, "Counter");
    let ancestor = builtin_method("printString", 0, "Object");
    assert!(child.can_override(&ancestor));
}
#[test]
fn method_info_cannot_override_sealed() {
    let child = builtin_method("printString", 0, "Counter");
    let ancestor = MethodInfo {
        is_sealed: true,
        ..builtin_method("printString", 0, "Actor")
    };
    assert!(!child.can_override(&ancestor));
}
#[test]
fn method_info_different_selectors_not_override() {
    let child = builtin_method("increment", 0, "Counter");
    let sealed_ancestor = MethodInfo {
        is_sealed: true,
        ..builtin_method("printString", 0, "Actor")
    };
    // Different selectors → not an override (regardless of sealed status)
    assert!(!child.can_override(&sealed_ancestor));
}
#[test]
fn class_info_can_be_subclassed() {
    let h = ClassHierarchy::with_builtins();
    assert!(h.get_class("Actor").unwrap().can_be_subclassed());
    assert!(h.get_class("Object").unwrap().can_be_subclassed());
    assert!(!h.get_class("Integer").unwrap().can_be_subclassed());
    assert!(!h.get_class("String").unwrap().can_be_subclassed());
}
// --- Hierarchy structure tests ---
#[test]
fn builtins_include_core_classes() {
    let h = ClassHierarchy::with_builtins();
    assert!(h.has_class("ProtoObject"));
    assert!(h.has_class("Object"));
    assert!(h.has_class("Actor"));
    assert!(h.has_class("Integer"));
    assert!(h.has_class("Float"));
    assert!(h.has_class("String"));
    assert!(h.has_class("List"));
    assert!(h.has_class("Dictionary"));
    assert!(h.has_class("Set"));
    assert!(h.has_class("Block"));
    assert!(h.has_class("UndefinedObject"));
    assert!(h.has_class("True"));
    assert!(h.has_class("False"));
    assert!(h.has_class("Collection"));
}
#[test]
fn proto_object_has_no_superclass() {
    let h = ClassHierarchy::with_builtins();
    let proto = h.get_class("ProtoObject").unwrap();
    assert!(proto.superclass.is_none());
}
#[test]
fn object_extends_proto_object() {
    let h = ClassHierarchy::with_builtins();
    let obj = h.get_class("Object").unwrap();
    assert_eq!(obj.superclass.as_deref(), Some("ProtoObject"));
}
#[test]
fn actor_extends_object() {
    let h = ClassHierarchy::with_builtins();
    let actor = h.get_class("Actor").unwrap();
    assert_eq!(actor.superclass.as_deref(), Some("Object"));
}
#[test]
fn integer_is_sealed() {
    let h = ClassHierarchy::with_builtins();
    let int = h.get_class("Integer").unwrap();
    assert!(int.is_sealed);
}
#[test]
fn float_is_sealed() {
    let h = ClassHierarchy::with_builtins();
    let float = h.get_class("Float").unwrap();
    assert!(float.is_sealed);
}
// --- Superclass chain tests ---
#[test]
fn superclass_chain_for_actor() {
    let h = ClassHierarchy::with_builtins();
    let chain = h.superclass_chain("Actor");
    assert_eq!(
        chain,
        vec![EcoString::from("Object"), EcoString::from("ProtoObject")]
    );
}
#[test]
fn superclass_chain_for_proto_object_is_empty() {
    let h = ClassHierarchy::with_builtins();
    let chain = h.superclass_chain("ProtoObject");
    assert!(chain.is_empty());
}
#[test]
fn superclass_chain_for_integer() {
    let h = ClassHierarchy::with_builtins();
    let chain = h.superclass_chain("Integer");
    assert_eq!(
        chain,
        vec![
            EcoString::from("Number"),
            EcoString::from("Value"),
            EcoString::from("Object"),
            EcoString::from("ProtoObject"),
        ]
    );
}
#[test]
fn superclass_chain_for_unknown_class() {
    let h = ClassHierarchy::with_builtins();
    let chain = h.superclass_chain("DoesNotExist");
    assert!(chain.is_empty());
}

// --- Subclass query tests ---
#[test]
fn direct_subclasses_of_object_includes_actor_and_value() {
    let h = ClassHierarchy::with_builtins();
    let mut subs = h.direct_subclasses("Object");
    subs.sort();
    assert!(
        subs.iter().any(|s| s.as_str() == "Actor"),
        "Object's direct subclasses should include Actor, got {subs:?}"
    );
    assert!(
        subs.iter().any(|s| s.as_str() == "Value"),
        "Object's direct subclasses should include Value, got {subs:?}"
    );
}

#[test]
fn direct_subclasses_of_leaf_class_is_empty() {
    let h = ClassHierarchy::with_builtins();
    // `Character` is a leaf in the builtin hierarchy: it extends
    // `Integer` (Character inherits from Integer), but nothing extends
    // `Character`. Pick it for the leaf check — `Integer` itself has
    // `Character` as a direct subclass and would not be a valid leaf
    // example.
    let subs = h.direct_subclasses("Character");
    assert!(
        subs.is_empty(),
        "Character should have no builtin subclasses, got {subs:?}"
    );
}

#[test]
fn direct_subclasses_of_unknown_class_is_empty() {
    let h = ClassHierarchy::with_builtins();
    let subs = h.direct_subclasses("DoesNotExist");
    assert!(subs.is_empty());
}

#[test]
fn all_subclasses_transitively_includes_grandchildren() {
    let h = ClassHierarchy::with_builtins();
    // ProtoObject -> Object -> {Actor, Value, ...}; transitive subclasses
    // of ProtoObject include Object, Actor, Number, Integer, ...
    let subs = h.all_subclasses("ProtoObject");
    assert!(
        subs.iter().any(|s| s.as_str() == "Object"),
        "expected Object in transitive subclasses of ProtoObject, got {subs:?}"
    );
    assert!(
        subs.iter().any(|s| s.as_str() == "Integer"),
        "expected Integer in transitive subclasses of ProtoObject, got {subs:?}"
    );
    // The receiver itself must not appear.
    assert!(
        !subs.iter().any(|s| s.as_str() == "ProtoObject"),
        "all_subclasses must not include the receiver, got {subs:?}"
    );
}

#[test]
fn all_subclasses_of_leaf_is_empty() {
    let h = ClassHierarchy::with_builtins();
    let subs = h.all_subclasses("Character");
    assert!(subs.is_empty(), "Character has no builtin subclasses");
}

#[test]
fn all_subclasses_breadth_first_orders_direct_before_indirect() {
    let h = ClassHierarchy::with_builtins();
    let subs = h.all_subclasses("ProtoObject");
    let object_pos = subs.iter().position(|s| s.as_str() == "Object");
    let integer_pos = subs.iter().position(|s| s.as_str() == "Integer");
    assert!(
        object_pos.is_some() && integer_pos.is_some(),
        "expected both Object and Integer in subclasses, got {subs:?}"
    );
    assert!(
        object_pos < integer_pos,
        "Object (direct child) should precede Integer (grandchild via Number->Value->Object), got order {subs:?}"
    );
}

// --- all_methods tests ---
#[test]
fn all_methods_includes_inherited() {
    let h = ClassHierarchy::with_builtins();
    let methods = h.all_methods("Actor");
    let selectors: Vec<&str> = methods.iter().map(|m| m.selector.as_str()).collect();
    // Actor instance methods (spawn/spawnWith: are class-side, not instance-side)
    assert!(selectors.contains(&"stop"));
    assert!(selectors.contains(&"isAlive"));
    assert!(
        !selectors.contains(&"spawn"),
        "spawn must NOT be in instance methods"
    );
    assert!(
        !selectors.contains(&"spawnWith:"),
        "spawnWith: must NOT be in instance methods"
    );
    // Note: `new` is still in Object's instance methods and thus inherited here
    // Inherited from Object
    assert!(selectors.contains(&"isNil"));
    assert!(selectors.contains(&"respondsTo:"));
    // Inherited from ProtoObject
    assert!(selectors.contains(&"class"));
    assert!(selectors.contains(&"=="));
}
#[test]
fn actor_spawn_methods_are_class_side() {
    let h = ClassHierarchy::with_builtins();
    let class_methods = h.all_class_methods("Actor");
    let selectors: Vec<&str> = class_methods.iter().map(|m| m.selector.as_str()).collect();
    assert!(
        selectors.contains(&"spawn"),
        "spawn must be in class_methods"
    );
    assert!(
        selectors.contains(&"spawnWith:"),
        "spawnWith: must be in class_methods"
    );
    // new/new: are on Actor's class_methods as real, documented `class
    // sealed new` / `new:` declarations in actor.bt, not as codegen-injected
    // error stubs. This does NOT reopen instantiation: the compiled body
    // still always raises `instantiation_error`, and
    // `check_actor_new_usage` (a hard compile-time error,
    // unaffected by hierarchy method resolution — see
    // `test_actor_new_error_in_standalone_method` in
    // `semantic_analysis::tests`) still rejects `ActorSubclass new` before
    // this method is ever reached.
    assert!(
        selectors.contains(&"new"),
        "new must be in Actor class_methods after BT-3071"
    );
    assert!(
        selectors.contains(&"new:"),
        "new: must be in Actor class_methods after BT-3071"
    );
    // Instance methods must NOT appear on class side
    assert!(
        !selectors.contains(&"stop"),
        "stop must NOT be in class_methods"
    );
    assert!(
        !selectors.contains(&"isAlive"),
        "isAlive must NOT be in class_methods"
    );
    // Actor class methods carry doc strings from generated_builtins.rs
    // (sourced from actor.bt via build-stdlib)
    let spawn = class_methods
        .iter()
        .find(|m| m.selector == "spawn")
        .unwrap();
    let spawn_doc = spawn.doc.as_deref().unwrap();
    assert!(
        spawn_doc.contains("actor process"),
        "spawn doc should describe actor process creation: {spawn_doc}"
    );
    let spawn_with = class_methods
        .iter()
        .find(|m| m.selector == "spawnWith:")
        .unwrap();
    assert!(
        spawn_with.doc.as_deref().unwrap().contains("actor"),
        "spawnWith: doc should describe actor creation"
    );
    // The codegen-injected error-stub overrides of new/new: are gone — they
    // no longer appear separately in Actor class_methods.
}
#[test]
fn all_methods_overrides_use_most_specific() {
    let h = ClassHierarchy::with_builtins();
    let methods = h.all_methods("Actor");
    // Object defines 'printString', Actor inherits it
    // Only the most-specific (Object's) version should appear once
    let print_methods: Vec<&MethodInfo> = methods
        .iter()
        .filter(|m| m.selector == "printString")
        .collect();
    assert_eq!(print_methods.len(), 1);
    assert_eq!(print_methods[0].defined_in.as_str(), "Object");
}
#[test]
fn all_methods_for_unknown_class_is_empty() {
    let h = ClassHierarchy::with_builtins();
    let methods = h.all_methods("DoesNotExist");
    assert!(methods.is_empty());
}
// --- resolves_selector tests ---
#[test]
fn resolves_selector_local() {
    let h = ClassHierarchy::with_builtins();
    assert!(h.resolves_selector("Integer", "+"));
    assert!(h.resolves_selector("Integer", "abs"));
}
#[test]
fn resolves_selector_inherited() {
    let h = ClassHierarchy::with_builtins();
    // Integer inherits from Object which has isNil
    assert!(h.resolves_selector("Integer", "isNil"));
    // Integer inherits from ProtoObject which has class
    assert!(h.resolves_selector("Integer", "class"));
}
#[test]
fn resolves_selector_unknown_returns_false() {
    let h = ClassHierarchy::with_builtins();
    assert!(!h.resolves_selector("Integer", "nonExistentMethod"));
}
#[test]
fn resolves_selector_unknown_class_returns_false() {
    let h = ClassHierarchy::with_builtins();
    assert!(!h.resolves_selector("Nope", "anything"));
    // Guard: Object methods must NOT resolve for a completely-unknown root class.
    assert!(!h.resolves_selector("Nope", "isNil"));
    assert!(!h.resolves_selector("Nope", "subclassResponsibility"));
}
/// When a class inherits from an external class (defined in a
/// separately-compiled file and therefore absent from the hierarchy), the
/// walk must fall through to Object so that Object-level methods are
/// still visible.
#[test]
fn resolves_selector_through_unknown_external_parent_to_object() {
    // Simulate file 2 compiling CondimentDecorator (extends Beverage from file 1).
    // Beverage is NOT in the hierarchy — only CondimentDecorator is.
    // subclassResponsibility lives on Object, two hops above CondimentDecorator.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("CondimentDecorator", "Beverage")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, _diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    // Object method must be reachable even though Beverage is unknown.
    assert!(
        h.resolves_selector("CondimentDecorator", "subclassResponsibility"),
        "subclassResponsibility (on Object) should resolve through unknown Beverage parent"
    );
    assert!(
        h.resolves_selector("CondimentDecorator", "isNil"),
        "isNil (on Object) should resolve through unknown Beverage parent"
    );
    // A genuinely non-existent method must still return false.
    assert!(
        !h.resolves_selector("CondimentDecorator", "totallyBogusMethod"),
        "unknown selector should still return false"
    );
    // find_method must also traverse through the unknown parent.
    let method = h.find_method("CondimentDecorator", "subclassResponsibility");
    assert!(
        method.is_some(),
        "find_method should locate subclassResponsibility via Object"
    );
    assert_eq!(method.unwrap().defined_in.as_str(), "Object");
}
// --- User-defined class tests ---
fn make_user_class(name: &str, superclass: &str) -> ClassDefinition {
    ClassDefinition {
        name: Identifier::new(name, test_span()),
        superclass: Some(Identifier::new(superclass, test_span())),
        superclass_package: None,
        class_kind: ClassKind::from_superclass_name(superclass),
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("count", test_span()),
            type_annotation: None,
            default_value: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            slot_kind: crate::ast::SlotKind::default(),
            span: test_span(),
        }],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary("increment".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    }
}
#[test]
fn user_defined_class_added_to_hierarchy() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert!(diags.is_empty());
    assert!(h.has_class("Counter"));
    let counter = h.get_class("Counter").unwrap();
    assert_eq!(counter.superclass.as_deref(), Some("Actor"));
    assert_eq!(counter.state, vec![EcoString::from("count")]);
    assert_eq!(counter.methods.len(), 1);
    assert_eq!(counter.methods[0].selector.as_str(), "increment");
}
#[test]
fn user_class_inherits_from_actor_chain() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let h = ClassHierarchy::build(&module).0.unwrap();
    let chain = h.superclass_chain("Counter");
    assert_eq!(
        chain,
        vec![
            EcoString::from("Actor"),
            EcoString::from("Object"),
            EcoString::from("ProtoObject"),
        ]
    );
}
#[test]
fn user_class_resolves_inherited_selectors() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let h = ClassHierarchy::build(&module).0.unwrap();
    // Local method
    assert!(h.resolves_selector("Counter", "increment"));
    // spawn is class-side on Actor — verify via all_class_methods
    assert!(
        h.all_class_methods("Counter")
            .iter()
            .any(|m| m.selector.as_str() == "spawn"),
        "Counter should inherit class-side spawn from Actor"
    );
    // Inherited from Object
    assert!(h.resolves_selector("Counter", "respondsTo:"));
    // Inherited from ProtoObject
    assert!(h.resolves_selector("Counter", "class"));
}
// --- Sealed class enforcement tests ---
#[test]
fn sealed_class_subclassing_rejected() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("MyInt", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed"));
    assert!(diags[0].message.contains("Integer"));
    // Class IS still registered (so codegen can route correctly despite error)
    assert!(h.has_class("MyInt"));
}
#[test]
fn sealed_string_subclassing_rejected() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("MyString", "String")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build(&module);
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed"));
}
#[test]
fn non_sealed_subclassing_allowed() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("MyActor", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert!(diags.is_empty());
    assert!(h.has_class("MyActor"));
}
// --- stdlib_mode gating tests ---
#[test]
fn stdlib_mode_exempts_builtin_class_from_sealed_check() {
    // In stdlib_mode, a class named "Character" extending sealed "Integer" is allowed.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Character", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build_with_options(&module, true);
    let h = h.unwrap();
    assert!(
        diags.is_empty(),
        "stdlib_mode should exempt builtin class: {diags:?}"
    );
    assert!(h.has_class("Character"));
}
#[test]
fn user_code_character_subclassing_integer_rejected() {
    // Without stdlib_mode, even a class named "Character" is rejected.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Character", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build_with_options(&module, false);
    let h = h.unwrap();
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed"));
    assert!(diags[0].message.contains("Integer"));
    // Class is still registered despite the error
    assert!(h.has_class("Character"));
}
#[test]
fn stdlib_mode_does_not_exempt_non_builtin_class() {
    // Even in stdlib_mode, a non-builtin name extending a sealed class is rejected.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("MyCustomClass", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build_with_options(&module, true);
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed"));
}
// --- Character hierarchy tests ---
#[test]
fn character_superclass_chain_includes_integer() {
    // Character inherits from Integer in the builtin hierarchy.
    let h = ClassHierarchy::with_builtins();
    let chain = h.superclass_chain("Character");
    assert!(
        chain.contains(&"Integer".into()),
        "Chain should include Integer: {chain:?}"
    );
    assert!(
        chain.contains(&"Number".into()),
        "Chain should include Number: {chain:?}"
    );
}
#[test]
fn character_resolves_integer_selectors() {
    // Character should resolve Integer methods via inheritance.
    let h = ClassHierarchy::with_builtins();
    assert!(
        h.resolves_selector("Character", "+"),
        "Character should understand '+'"
    );
    assert!(
        h.resolves_selector("Character", "-"),
        "Character should understand '-'"
    );
    assert!(
        h.resolves_selector("Character", "*"),
        "Character should understand '*'"
    );
    assert!(
        h.resolves_selector("Character", "isLetter"),
        "Character should understand 'isLetter'"
    );
    assert!(
        !h.resolves_selector("Character", "bogusMethod"),
        "Character should NOT understand 'bogusMethod'"
    );
}
#[test]
fn character_is_numeric_type() {
    // Character inherits from Integer which inherits from Number,
    // so it should be treated as numeric for operand-type checks.
    let h = ClassHierarchy::with_builtins();
    assert!(h.is_numeric_type("Integer"), "Integer is numeric");
    assert!(h.is_numeric_type("Float"), "Float is numeric");
    assert!(h.is_numeric_type("Number"), "Number is numeric");
    assert!(
        h.is_numeric_type("Character"),
        "Character is numeric (inherits Integer)"
    );
    assert!(!h.is_numeric_type("String"), "String is not numeric");
    assert!(!h.is_numeric_type("Boolean"), "Boolean is not numeric");
}
// --- Sealed method override enforcement tests ---
fn make_class_with_sealed_method(
    name: &str,
    superclass: &str,
    method_name: &str,
    sealed: bool,
) -> ClassDefinition {
    ClassDefinition {
        name: Identifier::new(name, test_span()),
        superclass: Some(Identifier::new(superclass, test_span())),
        superclass_package: None,
        class_kind: ClassKind::from_superclass_name(superclass),
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary(method_name.into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: sealed,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    }
}
fn make_class_with_sealed_class_method(
    name: &str,
    superclass: &str,
    method_name: &str,
    sealed: bool,
) -> ClassDefinition {
    ClassDefinition {
        name: Identifier::new(name, test_span()),
        superclass: Some(Identifier::new(superclass, test_span())),
        superclass_package: None,
        class_kind: ClassKind::from_superclass_name(superclass),
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary(method_name.into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: sealed,
            is_internal: false,
            is_class_method: true,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    }
}
#[test]
fn sealed_method_override_rejected() {
    // Parent defines sealed method "doCustomWork", child tries to override it
    let parent = make_class_with_sealed_method("Parent", "Actor", "doCustomWork", true);
    let child = make_class_with_sealed_method("Child", "Parent", "doCustomWork", false);
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed method"));
    assert!(diags[0].message.contains("`doCustomWork`"));
    assert!(diags[0].message.contains("`Parent`"));
    // Child class is still added despite the error
    assert!(h.has_class("Child"));
}
#[test]
fn non_sealed_method_override_allowed() {
    // Parent defines non-sealed method, child overrides it — no diagnostic
    let parent = make_class_with_sealed_method("Parent", "Actor", "doWork", false);
    let child = make_class_with_sealed_method("Child", "Parent", "doWork", false);
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build(&module);
    assert!(diags.is_empty());
}
#[test]
fn sealed_method_in_grandparent_enforced() {
    // Grandparent defines sealed method, grandchild tries to override it
    let grandparent = make_class_with_sealed_method("GrandParent", "Actor", "locked", true);
    let parent = make_class_with_sealed_method("Parent", "GrandParent", "doWork", false);
    let grandchild = make_class_with_sealed_method("GrandChild", "Parent", "locked", false);
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![grandparent, parent, grandchild],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed method"));
    assert!(diags[0].message.contains("`locked`"));
    assert!(diags[0].message.contains("`GrandParent`"));
    assert!(h.has_class("GrandChild"));
}
#[test]
fn builtin_sealed_method_override_rejected() {
    // Object's respondsTo: is sealed in builtins — user class cannot override it
    let child = make_class_with_sealed_method("MyObj", "Object", "respondsTo:", false);
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed method"));
    assert!(diags[0].message.contains("`respondsTo:`"));
    assert!(diags[0].message.contains("`Object`"));
    assert!(h.has_class("MyObj"));
}
#[test]
fn builtin_actor_spawn_override_rejected() {
    // Actor's class-side spawn is sealed in builtins — subclass cannot override it
    let child = make_class_with_sealed_class_method("MyActor", "Actor", "spawn", false);
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed method"));
    assert!(diags[0].message.contains("`spawn`"));
    assert!(diags[0].message.contains("`Actor`"));
    assert!(h.has_class("MyActor"));
}
// --- MRO verification ---
#[test]
fn mro_is_depth_first() {
    // Counter -> Actor -> Object -> ProtoObject
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let h = ClassHierarchy::build(&module).0.unwrap();
    let methods = h.all_methods("Counter");
    // First method should be from Counter (most specific)
    assert_eq!(methods[0].defined_in.as_str(), "Counter");
    // Actor methods should come before Object methods
    let actor_idx = methods
        .iter()
        .position(|m| m.defined_in == "Actor")
        .unwrap();
    let object_idx = methods
        .iter()
        .position(|m| m.defined_in == "Object")
        .unwrap();
    assert!(actor_idx < object_idx);
}
// --- Edge case tests ---
#[test]
fn cycle_detection_in_superclass_chain() {
    // Manually create a cycle: A -> B -> A
    let mut h = ClassHierarchy::with_builtins();
    h.classes.insert(
        "A".into(),
        ClassInfo {
            surface_incomplete: false,
            in_caller_new1: false,
            is_module_less: false,
            name: "A".into(),
            superclass: Some("B".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            auto_ctors: None,
            handle_scope: None,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            state_kinds: HashMap::new(),
            initialize_assigns: std::collections::BTreeSet::new(),
            has_dynamic_field_writer: false,
            methods: vec![builtin_method("methodA", 0, "A")],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
    );
    h.classes.insert(
        "B".into(),
        ClassInfo {
            surface_incomplete: false,
            in_caller_new1: false,
            is_module_less: false,
            name: "B".into(),
            superclass: Some("A".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            auto_ctors: None,
            handle_scope: None,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            state_kinds: HashMap::new(),
            initialize_assigns: std::collections::BTreeSet::new(),
            has_dynamic_field_writer: false,
            methods: vec![builtin_method("methodB", 0, "B")],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
    );
    // Should not loop forever
    let chain = h.superclass_chain("A");
    assert!(chain.len() <= 2);
    let methods = h.all_methods("A");
    assert!(!methods.is_empty());
    // Should not hang
    let _ = h.resolves_selector("A", "methodB");
}
#[test]
#[allow(clippy::too_many_lines)] // long AST fixture
fn multiple_user_classes_in_module() {
    let base = ClassDefinition {
        name: Identifier::new("Base", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary("baseMethod".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let derived = ClassDefinition {
        name: Identifier::new("Derived", test_span()),
        superclass: Some(Identifier::new("Base", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary("derivedMethod".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![base, derived],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert!(diags.is_empty());
    // Derived should inherit from Base -> Actor -> Object -> ProtoObject
    assert!(h.resolves_selector("Derived", "derivedMethod"));
    assert!(h.resolves_selector("Derived", "baseMethod"));
    // spawn is class-side on Actor — verify via all_class_methods
    assert!(
        h.all_class_methods("Derived")
            .iter()
            .any(|m| m.selector.as_str() == "spawn"),
        "Derived should inherit class-side spawn from Actor"
    );
    assert!(h.resolves_selector("Derived", "class"));
}
#[test]
fn user_class_with_unknown_superclass() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Orphan", "NonExistent")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    // No diagnostic for unknown superclass (sealed check passes)
    assert!(diags.is_empty());
    // Class is still added
    assert!(h.has_class("Orphan"));
    // Local method resolves
    assert!(h.resolves_selector("Orphan", "increment"));
    // Chain stops at unknown superclass
    let chain = h.superclass_chain("Orphan");
    assert_eq!(chain, vec![EcoString::from("NonExistent")]);
}
// --- Duplicate method detection tests ---
#[test]
fn duplicate_instance_method_detected() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![
            MethodDefinition {
                pipeline_sends: crate::ast::PipelineSends::default(),
                literal_comments: crate::ast::LiteralComments::default(),
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(10, 20),
            },
            MethodDefinition {
                pipeline_sends: crate::ast::PipelineSends::default(),
                literal_comments: crate::ast::LiteralComments::default(),
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(30, 40),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build(&module);
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("Duplicate method"));
    assert!(diags[0].message.contains("`increment`"));
    assert!(diags[0].message.contains("`Counter`"));
    assert!(diags[0].hint.is_some());
    assert!(diags[0].hint.as_ref().unwrap().contains("already defined"));
}
#[test]
fn duplicate_class_method_detected() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![
            MethodDefinition {
                pipeline_sends: crate::ast::PipelineSends::default(),
                literal_comments: crate::ast::LiteralComments::default(),
                selector: crate::ast::MessageSelector::Unary("create".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: true,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(10, 20),
            },
            MethodDefinition {
                pipeline_sends: crate::ast::PipelineSends::default(),
                literal_comments: crate::ast::LiteralComments::default(),
                selector: crate::ast::MessageSelector::Unary("create".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: true,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(30, 40),
            },
        ],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build(&module);
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("Duplicate class method"));
    assert!(diags[0].message.contains("`create`"));
}
#[test]
fn no_duplicate_for_different_selectors() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![
            MethodDefinition {
                pipeline_sends: crate::ast::PipelineSends::default(),
                literal_comments: crate::ast::LiteralComments::default(),
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            },
            MethodDefinition {
                pipeline_sends: crate::ast::PipelineSends::default(),
                literal_comments: crate::ast::LiteralComments::default(),
                selector: crate::ast::MessageSelector::Unary("decrement".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build(&module);
    assert!(diags.is_empty());
}
// --- Typed class inheritance tests ---
#[test]
fn typed_class_is_typed() {
    let mut class = make_class_with_sealed_method("StrictCounter", "Actor", "increment", false);
    class.is_typed = true;
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    assert!(
        hierarchy.is_typed("StrictCounter"),
        "explicitly typed class should be typed"
    );
}
#[test]
fn typed_class_inheritance() {
    // Parent is typed, child inherits typed
    let mut parent = make_class_with_sealed_method("TypedParent", "Actor", "method", false);
    parent.is_typed = true;
    let child = make_class_with_sealed_method("UntypedChild", "TypedParent", "method", false);
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    assert!(hierarchy.is_typed("TypedParent"), "parent should be typed");
    assert!(
        hierarchy.is_typed("UntypedChild"),
        "child of typed class should inherit typed"
    );
}
#[test]
fn typed_class_inheritance_reverse_order() {
    // Child defined BEFORE typed parent — should still inherit typed
    let mut parent = make_class_with_sealed_method("TypedParent", "Actor", "method", false);
    parent.is_typed = true;
    let child = make_class_with_sealed_method("UntypedChild", "TypedParent", "method", false);
    // Note: child comes first in the vec! (reverse definition order)
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![child, parent],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    assert!(hierarchy.is_typed("TypedParent"), "parent should be typed");
    assert!(
        hierarchy.is_typed("UntypedChild"),
        "child of typed class should inherit typed even when defined before parent"
    );
}
#[test]
fn non_typed_class_not_inherited() {
    // Parent is NOT typed, child should not be typed
    let parent = make_class_with_sealed_method("Parent", "Actor", "method", false);
    let child = make_class_with_sealed_method("Child", "Parent", "method", false);
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    assert!(!hierarchy.is_typed("Parent"));
    assert!(!hierarchy.is_typed("Child"));
}
// --- Type annotation propagation tests ---
#[test]
fn stdlib_integer_plus_has_return_type() {
    let h = ClassHierarchy::with_builtins();
    let method = h
        .find_method("Integer", "+")
        .expect("Integer >> + should exist");
    assert_eq!(method.return_type, Some(DeclaredType::simple("Integer")));
}
#[test]
fn stdlib_integer_plus_has_param_types() {
    let h = ClassHierarchy::with_builtins();
    let method = h
        .find_method("Integer", "+")
        .expect("Integer >> + should exist");
    assert_eq!(
        method.param_types,
        vec![Some(DeclaredType::simple("Number"))]
    );
}
#[test]
fn stdlib_unary_method_has_empty_param_types() {
    let h = ClassHierarchy::with_builtins();
    let method = h
        .find_method("Integer", "asFloat")
        .expect("Integer >> asFloat should exist");
    assert!(method.param_types.is_empty());
}
#[test]
fn user_class_return_type_propagated() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Object", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        state: vec![],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![],
            return_type: Some(TypeAnnotation::simple("Integer", test_span())),
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        is_sealed: false,
        is_internal: false,
        is_abstract: false,
        is_typed: false,
        supervisor_kind: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let method = hierarchy
        .find_method("Counter", "getValue")
        .expect("Counter >> getValue should exist");
    assert_eq!(method.return_type, Some(DeclaredType::simple("Integer")));
}
#[test]
fn user_class_param_types_propagated() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Object", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        state: vec![],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                keyword: "add:".into(),
                span: test_span(),
            }]),
            parameters: vec![ParameterDefinition::with_type(
                Identifier::new("amount", test_span()),
                TypeAnnotation::simple("Integer", test_span()),
            )],
            body: vec![],
            return_type: Some(TypeAnnotation::simple("Counter", test_span())),
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        is_sealed: false,
        is_internal: false,
        is_abstract: false,
        is_typed: false,
        supervisor_kind: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let method = hierarchy
        .find_method("Counter", "add:")
        .expect("Counter >> add: should exist");
    assert_eq!(method.return_type, Some(DeclaredType::simple("Counter")));
    assert_eq!(
        method.param_types,
        vec![Some(DeclaredType::simple("Integer"))]
    );
}
// --- State field type tests ---
fn make_typed_state_class(name: &str, superclass: &str) -> ClassDefinition {
    ClassDefinition {
        name: Identifier::new(name, test_span()),
        superclass: Some(Identifier::new(superclass, test_span())),
        superclass_package: None,
        class_kind: ClassKind::from_superclass_name(superclass),
        is_abstract: false,
        is_sealed: false,
        is_typed: true,
        is_internal: false,
        supervisor_kind: None,
        state: vec![
            StateDeclaration::with_type(
                Identifier::new("count", test_span()),
                TypeAnnotation::simple("Integer", test_span()),
                test_span(),
            ),
            StateDeclaration::new(Identifier::new("label", test_span()), test_span()),
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    }
}
/// BT-4679: the former `state_field_type_returns_type_for_annotated_field`,
/// `..._returns_none_for_untyped_field`, `..._returns_none_for_unknown_field` and
/// `..._returns_none_for_unknown_class` tests, all queried against one hierarchy built
/// from a typed `Counter` actor. (The unknown-class case used a bare builtins hierarchy;
/// `build` includes the builtins, so `DoesNotExist` is unknown in both.)
#[test]
fn state_field_type_table() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_typed_state_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert!(diags.is_empty());
    let cases = [
        (
            "annotated field",
            "Counter",
            "count",
            Some(DeclaredType::simple("Integer")),
        ),
        ("untyped field", "Counter", "label", None),
        ("unknown field", "Counter", "nonexistent", None),
        ("unknown class", "DoesNotExist", "count", None),
    ];
    for (label, class, field, expected) in cases {
        assert_eq!(
            h.state_field_type(class, field),
            expected,
            "[{label}] state_field_type({class}, {field})"
        );
    }
}

#[test]
fn state_field_type_inherited_from_parent() {
    let parent = make_typed_state_class("TypedParent", "Actor");
    let child = ClassDefinition {
        name: Identifier::new("Child", test_span()),
        superclass: Some(Identifier::new("TypedParent", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration::with_type(
            Identifier::new("extra", test_span()),
            TypeAnnotation::simple("String", test_span()),
            test_span(),
        )],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert!(diags.is_empty());
    // Child's own typed field
    assert_eq!(
        h.state_field_type("Child", "extra"),
        Some(DeclaredType::simple("String"))
    );
    // Inherited typed field from parent
    assert_eq!(
        h.state_field_type("Child", "count"),
        Some(DeclaredType::simple("Integer"))
    );
    // Inherited untyped field from parent
    assert_eq!(h.state_field_type("Child", "label"), None);
}
#[test]
fn state_field_type_builtin_classes_return_none() {
    let h = ClassHierarchy::with_builtins();
    // Built-in classes have no typed state currently
    assert_eq!(h.state_field_type("Integer", "anything"), None);
    assert_eq!(h.state_field_type("Actor", "anything"), None);
}
#[test]
fn state_field_type_shadowed_untyped_field() {
    // Parent declares `count: Integer`, child redeclares `count` without type.
    // The child's untyped declaration should shadow the parent's type.
    let parent = make_typed_state_class("TypedParent", "Actor");
    let child = ClassDefinition {
        name: Identifier::new("Child", test_span()),
        superclass: Some(Identifier::new("TypedParent", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration::new(
            Identifier::new("count", test_span()),
            test_span(),
        )],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let h = ClassHierarchy::build(&module).0.unwrap();
    // Child's untyped `count` shadows parent's typed `count: Integer`
    assert_eq!(h.state_field_type("Child", "count"), None);
    // Parent's typed `count` is still accessible on the parent
    assert_eq!(
        h.state_field_type("TypedParent", "count"),
        Some(DeclaredType::simple("Integer"))
    );
}
#[test]
fn has_instance_dnu_override_detects_erlang() {
    // Erlang has doesNotUnderstand:args: as an instance method
    // (the singleton tagged map dispatches through instance dispatch).
    let h = ClassHierarchy::with_builtins();
    assert!(
        h.has_instance_dnu_override("Erlang"),
        "Erlang instance-side DNU override should be detected"
    );
    // Erlang has no class-side DNU
    assert!(
        !h.has_class_dnu_override("Erlang"),
        "Erlang should not have class-side DNU override"
    );
}
#[test]
fn has_instance_dnu_override_detects_erlang_module() {
    // ErlangModule has doesNotUnderstand:args: as an instance method.
    let h = ClassHierarchy::with_builtins();
    assert!(
        h.has_instance_dnu_override("ErlangModule"),
        "ErlangModule instance-side DNU override should be detected"
    );
    // ErlangModule has no class-side DNU
    assert!(
        !h.has_class_dnu_override("ErlangModule"),
        "ErlangModule should not have class-side DNU override"
    );
}
#[test]
fn has_dnu_override_false_for_normal_class() {
    let h = ClassHierarchy::with_builtins();
    assert!(
        !h.has_instance_dnu_override("Integer"),
        "Integer should not have instance DNU override"
    );
    assert!(
        !h.has_class_dnu_override("Integer"),
        "Integer should not have class DNU override"
    );
}
// --- Cross-file superclass enrichment tests ---
#[test]
fn add_external_superclasses_resolves_value_object_chain() {
    let mut h = ClassHierarchy::with_builtins();
    // Simulate: File A defines "MyParent" as Object subclass
    // File B defines "MyChild" as MyParent subclass
    // When compiling File B, only MyChild is in the hierarchy.
    // The external superclass index provides the missing link.
    let mut index = HashMap::new();
    index.insert("MyParent".to_string(), "Object".to_string());
    h.add_external_superclasses(&index);
    // MyParent should now resolve through to Object
    let chain = h.superclass_chain("MyParent");
    assert_eq!(
        chain,
        vec![EcoString::from("Object"), EcoString::from("ProtoObject")]
    );
    // A child class added to the hierarchy should now resolve through MyParent → Object
    assert!(!h.is_actor_subclass("MyParent"));
}
#[test]
fn add_external_superclasses_resolves_actor_chain() {
    let mut h = ClassHierarchy::with_builtins();
    // Simulate: File A defines "MyActor" as Actor subclass
    // File B defines "MySpecialActor" as MyActor subclass
    let mut index = HashMap::new();
    index.insert("MyActor".to_string(), "Actor".to_string());
    h.add_external_superclasses(&index);
    // MyActor should resolve through to Actor
    assert!(h.is_actor_subclass("MyActor"));
}
// --- is_value_subclass tests (ADR 0042) ---
#[test]
fn value_itself_is_value_subclass() {
    let h = ClassHierarchy::with_builtins();
    assert!(h.is_value_subclass("Value"));
}
#[test]
fn object_is_not_value_subclass() {
    let h = ClassHierarchy::with_builtins();
    assert!(!h.is_value_subclass("Object"));
}
#[test]
fn actor_is_not_value_subclass() {
    let h = ClassHierarchy::with_builtins();
    assert!(!h.is_value_subclass("Actor"));
}
#[test]
fn user_value_subclass_detected() {
    let mut h = ClassHierarchy::with_builtins();
    let mut index = HashMap::new();
    index.insert("Point".to_string(), "Value".to_string());
    h.add_external_superclasses(&index);
    assert!(h.is_value_subclass("Point"));
}
#[test]
fn user_object_subclass_is_not_value_subclass() {
    let mut h = ClassHierarchy::with_builtins();
    let mut index = HashMap::new();
    index.insert("Plain".to_string(), "Object".to_string());
    h.add_external_superclasses(&index);
    assert!(!h.is_value_subclass("Plain"));
}
#[test]
fn add_external_superclasses_does_not_overwrite_existing() {
    let mut h = ClassHierarchy::with_builtins();
    // Object is already in the hierarchy — external index should not overwrite it
    let mut index = HashMap::new();
    index.insert("Object".to_string(), "SomethingElse".to_string());
    h.add_external_superclasses(&index);
    // Object should still resolve to ProtoObject (built-in), not SomethingElse
    let chain = h.superclass_chain("Object");
    assert_eq!(chain, vec![EcoString::from("ProtoObject")]);
}
// --- ClassKind / is_value integration tests ---
#[test]
fn value_subclass_sets_is_value_flag() {
    let class = ClassDefinition {
        name: Identifier::new("Point", test_span()),
        superclass: Some(Identifier::new("Value", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module::new(vec![], test_span());
    let mut module_with_class = module;
    module_with_class.classes.push(class);
    let (Ok(h), _) = ClassHierarchy::build(&module_with_class) else {
        panic!("build should succeed");
    };
    let info = h.get_class("Point").expect("Point should be registered");
    assert!(info.is_value, "Value subclass should have is_value = true");
    assert!(h.is_value_subclass("Point"));
}
#[test]
fn value_subclass_auto_generates_slot_methods() {
    let class = ClassDefinition {
        name: Identifier::new("Point", test_span()),
        superclass: Some(Identifier::new("Value", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![
            StateDeclaration::new(Identifier::new("x", test_span()), test_span()),
            StateDeclaration::new(Identifier::new("y", test_span()), test_span()),
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    let info = h.get_class("Point").expect("Point should be registered");
    // Auto-generated instance methods: getters (x, y) + setters (withX:, withY:)
    let instance_sels: Vec<&str> = info.methods.iter().map(|m| m.selector.as_str()).collect();
    assert!(
        instance_sels.contains(&"x"),
        "getter x missing: {instance_sels:?}"
    );
    assert!(
        instance_sels.contains(&"y"),
        "getter y missing: {instance_sels:?}"
    );
    assert!(
        instance_sels.contains(&"withX:"),
        "setter withX: missing: {instance_sels:?}"
    );
    assert!(
        instance_sels.contains(&"withY:"),
        "setter withY: missing: {instance_sels:?}"
    );
    // Auto-generated class method: keyword constructor x:y:
    let class_sels: Vec<&str> = info
        .class_methods
        .iter()
        .map(|m| m.selector.as_str())
        .collect();
    assert!(
        class_sels.contains(&"x:y:"),
        "keyword constructor x:y: missing: {class_sels:?}"
    );
}
#[test]
fn value_subclass_auto_methods_have_generated_docs() {
    use crate::ast::{Expression, Literal};
    let state = vec![StateDeclaration::with_default(
        Identifier::new("x", test_span()),
        Expression::Literal(Literal::Integer(0), test_span()),
        test_span(),
    )];
    let class = ClassDefinition {
        name: Identifier::new("Point", test_span()),
        superclass: Some(Identifier::new("Value", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    let info = h.get_class("Point").expect("Point should be registered");
    // Getter doc contains field name, default, and compiler-generated tag
    let getter = info.methods.iter().find(|m| m.selector == "x").unwrap();
    let getter_doc = getter.doc.as_deref().unwrap();
    assert!(getter_doc.contains("`x`"), "getter doc missing field name");
    assert!(
        getter_doc.contains("`0`"),
        "getter doc missing default value"
    );
    assert!(
        getter_doc.contains("*(compiler-generated)*"),
        "getter doc missing tag"
    );
    // Setter doc mentions class name and field name
    let setter = info
        .methods
        .iter()
        .find(|m| m.selector == "withX:")
        .unwrap();
    let setter_doc = setter.doc.as_deref().unwrap();
    assert!(
        setter_doc.contains("`Point`"),
        "setter doc missing class name"
    );
    assert!(setter_doc.contains("`x`"), "setter doc missing field name");
    assert!(
        setter_doc.contains("*(compiler-generated)*"),
        "setter doc missing tag"
    );
    // Keyword constructor doc mentions class name and field arg with default
    let ctor = info
        .class_methods
        .iter()
        .find(|m| m.selector == "x:")
        .unwrap();
    let ctor_doc = ctor.doc.as_deref().unwrap();
    assert!(ctor_doc.contains("`Point`"), "ctor doc missing class name");
    assert!(
        ctor_doc.contains("x (default: 0)"),
        "ctor doc missing arg desc"
    );
    assert!(
        ctor_doc.contains("*(compiler-generated)*"),
        "ctor doc missing tag"
    );
}
#[test]
fn value_subclass_auto_methods_respect_user_overrides() {
    let class = ClassDefinition {
        name: Identifier::new("Point", test_span()),
        superclass: Some(Identifier::new("Value", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration::new(
            Identifier::new("x", test_span()),
            test_span(),
        )],
        // User already defines getter `x`
        methods: vec![MethodDefinition::new(
            crate::ast::MessageSelector::Unary(EcoString::from("x")),
            vec![],
            vec![],
            test_span(),
        )],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    let info = h.get_class("Point").expect("Point should be registered");
    // User-defined `x` should appear once, not duplicated by auto-generation
    let x_count = info.methods.iter().filter(|m| m.selector == "x").count();
    assert_eq!(x_count, 1, "getter `x` should appear exactly once");
    // Auto-generated withX: should still be present
    assert!(
        info.methods.iter().any(|m| m.selector == "withX:"),
        "withX: should be auto-generated"
    );
}
#[test]
fn actor_subclass_does_not_set_is_value_flag() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    let info = h
        .get_class("Counter")
        .expect("Counter should be registered");
    assert!(!info.is_value, "Actor subclass should not have is_value");
}
/// Test sealed method override detection with external superclasses.
/// When a class inherits from an external (unknown) class, ancestor lookup
/// should not break early; it should continue walking up the chain
/// using the `external_superclasses` data.
#[test]
#[allow(clippy::too_many_lines)] // several hand-built ClassDefinition literals
fn sealed_override_checks_with_external_superclasses() {
    // ClassB (in module) has sealed method foo
    let sealed_base = ClassDefinition {
        name: Identifier::new("ClassB", test_span()),
        superclass: Some(Identifier::new("Object", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary("foo".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: true,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    // ClassC (in module) inherits from UnknownClass (external)
    // UnknownClass will later be declared to inherit from ClassB
    let override_class = ClassDefinition {
        name: Identifier::new("ClassC", test_span()),
        superclass: Some(Identifier::new("UnknownClass", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector: crate::ast::MessageSelector::Unary("foo".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: test_span(),
    };
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![sealed_base, override_class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    // Register external class relationship: UnknownClass <- ClassB
    let mut external_index = std::collections::HashMap::new();
    external_index.insert("UnknownClass".to_string(), "ClassB".to_string());
    h.add_external_superclasses(&external_index);
    // After adding external superclass info, UnknownClass should be in the hierarchy
    // and we should be able to walk from it to ClassB.
    assert!(
        h.get_class("UnknownClass").is_some(),
        "UnknownClass should exist after add_external_superclasses"
    );
    assert_eq!(
        h.superclass_chain("UnknownClass"),
        vec![
            EcoString::from("ClassB"),
            EcoString::from("Object"),
            EcoString::from("ProtoObject")
        ],
        "UnknownClass superclass chain should reach ClassB"
    );
}
// --- ADR 0050 Phase 4: add_from_beam_meta tests ---
#[test]
fn add_from_beam_meta_inserts_non_builtin_class() {
    let mut h = ClassHierarchy::with_builtins();
    let info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("Counter"),
        superclass: Some(EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![EcoString::from("count")],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![MethodInfo {
            selector: EcoString::from("value"),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: EcoString::from("Counter"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(DeclaredType::simple("Integer")),
            param_types: vec![],
            doc: None,

            origin: None,
            primitive: None,
            constant_value: None,
            answers_receiver: false,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![info]);
    assert!(h.has_class("Counter"));
    let cls = h.get_class("Counter").unwrap();
    assert_eq!(cls.superclass.as_deref(), Some("Actor"));
    assert_eq!(cls.methods.len(), 1);
    assert_eq!(cls.methods[0].selector.as_str(), "value");
}
#[test]
fn add_from_beam_meta_preserves_existing_entries() {
    let mut h = ClassHierarchy::with_builtins();
    // Simulate AST-derived entry (from ClassHierarchy::build)
    let ast_info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("Counter"),
        superclass: Some(EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![EcoString::from("count")],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![MethodInfo {
            selector: EcoString::from("increment"),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: EcoString::from("Counter"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![],
            doc: None,

            origin: None,
            primitive: None,
            constant_value: None,
            answers_receiver: false,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.classes.insert(EcoString::from("Counter"), ast_info);
    // Stale cache entry with different method
    let cache_info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("Counter"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![MethodInfo {
            selector: EcoString::from("old_method"),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: EcoString::from("Counter"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![],
            doc: None,

            origin: None,
            primitive: None,
            constant_value: None,
            answers_receiver: false,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![cache_info]);
    // AST-derived entry should win
    let cls = h.get_class("Counter").unwrap();
    assert_eq!(cls.superclass.as_deref(), Some("Actor"));
    assert_eq!(cls.methods[0].selector.as_str(), "increment");
}
#[test]
fn add_from_beam_meta_skips_builtins() {
    let mut h = ClassHierarchy::with_builtins();
    let original_method_count = h.get_class("Integer").unwrap().methods.len();
    let stub = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("Integer"),
        superclass: None,
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![stub]);
    // Built-in should be unchanged
    assert_eq!(
        h.get_class("Integer").unwrap().methods.len(),
        original_method_count
    );
}
// --- ClassInfo package + is_internal infrastructure ---
#[test]
fn classinfo_from_ast_populates_is_internal() {
    let source = "internal Actor subclass: InternalCounter\n  state: count = 0\n  increment => self.count := self.count + 1\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let infos = ClassHierarchy::extract_class_infos(&module);
    assert_eq!(infos.len(), 1);
    assert!(
        infos[0].is_internal,
        "is_internal should be true for internal classes"
    );
    assert!(
        infos[0].package.is_none(),
        "package should be None when built from AST (stamped later)"
    );
}
#[test]
fn classinfo_from_ast_defaults_is_internal_false() {
    let source = "Actor subclass: PublicCounter\n  state: count = 0\n  increment => self.count := self.count + 1\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let infos = ClassHierarchy::extract_class_infos(&module);
    assert_eq!(infos.len(), 1);
    assert!(
        !infos[0].is_internal,
        "is_internal should be false for non-internal classes"
    );
}
#[test]
fn stamp_package_sets_package_on_non_builtin_classes() {
    let source = "Actor subclass: Counter\n  state: count = 0\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    h.stamp_package("myapp");
    let counter = h.get_class("Counter").unwrap();
    assert_eq!(
        counter.package.as_deref(),
        Some("myapp"),
        "stamp_package should set package on AST-derived classes"
    );
    // Builtins should not be stamped
    let integer = h.get_class("Integer").unwrap();
    assert!(
        integer.package.is_some(),
        "stdlib builtins already have package=stdlib from generated_builtins"
    );
}
#[test]
fn stamp_package_does_not_overwrite_existing_package() {
    let mut h = ClassHierarchy::with_builtins();
    let info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("RemoteClass"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: Some(EcoString::from("other_pkg")),
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![info]);
    h.stamp_package("myapp");
    let cls = h.get_class("RemoteClass").unwrap();
    assert_eq!(
        cls.package.as_deref(),
        Some("other_pkg"),
        "stamp_package should not overwrite existing package"
    );
}
#[test]
fn stamp_package_on_infos_sets_package_on_non_builtin_classes() {
    // `stamp_package_on_infos` is `stamp_package`'s counterpart for
    // Pass 1's cross-file `ClassInfo` extraction (build's class index,
    // lint's package walk) — a plain `Vec<ClassInfo>` collected before any
    // `ClassHierarchy` exists, so `stamp_package` itself can't reach it.
    let source = "Actor subclass: TokenBuffer\n  data => nil\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let mut infos = ClassHierarchy::extract_class_infos(&module);
    assert_eq!(
        infos[0].package, None,
        "freshly extracted infos start unpackaged"
    );

    ClassHierarchy::stamp_package_on_infos(&mut infos, "json");

    assert_eq!(
        infos[0].package.as_deref(),
        Some("json"),
        "stamp_package_on_infos should set package on AST-derived infos"
    );
}
#[test]
fn stamp_package_on_infos_does_not_overwrite_existing_package() {
    let mut infos = vec![ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("RemoteClass"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: Some(EcoString::from("other_pkg")),
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }];

    ClassHierarchy::stamp_package_on_infos(&mut infos, "myapp");

    assert_eq!(
        infos[0].package.as_deref(),
        Some("other_pkg"),
        "stamp_package_on_infos should not overwrite an already-set package"
    );
}
#[test]
fn add_from_beam_meta_preserves_is_internal_and_package() {
    let mut h = ClassHierarchy::with_builtins();
    let info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("InternalHelper"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: true,
        package: Some(EcoString::from("mylib")),
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![info]);
    let cls = h.get_class("InternalHelper").unwrap();
    assert!(
        cls.is_internal,
        "is_internal should be preserved from BEAM metadata"
    );
    assert_eq!(
        cls.package.as_deref(),
        Some("mylib"),
        "package should be preserved from BEAM metadata"
    );
}
// --- extract_class_infos tests ---
#[test]
fn extract_class_infos_from_empty_module() {
    let module = Module::new(vec![], crate::source_analysis::Span::default());
    let infos = ClassHierarchy::extract_class_infos(&module);
    assert!(infos.is_empty());
}
#[test]
fn extract_class_infos_captures_methods_and_state() {
    let source = "Actor subclass: Counter\n  state: count :: Integer = 0\n  increment => self.count := self.count + 1\n  getValue => self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let infos = ClassHierarchy::extract_class_infos(&module);
    assert_eq!(infos.len(), 1);
    let info = &infos[0];
    assert_eq!(info.name.as_str(), "Counter");
    assert_eq!(info.superclass.as_deref(), Some("Actor"));
    assert!(info.state.iter().any(|s| s == "count"));
    assert!(info.methods.iter().any(|m| m.selector == "increment"));
    assert!(info.methods.iter().any(|m| m.selector == "getValue"));
}
// --- Extension method registration tests ---
#[test]
fn register_extensions_adds_instance_method() {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;
    let mut h = ClassHierarchy::with_builtins();
    let original_count = h.all_methods("String").len();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "String".into(),
        side: MethodSide::Instance,
        selector: "shout".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("String+Shout.bt"),
            span: test_span(),
            type_info: ExtensionTypeInfo {
                arity: 0,
                param_types: vec![],
                return_type: Some("String".into()),
            },
        }],
    );
    h.register_extensions(&index);
    let methods = h.all_methods("String");
    assert_eq!(methods.len(), original_count + 1);
    let ext = methods.iter().find(|m| m.selector == "shout").unwrap();
    assert_eq!(ext.arity, 0);
    assert_eq!(ext.return_type, Some(DeclaredType::simple("String")));
    assert_eq!(ext.defined_in.as_str(), "String");
}
#[test]
fn register_extensions_adds_class_method() {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;
    let mut h = ClassHierarchy::with_builtins();
    let original_count = h.all_class_methods("String").len();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "String".into(),
        side: MethodSide::Class,
        selector: "fromJson:".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("String+JSON.bt"),
            span: test_span(),
            type_info: ExtensionTypeInfo {
                arity: 1,
                param_types: vec![Some("String".into())],
                return_type: Some("String".into()),
            },
        }],
    );
    h.register_extensions(&index);
    let methods = h.all_class_methods("String");
    assert_eq!(methods.len(), original_count + 1);
    let ext = methods.iter().find(|m| m.selector == "fromJson:").unwrap();
    assert_eq!(ext.arity, 1);
    assert_eq!(ext.param_types, vec![Some(DeclaredType::simple("String"))]);
}
#[test]
fn register_extensions_unannotated_uses_dynamic() {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;
    let mut h = ClassHierarchy::with_builtins();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "Array".into(),
        side: MethodSide::Instance,
        selector: "shuffle".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("Array+Shuffle.bt"),
            span: test_span(),
            type_info: ExtensionTypeInfo {
                arity: 0,
                param_types: vec![],
                return_type: None,
            },
        }],
    );
    h.register_extensions(&index);
    let ext = h
        .find_method("Array", "shuffle")
        .expect("extension should be resolvable");
    assert!(ext.return_type.is_none(), "unannotated = Dynamic");
}
#[test]
fn register_extensions_skips_unknown_class() {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;
    let mut h = ClassHierarchy::with_builtins();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "NonexistentClass".into(),
        side: MethodSide::Instance,
        selector: "doStuff".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("Nonexistent+Stuff.bt"),
            span: test_span(),
            type_info: ExtensionTypeInfo {
                arity: 0,
                param_types: vec![],
                return_type: None,
            },
        }],
    );
    // Should not panic
    h.register_extensions(&index);
    assert!(!h.has_class("NonexistentClass"));
}
#[test]
fn register_extensions_does_not_override_existing_method() {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;
    let mut h = ClassHierarchy::with_builtins();
    // "size" already exists on String
    let original = h.find_method("String", "size").unwrap().clone();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "String".into(),
        side: MethodSide::Instance,
        selector: "size".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("String+Size.bt"),
            span: test_span(),
            type_info: ExtensionTypeInfo {
                arity: 0,
                param_types: vec![],
                return_type: Some("Integer".into()),
            },
        }],
    );
    h.register_extensions(&index);
    // Original method should be unchanged
    let after = h.find_method("String", "size").unwrap();
    assert_eq!(after.defined_in, original.defined_in);
}
#[test]
fn register_extensions_resolves_selector() {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;
    let mut h = ClassHierarchy::with_builtins();
    assert!(
        !h.resolves_selector("String", "toJson"),
        "precondition: toJson doesn't exist"
    );
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "String".into(),
        side: MethodSide::Instance,
        selector: "toJson".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("String+JSON.bt"),
            span: test_span(),
            type_info: ExtensionTypeInfo {
                arity: 0,
                param_types: vec![],
                return_type: Some("String".into()),
            },
        }],
    );
    h.register_extensions(&index);
    assert!(
        h.resolves_selector("String", "toJson"),
        "extension should be resolvable via resolves_selector"
    );
}
// --- ClassKind hierarchy propagation tests ---
/// BT-4679: the former five `resolve_class_kind_*` tests. Each row seeds the external
/// superclass index (when non-empty), then resolves a class's `ClassKind`.
#[test]
fn resolve_class_kind_table() {
    // (label, external superclass index, class to resolve, expected kind, also value subclass)
    let cases: [(&str, &[(&str, &str)], &str, ClassKind, bool); 5] = [
        (
            "direct Value subclass",
            &[("Point", "Value")],
            "Point",
            ClassKind::Value,
            false,
        ),
        (
            // MyValueBase inherits from Value, MyValueChild inherits from MyValueBase
            "indirect Value subclass",
            &[("MyValueBase", "Value"), ("MyValueChild", "MyValueBase")],
            "MyValueChild",
            ClassKind::Value,
            true,
        ),
        (
            "direct Actor subclass",
            &[("Counter", "Actor")],
            "Counter",
            ClassKind::Actor,
            false,
        ),
        (
            "indirect Actor subclass",
            &[("MyActor", "Actor"), ("SpecialActor", "MyActor")],
            "SpecialActor",
            ClassKind::Actor,
            false,
        ),
        (
            // Supervisor inherits from Object (not Actor, not Value)
            "Supervisor stays Object",
            &[],
            "Supervisor",
            ClassKind::Object,
            false,
        ),
    ];
    for (label, external, class, expected, also_value_subclass) in cases {
        let mut h = ClassHierarchy::with_builtins();
        if !external.is_empty() {
            let index: HashMap<String, String> = external
                .iter()
                .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
                .collect();
            h.add_external_superclasses(&index);
        }
        assert_eq!(
            h.resolve_class_kind(class),
            expected,
            "[{label}] resolve_class_kind({class})"
        );
        if also_value_subclass {
            assert!(
                h.is_value_subclass(class),
                "[{label}] {class} should be a value subclass"
            );
        }
    }
}

// --- ADR 0103: handleScope accessor ---
#[test]
fn handle_scope_stored_and_read() {
    let tokens = crate::source_analysis::lex_with_eof(
        "typed Object subclass: MetricsTable native: metrics
  handleScope: #node",
    );
    let (module, _) = crate::source_analysis::parse(tokens);
    let h = ClassHierarchy::build(&module).0.unwrap();
    assert_eq!(
        h.handle_scope("MetricsTable").map(EcoString::as_str),
        Some("node")
    );
}

#[test]
fn handle_scope_none_when_undeclared() {
    let tokens = crate::source_analysis::lex_with_eof("Object subclass: Plain");
    let (module, _) = crate::source_analysis::parse(tokens);
    let h = ClassHierarchy::build(&module).0.unwrap();
    assert_eq!(h.handle_scope("Plain"), None);
}

#[test]
fn handle_scope_inherited_from_ancestor() {
    // A subclass with no declaration of its own inherits the ancestor's scope.
    let tokens = crate::source_analysis::lex_with_eof(
        "typed Object subclass: HandleBase native: hb
  handleScope: #process

HandleBase subclass: HandleChild",
    );
    let (module, _) = crate::source_analysis::parse(tokens);
    let h = ClassHierarchy::build(&module).0.unwrap();
    assert_eq!(
        h.handle_scope("HandleChild").map(EcoString::as_str),
        Some("process")
    );
}
#[test]
fn indirect_value_subclass_sets_is_value_in_hierarchy() {
    // MyValueBase inherits from Value, MyValueChild inherits from MyValueBase.
    // MyValueChild should get is_value = true even though its direct superclass
    // is MyValueBase, not Value.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![
            make_user_class("MyValueBase", "Value"),
            make_user_class("MyValueChild", "MyValueBase"),
        ],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    let base_info = h
        .get_class("MyValueBase")
        .expect("MyValueBase should exist");
    assert!(
        base_info.is_value,
        "MyValueBase (direct Value subclass) should have is_value = true"
    );
    let child_info = h
        .get_class("MyValueChild")
        .expect("MyValueChild should exist");
    assert!(
        child_info.is_value,
        "MyValueChild (indirect Value subclass via MyValueBase) should have is_value = true"
    );
}
#[test]
fn indirect_value_subclass_gets_auto_methods() {
    // MyValueChild inherits from MyValueBase which inherits from Value.
    // MyValueChild should get auto-slot methods synthesized.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![
            make_user_class("MyValueBase", "Value"),
            make_user_class("MyValueChild", "MyValueBase"),
        ],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    let child_info = h
        .get_class("MyValueChild")
        .expect("MyValueChild should exist");
    // make_user_class gives a "count" state field, so we expect an auto-getter
    assert!(
        child_info
            .methods
            .iter()
            .any(|m| m.selector.as_str() == "count"),
        "MyValueChild should have auto-generated 'count' getter"
    );
    assert!(
        child_info
            .methods
            .iter()
            .any(|m| m.selector.as_str() == "withCount:"),
        "MyValueChild should have auto-generated 'withCount:' setter"
    );
}
#[test]
fn testcase_indirect_value_subclass() {
    // TestCase inherits from Value, MyTest inherits from TestCase.
    // MyTest should get is_value = true.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![
            make_user_class("TestCase", "Value"),
            make_user_class("MyTest", "TestCase"),
        ],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    assert!(
        h.is_value_subclass("MyTest"),
        "MyTest should be a Value subclass"
    );
    let info = h.get_class("MyTest").expect("MyTest should exist");
    assert!(info.is_value, "MyTest should have is_value = true");
}
#[test]
fn supervisor_subclass_stays_object() {
    // Supervisor inherits from Object. WebApp inherits from Supervisor.
    // WebApp should remain ClassKind::Object.
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("WebApp", "Supervisor")],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    let info = h.get_class("WebApp").expect("WebApp should exist");
    assert!(
        !info.is_value,
        "WebApp (Supervisor subclass) should not have is_value"
    );
    assert_eq!(
        h.resolve_class_kind("WebApp"),
        ClassKind::Object,
        "WebApp should resolve to ClassKind::Object"
    );
}
#[test]
fn cross_file_indirect_value_subclass() {
    // Simulate: File A defines MyValueBase as Value subclass (external).
    // File B defines MyChild as MyValueBase subclass (in module).
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("MyChild", "MyValueBase")],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    // At this point, MyValueBase is unknown, so MyChild has is_value = false
    let info_before = h.get_class("MyChild").expect("MyChild should exist");
    assert!(
        !info_before.is_value,
        "MyChild should not have is_value before external info"
    );
    // Add external superclass info: MyValueBase inherits from Value
    let mut external = HashMap::new();
    external.insert("MyValueBase".to_string(), "Value".to_string());
    h.add_external_superclasses(&external);
    // Now MyChild should resolve as a Value subclass through the hierarchy
    assert!(
        h.is_value_subclass("MyChild"),
        "MyChild should be detected as Value subclass after cross-file info added"
    );
    // The is_value flag should also be propagated
    let info_after = h.get_class("MyChild").expect("MyChild should exist");
    assert!(
        info_after.is_value,
        "MyChild should have is_value = true after external info added"
    );
}
#[test]
fn add_external_superclasses_indirect_value_is_value() {
    // Both classes are added via external index:
    // MyValueBase -> Value, MyValueChild -> MyValueBase
    let mut h = ClassHierarchy::with_builtins();
    let mut index = HashMap::new();
    index.insert("MyValueBase".to_string(), "Value".to_string());
    index.insert("MyValueChild".to_string(), "MyValueBase".to_string());
    h.add_external_superclasses(&index);
    let base_info = h
        .get_class("MyValueBase")
        .expect("MyValueBase should exist");
    assert!(
        base_info.is_value,
        "MyValueBase should have is_value = true"
    );
    let child_info = h
        .get_class("MyValueChild")
        .expect("MyValueChild should exist");
    assert!(
        child_info.is_value,
        "MyValueChild (indirect Value subclass) should have is_value = true via external superclasses"
    );
}
/// Cross-file Value sub-subclass should find `new:` via hierarchy walk
/// and `propagate_cross_file_class_kind` should synthesize auto-slot methods.
///
/// Simulates: file1 has `Value subclass: Base`, file2 has `Base subclass: Child`.
/// When Child's file is compiled, Base is injected via `add_from_beam_meta`.
#[test]
fn cross_file_value_sub_subclass_finds_new() {
    // Build hierarchy from a module containing only Child (extends Base)
    // Give Child a field so auto-slot methods can be verified.
    let mut child_class = make_user_class("Child", "Base");
    child_class.state.push(crate::ast::StateDeclaration {
        name: crate::ast::Identifier {
            name: EcoString::from("count"),
            span: test_span(),
        },
        default_value: None,
        type_annotation: None,
        declared_keyword: crate::ast::DeclaredKeyword::Field,
        slot_kind: crate::ast::SlotKind::default(),
        expect: None,
        comments: crate::ast::CommentAttachment::default(),
        doc_comment: None,
        span: test_span(),
    });
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![child_class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    // Inject cross-file Base class (from another file: Value subclass: Base)
    let base_info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("Base"),
        superclass: Some(EcoString::from("Value")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![EcoString::from("x")],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![base_info]);
    // Before propagation, Child should NOT be marked as Value
    let child_before = h.get_class("Child").unwrap();
    assert!(
        !child_before.is_value,
        "Child should not be is_value before propagation"
    );
    // Run cross-file propagation
    h.propagate_cross_file_class_kind();
    // After propagation, Child should be marked as Value with auto-slot methods
    let child = h.get_class("Child").unwrap();
    assert!(child.is_value, "Child should be is_value after propagation");
    assert!(
        child.methods.iter().any(|m| m.selector == "count"),
        "Child should have auto-generated 'count' getter"
    );
    assert!(
        child.methods.iter().any(|m| m.selector == "withCount:"),
        "Child should have auto-generated 'withCount:' setter"
    );
    // find_class_method should walk: Child → Base → Value and find new:
    let result = h.find_class_method("Child", "new:");
    assert!(
        result.is_some(),
        "find_class_method('Child', 'new:') should find new: on Value via Base"
    );
    assert_eq!(result.unwrap().defined_in.as_str(), "Value");
}

/// ADR 0124 §1/B5a cross-file test: class `B` (this file) subclasses `A`
/// (a different file, injected via `add_from_beam_meta` the way a
/// cross-package/prior-compiled ancestor is), where `A` declares
/// `late state: proc` and `late classState: current` alongside an
/// ordinary eager field/class-variable of each kind. `ClassInfo` for `B`
/// must report both kinds through the flattened
/// `state_field_kind`/`class_variable_kind` accessors — the metadata B3's
/// guarded read depends on — without `A`'s AST being present in this
/// compilation.
#[test]
fn cross_file_state_field_kind_and_class_variable_kind_report_late() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("B", "A")],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };

    // Inject cross-file A: `late state: proc`, `state: id`,
    // `late classState: current`, `classState: total`.
    let a_info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("A"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: true,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![EcoString::from("proc"), EcoString::from("id")],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::from([
            (EcoString::from("proc"), crate::ast::SlotKind::Late),
            (EcoString::from("id"), crate::ast::SlotKind::Eager),
        ]),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![
            ClassVarInfo {
                name: EcoString::from("current"),
                ty: Some(DeclaredType::simple("TranscriptStream")),
                has_default: false,
                kind: crate::ast::SlotKind::Late,
            },
            ClassVarInfo {
                name: EcoString::from("total"),
                ty: None,
                has_default: true,
                kind: crate::ast::SlotKind::Eager,
            },
        ],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![a_info]);

    // B declares no state/class-variables of its own (make_user_class's
    // "count" state field is irrelevant here) — both kinds must be found
    // by walking B → A.
    assert_eq!(
        h.state_field_kind("B", "proc"),
        crate::ast::SlotKind::Late,
        "B should report A's late instance field as SlotKind::Late"
    );
    assert_eq!(
        h.state_field_kind("B", "id"),
        crate::ast::SlotKind::Eager,
        "B should report A's ordinary instance field as SlotKind::Eager"
    );
    assert_eq!(
        h.class_variable_kind("B", "current"),
        crate::ast::SlotKind::Late,
        "B should report A's late class variable as SlotKind::Late"
    );
    assert_eq!(
        h.class_variable_kind("B", "total"),
        crate::ast::SlotKind::Eager,
        "B should report A's ordinary class variable as SlotKind::Eager"
    );
    // An unknown field degrades to Eager rather than panicking.
    assert_eq!(
        h.state_field_kind("B", "doesNotExist"),
        crate::ast::SlotKind::Eager
    );
    // ADR 0124 B3: the guarded read's hint names the declared type, walked
    // the same way as the kind.
    assert_eq!(
        h.class_variable_type("B", "current"),
        Some(DeclaredType::simple("TranscriptStream")),
        "B should report A's late class variable's declared type"
    );
    assert_eq!(
        h.class_variable_type("B", "total"),
        None,
        "an untyped class variable has no declared type"
    );
    assert_eq!(h.class_variable_type("B", "doesNotExist"), None);
}

// --- ADR 0124 A2a: all_initialize_assigns tests ---

/// Cross-file test (acceptance criterion): parent `A` (a different file,
/// injected via `add_from_beam_meta`) assigns `socket` in its own
/// `initialize`; child `B` (this file) declares nothing of its own.
/// `all_initialize_assigns("B")` must contain `socket` even though `A`'s AST
/// is not present in this compilation — only its `__beamtalk_meta`-sourced
/// `ClassInfo::initialize_assigns` summary is.
#[test]
fn cross_file_all_initialize_assigns_includes_parent_summary() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("B", "A")],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };

    let a_info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("A"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![EcoString::from("socket")],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: BTreeSet::from([EcoString::from("socket")]),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![a_info]);

    let summary = h.all_initialize_assigns("B");
    assert!(
        summary.assigned.contains("socket"),
        "B's flattened summary should include A's own initialize_assigns entry"
    );
    assert!(
        !summary.incomplete,
        "a fully-resolved chain (no native ancestor, no missing link) should not be incomplete"
    );
}

/// A class whose own `initialize` assigns a slot reports it directly (no
/// ancestor involved).
#[test]
fn all_initialize_assigns_includes_own_summary() {
    let mut class = make_user_class("Standalone", "Object");
    class.methods.push(MethodDefinition::new(
        crate::ast::MessageSelector::Unary("initialize".into()),
        vec![],
        vec![],
        test_span(),
    ));
    // Directly seed ClassInfo's initialize_assigns rather than parsing a
    // real body — this test is about the flattening accessor, not the
    // must-analysis itself (covered by `initialize_assigns.rs`'s own tests).
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    // Overwrite the AST-derived (empty-body) summary with a non-empty one,
    // simulating a class whose real `initialize` assigns `value`.
    if let Some(info) = h.get_class_mut("Standalone") {
        info.initialize_assigns = BTreeSet::from([EcoString::from("value")]);
    }

    let summary = h.all_initialize_assigns("Standalone");
    assert_eq!(summary.assigned, BTreeSet::from([EcoString::from("value")]));
    assert!(!summary.incomplete);
}

/// A `native:` ancestor anywhere in the chain marks the summary incomplete
/// (ADR 0056 — its slots belong to a backing `gen_server`, no `initialize`
/// AST to analyse), not merely empty — and `incomplete` is independent of
/// `assigned`: a slot the leaf's own `initialize` genuinely assigns still
/// shows up alongside the incomplete flag, rather than the flag suppressing
/// or replacing the (partial) answer.
#[test]
fn all_initialize_assigns_native_ancestor_marks_incomplete() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Child", "NativeParent")],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };
    // Give Child its own definitely-assigned slot so this test can prove
    // `incomplete` doesn't come at the cost of losing `assigned` data.
    if let Some(info) = h.get_class_mut("Child") {
        info.initialize_assigns = BTreeSet::from([EcoString::from("own")]);
    }

    let native_info = ClassInfo {
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: EcoString::from("NativeParent"),
        superclass: Some(EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: true,
        auto_ctors: None,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    h.add_from_beam_meta(vec![native_info]);

    let summary = h.all_initialize_assigns("Child");
    assert!(
        summary.incomplete,
        "a native: ancestor in the chain must mark the summary incomplete"
    );
    assert!(
        summary.assigned.contains("own"),
        "incomplete must not suppress a slot the leaf's own initialize genuinely assigns"
    );
}

/// A missing chain link (an ancestor absent from the hierarchy entirely —
/// not injected via `add_from_beam_meta` at all) also marks the summary
/// incomplete, distinct from a merely-empty result.
#[test]
fn all_initialize_assigns_missing_chain_link_marks_incomplete() {
    let module = Module {
        pipeline_sends: crate::ast::PipelineSends::default(),
        literal_comments: crate::ast::LiteralComments::default(),
        classes: vec![make_user_class("Orphan", "UnknownAncestor")],
        method_definitions: vec![],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (Ok(h), _) = ClassHierarchy::build(&module) else {
        panic!("build should succeed");
    };

    let summary = h.all_initialize_assigns("Orphan");
    assert!(
        summary.incomplete,
        "an unresolved ancestor (missing from the hierarchy) must mark the summary incomplete"
    );
    assert!(summary.assigned.is_empty());
}

// --- direct constructor tests ---
fn bt4099_hierarchy_for_source(source: &str) -> ClassHierarchy {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    ClassHierarchy::build(&module)
        .0
        .expect("fixture hierarchy should build")
}

#[test]
fn direct_constructors_value_new_init_is_intrinsic_router() {
    // The walk skips `Value`'s `new:` assuming the `@intrinsic
    // basicNewWith` router; `MethodInfo` has no body, so pin the body
    // by parsing `stdlib/src/value.bt` itself.
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/src/value.bt");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
    let tokens = crate::source_analysis::lex_with_eof(&source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let value = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Value")
        .expect("value.bt should define class Value");
    let new_init = value
        .class_methods
        .iter()
        .find(|m| m.selector.name().as_str() == "new:")
        .expect("Value should define class-side `new:`");
    assert_eq!(
        new_init.body.len(),
        1,
        "Value's `new:` should be a single-expression router"
    );
    match &new_init.body[0].expression {
        crate::ast::Expression::Primitive {
            name, is_intrinsic, ..
        } => {
            assert!(
                *is_intrinsic,
                "Value's `new:` must be `@intrinsic`, not `@primitive`"
            );
            assert_eq!(
                name.as_str(),
                "basicNewWith",
                "Value's `new:` must route via `basicNewWith`"
            );
        }
        other => panic!("Value's `new:` must be the intrinsic router, found: {other:?}"),
    }
}

#[test]
fn direct_constructors_plain_value() {
    // Builtin Value itself defines class `new:` (the @intrinsic router), so
    // `new1` being true here proves the ancestor walk skips it. Pin that
    // premise explicitly: if Value ever loses `new:`, revisit the skip.
    let h = bt4099_hierarchy_for_source(
        "Value subclass: Pt\n  state: x :: Integer = 0\n  state: y :: Integer = 0\n",
    );
    let value_info = h.get_class("Value").expect("builtins provide Value");
    assert!(
        value_info
            .class_methods
            .iter()
            .any(|m| m.selector.as_str() == "new:"),
        "premise: Value defines class new: (the @intrinsic router the walk skips)"
    );
    let ctors = h.direct_constructors("Pt");
    assert!(ctors.new0, "plain Value: `Pt new` builds in the caller");
    assert!(ctors.new1, "plain Value: `Pt new:` builds in the caller");
    assert_eq!(
        ctors.keyword.as_deref(),
        Some("x:y:"),
        "plain Value: keyword constructor builds in the caller"
    );
}

/// BT-4409: the per-class fact baked as `defines_user_class_new1` (and asked
/// of each ancestor by the compile-time chain walk) is false for `Value`,
/// whose `new:` is the intrinsic router, and true for `Set`, whose `new:`
/// (`stdlib/src/set.bt`) is user code. Unknown classes fail closed.
#[test]
fn defines_user_class_new1_value_router_and_set_override() {
    let h = ClassHierarchy::with_builtins();
    assert!(
        !h.defines_user_class_new1("Value"),
        "Value's `new:` is the basicNewWith router, not user code"
    );
    assert!(
        h.defines_user_class_new1("Set"),
        "Set defines a user class-side `new:`"
    );
    assert!(
        !h.defines_user_class_new1("Object"),
        "Object defines no class-side `new:`"
    );
    assert!(
        h.defines_user_class_new1("NoSuchClass"),
        "an unknown class fails closed"
    );
}

/// BT-4409: the chain walk and the per-class fact are one rule: an ancestor
/// whose fact is true keeps a descendant's baked `new:` verdict off, and the
/// descendant's own fact stays false (only the class itself is asked).
#[test]
fn chain_walk_uses_per_class_user_new1_fact() {
    let h = bt4099_hierarchy_for_source(
        "Value subclass: Base\n  state: x :: Integer = 0\n  class new: m => Base x: 1\n\
         Base subclass: Leaf\n",
    );
    assert!(h.defines_user_class_new1("Base"));
    assert!(!h.defines_user_class_new1("Leaf"));
    assert!(
        !h.direct_constructors("Leaf").new1,
        "an ancestor's user `new:` keeps the descendant's baked `new:` off"
    );
}

/// BT-4479: the per-class fact baked as `defines_user_class_new0` (and asked
/// of each ancestor by the compile-time chain walk) is false for the root
/// definitions (`Value`'s `basicNew` router, `Actor`'s `Use spawn` error,
/// `Object` which has none) and true for a class with a real class-side `new`.
/// Unlike the `new:` rule, unknown classes read as false (fail open).
#[test]
fn defines_user_class_new0_roots_and_user_override() {
    let h = ClassHierarchy::with_builtins();
    assert!(
        !h.defines_user_class_new0("Value"),
        "Value's `new` is basicNew"
    );
    assert!(
        !h.defines_user_class_new0("Actor"),
        "Actor's `new` only raises"
    );
    assert!(
        !h.defines_user_class_new0("Object"),
        "Object has no class-side `new`"
    );
    assert!(
        !h.defines_user_class_new0("NoSuchClass"),
        "an unknown class fails open"
    );
    assert!(
        h.defines_user_class_new0("Announcer"),
        "Announcer defines a user class-side `new`"
    );
    assert!(
        h.chain_has_user_new0("SystemAnnouncer") == h.defines_user_class_new0("Announcer"),
        "SystemAnnouncer's chain reaches Announcer's `new`"
    );
    assert!(
        !h.chain_has_user_new0("Announcer"),
        "a class's own `new` is not part of its chain answer"
    );
}

/// BT-4479: an ancestor's user `new` is seen by every descendant's chain walk,
/// and keeps the descendant's baked `new` verdict off; a class with its own
/// `new` and an unrelated class are unaffected.
#[test]
fn chain_walk_uses_per_class_user_new0_fact() {
    let h = bt4099_hierarchy_for_source(
        "Object subclass: Base\n  class new => 1\n\
         Base subclass: Leaf\n\
         Leaf subclass: Grand\n\
         Base subclass: Own\n  class new => 2\n\
         Object subclass: Plain\n",
    );
    assert!(h.defines_user_class_new0("Base"));
    assert!(!h.defines_user_class_new0("Leaf"));
    assert!(h.defines_user_class_new0("Own"));
    assert!(!h.chain_has_user_new0("Base"));
    assert!(h.chain_has_user_new0("Leaf"));
    assert!(h.chain_has_user_new0("Grand"));
    assert!(h.chain_has_user_new0("Own"));
    assert!(!h.chain_has_user_new0("Plain"));
    assert!(
        !h.direct_constructors("Leaf").new0,
        "an inherited user `new` keeps the baked `new` off"
    );
}

/// BT-4479: a Value descendant of a plain Value keeps its fast `new` verdict:
/// `Value`'s own `new` does not count as user code.
#[test]
fn value_root_new_does_not_disable_direct_new0() {
    let h = bt4099_hierarchy_for_source("Value subclass: Pt\n  field: x = 0\nPt subclass: Pt3\n");
    assert!(h.direct_constructors("Pt").new0);
    assert!(h.direct_constructors("Pt3").new0);
}

/// BT-4679: the former source-only `direct_constructors_*` tests (12 cases). Each row
/// builds a hierarchy from one source and checks the class's [`DirectConstructors`]
/// verdicts; `None` means the original test did not assert that field.
/// Tests that edit the hierarchy by hand, pin premises or walk missing/cyclic ancestors
/// stay separate.
#[test]
fn direct_constructors_table() {
    struct Row {
        label: &'static str,
        source: &'static str,
        class: &'static str,
        new0: Option<bool>,
        new1: Option<bool>,
        keyword: Option<Option<&'static str>>,
    }
    let rows = [
        Row {
            label: "stateless Value",
            source: "Value subclass: Empty\n",
            class: "Empty",
            new0: Some(true),
            new1: Some(true),
            keyword: Some(None),
        },
        // user nullary `new` runs in the class process but does not affect `new:`
        Row {
            label: "user class-side new blocks new0 only",
            source: "Value subclass: Pt\n  state: x :: Integer = 0\n  class new => Pt x: 1\n",
            class: "Pt",
            new0: Some(false),
            new1: Some(true),
            keyword: None,
        },
        // instance `new` suppresses the synthesized new/0
        Row {
            label: "instance new blocks new0",
            source: "Value subclass: Pt\n  state: x :: Integer = 0\n  new => 42\n",
            class: "Pt",
            new0: Some(false),
            new1: None,
            keyword: None,
        },
        // user `new:` runs in the class process but does not affect nullary `new`
        Row {
            label: "user class-side new: blocks new1 only",
            source: "Value subclass: Pt\n  state: x :: Integer = 0\n  class new: m => Pt x: 1\n",
            class: "Pt",
            new0: Some(true),
            new1: Some(false),
            keyword: None,
        },
        // instance `new:` suppresses the synthesized new/1
        Row {
            label: "instance new: blocks new1",
            source: "Value subclass: Pt\n  state: x :: Integer = 0\n  new: m => 42\n",
            class: "Pt",
            new0: None,
            new1: Some(false),
            keyword: None,
        },
        Row {
            label: "classState blocks all",
            source: "Value subclass: Pt\n  state: x :: Integer = 0\n  classState: total = 0\n",
            class: "Pt",
            new0: Some(false),
            new1: Some(false),
            keyword: Some(None),
        },
        // `new`/`new:` route through the `{new, _}` handler, which rejects abstract
        // classes; keyword constructors go through `class_method_call`, which has no
        // abstract check — the direct call preserves both paths.
        Row {
            label: "abstract blocks new but not keyword",
            source: "abstract Value subclass: Pt\n  state: x :: Integer = 0\n",
            class: "Pt",
            new0: Some(false),
            new1: Some(false),
            keyword: Some(Some("x:")),
        },
        Row {
            label: "non-Value excluded",
            source: "Object subclass: Box\n  state: x :: Integer = 0\n",
            class: "Box",
            new0: Some(false),
            new1: Some(false),
            keyword: Some(None),
        },
        Row {
            label: "Actor excluded",
            source: "Actor subclass: Counter\n  state: count :: Integer = 0\n",
            class: "Counter",
            new0: Some(false),
            new1: Some(false),
            keyword: Some(None),
        },
        Row {
            label: "native excluded",
            source: "Value subclass: Natty native: beamtalk_natty\n  state: x :: Integer = 0\n",
            class: "Natty",
            new0: Some(false),
            new1: Some(false),
            keyword: None,
        },
        // user class-side `x:` suppresses the synthesized keyword constructor
        Row {
            label: "user keyword suppresses keyword",
            source: "Value subclass: Pt\n  state: x :: Integer = 0\n  class x: v => Pt new\n",
            class: "Pt",
            new0: Some(true),
            new1: Some(true),
            keyword: Some(None),
        },
        // BT-4132, revised by BT-4479: same real propagation as the ancestor-`new:`
        // test; an ancestor's nullary user `new` now runs for `Sub new` (the
        // `{new, []}` handler routes through class dispatch to it), so it keeps the
        // baked `new` verdict off. It does not affect `new:`.
        Row {
            label: "ancestor user new is inherited",
            source: "Value subclass: Mid\n  class new => Mid new\nMid subclass: Sub\n",
            class: "Sub",
            new0: Some(false),
            new1: Some(true),
            keyword: None,
        },
    ];
    for row in rows {
        let h = bt4099_hierarchy_for_source(row.source);
        let ctors = h.direct_constructors(row.class);
        if let Some(expected) = row.new0 {
            assert_eq!(ctors.new0, expected, "[{}] new0", row.label);
        }
        if let Some(expected) = row.new1 {
            assert_eq!(ctors.new1, expected, "[{}] new1", row.label);
        }
        if let Some(expected) = row.keyword {
            assert_eq!(
                ctors.keyword.as_deref(),
                expected,
                "[{}] keyword",
                row.label
            );
        }
    }
}

#[test]
fn direct_constructors_unknown_class_fails_closed() {
    let h = bt4099_hierarchy_for_source("Value subclass: Pt\n");
    assert_eq!(h.direct_constructors("Nope"), DirectConstructors::default());
}

#[test]
fn direct_constructors_missing_ancestor_fails_closed_for_new1() {
    let mut h = bt4099_hierarchy_for_source("Value subclass: Pt\n");
    h.classes.get_mut("Pt").expect("Pt should exist").superclass =
        Some(ecow::EcoString::from("Missing"));
    let ctors = h.direct_constructors("Pt");
    assert!(!ctors.new1, "unknown ancestors fail closed");
    assert!(
        ctors.new0,
        "nullary `new` needs no ancestor walk: it always calls the class's own new/0"
    );
}

/// BT-4132: an indirect Value subclass is anchored by `propagate_class_kind`
/// (the real pass, not a hand-built post-writeback shape). Its ancestor's
/// user `new:` wins at run time via chain dispatch, so its `new:` verdict
/// is off; nullary `new` is unaffected.
#[test]
fn direct_constructors_ancestor_user_new_init_blocks_new1() {
    let h = bt4099_hierarchy_for_source(
        "Value subclass: Mid\n  class new: m => Mid new\nMid subclass: Sub\n",
    );
    assert!(
        h.get_class("Sub").expect("Sub").auto_ctors.is_some(),
        "propagation anchors the indirect Value subclass"
    );
    let ctors = h.direct_constructors("Sub");
    assert!(
        !ctors.new1,
        "ancestor user `new:` wins at runtime via chain dispatch"
    );
    assert!(ctors.new0, "ancestor `new:` does not affect nullary `new`");
}

/// BT-4132: `propagate_class_kind` stores the keyword decision as the
/// anchor of an indirect Value subclass, so it gets the same verdicts as a
/// direct `Value subclass:`.
#[test]
fn indirect_value_subclass_gets_constructor_anchor_and_verdicts() {
    let h = bt4099_hierarchy_for_source(
        "Value subclass: Base\n  state: x :: Integer = 0\n\
         Base subclass: Leaf\n  state: y :: Integer = 0\n",
    );
    assert_eq!(
        h.get_class("Leaf").expect("Leaf").auto_ctors,
        Some(AutoConstructors {
            keyword_constructor: Some("y:".into())
        }),
        "the keyword decision (own slots only) is the anchor"
    );
    assert_eq!(
        h.direct_constructors("Leaf"),
        DirectConstructors {
            new0: true,
            new1: true,
            keyword: Some("y:".into()),
        }
    );
}

/// BT-4132: the analysis pipeline runs `propagate_class_kind` twice (again
/// after cross-file classes are injected). The second run must not re-read
/// the class's own synthesized keyword constructor as a user collision.
#[test]
fn repropagation_keeps_indirect_keyword_anchor() {
    let tokens = crate::source_analysis::lex_with_eof(
        "Value subclass: Base\nBase subclass: Leaf\n  state: y :: Integer = 0\n",
    );
    let (module, _) = crate::source_analysis::parse(tokens);
    let mut h = ClassHierarchy::build(&module)
        .0
        .expect("fixture hierarchy should build");
    h.propagate_class_kind(&module);
    h.propagate_cross_file_class_kind();
    assert_eq!(
        h.direct_constructors("Leaf").keyword.as_deref(),
        Some("y:"),
        "re-propagation keeps the recorded keyword decision"
    );
}

/// BT-4132: a user class-side method with the keyword selector suppresses
/// synthesis, and the indirect anchor records that (`None` keyword), while
/// `new`/`new:` stay eligible.
#[test]
fn indirect_value_subclass_user_keyword_suppresses_keyword_verdict() {
    let h = bt4099_hierarchy_for_source(
        "Value subclass: Base\nBase subclass: Leaf\n  state: y :: Integer = 0\n\
         \n  class y: v => Leaf new\n",
    );
    assert_eq!(
        h.get_class("Leaf").expect("Leaf").auto_ctors,
        Some(AutoConstructors {
            keyword_constructor: None
        })
    );
    let ctors = h.direct_constructors("Leaf");
    assert_eq!(ctors.keyword, None);
    assert!(ctors.new0 && ctors.new1);
}

/// BT-4132: `Value` itself is not a Value subclass. Compiling `value.bt`
/// reaches it in `propagate_class_kind`, but it keeps a `None` anchor.
#[test]
fn value_itself_is_never_anchored() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/src/value.bt");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
    let tokens = crate::source_analysis::lex_with_eof(&source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let h = ClassHierarchy::build(&module)
        .0
        .expect("value.bt hierarchy should build");
    assert!(h.get_class("Value").expect("Value").auto_ctors.is_none());
    assert_eq!(
        h.direct_constructors("Value"),
        DirectConstructors::default()
    );
}

/// BT-4132: the chain walk's depth guard, reached through real
/// propagation: a Value subclass 55 classes below `Value` is anchored, but
/// its `new:` walk passes the runtime's cap (50) and fails closed. A class
/// within the cap keeps its `new:` verdict.
#[test]
fn indirect_value_subclass_deep_chain_fails_closed_for_new1() {
    let source: String = std::iter::once("Value subclass: Deep0\n".to_string())
        .chain((1..55).map(|i| format!("Deep{} subclass: Deep{i}\n", i - 1)))
        .collect();
    let h = bt4099_hierarchy_for_source(&source);
    let deep = h.direct_constructors("Deep54");
    assert!(
        h.get_class("Deep54").expect("Deep54").auto_ctors.is_some(),
        "propagation anchors every Value descendant"
    );
    assert!(!deep.new1, "a chain past the depth cap fails closed");
    assert!(deep.new0, "nullary `new` needs no ancestor walk");
    assert!(h.direct_constructors("Deep10").new1);
}

/// BT-4132: the chain walk's `surface_incomplete` guard, reached through the
/// real analysis pipeline: the ancestor comes from a sibling file that had
/// parse errors (the build's class index marks its infos incomplete), and
/// the indirect subclass is anchored by `propagate_class_kind` after the
/// injection. A complete ancestor keeps the `new:` verdict.
#[test]
fn indirect_value_subclass_incomplete_ancestor_fails_closed_for_new1() {
    let leaf_ctors = |mid_incomplete: bool| {
        let mid_module = crate::test_helpers::test_support::parse_bt("Value subclass: Mid\n");
        let mut mid_infos = ClassHierarchy::extract_class_infos(&mid_module);
        for info in &mut mid_infos {
            info.surface_incomplete = mid_incomplete;
        }
        let module = crate::test_helpers::test_support::parse_bt("Mid subclass: Sub\n");
        let result = crate::semantic_analysis::analyse_full(
            &module,
            crate::semantic_analysis::AnalysisContext::default().with_pre_loaded_classes(mid_infos),
        );
        assert!(
            result
                .class_hierarchy
                .get_class("Sub")
                .expect("Sub")
                .auto_ctors
                .is_some(),
            "propagation after injection anchors Sub"
        );
        result.class_hierarchy.direct_constructors("Sub")
    };
    let incomplete = leaf_ctors(true);
    assert!(
        !incomplete.new1,
        "an ancestor with an incomplete surface may hide a user `new:`"
    );
    assert!(incomplete.new0);
    assert!(leaf_ctors(false).new1, "a complete ancestor keeps `new:`");
}

/// BT-4132: an unknown ancestor or a cycle never reaches the walk through
/// real propagation: such a chain never reaches `Value`, so the class is not
/// anchored and gets no verdicts at all. (The walk's own unknown/cycle
/// guards below are defence in depth for a link changed after anchoring.)
#[test]
fn indirect_unknown_or_cyclic_chain_is_never_anchored() {
    let h = bt4099_hierarchy_for_source("Missing subclass: Orphan\n");
    assert!(h.get_class("Orphan").expect("Orphan").auto_ctors.is_none());
    assert_eq!(
        h.direct_constructors("Orphan"),
        DirectConstructors::default()
    );

    let tokens = crate::source_analysis::lex_with_eof("Aaa subclass: Bbb\nBbb subclass: Aaa\n");
    let (module, _) = crate::source_analysis::parse(tokens);
    let (h, _) = ClassHierarchy::build(&module);
    if let Ok(h) = h {
        for name in ["Aaa", "Bbb"] {
            assert!(h.get_class(name).is_none_or(|i| i.auto_ctors.is_none()));
            assert_eq!(h.direct_constructors(name), DirectConstructors::default());
        }
    }
}

/// BT-4132: `propagate_cross_file_class_kind` anchors an AST-less sibling
/// class only where the stored state determines the keyword decision, and
/// never overwrites an anchor the info already carries.
#[test]
fn cross_file_pass_anchors_only_derivable_keyword_decisions() {
    let infos = |src: &str| {
        ClassHierarchy::extract_class_infos(&crate::test_helpers::test_support::parse_bt(src))
    };
    let mut pre_loaded = infos("Value subclass: Base\n");
    pre_loaded.extend(infos("Base subclass: Plain\n  state: y :: Integer = 0\n"));
    pre_loaded.extend(infos("Base subclass: Empty\n"));
    pre_loaded.extend(infos(
        "Base subclass: Collide\n  state: y :: Integer = 0\n\n  class y: v => nil\n",
    ));
    let module = crate::test_helpers::test_support::parse_bt("Object subclass: User\n");
    let h = crate::semantic_analysis::analyse_full(
        &module,
        crate::semantic_analysis::AnalysisContext::default().with_pre_loaded_classes(pre_loaded),
    )
    .class_hierarchy;
    let anchor = |name: &str| h.get_class(name).expect(name).auto_ctors.clone();
    assert_eq!(
        anchor("Plain"),
        Some(AutoConstructors {
            keyword_constructor: Some("y:".into())
        }),
        "a selector absent from the stored class side is synthesized here"
    );
    assert_eq!(
        anchor("Empty"),
        Some(AutoConstructors {
            keyword_constructor: None
        }),
        "a slot-less class provably has no keyword constructor"
    );
    assert_eq!(
        anchor("Collide"),
        None,
        "a selector already on the class side (user or earlier synthesis) is ambiguous: fail closed"
    );
    assert_eq!(
        anchor("Bag"),
        ClassHierarchy::with_builtins()
            .get_class("Bag")
            .expect("Bag")
            .auto_ctors
            .clone(),
        "a builtin's generated anchor is kept"
    );
}

/// BT-4132: the regenerated stdlib builtins anchor indirect Value
/// subclasses too. `Bag` (`Collection(E) subclass:`) gets verdicts; `Set`
/// keeps `new1 = false` (its own user `class new:`); abstract `Collection`
/// keeps `new0 = false`; a user `TestCase` subclass is anchored as well.
#[test]
fn builtin_indirect_value_subclasses_get_verdicts() {
    let h = ClassHierarchy::with_builtins();
    for name in ["Bag", "Interval", "Set", "Collection", "TestCase"] {
        assert!(
            h.get_class(name).expect(name).auto_ctors.is_some(),
            "{name} should carry a generated anchor"
        );
    }
    let bag = h.direct_constructors("Bag");
    assert!(bag.new0 && bag.new1, "Bag: {bag:?}");
    let set = h.direct_constructors("Set");
    assert!(set.new0, "Set: {set:?}");
    assert!(!set.new1, "Set defines a user `class new:`: {set:?}");
    let collection = h.direct_constructors("Collection");
    assert!(
        !collection.new0 && !collection.new1,
        "abstract: {collection:?}"
    );
    assert!(h.get_class("Value").expect("Value").auto_ctors.is_none());

    let t = bt4099_hierarchy_for_source("TestCase subclass: MyTest\n  testOne => nil\n");
    let my_test = t.direct_constructors("MyTest");
    assert!(
        my_test.new0 && my_test.new1,
        "TestCase subclass: {my_test:?}"
    );
}

#[test]
fn direct_constructors_cycle_terminates_and_fails_closed() {
    let mut h = bt4099_hierarchy_for_source("Value subclass: Aaa\nValue subclass: Bbb\n");
    h.classes
        .get_mut("Aaa")
        .expect("Aaa should exist")
        .superclass = Some(ecow::EcoString::from("Bbb"));
    h.classes
        .get_mut("Bbb")
        .expect("Bbb should exist")
        .superclass = Some(ecow::EcoString::from("Aaa"));
    let ctors = h.direct_constructors("Aaa");
    assert!(!ctors.new1);
    assert!(ctors.new0);
}

#[test]
fn direct_constructors_incomplete_ancestor_fails_closed_for_new1() {
    // An existing ancestor with an incomplete surface may hide a user
    // `new:` dropped by error recovery: fail closed.
    let mut h = bt4099_hierarchy_for_source("Value subclass: Mid\nValue subclass: Sub\n");
    {
        let sub = h.classes.get_mut("Sub").expect("Sub should exist");
        sub.superclass = Some(ecow::EcoString::from("Mid"));
        let mid = h.classes.get_mut("Mid").expect("Mid should exist");
        mid.surface_incomplete = true;
    }
    let ctors = h.direct_constructors("Sub");
    assert!(
        !ctors.new1,
        "an ancestor with an incomplete surface may hide a user `new:`"
    );
    assert!(
        ctors.new0,
        "nullary `new` needs no ancestor walk: it always calls the class's own new/0"
    );
}

#[test]
fn process_bound_selectors_are_never_direct_call_eligible() {
    // ADR 0142 §1 rule 3 (BT-4299, BT-4301): every process-bound selector
    // keeps its routing, so none is direct-call eligible even when the class
    // defines it and its chain has no class variables.
    let bound = ClassInfo::process_bound_class_selectors();
    assert_eq!(bound.len(), 13);
    let methods: String = bound
        .iter()
        .map(|sel| {
            let parts: Vec<&str> = sel.split_inclusive(':').collect();
            if sel.ends_with(':') {
                let header: Vec<String> = parts
                    .iter()
                    .enumerate()
                    .map(|(i, kw)| format!("{kw} a{i}"))
                    .collect();
                format!("  class {} => nil\n", header.join(" "))
            } else {
                format!("  class {sel} => nil\n")
            }
        })
        .collect();
    let source = format!("Object subclass: Pb\n{methods}  class plain => nil\n");
    let hierarchy = bt4099_hierarchy_for_source(&source);
    for sel in bound {
        assert!(!hierarchy.is_direct_call_eligible("Pb", sel), "{sel}");
    }
    assert!(hierarchy.is_direct_call_eligible("Pb", "plain"));
}

#[test]
fn direct_call_eligibility_is_adr_0142s_rule() {
    // BT-4301: no `sealed`/`class sealed` requirement; the class must define
    // the method itself; no class in its chain may declare class variables
    // (the runtime's chain-wide rule 2); class-variable reflection selectors
    // are never eligible.
    let hierarchy = bt4099_hierarchy_for_source(
        "Object subclass: Open\n  class foo => 1\n  class hasField: x => 2\n\n\
         Open subclass: OpenSub\n  class bar => 3\n\n\
         Object subclass: Stateful\n  classState: n :: Integer = 0\n  class foo => 1\n\n\
         Stateful subclass: StatelessSub\n  class bar => 3\n",
    );
    assert!(hierarchy.is_direct_call_eligible("Open", "foo"));
    assert!(!hierarchy.is_direct_call_eligible("Open", "hasField:"));
    assert!(hierarchy.is_direct_call_eligible("OpenSub", "bar"));
    assert!(
        !hierarchy.is_direct_call_eligible("OpenSub", "foo"),
        "an inherited class method is not in the class's own table"
    );
    assert!(!hierarchy.is_direct_call_eligible("Stateful", "foo"));
    assert!(
        !hierarchy.is_direct_call_eligible("StatelessSub", "bar"),
        "a subclass of a class with class variables keeps its process"
    );
    assert!(hierarchy.chain_has_class_variables("StatelessSub"));
    assert!(!hierarchy.chain_has_class_variables("OpenSub"));
    assert!(hierarchy.chain_has_class_variables("NoSuchClass"));
}

/// BT-4148: the compile-time class-variable reflection selector set agrees
/// with the runtime's (`beamtalk_object_class:is_class_var_reflection_selector/1`).
/// Both sides are pinned to the shared fixture; the Erlang half is
/// `beamtalk_object_class_tests:class_var_reflection_selectors_match_shared_fixture_test/0`.
#[test]
fn class_var_reflection_selectors_match_shared_fixture() {
    let path = crate::source_analysis::corpus_test_support::repo_root().join(
        "runtime/apps/beamtalk_runtime/test/fixtures/class_var_reflection_selectors_conformance.json",
    );
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read fixture {}: {e}", path.display()));
    let fixture: serde_json::Value = serde_json::from_str(&raw).expect("fixture is JSON");
    let list = |key: &str| -> Vec<String> {
        fixture[key]
            .as_array()
            .unwrap_or_else(|| panic!("fixture.{key} is an array"))
            .iter()
            .map(|v| v.as_str().expect("selector is a string").to_string())
            .collect()
    };
    let members = list("selectors");
    assert!(!members.is_empty());
    for sel in &members {
        assert!(
            ClassInfo::is_class_var_reflection_selector(sel),
            "{sel} is a class-variable reflection selector at runtime"
        );
    }
    for sel in list("non_selectors") {
        assert!(
            !ClassInfo::is_class_var_reflection_selector(&sel),
            "{sel} is not a class-variable reflection selector"
        );
    }
}

/// BT-4148: a user class method named `fieldAt:` / `fieldAt:put:` is never
/// what a send reaches (the class process answers it first), so it is never
/// direct-call eligible.
#[test]
fn field_at_class_methods_are_not_direct_call_eligible() {
    let hierarchy = bt4099_hierarchy_for_source(
        "Object subclass: Reg\n  class fieldAt: k => 1\n  class fieldAt: k put: v => 2\n",
    );
    assert!(!hierarchy.is_direct_call_eligible("Reg", "fieldAt:"));
    assert!(!hierarchy.is_direct_call_eligible("Reg", "fieldAt:put:"));
}

/// BT-4464: `ClassHierarchy::explicit_class_new_builds_in_caller` over the
/// first class of `source`.
fn bt4464_explicit_new_verdict(source: &str) -> bool {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let h = ClassHierarchy::build(&module)
        .0
        .expect("fixture hierarchy should build");
    h.explicit_class_new_builds_in_caller(&module.classes[0])
}

#[test]
fn explicit_class_new_ffi_forward_builds_in_caller() {
    // The `Reference new` shape (stdlib/src/reference.bt).
    assert!(bt4464_explicit_new_verdict(
        "sealed Object subclass: Tag\n  class sealed new -> Tag => (Erlang erlang) make_ref\n"
    ));
    // Sealing plays no part (ADR 0142); literal arguments are fine.
    assert!(bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  class new => (Erlang erlang) phash2: 42\n"
    ));
}

#[test]
fn explicit_class_new_native_self_delegate_builds_in_caller() {
    // The `Queue new` / `Random new` shape.
    assert!(bt4464_explicit_new_verdict(
        "Value subclass: Q native: my_queue\n  class sealed new -> Q => self delegate\n"
    ));
    // `self delegate` without a backing module is the sentinel error, not a
    // forward: leave it on the class process.
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Q\n  class new => self delegate\n"
    ));
}

#[test]
fn explicit_class_new_reading_class_or_args_stays_on_class_process() {
    // `self basicNew` reads the class process's dictionary.
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  class new => self basicNew\n"
    ));
    // A non-literal argument may read `self` or a class variable.
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  class new => (Erlang erlang) list_to_atom: self name\n"
    ));
    // An FFI call with an effect on the calling process keeps the class
    // process, which owns what it creates (the `class-method-process-resource`
    // table; that lint never inspects process-bound selectors).
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  class new => (Erlang ets) new: #tags options: #()\n"
    ));
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  class new => (Erlang erlang) self\n"
    ));
    // Any table row fails closed, including the socket-owning ones with no
    // caller-side lint row: the class process used to own the socket.
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  class new => (Erlang gen_udp) open: 0\n"
    ));
    // More than one statement is not a plain forward.
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  class new =>\n    (Erlang erlang) make_ref.\n    (Erlang erlang) make_ref\n"
    ));
    // No explicit class-side `new` at all.
    assert!(!bt4464_explicit_new_verdict("Object subclass: Tag\n"));
}

#[test]
fn explicit_class_new_needs_a_concrete_stateless_class() {
    assert!(!bt4464_explicit_new_verdict(
        "abstract Object subclass: Tag\n  class new => (Erlang erlang) make_ref\n"
    ));
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  classState: count = 0\n  class new => (Erlang erlang) make_ref\n"
    ));
    assert!(!bt4464_explicit_new_verdict(
        "Actor subclass: Tag\n  class new => (Erlang erlang) make_ref\n"
    ));
    // Class variables anywhere in the chain keep the class process.
    let tokens = crate::source_analysis::lex_with_eof(
        "Object subclass: Base\n  classState: count = 0\n\
         Base subclass: Tag\n  class new => (Erlang erlang) make_ref\n",
    );
    let (module, _) = crate::source_analysis::parse(tokens);
    let h = ClassHierarchy::build(&module)
        .0
        .expect("fixture hierarchy should build");
    assert!(
        !h.explicit_class_new_builds_in_caller(&module.classes[1]),
        "an ancestor's class variables keep the class process"
    );
    // An instance-side `new` replaces `new/0`: nothing to call directly.
    assert!(!bt4464_explicit_new_verdict(
        "Object subclass: Tag\n  new => self\n  class new => (Erlang erlang) make_ref\n"
    ));
}

/// The stdlib classes BT-4464 moves off the class process: pinned by parsing
/// their real sources, so a body change that leaves the rule is caught here.
#[test]
fn explicit_class_new_stdlib_audit() {
    for (file, expected) in [
        ("reference.bt", true),
        ("random.bt", true),
        ("queue.bt", true),
        // BT-4479: passes `self` (so a subclass's instances carry its name),
        // which is not a literal-argument forward.
        ("announcer.bt", false),
        // `@primitive "metaclassNew"` builds a metaclass: not a plain forward.
        ("metaclass.bt", false),
    ] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../stdlib/src")
            .join(file);
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
        let tokens = crate::source_analysis::lex_with_eof(&source);
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::with_builtins();
        assert_eq!(
            h.explicit_class_new_builds_in_caller(&module.classes[0]),
            expected,
            "{file}"
        );
    }
}

/// BT-4475: `ClassHierarchy::explicit_class_new1_runs_in_caller` over the
/// first class of `source`.
fn bt4475_explicit_new1_verdict(source: &str) -> bool {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let h = ClassHierarchy::build(&module)
        .0
        .expect("fixture hierarchy should build");
    h.explicit_class_new1_runs_in_caller(&module.classes[0])
}

#[test]
fn explicit_class_new1_forward_to_own_in_caller_method_runs_in_caller() {
    // The `List new:` shape (stdlib/src/list.bt).
    assert!(bt4475_explicit_new1_verdict(
        "Object subclass: Bag2\n  class withAll: xs => (Erlang lists) reverse: xs\n  class new: xs => self withAll: xs\n"
    ));
    // An FFI forward of the parameter, outside the process-sensitive table.
    assert!(bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class new: xs => (Erlang lists) reverse: xs\n"
    ));
}

#[test]
fn explicit_class_new1_needing_the_class_process_stays_there() {
    // `self delegate` is opaque for `new:` (`AtomicCounter new:` makes an
    // ETS table owned by the running process).
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Q native: my_counter\n  class new: n => self delegate\n"
    ));
    // A process-sensitive FFI row keeps the class process.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class new: n => (Erlang ets) new: n options: #()\n"
    ));
    // `self error:` is not one of the class's own class methods.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class new: _ => self error: \"no\"\n"
    ));
    // Forwarding to a process-bound selector, or to `basicNew`.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class spawnWith: xs => xs\n  class new: xs => self spawnWith: xs\n"
    ));
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class new: xs => self basicNew\n"
    ));
    // A non-parameter, non-literal argument may read anything.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class withAll: xs => (Erlang lists) reverse: xs\n  class new: xs => self withAll: self name\n"
    ));
    // Class variables in the chain keep the class process for `new:` too.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  classState: count = 0\n  class withAll: xs => (Erlang lists) reverse: xs\n  class new: xs => self withAll: xs\n"
    ));
    // An instance-side `new:` of the same name fails closed.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  new: x => x\n  class withAll: xs => (Erlang lists) reverse: xs\n  class new: xs => self withAll: xs\n"
    ));
    // The forward target's own body is checked (one hop): a target that is
    // `self delegate` on a `native:` class, a process-sensitive FFI call, a
    // body reading anything, or itself a forward keeps the class process.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Q native: my_counter\n  class make: n => self delegate\n  class new: n => self make: n\n"
    ));
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class make: n => (Erlang ets) new: n options: #()\n  class new: n => self make: n\n"
    ));
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class make: n => self basicNew\n  class new: n => self make: n\n"
    ));
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class make: n => n\n  class new: n => self make: n\n"
    ));
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class a: n => (Erlang lists) reverse: n\n  class make: n => self a: n\n  class new: n => self make: n\n"
    ));
    // Two statements are not a plain forward.
    assert!(!bt4475_explicit_new1_verdict(
        "Object subclass: Tag\n  class withAll: xs => (Erlang lists) reverse: xs\n  class new: xs =>\n    self withAll: xs.\n    self withAll: xs\n"
    ));
}

/// The stdlib `new:` bodies BT-4475 audited: pinned by parsing their real
/// sources, so a body change that leaves (or enters) the rule is caught here.
#[test]
fn explicit_class_new1_stdlib_audit() {
    for (file, expected) in [
        ("list.bt", true),
        ("set.bt", true),
        ("array.bt", true),
        ("tuple.bt", true),
        // `self delegate` making a process-owned named ETS table.
        ("atomic_counter.bt", false),
        // Bodies that only raise (`self error: ...`).
        ("date_time.bt", false),
        ("regex.bt", false),
        ("uuid.bt", false),
    ] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../stdlib/src")
            .join(file);
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
        let tokens = crate::source_analysis::lex_with_eof(&source);
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::with_builtins();
        assert_eq!(
            h.explicit_class_new1_runs_in_caller(&module.classes[0]),
            expected,
            "{file}"
        );
    }
}

/// BT-4501: `ClassHierarchy::build` publishes the explicit-`new:` verdict on
/// the class's `ClassInfo`, and `runs_class_method_in_caller` answers `new:`
/// from it; a `new:` that needs the class process, and a subclass inheriting
/// a forwarding `new:`, stay process-bound.
#[test]
fn runs_class_method_in_caller_follows_published_explicit_new1_verdict() {
    let src = "Object subclass: Fwd\n  class withAll: xs => (Erlang lists) reverse: xs\n  class new: xs => self withAll: xs\n\nFwd subclass: FwdChild\n\nObject subclass: Basic\n  class new: xs => self basicNew\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let (h, _) = ClassHierarchy::build(&module);
    let h = h.expect("hierarchy");
    assert!(h.get_class("Fwd").unwrap().in_caller_new1);
    assert!(h.runs_class_method_in_caller("Fwd", "new:"));
    assert!(!h.get_class("FwdChild").unwrap().in_caller_new1);
    assert!(!h.runs_class_method_in_caller("FwdChild", "new:"));
    assert!(!h.get_class("Basic").unwrap().in_caller_new1);
    assert!(!h.runs_class_method_in_caller("Basic", "new:"));
    // The ordinary rule is unchanged for other selectors.
    assert!(h.runs_class_method_in_caller("Fwd", "withAll:"));
    assert!(!h.runs_class_method_in_caller("Fwd", "new"));
}

/// BT-4625 / BT-4640: the published `in_caller_new1` is the chain-independent
/// body shape, so Pass 1's `extract_class_infos` carries it and a caller file
/// that only has the sibling's `ClassInfo` answers like the defining module:
/// `true` for a forwarding `new:` whose parent is another file, and `false`
/// when that parent declares class variables.
#[test]
fn explicit_new1_verdict_survives_pass1_extraction_across_files() {
    let parse = |src: &str| {
        let tokens = crate::source_analysis::lex_with_eof(src);
        crate::source_analysis::parse(tokens).0
    };
    let fwd_src = "Base subclass: Fwd\n  class withAll: xs => (Erlang lists) reverse: xs\n  class new: xs => self withAll: xs\n";
    let fwd = parse(fwd_src);
    let caller = parse("Object subclass: Caller\n");
    let fwd_infos = ClassHierarchy::extract_class_infos(&fwd);
    assert!(fwd_infos[0].in_caller_new1);

    for (base_src, expected) in [
        ("Object subclass: Base\n", true),
        ("Object subclass: Base\n  classState: n = 0\n", false),
        ("Actor subclass: Base\n", false),
        ("Value subclass: Base\n", true),
    ] {
        let base = parse(base_src);
        // Caller's view: only Pass 1 infos of both sibling files.
        let (h, _) = ClassHierarchy::build(&caller);
        let mut h = h.expect("hierarchy");
        let mut injected = ClassHierarchy::extract_class_infos(&base);
        injected.extend(fwd_infos.clone());
        assert!(h.add_from_beam_meta(injected));
        // Defining module's view (codegen).
        let (hd, _) = ClassHierarchy::build(&fwd);
        let mut hd = hd.expect("hierarchy");
        assert!(hd.add_from_beam_meta(ClassHierarchy::extract_class_infos(&base)));
        assert_eq!(
            hd.explicit_class_new1_runs_in_caller(&fwd.classes[0]),
            expected,
            "{base_src}"
        );
        assert_eq!(
            h.runs_class_method_in_caller("Fwd", "new:"),
            expected,
            "{base_src}"
        );
        assert_eq!(
            hd.runs_class_method_in_caller("Fwd", "new:"),
            expected,
            "{base_src}"
        );
    }
}

/// BT-4663: the `ClassInfo`/state producer (cross-file and REPL classes) types the
/// synthesized keyword constructor's parameters from the stored slot types.
#[test]
fn state_path_keyword_constructor_carries_slot_types() {
    let slots = vec![
        (EcoString::from("x"), Some(DeclaredType::simple("Integer"))),
        (EcoString::from("label"), None),
    ];
    let mut instance_methods = Vec::new();
    let mut class_methods = Vec::new();
    let _ = ClassHierarchy::synthesize_auto_methods_from_state(
        &EcoString::from("Point"),
        &slots,
        &HashSet::new(),
        &HashSet::new(),
        &mut instance_methods,
        &mut class_methods,
    );
    let ctor = class_methods
        .iter()
        .find(|m| m.selector.as_str() == "x:label:")
        .expect("keyword constructor synthesized");
    assert_eq!(
        ctor.param_types,
        vec![Some(DeclaredType::simple("Integer")), None]
    );
}

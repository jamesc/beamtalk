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
    // BT-1524: new/new: overrides removed from Actor — no longer class-side
    assert!(
        !selectors.contains(&"new"),
        "new must NOT be in Actor class_methods after BT-1524"
    );
    assert!(
        !selectors.contains(&"new:"),
        "new: must NOT be in Actor class_methods after BT-1524"
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
    // (sourced from Actor.bt via build-stdlib)
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

    // BT-1524: new/new: overrides removed — no longer in Actor class_methods
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

/// BT-889: When a class inherits from an external class (defined in a
/// separately-compiled file and therefore absent from the hierarchy), the
/// walk must fall through to Object so that Object-level methods are
/// still visible.
#[test]
fn resolves_selector_through_unknown_external_parent_to_object() {
    // Simulate file 2 compiling CondimentDecorator (extends Beverage from file 1).
    // Beverage is NOT in the hierarchy — only CondimentDecorator is.
    // subclassResponsibility lives on Object, two hops above CondimentDecorator.
    let module = Module {
        classes: vec![make_user_class("CondimentDecorator", "Beverage")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: test_span(),
        }],
        methods: vec![MethodDefinition {
            selector: crate::ast::MessageSelector::Unary("increment".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    }
}

#[test]
fn user_defined_class_added_to_hierarchy() {
    let module = Module {
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("MyInt", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("MyString", "String")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("MyActor", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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

// --- BT-791: stdlib_mode gating tests ---

#[test]
fn stdlib_mode_exempts_builtin_class_from_sealed_check() {
    // In stdlib_mode, a class named "Character" extending sealed "Integer" is allowed.
    let module = Module {
        classes: vec![make_user_class("Character", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("Character", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("MyCustomClass", "Integer")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build_with_options(&module, true);
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("sealed"));
}

// --- BT-778: Character hierarchy tests ---

#[test]
fn character_superclass_chain_includes_integer() {
    // BT-778: Character inherits from Integer in the builtin hierarchy.
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
    // BT-778: Character should resolve Integer methods via inheritance.
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
    // BT-778: Character inherits from Integer which inherits from Number,
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
            selector: crate::ast::MessageSelector::Unary(method_name.into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: sealed,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
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
            selector: crate::ast::MessageSelector::Unary(method_name.into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: sealed,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    }
}

#[test]
fn sealed_method_override_rejected() {
    // Parent defines sealed method "doCustomWork", child tries to override it
    let parent = make_class_with_sealed_method("Parent", "Actor", "doCustomWork", true);
    let child = make_class_with_sealed_method("Child", "Parent", "doCustomWork", false);

    let module = Module {
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![grandparent, parent, grandchild],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
            name: "A".into(),
            superclass: Some("B".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
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
            name: "B".into(),
            superclass: Some("A".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
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
            selector: crate::ast::MessageSelector::Unary("baseMethod".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
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
            selector: crate::ast::MessageSelector::Unary("derivedMethod".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![base, derived],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![make_user_class("Orphan", "NonExistent")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(10, 20),
            },
            MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(30, 40),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
                selector: crate::ast::MessageSelector::Unary("create".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(10, 20),
            },
            MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("create".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(30, 40),
            },
        ],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            },
            MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("decrement".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (_, diags) = ClassHierarchy::build(&module);

    assert!(diags.is_empty());
}

// --- Typed class inheritance tests (BT-587) ---

#[test]
fn typed_class_is_typed() {
    let mut class = make_class_with_sealed_method("StrictCounter", "Actor", "increment", false);
    class.is_typed = true;

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![child, parent],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
    assert_eq!(method.return_type.as_deref(), Some("Integer"));
}

#[test]
fn stdlib_integer_plus_has_param_types() {
    let h = ClassHierarchy::with_builtins();
    let method = h
        .find_method("Integer", "+")
        .expect("Integer >> + should exist");
    assert_eq!(method.param_types, vec![Some("Number".into())]);
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
            selector: crate::ast::MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![],
            return_type: Some(TypeAnnotation::simple("Integer", test_span())),
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        is_sealed: false,
        is_internal: false,
        is_abstract: false,
        is_typed: false,
        supervisor_kind: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let method = hierarchy
        .find_method("Counter", "getValue")
        .expect("Counter >> getValue should exist");
    assert_eq!(method.return_type.as_deref(), Some("Integer"));
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
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        is_sealed: false,
        is_internal: false,
        is_abstract: false,
        is_typed: false,
        supervisor_kind: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let method = hierarchy
        .find_method("Counter", "add:")
        .expect("Counter >> add: should exist");
    assert_eq!(method.return_type.as_deref(), Some("Counter"));
    assert_eq!(method.param_types, vec![Some("Integer".into())]);
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
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    }
}

#[test]
fn state_field_type_returns_type_for_annotated_field() {
    let module = Module {
        classes: vec![make_typed_state_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let (h, diags) = ClassHierarchy::build(&module);
    let h = h.unwrap();
    assert!(diags.is_empty());
    assert_eq!(
        h.state_field_type("Counter", "count"),
        Some(EcoString::from("Integer"))
    );
}

#[test]
fn state_field_type_returns_none_for_untyped_field() {
    let module = Module {
        classes: vec![make_typed_state_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let h = ClassHierarchy::build(&module).0.unwrap();
    assert_eq!(h.state_field_type("Counter", "label"), None);
}

#[test]
fn state_field_type_returns_none_for_unknown_field() {
    let module = Module {
        classes: vec![make_typed_state_class("Counter", "Actor")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let h = ClassHierarchy::build(&module).0.unwrap();
    assert_eq!(h.state_field_type("Counter", "nonexistent"), None);
}

#[test]
fn state_field_type_returns_none_for_unknown_class() {
    let h = ClassHierarchy::with_builtins();
    assert_eq!(h.state_field_type("DoesNotExist", "count"), None);
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
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        Some(EcoString::from("String"))
    );
    // Inherited typed field from parent
    assert_eq!(
        h.state_field_type("Child", "count"),
        Some(EcoString::from("Integer"))
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
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![parent, child],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
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
        Some(EcoString::from("Integer"))
    );
}

#[test]
fn has_class_dnu_override_detects_erlang() {
    // Erlang has doesNotUnderstand:args: as a class method.
    let h = ClassHierarchy::with_builtins();
    assert!(
        h.has_class_dnu_override("Erlang"),
        "Erlang class-side DNU override should be detected"
    );
    // Erlang has no instance-side DNU
    assert!(
        !h.has_instance_dnu_override("Erlang"),
        "Erlang should not have instance-side DNU override"
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

// --- BT-894: Cross-file superclass enrichment tests ---

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

// --- ClassKind / is_value integration tests (BT-922) ---

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
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
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
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        classes: vec![make_user_class("Counter", "Actor")],
        method_definitions: vec![],
        protocols: Vec::new(),
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

/// BT-1056: Test sealed method override detection with external superclasses.
/// When a class inherits from an external (unknown) class, ancestor lookup
/// should not break early; it should continue walking up the chain
/// using the `external_superclasses` data.
#[test]
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
            selector: crate::ast::MessageSelector::Unary("foo".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: true,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
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
            selector: crate::ast::MessageSelector::Unary("foo".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![sealed_base, override_class],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        name: EcoString::from("Counter"),
        superclass: Some(EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![EcoString::from("count")],
        state_types: HashMap::new(),
        methods: vec![MethodInfo {
            selector: EcoString::from("value"),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: EcoString::from("Counter"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(EcoString::from("Integer")),
            param_types: vec![],
            doc: None,
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
        name: EcoString::from("Counter"),
        superclass: Some(EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![EcoString::from("count")],
        state_types: HashMap::new(),
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
        name: EcoString::from("Counter"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
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
        name: EcoString::from("Integer"),
        superclass: None,
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
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

// --- BT-1700: ClassInfo package + is_internal infrastructure ---

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
        name: EcoString::from("RemoteClass"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: Some(EcoString::from("other_pkg")),
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
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
fn add_from_beam_meta_preserves_is_internal_and_package() {
    let mut h = ClassHierarchy::with_builtins();
    let info = ClassInfo {
        name: EcoString::from("InternalHelper"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: true,
        package: Some(EcoString::from("mylib")),
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
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

// --- extract_class_infos tests (BT-1523) ---

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

// --- Extension method registration tests (BT-1517) ---

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
    assert_eq!(ext.return_type.as_deref(), Some("String"));
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
    assert_eq!(ext.param_types, vec![Some("String".into())]);
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
    let original = h.find_method("String", "size").unwrap();

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

// --- BT-1528: ClassKind hierarchy propagation tests ---

#[test]
fn resolve_class_kind_for_direct_value_subclass() {
    let mut h = ClassHierarchy::with_builtins();
    let mut index = HashMap::new();
    index.insert("Point".to_string(), "Value".to_string());
    h.add_external_superclasses(&index);
    assert_eq!(h.resolve_class_kind("Point"), ClassKind::Value);
}

#[test]
fn resolve_class_kind_for_indirect_value_subclass() {
    let mut h = ClassHierarchy::with_builtins();
    let mut index = HashMap::new();
    // MyValueBase inherits from Value, MyValueChild inherits from MyValueBase
    index.insert("MyValueBase".to_string(), "Value".to_string());
    index.insert("MyValueChild".to_string(), "MyValueBase".to_string());
    h.add_external_superclasses(&index);
    assert_eq!(h.resolve_class_kind("MyValueChild"), ClassKind::Value);
    assert!(h.is_value_subclass("MyValueChild"));
}

#[test]
fn resolve_class_kind_for_direct_actor_subclass() {
    let mut h = ClassHierarchy::with_builtins();
    let mut index = HashMap::new();
    index.insert("Counter".to_string(), "Actor".to_string());
    h.add_external_superclasses(&index);
    assert_eq!(h.resolve_class_kind("Counter"), ClassKind::Actor);
}

#[test]
fn resolve_class_kind_for_indirect_actor_subclass() {
    let mut h = ClassHierarchy::with_builtins();
    let mut index = HashMap::new();
    index.insert("MyActor".to_string(), "Actor".to_string());
    index.insert("SpecialActor".to_string(), "MyActor".to_string());
    h.add_external_superclasses(&index);
    assert_eq!(h.resolve_class_kind("SpecialActor"), ClassKind::Actor);
}

#[test]
fn resolve_class_kind_supervisor_stays_object() {
    // Supervisor inherits from Object (not Actor, not Value)
    let h = ClassHierarchy::with_builtins();
    assert_eq!(h.resolve_class_kind("Supervisor"), ClassKind::Object);
}

#[test]
fn indirect_value_subclass_sets_is_value_in_hierarchy() {
    // MyValueBase inherits from Value, MyValueChild inherits from MyValueBase.
    // MyValueChild should get is_value = true even though its direct superclass
    // is MyValueBase, not Value.
    let module = Module {
        classes: vec![
            make_user_class("MyValueBase", "Value"),
            make_user_class("MyValueChild", "MyValueBase"),
        ],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        classes: vec![
            make_user_class("MyValueBase", "Value"),
            make_user_class("MyValueChild", "MyValueBase"),
        ],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        classes: vec![
            make_user_class("TestCase", "Value"),
            make_user_class("MyTest", "TestCase"),
        ],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        classes: vec![make_user_class("WebApp", "Supervisor")],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        classes: vec![make_user_class("MyChild", "MyValueBase")],
        method_definitions: vec![],
        protocols: Vec::new(),
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

/// BT-1559: Cross-file Value sub-subclass should find `new:` via hierarchy walk
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
        comments: crate::ast::CommentAttachment::default(),
        doc_comment: None,
        span: test_span(),
    });
    let module = Module {
        classes: vec![child_class],
        method_definitions: vec![],
        protocols: Vec::new(),
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
        name: EcoString::from("Base"),
        superclass: Some(EcoString::from("Value")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![EcoString::from("x")],
        state_types: HashMap::new(),
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

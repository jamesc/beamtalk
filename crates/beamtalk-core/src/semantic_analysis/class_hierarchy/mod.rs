// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Static class hierarchy for compile-time method resolution.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements the `ClassHierarchy` domain service from ADR 0006 Phase 1a.
//! It provides compile-time knowledge of the full class hierarchy, consumed by:
//! - **Codegen**: method table generation, actor detection, fast-path decisions
//! - **LSP**: completions with inherited methods, hover info
//! - **Diagnostics**: sealed class enforcement, method existence validation
//!
//! The hierarchy uses simple depth-first MRO (no multiple inheritance).

use crate::ast::{MethodKind, Module};
use crate::source_analysis::Diagnostic;
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

mod builtins;

/// Information about a method in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodInfo {
    /// Full selector name (e.g., "at:put:", "+", "size").
    pub selector: EcoString,
    /// Number of arguments.
    pub arity: usize,
    /// Method kind (Primary, Before, After, Around).
    pub kind: MethodKind,
    /// Class that defines this method.
    pub defined_in: EcoString,
    /// Whether this method is sealed (cannot be overridden).
    pub is_sealed: bool,
}

impl MethodInfo {
    /// Returns `true` if this is a Before, After, or Around advice method.
    #[must_use]
    pub fn is_advice(&self) -> bool {
        matches!(
            self.kind,
            MethodKind::Before | MethodKind::After | MethodKind::Around
        )
    }

    /// Returns `true` if `self` (a subclass method) can override `ancestor`.
    ///
    /// A primary method cannot override a sealed primary method with the same selector.
    /// Advice methods are never overrides. Different selectors are never overrides.
    #[must_use]
    pub fn can_override(&self, ancestor: &MethodInfo) -> bool {
        if self.selector != ancestor.selector {
            return false;
        }
        !(ancestor.is_sealed && self.kind == MethodKind::Primary)
    }
}

/// Information about a class in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassInfo {
    /// Class name.
    pub name: EcoString,
    /// Superclass name (`None` for `ProtoObject`).
    pub superclass: Option<EcoString>,
    /// Whether this class is sealed (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether this class is abstract.
    pub is_abstract: bool,
    /// State (instance variable) names.
    pub state: Vec<EcoString>,
    /// Methods defined directly on this class.
    pub methods: Vec<MethodInfo>,
}

impl ClassInfo {
    /// Returns `true` if this class can be subclassed.
    #[must_use]
    pub fn can_be_subclassed(&self) -> bool {
        !self.is_sealed
    }
}

/// Static class hierarchy built during semantic analysis.
///
/// Provides compile-time knowledge of the full class hierarchy,
/// including built-in classes and user-defined classes from the AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassHierarchy {
    classes: HashMap<EcoString, ClassInfo>,
}

impl ClassHierarchy {
    /// Build a class hierarchy from built-in definitions and a parsed module.
    ///
    /// Returns the hierarchy and any diagnostics (e.g., sealed class violations).
    #[must_use]
    pub fn build(module: &Module) -> (Self, Vec<Diagnostic>) {
        let mut hierarchy = Self::with_builtins();
        let diagnostics = hierarchy.add_module_classes(module);
        (hierarchy, diagnostics)
    }

    /// Create a hierarchy with only built-in classes.
    #[must_use]
    pub fn with_builtins() -> Self {
        Self {
            classes: builtins::builtin_classes(),
        }
    }

    /// Look up a class by name.
    #[must_use]
    pub fn get_class(&self, name: &str) -> Option<&ClassInfo> {
        self.classes.get(name)
    }

    /// Check if a class exists in the hierarchy.
    #[must_use]
    pub fn has_class(&self, name: &str) -> bool {
        self.classes.contains_key(name)
    }

    /// Returns the ordered superclass chain for a class (excluding the class itself).
    ///
    /// Example: `superclass_chain("Counter")` → `["Actor", "Object", "ProtoObject"]`
    ///
    /// Handles cycles gracefully by tracking visited classes.
    #[must_use]
    pub fn superclass_chain(&self, class_name: &str) -> Vec<EcoString> {
        let mut chain = Vec::new();
        let mut current = class_name.to_string();
        let mut visited = HashSet::new();
        visited.insert(class_name.to_string());

        loop {
            let Some(info) = self.classes.get(current.as_str()) else {
                break;
            };
            let Some(ref superclass) = info.superclass else {
                break;
            };
            if !visited.insert(superclass.to_string()) {
                break; // Cycle detected
            }
            chain.push(superclass.clone());
            current = superclass.to_string();
        }

        chain
    }

    /// Returns all methods available on a class (local + inherited).
    ///
    /// Methods are returned in MRO order: local methods first, then inherited.
    /// If a subclass overrides a superclass method (same selector), only the
    /// subclass version is included.
    ///
    /// Handles cycles gracefully by tracking visited classes.
    #[must_use]
    pub fn all_methods(&self, class_name: &str) -> Vec<MethodInfo> {
        let mut seen_selectors: HashMap<EcoString, usize> = HashMap::new();
        let mut methods = Vec::new();
        let mut visited = HashSet::new();

        // Walk MRO: class itself, then superclass chain
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break; // Cycle detected
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                for method in &info.methods {
                    if method.kind == MethodKind::Primary {
                        if !seen_selectors.contains_key(&method.selector) {
                            seen_selectors.insert(method.selector.clone(), methods.len());
                            methods.push(method.clone());
                        }
                    } else {
                        // Before/After/Around methods are always collected
                        methods.push(method.clone());
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }

        methods
    }

    /// Check if a class can respond to a given selector (local or inherited).
    ///
    /// Handles cycles gracefully by tracking visited classes.
    #[must_use]
    pub fn resolves_selector(&self, class_name: &str, selector: &str) -> bool {
        let mut current = Some(class_name.to_string());
        let mut visited = HashSet::new();
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break; // Cycle detected
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                if info.methods.iter().any(|m| m.selector.as_str() == selector) {
                    return true;
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }
        false
    }

    /// Walk the superclass chain looking for a sealed primary method with the given selector.
    /// Returns `Some((class_name, MethodInfo))` if found, `None` otherwise.
    fn find_sealed_method_in_ancestors(
        &self,
        start_class: &str,
        selector: &str,
    ) -> Option<(EcoString, MethodInfo)> {
        let mut current = Some(start_class.to_string());
        let mut visited = HashSet::new();
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                for method in &info.methods {
                    if method.selector.as_str() == selector
                        && method.is_sealed
                        && !method.is_advice()
                    {
                        return Some((info.name.clone(), method.clone()));
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }
        None
    }

    /// Add classes from a parsed module. Returns diagnostics for errors.
    fn add_module_classes(&mut self, module: &Module) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for class in &module.classes {
            // Check sealed class enforcement
            if let Some(super_info) = self.classes.get(class.superclass.name.as_str()) {
                if !super_info.can_be_subclassed() {
                    diagnostics.push(Diagnostic::error(
                        format!("Cannot subclass sealed class `{}`", class.superclass.name),
                        class.superclass.span,
                    ));
                    // Still register the class so downstream passes (codegen routing,
                    // completions) can reason about it despite the error.
                }
            }

            // Check sealed method override enforcement (advice methods are not overrides)
            for method in &class.methods {
                if method.kind != MethodKind::Primary {
                    continue;
                }
                let selector = method.selector.name();
                if let Some((sealed_class, _)) =
                    self.find_sealed_method_in_ancestors(class.superclass.name.as_str(), &selector)
                {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "Cannot override sealed method `{selector}` from class `{sealed_class}`"
                        ),
                        method.span,
                    ));
                }
            }

            let class_info = ClassInfo {
                name: class.name.name.clone(),
                superclass: Some(class.superclass.name.clone()),
                is_sealed: class.is_sealed,
                is_abstract: class.is_abstract,
                state: class.state.iter().map(|s| s.name.name.clone()).collect(),
                methods: class
                    .methods
                    .iter()
                    .map(|m| MethodInfo {
                        selector: m.selector.name(),
                        arity: m.selector.arity(),
                        kind: m.kind,
                        defined_in: class.name.name.clone(),
                        is_sealed: m.is_sealed,
                    })
                    .collect(),
            };

            self.classes.insert(class.name.name.clone(), class_info);
        }

        diagnostics
    }
}

impl Default for ClassHierarchy {
    fn default() -> Self {
        Self::with_builtins()
    }
}

#[cfg(test)]
mod tests {
    use super::builtins::builtin_method;
    use super::*;
    use crate::ast::{ClassDefinition, Identifier, MethodDefinition, StateDeclaration};
    use crate::source_analysis::Span;

    fn test_span() -> Span {
        Span::new(0, 0)
    }

    // --- Value object method tests ---

    #[test]
    fn method_info_is_advice() {
        let primary = builtin_method("foo", 0, "Test");
        assert!(!primary.is_advice());

        let before = MethodInfo {
            kind: MethodKind::Before,
            ..builtin_method("foo", 0, "Test")
        };
        assert!(before.is_advice());

        let after = MethodInfo {
            kind: MethodKind::After,
            ..builtin_method("foo", 0, "Test")
        };
        assert!(after.is_advice());

        let around = MethodInfo {
            kind: MethodKind::Around,
            ..builtin_method("foo", 0, "Test")
        };
        assert!(around.is_advice());
    }

    #[test]
    fn method_info_can_override_non_sealed() {
        let child = builtin_method("describe", 0, "Counter");
        let ancestor = builtin_method("describe", 0, "Object");
        assert!(child.can_override(&ancestor));
    }

    #[test]
    fn method_info_cannot_override_sealed() {
        let child = builtin_method("describe", 0, "Counter");
        let ancestor = MethodInfo {
            is_sealed: true,
            ..builtin_method("describe", 0, "Actor")
        };
        assert!(!child.can_override(&ancestor));
    }

    #[test]
    fn method_info_different_selectors_not_override() {
        let child = builtin_method("increment", 0, "Counter");
        let sealed_ancestor = MethodInfo {
            is_sealed: true,
            ..builtin_method("describe", 0, "Actor")
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
        assert!(h.has_class("String"));
        assert!(h.has_class("Array"));
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
            vec![EcoString::from("Object"), EcoString::from("ProtoObject"),]
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

        // Actor's own methods
        assert!(selectors.contains(&"spawn"));
        assert!(selectors.contains(&"describe"));

        // Inherited from Object
        assert!(selectors.contains(&"isNil"));
        assert!(selectors.contains(&"respondsTo:"));

        // Inherited from ProtoObject
        assert!(selectors.contains(&"class"));
        assert!(selectors.contains(&"=="));
    }

    #[test]
    fn all_methods_overrides_use_most_specific() {
        let h = ClassHierarchy::with_builtins();
        let methods = h.all_methods("Actor");

        // Actor defines 'describe', Object also defines 'describe'
        // Only Actor's version should appear
        let describe_methods: Vec<&MethodInfo> = methods
            .iter()
            .filter(|m| m.selector == "describe")
            .collect();
        assert_eq!(describe_methods.len(), 1);
        assert_eq!(describe_methods[0].defined_in.as_str(), "Actor");
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
    }

    // --- User-defined class tests ---

    fn make_user_class(name: &str, superclass: &str) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, test_span()),
            superclass: Identifier::new(superclass, test_span()),
            is_abstract: false,
            is_sealed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("count", test_span()),
                type_annotation: None,
                default_value: None,
                span: test_span(),
            }],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                span: test_span(),
            }],
            span: test_span(),
        }
    }

    #[test]
    fn user_defined_class_added_to_hierarchy() {
        let module = Module {
            classes: vec![make_user_class("Counter", "Actor")],
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, diags) = ClassHierarchy::build(&module);
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
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, _) = ClassHierarchy::build(&module);

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
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, _) = ClassHierarchy::build(&module);

        // Local method
        assert!(h.resolves_selector("Counter", "increment"));
        // Inherited from Actor
        assert!(h.resolves_selector("Counter", "spawn"));
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
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, diags) = ClassHierarchy::build(&module);

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
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (_, diags) = ClassHierarchy::build(&module);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed"));
    }

    #[test]
    fn non_sealed_subclassing_allowed() {
        let module = Module {
            classes: vec![make_user_class("MyActor", "Actor")],
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, diags) = ClassHierarchy::build(&module);
        assert!(diags.is_empty());
        assert!(h.has_class("MyActor"));
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
            superclass: Identifier::new(superclass, test_span()),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary(method_name.into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: sealed,
                kind: MethodKind::Primary,
                span: test_span(),
            }],
            span: test_span(),
        }
    }

    #[test]
    fn sealed_method_override_rejected() {
        // Parent defines sealed method "describe", child tries to override it
        let parent = make_class_with_sealed_method("Parent", "Actor", "describe", true);
        let child = make_class_with_sealed_method("Child", "Parent", "describe", false);

        let module = Module {
            classes: vec![parent, child],
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, diags) = ClassHierarchy::build(&module);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed method"));
        assert!(diags[0].message.contains("`describe`"));
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
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
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
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, diags) = ClassHierarchy::build(&module);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed method"));
        assert!(diags[0].message.contains("`locked`"));
        assert!(diags[0].message.contains("`GrandParent`"));
        assert!(h.has_class("GrandChild"));
    }

    #[test]
    fn before_method_on_sealed_method_allowed() {
        // Parent defines sealed primary method, child adds a before advice — not an override
        let parent = make_class_with_sealed_method("Parent", "Actor", "frozen", true);
        let mut child = make_class_with_sealed_method("Child", "Parent", "frozen", false);
        child.methods[0].kind = MethodKind::Before;

        let module = Module {
            classes: vec![parent, child],
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (_, diags) = ClassHierarchy::build(&module);
        assert!(diags.is_empty());
    }

    // --- MRO verification ---

    #[test]
    fn mro_is_depth_first() {
        // Counter -> Actor -> Object -> ProtoObject
        let module = Module {
            classes: vec![make_user_class("Counter", "Actor")],
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, _) = ClassHierarchy::build(&module);
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
                state: vec![],
                methods: vec![builtin_method("methodA", 0, "A")],
            },
        );
        h.classes.insert(
            "B".into(),
            ClassInfo {
                name: "B".into(),
                superclass: Some("A".into()),
                is_sealed: false,
                is_abstract: false,
                state: vec![],
                methods: vec![builtin_method("methodB", 0, "B")],
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
            superclass: Identifier::new("Actor", test_span()),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("baseMethod".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                span: test_span(),
            }],
            span: test_span(),
        };
        let derived = ClassDefinition {
            name: Identifier::new("Derived", test_span()),
            superclass: Identifier::new("Base", test_span()),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("derivedMethod".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                span: test_span(),
            }],
            span: test_span(),
        };

        let module = Module {
            classes: vec![base, derived],
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, diags) = ClassHierarchy::build(&module);
        assert!(diags.is_empty());

        // Derived should inherit from Base -> Actor -> Object -> ProtoObject
        assert!(h.resolves_selector("Derived", "derivedMethod"));
        assert!(h.resolves_selector("Derived", "baseMethod"));
        assert!(h.resolves_selector("Derived", "spawn"));
        assert!(h.resolves_selector("Derived", "class"));
    }

    #[test]
    fn user_class_with_unknown_superclass() {
        let module = Module {
            classes: vec![make_user_class("Orphan", "NonExistent")],
            expressions: vec![],
            span: test_span(),
            leading_comments: vec![],
        };
        let (h, diags) = ClassHierarchy::build(&module);

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
}

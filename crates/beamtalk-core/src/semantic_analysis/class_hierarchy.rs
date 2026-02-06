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

/// Information about a method in the hierarchy.
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

/// Information about a class in the hierarchy.
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
        let mut hierarchy = Self {
            classes: HashMap::new(),
        };
        hierarchy.register_builtins();
        hierarchy
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

    /// Add classes from a parsed module. Returns diagnostics for errors.
    fn add_module_classes(&mut self, module: &Module) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for class in &module.classes {
            // Check sealed class enforcement
            if let Some(super_info) = self.classes.get(class.superclass.name.as_str()) {
                if super_info.is_sealed {
                    diagnostics.push(Diagnostic::error(
                        format!("Cannot subclass sealed class `{}`", class.superclass.name),
                        class.superclass.span,
                    ));
                    continue;
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

    /// Register all built-in classes.
    fn register_builtins(&mut self) {
        self.register_proto_object();
        self.register_object();
        self.register_actor();
        self.register_integer();
        self.register_string();
        self.register_array();
        self.register_list();
        self.register_dictionary();
        self.register_set();
        self.register_block();
        self.register_nil();
        self.register_true();
        self.register_false();
        self.register_collection();
    }

    fn register_proto_object(&mut self) {
        self.classes.insert(
            "ProtoObject".into(),
            ClassInfo {
                name: "ProtoObject".into(),
                superclass: None,
                is_sealed: false,
                is_abstract: true,
                state: vec![],
                methods: vec![
                    builtin_method("class", 0, "ProtoObject"),
                    builtin_method("==", 1, "ProtoObject"),
                    builtin_method("~=", 1, "ProtoObject"),
                    builtin_method("doesNotUnderstand:args:", 2, "ProtoObject"),
                    builtin_method("perform:withArguments:", 2, "ProtoObject"),
                    builtin_method("error:", 1, "ProtoObject"),
                ],
            },
        );
    }

    fn register_object(&mut self) {
        self.classes.insert(
            "Object".into(),
            ClassInfo {
                name: "Object".into(),
                superclass: Some("ProtoObject".into()),
                is_sealed: false,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("isNil", 0, "Object"),
                    builtin_method("notNil", 0, "Object"),
                    builtin_method("ifNil:", 1, "Object"),
                    builtin_method("ifNotNil:", 1, "Object"),
                    builtin_method("ifNil:ifNotNil:", 2, "Object"),
                    builtin_method("inspect", 0, "Object"),
                    builtin_method("describe", 0, "Object"),
                    builtin_method("respondsTo:", 1, "Object"),
                    builtin_method("instVarNames", 0, "Object"),
                    builtin_method("instVarAt:", 1, "Object"),
                    builtin_method("instVarAt:put:", 2, "Object"),
                    builtin_method("perform:", 1, "Object"),
                    builtin_method("new", 0, "Object"),
                    builtin_method("new:", 1, "Object"),
                ],
            },
        );
    }

    fn register_actor(&mut self) {
        self.classes.insert(
            "Actor".into(),
            ClassInfo {
                name: "Actor".into(),
                superclass: Some("Object".into()),
                is_sealed: false,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("spawn", 0, "Actor"),
                    builtin_method("spawn:", 1, "Actor"),
                    builtin_method("describe", 0, "Actor"),
                ],
            },
        );
    }

    fn register_integer(&mut self) {
        self.classes.insert(
            "Integer".into(),
            ClassInfo {
                name: "Integer".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("+", 1, "Integer"),
                    builtin_method("-", 1, "Integer"),
                    builtin_method("*", 1, "Integer"),
                    builtin_method("/", 1, "Integer"),
                    builtin_method("%", 1, "Integer"),
                    builtin_method("**", 1, "Integer"),
                    builtin_method("=", 1, "Integer"),
                    builtin_method("<", 1, "Integer"),
                    builtin_method("<=", 1, "Integer"),
                    builtin_method(">", 1, "Integer"),
                    builtin_method(">=", 1, "Integer"),
                    builtin_method("negated", 0, "Integer"),
                    builtin_method("abs", 0, "Integer"),
                    builtin_method("isZero", 0, "Integer"),
                    builtin_method("isEven", 0, "Integer"),
                    builtin_method("isOdd", 0, "Integer"),
                    builtin_method("isPositive", 0, "Integer"),
                    builtin_method("isNegative", 0, "Integer"),
                    builtin_method("min:", 1, "Integer"),
                    builtin_method("max:", 1, "Integer"),
                    builtin_method("timesRepeat:", 1, "Integer"),
                    builtin_method("to:do:", 2, "Integer"),
                    builtin_method("to:by:do:", 3, "Integer"),
                    builtin_method("asFloat", 0, "Integer"),
                    builtin_method("asString", 0, "Integer"),
                ],
            },
        );
    }

    fn register_string(&mut self) {
        self.classes.insert(
            "String".into(),
            ClassInfo {
                name: "String".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("length", 0, "String"),
                    builtin_method("at:", 1, "String"),
                    builtin_method("++", 1, "String"),
                    builtin_method(",", 1, "String"),
                    builtin_method("=", 1, "String"),
                    builtin_method("<", 1, "String"),
                    builtin_method(">", 1, "String"),
                    builtin_method("<=", 1, "String"),
                    builtin_method(">=", 1, "String"),
                    builtin_method("uppercase", 0, "String"),
                    builtin_method("lowercase", 0, "String"),
                    builtin_method("trim", 0, "String"),
                    builtin_method("includes:", 1, "String"),
                    builtin_method("startsWith:", 1, "String"),
                    builtin_method("endsWith:", 1, "String"),
                    builtin_method("split:", 1, "String"),
                    builtin_method("isEmpty", 0, "String"),
                    builtin_method("isNotEmpty", 0, "String"),
                    builtin_method("reverse", 0, "String"),
                    builtin_method("asInteger", 0, "String"),
                    builtin_method("asFloat", 0, "String"),
                    builtin_method("asList", 0, "String"),
                ],
            },
        );
    }

    fn register_array(&mut self) {
        self.classes.insert(
            "Array".into(),
            ClassInfo {
                name: "Array".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("size", 0, "Array"),
                    builtin_method("at:", 1, "Array"),
                    builtin_method("at:put:", 2, "Array"),
                    builtin_method("++", 1, "Array"),
                    builtin_method("=", 1, "Array"),
                    builtin_method("each:", 1, "Array"),
                    builtin_method("collect:", 1, "Array"),
                    builtin_method("select:", 1, "Array"),
                    builtin_method("inject:into:", 2, "Array"),
                    builtin_method("asList", 0, "Array"),
                    builtin_method("asSet", 0, "Array"),
                ],
            },
        );
    }

    fn register_list(&mut self) {
        self.classes.insert(
            "List".into(),
            ClassInfo {
                name: "List".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("size", 0, "List"),
                    builtin_method("head", 0, "List"),
                    builtin_method("tail", 0, "List"),
                    builtin_method("at:", 1, "List"),
                    builtin_method("++", 1, "List"),
                    builtin_method("prepend:", 1, "List"),
                    builtin_method("=", 1, "List"),
                    builtin_method("each:", 1, "List"),
                    builtin_method("collect:", 1, "List"),
                    builtin_method("select:", 1, "List"),
                    builtin_method("inject:into:", 2, "List"),
                    builtin_method("reversed", 0, "List"),
                    builtin_method("asArray", 0, "List"),
                    builtin_method("asSet", 0, "List"),
                ],
            },
        );
    }

    fn register_dictionary(&mut self) {
        self.classes.insert(
            "Dictionary".into(),
            ClassInfo {
                name: "Dictionary".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("at:", 1, "Dictionary"),
                    builtin_method("at:put:", 2, "Dictionary"),
                    builtin_method("size", 0, "Dictionary"),
                    builtin_method("keys", 0, "Dictionary"),
                    builtin_method("values", 0, "Dictionary"),
                    builtin_method("includesKey:", 1, "Dictionary"),
                    builtin_method("removeKey:", 1, "Dictionary"),
                    builtin_method("each:", 1, "Dictionary"),
                    builtin_method("collect:", 1, "Dictionary"),
                    builtin_method("select:", 1, "Dictionary"),
                    builtin_method("inject:into:", 2, "Dictionary"),
                    builtin_method("=", 1, "Dictionary"),
                    builtin_method("asList", 0, "Dictionary"),
                ],
            },
        );
    }

    fn register_set(&mut self) {
        self.classes.insert(
            "Set".into(),
            ClassInfo {
                name: "Set".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("add:", 1, "Set"),
                    builtin_method("remove:", 1, "Set"),
                    builtin_method("includes:", 1, "Set"),
                    builtin_method("size", 0, "Set"),
                    builtin_method("each:", 1, "Set"),
                    builtin_method("collect:", 1, "Set"),
                    builtin_method("select:", 1, "Set"),
                    builtin_method("=", 1, "Set"),
                    builtin_method("asList", 0, "Set"),
                    builtin_method("asArray", 0, "Set"),
                ],
            },
        );
    }

    fn register_block(&mut self) {
        self.classes.insert(
            "Block".into(),
            ClassInfo {
                name: "Block".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("value", 0, "Block"),
                    builtin_method("value:", 1, "Block"),
                    builtin_method("value:value:", 2, "Block"),
                    builtin_method("value:value:value:", 3, "Block"),
                    builtin_method("whileTrue:", 1, "Block"),
                    builtin_method("whileFalse:", 1, "Block"),
                    builtin_method("repeat", 0, "Block"),
                ],
            },
        );
    }

    fn register_nil(&mut self) {
        self.classes.insert(
            "Nil".into(),
            ClassInfo {
                name: "Nil".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("isNil", 0, "Nil"),
                    builtin_method("notNil", 0, "Nil"),
                    builtin_method("ifNil:", 1, "Nil"),
                    builtin_method("ifNotNil:", 1, "Nil"),
                    builtin_method("ifNil:ifNotNil:", 2, "Nil"),
                ],
            },
        );
    }

    fn register_true(&mut self) {
        self.classes.insert(
            "True".into(),
            ClassInfo {
                name: "True".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("ifTrue:ifFalse:", 2, "True"),
                    builtin_method("ifTrue:", 1, "True"),
                    builtin_method("ifFalse:", 1, "True"),
                    builtin_method("and:", 1, "True"),
                    builtin_method("or:", 1, "True"),
                    builtin_method("not", 0, "True"),
                ],
            },
        );
    }

    fn register_false(&mut self) {
        self.classes.insert(
            "False".into(),
            ClassInfo {
                name: "False".into(),
                superclass: Some("Object".into()),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![
                    builtin_method("ifTrue:ifFalse:", 2, "False"),
                    builtin_method("ifTrue:", 1, "False"),
                    builtin_method("ifFalse:", 1, "False"),
                    builtin_method("and:", 1, "False"),
                    builtin_method("or:", 1, "False"),
                    builtin_method("not", 0, "False"),
                ],
            },
        );
    }

    fn register_collection(&mut self) {
        self.classes.insert(
            "Collection".into(),
            ClassInfo {
                name: "Collection".into(),
                superclass: Some("Object".into()),
                is_sealed: false,
                is_abstract: true,
                state: vec![],
                methods: vec![
                    builtin_method("size", 0, "Collection"),
                    builtin_method("isEmpty", 0, "Collection"),
                    builtin_method("isNotEmpty", 0, "Collection"),
                    builtin_method("includes:", 1, "Collection"),
                    builtin_method("each:", 1, "Collection"),
                    builtin_method("collect:", 1, "Collection"),
                    builtin_method("select:", 1, "Collection"),
                    builtin_method("reject:", 1, "Collection"),
                    builtin_method("inject:into:", 2, "Collection"),
                ],
            },
        );
    }
}

impl Default for ClassHierarchy {
    fn default() -> Self {
        Self::with_builtins()
    }
}

/// Create a built-in method info.
fn builtin_method(selector: &str, arity: usize, defined_in: &str) -> MethodInfo {
    MethodInfo {
        selector: selector.into(),
        arity,
        kind: MethodKind::Primary,
        defined_in: defined_in.into(),
        is_sealed: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ClassDefinition, Identifier, MethodDefinition, StateDeclaration};
    use crate::source_analysis::Span;

    fn test_span() -> Span {
        Span::new(0, 0)
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
        assert!(h.has_class("Nil"));
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
        // Class should NOT be added
        assert!(!h.has_class("MyInt"));
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

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Scope tracking for semantic analysis.
//!
//! This module provides scope tracking to distinguish local variables from
//! captured variables during AST traversal. Scopes are organized hierarchically:
//! - Module (depth 0)
//! - Class (depth 1)
//! - Method (depth 2)
//! - Block (depth 3+)
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements the `Binding` value object from the DDD model.

use crate::ast::TypeAnnotation;
use crate::source_analysis::Span;
use std::collections::HashMap;

/// Tracks variable definitions across nested scopes.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Stack of scope levels, each containing variable definitions.
    levels: Vec<ScopeLevel>,
}

#[derive(Debug, Clone)]
struct ScopeLevel {
    variables: HashMap<String, Binding>,
}

/// The kind of binding in a scope.
///
/// **DDD Context:** Semantic Analysis - Value Object
///
/// Distinguishes between different kinds of variable bindings to support
/// proper scoping and future type checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    /// Local variable (let binding, loop variable)
    Local,
    /// Method or block parameter
    Parameter,
    /// Instance field (self.field)
    InstanceField,
    /// Class-level binding
    ClassField,
}

/// A binding in a scope.
///
/// **DDD Context:** Semantic Analysis - Value Object
///
/// Represents a variable binding with its name, location, kind, and optional
/// type annotation. This value object is the foundation for gradual typing
/// and proper semantic analysis.
///
/// # Fields
///
/// - `name`: The variable name
/// - `defined_at`: Source location where the binding was defined
/// - `depth`: Scope depth (0 = module, 1 = class, 2 = method, 3+ = blocks)
/// - `kind`: The kind of binding (local, parameter, field, etc.)
/// - `type_annotation`: Optional type annotation for gradual typing
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    pub name: String,
    pub defined_at: Span,
    pub depth: usize,
    pub kind: BindingKind,
    pub type_annotation: Option<TypeAnnotation>,
}

impl Scope {
    /// Creates a new scope tracker with module-level scope (depth 0).
    pub fn new() -> Self {
        Self {
            levels: vec![ScopeLevel {
                variables: HashMap::new(),
            }],
        }
    }

    /// Enters a new nested scope.
    pub fn push(&mut self) {
        self.levels.push(ScopeLevel {
            variables: HashMap::new(),
        });
    }

    /// Exits the current scope.
    ///
    /// Returns `true` if a scope was popped, `false` if already at module level (depth 0).
    /// This method will not panic; attempting to pop the module-level scope is a no-op.
    pub fn pop(&mut self) -> bool {
        if self.levels.len() > 1 {
            self.levels.pop();
            true
        } else {
            false
        }
    }

    /// Defines or redefines a variable in the current scope.
    ///
    /// If a variable with the same name already exists in the current scope,
    /// its existing binding in this scope will be overwritten.
    ///
    /// # Parameters
    ///
    /// - `name`: The variable name
    /// - `span`: Source location of the definition
    /// - `kind`: The kind of binding (local, parameter, field, etc.)
    ///
    /// # Panics
    /// Never panics. The `expect` is for internal invariant checking only.
    /// The `levels` vec is guaranteed to have at least one element (module scope).
    pub fn define(&mut self, name: &str, span: Span, kind: BindingKind) {
        let depth = self.current_depth();
        let binding = Binding {
            name: name.to_string(),
            defined_at: span,
            depth,
            kind,
            type_annotation: None,
        };

        // INVARIANT: levels always contains at least the module-level scope
        self.levels
            .last_mut()
            .expect("levels should never be empty")
            .variables
            .insert(name.to_string(), binding);
    }

    /// Looks up a variable by name, searching from innermost to outermost scope.
    ///
    /// Returns the variable information if found.
    pub fn lookup(&self, name: &str) -> Option<&Binding> {
        for level in self.levels.iter().rev() {
            if let Some(var_info) = level.variables.get(name) {
                return Some(var_info);
            }
        }
        None
    }

    /// Returns the current scope depth (0 = module, 1 = class, 2 = method, 3+ = blocks).
    pub fn current_depth(&self) -> usize {
        self.levels.len() - 1
    }

    /// Returns true if the variable is captured from an outer scope.
    ///
    /// A variable is considered captured if it was defined at a lower depth
    /// than the current scope.
    pub fn is_captured(&self, name: &str) -> bool {
        self.lookup(name)
            .is_some_and(|v| v.depth < self.current_depth())
    }

    /// Returns an iterator over all variables in the current scope.
    ///
    /// This only returns variables defined in the current scope level,
    /// not variables from outer scopes.
    ///
    /// # Panics
    /// Never panics. The `expect` is for internal invariant checking only.
    /// The `levels` vec is guaranteed to have at least one element (module scope).
    pub fn current_scope_vars(&self) -> impl Iterator<Item = (&str, &Binding)> {
        self.levels
            .last()
            .expect("levels should never be empty")
            .variables
            .iter()
            .map(|(name, info)| (name.as_str(), info))
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn new_scope_starts_at_module_depth() {
        let scope = Scope::new();
        assert_eq!(scope.current_depth(), 0);
    }

    #[test]
    fn push_increments_depth() {
        let mut scope = Scope::new();
        assert_eq!(scope.current_depth(), 0);

        scope.push(); // class
        assert_eq!(scope.current_depth(), 1);

        scope.push(); // method
        assert_eq!(scope.current_depth(), 2);

        scope.push(); // block
        assert_eq!(scope.current_depth(), 3);
    }

    #[test]
    fn pop_decrements_depth() {
        let mut scope = Scope::new();
        scope.push();
        scope.push();
        assert_eq!(scope.current_depth(), 2);

        assert!(scope.pop()); // Returns true when successful
        assert_eq!(scope.current_depth(), 1);

        assert!(scope.pop()); // Returns true when successful
        assert_eq!(scope.current_depth(), 0);

        assert!(!scope.pop()); // Returns false at module level
        assert_eq!(scope.current_depth(), 0);
    }

    #[test]
    fn pop_returns_false_at_module_level() {
        let mut scope = Scope::new();
        assert_eq!(scope.current_depth(), 0);

        // Attempting to pop at module level returns false (no-op)
        assert!(!scope.pop());
        assert_eq!(scope.current_depth(), 0); // Still at module level
    }

    #[test]
    fn define_adds_variable_to_current_scope() {
        let mut scope = Scope::new();
        scope.define("x", test_span(), BindingKind::Local);

        let var = scope.lookup("x").unwrap();
        assert_eq!(var.depth, 0);
    }

    #[test]
    fn lookup_finds_variable_in_current_scope() {
        let mut scope = Scope::new();
        scope.define("x", test_span(), BindingKind::Local);

        assert!(scope.lookup("x").is_some());
        assert!(scope.lookup("y").is_none());
    }

    #[test]
    fn lookup_searches_outer_scopes() {
        let mut scope = Scope::new();
        scope.define("outer", test_span(), BindingKind::Local);

        scope.push();
        scope.define("inner", test_span(), BindingKind::Local);

        // Inner scope can see both variables
        assert!(scope.lookup("outer").is_some());
        assert!(scope.lookup("inner").is_some());
    }

    #[test]
    fn lookup_finds_innermost_shadowing_variable() {
        let mut scope = Scope::new();
        scope.define("x", Span::new(0, 1), BindingKind::Local);

        scope.push();
        scope.define("x", Span::new(10, 11), BindingKind::Local);

        let var = scope.lookup("x").unwrap();
        assert_eq!(var.depth, 1); // Inner definition
        assert_eq!(var.defined_at.start(), 10);
    }

    #[test]
    fn is_captured_returns_false_for_local_variable() {
        let mut scope = Scope::new();
        scope.push(); // method level

        scope.define("local", test_span(), BindingKind::Local);
        assert!(!scope.is_captured("local"));
    }

    #[test]
    fn is_captured_returns_true_for_outer_variable() {
        let mut scope = Scope::new();
        scope.define("outer", test_span(), BindingKind::Local);

        scope.push(); // class
        scope.push(); // method

        assert!(scope.is_captured("outer"));
    }

    #[test]
    fn is_captured_returns_false_for_undefined_variable() {
        let scope = Scope::new();
        assert!(!scope.is_captured("undefined"));
    }

    #[test]
    fn redefine_variable_in_same_scope() {
        let mut scope = Scope::new();
        scope.define("x", Span::new(0, 1), BindingKind::Local);

        // Redefine in same scope with different span
        scope.define("x", Span::new(10, 11), BindingKind::Local);

        let var = scope.lookup("x").unwrap();
        assert_eq!(var.depth, 0);
        assert_eq!(var.defined_at.start(), 10); // Uses most recent definition
    }

    #[test]
    fn scope_levels_track_correct_depth() {
        let mut scope = Scope::new();

        // Module level (0)
        scope.define("module_var", test_span(), BindingKind::Local);
        assert_eq!(scope.lookup("module_var").unwrap().depth, 0);

        // Class level (1)
        scope.push();
        scope.define("class_var", test_span(), BindingKind::ClassField);
        assert_eq!(scope.lookup("class_var").unwrap().depth, 1);

        // Method level (2)
        scope.push();
        scope.define("method_var", test_span(), BindingKind::Parameter);
        assert_eq!(scope.lookup("method_var").unwrap().depth, 2);

        // Block level (3)
        scope.push();
        scope.define("block_var", test_span(), BindingKind::Local);
        assert_eq!(scope.lookup("block_var").unwrap().depth, 3);

        // All variables are visible from innermost scope
        assert!(scope.lookup("module_var").is_some());
        assert!(scope.lookup("class_var").is_some());
        assert!(scope.lookup("method_var").is_some());
        assert!(scope.lookup("block_var").is_some());

        // Captured variable checks
        assert!(scope.is_captured("module_var"));
        assert!(scope.is_captured("class_var"));
        assert!(scope.is_captured("method_var"));
        assert!(!scope.is_captured("block_var")); // Local to current scope
    }

    #[test]
    fn current_scope_vars_returns_only_current_level() {
        let mut scope = Scope::new();
        scope.define("outer", test_span(), BindingKind::Local);

        scope.push();
        scope.define("inner1", test_span(), BindingKind::Local);
        scope.define("inner2", test_span(), BindingKind::Local);

        // Should only return variables from current (inner) scope
        let vars: Vec<&str> = scope.current_scope_vars().map(|(name, _)| name).collect();
        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&"inner1"));
        assert!(vars.contains(&"inner2"));
        assert!(!vars.contains(&"outer")); // From parent scope
    }

    #[test]
    fn handles_empty_variable_name() {
        let mut scope = Scope::new();
        scope.define("", test_span(), BindingKind::Local);
        scope.define("valid", test_span(), BindingKind::Local);

        // Empty names are allowed (parser's responsibility to reject)
        assert!(scope.lookup("").is_some());
        assert!(scope.lookup("valid").is_some());
        assert_eq!(scope.lookup("").unwrap().depth, 0);
    }

    #[test]
    fn iterator_lifetime_is_correct() {
        let mut scope = Scope::new();
        scope.define("x", test_span(), BindingKind::Local);
        scope.define("y", test_span(), BindingKind::Local);

        // Iterator should be independent of scope lifetime
        let iter = scope.current_scope_vars();
        let names: Vec<&str> = iter.map(|(name, _)| name).collect();

        // Can still use scope after iterator is collected
        assert_eq!(scope.current_depth(), 0);
        assert_eq!(names.len(), 2);
        assert!(names.contains(&"x"));
        assert!(names.contains(&"y"));
    }

    #[test]
    fn binding_kind_is_preserved() {
        let mut scope = Scope::new();
        scope.define("param", test_span(), BindingKind::Parameter);
        scope.define("local", test_span(), BindingKind::Local);
        scope.define("field", test_span(), BindingKind::InstanceField);

        assert_eq!(scope.lookup("param").unwrap().kind, BindingKind::Parameter);
        assert_eq!(scope.lookup("local").unwrap().kind, BindingKind::Local);
        assert_eq!(
            scope.lookup("field").unwrap().kind,
            BindingKind::InstanceField
        );
    }
}

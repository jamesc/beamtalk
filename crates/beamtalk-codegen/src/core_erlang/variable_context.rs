// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Variable binding and scope management.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module provides the [`VariableContext`] aggregate which manages:
//!
//! - **Variable scopes**: Nested scopes for block parameters, method parameters, and local variables
//! - **Variable generation**: Fresh, unique Core Erlang variable names
//! - **Name conversion**: Beamtalk identifiers to Core Erlang variables
//!
//! # Scope Stack
//!
//! Variables are resolved by searching from innermost to outermost scope:
//!
//! ```text
//! Scope 0 (outermost): method parameters
//! Scope 1: block parameters
//! Scope 2 (innermost): nested block parameters
//! ```
//!
//! Inner scopes shadow outer scopes.

use std::collections::HashMap;
use std::fmt::Write;

/// The character every compiler temporary from [`VariableContext::fresh_var`]
/// carries, and no user-derived Core Erlang variable can (BT-4140).
///
/// Core Erlang admits `@` in variable names, but a Beamtalk identifier is
/// ASCII alphanumerics and `_` only (the lexer's identifier rule), and
/// [`VariableContext::to_core_var`] and `util::versioned_var` add only a
/// capital letter or digits. So a temp can never spell a user binding, however
/// the user names it. Pinned by `tests::fresh_temp_namespace`.
pub(super) const TEMP_MARKER: char = '@';

/// Whether `text` (rendered Core Erlang) references variable `var` as a whole
/// token, not as part of a longer name. A name character is anything a Core
/// variable this compiler emits can contain: ASCII alphanumerics, `_`, and
/// [`TEMP_MARKER`]. A match inside a string or atom literal still counts.
pub(super) fn text_mentions_var(text: &str, var: &str) -> bool {
    let is_name_char = |c: char| c.is_ascii_alphanumeric() || c == '_' || c == TEMP_MARKER;
    text.match_indices(var).any(|(at, _)| {
        let before = text[..at].chars().next_back();
        let after = text[at + var.len()..].chars().next();
        !before.is_some_and(is_name_char) && !after.is_some_and(is_name_char)
    })
}

/// Variable binding and scope management for code generation.
///
/// This aggregate encapsulates all variable-related state and operations,
/// providing a clear domain boundary for scope management.
#[derive(Debug)]
pub(super) struct VariableContext {
    /// Counter for generating unique variable names.
    var_counter: usize,
    /// Stack of variable binding scopes.
    /// Each scope maps Beamtalk identifiers to Core Erlang variable names.
    scopes: Vec<HashMap<String, String>>,
}

impl VariableContext {
    /// Creates a new variable context with an empty global scope.
    pub(super) fn new() -> Self {
        Self {
            var_counter: 0,
            scopes: vec![HashMap::new()],
        }
    }

    /// Pushes a new scope for variable bindings.
    ///
    /// Call this when entering a block, method body, or any nested context
    /// that introduces new parameter bindings.
    pub(super) fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pops the current scope, discarding its bindings.
    ///
    /// Call this when exiting a block or method body. The outermost scope
    /// (scope 0) cannot be popped.
    pub(super) fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Looks up a variable binding in the current scope stack.
    ///
    /// Searches from innermost to outermost scope, returning the first match.
    /// Returns `None` if the variable is not bound in any scope.
    pub(super) fn lookup(&self, name: &str) -> Option<&String> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(var_name) = scope.get(name) {
                return Some(var_name);
            }
        }
        None
    }

    /// Binds a Beamtalk identifier to a Core Erlang variable in the current scope.
    ///
    /// This establishes a mapping from the Beamtalk name (e.g., `"counter"`)
    /// to a Core Erlang variable (e.g., `"Counter"`).
    ///
    /// If the name is already bound in the current scope, the binding is replaced.
    pub(super) fn bind(&mut self, name: &str, core_var: &str) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name.to_string(), core_var.to_string());
        }
    }

    /// Generates a fresh, unique variable name with the given base.
    ///
    /// The result is `_` + the base with underscores removed + [`TEMP_MARKER`]
    /// + a counter:
    ///
    /// - `fresh_var("temp")` → `"_temp@1"`, `"_temp@2"`, etc.
    /// - `fresh_var("my_var")` → `"_myvar@3"`, `"_myvar@4"`, etc.
    ///
    /// Guarantees:
    ///
    /// - **Unique among temps:** the text after the last [`TEMP_MARKER`] is
    ///   the counter, so two calls never return the same name whatever their
    ///   bases (without the separator, base `a1` at counter 1 and base `a` at
    ///   counter 11 both spelled `_a11`).
    /// - **Disjoint from user bindings:** no name derived from a Beamtalk
    ///   identifier contains [`TEMP_MARKER`], so a temp never aliases a user
    ///   variable — including `_`-prefixed ones, which
    ///   [`Self::to_core_var`] maps into Core unchanged.
    ///
    /// It does not guard hand-written internal names (`State`, `Self`, …);
    /// those are outside this allocator.
    pub(super) fn fresh_var(&mut self, base: &str) -> String {
        self.var_counter += 1;
        let counter = self.var_counter;
        // Pre-allocate: underscore + base (minus underscores) + marker + counter digits.
        let mut result = String::with_capacity(2 + base.len() + 4);
        result.push('_');
        for ch in base.chars() {
            if ch != '_' {
                result.push(ch);
            }
        }
        result.push(TEMP_MARKER);
        // Write the counter directly instead of using format!.
        let _ = write!(result, "{counter}");
        result
    }

    /// Converts a Beamtalk identifier to a Core Erlang variable name.
    ///
    /// Core Erlang variables must start with an uppercase letter or underscore.
    /// This function:
    ///
    /// - Capitalizes the first letter: `"counter"` → `"Counter"`
    /// - Preserves underscores: `"my_var"` → `"My_var"`
    /// - Handles edge cases: empty string → `"_"`
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let ctx = VariableContext::new();
    /// assert_eq!(VariableContext::to_core_var("counter"), "Counter");
    /// assert_eq!(VariableContext::to_core_var("my_var"), "My_var");
    /// assert_eq!(VariableContext::to_core_var("_private"), "_private");
    /// ```
    pub(super) fn to_core_var(name: &str) -> String {
        if name.is_empty() {
            return "_".to_string();
        }

        let mut chars = name.chars();
        let first = chars.next().unwrap();

        if first.is_uppercase() || first == '_' {
            // Already valid Core Erlang variable
            name.to_string()
        } else {
            // Capitalize first letter
            let mut result = String::with_capacity(name.len());
            result.push(first.to_uppercase().next().unwrap());
            result.push_str(chars.as_str());
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_context_has_global_scope() {
        let ctx = VariableContext::new();
        assert_eq!(ctx.scopes.len(), 1);
    }

    #[test]
    fn test_push_pop_scope() {
        let mut ctx = VariableContext::new();
        assert_eq!(ctx.scopes.len(), 1);

        ctx.push_scope();
        assert_eq!(ctx.scopes.len(), 2);

        ctx.push_scope();
        assert_eq!(ctx.scopes.len(), 3);

        ctx.pop_scope();
        assert_eq!(ctx.scopes.len(), 2);

        ctx.pop_scope();
        assert_eq!(ctx.scopes.len(), 1);

        // Cannot pop global scope
        ctx.pop_scope();
        assert_eq!(ctx.scopes.len(), 1);
    }

    #[test]
    fn test_bind_and_lookup() {
        let mut ctx = VariableContext::new();

        ctx.bind("foo", "Foo");
        assert_eq!(ctx.lookup("foo"), Some(&"Foo".to_string()));
        assert_eq!(ctx.lookup("bar"), None);
    }

    #[test]
    fn test_scope_shadowing() {
        let mut ctx = VariableContext::new();

        ctx.bind("x", "X1");
        assert_eq!(ctx.lookup("x"), Some(&"X1".to_string()));

        ctx.push_scope();
        ctx.bind("x", "X2");
        assert_eq!(ctx.lookup("x"), Some(&"X2".to_string()));

        ctx.pop_scope();
        assert_eq!(ctx.lookup("x"), Some(&"X1".to_string()));
    }

    #[test]
    fn test_fresh_var_generates_unique_names() {
        let mut ctx = VariableContext::new();

        let var1 = ctx.fresh_var("temp");
        let var2 = ctx.fresh_var("temp");
        let var3 = ctx.fresh_var("loop");

        assert_eq!(var1, "_temp@1");
        assert_eq!(var2, "_temp@2");
        assert_eq!(var3, "_loop@3");
    }

    #[test]
    fn test_fresh_var_unique_across_bases_ending_in_digits() {
        // Base `a1` at counter 1 and base `a` at counter 11 used to both
        // spell `_a11`; the marker separates base from counter.
        let mut ctx = VariableContext::new();
        let first = ctx.fresh_var("a1");
        for _ in 2..11 {
            ctx.fresh_var("x");
        }
        let eleventh = ctx.fresh_var("a");
        assert_eq!(first, "_a1@1");
        assert_eq!(eleventh, "_a@11");
        assert_ne!(first, eleventh);
    }

    #[test]
    fn test_fresh_var_removes_underscores() {
        let mut ctx = VariableContext::new();
        let var = ctx.fresh_var("my_var");
        assert_eq!(var, "_myvar@1");
    }

    #[test]
    fn test_to_core_var_capitalizes() {
        assert_eq!(VariableContext::to_core_var("counter"), "Counter");
        assert_eq!(VariableContext::to_core_var("my_var"), "My_var");
        assert_eq!(VariableContext::to_core_var("x"), "X");
    }

    #[test]
    fn test_to_core_var_preserves_valid_names() {
        assert_eq!(VariableContext::to_core_var("Counter"), "Counter");
        assert_eq!(VariableContext::to_core_var("_private"), "_private");
    }

    #[test]
    fn test_to_core_var_handles_empty_string() {
        assert_eq!(VariableContext::to_core_var(""), "_");
    }
}

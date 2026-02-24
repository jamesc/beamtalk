// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Utility functions for Core Erlang code generation.
//!
//! This module provides helper functions for:
//! - Indentation management
//! - Variable name generation (fresh variables, temp variables)
//! - State variable threading (State, State1, State2, ...)
//! - Name conversions (class names, module names)
//! - Class identity (DDD Value Object bundling module + class names)

use super::document::{Document, join};
use super::{CoreErlangGenerator, Result};
use crate::ast::{ClassDefinition, Expression};
use crate::docvec;

/// BT-745: Generate a `'beamtalk_class' = [{...}]` attribute fragment for the
/// module attributes section. Returns `Document::Nil` when classes is empty.
pub(super) fn beamtalk_class_attribute(classes: &[ClassDefinition]) -> Document<'static> {
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries = classes.iter().map(|c| {
        docvec![
            "{'",
            Document::String(c.name.name.to_string()),
            "', '",
            Document::String(c.superclass_name().to_string()),
            "'}"
        ]
    });
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]"
    ]
}

/// Value Object: A class's compile-time identity.
///
/// **DDD Context:** Code Generation
///
/// Holds the user-facing class name (from the AST class definition).
/// This decouples class identity from the Erlang module name, which may
/// differ for stdlib classes (e.g., module `bt@stdlib@string` → class `String`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ClassIdentity {
    class_name: String,
    /// BT-403: Whether this class is sealed (no subclasses allowed).
    /// Enables codegen optimizations: direct dispatch for self-sends.
    is_sealed: bool,
    /// BT-403: Whether this class is abstract (cannot be instantiated).
    /// Enables codegen optimization: reduced `gen_server` scaffolding.
    is_abstract: bool,
}

impl ClassIdentity {
    /// Create from an AST class name.
    pub fn new(class_name: &str) -> Self {
        Self {
            class_name: class_name.to_string(),
            is_sealed: false,
            is_abstract: false,
        }
    }

    /// Create from an AST class definition with sealed/abstract flags.
    pub fn from_class_def(class_name: &str, is_sealed: bool, is_abstract: bool) -> Self {
        Self {
            class_name: class_name.to_string(),
            is_sealed,
            is_abstract,
        }
    }

    /// The user-facing class name (CamelCase).
    pub fn class_name(&self) -> &str {
        &self.class_name
    }

    /// Whether the class is sealed (BT-403).
    pub fn is_sealed(&self) -> bool {
        self.is_sealed
    }
}

impl CoreErlangGenerator {
    /// Captures the output of `generate_expression` as a `String`.
    ///
    /// ADR 0018: Renders the expression Document to a string. Used by code
    /// that needs string interpolation or direct string manipulation
    /// (e.g., cascade generation, value type default values).
    pub(super) fn capture_expression(&mut self, expr: &Expression) -> Result<String> {
        let doc = self.generate_expression(expr)?;
        Ok(doc.to_pretty_string())
    }

    /// Returns the expression as a `Document` for direct composition via `docvec!`.
    ///
    /// ADR 0018: Simple forwarding to `generate_expression`.
    pub(super) fn expression_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<super::document::Document<'static>> {
        self.generate_expression(expr)
    }

    /// Generates a fresh variable name and binds it in the current scope.
    ///
    /// Use this for user-visible bindings (block parameters, assignments, etc.)
    /// where the name should be looked up later via `lookup_var`.
    pub(super) fn fresh_var(&mut self, base: &str) -> String {
        let var_name = self.var_context.fresh_var(base);
        // Bind it in the current scope
        self.var_context.bind(base, &var_name);
        var_name
    }

    /// Generates a fresh temporary variable name WITHOUT binding it in scope.
    ///
    /// Use this for internal codegen temporaries (loop variables, function bindings,
    /// etc.) that should never shadow or be confused with user identifiers.
    pub(super) fn fresh_temp_var(&mut self, base: &str) -> String {
        self.var_context.fresh_var(base)
    }

    /// Converts a Beamtalk identifier to a valid Core Erlang variable name.
    ///
    /// Core Erlang variables must start with an uppercase letter or underscore.
    /// This function capitalizes the first letter of the identifier.
    pub(super) fn to_core_erlang_var(name: &str) -> String {
        super::variable_context::VariableContext::to_core_var(name)
    }

    /// Returns the class name for the currently compiled class.
    ///
    /// Prefers the AST-derived class identity when available (set during class
    /// compilation). Falls back to deriving from the module name for backward
    /// compatibility with compilation units that don't set class identity.
    ///
    /// # Examples
    ///
    /// - Module `"counter"` (no class identity) → `"Counter"`
    /// - Module `"bt@stdlib@string"` with class identity `"String"` → `"String"`
    pub(super) fn class_name(&self) -> String {
        if let Some(ref identity) = self.class_identity {
            return identity.class_name().to_string();
        }
        // Fall back to deriving from module name (snake_case → CamelCase)
        self.module_name
            .split('_')
            .map(|s| {
                let mut chars = s.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                }
            })
            .collect()
    }

    /// Whether the current class is sealed (BT-403).
    pub(super) fn is_class_sealed(&self) -> bool {
        self.class_identity
            .as_ref()
            .is_some_and(ClassIdentity::is_sealed)
    }

    /// Build the `beamtalk_source` Core Erlang attribute fragment (BT-845/BT-860).
    ///
    /// Returns `, 'beamtalk_source' = ["<path>"]` when `source_path` is set,
    /// or `Document::Nil` for stdlib and ClassBuilder-created classes.
    pub(super) fn source_path_attr(&self) -> Document<'static> {
        match &self.source_path {
            Some(path) => {
                let escaped = path.replace('\\', "\\\\").replace('"', "\\\"");
                Document::String(format!(", 'beamtalk_source' = [\"{escaped}\"]"))
            }
            None => Document::Nil,
        }
    }
}

/// Converts class name (`CamelCase`) to module name (`snake_case`).
///
/// Delegates to [`crate::ast::to_module_name`] (Shared Kernel).
///
/// Visibility: `pub` to allow usage in IDE queries (hover, completion, etc.)
pub fn to_module_name(class_name: &str) -> String {
    crate::ast::to_module_name(class_name)
}

/// Extracts the user package prefix from a workspace-qualified module name (BT-794).
///
/// Given `bt@{package}@{rest}`, returns `Some("bt@{package}@")`.
/// Returns `None` for stdlib modules (`bt@stdlib@...`), unprefixed names, or
/// names without a package segment.
///
/// # Limitations
///
/// This function intentionally returns only the top-level package segment
/// (`bt@{package}@`), discarding any subdirectory path components. For example,
/// `bt@sicp@scheme@eval` returns `bt@sicp@` rather than `bt@sicp@scheme@`.
///
/// Callers such as `compiled_module_name` use this prefix to construct module
/// names for referenced classes. This means cross-module references within a
/// package only produce correct names when the referenced class lives at the
/// package root (e.g. `bt@{package}@{class}`). Classes nested in subdirectories
/// (e.g. `bt@{package}@{subdir}@{class}`) cannot be resolved by class name alone
/// and are not currently supported for inter-class dispatch.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(user_package_prefix("bt@bank@account"), Some("bt@bank@".into()));
/// // Subdirectory segments are stripped — `scheme@` is not preserved:
/// assert_eq!(user_package_prefix("bt@sicp@scheme@eval"), Some("bt@sicp@".into()));
/// assert_eq!(user_package_prefix("bt@stdlib@integer"), None);
/// assert_eq!(user_package_prefix("counter"), None);
/// assert_eq!(user_package_prefix("bt@counter"), None);
/// ```
pub(super) fn user_package_prefix(module_name: &str) -> Option<String> {
    let rest = module_name.strip_prefix("bt@")?;
    let (pkg, suffix) = rest.split_once('@')?;
    if pkg == "stdlib" || suffix.is_empty() {
        return None;
    }
    Some(format!("bt@{pkg}@"))
}

/// Returns true if `module_name` corresponds to the compiled form of `class_name`.
///
/// ADR 0016/0026: Module names may be prefixed with `bt@` (user code),
/// `bt@stdlib@` (stdlib), `bt@{package}@` (package mode), or unprefixed (legacy/tests).
pub(super) fn module_matches_class(module_name: &str, class_name: &str) -> bool {
    let snake = to_module_name(class_name);
    module_name == snake
        || module_name == format!("bt@{snake}")
        || module_name == format!("bt@stdlib@{snake}")
        || module_name
            .strip_prefix("bt@")
            .and_then(|rest| rest.rsplit_once('@'))
            .is_some_and(|(_, suffix)| suffix == snake)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_matches_class_unprefixed() {
        assert!(module_matches_class("counter", "Counter"));
    }

    #[test]
    fn test_module_matches_class_bt_prefix() {
        assert!(module_matches_class("bt@counter", "Counter"));
    }

    #[test]
    fn test_module_matches_class_stdlib_prefix() {
        assert!(module_matches_class("bt@stdlib@integer", "Integer"));
    }

    #[test]
    fn test_module_matches_class_package_prefix() {
        assert!(module_matches_class("bt@my_app@counter", "Counter"));
    }

    #[test]
    fn test_module_matches_class_package_multi_word() {
        assert!(module_matches_class("bt@my_app@my_counter", "MyCounter"));
    }

    #[test]
    fn test_module_matches_class_package_subdirectory() {
        // bt@my_app@util@math should match class Math (rsplit_once on last @)
        assert!(module_matches_class("bt@my_app@util@math", "Math"));
    }

    #[test]
    fn test_module_matches_class_no_match() {
        assert!(!module_matches_class("bt@other", "Counter"));
    }

    #[test]
    fn test_user_package_prefix_package_mode() {
        assert_eq!(
            user_package_prefix("bt@bank@account"),
            Some("bt@bank@".into())
        );
    }

    #[test]
    fn test_user_package_prefix_deep_path() {
        assert_eq!(
            user_package_prefix("bt@sicp@scheme@eval"),
            Some("bt@sicp@".into())
        );
    }

    #[test]
    fn test_user_package_prefix_stdlib() {
        assert_eq!(user_package_prefix("bt@stdlib@integer"), None);
    }

    #[test]
    fn test_user_package_prefix_unprefixed() {
        assert_eq!(user_package_prefix("counter"), None);
    }

    #[test]
    fn test_user_package_prefix_bt_only() {
        assert_eq!(user_package_prefix("bt@counter"), None);
    }
}

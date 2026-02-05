// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core Erlang value objects for type-safe code generation.
//!
//! # DDD: Value Objects
//!
//! This module provides value objects that encapsulate Core Erlang primitives
//! with validation and proper formatting. Value objects are immutable and
//! compared by value, not identity.
//!
//! ## `ErlangAtom`
//!
//! Represents a Core Erlang atom. Atoms are always quoted in Core Erlang output:
//!
//! ```
//! use beamtalk_core::codegen::core_erlang::erlang_types::ErlangAtom;
//!
//! let atom = ErlangAtom::new("increment");
//! assert_eq!(atom.to_string(), "'increment'");
//!
//! let keyword = ErlangAtom::new("at:put:");
//! assert_eq!(keyword.to_string(), "'at:put:'");
//! ```
//!
//! ## `ErlangVar`
//!
//! Represents a Core Erlang variable. Variables must start with uppercase or underscore:
//!
//! ```
//! use beamtalk_core::codegen::core_erlang::erlang_types::ErlangVar;
//!
//! let var = ErlangVar::new("Counter");
//! assert_eq!(var.to_string(), "Counter");
//!
//! let temp = ErlangVar::from_beamtalk_ident("myVar");
//! assert_eq!(temp.to_string(), "MyVar");
//! ```
//!
//! ## `ModuleName`
//!
//! Represents a Beamtalk/Erlang module name with proper naming conventions:
//!
//! ```
//! use beamtalk_core::codegen::core_erlang::erlang_types::ModuleName;
//!
//! // Instance module for a class
//! let instance = ModuleName::instance("Counter");
//! assert_eq!(instance.to_string(), "'beamtalk_counter'");
//!
//! // Class module (metadata)
//! let class = ModuleName::class("Counter");
//! assert_eq!(class.to_string(), "'beamtalk_counter_class'");
//!
//! // Raw module (stdlib)
//! let stdlib = ModuleName::raw("erlang");
//! assert_eq!(stdlib.to_string(), "'erlang'");
//! ```

use std::fmt;

use super::util::to_module_name;

/// A Core Erlang atom value object.
///
/// Atoms are symbolic constants in Erlang. In Core Erlang, they are always
/// written with single quotes: `'atom_name'`.
///
/// # Invariants
///
/// - The name is never empty
/// - Special characters in the name are escaped when displayed
///
/// # Example
///
/// ```
/// use beamtalk_core::codegen::core_erlang::erlang_types::ErlangAtom;
///
/// let atom = ErlangAtom::new("hello");
/// assert_eq!(format!("{atom}"), "'hello'");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErlangAtom {
    name: String,
}

impl ErlangAtom {
    /// Creates a new atom with the given name.
    ///
    /// The name should not include surrounding quotes - those are added
    /// automatically when the atom is formatted.
    #[must_use]
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    /// Returns the raw atom name without quotes.
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Creates an atom from a message selector name.
    ///
    /// This is a convenience constructor for the common case of converting
    /// Beamtalk selectors to Erlang atoms.
    #[must_use]
    pub fn from_selector(selector: &crate::ast::MessageSelector) -> Self {
        Self::new(selector.to_erlang_atom())
    }
}

impl fmt::Display for ErlangAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Escape special characters in atom names
        write!(f, "'")?;
        for c in self.name.chars() {
            match c {
                '\'' => write!(f, "\\'")?,
                '\\' => write!(f, "\\\\")?,
                '\n' => write!(f, "\\n")?,
                '\r' => write!(f, "\\r")?,
                '\t' => write!(f, "\\t")?,
                _ => write!(f, "{c}")?,
            }
        }
        write!(f, "'")
    }
}

/// A Core Erlang variable value object.
///
/// Variables in Core Erlang must start with an uppercase letter or underscore.
/// This type ensures that invariant and provides conversion from Beamtalk identifiers.
///
/// # Invariants
///
/// - The name starts with an uppercase letter or underscore
/// - The name is never empty
///
/// # Example
///
/// ```
/// use beamtalk_core::codegen::core_erlang::erlang_types::ErlangVar;
///
/// let var = ErlangVar::new("Counter");
/// assert_eq!(format!("{var}"), "Counter");
///
/// // Convert from Beamtalk identifier (auto-capitalizes)
/// let var = ErlangVar::from_beamtalk_ident("counter");
/// assert_eq!(format!("{var}"), "Counter");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErlangVar {
    name: String,
}

impl ErlangVar {
    /// Creates a new variable with the given name.
    ///
    /// # Panics
    ///
    /// Panics in debug mode if the name doesn't start with uppercase or underscore.
    #[must_use]
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        debug_assert!(
            name.starts_with(|c: char| c.is_ascii_uppercase() || c == '_'),
            "Core Erlang variable must start with uppercase or underscore: {name}"
        );
        Self { name }
    }

    /// Creates a variable from a Beamtalk identifier.
    ///
    /// Converts the identifier to a valid Core Erlang variable by capitalizing
    /// the first letter if necessary.
    ///
    /// - `"counter"` → `"Counter"`
    /// - `"_private"` → `"_private"`
    /// - `"MyVar"` → `"MyVar"`
    /// - `""` → `"_"`
    #[must_use]
    pub fn from_beamtalk_ident(name: &str) -> Self {
        let Some(first) = name.chars().next() else {
            return Self {
                name: "_".to_string(),
            };
        };

        if first.is_ascii_uppercase() || first == '_' {
            Self {
                name: name.to_string(),
            }
        } else {
            let mut result = String::with_capacity(name.len());
            result.push(first.to_ascii_uppercase());
            result.push_str(&name[first.len_utf8()..]);
            Self { name: result }
        }
    }

    /// Creates a fresh temporary variable with a unique suffix.
    ///
    /// The base name is sanitized (underscores removed) and prefixed with `_`
    /// to avoid conflicts with user-defined variables.
    ///
    /// - `fresh("temp", 1)` → `"_temp1"`
    /// - `fresh("my_var", 42)` → `"_myvar42"`
    #[must_use]
    pub fn fresh(base: &str, counter: usize) -> Self {
        Self {
            name: format!("_{}{}", base.replace('_', ""), counter),
        }
    }

    /// Returns the variable name.
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl fmt::Display for ErlangVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A Beamtalk module name value object.
///
/// Encapsulates the naming convention for Beamtalk modules:
/// - Instance modules: `beamtalk_<class>` (e.g., `beamtalk_counter`)
/// - Class modules: `beamtalk_<class>_class` (e.g., `beamtalk_counter_class`)
/// - Raw modules: Direct Erlang module names (e.g., `erlang`, `maps`)
///
/// # DDD: Value Object
///
/// `ModuleName` is a value object in the Code Generation bounded context.
/// It encapsulates the Two-Module Pattern from the Flavors architecture
/// where each Beamtalk class generates two Erlang modules.
///
/// # Invariants
///
/// - Module names are valid Erlang atoms (lowercase, alphanumeric + underscore)
/// - Instance and class modules always have `beamtalk_` prefix
/// - Class names are converted from `CamelCase` to `snake_case`
///
/// # Example
///
/// ```
/// use beamtalk_core::codegen::core_erlang::erlang_types::ModuleName;
///
/// let instance = ModuleName::instance("Counter");
/// assert_eq!(instance.name(), "beamtalk_counter");
/// assert_eq!(instance.to_string(), "'beamtalk_counter'");
///
/// let class = ModuleName::class("Counter");
/// assert_eq!(class.name(), "beamtalk_counter_class");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    name: String,
}

impl ModuleName {
    /// Creates a module name for a Beamtalk class instance module.
    ///
    /// Converts the class name from `CamelCase` to `snake_case` and adds
    /// the `beamtalk_` prefix.
    ///
    /// - `"Counter"` → `"beamtalk_counter"`
    /// - `"HttpRouter"` → `"beamtalk_http_router"`
    #[must_use]
    pub fn instance(class_name: &str) -> Self {
        let snake = to_module_name(class_name);
        Self {
            name: format!("beamtalk_{snake}"),
        }
    }

    /// Creates a module name for a Beamtalk class metadata module.
    ///
    /// Like `instance()` but adds `_class` suffix for the class-level module.
    ///
    /// - `"Counter"` → `"beamtalk_counter_class"`
    /// - `"HttpRouter"` → `"beamtalk_http_router_class"`
    #[must_use]
    pub fn class(class_name: &str) -> Self {
        let snake = to_module_name(class_name);
        Self {
            name: format!("beamtalk_{snake}_class"),
        }
    }

    /// Creates a raw module name without any transformation.
    ///
    /// Use this for Erlang stdlib modules or other external modules.
    ///
    /// - `"erlang"` → `"erlang"`
    /// - `"gen_server"` → `"gen_server"`
    #[must_use]
    pub fn raw(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    /// Returns the raw module name without quotes.
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Converts this module name to an `ErlangAtom`.
    #[must_use]
    pub fn as_atom(&self) -> ErlangAtom {
        ErlangAtom::new(&self.name)
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Module names are atoms in Core Erlang
        write!(f, "'{}'", self.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{KeywordPart, MessageSelector};
    use crate::source_analysis::Span;

    // ErlangAtom tests

    #[test]
    fn atom_simple() {
        let atom = ErlangAtom::new("hello");
        assert_eq!(atom.name(), "hello");
        assert_eq!(atom.to_string(), "'hello'");
    }

    #[test]
    fn atom_with_colon() {
        let atom = ErlangAtom::new("at:put:");
        assert_eq!(atom.to_string(), "'at:put:'");
    }

    #[test]
    fn atom_with_special_chars() {
        let atom = ErlangAtom::new("it's");
        assert_eq!(atom.to_string(), "'it\\'s'");

        let atom = ErlangAtom::new("back\\slash");
        assert_eq!(atom.to_string(), "'back\\\\slash'");
    }

    #[test]
    fn atom_from_selector_unary() {
        let selector = MessageSelector::Unary("increment".into());
        let atom = ErlangAtom::from_selector(&selector);
        assert_eq!(atom.to_string(), "'increment'");
    }

    #[test]
    fn atom_from_selector_keyword() {
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::new(0, 3)),
            KeywordPart::new("put:", Span::new(5, 9)),
        ]);
        let atom = ErlangAtom::from_selector(&selector);
        assert_eq!(atom.to_string(), "'at:put:'");
    }

    #[test]
    fn atom_equality() {
        let a = ErlangAtom::new("test");
        let b = ErlangAtom::new("test");
        let c = ErlangAtom::new("other");
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    // ErlangVar tests

    #[test]
    fn var_uppercase() {
        let var = ErlangVar::new("Counter");
        assert_eq!(var.name(), "Counter");
        assert_eq!(var.to_string(), "Counter");
    }

    #[test]
    fn var_underscore() {
        let var = ErlangVar::new("_temp");
        assert_eq!(var.to_string(), "_temp");
    }

    #[test]
    fn var_from_beamtalk_lowercase() {
        let var = ErlangVar::from_beamtalk_ident("counter");
        assert_eq!(var.to_string(), "Counter");
    }

    #[test]
    fn var_from_beamtalk_already_uppercase() {
        let var = ErlangVar::from_beamtalk_ident("MyVar");
        assert_eq!(var.to_string(), "MyVar");
    }

    #[test]
    fn var_from_beamtalk_underscore() {
        let var = ErlangVar::from_beamtalk_ident("_private");
        assert_eq!(var.to_string(), "_private");
    }

    #[test]
    fn var_from_beamtalk_empty() {
        let var = ErlangVar::from_beamtalk_ident("");
        assert_eq!(var.to_string(), "_");
    }

    #[test]
    fn var_fresh() {
        let v1 = ErlangVar::fresh("temp", 1);
        assert_eq!(v1.to_string(), "_temp1");

        let v2 = ErlangVar::fresh("my_var", 42);
        assert_eq!(v2.to_string(), "_myvar42");
    }

    #[test]
    fn var_equality() {
        let a = ErlangVar::new("X");
        let b = ErlangVar::new("X");
        let c = ErlangVar::new("Y");
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    #[should_panic(expected = "Core Erlang variable must start with uppercase or underscore")]
    #[cfg(debug_assertions)]
    fn var_invalid_panics_in_debug() {
        let _ = ErlangVar::new("lowercase");
    }

    // ModuleName tests

    #[test]
    fn module_name_instance() {
        let m = ModuleName::instance("Counter");
        assert_eq!(m.name(), "beamtalk_counter");
        assert_eq!(m.to_string(), "'beamtalk_counter'");
    }

    #[test]
    fn module_name_instance_camel_case() {
        let m = ModuleName::instance("HttpRouter");
        assert_eq!(m.name(), "beamtalk_http_router");
    }

    #[test]
    fn module_name_class() {
        let m = ModuleName::class("Counter");
        assert_eq!(m.name(), "beamtalk_counter_class");
        assert_eq!(m.to_string(), "'beamtalk_counter_class'");
    }

    #[test]
    fn module_name_class_camel_case() {
        let m = ModuleName::class("MyCounterActor");
        assert_eq!(m.name(), "beamtalk_my_counter_actor_class");
    }

    #[test]
    fn module_name_raw() {
        let m = ModuleName::raw("erlang");
        assert_eq!(m.name(), "erlang");
        assert_eq!(m.to_string(), "'erlang'");
    }

    #[test]
    fn module_name_raw_gen_server() {
        let m = ModuleName::raw("gen_server");
        assert_eq!(m.name(), "gen_server");
    }

    #[test]
    fn module_name_as_atom() {
        let m = ModuleName::instance("Counter");
        let atom = m.as_atom();
        assert_eq!(atom.name(), "beamtalk_counter");
    }

    #[test]
    fn module_name_equality() {
        let a = ModuleName::instance("Counter");
        let b = ModuleName::instance("Counter");
        let c = ModuleName::class("Counter");
        assert_eq!(a, b);
        assert_ne!(a, c);
    }
}

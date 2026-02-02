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

use std::fmt;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{KeywordPart, MessageSelector};
    use crate::parse::Span;

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
}

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

use super::{CoreErlangGenerator, Result};
use crate::ast::Expression;

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
    /// Bridge: renders a `Document` tree and appends it to `self.output`.
    ///
    /// This enables incremental migration — functions that return `Document`
    /// can coexist with functions that write directly to `self.output`.
    /// The document is rendered starting at indent level 0; callers are
    /// responsible for embedding indentation in the document tree itself
    /// (e.g., via `nest()` or literal spaces).
    pub(super) fn write_document(&mut self, doc: &super::document::Document<'_>) {
        let rendered = doc.to_pretty_string();
        self.output.push_str(&rendered);
    }

    /// Captures the output of `generate_expression` as a `String`.
    ///
    /// ADR 0018 Phase 3: Now that `generate_expression` returns `Document`,
    /// this renders the document to a string. Retained for backward compatibility
    /// with code that needs string interpolation or direct string manipulation.
    pub(super) fn capture_expression(&mut self, expr: &Expression) -> Result<String> {
        let doc = self.generate_expression(expr)?;
        Ok(doc.to_pretty_string())
    }

    /// Returns the expression as a `Document` for direct composition via `docvec!`.
    ///
    /// ADR 0018 Phase 3: Now a simple forwarding to `generate_expression`.
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
}

/// Converts class name (`CamelCase`) to module name (`snake_case`).
///
/// This is the inverse of the `snake_case` → `CamelCase` derivation used as a
/// fallback in `class_name()`, and properly handles multi-word
/// class names like `MyCounterActor` -> `my_counter_actor`.
///
/// Note: Acronyms like `HTTPRouter` become `httprouter` (no underscores within acronyms).
///
/// Visibility: `pub` to allow usage in IDE queries (hover, completion, etc.)
pub fn to_module_name(class_name: &str) -> String {
    let mut result = String::new();
    let mut prev_was_lowercase = false;

    for ch in class_name.chars() {
        if ch.is_uppercase() {
            // Add underscore before uppercase if previous char was lowercase
            if prev_was_lowercase {
                result.push('_');
            }
            result.extend(ch.to_lowercase());
            prev_was_lowercase = false;
        } else {
            result.push(ch);
            prev_was_lowercase = ch.is_lowercase();
        }
    }

    result
}

/// Returns true if `module_name` corresponds to the compiled form of `class_name`.
///
/// ADR 0016: Module names may be prefixed with `bt@` (user code) or
/// `bt@stdlib@` (stdlib), or unprefixed (legacy/tests).
pub(super) fn module_matches_class(module_name: &str, class_name: &str) -> bool {
    let snake = to_module_name(class_name);
    module_name == snake
        || module_name == format!("bt@{snake}")
        || module_name == format!("bt@stdlib@{snake}")
}

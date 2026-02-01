// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Utility functions for Core Erlang code generation.
//!
//! This module provides helper functions for:
//! - Indentation management
//! - Variable name generation (fresh variables, temp variables)
//! - State variable threading (State, State1, State2, ...)
//! - Name conversions (class names, module names)

use super::{CoreErlangGenerator, Result};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Writes indentation to the output buffer.
    ///
    /// Each indentation level is 4 spaces.
    pub(super) fn write_indent(&mut self) -> Result<()> {
        for _ in 0..self.indent {
            write!(self.output, "    ")?;
        }
        Ok(())
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

    /// Returns the current state variable name for state threading.
    ///
    /// State threading uses incrementing variable names to simulate mutation:
    /// - Version 0: `State` (the original state passed to the method)
    /// - Version 1: `State1` (after first assignment)
    /// - Version 2: `State2` (after second assignment)
    /// - etc.
    pub(super) fn current_state_var(&self) -> String {
        if self.state_version == 0 {
            "State".to_string()
        } else {
            format!("State{}", self.state_version)
        }
    }

    /// Increments the state version and returns the new state variable name.
    ///
    /// Call this when generating a field assignment (`self.field := value`)
    /// to get the name for the new state after the update.
    pub(super) fn next_state_var(&mut self) -> String {
        self.state_version += 1;
        self.current_state_var()
    }

    /// Resets the state version to 0 at the start of each method.
    pub(super) fn reset_state_version(&mut self) {
        self.state_version = 0;
    }

    /// Converts a module name to a class name by capitalizing the first letter.
    ///
    /// Module names in Beamtalk follow Erlang conventions (lowercase atoms),
    /// while class names follow Smalltalk conventions (`UpperCamelCase`).
    ///
    /// # Examples
    ///
    /// - `"counter"` -> `"Counter"`
    /// - `"my_class"` -> `"My_class"`
    ///
    /// # Panics
    ///
    /// Panics if the module name is empty, as an empty module name represents an
    /// invalid state in the code generation pipeline.
    /// Converts module name (`snake_case`) to class name (`CamelCase`).
    pub(super) fn to_class_name(&self) -> String {
        // Convert snake_case to CamelCase
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

    /// Converts class name (`CamelCase`) to module name (`snake_case`).
    ///
    /// This is the inverse of `to_class_name` and properly handles multi-word
    /// class names like `MyCounterActor` -> `my_counter_actor`.
    ///
    /// Note: Acronyms like `HTTPRouter` become `httprouter` (no underscores within acronyms).
    pub(super) fn to_module_name(class_name: &str) -> String {
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
}

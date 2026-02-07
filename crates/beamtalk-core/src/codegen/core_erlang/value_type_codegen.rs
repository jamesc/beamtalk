// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for value type classes — plain Erlang
//! terms (maps) with no process. They are created with `new` and `new:`,
//! not `spawn`, and methods are synchronous functions operating on maps.

use super::util::ClassIdentity;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{ClassDefinition, MessageSelector, MethodDefinition, MethodKind, Module};
use std::fmt::Write;

use super::variable_context;

impl CoreErlangGenerator {
    /// Generates a value type module (BT-213).
    ///
    /// Value types are plain Erlang terms (maps/records) with no process.
    /// They are created with `new` and `new:`, not `spawn`.
    ///
    /// ## Generated Structure
    ///
    /// ```erlang
    /// module 'point' ['new'/0, 'new'/1, 'plus'/2, ...]
    ///   attributes []
    ///
    /// 'new'/0 = fun () ->
    ///     ~{'__class__' => 'Point', 'x' => 0, 'y' => 0}~
    ///
    /// 'new'/1 = fun (InitArgs) ->
    ///     DefaultState = call 'point':'new'(),
    ///     call 'maps':'merge'(DefaultState, InitArgs)
    ///
    /// 'plus'/2 = fun (Self, Other) ->
    ///     % Direct function call, no gen_server
    ///     NewX = call 'erlang':'+'(
    ///         call 'maps':'get'('x', Self),
    ///         call 'maps':'get'('x', Other)
    ///     ),
    ///     ...
    ///     ~{'__class__' => 'Point', 'x' => NewX, 'y' => NewY}~
    /// ```
    ///
    /// ## Key Differences from Actors
    ///
    /// - No `gen_server` behavior
    /// - No `spawn/0` or `spawn/1` - use `new/0` and `new/1` instead
    /// - Methods are synchronous functions operating on maps
    /// - No state threading (value types are immutable)
    /// - Methods return new instances rather than mutating
    pub(super) fn generate_value_type_module(&mut self, module: &Module) -> Result<()> {
        // BT-213: Set context to ValueType for this module
        self.context = CodeGenContext::ValueType;

        let class = module
            .classes
            .first()
            .ok_or_else(|| CodeGenError::Internal("Value type module has no class".to_string()))?;

        // Set class identity early — needed by all code paths including the
        // Beamtalk special case below, so that class_name() returns the AST
        // class name rather than deriving from the module name.
        self.class_identity = Some(ClassIdentity::new(&class.name.name));

        // BT-220: Special case for Beamtalk global class
        // The Beamtalk class provides system reflection via class methods, not instances
        if class.name.name.as_str() == "Beamtalk" {
            return self.generate_beamtalk_module(class);
        }

        // Check if the class explicitly defines new/new: methods
        // (e.g., Object.bt defines `new => @primitive basicNew`)
        // If so, skip auto-generating constructors to avoid duplicate definitions
        // Only check Primary methods — advice (before/after/around) shouldn't suppress auto-generation
        let has_explicit_new = class.methods.iter().any(|m| {
            m.kind == MethodKind::Primary
                && matches!(&m.selector, MessageSelector::Unary(name) if name.as_str() == "new")
        });
        let has_explicit_new_with = class
            .methods
            .iter()
            .any(|m| m.kind == MethodKind::Primary && m.selector.name() == "new:");

        // Collect method exports
        let mut exports = Vec::new();
        if !has_explicit_new {
            exports.push("'new'/0".to_string());
        }
        if !has_explicit_new_with {
            exports.push("'new'/1".to_string());
        }

        // Add instance method exports (each takes Self as first parameter)
        for method in &class.methods {
            // All methods in a class are instance methods in Beamtalk
            // MethodKind is about method combination (Primary, Before, After, Around)
            let arity = method.parameters.len() + 1; // +1 for Self parameter
            let mangled = method.selector.to_erlang_atom();
            exports.push(format!("'{mangled}'/{arity}"));
        }

        // Module header
        writeln!(
            self.output,
            "module '{}' [{}]",
            self.module_name,
            exports.join(", ")
        )?;
        writeln!(self.output, "  attributes []")?;
        writeln!(self.output)?;

        // Generate new/0 - creates instance with default field values
        if !has_explicit_new {
            self.generate_value_type_new(class)?;
            writeln!(self.output)?;
        }

        // Generate new/1 - creates instance with initialization arguments
        if !has_explicit_new_with {
            self.generate_value_type_new_with_args()?;
            writeln!(self.output)?;
        }

        // Generate instance methods as pure functions
        for method in &class.methods {
            self.generate_value_type_method(method, class)?;
            writeln!(self.output)?;
        }

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }

    /// Generates the `new/0` function for a value type.
    ///
    /// Creates an instance map with __class__ and default field values.
    fn generate_value_type_new(&mut self, class: &ClassDefinition) -> Result<()> {
        writeln!(self.output, "'new'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;

        // Generate map literal with __class__ and fields
        write!(self.output, "~{{'__class__' => '{}'", self.class_name())?;

        // Add each field with its default value
        for field in &class.state {
            write!(self.output, ", '{}' => ", field.name.name)?;
            if let Some(default_value) = &field.default_value {
                self.generate_expression(default_value)?;
            } else {
                write!(self.output, "'nil'")?;
            }
        }

        writeln!(self.output, "}}~")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `new/1` function for a value type.
    ///
    /// Merges initialization arguments with default values.
    fn generate_value_type_new_with_args(&mut self) -> Result<()> {
        writeln!(self.output, "'new'/1 = fun (InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let DefaultState = call '{}':'new'() in",
            self.module_name
        )?;
        self.write_indent()?;
        writeln!(self.output, "call 'maps':'merge'(DefaultState, InitArgs)")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates an instance method for a value type.
    ///
    /// Value type methods are pure functions that take Self as first parameter
    /// and return a new instance (immutable semantics).
    fn generate_value_type_method(
        &mut self,
        method: &MethodDefinition,
        _class: &ClassDefinition,
    ) -> Result<()> {
        let mangled = method.selector.to_erlang_atom();
        let arity = method.parameters.len() + 1; // +1 for Self

        // Function signature: 'methodName'/Arity = fun (Self, Param1, Param2, ...) ->
        write!(self.output, "'{mangled}'/{arity}= fun (Self")?;
        for param in &method.parameters {
            let core_var = variable_context::VariableContext::to_core_var(&param.name);
            write!(self.output, ", {core_var}")?;
        }
        writeln!(self.output, ") ->")?;

        self.indent += 1;

        // Bind parameters in scope
        self.push_scope();
        // BT-295: Track method params for @primitive codegen
        self.current_method_params.clear();
        for param in &method.parameters {
            let core_var = variable_context::VariableContext::to_core_var(&param.name);
            self.bind_var(&param.name, &core_var);
            self.current_method_params.push(core_var);
        }

        // Generate method body expressions
        // For value types, there's no state threading - they're immutable
        // TODO(BT-213): Consider adding immutable update syntax (e.g., withX: newX) in future
        // Currently, field assignments are rejected at codegen (see generate_field_assignment)
        // Field reads work via CodeGenContext routing to Self parameter
        for (i, expr) in method.body.iter().enumerate() {
            if i > 0 {
                self.write_indent()?;
            }
            self.generate_expression(expr)?;
            if i < method.body.len() - 1 {
                writeln!(self.output)?;
            }
        }

        self.pop_scope();
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }
}

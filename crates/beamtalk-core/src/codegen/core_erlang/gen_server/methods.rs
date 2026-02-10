// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method body code generation and class registration.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates method dispatch case clauses, method body with state threading
//! and reply tuples, and the `register_class/0` on-load function.

use super::super::{CodeGenContext, CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, ClassDefinition, Expression, MethodDefinition, MethodKind, Module};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates dispatch case clauses for all methods in a class definition.
    pub(in crate::codegen::core_erlang) fn generate_class_method_dispatches(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<()> {
        for method in &class.methods {
            // Only generate dispatch for primary methods for now
            if method.kind == MethodKind::Primary {
                self.generate_method_dispatch(method)?;
            }
        }
        Ok(())
    }

    /// Generates a single method dispatch case clause.
    pub(in crate::codegen::core_erlang) fn generate_method_dispatch(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<()> {
        // Reset state version at the start of each method
        self.reset_state_version();

        // Push a new scope for this method's parameter bindings
        self.push_scope();
        // BT-295: Clear method params (will be populated below if present)
        self.current_method_params.clear();

        let selector_name = method.selector.name();
        self.write_indent()?;
        write!(self.output, "<'{selector_name}'> when 'true' ->")?;
        writeln!(self.output)?;
        self.indent += 1;

        // Bind method parameters from Args list
        if !method.parameters.is_empty() {
            self.write_indent()?;
            write!(self.output, "case Args of")?;
            writeln!(self.output)?;
            self.indent += 1;

            self.write_indent()?;
            write!(self.output, "<[")?;
            // BT-295: Track method params for @primitive codegen
            for (i, param) in method.parameters.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                let var_name = self.fresh_var(&param.name);
                write!(self.output, "{var_name}")?;
                self.current_method_params.push(var_name);
            }
            write!(self.output, "]> when 'true' ->")?;
            writeln!(self.output)?;
            self.indent += 1;
        }

        // Generate the method body with state threading
        self.write_indent()?;
        self.generate_method_definition_body_with_reply(method)?;
        writeln!(self.output)?;

        if !method.parameters.is_empty() {
            self.indent -= 1;
            self.write_indent()?;
            writeln!(
                self.output,
                "<_> when 'true' -> {{'reply', {{'error', 'bad_arity'}}, State}}"
            )?;
            self.indent -= 1;
            self.write_indent()?;
            writeln!(self.output, "end")?;
        }

        self.indent -= 1;

        // Pop the scope when done with this method
        self.pop_scope();

        Ok(())
    }

    /// Generates a method definition body wrapped in a reply tuple.
    ///
    /// For `MethodDefinition` nodes with explicit body expressions.
    pub(in crate::codegen::core_erlang) fn generate_method_definition_body_with_reply(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<()> {
        if method.body.is_empty() {
            // Empty method body returns nil
            write!(
                self.output,
                "{{'reply', 'nil', {}}}",
                self.current_state_var()
            )?;
            return Ok(());
        }

        // Generate all expressions except the last with state threading
        for (i, expr) in method.body.iter().enumerate() {
            let is_last = i == method.body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);

            // Check for early return
            if let Expression::Return { value, .. } = expr {
                let final_state = self.current_state_var();
                write!(self.output, "let _ReturnValue = ")?;
                self.generate_expression(value)?;
                write!(self.output, " in {{'reply', _ReturnValue, {final_state}}}")?;
                return Ok(());
            }

            if is_last {
                // Last expression: bind to Result and generate reply tuple
                let final_state = self.current_state_var();

                // If the last expression is a field assignment, handle specially
                if is_field_assignment {
                    // Generate the assignment (leaves state binding open)
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let val_var = self.fresh_temp_var("Val");
                            let current_state = self.current_state_var();

                            write!(self.output, "let {val_var} = ")?;
                            self.generate_expression(value)?;

                            let new_state = self.next_state_var();
                            write!(
                                self.output,
                                " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in ",
                                field.name
                            )?;

                            // Reply tuple using the NEW state (after assignment)
                            write!(self.output, "{{'reply', {val_var}, {new_state}}}")?;
                        }
                    }
                } else if Self::is_super_message_send(expr) {
                    // Super message send as last expression: unpack {reply, Result, NewState}
                    write!(self.output, "let _SuperTuple = ")?;
                    self.generate_expression(expr)?;
                    write!(
                        self.output,
                        " in let _Result = call 'erlang':'element'(2, _SuperTuple)"
                    )?;
                    write!(
                        self.output,
                        " in let _NewState = call 'erlang':'element'(3, _SuperTuple)"
                    )?;
                    write!(self.output, " in {{'reply', _Result, _NewState}}")?;
                } else if Self::is_error_message_send(expr) {
                    // Error message send: never returns, so just emit the call directly
                    // without wrapping in a reply tuple (would be unreachable code)
                    self.generate_expression(expr)?;
                } else {
                    // Regular last expression: bind to Result and reply
                    write!(self.output, "let _Result = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in {{'reply', _Result, {final_state}}}")?;
                }
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                self.generate_field_assignment_open(expr)?;
            } else if self.control_flow_has_mutations(expr) {
                // Control flow that actually threads state: bind result to the next state variable
                //
                // We need to:
                // 1. Write "let State1 = " (using NEXT state name)
                // 2. Generate expression which uses CURRENT state (State)
                // 3. Write " in "
                //
                // The trick: calculate next state name without incrementing yet,
                // generate expression (which uses current state), then increment.
                let next_version = self.state_version() + 1;
                let new_state = if next_version == 1 {
                    "State1".to_string()
                } else {
                    format!("State{next_version}")
                };
                write!(self.output, "let {new_state} = ")?;
                self.generate_expression(expr)?;
                // Now actually increment the version so subsequent code uses State1
                let _ = self.next_state_var();
                write!(self.output, " in ")?;
            } else {
                // Regular intermediate expression: wrap in let to discard value
                let tmp_var = self.fresh_temp_var("seq");
                write!(self.output, "let {tmp_var} = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in ")?;
            }
        }

        Ok(())
    }

    /// Generates a block-based method body with state threading and reply tuple.
    ///
    /// For methods using block syntax with implicit returns.
    #[expect(
        clippy::too_many_lines,
        reason = "method body generation handles many expression types and state threading"
    )]
    pub(in crate::codegen::core_erlang) fn generate_method_body_with_reply(
        &mut self,
        block: &Block,
    ) -> Result<()> {
        if block.body.is_empty() {
            let final_state = self.current_state_var();
            write!(self.output, "{{'reply', 'nil', {final_state}}}")?;
            return Ok(());
        }

        // Generate all expressions except the last with state threading
        for (i, expr) in block.body.iter().enumerate() {
            let is_last = i == block.body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);
            let is_local_assignment = Self::is_local_var_assignment(expr);

            if is_last {
                // Last expression: bind to Result and generate reply tuple
                let final_state = self.current_state_var();

                // If the last expression is a field assignment, handle specially
                if is_field_assignment {
                    // Generate the assignment (leaves state binding open)
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let val_var = self.fresh_temp_var("Val");
                            let current_state = self.current_state_var();

                            write!(self.output, "let {val_var} = ")?;
                            self.generate_expression(value)?;

                            let new_state = self.next_state_var();
                            write!(
                                self.output,
                                " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in ",
                                field.name
                            )?;

                            // Reply tuple using the NEW state (after assignment)
                            write!(self.output, "{{'reply', {val_var}, {new_state}}}")?;
                        }
                    }
                } else if Self::is_super_message_send(expr) {
                    // Super message send as last expression: unpack {reply, Result, NewState}
                    write!(self.output, "let _SuperTuple = ")?;
                    self.generate_expression(expr)?;
                    write!(
                        self.output,
                        " in let _Result = call 'erlang':'element'(2, _SuperTuple)"
                    )?;
                    write!(
                        self.output,
                        " in let _NewState = call 'erlang':'element'(3, _SuperTuple)"
                    )?;
                    write!(self.output, " in {{'reply', _Result, _NewState}}")?;
                } else if Self::is_error_message_send(expr) {
                    // Error message send: never returns, so just emit the call directly
                    // without wrapping in a reply tuple (would be unreachable code)
                    self.generate_expression(expr)?;
                } else {
                    // Regular last expression: bind to Result and reply
                    write!(self.output, "let _Result = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in {{'reply', _Result, {final_state}}}")?;
                }
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                self.generate_field_assignment_open(expr)?;
            } else if is_local_assignment {
                // Local variable assignment: generate with proper binding
                if let Expression::Assignment { target, value, .. } = expr {
                    // Check if we're storing a block with mutations (ERROR)
                    if let Expression::Block(block) = value.as_ref() {
                        Self::validate_stored_closure(block, format!("{:?}", expr.span()))?;
                    }

                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = &id.name;
                        // Reuse existing Core Erlang variable if already bound (e.g. parameter),
                        // otherwise create a new one following Core Erlang conventions.
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                        // Generate: let VarName = <value> in ...
                        // Note: we delay updating the binding until after generating the RHS
                        // so that the RHS sees the prior value (for rebinding cases).
                        write!(self.output, "let {core_var} = ")?;
                        self.generate_expression(value)?;
                        // Now update the variable binding to point at the (possibly shadowing) let-bound var.
                        self.bind_var(var_name, &core_var);
                        write!(self.output, " in ")?;
                    }
                }
            } else if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(expr) {
                // whileTrue:/whileFalse:/timesRepeat: with mutations - need to rebind threaded vars after loop
                if threaded_vars.len() == 1 {
                    let var = &threaded_vars[0];
                    let core_var = self
                        .lookup_var(var)
                        .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                    write!(self.output, "let {core_var} = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in ")?;
                } else {
                    // Multiple threaded vars - fall back for now
                    let tmp_var = self.fresh_temp_var("seq");
                    write!(self.output, "let {tmp_var} = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in ")?;
                }
            } else if Self::is_super_message_send(expr) {
                // Super message send: must thread state from {reply, Result, NewState} tuple
                let super_result_var = self.fresh_temp_var("SuperReply");
                let current_state = self.current_state_var();
                let new_state = self.next_state_var();
                let class_name = self.class_name();

                // Generate beamtalk_dispatch:super/5 call (ADR 0006)
                if let Expression::MessageSend {
                    selector,
                    arguments,
                    ..
                } = expr
                {
                    // Use the domain service method for selector-to-atom conversion
                    let selector_atom = selector.to_erlang_atom();
                    write!(
                        self.output,
                        "let {super_result_var} = call 'beamtalk_dispatch':'super'('{selector_atom}', ["
                    )?;
                    for (i, arg) in arguments.iter().enumerate() {
                        if i > 0 {
                            write!(self.output, ", ")?;
                        }
                        self.generate_expression(arg)?;
                    }
                    write!(self.output, "], Self, {current_state}, '{class_name}')")?;
                }

                // Extract state from the {reply, Result, NewState} tuple using element/2
                write!(
                    self.output,
                    " in let {new_state} = call 'erlang':'element'(3, {super_result_var}) in "
                )?;
            } else {
                // Non-assignment intermediate expression: wrap in let
                let tmp_var = self.fresh_temp_var("seq");
                write!(self.output, "let {tmp_var} = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in ")?;
            }
        }
        Ok(())
    }

    /// Generates the `register_class/0` function for class registration.
    ///
    /// This function is called automatically via `-on_load` when the module loads.
    /// It registers the class with `beamtalk_class:start_link/2`, making the class
    /// available as a first-class object for reflection and metaprogramming.
    ///
    /// The function is defensive - if `beamtalk_class` is not available (e.g., during
    /// early module loading), it returns `ok` to allow the module to load. Classes can
    /// be registered later via explicit calls if needed.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'register_class'/0 = fun () ->
    ///     try
    ///         case call 'beamtalk_object_class':'start_link'('Counter', ~{...}~) of
    ///             <{'ok', _Pid}> when 'true' -> 'ok'
    ///             <{'error', {'already_started', _}}> when 'true' -> 'ok'
    ///             <{'error', _Reason}> when 'true' -> 'ok'
    ///         end
    ///     catch <_,_,_> -> 'ok'
    /// ```
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_register_class(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        // Skip if no class definitions
        if module.classes.is_empty() {
            return Ok(());
        }

        writeln!(self.output, "'register_class'/0 = fun () ->")?;
        self.indent += 1;

        // Wrap in try-catch for defensive loading
        self.write_indent()?;
        writeln!(self.output, "try")?;
        self.indent += 1;

        // Generate registration for each class in the module
        for (i, class) in module.classes.iter().enumerate() {
            self.write_indent()?;
            // Use case expressions for error handling
            if i > 0 {
                write!(self.output, "in ")?;
            }
            write!(
                self.output,
                "let _Reg{} = case call 'beamtalk_object_class':'start_link'('{}', ~{{",
                i, class.name.name
            )?;
            writeln!(self.output)?;
            self.indent += 1;

            // Class name
            self.write_indent()?;
            writeln!(self.output, "'name' => '{}',", class.name.name)?;

            // Module name
            self.write_indent()?;
            writeln!(self.output, "'module' => '{}',", self.module_name)?;

            // Superclass
            self.write_indent()?;
            writeln!(
                self.output,
                "'superclass' => '{}',",
                class.superclass_name()
            )?;

            // BT-105: Sealing modifiers
            self.write_indent()?;
            writeln!(
                self.output,
                "'is_sealed' => '{}',",
                if class.is_sealed { "true" } else { "false" }
            )?;
            self.write_indent()?;
            writeln!(
                self.output,
                "'is_abstract' => '{}',",
                if class.is_abstract { "true" } else { "false" }
            )?;

            // Instance methods
            self.write_indent()?;
            write!(self.output, "'instance_methods' => ~{{")?;
            let instance_methods: Vec<_> = class
                .methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .collect();

            for (method_idx, method) in instance_methods.iter().enumerate() {
                if method_idx > 0 {
                    write!(self.output, ", ")?;
                }
                // BT-403: Include per-method is_sealed flag
                let is_sealed = if method.is_sealed || class.is_sealed {
                    "'true'"
                } else {
                    "'false'"
                };
                write!(
                    self.output,
                    "'{}' => ~{{'arity' => {}, 'is_sealed' => {}}}~",
                    method.selector.name(),
                    method.selector.arity(),
                    is_sealed
                )?;
            }
            writeln!(self.output, "}}~,")?;

            // Instance variables
            self.write_indent()?;
            write!(self.output, "'instance_variables' => [")?;
            for (state_idx, state_decl) in class.state.iter().enumerate() {
                if state_idx > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "'{}'", state_decl.name.name)?;
            }
            writeln!(self.output, "],")?;

            // Class methods — depends on whether this is an actor or value type
            // BT-411: Include both built-in and user-defined class methods
            self.write_indent()?;
            writeln!(self.output, "'class_methods' => ~{{")?;
            self.indent += 1;
            if self.context == CodeGenContext::Actor {
                self.write_indent()?;
                writeln!(self.output, "'spawn' => ~{{'arity' => 0}}~,")?;
                self.write_indent()?;
                writeln!(self.output, "'spawnWith:' => ~{{'arity' => 1}}~,")?;
            } else {
                self.write_indent()?;
                writeln!(self.output, "'new' => ~{{'arity' => 0}}~,")?;
                self.write_indent()?;
                writeln!(self.output, "'new:' => ~{{'arity' => 1}}~,")?;
            }
            self.write_indent()?;
            writeln!(self.output, "'superclass' => ~{{'arity' => 0}}~")?;
            // BT-411: User-defined class methods
            for method in &class.class_methods {
                if method.kind == MethodKind::Primary {
                    self.write_indent()?;
                    writeln!(
                        self.output,
                        ", '{}' => ~{{'arity' => {}}}~",
                        method.selector.name(),
                        method.selector.arity()
                    )?;
                }
            }
            self.indent -= 1;
            self.write_indent()?;
            writeln!(self.output, "}}~,")?;

            // BT-101: Method source for CompiledMethod introspection
            self.write_indent()?;
            write!(self.output, "'method_source' => ~{{")?;
            for (method_idx, method) in instance_methods.iter().enumerate() {
                if method_idx > 0 {
                    write!(self.output, ", ")?;
                }
                let source_str = self.extract_method_source(method);
                write!(self.output, "'{}' => ", method.selector.name())?;
                self.generate_binary_string(&source_str)?;
            }
            writeln!(self.output, "}}~")?;

            self.indent -= 1;
            self.write_indent()?;
            writeln!(self.output, "}}~) of")?;

            // Handle success case
            self.indent += 1;
            self.write_indent()?;
            writeln!(self.output, "<{{'ok', _Pid{i}}}> when 'true' -> 'ok'")?;

            // Handle error cases - allow module to load anyway
            self.write_indent()?;
            writeln!(
                self.output,
                "<{{'error', {{'already_started', _Existing{i}}}}}> when 'true' -> 'ok'"
            )?;

            self.write_indent()?;
            writeln!(self.output, "<{{'error', _Reason{i}}}> when 'true' -> 'ok'")?;
            self.indent -= 1;

            self.write_indent()?;
            writeln!(self.output, "end")?;
        }

        writeln!(self.output)?;
        self.write_indent()?;
        writeln!(self.output, "in 'ok'")?;

        // Catch any exceptions during registration
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "of RegResult -> RegResult")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "catch <CatchType, CatchError, CatchStack> -> 'ok'"
        )?;

        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates standalone function bodies for class-side methods.
    ///
    /// Class methods are module-level functions with a `class_` prefix.
    /// They take `ClassSelf` as the first parameter (the class object),
    /// followed by any user-defined parameters.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'class_defaultValue'/1 = fun (ClassSelf) ->
    ///     42
    ///
    /// 'class_create'/1 = fun (ClassSelf) ->
    ///     let _Result = call 'beamtalk_object_class':'class_send'(
    ///         call 'erlang':'element'(4, ClassSelf), 'new:', [~{}~])
    ///     in _Result
    /// ```
    pub(in crate::codegen::core_erlang) fn generate_class_method_functions(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<()> {
        for method in &class.class_methods {
            if method.kind != MethodKind::Primary {
                continue;
            }

            let selector_name = method.selector.name();
            let arity = method.selector.arity() + 1; // +1 for ClassSelf

            writeln!(self.output)?;
            write!(
                self.output,
                "'class_{selector_name}'/{arity} = fun (ClassSelf"
            )?;

            // Push scope for parameter bindings
            self.push_scope();
            self.current_method_params.clear();
            self.reset_state_version();

            // Bind ClassSelf as 'self' in scope
            self.bind_var("self", "ClassSelf");
            self.in_class_method = true;

            // Generate parameter names
            for param in &method.parameters {
                let var_name = self.fresh_var(&param.name);
                write!(self.output, ", {var_name}")?;
                self.current_method_params.push(var_name);
            }
            writeln!(self.output, ") ->")?;
            self.indent += 1;

            // Generate method body
            self.write_indent()?;
            if method.body.is_empty() {
                write!(self.output, "'nil'")?;
            } else {
                self.generate_class_method_body(method)?;
            }
            writeln!(self.output)?;

            self.indent -= 1;
            self.pop_scope();
            self.in_class_method = false;
        }
        Ok(())
    }

    /// Generates the body of a class-side method.
    ///
    /// Unlike instance methods, class methods have no state threading.
    /// They simply evaluate expressions and return the last value.
    fn generate_class_method_body(&mut self, method: &MethodDefinition) -> Result<()> {
        for (i, expr) in method.body.iter().enumerate() {
            let is_last = i == method.body.len() - 1;

            if let Expression::Return { value, .. } = expr {
                self.generate_expression(value)?;
                return Ok(());
            }

            if is_last {
                self.generate_expression(expr)?;
            } else {
                let tmp_var = self.fresh_temp_var("seq");
                write!(self.output, "let {tmp_var} = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in ")?;
            }
        }
        Ok(())
    }

    /// Extracts source text for a method from the original source (BT-101).
    fn extract_method_source(&self, method: &MethodDefinition) -> String {
        if let Some(ref source) = self.source_text {
            let start = method.span.start() as usize;
            let end = method.span.end() as usize;
            if end <= source.len() {
                return source[start..end].trim().to_string();
            }
        }
        // Fallback: use selector name
        method.selector.name().to_string()
    }

    /// Checks if a control flow expression actually threads state through mutations.
    ///
    /// This goes beyond `is_state_threading_control_flow` by analyzing whether
    /// the block argument contains field mutations that require state threading.
    ///
    /// Returns `true` only if:
    /// 1. The expression is a state-threading control flow construct (do:, whileTrue:, etc.)
    /// 2. The block argument contains field writes that need threading
    ///
    /// This prevents binding non-state return values (like `ok` or `nil`) to `StateN`.
    fn control_flow_has_mutations(&self, expr: &Expression) -> bool {
        // First check if it's a potential state-threading construct
        if !Self::is_state_threading_control_flow(expr) {
            return false;
        }

        // Extract the block argument and analyze for mutations
        if let Expression::MessageSend { arguments, .. } = expr {
            // The block is typically the last argument for these control flow constructs
            if let Some(Expression::Block(block)) = arguments.last() {
                let analysis = block_analysis::analyze_block(block);
                return self.needs_mutation_threading(&analysis);
            }
        }

        // If we can't analyze (e.g., block isn't a literal), conservatively return false
        // to avoid binding non-state values to StateN
        false
    }
}

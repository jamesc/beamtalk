// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for actor classes, producing
//! `gen_server`-based Erlang modules with async messaging, error isolation,
//! and hot code reload support.

use super::util::ClassIdentity;
use super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::{MethodKind, Module};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates a full actor module with `gen_server` behaviour.
    ///
    /// Per BT-29 design doc, each actor class generates a `gen_server` module with:
    /// - Error isolation via `safe_dispatch/3`
    /// - `doesNotUnderstand:args:` fallback dispatch
    /// - `terminate/2` with method call support
    ///
    /// ## Object References (BT-100)
    ///
    /// The `#beamtalk_object{}` record is defined in `runtime/include/beamtalk.hrl`
    /// and is now used by generated code:
    /// - `spawn/0` and `spawn/1` return `#beamtalk_object{class, class_mod, pid}` records
    /// - Message sends extract the pid using `call 'erlang':'element'(4, Obj)`
    /// - This enables reflection (`obj class`) and proper object semantics
    pub(super) fn generate_actor_module(&mut self, module: &Module) -> Result<()> {
        // BT-213: Set context to Actor for this module
        self.context = CodeGenContext::Actor;

        // BT-295: Set class identity for @primitive codegen
        // BT-403: Include sealed/abstract flags for codegen optimization
        if let Some(class) = module.classes.first() {
            self.class_identity = Some(ClassIdentity::from_class_def(
                &class.name.name,
                class.is_sealed,
                class.is_abstract,
            ));

            // BT-403: Collect sealed method selectors for direct-call optimization.
            // In sealed class: all primary methods are eligible.
            // In non-sealed class: only explicitly sealed methods.
            self.sealed_method_selectors.clear();
            for method in &class.methods {
                if method.kind == MethodKind::Primary && (class.is_sealed || method.is_sealed) {
                    self.sealed_method_selectors
                        .insert(method.selector.name().to_string());
                }
            }
        }

        // Check if module has class definitions for registration
        let has_classes = !module.classes.is_empty();
        let is_abstract = self.is_class_abstract();

        // BT-403: Build sealed method exports
        let sealed_exports: Vec<String> = self
            .sealed_method_selectors
            .iter()
            .map(|sel| {
                let arity = module.classes.first().map_or(0, |c| {
                    c.methods
                        .iter()
                        .find(|m| m.selector.name() == sel.as_str())
                        .map_or(0, |m| m.selector.arity())
                });
                // Standalone function takes Args list, Self, State = 3 params
                format!("'__sealed_{sel}'/{}", arity + 2)
            })
            .collect();
        let sealed_export_str = if sealed_exports.is_empty() {
            String::new()
        } else {
            format!(", {}", sealed_exports.join(", "))
        };

        // Module header with expanded exports per BT-29
        // BT-217: Add 'new'/0 and 'new'/1 exports for error methods
        // BT-242: Add 'has_method'/1 export for reflection
        let base_exports = "'start_link'/1, 'init'/1, 'handle_cast'/2, 'handle_call'/3, \
                            'code_change'/3, 'terminate'/2, 'dispatch'/4, 'safe_dispatch'/3, \
                            'method_table'/0, 'has_method'/1, 'spawn'/0, 'spawn'/1, 'new'/0, 'new'/1";

        if has_classes {
            writeln!(
                self.output,
                "module '{}' [{base_exports}{sealed_export_str}, 'register_class'/0]",
                self.module_name
            )?;
            writeln!(
                self.output,
                "  attributes ['behaviour' = ['gen_server'], \
                 'on_load' = [{{'register_class', 0}}]]"
            )?;
        } else {
            writeln!(
                self.output,
                "module '{}' [{base_exports}{sealed_export_str}]",
                self.module_name
            )?;
            writeln!(self.output, "  attributes ['behaviour' = ['gen_server']]")?;
        }
        writeln!(self.output)?;

        // Generate start_link/1 (standard gen_server entry point)
        self.generate_start_link()?;
        writeln!(self.output)?;

        // Generate spawn/0 function (class method to instantiate actors)
        self.generate_spawn_function(module)?;
        writeln!(self.output)?;

        // Generate spawn/1 function (class method with init args)
        self.generate_spawn_with_args_function(module)?;
        writeln!(self.output)?;

        // BT-217: Generate new/0 and new/1 error methods for actors
        self.generate_actor_new_error_method()?;
        writeln!(self.output)?;
        self.generate_actor_new_with_args_error_method()?;
        writeln!(self.output)?;

        // Generate init/1 function
        self.generate_init_function(module)?;
        writeln!(self.output)?;

        // BT-403: Abstract classes skip gen_server callback scaffolding.
        // These callbacks are only needed for instantiable actors that receive messages.
        if is_abstract {
            // BT-403: Abstract classes need minimal gen_server callbacks
            // (required by gen_server behaviour but will never be called)
            self.generate_abstract_callbacks()?;
            writeln!(self.output)?;
        } else {
            // Generate handle_cast/2 function with error handling
            self.generate_handle_cast()?;
            writeln!(self.output)?;

            // Generate handle_call/3 function with error handling
            self.generate_handle_call()?;
            writeln!(self.output)?;

            // Generate code_change/3 function
            self.generate_code_change()?;
            writeln!(self.output)?;

            // Generate terminate/2 function (per BT-29)
            self.generate_terminate(module)?;
            writeln!(self.output)?;

            // Generate safe_dispatch/3 with error isolation (per BT-29)
            self.generate_safe_dispatch()?;
            writeln!(self.output)?;
        }

        // Generate dispatch function with DNU fallback
        self.generate_dispatch(module)?;
        writeln!(self.output)?;

        // Generate method table
        self.generate_method_table(module)?;
        writeln!(self.output)?;

        // Generate has_method/1 for reflection (BT-242)
        self.generate_has_method(module)?;

        // BT-403: Generate sealed method standalone functions
        if !self.sealed_method_selectors.is_empty() {
            self.generate_sealed_method_functions(module)?;
        }

        // Generate class registration function (BT-218)
        if !module.classes.is_empty() {
            writeln!(self.output)?;
            self.generate_register_class(module)?;
        }

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }

    /// Generates minimal `gen_server` callbacks for abstract classes (BT-403).
    ///
    /// Abstract classes can't be instantiated, so these callbacks will never
    /// be called. But `gen_server` behaviour requires them to be exported.
    fn generate_abstract_callbacks(&mut self) -> Result<()> {
        // handle_cast - never called for abstract classes
        writeln!(
            self.output,
            "'handle_cast'/2 = fun (_Msg, State) -> {{'noreply', State}}"
        )?;
        writeln!(self.output)?;

        // handle_call - never called for abstract classes
        writeln!(
            self.output,
            "'handle_call'/3 = fun (_Msg, _From, State) -> {{'reply', 'nil', State}}"
        )?;
        writeln!(self.output)?;

        // code_change
        writeln!(
            self.output,
            "'code_change'/3 = fun (_OldVsn, State, _Extra) -> {{'ok', State}}"
        )?;
        writeln!(self.output)?;

        // terminate
        writeln!(self.output, "'terminate'/2 = fun (_Reason, _State) -> 'ok'")?;
        writeln!(self.output)?;

        // safe_dispatch - abstract classes use dispatch directly (no error isolation needed)
        writeln!(
            self.output,
            "'safe_dispatch'/3 = fun (Selector, Args, State) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Self = call 'beamtalk_actor':'make_self'(State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "call '{}':'dispatch'(Selector, Args, Self, State)",
            self.module_name
        )?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates standalone functions for sealed methods (BT-403).
    ///
    /// Each sealed method gets a `'__sealed_{selector}'/N` function that
    /// can be called directly from self-sends, bypassing both `safe_dispatch/3`
    /// and the `dispatch/4` case selector matching.
    fn generate_sealed_method_functions(&mut self, module: &Module) -> Result<()> {
        let Some(class) = module.classes.first() else {
            return Ok(());
        };

        for method in &class.methods {
            if method.kind != MethodKind::Primary {
                continue;
            }
            let selector_name = method.selector.name().to_string();
            if !self.sealed_method_selectors.contains(&selector_name) {
                continue;
            }

            writeln!(self.output)?;

            // Generate: '__sealed_{selector}'/N = fun (Arg1, ..., Self, State) ->
            let arity = method.selector.arity() + 2; // + Self + State
            writeln!(self.output, "'__sealed_{selector_name}'/{arity}  = fun (",)?;

            // Reset state version for this method
            self.reset_state_version();
            self.push_scope();
            self.current_method_params.clear();

            // Generate parameter list
            let mut params = Vec::new();
            for param in &method.parameters {
                let var_name = self.fresh_var(&param.name);
                params.push(var_name);
            }

            // Parameters, then Self, then State
            let mut all_params: Vec<String> = params.clone();
            all_params.push("Self".to_string());
            all_params.push("State".to_string());
            write!(self.output, "{}", all_params.join(", "))?;
            writeln!(self.output, ") ->")?;

            self.indent += 1;
            self.write_indent()?;

            // Generate method body with reply tuple (reuse existing codegen)
            self.generate_method_definition_body_with_reply(method)?;
            writeln!(self.output)?;

            self.indent -= 1;
            self.pop_scope();
        }

        Ok(())
    }
}

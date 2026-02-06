// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for actor classes, producing
//! `gen_server`-based Erlang modules with async messaging, error isolation,
//! and hot code reload support.

use super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::Module;
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

        // BT-295: Set current class name for @primitive codegen
        if let Some(class) = module.classes.first() {
            self.current_class_name = Some(class.name.name.to_string());
        }

        // Check if module has class definitions for registration
        let has_classes = !module.classes.is_empty();

        // Module header with expanded exports per BT-29
        // BT-217: Add 'new'/0 and 'new'/1 exports for error methods
        // BT-242: Add 'has_method'/1 export for reflection
        let base_exports = "'start_link'/1, 'init'/1, 'handle_cast'/2, 'handle_call'/3, \
                            'code_change'/3, 'terminate'/2, 'dispatch'/4, 'safe_dispatch'/3, \
                            'method_table'/0, 'has_method'/1, 'spawn'/0, 'spawn'/1, 'new'/0, 'new'/1";

        if has_classes {
            writeln!(
                self.output,
                "module '{}' [{base_exports}, 'register_class'/0]",
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
                "module '{}' [{base_exports}]",
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

        // Generate dispatch function with DNU fallback
        self.generate_dispatch(module)?;
        writeln!(self.output)?;

        // Generate method table
        self.generate_method_table(module)?;
        writeln!(self.output)?;

        // Generate has_method/1 for reflection (BT-242)
        self.generate_has_method(module)?;

        // Generate class registration function (BT-218)
        if !module.classes.is_empty() {
            writeln!(self.output)?;
            self.generate_register_class(module)?;
        }

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }
}

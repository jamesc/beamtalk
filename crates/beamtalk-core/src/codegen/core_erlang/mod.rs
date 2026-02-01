// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core Erlang code generation for Beamtalk.
//!
//! This module transforms Beamtalk AST into Core Erlang, which is then
//! compiled to BEAM bytecode by `erlc`. The generated code follows the
//! actor runtime model using OTP's `gen_server` behaviour.
//!
//! # Architecture
//!
//! Each Beamtalk module becomes an Erlang module implementing `gen_server`:
//!
//! - **Actor State**: A map containing `__class__`, `__methods__`, and actor fields
//! - **Message Dispatch**: Messages route through `handle_cast` or `handle_call`
//! - **Hot Code Reload**: The `code_change/3` callback handles state migration
//!
//! # Example
//!
//! Beamtalk source:
//! ```beamtalk
//! value := 0.
//! increment := [ self.value := self.value + 1. ^self.value ].
//! ```
//!
//! Generated Core Erlang:
//! ```erlang
//! module 'counter' ['init'/1, 'handle_cast'/2, 'handle_call'/3,
//!                   'code_change'/3, 'dispatch'/3, 'method_table'/0, 'spawn'/0]
//!   attributes ['behaviour' = ['gen_server']]
//!
//! 'init'/1 = fun (_Args) ->
//!     let InitialState = ~{
//!       '__class__' => 'Counter',
//!       '__methods__' => call 'counter':'method_table'(),
//!       'value' => 0
//!     }~
//!     in {'ok', InitialState}
//!
//! 'handle_cast'/2 = fun (Msg, State) ->
//!     case Msg of
//!       <{Selector, Args, FuturePid}> when 'true' ->
//!         case call 'counter':'dispatch'(Selector, Args, State) of
//!           <{'reply', Result, NewState}> when 'true' ->
//!             let _ = call 'erlang':'!'(FuturePid, {'resolved', Result})
//!             in {'noreply', NewState}
//!         end
//!     end
//! ```
//!
//! # Core Erlang Syntax
//!
//! Core Erlang is a simplified functional IR for Erlang:
//!
//! - **Atoms**: `'atom_name'` (always quoted)
//! - **Variables**: `VariableName` (starts with uppercase)
//! - **Function calls**: `call 'module':'function'(args)`
//! - **Let bindings**: `let Var = Expr in Body`
//! - **Case expressions**: `case Expr of Pattern -> Body end`
//! - **Maps**: `~{'key' => value}~`
//! - **Tuples**: `{'tuple', 'elements'}`
//! - **Lists**: `[1, 2, 3]` or `[Head | Tail]`
//!
//! # References
//!
//! - [Core Erlang Specification](https://www.it.uu.se/research/group/hipe/cerl/)
//! - [Gleam Erlang Codegen](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/erlang.rs)

mod block_analysis;
mod builtins;
mod expressions;
mod gen_server;
mod util;

use crate::ast::{
    Block, ClassDefinition, Expression, Identifier, Literal, MapPair, MessageSelector,
    MethodDefinition, MethodKind, Module,
};
use std::collections::HashMap;
use std::fmt::{self, Write};
use thiserror::Error;

/// Errors that can occur during code generation.
#[derive(Debug, Error)]
pub enum CodeGenError {
    /// Unsupported language feature.
    #[error("unsupported feature: {feature} at {location}")]
    UnsupportedFeature {
        /// The feature that is not yet supported.
        feature: String,
        /// Source location.
        location: String,
    },

    /// Internal code generation error.
    #[error("code generation error: {0}")]
    Internal(String),

    /// Formatting error during code generation.
    #[error("formatting error: {0}")]
    Format(#[from] fmt::Error),

    /// Field assignment in a stored closure.
    #[error(
        "Cannot assign to field '{field}' inside a stored closure at {location}.\n\n\
             Field assignments require immediate execution context for state threading.\n\n\
             Fix: Use control flow directly, or extract to a method:\n\
             \x20 // Instead of:\n\
             \x20 myBlock := [:item | self.{field} := self.{field} + item].\n\
             \x20 items do: myBlock.\n\
             \x20 \n\
             \x20 // Write:\n\
             \x20 items do: [:item | self.{field} := self.{field} + item].\n\
             \x20 \n\
             \x20 // Or use a method:\n\
             \x20 addTo{field_capitalized}: item => self.{field} := self.{field} + item.\n\
             \x20 items do: [:item | self addTo{field_capitalized}: item]."
    )]
    FieldAssignmentInStoredClosure {
        /// The field being assigned.
        field: String,
        /// The capitalized field name for method suggestion.
        field_capitalized: String,
        /// Source location.
        location: String,
    },

    /// Local variable mutation in a stored closure.
    #[error(
        "Warning: Assignment to '{variable}' inside stored closure has no effect on outer scope at {location}.\n\n\
             Closures capture variables by value. The outer '{variable}' won't change.\n\n\
             Fix: Use control flow directly:\n\
             \x20 // Instead of:\n\
             \x20 myBlock := [{variable} := {variable} + 1].\n\
             \x20 10 timesRepeat: myBlock.\n\
             \x20 \n\
             \x20 // Write:\n\
             \x20 10 timesRepeat: [{variable} := {variable} + 1]."
    )]
    LocalMutationInStoredClosure {
        /// The variable being mutated.
        variable: String,
        /// Source location.
        location: String,
    },
}

/// Result type for code generation operations.
pub type Result<T> = std::result::Result<T, CodeGenError>;

/// Generates Core Erlang code from a Beamtalk module.
///
/// This is the main entry point for code generation. It transforms
/// the parsed AST into Core Erlang text that can be compiled by `erlc`.
///
/// # Errors
///
/// Returns [`CodeGenError`] if:
/// - The module uses unsupported features
/// - Code generation encounters an internal error
/// - Formatting fails
///
/// # Example
///
/// ```no_run
/// use beamtalk_core::codegen::core_erlang::generate;
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::parse::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let core_erlang = generate(&module)?;
/// println!("{}", core_erlang);
/// # Ok::<(), beamtalk_core::codegen::core_erlang::CodeGenError>(())
/// ```
pub fn generate(module: &Module) -> Result<String> {
    let mut generator = CoreErlangGenerator::new("beamtalk_module");
    generator.generate_module(module)?;
    Ok(generator.output)
}

/// Generates Core Erlang code with a specified module name.
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_with_name(module: &Module, module_name: &str) -> Result<String> {
    let mut generator = CoreErlangGenerator::new(module_name);
    generator.generate_module(module)?;
    Ok(generator.output)
}

/// Generates Core Erlang for a REPL expression.
///
/// This creates a simple module that evaluates a single expression and
/// returns its value. Used by the REPL for interactive evaluation.
///
/// The generated module has an `eval/1` function that takes a bindings map
/// and returns the expression result.
///
/// # Arguments
///
/// * `expression` - The Beamtalk expression AST to evaluate
/// * `module_name` - Unique module name (e.g., `repl_eval_42`)
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_repl_expression(expression: &Expression, module_name: &str) -> Result<String> {
    let mut generator = CoreErlangGenerator::new(module_name);
    generator.generate_repl_module(expression)?;
    Ok(generator.output)
}

// NOTE: Implementation copied from erlang.rs - will be split into submodules next
struct CoreErlangGenerator {
    /// The module name being generated.
    module_name: String,
    /// The output buffer.
    output: String,
    /// Current indentation level.
    indent: usize,
    /// Counter for generating unique variable names.
    var_counter: usize,
    /// Stack of variable binding scopes. Each scope is a map of identifiers
    /// to Core Erlang variable names. Inner scopes shadow outer scopes.
    var_scopes: Vec<HashMap<String, String>>,
    /// Current state version for state threading in method bodies.
    /// Version 0 = "State", 1 = "State1", 2 = "State2", etc.
    /// This is used to thread state through field assignments.
    state_version: usize,
}

impl CoreErlangGenerator {
    /// Creates a new code generator for the given module name.
    fn new(module_name: &str) -> Self {
        Self {
            module_name: module_name.to_string(),
            output: String::new(),
            indent: 0,
            var_counter: 0,
            var_scopes: vec![HashMap::new()],
            state_version: 0,
        }
    }

    /// Pushes a new scope for variable bindings.
    fn push_scope(&mut self) {
        self.var_scopes.push(HashMap::new());
    }

    /// Pops the current scope, discarding its bindings.
    fn pop_scope(&mut self) {
        if self.var_scopes.len() > 1 {
            self.var_scopes.pop();
        }
    }

    /// Looks up a variable binding in the current scope stack.
    fn lookup_var(&self, name: &str) -> Option<&String> {
        // Search from innermost to outermost scope
        for scope in self.var_scopes.iter().rev() {
            if let Some(var_name) = scope.get(name) {
                return Some(var_name);
            }
        }
        None
    }

    /// Binds an identifier to a Core Erlang variable name in the current scope.
    fn bind_var(&mut self, name: &str, core_var: &str) {
        if let Some(current_scope) = self.var_scopes.last_mut() {
            current_scope.insert(name.to_string(), core_var.to_string());
        }
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
    fn module_name_to_class_name(&self) -> String {
        assert!(
            !self.module_name.is_empty(),
            "module_name_to_class_name called with empty module name; this is an invalid state in code generation"
        );

        let mut chars = self.module_name.chars();
        let first = chars.next().unwrap().to_uppercase().to_string();
        first + chars.as_str()
    }

    /// Generates a full module with `gen_server` behaviour.
    ///
    /// Per BT-29 design doc, each class generates a `gen_server` module with:
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
    fn generate_module(&mut self, module: &Module) -> Result<()> {
        // Module header with expanded exports per BT-29
        writeln!(
            self.output,
            "module '{}' ['start_link'/1, 'init'/1, 'handle_cast'/2, 'handle_call'/3, \
             'code_change'/3, 'terminate'/2, 'dispatch'/3, 'safe_dispatch'/3, \
             'method_table'/0, 'spawn'/0, 'spawn'/1]",
            self.module_name
        )?;
        writeln!(self.output, "  attributes ['behaviour' = ['gen_server']]")?;
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

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }

    /// Generates a simple REPL evaluation module.
    ///
    /// Creates a module with a single `eval/1` function that evaluates
    /// an expression with the provided bindings map.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression to evaluate
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// module 'repl_eval_42' ['eval'/1]
    ///   attributes []
    ///
    /// 'eval'/1 = fun (Bindings) ->
    ///     let State = Bindings in
    ///     <expression>
    /// end
    /// ```
    ///
    /// The `State` alias ensures that identifier lookups work correctly,
    /// since `generate_identifier` falls back to `maps:get(Name, State)`
    /// for variables not bound in the current scope.
    fn generate_repl_module(&mut self, expression: &Expression) -> Result<()> {
        // Module header - simple module with just eval/1
        writeln!(self.output, "module '{}' ['eval'/1]", self.module_name)?;
        writeln!(self.output, "  attributes []")?;
        writeln!(self.output)?;

        // Generate eval/1 function
        // Alias State = Bindings so identifier lookup works (it falls back to State)
        writeln!(self.output, "'eval'/1 = fun (Bindings) ->")?;
        self.indent += 1;

        // Register Bindings in scope for variable lookups
        self.push_scope();
        self.bind_var("__bindings__", "Bindings");

        // Alias State to Bindings for identifier fallback lookup
        self.write_indent()?;
        writeln!(self.output, "let State = Bindings in")?;

        // Generate the expression
        self.write_indent()?;
        self.generate_expression(expression)?;
        writeln!(self.output)?;

        self.pop_scope();
        self.indent -= 1;

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }

    /// Generates the `start_link/1` function for supervised `gen_server` startup.
    ///
    /// This is the standard OTP entry point for starting a supervised `gen_server`.
    /// It calls `gen_server:start_link/3` directly with the provided init args.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'start_link'/1 = fun (InitArgs) ->
    ///     call 'gen_server':'start_link'('module_name', InitArgs, [])
    /// ```
    fn generate_start_link(&mut self) -> Result<()> {
        writeln!(self.output, "'start_link'/1 = fun (InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "call 'gen_server':'start_link'('{}', InitArgs, [])",
            self.module_name
        )?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `spawn/0` class method for creating actor instances.
    ///
    /// This is a class-level method that instantiates a new actor process
    /// using `gen_server:start_link/3`. The function:
    /// 1. Calls `gen_server:start_link/3` with empty args (init/1 creates the state)
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/0 = fun () ->
    ///     case call 'gen_server':'start_link'('counter', ~{}~, []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             {'beamtalk_object', 'Counter', 'counter', Pid};
    ///         <{'error', Reason}> when 'true' ->
    ///             call 'erlang':'error'({'spawn_failed', Reason})
    ///     end
    /// ```
    fn generate_spawn_function(&mut self, _module: &Module) -> Result<()> {
        writeln!(self.output, "'spawn'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Pass empty map as InitArgs - init/1 merges this with default state
        writeln!(
            self.output,
            "case call 'gen_server':'start_link'('{}', ~{{}}~, []) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', Pid}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Return #beamtalk_object{} record instead of raw pid
        // Record syntax in Core Erlang: {RecordTag, Field1, Field2, ...}
        let class_name = self.module_name_to_class_name();
        writeln!(
            self.output,
            "{{'beamtalk_object', '{}', '{}', Pid}}",
            class_name, self.module_name
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Reason}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "call 'erlang':'error'({{'spawn_failed', Reason}})"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `spawn/1` class method for creating actor instances with init args.
    ///
    /// This is a class-level method that instantiates a new actor process
    /// with initialization arguments passed to `init/1`. The function:
    /// 1. Calls `gen_server:start_link/3` with the provided args
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/1 = fun (InitArgs) ->
    ///     case call 'gen_server':'start_link'('counter', InitArgs, []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             {'beamtalk_object', 'Counter', 'counter', Pid};
    ///         <{'error', Reason}> when 'true' ->
    ///             call 'erlang':'error'({'spawn_failed', Reason})
    ///     end
    /// ```
    fn generate_spawn_with_args_function(&mut self, _module: &Module) -> Result<()> {
        writeln!(self.output, "'spawn'/1 = fun (InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'gen_server':'start_link'('{}', InitArgs, []) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', Pid}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Return #beamtalk_object{} record instead of raw pid
        let class_name = self.module_name_to_class_name();
        writeln!(
            self.output,
            "{{'beamtalk_object', '{}', '{}', Pid}}",
            class_name, self.module_name
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Reason}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "call 'erlang':'error'({{'spawn_failed', Reason}})"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `init/1` callback for `gen_server`.
    ///
    /// The init function:
    /// 1. Creates a default state map with `__class__`, `__methods__`, and default field values
    /// 2. Merges the `InitArgs` map into the default state (`InitArgs` values override defaults)
    /// 3. Returns `{ok, FinalState}`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'init'/1 = fun (InitArgs) ->
    ///     let DefaultState = ~{
    ///         '__class__' => 'Counter',
    ///         '__methods__' => call 'counter':'method_table'(),
    ///         'value' => 0
    ///     }~
    ///     in let FinalState = call 'maps':'merge'(DefaultState, InitArgs)
    ///        in {'ok', FinalState}
    /// ```
    fn generate_init_function(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'init'/1 = fun (InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let DefaultState = ~{{")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'__class__' => '{}',", self.to_class_name())?;
        self.write_indent()?;
        writeln!(
            self.output,
            "'__methods__' => call '{}':'method_table'()",
            self.module_name
        )?;

        // Initialize fields from module expressions
        self.generate_initial_state_fields(module)?;

        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "}}~")?;
        self.write_indent()?;
        // Merge InitArgs into DefaultState - InitArgs values override defaults
        writeln!(
            self.output,
            "in let FinalState = call 'maps':'merge'(DefaultState, InitArgs)"
        )?;
        self.write_indent()?;
        writeln!(self.output, "in {{'ok', FinalState}}")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_cast/2` callback for async message sends.
    ///
    /// Per BT-29 design doc, uses `safe_dispatch/3` for error isolation and
    /// sends `{resolved, Result}` or `{rejected, Error}` to the `FuturePid`.
    fn generate_handle_cast(&mut self) -> Result<()> {
        writeln!(self.output, "'handle_cast'/2 = fun (Msg, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Msg of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{Selector, Args, FuturePid}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        // Use safe_dispatch for error isolation per BT-29
        writeln!(
            self.output,
            "case call '{}':'safe_dispatch'(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
        // Success case: resolve the future
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'reply', Result, NewState}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let _Ignored = call 'erlang':'!'(FuturePid, {{'resolved', Result}})"
        )?;
        self.write_indent()?;
        writeln!(self.output, "in {{'noreply', NewState}}")?;
        self.indent -= 1;
        // Error case: reject the future (error isolation - don't crash the instance)
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Error, NewState}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let _Ignored = call 'erlang':'!'(FuturePid, {{'rejected', Error}})"
        )?;
        self.write_indent()?;
        writeln!(self.output, "in {{'noreply', NewState}}")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_call/3` callback for sync message sends.
    ///
    /// Per BT-29 design doc, uses `safe_dispatch/3` for error isolation and
    /// returns `{ok, Result}` or `{error, Error}` tuples.
    fn generate_handle_call(&mut self) -> Result<()> {
        writeln!(self.output, "'handle_call'/3 = fun (Msg, _From, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Msg of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{Selector, Args}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Use safe_dispatch for error isolation per BT-29
        writeln!(
            self.output,
            "case call '{}':'safe_dispatch'(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
        // Success case: return {ok, Result}
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'reply', Result, NewState}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', {{'ok', Result}}, NewState}}")?;
        self.indent -= 1;
        // Error case: return {error, Error}
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Error, NewState}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', {{'error', Error}}, NewState}}")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `code_change/3` callback for hot code reload.
    fn generate_code_change(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "'code_change'/3 = fun (_OldVsn, State, _Extra) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "%% TODO: Add state migration logic")?;
        self.write_indent()?;
        writeln!(self.output, "{{'ok', State}}")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `terminate/2` callback for `gen_server` shutdown.
    ///
    /// Per BT-29 design doc, this calls the `terminate` method if defined.
    /// Instance tracking cleanup (BT-96) is automatic via process monitor.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'terminate'/2 = fun (Reason, State) ->
    ///     %% Call terminate method if defined
    ///     case call 'module':'dispatch'('terminate', [Reason], State) of
    ///         <{'reply', _Result, _State}> when 'true' -> 'ok'
    ///         <_Other> when 'true' -> 'ok'
    ///     end
    /// ```
    fn generate_terminate(&mut self, _module: &Module) -> Result<()> {
        writeln!(self.output, "'terminate'/2 = fun (Reason, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% Call terminate method if defined (Flavors pattern)"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call '{}':'dispatch'('terminate', [Reason], State) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        // Core Erlang doesn't allow duplicate _ patterns, use unique ignored vars
        writeln!(
            self.output,
            "<{{'reply', _TermResult, _TermState}}> when 'true' -> 'ok'"
        )?;
        self.write_indent()?;
        // dispatch returns 3-tuple {'error', Error, State}, not 2-tuple
        writeln!(
            self.output,
            "<{{'error', _TermError, _TermState2}}> when 'true' -> 'ok'"
        )?;
        self.write_indent()?;
        writeln!(self.output, "<_TermOther> when 'true' -> 'ok'")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `safe_dispatch/3` function with error isolation.
    ///
    /// Per BT-29 design doc (following LFE Flavors pattern), errors in method
    /// dispatch are caught and returned to the caller rather than crashing
    /// the actor instance.
    ///
    /// Note: Core Erlang try expression uses simple variable patterns (not case-style).
    /// Stacktrace is captured but not returned to caller to prevent information leakage.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'safe_dispatch'/3 = fun (Selector, Args, State) ->
    ///     try call 'module':'dispatch'(Selector, Args, State)
    ///     of Result -> Result
    ///     catch <Type, Error, _Stacktrace> ->
    ///         {'error', {Type, Error}, State}
    /// ```
    fn generate_safe_dispatch(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "'safe_dispatch'/3 = fun (Selector, Args, State) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        // Core Erlang try uses simple variable patterns in of/catch, not case-style <Pattern> when Guard
        writeln!(
            self.output,
            "try call '{}':'dispatch'(Selector, Args, State)",
            self.module_name
        )?;
        self.write_indent()?;
        writeln!(self.output, "of Result -> Result")?;
        self.write_indent()?;
        // Capture but don't return stacktrace to prevent information leakage to callers
        writeln!(
            self.output,
            "catch <Type, Error, _Stacktrace> -> {{'error', {{Type, Error}}, State}}"
        )?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the dispatch/3 function for message routing.
    ///
    /// This function is complex because it handles both legacy expression-based modules
    /// and class definitions, including the `doesNotUnderstand:args:` fallback per BT-29.
    #[expect(
        clippy::too_many_lines,
        reason = "dispatch codegen is inherently complex"
    )]
    fn generate_dispatch(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'dispatch'/3 = fun (Selector, Args, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Selector of")?;
        self.indent += 1;

        // Generate case clause for each method in the module (legacy expression-based)
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let (Expression::Identifier(id), Expression::Block(block)) =
                    (target.as_ref(), value.as_ref())
                {
                    // Reset state version at the start of each method
                    self.reset_state_version();

                    // Push a new scope for this method's parameter bindings
                    self.push_scope();

                    self.write_indent()?;
                    write!(self.output, "<'{}'> when 'true' ->", id.name)?;
                    writeln!(self.output)?;
                    self.indent += 1;

                    // Bind block parameters from Args list
                    if !block.parameters.is_empty() {
                        self.write_indent()?;
                        write!(self.output, "case Args of")?;
                        writeln!(self.output)?;
                        self.indent += 1;

                        self.write_indent()?;
                        write!(self.output, "<[")?;
                        for (i, param) in block.parameters.iter().enumerate() {
                            if i > 0 {
                                write!(self.output, ", ")?;
                            }
                            let var_name = self.fresh_var(&param.name);
                            write!(self.output, "{var_name}")?;
                        }
                        write!(self.output, "]> when 'true' ->")?;
                        writeln!(self.output)?;
                        self.indent += 1;
                    }

                    // Generate the method body with state threading
                    // For state threading to work, we can't wrap in "let Result = ... in"
                    // because that would put State{n} bindings out of scope for the reply.
                    // Instead, we generate the state threading let bindings directly,
                    // and then generate the reply tuple inline.
                    self.write_indent()?;
                    self.generate_method_body_with_reply(block)?;
                    writeln!(self.output)?;

                    if !block.parameters.is_empty() {
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
                }
            }
        }

        // Generate case clauses for methods in class definitions
        for class in &module.classes {
            self.generate_class_method_dispatches(class)?;
        }

        // Default case for unknown messages - try doesNotUnderstand:args: first (per BT-29)
        self.write_indent()?;
        writeln!(self.output, "<OtherSelector> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% Try doesNotUnderstand:args: fallback (BT-29)"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let DnuSelector = 'doesNotUnderstand:args:' in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Methods = call 'maps':'get'('__methods__', State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'maps':'is_key'(DnuSelector, Methods) of"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<'true'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% Call doesNotUnderstand:args: with [Selector, Args]"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "call '{}':'dispatch'(DnuSelector, [OtherSelector, Args], State)",
            self.module_name
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<'false'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% No DNU handler - return unknown_message error"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let ClassName = call 'maps':'get'('__class__', State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'error', {{'unknown_message', OtherSelector, ClassName}}, State}}"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates dispatch case clauses for all methods in a class definition.
    fn generate_class_method_dispatches(&mut self, class: &ClassDefinition) -> Result<()> {
        for method in &class.methods {
            // Only generate dispatch for primary methods for now
            // TODO: Implement before/after/around method combinations
            if method.kind != MethodKind::Primary {
                continue;
            }

            self.generate_method_dispatch(method)?;
        }
        Ok(())
    }

    /// Generates a dispatch case clause for a single method definition.
    fn generate_method_dispatch(&mut self, method: &MethodDefinition) -> Result<()> {
        // Reset state version at the start of each method
        self.reset_state_version();

        // Push a new scope for this method's parameter bindings
        self.push_scope();

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
            for (i, param) in method.parameters.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                let var_name = self.fresh_var(&param.name);
                write!(self.output, "{var_name}")?;
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

    /// Generates the method body with a reply tuple for a `MethodDefinition`.
    ///
    /// This handles the `body: Vec<Expression>` structure of `MethodDefinition`
    /// (as opposed to `Block` which is used for expression-based methods).
    fn generate_method_definition_body_with_reply(
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
                } else {
                    // Regular last expression: bind to Result and reply
                    write!(self.output, "let _Result = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in {{'reply', _Result, {final_state}}}")?;
                }
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                self.generate_field_assignment_open(expr)?;
            } else {
                // Non-field-assignment intermediate expression: wrap in let
                let tmp_var = self.fresh_temp_var("seq");
                write!(self.output, "let {tmp_var} = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in ")?;
            }
        }

        Ok(())
    }

    /// Generates the `method_table/0` function.
    fn generate_method_table(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'method_table'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        write!(self.output, "~{{")?;

        // Collect methods from expression-based definitions (legacy)
        let mut methods: Vec<(String, usize)> = module
            .expressions
            .iter()
            .filter_map(|expr| {
                if let Expression::Assignment { target, value, .. } = expr {
                    if let (Expression::Identifier(id), Expression::Block(block)) =
                        (target.as_ref(), value.as_ref())
                    {
                        return Some((id.name.to_string(), block.arity()));
                    }
                }
                None
            })
            .collect();

        // Collect methods from class definitions
        for class in &module.classes {
            for method in &class.methods {
                // Only include primary methods in the method table for now
                if method.kind == MethodKind::Primary {
                    methods.push((method.selector.name().to_string(), method.selector.arity()));
                }
            }
        }

        for (i, (name, arity)) in methods.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "'{name}' => {arity}")?;
        }

        writeln!(self.output, "}}~")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates code for an expression.
    fn generate_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => self.generate_identifier(id),
            Expression::Super(_) => {
                // Super by itself is not a valid expression - it must be used
                // as a message receiver (e.g., `super increment`)
                Err(CodeGenError::UnsupportedFeature {
                    feature: "'super' must be used with a message send".to_string(),
                    location: format!("{:?}", expr.span()),
                })
            }
            Expression::Block(block) => self.generate_block(block),
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => self.generate_message_send(receiver, selector, arguments),
            Expression::Assignment {
                target,
                value,
                span,
            } => {
                // Check if we're storing a block with mutations (ERROR)
                // Per BT-90: only literal blocks in control flow can mutate
                if let Expression::Block(block) = value.as_ref() {
                    Self::validate_stored_closure(block, format!("{span:?}"))?;
                }

                // Check if this is a field assignment (self.field := value)
                if let Expression::FieldAccess {
                    receiver, field, ..
                } = target.as_ref()
                {
                    // Verify the receiver is 'self'
                    if let Expression::Identifier(recv_id) = receiver.as_ref() {
                        if recv_id.name == "self" {
                            // Field assignment: self.field := value
                            // Generate state-threaded update:
                            // let _Val = <value> in let State{n} = maps:put('field', _Val, State{n-1}) in _Val
                            return self.generate_field_assignment(&field.name, value);
                        }
                    }
                    // Field assignment to non-self receiver (e.g., other.field := value)
                    // This is not supported in the current implementation - actors can
                    // only mutate their own state, not the state of other objects.
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "field assignment to non-self receiver".to_string(),
                        location: format!("{:?}", target.span()),
                    });
                }
                // For identifier assignments (e.g., local variables in REPL like `x := 1`),
                // just return the value - REPL handles binding updates externally.
                // In compiled code, local variable assignments should be handled by
                // the block/method scope, but for now we generate just the value.
                self.generate_expression(value)
            }
            Expression::Return { value, .. } => {
                // Return in Core Erlang is just the value
                self.generate_expression(value)
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => self.generate_field_access(receiver, field),
            Expression::Parenthesized { expression, .. } => self.generate_expression(expression),
            Expression::MapLiteral { pairs, .. } => self.generate_map_literal(pairs),
            Expression::Cascade {
                receiver, messages, ..
            } => self.generate_cascade(receiver, messages),
            _ => Err(CodeGenError::UnsupportedFeature {
                feature: format!("expression type: {expr:?}"),
                location: format!("{:?}", expr.span()),
            }),
        }
    }

    /// Generates code for a literal value.
    fn generate_literal(&mut self, lit: &Literal) -> Result<()> {
        match lit {
            Literal::Integer(n) => write!(self.output, "{n}")?,
            Literal::Float(f) => write!(self.output, "{f}")?,
            Literal::String(s) => {
                // Core Erlang binary syntax: #{segment, segment, ...}#
                // Each segment is #<value>(size, units, type, flags)
                write!(self.output, "#{{")?;
                for (i, ch) in s.chars().enumerate() {
                    if i > 0 {
                        write!(self.output, ",")?;
                    }
                    write!(
                        self.output,
                        "#<{}>(8,1,'integer',['unsigned'|['big']])",
                        ch as u32
                    )?;
                }
                write!(self.output, "}}#")?;
            }
            Literal::Symbol(s) => write!(self.output, "'{s}'")?,
            Literal::Character(c) => write!(self.output, "{}", *c as u32)?,
            Literal::Array(elements) => {
                write!(self.output, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ")?;
                    }
                    self.generate_literal(elem)?;
                }
                write!(self.output, "]")?;
            }
        }
        Ok(())
    }

    /// Generates code for an identifier reference.
    fn generate_identifier(&mut self, id: &Identifier) -> Result<()> {
        // Handle special reserved identifiers as atoms
        match id.name.as_str() {
            "true" => write!(self.output, "'true'")?,
            "false" => write!(self.output, "'false'")?,
            "nil" => write!(self.output, "'nil'")?,
            _ => {
                // Check if it's a bound variable in current or outer scopes
                if let Some(var_name) = self.lookup_var(id.name.as_str()).cloned() {
                    write!(self.output, "{var_name}")?;
                } else {
                    // Field access from state (uses current state variable for state threading)
                    let state_var = self.current_state_var();
                    write!(self.output, "call 'maps':'get'('{}', {state_var})", id.name)?;
                }
            }
        }
        Ok(())
    }

    /// Generates code for a map literal: `#{key => value, ...}`
    fn generate_map_literal(&mut self, pairs: &[MapPair]) -> Result<()> {
        if pairs.is_empty() {
            write!(self.output, "~{{}}~")?;
            return Ok(());
        }

        write!(self.output, "~{{ ")?;

        for (i, pair) in pairs.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }

            // Generate the key
            self.generate_expression(&pair.key)?;
            write!(self.output, " => ")?;

            // Generate the value
            self.generate_expression(&pair.value)?;
        }

        write!(self.output, " }}~")?;
        Ok(())
    }

    /// Generates code for field access (e.g., self.value).
    fn generate_field_access(&mut self, receiver: &Expression, field: &Identifier) -> Result<()> {
        // For now, assume receiver is 'self' and access from State
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
                let state_var = self.current_state_var();
                write!(
                    self.output,
                    "call 'maps':'get'('{}', {state_var})",
                    field.name
                )?;
                return Ok(());
            }
        }

        Err(CodeGenError::UnsupportedFeature {
            feature: "complex field access".to_string(),
            location: format!("{:?}", receiver.span()),
        })
    }

    /// Generates code for a field assignment (self.field := value).
    ///
    /// Uses state threading to simulate mutation in Core Erlang:
    /// ```erlang
    /// let _Val = <value> in
    /// let State{n} = call 'maps':'put'('fieldName', _Val, State{n-1}) in
    /// _Val
    /// ```
    ///
    /// The assignment returns the assigned value (Smalltalk semantics).
    fn generate_field_assignment(&mut self, field_name: &str, value: &Expression) -> Result<()> {
        let val_var = self.fresh_temp_var("Val");

        // Capture current state BEFORE generating value expression,
        // because the value expression may reference state (e.g., self.value + 1)
        let current_state = self.current_state_var();

        // let _Val = <value> in
        write!(self.output, "let {val_var} = ")?;
        self.generate_expression(value)?;

        // Now increment state version for the new state after assignment
        let new_state = self.next_state_var();

        // let State{n} = call 'maps':'put'('field', _Val, State{n-1}) in
        write!(
            self.output,
            " in let {new_state} = call 'maps':'put'('{field_name}', {val_var}, {current_state}) in "
        )?;

        // _Val (assignment returns the assigned value)
        write!(self.output, "{val_var}")?;

        Ok(())
    }

    /// Generates code for a block (closure).
    fn generate_block(&mut self, block: &Block) -> Result<()> {
        // Push a new scope for block parameters
        self.push_scope();

        write!(self.output, "fun (")?;
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            let var_name = self.fresh_var(&param.name);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ") -> ")?;
        self.generate_block_body(block)?;

        // Pop the scope when done with the block
        self.pop_scope();
        Ok(())
    }

    /// Generates a method body with the reply tuple embedded.
    ///
    /// This is used for actor method dispatch to ensure state threading works correctly.
    /// The generated code looks like:
    /// ```erlang
    /// let _Val1 = <value1> in let State1 = ... in
    /// let _Val2 = <value2> in let State2 = ... in
    /// {'reply', <final_value>, State2}
    /// ```
    ///
    /// This ensures that State{n} bindings are in scope when generating the reply tuple.
    #[allow(clippy::too_many_lines)]
    fn generate_method_body_with_reply(&mut self, block: &Block) -> Result<()> {
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

                // Manually generate super_dispatch call with current state (not via generate_expression)
                if let Expression::MessageSend {
                    selector,
                    arguments,
                    ..
                } = expr
                {
                    let selector_atom = match selector {
                        MessageSelector::Unary(name) => name.to_string(),
                        MessageSelector::Binary(op) => op.to_string(),
                        MessageSelector::Keyword(parts) => {
                            parts.iter().map(|p| p.keyword.as_str()).collect::<String>()
                        }
                    };
                    write!(
                        self.output,
                        "let {super_result_var} = call 'beamtalk_classes':'super_dispatch'({current_state}, '{selector_atom}', ["
                    )?;
                    for (i, arg) in arguments.iter().enumerate() {
                        if i > 0 {
                            write!(self.output, ", ")?;
                        }
                        self.generate_expression(arg)?;
                    }
                    write!(self.output, "])")?;
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

    /// Generates the body of a block.
    fn generate_block_body(&mut self, block: &Block) -> Result<()> {
        if block.body.is_empty() {
            write!(self.output, "'nil'")?;
            return Ok(());
        }

        // Generate body expressions in sequence
        // For state threading to work correctly, field assignments must leave
        // their let bindings OPEN so that State{n} is visible to subsequent expressions.
        //
        // For a block like: [self.value := self.value + 1. ^self.value]
        // We need:
        //   let _Val1 = ... in let State1 = ... in <return expression>
        // NOT:
        //   let _seq1 = (let _Val1 = ... in let State1 = ... in _Val1) in <return expression>
        //
        // The difference is crucial: in the first form, State1 is visible in <return expression>.
        //
        // Similarly, for local variable assignments like: [count := 0. count + 1]
        // We need:
        //   let Count = 0 in Count + 1
        // NOT:
        //   let _seq1 = 0 in <expression that can't see Count>

        for (i, expr) in block.body.iter().enumerate() {
            let is_last = i == block.body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);
            let is_local_assignment = Self::is_local_var_assignment(expr);

            if is_last {
                // Last expression: generate directly (its value is the block's result)
                self.generate_expression(expr)?;
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                // This leaves the let bindings open for subsequent expressions
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
                        // Determine the Core Erlang variable name:
                        // - If the variable is already bound (e.g. block parameter), reuse that Core var.
                        // - Otherwise, create a new Core Erlang variable name.
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                        // Generate: let VarName = <value> in ...
                        // Important: do NOT update the mapping before generating the RHS,
                        // so that any uses of the variable in the RHS see the previous binding.
                        write!(self.output, "let {core_var} = ")?;
                        self.generate_expression(value)?;
                        // Now update the mapping so subsequent expressions see this binding.
                        self.bind_var(var_name, &core_var);
                        write!(self.output, " in ")?;
                    }
                }
            } else if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(expr) {
                // whileTrue:/whileFalse:/timesRepeat: with mutations - need to rebind threaded vars after loop
                // For single var, the loop returns its final value directly
                // For multiple vars, we'd need a tuple (not yet supported)
                if threaded_vars.len() == 1 {
                    let var = &threaded_vars[0];
                    // Get the Core Erlang variable name for this var
                    let core_var = self
                        .lookup_var(var)
                        .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                    // Generate: let CoreVar = <loop> in ...
                    // This rebinds the variable to the loop's result
                    write!(self.output, "let {core_var} = ")?;
                    self.generate_expression(expr)?;
                    // The binding already points to core_var, no need to update
                    write!(self.output, " in ")?;
                } else {
                    // Multiple threaded vars - fall back to simple wrapper for now
                    // TODO: Support multiple vars with tuple destructuring
                    let tmp_var = self.fresh_temp_var("seq");
                    write!(self.output, "let {tmp_var} = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in ")?;
                }
            } else if Self::is_super_message_send(expr) {
                // Super message send in block: thread state from {reply, Result, NewState}
                let super_result_var = self.fresh_temp_var("SuperReply");
                let current_state = self.current_state_var();
                let new_state = self.next_state_var();

                // Manually generate super_dispatch call with current state
                if let Expression::MessageSend {
                    selector,
                    arguments,
                    ..
                } = expr
                {
                    let selector_atom = match selector {
                        MessageSelector::Unary(name) => name.to_string(),
                        MessageSelector::Binary(op) => op.to_string(),
                        MessageSelector::Keyword(parts) => {
                            parts.iter().map(|p| p.keyword.as_str()).collect::<String>()
                        }
                    };
                    write!(
                        self.output,
                        "let {super_result_var} = call 'beamtalk_classes':'super_dispatch'({current_state}, '{selector_atom}', ["
                    )?;
                    for (i, arg) in arguments.iter().enumerate() {
                        if i > 0 {
                            write!(self.output, ", ")?;
                        }
                        self.generate_expression(arg)?;
                    }
                    write!(self.output, "])")?;
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

    /// Check if an expression is a field assignment (self.field := value)
    fn is_field_assignment(expr: &Expression) -> bool {
        if let Expression::Assignment { target, .. } = expr {
            if let Expression::FieldAccess { receiver, .. } = target.as_ref() {
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    return recv_id.name == "self";
                }
            }
        }
        false
    }

    /// Check if an expression is a local variable assignment (identifier := value)
    fn is_local_var_assignment(expr: &Expression) -> bool {
        if let Expression::Assignment { target, .. } = expr {
            matches!(target.as_ref(), Expression::Identifier(_))
        } else {
            false
        }
    }

    /// Check if an expression is a super message send (super method call)
    fn is_super_message_send(expr: &Expression) -> bool {
        if let Expression::MessageSend { receiver, .. } = expr {
            matches!(receiver.as_ref(), Expression::Super(_))
        } else {
            false
        }
    }

    /// Check if an expression is a control flow construct (whileTrue:, whileFalse:, timesRepeat:, etc.)
    /// with literal blocks that has threaded mutations. Returns the threaded variable names if so.
    #[allow(clippy::too_many_lines)]
    fn get_control_flow_threaded_vars(expr: &Expression) -> Option<Vec<String>> {
        use crate::codegen::core_erlang::block_analysis::analyze_block;

        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return None;
        };

        let selector_name: String = parts.iter().map(|kw| kw.keyword.as_str()).collect();

        // Check for whileTrue:/whileFalse: with literal blocks
        if (selector_name == "whileTrue:" || selector_name == "whileFalse:")
            && matches!(receiver.as_ref(), Expression::Block(_))
            && arguments
                .first()
                .is_some_and(|a| matches!(a, Expression::Block(_)))
        {
            // Extract blocks - the matches! above guarantees these will succeed
            let Expression::Block(cond_block) = receiver.as_ref() else {
                return None;
            };
            let Some(Expression::Block(body_block)) = arguments.first() else {
                return None;
            };

            let cond_analysis = analyze_block(cond_block);
            let body_analysis = analyze_block(body_block);

            // Combine reads and writes from both blocks
            let all_reads: std::collections::HashSet<String> = cond_analysis
                .local_reads
                .union(&body_analysis.local_reads)
                .cloned()
                .collect();
            let all_writes: std::collections::HashSet<String> = cond_analysis
                .local_writes
                .union(&body_analysis.local_writes)
                .cloned()
                .collect();

            // Threaded vars are those that are both read AND written
            let threaded: Vec<String> = all_reads.intersection(&all_writes).cloned().collect();

            if !threaded.is_empty() {
                return Some(threaded);
            }
        }

        // Check for timesRepeat: with literal block
        if selector_name == "timesRepeat:"
            && arguments
                .first()
                .is_some_and(|a| matches!(a, Expression::Block(_)))
        {
            let Some(Expression::Block(body_block)) = arguments.first() else {
                return None;
            };

            let body_analysis = analyze_block(body_block);

            // Threaded vars are those that are both read AND written
            let threaded: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .cloned()
                .collect();

            if !threaded.is_empty() {
                return Some(threaded);
            }
        }

        // Check for to:do: with literal block
        if selector_name == "to:do:"
            && arguments.len() == 2
            && matches!(&arguments[1], Expression::Block(_))
        {
            let Expression::Block(body_block) = &arguments[1] else {
                return None;
            };

            let body_analysis = analyze_block(body_block);

            // The block parameter (e.g., `n` in `[:n | ...]`) is NOT a threaded variable
            let block_params: std::collections::HashSet<String> = body_block
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect();

            // Threaded vars are those that are both read AND written, but NOT block params
            let threaded: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .filter(|v| !block_params.contains(*v))
                .cloned()
                .collect();

            if !threaded.is_empty() {
                return Some(threaded);
            }
        }

        // Check for do: (list iteration) with literal block
        if selector_name == "do:"
            && arguments
                .first()
                .is_some_and(|a| matches!(a, Expression::Block(_)))
        {
            let Some(Expression::Block(body_block)) = arguments.first() else {
                return None;
            };

            let body_analysis = analyze_block(body_block);

            // The block parameter (e.g., `item` in `[:item | ...]`) is NOT a threaded variable
            let block_params: std::collections::HashSet<String> = body_block
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect();

            // Threaded vars are those that are both read AND written, but NOT block params
            let threaded: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .filter(|v| !block_params.contains(*v))
                .cloned()
                .collect();

            if !threaded.is_empty() {
                return Some(threaded);
            }
        }

        // Check for inject:into: with literal block
        if selector_name == "inject:into:"
            && arguments.len() == 2
            && matches!(&arguments[1], Expression::Block(_))
        {
            let Expression::Block(body_block) = &arguments[1] else {
                return None;
            };

            let body_analysis = analyze_block(body_block);

            // The block parameters (acc, item) are NOT threaded variables
            let block_params: std::collections::HashSet<String> = body_block
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect();

            // Threaded vars are those that are both read AND written, but NOT block params
            let threaded: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .filter(|v| !block_params.contains(*v))
                .cloned()
                .collect();

            if !threaded.is_empty() {
                return Some(threaded);
            }
        }

        None
    }

    /// Validates that a stored closure (block assigned to variable) doesn't contain mutations.
    /// Returns an error for field assignments, logs a warning for local mutations.
    ///
    /// Per BT-90 design: only literal blocks in control flow positions can mutate.
    fn validate_stored_closure(block: &Block, span_str: String) -> Result<()> {
        use crate::codegen::core_erlang::block_analysis::analyze_block;

        let analysis = analyze_block(block);

        // ERROR: Field assignments in stored closures are not allowed
        if !analysis.field_writes.is_empty() {
            let field = analysis
                .field_writes
                .iter()
                .next()
                .expect("field_writes is non-empty");
            let field_capitalized = {
                let mut chars = field.chars();
                chars
                    .next()
                    .map(|c| c.to_uppercase().to_string())
                    .unwrap_or_default()
                    + chars.as_str()
            };
            return Err(CodeGenError::FieldAssignmentInStoredClosure {
                field: field.clone(),
                field_capitalized,
                location: span_str,
            });
        }

        // WARNING: Local mutations in stored closures won't work as expected
        // Note: For now we're treating this as an error too, but the error type
        // is labeled as a warning in the message.
        if !analysis.local_writes.is_empty() {
            let variable = analysis
                .local_writes
                .iter()
                .next()
                .expect("local_writes is non-empty");
            return Err(CodeGenError::LocalMutationInStoredClosure {
                variable: variable.clone(),
                location: span_str,
            });
        }

        Ok(())
    }

    /// Generate a field assignment WITHOUT the closing value.
    /// This is used when the assignment is not the last expression in a block,
    /// so that the State binding extends to subsequent expressions.
    fn generate_field_assignment_open(&mut self, expr: &Expression) -> Result<()> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                let current_state = self.current_state_var();

                // let _Val = <value> in
                write!(self.output, "let {val_var} = ")?;
                self.generate_expression(value)?;

                // Increment state version for the new state after assignment
                let new_state = self.next_state_var();

                // let State{n} = call 'maps':'put'('field', _Val, State{n-1}) in
                // Note: we do NOT close with the value - subsequent expressions are the body
                write!(
                    self.output,
                    " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in ",
                    field.name
                )?;

                return Ok(());
            }
        }
        // Fallback: should not reach here if is_field_assignment check was correct
        self.generate_expression(expr)
    }

    /// Generates code for a super message send.
    ///
    /// Super calls use `beamtalk_classes:super_dispatch/3` to invoke the superclass implementation.
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// super increment
    /// super getValue
    /// super at: 1 put: value
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// call 'beamtalk_classes':'super_dispatch'(State, 'increment', [])
    /// call 'beamtalk_classes':'super_dispatch'(State, 'getValue', [])
    /// call 'beamtalk_classes':'super_dispatch'(State, 'at:put:', [1, Value])
    /// ```
    fn generate_super_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<()> {
        // Build the selector atom
        let selector_atom = match selector {
            MessageSelector::Unary(name) => name.to_string(),
            MessageSelector::Binary(op) => op.to_string(),
            MessageSelector::Keyword(parts) => {
                parts.iter().map(|p| p.keyword.as_str()).collect::<String>()
            }
        };

        // Generate: call 'beamtalk_classes':'super_dispatch'(State, 'selector', [Args])
        write!(
            self.output,
            "call 'beamtalk_classes':'super_dispatch'({}, '{selector_atom}', [",
            self.current_state_var()
        )?;

        // Generate arguments
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }

        write!(self.output, "])")?;
        Ok(())
    }

    /// Generates code for a message send.
    ///
    /// Message sends are **asynchronous by default** and return futures.
    /// This implements the async-first protocol:
    /// 1. Create a future process
    /// 2. Send message via `gen_server:cast` with `{Selector, Args, FuturePid}`
    /// 3. Return the future reference
    ///
    /// The receiving actor's `handle_cast/2` will resolve the future when complete.
    ///
    /// **Special cases:**
    /// - Block evaluation messages (`value`, `value:`, `whileTrue:`, `whileFalse:`,
    ///   `repeat`) - These are direct function calls, not async actor messages
    /// - `spawn` - When the selector is "spawn" on a class identifier, generates
    ///   a call to the module's `spawn/0` function instead
    /// - `spawnWith:` - When the selector is "spawnWith:" on a class identifier with
    ///   one argument, generates a call to the module's `spawn/1` function
    /// - `await` - When the selector is "await", generates a blocking await operation
    fn generate_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<()> {
        // Special case: super message send
        // Super calls invoke the superclass implementation
        if matches!(receiver, Expression::Super(_)) {
            return self.generate_super_send(selector, arguments);
        }

        // For binary operators, use Erlang's built-in operators (these are synchronous)
        if let MessageSelector::Binary(op) = selector {
            return self.generate_binary_op(op, receiver, arguments);
        }

        // Special case: Block evaluation messages (value, value:, whileTrue:, etc.)
        // These are synchronous function calls, not async actor messages
        if let Some(result) = self.try_generate_block_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: String methods - synchronous Erlang string operations
        // Check BEFORE Dictionary because both use at: but string handles literal strings
        if let Some(result) = self.try_generate_string_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: Dictionary/Map methods - direct calls to Erlang maps module
        // These are synchronous operations, not async actor messages
        if let Some(result) = self.try_generate_dictionary_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: Boolean methods - synchronous case expressions
        // ifTrue:ifFalse:, and:, or:, not generate direct Erlang case expressions
        if let Some(result) = self.try_generate_boolean_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: Integer methods - synchronous Erlang operations
        // negated, abs, isZero, isEven, isOdd generate direct arithmetic/comparison
        if let Some(result) = self.try_generate_integer_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: List/Array methods - synchronous Erlang list operations
        // do:, collect:, select:, inject:into: generate direct iteration
        if let Some(result) = self.try_generate_list_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: unary "spawn" message on a class/identifier
        // This creates a new actor instance via gen_server:start_link
        if let MessageSelector::Unary(name) = selector {
            if name == "spawn" && arguments.is_empty() {
                if let Expression::Identifier(id) = receiver {
                    // Generate: call 'module':'spawn'()
                    // Convert class name to module name (CamelCase -> snake_case)
                    let module_name = Self::to_module_name(&id.name);
                    write!(self.output, "call '{module_name}':'spawn'()")?;
                    return Ok(());
                }
            }

            // Special case: "await" is a blocking operation on a future
            if name == "await" && arguments.is_empty() {
                return self.generate_await(receiver);
            }
        }

        // Special case: "spawnWith:" keyword message on a class/identifier
        // This creates a new actor instance with initialization arguments
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "spawnWith:" && arguments.len() == 1 {
                if let Expression::Identifier(id) = receiver {
                    // Generate: call 'module':'spawn'(InitArgs)
                    let module_name = Self::to_module_name(&id.name);
                    write!(self.output, "call '{module_name}':'spawn'(")?;
                    self.generate_expression(&arguments[0])?;
                    write!(self.output, ")")?;
                    return Ok(());
                }
            }
        }

        // Generate the async message send protocol:
        // let Receiver1 = <receiver expression>
        // in let Pid1 = call 'erlang':'element'(4, Receiver1)  % Extract pid from #beamtalk_object{}
        // in let Future1 = call 'beamtalk_future':'new'()
        // in let _ = call 'gen_server':'cast'(Pid1, {Selector, Args, Future1})
        //    in Future1
        //
        // Use fresh_var to avoid shadowing in nested message sends

        let receiver_var = self.fresh_var("Receiver");
        let pid_var = self.fresh_var("Pid");
        let future_var = self.fresh_var("Future");

        // Bind receiver to a variable
        write!(self.output, "let {receiver_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in ")?;

        // Extract pid from #beamtalk_object{} record (4th element, 1-indexed)
        // Record tuple is: {'beamtalk_object', Class, ClassMod, Pid}
        write!(
            self.output,
            "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
        )?;

        // Create future
        write!(
            self.output,
            "let {future_var} = call 'beamtalk_future':'new'() in "
        )?;

        // Build the message tuple: {Selector, Args, Future}
        write!(
            self.output,
            "let _ = call 'gen_server':'cast'({pid_var}, {{"
        )?;

        // First element: the selector name as an atom
        write!(self.output, "'")?;
        match selector {
            MessageSelector::Unary(name) => write!(self.output, "{name}")?,
            MessageSelector::Keyword(parts) => {
                let name = parts.iter().map(|p| p.keyword.as_str()).collect::<String>();
                write!(self.output, "{name}")?;
            }
            // Binary selectors are handled above and should not reach here.
            MessageSelector::Binary(op) => {
                return Err(CodeGenError::Internal(format!(
                    "unexpected binary selector in generate_message_send: {op}"
                )));
            }
        }
        write!(self.output, "', [")?;

        // Second element: list of message arguments
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }

        // Third element: the future variable
        write!(self.output, "], {future_var}}}) in {future_var}")?;

        Ok(())
    }

    /// Generates code for await expression.
    ///
    /// Delegates to `beamtalk_future:await/1`, which implements roughly:
    /// ```erlang
    /// FuturePid ! {await, self()},
    /// receive
    ///     {future_resolved, FuturePid, Value} ->
    ///         Value;
    ///     {future_rejected, FuturePid, Reason} ->
    ///         throw({future_rejected, Reason})
    /// end
    /// ```
    ///
    /// For a timed await with timeout handling, use `beamtalk_future:await/2`.
    fn generate_await(&mut self, future: &Expression) -> Result<()> {
        // Delegate to beamtalk_future:await/1, which blocks until resolution/rejection
        write!(self.output, "call 'beamtalk_future':'await'(")?;
        self.generate_expression(future)?;
        write!(self.output, ")")?;
        Ok(())
    }

    /// Generates code for cascade expressions.
    ///
    /// Cascades send multiple messages to the same receiver using semicolon separators.
    /// The receiver is evaluated once and each message is sent to that receiver.
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// collection add: 1; add: 2; add: 3
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// let Receiver = <evaluate collection> in
    ///   let _ = <send add: 1 to Receiver> in
    ///   let _ = <send add: 2 to Receiver> in
    ///   <send add: 3 to Receiver>
    /// ```
    ///
    /// The cascade returns the result of the final message.
    #[expect(
        clippy::too_many_lines,
        reason = "cascade codegen handles both normal and fallback paths"
    )]
    fn generate_cascade(
        &mut self,
        receiver: &Expression,
        messages: &[crate::ast::CascadeMessage],
    ) -> Result<()> {
        if messages.is_empty() {
            // Edge case: cascade with no messages just evaluates to the receiver
            return self.generate_expression(receiver);
        }

        // The parser represents cascades such that `receiver` is the *first*
        // message send expression, e.g. for:
        //
        //   counter increment; increment; getValue
        //
        // `receiver` is a MessageSend for `counter increment`, and `messages`
        // holds the remaining cascade messages. We need to:
        //   1. Evaluate the underlying receiver expression (`counter`) once,
        //      bind it to a temp variable.
        //   2. Send the first message (`increment`) and all subsequent
        //      cascade messages to that same bound receiver.
        if let Expression::MessageSend {
            receiver: underlying_receiver,
            selector: first_selector,
            arguments: first_arguments,
            ..
        } = receiver
        {
            // Bind the underlying receiver once
            let receiver_var = self.fresh_temp_var("Receiver");
            write!(self.output, "let {receiver_var} = ")?;
            self.generate_expression(underlying_receiver)?;
            write!(self.output, " in ")?;

            // Extract pid from #beamtalk_object{} record (4th element, 1-indexed)
            let pid_var = self.fresh_temp_var("Pid");
            write!(
                self.output,
                "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
            )?;

            // Total number of messages in the cascade: first + remaining
            let total_messages = messages.len() + 1;

            for index in 0..total_messages {
                let is_last = index == total_messages - 1;

                if !is_last {
                    // For all but the last message, discard the result
                    write!(self.output, "let _ = ")?;
                }

                // Determine which selector/arguments to use:
                // index 0 -> first message from the initial MessageSend
                // index > 0 -> messages[index - 1]
                let (selector, arguments): (&crate::ast::MessageSelector, &[Expression]) =
                    if index == 0 {
                        (first_selector, first_arguments.as_slice())
                    } else {
                        let msg = &messages[index - 1];
                        (&msg.selector, msg.arguments.as_slice())
                    };

                // Generate async message send to the receiver pid variable
                let future_var = self.fresh_var("Future");
                write!(
                    self.output,
                    "let {future_var} = call 'beamtalk_future':'new'() in "
                )?;
                write!(
                    self.output,
                    "let _ = call 'gen_server':'cast'({pid_var}, {{"
                )?;

                // Selector
                write!(self.output, "'")?;
                match selector {
                    crate::ast::MessageSelector::Unary(name) => write!(self.output, "{name}")?,
                    crate::ast::MessageSelector::Keyword(parts) => {
                        let name = parts.iter().map(|p| p.keyword.as_str()).collect::<String>();
                        write!(self.output, "{name}")?;
                    }
                    crate::ast::MessageSelector::Binary(_) => {
                        return Err(CodeGenError::UnsupportedFeature {
                            feature: "binary selectors in cascades".to_string(),
                            location: "cascade message with binary selector".to_string(),
                        });
                    }
                }
                write!(self.output, "', [")?;

                // Arguments
                for (j, arg) in arguments.iter().enumerate() {
                    if j > 0 {
                        write!(self.output, ", ")?;
                    }
                    self.generate_expression(arg)?;
                }

                write!(self.output, "], {future_var}}}) in {future_var}")?;

                if !is_last {
                    write!(self.output, " in ")?;
                }
            }

            Ok(())
        } else {
            // Fallback: if the receiver is not a MessageSend (which should not
            // happen for well-formed cascades), preserve the previous behavior:
            // evaluate the receiver once and send all cascade messages to it.
            let receiver_var = self.fresh_temp_var("Receiver");
            write!(self.output, "let {receiver_var} = ")?;
            self.generate_expression(receiver)?;
            write!(self.output, " in ")?;

            // Extract pid from #beamtalk_object{} record (4th element, 1-indexed)
            let pid_var = self.fresh_temp_var("Pid");
            write!(
                self.output,
                "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
            )?;

            // Generate each message send, discarding intermediate results
            for (i, message) in messages.iter().enumerate() {
                let is_last = i == messages.len() - 1;

                if !is_last {
                    // For all but the last message, discard the result
                    write!(self.output, "let _ = ")?;
                }

                // Generate async message send to the receiver pid variable
                let future_var = self.fresh_var("Future");
                write!(
                    self.output,
                    "let {future_var} = call 'beamtalk_future':'new'() in "
                )?;
                write!(
                    self.output,
                    "let _ = call 'gen_server':'cast'({pid_var}, {{"
                )?;

                // Selector
                write!(self.output, "'")?;
                match &message.selector {
                    crate::ast::MessageSelector::Unary(name) => write!(self.output, "{name}")?,
                    crate::ast::MessageSelector::Keyword(parts) => {
                        let name = parts.iter().map(|p| p.keyword.as_str()).collect::<String>();
                        write!(self.output, "{name}")?;
                    }
                    crate::ast::MessageSelector::Binary(_) => {
                        return Err(CodeGenError::UnsupportedFeature {
                            feature: "binary selectors in cascades".to_string(),
                            location: "cascade message with binary selector".to_string(),
                        });
                    }
                }
                write!(self.output, "', [")?;

                // Arguments
                for (j, arg) in message.arguments.iter().enumerate() {
                    if j > 0 {
                        write!(self.output, ", ")?;
                    }
                    self.generate_expression(arg)?;
                }

                write!(self.output, "], {future_var}}}) in {future_var}")?;

                if !is_last {
                    write!(self.output, " in ")?;
                }
            }

            Ok(())
        }
    }

    /// Tries to generate code for block evaluation messages.
    ///
    /// Block evaluation messages (`value`, `value:`, `whileTrue:`, etc.) are
    /// direct function calls, not async actor messages. This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a block message and code was generated
    /// - Returns `Ok(None)` if the message is NOT a block message (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Block Evaluation Messages
    ///
    /// - `value` (0 args) → `apply Fun ()`
    /// - `value:` (1 arg) → `apply Fun (Arg1)`
    /// - `value:value:` (2 args) → `apply Fun (Arg1, Arg2)`
    /// - `value:value:value:` (3 args) → `apply Fun (Arg1, Arg2, Arg3)`
    ///
    /// # Control Flow Messages
    ///
    /// - `whileTrue:` → loop while condition block returns true
    /// - `whileFalse:` → loop while condition block returns false
    /// - `repeat` → infinite loop (until return or error)
    fn try_generate_block_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            // `value` - evaluate block with no arguments
            MessageSelector::Unary(name) if name == "value" => {
                self.generate_block_value_call(receiver, &[])?;
                Ok(Some(()))
            }

            // `repeat` - infinite loop
            MessageSelector::Unary(name) if name == "repeat" => {
                self.generate_repeat(receiver)?;
                Ok(Some(()))
            }

            // Keyword messages for block evaluation
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    // `value:`, `value:value:`, `value:value:value:` - evaluate block with args
                    "value:" | "value:value:" | "value:value:value:" => {
                        self.generate_block_value_call(receiver, arguments)?;
                        Ok(Some(()))
                    }

                    // `whileTrue:` - loop while condition block returns true
                    "whileTrue:" => {
                        self.generate_while_true(receiver, arguments)?;
                        Ok(Some(()))
                    }

                    // `whileFalse:` - loop while condition block returns false
                    "whileFalse:" => {
                        self.generate_while_false(receiver, arguments)?;
                        Ok(Some(()))
                    }

                    // `timesRepeat:` - repeat body N times
                    "timesRepeat:" => {
                        self.generate_times_repeat(receiver, arguments)?;
                        Ok(Some(()))
                    }

                    // Not a block evaluation message
                    _ => Ok(None),
                }
            }

            // Not a block evaluation message
            _ => Ok(None),
        }
    }

    /// Tries to generate code for Dictionary/Map methods.
    ///
    /// Dictionary methods are synchronous calls to the Erlang `maps` module.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a Dictionary method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a Dictionary method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Dictionary Methods
    ///
    /// - `at:` (1 arg) → `maps:get(Key, Map)`
    /// - `at:ifAbsent:` (2 args) → `case maps:find(...) of ... end`
    /// - `at:put:` (2 args) → `maps:put(Key, Value, Map)`
    /// - `includesKey:` (1 arg) → `maps:is_key(Key, Map)`
    /// - `removeKey:` (1 arg) → `maps:remove(Key, Map)`
    /// - `keys` (0 args) → `maps:keys(Map)`
    /// - `values` (0 args) → `maps:values(Map)`
    /// - `size` (0 args) → `maps:size(Map)`
    /// - `merge:` (1 arg) → `maps:merge(Map1, Map2)`
    /// - `keysAndValuesDo:` (1 arg block) → `maps:foreach(Fun, Map)`
    ///
    /// # Note
    ///
    /// These selectors are treated as Dictionary operations for ALL receivers.
    /// This means Array/List/Set must use different selectors (e.g., `elementAt:`
    /// instead of `at:`). Future type inference may allow disambiguation.
    fn try_generate_dictionary_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            // Unary messages
            MessageSelector::Unary(name) => match name.as_str() {
                "keys" if arguments.is_empty() => {
                    write!(self.output, "call 'maps':'keys'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "values" if arguments.is_empty() => {
                    write!(self.output, "call 'maps':'values'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "size" if arguments.is_empty() => {
                    write!(self.output, "call 'maps':'size'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            // Keyword messages
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "at:" if arguments.len() == 1 => {
                        // maps:get(Key, Map)
                        write!(self.output, "call 'maps':'get'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "at:ifAbsent:" if arguments.len() == 2 => {
                        // case maps:find(Key, Map) of {ok, V} -> V; error -> Block() end
                        let result_var = self.fresh_var("FindResult");
                        write!(self.output, "case call 'maps':'find'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ") of ")?;
                        write!(
                            self.output,
                            "<{{'ok', {result_var}}}> when 'true' -> {result_var}"
                        )?;
                        write!(self.output, "; <'error'> when 'true' -> apply ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(self.output, " () end")?;
                        Ok(Some(()))
                    }
                    "at:put:" if arguments.len() == 2 => {
                        // maps:put(Key, Value, Map)
                        write!(self.output, "call 'maps':'put'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "includesKey:" if arguments.len() == 1 => {
                        // maps:is_key(Key, Map)
                        write!(self.output, "call 'maps':'is_key'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "removeKey:" if arguments.len() == 1 => {
                        // maps:remove(Key, Map)
                        write!(self.output, "call 'maps':'remove'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "merge:" if arguments.len() == 1 => {
                        // maps:merge(Map1, Map2)
                        write!(self.output, "call 'maps':'merge'(")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ", ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "keysAndValuesDo:" if arguments.len() == 1 => {
                        // maps:foreach(Fun, Map)
                        write!(self.output, "call 'maps':'foreach'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            // Binary selectors - not used for Dictionary methods
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for Boolean methods.
    ///
    /// Boolean methods are synchronous operations that generate Erlang case expressions.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a Boolean method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a Boolean method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Boolean Methods
    ///
    /// - `ifTrue:ifFalse:` (2 args) → case Receiver of true -> `TrueBlock()`; false -> `FalseBlock()` end
    /// - `ifTrue:` (1 arg) → case Receiver of true -> `TrueBlock()`; false -> Receiver end
    /// - `ifFalse:` (1 arg) → case Receiver of true -> Receiver; false -> `FalseBlock()` end
    /// - `and:` (1 arg) → case Receiver of true -> `Block()`; false -> false end (short-circuit)
    /// - `or:` (1 arg) → case Receiver of true -> true; false -> `Block()` end (short-circuit)
    /// - `not` (0 args) → case Receiver of true -> false; false -> true end
    fn try_generate_boolean_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            // Unary messages
            MessageSelector::Unary(name) => match name.as_str() {
                "not" if arguments.is_empty() => {
                    // case Receiver of 'true' -> 'false'; 'false' -> 'true' end
                    let recv_var = self.fresh_temp_var("Bool");
                    write!(self.output, "let {recv_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(
                        self.output,
                        " in case {recv_var} of <'true'> when 'true' -> 'false' <'false'> when 'true' -> 'true' end"
                    )?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            // Keyword messages
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "ifTrue:ifFalse:" if arguments.len() == 2 => {
                        // case Receiver of 'true' -> apply TrueBlock (); 'false' -> apply FalseBlock () end
                        let recv_var = self.fresh_temp_var("Bool");
                        let true_var = self.fresh_temp_var("TrueBlk");
                        let false_var = self.fresh_temp_var("FalseBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {true_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in let {false_var} = ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> apply {true_var} () <'false'> when 'true' -> apply {false_var} () end"
                        )?;
                        Ok(Some(()))
                    }
                    "ifTrue:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> apply TrueBlock (); 'false' -> Receiver end
                        let recv_var = self.fresh_temp_var("Bool");
                        let true_var = self.fresh_temp_var("TrueBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {true_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> apply {true_var} () <'false'> when 'true' -> {recv_var} end"
                        )?;
                        Ok(Some(()))
                    }
                    "ifFalse:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> Receiver; 'false' -> apply FalseBlock () end
                        let recv_var = self.fresh_temp_var("Bool");
                        let false_var = self.fresh_temp_var("FalseBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {false_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> {recv_var} <'false'> when 'true' -> apply {false_var} () end"
                        )?;
                        Ok(Some(()))
                    }
                    "and:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> apply Block (); 'false' -> 'false' end
                        let recv_var = self.fresh_temp_var("Bool");
                        let block_var = self.fresh_temp_var("AndBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {block_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> apply {block_var} () <'false'> when 'true' -> 'false' end"
                        )?;
                        Ok(Some(()))
                    }
                    "or:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> 'true'; 'false' -> apply Block () end
                        let recv_var = self.fresh_temp_var("Bool");
                        let block_var = self.fresh_temp_var("OrBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {block_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> 'true' <'false'> when 'true' -> apply {block_var} () end"
                        )?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            // Binary selectors - not used for Boolean methods
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for Integer methods.
    ///
    /// Integer methods are synchronous operations that generate direct Erlang arithmetic.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was an Integer method and code was generated
    /// - Returns `Ok(None)` if the message is NOT an Integer method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Integer Methods
    ///
    /// - `negated` (0 args) → `0 - Receiver`
    /// - `abs` (0 args) → case Receiver < 0 of true -> 0 - Receiver; false -> Receiver end
    /// - `isZero` (0 args) → `Receiver =:= 0`
    /// - `isEven` (0 args) → `Receiver rem 2 =:= 0`
    /// - `isOdd` (0 args) → `Receiver rem 2 =/= 0`
    /// - `to:do:` (2 args) → range iteration with body block
    fn try_generate_integer_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "negated" if arguments.is_empty() => {
                    // call 'erlang':'-'(0, Receiver)
                    write!(self.output, "call 'erlang':'-'(0, ")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "abs" if arguments.is_empty() => {
                    // case call 'erlang':'<'(N, 0) of 'true' -> call 'erlang':'-'(0, N); 'false' -> N end
                    let n_var = self.fresh_temp_var("N");
                    write!(self.output, "let {n_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(
                        self.output,
                        " in case call 'erlang':'<'({n_var}, 0) of <'true'> when 'true' -> call 'erlang':'-'(0, {n_var}) <'false'> when 'true' -> {n_var} end"
                    )?;
                    Ok(Some(()))
                }
                "isZero" if arguments.is_empty() => {
                    // call 'erlang':'=:='(Receiver, 0)
                    write!(self.output, "call 'erlang':'=:='(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", 0)")?;
                    Ok(Some(()))
                }
                "isEven" if arguments.is_empty() => {
                    // call 'erlang':'=:='(call 'erlang':'rem'(Receiver, 2), 0)
                    write!(self.output, "call 'erlang':'=:='(call 'erlang':'rem'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", 2), 0)")?;
                    Ok(Some(()))
                }
                "isOdd" if arguments.is_empty() => {
                    // call 'erlang':'=/='(call 'erlang':'rem'(Receiver, 2), 0)
                    write!(self.output, "call 'erlang':'=/='(call 'erlang':'rem'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", 2), 0)")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "to:do:" if arguments.len() == 2 => {
                        // 1 to: 10 do: [:n | body]
                        // receiver = start, arguments[0] = end, arguments[1] = body block
                        self.generate_to_do(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            // No binary Integer methods handled here
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for List/Array methods.
    ///
    /// List methods are synchronous operations that generate direct Erlang list calls.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a List method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a List method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # List Methods
    ///
    /// - `do:` (1 arg block) → iterate over elements with side effects
    /// - `collect:` (1 arg block) → map to new list
    /// - `select:` (1 arg block) → filter elements
    /// - `reject:` (1 arg block) → filter out elements
    /// - `inject:into:` (2 args) → fold with accumulator
    /// - `first` (0 args) → `hd(List)`
    /// - `rest` (0 args) → `tl(List)`
    /// - `size` (0 args) → `length(List)`
    /// - `isEmpty` (0 args) → `List =:= []`
    fn try_generate_list_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "first" if arguments.is_empty() => {
                    // call 'erlang':'hd'(Receiver)
                    write!(self.output, "call 'erlang':'hd'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "rest" if arguments.is_empty() => {
                    // call 'erlang':'tl'(Receiver)
                    write!(self.output, "call 'erlang':'tl'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "size" if arguments.is_empty() => {
                    // call 'erlang':'length'(Receiver)
                    write!(self.output, "call 'erlang':'length'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "isEmpty" if arguments.is_empty() => {
                    // call 'erlang':'=:='(Receiver, [])
                    write!(self.output, "call 'erlang':'=:='(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", [])")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "do:" if arguments.len() == 1 => {
                        // list do: [:item | body]
                        self.generate_list_do(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "collect:" if arguments.len() == 1 => {
                        // list collect: [:item | transform]
                        self.generate_list_collect(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "select:" if arguments.len() == 1 => {
                        // list select: [:item | predicate]
                        self.generate_list_select(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "reject:" if arguments.len() == 1 => {
                        // list reject: [:item | predicate]
                        self.generate_list_reject(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "inject:into:" if arguments.len() == 2 => {
                        // list inject: 0 into: [:acc :item | acc + item]
                        self.generate_list_inject(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Generates a `do:` iteration over a list.
    ///
    /// Beamtalk: `list do: [:item | body]`
    ///
    /// Core Erlang (simple, no mutations):
    /// ```erlang
    /// let _List = <list> in let _Body = <body> in
    /// call 'lists':'foreach'(_Body, _List)
    /// ```
    fn generate_list_do(&mut self, receiver: &Expression, body: &Expression) -> Result<()> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let body_analysis = block_analysis::analyze_block(body_block);

            // The block parameter (e.g., `item` in `[:item | ...]`) is NOT a threaded variable
            let block_params: std::collections::HashSet<String> = body_block
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect();

            // Variables that need threading: read AND written, but NOT block params
            let threaded_vars: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .filter(|v| !block_params.contains(*v))
                .cloned()
                .collect();

            if !threaded_vars.is_empty() {
                // Generate tail-recursive loop with mutation support
                return self.generate_list_do_with_mutations(receiver, body_block, &threaded_vars);
            }
        }

        // Fall back to simple lists:foreach (no mutations)
        let list_var = self.fresh_temp_var("List");
        let body_var = self.fresh_temp_var("Body");

        write!(self.output, "let {list_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in call 'lists':'foreach'({body_var}, {list_var})"
        )?;

        Ok(())
    }

    /// Generates a `do:` iteration over a list with mutation support.
    fn generate_list_do_with_mutations(
        &mut self,
        receiver: &Expression,
        body_block: &Block,
        threaded_vars: &[String],
    ) -> Result<()> {
        let loop_fn = self.fresh_temp_var("Loop");
        let list_var = self.fresh_temp_var("List");
        let head_var = self.fresh_temp_var("H");
        let tail_var = self.fresh_temp_var("T");
        // Arity: 1 for list, plus one for each threaded variable
        let arity = 1 + threaded_vars.len();

        // Convert variable names to Core Erlang format
        let core_vars: Vec<String> = threaded_vars
            .iter()
            .map(|v| Self::to_core_erlang_var(v))
            .collect();

        // Get the block parameter name for item variable (if any)
        let item_param = body_block.parameters.first().map(|p| p.name.as_str());

        // let _List = <list> in
        write!(self.output, "let {list_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in ")?;

        // Generate letrec with List and threaded vars as parameters
        write!(self.output, "letrec '{loop_fn}'/{arity} = fun ({list_var}")?;
        for core_var in &core_vars {
            write!(self.output, ", {core_var}")?;
        }
        write!(self.output, ") -> ")?;

        // Push a new scope with the loop variables bound
        self.push_scope();
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            self.bind_var(var, core_var);
        }

        // case List of
        //   <[]> when 'true' -> ThreadedVars
        //   <[H|T]> when 'true' -> <body> apply Loop (T, UpdatedVars)
        write!(self.output, "case {list_var} of ")?;

        // Empty list case: return final values
        write!(self.output, "<[]> when 'true' -> ")?;
        if core_vars.len() == 1 {
            write!(self.output, "{}", &core_vars[0])?;
        } else {
            write!(self.output, "{{")?;
            for (i, core_var) in core_vars.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "{core_var}")?;
            }
            write!(self.output, "}}")?;
        }

        // Non-empty list case: [H|T]
        write!(self.output, " <[{head_var}|{tail_var}]> when 'true' -> ")?;

        // Bind item parameter if it exists
        if let Some(param) = item_param {
            self.bind_var(param, &head_var);
        }

        // Generate body and extract new variable values
        self.generate_list_do_body_with_threading(
            body_block,
            threaded_vars,
            &core_vars,
            &loop_fn,
            arity,
            &tail_var,
        )?;

        write!(self.output, " end ")?;

        // Pop the scope
        self.pop_scope();

        // Initial call: apply '_Loop'/N (List, Var1, Var2, ...)
        write!(self.output, "in apply '{loop_fn}'/{arity} ({list_var}")?;
        for var in threaded_vars {
            write!(self.output, ", ")?;
            let var_name = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates the body of a list do: loop that threads mutated variables.
    fn generate_list_do_body_with_threading(
        &mut self,
        body_block: &Block,
        threaded_vars: &[String],
        core_vars: &[String],
        loop_fn: &str,
        arity: usize,
        tail_var: &str,
    ) -> Result<()> {
        if body_block.body.is_empty() {
            // Empty body - just recurse with tail and same values
            write!(self.output, "apply '{loop_fn}'/{arity} ({tail_var}")?;
            for core_var in core_vars {
                write!(self.output, ", {core_var}")?;
            }
            write!(self.output, ")")?;
            return Ok(());
        }

        // Track which variables have been assigned new values
        let mut assigned_vars: std::collections::HashSet<String> = std::collections::HashSet::new();

        // Process each expression in the body
        for (i, expr) in body_block.body.iter().enumerate() {
            let is_last = i == body_block.body.len() - 1;

            match expr {
                Expression::Assignment { target, value, .. } => {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = id.name.as_str();
                        if threaded_vars.contains(&var_name.to_string()) {
                            let core_var = Self::to_core_erlang_var(var_name);
                            let new_var = format!("{core_var}1");
                            write!(self.output, "let {new_var} = ")?;
                            self.generate_expression(value)?;
                            write!(self.output, " in ")?;
                            assigned_vars.insert(var_name.to_string());
                            continue;
                        }
                    }
                    // Not a threaded variable
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
                _ => {
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
            }
        }

        // Recurse with tail and updated values
        write!(self.output, " apply '{loop_fn}'/{arity} ({tail_var}")?;
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            write!(self.output, ", ")?;
            if assigned_vars.contains(var) {
                write!(self.output, "{core_var}1")?;
            } else {
                write!(self.output, "{core_var}")?;
            }
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates a `collect:` map over a list.
    fn generate_list_collect(&mut self, receiver: &Expression, body: &Expression) -> Result<()> {
        let list_var = self.fresh_temp_var("List");
        let body_var = self.fresh_temp_var("Body");

        write!(self.output, "let {list_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in call 'lists':'map'({body_var}, {list_var})"
        )?;

        Ok(())
    }

    /// Generates a `select:` filter over a list.
    fn generate_list_select(&mut self, receiver: &Expression, body: &Expression) -> Result<()> {
        let list_var = self.fresh_temp_var("List");
        let body_var = self.fresh_temp_var("Body");

        write!(self.output, "let {list_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in call 'lists':'filter'({body_var}, {list_var})"
        )?;

        Ok(())
    }

    /// Generates a `reject:` filter over a list (inverse of select:).
    fn generate_list_reject(&mut self, receiver: &Expression, body: &Expression) -> Result<()> {
        let list_var = self.fresh_temp_var("List");
        let body_var = self.fresh_temp_var("Body");

        // Use lists:filter with a predicate that inverts the result of the body function
        write!(self.output, "let {list_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in call 'lists':'filter'(fun (X) -> call 'erlang':'not'(apply {body_var} (X)) end, {list_var})"
        )?;

        Ok(())
    }

    /// Generates an `inject:into:` fold over a list.
    fn generate_list_inject(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let body_analysis = block_analysis::analyze_block(body_block);

            // The block parameters (e.g., `acc` and `item` in `[:acc :item | ...]`) are NOT threaded
            let block_params: std::collections::HashSet<String> = body_block
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect();

            // Variables that need threading: read AND written, but NOT block params
            let threaded_vars: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .filter(|v| !block_params.contains(*v))
                .cloned()
                .collect();

            if !threaded_vars.is_empty() {
                // Generate tail-recursive loop with mutation support
                return self.generate_list_inject_with_mutations(
                    receiver,
                    initial,
                    body_block,
                    &threaded_vars,
                );
            }
        }

        // Fall back to simple lists:foldl (no mutations)
        let list_var = self.fresh_temp_var("List");
        let init_var = self.fresh_temp_var("Init");
        let body_var = self.fresh_temp_var("Body");

        write!(self.output, "let {list_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in let {init_var} = ")?;
        self.generate_expression(initial)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        // lists:foldl(Fun, Acc0, List) - Fun(Elem, Acc) -> Acc1
        // Beamtalk uses [:acc :item | ...] so we need to swap the arguments
        write!(
            self.output,
            " in call 'lists':'foldl'(fun (Item, Acc) -> apply {body_var} (Acc, Item) end, {init_var}, {list_var})"
        )?;

        Ok(())
    }

    /// Generates an `inject:into:` fold with mutation support.
    fn generate_list_inject_with_mutations(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body_block: &Block,
        threaded_vars: &[String],
    ) -> Result<()> {
        let loop_fn = self.fresh_temp_var("Loop");
        let list_var = self.fresh_temp_var("List");
        let acc_var = self.fresh_temp_var("Acc");
        let head_var = self.fresh_temp_var("H");
        let tail_var = self.fresh_temp_var("T");
        // Arity: 2 for list + acc, plus one for each threaded variable
        let arity = 2 + threaded_vars.len();

        // Convert variable names to Core Erlang format
        let core_vars: Vec<String> = threaded_vars
            .iter()
            .map(|v| Self::to_core_erlang_var(v))
            .collect();

        // Get the block parameter names
        let acc_param = body_block.parameters.first().map(|p| p.name.as_str());
        let item_param = body_block.parameters.get(1).map(|p| p.name.as_str());

        // let _List = <list> in let _Acc = <initial> in
        write!(self.output, "let {list_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in let {acc_var} = ")?;
        self.generate_expression(initial)?;
        write!(self.output, " in ")?;

        // Generate letrec with List, Acc, and threaded vars as parameters
        write!(
            self.output,
            "letrec '{loop_fn}'/{arity} = fun ({list_var}, {acc_var}"
        )?;
        for core_var in &core_vars {
            write!(self.output, ", {core_var}")?;
        }
        write!(self.output, ") -> ")?;

        // Push a new scope with the loop variables bound
        self.push_scope();
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            self.bind_var(var, core_var);
        }
        // Bind acc parameter if it exists
        if let Some(param) = acc_param {
            self.bind_var(param, &acc_var);
        }

        // case List of
        //   <[]> when 'true' -> {Acc, ThreadedVars}
        //   <[H|T]> when 'true' -> <body> apply Loop (T, NewAcc, UpdatedVars)
        write!(self.output, "case {list_var} of ")?;

        // Empty list case: return acc and final threaded values
        write!(self.output, "<[]> when 'true' -> ")?;
        // Return tuple of (acc, threaded_vars...)
        if core_vars.is_empty() {
            write!(self.output, "{acc_var}")?;
        } else {
            write!(self.output, "{{{acc_var}")?;
            for core_var in &core_vars {
                write!(self.output, ", {core_var}")?;
            }
            write!(self.output, "}}")?;
        }

        // Non-empty list case: [H|T]
        write!(self.output, " <[{head_var}|{tail_var}]> when 'true' -> ")?;

        // Bind item parameter if it exists
        if let Some(param) = item_param {
            self.bind_var(param, &head_var);
        }

        // Generate body and extract new variable values
        self.generate_list_inject_body_with_threading(
            body_block,
            threaded_vars,
            &core_vars,
            &loop_fn,
            arity,
            &tail_var,
            &acc_var,
        )?;

        write!(self.output, " end ")?;

        // Pop the scope
        self.pop_scope();

        // Initial call: apply '_Loop'/N (List, Init, Var1, Var2, ...)
        write!(
            self.output,
            "in apply '{loop_fn}'/{arity} ({list_var}, {acc_var}"
        )?;
        for var in threaded_vars {
            write!(self.output, ", ")?;
            let var_name = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates the body of a list inject:into: loop that threads mutated variables.
    #[allow(clippy::too_many_arguments)]
    fn generate_list_inject_body_with_threading(
        &mut self,
        body_block: &Block,
        threaded_vars: &[String],
        core_vars: &[String],
        loop_fn: &str,
        arity: usize,
        tail_var: &str,
        acc_var: &str,
    ) -> Result<()> {
        if body_block.body.is_empty() {
            // Empty body - just recurse with tail, same acc, and same values
            write!(
                self.output,
                "apply '{loop_fn}'/{arity} ({tail_var}, {acc_var}"
            )?;
            for core_var in core_vars {
                write!(self.output, ", {core_var}")?;
            }
            write!(self.output, ")")?;
            return Ok(());
        }

        // Track which variables have been assigned new values
        let mut assigned_vars: std::collections::HashSet<String> = std::collections::HashSet::new();

        // Track the result of the body (new accumulator value)
        let body_result = self.fresh_temp_var("Result");

        // Process each expression in the body
        for (i, expr) in body_block.body.iter().enumerate() {
            let is_last = i == body_block.body.len() - 1;

            match expr {
                Expression::Assignment { target, value, .. } => {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = id.name.as_str();
                        if threaded_vars.contains(&var_name.to_string()) {
                            let core_var = Self::to_core_erlang_var(var_name);
                            let new_var = format!("{core_var}1");
                            write!(self.output, "let {new_var} = ")?;
                            self.generate_expression(value)?;
                            write!(self.output, " in ")?;
                            assigned_vars.insert(var_name.to_string());
                            continue;
                        }
                    }
                    // Not a threaded variable
                    if is_last {
                        write!(self.output, "let {body_result} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
                _ => {
                    if is_last {
                        write!(self.output, "let {body_result} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
            }
        }

        // Recurse with tail, new acc (body result), and updated values
        write!(
            self.output,
            "apply '{loop_fn}'/{arity} ({tail_var}, {body_result}"
        )?;
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            write!(self.output, ", ")?;
            if assigned_vars.contains(var) {
                write!(self.output, "{core_var}1")?;
            } else {
                write!(self.output, "{core_var}")?;
            }
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Tries to generate code for String methods.
    ///
    /// String methods are synchronous operations that generate direct Erlang calls.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a String method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a String method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # String Methods
    ///
    /// - `length` (0 args) → `string:length(Receiver)`
    /// - `isEmpty` (0 args) → `string:length(Receiver) =:= 0`
    fn try_generate_string_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "length" if arguments.is_empty() => {
                    // call 'string':'length'(Receiver)
                    write!(self.output, "call 'string':'length'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "isEmpty" if arguments.is_empty() => {
                    // call 'erlang':'=:='(call 'string':'length'(Receiver), 0)
                    // Using string:length == 0 because Core Erlang empty binary syntax is complex
                    write!(self.output, "call 'erlang':'=:='(call 'string':'length'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, "), 0)")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            // No keyword or binary String methods handled here
            _ => Ok(None),
        }
    }

    /// Generates a block value call: `let _Fun = <receiver> in apply _Fun (Args...)`.
    ///
    /// In Core Erlang, we bind the receiver to a variable first to ensure proper
    /// evaluation order and handle complex receiver expressions correctly.
    ///
    /// ```erlang
    /// let _Fun1 = <receiver-expr> in apply _Fun1 (Arg1, Arg2, ...)
    /// ```
    fn generate_block_value_call(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<()> {
        // Bind receiver to a variable first for proper evaluation
        // Use fresh_temp_var to avoid shadowing user identifiers named "Fun"
        let fun_var = self.fresh_temp_var("Fun");
        write!(self.output, "let {fun_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in apply {fun_var} (")?;
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }
        write!(self.output, ")")?;
        Ok(())
    }

    /// Generates a `whileTrue:` loop.
    ///
    /// Beamtalk: `[condition] whileTrue: [body]`
    ///
    /// Core Erlang uses letrec for loops:
    /// ```erlang
    /// letrec '_Loop1'/0 = fun () ->
    ///     case apply ConditionFun () of
    ///       <'true'> when 'true' ->
    ///         do apply BodyFun ()
    ///            apply '_Loop1'/0 ()
    ///       <'false'> when 'true' ->
    ///         'nil'
    ///     end
    /// in apply '_Loop1'/0 ()
    /// ```
    fn generate_while_true(
        &mut self,
        condition: &Expression,
        arguments: &[Expression],
    ) -> Result<()> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(
                "whileTrue: requires exactly one argument (body block)".to_string(),
            ));
        }
        let body = &arguments[0];

        // Check if both condition and body are literal blocks (enables mutation analysis)
        if let (Expression::Block(condition_block), Expression::Block(body_block)) =
            (condition, body)
        {
            // Analyze both blocks for mutations
            let condition_analysis = block_analysis::analyze_block(condition_block);
            let body_analysis = block_analysis::analyze_block(body_block);

            // Combine mutations from both blocks
            let mut all_local_writes = condition_analysis.local_writes.clone();
            all_local_writes.extend(body_analysis.local_writes.iter().cloned());

            let mut all_local_reads = condition_analysis.local_reads.clone();
            all_local_reads.extend(body_analysis.local_reads.iter().cloned());

            let mut all_field_writes = condition_analysis.field_writes.clone();
            all_field_writes.extend(body_analysis.field_writes.iter().cloned());

            let mut all_field_reads = condition_analysis.field_reads.clone();
            all_field_reads.extend(body_analysis.field_reads.iter().cloned());

            // Variables that need threading: read AND written
            let threaded_vars: Vec<String> = all_local_reads
                .intersection(&all_local_writes)
                .cloned()
                .collect();

            // Fields that need threading: read AND written
            let threaded_fields: Vec<String> = all_field_reads
                .intersection(&all_field_writes)
                .cloned()
                .collect();

            if !threaded_vars.is_empty() || !threaded_fields.is_empty() {
                // Generate tail-recursive loop with mutation support
                return self.generate_while_true_with_mutations(
                    condition_block,
                    body_block,
                    &threaded_vars,
                    &threaded_fields,
                );
            }
        }

        // Fall back to simple loop (no mutations)
        self.generate_while_true_simple(condition, body)
    }

    /// Generates a simple `whileTrue:` loop without mutation support.
    fn generate_while_true_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Use fresh_temp_var to avoid shadowing user identifiers
        let loop_fn = self.fresh_temp_var("Loop");
        let condition_var = self.fresh_temp_var("Cond");
        let body_var = self.fresh_temp_var("Body");

        // Bind condition and body to variables first to avoid repeated evaluation
        write!(self.output, "let {condition_var} = ")?;
        self.generate_expression(condition)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(self.output, " in letrec '{loop_fn}'/0 = fun () -> ")?;
        write!(self.output, "case apply {condition_var} () of ")?;
        write!(self.output, "<'true'> when 'true' -> ")?;
        write!(self.output, "do apply {body_var} () ")?;
        write!(self.output, "apply '{loop_fn}'/0 () ")?;
        write!(self.output, "<'false'> when 'true' -> 'nil' end ")?;
        write!(self.output, "in apply '{loop_fn}'/0 ()")?;

        Ok(())
    }

    /// Generates a `whileTrue:` loop with mutation support.
    ///
    /// Generates a tail-recursive loop that threads mutated variables and fields.
    /// Variables are passed as loop parameters, fields are threaded through a state map.
    fn generate_while_true_with_mutations(
        &mut self,
        condition_block: &Block,
        body_block: &Block,
        threaded_vars: &[String],
        threaded_fields: &[String],
    ) -> Result<()> {
        // For now, only implement local variable threading
        // Field threading will be added in a follow-up
        if !threaded_fields.is_empty() {
            // Fall back to simple implementation for field mutations
            return self.generate_while_true_simple(
                &Expression::Block(condition_block.clone()),
                &Expression::Block(body_block.clone()),
            );
        }

        let loop_fn = self.fresh_temp_var("Loop");
        let arity = threaded_vars.len();

        // Convert variable names to Core Erlang format and bind them in a new scope
        let core_vars: Vec<String> = threaded_vars
            .iter()
            .map(|v| Self::to_core_erlang_var(v))
            .collect();

        // Generate letrec
        write!(self.output, "letrec '{loop_fn}'/{arity} = fun (")?;

        // Parameters: one for each threaded variable (capitalized)
        for (i, core_var) in core_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "{core_var}")?;
        }
        write!(self.output, ") -> ")?;

        // Push a new scope with the loop variables bound
        self.push_scope();
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            self.bind_var(var, core_var);
        }

        // Evaluate condition inline (uses the loop parameters now)
        write!(self.output, "case ")?;
        self.generate_block_body(condition_block)?;
        write!(self.output, " of ")?;

        // True case: execute body and recurse
        write!(self.output, "<'true'> when 'true' -> ")?;

        // Generate body and extract new variable values
        self.generate_while_body_with_threading(
            body_block,
            threaded_vars,
            &core_vars,
            &loop_fn,
            arity,
        )?;

        // False case: return final values
        write!(self.output, " <'false'> when 'true' -> ")?;
        if core_vars.len() == 1 {
            // Single variable: return it directly
            write!(self.output, "{}", &core_vars[0])?;
        } else {
            // Multiple values: return tuple
            write!(self.output, "{{")?;
            for (i, core_var) in core_vars.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "{core_var}")?;
            }
            write!(self.output, "}}")?;
        }
        write!(self.output, " end ")?;

        // Pop the scope
        self.pop_scope();

        // Initial call to loop function (uses the outer scope variable values)
        write!(self.output, "in apply '{loop_fn}'/{arity} (")?;
        for (i, var) in threaded_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            // Prefer the currently bound Core Erlang variable name (which may be a fresh_var),
            // and fall back to the synthesized name if there is no existing binding.
            let var_name = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates a while loop body that threads mutated variables.
    fn generate_while_body_with_threading(
        &mut self,
        body_block: &Block,
        threaded_vars: &[String],
        core_vars: &[String],
        loop_fn: &str,
        arity: usize,
    ) -> Result<()> {
        // For each expression in the body, we need to track assignments
        // and generate new variable bindings

        if body_block.body.is_empty() {
            // Empty body - just recurse with same values
            write!(self.output, "apply '{loop_fn}'/{arity} (")?;
            for (i, core_var) in core_vars.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "{core_var}")?;
            }
            write!(self.output, ")")?;
            return Ok(());
        }

        // Track which variables have been assigned new values
        let mut assigned_vars: std::collections::HashSet<String> = std::collections::HashSet::new();

        // Process each expression in the body
        for (i, expr) in body_block.body.iter().enumerate() {
            let is_last = i == body_block.body.len() - 1;

            match expr {
                Expression::Assignment { target, value, .. } => {
                    // Check if this is a threaded variable assignment
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = id.name.as_str();
                        if threaded_vars.contains(&var_name.to_string()) {
                            // Generate: let Var1 = <value> in ...
                            let core_var = Self::to_core_erlang_var(var_name);
                            let new_var = format!("{core_var}1");
                            write!(self.output, "let {new_var} = ")?;
                            self.generate_expression(value)?;
                            write!(self.output, " in ")?;
                            assigned_vars.insert(var_name.to_string());
                            continue;
                        }
                    }
                    // Not a threaded variable - treat as normal expression
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
                _ => {
                    // Non-assignment expression
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
            }
        }

        // After processing all expressions, recurse with updated values
        write!(self.output, " apply '{loop_fn}'/{arity} (")?;
        for (i, (var, core_var)) in threaded_vars.iter().zip(core_vars.iter()).enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            // Use the new variable name (Var1) if it was assigned in the body
            if assigned_vars.contains(var) {
                write!(self.output, "{core_var}1")?;
            } else {
                write!(self.output, "{core_var}")?;
            }
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates a `whileFalse:` loop.
    ///
    /// Beamtalk: `[condition] whileFalse: [body]`
    ///
    /// Same as whileTrue but with inverted condition check.
    fn generate_while_false(
        &mut self,
        condition: &Expression,
        arguments: &[Expression],
    ) -> Result<()> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(
                "whileFalse: requires exactly one argument (body block)".to_string(),
            ));
        }
        let body = &arguments[0];

        // Check if both condition and body are literal blocks (enables mutation analysis)
        if let (Expression::Block(condition_block), Expression::Block(body_block)) =
            (condition, body)
        {
            // Analyze both blocks for mutations
            let condition_analysis = block_analysis::analyze_block(condition_block);
            let body_analysis = block_analysis::analyze_block(body_block);

            // Combine mutations from both blocks
            let mut all_local_writes = condition_analysis.local_writes.clone();
            all_local_writes.extend(body_analysis.local_writes.iter().cloned());

            let mut all_local_reads = condition_analysis.local_reads.clone();
            all_local_reads.extend(body_analysis.local_reads.iter().cloned());

            let mut all_field_writes = condition_analysis.field_writes.clone();
            all_field_writes.extend(body_analysis.field_writes.iter().cloned());

            let mut all_field_reads = condition_analysis.field_reads.clone();
            all_field_reads.extend(body_analysis.field_reads.iter().cloned());

            // Variables that need threading: read AND written
            let threaded_vars: Vec<String> = all_local_reads
                .intersection(&all_local_writes)
                .cloned()
                .collect();

            // Fields that need threading: read AND written
            let threaded_fields: Vec<String> = all_field_reads
                .intersection(&all_field_writes)
                .cloned()
                .collect();

            if !threaded_vars.is_empty() || !threaded_fields.is_empty() {
                // Generate tail-recursive loop with mutation support
                return self.generate_while_false_with_mutations(
                    condition_block,
                    body_block,
                    &threaded_vars,
                    &threaded_fields,
                );
            }
        }

        // Fall back to simple loop (no mutations)
        self.generate_while_false_simple(condition, body)
    }

    /// Generates a simple `whileFalse:` loop without mutation support.
    fn generate_while_false_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Use fresh_temp_var to avoid shadowing user identifiers
        let loop_fn = self.fresh_temp_var("Loop");
        let condition_var = self.fresh_temp_var("Cond");
        let body_var = self.fresh_temp_var("Body");

        write!(self.output, "let {condition_var} = ")?;
        self.generate_expression(condition)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(self.output, " in letrec '{loop_fn}'/0 = fun () -> ")?;
        write!(self.output, "case apply {condition_var} () of ")?;
        write!(self.output, "<'false'> when 'true' -> ")?;
        write!(self.output, "do apply {body_var} () ")?;
        write!(self.output, "apply '{loop_fn}'/0 () ")?;
        write!(self.output, "<'true'> when 'true' -> 'nil' end ")?;
        write!(self.output, "in apply '{loop_fn}'/0 ()")?;

        Ok(())
    }

    /// Generates a `whileFalse:` loop with mutation support.
    fn generate_while_false_with_mutations(
        &mut self,
        condition_block: &Block,
        body_block: &Block,
        threaded_vars: &[String],
        threaded_fields: &[String],
    ) -> Result<()> {
        // For now, only implement local variable threading
        // Field threading will be added in a follow-up
        if !threaded_fields.is_empty() {
            // Fall back to simple implementation for field mutations
            return self.generate_while_false_simple(
                &Expression::Block(condition_block.clone()),
                &Expression::Block(body_block.clone()),
            );
        }

        let loop_fn = self.fresh_temp_var("Loop");
        let arity = threaded_vars.len();

        // Convert variable names to Core Erlang format and bind them in a new scope
        let core_vars: Vec<String> = threaded_vars
            .iter()
            .map(|v| Self::to_core_erlang_var(v))
            .collect();

        // Generate letrec
        write!(self.output, "letrec '{loop_fn}'/{arity} = fun (")?;

        // Parameters: one for each threaded variable (capitalized)
        for (i, core_var) in core_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "{core_var}")?;
        }
        write!(self.output, ") -> ")?;

        // Push a new scope with the loop variables bound
        self.push_scope();
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            self.bind_var(var, core_var);
        }

        // Evaluate condition inline (uses the loop parameters now)
        write!(self.output, "case ")?;
        self.generate_block_body(condition_block)?;
        write!(self.output, " of ")?;

        // False case: execute body and recurse (note: inverted from whileTrue)
        write!(self.output, "<'false'> when 'true' -> ")?;

        // Generate body and extract new variable values
        self.generate_while_body_with_threading(
            body_block,
            threaded_vars,
            &core_vars,
            &loop_fn,
            arity,
        )?;

        // True case: return final values
        write!(self.output, " <'true'> when 'true' -> ")?;
        if core_vars.len() == 1 {
            // Single variable: return it directly
            write!(self.output, "{}", &core_vars[0])?;
        } else {
            // Multiple values: return tuple
            write!(self.output, "{{")?;
            for (i, core_var) in core_vars.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "{core_var}")?;
            }
            write!(self.output, "}}")?;
        }
        write!(self.output, " end ")?;

        // Pop the scope
        self.pop_scope();

        // Initial call to loop function (uses the outer scope variable values)
        write!(self.output, "in apply '{loop_fn}'/{arity} (")?;
        for (i, var) in threaded_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            // Look up the variable in the OUTER scope; fall back to a synthesized name
            let var_name = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates a `repeat` infinite loop.
    ///
    /// Beamtalk: `[body] repeat`
    ///
    /// Core Erlang:
    /// ```erlang
    /// letrec '_Loop1'/0 = fun () ->
    ///     do apply BodyFun ()
    ///        apply '_Loop1'/0 ()
    /// in apply '_Loop1'/0 ()
    /// ```
    fn generate_repeat(&mut self, body: &Expression) -> Result<()> {
        // Use fresh_temp_var to avoid shadowing user identifiers
        let loop_fn = self.fresh_temp_var("Loop");
        let body_var = self.fresh_temp_var("Body");

        write!(self.output, "let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(self.output, " in letrec '{loop_fn}'/0 = fun () -> ")?;
        write!(self.output, "do apply {body_var} () ")?;
        write!(self.output, "apply '{loop_fn}'/0 () ")?;
        write!(self.output, "in apply '{loop_fn}'/0 ()")?;

        Ok(())
    }

    /// Generates a `timesRepeat:` loop.
    ///
    /// Beamtalk: `5 timesRepeat: [body]`
    ///
    /// Core Erlang (simple, no mutations):
    /// ```erlang
    /// letrec '_Loop'/1 = fun (N) ->
    ///     case N > 0 of
    ///       'true' -> do apply BodyFun () apply '_Loop'/1 (N - 1)
    ///       'false' -> 'nil'
    ///     end
    /// in apply '_Loop'/1 (5)
    /// ```
    fn generate_times_repeat(
        &mut self,
        count: &Expression,
        arguments: &[Expression],
    ) -> Result<()> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(
                "timesRepeat: requires exactly one argument (body block)".to_string(),
            ));
        }
        let body = &arguments[0];

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let body_analysis = block_analysis::analyze_block(body_block);

            // Variables that need threading: read AND written
            let threaded_vars: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .cloned()
                .collect();

            // Fields that need threading: read AND written
            let threaded_fields: Vec<String> = body_analysis
                .field_reads
                .intersection(&body_analysis.field_writes)
                .cloned()
                .collect();

            if !threaded_vars.is_empty() || !threaded_fields.is_empty() {
                // Generate tail-recursive loop with mutation support
                return self.generate_times_repeat_with_mutations(
                    count,
                    body_block,
                    &threaded_vars,
                    &threaded_fields,
                );
            }
        }

        // Fall back to simple loop (no mutations)
        self.generate_times_repeat_simple(count, body)
    }

    /// Generates a simple `timesRepeat:` loop without mutation support.
    fn generate_times_repeat_simple(
        &mut self,
        count: &Expression,
        body: &Expression,
    ) -> Result<()> {
        let loop_fn = self.fresh_temp_var("Loop");
        let body_var = self.fresh_temp_var("Body");
        let count_var = self.fresh_temp_var("N");

        // let _Body = <body> in let _N = <count> in
        write!(self.output, "let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(self.output, " in let {count_var} = ")?;
        self.generate_expression(count)?;
        write!(self.output, " in ")?;

        // letrec '_Loop'/1 = fun (N) ->
        //     case call 'erlang':'>'(N, 0) of
        //       <'true'> when 'true' ->
        //         do apply _Body () apply '_Loop'/1 (call 'erlang':'-'(N, 1))
        //       <'false'> when 'true' -> 'nil'
        //     end
        // in apply '_Loop'/1 (_N)
        write!(self.output, "letrec '{loop_fn}'/1 = fun (N) -> ")?;
        write!(
            self.output,
            "case call 'erlang':'>'(N, 0) of <'true'> when 'true' -> "
        )?;
        write!(self.output, "do apply {body_var} () ")?;
        write!(
            self.output,
            "apply '{loop_fn}'/1 (call 'erlang':'-'(N, 1)) "
        )?;
        write!(self.output, "<'false'> when 'true' -> 'nil' end ")?;
        write!(self.output, "in apply '{loop_fn}'/1 ({count_var})")?;

        Ok(())
    }

    /// Generates a `timesRepeat:` loop with mutation support.
    ///
    /// Generates a tail-recursive loop that threads mutated variables.
    fn generate_times_repeat_with_mutations(
        &mut self,
        count: &Expression,
        body_block: &Block,
        threaded_vars: &[String],
        threaded_fields: &[String],
    ) -> Result<()> {
        // For now, only implement local variable threading
        // Field threading will be added in a follow-up
        if !threaded_fields.is_empty() {
            // Fall back to simple implementation for field mutations
            return self
                .generate_times_repeat_simple(count, &Expression::Block(body_block.clone()));
        }

        let loop_fn = self.fresh_temp_var("Loop");
        let counter_var = self.fresh_temp_var("N"); // Use fresh var to avoid collision with user variables
        // Arity: 1 for counter, plus one for each threaded variable
        let arity = 1 + threaded_vars.len();

        // Convert variable names to Core Erlang format
        let core_vars: Vec<String> = threaded_vars
            .iter()
            .map(|v| Self::to_core_erlang_var(v))
            .collect();

        // Generate letrec with counter and threaded vars as parameters
        write!(
            self.output,
            "letrec '{loop_fn}'/{arity} = fun ({counter_var}"
        )?;
        for core_var in &core_vars {
            write!(self.output, ", {core_var}")?;
        }
        write!(self.output, ") -> ")?;

        // Push a new scope with the loop variables bound
        self.push_scope();
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            self.bind_var(var, core_var);
        }

        // case call 'erlang':'>'(CounterVar, 0) of
        write!(self.output, "case call 'erlang':'>'({counter_var}, 0) of ")?;

        // True case: execute body and recurse with counter-1
        write!(self.output, "<'true'> when 'true' -> ")?;

        // Generate body and extract new variable values
        self.generate_times_repeat_body_with_threading(
            body_block,
            threaded_vars,
            &core_vars,
            &loop_fn,
            arity,
            &counter_var,
        )?;

        // False case: return final values
        write!(self.output, " <'false'> when 'true' -> ")?;
        if core_vars.len() == 1 {
            write!(self.output, "{}", &core_vars[0])?;
        } else {
            write!(self.output, "{{")?;
            for (i, core_var) in core_vars.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "{core_var}")?;
            }
            write!(self.output, "}}")?;
        }
        write!(self.output, " end ")?;

        // Pop the scope
        self.pop_scope();

        // Initial call: apply '_Loop'/N (Count, Var1, Var2, ...)
        write!(self.output, "in apply '{loop_fn}'/{arity} (")?;
        self.generate_expression(count)?;
        for var in threaded_vars {
            write!(self.output, ", ")?;
            let var_name = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates the body of a timesRepeat: loop that threads mutated variables.
    fn generate_times_repeat_body_with_threading(
        &mut self,
        body_block: &Block,
        threaded_vars: &[String],
        core_vars: &[String],
        loop_fn: &str,
        arity: usize,
        counter_var: &str,
    ) -> Result<()> {
        if body_block.body.is_empty() {
            // Empty body - just recurse with counter-1 and same values
            write!(
                self.output,
                "apply '{loop_fn}'/{arity} (call 'erlang':'-'({counter_var}, 1)"
            )?;
            for core_var in core_vars {
                write!(self.output, ", {core_var}")?;
            }
            write!(self.output, ")")?;
            return Ok(());
        }

        // Track which variables have been assigned new values
        let mut assigned_vars: std::collections::HashSet<String> = std::collections::HashSet::new();

        // Process each expression in the body
        for (i, expr) in body_block.body.iter().enumerate() {
            let is_last = i == body_block.body.len() - 1;

            match expr {
                Expression::Assignment { target, value, .. } => {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = id.name.as_str();
                        if threaded_vars.contains(&var_name.to_string()) {
                            let core_var = Self::to_core_erlang_var(var_name);
                            let new_var = format!("{core_var}1");
                            write!(self.output, "let {new_var} = ")?;
                            self.generate_expression(value)?;
                            write!(self.output, " in ")?;
                            assigned_vars.insert(var_name.to_string());
                            continue;
                        }
                    }
                    // Not a threaded variable
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
                _ => {
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
            }
        }

        // Recurse with counter-1 and updated values
        write!(
            self.output,
            " apply '{loop_fn}'/{arity} (call 'erlang':'-'({counter_var}, 1)"
        )?;
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            write!(self.output, ", ")?;
            if assigned_vars.contains(var) {
                write!(self.output, "{core_var}1")?;
            } else {
                write!(self.output, "{core_var}")?;
            }
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates a `to:do:` range iteration loop.
    ///
    /// Beamtalk: `1 to: 10 do: [:n | body]`
    ///
    /// Core Erlang (simple, no mutations):
    /// ```erlang
    /// let _Start = 1, _End = 10, _Body = fun(...) in
    /// letrec '_Loop'/1 = fun (I) ->
    ///     case I > End of
    ///       'true' -> 'nil'
    ///       'false' -> do apply _Body (I) apply '_Loop'/1 (I + 1)
    ///     end
    /// in apply '_Loop'/1 (_Start)
    /// ```
    fn generate_to_do(
        &mut self,
        start: &Expression,
        end: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let body_analysis = block_analysis::analyze_block(body_block);

            // The block parameter (e.g., `n` in `[:n | ...]`) is the loop index
            // It should NOT be considered a threaded variable since it's passed as arg
            let block_params: std::collections::HashSet<String> = body_block
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect();

            // Variables that need threading: read AND written, but NOT block params
            let threaded_vars: Vec<String> = body_analysis
                .local_reads
                .intersection(&body_analysis.local_writes)
                .filter(|v| !block_params.contains(*v))
                .cloned()
                .collect();

            // Fields that need threading: read AND written
            let threaded_fields: Vec<String> = body_analysis
                .field_reads
                .intersection(&body_analysis.field_writes)
                .cloned()
                .collect();

            if !threaded_vars.is_empty() || !threaded_fields.is_empty() {
                // Generate tail-recursive loop with mutation support
                return self.generate_to_do_with_mutations(
                    start,
                    end,
                    body_block,
                    &threaded_vars,
                    &threaded_fields,
                );
            }
        }

        // Fall back to simple loop (no mutations)
        self.generate_to_do_simple(start, end, body)
    }

    /// Generates a simple `to:do:` loop without mutation support.
    fn generate_to_do_simple(
        &mut self,
        start: &Expression,
        end: &Expression,
        body: &Expression,
    ) -> Result<()> {
        let loop_fn = self.fresh_temp_var("Loop");
        let start_var = self.fresh_temp_var("Start");
        let end_var = self.fresh_temp_var("End");
        let body_var = self.fresh_temp_var("Body");
        let i_var = self.fresh_temp_var("I");

        // let _Start = <start> in let _End = <end> in let _Body = <body> in
        write!(self.output, "let {start_var} = ")?;
        self.generate_expression(start)?;
        write!(self.output, " in let {end_var} = ")?;
        self.generate_expression(end)?;
        write!(self.output, " in let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(self.output, " in ")?;

        // letrec '_Loop'/1 = fun (I) ->
        //     case call 'erlang':'>'(I, _End) of
        //       <'true'> when 'true' -> 'nil'
        //       <'false'> when 'true' -> do apply _Body (I) apply '_Loop'/1 (call 'erlang':'+'(I, 1))
        //     end
        // in apply '_Loop'/1 (_Start)
        write!(self.output, "letrec '{loop_fn}'/1 = fun ({i_var}) -> ")?;
        write!(
            self.output,
            "case call 'erlang':'>'({i_var}, {end_var}) of <'true'> when 'true' -> 'nil' "
        )?;
        write!(self.output, "<'false'> when 'true' -> ")?;
        write!(self.output, "do apply {body_var} ({i_var}) ")?;
        write!(
            self.output,
            "apply '{loop_fn}'/1 (call 'erlang':'+'({i_var}, 1)) end "
        )?;
        write!(self.output, "in apply '{loop_fn}'/1 ({start_var})")?;

        Ok(())
    }

    /// Generates a `to:do:` loop with mutation support.
    ///
    /// Generates a tail-recursive loop that threads mutated variables.
    fn generate_to_do_with_mutations(
        &mut self,
        start: &Expression,
        end: &Expression,
        body_block: &Block,
        threaded_vars: &[String],
        threaded_fields: &[String],
    ) -> Result<()> {
        // For now, only implement local variable threading
        // Field threading will be added in a follow-up
        if !threaded_fields.is_empty() {
            // Fall back to simple implementation for field mutations
            return self.generate_to_do_simple(start, end, &Expression::Block(body_block.clone()));
        }

        let loop_fn = self.fresh_temp_var("Loop");
        let end_var = self.fresh_temp_var("End");
        let i_var = self.fresh_temp_var("I");
        // Arity: 1 for loop index I, plus one for each threaded variable
        let arity = 1 + threaded_vars.len();

        // Convert variable names to Core Erlang format
        let core_vars: Vec<String> = threaded_vars
            .iter()
            .map(|v| Self::to_core_erlang_var(v))
            .collect();

        // Get the block parameter name for index variable (if any)
        let index_param = body_block.parameters.first().map(|p| p.name.as_str());

        // let _End = <end> in
        write!(self.output, "let {end_var} = ")?;
        self.generate_expression(end)?;
        write!(self.output, " in ")?;

        // Generate letrec with I and threaded vars as parameters
        write!(self.output, "letrec '{loop_fn}'/{arity} = fun ({i_var}")?;
        for core_var in &core_vars {
            write!(self.output, ", {core_var}")?;
        }
        write!(self.output, ") -> ")?;

        // Push a new scope with the loop variables bound
        self.push_scope();
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            self.bind_var(var, core_var);
        }
        // Also bind the index parameter if it exists
        if let Some(param) = index_param {
            self.bind_var(param, &i_var);
        }

        // case call 'erlang':'>'(I, _End) of
        write!(
            self.output,
            "case call 'erlang':'>'({i_var}, {end_var}) of "
        )?;

        // True case (past end): return final values
        write!(self.output, "<'true'> when 'true' -> ")?;
        if core_vars.len() == 1 {
            write!(self.output, "{}", &core_vars[0])?;
        } else {
            write!(self.output, "{{")?;
            for (i, core_var) in core_vars.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "{core_var}")?;
            }
            write!(self.output, "}}")?;
        }

        // False case: execute body and recurse with I+1
        write!(self.output, " <'false'> when 'true' -> ")?;

        // Generate body and extract new variable values
        self.generate_to_do_body_with_threading(
            body_block,
            threaded_vars,
            &core_vars,
            &loop_fn,
            arity,
            &i_var,
        )?;

        write!(self.output, " end ")?;

        // Pop the scope
        self.pop_scope();

        // Initial call: apply '_Loop'/N (Start, Var1, Var2, ...)
        write!(self.output, "in apply '{loop_fn}'/{arity} (")?;
        self.generate_expression(start)?;
        for var in threaded_vars {
            write!(self.output, ", ")?;
            let var_name = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates the body of a to:do: loop that threads mutated variables.
    fn generate_to_do_body_with_threading(
        &mut self,
        body_block: &Block,
        threaded_vars: &[String],
        core_vars: &[String],
        loop_fn: &str,
        arity: usize,
        i_var: &str,
    ) -> Result<()> {
        if body_block.body.is_empty() {
            // Empty body - just recurse with I+1 and same values
            write!(
                self.output,
                "apply '{loop_fn}'/{arity} (call 'erlang':'+'({i_var}, 1)"
            )?;
            for core_var in core_vars {
                write!(self.output, ", {core_var}")?;
            }
            write!(self.output, ")")?;
            return Ok(());
        }

        // Track which variables have been assigned new values
        let mut assigned_vars: std::collections::HashSet<String> = std::collections::HashSet::new();

        // Process each expression in the body
        for (i, expr) in body_block.body.iter().enumerate() {
            let is_last = i == body_block.body.len() - 1;

            match expr {
                Expression::Assignment { target, value, .. } => {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = id.name.as_str();
                        if threaded_vars.contains(&var_name.to_string()) {
                            let core_var = Self::to_core_erlang_var(var_name);
                            let new_var = format!("{core_var}1");
                            write!(self.output, "let {new_var} = ")?;
                            self.generate_expression(value)?;
                            write!(self.output, " in ")?;
                            assigned_vars.insert(var_name.to_string());
                            continue;
                        }
                    }
                    // Not a threaded variable
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
                _ => {
                    if is_last {
                        self.generate_expression(expr)?;
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        write!(self.output, "let {tmp_var} = ")?;
                        self.generate_expression(expr)?;
                        write!(self.output, " in ")?;
                    }
                }
            }
        }

        // Recurse with I+1 and updated values
        write!(
            self.output,
            " apply '{loop_fn}'/{arity} (call 'erlang':'+'({i_var}, 1)"
        )?;
        for (var, core_var) in threaded_vars.iter().zip(core_vars.iter()) {
            write!(self.output, ", ")?;
            if assigned_vars.contains(var) {
                write!(self.output, "{core_var}1")?;
            } else {
                write!(self.output, "{core_var}")?;
            }
        }
        write!(self.output, ")")?;

        Ok(())
    }

    /// Generates code for binary operators.
    fn generate_binary_op(
        &mut self,
        op: &str,
        left: &Expression,
        arguments: &[Expression],
    ) -> Result<()> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(
                "binary operator must have exactly one argument".to_string(),
            ));
        }

        // Special case: string concatenation uses iolist_to_binary
        if op == "++" {
            write!(
                self.output,
                "call 'erlang':'iolist_to_binary'([call 'erlang':'binary_to_list'("
            )?;
            self.generate_expression(left)?;
            write!(self.output, "), call 'erlang':'binary_to_list'(")?;
            self.generate_expression(&arguments[0])?;
            write!(self.output, ")])")?;
            return Ok(());
        }

        let erlang_op = match op {
            "+" => "+",
            "-" => "-",
            "*" => "*",
            "/" => "/",
            "%" => "rem",
            "==" => "==",
            "=" => "=:=",  // Strict equality
            "~=" => "=/=", // Strict inequality
            "<" => "<",
            ">" => ">",
            "<=" => "=<",
            ">=" => ">=",
            _ => {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!("binary operator: {op}"),
                    location: "unknown".to_string(),
                });
            }
        };

        write!(self.output, "call 'erlang':'{erlang_op}'(")?;
        self.generate_expression(left)?;
        write!(self.output, ", ")?;
        self.generate_expression(&arguments[0])?;
        write!(self.output, ")")?;

        Ok(())
    }

    /// Writes the current indentation level.
    fn write_indent(&mut self) -> Result<()> {
        for _ in 0..self.indent {
            write!(self.output, "    ")?;
        }
        Ok(())
    }

    /// Generates a fresh variable name and binds it in the current scope.
    ///
    /// Use this for user-visible bindings (block parameters, assignments, etc.)
    /// where the name should be looked up later via `lookup_var`.
    fn fresh_var(&mut self, base: &str) -> String {
        self.var_counter += 1;
        let var_name = format!("_{}{}", base.replace('_', ""), self.var_counter);
        // Insert into the current (innermost) scope
        if let Some(current_scope) = self.var_scopes.last_mut() {
            current_scope.insert(base.to_string(), var_name.clone());
        }
        var_name
    }

    /// Generates a fresh temporary variable name WITHOUT binding it in scope.
    ///
    /// Use this for internal codegen temporaries (loop variables, function bindings,
    /// etc.) that should never shadow or be confused with user identifiers.
    fn fresh_temp_var(&mut self, base: &str) -> String {
        self.var_counter += 1;
        format!("_{}{}", base.replace('_', ""), self.var_counter)
    }

    /// Converts a Beamtalk identifier to a valid Core Erlang variable name.
    ///
    /// Core Erlang variables must start with an uppercase letter or underscore.
    /// This function capitalizes the first letter of the identifier.
    fn to_core_erlang_var(name: &str) -> String {
        if name.is_empty() {
            return "_Empty".to_string();
        }
        let mut chars = name.chars();
        let first = chars.next().unwrap();
        let rest: String = chars.collect();
        format!("{}{}", first.to_uppercase(), rest)
    }

    /// Returns the current state variable name for state threading.
    ///
    /// State threading uses incrementing variable names to simulate mutation:
    /// - Version 0: `State` (the original state passed to the method)
    /// - Version 1: `State1` (after first assignment)
    /// - Version 2: `State2` (after second assignment)
    /// - etc.
    fn current_state_var(&self) -> String {
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
    fn next_state_var(&mut self) -> String {
        self.state_version += 1;
        self.current_state_var()
    }

    /// Resets the state version to 0 at the start of each method.
    fn reset_state_version(&mut self) {
        self.state_version = 0;
    }

    /// Converts module name (`snake_case`) to class name (`CamelCase`).
    fn to_class_name(&self) -> String {
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
    fn to_module_name(class_name: &str) -> String {
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

    /// Generates field initialization code for the initial state map.
    ///
    /// This extracts the common logic of initializing fields from module-level
    /// assignments, used by both `spawn/0` and `init/1`.
    fn generate_initial_state_fields(&mut self, module: &Module) -> Result<()> {
        // Initialize fields from module expressions (assignments at top level)
        // Only include literal values - blocks are methods handled by dispatch/3
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let Expression::Identifier(id) = target.as_ref() {
                    // Only generate field if it's a simple literal (not a block/method)
                    if matches!(value.as_ref(), Expression::Literal(..)) {
                        self.write_indent()?;
                        write!(self.output, ", '{}' => ", id.name)?;
                        self.generate_expression(value)?;
                        writeln!(self.output)?;
                    }
                }
            }
        }

        // Initialize fields from class state declarations
        for class in &module.classes {
            for state in &class.state {
                self.write_indent()?;
                write!(self.output, ", '{}' => ", state.name.name)?;
                if let Some(ref default_value) = state.default_value {
                    self.generate_expression(default_value)?;
                } else {
                    // No default value - initialize to nil
                    write!(self.output, "'nil'")?;
                }
                writeln!(self.output)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::parse::Span;

    #[test]
    fn test_generate_empty_module() {
        let module = Module::new(Vec::new(), Span::new(0, 0));
        let result = generate(&module);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("module 'beamtalk_module'"));
        assert!(code.contains("attributes ['behaviour' = ['gen_server']]"));
    }

    #[test]
    fn test_generate_literal_integer() {
        let mut generator = CoreErlangGenerator::new("test");
        let lit = Literal::Integer(42);
        let result = generator.generate_literal(&lit);
        assert!(result.is_ok());
        assert_eq!(generator.output, "42");
    }

    #[test]
    fn test_generate_literal_float() {
        let mut generator = CoreErlangGenerator::new("test");
        let lit = Literal::Float(2.5);
        let result = generator.generate_literal(&lit);
        assert!(result.is_ok());
        assert_eq!(generator.output, "2.5");
    }

    #[test]
    fn test_generate_literal_symbol() {
        let mut generator = CoreErlangGenerator::new("test");
        let lit = Literal::Symbol("ok".into());
        let result = generator.generate_literal(&lit);
        assert!(result.is_ok());
        assert_eq!(generator.output, "'ok'");
    }

    #[test]
    fn test_generate_literal_string() {
        let mut generator = CoreErlangGenerator::new("test");
        let lit = Literal::String("hello".into());
        let result = generator.generate_literal(&lit);
        assert!(result.is_ok());
        // Core Erlang binary syntax: #{segment, ...}#
        // Each segment is #<charcode>(8,1,'integer',['unsigned'|['big']])
        assert_eq!(
            generator.output,
            "#\
{#<104>(8,1,'integer',['unsigned'|['big']]),\
#<101>(8,1,'integer',['unsigned'|['big']]),\
#<108>(8,1,'integer',['unsigned'|['big']]),\
#<108>(8,1,'integer',['unsigned'|['big']]),\
#<111>(8,1,'integer',['unsigned'|['big']])}#",
            "String 'hello' should generate correct Core Erlang binary literal"
        );
    }

    #[test]
    fn test_generate_binary_op_addition() {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
        let right = vec![Expression::Literal(Literal::Integer(4), Span::new(4, 5))];
        let result = generator.generate_binary_op("+", &left, &right);
        assert!(result.is_ok());
        assert!(generator.output.contains("call 'erlang':'+'(3, 4)"));
    }

    #[test]
    fn test_generate_string_concatenation_operator() {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::String("Hello".into()), Span::new(0, 7));
        let right = vec![Expression::Literal(
            Literal::String(" World".into()),
            Span::new(11, 19),
        )];
        let result = generator.generate_binary_op("++", &left, &right);
        assert!(result.is_ok());
        assert!(
            generator.output.contains("iolist_to_binary"),
            "Should use iolist_to_binary for string concatenation. Got: {}",
            generator.output
        );
        assert!(
            generator.output.contains("binary_to_list"),
            "Should convert binaries to lists first. Got: {}",
            generator.output
        );
    }

    #[test]
    fn test_generate_strict_equality_operator() {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
        let right = vec![Expression::Literal(Literal::Integer(42), Span::new(6, 8))];
        let result = generator.generate_binary_op("=", &left, &right);
        assert!(result.is_ok());
        assert!(
            generator.output.contains("call 'erlang':'=:='"),
            "Should use strict equality =:=. Got: {}",
            generator.output
        );
    }

    #[test]
    fn test_generate_strict_inequality_operator() {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
        let right = vec![Expression::Literal(Literal::Integer(99), Span::new(7, 9))];
        let result = generator.generate_binary_op("~=", &left, &right);
        assert!(result.is_ok());
        assert!(
            generator.output.contains("call 'erlang':'=/='"),
            "Should use strict inequality =/=. Got: {}",
            generator.output
        );
    }

    #[test]
    fn test_generate_loose_equality_operator() {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
        let right = vec![Expression::Literal(Literal::Integer(5), Span::new(6, 7))];
        let result = generator.generate_binary_op("==", &left, &right);
        assert!(result.is_ok());
        assert!(
            generator.output.contains("call 'erlang':'=='"),
            "Should use loose equality ==. Got: {}",
            generator.output
        );
        assert_eq!(generator.output, "call 'erlang':'=='(5, 5)");
    }

    #[test]
    fn test_fresh_var_generation() {
        let mut generator = CoreErlangGenerator::new("test");
        let var1 = generator.fresh_var("temp");
        let var2 = generator.fresh_var("temp");
        assert_ne!(var1, var2);
        assert!(var1.starts_with("_temp"));
        assert!(var2.starts_with("_temp"));
    }

    #[test]
    fn test_module_name_to_class_name() {
        let generator = CoreErlangGenerator::new("my_counter_actor");
        assert_eq!(generator.to_class_name(), "MyCounterActor");

        let generator = CoreErlangGenerator::new("simple");
        assert_eq!(generator.to_class_name(), "Simple");
    }

    #[test]
    fn test_class_name_to_module_name() {
        // Single word
        assert_eq!(CoreErlangGenerator::to_module_name("Counter"), "counter");

        // Multi-word CamelCase
        assert_eq!(
            CoreErlangGenerator::to_module_name("MyCounterActor"),
            "my_counter_actor"
        );

        // With acronyms
        assert_eq!(
            CoreErlangGenerator::to_module_name("HTTPRouter"),
            "httprouter"
        );

        // Mixed case
        assert_eq!(
            CoreErlangGenerator::to_module_name("HTTPSConnection"),
            "httpsconnection"
        );
    }

    #[test]
    fn test_generated_core_erlang_compiles() {
        use std::fs;
        use std::process::Command;

        // Test a self-contained Core Erlang module to verify syntax is valid
        // This specifically tests that:
        // 1. Empty map syntax ~{}~ compiles correctly
        // 2. The overall Core Erlang structure is valid
        // Full gen_server integration is tested in integration tests
        let core_erlang = r"module 'test_module' ['get_methods'/0, 'simple_fun'/0]
  attributes []

'get_methods'/0 = fun () ->
    ~{}~

'simple_fun'/0 = fun () ->
    42

end
";

        // Write to temporary file
        let temp_dir = std::env::temp_dir();
        let core_file = temp_dir.join("test_module.core");
        fs::write(&core_file, core_erlang).expect("should write core erlang file");

        // Try to compile with erlc
        let output = Command::new("erlc")
            .arg("+from_core")
            .arg(&core_file)
            .current_dir(&temp_dir)
            .output();

        // Clean up
        let _ = fs::remove_file(&core_file);
        let beam_file = temp_dir.join("test_module.beam");
        let _ = fs::remove_file(&beam_file);

        // Check compilation result
        match output {
            Ok(output) => {
                assert!(
                    output.status.success(),
                    "erlc compilation failed:\nstdout: {}\nstderr: {}\nGenerated code:\n{}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                    core_erlang
                );
            }
            Err(_) => {
                // erlc not available, skip test
                println!("Skipping test - erlc not installed in CI environment");
            }
        }
    }

    #[test]
    fn test_string_literal_core_erlang_compiles() {
        use std::fs;
        use std::process::Command;

        // Test that string literals compile correctly through the full pipeline
        // This tests the new binary syntax: #{#<value>(8,1,'integer',['unsigned'|['big']]),...}#
        let core_erlang = r"module 'test_string' ['get_greeting'/0]
  attributes []

'get_greeting'/0 = fun () ->
    #{#<104>(8,1,'integer',['unsigned'|['big']]),#<105>(8,1,'integer',['unsigned'|['big']])}#

end
";

        // Write to temporary file
        let temp_dir = std::env::temp_dir();
        let core_file = temp_dir.join("test_string.core");
        fs::write(&core_file, core_erlang).expect("should write core erlang file");

        // Try to compile with erlc
        let output = Command::new("erlc")
            .arg("+from_core")
            .arg(&core_file)
            .current_dir(&temp_dir)
            .output();

        // Clean up
        let _ = fs::remove_file(&core_file);
        let beam_file = temp_dir.join("test_string.beam");
        let _ = fs::remove_file(&beam_file);

        // Check compilation result
        match output {
            Ok(output) => {
                assert!(
                    output.status.success(),
                    "erlc compilation of string literal failed:\nstdout: {}\nstderr: {}\nGenerated code:\n{}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                    core_erlang
                );
            }
            Err(_) => {
                // erlc not available, skip test
                println!("Skipping test - erlc not installed in CI environment");
            }
        }
    }

    #[test]
    fn test_generate_unary_message_send_creates_future() {
        let mut generator = CoreErlangGenerator::new("test");

        // Build: receiver unarySelector
        let receiver = Expression::Identifier(Identifier::new("counter", Span::new(0, 7)));
        let selector = MessageSelector::Unary("increment".into());

        let result = generator.generate_message_send(&receiver, &selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;
        // Check async protocol: create future, cast, return future
        assert!(
            output.contains("beamtalk_future':'new'()"),
            "Should create a future with beamtalk_future:new(). Got: {output}"
        );
        assert!(
            output.contains("gen_server':'cast'("),
            "Should send via gen_server:cast(). Got: {output}"
        );
        assert!(
            output.contains("'increment'"),
            "Should include selector atom. Got: {output}"
        );
        // Check that a fresh future variable is used (pattern: _Future + number)
        assert!(
            output.contains("_Future") && output.matches("_Future").count() == 3,
            "Should use a fresh future variable 3 times (bind, send, return). Got: {output}"
        );
    }

    #[test]
    fn test_generate_keyword_message_send_creates_future() {
        let mut generator = CoreErlangGenerator::new("test");

        // Build: object foo: 1 bar: 'x'
        // (using a non-Dictionary selector to avoid interception)
        let receiver = Expression::Identifier(Identifier::new("object", Span::new(0, 6)));
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("foo:", Span::new(7, 11)),
            KeywordPart::new("bar:", Span::new(14, 18)),
        ]);
        // Arguments are passed separately to generate_message_send
        let arguments = vec![
            Expression::Literal(Literal::Integer(1), Span::new(12, 13)),
            Expression::Literal(Literal::String("x".into()), Span::new(19, 22)),
        ];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());

        let output = &generator.output;
        // Check async protocol
        assert!(
            output.contains("beamtalk_future':'new'()"),
            "Should create a future. Got: {output}"
        );
        assert!(
            output.contains("gen_server':'cast'("),
            "Should send via gen_server:cast(). Got: {output}"
        );
        assert!(
            output.contains("'foo:bar:'"),
            "Should include combined keyword selector. Got: {output}"
        );
    }

    #[test]
    fn test_generate_await_message_uses_future_await() {
        let mut generator = CoreErlangGenerator::new("test");

        // Build: future await (special case - should NOT create new future)
        let receiver = Expression::Identifier(Identifier::new("myFuture", Span::new(0, 8)));
        let selector = MessageSelector::Unary("await".into());

        let result = generator.generate_message_send(&receiver, &selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;
        // Special case: await uses beamtalk_future:await(), not the async protocol
        assert!(
            output.contains("beamtalk_future':'await'("),
            "Should call beamtalk_future:await(). Got: {output}"
        );
        assert!(
            !output.contains("gen_server':'cast'"),
            "Should NOT use gen_server:cast for await. Got: {output}"
        );
    }

    #[test]
    fn test_generate_binary_op_is_synchronous() {
        let mut generator = CoreErlangGenerator::new("test");

        // Build: 3 + 4 (binary ops are synchronous, not async)
        let receiver = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
        let selector = MessageSelector::Binary("+".into());
        let arguments = vec![Expression::Literal(Literal::Integer(4), Span::new(4, 5))];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());

        let output = &generator.output;
        // Binary ops use erlang's built-in operators - synchronous
        assert!(
            output.contains("erlang':'+'("),
            "Should use erlang:'+'. Got: {output}"
        );
        assert!(
            !output.contains("beamtalk_future':'new'"),
            "Binary ops should NOT create futures. Got: {output}"
        );
    }

    #[test]
    fn test_generate_nested_message_sends_use_unique_variables() {
        let mut generator = CoreErlangGenerator::new("test");

        // Build: (counter new) increment
        // This is a nested message send: receiver is also a message send
        let inner_receiver = Expression::Identifier(Identifier::new("counter", Span::new(0, 7)));
        let inner_selector = MessageSelector::Unary("new".into());
        let inner_send = Expression::MessageSend {
            receiver: Box::new(inner_receiver),
            selector: inner_selector,
            arguments: vec![],
            span: Span::new(0, 11),
        };

        let outer_selector = MessageSelector::Unary("increment".into());
        let result = generator.generate_message_send(&inner_send, &outer_selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;

        // Check that we have at least two different _Future variables
        // (The exact numbers depend on the fresh_var counter state, but they should be different)
        let has_multiple_futures =
            output.contains("_Future") && output.matches("_Future").count() >= 6; // At least 2 futures * 3 occurrences each
        assert!(
            has_multiple_futures,
            "Nested message sends should use unique future variables. Got: {output}"
        );

        // Check that we have receiver and pid extraction in the output
        assert!(
            output.contains("_Receiver"),
            "Should have receiver variable. Got: {output}"
        );
        assert!(
            output.contains("_Pid"),
            "Should have pid variable for message sending. Got: {output}"
        );
        assert!(
            output.contains("call 'erlang':'element'(4,"),
            "Should extract pid from #beamtalk_object{{}}. Got: {output}"
        );
    }

    #[test]
    fn test_generate_spawn_message_send() {
        let mut generator = CoreErlangGenerator::new("test_module");

        // Create AST for: Counter spawn
        let receiver = Expression::Identifier(Identifier::new("Counter", Span::new(0, 7)));
        let selector = MessageSelector::Unary("spawn".into());
        let arguments = vec![];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());
        assert!(generator.output.contains("call 'counter':'spawn'()"));
    }

    #[test]
    fn test_generate_spawn_with_message_send() {
        let mut generator = CoreErlangGenerator::new("test_module");

        // Create AST for: Counter spawnWith: #{value => 10}
        // For simplicity, we'll use an integer literal as the init arg
        // (in practice this would be a map literal)
        let receiver = Expression::Identifier(Identifier::new("Counter", Span::new(0, 7)));
        let selector =
            MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", Span::new(8, 18))]);
        let arguments = vec![Expression::Literal(Literal::Integer(42), Span::new(19, 21))];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());
        // Should call spawn/1 with the argument
        assert!(
            generator.output.contains("call 'counter':'spawn'(42)"),
            "spawnWith: should generate spawn/1 call. Got: {}",
            generator.output
        );
        // Should NOT create a future (spawn is synchronous)
        assert!(
            !generator.output.contains("beamtalk_future"),
            "spawnWith: should NOT create futures. Got: {}",
            generator.output
        );
    }

    #[test]
    fn test_generate_spawn_function() {
        use crate::ast::*;

        // Create a module with a simple field assignment
        let value_assignment = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "value",
                Span::new(0, 5),
            ))),
            value: Box::new(Expression::Literal(Literal::Integer(0), Span::new(9, 10))),
            span: Span::new(0, 10),
        };

        let module = Module::new(vec![value_assignment], Span::new(0, 10));
        let code = generate_with_name(&module, "counter").expect("codegen should succeed");

        // Check that spawn/0 and spawn/1 are exported
        assert!(code.contains("'spawn'/0"));
        assert!(code.contains("'spawn'/1"));

        // Check that spawn/0 function exists and calls gen_server:start_link with empty map
        assert!(code.contains("'spawn'/0 = fun () ->"));
        assert!(code.contains("call 'gen_server':'start_link'('counter', ~{}~, [])"));

        // Check that spawn/1 function exists and calls gen_server:start_link with InitArgs
        assert!(code.contains("'spawn'/1 = fun (InitArgs) ->"));
        assert!(code.contains("call 'gen_server':'start_link'('counter', InitArgs, [])"));

        // Check that it uses a case expression to extract the Pid and wrap it in #beamtalk_object{}
        assert!(code.contains("case call 'gen_server':'start_link'"));
        assert!(code.contains("<{'ok', Pid}> when 'true' ->"));

        // Check that it returns a #beamtalk_object{} record (class='Counter', class_mod='counter', pid=Pid)
        assert!(
            code.contains("{'beamtalk_object', 'Counter', 'counter', Pid}"),
            "spawn functions should return #beamtalk_object{{}} record. Got: {code}"
        );

        // Check that it handles errors
        assert!(code.contains("<{'error', Reason}> when 'true' ->"));
        assert!(code.contains("call 'erlang':'error'({'spawn_failed', Reason})"));

        // Check that init/1 creates the default state with fields and merges with InitArgs
        assert!(code.contains("'init'/1 = fun (InitArgs) ->"));
        assert!(code.contains("let DefaultState = ~{"));
        assert!(code.contains("'__class__' => 'Counter'"));
        assert!(code.contains("'__methods__' => call 'counter':'method_table'()"));
        assert!(code.contains("'value' => 0"));
        // Check that InitArgs is merged into DefaultState
        assert!(code.contains("call 'maps':'merge'(DefaultState, InitArgs)"));
        assert!(code.contains("{'ok', FinalState}"));
    }

    #[test]
    fn test_generate_repl_module_aliases_state_to_bindings() {
        // BT-57: REPL modules must alias State to Bindings for identifier lookups
        let expression = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
        let code = generate_repl_expression(&expression, "repl_test").expect("codegen should work");

        // Check that the module aliases State to Bindings
        assert!(
            code.contains("let State = Bindings in"),
            "REPL module should alias State to Bindings. Got:\n{code}"
        );

        // Check that identifier lookup uses maps:get with State
        assert!(
            code.contains("call 'maps':'get'('x', State)"),
            "Identifier lookup should use State (aliased to Bindings). Got:\n{code}"
        );
    }

    #[test]
    fn test_generate_repl_module_block_value_call() {
        // Test full REPL module generation for block value call
        // Expression: [:x | x + 1] value: 5
        let block = Block::new(
            vec![BlockParameter::new("x", Span::new(1, 2))],
            vec![Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "x",
                    Span::new(5, 6),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(9, 10))],
                span: Span::new(5, 10),
            }],
            Span::new(0, 12),
        );

        let expression = Expression::MessageSend {
            receiver: Box::new(Expression::Block(block)),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(13, 19))]),
            arguments: vec![Expression::Literal(Literal::Integer(5), Span::new(20, 21))],
            span: Span::new(0, 22),
        };

        let code =
            generate_repl_expression(&expression, "test_block_repl").expect("codegen should work");

        // Check basic structure
        assert!(
            code.contains("let State = Bindings in"),
            "Should alias State to Bindings"
        );
        assert!(code.contains("apply"), "Should use apply for block call");
    }

    #[test]
    fn test_generate_repl_module_with_arithmetic() {
        // BT-57: Verify complex expressions with variable references work
        // Expression: x + 1
        let x_ref = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
        let one = Expression::Literal(Literal::Integer(1), Span::new(4, 5));
        let expression = Expression::MessageSend {
            receiver: Box::new(x_ref),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![one],
            span: Span::new(0, 5),
        };

        let code =
            generate_repl_expression(&expression, "repl_arith").expect("codegen should work");

        // Check State aliasing
        assert!(
            code.contains("let State = Bindings in"),
            "REPL module should alias State to Bindings. Got:\n{code}"
        );

        // Check that x lookup works through State
        assert!(
            code.contains("call 'maps':'get'('x', State)"),
            "Variable x should be looked up from State. Got:\n{code}"
        );

        // Check the arithmetic operation
        assert!(
            code.contains("call 'erlang':'+'("),
            "Should have addition operation. Got:\n{code}"
        );
    }

    // ========================================================================
    // Block Evaluation Message Tests (BT-32)
    // ========================================================================

    #[test]
    fn test_block_value_message_no_args() {
        // [42] value → let _Fun = fun () -> 42 in apply _Fun ()
        let mut generator = CoreErlangGenerator::new("test");

        let block = Block::new(
            vec![],
            vec![Expression::Literal(Literal::Integer(42), Span::new(1, 3))],
            Span::new(0, 4),
        );
        let receiver = Expression::Block(block);
        let selector = MessageSelector::Unary("value".into());

        let result = generator.generate_message_send(&receiver, &selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("let _Fun1 = fun () -> 42 in apply _Fun1 ()"),
            "Should generate let binding with apply. Got: {output}"
        );
        // Should NOT use async protocol
        assert!(
            !output.contains("beamtalk_future"),
            "value message should NOT create futures. Got: {output}"
        );
    }

    #[test]
    fn test_block_value_message_one_arg() {
        // [:x | x + 1] value: 5 → let _Fun = ... in apply _Fun (5)
        let mut generator = CoreErlangGenerator::new("test");

        let block = Block::new(
            vec![BlockParameter::new("x", Span::new(1, 2))],
            vec![Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "x",
                    Span::new(5, 6),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(9, 10))],
                span: Span::new(5, 10),
            }],
            Span::new(0, 12),
        );
        let receiver = Expression::Block(block);
        let selector =
            MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(13, 19))]);
        let arguments = vec![Expression::Literal(Literal::Integer(5), Span::new(20, 21))];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("let _Fun"),
            "Should use let binding for block evaluation. Got: {output}"
        );
        assert!(
            output.contains("apply _Fun"),
            "Should use apply on bound fun variable. Got: {output}"
        );
        assert!(
            output.contains("(5)"),
            "Should pass argument 5 to the block. Got: {output}"
        );
        assert!(
            !output.contains("beamtalk_future"),
            "value: message should NOT create futures. Got: {output}"
        );
    }

    #[test]
    fn test_block_value_message_two_args() {
        // [:x :y | x + y] value: 3 value: 4
        let mut generator = CoreErlangGenerator::new("test");

        let block = Block::new(
            vec![
                BlockParameter::new("x", Span::new(1, 2)),
                BlockParameter::new("y", Span::new(4, 5)),
            ],
            vec![Expression::Literal(Literal::Integer(0), Span::new(8, 9))], // placeholder body
            Span::new(0, 11),
        );
        let receiver = Expression::Block(block);
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("value:", Span::new(12, 18)),
            KeywordPart::new("value:", Span::new(21, 27)),
        ]);
        let arguments = vec![
            Expression::Literal(Literal::Integer(3), Span::new(19, 20)),
            Expression::Literal(Literal::Integer(4), Span::new(28, 29)),
        ];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("apply"),
            "Should use apply for block evaluation. Got: {output}"
        );
        assert!(
            output.contains("(3, 4)"),
            "Should pass arguments 3, 4 to the block. Got: {output}"
        );
    }

    #[test]
    fn test_block_while_true_loop() {
        // [counter < 5] whileTrue: [counter := counter + 1]
        let mut generator = CoreErlangGenerator::new("test");

        // Condition block: [counter < 5]
        let condition_block = Block::new(
            vec![],
            vec![Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "counter",
                    Span::new(1, 8),
                ))),
                selector: MessageSelector::Binary("<".into()),
                arguments: vec![Expression::Literal(Literal::Integer(5), Span::new(11, 12))],
                span: Span::new(1, 12),
            }],
            Span::new(0, 13),
        );

        // Body block: [counter := counter + 1]
        let body_block = Block::new(
            vec![],
            vec![Expression::Literal(Literal::Integer(0), Span::new(0, 1))], // simplified body
            Span::new(15, 38),
        );

        let receiver = Expression::Block(condition_block);
        let selector =
            MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(14, 24))]);
        let arguments = vec![Expression::Block(body_block)];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("letrec"),
            "whileTrue: should generate a letrec for looping. Got: {output}"
        );
        assert!(
            output.contains("case apply"),
            "whileTrue: should have case on condition result. Got: {output}"
        );
        assert!(
            output.contains("<'true'> when 'true'"),
            "whileTrue: should match on true to continue. Got: {output}"
        );
        assert!(
            output.contains("<'false'> when 'true' -> 'nil'"),
            "whileTrue: should return nil when condition is false. Got: {output}"
        );
        assert!(
            !output.contains("beamtalk_future"),
            "whileTrue: should NOT create futures. Got: {output}"
        );
    }

    #[test]
    fn test_block_while_false_loop() {
        // [done] whileFalse: [process next]
        let mut generator = CoreErlangGenerator::new("test");

        let condition_block = Block::new(
            vec![],
            vec![Expression::Identifier(Identifier::new(
                "done",
                Span::new(1, 5),
            ))],
            Span::new(0, 6),
        );
        let body_block = Block::new(vec![], vec![], Span::new(8, 10));

        let receiver = Expression::Block(condition_block);
        let selector =
            MessageSelector::Keyword(vec![KeywordPart::new("whileFalse:", Span::new(7, 18))]);
        let arguments = vec![Expression::Block(body_block)];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("letrec"),
            "whileFalse: should generate a letrec for looping. Got: {output}"
        );
        assert!(
            output.contains("<'false'> when 'true' ->"),
            "whileFalse: should continue when condition is false. Got: {output}"
        );
        assert!(
            output.contains("<'true'> when 'true' -> 'nil'"),
            "whileFalse: should stop when condition is true. Got: {output}"
        );
    }

    #[test]
    fn test_block_repeat_infinite_loop() {
        // [process] repeat
        let mut generator = CoreErlangGenerator::new("test");

        let body_block = Block::new(
            vec![],
            vec![Expression::Literal(Literal::Integer(1), Span::new(1, 2))],
            Span::new(0, 3),
        );

        let receiver = Expression::Block(body_block);
        let selector = MessageSelector::Unary("repeat".into());

        let result = generator.generate_message_send(&receiver, &selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("letrec"),
            "repeat should generate a letrec for looping. Got: {output}"
        );
        assert!(
            output.contains("'_Loop"),
            "repeat should create a loop function. Got: {output}"
        );
        // repeat has no condition, just loops forever
        assert!(
            !output.contains("case"),
            "repeat should NOT have a case (no condition). Got: {output}"
        );
        assert!(
            !output.contains("beamtalk_future"),
            "repeat should NOT create futures. Got: {output}"
        );
    }

    #[test]
    fn test_non_block_message_still_creates_future() {
        // Regular message sends should still use async protocol
        // actor increment → should create future
        let mut generator = CoreErlangGenerator::new("test");

        let receiver = Expression::Identifier(Identifier::new("actor", Span::new(0, 5)));
        let selector = MessageSelector::Unary("increment".into());

        let result = generator.generate_message_send(&receiver, &selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("beamtalk_future':'new'()"),
            "Non-block unary messages should create futures. Got: {output}"
        );
        assert!(
            output.contains("gen_server':'cast'("),
            "Non-block messages should use gen_server:cast(). Got: {output}"
        );
    }

    /// End-to-end test that generates Core Erlang for a whileTrue: loop and
    /// compiles it through erlc to verify the output is valid Core Erlang.
    #[test]
    fn test_while_true_compiles_through_erlc() {
        use std::io::Write;
        use std::process::Command;

        // Generate a complete module with a whileTrue: expression
        let mut generator = CoreErlangGenerator::new("test_while_loop");

        // Start module
        generator
            .output
            .push_str("module 'test_while_loop' ['main'/0]\n  attributes []\n\n");
        generator.output.push_str("'main'/0 = fun () ->\n    ");

        // Generate: [true] whileTrue: [42]
        // This creates a loop that runs once (returns nil after first iteration)
        let condition_block = Block::new(
            vec![],
            vec![Expression::Identifier(Identifier::new(
                "false",
                Span::new(1, 6),
            ))],
            Span::new(0, 7),
        );
        let body_block = Block::new(
            vec![],
            vec![Expression::Literal(Literal::Integer(42), Span::new(9, 11))],
            Span::new(8, 12),
        );

        let receiver = Expression::Block(condition_block);
        let selector =
            MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(7, 17))]);
        let arguments = vec![Expression::Block(body_block)];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok(), "Code generation should succeed");

        generator.output.push_str("\n\nend\n");

        // Write to temp file
        let temp_dir = std::env::temp_dir();
        let core_file = temp_dir.join("test_while_loop.core");
        let mut file = std::fs::File::create(&core_file).expect("Failed to create temp file");
        file.write_all(generator.output.as_bytes())
            .expect("Failed to write Core Erlang");

        // Try to compile with erlc
        let output = Command::new("erlc")
            .arg("+from_core")
            .arg("-o")
            .arg(&temp_dir)
            .arg(&core_file)
            .output();

        // Clean up temp files regardless of result
        let _ = std::fs::remove_file(&core_file);
        let beam_file = temp_dir.join("test_while_loop.beam");
        let _ = std::fs::remove_file(&beam_file);

        match output {
            Ok(result) => {
                if !result.status.success() {
                    let stderr = String::from_utf8_lossy(&result.stderr);
                    panic!(
                        "erlc compilation failed.\n\nGenerated Core Erlang:\n{}\n\nerlc error:\n{}",
                        generator.output, stderr
                    );
                }
            }
            Err(e) => {
                // erlc not available, skip this test with a message
                eprintln!("Skipping erlc compilation test: {e}");
            }
        }
    }

    /// Test that internal loop temporaries don't shadow user identifiers.
    ///
    /// This tests that a user variable named "Loop" in a block parameter
    /// is correctly resolved even after generating whileTrue: code.
    #[test]
    fn test_temp_vars_dont_shadow_user_identifiers() {
        let mut generator = CoreErlangGenerator::new("test");

        // First, generate a whileTrue: which creates internal _Loop, _Cond, _Body temps
        let condition_block = Block::new(
            vec![],
            vec![Expression::Identifier(Identifier::new(
                "false",
                Span::new(1, 6),
            ))],
            Span::new(0, 7),
        );
        let body_block = Block::new(vec![], vec![], Span::new(8, 10));

        let receiver = Expression::Block(condition_block);
        let selector =
            MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(7, 17))]);
        let arguments = vec![Expression::Block(body_block)];

        generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap();

        // Clear output for the next test
        generator.output.clear();

        // Now push a scope with a user variable named "Loop"
        generator.push_scope();
        let user_loop_var = generator.fresh_var("Loop");

        // Access the "Loop" identifier - it should resolve to the user's binding
        let loop_id = Identifier::new("Loop", Span::new(0, 4));
        generator.generate_identifier(&loop_id).unwrap();

        let output = &generator.output;

        // The identifier should resolve to the user's variable, not an internal temp
        assert!(
            output.contains(&user_loop_var),
            "User identifier 'Loop' should resolve to user's binding {user_loop_var}, got: {output}"
        );

        generator.pop_scope();
    }

    #[test]
    fn test_generate_empty_map_literal() {
        let mut generator = CoreErlangGenerator::new("test");
        let pairs = vec![];
        let result = generator.generate_map_literal(&pairs);
        assert!(result.is_ok());
        assert_eq!(generator.output.trim(), "~{}~");
    }

    #[test]
    fn test_generate_map_literal_with_atoms() {
        let mut generator = CoreErlangGenerator::new("test");

        let pairs = vec![
            MapPair::new(
                Expression::Literal(Literal::Symbol("name".into()), Span::new(2, 7)),
                Expression::Literal(Literal::String("Alice".into()), Span::new(11, 18)),
                Span::new(2, 18),
            ),
            MapPair::new(
                Expression::Literal(Literal::Symbol("age".into()), Span::new(20, 24)),
                Expression::Literal(Literal::Integer(30), Span::new(28, 30)),
                Span::new(20, 30),
            ),
        ];

        let result = generator.generate_map_literal(&pairs);
        assert!(result.is_ok());
        // Symbols become atoms in Core Erlang
        assert!(
            generator.output.contains("'name'"),
            "Output should contain 'name': {}",
            generator.output
        );
        // Strings are represented as binaries with character codes
        assert!(
            generator.output.contains("#<65>"),
            "Output should contain character code for 'A': {}",
            generator.output
        );
        assert!(
            generator.output.contains("'age'"),
            "Output should contain 'age': {}",
            generator.output
        );
        assert!(
            generator.output.contains("30"),
            "Output should contain 30: {}",
            generator.output
        );
    }

    #[test]
    fn test_generate_map_literal_compiles() {
        use std::fs;
        use std::process::Command;

        let pairs = vec![MapPair::new(
            Expression::Literal(Literal::Symbol("key".into()), Span::new(2, 6)),
            Expression::Literal(Literal::String("value".into()), Span::new(10, 17)),
            Span::new(2, 17),
        )];

        let map_expr = Expression::MapLiteral {
            pairs,
            span: Span::new(0, 19),
        };

        let code =
            generate_repl_expression(&map_expr, "test_map_lit").expect("codegen should succeed");

        // Verify the generated Core Erlang contains the map literal syntax
        assert!(
            code.contains("~{"),
            "Should contain Core Erlang map syntax ~{{"
        );
        // Symbols become atoms in Core Erlang
        assert!(code.contains("'key'"));
        // Strings are represented as binaries with character codes
        assert!(
            code.contains("#<"),
            "String should be represented as binary"
        );

        // Try to compile with erlc if available
        let temp_dir = std::env::temp_dir();
        let core_file = temp_dir.join("test_map_lit.core");
        if let Ok(()) = fs::write(&core_file, &code) {
            let output = Command::new("erlc")
                .arg("+from_core")
                .arg(&core_file)
                .current_dir(&temp_dir)
                .output();

            // Clean up
            let _ = fs::remove_file(&core_file);
            let beam_file = temp_dir.join("test_map_lit.beam");
            let _ = fs::remove_file(&beam_file);

            // Check compilation result if erlc is available
            if let Ok(output) = output {
                assert!(
                    output.status.success(),
                    "erlc compilation of map literal failed:\nstdout: {}\nstderr: {}\nGenerated code:\n{}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                    code
                );
            }
        }
    }

    // ========================================================================
    // Dictionary Method Code Generation Tests
    // ========================================================================

    #[test]
    fn test_dictionary_at_on_identifier() {
        // person at: #name -> maps:get('name', Person)
        let mut generator = CoreErlangGenerator::new("test");
        generator.push_scope();
        generator.bind_var("person", "Person");

        let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
        let selector = MessageSelector::Keyword(vec![KeywordPart::new("at:", Span::new(7, 10))]);
        let arguments = vec![Expression::Literal(
            Literal::Symbol("name".into()),
            Span::new(11, 16),
        )];

        generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap();
        let output = &generator.output;

        assert!(
            output.contains("call 'maps':'get'('name', Person)"),
            "Should generate maps:get call. Got: {output}"
        );
        // Should NOT create a future (synchronous operation)
        assert!(
            !output.contains("beamtalk_future"),
            "Dictionary methods should be synchronous. Got: {output}"
        );
    }

    #[test]
    fn test_dictionary_at_put_on_identifier() {
        // person at: #age put: 31 -> maps:put('age', 31, Person)
        let mut generator = CoreErlangGenerator::new("test");
        generator.push_scope();
        generator.bind_var("person", "Person");

        let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::new(7, 10)),
            KeywordPart::new("put:", Span::new(14, 18)),
        ]);
        let arguments = vec![
            Expression::Literal(Literal::Symbol("age".into()), Span::new(11, 14)),
            Expression::Literal(Literal::Integer(31), Span::new(19, 21)),
        ];

        generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap();
        let output = &generator.output;

        assert!(
            output.contains("call 'maps':'put'('age', 31, Person)"),
            "Should generate maps:put call. Got: {output}"
        );
    }

    #[test]
    fn test_dictionary_size_on_identifier() {
        // person size -> maps:size(Person)
        let mut generator = CoreErlangGenerator::new("test");
        generator.push_scope();
        generator.bind_var("person", "Person");

        let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
        let selector = MessageSelector::Unary("size".into());

        generator
            .generate_message_send(&receiver, &selector, &[])
            .unwrap();
        let output = &generator.output;

        assert!(
            output.contains("call 'maps':'size'(Person)"),
            "Should generate maps:size call. Got: {output}"
        );
    }

    // ========================================================================
    // Cascade Code Generation Tests (BT-86)
    // ========================================================================

    #[test]
    fn test_cascade_unary_messages() {
        // x negated; abs  (two unary messages to x)
        // Parser creates:
        // - receiver: MessageSend { receiver: Identifier(x), selector: Unary(negated), args: [] }
        // - messages: [CascadeMessage { selector: Unary(abs), args: [] }]
        let mut generator = CoreErlangGenerator::new("test");
        generator.push_scope();

        let x_ident = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
        let first_msg = Expression::MessageSend {
            receiver: Box::new(x_ident),
            selector: MessageSelector::Unary("negated".into()),
            arguments: vec![],
            span: Span::new(0, 9),
        };

        let cascade = Expression::Cascade {
            receiver: Box::new(first_msg),
            messages: vec![CascadeMessage::new(
                MessageSelector::Unary("abs".into()),
                vec![],
                Span::new(11, 14),
            )],
            span: Span::new(0, 14),
        };

        generator.generate_expression(&cascade).unwrap();
        let output = &generator.output;

        // Should bind the underlying receiver (x) once
        assert!(
            output.contains("let _Receiver1 = call 'maps':'get'('x', State) in"),
            "Should bind the underlying receiver x. Got: {output}"
        );

        // Should send BOTH messages (negated AND abs) to the receiver
        assert!(
            output.contains("'negated'"),
            "Should send first message 'negated'. Got: {output}"
        );
        assert!(
            output.contains("'abs'"),
            "Should send second message 'abs'. Got: {output}"
        );

        // Should use gen_server:cast for async message sends with extracted pid
        assert!(
            output.contains("call 'gen_server':'cast'(_Pid"),
            "Should send messages to the extracted pid from receiver object. Got: {output}"
        );

        // Should extract pid from beamtalk_object record
        assert!(
            output.contains("call 'erlang':'element'(4, _Receiver"),
            "Should extract pid from #beamtalk_object{{}} record. Got: {output}"
        );

        // Should create futures for async messages
        assert!(
            output.contains("call 'beamtalk_future':'new'()"),
            "Should create futures for async messages. Got: {output}"
        );

        generator.pop_scope();
    }

    #[test]
    fn test_cascade_keyword_messages() {
        // collection at: 1 put: 'a'; at: 2 put: 'b'; size
        // Parser creates:
        // - receiver: MessageSend { receiver: Identifier(collection), selector: Keyword(at:put:), args: [1, 'a'] }
        // - messages: [CascadeMessage { selector: Keyword(at:put:), args: [2, 'b'] },
        //              CascadeMessage { selector: Unary(size), args: [] }]
        let mut generator = CoreErlangGenerator::new("test");
        generator.push_scope();

        let collection_ident =
            Expression::Identifier(Identifier::new("collection", Span::new(0, 10)));
        let first_msg = Expression::MessageSend {
            receiver: Box::new(collection_ident),
            selector: MessageSelector::Keyword(vec![
                KeywordPart::new("at:", Span::new(11, 14)),
                KeywordPart::new("put:", Span::new(16, 20)),
            ]),
            arguments: vec![
                Expression::Literal(Literal::Integer(1), Span::new(15, 16)),
                Expression::Literal(Literal::String("a".into()), Span::new(21, 24)),
            ],
            span: Span::new(0, 24),
        };

        let cascade = Expression::Cascade {
            receiver: Box::new(first_msg),
            messages: vec![
                CascadeMessage::new(
                    MessageSelector::Keyword(vec![
                        KeywordPart::new("at:", Span::new(26, 29)),
                        KeywordPart::new("put:", Span::new(31, 35)),
                    ]),
                    vec![
                        Expression::Literal(Literal::Integer(2), Span::new(30, 31)),
                        Expression::Literal(Literal::String("b".into()), Span::new(36, 39)),
                    ],
                    Span::new(26, 39),
                ),
                CascadeMessage::new(
                    MessageSelector::Unary("size".into()),
                    vec![],
                    Span::new(41, 45),
                ),
            ],
            span: Span::new(0, 45),
        };

        generator.generate_expression(&cascade).unwrap();
        let output = &generator.output;

        // Should bind the underlying receiver (collection) once
        assert!(
            output.contains("let _Receiver1 = call 'maps':'get'('collection', State) in"),
            "Should bind the underlying receiver collection. Got: {output}"
        );

        // Should send all three messages
        assert!(
            output.contains("'at:put:'"),
            "Should send keyword message 'at:put:'. Got: {output}"
        );
        assert!(
            output.contains("'size'"),
            "Should send unary message 'size'. Got: {output}"
        );

        generator.pop_scope();
    }

    #[test]
    fn test_cascade_binary_selector_error() {
        // counter + 1; negated  (binary selector in cascade - should error)
        let mut generator = CoreErlangGenerator::new("test");
        generator.push_scope();

        let counter_ident = Expression::Identifier(Identifier::new("counter", Span::new(0, 7)));
        let first_msg = Expression::MessageSend {
            receiver: Box::new(counter_ident),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(10, 11))],
            span: Span::new(0, 11),
        };

        let cascade = Expression::Cascade {
            receiver: Box::new(first_msg),
            messages: vec![CascadeMessage::new(
                MessageSelector::Unary("negated".into()),
                vec![],
                Span::new(13, 20),
            )],
            span: Span::new(0, 20),
        };

        let result = generator.generate_expression(&cascade);

        // Binary selectors in cascades should return UnsupportedFeature error, not Internal
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            matches!(err, CodeGenError::UnsupportedFeature { .. }),
            "Binary selectors in cascades should return UnsupportedFeature, got: {err:?}"
        );

        generator.pop_scope();
    }

    #[test]
    fn test_cascade_repl_expression() {
        // Test cascade in a full REPL module context
        // x negated; abs
        let x_ident = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
        let first_msg = Expression::MessageSend {
            receiver: Box::new(x_ident),
            selector: MessageSelector::Unary("negated".into()),
            arguments: vec![],
            span: Span::new(0, 9),
        };

        let cascade = Expression::Cascade {
            receiver: Box::new(first_msg),
            messages: vec![CascadeMessage::new(
                MessageSelector::Unary("abs".into()),
                vec![],
                Span::new(11, 14),
            )],
            span: Span::new(0, 14),
        };

        let code = generate_repl_expression(&cascade, "test_cascade").expect("codegen should work");

        // Should have module structure
        assert!(
            code.contains("module 'test_cascade' ['eval'/1]"),
            "Should have module header. Got:\n{code}"
        );

        // Should bind the underlying receiver once
        assert!(
            code.contains("let _Receiver1 = call 'maps':'get'('x', State) in"),
            "Should bind receiver x from State. Got:\n{code}"
        );

        // Should send both messages
        assert!(
            code.contains("'negated'"),
            "Should have first message negated. Got:\n{code}"
        );
        assert!(
            code.contains("'abs'"),
            "Should have second message abs. Got:\n{code}"
        );
    }
}

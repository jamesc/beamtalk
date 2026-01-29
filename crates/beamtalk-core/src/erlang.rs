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

use crate::ast::{Block, Expression, Identifier, Literal, MessageSelector, Module};
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
/// use beamtalk_core::erlang::generate;
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::parse::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let core_erlang = generate(&module)?;
/// println!("{}", core_erlang);
/// # Ok::<(), beamtalk_core::erlang::CodeGenError>(())
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

/// Core Erlang code generator.
///
/// This struct maintains state during code generation, including
/// variable name generation and indentation tracking.
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

    /// Generates a full module with `gen_server` behaviour.
    fn generate_module(&mut self, module: &Module) -> Result<()> {
        // Module header
        writeln!(
            self.output,
            "module '{}' ['init'/1, 'handle_cast'/2, 'handle_call'/3, 'code_change'/3, 'dispatch'/3, 'method_table'/0, 'spawn'/0]",
            self.module_name
        )?;
        writeln!(self.output, "  attributes ['behaviour' = ['gen_server']]")?;
        writeln!(self.output)?;

        // Generate spawn/0 function (class method to instantiate actors)
        self.generate_spawn_function(module)?;
        writeln!(self.output)?;

        // Generate init/1 function
        self.generate_init_function(module)?;
        writeln!(self.output)?;

        // Generate handle_cast/2 function
        self.generate_handle_cast()?;
        writeln!(self.output)?;

        // Generate handle_call/3 function
        self.generate_handle_call()?;
        writeln!(self.output)?;

        // Generate code_change/3 function
        self.generate_code_change()?;
        writeln!(self.output)?;

        // Generate dispatch function
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

    /// Generates the `spawn/0` class method for creating actor instances.
    ///
    /// This is a class-level method that instantiates a new actor process
    /// using `gen_server:start_link/3`. The function:
    /// 1. Calls `gen_server:start_link/3` with empty args (init/1 creates the state)
    /// 2. Extracts and returns the process Pid, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/0 = fun () ->
    ///     case call 'gen_server':'start_link'('counter', [], []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             Pid;
    ///         <{'error', Reason}> when 'true' ->
    ///             call 'erlang':'error'({'spawn_failed', Reason})
    ///     end
    /// ```
    ///
    /// Note: The `module` parameter is currently unused because field initialization
    /// is handled by `init/1`. It's kept for future compatibility when parameterized
    /// spawn (e.g., `Counter spawn initial: 10`) passes arguments to init.
    fn generate_spawn_function(&mut self, _module: &Module) -> Result<()> {
        writeln!(self.output, "'spawn'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'gen_server':'start_link'('{}', [], []) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', Pid}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "Pid")?;
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
    fn generate_init_function(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'init'/1 = fun (_Args) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let InitialState = ~{{")?;
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
        writeln!(self.output, "in {{'ok', InitialState}}")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_cast/2` callback for async message sends.
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
        writeln!(
            self.output,
            "case call '{}':'dispatch'(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
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
        writeln!(
            self.output,
            "case call '{}':'dispatch'(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'reply', Result, NewState}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', Result, NewState}}")?;
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

    /// Generates the dispatch/3 function for message routing.
    fn generate_dispatch(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'dispatch'/3 = fun (Selector, Args, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Selector of")?;
        self.indent += 1;

        // Generate case clause for each method in the module
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let (Expression::Identifier(id), Expression::Block(block)) =
                    (target.as_ref(), value.as_ref())
                {
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

                    self.write_indent()?;
                    write!(self.output, "let Result = ")?;
                    self.generate_block_body(block)?;
                    writeln!(self.output)?;
                    self.write_indent()?;
                    writeln!(self.output, "in {{'reply', Result, State}}")?;

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

        // Default case for unknown messages
        self.write_indent()?;
        writeln!(self.output, "<_> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'reply', {{'error', 'does_not_understand'}}, State}}"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `method_table/0` function.
    fn generate_method_table(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'method_table'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        write!(self.output, "~{{")?;

        let methods: Vec<_> = module
            .expressions
            .iter()
            .filter_map(|expr| {
                if let Expression::Assignment { target, value, .. } = expr {
                    if let (Expression::Identifier(id), Expression::Block(block)) =
                        (target.as_ref(), value.as_ref())
                    {
                        return Some((id.name.as_str(), block.arity()));
                    }
                }
                None
            })
            .collect();

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
            Expression::Block(block) => self.generate_block(block),
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => self.generate_message_send(receiver, selector, arguments),
            Expression::Assignment { value, .. } => {
                // In REPL context and elsewhere, assignment returns the assigned value
                // The REPL extracts the variable name separately and updates bindings
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
                    // Field access from state
                    write!(self.output, "call 'maps':'get'('{}', State)", id.name)?;
                }
            }
        }
        Ok(())
    }

    /// Generates code for field access (e.g., self.value).
    fn generate_field_access(&mut self, receiver: &Expression, field: &Identifier) -> Result<()> {
        // For now, assume receiver is 'self' and access from State
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
                write!(self.output, "call 'maps':'get'('{}', State)", field.name)?;
                return Ok(());
            }
        }

        Err(CodeGenError::UnsupportedFeature {
            feature: "complex field access".to_string(),
            location: format!("{:?}", receiver.span()),
        })
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

    /// Generates the body of a block.
    fn generate_block_body(&mut self, block: &Block) -> Result<()> {
        if block.body.is_empty() {
            write!(self.output, "'nil'")?;
            return Ok(());
        }

        // Generate body expressions in sequence
        if block.body.len() == 1 {
            self.generate_expression(&block.body[0])?;
        } else {
            // Multiple expressions need to be sequenced with do
            write!(self.output, "do ")?;
            for (i, expr) in block.body.iter().enumerate() {
                if i > 0 {
                    write!(self.output, " ")?;
                }
                self.generate_expression(expr)?;
            }
        }
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
    /// - `spawn` - When the selector is "spawn" on a class identifier, generates
    ///   a call to the module's `spawn/0` function instead
    /// - `await` - When the selector is "await", generates a blocking await operation
    fn generate_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<()> {
        // For binary operators, use Erlang's built-in operators (these are synchronous)
        if let MessageSelector::Binary(op) = selector {
            return self.generate_binary_op(op, receiver, arguments);
        }

        // Special case: Block evaluation messages (value, value:, whileTrue:, etc.)
        // These are synchronous function calls, not async actor messages
        if let Some(result) = self.try_generate_block_message(receiver, selector, arguments)? {
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

        // Generate the async message send protocol:
        // let Future1 = call 'beamtalk_future':'new'()
        // in let _ = call 'gen_server':'cast'(ReceiverPid, {Selector, Args, Future1})
        //    in Future1
        //
        // Use fresh_var to avoid shadowing in nested message sends

        let future_var = self.fresh_var("Future");
        write!(
            self.output,
            "let {future_var} = call 'beamtalk_future':'new'() in "
        )?;

        // Build the message tuple: {Selector, Args, Future}
        write!(self.output, "let _ = call 'gen_server':'cast'(")?;

        // Receiver evaluation
        self.generate_expression(receiver)?;
        write!(self.output, ", {{")?;

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

                    // Not a block evaluation message
                    _ => Ok(None),
                }
            }

            // Not a block evaluation message
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
        let fun_var = self.fresh_var("Fun");
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

        let loop_fn = self.fresh_var("Loop");
        let condition_var = self.fresh_var("Cond");
        let body_var = self.fresh_var("Body");

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

        let loop_fn = self.fresh_var("Loop");
        let condition_var = self.fresh_var("Cond");
        let body_var = self.fresh_var("Body");

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
        let loop_fn = self.fresh_var("Loop");
        let body_var = self.fresh_var("Body");

        write!(self.output, "let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(self.output, " in letrec '{loop_fn}'/0 = fun () -> ")?;
        write!(self.output, "do apply {body_var} () ")?;
        write!(self.output, "apply '{loop_fn}'/0 () ")?;
        write!(self.output, "in apply '{loop_fn}'/0 ()")?;

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

        let erlang_op = match op {
            "+" => "+",
            "-" => "-",
            "*" => "*",
            "/" => "/",
            "%" => "rem",
            "==" => "==",
            "!=" => "/=",
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
    fn fresh_var(&mut self, base: &str) -> String {
        self.var_counter += 1;
        let var_name = format!("_{}{}", base.replace('_', ""), self.var_counter);
        // Insert into the current (innermost) scope
        if let Some(current_scope) = self.var_scopes.last_mut() {
            current_scope.insert(base.to_string(), var_name.clone());
        }
        var_name
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
        // Only literal values are supported for now
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let Expression::Identifier(id) = target.as_ref() {
                    // Only generate field if it's a simple literal or block
                    if matches!(
                        value.as_ref(),
                        Expression::Literal(..) | Expression::Block(_)
                    ) {
                        self.write_indent()?;
                        write!(self.output, ", '{}' => ", id.name)?;
                        self.generate_expression(value)?;
                        writeln!(self.output)?;
                    }
                }
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

        // Build: array at: 1 put: 'x'
        let receiver = Expression::Identifier(Identifier::new("array", Span::new(0, 5)));
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::new(6, 9)),
            KeywordPart::new("put:", Span::new(12, 16)),
        ]);
        // Arguments are passed separately to generate_message_send
        let arguments = vec![
            Expression::Literal(Literal::Integer(1), Span::new(10, 11)),
            Expression::Literal(Literal::String("x".into()), Span::new(17, 20)),
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
            output.contains("'at:put:'"),
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
        // Should have TWO different future variables: _Future1 and _Future2
        assert!(
            output.contains("_Future1") && output.contains("_Future2"),
            "Nested message sends should use unique future variables. Got: {output}"
        );
        // Each variable should appear 3 times (bind, send arg, return)
        assert_eq!(
            output.matches("_Future1").count(),
            3,
            "Inner future variable should appear 3 times. Got: {output}"
        );
        assert_eq!(
            output.matches("_Future2").count(),
            3,
            "Outer future variable should appear 3 times. Got: {output}"
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

        // Check that spawn/0 is exported
        assert!(code.contains("'spawn'/0"));

        // Check that spawn function exists and calls gen_server:start_link
        assert!(code.contains("'spawn'/0 = fun () ->"));

        // Check that it calls gen_server:start_link with empty args (init creates state)
        assert!(code.contains("call 'gen_server':'start_link'('counter', [], [])"));

        // Check that it uses a case expression to extract the Pid
        assert!(code.contains("case call 'gen_server':'start_link'"));
        assert!(code.contains("<{'ok', Pid}> when 'true' ->"));
        assert!(code.contains("Pid"));

        // Check that it handles errors
        assert!(code.contains("<{'error', Reason}> when 'true' ->"));
        assert!(code.contains("call 'erlang':'error'({'spawn_failed', Reason})"));

        // Check that init/1 creates the initial state with fields
        assert!(code.contains("'init'/1 = fun (_Args) ->"));
        assert!(code.contains("let InitialState = ~{"));
        assert!(code.contains("'__class__' => 'Counter'"));
        assert!(code.contains("'__methods__' => call 'counter':'method_table'()"));
        assert!(code.contains("'value' => 0"));
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
}

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
//! module 'counter' ['init'/0, 'handle_cast'/2]
//!   attributes ['-behaviour'(['gen_server'])]
//!
//! 'init'/1 = fun (_Args) ->
//!     let InitialState = #{
//!       '__class__' => 'Counter',
//!       '__methods__' => call 'counter':'method_table'/0(),
//!       'value' => 0
//!     }
//!     in {'ok', InitialState}
//!
//! 'handle_cast'/2 = fun ({Selector, Args, FuturePid}, State) ->
//!     case call 'counter':'dispatch'/3(Selector, Args, State) of
//!       {'reply', Result, NewState} ->
//!         FuturePid ! {'resolved', Result},
//!         {'noreply', NewState}
//!     end
//! ```
//!
//! # Core Erlang Syntax
//!
//! Core Erlang is a simplified functional IR for Erlang:
//!
//! - **Atoms**: `'atom_name'` (always quoted)
//! - **Variables**: `VariableName` (starts with uppercase)
//! - **Function calls**: `call 'module':'function'/arity(args)`
//! - **Let bindings**: `let Var = Expr in Body`
//! - **Case expressions**: `case Expr of Pattern -> Body end`
//! - **Maps**: `#{'key' => value}`
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
    /// Map of identifiers to Core Erlang variable names.
    var_bindings: HashMap<String, String>,
}

impl CoreErlangGenerator {
    /// Creates a new code generator for the given module name.
    fn new(module_name: &str) -> Self {
        Self {
            module_name: module_name.to_string(),
            output: String::new(),
            indent: 0,
            var_counter: 0,
            var_bindings: HashMap::new(),
        }
    }

    /// Generates a full module with `gen_server` behaviour.
    fn generate_module(&mut self, module: &Module) -> Result<()> {
        // Module header
        writeln!(
            self.output,
            "module '{}' ['init'/1, 'handle_cast'/2, 'handle_call'/3, 'code_change'/3]",
            self.module_name
        )?;
        writeln!(self.output, "  attributes ['-behaviour'(['gen_server'])]")?;
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

        Ok(())
    }

    /// Generates the `init/1` callback for `gen_server`.
    fn generate_init_function(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'init'/1 = fun (_Args) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let InitialState = #{{")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'__class__' => '{}',", self.to_class_name())?;
        self.write_indent()?;
        writeln!(
            self.output,
            "'__methods__' => call '{}':'method_table'/0(){{}}",
            self.module_name
        )?;

        // Initialize fields from module expressions (assignments at top level)
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let Expression::Identifier(id) = target.as_ref() {
                    self.write_indent()?;
                    write!(self.output, ", '{}' => ", id.name)?;
                    self.generate_expression(value)?;
                    writeln!(self.output)?;
                }
            }
        }

        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "}}")?;
        self.write_indent()?;
        writeln!(self.output, "in {{'ok', InitialState}}")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_cast/2` callback for async message sends.
    fn generate_handle_cast(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "'handle_cast'/2 = fun ({{Selector, Args, FuturePid}}, State) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call '{}':'dispatch'/3(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', Result, NewState}} ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "do FuturePid ! {{'resolved', Result}}")?;
        self.write_indent()?;
        writeln!(self.output, "{{'noreply', NewState}}")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_call/3` callback for sync message sends.
    fn generate_handle_call(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "'handle_call'/3 = fun ({{Selector, Args}}, _From, State) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call '{}':'dispatch'/3(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', Result, NewState}} ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', Result, NewState}}")?;
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
                    self.write_indent()?;
                    write!(self.output, "'{}' ->", id.name)?;
                    writeln!(self.output)?;
                    self.indent += 1;
                    self.write_indent()?;
                    write!(self.output, "let Result = ")?;
                    self.generate_block_body(block)?;
                    writeln!(self.output)?;
                    self.write_indent()?;
                    writeln!(self.output, "in {{'reply', Result, State}}")?;
                    self.indent -= 1;
                }
            }
        }

        // Default case for unknown messages
        self.write_indent()?;
        writeln!(self.output, "_ ->")?;
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
        write!(self.output, "#{{")?;

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

        writeln!(self.output, "}}")?;
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
            Expression::Assignment { .. } => {
                // Assignments in expressions are not yet supported
                Err(CodeGenError::UnsupportedFeature {
                    feature: "assignment in expression".to_string(),
                    location: format!("{:?}", expr.span()),
                })
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
                // Core Erlang strings are binaries
                write!(self.output, "<<")?;
                for (i, ch) in s.chars().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ")?;
                    }
                    write!(self.output, "{}", ch as u32)?;
                }
                write!(self.output, ">>")?;
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
        // Check if it's a bound variable
        if let Some(var_name) = self.var_bindings.get(id.name.as_str()) {
            write!(self.output, "{var_name}")?;
        } else {
            // Field access from state
            write!(self.output, "call 'maps':'get'/2('{}', State)", id.name)?;
        }
        Ok(())
    }

    /// Generates code for field access (e.g., self.value).
    fn generate_field_access(&mut self, receiver: &Expression, field: &Identifier) -> Result<()> {
        // For now, assume receiver is 'self' and access from State
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
                write!(self.output, "call 'maps':'get'/2('{}', State)", field.name)?;
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
    fn generate_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<()> {
        // For binary operators, use Erlang's built-in operators
        if let MessageSelector::Binary(op) = selector {
            return self.generate_binary_op(op, receiver, arguments);
        }

        // For other messages, this would be an actor send
        // For now, treat as local function call
        write!(self.output, "call '{}':'", self.module_name)?;
        match selector {
            MessageSelector::Unary(name) => write!(self.output, "{name}'")?,
            MessageSelector::Binary(op) => write!(self.output, "{op}'")?,
            MessageSelector::Keyword(parts) => {
                let name = parts.iter().map(|p| p.keyword.as_str()).collect::<String>();
                write!(self.output, "{name}'")?;
            }
        }
        write!(self.output, "/{}", arguments.len())?;
        write!(self.output, "(")?;
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
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

        write!(self.output, "call 'erlang':'{erlang_op}'/2(")?;
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

    /// Generates a fresh variable name.
    fn fresh_var(&mut self, base: &str) -> String {
        self.var_counter += 1;
        let var_name = format!("_{}{}", base.replace('_', ""), self.var_counter);
        self.var_bindings.insert(base.to_string(), var_name.clone());
        var_name
    }

    /// Converts module name to class name.
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
        assert!(code.contains("'-behaviour'(['gen_server'])"));
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
    fn test_generate_binary_op_addition() {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
        let right = vec![Expression::Literal(Literal::Integer(4), Span::new(4, 5))];
        let result = generator.generate_binary_op("+", &left, &right);
        assert!(result.is_ok());
        assert!(generator.output.contains("call 'erlang':'+'/2(3, 4)"));
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
}

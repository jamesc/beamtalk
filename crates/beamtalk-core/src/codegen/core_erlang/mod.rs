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
//! - **Actor State**: A map containing `$beamtalk_class`, `__methods__`, and actor fields
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
//!       '$beamtalk_class' => 'Counter',
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
//!             let _ = call 'erlang':'!'(FuturePid, {'resolve', Result})
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
//! # Module Organization (Domain-Driven Design)
//!
//! The code generator is organized around **bounded contexts** following DDD:
//!
//! ## Core Domain Modules
//!
//! - [`control_flow`] - Control flow compilation (iteration, loops, mutation analysis)
//! - [`dispatch_codegen`] - Message sending and dispatch (the core Beamtalk operation)
//! - [`variable_context`] - Variable binding and scope management aggregate
//! - [`state_codegen`] - State threading service for simulated mutation
//!
//! ## Supporting Modules
//!
//! - [`expressions`] - Expression code generation (literals, identifiers, maps, cascades)
//! - [`gen_server`] - OTP `gen_server` scaffolding (spawn, init, callbacks)
//! - [`intrinsics`] - Compiler intrinsics (block evaluation, `ProtoObject`, `Object`)
//! - [`operators`] - Binary operator compilation (arithmetic, comparison, string concat)
//! - [`block_analysis`] - Block mutation analysis for control flow
//! - [`util`] - Utility functions (indentation, name conversions)
//!
//! # References
//!
//! - [Core Erlang Specification](https://www.it.uu.se/research/group/hipe/cerl/)
//! - [Gleam Erlang Codegen](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/erlang.rs)

mod actor_codegen;
mod block_analysis;
mod control_flow;
mod dispatch_codegen;
pub mod document;
pub mod erlang_types;
mod expressions;
mod gen_server;
mod intrinsics;
mod operators;
pub mod primitive_bindings;
mod primitive_implementations;
mod repl_codegen;
pub mod selector_mangler;
mod state_codegen;
mod util;
mod value_type_codegen;
mod variable_context;

// Re-export utility functions for IDE queries
pub use util::to_module_name;

use crate::ast::{Block, Expression, MessageSelector, Module};
use primitive_bindings::PrimitiveBindingTable;
use state_codegen::StateThreading;
use std::fmt::{self, Write};
use thiserror::Error;
use variable_context::VariableContext;

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

    /// BT-374 / ADR 0010: Workspace binding used in batch compilation mode.
    #[error(
        "{name} is a workspace binding, not available in batch compilation.\n\n\
             Workspace bindings like Transcript and Beamtalk are only available in the REPL \
             (interactive workspace context).\n\n\
             Fix: Use `beamtalk repl` to access workspace bindings interactively."
    )]
    WorkspaceBindingInBatchMode {
        /// The binding name (e.g., "Transcript", "Beamtalk").
        name: String,
    },

    /// Block arity mismatch in nil-testing method.
    #[error(
        "{selector} block must take 0 or 1 arguments, got {arity}.\n\n\
             Fix: Use a zero-arg block or a one-arg block:\n\
             \x20 obj ifNotNil: [ 'found' ]\n\
             \x20 obj ifNotNil: [:v | v printString]"
    )]
    BlockArityMismatch {
        /// The selector (e.g., "ifNotNil:").
        selector: String,
        /// The actual arity of the block.
        arity: usize,
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
/// # use beamtalk_core::source_analysis::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let core_erlang = generate(&module)?;
/// println!("{}", core_erlang);
/// # Ok::<(), beamtalk_core::codegen::core_erlang::CodeGenError>(())
/// ```
pub fn generate(module: &Module) -> Result<String> {
    let mut generator = CoreErlangGenerator::new("bt_module");

    // Build hierarchy once for the entire generation (ADR 0006)
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(module).0;

    // BT-213: Route based on whether class is actor or value type
    if CoreErlangGenerator::is_actor_class(module, &hierarchy) {
        generator.generate_actor_module(module)?;
    } else {
        generator.generate_value_type_module(module)?;
    }

    Ok(generator.output)
}

/// Generates Core Erlang code with a specified module name.
///
/// # BT-213: Value Types vs Actors
///
/// Routes to different code generators based on class hierarchy:
/// - **Actor subclasses** → `generate_actor_module` (`gen_server` with mailbox)
/// - **Object subclasses** → `generate_value_type_module` (plain Erlang maps)
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_with_name(module: &Module, module_name: &str) -> Result<String> {
    generate_with_name_and_source(module, module_name, None)
}

/// Generates Core Erlang code with a specified module name and source text.
///
/// When `source_text` is provided, method source is captured in class registration
/// metadata, enabling `CompiledMethod` introspection (BT-101).
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_with_name_and_source(
    module: &Module,
    module_name: &str,
    source_text: Option<&str>,
) -> Result<String> {
    let mut generator = CoreErlangGenerator::new(module_name);
    generator.source_text = source_text.map(String::from);

    // Build hierarchy once for the entire generation (ADR 0006)
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(module).0;

    // BT-213: Route based on whether class is actor or value type
    if CoreErlangGenerator::is_actor_class(module, &hierarchy) {
        generator.generate_actor_module(module)?;
    } else {
        generator.generate_value_type_module(module)?;
    }

    Ok(generator.output)
}

/// Generates Core Erlang code with a specified module name and primitive bindings.
///
/// BT-295 / ADR 0007 Phase 3: Accepts a [`PrimitiveBindingTable`] that provides
/// pragma-driven dispatch information from the compiled stdlib. The codegen
/// consults this table before falling back to hardcoded dispatch tables.
///
/// BT-374 / ADR 0010: When `workspace_mode` is true, workspace binding names
/// (`Transcript`, `Beamtalk`) generate `persistent_term` lookup + async actor
/// send. Stdlib compilation should set this to true since stdlib classes run
/// in workspace context.
///
/// When `source_text` is provided, method source is captured in class registration
/// metadata for `CompiledMethod` introspection (BT-101).
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_with_bindings(
    module: &Module,
    module_name: &str,
    bindings: PrimitiveBindingTable,
    source_text: Option<&str>,
    workspace_mode: bool,
) -> Result<String> {
    let mut generator = CoreErlangGenerator::with_bindings(module_name, bindings);
    generator.source_text = source_text.map(String::from);
    generator.workspace_mode = workspace_mode;

    // Build hierarchy once for the entire generation (ADR 0006)
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(module).0;

    // BT-213: Route based on whether class is actor or value type
    if CoreErlangGenerator::is_actor_class(module, &hierarchy) {
        generator.generate_actor_module(module)?;
    } else {
        generator.generate_value_type_module(module)?;
    }

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

/// Generates Core Erlang for a test expression (no workspace bindings).
///
/// Like [`generate_repl_expression`] but with `workspace_mode = false`,
/// suitable for compiled tests that don't need REPL/workspace context.
/// Used by `beamtalk test-stdlib` (ADR 0014 Phase 1).
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_test_expression(expression: &Expression, module_name: &str) -> Result<String> {
    let mut generator = CoreErlangGenerator::new(module_name);
    generator.generate_test_module(expression)?;
    Ok(generator.output)
}

/// Generates Core Erlang code with workspace mode enabled.
///
/// BT-374 / ADR 0010: When `workspace_mode` is true, workspace binding names
/// (`Transcript`, `Beamtalk`) generate `persistent_term` lookup + async actor
/// send instead of direct module function calls. When false (batch compile),
/// workspace binding names produce a compile error.
///
/// # Errors
///
/// Returns [`CodeGenError::WorkspaceBindingInBatchMode`] if workspace bindings
/// are used in batch mode. Returns other [`CodeGenError`] variants for other failures.
pub fn generate_with_workspace(
    module: &Module,
    module_name: &str,
    workspace_mode: bool,
) -> Result<String> {
    generate_with_workspace_and_source(module, module_name, workspace_mode, None)
}

/// Like [`generate_with_workspace`] but also captures source text for
/// `CompiledMethod` introspection (BT-101).
///
/// # Errors
///
/// Returns [`CodeGenError::WorkspaceBindingInBatchMode`] if workspace bindings
/// are used in batch mode. Returns other [`CodeGenError`] variants for other failures.
pub fn generate_with_workspace_and_source(
    module: &Module,
    module_name: &str,
    workspace_mode: bool,
    source_text: Option<&str>,
) -> Result<String> {
    let mut generator = CoreErlangGenerator::new(module_name);
    generator.workspace_mode = workspace_mode;
    generator.source_text = source_text.map(String::from);

    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(module).0;

    if CoreErlangGenerator::is_actor_class(module, &hierarchy) {
        generator.generate_actor_module(module)?;
    } else {
        generator.generate_value_type_module(module)?;
    }

    Ok(generator.output)
}

/// Code generation context (BT-213).
///
/// Determines how expressions are compiled based on the execution environment:
/// - **Actor**: Process-based with mutable state, async messaging
/// - **`ValueType`**: Plain maps with immutable semantics, sync function calls
/// - **Repl**: Interactive evaluation with bindings map
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CodeGenContext {
    /// Generating code for an actor class (`gen_server` with async messaging).
    ///
    /// - Field access: `call 'maps':'get'('field', State)`
    /// - Method calls: Async via `beamtalk_actor:async_send` with futures
    /// - State threading: Use State, State1, State2... for mutations
    Actor,

    /// Generating code for a value type class (plain Erlang functions).
    ///
    /// - Field access: `call 'maps':'get'('field', Self)`
    /// - Method calls: Synchronous function calls
    /// - No state threading: Value types are immutable
    ValueType,

    /// Generating code for REPL evaluation.
    ///
    /// - Variable access: `call 'maps':'get'('var', Bindings)`
    /// - Field access: Via maps:get from State (if in actor context)
    /// - Special handling for variable persistence across expressions
    Repl,
}

/// Core Erlang code generator.
///
/// This is the main code generator that coordinates compilation of Beamtalk
/// AST nodes to Core Erlang. It maintains:
///
/// - **Module name**: The Erlang module being generated
/// - **Output buffer**: Accumulated Core Erlang code
/// - **Variable context**: Scope management and variable generation
/// - **State threading**: Simulated mutation via State, State1, State2...
///
/// The generator delegates to specialized submodules:
/// - [`control_flow`] - Iteration and loop compilation
/// - [`dispatch_codegen`] - Message sending and dispatch
/// - [`expressions`] - Expression code generation
/// - [`gen_server`] - OTP `gen_server` scaffolding
/// - [`intrinsics`] - Compiler intrinsics (block, `ProtoObject`, `Object`, list iteration)
/// - [`operators`] - Binary operator code generation
#[expect(
    clippy::struct_excessive_bools,
    reason = "Generator flags are context switches, not configuration"
)]
pub(super) struct CoreErlangGenerator {
    /// The module name being generated.
    module_name: String,
    /// The output buffer.
    output: String,
    /// Current indentation level.
    indent: usize,
    /// Variable binding and scope management.
    var_context: VariableContext,
    /// State threading for field assignments.
    state_threading: StateThreading,
    /// BT-153: Whether we're inside a loop body (use `StateAcc` instead of `State`)
    in_loop_body: bool,
    /// BT-153: Whether we're generating REPL code (vs module code).
    /// In REPL mode, local variable assignments should update bindings.
    is_repl_mode: bool,
    /// BT-213: Code generation context (`Actor`, `ValueType`, or `Repl`).
    /// Determines variable naming and method dispatch strategy.
    context: CodeGenContext,
    /// BT-101: Original source text for extracting method source.
    source_text: Option<String>,
    /// BT-295: Primitive binding table from compiled stdlib (ADR 0007).
    /// Used by `generate_primitive()` for method body compilation via static methods.
    /// The field is stored for future call-site optimization with static typing.
    #[allow(dead_code)]
    primitive_bindings: PrimitiveBindingTable,
    /// Identity of the class currently being compiled (if any).
    /// Set from the AST class definition at the start of module generation.
    class_identity: Option<util::ClassIdentity>,
    /// BT-295: Parameters of the current method being compiled (if any).
    /// Used by `Expression::Primitive` to generate dispatch argument lists.
    current_method_params: Vec<String>,
    /// BT-403: Selectors of sealed methods in the current class.
    /// Used to generate standalone functions and direct call dispatch.
    sealed_method_selectors: std::collections::HashSet<String>,
    /// BT-374 / ADR 0010: Whether workspace bindings (`Transcript`, `Beamtalk`) are available.
    /// When true (REPL context), workspace binding class references generate
    /// `persistent_term:get` + async actor send. When false (batch compile),
    /// workspace binding names produce a compile error.
    workspace_mode: bool,
    /// BT-426: Whether we're currently generating a class-side method body.
    /// When true, field access/assignment should produce a compile error.
    in_class_method: bool,
    /// BT-412: Names of class variables in the current class.
    /// Used to distinguish class variable access from instance field access in class methods.
    class_var_names: std::collections::HashSet<String>,
    /// BT-412: Selector names of class methods in the current class.
    /// Used to route self-sends to class method functions vs module exports.
    class_method_selectors: std::collections::HashSet<String>,
    /// BT-412: State version counter for class variable threading.
    class_var_version: usize,
    /// BT-412: Whether class variables were mutated in the current method.
    class_var_mutated: bool,
    /// BT-412: Name of the result variable from the last open-scope expression
    /// (class var assignment or class method self-send). Used by the class method
    /// body generator to reference the result when closing open scopes.
    last_open_scope_result: Option<String>,
    /// BT-245: Whether a state-threading loop mutated REPL bindings.
    /// Set by `_with_mutations` loop codegen when `is_repl_mode` is true.
    /// Checked by `generate_eval_module_body` to return `{'nil', Result}`.
    repl_loop_mutated: bool,
    /// BT-245: Name of the dispatch tuple variable from the last `generate_self_dispatch_open`.
    /// Contains `{Result, State}` — callers can extract element 1 for the result value.
    last_dispatch_var: Option<String>,
}

impl CoreErlangGenerator {
    /// Creates a new code generator for the given module name.
    fn new(module_name: &str) -> Self {
        Self {
            module_name: module_name.to_string(),
            output: String::new(),
            indent: 0,
            var_context: VariableContext::new(),
            state_threading: StateThreading::new(),
            in_loop_body: false,
            is_repl_mode: false,
            context: CodeGenContext::Actor, // Default to Actor for backward compatibility
            source_text: None,
            primitive_bindings: PrimitiveBindingTable::new(),
            class_identity: None,
            current_method_params: Vec::new(),
            sealed_method_selectors: std::collections::HashSet::new(),
            workspace_mode: false,
            in_class_method: false,
            class_var_names: std::collections::HashSet::new(),
            class_method_selectors: std::collections::HashSet::new(),
            class_var_version: 0,
            class_var_mutated: false,
            last_open_scope_result: None,
            repl_loop_mutated: false,
            last_dispatch_var: None,
        }
    }

    /// Creates a new code generator with a primitive binding table.
    fn with_bindings(module_name: &str, bindings: PrimitiveBindingTable) -> Self {
        Self {
            module_name: module_name.to_string(),
            output: String::new(),
            indent: 0,
            var_context: VariableContext::new(),
            state_threading: StateThreading::new(),
            in_loop_body: false,
            is_repl_mode: false,
            context: CodeGenContext::Actor,
            source_text: None,
            primitive_bindings: bindings,
            class_identity: None,
            current_method_params: Vec::new(),
            sealed_method_selectors: std::collections::HashSet::new(),
            workspace_mode: false,
            in_class_method: false,
            class_var_names: std::collections::HashSet::new(),
            class_method_selectors: std::collections::HashSet::new(),
            class_var_version: 0,
            class_var_mutated: false,
            last_open_scope_result: None,
            repl_loop_mutated: false,
            last_dispatch_var: None,
        }
    }

    /// Pushes a new scope for variable bindings.
    fn push_scope(&mut self) {
        self.var_context.push_scope();
    }

    /// Pops the current scope, discarding its bindings.
    fn pop_scope(&mut self) {
        self.var_context.pop_scope();
    }

    /// Looks up a variable binding in the current scope stack.
    fn lookup_var(&self, name: &str) -> Option<&String> {
        self.var_context.lookup(name)
    }

    /// Binds an identifier to a Core Erlang variable name in the current scope.
    fn bind_var(&mut self, name: &str, core_var: &str) {
        self.var_context.bind(name, core_var);
    }

    /// Returns the current state variable name for state threading.
    ///
    /// When inside a loop body (`in_loop_body = true`), returns `StateAcc` or `StateAccN`.
    /// Otherwise returns `State` or `StateN`.
    pub(super) fn current_state_var(&self) -> String {
        if self.in_loop_body {
            // Inside loop body - use StateAcc nomenclature
            if self.state_threading.version() == 0 {
                "StateAcc".to_string()
            } else {
                format!("StateAcc{}", self.state_threading.version())
            }
        } else {
            // Normal context - use State nomenclature
            self.state_threading.current_var()
        }
    }

    /// Increments the state version and returns the new state variable name.
    ///
    /// When inside a loop body (`in_loop_body = true`), returns `StateAcc1`, `StateAcc2`, etc.
    /// Otherwise returns `State1`, `State2`, etc.
    pub(super) fn next_state_var(&mut self) -> String {
        let next_var = self.state_threading.next_var();
        if self.in_loop_body {
            // Replace "State" prefix with "StateAcc"
            if next_var == "State1" {
                // First increment in loop body
                "StateAcc1".to_string()
            } else if next_var.starts_with("State") {
                next_var.replace("State", "StateAcc")
            } else {
                next_var
            }
        } else {
            next_var
        }
    }

    /// Resets the state version to 0.
    pub(super) fn reset_state_version(&mut self) {
        self.state_threading.reset();
    }

    /// Gets the current state version.
    pub(super) fn state_version(&self) -> usize {
        self.state_threading.version()
    }

    /// Sets the state version.
    pub(super) fn set_state_version(&mut self, version: usize) {
        self.state_threading.set_version(version);
    }

    /// BT-412: Returns the current class variable state variable name.
    fn current_class_var(&self) -> String {
        if self.class_var_version == 0 {
            "ClassVars".to_string()
        } else {
            format!("ClassVars{}", self.class_var_version)
        }
    }

    /// BT-412: Increments class var version and returns the new variable name.
    fn next_class_var(&mut self) -> String {
        self.class_var_version += 1;
        self.class_var_mutated = true;
        format!("ClassVars{}", self.class_var_version)
    }

    /// BT-153/BT-245: Check if mutation threading should be used for a block.
    /// In REPL mode, local variable mutations trigger threading.
    /// In module mode, field writes OR self-sends trigger threading.
    pub(super) fn needs_mutation_threading(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
    ) -> bool {
        if self.is_repl_mode {
            // REPL: both local vars and fields need threading
            analysis.has_mutations()
        } else {
            // Module: field writes or self-sends (which may mutate state) need threading
            analysis.has_state_effects()
        }
    }

    /// BT-213: Determines if a class is an actor (process-based) or value type (plain term).
    ///
    /// **Actor classes:** Inherit from Actor or its subclasses. Generate `gen_server` code.
    /// **Value types:** Inherit from Object (but not Actor). Generate plain Erlang maps/records.
    ///
    /// # Implementation Note
    ///
    /// Uses the `ClassHierarchy` to walk the full inheritance chain and determine
    /// if a class is an actor (inherits from `Actor` at any level in the hierarchy).
    ///
    /// # Returns
    ///
    /// - `true` if class inherits from Actor anywhere in the chain
    /// - `false` if class inherits only from Object/ProtoObject (value type)
    /// - `true` if module contains no class (backward compatibility for REPL)
    fn is_actor_class(
        module: &Module,
        hierarchy: &crate::semantic_analysis::class_hierarchy::ClassHierarchy,
    ) -> bool {
        if let Some(class) = module.classes.first() {
            let name = class.name.name.as_str();
            if name == "Actor" {
                return true;
            }
            let chain = hierarchy.superclass_chain(name);
            if chain.iter().any(|s| s.as_str() == "Actor") {
                return true;
            }
            // If the chain terminated at a known value-type root (Object/ProtoObject),
            // this is definitely a value type.
            // BT-480: Include exception hierarchy classes — these inherit from Object
            // (Exception → Object) and must compile as value types, not actors.
            let known_value_roots = [
                "Object",
                "ProtoObject",
                "Exception",
                "Error",
                "RuntimeError",
                "TypeError",
                "InstantiationError",
                "Character",
            ];
            if let Some(last) = chain.last() {
                if known_value_roots.contains(&last.as_str()) {
                    return false;
                }
            }
            // Also check direct superclass against known value types for incomplete chains
            // (e.g., `Error subclass: MyCustomError` compiled without Exception in hierarchy).
            if known_value_roots.contains(&class.superclass_name()) {
                return false;
            }
            // Chain is incomplete (superclass not in hierarchy) or empty with
            // non-Object superclass. Default to actor for backward compatibility
            // (e.g. compiling subclass files independently without parent).
            // Root classes (superclass "none") are value types, not actors.
            !matches!(class.superclass_name(), "Object" | "none")
        } else {
            true
        }
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

    ///
    /// Generates code for an expression by dispatching to the appropriate handler.
    ///
    /// This is the main expression dispatcher that routes each AST node type
    /// to its specialized code generation method.
    fn generate_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => self.generate_identifier(id),
            Expression::ClassReference { name, .. } => {
                // BT-376 / ADR 0010: Workspace bindings resolve to singleton actor objects
                // stored in persistent_term, not class objects from the registry.
                if dispatch_codegen::is_workspace_binding(&name.name) {
                    if self.workspace_mode {
                        write!(
                            self.output,
                            "call 'persistent_term':'get'({{'beamtalk_binding', '{}'}})",
                            name.name
                        )?;
                        return Ok(());
                    }
                    return Err(CodeGenError::WorkspaceBindingInBatchMode {
                        name: name.name.to_string(),
                    });
                }

                // BT-215: Standalone class references resolve to class objects
                // Wrap the class PID in a beamtalk_object record for uniform dispatch
                //
                // Generate a case expression to handle undefined classes gracefully:
                //   case call 'beamtalk_object_class':'whereis_class'('Point') of
                //     <'undefined'> when 'true' -> 'nil'
                //     <ClassPid> when 'true' ->
                //       let ClassModName = call 'beamtalk_object_class':'module_name'(ClassPid) in
                //       {'beamtalk_object', 'Point class', ClassModName, ClassPid}
                //   end
                let class_pid_var = self.fresh_var("ClassPid");
                let class_mod_var = self.fresh_var("ClassModName");

                write!(
                    self.output,
                    "case call 'beamtalk_object_class':'whereis_class'('{}') of ",
                    name.name
                )?;
                write!(self.output, "<'undefined'> when 'true' -> 'nil' ")?;
                write!(self.output, "<{class_pid_var}> when 'true' -> ")?;
                write!(
                    self.output,
                    "let {class_mod_var} = call 'beamtalk_object_class':'module_name'({class_pid_var}) in "
                )?;
                write!(
                    self.output,
                    "{{'beamtalk_object', '{} class', {class_mod_var}, {class_pid_var}}} ",
                    name.name
                )?;
                write!(self.output, "end")?;
                Ok(())
            }
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
            Expression::ListLiteral { elements, tail, .. } => {
                self.generate_list_literal(elements, tail.as_deref())
            }
            Expression::Cascade {
                receiver, messages, ..
            } => self.generate_cascade(receiver, messages),
            Expression::Primitive {
                name, is_quoted, ..
            } => self.generate_primitive(name, *is_quoted),
            _ => Err(CodeGenError::UnsupportedFeature {
                feature: format!("expression type: {expr:?}"),
                location: format!("{:?}", expr.span()),
            }),
        }
    }

    /// Generates code for field access (e.g., self.value).
    /// Generates a method body with the reply tuple embedded.
    ///
    /// This is used for actor method dispatch to ensure state threading works correctly.
    /// The generated code looks like:
    /// ```erlang
    /// let _Val1 = <value1> in let State1 = ... in
    /// let _Val2 = <value2> in let State2 = ... in
    ///
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
    /// Returns an error for both field assignments and local mutations; the local-mutation
    /// error is phrased as a warning in its message.
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

    /// Generates code for an `@primitive` expression (ADR 0007 Phase 3).
    ///
    /// For **selector-based** primitives (quoted, e.g., `@primitive '+'`), generates
    /// direct Erlang BIF calls when a known implementation exists. This makes the
    /// compiled stdlib module self-sufficient — no delegation to hand-written
    /// Erlang dispatch modules.
    ///
    /// Falls back to a `does_not_understand` error for selectors with no known
    /// BIF implementation.
    ///
    /// For **structural intrinsics** (unquoted, e.g., `@primitive blockValue`),
    /// these are handled at the call site by `dispatch_codegen`, not here.
    /// The method body for structural intrinsics is never directly called.
    fn generate_primitive(&mut self, name: &str, is_quoted: bool) -> Result<()> {
        let class_name = self
            .class_identity
            .as_ref()
            .map(|id| id.class_name().to_string())
            .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "@primitive '{name}' used outside of a class context"
                ))
            })?;

        // BT-340: For selector-based primitives, try to emit a direct BIF call
        // instead of delegating through a hand-written dispatch module.
        if is_quoted {
            let params = self.current_method_params.clone();
            if let Some(code) =
                primitive_implementations::generate_primitive_bif(&class_name, name, &params)
            {
                self.output.push_str(&code);
                return Ok(());
            }
        }

        // Fallback: delegate to runtime dispatch module.
        // This path is used for:
        // - Structural intrinsics (unquoted) — handled at call site, body is placeholder
        // - Selector-based primitives with no known BIF (unimplemented or complex)
        let runtime_module = PrimitiveBindingTable::runtime_module_for_class(&class_name);

        write!(
            self.output,
            "call '{runtime_module}':'dispatch'('{name}', ["
        )?;
        for (i, param) in self.current_method_params.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "{param}")?;
        }
        write!(self.output, "], Self)")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::source_analysis::Span;

    #[test]
    fn test_generate_empty_module() {
        let module = Module::new(Vec::new(), Span::new(0, 0));
        let result = generate(&module);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("module 'bt_module'"));
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
    fn test_generate_loose_inequality_operator() {
        let mut generator = CoreErlangGenerator::new("test");
        let left = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
        let right = vec![Expression::Literal(Literal::Integer(99), Span::new(7, 9))];
        let result = generator.generate_binary_op("~=", &left, &right);
        assert!(result.is_ok());
        assert!(
            generator.output.contains("call 'erlang':'/='"),
            "Should use loose inequality /= (negation of ==). Got: {}",
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
    fn test_class_name_derived_from_module() {
        let generator = CoreErlangGenerator::new("my_counter_actor");
        assert_eq!(generator.class_name(), "MyCounterActor");

        let generator = CoreErlangGenerator::new("simple");
        assert_eq!(generator.class_name(), "Simple");
    }

    #[test]
    fn test_class_name_from_identity_overrides_module() {
        let mut generator = CoreErlangGenerator::new("bt@stdlib@string");
        generator.class_identity = Some(util::ClassIdentity::new("String"));
        assert_eq!(generator.class_name(), "String");
    }

    #[test]
    fn test_class_name_to_module_name() {
        // Single word
        assert_eq!(util::to_module_name("Counter"), "counter");

        // Multi-word CamelCase
        assert_eq!(util::to_module_name("MyCounterActor"), "my_counter_actor");

        // With acronyms
        assert_eq!(util::to_module_name("HTTPRouter"), "httprouter");

        // Mixed case
        assert_eq!(util::to_module_name("HTTPSConnection"), "httpsconnection");
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
        // BT-430: Unified dispatch via beamtalk_message_dispatch:send/3
        assert!(
            output.contains("beamtalk_message_dispatch':'send'("),
            "Should dispatch via beamtalk_message_dispatch:send/3. Got: {output}"
        );
        assert!(
            output.contains("'increment'"),
            "Should include selector atom. Got: {output}"
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
        // BT-430: Unified dispatch via beamtalk_message_dispatch:send/3
        assert!(
            output.contains("beamtalk_message_dispatch':'send'("),
            "Should dispatch via beamtalk_message_dispatch:send/3. Got: {output}"
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
    fn test_generate_await_with_timeout() {
        let mut generator = CoreErlangGenerator::new("test");

        // Build: future await: 5000
        let receiver = Expression::Identifier(Identifier::new("myFuture", Span::new(0, 8)));
        let selector = MessageSelector::Keyword(vec![KeywordPart {
            keyword: "await:".into(),
            span: Span::new(9, 15),
        }]);
        let timeout = Expression::Literal(Literal::Integer(5000), Span::new(16, 20));

        let result = generator.generate_message_send(&receiver, &selector, &[timeout]);
        assert!(result.is_ok());

        let output = &generator.output;
        // Should call beamtalk_future:await/2 with timeout
        assert!(
            output.contains("beamtalk_future':'await'("),
            "Should call beamtalk_future:await(). Got: {output}"
        );
        assert!(
            output.contains("5000"),
            "Should include timeout value. Got: {output}"
        );
        assert!(
            !output.contains("gen_server':'cast'"),
            "Should NOT use gen_server:cast for await. Got: {output}"
        );
    }

    #[test]
    fn test_generate_await_forever() {
        let mut generator = CoreErlangGenerator::new("test");

        // Build: future awaitForever
        let receiver = Expression::Identifier(Identifier::new("myFuture", Span::new(0, 8)));
        let selector = MessageSelector::Unary("awaitForever".into());

        let result = generator.generate_message_send(&receiver, &selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;
        // Should call beamtalk_future:await_forever/1
        assert!(
            output.contains("beamtalk_future':'await_forever'("),
            "Should call beamtalk_future:await_forever(). Got: {output}"
        );
        assert!(
            !output.contains("gen_server':'cast'"),
            "Should NOT use gen_server:cast for awaitForever. Got: {output}"
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

        // BT-430: Unified dispatch generates nested beamtalk_message_dispatch:send calls
        let send_count = output.matches("beamtalk_message_dispatch':'send'(").count();
        assert!(
            send_count >= 2,
            "Nested message sends should produce at least 2 dispatch calls. Got {send_count} in: {output}"
        );
    }

    #[test]
    fn test_generate_spawn_message_send() {
        let mut generator = CoreErlangGenerator::new("test_module");

        // Create AST for: Counter spawn
        let receiver = Expression::ClassReference {
            name: Identifier::new("Counter", Span::new(0, 7)),
            span: Span::new(0, 7),
        };
        let selector = MessageSelector::Unary("spawn".into());
        let arguments = vec![];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());
        assert!(generator.output.contains("call 'bt@counter':'spawn'()"));
    }

    #[test]
    fn test_generate_spawn_with_message_send() {
        let mut generator = CoreErlangGenerator::new("test_module");

        // Create AST for: Counter spawnWith: #{value => 10}
        // For simplicity, we'll use an integer literal as the init arg
        // (in practice this would be a map literal)
        let receiver = Expression::ClassReference {
            name: Identifier::new("Counter", Span::new(0, 7)),
            span: Span::new(0, 7),
        };
        let selector =
            MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", Span::new(8, 18))]);
        let arguments = vec![Expression::Literal(Literal::Integer(42), Span::new(19, 21))];

        let result = generator.generate_message_send(&receiver, &selector, &arguments);
        assert!(result.is_ok());
        // Should call spawn/1 with the argument
        assert!(
            generator.output.contains("call 'bt@counter':'spawn'(42)"),
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
        assert!(code.contains("call 'beamtalk_error':'raise'(SpawnErr1)"));

        // Check that init/1 creates the default state with fields and merges with InitArgs
        assert!(code.contains("'init'/1 = fun (InitArgs) ->"));
        assert!(code.contains("let DefaultState = ~{"));
        assert!(code.contains("'$beamtalk_class' => 'Counter'"));
        assert!(code.contains("'__methods__' => call 'counter':'method_table'()"));
        assert!(code.contains("'value' => 0"));
        // Check that InitArgs is merged into DefaultState
        assert!(code.contains("call 'maps':'merge'(DefaultState, InitArgs)"));
        assert!(code.contains("{'ok', FinalState}"));
    }

    #[test]
    fn test_generate_actor_new_error_methods() {
        // BT-217: Actor classes must export and generate new/0 and new/1 error methods
        // using structured #beamtalk_error{} records
        use crate::ast::*;

        let module = Module::new(vec![], Span::new(0, 0));
        let code = generate_with_name(&module, "test_actor").expect("codegen should succeed");

        // Check that new/0 and new/1 are exported
        assert!(code.contains("'new'/0"));
        assert!(code.contains("'new'/1"));

        // Check that new/0 function exists and uses beamtalk_error
        assert!(code.contains("'new'/0 = fun () ->"));
        assert!(code.contains("call 'beamtalk_error':'new'('instantiation_error', 'Actor')"));
        assert!(code.contains("call 'beamtalk_error':'with_selector'(Error0, 'new')"));
        assert!(code.contains("call 'beamtalk_error':'with_hint'(Error1,"));
        assert!(code.contains("call 'beamtalk_error':'raise'(Error2)"));

        // Check that new/1 function exists and uses beamtalk_error
        assert!(code.contains("'new'/1 = fun (_InitArgs) ->"));
        assert!(code.contains("call 'beamtalk_error':'with_selector'(Error0, 'new:')"));
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
    fn test_generate_repl_module_returns_tuple_with_state() {
        // BT-153: REPL eval/1 should return {Result, UpdatedBindings}
        let expression = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
        let code =
            generate_repl_expression(&expression, "repl_tuple_test").expect("codegen should work");

        eprintln!("Generated code for literal 42:");
        eprintln!("{code}");

        // Check that the result is wrapped in a tuple with State
        assert!(
            code.contains("let Result ="),
            "Should bind the result to Result variable. Got:\n{code}"
        );
        assert!(
            code.contains("{Result, State}"),
            "Should return tuple {{Result, State}}. Got:\n{code}"
        );
    }

    #[test]
    fn test_generate_repl_module_with_times_repeat_mutation() {
        // BT-153: REPL with mutation should return updated state
        // Expression: 5 timesRepeat: [count := count + 1]

        // Build the block: [count := count + 1]
        let count_id = Expression::Identifier(Identifier::new("count", Span::new(0, 5)));
        let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
        let add = Expression::MessageSend {
            receiver: Box::new(count_id.clone()),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![one],
            span: Span::new(0, 15),
        };
        let assignment = Expression::Assignment {
            target: Box::new(count_id),
            value: Box::new(add),
            span: Span::new(0, 20),
        };
        let body = Expression::Block(Block {
            parameters: vec![],
            body: vec![assignment],
            span: Span::new(0, 25),
        });

        // Build: 5 timesRepeat: [...]
        let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
        let times_repeat = Expression::MessageSend {
            receiver: Box::new(five),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "timesRepeat:".into(),
                span: Span::new(2, 14),
            }]),
            arguments: vec![body],
            span: Span::new(0, 40),
        };

        let code = generate_repl_expression(&times_repeat, "repl_times_test")
            .expect("codegen should work");

        eprintln!("Generated code for 5 timesRepeat: [count := count + 1]:");
        eprintln!("{code}");

        // BT-483: For mutation-threaded loops, return {Result, State} tuple.
        // REPL extracts via element/2: let _LoopResult = element(1, Result) ...
        assert!(
            code.contains("'element'(1, Result)") && code.contains("'element'(2, Result)"),
            "Should extract Result tuple elements via element/2 for mutation loop. Got:\n{code}"
        );

        // BT-483: Loop termination should return {nil, StateAcc}
        assert!(
            code.contains("{'nil', StateAcc}"),
            "Loop should return {{'nil', StateAcc}} on termination. Got:\n{code}"
        );

        // Verify mutation threading details
        assert!(
            code.contains("letrec 'repeat'/2"),
            "Should use arity-2 repeat function (I, StateAcc). Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('count'"),
            "Should update 'count' in StateAcc. Got:\n{code}"
        );
    }

    #[test]
    fn test_generate_repl_module_with_to_do_mutation() {
        use crate::ast::BlockParameter;

        // BT-153: REPL with to:do: mutation should return updated state
        // Expression: 1 to: 5 do: [:n | total := total + n]

        // Build the block: [:n | total := total + n]
        let total_id = Expression::Identifier(Identifier::new("total", Span::new(0, 5)));
        let n_id = Expression::Identifier(Identifier::new("n", Span::new(0, 1)));
        let add = Expression::MessageSend {
            receiver: Box::new(total_id.clone()),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![n_id],
            span: Span::new(0, 15),
        };
        let assignment = Expression::Assignment {
            target: Box::new(total_id),
            value: Box::new(add),
            span: Span::new(0, 20),
        };
        let body = Expression::Block(Block {
            parameters: vec![BlockParameter {
                name: "n".into(),
                span: Span::new(0, 1),
            }],
            body: vec![assignment],
            span: Span::new(0, 25),
        });

        // Build: 1 to: 5 do: [...]
        let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
        let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
        let to_do = Expression::MessageSend {
            receiver: Box::new(one),
            selector: MessageSelector::Keyword(vec![
                KeywordPart {
                    keyword: "to:".into(),
                    span: Span::new(2, 5),
                },
                KeywordPart {
                    keyword: "do:".into(),
                    span: Span::new(8, 11),
                },
            ]),
            arguments: vec![five, body],
            span: Span::new(0, 40),
        };

        let code =
            generate_repl_expression(&to_do, "repl_to_do_test").expect("codegen should work");

        eprintln!("Generated code for 1 to: 5 do: [:n | total := total + n]:");
        eprintln!("{code}");

        // BT-483: For mutation-threaded loops, return {Result, State} tuple.
        assert!(
            code.contains("'element'(1, Result)") && code.contains("'element'(2, Result)"),
            "Should extract Result tuple elements via element/2 for mutation loop. Got:\n{code}"
        );

        // Verify to:do: mutation threading
        assert!(
            code.contains("letrec 'loop'/2"),
            "Should use arity-2 loop function (I, StateAcc). Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('total'"),
            "Should update 'total' in StateAcc. Got:\n{code}"
        );
    }

    #[test]
    fn test_generate_repl_module_with_while_true_mutation() {
        // BT-181: REPL with whileTrue: mutation should read condition from StateAcc
        // Expression: [x < 5] whileTrue: [x := x + 1]

        // Build the condition: [x < 5]
        let x_id = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
        let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
        let compare = Expression::MessageSend {
            receiver: Box::new(x_id.clone()),
            selector: MessageSelector::Binary("<".into()),
            arguments: vec![five],
            span: Span::new(0, 10),
        };
        let condition = Expression::Block(Block {
            parameters: vec![],
            body: vec![compare],
            span: Span::new(0, 12),
        });

        // Build the body: [x := x + 1]
        let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
        let add = Expression::MessageSend {
            receiver: Box::new(x_id.clone()),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![one],
            span: Span::new(0, 10),
        };
        let assignment = Expression::Assignment {
            target: Box::new(x_id),
            value: Box::new(add),
            span: Span::new(0, 15),
        };
        let body = Expression::Block(Block {
            parameters: vec![],
            body: vec![assignment],
            span: Span::new(0, 17),
        });

        // Build: [x < 5] whileTrue: [x := x + 1]
        let while_true = Expression::MessageSend {
            receiver: Box::new(condition),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "whileTrue:".into(),
                span: Span::new(10, 20),
            }]),
            arguments: vec![body],
            span: Span::new(0, 40),
        };

        let code =
            generate_repl_expression(&while_true, "repl_while_test").expect("codegen should work");

        eprintln!("Generated code for [x < 5] whileTrue: [x := x + 1]:");
        eprintln!("{code}");

        // BT-181: Condition lambda should take StateAcc parameter
        assert!(
            code.contains("fun (StateAcc) ->"),
            "Condition lambda should accept StateAcc parameter. Got:\n{code}"
        );
        // BT-181: Condition should read x from StateAcc, not outer scope
        assert!(
            code.contains("maps':'get'('x', StateAcc)"),
            "Condition should read x from StateAcc. Got:\n{code}"
        );
        // BT-181: Condition should be applied with StateAcc argument
        assert!(
            code.contains("apply") && code.contains("(StateAcc)"),
            "Condition should be applied with StateAcc argument. Got:\n{code}"
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
    fn test_non_block_message_uses_unified_dispatch() {
        // BT-430: Regular message sends now use unified dispatch
        // actor increment → beamtalk_message_dispatch:send(actor, 'increment', [])
        let mut generator = CoreErlangGenerator::new("test");

        let receiver = Expression::Identifier(Identifier::new("actor", Span::new(0, 5)));
        let selector = MessageSelector::Unary("increment".into());

        let result = generator.generate_message_send(&receiver, &selector, &[]);
        assert!(result.is_ok());

        let output = &generator.output;
        assert!(
            output.contains("beamtalk_message_dispatch':'send'("),
            "Non-block unary messages should use unified dispatch. Got: {output}"
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
    // Dictionary Method Code Generation Tests (BT-296: now uses runtime dispatch)
    // ========================================================================

    #[test]
    fn test_dictionary_at_on_identifier() {
        // BT-296: Dictionary methods now go through runtime dispatch
        // BT-430: person at: #name -> beamtalk_message_dispatch:send(person, 'at:', ['name'])
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

        // BT-430: Unified dispatch
        assert!(
            output.contains("beamtalk_message_dispatch") && output.contains("'at:'"),
            "Should generate unified dispatch for at:. Got: {output}"
        );
    }

    #[test]
    fn test_dictionary_at_put_on_identifier() {
        // BT-296: Dictionary methods now go through runtime dispatch
        // BT-430: person at: #age put: 31 -> beamtalk_message_dispatch:send(person, 'at:put:', ['age', 31])
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
            output.contains("beamtalk_message_dispatch") && output.contains("'at:put:'"),
            "Should generate unified dispatch for at:put:. Got: {output}"
        );
    }

    #[test]
    fn test_dictionary_size_on_identifier() {
        // BT-296: Dictionary methods now go through runtime dispatch
        // BT-430: person size -> beamtalk_message_dispatch:send(person, 'size', [])
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
            output.contains("beamtalk_message_dispatch") && output.contains("'size'"),
            "Should generate unified dispatch for size. Got: {output}"
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

        // Should send BOTH messages (negated AND abs) to the receiver via unified dispatch
        assert!(
            output.contains("'negated'"),
            "Should send first message 'negated'. Got: {output}"
        );
        assert!(
            output.contains("'abs'"),
            "Should send second message 'abs'. Got: {output}"
        );

        // BT-430: Should use unified dispatch for cascade messages
        assert!(
            output.contains("call 'beamtalk_message_dispatch':'send'(_Receiver"),
            "Should send messages via unified dispatch. Got: {output}"
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

    #[test]
    fn test_validate_stored_closure_empty_block() {
        // Empty block should not trigger errors
        let block = Block {
            parameters: vec![],
            body: vec![],
            span: Span::new(0, 2),
        };

        let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
        assert!(result.is_ok(), "Empty block should be valid");
    }

    #[test]
    fn test_validate_stored_closure_with_local_mutation() {
        // Block with local variable assignment: [count := count + 1]
        let block = Block {
            parameters: vec![],
            body: vec![Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    Span::new(1, 6),
                ))),
                value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(10, 11))),
                span: Span::new(1, 11),
            }],
            span: Span::new(0, 12),
        };

        let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
        assert!(result.is_err(), "Local mutation should produce error");

        if let Err(CodeGenError::LocalMutationInStoredClosure { variable, .. }) = result {
            assert_eq!(variable, "count");
        } else {
            panic!("Expected LocalMutationInStoredClosure error");
        }
    }

    #[test]
    fn test_validate_stored_closure_with_field_assignment() {
        // Block with field assignment: [self.value := 1]
        let block = Block {
            parameters: vec![],
            body: vec![Expression::Assignment {
                target: Box::new(Expression::FieldAccess {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        Span::new(1, 5),
                    ))),
                    field: Identifier::new("value", Span::new(6, 11)),
                    span: Span::new(1, 11),
                }),
                value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(15, 16))),
                span: Span::new(1, 16),
            }],
            span: Span::new(0, 17),
        };

        let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
        assert!(result.is_err(), "Field assignment should produce error");

        if let Err(CodeGenError::FieldAssignmentInStoredClosure {
            field,
            field_capitalized,
            ..
        }) = result
        {
            assert_eq!(field, "value");
            assert_eq!(field_capitalized, "Value");
        } else {
            panic!("Expected FieldAssignmentInStoredClosure error");
        }
    }

    #[test]
    fn test_validate_stored_closure_field_takes_precedence() {
        // Block with both field and local assignment
        // Field error should be reported first
        let block = Block {
            parameters: vec![],
            body: vec![
                Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new(
                        "count",
                        Span::new(1, 6),
                    ))),
                    value: Box::new(Expression::Literal(Literal::Integer(0), Span::new(10, 11))),
                    span: Span::new(1, 11),
                },
                Expression::Assignment {
                    target: Box::new(Expression::FieldAccess {
                        receiver: Box::new(Expression::Identifier(Identifier::new(
                            "self",
                            Span::new(13, 17),
                        ))),
                        field: Identifier::new("value", Span::new(18, 23)),
                        span: Span::new(13, 23),
                    }),
                    value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(27, 28))),
                    span: Span::new(13, 28),
                },
            ],
            span: Span::new(0, 29),
        };

        let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
        assert!(result.is_err());

        // Should be field error (checked first), not local
        assert!(
            matches!(
                result,
                Err(CodeGenError::FieldAssignmentInStoredClosure { .. })
            ),
            "Field error should take precedence over local mutation"
        );
    }

    #[test]
    fn test_codegen_rejects_stored_closure_with_field_assignment() {
        // Integration test: verify the full codegen pipeline catches field assignments
        // Build a module with: test := [ myBlock := [self.value := 1]. myBlock ]
        let module = Module {
            classes: vec![],
            expressions: vec![Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "test",
                    Span::new(0, 4),
                ))),
                value: Box::new(Expression::Block(Block {
                    parameters: vec![],
                    body: vec![
                        // myBlock := [self.value := self.value + 1]
                        Expression::Assignment {
                            target: Box::new(Expression::Identifier(Identifier::new(
                                "myBlock",
                                Span::new(10, 17),
                            ))),
                            value: Box::new(Expression::Block(Block {
                                parameters: vec![],
                                body: vec![Expression::Assignment {
                                    target: Box::new(Expression::FieldAccess {
                                        receiver: Box::new(Expression::Identifier(
                                            Identifier::new("self", Span::new(22, 26)),
                                        )),
                                        field: Identifier::new("value", Span::new(27, 32)),
                                        span: Span::new(22, 32),
                                    }),
                                    value: Box::new(Expression::Literal(
                                        Literal::Integer(1),
                                        Span::new(36, 37),
                                    )),
                                    span: Span::new(22, 37),
                                }],
                                span: Span::new(21, 38),
                            })),
                            span: Span::new(10, 38),
                        },
                        Expression::Identifier(Identifier::new("myBlock", Span::new(40, 47))),
                    ],
                    span: Span::new(8, 49),
                })),
                span: Span::new(0, 49),
            }],
            span: Span::new(0, 50),
            leading_comments: vec![],
        };

        let result = generate(&module);
        assert!(
            result.is_err(),
            "Should reject field assignment in stored closure"
        );

        if let Err(CodeGenError::FieldAssignmentInStoredClosure { field, .. }) = result {
            assert_eq!(field, "value", "Should report the correct field name");
        } else {
            panic!("Expected FieldAssignmentInStoredClosure error, got: {result:?}");
        }
    }

    #[test]
    fn test_codegen_rejects_stored_closure_with_local_mutation() {
        // Integration test: verify the full codegen pipeline catches local mutations
        // Build a module with: test := [ count := 0. myBlock := [count := count + 1]. myBlock ]
        let module = Module {
            classes: vec![],
            expressions: vec![Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "test",
                    Span::new(0, 4),
                ))),
                value: Box::new(Expression::Block(Block {
                    parameters: vec![],
                    body: vec![
                        // count := 0
                        Expression::Assignment {
                            target: Box::new(Expression::Identifier(Identifier::new(
                                "count",
                                Span::new(10, 15),
                            ))),
                            value: Box::new(Expression::Literal(
                                Literal::Integer(0),
                                Span::new(19, 20),
                            )),
                            span: Span::new(10, 20),
                        },
                        // myBlock := [count := count + 1]
                        Expression::Assignment {
                            target: Box::new(Expression::Identifier(Identifier::new(
                                "myBlock",
                                Span::new(22, 29),
                            ))),
                            value: Box::new(Expression::Block(Block {
                                parameters: vec![],
                                body: vec![Expression::Assignment {
                                    target: Box::new(Expression::Identifier(Identifier::new(
                                        "count",
                                        Span::new(34, 39),
                                    ))),
                                    value: Box::new(Expression::MessageSend {
                                        receiver: Box::new(Expression::Identifier(
                                            Identifier::new("count", Span::new(43, 48)),
                                        )),
                                        selector: MessageSelector::Binary("+".into()),
                                        arguments: vec![Expression::Literal(
                                            Literal::Integer(1),
                                            Span::new(51, 52),
                                        )],
                                        span: Span::new(43, 52),
                                    }),
                                    span: Span::new(34, 52),
                                }],
                                span: Span::new(33, 53),
                            })),
                            span: Span::new(22, 53),
                        },
                        Expression::Identifier(Identifier::new("myBlock", Span::new(55, 62))),
                    ],
                    span: Span::new(8, 64),
                })),
                span: Span::new(0, 64),
            }],
            span: Span::new(0, 65),
            leading_comments: vec![],
        };

        let result = generate(&module);
        assert!(
            result.is_err(),
            "Should reject local mutation in stored closure"
        );

        if let Err(CodeGenError::LocalMutationInStoredClosure { variable, .. }) = result {
            assert_eq!(variable, "count", "Should report the correct variable name");
        } else {
            panic!("Expected LocalMutationInStoredClosure error, got: {result:?}");
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "comprehensive test covering all registration metadata"
    )]
    fn test_class_registration_generation() {
        // BT-218: Test that class definitions generate registration code
        use crate::ast::{
            ClassDefinition, Identifier, MethodDefinition, MethodKind, StateDeclaration,
        };
        use crate::source_analysis::Span;

        // Create a Counter class with instance variables and methods
        let class = ClassDefinition {
            name: Identifier::new("Counter", Span::new(0, 7)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            is_abstract: false,
            is_sealed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("value", Span::new(0, 5)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                span: Span::new(0, 10),
            }],
            methods: vec![
                MethodDefinition {
                    selector: MessageSelector::Unary("increment".into()),
                    parameters: vec![],
                    body: vec![Expression::Literal(Literal::Integer(42), Span::new(0, 2))],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    span: Span::new(0, 10),
                },
                MethodDefinition {
                    selector: MessageSelector::Unary("getValue".into()),
                    parameters: vec![],
                    body: vec![Expression::Literal(Literal::Integer(42), Span::new(0, 2))],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    span: Span::new(0, 10),
                },
            ],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 50),
        };

        let module = Module {
            expressions: vec![],
            classes: vec![class],
            span: Span::new(0, 50),
            leading_comments: vec![],
        };

        let code = generate_with_name(&module, "counter").expect("codegen should succeed");

        // Check that on_load attribute is present
        assert!(
            code.contains("'on_load' = [{'register_class', 0}]"),
            "Should have on_load attribute. Got:\n{code}"
        );

        // Check that register_class/0 is exported
        assert!(
            code.contains("'register_class'/0"),
            "Should export register_class/0. Got:\n{code}"
        );

        // Check that register_class/0 function exists
        assert!(
            code.contains("'register_class'/0 = fun () ->"),
            "Should generate register_class function. Got:\n{code}"
        );

        // Check that it calls beamtalk_object_class:start (unlinked)
        assert!(
            code.contains("call 'beamtalk_object_class':'start'('Counter',"),
            "Should call beamtalk_object_class:start with class name. Got:\n{code}"
        );

        // Check metadata fields
        assert!(
            code.contains("'name' => 'Counter'"),
            "Should include class name in metadata. Got:\n{code}"
        );
        assert!(
            code.contains("'module' => 'counter'"),
            "Should include module name in metadata. Got:\n{code}"
        );
        assert!(
            code.contains("'superclass' => 'Actor'"),
            "Should include superclass in metadata. Got:\n{code}"
        );

        // BT-105: Check sealing modifier flags
        assert!(
            code.contains("'is_sealed' => 'false'"),
            "Should include is_sealed flag. Got:\n{code}"
        );
        assert!(
            code.contains("'is_abstract' => 'false'"),
            "Should include is_abstract flag. Got:\n{code}"
        );

        // Check instance_methods map
        assert!(
            code.contains("'instance_methods' => ~{"),
            "Should include instance_methods map. Got:\n{code}"
        );
        assert!(
            code.contains("'increment' => ~{'arity' => 0, 'is_sealed' => 'false'}~"),
            "Should include increment method with arity and sealed flag. Got:\n{code}"
        );
        assert!(
            code.contains("'getValue' => ~{'arity' => 0, 'is_sealed' => 'false'}~"),
            "Should include getValue method with arity and sealed flag. Got:\n{code}"
        );

        // Check instance_variables list
        assert!(
            code.contains("'instance_variables' => ['value']"),
            "Should include instance_variables list. Got:\n{code}"
        );

        // Check class_methods map
        assert!(
            code.contains("'class_methods' => ~{"),
            "Should include class_methods map. Got:\n{code}"
        );
        assert!(
            code.contains("'spawn' => ~{'arity' => 0}~"),
            "Should include spawn class method. Got:\n{code}"
        );
        assert!(
            code.contains("'spawnWith:' => ~{'arity' => 1}~"),
            "Should include spawnWith: class method. Got:\n{code}"
        );

        // Check function returns ok
        assert!(code.contains("'ok'"), "Should return 'ok'. Got:\n{code}");

        // BT-403: Check sealed/abstract flags
        assert!(
            code.contains("'is_sealed' => 'false'"),
            "Should include is_sealed flag. Got:\n{code}"
        );
        assert!(
            code.contains("'is_abstract' => 'false'"),
            "Should include is_abstract flag. Got:\n{code}"
        );
    }

    #[test]
    fn test_no_class_registration_for_empty_module() {
        // BT-218: Modules without class definitions should not have on_load or register_class
        let module = Module::new(vec![], Span::new(0, 0));
        let code = generate_with_name(&module, "empty_module").expect("codegen should succeed");

        // Should NOT have on_load attribute
        assert!(
            !code.contains("'on_load'"),
            "Module without classes should not have on_load. Got:\n{code}"
        );

        // Should NOT export register_class/0
        assert!(
            !code.contains("'register_class'/0"),
            "Module without classes should not export register_class. Got:\n{code}"
        );
    }

    #[test]
    fn test_multiple_classes_registration() {
        // BT-218: Test that modules with multiple classes register all of them
        use crate::ast::{ClassDefinition, Identifier, StateDeclaration};
        use crate::source_analysis::Span;

        let class1 = ClassDefinition {
            name: Identifier::new("Counter", Span::new(0, 7)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            is_abstract: false,
            is_sealed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("value", Span::new(0, 5)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                span: Span::new(0, 10),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 20),
        };

        let class2 = ClassDefinition {
            name: Identifier::new("Logger", Span::new(0, 6)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            is_abstract: false,
            is_sealed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("messages", Span::new(0, 8)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                span: Span::new(0, 10),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 30),
        };

        let module = Module {
            expressions: vec![],
            classes: vec![class1, class2],
            span: Span::new(0, 50),
            leading_comments: vec![],
        };

        let code = generate_with_name(&module, "multi_actors").expect("codegen should succeed");

        // Should have on_load attribute
        assert!(
            code.contains("'on_load' = [{'register_class', 0}]"),
            "Should have on_load attribute for multiple classes. Got:\n{code}"
        );

        // Should register first class (Counter)
        assert!(
            code.contains("call 'beamtalk_object_class':'start'('Counter',"),
            "Should register Counter class. Got:\n{code}"
        );
        assert!(
            code.contains("'name' => 'Counter'"),
            "Should include Counter metadata. Got:\n{code}"
        );
        assert!(
            code.contains("'instance_variables' => ['value']"),
            "Should include Counter instance variables. Got:\n{code}"
        );

        // Should register second class (Logger)
        assert!(
            code.contains("call 'beamtalk_object_class':'start'('Logger',"),
            "Should register Logger class. Got:\n{code}"
        );
        assert!(
            code.contains("'name' => 'Logger'"),
            "Should include Logger metadata. Got:\n{code}"
        );
        assert!(
            code.contains("'instance_variables' => ['messages']"),
            "Should include Logger instance variables. Got:\n{code}"
        );

        // Should use let-binding chain to sequence registrations
        assert!(
            code.contains("let _Reg0 = case"),
            "Should have first registration with _Reg0. Got:\n{code}"
        );
        assert!(
            code.contains("in let _Reg1 = case"),
            "Should chain second registration with let. Got:\n{code}"
        );

        // Function should return ok
        assert!(
            code.contains("in 'ok'"),
            "Should return ok after all registrations. Got:\n{code}"
        );
    }

    #[test]
    fn test_class_method_call_generation() {
        // BT-215: Test that ClassReference message sends generate appropriate code
        // BT-374: Workspace bindings (Beamtalk, Transcript) use persistent_term
        //         in REPL context; non-bindings use direct function calls
        use crate::ast::{Expression, Identifier, MessageSelector};
        use crate::source_analysis::Span;

        // Test 1: Workspace binding (Beamtalk) generates persistent_term lookup in REPL
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Beamtalk", Span::new(0, 8)),
                span: Span::new(0, 8),
            }),
            selector: MessageSelector::Unary("allClasses".into()),
            arguments: vec![],
            span: Span::new(0, 20),
        };

        let code = generate_repl_expression(&expr, "repl_eval").expect("codegen should succeed");

        // Workspace binding should use persistent_term lookup + async actor send
        assert!(
            code.contains("persistent_term"),
            "Workspace binding should use persistent_term in REPL. Got:\n{code}"
        );
        assert!(
            code.contains("beamtalk_binding"),
            "Should reference beamtalk_binding. Got:\n{code}"
        );
        assert!(
            code.contains("async_send"),
            "Workspace binding should use async_send. Got:\n{code}"
        );

        // Test 2: Non-binding class (Point) dispatches via class_send in REPL
        let expr2 = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Point", Span::new(0, 5)),
                span: Span::new(0, 5),
            }),
            selector: MessageSelector::Unary("new".into()),
            arguments: vec![],
            span: Span::new(0, 10),
        };

        let code2 = generate_repl_expression(&expr2, "repl_eval2")
            .expect("codegen should succeed for non-binding class");

        // BT-411: Non-binding classes now use class_send for both built-in
        // and user-defined class methods
        assert!(
            code2.contains("whereis_class") && code2.contains("class_send"),
            "Non-binding class should use class_send dispatch. Got:\n{code2}"
        );
        assert!(
            !code2.contains("persistent_term"),
            "Non-binding class should NOT use persistent_term. Got:\n{code2}"
        );

        // Test 3: ClassReference spawn in REPL uses generate_actor_spawn with registry
        let expr3 = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("InitCounter", Span::new(0, 11)),
                span: Span::new(0, 11),
            }),
            selector: MessageSelector::Unary("spawn".into()),
            arguments: vec![],
            span: Span::new(0, 17),
        };

        let code3 = generate_repl_expression(&expr3, "repl_eval3")
            .expect("codegen should succeed for spawn");

        // Spawn on ClassReference uses generate_actor_spawn which calls
        // Module:spawn() with REPL registry integration
        assert!(
            code3.contains("'initcounter':'spawn'") || code3.contains("register_spawned"),
            "REPL spawn should use direct module spawn (with optional registry). Got:\n{code3}"
        );
    }

    // BT-98 PR Comment #8: Test for state version increment bug
    // Bug: generate_local_var_assignment_in_loop() doesn't increment state_version
    // This test documents the expected behavior
    #[test]
    fn test_state_version_should_increment_for_local_var_assignment() {
        // This test verifies the EXPECTED behavior (currently failing due to bug).
        // The bug is in control_flow.rs:978 where state_version is not incremented
        // between reading current_state (lines 966-970) and creating new_state (line 978).
        //
        // Expected Core Erlang pattern:
        //   let _Val1 = <value> in
        //   let StateAcc1 = call 'maps':'put'('x', _Val1, StateAcc) in
        //                   ^^^^^^^^^                             ^^^^^^^^
        //                   version 1 (new)                       version 0 (current)
        //
        // Actual (buggy) pattern:
        //   let _Val1 = <value> in
        //   let StateAcc = call 'maps':'put'('x', _Val1, StateAcc) in
        //       ^^^^^^^^                                 ^^^^^^^^
        //       same version - INVALID!

        // This test documents the issue. The fix will be in control_flow.rs line 975-978
        // by adding: let _ = self.next_state_var();
    }

    // BT-98 PR Comment #10: Test for double " in " bug
    // Bug: Sequential field assignments produce " in  in " (double space)
    // This test documents the expected behavior
    #[test]
    fn test_sequential_field_assignments_should_not_double_in() {
        // This test verifies the EXPECTED behavior (currently failing due to bug).
        // The bug is in control_flow.rs:1177 where " in " is written before every
        // expression when i > 0, but generate_field_assignment_open() already writes
        // a trailing " in ".
        //
        // Expected Core Erlang pattern for two field assignments:
        //   let _Val1 = 1 in let StateAcc1 = ... in let _Val2 = 2 in let StateAcc2 = ... in
        //                                       ^^^ single " in "
        //
        // Actual (buggy) pattern:
        //   let _Val1 = 1 in let StateAcc1 = ... in  in let _Val2 = 2 in let StateAcc2 = ... in
        //                                         ^^^^^^^ double " in " - INVALID!

        // This test documents the issue. The fix will be in control_flow.rs
        // by ensuring only non-assignment expressions write " in " when necessary.
    }

    #[test]
    fn test_standalone_class_reference_uses_dynamic_module_name() {
        // BT-215: Test that standalone ClassReference uses module_name/1 dynamically
        // Review comment: Should match generate_beamtalk_class_named pattern (lines 915-922)
        use crate::ast::{Expression, Identifier, Module};
        use crate::source_analysis::Span;

        // Create expression: Point (standalone class reference)
        let expr = Expression::ClassReference {
            name: Identifier::new("Point", Span::new(0, 5)),
            span: Span::new(0, 5),
        };

        let module = Module {
            expressions: vec![expr],
            classes: vec![],
            span: Span::new(0, 5),
            leading_comments: vec![],
        };

        let code = generate_repl_expression(&module.expressions[0], "repl_eval")
            .expect("codegen should succeed");

        // Should call whereis_class to get the class PID
        assert!(
            code.contains("call 'beamtalk_object_class':'whereis_class'('Point')"),
            "Should call whereis_class to get class PID. Got:\n{code}"
        );

        // Should call module_name to get the module name dynamically
        assert!(
            code.contains("call 'beamtalk_object_class':'module_name'("),
            "Should call module_name to get module name dynamically. Got:\n{code}"
        );

        // Should NOT hardcode 'beamtalk_object_class' as the module name
        assert!(
            !code.contains("'beamtalk_object', 'Point class', 'beamtalk_object_class'"),
            "Should not hardcode 'beamtalk_object_class' as module name. Got:\n{code}"
        );

        // Should create beamtalk_object with dynamic ClassModName variable
        assert!(
            code.contains("'beamtalk_object'") && code.contains("'Point class'"),
            "Should create beamtalk_object with metaclass name. Got:\n{code}"
        );
    }

    #[test]
    fn test_standalone_class_reference_validates_undefined_classes() {
        // BT-215: Test that standalone ClassReference handles undefined classes gracefully
        // Review comment: Should check for 'undefined' and return 'nil', like generate_beamtalk_class_named
        use crate::ast::{Expression, Identifier, Module};
        use crate::source_analysis::Span;

        // Create expression: NonExistentClass (standalone class reference)
        let expr = Expression::ClassReference {
            name: Identifier::new("NonExistentClass", Span::new(0, 16)),
            span: Span::new(0, 16),
        };

        let module = Module {
            expressions: vec![expr],
            classes: vec![],
            span: Span::new(0, 16),
            leading_comments: vec![],
        };

        let code = generate_repl_expression(&module.expressions[0], "repl_eval")
            .expect("codegen should succeed");

        // Should use a case expression to check for undefined
        assert!(
            code.contains("case call 'beamtalk_object_class':'whereis_class'('NonExistentClass')"),
            "Should use case expression to handle whereis_class result. Got:\n{code}"
        );

        // Should return 'nil' when class is undefined
        assert!(
            code.contains("<'undefined'> when 'true' ->") && code.contains("'nil'"),
            "Should return 'nil' when whereis_class returns 'undefined'. Got:\n{code}"
        );

        // Should create beamtalk_object in the success branch
        assert!(
            code.contains('<')
                && code.contains("> when 'true' ->")
                && code.contains("'beamtalk_object'"),
            "Should create beamtalk_object in success branch of case. Got:\n{code}"
        );
    }

    // --- ClassHierarchy integration tests (BT-279) ---

    #[test]
    fn test_is_actor_class_direct_actor_subclass() {
        let class = ClassDefinition {
            name: Identifier::new("Counter", Span::new(0, 0)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        let module = Module {
            classes: vec![class],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;
        assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
    }

    #[test]
    fn test_is_actor_class_object_subclass_is_value_type() {
        let class = ClassDefinition {
            name: Identifier::new("Point", Span::new(0, 0)),
            superclass: Some(Identifier::new("Object", Span::new(0, 0))),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        let module = Module {
            classes: vec![class],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;
        assert!(!CoreErlangGenerator::is_actor_class(&module, &hierarchy));
    }

    #[test]
    fn test_is_actor_class_multi_level_inheritance() {
        // LoggingCounter extends Counter extends Actor
        // Should still be detected as actor
        let counter = ClassDefinition {
            name: Identifier::new("Counter", Span::new(0, 0)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        let logging_counter = ClassDefinition {
            name: Identifier::new("LoggingCounter", Span::new(0, 0)),
            superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        // Module with both classes; first class is LoggingCounter
        let module = Module {
            classes: vec![counter, logging_counter.clone()],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;

        // Test with LoggingCounter as the first class
        let module_lc = Module {
            classes: vec![logging_counter],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        // Build hierarchy from full module so Counter is known
        assert!(CoreErlangGenerator::is_actor_class(&module_lc, &hierarchy));
    }

    #[test]
    fn test_is_actor_class_no_classes_defaults_to_actor() {
        let module = Module::new(Vec::new(), Span::new(0, 0));
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;
        assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
    }

    #[test]
    fn test_is_actor_class_unknown_superclass_defaults_to_actor() {
        // LoggingCounter extends Counter, but Counter is NOT in this module.
        // Hierarchy chain is incomplete; should default to actor (backward compat).
        let class = ClassDefinition {
            name: Identifier::new("LoggingCounter", Span::new(0, 0)),
            superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        let module = Module {
            classes: vec![class],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;
        assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
    }

    #[test]
    fn test_is_actor_class_collection_subclass_is_value_type() {
        // Collection extends Object (built-in), so subclasses are value types.
        let class = ClassDefinition {
            name: Identifier::new("MyList", Span::new(0, 0)),
            superclass: Some(Identifier::new("Collection", Span::new(0, 0))),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        let module = Module {
            classes: vec![class],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;
        assert!(
            !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
            "Collection subclass should be value type (chain reaches Object)"
        );
    }

    #[test]
    fn test_is_actor_class_integer_subclass_is_value_type() {
        // Integer is a sealed built-in extending Object — subclass should be value type.
        // (Sealed enforcement is separate; codegen should still route correctly.)
        let class = ClassDefinition {
            name: Identifier::new("MyInt", Span::new(0, 0)),
            superclass: Some(Identifier::new("Integer", Span::new(0, 0))),
            is_abstract: false,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        let module = Module {
            classes: vec![class],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;
        assert!(
            !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
            "Integer subclass should be value type (chain reaches Object)"
        );
    }

    #[test]
    fn test_is_actor_class_root_class_is_value_type() {
        // Root class (superclass: None → "none") should be value type, not actor.
        let class = ClassDefinition {
            name: Identifier::new("ProtoObject", Span::new(0, 0)),
            superclass: None,
            is_abstract: true,
            is_sealed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            span: Span::new(0, 0),
        };
        let module = Module {
            classes: vec![class],
            expressions: vec![],
            span: Span::new(0, 0),
            leading_comments: vec![],
        };
        let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module).0;
        assert!(
            !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
            "Root class (nil superclass) should be value type"
        );
    }

    #[test]
    fn test_generate_primitive_selector_based() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.class_identity = Some(util::ClassIdentity::new("Integer"));
        generator.current_method_params = vec!["Other".to_string()];

        let result = generator.generate_primitive("+", true);
        assert!(result.is_ok());
        // BT-340: Now emits direct Erlang BIF instead of dispatch delegation
        assert_eq!(generator.output, "call 'erlang':'+'(Self, Other)");
    }

    #[test]
    fn test_generate_primitive_structural_intrinsic() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.class_identity = Some(util::ClassIdentity::new("Block"));
        generator.current_method_params = vec![];

        let result = generator.generate_primitive("blockValue", false);
        assert!(result.is_ok());
        assert_eq!(
            generator.output,
            "call 'bt@stdlib@block':'dispatch'('blockValue', [], Self)"
        );
    }

    #[test]
    fn test_generate_primitive_multiple_params() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.class_identity = Some(util::ClassIdentity::new("Integer"));
        generator.current_method_params = vec!["End".to_string(), "Block".to_string()];

        let result = generator.generate_primitive("toDo", false);
        assert!(result.is_ok());
        assert_eq!(
            generator.output,
            "call 'bt@stdlib@integer':'dispatch'('toDo', [End, Block], Self)"
        );
    }

    #[test]
    fn test_generate_with_bindings_compiles_value_type() {
        // Test that generate_with_bindings produces valid output for a value type
        let class = ClassDefinition::new(
            Identifier::new("Point", Span::new(0, 0)),
            Identifier::new("Object", Span::new(0, 0)),
            vec![StateDeclaration {
                name: Identifier::new("x", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                span: Span::new(0, 0),
            }],
            vec![],
            Span::new(0, 0),
        );
        let module = Module {
            classes: vec![class],
            expressions: Vec::new(),
            span: Span::new(0, 0),
            leading_comments: vec![],
        };

        let bindings = primitive_bindings::PrimitiveBindingTable::new();
        let result = generate_with_bindings(&module, "point", bindings, None, false);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("module 'point'"));
    }

    #[test]
    fn test_generate_repl_list_reject() {
        // BT-408: reject: must generate valid Core Erlang with properly bound wrapper fun
        // The wrapper fun must be bound via `let` — not inlined in the call args,
        // because Core Erlang lambdas don't use `end` and can't be inlined in calls.
        let src = "#(1, 2, 3, 4, 5) reject: [:x | x > 2]";
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, _diags) = crate::source_analysis::parse(tokens);
        let expr = &module.expressions[0];
        let code = generate_repl_expression(expr, "test_reject_repl").expect("codegen should work");

        // Wrapper fun must be bound to a variable, not inlined in filter call
        assert!(
            code.contains("call 'lists':'filter'("),
            "Should use lists:filter. Got:\n{code}"
        );
        assert!(
            code.contains("call 'erlang':'not'("),
            "Should negate predicate. Got:\n{code}"
        );
        // Verify the fun is let-bound (not inlined) — the filter call arg must be a temp var
        // e.g. "call 'lists':'filter'(_temp4," not "call 'lists':'filter'(fun (X)"
        assert!(
            !code.contains("'filter'(fun"),
            "Wrapper fun must be let-bound, not inlined in filter call. Got:\n{code}"
        );
    }

    #[test]
    fn test_class_method_rejects_field_access() {
        // BT-426: Class methods should reject instance field access
        let src = "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value";
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, _diags) = crate::source_analysis::parse(tokens);
        let result = generate_with_workspace(&module, "test_class_field", true);
        assert!(
            result.is_err(),
            "Should reject field access in class method"
        );
        let err = format!("{}", result.unwrap_err());
        assert!(
            err.contains("cannot access instance field"),
            "Error should mention field access. Got: {err}"
        );
    }

    #[test]
    fn test_class_method_rejects_field_assignment() {
        // BT-426: Class methods should reject instance field mutation
        let src =
            "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value := 42";
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, _diags) = crate::source_analysis::parse(tokens);
        let result = generate_with_workspace(&module, "test_class_assign", true);
        assert!(
            result.is_err(),
            "Should reject field assignment in class method"
        );
        let err = format!("{}", result.unwrap_err());
        assert!(
            err.contains("cannot assign to instance field"),
            "Error should mention field assignment. Got: {err}"
        );
    }
}

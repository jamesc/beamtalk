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
mod primitives;
mod repl_codegen;
pub mod selector_mangler;
mod spec_codegen;
mod state_codegen;
mod util;
mod value_type_codegen;
mod variable_context;

// Re-export utility functions for IDE queries
pub use util::to_module_name;

use crate::ast::{Block, Expression, MessageSelector, Module};
use crate::docvec;
use crate::source_analysis::{Diagnostic, Span};
use document::{Document, INDENT, line, nest};
use primitive_bindings::PrimitiveBindingTable;
use state_codegen::StateThreading;
use std::fmt;
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

    /// BT-493: Block arity mismatch with method-specific hint.
    #[error(
        "{selector} block must take {expected} argument(s), got {actual}.\n\n\
             {hint}"
    )]
    BlockArityError {
        /// The selector (e.g., "timesRepeat:").
        selector: String,
        /// The expected arity.
        expected: String,
        /// The actual arity of the block.
        actual: usize,
        /// Method-specific fix suggestion.
        hint: String,
    },
}

/// Result type for code generation operations.
pub type Result<T> = std::result::Result<T, CodeGenError>;

/// Options for Core Erlang code generation.
///
/// Replaces the combinatorial explosion of `generate_with_*` functions
/// with a single options struct. Use [`CodegenOptions::new`] to create
/// default options, then chain builder methods to customize.
///
/// # Example
///
/// ```no_run
/// use beamtalk_core::codegen::core_erlang::{CodegenOptions, generate_module};
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let code = generate_module(&module, CodegenOptions::new("counter")
///     .with_source("value := 0")
///     .with_workspace_mode(true))?;
/// # Ok::<(), beamtalk_core::codegen::core_erlang::CodeGenError>(())
/// ```
#[derive(Debug, Clone)]
pub struct CodegenOptions {
    /// The Erlang module name to generate.
    module_name: String,
    /// Original source text for `CompiledMethod` introspection (BT-101).
    source_text: Option<String>,
    /// Primitive binding table from compiled stdlib (ADR 0007).
    bindings: Option<PrimitiveBindingTable>,
    /// Whether workspace bindings are available (REPL/workspace context).
    workspace_mode: bool,
    /// Class name → compiled module name index for resolving cross-file class
    /// references in package mode (BT-794 follow-up).
    ///
    /// When populated, `compiled_module_name` checks this map first before
    /// falling back to the heuristic prefix approach. This allows classes in
    /// package subdirectories (e.g. `bt@pkg@sub@dir@class`) to be resolved
    /// correctly by all files in the package, regardless of where the caller
    /// lives in the directory tree.
    class_module_index: std::collections::HashMap<String, String>,
    /// BT-894: Class name → direct superclass name for all classes across all files.
    ///
    /// Populated during Pass 1 of package compilation alongside `class_module_index`.
    /// Used to enrich the per-file `ClassHierarchy` with cross-file inheritance
    /// information so that `is_actor_class` can resolve the full superclass chain
    /// even when the parent class is defined in another file.
    class_superclass_index: std::collections::HashMap<String, String>,
    /// Source file path to embed as `beamtalk_source` module attribute (BT-845/BT-860).
    ///
    /// When set, the generated Core Erlang module includes:
    ///   `'beamtalk_source' = ["path/to/file.bt"]`
    /// This survives workspace restarts and is the definitive source of truth
    /// for `Behaviour >> sourceFile`. Absent for stdlib and `ClassBuilder` classes.
    source_path: Option<String>,
}

impl CodegenOptions {
    /// Creates default options with the given module name.
    pub fn new(module_name: &str) -> Self {
        Self {
            module_name: module_name.to_string(),
            source_text: None,
            bindings: None,
            workspace_mode: false,
            class_module_index: std::collections::HashMap::new(),
            class_superclass_index: std::collections::HashMap::new(),
            source_path: None,
        }
    }

    /// Sets the source text for `CompiledMethod` introspection (BT-101).
    #[must_use]
    pub fn with_source(mut self, source: &str) -> Self {
        self.source_text = Some(source.to_string());
        self
    }

    /// Sets the source text from an optional value.
    #[must_use]
    pub fn with_source_opt(mut self, source: Option<&str>) -> Self {
        self.source_text = source.map(String::from);
        self
    }

    /// Sets the primitive binding table (ADR 0007).
    #[must_use]
    pub fn with_bindings(mut self, bindings: PrimitiveBindingTable) -> Self {
        self.bindings = Some(bindings);
        self
    }

    /// Enables or disables workspace mode (ADR 0010 / ADR 0019).
    #[must_use]
    pub fn with_workspace_mode(mut self, enabled: bool) -> Self {
        self.workspace_mode = enabled;
        self
    }

    /// Sets the class module index for resolving cross-file class references.
    ///
    /// Maps Beamtalk class names (e.g. `"SchemeEnv"`) to their compiled Erlang
    /// module names (e.g. `"bt@sicp_example@scheme@env"`). When set, these
    /// mappings take precedence over the heuristic prefix approach in
    /// `compiled_module_name`, fixing subdirectory class dispatch.
    #[must_use]
    pub fn with_class_module_index(
        mut self,
        index: std::collections::HashMap<String, String>,
    ) -> Self {
        self.class_module_index = index;
        self
    }

    /// BT-894: Sets the class superclass index for resolving cross-file inheritance.
    ///
    /// Maps Beamtalk class names to their direct superclass names. Used to
    /// enrich the per-file hierarchy so that `is_actor_class` can determine
    /// the correct codegen context for classes whose parents are in other files.
    #[must_use]
    pub fn with_class_superclass_index(
        mut self,
        index: std::collections::HashMap<String, String>,
    ) -> Self {
        self.class_superclass_index = index;
        self
    }

    /// Sets the source file path to embed as `beamtalk_source` module attribute (BT-845/BT-860).
    #[must_use]
    pub fn with_source_path(mut self, path: &str) -> Self {
        self.source_path = Some(path.to_string());
        self
    }

    /// Sets the source file path from an optional value (BT-845/BT-860).
    #[must_use]
    pub fn with_source_path_opt(mut self, path: Option<&str>) -> Self {
        self.source_path = path.map(String::from);
        self
    }
}

/// Generates Core Erlang code from a Beamtalk module.
///
/// This is the main entry point for code generation. It transforms
/// the parsed AST into Core Erlang text that can be compiled by `erlc`.
///
/// # BT-213: Value Types vs Actors
///
/// Routes to different code generators based on class hierarchy:
/// - **Actor subclasses** → `generate_actor_module` (`gen_server` with mailbox)
/// - **Object subclasses** → `generate_value_type_module` (plain Erlang maps)
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
/// use beamtalk_core::codegen::core_erlang::{CodegenOptions, generate_module};
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let core_erlang = generate_module(&module, CodegenOptions::new("counter"))?;
/// println!("{}", core_erlang);
/// # Ok::<(), beamtalk_core::codegen::core_erlang::CodeGenError>(())
/// ```
pub fn generate_module(module: &Module, options: CodegenOptions) -> Result<String> {
    generate_module_with_warnings(module, options).map(|m| m.code)
}

/// BT-855: Result of code generation including diagnostic warnings.
///
/// Returned by [`generate_module_with_warnings`]. Callers that need to surface
/// warnings (e.g., stateful blocks at Erlang boundaries) should use that function.
/// Callers that only need the generated code can use [`generate_module`] instead.
#[derive(Debug)]
pub struct GeneratedModule {
    /// The generated Core Erlang code.
    pub code: String,
    /// Diagnostic warnings emitted during code generation.
    ///
    /// Each entry is a structured [`Diagnostic`] with severity, source span, and
    /// message. Examples:
    /// - A stateful Beamtalk block was passed to an Erlang call site — mutations
    ///   inside the block will be silently dropped since Erlang cannot propagate
    ///   the updated `StateAcc` back to the Beamtalk caller.
    pub warnings: Vec<Diagnostic>,
}

/// Generates Core Erlang for a module, returning the code and any diagnostic warnings.
///
/// Like [`generate_module`] but also returns warnings emitted during generation.
/// Use this when you need to surface warnings (e.g., for IDE diagnostics or compiler output).
///
/// # Errors
///
/// Returns [`CodeGenError`] if:
/// - The module uses unsupported features
/// - Code generation encounters an internal error
/// - Formatting fails
pub fn generate_module_with_warnings(
    module: &Module,
    options: CodegenOptions,
) -> Result<GeneratedModule> {
    let mut generator = if let Some(bindings) = options.bindings {
        CoreErlangGenerator::with_bindings(&options.module_name, bindings)
    } else {
        CoreErlangGenerator::new(&options.module_name)
    };
    generator.source_text = options.source_text;
    generator.workspace_mode = options.workspace_mode;
    generator.class_module_index = options.class_module_index;
    generator.source_path = options.source_path;

    // Build hierarchy once for the entire generation (ADR 0006)
    let (hierarchy_result, _) =
        crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(module);
    let mut hierarchy =
        hierarchy_result.map_err(|e| CodeGenError::Internal(format!("hierarchy: {e:?}")))?;

    // BT-894: Enrich hierarchy with cross-file superclass information so that
    // is_actor_class can resolve the full inheritance chain for classes whose
    // parents are defined in other files.
    hierarchy.add_external_superclasses(&options.class_superclass_index);

    // BT-213: Route based on whether class is actor or value type
    let doc = if CoreErlangGenerator::is_actor_class(module, &hierarchy) {
        generator.generate_actor_module(module)?
    } else {
        generator.generate_value_type_module(module)?
    };

    Ok(GeneratedModule {
        code: doc.to_pretty_string(),
        warnings: generator.codegen_warnings,
    })
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
    let doc = generator.generate_repl_module(expression)?;
    Ok(doc.to_pretty_string())
}

/// Generates Core Erlang for multiple REPL expressions (BT-780).
///
/// Like [`generate_repl_expression`] but accepts a slice of expressions,
/// generating a single `eval/1` that evaluates all of them in sequence
/// and threads the bindings State between identifier assignments.
///
/// For a single expression, delegates to [`generate_repl_expression`].
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_repl_expressions(expressions: &[Expression], module_name: &str) -> Result<String> {
    generate_repl_expressions_with_index(expressions, module_name, std::collections::HashMap::new())
}

/// Generates Core Erlang for multiple REPL expressions with a class module index.
///
/// Like [`generate_repl_expressions`] but accepts a `class_module_index` that maps
/// class names to their compiled module names (e.g. `"Counter"` → `"bt@getting_started@counter"`).
/// This is needed in workspace/package mode so that `compiled_module_name` resolves
/// class references correctly instead of falling back to the heuristic prefix.
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
#[allow(clippy::implicit_hasher)] // concrete HashMap matches internal generator field type
pub fn generate_repl_expressions_with_index(
    expressions: &[Expression],
    module_name: &str,
    class_module_index: std::collections::HashMap<String, String>,
) -> Result<String> {
    if expressions.is_empty() {
        return Err(CodeGenError::UnsupportedFeature {
            feature: "empty expression list".to_string(),
            location: module_name.to_string(),
        });
    }
    let mut generator = CoreErlangGenerator::new(module_name);
    generator.class_module_index = class_module_index;
    let doc = generator.generate_repl_module_multi(expressions)?;
    Ok(doc.to_pretty_string())
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
    let doc = generator.generate_test_module(expression)?;
    Ok(doc.to_pretty_string())
}

/// Generates Core Erlang code with default module name `bt_module`.
///
/// Convenience wrapper around [`generate_module`] for simple use cases.
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate(module: &Module) -> Result<String> {
    generate_module(module, CodegenOptions::new("bt_module"))
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

/// BT-764: Variable names for value type NLR try/catch wrapping.
///
/// Holds the fresh temporary variable names generated by
/// `wrap_value_type_body_with_nlr_catch` so the caller can emit the
/// try/catch template with consistent variable names.
#[allow(clippy::struct_field_names)]
pub(super) struct NlrValueTypeCatchVars {
    pub token_var: String,
    pub result_var: String,
    pub cls_var: String,
    pub err_var: String,
    pub stk_var: String,
    pub ctk_var: String,
    pub val_var: String,
    /// BT-854: State variable captured from the 4-tuple NLR throw.
    pub state_var: String,
    pub ot_pair_var: String,
}

impl NlrValueTypeCatchVars {
    /// Formats the try prefix for the NLR wrapper.
    ///
    /// BT-774: Returns `Document` instead of `String` for composable codegen.
    ///
    /// ```text
    /// let TokenVar = call 'erlang':'make_ref'() in
    /// try
    /// ```
    pub fn format_try_prefix(&self) -> Document<'static> {
        docvec![
            "    let ",
            self.token_var.clone(),
            " = call 'erlang':'make_ref'() in",
            nest(INDENT, line()),
            "try",
            line(),
        ]
    }

    /// Formats the catch suffix for the NLR wrapper.
    ///
    /// BT-774: Returns `Document` instead of `String` for composable codegen.
    /// BT-854: Catches 4-tuple NLR throws and returns `{NlrVal, NlrState}`.
    ///
    /// ```text
    /// of Result -> Result
    /// catch <Cls, Err, Stk> ->
    ///   case {Cls, Err} of
    ///     <{'throw', {'$bt_nlr', CatchTok, Val, State}}> when ... -> {Val, State}
    ///     <Other> when 'true' -> primop 'raw_raise'(Cls, Err, Stk)
    ///   end
    /// ```
    pub fn format_catch_suffix(&self) -> Document<'static> {
        docvec![
            nest(INDENT, line()),
            "of ",
            self.result_var.clone(),
            " -> ",
            self.result_var.clone(),
            nest(INDENT, line()),
            "catch <",
            self.cls_var.clone(),
            ", ",
            self.err_var.clone(),
            ", ",
            self.stk_var.clone(),
            "> ->",
            nest(INDENT + 2, line()),
            "case {",
            self.cls_var.clone(),
            ", ",
            self.err_var.clone(),
            "} of",
            nest(INDENT + 4, line()),
            "<{'throw', {'$bt_nlr', ",
            self.ctk_var.clone(),
            ", ",
            self.val_var.clone(),
            ", ",
            self.state_var.clone(),
            "}}> ",
            "when call 'erlang':'=:='(",
            self.ctk_var.clone(),
            ", ",
            self.token_var.clone(),
            ") -> {",
            self.val_var.clone(),
            ", ",
            self.state_var.clone(),
            "}",
            nest(INDENT + 4, line()),
            "<",
            self.ot_pair_var.clone(),
            "> when 'true' -> ",
            "primop 'raw_raise'(",
            self.cls_var.clone(),
            ", ",
            self.err_var.clone(),
            ", ",
            self.stk_var.clone(),
            ")",
            nest(INDENT + 2, line()),
            "end",
            line(),
        ]
    }
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
    #[allow(dead_code)] // stored for future call-site optimization with static typing
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
    /// BT-374 / ADR 0010 / ADR 0019: Whether workspace bindings are available.
    /// When true (REPL/workspace context), class references resolve through
    /// session bindings or class registry. When false (batch compile),
    /// class references go directly to the class registry.
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
    /// BT-754: Core Erlang variable name holding the non-local return token for the current
    /// value type method, or `None` when no NLR infrastructure is active.
    ///
    /// Set by `generate_value_type_method` when the method body contains blocks with `^`.
    /// When set, `generate_expression` for `Expression::Return` generates a throw instead
    /// of a plain value, causing the return to escape from the enclosing block closure.
    current_nlr_token: Option<String>,
    /// BT-833: Self-threading version counter for value type field assignments.
    ///
    /// Mirrors `state_threading` for value types. Each field assignment increments
    /// this counter: `Self` → `Self1` → `Self2` → ... so that `self` in expression
    /// position always resolves to the latest immutable snapshot.
    self_version: usize,
    /// Class name → compiled module name index for resolving cross-file class references.
    ///
    /// Populated from `CodegenOptions::class_module_index` before generation begins.
    /// Used by `compiled_module_name` to resolve subdirectory classes correctly.
    class_module_index: std::collections::HashMap<String, String>,
    /// BT-845/BT-860: Source file path to embed as `beamtalk_source` module attribute.
    /// Set from `CodegenOptions::source_path` before generation begins.
    source_path: Option<String>,
    /// BT-851: Tier 2 block parameters for the current method being compiled.
    ///
    /// When a method parameter name is in this set, `value:` / `value:value:` calls
    /// on that parameter use the stateful Tier 2 protocol:
    /// `apply _Fun(Args..., State) → {Result, NewState}`.
    tier2_block_params: std::collections::HashSet<String>,
    /// BT-851: Pre-scanned Tier 2 block info for the current class.
    ///
    /// Maps method selector → list of parameter indices that receive Tier 2 blocks
    /// from self-sends within the same class. Populated by `scan_class_for_tier2_blocks`
    /// before method body generation.
    tier2_method_info: std::collections::HashMap<String, Vec<usize>>,
    /// BT-855: Diagnostic warnings emitted during code generation.
    ///
    /// Collected during generation and returned to callers via
    /// [`generate_module_with_warnings`]. Examples include stateful blocks
    /// passed to Erlang call sites where mutations will be silently dropped.
    pub(super) codegen_warnings: Vec<Diagnostic>,
}

impl CoreErlangGenerator {
    /// Creates a new code generator for the given module name.
    fn new(module_name: &str) -> Self {
        Self {
            module_name: module_name.to_string(),
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
            current_nlr_token: None,
            self_version: 0,
            class_module_index: std::collections::HashMap::new(),
            source_path: None,
            tier2_block_params: std::collections::HashSet::new(),
            tier2_method_info: std::collections::HashMap::new(),
            codegen_warnings: Vec::new(),
        }
    }

    /// Creates a new code generator with a primitive binding table.
    fn with_bindings(module_name: &str, bindings: PrimitiveBindingTable) -> Self {
        Self {
            module_name: module_name.to_string(),
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
            current_nlr_token: None,
            self_version: 0,
            class_module_index: std::collections::HashMap::new(),
            source_path: None,
            tier2_block_params: std::collections::HashSet::new(),
            tier2_method_info: std::collections::HashMap::new(),
            codegen_warnings: Vec::new(),
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

    /// BT-833: Returns the current Self variable name for value type Self-threading.
    ///
    /// Version 0 → `"Self"` (the original method parameter).
    /// Version N → `"Self{N}"` (after N field assignments have threaded a new snapshot).
    pub(super) fn current_self_var(&self) -> String {
        if self.self_version == 0 {
            "Self".to_string()
        } else {
            format!("Self{}", self.self_version)
        }
    }

    /// BT-833: Increments the Self version and returns the new variable name.
    pub(super) fn next_self_var(&mut self) -> String {
        self.self_version += 1;
        format!("Self{}", self.self_version)
    }

    /// BT-855: Records a structured diagnostic warning for the current module.
    ///
    /// Warnings are returned to callers via [`generate_module_with_warnings`].
    pub(super) fn add_codegen_warning(&mut self, diag: Diagnostic) {
        self.codegen_warnings.push(diag);
    }

    /// BT-855: Emits the standard warning for a stateful block at an Erlang call boundary.
    ///
    /// Both `generate_simple_list_op` and `generate_direct_erlang_call` call this helper
    /// to ensure consistent warning messages across all Erlang interop sites.
    ///
    /// `erlang_target` is a human-readable call target, e.g. `"'lists':'map'"` or
    /// `"'mymod':'myfun'"`.
    /// `span` is the source span of the block literal that crosses the boundary.
    pub(super) fn warn_stateful_block_at_erlang_boundary(
        &mut self,
        erlang_target: &str,
        span: Span,
    ) {
        self.add_codegen_warning(Diagnostic::warning(
            format!(
                "stateful block passed to Erlang {erlang_target} — mutations inside \
                 the block will be silently dropped (Erlang cannot propagate the updated \
                 StateAcc back to the Beamtalk caller)"
            ),
            span,
        ));
    }

    /// BT-909: Emits a warning for a non-literal callable at an Erlang call boundary.
    pub(super) fn warn_non_literal_callable_at_erlang_boundary(
        &mut self,
        erlang_target: &str,
        span: Span,
    ) {
        self.add_codegen_warning(Diagnostic::warning(
            format!(
                "non-literal callable passed to Erlang {erlang_target} — if this is a \
                 stateful block, mutations inside the block will be silently dropped \
                 (runtime arity check inserted to prevent badarity crash)"
            ),
            span,
        ));
    }

    /// BT-833: Resets the Self version to 0 (call at the start of each value type method).
    pub(super) fn reset_self_version(&mut self) {
        self.self_version = 0;
    }

    /// BT-940: Converts a byte-offset `Span` to a 1-based line number.
    ///
    /// Uses `self.source_text` to count newlines before the span's start offset.
    /// Returns `None` if source text is unavailable or the span is out of range.
    pub(super) fn span_to_line(&self, span: Span) -> Option<u32> {
        let source = self.source_text.as_deref()?;
        let offset = span.start() as usize;
        if offset > source.len() {
            return None;
        }
        let newline_count = source[..offset].bytes().filter(|&b| b == b'\n').count();
        #[expect(
            clippy::cast_possible_truncation,
            reason = "source files over 4GB (2^32 lines) are not supported"
        )]
        let line = newline_count as u32 + 1;
        Some(line)
    }

    /// BT-940: Wraps a Document with a Core Erlang line annotation.
    ///
    /// Produces `( Doc -| [{'line', N}] )` which `erlc` preserves into BEAM
    /// `debug_info`. The BEAM VM surfaces this as `{line, N}` in stacktraces.
    pub(super) fn annotate_with_line(doc: Document<'static>, line_num: u32) -> Document<'static> {
        docvec![
            "( ",
            doc,
            " -| [{'line', ",
            Document::String(line_num.to_string()),
            "}] )"
        ]
    }

    /// BT-153/BT-245/BT-598: Check if mutation threading should be used for a block.
    /// In REPL mode, local variable mutations trigger threading.
    /// In actor module mode, field writes, self-sends, OR local variable
    /// mutations trigger threading. Local vars are threaded through the state accumulator
    /// map alongside fields.
    /// In value type module mode, only field writes trigger threading (no state map).
    pub(super) fn needs_mutation_threading(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
    ) -> bool {
        if self.is_repl_mode {
            // REPL: both local vars and fields need threading
            analysis.has_mutations()
        } else if self.context == CodeGenContext::Actor {
            // BT-598: Actor methods: field writes, self-sends,
            // OR local variable mutations all need threading
            analysis.has_state_effects() || !analysis.local_writes.is_empty()
        } else {
            // BT-892: Value types have no State variable, so self-sends should
            // NOT trigger state threading. Only field writes need threading.
            !analysis.field_writes.is_empty()
        }
    }

    /// BT-764: Wraps an actor method body with NLR (non-local return) try/catch.
    ///
    /// Actor NLR uses a 4-element throw tuple `{$bt_nlr, Token, Value, State}` and
    /// catches it to produce `{reply, Value, State}`.
    ///
    /// When `needs_letrec` is true, the body is wrapped in a `letrec` function to
    /// create a separate function frame, avoiding BEAM validator
    /// `ambiguous_catch_try_state` errors in case arms. When false (sealed methods),
    /// the try/catch is emitted directly.
    ///
    /// BT-774: Accepts and returns `Document` instead of `String` to avoid
    /// intermediate string rendering.
    pub(super) fn wrap_actor_body_with_nlr_catch(
        &mut self,
        body_doc: Document<'static>,
        token_var: &str,
        needs_letrec: bool,
    ) -> Document<'static> {
        let result_var = self.fresh_temp_var("NlrResult");
        let cls_var = self.fresh_temp_var("NlrCls");
        let err_var = self.fresh_temp_var("NlrErr");
        let stk_var = self.fresh_temp_var("NlrStk");
        let ctk_var = self.fresh_temp_var("CatchTok");
        let val_var = self.fresh_temp_var("NlrVal");
        let state_var = self.fresh_temp_var("NlrState");
        let ot_pair_var = self.fresh_temp_var("OtherPair");

        let try_catch = docvec![
            "let ",
            token_var.to_string(),
            " = call 'erlang':'make_ref'() in\n",
            "try\n",
            body_doc,
            "\n",
            "of ",
            result_var.clone(),
            " -> ",
            result_var,
            "\n",
            "catch <",
            cls_var.clone(),
            ", ",
            err_var.clone(),
            ", ",
            stk_var.clone(),
            "> ->\n",
            "  case {",
            cls_var.clone(),
            ", ",
            err_var.clone(),
            "} of\n",
            "    <{'throw', {'$bt_nlr', ",
            ctk_var.clone(),
            ", ",
            val_var.clone(),
            ", ",
            state_var.clone(),
            "}}> ",
            "when call 'erlang':'=:='(",
            ctk_var,
            ", ",
            token_var.to_string(),
            ") -> ",
            "{'reply', ",
            val_var,
            ", ",
            state_var,
            "}\n",
            "    <",
            ot_pair_var,
            "> when 'true' -> ",
            "primop 'raw_raise'(",
            cls_var,
            ", ",
            err_var,
            ", ",
            stk_var,
            ")\n",
            "  end",
        ];

        if needs_letrec {
            docvec![
                "letrec '__nlr_body'/0 = fun () ->\n",
                try_catch,
                "\n",
                "in apply '__nlr_body'/0 ()",
            ]
        } else {
            try_catch
        }
    }

    /// BT-764/BT-854: Wraps a value type method body with NLR (non-local return) try/catch.
    ///
    /// Value type NLR uses a 4-element throw tuple `{$bt_nlr, Token, Value, State}`
    /// and catches it to return `{Value, State}`. The body parts are provided as a
    /// `Document` and the result is a complete function definition document.
    pub(super) fn wrap_value_type_body_with_nlr_catch(
        &mut self,
        token_var: &str,
    ) -> NlrValueTypeCatchVars {
        let result_var = self.fresh_temp_var("NlrResult");
        let cls_var = self.fresh_temp_var("NlrCls");
        let err_var = self.fresh_temp_var("NlrErr");
        let stk_var = self.fresh_temp_var("NlrStk");
        let ctk_var = self.fresh_temp_var("CatchTok");
        let val_var = self.fresh_temp_var("NlrVal");
        let state_var = self.fresh_temp_var("NlrState");
        let ot_pair_var = self.fresh_temp_var("OtherPair");

        NlrValueTypeCatchVars {
            token_var: token_var.to_string(),
            result_var,
            cls_var,
            err_var,
            stk_var,
            ctk_var,
            val_var,
            state_var,
            ot_pair_var,
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
            // If the chain terminated at a known value-type root (Object/ProtoObject/Value),
            // this is definitely a value type.
            // BT-480: Include exception hierarchy classes — these inherit from Object
            // (Exception → Object) and must compile as value types, not actors.
            // BT-925: Include "Value" — when cross-file superclass indexing is incomplete
            // (e.g. independent file compilation), chains for direct Value subclasses
            // (Collection, Number, Boolean, Exception) can terminate at "Value". Treat
            // "Value" as a known value-type root so these classes compile as value types
            // instead of actors.
            let known_value_roots = [
                "Object",
                "ProtoObject",
                "Value",
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
    fn generate_start_link_doc(&self) -> Document<'static> {
        docvec![
            "'start_link'/1 = fun (InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    format!(
                        "call 'gen_server':'start_link'('{}', InitArgs, [])",
                        self.module_name
                    ),
                ]
            ),
            "\n\n",
        ]
    }

    /// Generates the `start_link/2` function for named `gen_server` startup.
    ///
    /// This is the OTP entry point for starting a supervised `gen_server` with
    /// a registered name (e.g. `{local, 'Transcript'}`). Used by workspace
    /// supervisors to start singleton actors under their binding name.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'start_link'/2 = fun (ServerName, InitArgs) ->
    ///     call 'gen_server':'start_link'(ServerName, 'module_name', InitArgs, [])
    /// ```
    fn generate_start_link_named_doc(&self) -> Document<'static> {
        docvec![
            "'start_link'/2 = fun (ServerName, InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "call 'gen_server':'start_link'(ServerName, '",
                        Document::String(self.module_name.clone()),
                        "', InitArgs, [])",
                    ],
                ]
            ),
            "\n\n",
        ]
    }

    /// Generates the `dispatch/3` function that delegates to the primitives module.
    ///
    /// For actor classes with `@primitive` methods, the compiled dispatch/4 calls
    /// `Module:dispatch(Selector, Args, Self)` (3-arity) for primitive method bodies.
    /// This function provides that 3-arity entry point, delegating to the corresponding
    /// `beamtalk_{snake_case}_primitives` module which implements the actual Erlang logic.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'dispatch'/3 = fun (Selector, Args, Self) ->
    ///     call 'beamtalk_transcript_stream_primitives':'dispatch'(Selector, Args, Self)
    /// ```
    fn generate_primitive_dispatch_3_doc(&self) -> Document<'static> {
        // Build primitives module name: `beamtalk_{snake}_primitives`.
        // If `to_module_name` already emits a `beamtalk_` prefix (e.g.
        // `BeamtalkInterface` → `beamtalk_interface`), avoid doubling it.
        let snake_name = to_module_name(&self.class_name());
        let primitives_module = if snake_name.starts_with("beamtalk_") {
            format!("{snake_name}_primitives")
        } else {
            format!("beamtalk_{snake_name}_primitives")
        };
        docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "call '",
                        Document::String(primitives_module),
                        "':'dispatch'(Selector, Args, Self)",
                    ],
                ]
            ),
            "\n\n",
        ]
    }

    ///
    /// Generates code for an expression by dispatching to the appropriate handler.
    ///
    /// This is the main expression dispatcher that routes each AST node type
    /// to its specialized code generation method.
    ///
    /// ADR 0018 Phase 3: Returns `Document<'static>` directly for composable
    /// code generation without string buffer intermediaries.
    fn generate_expression(&mut self, expr: &Expression) -> Result<Document<'static>> {
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => self.generate_identifier(id),
            Expression::ClassReference { name, .. } => self.generate_class_reference(&name.name),
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
                span,
                is_cast,
                ..
            } => {
                let doc = if *is_cast {
                    self.generate_cast_send(receiver, selector, arguments)
                } else {
                    self.generate_message_send(receiver, selector, arguments)
                }?;
                // BT-940: Annotate message sends with source line for BEAM stacktraces.
                // Only annotate CLOSED expressions — class method sends return open let-chains
                // (last_open_scope_result is Some after the call) which cannot be annotated.
                if self.last_open_scope_result.is_none() {
                    if let Some(line_num) = self.span_to_line(*span) {
                        return Ok(Self::annotate_with_line(doc, line_num));
                    }
                }
                Ok(doc)
            }
            Expression::Assignment { target, value, .. } => {
                // BT-852: Stored blocks with mutations are now supported via Tier 2.
                // generate_block() handles stateful emission; no validation needed here.

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
                // BT-754: If inside a block with NLR infrastructure active, generate a throw
                // so the return escapes from the block closure back to the enclosing method.
                // Otherwise (at method body level, or no NLR), just emit the value.
                if let Some(nlr_token) = self.current_nlr_token.clone() {
                    let val_doc = self.generate_expression(value)?;
                    // BT-761/BT-854: All NLR throws carry state as a 4-tuple.
                    // Actor methods use the current gen_server state; value type
                    // methods use the latest Self{N} snapshot so field mutations
                    // accumulated before the ^ are preserved.
                    let state = if self.context == CodeGenContext::Actor {
                        self.current_state_var()
                    } else {
                        self.current_self_var()
                    };
                    Ok(docvec![
                        "call 'erlang':'throw'({'$bt_nlr', ",
                        nlr_token,
                        ", ",
                        val_doc,
                        ", ",
                        state,
                        "})"
                    ])
                } else {
                    // Return in Core Erlang is just the value
                    self.generate_expression(value)
                }
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => self.generate_field_access(receiver, field),
            Expression::Parenthesized { expression, .. } => self.generate_expression(expression),
            Expression::MapLiteral { pairs, .. } => self.generate_map_literal(pairs),
            Expression::ListLiteral { elements, tail, .. } => {
                self.generate_list_literal(elements, tail.as_deref())
            }
            Expression::ArrayLiteral { elements, .. } => self.generate_array_literal(elements),
            Expression::Cascade {
                receiver, messages, ..
            } => self.generate_cascade(receiver, messages),
            Expression::Primitive {
                name,
                is_quoted,
                span,
                ..
            } => self.generate_primitive(name, *is_quoted, *span),
            Expression::Match { value, arms, .. } => self.generate_match(value, arms),
            Expression::StringInterpolation { segments, .. } => {
                self.generate_string_interpolation(segments)
            }
            Expression::Error { message, span, .. } => Err(CodeGenError::UnsupportedFeature {
                feature: format!("expression error: {message}"),
                location: format!("{span:?}"),
            }),
            Expression::ExpectDirective { .. } => Ok(Document::Nil),
        }
    }

    /// Generates code for a standalone `ClassReference`.
    ///
    /// ADR 0019 Phase 3: In workspace mode, checks REPL bindings first for
    /// convenience names (Transcript, Beamtalk, Workspace), then falls back
    /// to class registry lookup. In batch mode, goes directly to the registry.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    fn generate_class_reference(&mut self, class_name: &str) -> Result<Document<'static>> {
        // ADR 0019 Phase 3: Only check bindings in REPL top-level context.
        // Actor methods compiled in workspace mode should NOT check REPL bindings.
        if self.workspace_mode && self.context == CodeGenContext::Repl {
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let state_var = self.current_state_var();
            let error_doc = self.class_not_found_error_doc(class_name);

            Ok(docvec![
                format!("case call 'maps':'find'('{class_name}', {state_var}) of "),
                "<{'ok', _BindingVal}> when 'true' -> _BindingVal ",
                "<'error'> when 'true' -> ",
                format!("case call 'beamtalk_class_registry':'whereis_class'('{class_name}') of "),
                error_doc,
                format!("<{class_pid_var}> when 'true' -> "),
                format!(
                    "let {class_mod_var} = call 'beamtalk_object_class':'module_name'({class_pid_var}) in "
                ),
                format!(
                    "{{'beamtalk_object', '{class_name} class', {class_mod_var}, {class_pid_var}}} "
                ),
                "end end",
            ])
        } else if self.workspace_mode {
            // Actor/ValueType methods in workspace mode: try class lookup.
            // ADR 0019 Phase 4: No persistent_term fallback — use class registry only.
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let error_doc = self.class_not_found_error_doc(class_name);

            Ok(docvec![
                format!("case call 'beamtalk_class_registry':'whereis_class'('{class_name}') of "),
                error_doc,
                format!("<{class_pid_var}> when 'true' -> "),
                format!(
                    "let {class_mod_var} = call 'beamtalk_object_class':'module_name'({class_pid_var}) in "
                ),
                format!(
                    "{{'beamtalk_object', '{class_name} class', {class_mod_var}, {class_pid_var}}} "
                ),
                "end",
            ])
        } else {
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let error_doc = self.class_not_found_error_doc(class_name);

            Ok(docvec![
                format!("case call 'beamtalk_class_registry':'whereis_class'('{class_name}') of "),
                error_doc,
                format!("<{class_pid_var}> when 'true' -> "),
                format!(
                    "let {class_mod_var} = call 'beamtalk_object_class':'module_name'({class_pid_var}) in "
                ),
                format!(
                    "{{'beamtalk_object', '{class_name} class', {class_mod_var}, {class_pid_var}}} "
                ),
                "end",
            ])
        }
    }

    /// Generates Core Erlang code that raises a `class_not_found` error for undefined classes.
    ///
    /// Returns the document fragment for the `<'undefined'>` case branch.
    fn class_not_found_error_doc(&mut self, class_name: &str) -> Document<'static> {
        let err0_var = self.fresh_var("CnfErr");
        let err1_var = self.fresh_var("CnfErr");
        let hint = format!("Define {class_name} with: Object subclass: {class_name}");
        let hint_binary = Self::binary_string_literal(&hint);

        docvec![
            format!(
                "<'undefined'> when 'true' -> let {err0_var} = call 'beamtalk_error':'new'('class_not_found', '{class_name}') in "
            ),
            format!(
                "let {err1_var} = call 'beamtalk_error':'with_hint'({err0_var}, {hint_binary}) in "
            ),
            format!("call 'beamtalk_error':'raise'({err1_var}) "),
        ]
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
    #[allow(clippy::too_many_lines)] // one branch per control flow construct (whileTrue:, timesRepeat:, etc.)
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

        // Check for to:do: or to:by:do: with literal block
        if (selector_name == "to:do:"
            && arguments.len() == 2
            && matches!(&arguments[1], Expression::Block(_)))
            || (selector_name == "to:by:do:"
                && arguments.len() == 3
                && matches!(&arguments[2], Expression::Block(_)))
        {
            let body_block_expr = if selector_name == "to:do:" {
                &arguments[1]
            } else {
                &arguments[2]
            };
            let Expression::Block(body_block) = body_block_expr else {
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
    /// BT-852: This function is retained for unit tests. Production call sites have been
    /// removed since stored closures with mutations are now supported via the Tier 2
    /// stateful block protocol (ADR 0041).
    #[allow(dead_code)]
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
        // BT-665: Only flag mutations of captured variables, not new local definitions.
        // A "captured mutation" is a write to a variable that was read before being
        // locally defined (i.e., it captures from outer scope).
        if let Some(variable) = analysis
            .local_writes
            .intersection(&analysis.captured_reads)
            .next()
        {
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
    fn generate_primitive(
        &mut self,
        name: &str,
        is_quoted: bool,
        span: crate::source_analysis::Span,
    ) -> Result<Document<'static>> {
        let class_name = self
            .class_identity
            .as_ref()
            .map(|id| id.class_name().to_string())
            .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "@primitive '{name}' used outside of a class context"
                ))
            })?;

        // ADR 0038: ClassBuilder register intrinsic — emits a call to
        // beamtalk_class_builder:register/1 with the builder's gen_server state.
        if !is_quoted && name == "classBuilderRegister" {
            return Ok(self.generate_class_builder_register());
        }

        // BT-340: For selector-based primitives, try to emit a direct BIF call
        // instead of delegating through a hand-written dispatch module.
        if is_quoted {
            let params = self.current_method_params.clone();
            if let Some(code) = primitives::generate_primitive_bif(&class_name, name, &params) {
                return Ok(code);
            }
        }

        // Fallback: delegate to runtime dispatch module.
        // This path is used for:
        // - Structural intrinsics (unquoted) — handled at call site, body is placeholder
        // - Selector-based primitives with no known BIF (unimplemented or complex)
        let runtime_module = PrimitiveBindingTable::runtime_module_for_class(&class_name);

        // BT-938: Validate that the target dispatch module exists in the known stdlib
        // module set. Only check when binding data is available (non-empty binding table).
        // An empty table means no stdlib was loaded, so we skip validation silently.
        if is_quoted && !self.primitive_bindings.is_empty() {
            let known = self.primitive_bindings.known_runtime_modules();
            if !known.contains(&runtime_module) {
                self.add_codegen_warning(Diagnostic::warning(
                    format!(
                        "@primitive '{name}' references module '{runtime_module}' which has not been compiled — ensure the class is included in the stdlib build"
                    ),
                    span,
                ));
            }
        }

        let params_str = self.current_method_params.join(", ");
        // BT-677: In class methods, self is bound to ClassSelf, not Self
        let self_var = if self.in_class_method {
            "ClassSelf"
        } else {
            "Self"
        };
        Ok(Document::String(format!(
            "call '{runtime_module}':'dispatch'('{name}', [{params_str}], {self_var})"
        )))
    }

    /// ADR 0038: Generates code for the `classBuilderRegister` intrinsic.
    ///
    /// Emits a call to `beamtalk_class_builder:register/1` with the builder's
    /// `gen_server` state augmented with the builder's own PID (for cleanup).
    ///
    /// On success: returns a `#beamtalk_object{class = 'Class', class_mod = beamtalk_class_bt, pid = Pid}`
    /// On error: raises the structured error via `beamtalk_error:raise/1`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Pid = call 'erlang':'self'() in
    /// let _BS = call 'maps':'put'('builderPid', _Pid, State) in
    /// case call 'beamtalk_class_builder':'register'(_BS) of
    ///   <{'ok', _CP}> when 'true' ->
    ///     {'beamtalk_object', 'Class', 'beamtalk_class_bt', _CP}
    ///   <{'error', _Err}> when 'true' ->
    ///     call 'beamtalk_error':'raise'(_Err)
    /// end
    /// ```
    fn generate_class_builder_register(&mut self) -> Document<'static> {
        let pid_var = self.fresh_temp_var("BuilderPid");
        let state_var = self.fresh_temp_var("BuilderState");
        let class_pid_var = self.fresh_temp_var("ClassPid");
        let error_var = self.fresh_temp_var("RegErr");
        let current_state = self.current_state_var();

        docvec![
            "let ",
            Document::String(pid_var.clone()),
            " = call 'erlang':'self'() in ",
            "let ",
            Document::String(state_var.clone()),
            " = call 'maps':'put'('builderPid', ",
            Document::String(pid_var),
            ", ",
            Document::String(current_state),
            ") in ",
            "case call 'beamtalk_class_builder':'register'(",
            Document::String(state_var),
            ") of ",
            "<{'ok', ",
            Document::String(class_pid_var.clone()),
            "}> when 'true' -> ",
            "{'beamtalk_object', 'Class', 'beamtalk_class_bt', ",
            Document::String(class_pid_var),
            "} ",
            "<{'error', ",
            Document::String(error_var.clone()),
            "}> when 'true' -> ",
            "call 'beamtalk_error':'raise'(",
            Document::String(error_var),
            ") ",
            "end"
        ]
    }
}

#[cfg(test)]
mod tests;

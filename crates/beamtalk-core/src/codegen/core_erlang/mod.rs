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
pub mod cerl;
mod class_builder_source;
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
pub mod selector_mangler;
mod spec_codegen;
mod state_codegen;
mod supervisor_codegen;
mod threaded_expr;
mod util;
mod value_type_codegen;
mod variable_context;

// Re-export utility functions for IDE queries
pub use util::escape_atom_chars;
pub use util::to_module_name;

use crate::ast::{Block, Expression, MessageSelector, Module, WellKnownSelector};
use crate::docvec;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use document::leaf;
use document::{Document, INDENT, line, nest};
use ecow::EcoString;
use primitive_bindings::PrimitiveBindingTable;
use state_codegen::StateThreading;
use std::collections::HashSet;
use std::fmt;
use thiserror::Error;
use variable_context::VariableContext;

/// Display wrapper for `Option<Span>` in error messages.
///
/// Renders `" at offset N"` when a span is present, or empty string when `None`.
/// Consumers with source text (REPL, MCP) should use the raw `Span` for richer
/// formatting (Miette highlighting, "line N, col C", etc.).
struct DisplayOptionalSpan<'a>(&'a Option<Span>);

impl fmt::Display for DisplayOptionalSpan<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(s) => write!(f, " at offset {}", s.start()),
            None => Ok(()),
        }
    }
}

/// Errors that can occur during code generation.
#[derive(Debug, Error)]
pub enum CodeGenError {
    /// Unsupported language feature.
    #[error("unsupported feature: {feature}{}", DisplayOptionalSpan(.span))]
    UnsupportedFeature {
        /// The feature that is not yet supported.
        feature: String,
        /// Source span for rich error rendering (Miette / MCP).
        span: Option<Span>,
    },

    /// BT-2233: A quoted `@primitive "selector"` in a stdlib value-type class has
    /// no inline BIF lowering registered in `generate_primitive_bif`. Without a
    /// mapping it would silently fall back to runtime dispatch and raise
    /// `does_not_understand` at runtime (the BT-2232 regression). Only raised in
    /// stdlib mode; actor classes and a small set of call-site-intercepted
    /// operations are exempt (see the guard in `generate_primitive`).
    #[error(
        "unmapped @primitive \"{selector}\" in class '{class}'{}: no inline BIF lowering registered. \
         Add a mapping for {class}:{selector} in \
         crates/beamtalk-core/src/codegen/core_erlang/primitives/, otherwise this method falls back \
         to runtime dispatch and raises does_not_understand at runtime.",
        DisplayOptionalSpan(.span)
    )]
    UnmappedPrimitive {
        /// The defining class name.
        class: String,
        /// The quoted primitive selector with no inline BIF mapping.
        selector: String,
        /// Source span for rich error rendering (Miette / MCP).
        span: Option<Span>,
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

impl CodeGenError {
    /// Returns the source span associated with this error, if any.
    ///
    /// Consumers with source text can use this for rich error formatting:
    /// - REPL: Miette source highlighting
    /// - MCP: "line N, col C" format
    pub fn span(&self) -> Option<Span> {
        match self {
            CodeGenError::UnsupportedFeature { span, .. }
            | CodeGenError::UnmappedPrimitive { span, .. } => *span,
            _ => None,
        }
    }
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
    /// The Erlang module name to generate (ref-counted for O(1) clone).
    module_name: EcoString,
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
    /// Whether this module is being compiled in stdlib mode (BT-791).
    ///
    /// When true, the generated `register_class/0` emits `stdlibMode => true` in
    /// the builder state map, which tells `beamtalk_class_builder:register/1` to
    /// bypass the sealed-superclass check. This allows stdlib classes like Character
    /// (which extends sealed Integer) to load correctly via their `on_load` hooks.
    stdlib_mode: bool,
    /// ADR 0050 Phase 4: pre-loaded class entries from BEAM metadata.
    /// Injected into the `ClassHierarchy` before codegen so user-defined REPL
    /// classes are visible to `is_actor_class` and related checks.
    pre_class_hierarchy: Vec<crate::semantic_analysis::class_hierarchy::ClassInfo>,
    /// BT-1343: Override for codegen diagnostics flag.
    /// `None` = read from `BEAMTALK_CODEGEN_DIAGNOSTICS` env var at generator creation.
    /// `Some(true/false)` = override the env var (used by tests).
    codegen_diagnostics: Option<bool>,
    /// ADR 0098 Phase 3: producing `BEAMTALK_VERSION` to bake into `__beamtalk_meta`.
    /// Set by the CLI via [`CodegenOptions::with_provenance`]; absent for REPL/tests.
    beamtalk_version: Option<String>,
    /// ADR 0098 Phase 3: producing compound OTP version (`<release>-<erts>`) to
    /// bake into `__beamtalk_meta`. Set alongside `beamtalk_version`.
    otp_release: Option<String>,
}

impl CodegenOptions {
    /// Creates default options with the given module name.
    pub fn new(module_name: &str) -> Self {
        Self {
            module_name: EcoString::from(module_name),
            source_text: None,
            bindings: None,
            workspace_mode: false,
            class_module_index: std::collections::HashMap::new(),
            class_superclass_index: std::collections::HashMap::new(),
            source_path: None,
            stdlib_mode: false,
            pre_class_hierarchy: Vec::new(),
            codegen_diagnostics: None,
            beamtalk_version: None,
            otp_release: None,
        }
    }

    /// ADR 0098 Phase 3: set the producing-toolchain identity baked into each
    /// module's `__beamtalk_meta/0` map. `beamtalk_version` is the full
    /// `BEAMTALK_VERSION`; `otp_release` is the compound `<release>-<erts>` key
    /// (the same the build stamp uses), `None` when OTP could not be probed.
    #[must_use]
    pub fn with_provenance(mut self, beamtalk_version: &str, otp_release: Option<&str>) -> Self {
        self.beamtalk_version = Some(beamtalk_version.to_string());
        self.otp_release = otp_release.map(String::from);
        self
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

    /// BT-1343: Explicitly enable or disable codegen diagnostics, overriding the env var.
    #[must_use]
    pub fn with_codegen_diagnostics(mut self, enabled: bool) -> Self {
        self.codegen_diagnostics = Some(enabled);
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

    /// ADR 0050 Phase 4: pre-load user-class entries from BEAM metadata into
    /// the `CodegenOptions` so `generate_module` injects them into the hierarchy.
    #[must_use]
    pub fn with_class_hierarchy(
        mut self,
        classes: Vec<crate::semantic_analysis::class_hierarchy::ClassInfo>,
    ) -> Self {
        self.pre_class_hierarchy = classes;
        self
    }

    /// Sets the source file path from an optional value (BT-845/BT-860).
    #[must_use]
    pub fn with_source_path_opt(mut self, path: Option<&str>) -> Self {
        self.source_path = path.map(String::from);
        self
    }

    /// Enables stdlib mode (BT-791): generated `register_class/0` emits `stdlibMode => true`
    /// so the runtime bypasses the sealed-superclass check for stdlib loading.
    #[must_use]
    pub fn with_stdlib_mode(mut self, enabled: bool) -> Self {
        self.stdlib_mode = enabled;
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
    generator.set_workspace_mode(options.workspace_mode);
    generator.set_stdlib_mode(options.stdlib_mode);
    generator.set_class_module_index(options.class_module_index);
    generator.source_path = options.source_path;
    // ADR 0098 Phase 3: bake the producing-toolchain identity into `__beamtalk_meta`.
    generator.beamtalk_version = options.beamtalk_version.map(EcoString::from);
    generator.otp_release = options.otp_release.map(EcoString::from);
    // BT-1343: Override codegen diagnostics flag if explicitly set in options.
    if let Some(enabled) = options.codegen_diagnostics {
        generator.codegen_diagnostics_enabled = enabled;
    }

    // BT-1288: Compute semantic facts before codegen begins.
    let semantic_facts = crate::semantic_analysis::compute_semantic_facts(module);
    generator.semantic_facts = semantic_facts;

    // Build hierarchy once for the entire generation (ADR 0006)
    let (hierarchy_result, _) =
        crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(module);
    let mut hierarchy =
        hierarchy_result.map_err(|e| CodeGenError::Internal(format!("hierarchy: {e:?}")))?;

    // ADR 0050 Phase 4: inject richer user-class entries from BEAM metadata first,
    // so that add_external_superclasses (which uses contains_key before inserting)
    // does not overwrite BEAM data with partial stubs.
    hierarchy.add_from_beam_meta(options.pre_class_hierarchy);

    // BT-894: Backfill missing cross-file superclass stubs (only for classes not
    // already present from build() or BEAM metadata).
    hierarchy.add_external_superclasses(&options.class_superclass_index);

    // BT-1005: Writeback inferred return types into the AST before codegen so
    // that unannotated methods appear in the emitted `method_return_types` map.
    // BT-1218: Also writeback supervisor_kind for Supervisor/DynamicSupervisor subclasses.
    // We clone to avoid mutating the caller's Module.
    let mut module_with_writeback = module.clone();
    crate::semantic_analysis::apply_return_type_writeback(&mut module_with_writeback, &hierarchy);
    crate::semantic_analysis::apply_supervisor_kind_writeback(
        &mut module_with_writeback,
        &hierarchy,
    );
    // BT-1534: Correct class_kind for indirect Value/Actor subclasses.
    // E.g. `TestCase subclass: MyTest` gets ClassKind::Object from the parser
    // (TestCase is not literally "Value"/"Actor"), but needs ClassKind::Value
    // so codegen generates auto-slot methods (withX: setters).
    crate::semantic_analysis::apply_class_kind_writeback(&mut module_with_writeback, &hierarchy);
    let module = &module_with_writeback;

    // ADR 0065 / BT-1457: Set Server subclass flag for handle_info codegen dispatch.
    if let Some(class) = module.classes.first() {
        generator.is_server_subclass = hierarchy.is_server_subclass(&class.name.name);
    }

    // BT-1639: Pre-compute direct-call eligible class methods from the hierarchy.
    // For sealed classes with no class variables, their class methods can be called
    // directly (bypassing gen_server dispatch). This is safe because the methods
    // are pure functions that don't mutate class state.
    generator.direct_call_eligible =
        CoreErlangGenerator::compute_direct_call_eligible(&hierarchy, &generator);

    // BT-1951: Stash the hierarchy for use by actor callback generation
    // (auto-chained initialize dispatch in handle_continue and inherited
    // typed-no-default field validation).
    generator.class_hierarchy = Some(hierarchy.clone());

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

// ── REPL / test expression generation (BT-1462) ────────────────────────
//
// These functions delegate to `crate::repl::codegen`, which owns the
// REPL-specific module assembly logic. They are kept here as thin
// re-exports for backward compatibility with existing callers.

/// Generates Core Erlang for a REPL expression.
///
/// Delegates to [`crate::repl::codegen::generate_repl_expression`].
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_repl_expression(expression: &Expression, module_name: &str) -> Result<String> {
    crate::repl::codegen::generate_repl_expression(expression, module_name)
}

/// Generates Core Erlang for multiple REPL expressions (BT-780).
///
/// Delegates to [`crate::repl::codegen::generate_repl_expressions`].
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_repl_expressions(expressions: &[Expression], module_name: &str) -> Result<String> {
    crate::repl::codegen::generate_repl_expressions(expressions, module_name)
}

/// Generates Core Erlang for multiple REPL expressions with a class module index.
///
/// Delegates to [`crate::repl::codegen::generate_repl_expressions_with_index`].
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
#[allow(clippy::implicit_hasher)]
pub fn generate_repl_expressions_with_index(
    expressions: &[Expression],
    module_name: &str,
    class_module_index: std::collections::HashMap<String, String>,
) -> Result<String> {
    crate::repl::codegen::generate_repl_expressions_with_index(
        expressions,
        module_name,
        class_module_index,
    )
}

/// Generates Core Erlang for trace mode eval (BT-1238).
///
/// Delegates to [`crate::repl::codegen::generate_repl_expressions_traced`].
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
#[allow(clippy::implicit_hasher)]
pub fn generate_repl_expressions_traced(
    expressions: &[Expression],
    source: &str,
    module_name: &str,
    class_module_index: std::collections::HashMap<String, String>,
) -> Result<String> {
    crate::repl::codegen::generate_repl_expressions_traced(
        expressions,
        source,
        module_name,
        class_module_index,
    )
}

/// Generates Core Erlang for a test expression (no workspace bindings).
///
/// Delegates to [`crate::repl::codegen::generate_test_expression`].
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_test_expression(expression: &Expression, module_name: &str) -> Result<String> {
    crate::repl::codegen::generate_test_expression(expression, module_name)
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
pub(crate) enum CodeGenContext {
    /// Generating code for an actor class (`gen_server` with async messaging).
    ///
    /// - Field access: `call 'maps':'get'('field', State)`
    /// - Method calls: Sync via `beamtalk_actor:sync_send` (ADR-0043)
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

/// Fresh temporary variable names shared by all three NLR try/catch wrappers.
///
/// Allocated by [`CoreErlangGenerator::alloc_nlr_catch_vars`] and consumed by the
/// single boundary-parameterised wrapper [`CoreErlangGenerator::wrap_body_with_nlr_catch`]
/// (via [`CoreErlangGenerator::wrap_actor_body_with_nlr_catch`] and
/// [`CoreErlangGenerator::wrap_class_method_body_with_nlr_catch`]) and by
/// [`CoreErlangGenerator::wrap_value_type_body_with_nlr_catch`].
#[allow(clippy::struct_field_names)]
struct NlrCatchVars {
    result_var: String,
    cls_var: String,
    err_var: String,
    stk_var: String,
    ctk_var: String,
    val_var: String,
    /// BT-854: State variable captured from the 4-tuple NLR throw.
    state_var: String,
    ot_pair_var: String,
}

/// BT-2361: The per-context NLR boundary — the *only* thing that differs between the
/// Actor, class-method and value-type non-local-return catch wrappers once the catch
/// vars are shared.
///
/// All three contexts catch the same 4-tuple throw `{'$bt_nlr', Token, Value, State}`
/// (ADR 0041's state-carrying NLR convention); they disagree only about the Document
/// the matching catch arm yields. This enum captures that single axis so the catch
/// scaffolding can be written once (see [`nlr_arm_result`]) instead of being
/// copy-evolved per context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NlrBoundary {
    /// Actor (`gen_server`) methods: the catch arm yields `{'reply', Value, State}`.
    ActorReply,
    /// Class methods: the catch arm yields `Value` (no class vars) or
    /// `{'class_var_result', Value, State}` when class vars were mutated.
    ClassMethod { has_class_vars: bool },
    /// Value-type methods: the catch arm yields `{Value, State}` so the normal and
    /// NLR-catch paths produce the same `{Result, Self{N}}` shape.
    ValueType,
}

/// BT-2361: Builds the Document the matching NLR catch arm yields for `boundary`.
///
/// This is the single place the per-context divergence between the three former
/// `wrap_*_body_with_nlr_catch` wrappers lives. `val_var`/`state_var` are the
/// catch-bound `Value`/`State` extracted from the 4-tuple throw. Shared by the
/// gen-server wrapper ([`CoreErlangGenerator::wrap_body_with_nlr_catch`]) and the
/// value-type suffix ([`NlrValueTypeCatchVars::format_catch_suffix`]).
///
/// BT-875: Use Document/docvec! — never format!() for Core Erlang fragments.
fn nlr_arm_result(val_var: &str, state_var: &str, boundary: NlrBoundary) -> Document<'static> {
    match boundary {
        NlrBoundary::ActorReply => docvec![
            "{'reply', ",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(state_var.to_string()),
            "}",
        ],
        NlrBoundary::ClassMethod {
            has_class_vars: true,
        } => docvec![
            "{'class_var_result', ",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(state_var.to_string()),
            "}",
        ],
        NlrBoundary::ClassMethod {
            has_class_vars: false,
        } => leaf::var(val_var.to_string()),
        NlrBoundary::ValueType => docvec![
            "{",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(state_var.to_string()),
            "}",
        ],
    }
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
            leaf::var(self.token_var.clone()),
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
            leaf::var(self.result_var.clone()),
            " -> ",
            leaf::var(self.result_var.clone()),
            nest(INDENT, line()),
            "catch <",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            ", ",
            leaf::var(self.stk_var.clone()),
            "> ->",
            nest(INDENT + 2, line()),
            "case {",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            "} of",
            nest(INDENT + 4, line()),
            "<{'throw', {'$bt_nlr', ",
            leaf::var(self.ctk_var.clone()),
            ", ",
            leaf::var(self.val_var.clone()),
            ", ",
            leaf::var(self.state_var.clone()),
            "}}> ",
            "when call 'erlang':'=:='(",
            leaf::var(self.ctk_var.clone()),
            ", ",
            leaf::var(self.token_var.clone()),
            ") -> ",
            // BT-2361: shared catch-arm builder — value-type yields `{Value, State}`.
            nlr_arm_result(&self.val_var, &self.state_var, NlrBoundary::ValueType),
            nest(INDENT + 4, line()),
            "<",
            leaf::var(self.ot_pair_var.clone()),
            "> when 'true' -> ",
            "primop 'raw_raise'(",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            ", ",
            leaf::var(self.stk_var.clone()),
            ")",
            nest(INDENT + 2, line()),
            "end",
            line(),
        ]
    }
}

/// BT-1461: REPL-specific codegen state.
///
/// Groups fields that are only relevant when generating REPL evaluation code.
/// Wrapped as `Option<ReplContext>` on the generator — `Some` when in REPL mode,
/// `None` during batch compilation. Accessor methods on `CoreErlangGenerator`
/// provide defaults when the context is absent.
#[derive(Debug, Clone)]
pub(crate) struct ReplContext {
    /// BT-153: Whether we're generating REPL code (vs module code).
    /// In REPL mode, local variable assignments should update bindings.
    pub is_repl_mode: bool,
    /// BT-245/BT-1448: Internal flag for REPL mutation-threaded expressions.
    ///
    /// Set deep inside `generate_expression` when mutation-threaded control flow
    /// (loops, conditionals, exception handlers, inline value calls) produces a
    /// `{Result, State}` tuple that the REPL must unpack.
    ///
    /// External callers should use `expression_doc_with_repl_mutation_tracking()`
    /// instead of reading this field directly.
    pub repl_loop_mutated: bool,
    /// BT-374 / ADR 0010 / ADR 0019: Whether workspace bindings are available.
    /// When true (REPL/workspace context), class references resolve through
    /// session bindings or class registry. When false (batch compile),
    /// class references go directly to the class registry.
    pub workspace_mode: bool,
}

impl ReplContext {
    /// Creates a new `ReplContext` with default values.
    pub(crate) fn new() -> Self {
        Self {
            is_repl_mode: false,
            repl_loop_mutated: false,
            workspace_mode: false,
        }
    }
}

/// BT-1639: Information about a sealed class eligible for direct-call optimization.
///
/// A class is eligible only if it is sealed and declares **no** class variables.
/// In that case, its `class sealed` methods can be called directly (bypassing
/// `gen_server` dispatch) since they don't mutate class state. This avoids the
/// ~5-10us `gen_server` round-trip overhead for utility-style class methods
/// (e.g., `File exists:`, `Json parse:`). The implementation does not inspect
/// individual method bodies for class-variable access; any presence of class
/// variables on the class makes the entire class ineligible.
#[derive(Debug, Clone)]
pub(super) struct DirectCallClassInfo {
    /// The compiled Erlang module name (e.g., `bt@stdlib@tracing`).
    pub module_name: EcoString,
    /// Set of selector names eligible for direct call (e.g., `{"setContext:", "context", ...}`).
    /// Excludes `startLink`-family selectors and non-sealed class methods.
    pub selectors: std::collections::HashSet<String>,
}

/// BT-1461: Class/actor-specific codegen state.
///
/// Groups fields that are only relevant when compiling a class definition
/// (actor or value type). Wrapped as `Option<ClassContext>` on the generator —
/// `Some` when a class is being compiled, `None` for standalone REPL expressions.
#[derive(Debug, Clone)]
pub(super) struct ClassContext {
    /// Identity of the class currently being compiled (if any).
    /// Set from the AST class definition at the start of module generation.
    class_identity: Option<util::ClassIdentity>,
    /// BT-412: Names of class variables in the current class.
    /// Used to distinguish class variable access from instance field access in class methods.
    pub class_var_names: std::collections::HashSet<String>,
    /// BT-412: Selector names of class methods in the current class.
    /// Used to route self-sends to class method functions vs module exports.
    pub class_method_selectors: std::collections::HashSet<String>,
    /// BT-412: State version counter for class variable threading.
    pub class_var_version: usize,
    /// BT-412: Whether class variables were mutated in the current method.
    pub class_var_mutated: bool,
    /// Class name → compiled module name index for resolving cross-file class references.
    ///
    /// Populated from `CodegenOptions::class_module_index` before generation begins.
    /// Used by `compiled_module_name` to resolve subdirectory classes correctly.
    pub class_module_index: std::collections::HashMap<String, String>,
    /// BT-403: Selectors of sealed methods in the current class.
    /// Used to generate standalone functions and direct call dispatch.
    pub sealed_method_selectors: std::collections::HashSet<String>,
    /// BT-996: Auto-generated keyword constructor selector for Value subclass: classes.
    /// E.g. `"symName:"` for a single-slot class, `"x:y:"` for two slots.
    /// Set during class method codegen to route `ClassName slot: value` to the correct
    /// class-side constructor instead of the instance-side getter.
    pub class_slot_constructor_selector: Option<String>,
    /// BT-426: Whether we're currently generating a class-side method body.
    /// When true, field access/assignment should produce a compile error.
    pub in_class_method: bool,
    /// BT-791: Whether this module is being compiled in stdlib mode.
    /// When true, `generate_register_class` emits `stdlibMode => true` in the builder
    /// state so the runtime can bypass the sealed-superclass check for stdlib loading.
    pub stdlib_mode: bool,
    /// ADR 0084 / BT-2267: When `Some(ClassName)`, we are lowering a programmatic
    /// `ClassBuilder` class-method block into an anonymous fun. Such a fun has no
    /// `class_<sel>` module export, so self-sends and `super` route through the
    /// runtime dispatch helpers (`class_self_dispatch_local`/`class_self_dispatch`)
    /// keyed on this class name, not through a direct module call.
    pub builder_class_method_class: Option<String>,
}

/// ADR 0084 / BT-2267: Snapshot of the class-method-relevant `ClassContext`
/// fields, captured when entering a programmatic `ClassBuilder` class-method
/// lowering and restored on exit. `had_context` records whether a `ClassContext`
/// existed beforehand, so a context created solely for a standalone builder
/// cascade is dropped rather than leaked.
#[derive(Debug)]
pub(super) struct SavedClassMethodCtx {
    had_context: bool,
    in_class_method: bool,
    class_var_names: std::collections::HashSet<String>,
    class_method_selectors: std::collections::HashSet<String>,
    class_var_version: usize,
    class_var_mutated: bool,
    class_slot_constructor_selector: Option<String>,
    builder_class_method_class: Option<String>,
}

impl ClassContext {
    /// Creates a new `ClassContext` with default values.
    fn new() -> Self {
        Self {
            class_identity: None,
            class_var_names: std::collections::HashSet::new(),
            class_method_selectors: std::collections::HashSet::new(),
            class_var_version: 0,
            class_var_mutated: false,
            class_module_index: std::collections::HashMap::new(),
            sealed_method_selectors: std::collections::HashSet::new(),
            class_slot_constructor_selector: None,
            in_class_method: false,
            stdlib_mode: false,
            builder_class_method_class: None,
        }
    }
}

/// BT-1461: Value-type-specific codegen state.
///
/// Groups fields that are only relevant when compiling value type (non-actor)
/// class methods. Wrapped as `Option<ValueTypeContext>` on the generator —
/// `Some` when compiling value type code, `None` otherwise.
#[derive(Debug, Clone)]
pub(super) struct ValueTypeContext {
    /// BT-833: Self-threading version counter for value type field assignments.
    ///
    /// Mirrors `state_threading` for value types. Each field assignment increments
    /// this counter: `Self` → `Self1` → `Self2` → ... so that `self` in expression
    /// position always resolves to the latest immutable snapshot.
    pub self_version: usize,
    /// BT-754: Core Erlang variable name holding the non-local return token for the current
    /// value type method, or `None` when no NLR infrastructure is active.
    ///
    /// Set by `generate_value_type_method` when the method body contains blocks with `^`.
    /// When set, `generate_expression` for `Expression::Return` generates a throw instead
    /// of a plain value, causing the return to escape from the enclosing block closure.
    pub current_nlr_token: Option<String>,
}

impl ValueTypeContext {
    /// Creates a new `ValueTypeContext` with default values.
    fn new() -> Self {
        Self {
            self_version: 0,
            current_nlr_token: None,
        }
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
///
/// # Context Structs (BT-1461)
///
/// Fields are organized into context-specific groups to reduce the cognitive
/// load of the god object:
/// - [`ReplContext`] — REPL-specific state (`is_repl_mode`, `workspace_mode`, etc.)
/// - [`ClassContext`] — Class/actor-specific state (`class_identity`, `class_var_*`, etc.)
/// - [`ValueTypeContext`] — Value-type-specific state (`self_version`, `current_nlr_token`)
///
/// Each context is `Option<T>` on the generator, set only when relevant.
/// Accessor methods provide safe defaults when the context is absent.
#[expect(
    clippy::struct_excessive_bools,
    reason = "Generator flags are context switches, not configuration"
)]
pub(crate) struct CoreErlangGenerator {
    /// The module name being generated (ref-counted for O(1) clone).
    pub(crate) module_name: EcoString,
    /// Variable binding and scope management.
    var_context: VariableContext,
    /// State threading for field assignments.
    state_threading: StateThreading,
    /// BT-153: Whether we're inside a loop body (use `StateAcc` instead of `State`)
    in_loop_body: bool,
    /// BT-1326: Whether we're inside a hybrid-params loop body.
    ///
    /// When `true`, `current_state_var()` and `next_state_var()` use `State*` naming
    /// instead of `StateAcc*`, even when `in_loop_body` is also true.
    /// Set by `generate_counted_stateful_loop_hybrid` and `generate_while_loop_hybrid`.
    in_hybrid_loop: bool,
    /// BT-1329: When `true`, the generator is inside a direct-params (or hybrid) counted
    /// loop body. List ops that thread captured outer-scope locals should skip the
    /// `append_repack_stateacc_doc` step and return just the result value (not
    /// `{Result, StateAcc}`), since there is no `StateAcc` variable in scope.
    in_direct_params_loop: bool,
    /// BT-1329: When a list op in direct-params mode generates an open let-chain
    /// (omitting the trailing result expression), it stores the result variable name
    /// here so the caller can append `let AssignedVar = <result_var> in` separately.
    /// `None` when no list op result is pending.
    direct_params_list_op_result: Option<String>,
    /// BT-1326: Map of actor field name → Core Erlang variable name for fields
    /// that have been pre-extracted before a hybrid/full-extract letrec loop.
    ///
    /// When non-empty, `generate_field_access` substitutes the variable name directly
    /// instead of emitting `call 'maps':'get'('field', State)`, eliminating per-iteration
    /// map reads for fields during the loop body.
    /// Contains both read-only fields (BT-1326) and mutated fields (BT-1342).
    /// Cleared after the loop body is generated.
    hybrid_readonly_field_params: std::collections::HashMap<String, String>,
    /// BT-1342: Set of actor field names that are mutated inside the current
    /// full-extract loop body. When a field write targets one of these fields,
    /// `generate_field_assignment_open` emits a simple variable rebinding instead
    /// of `maps:put` on State, and updates `hybrid_readonly_field_params` with the
    /// new variable name so subsequent reads see the updated value.
    /// Empty when not in full-extract mode.
    hybrid_mutated_fields: std::collections::HashSet<String>,
    /// BT-213: Code generation context (`Actor`, `ValueType`, or `Repl`).
    /// Determines variable naming and method dispatch strategy.
    pub(crate) context: CodeGenContext,
    /// BT-1475: Nesting depth of block (closure) bodies.
    /// When > 0, self-cast sends in Actor context must route through the
    /// actor mailbox (`beamtalk_message_dispatch:cast/3`) instead of calling
    /// `safe_dispatch` directly, because the block may execute in a different
    /// process (e.g. Timer callback, cross-actor callback).
    block_depth: usize,
    /// BT-101: Original source text for extracting method source.
    source_text: Option<String>,
    /// BT-295: Primitive binding table from compiled stdlib (ADR 0007).
    /// Used by `generate_primitive()` for method body compilation via static methods.
    #[allow(dead_code)] // stored for future call-site optimization with static typing
    primitive_bindings: PrimitiveBindingTable,
    /// BT-295: Parameters of the current method being compiled (if any).
    /// Used by `Expression::Primitive` to generate dispatch argument lists.
    current_method_params: Vec<String>,
    /// BT-2709: Declared types of the current method's parameters, keyed by
    /// **source** parameter name → simple type name (e.g. `"other" -> "Number"`).
    /// Used by the arithmetic fast-path classifier
    /// (`receiver_is_statically_numeric`) to drop the runtime `is_number` guard
    /// when a receiver is a `:: Integer/Float/Number`-annotated parameter. Only
    /// `Simple` annotations are recorded; absence falls back to the guard, which
    /// is always correct. Cleared at every method-body entry so a prior method's
    /// annotations never leak into the next.
    current_method_param_types: std::collections::HashMap<String, String>,
    /// BT-412/BT-1448: Internal side-channel for open-scope expression results.
    ///
    /// Set deep inside `generate_expression` when a class var assignment, class method
    /// self-send, or direct-params list-op produces an open let-chain. Read by:
    /// - `expression_doc_with_open_scope()` — the public API for callers to detect open scopes
    /// - The annotation guard in `generate_expression` — to skip line annotations on open chains
    ///
    /// External callers should use `expression_doc_with_open_scope()` instead of reading
    /// this field directly. Functions that produce their own open let-chains
    /// (`generate_field_assignment_open`, `generate_self_field_at_put_open`,
    /// `generate_local_var_assignment_in_loop`) return the result variable explicitly.
    last_open_scope_result: Option<String>,
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
    pub(crate) codegen_warnings: Vec<Diagnostic>,
    /// BT-1288: Pre-computed semantic facts from the pre-codegen analysis pass.
    /// Used for block profile lookups and dispatch classification.
    pub(super) semantic_facts: crate::semantic_analysis::SemanticFacts,
    /// BT-1343: Whether codegen diagnostics are enabled (`BEAMTALK_CODEGEN_DIAGNOSTICS=1`).
    /// When true, emits `Diagnostic::hint` for calling convention choices, dynamic dispatch
    /// fallbacks, non-local returns, and other codegen decisions.
    codegen_diagnostics_enabled: bool,
    /// BT-1343: Whether `StateAcc` fallback should be promoted to warning (`BEAMTALK_WARN_STATEACC=1`).
    warn_stateacc: bool,
    /// BT-1435: Selector name of the method currently being compiled.
    /// Used by Logger intrinsics to inject `beamtalk_selector` metadata.
    current_method_selector: Option<String>,
    /// ADR 0065 / BT-1457: Whether the current class is a Server subclass.
    /// When true, `generate_handle_info` dispatches to `handleInfo:` with
    /// log-and-continue error semantics instead of the default ignore-all stub.
    is_server_subclass: bool,
    /// BT-1639: Pre-computed direct-call eligible class methods.
    ///
    /// Maps class name → `DirectCallClassInfo` for sealed classes whose class methods
    /// can be called directly (without `gen_server` dispatch). Computed from the class
    /// hierarchy in `generate_module_with_warnings`.
    direct_call_eligible: std::collections::HashMap<String, DirectCallClassInfo>,
    /// BT-1461: REPL-specific codegen state. `Some` when in REPL mode.
    repl_context: Option<ReplContext>,
    /// BT-1461: Class/actor-specific codegen state. `Some` when compiling a class.
    class_context: Option<ClassContext>,
    /// BT-1461: Value-type-specific codegen state. `Some` when compiling value types.
    value_type_context: Option<ValueTypeContext>,
    /// BT-1951: Snapshot of the class hierarchy for this generation (ADR 0078).
    ///
    /// Populated by `generate_module_with_warnings` before codegen begins. Used by
    /// actor `handle_continue` generation to walk the superclass chain and emit
    /// parent-first `initialize` dispatches, and by the post-initialize validation
    /// check to collect inherited typed-no-default fields.
    pub(super) class_hierarchy: Option<crate::semantic_analysis::class_hierarchy::ClassHierarchy>,
    /// ADR 0098 Phase 3: producing `BEAMTALK_VERSION`, baked into `__beamtalk_meta`.
    /// Supplied by the CLI; `None` for REPL/test codegen (key omitted).
    beamtalk_version: Option<EcoString>,
    /// ADR 0098 Phase 3: producing compound OTP version (`<release>-<erts>`),
    /// baked into `__beamtalk_meta`. Supplied by the CLI; `None` omits the key.
    otp_release: Option<EcoString>,
}

impl CoreErlangGenerator {
    /// Creates a new code generator for the given module name.
    pub(crate) fn new(module_name: &str) -> Self {
        Self {
            module_name: EcoString::from(module_name),
            var_context: VariableContext::new(),
            state_threading: StateThreading::new(),
            in_loop_body: false,
            in_hybrid_loop: false,
            in_direct_params_loop: false,
            direct_params_list_op_result: None,
            hybrid_readonly_field_params: std::collections::HashMap::new(),
            hybrid_mutated_fields: std::collections::HashSet::new(),
            context: CodeGenContext::Actor, // Default to Actor for backward compatibility
            block_depth: 0,
            source_text: None,
            primitive_bindings: PrimitiveBindingTable::new(),
            current_method_params: Vec::new(),
            current_method_param_types: std::collections::HashMap::new(),
            last_open_scope_result: None,
            source_path: None,
            tier2_block_params: std::collections::HashSet::new(),
            tier2_method_info: std::collections::HashMap::new(),
            codegen_warnings: Vec::new(),
            semantic_facts: crate::semantic_analysis::SemanticFacts::default(),
            codegen_diagnostics_enabled: std::env::var("BEAMTALK_CODEGEN_DIAGNOSTICS")
                .is_ok_and(|v| v == "1"),
            warn_stateacc: std::env::var("BEAMTALK_WARN_STATEACC").is_ok_and(|v| v == "1"),
            current_method_selector: None,
            is_server_subclass: false,
            direct_call_eligible: std::collections::HashMap::new(),
            repl_context: Some(ReplContext::new()),
            class_context: Some(ClassContext::new()),
            value_type_context: Some(ValueTypeContext::new()),
            class_hierarchy: None,
            beamtalk_version: None,
            otp_release: None,
        }
    }

    /// ADR 0098 Phase 3: the producing-toolchain identity to bake into
    /// `__beamtalk_meta`. Borrows the generator's version fields; both are `None`
    /// unless the CLI supplied them via [`CodegenOptions::with_provenance`].
    pub(super) fn meta_provenance(&self) -> gen_server::MetaProvenance<'_> {
        gen_server::MetaProvenance {
            beamtalk_version: self.beamtalk_version.as_deref(),
            otp_release: self.otp_release.as_deref(),
        }
    }

    // ── BT-1461: Context accessor methods ──────────────────────────────
    //
    // These methods provide convenient access to context-specific fields,
    // returning safe defaults when the context is absent.

    /// Returns `true` if REPL mode is active.
    pub(crate) fn is_repl_mode(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.is_repl_mode)
    }

    /// Sets the REPL mode flag, initialising the context if absent.
    pub(crate) fn set_is_repl_mode(&mut self, value: bool) {
        self.repl_context_mut().is_repl_mode = value;
    }

    /// Returns `true` if REPL loop mutation tracking has been flagged.
    pub(super) fn repl_loop_mutated(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.repl_loop_mutated)
    }

    /// Sets the REPL loop mutated flag, initialising the context if absent.
    pub(super) fn set_repl_loop_mutated(&mut self, value: bool) {
        self.repl_context_mut().repl_loop_mutated = value;
    }

    /// Returns `true` if workspace mode is active.
    pub(crate) fn workspace_mode(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.workspace_mode)
    }

    /// Sets workspace mode, initialising the context if absent.
    pub(crate) fn set_workspace_mode(&mut self, value: bool) {
        self.repl_context_mut().workspace_mode = value;
    }

    /// Returns a mutable reference to the REPL context, creating it if absent.
    fn repl_context_mut(&mut self) -> &mut ReplContext {
        self.repl_context.get_or_insert_with(ReplContext::new)
    }

    /// Returns a reference to the class identity, if any.
    pub(in crate::codegen::core_erlang) fn class_identity(&self) -> Option<&util::ClassIdentity> {
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.class_identity.as_ref())
    }

    /// Sets the class identity, initialising the context if absent.
    pub(in crate::codegen::core_erlang) fn set_class_identity(
        &mut self,
        identity: Option<util::ClassIdentity>,
    ) {
        self.class_context_mut().class_identity = identity;
    }

    /// Returns a reference to the class variable names set.
    pub(super) fn class_var_names(&self) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_var_names)
    }

    /// Returns a mutable reference to the class variable names set.
    pub(super) fn class_var_names_mut(&mut self) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().class_var_names
    }

    /// Returns a reference to the class method selectors set.
    pub(super) fn class_method_selectors(&self) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_method_selectors)
    }

    /// Returns a mutable reference to the class method selectors set.
    pub(super) fn class_method_selectors_mut(&mut self) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().class_method_selectors
    }

    /// Returns the class variable version counter.
    pub(super) fn class_var_version(&self) -> usize {
        self.class_context
            .as_ref()
            .map_or(0, |ctx| ctx.class_var_version)
    }

    /// Sets the class variable version counter.
    pub(super) fn set_class_var_version(&mut self, version: usize) {
        self.class_context_mut().class_var_version = version;
    }

    /// Returns whether class variables were mutated in the current method.
    pub(super) fn class_var_mutated(&self) -> bool {
        self.class_context
            .as_ref()
            .is_some_and(|ctx| ctx.class_var_mutated)
    }

    /// Sets the class variable mutated flag.
    pub(super) fn set_class_var_mutated(&mut self, value: bool) {
        self.class_context_mut().class_var_mutated = value;
    }

    /// Returns a reference to the class module index.
    pub(super) fn class_module_index(&self) -> &std::collections::HashMap<String, String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashMap<String, String>> =
            std::sync::LazyLock::new(std::collections::HashMap::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_module_index)
    }

    /// Sets the class module index, initialising the context if absent.
    pub(crate) fn set_class_module_index(
        &mut self,
        index: std::collections::HashMap<String, String>,
    ) {
        self.class_context_mut().class_module_index = index;
    }

    /// Returns a reference to the sealed method selectors set.
    pub(super) fn sealed_method_selectors(&self) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.sealed_method_selectors)
    }

    /// Returns a mutable reference to the sealed method selectors set.
    pub(super) fn sealed_method_selectors_mut(&mut self) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().sealed_method_selectors
    }

    /// Returns the class slot constructor selector, if any.
    pub(super) fn class_slot_constructor_selector(&self) -> Option<&String> {
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.class_slot_constructor_selector.as_ref())
    }

    /// Sets the class slot constructor selector.
    pub(super) fn set_class_slot_constructor_selector(&mut self, sel: Option<String>) {
        self.class_context_mut().class_slot_constructor_selector = sel;
    }

    /// Returns whether we're in a class method body.
    pub(super) fn in_class_method(&self) -> bool {
        self.class_context
            .as_ref()
            .is_some_and(|ctx| ctx.in_class_method)
    }

    /// Sets the in-class-method flag.
    pub(super) fn set_in_class_method(&mut self, value: bool) {
        self.class_context_mut().in_class_method = value;
    }

    /// ADR 0084 / BT-2267: the builder class name when lowering a programmatic
    /// `ClassBuilder` class-method block into an anonymous fun, else `None`.
    pub(super) fn builder_class_method_class(&self) -> Option<String> {
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.builder_class_method_class.clone())
    }

    /// Sets (or clears) the builder class-method class name.
    pub(super) fn set_builder_class_method_class(&mut self, value: Option<String>) {
        self.class_context_mut().builder_class_method_class = value;
    }

    /// ADR 0084 / BT-2267: Enter the class-method lowering context for a
    /// programmatic `ClassBuilder` cascade's `classMethods:` funs, returning the
    /// prior state to restore. Sets `in_class_method`, the class-variable names
    /// (from the cascade's `classVars:` keys), and the builder class name used
    /// for runtime self/`super` dispatch. Safe whether or not an enclosing class
    /// is being compiled — a context created here is dropped on exit.
    pub(super) fn enter_builder_class_method_context(
        &mut self,
        class_name: &str,
        class_var_names: &[String],
    ) -> SavedClassMethodCtx {
        let saved = SavedClassMethodCtx {
            had_context: self.class_context.is_some(),
            in_class_method: self.in_class_method(),
            class_var_names: self.class_var_names().clone(),
            class_method_selectors: self.class_method_selectors().clone(),
            class_var_version: self.class_var_version(),
            class_var_mutated: self.class_var_mutated(),
            class_slot_constructor_selector: self.class_slot_constructor_selector().cloned(),
            builder_class_method_class: self.builder_class_method_class(),
        };
        self.set_in_class_method(true);
        *self.class_var_names_mut() = class_var_names.iter().cloned().collect();
        // class_method_selectors is intentionally left empty: in builder mode
        // `generate_class_method_self_send` routes EVERY self-send through
        // `class_self_dispatch_local` (the fun has no `class_<sel>` export) before
        // it ever consults this set, so it is not needed for dispatch. Class-var
        // threading across such self-sends rides on the open scope that
        // `emit_class_var_result_unwrap` always produces, not on this set.
        self.class_method_selectors_mut().clear();
        self.set_class_var_version(0);
        self.set_class_var_mutated(false);
        self.set_class_slot_constructor_selector(None);
        self.set_builder_class_method_class(Some(class_name.to_string()));
        saved
    }

    /// Restore the class-method context saved by
    /// [`enter_builder_class_method_context`](Self::enter_builder_class_method_context).
    pub(super) fn exit_builder_class_method_context(&mut self, saved: SavedClassMethodCtx) {
        if saved.had_context {
            self.set_in_class_method(saved.in_class_method);
            *self.class_var_names_mut() = saved.class_var_names;
            *self.class_method_selectors_mut() = saved.class_method_selectors;
            self.set_class_var_version(saved.class_var_version);
            self.set_class_var_mutated(saved.class_var_mutated);
            self.set_class_slot_constructor_selector(saved.class_slot_constructor_selector);
            self.set_builder_class_method_class(saved.builder_class_method_class);
        } else {
            // No enclosing class context — drop the one we created so standalone
            // (REPL) builder cascades don't leak a class context.
            self.class_context = None;
        }
    }

    /// BT-2709: Clears per-method parameter-type tracking. Call alongside
    /// `current_method_params.clear()` at every method-body entry so a prior
    /// method's `:: Number` annotations never leak into the next and cause a
    /// spurious bare-BIF fast path.
    pub(super) fn clear_method_param_types(&mut self) {
        self.current_method_param_types.clear();
    }

    /// BT-2709: Records a method parameter's declared type for the arithmetic
    /// fast-path classifier (keyed by **source** name → simple type name).
    /// Only `Simple` annotations are recorded; anything else is left absent so
    /// the classifier falls back to the runtime `is_number` guard, which is
    /// always correct.
    pub(super) fn record_method_param_type(
        &mut self,
        source_name: &str,
        annotation: Option<&crate::ast::TypeAnnotation>,
    ) {
        if let Some(crate::ast::TypeAnnotation::Simple(id)) = annotation {
            self.current_method_param_types
                .insert(source_name.to_string(), id.name.to_string());
        }
    }

    /// BT-2709: Whether `name` refers to a `:: Integer/Float/Number`-annotated
    /// parameter of the current method.
    pub(super) fn param_is_numeric(&self, name: &str) -> bool {
        self.current_method_param_types
            .get(name)
            .is_some_and(|ty| matches!(ty.as_str(), "Integer" | "Float" | "Number"))
    }

    /// Returns whether stdlib mode is active.
    pub(super) fn stdlib_mode(&self) -> bool {
        self.class_context
            .as_ref()
            .is_some_and(|ctx| ctx.stdlib_mode)
    }

    /// Sets stdlib mode.
    pub(super) fn set_stdlib_mode(&mut self, value: bool) {
        self.class_context_mut().stdlib_mode = value;
    }

    /// Returns a mutable reference to the class context, creating it if absent.
    fn class_context_mut(&mut self) -> &mut ClassContext {
        self.class_context.get_or_insert_with(ClassContext::new)
    }

    /// Returns the value type self-version counter.
    pub(super) fn self_version(&self) -> usize {
        self.value_type_context
            .as_ref()
            .map_or(0, |ctx| ctx.self_version)
    }

    /// Sets the value type self-version counter.
    pub(super) fn set_self_version(&mut self, version: usize) {
        self.value_type_context_mut().self_version = version;
    }

    /// Returns the current NLR token variable name, if any.
    pub(super) fn current_nlr_token(&self) -> Option<&String> {
        self.value_type_context
            .as_ref()
            .and_then(|ctx| ctx.current_nlr_token.as_ref())
    }

    /// Sets the current NLR token variable name.
    pub(super) fn set_current_nlr_token(&mut self, token: Option<String>) {
        self.value_type_context_mut().current_nlr_token = token;
    }

    /// Returns a mutable reference to the value type context, creating it if absent.
    fn value_type_context_mut(&mut self) -> &mut ValueTypeContext {
        self.value_type_context
            .get_or_insert_with(ValueTypeContext::new)
    }

    /// Creates a new code generator with a primitive binding table.
    fn with_bindings(module_name: &str, bindings: PrimitiveBindingTable) -> Self {
        let mut generator = Self::new(module_name);
        generator.primitive_bindings = bindings;
        generator
    }

    /// Pushes a new scope for variable bindings.
    pub(crate) fn push_scope(&mut self) {
        self.var_context.push_scope();
    }

    /// Pops the current scope, discarding its bindings.
    pub(crate) fn pop_scope(&mut self) {
        self.var_context.pop_scope();
    }

    /// Looks up a variable binding in the current scope stack.
    fn lookup_var(&self, name: &str) -> Option<&String> {
        self.var_context.lookup(name)
    }

    /// Binds an identifier to a Core Erlang variable name in the current scope.
    pub(crate) fn bind_var(&mut self, name: &str, core_var: &str) {
        self.var_context.bind(name, core_var);
    }

    /// Returns the current state variable name for state threading.
    ///
    /// When inside a hybrid-params loop (`in_hybrid_loop = true`), returns `State` or `StateN`
    /// (same as normal context) so that field mutations thread through the explicit `State`
    /// parameter instead of a `StateAcc` map.
    ///
    /// When inside a normal loop body (`in_loop_body = true`), returns `StateAcc` or `StateAccN`.
    /// Otherwise returns `State` or `StateN`.
    pub(crate) fn current_state_var(&self) -> String {
        if self.in_hybrid_loop {
            // Hybrid-params loop: use State* naming (State is an explicit fun parameter)
            self.state_threading.current_var()
        } else if self.in_loop_body {
            // Normal loop body: use StateAcc* nomenclature
            util::versioned_var("StateAcc", self.state_threading.version())
        } else {
            // Normal context - use State nomenclature
            self.state_threading.current_var()
        }
    }

    /// Increments the state version and returns the new state variable name.
    ///
    /// When inside a hybrid-params loop (`in_hybrid_loop = true`) or normal context,
    /// returns `State1`, `State2`, etc.
    /// When inside a normal loop body (`in_loop_body = true`), returns `StateAcc1`, etc.
    pub(crate) fn next_state_var(&mut self) -> String {
        let next_var = self.state_threading.next_var();
        if self.in_hybrid_loop || !self.in_loop_body {
            // Hybrid mode or normal context: use State* naming
            next_var
        } else {
            // Normal loop body: replace "State" prefix with "StateAcc"
            if next_var == "State1" {
                // First increment in loop body
                "StateAcc1".to_string()
            } else if next_var.starts_with("State") {
                next_var.replace("State", "StateAcc")
            } else {
                next_var
            }
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

    /// Returns the name of the next state variable without advancing the
    /// version counter.  Context-aware: uses `StateAcc*` in loop bodies.
    pub(super) fn peek_next_state_var(&self) -> String {
        let next_var = self.state_threading.peek_next_var();
        if self.in_hybrid_loop || !self.in_loop_body {
            next_var
        } else {
            next_var.replace("State", "StateAcc")
        }
    }

    /// Sets the state version.
    pub(super) fn set_state_version(&mut self, version: usize) {
        self.state_threading.set_version(version);
    }

    /// BT-1449: Executes `f` inside a branch context where `in_loop_body` is
    /// `true` and `state_version` is reset to 0.  The previous values are
    /// unconditionally restored after `f` returns (even on `Err`).
    ///
    /// BT-1550: Also saves/restores `class_var_version` so that self-calls
    /// inside a conditional branch don't leak `ClassVars{N}` bindings into
    /// the outer scope.  `class_var_mutated` is intentionally NOT restored —
    /// it is a method-level flag that must stay sticky once set.
    pub(super) fn with_branch_context<T>(
        &mut self,
        f: impl FnOnce(&mut CoreErlangGenerator) -> T,
    ) -> T {
        let saved_state_version = self.state_version();
        let saved_in_loop = self.in_loop_body;
        let saved_class_var_version = self.class_var_version();
        self.set_state_version(0);
        self.in_loop_body = true;

        let result = f(self);

        self.in_loop_body = saved_in_loop;
        self.set_state_version(saved_state_version);
        self.set_class_var_version(saved_class_var_version);
        result
    }

    /// BT-412: Returns the current class variable state variable name.
    pub(super) fn current_class_var(&self) -> String {
        util::versioned_var("ClassVars", self.class_var_version())
    }

    /// BT-412: Increments class var version and returns the new variable name.
    fn next_class_var(&mut self) -> String {
        let new_version = self.class_var_version() + 1;
        self.set_class_var_version(new_version);
        self.set_class_var_mutated(true);
        util::versioned_var("ClassVars", new_version)
    }

    /// BT-833: Returns the current Self variable name for value type Self-threading.
    ///
    /// Version 0 → `"Self"` (the original method parameter).
    /// Version N → `"Self{N}"` (after N field assignments have threaded a new snapshot).
    pub(super) fn current_self_var(&self) -> String {
        util::versioned_var("Self", self.self_version())
    }

    /// BT-833: Increments the Self version and returns the new variable name.
    pub(super) fn next_self_var(&mut self) -> String {
        let new_version = self.self_version() + 1;
        self.set_self_version(new_version);
        util::versioned_var("Self", new_version)
    }

    /// BT-855: Records a structured diagnostic warning for the current module.
    ///
    /// Warnings are returned to callers via [`generate_module_with_warnings`].
    pub(super) fn add_codegen_warning(&mut self, diag: Diagnostic) {
        self.codegen_warnings.push(diag);
    }

    /// BT-1343: Emits a codegen diagnostic (gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`).
    ///
    /// These are informational diagnostics about codegen decisions (calling conventions,
    /// dynamic dispatch, NLR throw/catch, etc.). Emitted as `Diagnostic::hint` by default.
    pub(super) fn emit_codegen_diagnostic(&mut self, message: String, span: Span) {
        if self.codegen_diagnostics_enabled {
            self.add_codegen_warning(
                Diagnostic::hint(message, span).with_category(DiagnosticCategory::Type),
            );
        }
    }

    /// BT-1343: Emits a `StateAcc` fallback diagnostic, gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`.
    ///
    /// Promoted to `Diagnostic::warning` when `BEAMTALK_WARN_STATEACC=1` is also set.
    pub(super) fn emit_stateacc_fallback_diagnostic(&mut self, message: String, span: Span) {
        if self.codegen_diagnostics_enabled {
            if self.warn_stateacc {
                self.add_codegen_warning(
                    Diagnostic::warning(message, span)
                        .with_hint("Extract the expression into a local variable or method to avoid state-accumulator fallback")
                        .with_category(DiagnosticCategory::Type),
                );
            } else {
                self.add_codegen_warning(
                    Diagnostic::hint(message, span)
                        .with_hint("Extract the expression into a local variable or method to avoid state-accumulator fallback")
                        .with_category(DiagnosticCategory::Type),
                );
            }
        }
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
        self.add_codegen_warning(
            Diagnostic::warning(
                format!(
                    "stateful block passed to Erlang {erlang_target} — mutations inside \
                     the block will be silently dropped (Erlang cannot propagate the updated \
                     StateAcc back to the Beamtalk caller)"
                ),
                span,
            )
            .with_hint("Extract the block body into a method, or use a stateless block")
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// BT-909: Emits a warning for a non-literal callable at an Erlang call boundary.
    pub(super) fn warn_non_literal_callable_at_erlang_boundary(
        &mut self,
        erlang_target: &str,
        span: Span,
    ) {
        self.add_codegen_warning(
            Diagnostic::warning(
                format!(
                    "non-literal callable passed to Erlang {erlang_target} — if this is a \
                     stateful block, mutations inside the block will be silently dropped \
                     (runtime arity check inserted to prevent badarity crash)"
                ),
                span,
            )
            .with_hint("Use a block literal directly, or extract into a method to avoid ambiguity")
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// BT-833: Resets the Self version to 0 (call at the start of each value type method).
    pub(super) fn reset_self_version(&mut self) {
        self.set_self_version(0);
    }

    /// BT-940: Converts a byte-offset `Span` to a 1-based line number.
    ///
    /// Uses `self.source_text` to count newlines before the span's start offset.
    /// Returns `None` if source text is unavailable or the span is out of range.
    pub(super) fn span_to_line(&self, span: Span) -> Option<u32> {
        let source = self.source_text.as_deref()?;
        if span.start() as usize > source.len() {
            return None;
        }
        Some(span.line_number(source))
    }

    /// BT-940: Wraps a Document with a Core Erlang line annotation.
    ///
    /// Produces `( Doc -| [{Line, 1}, {'file', "path.bt"}] )` which the BEAM compiler
    /// preserves into the Line chunk. The VM surfaces this as
    /// `[{file, "path.bt"}, {line, N}]` in stacktrace frames.
    ///
    /// The annotation format must match what Erlang's own compiler produces
    /// (`{Line, Column}` tuple + `{file, Path}`) — the `{'line', N}` format
    /// is not propagated into the BEAM line table.
    pub(super) fn annotate_with_line(
        &self,
        doc: Document<'static>,
        line_num: u32,
    ) -> Document<'static> {
        match &self.source_path {
            Some(path) => {
                docvec![
                    "( ",
                    doc,
                    " -| [{",
                    leaf::int_lit(i64::from(line_num)),
                    ", 1}, {'file', ",
                    leaf::string_lit(path),
                    "}] )"
                ]
            }
            None => {
                // No source path — use bare line number annotation
                docvec![
                    "( ",
                    doc,
                    " -| [",
                    leaf::int_lit(i64::from(line_num)),
                    "] )"
                ]
            }
        }
    }

    /// BT-153/BT-245/BT-598: Check if mutation threading should be used for a block.
    /// In REPL mode, local variable mutations trigger threading.
    /// In actor module mode, field writes, self-sends, OR local variable
    /// mutations trigger threading. Local vars are threaded through the state accumulator
    /// map alongside fields.
    /// In value type module mode, only field writes trigger threading (no state map).
    /// BT-1346: Class methods have no State variable — field/self-send threading is disabled.
    /// BT-1414: Captured local variable mutations in class method blocks are threaded
    /// via a fresh local map (same as value types).
    pub(super) fn needs_mutation_threading(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
    ) -> bool {
        if self.is_repl_mode() {
            // REPL: both local vars and fields need threading
            analysis.has_mutations()
        } else if self.in_class_method() {
            // BT-1346: Class methods have no actor State variable — field writes and
            // self-sends must NOT trigger state threading.
            // BT-1414: However, captured local variable mutations (outer vars both read
            // and written in the block) DO need threading via a fresh local map, same as
            // value types. Without this, `do:` blocks silently lose local mutations.
            analysis
                .captured_reads
                .iter()
                .any(|v| analysis.local_writes.contains(v))
        } else if self.context == CodeGenContext::Actor {
            // BT-598: Actor methods: field writes, self-sends,
            // OR local variable mutations all need threading
            analysis.has_state_effects() || !analysis.local_writes.is_empty()
        } else {
            // BT-892: Value types have no State variable, so self-sends should
            // NOT trigger state threading. Only field writes need threading.
            // BT-1053: Captured local variable mutations (outer vars both read and
            // written in the block) also need threading via a fresh local map.
            !analysis.field_writes.is_empty()
                || analysis
                    .captured_reads
                    .iter()
                    .any(|v| analysis.local_writes.contains(v))
        }
    }

    /// BT-1329: Returns `true` if the block body contains list op message sends
    /// (do:, collect:, select:, reject:, inject:into:) whose blocks capture and
    /// mutate variables from the outer scope. These cross-scope mutations are
    /// invisible to `analyze_block` (which doesn't propagate `local_writes` from
    /// nested non-conditional blocks), so they require a separate scan.
    pub(super) fn body_has_list_op_cross_scope_mutations(&self, body: &crate::ast::Block) -> bool {
        let mut cross_scope_writes = std::collections::HashSet::new();
        for stmt in &body.body {
            Self::collect_list_op_cross_scope_mutations_recursive(
                &stmt.expression,
                &self.semantic_facts,
                &mut cross_scope_writes,
            );
        }
        !cross_scope_writes.is_empty()
    }

    /// BT-1329: Recursively scans an expression for list op message sends with
    /// cross-scope mutations. Unlike `collect_list_op_cross_scope_mutations`,
    /// this also looks inside Assignment values.
    fn collect_list_op_cross_scope_mutations_recursive(
        expr: &Expression,
        facts: &crate::semantic_analysis::SemanticFacts,
        out: &mut std::collections::HashSet<String>,
    ) {
        match Self::peel_parens(expr) {
            Expression::Assignment { value, .. } => {
                Self::collect_list_op_cross_scope_mutations_recursive(value, facts, out);
            }
            send @ Expression::MessageSend { .. } => {
                Self::collect_list_op_cross_scope_mutations(send, facts, out);
            }
            _ => {}
        }
    }

    /// BT-2363: Collects outer-scope locals that are *written* inside a nested
    /// counted loop (`timesRepeat:`/`to:do:`/`to:by:do:`) or list op, including
    /// write-only mutations that `collect_list_op_cross_scope_mutations` (read+write
    /// only) misses.
    ///
    /// A name is collected when it is in the nested block's `local_writes`, is not a
    /// parameter of any enclosing block (`excluded_params` or the nested block's own
    /// params), and resolves to an existing outer-scope binding (`lookup_var`). The
    /// `lookup_var` guard is why this needs `&self` rather than being a free function:
    /// it distinguishes a genuine outer local from a block-internal temporary.
    fn collect_nested_loop_outer_local_writes(
        &self,
        expr: &Expression,
        excluded_params: &HashSet<String>,
        out: &mut HashSet<String>,
    ) {
        use crate::ast::MessageSelector;
        use crate::codegen::core_erlang::block_analysis::analyze_block;

        // Peel parens then an assignment RHS (which may itself be parenthesized) so
        // forms like `_r := (1 to: 5 do: [...])` are still inspected — mirrors
        // `expr_has_nested_counted_loop_threading`.
        let inner = match Self::peel_parens(expr) {
            Expression::Assignment { value, .. } => Self::peel_parens(value),
            other => other,
        };
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = inner
        else {
            return;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let body_block = match sel.as_str() {
            "do:" | "collect:" | "select:" | "reject:" | "anySatisfy:" | "allSatisfy:"
            | "timesRepeat:" => match arguments.last() {
                Some(Expression::Block(block)) => block,
                _ => return,
            },
            "inject:into:" | "to:do:" if arguments.len() == 2 => match &arguments[1] {
                Expression::Block(block) => block,
                _ => return,
            },
            "to:by:do:" if arguments.len() == 3 => match &arguments[2] {
                Expression::Block(block) => block,
                _ => return,
            },
            _ => return,
        };

        let analysis = self
            .semantic_facts
            .block_profile(&body_block.span)
            .cloned()
            .unwrap_or_else(|| analyze_block(body_block));
        // Accumulate enclosing params with this block's own params so a loop variable
        // bound at any enclosing level is never mistaken for a threadable outer local.
        let mut all_excluded: HashSet<String> = excluded_params.clone();
        all_excluded.extend(Self::block_param_names(body_block));

        for v in &analysis.local_writes {
            if !all_excluded.contains(v.as_str()) && self.lookup_var(v).is_some() {
                out.insert(v.clone());
            }
        }

        // Recurse so deeper nesting (loops two or more levels deep) is detected.
        for stmt in &body_block.body {
            self.collect_nested_loop_outer_local_writes(&stmt.expression, &all_excluded, out);
        }
    }

    /// Allocates fresh temporary variable names for an NLR try/catch wrapper.
    fn alloc_nlr_catch_vars(&mut self) -> NlrCatchVars {
        NlrCatchVars {
            result_var: self.fresh_temp_var("NlrResult"),
            cls_var: self.fresh_temp_var("NlrCls"),
            err_var: self.fresh_temp_var("NlrErr"),
            stk_var: self.fresh_temp_var("NlrStk"),
            ctk_var: self.fresh_temp_var("CatchTok"),
            val_var: self.fresh_temp_var("NlrVal"),
            state_var: self.fresh_temp_var("NlrState"),
            ot_pair_var: self.fresh_temp_var("OtherPair"),
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
        let try_catch = self.wrap_body_with_nlr_catch(body_doc, token_var, NlrBoundary::ActorReply);

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

    /// BT-1202: Wraps a class method body with NLR (non-local return) try/catch.
    ///
    /// Class method NLR uses a 4-element throw tuple `{$bt_nlr, Token, Value, ClassVars}` and
    /// catches it to return `NlrVal` directly (no class vars) or
    /// `{class_var_result, NlrVal, NlrState}` (with class vars).
    pub(super) fn wrap_class_method_body_with_nlr_catch(
        &mut self,
        body_doc: Document<'static>,
        token_var: &str,
        has_class_vars: bool,
    ) -> Document<'static> {
        self.wrap_body_with_nlr_catch(
            body_doc,
            token_var,
            NlrBoundary::ClassMethod { has_class_vars },
        )
    }

    /// BT-2361: The single boundary-parameterised NLR try/catch wrapper.
    ///
    /// Collapses what used to be the structurally-identical Actor and class-method
    /// `wrap_*_body_with_nlr_catch` wrappers (and shares its catch-arm builder with the
    /// value-type wrapper). The try/catch scaffolding — make the NLR token, `try`/`of`
    /// the body, `catch` the 4-tuple `{'$bt_nlr', Token, Value, State}` throw — is
    /// identical across contexts; only the matching arm's result Document varies, which
    /// `boundary` selects via [`nlr_arm_result`].
    ///
    /// BT-875: Use Document/docvec! — never format!() for Core Erlang fragments.
    fn wrap_body_with_nlr_catch(
        &mut self,
        body_doc: Document<'static>,
        token_var: &str,
        boundary: NlrBoundary,
    ) -> Document<'static> {
        let vars = self.alloc_nlr_catch_vars();
        let nlr_arm_result = nlr_arm_result(&vars.val_var, &vars.state_var, boundary);
        let NlrCatchVars {
            result_var,
            cls_var,
            err_var,
            stk_var,
            ctk_var,
            val_var,
            state_var,
            ot_pair_var,
        } = vars;

        docvec![
            "let ",
            leaf::var(token_var.to_string()),
            " = call 'erlang':'make_ref'() in\n",
            "try\n",
            body_doc,
            "\n",
            "of ",
            leaf::var(result_var.clone()),
            " -> ",
            leaf::var(result_var),
            "\n",
            "catch <",
            leaf::var(cls_var.clone()),
            ", ",
            leaf::var(err_var.clone()),
            ", ",
            leaf::var(stk_var.clone()),
            "> ->\n",
            "  case {",
            leaf::var(cls_var.clone()),
            ", ",
            leaf::var(err_var.clone()),
            "} of\n",
            "    <{'throw', {'$bt_nlr', ",
            leaf::var(ctk_var.clone()),
            ", ",
            leaf::var(val_var),
            ", ",
            leaf::var(state_var),
            "}}> ",
            "when call 'erlang':'=:='(",
            leaf::var(ctk_var),
            ", ",
            leaf::var(token_var.to_string()),
            ") -> ",
            nlr_arm_result,
            "\n",
            "    <",
            leaf::var(ot_pair_var),
            "> when 'true' -> ",
            "primop 'raw_raise'(",
            leaf::var(cls_var),
            ", ",
            leaf::var(err_var),
            ", ",
            leaf::var(stk_var),
            ")\n",
            "  end",
        ]
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
        let NlrCatchVars {
            result_var,
            cls_var,
            err_var,
            stk_var,
            ctk_var,
            val_var,
            state_var,
            ot_pair_var,
        } = self.alloc_nlr_catch_vars();

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
    /// BT-1639: Computes the set of sealed classes whose class methods are eligible
    /// for direct calls (bypassing `gen_server` dispatch).
    ///
    /// A class method is eligible when all four conditions hold:
    /// 1. The class is sealed (all methods visible at compile time)
    /// 2. The class has no class variables (no state to mutate)
    /// 3. The method is a class method (not instance-side)
    /// 4. The selector is not a supervisor constructor (`startLink`, `startLink:`)
    ///
    /// Returns a mapping from class name to `DirectCallClassInfo` with the module
    /// name and set of eligible selectors.
    fn compute_direct_call_eligible(
        hierarchy: &crate::semantic_analysis::class_hierarchy::ClassHierarchy,
        generator: &CoreErlangGenerator,
    ) -> std::collections::HashMap<String, DirectCallClassInfo> {
        // Selectors that depend on gen_server process state and must NOT be
        // called directly: supervisor constructors (`startLink` family) and
        // `basicNew`/`basicNewWith` constructors (`new`/`new:`) which read
        // `beamtalk_class_name`/`beamtalk_class_module` from the process dictionary.
        let excluded_selectors: std::collections::HashSet<&str> =
            ["startLink", "startLink:", "new", "new:"]
                .into_iter()
                .collect();
        let mut result = std::collections::HashMap::new();

        for (class_name, class_info) in hierarchy.classes() {
            // Gate 1: Class must be sealed
            if !class_info.is_sealed {
                continue;
            }
            // Gate 2: Class must have no class variables
            if !class_info.class_variables.is_empty() {
                continue;
            }
            // Gate 3: Class must have class methods
            if class_info.class_methods.is_empty() {
                continue;
            }

            let mut selectors = std::collections::HashSet::new();
            for method in &class_info.class_methods {
                // Gate 4: Skip selectors that depend on gen_server process state
                if excluded_selectors.contains(method.selector.as_str()) {
                    continue;
                }
                // Gate 5: Only optimize `class sealed` methods (is_sealed=true).
                // Non-sealed class methods may reference `self` (the class object)
                // for factory patterns or delegation, which would break with nil ClassSelf.
                if !method.is_sealed {
                    continue;
                }
                selectors.insert(method.selector.to_string());
            }

            if !selectors.is_empty() {
                let module_name = EcoString::from(generator.compiled_module_name(class_name));
                result.insert(
                    class_name.to_string(),
                    DirectCallClassInfo {
                        module_name,
                        selectors,
                    },
                );
            }
        }

        result
    }

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
            // BT-1220: Concrete Supervisor/DynamicSupervisor subclasses use supervisor codegen,
            // routed through generate_actor_module which delegates to supervisor_codegen.
            // Abstract base classes (Supervisor, DynamicSupervisor themselves) remain value types.
            if class.supervisor_kind.is_some() && !class.is_abstract {
                return true;
            }
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
                    docvec![
                        "call 'gen_server':'start_link'(",
                        leaf::atom(self.module_name.to_string()),
                        ", InitArgs, [])",
                    ],
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
                        "call 'gen_server':'start_link'(ServerName, ",
                        leaf::atom(self.module_name.to_string()),
                        ", InitArgs, [])",
                    ],
                ]
            ),
            "\n\n",
        ]
    }

    /// Generates the `dispatch/3` function that delegates to the actor's own Erlang module.
    ///
    /// For actor classes with `@primitive` methods, the compiled dispatch/4 calls
    /// `Module:dispatch(Selector, Args, Self)` (3-arity) for primitive method bodies.
    /// This function provides that 3-arity entry point, delegating to the actor's
    /// main Erlang module (e.g. `beamtalk_subprocess`) which exports `dispatch/3`.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'dispatch'/3 = fun (Selector, Args, Self) ->
    ///     call 'beamtalk_subprocess':'dispatch'(Selector, Args, Self)
    /// ```
    fn generate_primitive_dispatch_3_doc(&self) -> Document<'static> {
        // Call dispatch/3 on the actor's own Erlang module (same name as the backing
        // gen_server, e.g. `beamtalk_subprocess`). The actor module is responsible
        // for exporting `dispatch/3` to handle class-side @primitive methods.
        // If `to_module_name` already emits a `beamtalk_` prefix (e.g.
        // `BeamtalkInterface` → `beamtalk_interface`), use it as-is.
        let snake_name = to_module_name(&self.class_name());
        let actor_module_name = if snake_name.starts_with("beamtalk_") {
            snake_name
        } else {
            format!("beamtalk_{snake_name}")
        };
        docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "call ",
                        leaf::atom(actor_module_name),
                        ":'dispatch'(Selector, Args, Self)",
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
    #[allow(clippy::too_many_lines)]
    fn generate_expression(&mut self, expr: &Expression) -> Result<Document<'static>> {
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => self.generate_identifier(id),
            Expression::ClassReference { name, package, .. } => {
                self.generate_class_reference(&name.name, package.as_ref().map(|p| p.name.as_str()))
            }
            Expression::Super(_) => {
                // Super by itself is not a valid expression - it must be used
                // as a message receiver (e.g., `super increment`)
                Err(CodeGenError::UnsupportedFeature {
                    feature: "'super' must be used with a message send".to_string(),
                    span: Some(expr.span()),
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
                // Only annotate CLOSED expressions — open let-chains (class method sends,
                // direct-params list ops) cannot be annotated because the trailing `in`
                // breaks Core Erlang syntax when wrapped in `( expr -| [annotation] )`.
                if self.last_open_scope_result.is_none()
                    && self.direct_params_list_op_result.is_none()
                {
                    if let Some(line_num) = self.span_to_line(*span) {
                        return Ok(self.annotate_with_line(doc, line_num));
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
                        span: Some(target.span()),
                    });
                }
                // For identifier assignments (e.g., local variables in REPL like `x := 1`),
                // just return the value - REPL handles binding updates externally.
                // In compiled code, local variable assignments should be handled by
                // the block/method scope, but for now we generate just the value.
                self.generate_expression(value)
            }
            Expression::Return { value, span, .. } => {
                // BT-754: If inside a block with NLR infrastructure active, generate a throw
                // so the return escapes from the block closure back to the enclosing method.
                // Otherwise (at method body level, or no NLR), just emit the value.
                if let Some(nlr_token) = self.current_nlr_token().cloned() {
                    // BT-1343: Emit diagnostic for NLR throw/catch generation.
                    self.emit_codegen_diagnostic(
                        {
                            let line_info = self
                                .span_to_line(*span)
                                .map_or(String::new(), |l| format!(" at line {l}"));
                            format!(
                                "Non-local return{line_info}: compiled via throw/catch, \
                                 may inhibit JIT optimization"
                            )
                        },
                        *span,
                    );
                    let val_doc = self.generate_expression(value)?;
                    // BT-761/BT-854: All NLR throws carry state as a 4-tuple.
                    // Actor methods use the current gen_server state; value type
                    // methods use the latest Self{N} snapshot so field mutations
                    // accumulated before the ^ are preserved.
                    // BT-1202: Class methods use the current ClassVars snapshot.
                    let state = if self.in_class_method() {
                        self.current_class_var()
                    } else if self.context == CodeGenContext::Actor {
                        self.current_state_var()
                    } else {
                        self.current_self_var()
                    };
                    Ok(docvec![
                        "call 'erlang':'throw'({'$bt_nlr', ",
                        leaf::var(nlr_token),
                        ", ",
                        val_doc,
                        ", ",
                        leaf::var(state),
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
            Expression::DestructureAssignment { span, .. } => {
                // DestructureAssignment is only valid as a statement in a body context.
                // All statement-body generators handle it explicitly. Reaching here means
                // it appeared in a pure expression position, which is not supported.
                Err(CodeGenError::UnsupportedFeature {
                    feature: "destructuring assignment in expression position".to_string(),
                    span: Some(*span),
                })
            }
            Expression::Error { message, span, .. } => Err(CodeGenError::UnsupportedFeature {
                feature: format!("expression error: {message}"),
                span: Some(*span),
            }),
            Expression::ExpectDirective { .. } => Ok(Document::Nil),
            Expression::Spread { name, .. } => Err(CodeGenError::UnsupportedFeature {
                feature: format!("spread expression: {}", name.name),
                span: Some(name.span),
            }),
        }
    }

    /// Generates code for a standalone `ClassReference`.
    ///
    /// ADR 0019 Phase 3: In workspace mode, checks REPL bindings first for
    /// convenience names (Transcript, Beamtalk, Workspace), then falls back
    /// to class registry lookup. In batch mode, goes directly to the registry.
    ///
    /// ADR 0070 Phase 2: When `package` is `Some`, the class is from a known
    /// dependency and the module name is deterministic (`bt@{pkg}@{snake}`).
    /// The class registry lookup uses the class name for now — package-aware
    /// registry disambiguation is a future phase.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    fn generate_class_reference(
        &mut self,
        class_name: &str,
        package: Option<&str>,
    ) -> Result<Document<'static>> {
        // ADR 0070 Phase 2: For package-qualified references, we know the exact
        // display name to use in the class object tuple.
        let display_name = match package {
            Some(pkg) => format!("{pkg}@{class_name}"),
            None => class_name.to_string(),
        };

        // ADR 0019 Phase 3: Only check bindings in REPL top-level context.
        // Actor methods compiled in workspace mode should NOT check REPL bindings.
        //
        // BT-2365 (ADR 0081 Phase 1): for an unqualified class reference, check the
        // session locals map first so a session local of the same name takes
        // precedence. (A capitalised name parses as a ClassReference, not an
        // assignment target, so it cannot itself be rebound via `:=`; the locals
        // check is for symmetry with resolve_name/2 and is essentially always a
        // miss.) On a miss, delegate to the shared runtime resolver, which consults
        // the live singleton + class registries — the singletons
        // (Transcript/Beamtalk/Workspace) are no longer eagerly injected into
        // State, so this lazy lookup replaces the old inline class-registry branch.
        // The resolver raises the same class_not_found error for a genuinely
        // unknown class, preserving REPL output. Package-qualified references
        // (`json@Parser`) keep the inline path below because the resolver does not
        // carry the package-qualified display name.
        if self.workspace_mode() && self.context == CodeGenContext::Repl && package.is_none() {
            let state_var = self.current_state_var();
            let resolved_var = self.fresh_var("ResolvedClass");

            Ok(docvec![
                "case call 'maps':'find'(",
                leaf::atom(class_name.to_string()),
                ", ",
                leaf::var(state_var.clone()),
                ") of ",
                "<{'ok', ",
                leaf::var(resolved_var.clone()),
                "}> when 'true' -> ",
                leaf::var(resolved_var),
                " <'error'> when 'true' -> call 'beamtalk_workspace':'resolve_class_reference'(",
                leaf::var(state_var),
                ", ",
                leaf::atom(class_name.to_string()),
                ") ",
                "end",
            ])
        } else if self.workspace_mode() && self.context == CodeGenContext::Repl {
            // Package-qualified REPL class reference: keep the original
            // locals-then-registry path with the package-qualified display name.
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let state_var = self.current_state_var();
            let error_doc = self.class_not_found_error_doc(class_name);

            Ok(docvec![
                "case call 'maps':'find'(",
                leaf::atom(class_name.to_string()),
                ", ",
                leaf::var(state_var),
                ") of ",
                "<{'ok', _BindingVal}> when 'true' -> _BindingVal ",
                "<'error'> when 'true' -> ",
                "case call 'beamtalk_class_registry':'whereis_class'(",
                leaf::atom(class_name.to_string()),
                ") of ",
                error_doc,
                "<",
                leaf::var(class_pid_var.clone()),
                "> when 'true' -> ",
                "let ",
                leaf::var(class_mod_var.clone()),
                " = call 'beamtalk_object_class':'module_name'(",
                leaf::var(class_pid_var.clone()),
                ") in ",
                "{'beamtalk_object', ",
                leaf::atom(format!("{display_name} class")),
                ", ",
                leaf::var(class_mod_var),
                ", ",
                leaf::var(class_pid_var),
                "} ",
                "end end",
            ])
        } else {
            // Actor/ValueType methods in workspace mode and batch mode both use
            // registry-only lookup. ADR 0019 Phase 4: No persistent_term fallback.
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let error_doc = self.class_not_found_error_doc(class_name);

            Ok(docvec![
                "case call 'beamtalk_class_registry':'whereis_class'(",
                leaf::atom(class_name.to_string()),
                ") of ",
                error_doc,
                "<",
                leaf::var(class_pid_var.clone()),
                "> when 'true' -> ",
                "let ",
                leaf::var(class_mod_var.clone()),
                " = call 'beamtalk_object_class':'module_name'(",
                leaf::var(class_pid_var.clone()),
                ") in ",
                "{'beamtalk_object', ",
                leaf::atom(format!("{display_name} class")),
                ", ",
                leaf::var(class_mod_var),
                ", ",
                leaf::var(class_pid_var),
                "} ",
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

        docvec![
            "<'undefined'> when 'true' -> let ",
            leaf::var(err0_var.clone()),
            " = call 'beamtalk_error':'new'('class_not_found', ",
            leaf::atom(class_name.to_string()),
            ") in ",
            "let ",
            leaf::var(err1_var.clone()),
            " = call 'beamtalk_error':'with_hint'(",
            leaf::var(err0_var),
            ", ",
            leaf::binary_lit(hint),
            ") in ",
            "call 'beamtalk_error':'raise'(",
            leaf::var(err1_var),
            ") ",
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
    ///
    /// BT-2374: the loop / foldl-list-op extraction set is no longer re-derived by a
    /// parallel `threaded_vars_*` family — it delegates to the single packing-side
    /// authority [`Self::compute_threaded_locals_for_loop`] (which already branches per
    /// context). The extraction side reading back exactly the set the packing side wrote
    /// is the invariant that keeps `maps:get/2` from hitting a missing `__local__` key;
    /// sharing one function makes that symmetry structural rather than a hand-maintained
    /// mirror. Conditionals retain [`Self::conditional_threaded_locals`], which is already
    /// the shared seed/extract authority for the inline-`case` path.
    fn get_control_flow_threaded_vars(&self, expr: &Expression) -> Option<Vec<String>> {
        // BT-2355: `_r := (loop)` wraps the construct in parentheses; peel them so
        // the threaded locals are still discovered when the construct is an
        // assignment RHS or sub-expression.
        let expr = Self::peel_parens(expr);
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        else {
            return None;
        };

        // BT-2073: `whileTrue:` / `whileFalse:` are well-known; dispatch via the enum.
        // The condition block (receiver) and body block (first argument) reads/writes are
        // unioned by `compute_threaded_locals_for_loop(body, Some(condition))`.
        if matches!(
            selector.well_known(),
            Some(WellKnownSelector::WhileTrue | WellKnownSelector::WhileFalse)
        ) {
            let (Expression::Block(_), Some(Expression::Block(body_block))) =
                (receiver.as_ref(), arguments.first())
            else {
                return None;
            };
            return Self::non_empty(
                self.compute_threaded_locals_for_loop(body_block, Some(receiver.as_ref())),
            );
        }

        let MessageSelector::Keyword(parts) = selector else {
            return None;
        };
        let selector_name: String = parts.iter().map(|kw| kw.keyword.as_str()).collect();

        match selector_name.as_str() {
            "to:do:" if arguments.len() == 2 => self.threaded_locals_of_loop_body(arguments.get(1)),
            "to:by:do:" if arguments.len() == 3 => {
                self.threaded_locals_of_loop_body(arguments.get(2))
            }
            // BT-1276: collect:/select:/reject: pack updated locals into the StateAcc map
            // returned as element(2, ...) of the result tuple, so the outer method body can
            // extract them via maps:get — same pattern as do:/inject:into:.
            //
            // BT-2355: foldl predicate ops (count:/detect:/detect:ifNone:) follow the same
            // shape — the mutating predicate is the first argument and its updated locals are
            // packed into the StateAcc map at the end of the fold (always, regardless of
            // iteration count). Any trailing handler (e.g. detect:ifNone:'s ifNone block) is
            // not part of the fold, so it is ignored here.
            //
            // BT-2356/BT-2374: the remaining state-threading list/dict ops — and the counted
            // `timesRepeat:` loop — pack via the same `ThreadingPlan` machinery and so must be
            // extracted with the identical local set. All route through
            // `compute_threaded_locals_for_loop` (the packing side) so every packed
            // `__local__` key — including write-only and nested cross-scope mutations — is read
            // back (no missing key ⇒ no `{badkey}`). They share the body-block-is-first-argument
            // shape, so they share one arm.
            "timesRepeat:" | "do:" | "collect:" | "select:" | "reject:" | "count:" | "detect:"
            | "detect:ifNone:" | "anySatisfy:" | "allSatisfy:" | "flatMap:" | "takeWhile:"
            | "dropWhile:" | "groupBy:" | "partition:" | "sort:" | "doWithKey:"
            | "keysAndValuesDo:" => self.threaded_locals_of_loop_body(arguments.first()),
            "inject:into:" if arguments.len() == 2 => {
                self.threaded_locals_of_loop_body(arguments.get(1))
            }
            // BT-2703: `eachWithIndex:`/`do:separatedBy:` desugar to an `inject:into:`
            // fold (see `enumeration_ops`), packing the block's outer-local mutations
            // into the same `__local__` StateAcc keys. The element block is the first
            // argument; `do:separatedBy:`'s separator (the second block) runs in the
            // fold too, so its outer-local writes are unioned in as well. Gated on
            // `enumeration_threads_actor_state`: only the actor fold packs those keys
            // into a `{Acc, State}` reply tuple, so outside it (value types, REPL, a
            // direct-params loop) there is no `__local__` StateAcc to extract from.
            "eachWithIndex:" if arguments.len() == 1 && self.enumeration_threads_actor_state() => {
                self.threaded_locals_of_loop_body(arguments.first())
            }
            "do:separatedBy:" if arguments.len() == 2 && self.enumeration_threads_actor_state() => {
                Self::non_empty(self.conditional_threaded_locals(&Self::block_args(arguments)))
            }
            // BT-2355: conditionals thread outer-local mutations through the StateAcc
            // map under `__local__` keys (see generate_*_with_mutations, which also seed
            // those keys so extraction is safe even when the taken branch did not write
            // them). Only the selectors that (a) `is_conditional_selector` recognises as
            // state-threading and (b) have a `generate_*_with_mutations` inline-case
            // generator are listed here — others (`ifNil:`, `ifFalse:ifTrue:`, …) are not
            // routed through that path, so adding them here would be unreachable.
            "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifNotNil:" => {
                Self::non_empty(self.conditional_threaded_locals(&Self::block_args(arguments)))
            }
            _ => None,
        }
    }

    /// BT-2374: Computes the threaded outer-locals for a counted loop (`timesRepeat:`,
    /// `to:do:`, `to:by:do:`) or foldl list/dict op body block via the single packing-side
    /// authority [`Self::compute_threaded_locals_for_loop`], returning `None` when the set
    /// is empty (so the caller's `if let Some(..)` short-circuits) or when `body_arg` is
    /// not a literal block.
    fn threaded_locals_of_loop_body(&self, body_arg: Option<&Expression>) -> Option<Vec<String>> {
        let Some(Expression::Block(body_block)) = body_arg else {
            return None;
        };
        Self::non_empty(self.compute_threaded_locals_for_loop(body_block, None))
    }

    /// BT-2374: `Some(v)` when `v` is non-empty, else `None`. Lets the threaded-locals
    /// extraction collapse a `Vec<String>` packing-side set into the `Option<Vec<String>>`
    /// the Actor method-body sequencer consumes, where empty and absent are equivalent.
    fn non_empty(v: Vec<String>) -> Option<Vec<String>> {
        if v.is_empty() { None } else { Some(v) }
    }

    /// BT-2355: Peels any `Expression::Parenthesized` wrappers, returning the inner
    /// expression. Used so control-flow classification/threading sees through the
    /// parentheses in forms like `_r := (1 to: 5 do: [...])`.
    pub(super) fn peel_parens(expr: &Expression) -> &Expression {
        let mut current = expr;
        while let Expression::Parenthesized { expression, .. } = current {
            current = expression;
        }
        current
    }

    /// BT-2355: Collects the `Block` arguments of a message send (e.g. the branch
    /// blocks of a conditional), preserving order.
    fn block_args(arguments: &[Expression]) -> Vec<&Block> {
        arguments
            .iter()
            .filter_map(|a| {
                if let Expression::Block(b) = a {
                    Some(b)
                } else {
                    None
                }
            })
            .collect()
    }

    /// BT-2355: Computes the outer-local variables that a conditional's branch
    /// blocks mutate and that must be threaded back through the `StateAcc` map.
    ///
    /// A variable is threaded when it is written in some branch, is bound in the
    /// enclosing (outer) scope, and is not a block parameter. This covers both
    /// write-only (`flag ifTrue: [m := 9]`) and read+write
    /// (`flag ifTrue: [sum := sum + 7]`) mutations, while excluding block-local
    /// temporaries (which are not bound in the outer scope).
    ///
    /// The same set drives both the seeding emitted by `generate_*_with_mutations`
    /// and the extraction emitted by the method-body sequencer, keeping them in
    /// sync so a non-taken branch never leaves a `__local__` key missing.
    pub(super) fn conditional_threaded_locals(&self, blocks: &[&Block]) -> Vec<String> {
        use crate::codegen::core_erlang::block_analysis::analyze_block;

        let mut set = HashSet::new();
        for block in blocks {
            let analysis = self
                .semantic_facts
                .block_profile(&block.span)
                .cloned()
                .unwrap_or_else(|| analyze_block(block));
            let params = Self::block_param_names(block);
            // BT-2356: `analyze_block` does not propagate `local_writes` out of nested
            // (non-conditional) blocks, so an outer local mutated by a nested list op in a
            // branch — e.g. `flag ifTrue: [ items do: [:x | sum := sum + x] ]` — is invisible
            // to `analysis.local_writes`. Collect those cross-scope mutations too so the var is
            // both seeded (by `seed_conditional_locals`) and extracted by the method-body
            // sequencer. The branch body re-threads the nested op's mutation into the branch's
            // returned StateAcc (the nested op is itself classified as state-threading), so the
            // seeded key is overwritten with the live value rather than left stale.
            let mut cross_scope = HashSet::new();
            for stmt in &block.body {
                Self::collect_list_op_cross_scope_mutations_recursive(
                    &stmt.expression,
                    &self.semantic_facts,
                    &mut cross_scope,
                );
            }
            for v in analysis.local_writes.iter().chain(cross_scope.iter()) {
                if params.contains(v) {
                    continue;
                }
                if self.lookup_var(v).is_some() {
                    set.insert(v.clone());
                }
            }
        }
        let mut out: Vec<String> = set.into_iter().collect();
        // Deterministic order for stable codegen output.
        out.sort();
        out
    }

    /// Returns the set of block parameter names for exclusion from threaded vars.
    fn block_param_names(block: &Block) -> HashSet<String> {
        block
            .parameters
            .iter()
            .map(|p| p.name.to_string())
            .collect()
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
    /// For **selector-based** primitives (quoted, e.g., `@primitive "+"`), generates
    /// direct Erlang BIF calls when a known implementation exists. This makes the
    /// compiled stdlib module self-sufficient — no delegation to hand-written
    /// Erlang dispatch modules.
    ///
    /// Falls back to a `does_not_understand` error for selectors with no known
    /// BIF implementation.
    ///
    /// For **structural intrinsics** (bare, e.g., `@primitive blockValue`),
    /// these are handled at the call site by `dispatch_codegen`, not here.
    /// The method body for structural intrinsics is never directly called.
    #[allow(clippy::too_many_lines)] // BT-1763: Erlang interop intrinsics add essential branches
    fn generate_primitive(
        &mut self,
        name: &str,
        is_quoted: bool,
        span: crate::source_analysis::Span,
    ) -> Result<Document<'static>> {
        let class_name = self
            .class_identity()
            .map(|id| id.class_name().to_string())
            .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "@primitive \"{name}\" used outside of a class context"
                ))
            })?;

        // ADR 0038: ClassBuilder register intrinsic — emits a call to
        // beamtalk_class_builder:register/1 with the builder's gen_server state.
        if !is_quoted && name == "classBuilderRegister" {
            return Ok(self.generate_class_builder_register());
        }

        // BT-1548: basicNew/basicNewWith intrinsics in class method context.
        // When Value defines `class sealed new => @intrinsic basicNew`, the class
        // method body needs to call class_self_new (which routes through handle_new
        // to the target class's auto-generated new/0 via the process dictionary).
        if !is_quoted && self.in_class_method() {
            match name {
                "basicNew" => {
                    return Ok(docvec![
                        "call 'beamtalk_class_instantiation':'class_self_new'(",
                        "call 'erlang':'get'('beamtalk_class_name'), ",
                        "call 'erlang':'get'('beamtalk_class_module'), [])",
                    ]);
                }
                "basicNewWith" => {
                    let param = self
                        .current_method_params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "InitArgs".to_string());
                    return Ok(docvec![
                        "call 'beamtalk_class_instantiation':'class_self_new'(",
                        "call 'erlang':'get'('beamtalk_class_name'), ",
                        "call 'erlang':'get'('beamtalk_class_module'), [",
                        leaf::var(param),
                        "])",
                    ]);
                }
                _ => {}
            }
        }

        // BT-1763: Erlang interop DNU intrinsics — forward selector/args to
        // the handler module's dispatch/3 rather than passing the intrinsic name.
        // doesNotUnderstand:args: receives (Self, Selector, Args) and we need to
        // forward Selector and Args as the dispatch selector and argument list.
        if !is_quoted {
            match name {
                "erlangApply" => {
                    let params = &self.current_method_params;
                    let selector_param = params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Selector".to_string());
                    let args_param = params
                        .get(1)
                        .cloned()
                        .unwrap_or_else(|| "Arguments".to_string());
                    return Ok(docvec![
                        "call 'beamtalk_erlang_proxy':'dispatch'(",
                        leaf::var(selector_param),
                        ", ",
                        leaf::var(args_param),
                        ", Self)"
                    ]);
                }
                "erlangModuleLookup" => {
                    let params = &self.current_method_params;
                    let selector_param = params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Selector".to_string());
                    let args_param = params
                        .get(1)
                        .cloned()
                        .unwrap_or_else(|| "Arguments".to_string());
                    return Ok(docvec![
                        "call 'beamtalk_erlang_class':'dispatch'(",
                        leaf::var(selector_param),
                        ", ",
                        leaf::var(args_param),
                        ", Self)"
                    ]);
                }
                _ => {}
            }
        }

        // BT-1478: Logger intrinsics — generate inline logger:log/3 calls.
        // These are the method bodies for Logger.bt's @intrinsic declarations.
        // Direct `Logger warn:` calls are intercepted at the call site by
        // try_generate_logger_intrinsic (which injects the caller's class/selector
        // as metadata). This body path is reached only via indirect dispatch
        // (e.g., perform:), in which case Logger's own class/selector metadata
        // is appropriate.
        if !is_quoted {
            if let Some(doc) = self.try_generate_logger_body_intrinsic(name) {
                return Ok(doc);
            }
        }

        // BT-340: For selector-based primitives, try to emit a direct BIF call
        // instead of delegating through a hand-written dispatch module.
        if is_quoted {
            let params = self.current_method_params.clone();
            if let Some(code) = primitives::generate_primitive_bif(&class_name, name, &params) {
                return Ok(code);
            }

            // BT-2233: An unmapped quoted @primitive in a stdlib value-type class
            // is a bug — it would silently fall back to the runtime-dispatch path
            // below and raise does_not_understand at runtime (the BT-2232
            // regression). Fail the build instead. The check is scoped so it has
            // no false positives:
            //  - Stdlib mode only. User/FFI @primitive (via --allow-primitives)
            //    keeps BT-938's warn-and-fallback behavior for runtime dispatch.
            //  - Value-type context only. Actor classes legitimately route
            //    unmapped quoted primitives through their hand-written
            //    `beamtalk_X:dispatch` module (e.g. Actor's `actorPid`,
            //    ReactiveSubprocess's `open:args:notify:`).
            //  - Excluding a small set of call-site-intercepted reflective /
            //    identity / dynamic operations whose method body is only a
            //    runtime-dispatch placeholder (see
            //    `primitives::is_runtime_dispatched_primitive`).
            if self.stdlib_mode()
                && self.context == CodeGenContext::ValueType
                && !primitives::is_runtime_dispatched_primitive(&class_name, name)
            {
                return Err(CodeGenError::UnmappedPrimitive {
                    class: class_name.clone(),
                    selector: name.to_string(),
                    span: Some(span),
                });
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
                self.add_codegen_warning(
                    Diagnostic::warning(
                        format!(
                            "@primitive \"{name}\" references module '{runtime_module}' which has not been compiled — ensure the class is included in the stdlib build"
                        ),
                        span,
                    )
                    .with_hint(format!("Add the '{runtime_module}' module to the stdlib build, or check the @primitive name for typos"))
                    .with_category(DiagnosticCategory::Type),
                );
            }
        }

        let params_doc = document::join(
            self.current_method_params
                .iter()
                .map(|p| leaf::var(p.clone())),
            &Document::Str(", "),
        );
        // BT-677: In class methods, self is bound to ClassSelf, not Self
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        Ok(docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'(",
            leaf::atom(name.to_string()),
            ", [",
            params_doc,
            "], ",
            Document::Str(self_var),
            ")"
        ])
    }

    /// BT-1478: Generates inline `logger:log/3` code for Logger @intrinsic bodies.
    ///
    /// Maps intrinsic names to OTP logger levels:
    /// - `loggerDebug` / `loggerDebugMeta` → `debug`
    /// - `loggerInfo` / `loggerInfoMeta` → `info`
    /// - `loggerWarn` / `loggerWarnMeta` → `warning`
    /// - `loggerError` / `loggerErrorMeta` → `error`
    ///
    /// Returns `None` for non-logger intrinsic names.
    fn try_generate_logger_body_intrinsic(&mut self, name: &str) -> Option<Document<'static>> {
        let (level, has_metadata) = match name {
            "loggerDebug" => ("debug", false),
            "loggerInfo" => ("info", false),
            "loggerWarn" => ("warning", false),
            "loggerError" => ("error", false),
            "loggerDebugMeta" => ("debug", true),
            "loggerInfoMeta" => ("info", true),
            "loggerWarnMeta" => ("warning", true),
            "loggerErrorMeta" => ("error", true),
            _ => return None,
        };

        let params = self.current_method_params.clone();
        let msg_param = params
            .first()
            .cloned()
            .unwrap_or_else(|| "Message".to_string());

        let ctx_class = self.class_name();
        let ctx_selector = self
            .current_method_selector
            .clone()
            .unwrap_or_else(|| "unknown".to_string());

        let metadata_map_doc = docvec![
            "~{",
            "'domain' => ['beamtalk' | ['user']], ",
            "'beamtalk_class' => ",
            leaf::atom(ctx_class),
            ", ",
            "'beamtalk_selector' => ",
            leaf::atom(ctx_selector),
            "}~",
        ];

        let discard_var = self.fresh_temp_var("LogOk");

        let log_call_doc = if has_metadata {
            let meta_param = params.get(1).cloned().unwrap_or_else(|| "Meta".to_string());
            let merge_var = self.fresh_temp_var("LogMeta");
            docvec![
                "let ",
                leaf::var(merge_var.clone()),
                " = call 'maps':'merge'(",
                leaf::var(meta_param),
                ", ",
                metadata_map_doc,
                ") in call 'logger':'log'(",
                leaf::atom(level.to_string()),
                ", ",
                leaf::var(msg_param),
                ", ",
                leaf::var(merge_var),
                ")"
            ]
        } else {
            docvec![
                "call 'logger':'log'(",
                leaf::atom(level.to_string()),
                ", ",
                leaf::var(msg_param),
                ", ",
                metadata_map_doc,
                ")"
            ]
        };

        let doc = docvec![
            "let ",
            leaf::var(discard_var),
            " = ",
            log_call_doc,
            " in 'nil'"
        ];

        Some(doc)
    }

    /// ADR 0038: Generates code for the `classBuilderRegister` intrinsic.
    ///
    /// Emits a call to `beamtalk_class_builder:register/1` with the builder's
    /// `gen_server` state augmented with the builder's own PID (for cleanup).
    ///
    /// On success: returns the canonical class-object record built by
    /// `beamtalk_class_registry:class_object_from_pid/1`, i.e.
    /// `#beamtalk_object{class = '<Name> class', class_mod = ModuleName, pid = Pid}`
    /// — the same shape produced by `generate_class_reference` and
    /// `beamtalk_interface:handle_class_named/1`, so the value is dispatchable
    /// and `==` to the registry reference (BT-2258).
    /// On error: raises the structured error via `beamtalk_error:raise/1`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Pid = call 'erlang':'self'() in
    /// let _BS = call 'maps':'put'('builderPid', _Pid, State) in
    /// case call 'beamtalk_class_builder':'register'(_BS) of
    ///   <{'ok', _CP}> when 'true' ->
    ///     call 'beamtalk_class_registry':'class_object_from_pid'(_CP)
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
            leaf::var(pid_var.clone()),
            " = call 'erlang':'self'() in ",
            "let ",
            leaf::var(state_var.clone()),
            " = call 'maps':'put'('builderPid', ",
            leaf::var(pid_var),
            ", ",
            leaf::var(current_state),
            ") in ",
            "case call 'beamtalk_class_builder':'register'(",
            leaf::var(state_var),
            ") of ",
            "<{'ok', ",
            leaf::var(class_pid_var.clone()),
            "}> when 'true' -> ",
            // BT-2258: return the canonical class-object shape
            // {'beamtalk_object', <Name> ++ " class", ModuleName, ClassPid}
            // built by the runtime helper, instead of an unusable hardcoded
            // {'beamtalk_object', 'Class', 'beamtalk_class_bt', ClassPid} wrapper.
            "call 'beamtalk_class_registry':'class_object_from_pid'(",
            leaf::var(class_pid_var),
            ") ",
            "<{'error', ",
            leaf::var(error_var.clone()),
            "}> when 'true' -> ",
            "call 'beamtalk_error':'raise'(",
            leaf::var(error_var),
            ") ",
            "end"
        ]
    }
}

#[cfg(test)]
mod tests;

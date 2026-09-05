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
//! - [`threaded_ir`] - BT-3131: `VersionCounter`, the single implementation behind
//!   the state/class-var/self-type-threaded version counters (formerly `state_codegen`)
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
mod class_builder_source;
mod control_flow;
mod dispatch_codegen;
pub mod erlang_types;
mod expressions;
mod gen_server;
mod intrinsics;
mod operators;
pub mod primitive_bindings;
mod primitives;
pub mod selector_mangler;
mod spec_codegen;
mod supervisor_codegen;
mod threaded_expr;
mod threaded_ir;
mod util;
mod value_type_codegen;
mod variable_context;

// Re-export utility functions for IDE queries
pub use beamtalk_cerl_doc::escape::{escape_atom_chars, escape_erlang_string};
pub use util::to_module_name;

use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_cerl_doc::{Document, INDENT, line, nest};
use beamtalk_core::ast::{
    Block, ClassKind, Expression, MessageSelector, Module, WellKnownSelector,
};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;
use primitive_bindings::PrimitiveBindingTable;
use std::collections::HashSet;
use std::fmt;
use thiserror::Error;
use threaded_ir::{VersionCounter, VersionPrefix};
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

    /// BT-3150: A self-send to a same-class class method (`self someSelector`) used
    /// as a statement inside a `whileTrue:`/`timesRepeat:`/`to:do:`/`to:by:do:`
    /// (`BodyKind::Letrec`) loop body in a class method.
    ///
    /// BT-3168 (ADR 0111 Addendum 9): narrowed to a defensive fallback —
    /// `ClassVars` now threads through a `Letrec` loop's own recursive tail
    /// call as an extra fun parameter (`while_loops.rs`/`counted_loops.rs`),
    /// so this is no longer raised for the shape it originally targeted. Kept
    /// as a conservative rejection for the one case that check found
    /// structurally guaranteed not to occur (see
    /// `control_flow::generate_threaded_loop_body_inner`'s
    /// `loop_threads_class_vars`-gated branch) rather than an `unreachable!()`,
    /// per CLAUDE.md's "never panic on user input" rule.
    ///
    /// Every same-class class-method self-send routes through
    /// `emit_class_var_result_unwrap`'s `{class_var_result, Result, ClassVarsN}`
    /// unwrap convention (BT-412) — regardless of whether the callee actually
    /// mutates a class var, since the caller can't know that statically. `Letrec`
    /// threads only the loop's own local-variable `StateAcc` through its
    /// recursive tail call; `ClassVarsN` is never part of that thread, so any
    /// class-var mutation the self-send makes is discarded at the end of every
    /// iteration, and the class method's own final return still hands back the
    /// original pre-loop `ClassVars` regardless — the self-send analog of
    /// BT-3140's finding for direct field writes (`generate_field_assignment_open`'s
    /// State/StateAcc threading has no class-var branch either). Confirmed
    /// empirically: a mutating count stayed at `0` across 3 loop iterations
    /// instead of accumulating — a compile-time rejection is safer than a
    /// silent runtime no-op.
    ///
    /// Deliberately scoped to `Letrec` only, not any `BodyKind::Foldl*` variant
    /// (`do:`/`collect:`/`select:`/`inject:into:`/...): tried and reverted after
    /// two rounds of CI failure against `stdlib/test/fixtures/class_method_block.bt`.
    /// `Letrec`'s own body value is *always* discarded (a `whileTrue:`/
    /// `timesRepeat:`/`to:do:`/`to:by:do:` loop unconditionally evaluates to
    /// `nil`), so a self-send there can only ever be present for a side effect
    /// — no legitimate use depends on its return value, which is what makes
    /// blanket rejection safe.
    /// `Foldl*` bodies don't share that property: they routinely use a
    /// self-send's return value as the fold's own output, and even a
    /// *discarded*, non-last self-send statement is an intentionally-supported,
    /// tested pattern there (BT-2350) with real pure (non-mutating) self-sends
    /// in the fixture above. The identical class-var-mutation-loss bug is
    /// reachable via `Foldl*` too (confirmed empirically for `do:`), but closing
    /// it without also rejecting that legitimate pattern needs either real
    /// `ClassVars` threading through fold accumulators or static purity
    /// analysis of the callee — tracked as a follow-up under BT-3151.
    #[error(
        "Cannot send '{selector}' to self inside this loop body at {location}: \
             a self-send to a class method can't thread class-variable mutations back \
             through a whileTrue:/timesRepeat:/to:do:/to:by:do: loop body — any mutation \
             '{selector}' makes is silently discarded by the time the loop finishes.\n\n\
             Fix: Accumulate what each call needs into a local variable (or collection) \
             inside the loop, then make the self-send(s) once after the loop finishes, \
             outside the threaded body."
    )]
    ClassMethodSelfSendInThreadedLoopBody {
        /// The selector being self-sent.
        selector: String,
        /// Source location.
        location: String,
    },

    /// BT-3151: A self-send to a same-class class method, inside a block that
    /// compiles through `generate_block`'s generic "plain fun"/Tier 2 fallback
    /// (`select:`/`collect:`/`do:`/etc. arguments, a block stored in a local var
    /// then invoked, or any other bare/passed block) in a class method — the
    /// sibling gap BT-3150 left open (see that error's doc comment and ADR
    /// 0110's BT-3150 amendment): unlike a `Letrec`/`FoldlDo` threaded loop
    /// body, this shape has no legitimate "unconditionally discard the
    /// self-send's return value" reading to justify rejecting *every*
    /// self-send here the way BT-3150 does — `select:`/`collect:`/etc. bodies
    /// routinely and correctly use a self-send's return value (confirmed by a
    /// real stdlib fixture, `stdlib/test/fixtures/class_method_block.bt`,
    /// BT-2350), so a blanket rejection would break real code exactly as it
    /// did when first tried for BT-3150's own `Foldl*` widening attempt.
    ///
    /// Resolved instead with `block_analysis::compute_class_var_mutating_selectors`:
    /// a same-class self-send is only rejected here when its target selector is
    /// *not provably free* of class-variable mutation — it (or something it
    /// calls, transitively) writes a class variable directly, or it isn't a
    /// locally defined class method at all (inherited/unresolvable, where
    /// purity can't be checked — conservatively assumed unsafe, the same
    /// "can't know statically" call BT-3150 makes). A self-send to a provably
    /// pure class method (the common case — `self double:`-style helpers)
    /// keeps compiling.
    #[error(
        "Cannot send '{selector}' to self inside this block at {location}: this self-send \
             cannot be proven free of class-variable mutation ('{selector}' either writes a \
             class variable itself, calls another method that might, or isn't defined locally \
             in this class where that could be checked) — and unlike a threaded loop body, this \
             block has no way to thread such a mutation back to the class method that owns it.\n\n\
             Fix: Call '{selector}' directly from the class method's own body instead of from \
             inside this block, or extract whatever the block needs into a helper method \
             that's provably free of class-variable mutation."
    )]
    ClassMethodSelfSendInUnthreadedBlock {
        /// The selector being self-sent.
        selector: String,
        /// Source location.
        location: String,
    },

    /// BT-3140: A class-var assignment (`self.field := ...`) inside a loop, conditional,
    /// exception handler, or list-op body (or any other context reached via
    /// `generate_field_assignment_open`) in a class method.
    ///
    /// BT-3168 (ADR 0111 Addendum 9): no longer raised for a class-var write
    /// directly inside a `Letrec` (`whileTrue:`/`timesRepeat:`/`to:do:`/
    /// `to:by:do:`) loop body that threads `ClassVars` through the loop's
    /// own recursive tail call (`dispatch_codegen.rs`'s
    /// `generate_field_assignment_open` now threads that shape via a real
    /// `Bind` instead) — still raised for every other reachable shape:
    /// `Foldl*` bodies (`do:`/`collect:`/`select:`/`inject:into:`, BT-3169's
    /// territory), the still-open C4 conditional case
    /// (`conditionals.rs`'s `lower_field_assignment_bind`, tracked by
    /// BT-3159), and exception-handler/other list-op bodies.
    ///
    /// `generate_field_assignment_open` threads field writes via the generic
    /// `State`/`StateAcc` map used for Actor instance state and `ValueType` `Self`
    /// threading — it has no class-var branch (unlike `generate_field_assignment`,
    /// BT-412 / ADR 0110's `current_class_var`/`next_class_var`/shadow-write logic).
    /// A class var written from inside one of these bodies therefore threads into
    /// the construct's own scratch state map instead of `ClassVars`, and that map
    /// is discarded once the construct finishes — losing the mutation identically
    /// on both normal return and a foreign non-local-return escape (investigated in
    /// BT-3140; see ADR 0110's Consequences > Negative section, 2026-08-11 amendment).
    #[error(
        "Cannot assign to class variable '{field}' inside this loop/conditional body at {location}.\n\n\
             Class-variable assignments only thread state back to the class's ClassVars map at a \
             class method's own top frame (ADR 0110) — not from inside whileTrue:/timesRepeat:/\
             ifTrue:/do:/... bodies, where the mutation is silently lost on both normal return and \
             a foreign non-local return (BT-3140).\n\n\
             Fix: Accumulate into a local variable inside the loop, then assign the class variable \
             once after the loop:\n\
             \x20 // Instead of:\n\
             \x20 [cond] whileTrue: [self.{field} := self.{field} + 1. ...].\n\
             \x20 \n\
             \x20 // Write:\n\
             \x20 delta := 0.\n\
             \x20 [cond] whileTrue: [delta := delta + 1. ...].\n\
             \x20 self.{field} := self.{field} + delta."
    )]
    ClassVarAssignmentInThreadedBody {
        /// The class variable being assigned.
        field: String,
        /// Source location.
        location: String,
    },

    /// BT-3172 (BT-3168 follow-up): a `Letrec`- or `Foldl*`-shaped loop
    /// (`whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:`/
    /// `do:`/`collect:`/`select:`/`reject:`/`anySatisfy:`/`allSatisfy:`/
    /// `inject:into:`/`detect:`/`count:`/`takeWhile:`/`dropWhile:`/
    /// `partition:`/`groupBy:`) nested inside another such loop — in any
    /// Letrec/Letrec, Foldl*/Foldl*, or mixed combination — where the INNER
    /// loop's own body would thread a `ClassVars` mutation through its own
    /// recursive tail call or fold accumulator, but the OUTER loop's own
    /// top-level statements don't independently trigger `ClassVars`
    /// threading (see `nested_loop_lost_class_var_mutation`'s doc comment
    /// for the exact per-shape trigger).
    ///
    /// For a `Letrec`-in-`Letrec` nesting, the failure mode is silent data
    /// loss: no code path in `generate_threaded_loop_body_inner`/
    /// `emit_non_assign_expr` unpacks a nested loop's `ClassVars` tuple
    /// element back into the outer loop (only `StateAcc`, via the
    /// BT-478/BT-483 last-statement `element(2)` unpack), so the mutation is
    /// silently discarded once the inner loop's own branch exits
    /// (`with_branch_context`'s `class_var_version` restore has nothing to
    /// hand off to). For a nesting involving `Foldl*`, the failure mode is
    /// worse — confirmed empirically to be an `erlc` "unbound variable"
    /// compile crash instead: the inner construct's own `next_class_var()`
    /// mint permanently advances the generator's single, unscoped
    /// class-var-name counter, but the resulting name is only ever bound
    /// inside the inner construct's own (already-exited) closure, not the
    /// enclosing scope the outer loop's own `ClassVars` wrap then tries to
    /// reference it from.
    ///
    /// Extending `ClassVars` threading to propagate through nested loop
    /// levels is tracked as a real design follow-up (needs its own
    /// frame/accumulator design, comparable in scope to ADR 0111 Addendum 9)
    /// — not attempted here, per BT-3172's own risk assessment (this
    /// predicate/generator pair already absorbed two reverted
    /// over-broad-detection attempts during BT-3168's own development).
    #[error(
        "Cannot mutate {mutation} inside a loop nested inside another loop, at {location}.\n\n\
             The inner loop's own mutation would be threaded correctly on its own, but the outer \
             loop (whileTrue:/whileFalse:/timesRepeat:/to:do:/to:by:do:/do:/collect:/select:/\
             reject:/anySatisfy:/allSatisfy:/inject:into:/detect:/count:/takeWhile:/dropWhile:/\
             partition:/groupBy:) has no class-variable mutation of its own to carry it back out \
             — so it is silently discarded, or fails to compile, once \
             the inner loop finishes.\n\n\
             Fix: Accumulate into a local variable across both loops, then mutate the class variable \
             once after the outer loop finishes:\n\
             \x20 // Instead of:\n\
             \x20 [i < n] whileTrue: [\n\
             \x20   [j < n] whileTrue: [self.runs := self.runs + 1. j := j + 1].\n\
             \x20   i := i + 1].\n\
             \x20 \n\
             \x20 // Write:\n\
             \x20 delta := 0.\n\
             \x20 [i < n] whileTrue: [\n\
             \x20   [j < n] whileTrue: [delta := delta + 1. j := j + 1].\n\
             \x20   i := i + 1].\n\
             \x20 self.runs := self.runs + delta."
    )]
    ClassVarMutationLostAcrossNestedLoop {
        /// Description of the inner loop's mutation (e.g. "class variable 'runs'" or "'self bump'").
        mutation: String,
        /// Source location.
        location: String,
    },

    /// Field assignment in a block that can't thread state back — whether the block is
    /// assigned to a variable, passed as an argument, or returned (BT-2792).
    #[error(
        "Cannot assign to field '{field}' inside this block at {location}.\n\n\
             Field assignments only thread state back to the actor when the block is used \
             directly with a control-flow construct (ifTrue:/whileTrue:/do:/collect:/...), \
             sent directly to self, or immediately invoked (`[...] value`, `[...] value: arg`, \
             `[...] value:value:`, etc.) — not when it's stored in a variable, passed to a \
             user-defined method, or returned as a value.\n\n\
             Fix: Use the block directly at the call site, or extract the mutation into a method:\n\
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
    FieldAssignmentInUnsupportedBlock {
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
/// use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module};
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let code = generate_module(&module, CodegenOptions::new("counter")
///     .with_source("value := 0")
///     .with_workspace_mode(true))?;
/// # Ok::<(), beamtalk_codegen::core_erlang::CodeGenError>(())
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
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
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
    /// BT-2887: optional FFI type registry (ADR 0075) threaded to the
    /// return-type writeback pass so methods whose body type is inferred
    /// purely via an FFI call (e.g. `foo => Erlang lists reverse: x`) get
    /// `List` written back to `method_return_types` before codegen.
    native_type_registry:
        Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>>,
    /// BT-2932: type alias declarations (`type Name = ...`) from other
    /// modules in the same compilation unit — the codegen counterpart of
    /// `AnalysisContext`/`ClassHierarchyContext`'s `pre_loaded_aliases`
    /// (BT-2928). Merged with this module's own `module.type_aliases` via
    /// `AliasRegistry::from_module_declarations_with_pre_loaded` so a
    /// cross-module alias reference resolves to a `user_type` reference in
    /// generated `-spec`/`-type` attributes instead of falling through to
    /// `any()`.
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    /// BT-3123: pre-computed analysis outputs from the driver's own
    /// `analyse_full` call. See [`Self::with_analysis`].
    analysis: Option<beamtalk_core::semantic_analysis::AnalysisResult>,
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
            native_type_registry: None,
            pre_loaded_aliases: Vec::new(),
            analysis: None,
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
        classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
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

    /// BT-2887: sets the native FFI type registry (ADR 0075) used by the
    /// return-type writeback pass, from an optional value.
    #[must_use]
    pub fn with_native_type_registry(
        mut self,
        registry: Option<
            std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
        >,
    ) -> Self {
        self.native_type_registry = registry;
        self
    }

    /// BT-2932: sets pre-loaded type alias declarations from other modules
    /// in the same compilation unit, so `generate_module` can resolve a
    /// cross-module alias reference to a `user_type` reference in generated
    /// `-spec`/`-type` attributes instead of falling through to `any()`.
    /// Mirrors [`Self::with_class_hierarchy`]'s pre-loaded-metadata shape.
    #[must_use]
    pub fn with_pre_loaded_aliases(
        mut self,
        aliases: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    ) -> Self {
        self.pre_loaded_aliases = aliases;
        self
    }

    /// BT-3123: threads a driver's already-computed [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult)
    /// into codegen, so `generate_module`/`generate_module_with_warnings` consume
    /// the same class hierarchy, semantic facts, and inferred method return types
    /// the driver's own `analyse_full` call already produced for diagnostics,
    /// instead of re-deriving all three from scratch (see ADR 0006, BT-1288, BT-1005).
    ///
    /// `None` (the default) preserves the previous self-sufficient behaviour —
    /// codegen computes its own analysis internally, including running the
    /// pre-codegen writeback trio (`semantic_analysis::lower_module_for_codegen`)
    /// on its own clone of `module`. Callers that already run `analyse_full`
    /// before codegen (e.g. the CLI build pipeline via
    /// `compile_source_with_bindings`) should always supply it here to avoid
    /// running the type checker twice per compiled module.
    ///
    /// **BT-3125 contract:** when supplying `Some`, the caller is expected to
    /// have already called [`beamtalk_core::semantic_analysis::lower_module_for_codegen`]
    /// on its own `module` — using this same `analysis.class_hierarchy` and
    /// `analysis.method_return_types` — *before* passing `module` to
    /// `generate_module`. Codegen trusts that hand-off (skipping the writeback
    /// trio itself, and the `module.clone()` it used to require) whenever no
    /// cross-file enrichment from `with_class_hierarchy`/
    /// `with_class_superclass_index` adds anything the driver's own hierarchy
    /// didn't already have; codegen still prepares the AST itself in the
    /// rarer case that enrichment invalidates the hand-off. Skipping the
    /// `lower_module_for_codegen` call while still supplying `Some` silently
    /// produces a module missing inferred return types / corrected
    /// `class_kind` / `supervisor_kind` in the common case — see
    /// `generate_module_with_warnings`'s BT-3125 comment.
    #[must_use]
    pub fn with_analysis(
        mut self,
        analysis: beamtalk_core::semantic_analysis::AnalysisResult,
    ) -> Self {
        self.analysis = Some(analysis);
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
/// use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module};
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let core_erlang = generate_module(&module, CodegenOptions::new("counter"))?;
/// println!("{}", core_erlang);
/// # Ok::<(), beamtalk_codegen::core_erlang::CodeGenError>(())
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

    // BT-3123: Consume the driver's already-computed analysis when supplied
    // (`CodegenOptions::with_analysis`) instead of re-deriving semantic facts,
    // the class hierarchy, and inferred method return types from scratch —
    // eliminating a second full type-checking pass per compiled module. `None`
    // preserves the previous self-sufficient behaviour for callers that don't
    // run analysis separately (unit tests, ad-hoc codegen).
    let (mut hierarchy, analysis_handed_off, mut driver_method_return_types) =
        if let Some(analysis) = options.analysis {
            generator.semantic_facts = analysis.semantic_facts;
            // BT-3217: carry the driver's already-computed `TypeMap` through
            // for `recv_type` projection. May be superseded below if this
            // generation's own cross-file enrichment invalidates the
            // hand-off and forces a fuller re-inference pass.
            generator.type_map = analysis.type_map;
            (
                analysis.class_hierarchy,
                true,
                Some(analysis.method_return_types),
            )
        } else {
            // BT-1288: Compute semantic facts before codegen begins.
            generator.semantic_facts =
                beamtalk_core::semantic_analysis::compute_semantic_facts(module);

            // Build hierarchy once for the entire generation (ADR 0006)
            let (hierarchy_result, _) =
                beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(module);
            let hierarchy = hierarchy_result
                .map_err(|e| CodeGenError::Internal(format!("hierarchy: {e:?}")))?;
            (hierarchy, false, None)
        };

    // ADR 0050 Phase 4: inject richer user-class entries from BEAM metadata first,
    // so that add_external_superclasses (which uses contains_key before inserting)
    // does not overwrite BEAM data with partial stubs. Both calls are no-ops for
    // classes the handed-off analysis hierarchy already carries (BT-1523/BT-894
    // insert only into vacant entries) — the common case, since a driver that
    // hands off analysis typically fed the same cross-file class metadata to
    // `AnalysisContext::with_pre_loaded_classes`. `class_superclass_index`
    // (BT-894) is codegen-only and has no `AnalysisContext` counterpart, so it
    // can still add genuinely new stub entries analysis never saw; both calls
    // report whether they did so the lowering step below knows whether the
    // handed-off AST preparation is still trustworthy.
    let added_beam_meta = hierarchy.add_from_beam_meta(options.pre_class_hierarchy);

    // BT-894: Backfill missing cross-file superclass stubs (only for classes not
    // already present from build() or BEAM metadata).
    let added_superclasses = hierarchy.add_external_superclasses(&options.class_superclass_index);

    // BT-3125: A driver that handed off `AnalysisResult` is expected to have
    // already called `semantic_analysis::lower_module_for_codegen` on its own
    // module — using the very same `class_hierarchy`/`method_return_types` —
    // *before* invoking `generate_module`, per `CodegenOptions::with_analysis`'s
    // contract. When that hand-off is still trustworthy (no cross-file
    // enrichment above added anything the driver's own hierarchy didn't have),
    // codegen no longer schedules the writeback trio itself: it trusts the
    // already-prepared `module` it was given and skips the clone entirely.
    //
    // Two cases still require preparing the AST here, exactly as before
    // BT-3125: no analysis was handed off at all (self-sufficient codegen —
    // unit tests, ad-hoc codegen, REPL trace mode), or this generation's own
    // cross-file enrichment (above) added stub classes the driver's
    // `lower_module_for_codegen` call never saw, making its writeback
    // possibly incomplete for this generation's fuller view of the hierarchy.
    let mut module_owned;
    let module: &Module = if analysis_handed_off && !added_beam_meta && !added_superclasses {
        // BT-3249: `module` is used exactly as the driver prepared it (no
        // clone/re-infer below), so the driver's own `method_return_types`
        // map is precisely "which methods did inference write a return type
        // into" for *this* `module` — record it for `extract_method_source`
        // to strip before baking image-resident `__source__` text. `.take()`
        // rather than `.clone()`: `driver_method_return_types` is only read
        // again in the `else` arm below, which this branch never executes.
        generator.method_return_types_written_back =
            driver_method_return_types.take().unwrap_or_default();
        module
    } else {
        module_owned = module.clone();
        // A driver that handed off `AnalysisResult` already wrote its
        // (narrower-hierarchy) inference into `module_owned` before we got
        // here — but `added_beam_meta`/`added_superclasses` just proved that
        // hand-off stale. Undo exactly those driver-written entries before
        // re-inferring: both `infer_method_return_types` (via
        // `resolve_self_delegate_return_type`, which trusts an
        // already-populated `return_type` as a declared annotation) and
        // `apply_return_type_writeback_from_map` only fill in a `None`
        // `return_type`, so without this the "fuller hierarchy" recompute
        // below would silently be a no-op for every method the driver's
        // pass already answered. `written_by` only ever contains
        // inference-derived keys, so this can never clear a genuine user
        // annotation (see `clear_return_type_writeback_for_keys`'s doc).
        if let Some(written_by) = &driver_method_return_types {
            beamtalk_core::semantic_analysis::clear_return_type_writeback_for_keys(
                &mut module_owned,
                written_by,
            );
        }
        // BT-3217 (ADR 0115 Phase 2 spike §1d): `infer_types_and_returns`
        // returns both `TypeMap` and `method_return_types` from the same
        // single `TypeChecker` pass `infer_method_return_types` already ran
        // — zero extra inference. Refreshes `generator.type_map` for this
        // (possibly fuller, re-inferred) hierarchy, superseding whatever the
        // `Some(analysis)` branch above set from the driver's now-stale
        // hand-off.
        let (type_map, method_return_types) =
            beamtalk_core::semantic_analysis::type_checker::infer_types_and_returns(
                &module_owned,
                &hierarchy,
                options.native_type_registry.as_deref(),
            );
        generator.type_map = type_map;
        beamtalk_core::semantic_analysis::lower_module_for_codegen(
            &mut module_owned,
            &hierarchy,
            &method_return_types,
        );
        // BT-3249: record which methods *this* (re-)inference wrote a
        // return type into, for `extract_method_source` to strip before
        // emitting image-resident `__source__` text — see the field's doc.
        generator.method_return_types_written_back = method_return_types;
        &module_owned
    };

    // BT-2932: build the alias registry once, merging this module's own
    // `type_aliases` with any pre-loaded aliases from other modules in the
    // same compilation unit, so a cross-module alias reference resolves to
    // a `user_type` reference instead of falling through to `any()` in
    // generated `-spec`/`-type` attributes.
    generator.alias_registry =
        beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::from_module_declarations_with_pre_loaded(
            module,
            &options.pre_loaded_aliases,
        );

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
// BT-3340: widened from `pub(crate)` — `beamtalk-repl` sets `context` to
// `CodeGenContext::Repl` on the generator it owns.
pub enum CodeGenContext {
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
/// (the class-method boundary variant, `wrap_class_method_body_with_nlr_catch`, was
/// deleted by BT-3164 once its sole caller — `generate_class_method_fun_from_block` —
/// migrated to prepending a real `ThreadedStmt::NlrCatch` instead; the Actor boundary
/// variant, `wrap_actor_body_with_nlr_catch`, was deleted the same way by BT-3171 once
/// its last of 3 remaining callers migrated) and by
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

/// Renders a `VersionPrefix::State` counter value, honoring loop context —
/// the single shared implementation behind `current_state_var`/
/// `next_state_var`/`peek_next_state_var` (this impl block) and
/// `threaded_ir::RenderCtx::resolve_prefix` (ADR 0111 §Addendum, "Renderer
/// design sketch": "prefix rendering is a function of (counter, loop
/// context), decided at Document-construction time, not stored in the
/// IR" — CLAUDE.md's no-duplicate-implementations rule pins that decision
/// to exactly one place instead of leaving the live-generator and
/// `ThreadedIr`-renderer paths to duplicate/drift it independently).
///
/// Hybrid-params loops (`in_hybrid_loop = true`) use `State`/`StateN` —
/// same as normal (non-loop) context — because `State` is an explicit fun
/// parameter there, not a `StateAcc` map. Normal loop bodies
/// (`in_loop_body = true`, `in_hybrid_loop = false`) use `StateAcc`/
/// `StateAccN`.
fn render_state_prefix(in_hybrid_loop: bool, in_loop_body: bool, version: usize) -> String {
    let prefix = if in_hybrid_loop || !in_loop_body {
        "State"
    } else {
        "StateAcc"
    };
    util::versioned_var(prefix, version)
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
    /// BT-3151: Selector names of class methods (in the current class) that are
    /// known or suspected to mutate a class variable, directly or transitively
    /// — see `block_analysis::compute_class_var_mutating_selectors`. Used to
    /// let a self-send to a provably pure class method compile inside a bare,
    /// unthreaded block (`select:`/`collect:`/`do:`/etc.) while rejecting one
    /// that may mutate class state there, where BT-3150's `Letrec`-only guard
    /// doesn't reach.
    pub class_var_mutating_selectors: std::collections::HashSet<String>,
    /// BT-412/BT-3131: State version counter for class variable threading.
    ///
    /// Not `pub` (unlike this struct's other fields) — [`VersionCounter`] is
    /// `pub(super)` within `threaded_ir`, narrower than `ClassContext`'s own
    /// `pub(super)` (= `pub(in crate)`); all access stays inside
    /// `mod.rs` via the `class_var_version()`/`set_class_var_version()`
    /// accessor methods, exactly as before.
    class_var_version: VersionCounter,
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
    class_var_mutating_selectors: std::collections::HashSet<String>,
    class_var_version: usize,
    class_var_mutated: bool,
    class_slot_constructor_selector: Option<String>,
    builder_class_method_class: Option<String>,
    // BT-3289: unlike `class_var_version` above, the instance-`State` version
    // counter lives outside `ClassContext` (it's shared by every class, not
    // per-class-context), so nothing captured it here even though
    // `generate_class_method_fun_from_block` unconditionally resets it. A
    // builder cascade nested inside an enclosing method's own field-assignment
    // value (e.g. `self.x := Object classBuilder … addClassMethod:body: […]; register`)
    // would silently clobber the enclosing method's in-progress `State`
    // version, producing two bindings for the same version — caught by the
    // ADR-0111 verifier as `NonLinearVersion`.
    state_version: usize,
    // BT-3300: same unguarded-reset shape as `state_version` above, for the
    // enclosing method's own parameter list/types. `generate_class_method_fun_from_block`
    // unconditionally clears both (`current_method_params.clear()` /
    // `clear_method_param_types()`) to give the class-method fun its own fresh
    // set — but they too live outside `ClassContext`, so a builder cascade
    // nested inside an enclosing method's body would wipe out the enclosing
    // method's real parameters/types for any later statement that reads them
    // (e.g. the `erlangApply`/`erlangModuleLookup` FFI intrinsics, or
    // primitive-BIF codegen).
    current_method_params: Vec<String>,
    current_method_param_types: std::collections::HashMap<String, String>,
}

impl ClassContext {
    /// Creates a new `ClassContext` with default values.
    fn new() -> Self {
        Self {
            class_identity: None,
            class_var_names: std::collections::HashSet::new(),
            class_method_selectors: std::collections::HashSet::new(),
            class_var_mutating_selectors: std::collections::HashSet::new(),
            class_var_version: VersionCounter::new(),
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
    /// BT-833/BT-3131: Self-threading version counter for value type field assignments.
    ///
    /// Mirrors `state_threading` for value types. Each field assignment increments
    /// this counter: `Self` → `Self1` → `Self2` → ... so that `self` in expression
    /// position always resolves to the latest immutable snapshot.
    ///
    /// Not `pub` (unlike `current_nlr_token`) — [`VersionCounter`] is
    /// `pub(super)` within `threaded_ir`, narrower than `ValueTypeContext`'s
    /// own `pub(super)`; all access stays inside `mod.rs` via the
    /// `self_version()`/`set_self_version()` accessor methods, exactly as
    /// before.
    self_version: VersionCounter,
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
            self_version: VersionCounter::new(),
            current_nlr_token: None,
        }
    }
}

/// One entry of [`CoreErlangGenerator::precompiled_subexprs`] — see that
/// field's doc comment.
struct PrecompiledSubexpr {
    /// The already-compiled value to substitute for the node.
    doc: Document<'static>,
    /// Whether a hit should wrap `doc` in the BT-940 source-line
    /// annotation `generate_expression` gives every closed message send —
    /// `true` only for a producer's own result reference (which never
    /// went through `generate_expression`), so a sequenced self-send
    /// renders byte-identically to the planner's substitution; `false` for
    /// a sequencing temp or a value `generate_expression` already built.
    ///
    /// INVARIANT: an `annotate: true` doc must be a *closed* expression —
    /// `( doc -| [line] )` around an open `let … in ` chain is invalid Core
    /// Erlang (the hazard the `MessageSend` arm's open-scope guard exists
    /// for). Today the only `true` registration is
    /// `CoreErlangGenerator::self_dispatch_result_value`'s fixed
    /// `call 'erlang':'element'(1, _SD)` shape; `take_precompiled_subexpr`
    /// additionally re-applies that arm's guard as defence in depth.
    annotate: bool,
    /// Set on the first hit; a never-hit entry is an invariant violation
    /// reported by `finish_precompiled_scope`.
    used: bool,
}

/// The set of [`CoreErlangGenerator::precompiled_subexprs`] entries one
/// sequencing pass registered — returned by the pass, handed back to
/// [`CoreErlangGenerator::finish_precompiled_scope`] once the parent has
/// been compiled. `#[must_use]`: dropping it leaks entries into the next
/// statement and skips the consulted-exactly check.
#[must_use = "hand this back to finish_precompiled_scope once the parent is compiled"]
pub(super) struct PrecompiledScope(Vec<Span>);

impl PrecompiledScope {
    pub(super) fn new() -> Self {
        Self(Vec::new())
    }
}

impl CoreErlangGenerator {
    /// ADR 0118 phase 1a (BT-3415): records `expr`'s already-sequenced
    /// value so the enclosing parent's ordinary compile substitutes it —
    /// see [`Self::precompiled_subexprs`]. Keyed by the paren-unwrapped
    /// span: `generate_expression`'s `Parenthesized` arm recurses, and
    /// every `unwrap_parens()`-first path reaches the inner node, so the
    /// inner span is the one every route converges on.
    ///
    /// `annotate` may be `true` only for a closed expression document —
    /// see [`PrecompiledSubexpr::annotate`]'s invariant.
    ///
    /// # Errors
    ///
    /// Two live scopes registering the same node would let the inner
    /// `finish_precompiled_scope` remove the entry out from under the
    /// outer one, whose consulted-exactly check would then pass vacuously
    /// while the parent compiled the child afresh — a double dispatch with
    /// no error. A duplicate registration is therefore a hard
    /// [`CodeGenError::Internal`] in every build profile (a diagnostic in
    /// `codegen_warnings` would be discarded by the CLI's build path).
    pub(super) fn register_precompiled_subexpr(
        &mut self,
        scope: &mut PrecompiledScope,
        expr: &Expression,
        doc: Document<'static>,
        annotate: bool,
    ) -> Result<()> {
        let span = expr.unwrap_parens().span();
        if self.precompiled_subexprs.contains_key(&span) {
            return Err(CodeGenError::Internal(format!(
                "ADR 0118 sequencing: sub-expression at {span:?} registered twice"
            )));
        }
        self.precompiled_subexprs.insert(
            span,
            PrecompiledSubexpr {
                doc,
                annotate,
                used: false,
            },
        );
        scope.0.push(span);
        Ok(())
    }

    /// ADR 0118 phase 5b (BT-3422): `true` if `expr` (any nesting of
    /// parens) was already registered by an enclosing `sequence_children`
    /// call — a pure, non-consuming check for a caller deciding whether to
    /// re-thread `expr` itself (wrong: double-dispatch) or read the
    /// substitution back via the ordinary `expression_doc`/
    /// `take_precompiled_subexpr` path.
    pub(super) fn precompiled_subexprs_contains(&self, expr: &Expression) -> bool {
        self.precompiled_subexprs
            .contains_key(&expr.unwrap_parens().span())
    }

    /// The `generate_expression` entry hook for
    /// [`Self::precompiled_subexprs`]: `Some(doc)` if `expr` was
    /// pre-sequenced, marking the entry consulted.
    fn take_precompiled_subexpr(&mut self, expr: &Expression) -> Option<Document<'static>> {
        if self.precompiled_subexprs.is_empty() {
            return None;
        }
        let span = expr.span();
        let (doc, annotate) = {
            let entry = self.precompiled_subexprs.get_mut(&span)?;
            entry.used = true;
            (entry.doc.clone(), entry.annotate)
        };
        // Never annotate while an open let-chain is in flight — an
        // annotated open chain is invalid Core Erlang (BT-940). Defence in
        // depth over the closed-doc invariant on `PrecompiledSubexpr::annotate`.
        if annotate && self.can_annotate_closed_expression() {
            if let Some(line_num) = self.span_to_line(span) {
                return Some(self.annotate_with_line(doc, line_num));
            }
        }
        Some(doc)
    }

    /// BT-940: whether the expression just produced may be wrapped in a
    /// source-line annotation — only a CLOSED expression can be; an open
    /// let-chain (a class-method send, a class-var assignment, a
    /// direct-params list op) ends in a dangling `in ` that `( expr -|
    /// [annotation] )` would break. The single predicate behind
    /// `generate_expression`'s `MessageSend` arm and
    /// [`Self::take_precompiled_subexpr`], so a new open-scope side channel
    /// only has to be added here.
    fn can_annotate_closed_expression(&self) -> bool {
        !self.direct_params_do_open_chain && self.direct_params_list_op_result.is_none()
    }

    /// Removes every entry `scope` registered, once the parent compile
    /// that was meant to consult them is done. An entry that was never
    /// consulted means that compile bypassed `generate_expression` for the
    /// child — its prelude already ran (or its temp is already bound) but
    /// the parent compiled the child afresh, so a state-effecting child
    /// would dispatch twice: an internal error, never a silent drop.
    pub(super) fn finish_precompiled_scope(&mut self, scope: PrecompiledScope) -> Result<()> {
        let mut unused = Vec::new();
        for span in scope.0 {
            if let Some(entry) = self.precompiled_subexprs.remove(&span) {
                if !entry.used {
                    unused.push(span);
                }
            }
        }
        if let Some(span) = unused.first() {
            return Err(CodeGenError::Internal(format!(
                "ADR 0118 sequencing: a pre-sequenced sub-expression at {span:?} was never \
                 substituted by its parent's compile (the parent's codegen path bypasses \
                 generate_expression for that child)"
            )));
        }
        Ok(())
    }
}

// Core Erlang code generator.
//
// This is the main code generator that coordinates compilation of Beamtalk
// AST nodes to Core Erlang. It maintains:
//
// - Module name: The Erlang module being generated
// - Output buffer: Accumulated Core Erlang code
// - Variable context: Scope management and variable generation
// - State threading: Simulated mutation via State, State1, State2...
//
// ADR 0118 phase 5b (BT-3422): the ClassVars "open let-chain" protocol
// (a `Document`-side channel every consumer had to know to keep open) is
// deleted — a class-var producer's `Bind` is now a real
// `threaded_ir::ThreadedValue` prelude entry, spliced or closed at each
// consumer directly. The one remaining signal that old side channel used
// to carry that ISN'T a class-var Bind: a mutation-threaded `do:`/dict-`do:`
// nested in a direct-params loop (`in_direct_params_loop`) has several
// rebound accumulator vars, not one meaningful result, so it can only
// signal "the chain I return stays open, and answers `nil`" — never a
// value to reference by name (BT-3053).
// `CoreErlangGenerator::direct_params_do_open_chain` carries just that
// narrower signal; see its own doc comment.

/// BT-3131/BT-1449: RAII guard for [`CoreErlangGenerator::with_branch_context`]'s
/// per-prefix save/reset/restore discipline (ADR 0111 §Phase A2). Replaces the
/// previous manual save-before/restore-after sequencing with a `Drop` impl
/// that restores unconditionally when the guard goes out of scope — including
/// through an early return via `?` inside the branch closure, which the old
/// manual-restore-after-the-call sequencing could not cover.
///
/// Per-prefix branch discipline, preserved exactly (state, `class_vars`) or
/// decided (self) by BT-3131:
/// - **state**: reset to 0 on entry, restored on exit.
/// - **`class_vars`**: NOT reset on entry (the branch inherits the outer
///   scope's current version) but restored on exit. `class_var_mutated` is
///   intentionally NOT restored — BT-1550: it is a method-level flag that
///   must stay sticky once set.
/// - **self**: BT-3131 decision, revised during review — saved and restored
///   on exit, but **NOT reset to 0 on entry**: the same discipline as
///   `class_vars`, not `state`. `state`'s reset is safe because `state`
///   inside a loop body renders as `StateAcc{N}` (a context-dependent
///   rename, `in_loop_body`), so a reset only affects that local rendering
///   convention. `Self{N}` has no such rename — `self.field` reads compile
///   directly to `maps:get(field, Self{N})` — and `Self` (version 0, the
///   bare method parameter) is always a syntactically valid Core Erlang
///   variable, so resetting to 0 does not fail to compile: it silently
///   reads the pre-mutation value. `generate_threaded_loop_body` calls
///   `with_branch_context` unconditionally and is shared by `ValueType`
///   contexts (confirmed empirically: a `self.field := ...` assignment
///   followed by a `do:`/conditional body in the same method that reads
///   `self.field` produced `maps:get(field, Self)` instead of
///   `maps:get(field, Self1)` under a reset-on-entry policy). The original
///   "no current call site enters `with_branch_context` while
///   `self_version` is non-zero" claim was wrong. Before BT-3131,
///   `self_version` was neither saved nor restored here at all — the "live
///   landmine" the issue's ADR calls out; giving it `class_vars`' discipline
///   (not `state`'s) closes that landmine without introducing this one.
struct BranchContextGuard<'a> {
    generator: &'a mut CoreErlangGenerator,
    saved_in_loop: bool,
    saved_state_version: usize,
    saved_class_var_version: usize,
    saved_self_version: usize,
    saved_loop_threads_class_vars: bool,
}

impl Drop for BranchContextGuard<'_> {
    fn drop(&mut self) {
        self.generator.in_loop_body = self.saved_in_loop;
        self.generator.set_state_version(self.saved_state_version);
        self.generator
            .set_class_var_version(self.saved_class_var_version);
        self.generator.set_self_version(self.saved_self_version);
        self.generator.loop_threads_class_vars = self.saved_loop_threads_class_vars;
        // class_var_mutated intentionally NOT restored — sticky (BT-1550).
    }
}

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
// BT-3340: widened from `pub(crate)` to `pub` (ADR 0117 Decision step 2),
// which brought this struct under the `missing_debug_implementations` lint
// (public-only). Not deriving `Debug`: several fields (e.g.
// `PrimitiveBindingTable`) are internal codegen state with no existing
// `Debug` impl, and this struct was never meant to be inspected/printed —
// only constructed and driven through its own methods.
#[allow(missing_debug_implementations)]
// BT-3340: widened from `pub(crate)` — the standalone `beamtalk-repl` crate
// (ADR 0117 Decision step 2) builds `CoreErlangGenerator` directly and reads
// its REPL-relevant state. Its many other fields stay module-private; only
// this struct and the specific members `beamtalk-repl` touches are `pub`.
pub struct CoreErlangGenerator {
    /// The module name being generated (ref-counted for O(1) clone).
    pub module_name: EcoString,
    /// Variable binding and scope management.
    var_context: VariableContext,
    /// State threading for field assignments. BT-3131: `VersionCounter` is the
    /// single implementation shared with `ClassContext::class_var_version` and
    /// `ValueTypeContext::self_version` (formerly `StateThreading`).
    state_threading: VersionCounter,
    /// BT-153: Whether we're inside a loop body (use `StateAcc` instead of `State`)
    in_loop_body: bool,
    /// BT-3146 (ADR 0111 Addendum 5, §Branch-context version discipline):
    /// monotonic counter minting a fresh [`threaded_ir::FrameId`] per
    /// [`Self::enter_branch_context`] call — every `with_branch_context` arm
    /// (conditional branch, `on:do:`/`ensure:` body, loop body) gets its own
    /// distinct frame identity, never reused, so sibling arms that legitimately
    /// reach the same `State` version in disjoint scopes are modeled as
    /// distinct producer/consumer identities rather than colliding as a false
    /// [`threaded_ir::VerifyError::NonLinearVersion`]. `0` is reserved for
    /// [`threaded_ir::FrameId::ROOT`] (the method's own entry frame, never
    /// allocated by this counter); the first `enter_branch_context` call
    /// mints frame `1`. Never reset — frame identity must stay unique across
    /// an entire module compile, not just within one method.
    branch_frame_counter: u32,
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
    /// BT-3168 (ADR 0111 Addendum 9, Questions 2/3/4): whether the generator
    /// is directly inside a Letrec loop body that threads a `ClassVars`
    /// mutation through its own recursive tail call. When `true`,
    /// `generate_field_assignment_open`'s class-var branch threads the write
    /// via a real, `current_branch_frame()`-tagged `Bind` instead of calling
    /// `reject_class_var_field_assignment`, and the `BodyKind::Letrec`
    /// same-class self-send branch in `generate_threaded_loop_body_inner`
    /// emits the self-send (via its own `emit_class_var_result_unwrap` open
    /// chain) instead of raising `ClassMethodSelfSendInThreadedLoopBody`.
    /// Reset to `false` on every `enter_branch_context` entry (mirroring
    /// `state_version`'s reset-on-entry discipline, not `class_var_version`'s
    /// restore-without-reset one) and restored on exit, so it can never leak
    /// from an enclosing Letrec loop into a nested construct
    /// (conditional, `sort:`'s manually-inlined body, a nested Foldl body, …)
    /// that doesn't understand this loop's specific tuple-shape convention.
    /// `generate_threaded_loop_body` is the only place that sets it `true`,
    /// immediately after entry, from that specific call's own
    /// `ThreadingPlan::threads_class_vars`.
    loop_threads_class_vars: bool,
    /// BT-3168: the current Letrec loop body's final in-body `ClassVars`
    /// name (`current_class_var()`, captured just before
    /// `with_branch_context`'s guard restores `class_var_version` to its
    /// pre-loop value), stashed by `generate_threaded_loop_body` for
    /// `while_loops.rs`/`counted_loops.rs` to read immediately afterward as
    /// the loop's own recursive-tail-call `ClassVars` argument. `Some` only
    /// when that call's `plan.threads_class_vars` was `true`; consumed
    /// (`Option::take`) by the reader so a stale value can never leak into
    /// an unrelated later loop.
    last_loop_class_var: Option<String>,
    /// BT-1329: When a list op in direct-params mode generates an open let-chain
    /// (omitting the trailing result expression), it stores the result variable name
    /// here so the caller can append `let AssignedVar = <result_var> in` separately.
    /// `None` when no list op result is pending.
    direct_params_list_op_result: Option<String>,
    /// BT-3169: side channel from [`control_flow::CoreErlangGenerator::generate_threaded_loop_body_inner`]'s
    /// `ClassVars`-threading wrap to [`control_flow::ThreadingPlan::foldl_call_doc`] —
    /// the peak `class_var_version` reached *inside* a `Foldl*` body's own
    /// `with_branch_context` scope (captured just before that scope's guard
    /// restores the live counter to its pre-loop value on drop, per
    /// `BranchContextGuard`'s `class_var_version` restore-without-reset
    /// discipline, shared with conditionals/`on:do:`/`ensure:`).
    ///
    /// Needed because Core Erlang requires globally unique variable names
    /// across nested `fun` scopes within one compiled function (confirmed
    /// empirically — `erlc` rejects a reused name with "unbound variable",
    /// not a "shadowing" diagnostic): the fold body's own internal
    /// `emit_class_var_result_unwrap` self-send rebind mints
    /// `ClassVars1`..`ClassVars{peak}` starting from the SAME pre-loop
    /// version the (restored) live counter sits at again once
    /// `generate_threaded_loop_body` returns — so a naive `next_class_var()`
    /// call right after would mint an already-used name.
    /// [`control_flow::ThreadingPlan::foldl_call_doc`] consumes (takes) this
    /// field to fast-forward the live counter past the peak before minting
    /// the post-fold rebind, guaranteeing a fresh name. `None` when the fold
    /// body did not thread `ClassVars` (`plan.threads_class_vars == false`)
    /// or hasn't run yet.
    last_foldl_class_var_peak: Option<usize>,
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` sets this to
    // `CodeGenContext::Repl` around its own generation calls.
    pub context: CodeGenContext,
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
    /// BT-2710 follow-up: maps an instance field's source name → its declared
    /// `Simple` type name, for the operator fast-path classifiers. Lets a
    /// `self.<field>` read with an explicit **non-primitive** (object) type be
    /// routed through the runtime guard so it dispatches (e.g. `self.lo < x`
    /// where `lo :: Money` reaches `Money>><`) instead of silently term-ordering
    /// the tagged map. Primitive-typed and *untyped* fields are absent from the
    /// guarding decision and stay on the bare BIF, preserving the counter /
    /// accumulator hot paths and their state-threading optimisations. Populated
    /// at value-type / actor class entry; cleared in extension bodies (which
    /// don't carry the target class's field types).
    current_class_field_types: std::collections::HashMap<String, String>,
    /// ADR 0118 phase 5b (BT-3422): narrow side-channel — set deep inside
    /// `generate_expression` (`generate_list_do_with_mutations`/
    /// `generate_dict_do_with_mutations`) exactly when a mutation-threaded
    /// `do:`/dict-`do:` nested in a direct-params loop (`in_direct_params_loop`)
    /// leaves its returned `Document` as an open, dangling let-chain that
    /// answers `do:`'s own `nil` contract rather than a single value
    /// (BT-3053 — see the doc comment above [`CoreErlangGenerator`]). Read
    /// by [`Self::threaded_expression`]'s generic fallback, which converts
    /// it into the `ThreadedValue` shape every other producer already
    /// returns (`value: ValueRef::Literal("'nil'")`), and by the annotation
    /// guard in `generate_expression` to skip line annotations on the open
    /// chain. Reset to `false` by [`Self::threaded_expression`] before each
    /// compile it wraps this way; never read or written anywhere else.
    direct_params_do_open_chain: bool,
    /// ADR 0118 phase 1a (BT-3415): sub-expressions `threaded_expression`'s
    /// sequencing rule (`util.rs`) has already compiled — each one's value
    /// (a sequencing temp, or a state-effecting producer's pure result
    /// reference) keyed by the sub-expression's paren-unwrapped `Span`.
    /// `generate_expression` consults this FIRST, for every node, so when
    /// the enclosing parent is compiled through its ordinary AST-directed
    /// path it substitutes the already-sequenced value instead of
    /// compiling (and, for a self-send, dispatching) the child a second
    /// time. Entries are scoped by [`PrecompiledScope`]: registered by the
    /// sequencing helper, removed by `finish_precompiled_scope` right after
    /// the parent's compile, which also fails loudly if any entry was never
    /// consulted — the parent's compile path bypassed `generate_expression`
    /// for that child, so its prelude ran without its value being used
    /// (a double dispatch or a dropped operand, never silently).
    ///
    /// This started as the phase-1a substitution mechanism for the one
    /// parent kind the sequencing rule covered (message sends, incl. binary
    /// operators); ADR 0118 phase 2b (BT-3418) deleted the planner-driven
    /// consumers this used to run alongside (`hoisted_self_send_results`/
    /// `hoisted_field_reads`), so this is now the ONLY substitution
    /// mechanism a `threaded_expression`/`thread_ahead` caller relies on.
    precompiled_subexprs:
        std::collections::HashMap<beamtalk_core::source_analysis::Span, PrecompiledSubexpr>,
    /// BT-845/BT-860: Source file path to embed as `beamtalk_source` module attribute.
    /// Set from `CodegenOptions::source_path` before generation begins.
    source_path: Option<String>,
    /// BT-851: Tier 2 block parameters for the current method being compiled.
    ///
    /// When a method parameter name is in this set, `value:` / `value:value:` calls
    /// on that parameter use the stateful Tier 2 protocol:
    /// `apply _Fun(Args..., State) → {Result, NewState}`.
    tier2_block_params: std::collections::HashSet<String>,
    /// BT-2797: Local variables in the current method/block body known to hold
    /// a Tier 2 block value — i.e. a `var := [block]` assignment where the
    /// block literal has captured-local or field mutations, *and* every later
    /// reference to `var` in the same body is a safe `value`/`value:`/etc.
    /// call (proven by `prescan_tier2_local_vars`, which runs once at the top
    /// of `lower_body_exprs_with_reply` before classification starts).
    /// `value:` / `value:value:` calls on a variable in this set use the
    /// stateful Tier 2 protocol: `apply _Fun(Args..., State) → {Result, NewState}`.
    /// A block whose safety can't be proven (returned, passed elsewhere,
    /// reassigned, ...) is deliberately left out — it keeps hitting the
    /// `generate_block`/`validate_stored_closure` compile-time diagnostic
    /// instead, since no known call site would thread state through it.
    tier2_local_vars: std::collections::HashSet<String>,
    /// BT-2815: For each name in `tier2_local_vars` whose assigned block's
    /// only mutation is a captured outer local (not a field write), the
    /// names of those captured locals — mirrors what `captured_mutations_for_block`
    /// computes for an inline block literal, but keyed by variable name so a
    /// later `value(:...)` call site (which only has an identifier, not the
    /// block AST) can still find them. Populated alongside `tier2_local_vars`
    /// in `prescan_tier2_local_vars`; consulted by
    /// `get_inline_block_captured_mutations` to rebind the caller's own
    /// variable after the call, the same way it already does for an inline
    /// block literal receiver.
    tier2_local_var_captured_mutations: std::collections::HashMap<String, Vec<String>>,
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
    pub(super) semantic_facts: beamtalk_core::semantic_analysis::SemanticFacts,
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
    pub(super) class_hierarchy:
        Option<beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy>,
    /// ADR 0098 Phase 3: producing `BEAMTALK_VERSION`, baked into `__beamtalk_meta`.
    /// Supplied by the CLI; `None` for REPL/test codegen (key omitted).
    beamtalk_version: Option<EcoString>,
    /// ADR 0098 Phase 3: producing compound OTP version (`<release>-<erts>`),
    /// baked into `__beamtalk_meta`. Supplied by the CLI; `None` omits the key.
    otp_release: Option<EcoString>,
    /// BT-2932: cross-module-aware alias registry for this generation —
    /// this module's own `type_aliases` merged with any pre-loaded aliases
    /// from other modules in the same compilation unit
    /// (`CodegenOptions::pre_loaded_aliases`). Populated by
    /// `generate_module_with_warnings` before codegen begins; consumed by
    /// the `generate_class_specs`/`generate_method_spec`/
    /// `generate_type_alias`/`generate_alias_type_attrs` call sites in
    /// `actor_codegen.rs`, `value_type_codegen.rs`, `supervisor_codegen.rs`,
    /// and `gen_server/native_facade.rs` so an alias-typed annotation
    /// resolves to a `user_type` reference regardless of which module
    /// declared the alias. Empty (not `None`) when the module has no
    /// `type_aliases` and no pre-loaded aliases were supplied — mirrors
    /// `AliasRegistry::from_module_declarations`'s empty-registry default,
    /// so every downstream `Some(&self.alias_registry)` call site is a
    /// no-op for the common case.
    pub(super) alias_registry: beamtalk_core::semantic_analysis::alias_registry::AliasRegistry,
    /// BT-3217 (ADR 0115 Phase 2): per-expression inferred types (keyed by
    /// file-absolute `Span`), sourced from the driver's handed-off
    /// `AnalysisResult::type_map` when `CodegenOptions::with_analysis` was
    /// used, or from `infer_types_and_returns` in the self-sufficient path
    /// (`generate_module_with_warnings`, `mod.rs:944`). Consumed by
    /// `gen_server/methods.rs::build_method_xref_entry` to project each
    /// send's receiver type onto the xref `recv_type` field — see
    /// `docs/internal/adr-0115-phase1-spike-findings.md` §1 for why this
    /// field previously had no plumbing path into codegen. Empty (not
    /// populated) for codegen contexts that never ran type inference (rare
    /// unit-test-only paths); `recv_type` degrades safely to `dynamic` in
    /// that case, matching the runtime live-patch path's precedent.
    pub(super) type_map: beamtalk_core::semantic_analysis::TypeMap,
    /// BT-3249: keys of methods whose `return_type` was set by the return-type
    /// writeback pass (`apply_return_type_writeback_from_map`) rather than
    /// typed by the user — the same map used to build `module`/`module_owned`
    /// (whichever this generation's `generate_module_with_warnings` ended up
    /// using), populated once before codegen begins. Consulted by
    /// `gen_server/methods.rs::extract_method_source` so the image-resident
    /// `__source__` text it bakes never carries an inferred `-> Type`
    /// annotation the user never wrote, while the method's real
    /// `return_type` (used for `method_return_types` metadata, specs, etc.)
    /// stays untouched. Empty for codegen contexts that never ran writeback.
    pub(super) method_return_types_written_back: std::collections::HashMap<
        beamtalk_core::semantic_analysis::MethodReturnKey,
        beamtalk_core::semantic_analysis::InferredType,
    >,
}

impl CoreErlangGenerator {
    /// Creates a new code generator for the given module name.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` constructs its
    // own generator.
    pub fn new(module_name: &str) -> Self {
        Self {
            module_name: EcoString::from(module_name),
            var_context: VariableContext::new(),
            state_threading: VersionCounter::new(),
            in_loop_body: false,
            branch_frame_counter: 0,
            in_hybrid_loop: false,
            in_direct_params_loop: false,
            loop_threads_class_vars: false,
            last_loop_class_var: None,
            direct_params_list_op_result: None,
            last_foldl_class_var_peak: None,
            hybrid_readonly_field_params: std::collections::HashMap::new(),
            hybrid_mutated_fields: std::collections::HashSet::new(),
            context: CodeGenContext::Actor, // Default to Actor for backward compatibility
            block_depth: 0,
            source_text: None,
            primitive_bindings: PrimitiveBindingTable::new(),
            current_method_params: Vec::new(),
            current_method_param_types: std::collections::HashMap::new(),
            current_class_field_types: std::collections::HashMap::new(),
            direct_params_do_open_chain: false,
            precompiled_subexprs: std::collections::HashMap::new(),
            source_path: None,
            tier2_block_params: std::collections::HashSet::new(),
            tier2_local_vars: std::collections::HashSet::new(),
            tier2_local_var_captured_mutations: std::collections::HashMap::new(),
            tier2_method_info: std::collections::HashMap::new(),
            codegen_warnings: Vec::new(),
            semantic_facts: beamtalk_core::semantic_analysis::SemanticFacts::default(),
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
            alias_registry: beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::new(),
            type_map: beamtalk_core::semantic_analysis::TypeMap::new(),
            method_return_types_written_back: std::collections::HashMap::new(),
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` queries/sets this
    // around its own generation calls.
    pub fn is_repl_mode(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.is_repl_mode)
    }

    /// Sets the REPL mode flag, initialising the context if absent.
    pub fn set_is_repl_mode(&mut self, value: bool) {
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` queries/sets this
    // around its own generation calls.
    pub fn workspace_mode(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.workspace_mode)
    }

    /// Sets workspace mode, initialising the context if absent.
    pub fn set_workspace_mode(&mut self, value: bool) {
        self.repl_context_mut().workspace_mode = value;
    }

    /// Returns a mutable reference to the REPL context, creating it if absent.
    fn repl_context_mut(&mut self) -> &mut ReplContext {
        self.repl_context.get_or_insert_with(ReplContext::new)
    }

    /// Returns a reference to the class identity, if any.
    pub(in crate::core_erlang) fn class_identity(&self) -> Option<&util::ClassIdentity> {
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.class_identity.as_ref())
    }

    /// Sets the class identity, initialising the context if absent.
    pub(in crate::core_erlang) fn set_class_identity(
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

    /// BT-3151: Returns a reference to the class-var-mutating selectors set.
    pub(super) fn class_var_mutating_selectors(&self) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_var_mutating_selectors)
    }

    /// BT-3151: Returns a mutable reference to the class-var-mutating selectors set.
    pub(super) fn class_var_mutating_selectors_mut(
        &mut self,
    ) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().class_var_mutating_selectors
    }

    /// Returns the class variable version counter.
    pub(super) fn class_var_version(&self) -> usize {
        self.class_context
            .as_ref()
            .map_or(0, |ctx| ctx.class_var_version.version())
    }

    /// Sets the class variable version counter.
    pub(super) fn set_class_var_version(&mut self, version: usize) {
        self.class_context_mut()
            .class_var_version
            .set_version(version);
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` sets this before
    // generating a REPL module so cross-class self-sends resolve.
    pub fn set_class_module_index(&mut self, index: std::collections::HashMap<String, String>) {
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
    ///
    /// BT-3131: `class_var_version`'s save-reset-restore here rides the same
    /// unified `VersionCounter` mechanism as [`BranchContextGuard`] — a
    /// distinct *reset* policy (this is a fresh method context, not a branch:
    /// the counter resets to 0 here, whereas `with_branch_context` restores
    /// without resetting), but through the identical counter implementation
    /// and accessor methods (`class_var_version`/`set_class_var_version`).
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
            class_var_mutating_selectors: self.class_var_mutating_selectors().clone(),
            class_var_version: self.class_var_version(),
            class_var_mutated: self.class_var_mutated(),
            class_slot_constructor_selector: self.class_slot_constructor_selector().cloned(),
            builder_class_method_class: self.builder_class_method_class(),
            state_version: self.state_version(),
            current_method_params: self.current_method_params.clone(),
            current_method_param_types: self.current_method_param_types.clone(),
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
        // BT-3151: also cleared, for the same reason plus one more — an empty
        // `class_method_selectors` already makes `generate_block`'s bare-block
        // self-send check treat every self-send here as unresolvable (so
        // conservatively unsafe) regardless of this set's contents, since a
        // programmatic `ClassBuilder` cascade has no static `ClassDefinition`
        // to run `compute_class_var_mutating_selectors` over in the first
        // place. Clearing just avoids leaking the enclosing class's own
        // mutating-selector set into an unrelated builder class's selector
        // namespace.
        self.class_var_mutating_selectors_mut().clear();
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
            *self.class_var_mutating_selectors_mut() = saved.class_var_mutating_selectors;
            self.set_class_var_version(saved.class_var_version);
            self.set_class_var_mutated(saved.class_var_mutated);
            self.set_class_slot_constructor_selector(saved.class_slot_constructor_selector);
            self.set_builder_class_method_class(saved.builder_class_method_class);
        } else {
            // No enclosing class context — drop the one we created so standalone
            // (REPL) builder cascades don't leak a class context.
            self.class_context = None;
        }
        // BT-3289: `state_version` lives outside `ClassContext`, so it must be
        // restored unconditionally here — independent of `had_context` — or a
        // builder cascade nested inside an enclosing method's field-assignment
        // value leaves that method's `State` version counter reset to 0
        // instead of wherever it legitimately was.
        self.set_state_version(saved.state_version);
        // BT-3300: same reasoning as `state_version` above — restore
        // unconditionally, independent of `had_context`.
        self.current_method_params = saved.current_method_params;
        self.current_method_param_types = saved.current_method_param_types;
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
        annotation: Option<&beamtalk_core::ast::TypeAnnotation>,
    ) {
        if let Some(beamtalk_core::ast::TypeAnnotation::Simple(id)) = annotation {
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

    /// BT-2710: Whether `name` refers to a parameter declared with a builtin
    /// comparable type. A superset of [`Self::param_is_numeric`]: bare
    /// comparison BIFs are correct for `Character`/`String` too (both define
    /// `< <=` as `@primitive`), so a `:: Character`/`:: String` param stays on
    /// the bare-BIF fast path and skips the `is_object` guard.
    pub(super) fn param_is_comparable(&self, name: &str) -> bool {
        self.current_method_param_types.get(name).is_some_and(|ty| {
            matches!(
                ty.as_str(),
                "Integer" | "Float" | "Number" | "Character" | "String"
            )
        })
    }

    /// BT-2710 follow-up: Records each instance field's declared `Simple` type
    /// from a class's state declarations, for the operator fast-path
    /// classifiers. Replaces any previously-recorded set (call once per class at
    /// codegen entry). Only `Simple` annotations are recorded; untyped fields
    /// are deliberately absent so they keep the bare-BIF status quo.
    pub(super) fn set_class_field_types(&mut self, state: &[beamtalk_core::ast::StateDeclaration]) {
        self.current_class_field_types.clear();
        for decl in state {
            if let Some(beamtalk_core::ast::TypeAnnotation::Simple(id)) =
                decl.type_annotation.as_ref()
            {
                self.current_class_field_types
                    .insert(decl.name.name.to_string(), id.name.to_string());
            }
        }
    }

    /// BT-2728: Populates instance-field type tracking for an **extension**
    /// method from the *target* class's declared state types, resolved via the
    /// class hierarchy. The target class is foreign (declared in another
    /// module), so its AST `state` is unavailable at extension-codegen time, but
    /// its [`ClassInfo`] carries the field-type strings. This lets an extension
    /// method's `self.<field>` operator dispatch be type-aware, matching in-class
    /// methods (which use [`Self::set_class_field_types`]).
    ///
    /// Mirrors `set_class_field_types`'s filtering: only *simple* named types are
    /// recorded, so generic/union/singleton-typed fields keep the bare-BIF
    /// status quo (parity with the in-class path, which records only
    /// `TypeAnnotation::Simple`). When the target class is not in the hierarchy,
    /// the map is cleared — the bare-BIF fallback, unchanged status quo.
    ///
    /// [`ClassInfo`]: beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo
    pub(super) fn set_extension_target_field_types(&mut self, target_class: &str) {
        let field_types: Vec<(String, String)> = self
            .class_hierarchy
            .as_ref()
            .and_then(|h| h.get_class(target_class))
            .map(|info| {
                info.state_types
                    .iter()
                    .filter(|(_, ty)| Self::is_simple_type_name(ty))
                    .map(|(name, ty)| (name.to_string(), ty.to_string()))
                    .collect()
            })
            .unwrap_or_default();
        self.current_class_field_types.clear();
        for (name, ty) in field_types {
            self.current_class_field_types.insert(name, ty);
        }
    }

    /// Whether `ty` is a *simple* named type (a bare identifier such as `Money`
    /// or `Integer`), as opposed to a generic (`List(Integer)`), union
    /// (`Integer | String`), singleton (`#north`), or metatype (`Foo class`).
    ///
    /// So extension-method field typing matches the in-class path (which
    /// records only `TypeAnnotation::Simple` fields, see
    /// [`Self::set_class_field_types`]). `Self` needs no explicit exclusion
    /// here: unlike the pre-BT-3076 string-rendered check, `Self` is
    /// [`DeclaredType::SelfType`], never `Simple("Self")`, so it already
    /// falls through to `false`.
    ///
    /// [`ClassInfo::state_types`]: beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo::state_types
    fn is_simple_type_name(
        ty: &beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType,
    ) -> bool {
        matches!(
            ty,
            beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::Simple(_)
        )
    }

    /// BT-2710 follow-up: Whether `self.<name>` is known to hold a value with a
    /// builtin total order / numeric type, so the comparison/arithmetic fast
    /// path may stay bare. True when the field is **untyped** (no info — keep
    /// the status quo) or its declared type is in `primitive_set`; false only
    /// when the field has an explicit non-primitive (object) type, which then
    /// routes through the runtime guard and dispatches.
    fn field_is_bare(&self, name: &str, primitive_set: &[&str]) -> bool {
        match self.current_class_field_types.get(name) {
            Some(ty) => primitive_set.contains(&ty.as_str()),
            None => true,
        }
    }

    /// BT-2710 follow-up: `self.<field>` is comparison-bare when untyped or a
    /// primitive-ordered type (numeric, `Character`, or `String`).
    pub(super) fn field_is_comparable(&self, name: &str) -> bool {
        self.field_is_bare(name, &["Integer", "Float", "Number", "Character", "String"])
    }

    /// BT-2709 / BT-2710 follow-up: `self.<field>` is arithmetic-bare when
    /// untyped or a numeric type.
    pub(super) fn field_is_numeric(&self, name: &str) -> bool {
        self.field_is_bare(name, &["Integer", "Float", "Number"])
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
            .map_or(0, |ctx| ctx.self_version.version())
    }

    /// Sets the value type self-version counter.
    pub(super) fn set_self_version(&mut self, version: usize) {
        self.value_type_context_mut()
            .self_version
            .set_version(version);
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` pushes/pops its
    // own scopes around REPL binding generation.
    pub fn push_scope(&mut self) {
        self.var_context.push_scope();
    }

    /// Pops the current scope, discarding its bindings.
    pub fn pop_scope(&mut self) {
        self.var_context.pop_scope();
    }

    /// Looks up a variable binding in the current scope stack.
    fn lookup_var(&self, name: &str) -> Option<&String> {
        self.var_context.lookup(name)
    }

    /// Binds an identifier to a Core Erlang variable name in the current scope.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` binds
    // `__bindings__`/workspace variable names before generating a REPL body.
    pub fn bind_var(&mut self, name: &str, core_var: &str) {
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` reads the current
    // state variable name while threading REPL bindings.
    pub fn current_state_var(&self) -> String {
        render_state_prefix(
            self.in_hybrid_loop,
            self.in_loop_body,
            self.state_threading.version(),
        )
    }

    /// Increments the state version and returns the new state variable name.
    ///
    /// When inside a hybrid-params loop (`in_hybrid_loop = true`) or normal context,
    /// returns `State1`, `State2`, etc.
    /// When inside a normal loop body (`in_loop_body = true`), returns `StateAcc1`, etc.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` advances the
    // state version while threading REPL bindings.
    pub fn next_state_var(&mut self) -> String {
        self.state_threading.next_var(VersionPrefix::State);
        render_state_prefix(
            self.in_hybrid_loop,
            self.in_loop_body,
            self.state_threading.version(),
        )
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
        render_state_prefix(
            self.in_hybrid_loop,
            self.in_loop_body,
            self.state_threading.version() + 1,
        )
    }

    /// Sets the state version.
    pub(super) fn set_state_version(&mut self, version: usize) {
        self.state_threading.set_version(version);
    }

    /// BT-3146 (ADR 0111 Addendum 5, §Branch-context version discipline,
    /// "`FrameId` allocation is the one missing production mechanism"): mints
    /// and returns the [`threaded_ir::FrameId`] for the CURRENT (already
    /// entered) branch context — the frame every real `Bind`/`Threaded` node
    /// a branch-arm lowering constructs must use. Distinct from
    /// [`threaded_ir::FrameId::ROOT`] always: `enter_branch_context` mints
    /// starting at `1`.
    pub(in crate::core_erlang) fn current_branch_frame(&self) -> threaded_ir::FrameId {
        threaded_ir::FrameId::new(self.branch_frame_counter)
    }

    /// ADR 0118 phase 2a (BT-3417): the [`threaded_ir::FrameId`] a
    /// `threaded_expression`/`thread_ahead` caller should splice a prelude's
    /// `Bind`s into RIGHT NOW — [`Self::current_branch_frame`] while inside
    /// any `with_branch_context` arm (a conditional branch, an
    /// `on:do:`/`ensure:` body, a Tier 2 stateful-block body, or — since
    /// ADR 0118 phase 2b (BT-3418) — a real loop body itself, all of which
    /// set `in_loop_body`), [`FrameId::ROOT`](threaded_ir::FrameId::ROOT)
    /// at the flat method body.
    pub(in crate::core_erlang) fn current_frame(&self) -> threaded_ir::FrameId {
        if self.in_loop_body {
            self.current_branch_frame()
        } else {
            threaded_ir::FrameId::ROOT
        }
    }

    /// BT-3131: Enters a branch context, applying the per-prefix reset policy
    /// documented on [`BranchContextGuard`] and returning a guard that
    /// restores everything (per that same policy) when dropped.
    fn enter_branch_context(&mut self) -> BranchContextGuard<'_> {
        let saved_state_version = self.state_version();
        let saved_in_loop = self.in_loop_body;
        let saved_class_var_version = self.class_var_version();
        let saved_self_version = self.self_version();
        let saved_loop_threads_class_vars = self.loop_threads_class_vars;
        self.set_state_version(0);
        self.in_loop_body = true;
        // BT-3168: reset-on-entry, like `state_version` — see
        // `loop_threads_class_vars`'s own doc comment for why this must
        // never inherit an enclosing Letrec loop's `true` by default.
        self.loop_threads_class_vars = false;
        // BT-3146: mint a fresh frame identity for this branch context —
        // see `current_branch_frame`'s doc comment. Never reset/restored
        // (unlike the version counters above): frame identity must stay
        // globally unique across the whole module compile.
        self.branch_frame_counter += 1;
        // BT-3131 review fix: do NOT reset self_version to 0 here — unlike
        // `state`, a `Self{N}` reference is always a syntactically valid
        // Core Erlang variable (the bare `Self` parameter always exists), so
        // resetting doesn't fail to compile, it silently reads the
        // pre-mutation value. `generate_threaded_loop_body` calls this
        // unconditionally and is shared with ValueType contexts (confirmed:
        // resetting produces `maps:get(field, Self)` instead of
        // `maps:get(field, Self1)` for a `self.field := ...` read inside a
        // `do:`/conditional body that follows an earlier `self.field := ...`
        // in the same method — see `BranchContextGuard`'s doc comment).
        // `self` gets `class_vars`' restore-only discipline instead.
        BranchContextGuard {
            generator: self,
            saved_in_loop,
            saved_state_version,
            saved_class_var_version,
            saved_self_version,
            saved_loop_threads_class_vars,
        }
    }

    /// BT-1449: Executes `f` inside a branch context where `in_loop_body` is
    /// `true` and `state_version` is reset to 0.  The previous values are
    /// unconditionally restored — via [`BranchContextGuard`]'s `Drop` impl —
    /// once this function returns, including through an early return via
    /// `?` inside `f`.
    ///
    /// BT-1550: Also saves/restores `class_var_version` (without resetting
    /// it — the branch inherits the outer scope's current version) so that
    /// self-calls inside a conditional branch don't leak `ClassVars{N}`
    /// bindings into the outer scope.  `class_var_mutated` is intentionally
    /// NOT restored — it is a method-level flag that must stay sticky once
    /// set.
    ///
    /// BT-3131: Also saves/restores `self_version`, with the same
    /// restore-only-no-reset discipline as `class_var_version` — see
    /// [`BranchContextGuard`]'s doc comment for why a `state`-style reset is
    /// unsafe for `self`.
    pub(super) fn with_branch_context<T>(
        &mut self,
        f: impl FnOnce(&mut CoreErlangGenerator) -> T,
    ) -> T {
        let guard = self.enter_branch_context();
        f(guard.generator)
    }

    /// BT-412: Returns the current class variable state variable name.
    pub(super) fn current_class_var(&self) -> String {
        self.class_context
            .as_ref()
            .map_or(VersionCounter::new(), |ctx| ctx.class_var_version)
            .current_var(VersionPrefix::ClassVars)
    }

    /// BT-412: Increments class var version and returns the new variable name.
    fn next_class_var(&mut self) -> String {
        let name = self
            .class_context_mut()
            .class_var_version
            .next_var(VersionPrefix::ClassVars);
        self.set_class_var_mutated(true);
        name
    }

    /// BT-3169: records the peak `class_var_version` reached inside a
    /// `Foldl*` body's own `with_branch_context` scope, for
    /// [`Self::take_foldl_class_var_peak`] to consume once that scope's
    /// guard has restored the live counter — see
    /// `last_foldl_class_var_peak`'s own doc comment for the full rationale.
    pub(super) fn set_foldl_class_var_peak(&mut self, version: usize) {
        self.last_foldl_class_var_peak = Some(version);
    }

    /// BT-3169: takes (clears) the peak class-var version recorded by
    /// [`Self::set_foldl_class_var_peak`], if any, and — when it exceeds the
    /// live (already-restored) counter — fast-forwards the live counter to
    /// it, so the next [`Self::next_class_var`] mint is guaranteed not to
    /// collide with a name already used inside the fold body's own closure.
    /// A no-op when no peak was recorded (non-`ClassVars`-threading bodies).
    pub(super) fn catch_up_class_var_version_to_foldl_peak(&mut self) {
        if let Some(peak) = self.last_foldl_class_var_peak.take() {
            if peak > self.class_var_version() {
                self.set_class_var_version(peak);
            }
        }
    }

    /// BT-3169: refreshes the live `ClassVars` name after generating an
    /// expression whose caller is about to bind the WHOLE returned
    /// `Document` opaquely (`let X = <expr> in ...`, e.g.
    /// `emit_vt_threaded_local_assignment`'s `{Value, StateAcc}`-tuple
    /// binding), catching up to a class-var mutation the ADR 0110 shadow
    /// write recorded but that opaque compile never surfaced as its own
    /// `ThreadedValue` prelude.
    ///
    /// The gap this closes: a class-method self-send inside a `Foldl*` body
    /// (`do:`/`collect:`/`select:`/`inject:into:`) correctly threads its own
    /// `ClassVarsN` rebind through the fold's `{ClassVars, StateAcc}`
    /// accumulator (ADR 0111 Addendum 9, Question 6) — but that name is
    /// minted, and lexically bound, entirely INSIDE the list-op function's
    /// own returned `Document`. A caller that treats the whole thing as an
    /// opaque value (rather than splicing it into its own open let-chain,
    /// the way `push_discarded_stmt` and the value-type/class-method "open"
    /// loop-body family already do) confines that binding to its own `let`'s
    /// RHS — any LATER code in the same method that references
    /// `self.current_class_var()`'s name (a class-var read, or another
    /// self-send) would then reference a name Core Erlang never actually
    /// bound at that point — confirmed empirically as an `erlc` "unbound
    /// variable" compiler crash, not merely a silently-wrong value.
    ///
    /// Rather than widening every list-op function's own external contract
    /// to also expose `ClassVars` as an explicit extra tuple element (a much
    /// larger, cross-cutting change to every consumer of that contract),
    /// this reaches for the ADR 0110 process-dictionary shadow write that
    /// already exists for the analogous foreign-NLR-relay problem: a
    /// same-class, locally-defined self-send call (`class_bump`-style,
    /// compiled as a direct module call, not a dispatch) unconditionally
    /// writes its own mutation to `{'$bt_class_vars_shadow', element(2,
    /// ClassSelf)}` before returning, and that key is erased only by the
    /// OUTER-MOST dispatch wrapper (`invoke_class_method/7`'s `after`) —
    /// never by the direct call itself — so it is still live, and correct,
    /// for the remainder of the SAME class method's own body. Reading it
    /// back here is a safe, minimal escape hatch scoped to exactly the
    /// opaque-wrap gap above, not a substitute for the accumulator threading
    /// itself (which is still what makes the fold's OWN cross-iteration
    /// threading correct in the first place).
    ///
    /// Returns `Some(prelude_doc)` — a `"let <fresh ClassVarsN> = <shadow
    /// read, falling back to the pre-scope value> in "` binding the caller
    /// should push immediately after its own opaque-value `let` — when
    /// `self.class_var_version()` advanced across generating the just-built
    /// expression (`version_before` is the version read immediately before
    /// generating it); `None` when nothing changed (including, by
    /// construction, every non-class-method context, where no code path
    /// ever advances `class_var_version` at all).
    ///
    /// **Guarded against a false-positive shadow read** (found during
    /// review): `class_var_version` advances on EVERY class-method
    /// self-send (`emit_class_var_result_unwrap` calls `next_class_var()`
    /// unconditionally, whether or not the callee performs a real field
    /// write), but the shadow key is written only by an actual
    /// `self.field := value` (`shadow_write: true`). A pure self-send (e.g.
    /// a `select:`/`collect:` predicate/transform with no field write) would
    /// otherwise read back the atom `'undefined'` and corrupt this class
    /// method's own class-var state. The `case` below treats `'undefined'`
    /// as "nothing new was shadow-written" and falls back to the value that
    /// was already live before this scope, rather than trusting the read.
    /// This also subsumes the narrower, previously-documented INHERITED
    /// self-send gap (`self someInheritedMethod` routing through
    /// `class_self_dispatch`, which may erase the shadow key the same way
    /// `invoke_class_method/7`'s own `after` does) — that path now falls
    /// back safely too, for the same reason.
    pub(super) fn refresh_class_var_after_opaque_scope(
        &mut self,
        version_before: usize,
    ) -> Option<Document<'static>> {
        if self.class_var_version() == version_before {
            return None;
        }
        let mut before_counter = VersionCounter::new();
        before_counter.set_version(version_before);
        let cv_before = before_counter.current_var(VersionPrefix::ClassVars);
        let shadow_raw = self.fresh_temp_var("ClassVarsShadow");
        let cv_new = self.next_class_var();
        Some(docvec![
            "let ",
            leaf::var(shadow_raw.clone()),
            " = call 'erlang':'get'({",
            leaf::atom("$bt_class_vars_shadow"),
            ", call 'erlang':'element'(2, ",
            leaf::var("ClassSelf"),
            ")}) in let ",
            leaf::var(cv_new),
            " = case ",
            leaf::var(shadow_raw),
            " of <'undefined'> when 'true' -> ",
            leaf::var(cv_before),
            " <_ShadowVal> when 'true' -> _ShadowVal end in ",
        ])
    }

    /// BT-833: Returns the current Self variable name for value type Self-threading.
    ///
    /// Version 0 → `"Self"` (the original method parameter).
    /// Version N → `"Self{N}"` (after N field assignments have threaded a new snapshot).
    pub(super) fn current_self_var(&self) -> String {
        self.value_type_context
            .as_ref()
            .map_or(VersionCounter::new(), |ctx| ctx.self_version)
            .current_var(VersionPrefix::SelfVt)
    }

    /// BT-833: Increments the Self version and returns the new variable name.
    pub(super) fn next_self_var(&mut self) -> String {
        self.value_type_context_mut()
            .self_version
            .next_var(VersionPrefix::SelfVt)
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

    /// BT-940/BT-3127: Wraps a Document with a Core Erlang line annotation.
    ///
    /// Delegates to [`leaf::annotated`] for the `[Line, {'file', Path}]` shape
    /// (BT-3119 spike), which the BEAM compiler preserves into the Line chunk.
    /// The VM surfaces this as `[{file, "path.bt"}, {line, N}]` in stacktrace
    /// frames. Falls back to a bare `[Line]` annotation when no source path is
    /// known (e.g. compiling from a string with no backing file).
    pub(super) fn annotate_with_line(
        &self,
        doc: Document<'static>,
        line_num: u32,
    ) -> Document<'static> {
        match &self.source_path {
            Some(path) => leaf::annotated(doc, &leaf::BtSpan::new(path, line_num)),
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
    pub(super) fn body_has_list_op_cross_scope_mutations(
        &self,
        body: &beamtalk_core::ast::Block,
    ) -> bool {
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
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
        out: &mut std::collections::HashSet<String>,
    ) {
        match expr.unwrap_parens() {
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
        use crate::core_erlang::block_analysis::analyze_block;
        use beamtalk_core::ast::MessageSelector;

        // Peel parens then an assignment RHS (which may itself be parenthesized) so
        // forms like `_r := (1 to: 5 do: [...])` are still inspected — mirrors
        // `expr_has_nested_counted_loop_threading`.
        let inner = match expr.unwrap_parens() {
            Expression::Assignment { value, .. } => value.unwrap_parens(),
            other => other,
        };
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = inner
        else {
            return;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();

        // BT-3173: ensure:/on:do:/ifNotNil: aren't loops themselves, but a
        // loop may be nested inside one of their blocks — recurse straight
        // through (the receiver for ensure:/on:do:, any block arguments for
        // all three) so a nested loop's outer-local write buried behind one
        // of these constructs is still found. Mirrors the identical
        // extension in `control_flow::collect_list_op_cross_scope_mutations`.
        if beamtalk_core::state_threading_selectors::is_exception_selector(&sel)
            || beamtalk_core::state_threading_selectors::is_conditional_selector(&sel)
        {
            let mut blocks: Vec<&beamtalk_core::ast::Block> = Vec::new();
            if beamtalk_core::state_threading_selectors::is_exception_selector(&sel) {
                if let Expression::Block(b) = receiver.as_ref() {
                    blocks.push(b);
                }
            }
            for arg in arguments {
                if let Expression::Block(b) = arg {
                    blocks.push(b);
                }
            }
            for block in blocks {
                let mut all_excluded: HashSet<String> = excluded_params.clone();
                all_excluded.extend(Self::block_param_names(block));
                for stmt in &block.body {
                    self.collect_nested_loop_outer_local_writes(
                        &stmt.expression,
                        &all_excluded,
                        out,
                    );
                }
            }
            return;
        }

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
        // BT-3135 (ADR 0111 Phase D): this is the true call site
        // `ThreadedStmt::NlrCatch` faithfully models (module docs on
        // `threaded_ir::ThreadedStmt::NlrCatch`) — every NLR try/catch this
        // generator ever emits (Actor/ClassMethod/ValueType alike) is built
        // here; its `boundary` shape is also what
        // `threaded_ir::construct_and_verify_class_var_bind`'s synthetic
        // marker (built from its `frame`/`shadow_write_eligible` pair, ADR
        // 0111 Addendum 9) reconstructs at the class-var
        // Bind-emission sites (`expressions.rs`, `dispatch_codegen.rs`) for
        // the ADR 0110 ShadowWriteMissing contract. No standalone
        // `verify()` call here: a
        // lone `NlrCatch` with no `Bind` can never trigger any
        // `VerifyError` (`walk_stmt` treats it as a no-op), so constructing
        // one on every NLR-catch wrap — a hot path — would pay a real
        // allocation for a check that can't fire (caught in review).

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
        hierarchy: &beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy,
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
    /// **Value types:** Inherit from Object or Value (but not Actor). Generate plain Erlang
    /// maps/records via `generate_value_type_module`.
    ///
    /// # Implementation Note
    ///
    /// BT-3086: Delegates to `ClassHierarchy::resolve_class_kind`, the single authority for
    /// actor/value classification (see its doc comment for the walk + default-to-`Object`
    /// policy on a fully-known chain). This used to be a third, independent implementation
    /// that re-walked the chain itself and consulted a hand-maintained list of "known value
    /// roots" (`Object`, `Exception`, `RuntimeError`, ...) that went stale every time the
    /// exception hierarchy grew. Both `ClassKind::Value` and `ClassKind::Object` route to
    /// `generate_value_type_module` here — the Value/Object distinction only matters for
    /// auto-slot codegen *within* the value-type path, not for actor-vs-value routing.
    ///
    /// The one case `resolve_class_kind` cannot see through is a genuinely incomplete
    /// ancestor chain — a superclass that isn't registered in this `ClassHierarchy` at all
    /// (e.g. compiling a subclass file independently of its parent). `resolve_class_kind`
    /// resolves that to `ClassKind::Object` (neither `Actor` nor `Value` literal is found),
    /// but codegen has historically defaulted such classes to *actor* instead, for backward
    /// compatibility with independent per-file compilation. `ClassHierarchy::has_cross_file_parent`
    /// is the existing, already-tested predicate for "this chain has an unregistered
    /// ancestor" — reused here rather than re-deriving the same fact from a hardcoded list.
    ///
    /// # Returns
    ///
    /// - `true` if class inherits from Actor anywhere in the (fully-known) chain
    /// - `true` if a concrete (non-abstract) Supervisor/DynamicSupervisor subclass (BT-1220)
    /// - `true` if the chain has an unregistered ancestor (incomplete-chain default, above)
    /// - `false` if class resolves to Value or Object on a fully-known chain
    /// - `true` if module contains no class (backward compatibility for REPL)
    fn is_actor_class(
        module: &Module,
        hierarchy: &beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy,
    ) -> bool {
        let Some(class) = module.classes.first() else {
            return true;
        };
        // BT-1220: Concrete Supervisor/DynamicSupervisor subclasses use supervisor codegen,
        // routed through generate_actor_module which delegates to supervisor_codegen.
        // Abstract base classes (Supervisor, DynamicSupervisor themselves) remain value types.
        if class.supervisor_kind.is_some() && !class.is_abstract {
            return true;
        }
        let name = class.name.name.as_str();
        match hierarchy.resolve_class_kind(name) {
            ClassKind::Actor => true,
            ClassKind::Value => false,
            ClassKind::Object => hierarchy.has_cross_file_parent(name),
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
        // ADR 0118 phase 1a (BT-3415): a sub-expression the sequencing rule
        // already compiled ahead of this parent substitutes its value here
        // — see `precompiled_subexprs`.
        if let Some(doc) = self.take_precompiled_subexpr(expr) {
            return Ok(doc);
        }
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
                // Only annotate CLOSED expressions — see `can_annotate_closed_expression`.
                if self.can_annotate_closed_expression() {
                    if let Some(line_num) = self.span_to_line(*span) {
                        return Ok(self.annotate_with_line(doc, line_num));
                    }
                }
                Ok(doc)
            }
            Expression::Assignment { target, value, .. } => {
                // BT-2792: Tier 2 only ever supported captured-local mutations, not
                // field writes — a stored block with `self.field :=` is rejected by
                // generate_block()'s validate_stored_closure call, not silently
                // accepted. No validation needed *here* only because that check
                // already lives inside generate_block() itself.

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
                    // BT-3374, generalized by ADR 0118 phase 1b (BT-3416)
                    // and phase 5b (BT-3422): `value` may itself dispatch a
                    // self-send that threads new state (Actor `State` or,
                    // since phase 5b, class-method `ClassVars`) — nested
                    // anywhere inside it (`^ self.items at: (self bump)`),
                    // not just at its own top level. `state` below must be
                    // computed AFTER `value` threads, so it reflects
                    // whatever `Bind` that dispatch just produced —
                    // `threaded_expression` is the one mechanism that
                    // threads BOTH prefixes correctly for whichever context
                    // this `^` runs in.
                    //
                    // ADR 0118 phase 2a (BT-3417): when THIS `Return` node
                    // is itself the sole child `threaded_expression`'s own
                    // `single_sequenced_child` branch is sequencing (e.g.
                    // `thread_ahead`'s C12-catch-all reaching `^self.items
                    // at: (self bump)` nested in a conditional branch —
                    // `single_sequenced_child` already ran
                    // `sequence_children` on `value` and is about to compile
                    // THIS WHOLE node via `generate_expression`), `value`'s
                    // span already carries a live `precompiled_subexprs`
                    // registration from that outer call. Re-threading it
                    // here via `threaded_expression` would dispatch any
                    // nested self-send a SECOND time and leave the outer
                    // registration's `finish_precompiled_scope` check
                    // failing with "never substituted" (confirmed by a
                    // `just test-bunit` failure on this exact shape during
                    // this migration) — so the "already sequenced" case
                    // instead just reads that registration back via the
                    // ordinary `expression_doc` (`generate_expression`'s
                    // `take_precompiled_subexpr` entry), with no prelude of
                    // its own to add here (it already ran in the outer
                    // frame).
                    let value_already_sequenced = self
                        .precompiled_subexprs
                        .contains_key(&value.unwrap_parens().span());
                    let (val_preamble, value_doc) = if value_already_sequenced {
                        (Document::Nil, self.expression_doc(value)?)
                    } else if self.in_class_method()
                        && !(self.is_class_var_assignment(value.unwrap_parens())
                            || self.is_class_method_self_send(value.unwrap_parens()))
                    {
                        // ADR 0118 phase 5b (BT-3422): `value` is not ITSELF
                        // a recognized producer at its own top level (e.g.
                        // `^self foo` where `foo` is BT-2007 inherited, so
                        // `is_class_method_self_send`'s `class_method_selectors()`
                        // check excludes it) — `threaded_expression` would
                        // still dispatch it, but through the opaque
                        // `sequenced_send_children` fallback, which closes
                        // over any `ClassVarsN` it rebinds internally: the
                        // compiler's OWN `current_class_var()` bookkeeping
                        // advances to track that rebind regardless, so
                        // reading it below (`state`) would reference a name
                        // never bound in THIS scope.
                        // `refresh_class_var_after_opaque_scope` recovers
                        // the live value via the ADR 0110 shadow write and
                        // re-binds it to a name that IS in scope here.
                        let cv_version_before = self.class_var_version();
                        let result_doc = self.expression_doc(value)?;
                        let refresh = self
                            .refresh_class_var_after_opaque_scope(cv_version_before)
                            .unwrap_or(Document::Nil);
                        (refresh, result_doc)
                    } else {
                        // ADR 0118 phase 2a (BT-3417): `current_frame()` —
                        // this generic `Return` handler fires for a `^`
                        // reached from any nesting, not only the flat
                        // method body, now that branch/exception/
                        // stateful-block arms have their own `threaded_expression`
                        // consumers too.
                        let frame = self.current_frame();
                        let tv = self.threaded_expression(value, frame)?;
                        let dispatch_doc = self.threaded_prelude_doc(&tv.prelude);
                        let result_doc = self.threaded_value_doc(&tv.value);
                        (dispatch_doc, result_doc)
                    };
                    // BT-761/BT-854: All NLR throws carry state as a 4-tuple.
                    // Actor methods use the current gen_server state; value type
                    // methods use the latest Self{N} snapshot so field mutations
                    // accumulated before the ^ are preserved.
                    // BT-1202: Class methods use the current ClassVars snapshot
                    // — computed after the value above so it reflects any
                    // rebind that value's evaluation just performed.
                    let state = if self.in_class_method() {
                        self.current_class_var()
                    } else if self.context == CodeGenContext::Actor {
                        self.current_state_var()
                    } else {
                        self.current_self_var()
                    };
                    let throw_doc = docvec![
                        "call 'erlang':'throw'({'$bt_nlr', ",
                        leaf::var(nlr_token),
                        ", ",
                        value_doc,
                        ", ",
                        leaf::var(state),
                        "})"
                    ];
                    Ok(docvec![val_preamble, throw_doc])
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
                leaf::atom(util::metaclass_tag(&display_name)),
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
                leaf::atom(util::metaclass_tag(&display_name)),
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
        let expr = expr.unwrap_parens();
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
            // BT-3402: `and:`/`or:` now have their own inline-case generators
            // (`generate_and_with_mutations`/`generate_or_with_mutations`), so they
            // satisfy (b) the same way `ifTrue:`/`ifFalse:` do.
            // BT-3420: `ifNil:`/`ifNil:ifNotNil:`/`ifNotNil:ifNil:` now share
            // `generate_nil_conditional_with_mutations`, satisfying (b) too.
            "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifNotNil:" | "and:" | "or:"
            | "ifNil:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:" => {
                Self::non_empty(self.conditional_threaded_locals(&Self::block_args(arguments)))
            }
            // BT-3160: on:do:/ensure: thread outer-local mutations the same way a
            // conditional's branches do — the try (receiver) block and any
            // handler/cleanup block(s) are mutually-exclusive-or-sequential
            // alternatives that are all compiled, only some of which run at a given
            // call, so the union of their local writes is the threaded set. The
            // seeding counterpart (`generate_on_do_with_mutations`/
            // `generate_ensure_with_mutations`, via `seed_conditional_locals`)
            // guarantees every `__local__` key extracted here is present even on a
            // path that didn't itself write it.
            sel if beamtalk_core::state_threading_selectors::is_exception_selector(sel) => {
                let mut blocks: Vec<&Block> = Vec::new();
                if let Expression::Block(b) = receiver.as_ref() {
                    blocks.push(b);
                }
                blocks.extend(Self::block_args(arguments));
                Self::non_empty(self.conditional_threaded_locals(&blocks))
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
        use crate::core_erlang::block_analysis::analyze_block;

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

    /// Validates a block's mutation analysis for shapes that can't correctly thread state:
    /// field assignments, and (separately) captured-local mutations. Returns an error for
    /// either; the local-mutation error is phrased as a warning in its message.
    ///
    /// **Precondition for production callers:** only call this when
    /// `analysis.field_writes` is non-empty. A valid Tier 2 block (captured-local
    /// mutations, no field writes) passed here would incorrectly hit the
    /// local-mutation branch and produce a spurious `LocalMutationInStoredClosure`
    /// — that branch exists for this function's own unit tests, which construct
    /// analyses field-write-empty on purpose to test it, not for callers on a path
    /// where a genuine Tier 2 block could reach this function.
    ///
    /// BT-852 claimed production call sites could be removed because blocks with
    /// mutations are supported via the Tier 2 stateful block protocol (ADR 0041).
    /// BT-2792 found that's only true for *captured local* mutations
    /// (`captured_mutations_for_block` in `expressions.rs`, which promotes to
    /// `generate_block_stateful`) — Tier 2 promotion never triggers on `self.field :=`
    /// writes. A block with field writes that reaches the generic "pure fun" fallback
    /// in `generate_block` silently emits Core Erlang `erlc` rejects with "unbound
    /// variable" (the block's own `fun` bumps the shared state-version counter, but
    /// that binding is scoped inside the `fun` and never reaches the caller).
    ///
    /// Called from `generate_block` (with an already-computed analysis, so callers
    /// that need more than this check don't re-walk the block's AST) to turn that into
    /// a clear compile-time diagnostic instead. `generate_block` only calls this when
    /// `field_writes` is non-empty (checked *before* the Tier 2 captured-local-mutation
    /// promotion, so a block with both a field write and a local mutation errors here
    /// instead of silently reaching Tier 2 for the local mutation alone), so the field
    /// branch below always fires from that call site and the local-mutation branch is
    /// unreachable from it — it's kept live (and directly unit-tested) since this
    /// function checks a block's mutation shape in general, not just the field-write
    /// case `generate_block` currently cares about. BT-2797 tracks lifting the
    /// field-write restriction for stored/opaque blocks by generalizing Tier 2 the same
    /// way; once that lands this function's field-write branch should shrink to
    /// whatever shapes remain genuinely unsupported.
    ///
    /// `location` is a lazy thunk rather than a pre-formatted `String`, so formatting
    /// only happens when an error is actually produced. From `generate_block`'s call
    /// site this always errors (it's only called when `field_writes` is non-empty), but
    /// `generate_block` still runs `analyze_block` and checks `field_writes` on every
    /// block it compiles — the thunk keeps this function itself free of `span_to_line`/
    /// `format!` cost for callers (present or future) that reach it on a path where an
    /// `Ok(())` result is actually possible, e.g. this function's own unit tests.
    fn validate_stored_closure(
        analysis: &block_analysis::BlockMutationAnalysis,
        location: impl FnOnce() -> String,
    ) -> Result<()> {
        // ERROR: Field assignments that can't thread state back are not allowed.
        // Sort before picking one so the reported field is deterministic across
        // builds/runs when a block writes more than one field.
        if !analysis.field_writes.is_empty() {
            let mut fields: Vec<&String> = analysis.field_writes.iter().collect();
            fields.sort_unstable();
            let field = fields[0];
            let field_capitalized = {
                let mut chars = field.chars();
                chars
                    .next()
                    .map(|c| c.to_uppercase().to_string())
                    .unwrap_or_default()
                    + chars.as_str()
            };
            return Err(CodeGenError::FieldAssignmentInUnsupportedBlock {
                field: field.clone(),
                field_capitalized,
                location: location(),
            });
        }

        // WARNING: Local mutations in stored closures won't work as expected
        // Note: For now we're treating this as an error too, but the error type
        // is labeled as a warning in the message.
        // BT-665: Only flag mutations of captured variables, not new local definitions.
        // A "captured mutation" is a write to a variable that was read before being
        // locally defined (i.e., it captures from outer scope).
        if let Some(variable) = {
            let mut vars: Vec<&String> = analysis
                .local_writes
                .intersection(&analysis.captured_reads)
                .collect();
            vars.sort_unstable();
            vars.into_iter().next()
        } {
            return Err(CodeGenError::LocalMutationInStoredClosure {
                variable: variable.clone(),
                location: location(),
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
        span: beamtalk_core::source_analysis::Span,
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
        // to the target class's auto-generated new/0).
        //
        // BT-3047 / ADR 0109 amendment: ClassName is derived from `ClassSelf`
        // (closure-captured, correct even inside a block executing in a foreign
        // class's process) rather than the process dictionary — the same fix
        // applied to the instantiation intrinsics in `dispatch_codegen.rs`. Module
        // is resolved by name via `beamtalk_class_instantiation:resolve_module_or_raise/2`
        // rather than `element(3, ClassSelf)` (`class_mod`), which is not reliably
        // the calling class's own module for an inherited class method — see that
        // function's doc for why (discovered as a `Value does not understand 'x'`-
        // shaped regression while implementing this amendment).
        if !is_quoted && self.in_class_method() {
            match name {
                "basicNew" => {
                    return Ok(docvec![
                        "call 'beamtalk_class_instantiation':'class_self_new'(",
                        Self::class_self_name_doc(),
                        ", ",
                        Self::class_self_module_doc("new"),
                        ", [])",
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
                        Self::class_self_name_doc(),
                        ", ",
                        Self::class_self_module_doc("new:"),
                        ", [",
                        leaf::var(param),
                        "])",
                    ]);
                }
                _ => {}
            }
        }

        // BT-2803 (adversarial review): `blockValueWithArguments`'s compiled
        // method body is real, not a placeholder — unlike `blockValue`/
        // `blockValue1`/etc. (which truly are call-site-only, since a Tier 2
        // block's extra state argument can only come from a calling method's
        // live `State`/`StateAcc`), a plain `erlang:apply(Self, Args)` is
        // correct for *any* receiver reached via generic runtime dispatch
        // (`beamtalk_primitive:send/3`, `perform:withArguments:`, …) — a
        // Tier 2 block can never correctly reach this path in the first
        // place (see `is_tier2_value_call`'s scoping in
        // `gen_server/methods.rs`), so there's no state to thread here.
        // Restores the exact behaviour `valueWithArguments:`'s `@primitive`
        // form had before being converted to a call-site-intercepted
        // `@intrinsic`, fixing `send_block_valueWithArguments_test_` in
        // `beamtalk_primitive_tests.erl`.
        if !is_quoted && name == "blockValueWithArguments" {
            let args_param = self
                .current_method_params
                .first()
                .cloned()
                .unwrap_or_else(|| "_Args".to_string());
            return Ok(docvec![
                "call 'erlang':'apply'(Self, ",
                leaf::var(args_param),
                ")",
            ]);
        }

        // BT-2812: `blockValue`/`blockValue1`/`blockValue2`/`blockValue3` — Block's
        // `value`/`value:`/`value:value:`/`value:value:value:`. Same gap as
        // `blockValueWithArguments` above, but these can't unconditionally
        // `erlang:apply` (a Tier 2/stateful block needs a live `StateAcc` this
        // generic dispatch site doesn't have — see ADR-0041). See
        // `generate_block_value_structural_fallback` for the Tier 1/Tier 2
        // discrimination.
        //
        // Adversarial review (BT-2812): this match is intentionally exhaustive
        // over today's four `value*` arities, not pattern-derived from them. A
        // hypothetical 5th arity (`blockValue4`) or a change to ADR-0041's
        // `Args..., StateAcc` shape (extra state args, different position)
        // would silently fall through to the unfixed placeholder below rather
        // than fail to compile — there's no static check tying this arm list
        // to `STRUCTURAL_INTRINSICS` or to the Block.bt method declarations.
        if !is_quoted {
            let block_value_info = match name {
                "blockValue" => Some((0usize, "value")),
                "blockValue1" => Some((1usize, "value:")),
                "blockValue2" => Some((2usize, "value:value:")),
                "blockValue3" => Some((3usize, "value:value:value:")),
                _ => None,
            };
            if let Some((arity, real_selector)) = block_value_info {
                return Ok(self.generate_block_value_structural_fallback(
                    name,
                    arity,
                    real_selector,
                    &class_name,
                ));
            }
        }

        // BT-2908: `whileTrue`/`whileFalse`/`repeat`/`onDo`/`ensure` — Block's
        // `whileTrue:`/`whileFalse:`/`repeat`/`on:do:`/`ensure:`. The other half
        // of the gap BT-2812's audit found but deliberately left unfixed above
        // (see the BT-2812 comment on this match's sibling below): unlike
        // `value*`, these need real loop/exception-handling semantics, not a
        // bare `erlang:apply`. Feasible generically for the Tier 1 (pure) case
        // because Core Erlang's `case`/`try`/`catch` mechanics don't themselves
        // need the block's AST — only ADR-0041's state-threading convention
        // does, and a Tier 2 (stateful) receiver/argument raises the same
        // `stateful_block_dispatch` error `generate_block_value_structural_fallback`
        // established rather than being reimplemented generically.
        //
        // Adversarial review (BT-2908): like BT-2812's `value*` match above,
        // the `is_function/2` arity+1 check each of these five functions runs
        // is not statically tied to ADR-0041's `Args..., StateAcc` convention
        // — a future change to that shape (extra state args, different
        // position) would silently degrade Tier 2 detection here too, with no
        // compile-time signal.
        if !is_quoted {
            match name {
                "whileTrue" => {
                    return Ok(self.generate_while_structural_fallback(
                        name,
                        false,
                        "whileTrue:",
                        &class_name,
                    ));
                }
                "whileFalse" => {
                    return Ok(self.generate_while_structural_fallback(
                        name,
                        true,
                        "whileFalse:",
                        &class_name,
                    ));
                }
                "repeat" => {
                    return Ok(self.generate_repeat_structural_fallback(&class_name));
                }
                "onDo" => {
                    return Ok(self.generate_on_do_structural_fallback(&class_name));
                }
                "ensure" => {
                    return Ok(self.generate_ensure_structural_fallback(&class_name));
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
                // BT-2888: `do:`/`collect:`/`select:`/`reject:`/`inject:into:` are
                // real BIF-lowered bodies, already correct for a Tier 1 (pure)
                // block via generic dispatch. Guard against a Tier 2 (stateful)
                // block hitting a raw arity crash instead of a clear error — see
                // `generate_stateful_block_guard`.
                // BT-2913: `doWithKey:` (2-arg-block convention, same shape as
                // `inject:into:`'s block) has the identical gap — extend the
                // same guard. `keysAndValuesDo:` is self-hosted as `self
                // doWithKey: block` (Dictionary.bt), so it inherits the fix
                // once `doWithKey:` itself is guarded.
                let stateful_guard_block_param = match name {
                    "do:" | "collect:" | "select:" | "reject:" | "doWithKey:" => params.first(),
                    "inject:into:" => params.get(1),
                    _ => None,
                };
                if let Some(block_param) = stateful_guard_block_param {
                    let pure_arity = if matches!(name, "inject:into:" | "doWithKey:") {
                        2
                    } else {
                        1
                    };
                    return Ok(self.generate_stateful_block_guard(
                        block_param,
                        pure_arity,
                        name,
                        &class_name,
                        code,
                    ));
                }
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
            //    unmapped quoted primitives/intrinsics through their
            //    hand-written `beamtalk_X:dispatch` module (e.g. Actor's
            //    `actorPid`, now a quoted `@intrinsic`).
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
        //
        // BT-2803 follow-up: for a structural intrinsic, this placeholder body is
        // never a real implementation of the selector's semantics — it self-calls
        // `<runtime_module>:dispatch(<intrinsic_name_atom>, Args, Self)`, passing
        // the *intrinsic name* (e.g. `blockValue`), not the real selector. Any
        // call path that reaches the compiled method body directly instead of
        // through the call-site interception — e.g.
        // `[42] perform: #value withArguments: #()` — resolves to this
        // placeholder and raises `does_not_understand` for the intrinsic name.
        //
        // BT-2812: Block's `value`/`value:`/`value:value:`/`value:value:value:`/
        // `valueWithArguments:` are now special-cased above and no longer hit this
        // placeholder — those were the concrete repro. BT-2812's audit found the
        // same root cause (wrong dispatch key) also applies, in principle, to
        // every other structural intrinsic without a special case here — e.g.
        // `whileTrue`/`whileFalse`/`repeat`/`onDo`/`ensure` (Block) and
        // `listDo`/`listCollect`/`listSelect`/`listReject`/`listInjectInto`
        // (List/Collection). BT-2888 fixed the List/Collection family (guarding
        // their existing correct Tier 1 bodies against a Tier 2 receiver) and
        // BT-2908 fixed the Block loop/exception-handling family above (a real
        // generic reimplementation for Tier 1, a clear error for Tier 2) — this
        // placeholder now only remains live for primitives with no BIF lowering
        // and no structural special-case, not for any of the originally-audited
        // selectors.
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

        let params_doc = beamtalk_cerl_doc::join(
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

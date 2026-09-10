// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code generation error types.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! `CodeGenError`, the crate's structured error enum for code generation
//! failures, and its `Result` alias.

use beamtalk_core::source_analysis::Span;
use std::fmt;
use thiserror::Error;

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

    /// A quoted `@primitive "selector"` in a stdlib value-type class has no
    /// inline BIF lowering registered. Raised only in stdlib mode (actor
    /// classes and a small set of call-site-intercepted operations are
    /// exempt): without a mapping the call would silently fall back to
    /// runtime dispatch and raise `does_not_understand`.
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

    /// A self-send to a same-class class method (`self someSelector`), used
    /// as a statement directly inside a `whileTrue:`/`timesRepeat:`/`to:do:`/
    /// `to:by:do:` loop body in a class method. Such a loop threads only its
    /// own local `StateAcc` through its recursive tail call, never
    /// `ClassVars`, so any class-variable mutation the self-send makes would
    /// be silently discarded at the end of every iteration — rejected at
    /// compile time instead. Scoped to this loop shape only, not
    /// `Foldl`-shaped bodies (`do:`/`collect:`/`select:`/`inject:into:`/...),
    /// which routinely and legitimately use a self-send's return value. See
    /// ADR 0111 Addendum 9.
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

    /// A self-send to a same-class class method inside a block that compiles
    /// through the generic "plain fun"/Tier 2 fallback (`select:`/`collect:`/
    /// `do:`/etc. arguments, a block stored in a local var then invoked, or
    /// any other bare/passed block) in a class method, where the target
    /// selector is not provably free of class-variable mutation (see
    /// `block_analysis::compute_class_var_mutating_selectors`) — such a block
    /// has no way to thread a mutation back to the class method that owns
    /// it. A self-send to a provably pure class method keeps compiling.
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

    /// A class-variable assignment (`self.field := ...`) inside a loop,
    /// conditional, exception handler, or list-op body in a class method
    /// (any shape besides a `whileTrue:`/`timesRepeat:`/`to:do:`/`to:by:do:`
    /// loop that threads `ClassVars` through its own recursive tail call).
    /// These bodies thread field writes via the generic `State`/`StateAcc`
    /// map, which has no class-var branch, so the write would be discarded
    /// once the construct finishes on both normal return and a non-local
    /// return. See ADR 0110's Consequences > Negative section.
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

    /// A `Letrec`- or `Foldl`-shaped loop nested inside another such loop,
    /// where the inner loop's own body would thread a `ClassVars` mutation
    /// through its own recursive tail call or fold accumulator, but the
    /// outer loop's own top-level statements don't independently trigger
    /// `ClassVars` threading. Nothing unpacks a nested loop's `ClassVars`
    /// back into the outer loop, so the mutation would be silently
    /// discarded (or, for a `Foldl` nesting, crash `erlc` with an unbound
    /// variable) once the inner loop exits. See ADR 0111 Addendum 9.
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

    /// BT-3484: the value-type (`Self`-threading) mirror of
    /// [`Self::ClassVarMutationLostAcrossNestedLoop`]. A `Letrec`-shaped
    /// loop nested inside another one, where the inner loop's own body
    /// threads a `self.field := ...` value-type mutation through its own
    /// recursive tail call, but the outer loop's own top-level statements
    /// don't independently trigger `Self` threading. Nothing unpacks a
    /// nested loop's trailing `Self` tuple slot back into the outer loop, so
    /// the mutation would be silently discarded once the inner loop exits.
    #[error(
        "Cannot mutate {mutation} inside a loop nested inside another loop, at {location}.\n\n\
             The inner loop's own mutation would be threaded correctly on its own, but the outer \
             loop (whileTrue:/whileFalse:/timesRepeat:/to:do:/to:by:do:) has no field mutation of \
             its own to carry it back out — so it is silently discarded once the inner loop \
             finishes.\n\n\
             Fix: Accumulate into a local variable across both loops, then mutate the field once \
             after the outer loop finishes:\n\
             \x20 // Instead of:\n\
             \x20 1 to: n do: [:i |\n\
             \x20   1 to: n do: [:j | self.total := self.total + 1]].\n\
             \x20 \n\
             \x20 // Write:\n\
             \x20 delta := 0.\n\
             \x20 1 to: n do: [:i |\n\
             \x20   1 to: n do: [:j | delta := delta + 1]].\n\
             \x20 self.total := self.total + delta."
    )]
    ValueSelfMutationLostAcrossNestedLoop {
        /// Description of the inner loop's mutation (e.g. "field 'self.total'").
        mutation: String,
        /// Source location.
        location: String,
    },

    /// Field assignment in a block that can't thread state back — whether the block is
    /// assigned to a variable, passed as an argument, or returned.
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

    /// Block arity mismatch with method-specific hint.
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
    /// BT-3488: builds a [`CodeGenError::FieldAssignmentInUnsupportedBlock`]
    /// from just the field name and location, deriving `field_capitalized`
    /// (the `addTo{Field}:` method suggestion in the message) here.
    ///
    /// The single place that capitalization happens, shared by both producers
    /// of this error — `validate_stored_closure` (the generic stored/opaque
    /// block path, BT-2792) and `reject_unthreadable_value_self_field_write` (the
    /// value-type-write-inside-a-loop-body path) — so the two can't drift
    /// (CLAUDE.md's no-duplicate-implementations rule).
    pub(super) fn field_assignment_in_unsupported_block(field: &str, location: String) -> Self {
        let mut chars = field.chars();
        let field_capitalized = chars
            .next()
            .map(|c| c.to_uppercase().to_string())
            .unwrap_or_default()
            + chars.as_str();
        CodeGenError::FieldAssignmentInUnsupportedBlock {
            field: field.to_string(),
            field_capitalized,
            location,
        }
    }

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

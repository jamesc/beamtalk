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

    /// A `Letrec`-shaped loop nested inside another one, where the inner loop's own body
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

    /// BT-3489: a `self.field := ...` write as a `match:` arm body in
    /// value-type context (reachable only through the `TestCase` immutability
    /// exemption — a genuine `Value subclass:` can never write `self.field :=`
    /// at all, per ADR 0042).
    ///
    /// The Actor form of this shape threads correctly — `generate_match_arm_body`
    /// routes it through the same `generate_conditional_branch_inline` branch-merge
    /// an `ifTrue:` branch's field write uses. The value type's `Self`/`SelfN`
    /// version chain (`VersionPrefix::SelfVt`) has its own, separate merge
    /// machinery (`generate_vt_conditional_open`'s `ThreadedFamilies`
    /// trailing-slot tuple, ADR 0122 / BT-3513), wired for exactly the two
    /// arms of `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` — a `match:`'s N
    /// pattern arms have no equivalent yet, so
    /// the arm's `Self{N}` binding never escapes its own `case` clause and `erlc`
    /// rejects the module (`unbound variable 'Self1'`). Rejected here rather than
    /// left to crash, mirroring how every other unsupported value-type field-write
    /// position produces a clean diagnostic.
    #[error(
        "Cannot assign to field 'self.{field}' inside a match: arm at {location}.\n\n\
             A value type's field writes thread through a separate `Self` version chain that \
             only ifTrue:/ifFalse:/ifTrue:ifFalse: can merge back today — a match: arm's write \
             never escapes its own case clause. (The same code in an `Actor subclass:` method \
             threads correctly.)\n\n\
             Fix: capture the match: result and assign the field once afterwards:\n\
             \x20 // Instead of:\n\
             \x20 v match: [1 -> self.{field} := self.{field} + 10; _ -> self.{field} := self.{field} + 1].\n\
             \x20 \n\
             \x20 // Write:\n\
             \x20 delta := v match: [1 -> 10; _ -> 1].\n\
             \x20 self.{field} := self.{field} + delta."
    )]
    ValueSelfFieldAssignmentInMatchArm {
        /// The field being assigned.
        field: String,
        /// Source location.
        location: String,
    },

    /// Field assignment in a block that can't thread state back — whether the block is
    /// assigned to a variable, passed as an argument, or returned.
    ///
    /// BT-3491: this diagnostic is shared by both producers — the BT-2792
    /// Actor stored-closure path and the `ValueType` `Foldl*`-shape rejection
    /// (`reject_unthreadable_value_self_field_write`) — so its fix suggestion
    /// can only use wording that is correct in both. An inline
    /// `items do: [:item | self.{field} := ...]` rewrite is only valid in the
    /// Actor case; in `ValueType` context it reproduces the exact same error.
    /// Only the `addTo{field_capitalized}:` method extraction is valid in
    /// both, so that's the only fix offered here. (A class-variable write is
    /// an in-place `put` into the class process, ADR 0130, so a class method
    /// never reaches this error.)
    #[error(
        "Cannot assign to field '{field}' inside this block at {location}.\n\n\
             Field assignments only thread state back to the actor when the block is used \
             directly with a control-flow construct (ifTrue:/whileTrue:/do:/collect:/...), \
             sent directly to self, or immediately invoked (`[...] value`, `[...] value: arg`, \
             `[...] value:value:`, etc.) — not when it's stored in a variable, passed to a \
             user-defined method, or returned as a value.\n\n\
             Fix: Extract the mutation into a method:\n\
             \x20 // Instead of:\n\
             \x20 myBlock := [:item | self.{field} := self.{field} + item].\n\
             \x20 items do: myBlock.\n\
             \x20 \n\
             \x20 // Use a method:\n\
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

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::source_analysis::Span;

    #[test]
    fn span_returns_some_for_unsupported_feature() {
        let span = Span::new(5, 15);
        let err = CodeGenError::UnsupportedFeature {
            feature: "closures".to_string(),
            span: Some(span),
        };
        assert_eq!(err.span(), Some(span));
    }

    #[test]
    fn span_returns_some_for_unmapped_primitive() {
        let span = Span::new(0, 5);
        let err = CodeGenError::UnmappedPrimitive {
            class: "Integer".to_string(),
            selector: "factorial".to_string(),
            span: Some(span),
        };
        assert_eq!(err.span(), Some(span));
    }

    #[test]
    fn span_returns_none_for_internal() {
        let err = CodeGenError::Internal("some error".to_string());
        assert_eq!(err.span(), None);
    }

    #[test]
    fn span_returns_none_for_value_self_mutation_lost_across_nested_loop() {
        let err = CodeGenError::ValueSelfMutationLostAcrossNestedLoop {
            mutation: "field 'self.total'".to_string(),
            location: "MyClass:myMethod:5".to_string(),
        };
        assert_eq!(err.span(), None);
    }

    #[test]
    fn span_returns_none_for_block_arity_mismatch() {
        let err = CodeGenError::BlockArityMismatch {
            selector: "ifNotNil:".to_string(),
            arity: 2,
        };
        assert_eq!(err.span(), None);
    }

    #[test]
    fn display_unsupported_feature_with_span_includes_offset() {
        let err = CodeGenError::UnsupportedFeature {
            feature: "closures".to_string(),
            span: Some(Span::new(10, 20)),
        };
        let s = err.to_string();
        assert!(s.contains("closures"), "expected 'closures' in: {s}");
        assert!(
            s.contains("at offset 10"),
            "expected 'at offset 10' in: {s}"
        );
    }

    #[test]
    fn display_unsupported_feature_without_span_omits_offset() {
        // Exercises the `None => Ok(())` branch in DisplayOptionalSpan::fmt.
        let err = CodeGenError::UnsupportedFeature {
            feature: "closures".to_string(),
            span: None,
        };
        let s = err.to_string();
        assert!(s.contains("closures"), "expected 'closures' in: {s}");
        assert!(!s.contains("at offset"), "unexpected 'at offset' in: {s}");
    }

    #[test]
    fn display_block_arity_mismatch() {
        let err = CodeGenError::BlockArityMismatch {
            selector: "ifNotNil:".to_string(),
            arity: 2,
        };
        let s = err.to_string();
        assert!(s.contains("ifNotNil:"), "expected 'ifNotNil:' in: {s}");
        assert!(
            s.contains("0 or 1 arguments"),
            "expected '0 or 1 arguments' in: {s}"
        );
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Environment-level refinement layers.
//!
//! A [`RefinementLayer`] is a single narrowing pushed by a rule — it binds one
//! variable to a narrowed type. Layers carry a [`Scope`] declaring how far the
//! refinement survives:
//!
//! * [`Scope::BlockScope`] — the refinement is active inside a block (today's
//!   behaviour for `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` narrowing).
//! * [`Scope::MethodRemainder`] — the refinement outlives the guard and applies
//!   to the remainder of the enclosing method body (BT-2049 post-guard
//!   narrowing after `isNil ifTrue: [^err]`).
//!
//! Extracted from `inference.rs` under BT-2050.

use ecow::EcoString;

use crate::semantic_analysis::type_checker::InferredType;

/// Scope of a [`RefinementLayer`] — how long the refinement survives.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Scope {
    /// Active only inside the block the rule narrowed. Today's default for
    /// `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` / `ifNotNil:` narrowings.
    BlockScope,
    /// Survives past the guard statement into the enclosing method body
    /// (BT-2049: `isNil ifTrue: [^err]` narrows the tested variable for the
    /// rest of the method).
    MethodRemainder,
}

/// A single `variable -> type` binding plus its scope.
///
/// Today, the type environment stores narrowed types as plain bindings; the
/// layer exists to make the scope explicit and future-proof the API for rules
/// that need method-remainder semantics. Multiple layers are pushed as a stack,
/// but since current rules only narrow one variable at a time the stack depth
/// matches the number of active refinements.
#[derive(Debug, Clone)]
pub(crate) struct RefinementLayer {
    /// Variable name being refined. May be a synthetic `self.field` key
    /// (BT-2048).
    pub(crate) variable: EcoString,
    /// The refined type.
    pub(crate) ty: InferredType,
    /// Scope — block-local or method-remainder.
    pub(crate) scope: Scope,
}

impl RefinementLayer {
    /// Build a block-scoped refinement for the common narrowing path.
    pub(crate) fn block_scope(variable: impl Into<EcoString>, ty: InferredType) -> Self {
        Self {
            variable: variable.into(),
            ty,
            scope: Scope::BlockScope,
        }
    }

    /// Build a method-remainder refinement (post-guard narrowing, BT-2049).
    pub(crate) fn method_remainder(variable: impl Into<EcoString>, ty: InferredType) -> Self {
        Self {
            variable: variable.into(),
            ty,
            scope: Scope::MethodRemainder,
        }
    }
}

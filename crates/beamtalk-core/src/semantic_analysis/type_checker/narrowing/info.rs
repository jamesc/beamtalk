// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Narrowing data types.
//!
//! Describes control-flow narrowings detected from type-test expressions,
//! plus the scope over which a refinement applies.
//!
//! Extracted from `inference.rs` under BT-2050.

use ecow::EcoString;

use crate::semantic_analysis::type_checker::{EnvKey, InferredType};

/// Describes a control-flow narrowing detected from a type-test expression.
///
/// When a Boolean-producing expression like `x class = Foo` or `x isNil` is used
/// as the receiver of `ifTrue:`/`ifFalse:`, the type checker narrows the tested
/// variable inside the block scope (ADR 0068 Phase 1g).
///
/// The `respondsTo:` variant (ADR 0068 Phase 2e) initially narrows to `Dynamic`,
/// then `refine_responds_to_narrowing` consults the protocol registry: if exactly
/// one protocol requires the tested selector, the type is refined to that
/// protocol (BT-1833). Multiple or zero matches fall back to `Dynamic`.
#[derive(Debug, Clone)]
pub(crate) struct NarrowingInfo {
    /// The env key being narrowed (local variable or synthetic
    /// `self.<field>` binding, BT-2048 / BT-2062).
    pub(crate) variable: EnvKey,
    /// The type the variable is narrowed to in the *true* branch.
    pub(crate) true_type: InferredType,
    /// The type the variable is narrowed to in the *false* branch, if any.
    ///
    /// When `Some`, the false branch of `ifFalse:` / `ifTrue:ifFalse:` uses
    /// this type.  When `None`, false-branch narrowing falls back to the
    /// `is_nil_check` logic (non-nil stripping) or no narrowing.
    pub(crate) false_type: Option<InferredType>,
    /// Whether this is a nil-check (`isNil`). If so, the *false* branch
    /// narrows to non-nil and early-return narrowing applies.
    pub(crate) is_nil_check: bool,
    /// Whether this is a Result `isOk` / `ok` check (BT-1859).
    ///
    /// When true, the true branch knows `value` is safe (ok variant) and the
    /// false branch knows `error` is safe (error variant).  The actual type
    /// of the variable stays `Result(T, E)` in both branches — the generic
    /// substitution already resolves `value -> T` and `error -> E`.
    pub(crate) is_result_ok_check: bool,
    /// Whether this is a Result `isError` check (BT-1859).
    ///
    /// Inverse of `is_result_ok_check`: true branch is the error variant,
    /// false branch is the ok variant.
    pub(crate) is_result_error_check: bool,
    /// The selector tested in a `respondsTo:` narrowing (ADR 0068 Phase 2e).
    ///
    /// When set, the narrowing was detected from `x respondsTo: #selector`.
    /// Used by `refine_responds_to_narrowing` to look up the matching
    /// protocol in the registry and narrow to that protocol type instead
    /// of `Dynamic` (BT-1833).
    pub(crate) responded_selector: Option<EcoString>,
}

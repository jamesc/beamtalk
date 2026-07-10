// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Narrowing data types.
//!
//! Describes control-flow narrowings detected from type-test expressions,
//! plus the scope over which a refinement applies.
//!
//! Extracted from `inference.rs` under BT-2050.

use std::fmt;

use ecow::{EcoString, eco_format};

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
    /// The singleton (in)equality tested in a `x = #foo` / `#foo = x`
    /// narrowing (BT-2617).
    ///
    /// When set, the narrowing was detected from an (in)equality test against
    /// a singleton symbol literal.  `detect` cannot know the variable's current
    /// union type, so `true_type` / `false_type` are left provisional and
    /// `refine_singleton_narrowing` in `inference.rs` resolves the variable's
    /// type, sets the matching branch to the singleton, and subtracts the
    /// singleton from the union for the complementary branch.
    pub(crate) singleton_eq: Option<SingletonEqInfo>,
    /// The class name (and which idiom tested it) in a `x class = ClassName`
    /// / `x isKindOf: ClassName` narrowing (ADR 0102 §2 group 2, §5,
    /// BT-2741, BT-2744).
    ///
    /// When set, `detect` cannot know the variable's current type, so
    /// `true_type` is left provisional (`Dynamic`) and
    /// `refine_class_narrowing` in `inference.rs` resolves the variable's
    /// current type and narrows the true branch through the hierarchy-aware
    /// `intersect(current, ClassName)` — a subclass test narrows precisely
    /// (`x :: Number; x isKindOf: Integer` true branch is `Integer`), and a
    /// hierarchy-unrelated class test types the (unreachable) true branch
    /// `Never`, reported via `check_impossible_class_comparison`. The false
    /// branch is populated only for `ClassTestKind::KindOf` — see
    /// [`ClassTestKind`].
    pub(crate) class_test: Option<ClassTestInfo>,
}

/// Details of a `x class = ClassName` / `x isKindOf: ClassName` narrowing
/// (ADR 0102 §2 group 2, §5, BT-2741, BT-2744).
#[derive(Debug, Clone)]
pub(crate) struct ClassTestInfo {
    /// The tested class name.
    pub(crate) class_name: EcoString,
    /// Which idiom produced this test — see [`ClassTestKind`] for why only
    /// `isKindOf:`'s false branch can be narrowed.
    pub(crate) kind: ClassTestKind,
}

/// Distinguishes `x isKindOf: ClassName` (subclass-inclusive) from `x class
/// =:= ClassName` (exact runtime-class equality) — the two idioms that
/// populate [`NarrowingInfo::class_test`].
///
/// **Only `KindOf`'s false branch can be narrowed** via nominal-class
/// `difference` (ADR 0102 §5, BT-2744): `Negation{base, excluded}` always
/// excludes `excluded`'s *entire* subtree (§5 Q1 — membership admits `C <:
/// base` that is *not* `<: excluded`), which matches `isKindOf:`'s negation
/// ("not `C` and not any subclass of `C`") but **not** `class =:=`'s ("not
/// exactly `C`" — `C`'s subclasses are still possible). Concretely: `x ::
/// Number; x class =:= Integer` false does **not** rule out `x` being a
/// `Character` (a sealed `Integer` subclass) — its `class` is `Character`,
/// not `Integer`, so the test is false, yet `x` still satisfies `isKindOf:
/// Integer`. Narrowing that false branch to `Number \ Integer` would
/// therefore wrongly exclude a live possibility and could produce a false
/// "comparison can never be true" hint on a subsequent, perfectly satisfiable
/// `isKindOf: Integer` test. `Exact`'s false branch is left unnarrowed
/// (`None`), exactly as before BT-2744; only its (already-shipped, BT-2741)
/// true branch is affected by set-theoretic narrowing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ClassTestKind {
    /// `x isKindOf: ClassName` — subclass-inclusive.
    KindOf,
    /// `x class =:= ClassName` — exact runtime-class equality.
    Exact,
}

/// Details of a singleton (in)equality narrowing (`x = #foo`, BT-2617).
#[derive(Debug, Clone)]
pub(crate) struct SingletonEqInfo {
    /// The singleton type name tested against (e.g. `#infinity`).
    pub(crate) singleton: SingletonName,
    /// Whether the test was an *inequality* (`/=`, `=/=`).  When true the
    /// true/false branches are swapped relative to an equality test: the true
    /// branch removes the singleton and the false branch narrows to it.
    pub(crate) negated: bool,
}

/// A bare-symbol singleton type name (`#foo`) — the leading `#` is guaranteed
/// **by construction** (BT-2764).
///
/// The narrowing paths (`refine_singleton_narrowing`,
/// `check_impossible_singleton_comparison` → `type_admits_singleton`) rely on
/// the tested name being a singleton, never a nominal class: singletons are
/// not entries in the class hierarchy, so a nominal name smuggled through
/// would silently mis-answer membership checks in release builds. This
/// newtype enforces that precondition at the type level instead of a
/// `debug_assert!`.
///
/// The stored spelling includes the leading `#` (e.g. `#infinity`), matching
/// the `InferredType::Known { class_name }` spelling produced by
/// `type_resolver` for `TypeAnnotation::Singleton`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SingletonName(EcoString);

impl SingletonName {
    /// Builds a singleton name from a bare symbol identifier *without* the
    /// leading `#` (the spelling stored in `Literal::Symbol`), prefixing `#`
    /// so the invariant holds by construction.
    pub(crate) fn from_symbol_identifier(name: &str) -> Self {
        Self(eco_format!("#{name}"))
    }

    /// The singleton's `InferredType::Known { class_name }` spelling,
    /// including the leading `#`.
    pub(crate) fn as_type_name(&self) -> &EcoString {
        &self.0
    }

    /// The singleton's spelling as a `&str`, including the leading `#`.
    /// Production code goes through [`as_type_name`](Self::as_type_name) or
    /// `Display`; this exists for test assertions.
    #[cfg(test)]
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for SingletonName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Typed keys for [`TypeEnv`](super::TypeEnv) (BT-2062).
//!
//! Before BT-2062 the environment was keyed by `EcoString`, with a string
//! convention — `"self.field"` — used to distinguish a narrowed state field
//! from a local named `self.field` (which is impossible, but the parser
//! didn't know that). Every narrowing site that touched state fields had
//! to construct or strip the prefix by hand, turning a quiet typo
//! (`"slef.field"`) into a silent loss of narrowing. The typed key rules
//! that footgun out.
//!
//! Two variants cover all current uses:
//!
//! * [`EnvKey::Local`] — a lexical binding (parameter, local, or the special
//!   `self` receiver).
//! * [`EnvKey::SelfField`] — the synthetic binding used by narrowing rules
//!   for `self.<field>` reads and writes (BT-2048).
//!
//! Adding a new key shape (e.g. temporaries, class-side bindings) is a matter
//! of extending the enum and updating the exhaustive match in consumers; the
//! compiler will point at every site that needs attention.

use ecow::EcoString;

/// A typed key into the [`TypeEnv`](super::TypeEnv).
///
/// The enum keeps each binding's shape explicit rather than leaning on a
/// string convention. See the module docs for background.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub(crate) enum EnvKey {
    /// A lexical binding — parameter, local variable, or the `self` receiver.
    Local(EcoString),
    /// A synthetic `self.<field>` binding used by the narrowing machinery
    /// (BT-2048). Stored alongside locals in the env so that the
    /// `true`/`false` branches of an `isNil` check can refine field types
    /// without mutating the class hierarchy.
    SelfField(EcoString),
}

impl EnvKey {
    /// Build a local binding key.
    pub(crate) fn local(name: impl Into<EcoString>) -> Self {
        Self::Local(name.into())
    }

    /// Build a synthetic `self.<field>` binding key.
    pub(crate) fn self_field(name: impl Into<EcoString>) -> Self {
        Self::SelfField(name.into())
    }
}

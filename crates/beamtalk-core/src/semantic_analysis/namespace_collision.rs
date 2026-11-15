// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The shared diagnostic for a type alias or protocol whose name collides with
//! another entry in the class/protocol namespace (ADR 0108, ADR 0068).
//!
//! One constructor, so the alias and protocol registries phrase the same
//! error the same way.

use ecow::EcoString;

use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

/// What is being declared (the colliding newcomer).
#[derive(Debug, Clone, Copy)]
pub(super) enum Declared {
    TypeAlias,
    Protocol,
}

impl Declared {
    fn noun(self) -> &'static str {
        match self {
            Self::TypeAlias => "type alias",
            Self::Protocol => "protocol",
        }
    }

    fn namespace_note(self) -> &'static str {
        match self {
            Self::TypeAlias => "type aliases share the class/protocol namespace",
            Self::Protocol => "protocols and classes share a namespace",
        }
    }
}

/// The already-registered entry the newcomer collides with.
#[derive(Debug, Clone, Copy)]
pub(super) enum Existing {
    Class,
    Protocol,
}

impl Existing {
    fn noun(self) -> &'static str {
        match self {
            Self::Class => "class",
            Self::Protocol => "protocol",
        }
    }
}

/// Where the colliding declaration came from.
#[derive(Debug, Clone, Copy)]
pub(super) enum Origin {
    /// Declared in the module being analysed.
    Local,
    /// Seeded from cross-file (pre-loaded) information, so the fix is in the
    /// declaring file.
    PreLoaded,
}

/// Builds the namespace-collision error for `name` at `span`.
pub(super) fn namespace_collision(
    declared: Declared,
    existing: Existing,
    origin: Origin,
    name: &EcoString,
    span: Span,
) -> Diagnostic {
    let (new, old, note) = (declared.noun(), existing.noun(), declared.namespace_note());
    let (message, hint) = match origin {
        Origin::Local => (
            format!("`{name}` is already defined as a {old} — {note}"),
            format!("Rename the {new} to avoid conflicting with {old} `{name}`"),
        ),
        Origin::PreLoaded => (
            format!("Pre-loaded {new} `{name}` collides with {old} of the same name — {note}"),
            format!(
                "Rename the {new} in its defining file to avoid conflicting with {old} `{name}`"
            ),
        ),
    };
    Diagnostic::error(message, span)
        .with_hint(hint)
        .with_category(DiagnosticCategory::Type)
}

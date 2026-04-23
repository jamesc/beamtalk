// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Well-known class names used throughout the type checker.
//!
//! Centralises the fixed set of class-name string literals that the type checker,
//! narrowing rules, and type resolver compare against. Using an enum instead of
//! ad-hoc string comparisons eliminates typo risks (e.g., `"undefined_object"`
//! silently under-narrowing) and makes it trivial to add new well-known classes:
//! just add a variant and its string mapping.
//!
//! **DDD Context:** Semantic Analysis
//!
//! **References:** BT-2064, BT-2016 (`Nil` vs `UndefinedObject` drift)

/// A class name that the type checker recognises and dispatches on.
///
/// Adding a new well-known class is a two-step change:
/// 1. Add the variant here.
/// 2. Add the `&'static str` mapping in [`as_str`](Self::as_str).
///
/// All call sites that match on the enum automatically get exhaustiveness
/// checking — no string grep required.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) enum WellKnownClass {
    /// The canonical nil class — `UndefinedObject`.
    UndefinedObject,
    /// Legacy alias for nil (BT-2016). Some code paths and BEAM metadata
    /// produce `"Nil"` instead of `"UndefinedObject"`; both must be treated
    /// as the nil type.
    Nil,
    /// Block (closure) type — parameterised as `Block(P1, ..., Pn, R)`.
    Block,
    /// Integer literal type.
    Integer,
    /// String literal type.
    String,
    /// Boolean type.
    Boolean,
    /// Bottom type name (used in annotation resolution).
    Never,
    /// Root of the class hierarchy.
    Object,
    /// Generic result type — `Result(T, E)`.
    Result,
    /// Proxy type for Erlang module references (`Erlang lists` etc.).
    ErlangModule,
    /// Top type / unknown — used as a fallback display name.
    Dynamic,
}

impl WellKnownClass {
    /// Returns the canonical string representation of this class name.
    #[must_use]
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            Self::UndefinedObject => "UndefinedObject",
            Self::Nil => "Nil",
            Self::Block => "Block",
            Self::Integer => "Integer",
            Self::String => "String",
            Self::Boolean => "Boolean",
            Self::Never => "Never",
            Self::Object => "Object",
            Self::Result => "Result",
            Self::ErlangModule => "ErlangModule",
            Self::Dynamic => "Dynamic",
        }
    }

    /// Attempts to match a string against well-known class names.
    ///
    /// Returns `None` for user-defined class names (e.g., `"Counter"`,
    /// `"MyActor"`).
    #[must_use]
    pub(crate) fn from_str(s: &str) -> Option<Self> {
        match s {
            "UndefinedObject" => Some(Self::UndefinedObject),
            "Nil" => Some(Self::Nil),
            "Block" => Some(Self::Block),
            "Integer" => Some(Self::Integer),
            "String" => Some(Self::String),
            "Boolean" => Some(Self::Boolean),
            "Never" => Some(Self::Never),
            "Object" => Some(Self::Object),
            "Result" => Some(Self::Result),
            "ErlangModule" => Some(Self::ErlangModule),
            "Dynamic" => Some(Self::Dynamic),
            _ => None,
        }
    }

    /// Returns `true` if this class represents the nil type.
    ///
    /// Both `UndefinedObject` (canonical) and `Nil` (legacy alias, BT-2016)
    /// are considered nil classes. This replaces the fragile pattern:
    /// ```text
    /// class_name.as_str() == "UndefinedObject" || class_name.as_str() == "Nil"
    /// ```
    #[must_use]
    pub(crate) fn is_nil_class(self) -> bool {
        matches!(self, Self::UndefinedObject | Self::Nil)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_all_variants() {
        let variants = [
            WellKnownClass::UndefinedObject,
            WellKnownClass::Nil,
            WellKnownClass::Block,
            WellKnownClass::Integer,
            WellKnownClass::String,
            WellKnownClass::Boolean,
            WellKnownClass::Never,
            WellKnownClass::Object,
            WellKnownClass::Result,
            WellKnownClass::ErlangModule,
            WellKnownClass::Dynamic,
        ];
        for v in variants {
            let s = v.as_str();
            let parsed = WellKnownClass::from_str(s);
            assert_eq!(parsed, Some(v), "round-trip failed for {s}");
        }
    }

    #[test]
    fn from_str_returns_none_for_unknown() {
        assert_eq!(WellKnownClass::from_str("Counter"), None);
        assert_eq!(WellKnownClass::from_str("MyActor"), None);
        assert_eq!(WellKnownClass::from_str(""), None);
    }

    #[test]
    fn is_nil_class_matches_both_spellings() {
        assert!(WellKnownClass::UndefinedObject.is_nil_class());
        assert!(WellKnownClass::Nil.is_nil_class());
        assert!(!WellKnownClass::Block.is_nil_class());
        assert!(!WellKnownClass::Integer.is_nil_class());
        assert!(!WellKnownClass::Dynamic.is_nil_class());
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Well-known message selectors used throughout the compiler.
//!
//! Centralises the fixed set of selector-name string literals that codegen,
//! narrowing rules, lint, and state-threading compare against. Using an enum
//! instead of ad-hoc string comparisons eliminates typo risks and makes it
//! trivial to add new well-known selectors: just add a variant and its string
//! mapping.
//!
//! **DDD Context:** Shared Kernel (AST layer — consumed by Semantic Analysis,
//! Code Generation, and Language Service)
//!
//! **References:** BT-2069, BT-2065 (parent epic), BT-2064 (`WellKnownClass`)

use super::MessageSelector;

/// A message selector that the compiler recognises and may intrinsify.
///
/// Adding a new well-known selector is a two-step change:
/// 1. Add the variant here.
/// 2. Add the string mapping in [`from_name`](Self::from_name).
///
/// All call sites that match on the enum automatically get exhaustiveness
/// checking — no string grep required.
///
/// # Block application modelling
///
/// Block `value`/`value:`/`value:value:`/`value:value:value:` are modelled as
/// separate variants rather than a `BlockValue { arity: u8 }` struct variant.
/// This keeps downstream `match` arms flat and avoids a nested guard for the
/// common case of matching a specific arity (e.g. codegen inlining `value`
/// differently from `value:value:`).
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum WellKnownSelector {
    // --- Nil testing / conditional ---
    /// `isNil` — returns `true` if the receiver is nil.
    IsNil,
    /// `notNil` — returns `true` if the receiver is not nil.
    NotNil,
    /// `ifNil:` — evaluate block if receiver is nil.
    IfNil,
    /// `ifNotNil:` — evaluate block if receiver is not nil.
    IfNotNil,
    /// `ifNil:ifNotNil:` — two-arm nil conditional.
    IfNilIfNotNil,
    /// `ifNotNil:ifNil:` — two-arm nil conditional (reversed argument order).
    IfNotNilIfNil,

    // --- Type testing ---
    /// `isKindOf:` — runtime type test against a class.
    IsKindOf,
    /// `class` — returns the receiver's class object.
    Class,
    /// `respondsTo:` — checks if the receiver implements a selector.
    RespondsTo,

    // --- Boolean conditionals ---
    /// `ifTrue:` — evaluate block if receiver is `true`.
    IfTrue,
    /// `ifFalse:` — evaluate block if receiver is `false`.
    IfFalse,
    /// `ifTrue:ifFalse:` — two-arm boolean conditional.
    IfTrueIfFalse,

    // --- Exception handling ---
    /// `on:do:` — evaluate receiver block, catch matching exceptions.
    OnDo,

    // --- Block application (one variant per arity) ---
    /// `value` — evaluate a zero-argument block.
    Value,
    /// `value:` — evaluate a one-argument block.
    ValueColon,
    /// `value:value:` — evaluate a two-argument block.
    ValueValue,
    /// `value:value:value:` — evaluate a three-argument block.
    ValueValueValue,

    // --- Result testing ---
    /// `isOk` — returns `true` if the result/tuple is an ok variant.
    IsOk,
    /// `isError` — returns `true` if the result/tuple is an error variant.
    IsError,
    /// `isOk:` — keyword form for result ok testing (reserved for future use).
    IsOkColon,
    /// `isError:` — keyword form for result error testing (reserved for future use).
    IsErrorColon,
}

impl WellKnownSelector {
    /// Returns the canonical selector-name string for this variant.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::IsNil => "isNil",
            Self::NotNil => "notNil",
            Self::IfNil => "ifNil:",
            Self::IfNotNil => "ifNotNil:",
            Self::IfNilIfNotNil => "ifNil:ifNotNil:",
            Self::IfNotNilIfNil => "ifNotNil:ifNil:",
            Self::IsKindOf => "isKindOf:",
            Self::Class => "class",
            Self::RespondsTo => "respondsTo:",
            Self::IfTrue => "ifTrue:",
            Self::IfFalse => "ifFalse:",
            Self::IfTrueIfFalse => "ifTrue:ifFalse:",
            Self::OnDo => "on:do:",
            Self::Value => "value",
            Self::ValueColon => "value:",
            Self::ValueValue => "value:value:",
            Self::ValueValueValue => "value:value:value:",
            Self::IsOk => "isOk",
            Self::IsError => "isError",
            Self::IsOkColon => "isOk:",
            Self::IsErrorColon => "isError:",
        }
    }

    /// Attempts to classify a selector name string as a well-known selector.
    ///
    /// Returns `None` for user-defined selectors (e.g., `"increment"`, `"at:put:"`).
    #[must_use]
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "isNil" => Some(Self::IsNil),
            "notNil" => Some(Self::NotNil),
            "ifNil:" => Some(Self::IfNil),
            "ifNotNil:" => Some(Self::IfNotNil),
            "ifNil:ifNotNil:" => Some(Self::IfNilIfNotNil),
            "ifNotNil:ifNil:" => Some(Self::IfNotNilIfNil),
            "isKindOf:" => Some(Self::IsKindOf),
            "class" => Some(Self::Class),
            "respondsTo:" => Some(Self::RespondsTo),
            "ifTrue:" => Some(Self::IfTrue),
            "ifFalse:" => Some(Self::IfFalse),
            "ifTrue:ifFalse:" => Some(Self::IfTrueIfFalse),
            "on:do:" => Some(Self::OnDo),
            "value" => Some(Self::Value),
            "value:" => Some(Self::ValueColon),
            "value:value:" => Some(Self::ValueValue),
            "value:value:value:" => Some(Self::ValueValueValue),
            "isOk" => Some(Self::IsOk),
            "isError" => Some(Self::IsError),
            "isOk:" => Some(Self::IsOkColon),
            "isError:" => Some(Self::IsErrorColon),
            _ => None,
        }
    }

    /// Classifies a [`MessageSelector`] as a well-known selector.
    ///
    /// This is the primary entry point. The selector's *kind* (unary vs keyword)
    /// must match the variant's expected kind — a `MessageSelector::Binary("class")`
    /// is not classified as `Class`, even though the name matches, because it
    /// could not have been produced by the parser.
    ///
    /// Returns `None` for user-defined selectors and for kind/name mismatches.
    #[must_use]
    pub fn from_selector(selector: &MessageSelector) -> Option<Self> {
        let known = Self::from_name(&selector.name())?;
        match (selector, known) {
            (
                MessageSelector::Unary(_),
                Self::IsNil | Self::NotNil | Self::Class | Self::IsOk | Self::IsError | Self::Value,
            )
            | (
                MessageSelector::Keyword(_),
                Self::IfNil
                | Self::IfNotNil
                | Self::IfNilIfNotNil
                | Self::IfNotNilIfNil
                | Self::IsKindOf
                | Self::RespondsTo
                | Self::IfTrue
                | Self::IfFalse
                | Self::IfTrueIfFalse
                | Self::OnDo
                | Self::ValueColon
                | Self::ValueValue
                | Self::ValueValueValue
                | Self::IsOkColon
                | Self::IsErrorColon,
            ) => Some(known),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::KeywordPart;
    use crate::source_analysis::Span;

    /// All variants, used for exhaustive round-trip testing.
    const ALL_VARIANTS: &[WellKnownSelector] = &[
        WellKnownSelector::IsNil,
        WellKnownSelector::NotNil,
        WellKnownSelector::IfNil,
        WellKnownSelector::IfNotNil,
        WellKnownSelector::IfNilIfNotNil,
        WellKnownSelector::IfNotNilIfNil,
        WellKnownSelector::IsKindOf,
        WellKnownSelector::Class,
        WellKnownSelector::RespondsTo,
        WellKnownSelector::IfTrue,
        WellKnownSelector::IfFalse,
        WellKnownSelector::IfTrueIfFalse,
        WellKnownSelector::OnDo,
        WellKnownSelector::Value,
        WellKnownSelector::ValueColon,
        WellKnownSelector::ValueValue,
        WellKnownSelector::ValueValueValue,
        WellKnownSelector::IsOk,
        WellKnownSelector::IsError,
        WellKnownSelector::IsOkColon,
        WellKnownSelector::IsErrorColon,
    ];

    #[test]
    fn round_trip_all_variants() {
        for &v in ALL_VARIANTS {
            let s = v.as_str();
            let parsed = WellKnownSelector::from_name(s);
            assert_eq!(parsed, Some(v), "round-trip failed for {s}");
        }
    }

    #[test]
    fn from_name_returns_none_for_unknown() {
        assert_eq!(WellKnownSelector::from_name("increment"), None);
        assert_eq!(WellKnownSelector::from_name("at:put:"), None);
        assert_eq!(WellKnownSelector::from_name(""), None);
        assert_eq!(WellKnownSelector::from_name("doSomething:"), None);
    }

    #[test]
    fn from_selector_unary() {
        let sel = MessageSelector::Unary("isNil".into());
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IsNil)
        );

        let sel = MessageSelector::Unary("notNil".into());
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::NotNil)
        );

        let sel = MessageSelector::Unary("class".into());
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::Class)
        );

        let sel = MessageSelector::Unary("value".into());
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::Value)
        );

        let sel = MessageSelector::Unary("isOk".into());
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IsOk)
        );

        let sel = MessageSelector::Unary("isError".into());
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IsError)
        );
    }

    #[test]
    fn from_selector_keyword_single() {
        let span = Span::new(0, 1);

        let sel = MessageSelector::Keyword(vec![KeywordPart::new("ifTrue:", span)]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IfTrue)
        );

        let sel = MessageSelector::Keyword(vec![KeywordPart::new("ifFalse:", span)]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IfFalse)
        );

        let sel = MessageSelector::Keyword(vec![KeywordPart::new("isKindOf:", span)]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IsKindOf)
        );

        let sel = MessageSelector::Keyword(vec![KeywordPart::new("respondsTo:", span)]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::RespondsTo)
        );

        let sel = MessageSelector::Keyword(vec![KeywordPart::new("value:", span)]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::ValueColon)
        );

        let sel = MessageSelector::Keyword(vec![KeywordPart::new("ifNil:", span)]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IfNil)
        );

        let sel = MessageSelector::Keyword(vec![KeywordPart::new("ifNotNil:", span)]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IfNotNil)
        );
    }

    #[test]
    fn from_selector_keyword_multi() {
        let span = Span::new(0, 1);

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("ifTrue:", span),
            KeywordPart::new("ifFalse:", span),
        ]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IfTrueIfFalse)
        );

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("ifNil:", span),
            KeywordPart::new("ifNotNil:", span),
        ]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IfNilIfNotNil)
        );

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("ifNotNil:", span),
            KeywordPart::new("ifNil:", span),
        ]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::IfNotNilIfNil)
        );

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("on:", span),
            KeywordPart::new("do:", span),
        ]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::OnDo)
        );

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("value:", span),
            KeywordPart::new("value:", span),
        ]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::ValueValue)
        );

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("value:", span),
            KeywordPart::new("value:", span),
            KeywordPart::new("value:", span),
        ]);
        assert_eq!(
            WellKnownSelector::from_selector(&sel),
            Some(WellKnownSelector::ValueValueValue)
        );
    }

    #[test]
    fn from_selector_binary_returns_none() {
        // Binary selectors (e.g., `+`, `=`) are never well-known selectors.
        let sel = MessageSelector::Binary("+".into());
        assert_eq!(WellKnownSelector::from_selector(&sel), None);

        let sel = MessageSelector::Binary("=".into());
        assert_eq!(WellKnownSelector::from_selector(&sel), None);
    }

    #[test]
    fn from_selector_rejects_kind_name_mismatches() {
        // A binary selector whose name happens to match a unary well-known name
        // (e.g., constructed by hand bypassing the parser) must not classify.
        let sel = MessageSelector::Binary("class".into());
        assert_eq!(WellKnownSelector::from_selector(&sel), None);

        // A unary selector whose name matches a keyword well-known name.
        let sel = MessageSelector::Unary("ifTrue:".into());
        assert_eq!(WellKnownSelector::from_selector(&sel), None);

        // A keyword selector whose flattened name matches a unary well-known name.
        let sel = MessageSelector::Keyword(vec![KeywordPart::new("class", Span::new(0, 1))]);
        assert_eq!(WellKnownSelector::from_selector(&sel), None);
    }

    #[test]
    fn from_selector_unknown_keyword_returns_none() {
        let span = Span::new(0, 1);

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", span),
            KeywordPart::new("put:", span),
        ]);
        assert_eq!(WellKnownSelector::from_selector(&sel), None);
    }

    #[test]
    fn from_selector_unknown_unary_returns_none() {
        let sel = MessageSelector::Unary("increment".into());
        assert_eq!(WellKnownSelector::from_selector(&sel), None);

        let sel = MessageSelector::Unary("size".into());
        assert_eq!(WellKnownSelector::from_selector(&sel), None);
    }

    #[test]
    fn arity_boundaries_value_selectors() {
        // `value` (unary, arity 0) is distinct from `value:` (keyword, arity 1)
        let unary = MessageSelector::Unary("value".into());
        let keyword_1 = MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(0, 1))]);
        let keyword_2 = MessageSelector::Keyword(vec![
            KeywordPart::new("value:", Span::new(0, 1)),
            KeywordPart::new("value:", Span::new(0, 1)),
        ]);
        let keyword_3 = MessageSelector::Keyword(vec![
            KeywordPart::new("value:", Span::new(0, 1)),
            KeywordPart::new("value:", Span::new(0, 1)),
            KeywordPart::new("value:", Span::new(0, 1)),
        ]);

        assert_eq!(
            WellKnownSelector::from_selector(&unary),
            Some(WellKnownSelector::Value)
        );
        assert_eq!(
            WellKnownSelector::from_selector(&keyword_1),
            Some(WellKnownSelector::ValueColon)
        );
        assert_eq!(
            WellKnownSelector::from_selector(&keyword_2),
            Some(WellKnownSelector::ValueValue)
        );
        assert_eq!(
            WellKnownSelector::from_selector(&keyword_3),
            Some(WellKnownSelector::ValueValueValue)
        );

        // Four `value:` parts is NOT a well-known selector
        let keyword_4 = MessageSelector::Keyword(vec![
            KeywordPart::new("value:", Span::new(0, 1)),
            KeywordPart::new("value:", Span::new(0, 1)),
            KeywordPart::new("value:", Span::new(0, 1)),
            KeywordPart::new("value:", Span::new(0, 1)),
        ]);
        assert_eq!(WellKnownSelector::from_selector(&keyword_4), None);
    }

    #[test]
    fn message_selector_well_known_accessor() {
        // Verify the `MessageSelector::well_known()` convenience method works
        let sel = MessageSelector::Unary("isNil".into());
        assert_eq!(sel.well_known(), Some(WellKnownSelector::IsNil));

        let sel = MessageSelector::Keyword(vec![
            KeywordPart::new("ifTrue:", Span::new(0, 1)),
            KeywordPart::new("ifFalse:", Span::new(0, 1)),
        ]);
        assert_eq!(sel.well_known(), Some(WellKnownSelector::IfTrueIfFalse));

        let sel = MessageSelector::Unary("customMethod".into());
        assert_eq!(sel.well_known(), None);
    }
}

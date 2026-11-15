// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared signature-text helpers for hover, completion and signature help.
//!
//! All three render a method's parameter types and split its selector into
//! keyword parts; this module owns those two steps so they cannot drift.

use beamtalk_core::unparse::SignatureParam;

/// Renders each optional parameter type to its display text.
///
/// Works for `MethodInfo`'s structured `DeclaredType`s (whose `Display`
/// matches `TypeAnnotation::type_name` verbatim) and for protocol
/// requirements' already-textual types alike.
pub(crate) fn param_type_texts<'a, T, I>(param_types: I) -> Vec<Option<String>>
where
    T: ToString + 'a,
    I: IntoIterator<Item = &'a Option<T>>,
{
    param_types
        .into_iter()
        .map(|ty| ty.as_ref().map(ToString::to_string))
        .collect()
}

/// Renders an optional type (e.g. a return type) to its display text.
pub(crate) fn type_text<T: ToString>(ty: Option<&T>) -> Option<String> {
    ty.map(ToString::to_string)
}

/// Splits a keyword selector into its parts, re-adding the trailing `:` the
/// split strips: `at:put:` -> `["at:", "put:"]`.
pub(crate) fn keyword_parts(selector: &str) -> Vec<String> {
    selector
        .split(':')
        .filter(|s| !s.is_empty())
        .map(|part| format!("{part}:"))
        .collect()
}

/// Pairs each keyword part with the type text at the same index, as nameless
/// [`SignatureParam`]s (the class hierarchy keeps types but not names).
pub(crate) fn keyword_params<'a>(
    keywords: &'a [String],
    param_type_texts: &'a [Option<String>],
) -> Vec<SignatureParam<'a>> {
    keywords
        .iter()
        .enumerate()
        .map(|(i, keyword)| SignatureParam {
            keyword,
            name: None,
            type_text: param_type_texts.get(i).and_then(Option::as_deref),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_parts_re_adds_colons() {
        assert_eq!(keyword_parts("at:put:"), ["at:", "put:"]);
        // Callers pass keyword selectors only; a unary name is treated as one part.
        assert_eq!(keyword_parts("size"), ["size:"]);
    }

    #[test]
    fn keyword_params_pairs_types_by_index() {
        let keywords = keyword_parts("at:put:");
        let types = param_type_texts(&[Some("Integer".to_string()), None]);
        let params = keyword_params(&keywords, &types);
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].keyword, "at:");
        assert_eq!(params[0].type_text, Some("Integer"));
        assert_eq!(params[1].type_text, None);
        assert!(params.iter().all(|p| p.name.is_none()));
    }

    #[test]
    fn keyword_params_tolerates_missing_types() {
        let keywords = keyword_parts("a:b:");
        let params = keyword_params(&keywords, &[]);
        assert_eq!(params.len(), 2);
        assert!(params.iter().all(|p| p.type_text.is_none()));
    }
}

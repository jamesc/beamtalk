// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared naming authority for compiler-synthesized `Value subclass:` methods.
//!
//! `Value` classes get auto-generated slot accessors, `with<Field>:` copy-setters,
//! and a keyword constructor (ADR 0042). The *names* of those selectors are a
//! convention that must agree across three consumers:
//!
//! - **codegen** (`value_type_codegen`, `gen_server::methods`) — emits the Core
//!   Erlang functions and their exports.
//! - **`class_hierarchy`** — synthesizes matching [`MethodInfo`](crate::semantic_analysis::MethodInfo)
//!   entries so the type checker and LSP know the methods exist before codegen runs.
//!
//! Keeping the selector-name computation here (rather than duplicating it in each
//! consumer) makes this module the single source of truth and removes the drift
//! risk that previously existed between codegen and the hierarchy.

/// Computes the `with*:` copy-setter selector name for a slot field.
///
/// Capitalises the first letter of the field name and prepends `"with"`:
/// - `"x"` → `"withX:"`
/// - `"firstName"` → `"withFirstName:"`
/// - `""` → `"with:"`
#[must_use]
pub fn with_star_selector(field_name: &str) -> String {
    let mut chars = field_name.chars();
    match chars.next() {
        None => "with:".to_string(),
        Some(first) => {
            let cap: String = first.to_uppercase().collect();
            format!("with{}{}:", cap, chars.as_str())
        }
    }
}

/// Returns `true` if `selector_name` has the shape a `with_star_selector`
/// call could have produced — the single-keyword-part `with<Field>:`
/// naming convention: the literal prefix `"with"`, an upper-cased first
/// letter, and a trailing `:` (e.g. `"withX:"`, `"withFirstName:"`, but not
/// `"withdraw:"`, `"with:"` — the empty-field-name degenerate case — or the
/// unary `"withCounter"` with no colon at all, since a bare unary selector
/// can never be the single keyword part this convention describes).
///
/// This is the recognition counterpart to [`with_star_selector`]'s
/// generation, kept in the same module as the single naming authority for
/// this convention (see the module doc comment) so callers on both sides of
/// the `beamtalk-core` / `beamtalk-lint` boundary — which can only depend on
/// `beamtalk-core`, never the reverse — share one implementation instead of
/// each re-deriving the shape check.
///
/// Callers that only have a full multi-part keyword selector string (e.g.
/// `"at:put:"`) should first confirm it is a *single* keyword part — this
/// function only judges the naming shape, not part count, since that
/// requires the caller's own selector representation (an AST
/// `MessageSelector`'s parts list, or a `MethodDefinition`'s parameter
/// count) rather than the bare string this function takes.
#[must_use]
pub fn is_with_star_selector(selector_name: &str) -> bool {
    let Some(rest) = selector_name.strip_prefix("with") else {
        return false;
    };
    rest.ends_with(':') && rest.chars().next().is_some_and(char::is_uppercase)
}

/// Computes the keyword-constructor selector for a value class's slots.
///
/// Each slot name becomes one keyword part, e.g. `["x", "y"]` → `"x:y:"`.
/// An empty slot list yields the empty string; callers should treat a class
/// with no slots as having no keyword constructor.
#[must_use]
pub fn keyword_constructor_selector<'a>(slot_names: impl IntoIterator<Item = &'a str>) -> String {
    let mut sel = String::new();
    for name in slot_names {
        sel.push_str(name);
        sel.push(':');
    }
    sel
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn with_star_single_char() {
        assert_eq!(with_star_selector("x"), "withX:");
    }

    #[test]
    fn with_star_multi_char() {
        assert_eq!(with_star_selector("firstName"), "withFirstName:");
    }

    #[test]
    fn with_star_empty() {
        assert_eq!(with_star_selector(""), "with:");
    }

    #[test]
    fn keyword_constructor_multiple_slots() {
        assert_eq!(keyword_constructor_selector(["x", "y"].into_iter()), "x:y:");
    }

    #[test]
    fn keyword_constructor_empty() {
        assert_eq!(keyword_constructor_selector(std::iter::empty()), "");
    }

    #[test]
    fn is_with_star_selector_recognizes_generated_shapes() {
        assert!(is_with_star_selector("withX:"));
        assert!(is_with_star_selector("withFirstName:"));
        assert!(is_with_star_selector(&with_star_selector("db")));
        assert!(is_with_star_selector(&with_star_selector("x")));
    }

    #[test]
    fn is_with_star_selector_rejects_non_setter_shapes() {
        // Lowercase after "with" — an unrelated `withdraw:` method, not a
        // copy-setter for a field named "draw".
        assert!(!is_with_star_selector("withdraw:"));
        // The empty-field-name degenerate case: `with_star_selector("")`
        // produces "with:", which has nothing uppercase after the prefix.
        assert!(!is_with_star_selector("with:"));
        assert!(!is_with_star_selector(&with_star_selector("")));
        // No "with" prefix at all.
        assert!(!is_with_star_selector("at:put:"));
        assert!(!is_with_star_selector(""));
        // No trailing colon — a bare unary selector can never be the single
        // keyword part this convention describes, even with the right
        // "with" + uppercase shape.
        assert!(!is_with_star_selector("withCounter"));
    }

    /// BT-3090: `with_star_selector` has a hand-rolled Erlang mirror,
    /// `beamtalk_recheck:with_star_selector/1`
    /// (`runtime/apps/beamtalk_workspace/src/beamtalk_recheck.erl`), needed
    /// because the workspace app cannot depend on this Rust crate. A shared
    /// corpus fixture
    /// (`runtime/apps/beamtalk_workspace/test/fixtures/with_star_selector_corpus.json`)
    /// pins both implementations to the same cases — including non-ASCII
    /// first letters — so the two can't silently drift apart. The Erlang
    /// side asserts the identical cases in
    /// `beamtalk_recheck_tests:with_star_selector_matches_shared_corpus_test/0`.
    #[test]
    fn with_star_selector_matches_shared_corpus() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crates/")
            .parent()
            .expect("repo root")
            .join("runtime/apps/beamtalk_workspace/test/fixtures/with_star_selector_corpus.json");
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
        let cases: Vec<serde_json::Value> =
            serde_json::from_str(&raw).expect("corpus is a JSON array");
        assert!(!cases.is_empty(), "corpus must have cases");
        for case in &cases {
            let field_name = case["field_name"]
                .as_str()
                .expect("case.field_name is a string");
            let expected = case["expected_selector"]
                .as_str()
                .expect("case.expected_selector is a string");
            let why = case["why"].as_str().unwrap_or("");
            assert_eq!(
                with_star_selector(field_name),
                expected,
                "corpus mismatch for field_name {field_name:?} ({why})"
            );
        }
    }
}

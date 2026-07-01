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
pub(crate) fn with_star_selector(field_name: &str) -> String {
    let mut chars = field_name.chars();
    match chars.next() {
        None => "with:".to_string(),
        Some(first) => {
            let cap: String = first.to_uppercase().collect();
            format!("with{}{}:", cap, chars.as_str())
        }
    }
}

/// Computes the keyword-constructor selector for a value class's slots.
///
/// Each slot name becomes one keyword part, e.g. `["x", "y"]` → `"x:y:"`.
/// An empty slot list yields the empty string; callers should treat a class
/// with no slots as having no keyword constructor.
#[must_use]
pub(crate) fn keyword_constructor_selector<'a>(
    slot_names: impl IntoIterator<Item = &'a str>,
) -> String {
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
}

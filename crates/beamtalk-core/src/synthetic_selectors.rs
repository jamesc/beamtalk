// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared naming and signature authority for compiler-synthesized
//! `Value subclass:` methods.
//!
//! `Value` classes get auto-generated slot accessors, `with<Field>:` copy-setters,
//! and a keyword constructor (ADR 0042). The *names* of those selectors, and
//! the return types they are recorded with, are a convention that must agree
//! across the consumers:
//!
//! - **codegen** (`value_type_codegen`, `gen_server::methods`) — emits the Core
//!   Erlang functions and their exports, and records them in the class's
//!   `__beamtalk_meta` `method_info` (which is how the REPL and other
//!   cross-file compiles see an already-loaded class).
//! - **`class_hierarchy`** — synthesizes matching [`MethodInfo`](crate::semantic_analysis::MethodInfo)
//!   entries so the type checker and LSP know the methods exist before codegen runs.
//!
//! Keeping the selector-name and return-type computation here (rather than
//! duplicating it in each consumer) makes this module the single source of
//! truth and removes the drift risk that previously existed between codegen
//! and the hierarchy.

use crate::ast::StateDeclaration;
use crate::semantic_analysis::class_hierarchy::DeclaredType;

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

/// The keyword constructor a value class's compilation synthesizes, if any:
/// [`keyword_constructor_selector`] over the class's *own* slots, unless the
/// class declares no slots or `user_defines_class_side` reports that the
/// class already defines that selector class-side.
///
/// The one decision behind codegen's emission, the hierarchy's synthesized
/// `MethodInfo` and its `auto_ctors` anchor, and the stdlib builtins
/// generator (BT-4132): a drift between them mints direct calls to functions
/// the class's module does not export. `user_defines_class_side` must see
/// user methods only — after synthesis the selector is in the class-side
/// surface too, so a re-check could not tell synthesis from a collision.
#[must_use]
pub fn synthesized_keyword_constructor<'a>(
    slot_names: impl IntoIterator<Item = &'a str>,
    user_defines_class_side: impl FnOnce(&str) -> bool,
) -> Option<String> {
    let sel = keyword_constructor_selector(slot_names);
    if sel.is_empty() || user_defines_class_side(&sel) {
        None
    } else {
        Some(sel)
    }
}

/// The declared return type of a synthesized `with<Field>:` copy-setter:
/// `Self`, since the copy is `maps:put` on the receiver and keeps its class.
///
/// `Self` rather than the bare class name, so a generic value class keeps
/// its type arguments through a copy: on `b :: Box(Integer)`,
/// `b withItem: 3` is `Box(Integer)` and `(b withItem: 3) item` is
/// `Integer`, where a bare `Box` would drop them (BT-4648). On a subclass
/// instance the copy is that subclass. The flat `'Self'` rendering
/// round-trips through `__beamtalk_meta` (`DeclaredType::parse`).
///
/// Read by the hierarchy's synthesized `MethodInfo`s, the stdlib builtins
/// generator (`build_stdlib`) and codegen's `__beamtalk_meta` `method_info`
/// entries, so a class seen through its loaded BEAM metadata (the REPL's
/// view of an earlier input) records the same return types as one built
/// from its AST (BT-4621) — the `cascade-drops-update` warning, for one,
/// reads them.
#[must_use]
pub fn synthesized_copy_return_type() -> DeclaredType {
    DeclaredType::SelfType
}

/// The declared return type of the synthesized keyword constructor on the
/// value class `class_name`: the class itself, not `Self`. The constructor
/// ignores its class-self argument and always builds an instance of the
/// defining class, so a subclass that inherits it (`Sub item: 3`) gets a
/// `class_name` instance, which `Self` would mistype as the subclass
/// (BT-4648). A class-side send has no receiver type arguments to keep.
/// Read by the same consumers as [`synthesized_copy_return_type`].
#[must_use]
pub fn synthesized_constructor_return_type(class_name: &str) -> DeclaredType {
    DeclaredType::simple(class_name)
}

/// The declared return type of the synthesized getter for `slot`: the
/// field's declared type, or none when the field is untyped. Read by the
/// same consumers as [`synthesized_copy_return_type`].
#[must_use]
pub fn synthesized_getter_return_type(slot: &StateDeclaration) -> Option<DeclaredType> {
    slot.type_annotation.as_ref().map(DeclaredType::from)
}

/// The declared parameter type of the synthesized `with<Field>:` copy-setter
/// for `slot`: the field's declared type, or none when the field is untyped.
///
/// With the copy typed `Self` ([`synthesized_copy_return_type`]), its argument
/// must be checked against the field type too — over the receiver's type
/// arguments on a generic class — or `(b withItem: 'hi') item` on
/// `b :: Box(Integer)` would infer `Integer` for a `String` (BT-4648). Read by
/// the same consumers as [`synthesized_copy_return_type`].
#[must_use]
pub fn synthesized_copy_param_type(slot: &StateDeclaration) -> Option<DeclaredType> {
    synthesized_getter_return_type(slot)
}

/// The declared parameter type of the synthesized keyword constructor's
/// argument for `slot` (`Point x: 1 y: 2`): the field's declared type, or none
/// when the field is untyped (BT-4663). Used by the AST-based producers (hierarchy
/// AST path, codegen `__beamtalk_meta`, `build_stdlib`); the hierarchy state path
/// copies slot types that were already derived via `synthesized_getter_return_type`,
/// which this delegates to, so a class-side send is checked like `with<Field>:`.
#[must_use]
pub fn synthesized_constructor_param_type(slot: &StateDeclaration) -> Option<DeclaredType> {
    synthesized_getter_return_type(slot)
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
    fn synthesized_keyword_constructor_rules() {
        assert_eq!(
            synthesized_keyword_constructor(["x", "y"], |_| false).as_deref(),
            Some("x:y:")
        );
        assert_eq!(
            synthesized_keyword_constructor(std::iter::empty(), |_| false),
            None,
            "no slots, no keyword constructor"
        );
        assert_eq!(
            synthesized_keyword_constructor(["x"], |sel| sel == "x:"),
            None,
            "a user class-side method with the selector suppresses synthesis"
        );
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

    /// `with_star_selector` has a hand-rolled Erlang mirror,
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
        let path = beamtalk_workspace::cargo_workspace_root!()
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

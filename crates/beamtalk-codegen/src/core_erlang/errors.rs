// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared `#beamtalk_error{}` construction chain.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Shared leaf module (`architecture-principles.md` § Duplication & the
//! Shared-Leaf-Module Pattern): the `beamtalk_error:'new'` →
//! `'with_selector'` → `'with_hint'` three-step chain is the one way every
//! structured runtime error (DNU, arity/type mismatch, instantiation error,
//! immutable-value write, stateful-block-dispatch guard, ...) gets built —
//! see CLAUDE.md § Structured errors. Before this module existed, each call
//! site hand-wrote the same three `let`s, in whatever `Document`-building
//! idiom (plain literal strings, or `nest`/`line` pretty-printing) its own
//! function already used.

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::join;
use beamtalk_cerl_doc::leaf;

use super::CoreErlangGenerator;

/// The `immutable_value` hint a user-defined Value raises for an in-place
/// slot write (`fieldAt:put:`, `clearField:`) — the one source for both
/// compiled paths: the `fieldAt:put:` object-reflection intrinsic
/// (`intrinsics.rs`) and every generated Value `dispatch/3` arm
/// (`value_type_codegen.rs`).
///
/// The runtime's dynamic path (`beamtalk_primitive:is_ivar_method/1`, hit by
/// `beamtalk_message_dispatch:send/3` and any non-intrinsic send) raises the
/// same hint from Erlang. That copy sits across the Rust/Erlang boundary, so
/// both sides are pinned to the shared fixture
/// `runtime/apps/beamtalk_runtime/test/fixtures/value_immutable_slot_hint_conformance.json`
/// (BT-4273): `core_erlang::tests::value_immutable_slot_hint_conformance`
/// (Rust, including the emitted Core Erlang) and
/// `beamtalk_primitive_tests:value_type_field_at_put_hint_matches_shared_fixture_test/0`
/// (Erlang).
pub(super) const VALUE_IMMUTABLE_SLOT_HINT: &str =
    "Cannot modify slot on value type \u{2014} use withSlot: to create a new instance";

/// Builds the `let Err0 = call 'beamtalk_error':'new'(Kind, Class) in <sep>
/// let Err1 = call 'beamtalk_error':'with_selector'(Err0, Selector) in <sep>
/// let Err2 = call 'beamtalk_error':'with_hint'(Err1, Hint) in <sep>` chain
/// shared by every structured runtime error emission site, stopping right
/// after the final `in` + `sep` so the caller appends its own tail (`call
/// 'beamtalk_error':'raise'(Err2)`, `{'error', Err2, State}`, ...)
/// referencing `err2` itself.
///
/// `err0`/`err1`/`err2` are each call site's own established `let`-binding
/// text — a fixed name (`Document::Str("Error0")`) or a freshly minted one
/// (`leaf::var(self.fresh_var("Err"))`), bracketed or not, however that site
/// already spelled it — so collapsing onto this builder stays a
/// byte-identical extraction rather than a renaming. `err0_ref`/`err1_ref`
/// are the corresponding bare variable-reference text (identical to
/// `err0`/`err1` at every call site that does not use Core Erlang's
/// bracketed single-variable `let` form). `sep` is whatever separates one
/// `let ... in` step from the next at the call site: a literal `" "` for an
/// inline chain, `"\n"` plus the site's own indent for a manually indented
/// one, or [`beamtalk_cerl_doc::line`] for one built with `nest`/`line`.
#[allow(clippy::too_many_arguments)] // one param per varying piece of a fixed 3-step chain
pub(super) fn beamtalk_error_doc(
    err0: Document<'static>,
    err0_ref: Document<'static>,
    err1: Document<'static>,
    err1_ref: Document<'static>,
    err2: Document<'static>,
    kind: &'static str,
    class: Document<'static>,
    selector: Document<'static>,
    hint: Document<'static>,
    sep: Document<'static>,
) -> Document<'static> {
    docvec![
        "let ",
        err0,
        " = call 'beamtalk_error':'new'(",
        leaf::atom(kind),
        ", ",
        class,
        ") in",
        sep.clone(),
        "let ",
        err1,
        " = call 'beamtalk_error':'with_selector'(",
        err0_ref,
        ", ",
        selector,
        ") in",
        sep.clone(),
        "let ",
        err2,
        " = call 'beamtalk_error':'with_hint'(",
        err1_ref,
        ", ",
        hint,
        ") in",
        sep,
    ]
}

/// BT-4030: the one `new:`/`spawnWith:` initialiser validation call.
///
/// Builds the shared validation call every generated `new/1` (value types)
/// and `init/1` (actors) emits before merging, so the two can never drift:
/// `call 'beamtalk_class_instantiation':'check_init_args'(<class>,
/// <selector>, <base>, <args>, [<extra...>])`. The allowed set resolves
/// from metadata at runtime, keeping inherited fields accepted without any
/// compile-time ancestor knowledge. Each call site wraps this in its own
/// idiom (`let`-prefix for `new/1`, `__skip_initialize__`-guarded case for
/// `init/1`, whose parent-helper calls carry the leaf's keys and skip).
pub(super) fn check_init_args_call_doc(
    class: Document<'static>,
    selector: &'static str,
    base_state_var: Document<'static>,
    args_var: Document<'static>,
    extra_allowed: &[&str],
) -> Document<'static> {
    let extra_docs: Vec<Document<'static>> = extra_allowed.iter().map(|f| leaf::atom(*f)).collect();
    docvec![
        "call 'beamtalk_class_instantiation':'check_init_args'(",
        class,
        ", ",
        leaf::atom(selector),
        ", ",
        base_state_var,
        ", ",
        args_var,
        ", [",
        join(extra_docs, &Document::Str(", ")),
        "])",
    ]
}

impl CoreErlangGenerator {
    /// BT-3943: the structured `#pattern_mismatch` raise for a failed user
    /// pattern (tuple/array destructure, `match:` with no matching arm).
    ///
    /// Built through [`beamtalk_error_doc`]; `with_details/2` carries the
    /// offending value (`#{value => Value}`) and `beamtalk_error:raise/1`
    /// throws it. Evaluates to a Core Erlang expression that never returns,
    /// so it is usable as a `case` clause body. A raw `badmatch` /
    /// `case_clause` still means `internal_error`; this kind is `RuntimeError`.
    pub(super) fn pattern_mismatch_raise_doc(
        &mut self,
        value_var: &str,
        selector: &'static str,
        hint: &'static str,
    ) -> Document<'static> {
        let err0 = self.fresh_temp_var("PmErr");
        let err1 = self.fresh_temp_var("PmErr");
        let err2 = self.fresh_temp_var("PmErr");
        let err3 = self.fresh_temp_var("PmErr");
        docvec![
            beamtalk_error_doc(
                leaf::var(err0.clone()),
                leaf::var(err0),
                leaf::var(err1.clone()),
                leaf::var(err1),
                leaf::var(err2.clone()),
                "pattern_mismatch",
                leaf::atom("Object"),
                leaf::atom(selector),
                leaf::binary_lit(hint),
                Document::Str(" "),
            ),
            "let ",
            leaf::var(err3.clone()),
            " = call 'beamtalk_error':'with_details'(",
            leaf::var(err2),
            ", ~{'value'=>",
            leaf::var(value_var.to_string()),
            "}~) in call 'beamtalk_error':'raise'(",
            leaf::var(err3),
            ")",
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::beamtalk_error_doc;
    use super::check_init_args_call_doc;
    use beamtalk_cerl_doc::{Document, leaf};

    #[test]
    fn inline_chain_renders_three_let_bindings() {
        let doc = beamtalk_error_doc(
            Document::Str("Error0"),
            Document::Str("Error0"),
            Document::Str("Error1"),
            Document::Str("Error1"),
            Document::Str("Error2"),
            "dnu",
            leaf::atom("Counter"),
            leaf::atom("badMethod"),
            leaf::binary_lit("no such method"),
            Document::Str(" "),
        );
        let rendered = doc.to_pretty_string();
        assert!(
            rendered.starts_with("let Error0 = call 'beamtalk_error':'new'('dnu', 'Counter') in "),
            "unexpected: {rendered}"
        );
        assert!(
            rendered.contains(
                "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'badMethod') in "
            ),
            "unexpected: {rendered}"
        );
        assert!(
            rendered.contains("let Error2 = call 'beamtalk_error':'with_hint'(Error1,"),
            "unexpected: {rendered}"
        );
        assert!(rendered.ends_with("in "), "unexpected ending: {rendered}");
    }

    #[test]
    fn kind_is_emitted_as_atom_in_new_call() {
        let doc = beamtalk_error_doc(
            Document::Str("E0"),
            Document::Str("E0"),
            Document::Str("E1"),
            Document::Str("E1"),
            Document::Str("E2"),
            "arity_mismatch",
            leaf::atom("MyClass"),
            leaf::atom("sel"),
            leaf::binary_lit("hint"),
            Document::Str(" "),
        );
        let rendered = doc.to_pretty_string();
        assert!(
            rendered.contains("'arity_mismatch'"),
            "kind must be atom-quoted, got: {rendered}"
        );
    }

    #[test]
    fn err0_ref_used_in_with_selector_not_err0_binding() {
        let doc = beamtalk_error_doc(
            Document::Str("_Error0"),
            Document::Str("Error0"),
            Document::Str("_Error1"),
            Document::Str("Error1"),
            Document::Str("_Error2"),
            "dnu",
            leaf::atom("C"),
            leaf::atom("s"),
            leaf::binary_lit("h"),
            Document::Str(" "),
        );
        let rendered = doc.to_pretty_string();
        assert!(
            rendered.contains("let _Error0 = call"),
            "binding: {rendered}"
        );
        assert!(
            rendered.contains("with_selector'(Error0,"),
            "ref in with_selector: {rendered}"
        );
        assert!(
            rendered.contains("with_hint'(Error1,"),
            "ref in with_hint: {rendered}"
        );
    }

    #[test]
    fn check_init_args_renders_call_with_base_state_and_extras() {
        let doc = check_init_args_call_doc(
            leaf::atom("Counter"),
            "spawnWith:",
            Document::Str("MergedState"),
            Document::Str("InitArgs"),
            &["$bt_init_ack"],
        );
        let rendered = doc.to_pretty_string();
        assert!(
            rendered.contains(
                "call 'beamtalk_class_instantiation':'check_init_args'\
                 ('Counter', 'spawnWith:', MergedState, InitArgs, ['$bt_init_ack'])"
            ),
            "unexpected: {rendered}"
        );
    }

    #[test]
    fn check_init_args_renders_empty_extras_for_value_new() {
        let doc = check_init_args_call_doc(
            leaf::atom("Conf"),
            "new:",
            Document::Str("DefaultState"),
            Document::Str("InitArgs"),
            &[],
        );
        let rendered = doc.to_pretty_string();
        assert!(
            rendered.contains("'check_init_args'('Conf', 'new:', DefaultState, InitArgs, [])"),
            "value new/1 passes no extra-allowed keys, got: {rendered}"
        );
    }

    #[test]
    fn separator_appears_after_each_in_keyword() {
        let doc = beamtalk_error_doc(
            Document::Str("E0"),
            Document::Str("E0"),
            Document::Str("E1"),
            Document::Str("E1"),
            Document::Str("E2"),
            "err",
            leaf::atom("Cls"),
            leaf::atom("sel"),
            leaf::binary_lit("h"),
            Document::Str("SEP"),
        );
        let rendered = doc.to_pretty_string();
        assert_eq!(rendered.matches("SEP").count(), 3, "sep count: {rendered}");
    }
}

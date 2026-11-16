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
//! see AGENTS.md § Structured errors. Before this module existed, each call
//! site hand-wrote the same three `let`s, in whatever `Document`-building
//! idiom (plain literal strings, or `nest`/`line` pretty-printing) its own
//! function already used.

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
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

/// How one step of a [`BeamtalkErrorChain`] binds its result: the text on
/// the left of the `let` (`binding`) and the text later steps use to refer
/// back to it (`reference`). They are the same bare name at most sites, but
/// Core Erlang's bracketed single-variable form (`let <X> = …`) and a
/// `_`-prefixed binding make them differ, and each site keeps whatever
/// spelling it already had so collapsing onto the builder stays a
/// byte-identical extraction rather than a renaming (BT-4887).
pub(super) struct ErrorBinding {
    binding: Document<'static>,
    reference: Document<'static>,
}

impl ErrorBinding {
    /// A bare variable, bound and referenced by the same name.
    pub(super) fn plain(name: impl Into<String>) -> Self {
        let name = name.into();
        Self {
            binding: leaf::var(name.clone()),
            reference: leaf::var(name),
        }
    }

    /// Core Erlang's bracketed single-variable `let <Name> = …` form.
    pub(super) fn bracketed(name: impl Into<String>) -> Self {
        let name = name.into();
        Self {
            binding: docvec!["<", leaf::var(name.clone()), ">"],
            reference: leaf::var(name),
        }
    }
}

/// One link after `beamtalk_error:new/2` in a [`BeamtalkErrorChain`].
enum ErrorStep {
    /// `beamtalk_error:with_selector/2`.
    Selector(Document<'static>),
    /// `beamtalk_error:with_hint/2`.
    Hint(Document<'static>),
    /// `beamtalk_error:with_message/2`.
    Message(Document<'static>),
    /// `beamtalk_error:with_details/2`.
    Details(Document<'static>),
    /// `with_hint/2` whose hint is bound by its own `let` just before the
    /// call: `let <hint binding> = <hint> in <sep> let <Err> = with_hint(prev,
    /// <hint reference>) in`.
    HintBound(ErrorBinding, Document<'static>),
}

/// BT-4887: the one builder for the structured `beamtalk_error` `let` chain
/// shared by every runtime error emission site:
///
/// ```text
/// let Err0 = call 'beamtalk_error':'new'(Kind, Class) in <sep>
/// let Err1 = call 'beamtalk_error':'with_selector'(Err0, Selector) in <sep>
/// let Err2 = call 'beamtalk_error':'with_hint'(Err1, Hint) in <sep>
/// ```
///
/// Steps are chosen with [`Self::selector`], [`Self::hint`],
/// [`Self::message`], [`Self::details`] and [`Self::hint_bound`], in the
/// order the site raises them. [`Self::build`] stops right after the final
/// `in` + `sep` so the caller appends its own tail (`call
/// 'beamtalk_error':'raise'(Err2)`, `{'error', Err2, State}`, …) referencing
/// the last binding itself.
///
/// `build` takes one [`ErrorBinding`] per link (`new` first) — a fixed name,
/// a freshly minted one, bracketed or not, however the site already spelled
/// it — and `sep`, whatever separates one `let … in` step from the next at
/// the site: a literal `" "` for an inline chain, `"\n"` plus the site's own
/// indent for a manually indented one, or [`beamtalk_cerl_doc::line`] for one
/// built with `nest`/`line`.
pub(super) struct BeamtalkErrorChain {
    kind: &'static str,
    class: Document<'static>,
    steps: Vec<ErrorStep>,
}

impl BeamtalkErrorChain {
    /// Starts a chain at `beamtalk_error:new(<kind>, <class>)`.
    pub(super) fn new(kind: &'static str, class: Document<'static>) -> Self {
        Self {
            kind,
            class,
            steps: Vec::new(),
        }
    }

    pub(super) fn selector(mut self, selector: Document<'static>) -> Self {
        self.steps.push(ErrorStep::Selector(selector));
        self
    }

    pub(super) fn hint(mut self, hint: Document<'static>) -> Self {
        self.steps.push(ErrorStep::Hint(hint));
        self
    }

    pub(super) fn message(mut self, message: Document<'static>) -> Self {
        self.steps.push(ErrorStep::Message(message));
        self
    }

    pub(super) fn details(mut self, details: Document<'static>) -> Self {
        self.steps.push(ErrorStep::Details(details));
        self
    }

    /// A `with_hint/2` step whose hint is first bound to `hint_binding`.
    pub(super) fn hint_bound(
        mut self,
        hint_binding: ErrorBinding,
        hint: Document<'static>,
    ) -> Self {
        self.steps.push(ErrorStep::HintBound(hint_binding, hint));
        self
    }

    /// Renders the chain. `bindings` holds one entry for `new` and one per
    /// step, in order.
    ///
    /// # Panics
    ///
    /// Panics if `bindings.len()` is not `steps + 1` — every call site knows
    /// its own chain length, so this is a codegen bug, never user input.
    pub(super) fn build(
        self,
        bindings: Vec<ErrorBinding>,
        sep: Document<'static>,
    ) -> Document<'static> {
        assert_eq!(
            bindings.len(),
            self.steps.len() + 1,
            "BeamtalkErrorChain: one binding per link (`new` plus each step)"
        );
        let mut bindings = bindings.into_iter();
        let first = bindings.next().expect("checked above");
        let mut previous = first.reference;
        // Each segment is one `let … in`; `sep` follows every one of them.
        let mut segments: Vec<Document<'static>> = vec![docvec![
            "let ",
            first.binding,
            " = ",
            form::call("beamtalk_error", "new", [leaf::atom(self.kind), self.class]),
            " in",
        ]];
        for step in self.steps {
            let (function, arg) = match step {
                ErrorStep::Selector(arg) => ("with_selector", arg),
                ErrorStep::Hint(arg) => ("with_hint", arg),
                ErrorStep::Message(arg) => ("with_message", arg),
                ErrorStep::Details(arg) => ("with_details", arg),
                ErrorStep::HintBound(hint_binding, hint) => {
                    segments.push(docvec!["let ", hint_binding.binding, " = ", hint, " in"]);
                    ("with_hint", hint_binding.reference)
                }
            };
            let next = bindings.next().expect("checked above");
            segments.push(docvec![
                "let ",
                next.binding,
                " = ",
                form::call("beamtalk_error", function, [previous, arg]),
                " in",
            ]);
            previous = next.reference;
        }
        let last = segments.pop().expect("`new` segment is always present");
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(segments.len() * 2 + 2);
        for segment in segments {
            parts.push(segment);
            parts.push(sep.clone());
        }
        parts.push(last);
        parts.push(sep);
        Document::Vec(parts)
    }
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
    /// Built through [`BeamtalkErrorChain`]; `with_details/2` carries the
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
            BeamtalkErrorChain::new("pattern_mismatch", leaf::atom("Object"))
                .selector(leaf::atom(selector))
                .hint(leaf::binary_lit(hint))
                .details(docvec![
                    "~{'value'=>",
                    leaf::var(value_var.to_string()),
                    "}~"
                ])
                .build(
                    vec![
                        ErrorBinding::plain(err0),
                        ErrorBinding::plain(err1),
                        ErrorBinding::plain(err2),
                        ErrorBinding::plain(err3.clone()),
                    ],
                    Document::Str(" "),
                ),
            "call 'beamtalk_error':'raise'(",
            leaf::var(err3),
            ")",
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::check_init_args_call_doc;
    use super::{BeamtalkErrorChain, ErrorBinding};
    use beamtalk_cerl_doc::{Document, leaf};

    /// A binding and reference spelled differently (e.g. `_Error0` / `Error0`).
    fn split(binding: Document<'static>, reference: Document<'static>) -> ErrorBinding {
        ErrorBinding { binding, reference }
    }

    #[test]
    fn inline_chain_renders_three_let_bindings() {
        let doc = BeamtalkErrorChain::new("dnu", leaf::atom("Counter"))
            .selector(leaf::atom("badMethod"))
            .hint(leaf::binary_lit("no such method"))
            .build(
                vec![
                    ErrorBinding::plain("Error0"),
                    ErrorBinding::plain("Error1"),
                    ErrorBinding::plain("Error2"),
                ],
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
        let doc = BeamtalkErrorChain::new("arity_mismatch", leaf::atom("MyClass"))
            .selector(leaf::atom("sel"))
            .hint(leaf::binary_lit("hint"))
            .build(
                vec![
                    ErrorBinding::plain("E0"),
                    ErrorBinding::plain("E1"),
                    ErrorBinding::plain("E2"),
                ],
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
        let doc = BeamtalkErrorChain::new("dnu", leaf::atom("C"))
            .selector(leaf::atom("s"))
            .hint(leaf::binary_lit("h"))
            .build(
                vec![
                    split(Document::Str("_Error0"), Document::Str("Error0")),
                    split(Document::Str("_Error1"), Document::Str("Error1")),
                    ErrorBinding::plain("_Error2"),
                ],
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
        let doc = BeamtalkErrorChain::new("err", leaf::atom("Cls"))
            .selector(leaf::atom("sel"))
            .hint(leaf::binary_lit("h"))
            .build(
                vec![
                    ErrorBinding::plain("E0"),
                    ErrorBinding::plain("E1"),
                    ErrorBinding::plain("E2"),
                ],
                Document::Str("SEP"),
            );
        let rendered = doc.to_pretty_string();
        assert_eq!(rendered.matches("SEP").count(), 3, "sep count: {rendered}");
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0122 Decision 3: the ONE emission helper appending
//! [`ThreadedFamilies`] slots to a construct's own result tuple and
//! extracting them back out afterwards. The families are `State` (an Actor
//! instance method's scratch map) and `SelfVt` (a value-type instance
//! method's `Self`); class variables are not a family (ADR 0130 §3 — they
//! live in the class process and are written in place). Sites that route
//! through it:
//! - `exception_handling.rs`'s `on:do:`/`ensure:` result tuple closes through
//!   [`append_family_slots`] via `close_exception_result_tuple`.
//! - `while_loops.rs`'s/`counted_loops.rs`'s own extra `letrec` fun
//!   parameter(s), `produces` entries, and exit-arm tuple slot(s) — generic
//!   over `ThreadingPlan::threaded_families()`;
//!   `ThreadingPlan::capture_loop_family_params` captures them with one
//!   call. `value_type_codegen.rs`'s value-type Letrec loop extraction reads
//!   [`ThreadedFamilies`] too, but dispatches through its own
//!   `extract_vt_loop_family_slot` onto `rebind_value_self_from_doc`, whose
//!   `verify_simple_bind` check is a correctness invariant this helper's
//!   plain [`BindOp::Direct`] rebind does not reproduce.
//! - `value_type_codegen.rs`'s `finish_vt_conditional_branch`/
//!   `rebind_vt_conditional_mutations` (BT-3513), the PRODUCTION caller of
//!   [`extract_family_slots`]. Each arm's own trailing family value is
//!   resolved to "this arm's own mutated version, else the pre-`case`
//!   baseline" BEFORE calling [`append_family_slots`] (the same
//!   both-or-neither resolution `exception_handling.rs`'s
//!   `exception_family_slot` performs for `on:do:`/`ensure:`).
//!   `append_family_slots` has a `Document::Nil` `base` convention for this
//!   site — an arm return value can have NO other tuple element before its
//!   one family slot (a branch that only writes a value-type field, no
//!   outer local) — see that function's own doc comment.
//! - `conditionals.rs`'s `with_branch_context`/six `generate_*_with_mutations`
//!   (BT-3514, the Actor-path consumer, and [`append_baseline_family_slots`]'s
//!   production caller — the Actor conditional's own
//!   non-taken/absent-block-passthrough arm genuinely is "whichever arm ran,
//!   the other's baseline must still be valid"). The family list is always
//!   `[State]` — ADR 0122's "State already fits" — never data-driven per
//!   call site.
//!
//! A `Foldl*` accumulator carries no family slot at all, so the list ops'
//! `ThreadingPlan::foldl_call_doc` builds the plain fold call.
//!
//! **Trailing position only** — no leading-slot mode.
//!
//! Three operations, all keyed off one [`ThreadedFamilies`] so a
//! construct's append/extract/non-taken-arm calls can never independently
//! drift on slot order or count:
//! - [`append_family_slots`] — the taken-arm / normal-completion shape.
//! - [`append_baseline_family_slots`] — the "non-taken arm" shape for branch
//!   merges (ADR 0122's both-or-neither discipline); a thin entry point over
//!   [`append_family_slots`] real call sites remain free to fold into their
//!   own `current` closure instead — both read the same way to a reviewer.
//! - [`extract_family_slots`] — unpacks the trailing slots back into fresh
//!   per-family versions after the construct completes.

use super::super::threaded_ir::{
    BindOp, RenderCtx, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar, render_value,
};
use super::analysis::ThreadedFamilies;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::{docvec, leaf};
use beamtalk_core::source_analysis::Span;

/// One family's `(source, target)` [`VersionedVar`] pair for
/// [`extract_family_slots`] — the version already live before the
/// extraction and the fresh one this call's `Bind` mints. Named rather than
/// a bare tuple so a call site's `step` closure reads as "this family's
/// version step," matching
/// [`super::super::threaded_ir::VersionCounter`]'s own vocabulary
/// (`next_var` mints a target from a source in exactly this shape).
/// BT-3513: `value_type_codegen.rs`'s `rebind_vt_conditional_mutations` is
/// this type's first production constructor — every earlier migration
/// (BT-3506, BT-3512) was append-only. `append_baseline_family_slots` alone
/// remains a test-only convenience (see the module doc comment's BT-3513
/// note on why real branch-merge sites fold "taken or baseline" into their
/// own closure instead).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct FamilyVersionStep {
    pub(in crate::core_erlang) source: VersionedVar,
    pub(in crate::core_erlang) target: VersionedVar,
}

impl FamilyVersionStep {
    pub(in crate::core_erlang) fn new(source: VersionedVar, target: VersionedVar) -> Self {
        Self { source, target }
    }
}

/// ADR 0122 Decision 3, append half: closes `base` into a full tuple
/// `Document`, appending one trailing slot per family in `families`'
/// canonical order first — e.g. `base = "{'nil', StateAcc"` (an OPEN
/// prefix — no closing brace of its own; the caller's own `base` value
/// supplies whatever comes before the family slots — a joined locals list,
/// a bare `'nil'`, …), `families = [State, SelfVt]`, `current`
/// answering `Self2` for `VersionPrefix::SelfVt` produces
/// `"{'nil', StateAcc, Self2}"`. With an empty `families` this is just
/// `base` plus the closing brace — the common case for a construct that
/// threads no extra family.
///
/// `base` is [`Document::Nil`] for a construct whose OWN tuple has NO
/// element before the family slots — BT-3513's value-type conditional arm
/// when the arm mutates a family but no outer local (`ifTrue: [self.x :=
/// v]` with no local write, `n_locals == 0`): there is no `"{'nil',
/// StateAcc"`-style prefix to open with, so this renders the opening `"{"`
/// itself and omits the leading `", "` before the FIRST family slot only
/// (matching the hand-rolled `if filled > 0 { push(", ") }` gate
/// `finish_vt_conditional_branch` used before this helper existed). Every
/// other existing call site passes a real, already-open `base` (at least
/// one prior element — `'nil'`, `Result`, a joined locals list, …), so this
/// is purely additive: unchanged for every caller that predates it.
///
/// `current` supplies each family's CURRENT [`VersionedVar`] — the version
/// already live at the point the construct's result tuple closes. Kept a
/// caller-supplied lookup rather than read off the live generator directly:
/// every existing hand-rolled call site sources "current" differently (a
/// bare `self.state_version()`/`self.self_version()`
/// read at the method's own top frame, vs. a `Letrec` loop's own already-
/// minted `Gensym` fun-parameter identity for the SAME family inside its
/// body) — this helper stays pure data-in/data-out over [`ThreadedFamilies`]
/// and leaves "what counts as current here" a caller decision, per ADR 0122
/// Decision 1's "capability is what varies between sites, not detection"
/// applied here to slot VALUES, not just eligibility.
///
/// Renders each slot through [`render_value`] on a [`ValueRef::Version`] —
/// the same production rendering every OTHER `VersionedVar` reference goes
/// through — so `VersionPrefix::State`'s own loop-context-dependent
/// `StateAcc` vs. `State` spelling (`RenderCtx`'s prefix resolution) is
/// handled for free, never re-derived here.
pub(in crate::core_erlang) fn append_family_slots(
    base: Document<'static>,
    families: &ThreadedFamilies,
    current: impl Fn(&VersionPrefix) -> VersionedVar,
    ctx: &RenderCtx<'_>,
) -> Document<'static> {
    let no_prior_elements = matches!(base, Document::Nil);
    let mut docs = vec![if no_prior_elements {
        Document::Str("{")
    } else {
        base
    }];
    for (i, prefix) in families.as_slice().iter().enumerate() {
        let var = current(prefix);
        let rendered = render_value(&ValueRef::Version(var), ctx);
        docs.push(if no_prior_elements && i == 0 {
            rendered
        } else {
            docvec![", ", rendered]
        });
    }
    docs.push(Document::Str("}"));
    Document::Vec(docs)
}

/// ADR 0122 Decision 3's "non-taken arm" operation for branch merges: the
/// same-shaped tuple [`append_family_slots`] builds, but sourced from each
/// family's BASELINE (pre-branch, unchanged) version rather than a version a
/// branch freshly mutated — the both-or-neither discipline
/// `finish_vt_conditional_branch` hand-wrote before BT-3513 (and
/// `conditionals.rs`'s still-unmigrated Actor conditional still does today —
/// ADR 0122 §"What is hand-written today"): whichever arm actually ran,
/// `element/N` after the merge must be valid, so an arm that did not itself
/// touch a family still carries a value in that family's slot.
///
/// A thin, self-documenting entry point over [`append_family_slots`] — never
/// a second tuple-building implementation, so the two can never
/// independently drift on shape, slot count, or order.
///
/// BT-3514: `conditionals.rs`'s `conditional_baseline_tuple` is this
/// function's first production caller — the Actor conditional's own
/// non-taken-arm/absent-block-passthrough tuple genuinely IS "whichever arm
/// actually ran, the other's baseline must still be valid," unlike
/// BT-3513's value-type conditional (which folds "taken or baseline" into
/// its own `arm_version_for` closure instead — see this module's own
/// BT-3513 note above).
pub(in crate::core_erlang) fn append_baseline_family_slots(
    base: Document<'static>,
    families: &ThreadedFamilies,
    baseline: impl Fn(&VersionPrefix) -> VersionedVar,
    ctx: &RenderCtx<'_>,
) -> Document<'static> {
    append_family_slots(base, families, baseline, ctx)
}

/// ADR 0122 Decision 3, extract half: the single emission-input counterpart
/// of [`append_family_slots`] — extracts each family's NEXT version from
/// `tuple_var`'s trailing slots (`call 'erlang':'element'(N, tuple_var)`),
/// producing one real [`ThreadedStmt::Bind`] per family in `families`'
/// canonical order — the SAME order [`append_family_slots`] wrote them, so a
/// construct's own append/extract pair can never independently drift on slot
/// order or count (the append/extract split every hand-rolled call site
/// already keeps as two separate functions today —
/// `finish_vt_conditional_branch`/`rebind_vt_conditional_mutations`,
/// `emit_vt_threaded_tuple_unwrap_to_var`'s inline pair — made structurally
/// impossible to desync here instead of merely documented).
///
/// `base_arity` is the count of tuple elements already occupied BEFORE the
/// family slots begin (`1` for a bare `{'nil', ...}` loop-exit tuple, `1`
/// for `on:do:`/`ensure:`'s own `{Result, ...}`, `2` for a conditional's
/// `{Value, StateAcc, ...}`, …) — the same count `base`'s own already-
/// rendered `Document` implicitly fixed when [`append_family_slots`] built
/// it. There is no single shared `usize` binding the two calls together
/// (`base` is an opaque, already-rendered `Document` by the time this runs)
/// — callers must pass the matching arity to both, exactly as every existing
/// hand-rolled pair already does.
///
/// `step` supplies each family's `(source, target)` [`VersionedVar`] pair
/// via [`FamilyVersionStep`] — mirrors [`append_family_slots`]'s own
/// "version lookup is a caller concern" design: a caller mints `target`
/// itself (its own `next_state_var`/`next_self_var`) BEFORE
/// calling this, exactly as every existing hand-rolled extraction site does
/// (`rebind_value_self_from_doc`, `rebind_vt_conditional_mutations`).
///
/// Every produced `Bind`'s `op` is a plain whole-value rebind
/// ([`BindOp::Direct`]), never a `maps:put` — see the comment at the
/// construction site.
pub(in crate::core_erlang) fn extract_family_slots(
    tuple_var: &str,
    base_arity: usize,
    families: &ThreadedFamilies,
    step: impl Fn(&VersionPrefix) -> FamilyVersionStep,
    span: Span,
) -> Vec<ThreadedStmt> {
    families
        .as_slice()
        .iter()
        .enumerate()
        .map(|(i, prefix)| {
            let FamilyVersionStep { source, target } = step(prefix);
            let slot = base_arity + i + 1;
            let value = ValueRef::Doc(docvec![
                "call 'erlang':'element'(",
                leaf::int_lit(i64::try_from(slot).unwrap_or(i64::MAX)),
                ", ",
                leaf::var(tuple_var.to_string()),
                ")",
            ]);
            ThreadedStmt::Bind {
                target,
                source,
                // ALWAYS `BindOp::Direct` — never `BindOp::Put`: `value` (the
                // tuple-element read built above) holds this family's ENTIRE
                // post-construct value — the whole `StateAcc`/`Self` map — not
                // one field of it. `maps:put(field, value, source)` would nest
                // the whole new map under one field of the OLD map instead of
                // replacing it.
                op: BindOp::Direct(value),
                span,
            }
        })
        .collect()
}

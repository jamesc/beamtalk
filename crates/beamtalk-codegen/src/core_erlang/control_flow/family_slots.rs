// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0122 Decision 3: the ONE emission helper appending
//! [`ThreadedFamilies`] slots to a construct's own result tuple and
//! extracting them back out afterwards — the shared machinery every
//! currently hand-rolled trailing-slot site will route through once each
//! migrates (later issues in ADR 0122's epic, BT-3508):
//! `while_loops.rs`/`counted_loops.rs`'s `{'nil', StateAcc[, ClassVars |
//! Self1]}`, `value_type_codegen.rs`'s `vt_construct_extra_slot`/
//! `emit_vt_threaded_tuple_unwrap_to_var`/`finish_vt_conditional_branch`/
//! `rebind_vt_conditional_mutations`, `exception_handling.rs`'s
//! `exception_self_slot`/`_doc`, the Actor conditional's
//! `with_branch_context`/six `generate_*_with_mutations`, and the Foldl
//! accumulator (once its leading slot normalizes to trailing, BT-3516).
//!
//! **Not wired into any site yet** — BT-3511's own scope is the helper
//! only; migrating a site to call it is each site's own later issue.
//!
//! **Trailing position only** — no leading-slot mode; Foldl's leading slot
//! is normalized to trailing when IT migrates (ADR 0122 §Alternatives
//! Considered, "Keep Foldl's leading slot").
//!
//! Three operations, all keyed off one [`ThreadedFamilies`] so a
//! construct's append/extract/non-taken-arm calls can never independently
//! drift on slot order or count:
//! - [`append_family_slots`] — the taken-arm / normal-completion shape.
//! - [`append_baseline_family_slots`] — the "non-taken arm" shape for branch
//!   merges (ADR 0122's both-or-neither discipline).
//! - [`extract_family_slots`] — unpacks the trailing slots back into fresh
//!   per-family versions after the construct completes.

use super::super::threaded_ir::{
    RenderCtx, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar, render_value,
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
// `#[allow(dead_code)]` throughout this file: nothing outside this module's
// own unit tests calls any of it yet — ADR 0122, BT-3511 is the emission
// helper only; migrating a site to route through it is each site's own
// later issue (BT-3506/3512-3518).
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct FamilyVersionStep {
    pub(in crate::core_erlang) source: VersionedVar,
    pub(in crate::core_erlang) target: VersionedVar,
}

impl FamilyVersionStep {
    #[allow(dead_code)]
    pub(in crate::core_erlang) fn new(source: VersionedVar, target: VersionedVar) -> Self {
        Self { source, target }
    }
}

/// ADR 0122 Decision 3, append half: closes `base` into a full tuple
/// `Document`, appending one trailing slot per family in `families`'
/// canonical order first — e.g. `base = "{'nil', StateAcc"` (an OPEN
/// prefix — no closing brace of its own, matching
/// `finish_vt_conditional_branch`'s own `tuple_parts` before ITS closing
/// `Document::Str("}")` push), `families = [State, ClassVars]`, `current`
/// answering `ClassVars2` for `VersionPrefix::ClassVars` produces
/// `"{'nil', StateAcc, ClassVars2}"`. With an empty `families` this is just
/// `base` plus the closing brace — the common case for a construct that
/// threads no extra family.
///
/// `current` supplies each family's CURRENT [`VersionedVar`] — the version
/// already live at the point the construct's result tuple closes. Kept a
/// caller-supplied lookup rather than read off the live generator directly:
/// every existing hand-rolled call site sources "current" differently (a
/// bare `self.state_version()`/`self.class_var_version()`/`self.self_version()`
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
#[allow(dead_code)]
pub(in crate::core_erlang) fn append_family_slots(
    base: Document<'static>,
    families: &ThreadedFamilies,
    current: impl Fn(&VersionPrefix) -> VersionedVar,
    ctx: &RenderCtx<'_>,
) -> Document<'static> {
    let mut docs = vec![base];
    for prefix in families.as_slice() {
        let var = current(prefix);
        docs.push(docvec![", ", render_value(&ValueRef::Version(var), ctx)]);
    }
    docs.push(Document::Str("}"));
    Document::Vec(docs)
}

/// ADR 0122 Decision 3's "non-taken arm" operation for branch merges: the
/// same-shaped tuple [`append_family_slots`] builds, but sourced from each
/// family's BASELINE (pre-branch, unchanged) version rather than a version a
/// branch freshly mutated — the both-or-neither discipline
/// `finish_vt_conditional_branch`/`conditionals.rs` both already hand-write
/// today (ADR 0122 §"What is hand-written today"): whichever arm actually
/// ran, `element/N` after the merge must be valid, so an arm that did not
/// itself touch a family still carries a value in that family's slot.
///
/// A thin, self-documenting entry point over [`append_family_slots`] — never
/// a second tuple-building implementation, so the two can never
/// independently drift on shape, slot count, or order.
#[allow(dead_code)]
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
/// itself (its own `next_state_var`/`next_class_var`/`next_self_var`) BEFORE
/// calling this, exactly as every existing hand-rolled extraction site does
/// (`rebind_class_vars_from_doc`, `rebind_value_self_from_doc`,
/// `rebind_vt_conditional_mutations`).
///
/// Every produced `Bind`'s `op`/`shadow_write` come from
/// [`VersionPrefix::extraction_bind_op`] — a method on the type, never a
/// match in this function (ADR 0122 Decision 4) — see that method's own doc
/// comment for why it is always a plain rebind, never a `maps:put`.
#[allow(dead_code)]
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
            let (op, shadow_write) = prefix.extraction_bind_op(value);
            ThreadedStmt::Bind {
                target,
                source,
                op,
                shadow_write,
                span,
            }
        })
        .collect()
}

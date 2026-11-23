// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The accumulator tails of the `Foldl*` fold bodies (BT-4888).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A fold-body walker (`CoreErlangGenerator::lower_foldl_body`) lowers the
//! body's statements, which bind the predicate (or key) variable; the fold
//! then ends in one per-[`BodyKind`] accumulator — `select:`'s
//! include-or-skip `case`, `count:`'s increment, `partition:`'s two-list
//! split, … — pushed as one typed [`ThreadedStmt::FoldAccumulator`]
//! (BT-3863) recording what it carries. Which tail applies is a property of
//! the `BodyKind` the fold driver ([`super::fold_driver`]) selects, so the
//! dispatch lives here, on [`BodyKind`] itself, next to the tails.

use super::super::super::CoreErlangGenerator;
use super::super::super::threaded_ir::{
    ACC_LIST, AccSlot, BoolArm, ConsPerCall, FOLD_RESULT_LIST_SLOT, FoldAccBody, FoldAccValue,
    FoldCarry, FoldResultList, ThreadedStmt, bool_arm_clause as bool_arm, render_tuple,
};
use super::super::ThreadingPlan;
use super::BodyKind;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, docvec, leaf};
use beamtalk_core::source_analysis::Span;

/// What every fold epilogue reads beyond its own `BodyKind` fields: the
/// predicate variable the statements bound, the threading plan, whether the
/// body mutated state, and the span the accumulator is tagged with.
#[derive(Clone, Copy)]
pub(in crate::core_erlang::control_flow) struct EpilogueCx<'a> {
    pub(in crate::core_erlang::control_flow) pv: &'a str,
    pub(in crate::core_erlang::control_flow) plan: &'a ThreadingPlan,
    pub(in crate::core_erlang::control_flow) has_mutations: bool,
    pub(in crate::core_erlang::control_flow) span: Span,
}

impl BodyKind {
    /// Whether the fold body's last statement is bound to a predicate (or key)
    /// variable its tail reads — every kind except `do:`, `collect:` and
    /// `inject:into:`, whose accumulator is the body's own value.
    pub(in crate::core_erlang::control_flow) fn binds_predicate_var(&self) -> bool {
        // Exhaustive on purpose: a new `BodyKind` must decide this (it moves
        // the `Pred` temp counter, and so the generated `.core`).
        match self {
            Self::FoldlDo | Self::FoldlCollect | Self::FoldlInject => false,
            Self::FoldlFilter { .. }
            | Self::FoldlBoolPredicate { .. }
            | Self::FoldlDetect { .. }
            | Self::FoldlCount
            | Self::FoldlTakeWhile { .. }
            | Self::FoldlDropWhile { .. }
            | Self::FoldlPartition { .. }
            | Self::FoldlGroupBy { .. }
            | Self::FoldlSort { .. } => true,
        }
    }

    /// BT-5097: the result list this kind's accumulator extends, for a kind
    /// that has one a keyed fold can drive: `select:`/`reject:` add the item
    /// or nothing, `collect:` adds the block's answer, both at the head of
    /// the kind's own accumulator ([`FOLD_RESULT_LIST_SLOT`]). Every other
    /// kind carries something else there (a count, a flag, a pair of lists, a
    /// map, the item found) or several elements, so a keyed fold over it is
    /// not defined; exhaustive on purpose, so a new kind decides.
    pub(in crate::core_erlang::control_flow) fn result_list(&self) -> Option<FoldResultList> {
        let list = |cons| {
            Some(FoldResultList {
                slot: FOLD_RESULT_LIST_SLOT,
                cons,
            })
        };
        match self {
            Self::FoldlFilter { .. } => list(ConsPerCall::AtMostOne),
            Self::FoldlCollect => list(ConsPerCall::ExactlyOne),
            Self::FoldlDo
            | Self::FoldlInject
            | Self::FoldlBoolPredicate { .. }
            | Self::FoldlDetect { .. }
            | Self::FoldlCount
            | Self::FoldlTakeWhile { .. }
            | Self::FoldlDropWhile { .. }
            | Self::FoldlPartition { .. }
            | Self::FoldlGroupBy { .. }
            | Self::FoldlSort { .. } => None,
        }
    }

    /// Pushes this kind's accumulator tail (exactly one
    /// [`ThreadedStmt::FoldAccumulator`]) after the fold body's statements;
    /// a kind that [binds no predicate variable](Self::binds_predicate_var)
    /// has none.
    pub(in crate::core_erlang::control_flow) fn lower_epilogue(
        &self,
        generator: &mut CoreErlangGenerator,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        match self {
            Self::FoldlFilter { item_var, negate } => {
                generator.lower_foldl_epilogue_filter(item_var, *negate, cx, stmts);
            }
            Self::FoldlBoolPredicate { is_all } => {
                generator.lower_foldl_epilogue_bool_predicate(*is_all, cx, stmts);
            }
            Self::FoldlDetect { item_var } => {
                generator.lower_foldl_epilogue_detect(item_var, cx, stmts);
            }
            Self::FoldlCount => generator.lower_foldl_epilogue_count(cx, stmts),
            Self::FoldlTakeWhile { item_var } => {
                generator.lower_foldl_epilogue_take_while(item_var, cx, stmts);
            }
            Self::FoldlDropWhile { item_var } => {
                generator.lower_foldl_epilogue_drop_while(item_var, cx, stmts);
            }
            Self::FoldlPartition { item_var } => {
                generator.lower_foldl_epilogue_partition(item_var, cx, stmts);
            }
            Self::FoldlGroupBy { item_var } => {
                generator.lower_foldl_epilogue_group_by(item_var, cx, stmts);
            }
            Self::FoldlSort { state_key_var } => {
                generator.lower_foldl_epilogue_sort(state_key_var, cx, stmts);
            }
            // These bind no predicate variable, so they have no epilogue.
            Self::FoldlDo | Self::FoldlCollect | Self::FoldlInject => {}
        }
    }
}

/// `{head…, tail}` — the shared accumulator tuple renderer.
use render_tuple as acc_tuple;

/// `[item | list]` — cons `item_var` onto the accumulator list `list`.
fn cons(item_var: &str, list: &'static str) -> Document<'static> {
    AccSlot::cons(item_var, list).render()
}

impl CoreErlangGenerator {
    /// The `(carry, tail_state_doc)` pair every fold epilogue ends in: the
    /// repacked current variable bindings in tuple mode, or the `StateAcc`
    /// map (the latest state var when the body mutated) in map mode. Each
    /// epilogue computes it once and builds its `case` once around it.
    fn epilogue_tail(&self, cx: EpilogueCx<'_>) -> (FoldCarry, Document<'static>) {
        if cx.plan.use_tuple_acc {
            (cx.plan.fold_carry(), cx.plan.current_vars_doc(self))
        } else {
            let final_state = if cx.has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            (FoldCarry::StateMap, leaf::var(final_state))
        }
    }

    /// `case scrutinee of <'a'> when 'true' -> … <'b'> when 'true' -> …
    /// <V> when 'true' -> error({'case_clause', V}) end` — the
    /// two-boolean-arm `case` every predicate epilogue is built from, arms
    /// in the order given (the generated text depends on it).
    fn bool_case(
        &mut self,
        scrutinee: Document<'static>,
        arms: [(bool, Document<'static>); 2],
    ) -> Document<'static> {
        let [(first, first_body), (second, second_body)] = arms;
        let var = self.fresh_temp_var("_PredBad");
        let fallback = Self::case_clause_fallback_clause(var);
        form::case_of(
            scrutinee,
            [
                bool_arm(first, first_body),
                bool_arm(second, second_body),
                fallback,
            ],
            Layout::Flat,
        )
    }

    // FoldlFilter: append the predicate case expression after all statements.
    fn lower_foldl_epilogue_filter(
        &mut self,
        item_var: &str,
        negate: bool,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let condition_doc: Document<'static> = if negate {
            form::call("erlang", "not", [leaf::var(cx.pv.to_string())])
        } else {
            leaf::var(cx.pv.to_string())
        };
        let (carry, tail) = self.epilogue_tail(cx);
        let var = self.fresh_temp_var("_PredBad");
        let fallback = Self::case_clause_fallback_clause(var);
        // BT-5117: typed, so the verifier counts the conses into `AccList`.
        // Test-only fault: the included item is consed twice.
        let included = if fault!(FilterConsTwice) {
            AccSlot::Cons {
                item: item_var.to_string(),
                list: Box::new(AccSlot::cons(item_var, ACC_LIST)),
            }
        } else {
            AccSlot::cons(item_var, ACC_LIST)
        };
        let arm = |value, head| {
            Box::new(BoolArm {
                value,
                body: FoldAccBody::Tuple {
                    head: vec![head],
                    tail: tail.clone(),
                },
            })
        };
        let value = FoldAccValue {
            prelude: None,
            body: FoldAccBody::BoolCase {
                scrutinee: condition_doc,
                arms: [arm(true, included), arm(false, AccSlot::Name(ACC_LIST))],
                fallback,
            },
        };
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlBoolPredicate — update boolean accumulator based on predicate result.
    // Match only 'true'/'false' explicitly (consistent with FoldlFilter and lists:any/all).
    // allSatisfy: pred=false → set BoolAcc to false; pred=true → keep.
    // anySatisfy: pred=true → set BoolAcc to true; pred=false → keep.
    fn lower_foldl_epilogue_bool_predicate(
        &mut self,
        is_all: bool,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let (carry, tail) = self.epilogue_tail(cx);
        let decided = !is_all;
        let arms = [
            (
                decided,
                acc_tuple(
                    [Document::Str(if decided { "'true'" } else { "'false'" })],
                    &tail,
                ),
            ),
            (!decided, acc_tuple([Document::Str("BoolAcc")], &tail)),
        ];
        let value = self.bool_case(leaf::var(cx.pv.to_string()), arms);
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlDetect — update found-item accumulator on first match.
    // Accumulator is {FoundItem, FoundFlag, StateVars...}.
    // The fold lambda skips the body once FoundFlag is 'true' (`skip_when_decided`,
    // BT-3805), so a predicate hit here is always the first match.
    fn lower_foldl_epilogue_detect(
        &mut self,
        item_var: &str,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let (carry, tail) = self.epilogue_tail(cx);
        let value = self.bool_case(
            leaf::var(cx.pv.to_string()),
            [
                (
                    true,
                    acc_tuple(
                        [leaf::var(item_var.to_string()), Document::Str("'true'")],
                        &tail,
                    ),
                ),
                (
                    false,
                    acc_tuple(
                        [Document::Str("FoundItem"), Document::Str("FoundFlag")],
                        &tail,
                    ),
                ),
            ],
        );
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlCount — increment count accumulator on predicate match.
    // Accumulator is {Count, StateVars...}.
    fn lower_foldl_epilogue_count(&mut self, cx: EpilogueCx<'_>, stmts: &mut Vec<ThreadedStmt>) {
        let (carry, tail) = self.epilogue_tail(cx);
        let value = self.bool_case(
            leaf::var(cx.pv.to_string()),
            [
                (
                    true,
                    acc_tuple(
                        [form::call(
                            "erlang",
                            "+",
                            [Document::Str("CountAcc"), leaf::int_lit(1)],
                        )],
                        &tail,
                    ),
                ),
                (false, acc_tuple([Document::Str("CountAcc")], &tail)),
            ],
        );
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlTakeWhile — include item while predicate holds.
    // Once predicate returns false, StillTaking flips to false and all subsequent
    // elements are excluded. Accumulator: {ResultList, StillTaking, StateVars...}.
    fn lower_foldl_epilogue_take_while(
        &mut self,
        item_var: &str,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let (carry, tail) = self.epilogue_tail(cx);
        let stop = |t: &Document<'static>| {
            acc_tuple([Document::Str("AccList"), Document::Str("'false'")], t)
        };
        let inner = self.bool_case(
            leaf::var(cx.pv.to_string()),
            [
                (
                    true,
                    acc_tuple([cons(item_var, "AccList"), Document::Str("'true'")], &tail),
                ),
                (false, stop(&tail)),
            ],
        );
        let value = form::case_of(
            Document::Str("StillTaking"),
            [bool_arm(false, stop(&tail)), bool_arm(true, inner)],
            Layout::Flat,
        );
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlDropWhile — drop items while predicate holds.
    // Once predicate returns false, StillDropping flips to false and all subsequent
    // elements are included. Accumulator: {ResultList, StillDropping, StateVars...}.
    fn lower_foldl_epilogue_drop_while(
        &mut self,
        item_var: &str,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let (carry, tail) = self.epilogue_tail(cx);
        let keep = |t: &Document<'static>| {
            acc_tuple([cons(item_var, "AccList"), Document::Str("'false'")], t)
        };
        let inner = self.bool_case(
            leaf::var(cx.pv.to_string()),
            [
                (
                    true,
                    acc_tuple([Document::Str("AccList"), Document::Str("'true'")], &tail),
                ),
                (false, keep(&tail)),
            ],
        );
        let value = form::case_of(
            Document::Str("StillDropping"),
            [bool_arm(false, keep(&tail)), bool_arm(true, inner)],
            Layout::Flat,
        );
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlPartition — route item to one of two lists based on predicate.
    // Accumulator: {MatchList, NoMatchList, StateVars...}.
    fn lower_foldl_epilogue_partition(
        &mut self,
        item_var: &str,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let (carry, tail) = self.epilogue_tail(cx);
        let value = self.bool_case(
            leaf::var(cx.pv.to_string()),
            [
                (
                    true,
                    acc_tuple(
                        [cons(item_var, "MatchList"), Document::Str("NoMatchList")],
                        &tail,
                    ),
                ),
                (
                    false,
                    acc_tuple(
                        [Document::Str("MatchList"), cons(item_var, "NoMatchList")],
                        &tail,
                    ),
                ),
            ],
        );
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlGroupBy — group item by key.
    // The pred_var holds the key result. Each element is added to the key's list in a map.
    // Accumulator: {Map, StateVars...}.
    fn lower_foldl_epilogue_group_by(
        &mut self,
        item_var: &str,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        // Use maps:get/3 to get current list for key (default []), prepend item, put back.
        let key_var = cx.pv;
        let (carry, tail) = self.epilogue_tail(cx);
        let existing_var = self.fresh_temp_var("ExistingList");
        let new_list_var = self.fresh_temp_var("NewList");
        let new_map_var = self.fresh_temp_var("NewMap");
        let value = form::let_in(
            leaf::var(existing_var.clone()),
            form::call(
                "maps",
                "get",
                [
                    leaf::var(key_var),
                    Document::Str("GroupMap"),
                    Document::Str("[]"),
                ],
            ),
            form::let_in(
                leaf::var(new_list_var.clone()),
                docvec![
                    "[",
                    leaf::var(item_var.to_string()),
                    " | ",
                    leaf::var(existing_var),
                    "]"
                ],
                form::let_in(
                    leaf::var(new_map_var.clone()),
                    form::call(
                        "maps",
                        "put",
                        [
                            leaf::var(key_var),
                            leaf::var(new_list_var),
                            Document::Str("GroupMap"),
                        ],
                    ),
                    acc_tuple([leaf::var(new_map_var)], &tail),
                    Layout::Flat,
                ),
                Layout::Flat,
            ),
            Layout::Flat,
        );
        stmts.push(Self::fold_accumulator(carry, value, cx.span));
    }

    // FoldlSort — write the comparator's final state back to its
    // process-dictionary key, then answer the predicate to `lists:sort/2`.
    // Always map mode (the generator builds the plan without a tuple
    // accumulator), so the final state is a `StateAcc{N}`.
    fn lower_foldl_epilogue_sort(
        &mut self,
        state_key_var: &str,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let final_state = if cx.has_mutations {
            self.current_state_var()
        } else {
            "StateAcc".to_string()
        };
        // The lambda answers the predicate; the `StateAcc{N}` leaves it
        // through the process dictionary (`erlang:put`), and that map is
        // what the tag records as carried.
        stmts.push(Self::fold_accumulator(
            FoldCarry::StateMap,
            form::let_in(
                leaf::var("_"),
                form::call(
                    "erlang",
                    "put",
                    [leaf::var(state_key_var.to_string()), leaf::var(final_state)],
                ),
                leaf::var(cx.pv.to_string()),
                Layout::Flat,
            ),
            cx.span,
        ));
    }
}

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
use super::super::super::threaded_ir::{FoldCarry, ThreadedStmt};
use super::super::ThreadingPlan;
use super::BodyKind;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
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

impl CoreErlangGenerator {
    // FoldlFilter: append the predicate case expression after all statements.
    fn lower_foldl_epilogue_filter(
        &mut self,
        item_var: &str,
        negate: bool,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        let condition_doc: Document<'static> = if negate {
            docvec!["call 'erlang':'not'(", leaf::var(pv.to_string()), ")",]
        } else {
            leaf::var(pv.to_string())
        };
        if plan.use_tuple_acc {
            // Tuple mode — repack current var bindings into the result tuple.
            let vars_doc = plan.current_vars_doc(self);
            stmts.push(Self::fold_accumulator(
                plan.fold_carry(),
                docvec![
                    "case ",
                    condition_doc,
                    " of <'true'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], ",
                    vars_doc.clone(),
                    "} <'false'> when 'true' -> {AccList, ",
                    vars_doc,
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            stmts.push(Self::fold_accumulator(
                FoldCarry::StateMap,
                docvec![
                    "case ",
                    condition_doc,
                    " of <'true'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], ",
                    leaf::var(final_state.clone()),
                    "} <'false'> when 'true' -> {AccList, ",
                    leaf::var(final_state),
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        }
    }

    // FoldlBoolPredicate — update boolean accumulator based on predicate result.
    // Match only 'true'/'false' explicitly (consistent with FoldlFilter and lists:any/all).
    fn lower_foldl_epilogue_bool_predicate(
        &mut self,
        is_all: bool,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self);
            if is_all {
                // allSatisfy: pred=false → set BoolAcc to false; pred=true → keep
                stmts.push(Self::fold_accumulator(
                    plan.fold_carry(),
                    docvec![
                        "case ",
                        leaf::var(pv.to_string()),
                        " of <'false'> when 'true' -> {'false', ",
                        vars_doc.clone(),
                        "} <'true'> when 'true' -> {BoolAcc, ",
                        vars_doc,
                        "}",
                        self.case_clause_fallback("_PredBad"),
                        " end",
                    ],
                    epilogue_span,
                ));
            } else {
                // anySatisfy: pred=true → set BoolAcc to true; pred=false → keep
                stmts.push(Self::fold_accumulator(
                    plan.fold_carry(),
                    docvec![
                        "case ",
                        leaf::var(pv.to_string()),
                        " of <'true'> when 'true' -> {'true', ",
                        vars_doc.clone(),
                        "} <'false'> when 'true' -> {BoolAcc, ",
                        vars_doc,
                        "}",
                        self.case_clause_fallback("_PredBad"),
                        " end",
                    ],
                    epilogue_span,
                ));
            }
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            if is_all {
                stmts.push(Self::fold_accumulator(
                    FoldCarry::StateMap,
                    docvec![
                        "case ",
                        leaf::var(pv.to_string()),
                        " of <'false'> when 'true' -> {'false', ",
                        leaf::var(final_state.clone()),
                        "} <'true'> when 'true' -> {BoolAcc, ",
                        leaf::var(final_state),
                        "}",
                        self.case_clause_fallback("_PredBad"),
                        " end",
                    ],
                    epilogue_span,
                ));
            } else {
                stmts.push(Self::fold_accumulator(
                    FoldCarry::StateMap,
                    docvec![
                        "case ",
                        leaf::var(pv.to_string()),
                        " of <'true'> when 'true' -> {'true', ",
                        leaf::var(final_state.clone()),
                        "} <'false'> when 'true' -> {BoolAcc, ",
                        leaf::var(final_state),
                        "}",
                        self.case_clause_fallback("_PredBad"),
                        " end",
                    ],
                    epilogue_span,
                ));
            }
        }
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
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self);
            stmts.push(Self::fold_accumulator(
                plan.fold_carry(),
                docvec![
                    "case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {",
                    leaf::var(item_var.to_string()),
                    ", 'true', ",
                    vars_doc.clone(),
                    "} <'false'> when 'true' -> {FoundItem, FoundFlag, ",
                    vars_doc,
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            stmts.push(Self::fold_accumulator(
                FoldCarry::StateMap,
                docvec![
                    "case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {",
                    leaf::var(item_var.to_string()),
                    ", 'true', ",
                    leaf::var(final_state.clone()),
                    "} <'false'> when 'true' -> {FoundItem, FoundFlag, ",
                    leaf::var(final_state),
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        }
    }

    // FoldlCount — increment count accumulator on predicate match.
    // Accumulator is {Count, StateVars...}.
    fn lower_foldl_epilogue_count(&mut self, cx: EpilogueCx<'_>, stmts: &mut Vec<ThreadedStmt>) {
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self);
            stmts.push(Self::fold_accumulator(
                plan.fold_carry(),
                docvec![
                    "case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {call 'erlang':'+'(CountAcc, 1), ",
                    vars_doc.clone(),
                    "} <'false'> when 'true' -> {CountAcc, ",
                    vars_doc,
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            stmts.push(Self::fold_accumulator(
                FoldCarry::StateMap,
                docvec![
                    "case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {call 'erlang':'+'(CountAcc, 1), ",
                    leaf::var(final_state.clone()),
                    "} <'false'> when 'true' -> {CountAcc, ",
                    leaf::var(final_state),
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        }
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
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self);
            stmts.push(Self::fold_accumulator(
                plan.fold_carry(),
                docvec![
                    "case StillTaking of \
                         <'false'> when 'true' -> {AccList, 'false', ",
                    vars_doc.clone(),
                    "} <'true'> when 'true' -> case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], 'true', ",
                    vars_doc.clone(),
                    "} <'false'> when 'true' -> {AccList, 'false', ",
                    vars_doc,
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end end",
                ],
                epilogue_span,
            ));
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            stmts.push(Self::fold_accumulator(
                FoldCarry::StateMap,
                docvec![
                    "case StillTaking of \
                         <'false'> when 'true' -> {AccList, 'false', ",
                    leaf::var(final_state.clone()),
                    "} <'true'> when 'true' -> case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], 'true', ",
                    leaf::var(final_state.clone()),
                    "} <'false'> when 'true' -> {AccList, 'false', ",
                    leaf::var(final_state),
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end end",
                ],
                epilogue_span,
            ));
        }
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
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self);
            stmts.push(Self::fold_accumulator(
                plan.fold_carry(),
                docvec![
                    "case StillDropping of \
                         <'false'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], 'false', ",
                    vars_doc.clone(),
                    "} <'true'> when 'true' -> case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {AccList, 'true', ",
                    vars_doc.clone(),
                    "} <'false'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], 'false', ",
                    vars_doc,
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end end",
                ],
                epilogue_span,
            ));
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            stmts.push(Self::fold_accumulator(
                FoldCarry::StateMap,
                docvec![
                    "case StillDropping of \
                         <'false'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], 'false', ",
                    leaf::var(final_state.clone()),
                    "} <'true'> when 'true' -> case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {AccList, 'true', ",
                    leaf::var(final_state.clone()),
                    "} <'false'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | AccList], 'false', ",
                    leaf::var(final_state),
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end end",
                ],
                epilogue_span,
            ));
        }
    }

    // FoldlPartition — route item to one of two lists based on predicate.
    // Accumulator: {MatchList, NoMatchList, StateVars...}.
    fn lower_foldl_epilogue_partition(
        &mut self,
        item_var: &str,
        cx: EpilogueCx<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self);
            stmts.push(Self::fold_accumulator(
                plan.fold_carry(),
                docvec![
                    "case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | MatchList], NoMatchList, ",
                    vars_doc.clone(),
                    "} <'false'> when 'true' -> {MatchList, [",
                    leaf::var(item_var.to_string()),
                    " | NoMatchList], ",
                    vars_doc,
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            stmts.push(Self::fold_accumulator(
                FoldCarry::StateMap,
                docvec![
                    "case ",
                    leaf::var(pv.to_string()),
                    " of <'true'> when 'true' -> {[",
                    leaf::var(item_var.to_string()),
                    " | MatchList], NoMatchList, ",
                    leaf::var(final_state.clone()),
                    "} <'false'> when 'true' -> {MatchList, [",
                    leaf::var(item_var.to_string()),
                    " | NoMatchList], ",
                    leaf::var(final_state),
                    "}",
                    self.case_clause_fallback("_PredBad"),
                    " end",
                ],
                epilogue_span,
            ));
        }
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
        let EpilogueCx {
            pv,
            plan,
            has_mutations,
            span: epilogue_span,
        } = cx;
        // Use maps:get/3 to get current list for key (default []), prepend item, put back.
        let key_var = pv;
        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self);
            let existing_var = self.fresh_temp_var("ExistingList");
            let new_list_var = self.fresh_temp_var("NewList");
            let new_map_var = self.fresh_temp_var("NewMap");
            stmts.push(Self::fold_accumulator(
                plan.fold_carry(),
                docvec![
                    "let ",
                    leaf::var(existing_var.clone()),
                    " = call 'maps':'get'(",
                    leaf::var(key_var),
                    ", GroupMap, []) in let ",
                    leaf::var(new_list_var.clone()),
                    " = [",
                    leaf::var(item_var.to_string()),
                    " | ",
                    leaf::var(existing_var),
                    "] in let ",
                    leaf::var(new_map_var.clone()),
                    " = call 'maps':'put'(",
                    leaf::var(key_var),
                    ", ",
                    leaf::var(new_list_var),
                    ", GroupMap) in {",
                    leaf::var(new_map_var),
                    ", ",
                    vars_doc,
                    "}",
                ],
                epilogue_span,
            ));
        } else {
            let final_state = if has_mutations {
                self.current_state_var()
            } else {
                "StateAcc".to_string()
            };
            let existing_var = self.fresh_temp_var("ExistingList");
            let new_list_var = self.fresh_temp_var("NewList");
            let new_map_var = self.fresh_temp_var("NewMap");
            stmts.push(Self::fold_accumulator(
                FoldCarry::StateMap,
                docvec![
                    "let ",
                    leaf::var(existing_var.clone()),
                    " = call 'maps':'get'(",
                    leaf::var(key_var),
                    ", GroupMap, []) in let ",
                    leaf::var(new_list_var.clone()),
                    " = [",
                    leaf::var(item_var.to_string()),
                    " | ",
                    leaf::var(existing_var),
                    "] in let ",
                    leaf::var(new_map_var.clone()),
                    " = call 'maps':'put'(",
                    leaf::var(key_var),
                    ", ",
                    leaf::var(new_list_var),
                    ", GroupMap) in {",
                    leaf::var(new_map_var),
                    ", ",
                    leaf::var(final_state),
                    "}",
                ],
                epilogue_span,
            ));
        }
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
        let EpilogueCx {
            pv,
            has_mutations,
            span: epilogue_span,
            ..
        } = cx;
        let final_state = if has_mutations {
            self.current_state_var()
        } else {
            "StateAcc".to_string()
        };
        // The lambda answers the predicate; the `StateAcc{N}` leaves it
        // through the process dictionary (`erlang:put`), and that map is
        // what the tag records as carried.
        stmts.push(Self::fold_accumulator(
            FoldCarry::StateMap,
            docvec![
                "let _ = call 'erlang':'put'(",
                leaf::var(state_key_var.to_string()),
                ", ",
                leaf::var(final_state),
                ") in ",
                leaf::var(pv.to_string()),
            ],
            epilogue_span,
        ));
    }
}

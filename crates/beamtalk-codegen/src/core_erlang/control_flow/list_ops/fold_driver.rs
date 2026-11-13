// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The shared scaffolding of the fold-style list generators (BT-4763).
//!
//! Every stateful list op (`do:`, `collect:`, `select:`, `reject:`, …) lowers
//! to `lists:foldl` with the same four steps:
//!
//! 1. build a [`ThreadingPlan`] (tuple accumulator or `StateAcc` map);
//! 2. convert the receiver to a safe list and mint the lambda / item vars
//!    ([`CoreErlangGenerator::begin_list_fold`]);
//! 3. lower the body inside the guarded-operator marker and a scope
//!    ([`CoreErlangGenerator::lower_tuple_fold_body`] /
//!    [`CoreErlangGenerator::lower_map_fold_body`]);
//! 4. emit the fold call, the result extraction and an op-specific epilogue.
//!
//! This module owns steps 1–3 for every op, and step 4 for the
//! collect-style ops (`collect:`, `select:`, `reject:`) through
//! [`CoreErlangGenerator::generate_accumulate_fold_with_mutations`], whose
//! per-op [`AccumulateOp`] spec supplies the label and body kind.

use super::super::super::{CoreErlangGenerator, Result};
use super::super::analysis::SELF_THREADING_FOLDS;
use super::super::family_slots::FoldFamilySlots;
use super::super::{BodyKind, ListOpKind, ThreadingPlan};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

/// The step 1–2 results every fold generator needs.
pub(super) struct FoldHead {
    pub(super) plan: ThreadingPlan,
    /// `true` when the body block was marked as a discarded fold (`do:` only).
    pub(super) discard: bool,
    /// The temp the receiver is bound to (also the String-aware result check).
    pub(super) list_var: String,
    pub(super) recv_code: Document<'static>,
    pub(super) safe_list_var: String,
    pub(super) lambda_var: String,
    pub(super) item_var: String,
}

/// What the tuple-accumulator body lowering hands back to its epilogue.
pub(super) struct TupleFoldBody {
    pub(super) body_doc: Document<'static>,
    /// Whether the guarded-operator marker asked for a `State` bracket.
    pub(super) guarded_op_bracket: bool,
    /// The pre-store to splice before the lambda (`nil` unless bracketing).
    pub(super) prestore: Document<'static>,
}

/// The collect-style ops that share one epilogue: the result is a list built
/// in the first accumulator slot, reversed and (for a String receiver)
/// repacked.
#[derive(Clone, Copy)]
pub(super) enum AccumulateOp {
    Collect,
    Select,
    Reject,
}

impl AccumulateOp {
    /// The selector: the receiver guard's label and the key
    /// [`SELF_THREADING_FOLDS`] is consulted with.
    pub(super) fn label(self) -> &'static str {
        match self {
            Self::Collect => "collect:",
            Self::Select => "select:",
            Self::Reject => "reject:",
        }
    }

    fn body_kind(self, item_var: &str) -> BodyKind {
        match self {
            Self::Collect => BodyKind::FoldlCollect,
            Self::Select | Self::Reject => BodyKind::FoldlFilter {
                item_var: item_var.to_string(),
                negate: matches!(self, Self::Reject),
            },
        }
    }
}

/// Whether the fold selector `label` threads a value-type `Self` through its
/// accumulator ([`SELF_THREADING_FOLDS`]); every other fold rejects a field
/// write in its body.
pub(super) fn fold_threads_self(label: &str) -> bool {
    SELF_THREADING_FOLDS.contains(&label)
}

impl CoreErlangGenerator {
    /// Steps 1–2: the plan, the receiver and the lambda / item variables.
    ///
    /// `handler` is a block packed with the body (`detect:ifNone:`'s `ifNone:`).
    /// `guard_discard` takes the discarded-fold mark for `body` (`do:`).
    pub(super) fn begin_list_fold(
        &mut self,
        receiver: &Expression,
        body: &Block,
        kind: ListOpKind,
        label: &str,
        handler: Option<&Block>,
        guard_discard: bool,
    ) -> Result<FoldHead> {
        let mut plan = ThreadingPlan::new_for_foldl_list_op_with_handler(self, body, kind, handler);
        if fold_threads_self(label) {
            plan = plan.with_value_self_slot(self, body)?;
        }
        self.emit_loop_convention_diagnostic(&plan, body.span);
        let discard = guard_discard && self.take_discarded_fold(body.span);

        // Add is_list guard for non-list collection types.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        Ok(FoldHead {
            plan,
            discard,
            list_var,
            recv_code,
            safe_list_var,
            lambda_var,
            item_var,
        })
    }

    /// Step 3, tuple-accumulator flavour: lowers `body` inside the
    /// guarded-operator marker (BT-4121) and a scope, then checks the marker
    /// out. `index_offset` is the number of leading accumulator slots.
    pub(super) fn lower_tuple_fold_body(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
        item_var: &str,
        acc_var: &str,
        index_offset: usize,
    ) -> Result<TupleFoldBody> {
        // The marker spans the body lowering (the receiver is method-level
        // setup, already lowered). `prestore_state` is the loop-entry `State`
        // the pre-store captures when the marker asks for the bracket.
        let prestore_state = self.state_var_to_store();
        self.push_guarded_op_loop(body, None);
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, item_var);
        }
        let lowered = self.generate_foldl_loop_body(body, plan, kind, acc_var, index_offset);
        self.pop_scope();
        // Check the marker out before propagating a lowering error so the
        // marker stack stays balanced.
        let marker = self.pop_guarded_op_loop();
        let (body_doc, _) = lowered?;
        let (guarded_op_bracket, prestore) =
            self.checkout_guarded_op_loop_docs(marker, false, body.span, &prestore_state);
        Ok(TupleFoldBody {
            body_doc,
            guarded_op_bracket,
            prestore,
        })
    }

    /// Step 3, map-accumulator flavour: lowers `body` in a fresh scope.
    pub(super) fn lower_map_fold_body(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
        item_var: &str,
        acc_param_name: &str,
        index_offset: usize,
    ) -> Result<Document<'static>> {
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, item_var);
        }
        let lowered = self.generate_foldl_loop_body(body, plan, kind, acc_param_name, index_offset);
        self.pop_scope();
        Ok(lowered?.0)
    }

    /// Stateful `collect:` / `select:` / `reject:` over `lists:foldl`.
    ///
    /// In map mode the accumulator is `{ResultList, StateAcc}`; in tuple mode
    /// `{ResultList, Var1, ..., VarN}`.
    #[allow(clippy::too_many_lines)]
    pub(super) fn generate_accumulate_fold_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        op: AccumulateOp,
    ) -> Result<Document<'static>> {
        let FoldHead {
            plan,
            list_var,
            recv_code,
            safe_list_var,
            lambda_var,
            item_var,
            ..
        } = self.begin_list_fold(
            receiver,
            body,
            ListOpKind::Accumulate,
            op.label(),
            None,
            false,
        )?;
        // Save recv var for is_binary check after foldl (String-aware result).
        let recv_var_for_str_check = list_var.clone();
        let acc_state_var = self.fresh_temp_var("AccSt");
        let body_kind = op.body_kind(&item_var);

        if plan.use_tuple_acc {
            // Initial fold acc: {[], Var1, ..., VarN} (flat tuple, no StateAcc map).
            let vars_doc = plan.current_vars_doc(self); // Before push_scope.
            let init_tuple_doc = docvec!["{ [], ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                Some(op.label()),
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let AccList = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            // Unpack vars starting at index 2 (slot 1 is AccList), merged
            // with the body into ONE verified `Threaded` node.
            let lowered =
                self.lower_tuple_fold_body(body, &plan, &body_kind, &item_var, &acc_state_var, 1)?;
            docs.push(lowered.body_doc);
            docs.insert(0, lowered.prestore);

            let fold_result = self.fresh_temp_var("FoldResult");
            let rev_list = self.fresh_temp_var("RevList");
            let final_list = self.fresh_temp_var("FinalList");

            // Extract each updated local var from tuple positions 2..N.
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);

            // String-aware result wrapping — if the original receiver
            // was a binary (String), convert the reversed list back to a binary.
            let (str_binding, str_result) =
                self.generate_list_like_result_binding(&recv_var_for_str_check, &final_list);

            // Re-pack into StateAcc for outer method-body `maps:get` extraction.
            let (repack_doc, stateacc) =
                plan.append_repack_stateacc_doc(self, lowered.guarded_op_bracket);
            docs.push(docvec![
                ThreadingPlan::foldl_call_doc(
                    &lambda_var,
                    init_tuple_doc,
                    &safe_list_var,
                    &fold_result
                ),
                "let ",
                leaf::var(rev_list.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(fold_result),
                ") in let ",
                leaf::var(final_list.clone()),
                " = call 'lists':'reverse'(",
                leaf::var(rev_list),
                ") in ",
                str_binding,
                " in ",
                extract_doc,
                repack_doc,
                "{",
                leaf::var(str_result),
                ", ",
                leaf::var(stateacc),
                "}",
            ]);
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            Some(op.label()),
            list_var,
            safe_list_var.clone(),
        ));
        // BT-3827: a value-type body that writes a field carries `Self` in a
        // `{Acc, Self}` wrapper around the accumulator (only the ops
        // `SELF_THREADING_FOLDS` names; the rest have no slots).
        let slots = plan.fold_family_slots(self);
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(
                slots
                    .as_ref()
                    .map_or(acc_state_var.as_str(), FoldFamilySlots::lambda_param)
                    .to_string()
            ),
            ") -> ",
            slots
                .as_ref()
                .map_or(Document::Nil, |s| s.lambda_prologue(&acc_state_var)),
            "let AccList = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var.clone()),
            ") in ",
        ]);

        docs.push(self.lower_map_fold_body(
            body,
            &plan,
            &body_kind,
            &item_var,
            &acc_state_var,
            1,
        )?);

        let fold_result = self.fresh_temp_var("FoldResult");
        let rev_list = self.fresh_temp_var("RevList");
        let final_list = self.fresh_temp_var("FinalList");
        let state_out = self.fresh_temp_var("StOut");

        // String-aware result wrapping for the map-acc path.
        let (str_binding_doc, str_result) =
            self.generate_list_like_result_binding(&recv_var_for_str_check, &final_list);

        let init_acc = docvec!["{[], ", leaf::var(init_state), "}"];
        let init_acc = match &slots {
            Some(s) => s.wrap_init(init_acc, self),
            None => init_acc,
        };
        let pre_doc = docvec![
            ThreadingPlan::foldl_call_doc(
                &lambda_var,
                init_acc,
                &safe_list_var,
                slots
                    .as_ref()
                    .map_or(fold_result.as_str(), FoldFamilySlots::result_var)
            ),
            slots
                .as_ref()
                .map_or(Document::Nil, |s| s.unwrap_result(&fold_result)),
            "let ",
            leaf::var(rev_list.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(final_list.clone()),
            " = call 'lists':'reverse'(",
            leaf::var(rev_list),
            ") in ",
        ];
        let post_doc = docvec![
            " in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            {
                let result_open = docvec!["{", leaf::var(str_result), ", ", leaf::var(state_out)];
                match &slots {
                    Some(s) => s.close_tuple(result_open, self),
                    None => docvec![result_open, "}"],
                }
            },
        ];
        docs.push(docvec![pre_doc, str_binding_doc, post_doc]);

        Ok(Document::Vec(docs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The intentional `SELF_THREADING_FOLDS` difference (BT-3827): `do:`,
    /// `collect:` and `inject:into:` thread a value-type `Self` through the
    /// accumulator; every other fold selector rejects a field write.
    #[test]
    fn self_threading_is_exactly_the_named_folds() {
        for label in ["do:", "collect:", "inject:into:"] {
            assert!(fold_threads_self(label), "{label} must thread Self");
        }
        for label in [
            "select:",
            "reject:",
            "detect:",
            "detect:ifNone:",
            "anySatisfy:",
            "allSatisfy:",
            "count:",
            "flatMap:",
            "takeWhile:",
            "dropWhile:",
            "partition:",
            "groupBy:",
        ] {
            assert!(!fold_threads_self(label), "{label} must not thread Self");
        }
    }

    #[test]
    fn accumulate_ops_use_their_selector_as_label() {
        assert_eq!(AccumulateOp::Collect.label(), "collect:");
        assert_eq!(AccumulateOp::Select.label(), "select:");
        assert_eq!(AccumulateOp::Reject.label(), "reject:");
        assert!(fold_threads_self(AccumulateOp::Collect.label()));
        assert!(!fold_threads_self(AccumulateOp::Select.label()));
        assert!(!fold_threads_self(AccumulateOp::Reject.label()));
    }
}

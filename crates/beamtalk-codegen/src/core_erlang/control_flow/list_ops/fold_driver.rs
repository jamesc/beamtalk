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
//! This module owns steps 1–3 for every op, and step 4 for the folds [`FoldOp`]
//! names through [`CoreErlangGenerator::generate_fold_with_mutations`], whose
//! per-op [`FoldOp`] spec supplies the label, body kind, leading accumulator
//! slots and post-fold epilogue.

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
pub(in crate::core_erlang::control_flow) struct TupleFoldBody {
    pub(in crate::core_erlang::control_flow) body_doc: Document<'static>,
    /// Whether the guarded-operator marker asked for a `State` bracket.
    pub(in crate::core_erlang::control_flow) guarded_op_bracket: bool,
    /// The pre-store to splice before the lambda (`nil` unless bracketing).
    pub(in crate::core_erlang::control_flow) prestore: Document<'static>,
}

/// Which Core Erlang variables the fold body block's parameters bind to.
#[derive(Clone, Copy)]
pub(in crate::core_erlang::control_flow) enum FoldParams<'a> {
    /// A one-parameter block: its parameter is the item.
    Item(&'a str),
    /// A two-parameter block (`inject:into:`'s accumulator and item, a
    /// dictionary's key and value): the first and second parameter.
    Pair { first: &'a str, second: &'a str },
}

impl<'a> From<&'a String> for FoldParams<'a> {
    fn from(item: &'a String) -> Self {
        Self::Item(item)
    }
}

impl<'a> From<&'a str> for FoldParams<'a> {
    fn from(item: &'a str) -> Self {
        Self::Item(item)
    }
}

/// Binds the `detect:` answer out of a mutation-threading fold result,
/// raising `not_found` when nothing matched.
///
/// The fold seeds its accumulator with `{'nil', 'false', …}` and processes every
/// element, so slot 1 alone cannot say whether the search succeeded — `'nil'` is
/// both the seed and a legitimate match. Slot 2 is the found flag, and it is the
/// one consulted here, which also makes `#(nil) detect: [:x | x isNil]` answer
/// `nil` rather than raise.
///
/// The class named in the error comes from the receiver, so a `Set` routed
/// through the list-op path reports `Set` rather than `List`.
fn bind_detect_found_or_raise_doc(
    found_result: &str,
    fold_result: &str,
    recv_var: &str,
    class_var: &str,
    no_match_var: String,
) -> Document<'static> {
    docvec![
        "let ",
        leaf::var(found_result.to_owned()),
        " = case call 'erlang':'element'(2, ",
        leaf::var(fold_result.to_owned()),
        ") of <'true'> when 'true' -> call 'erlang':'element'(1, ",
        leaf::var(fold_result.to_owned()),
        ") <'false'> when 'true' -> let ",
        leaf::var(class_var.to_owned()),
        " = call 'beamtalk_primitive':'class_of'(",
        leaf::var(recv_var.to_owned()),
        ") in call 'beamtalk_collection':'raiseDetectNotFound'(",
        leaf::var(class_var.to_owned()),
        ")",
        // An explicit fallback: a `case` without one inside a `try` body (a
        // protected body, a handler) trips OTP's `beam_validator`
        // (`ambiguous_catch_try_state`, see `case_clause_fallback`).
        CoreErlangGenerator::case_clause_fallback_doc(no_match_var),
        " end in ",
    ]
}

/// BT-3841 (BT-3805): the fold lambda of a writing `anySatisfy:` /
/// `allSatisfy:` / `detect:` / `detect:ifNone:` stops running the block once the
/// answer is decided (Smalltalk semantics: the block runs up to and including
/// the first decisive element). `lists:foldl` has no early exit, so the lambda
/// returns the accumulator unchanged for every later element: `decided_value` is
/// the value of `decided_var` (`BoolAcc` / `FoundFlag`) that means decided.
fn skip_when_decided(
    decided_value: &'static str,
    decided_var: &'static str,
    acc_state_var: &str,
    body: Document<'static>,
) -> Document<'static> {
    docvec![
        "case ",
        decided_var,
        " of <",
        decided_value,
        "> when 'true' -> ",
        leaf::var(acc_state_var.to_string()),
        " <_> when 'true' -> ",
        body,
        " end",
    ]
}

/// The spec of a stateful fold the driver
/// ([`CoreErlangGenerator::generate_fold_with_mutations`]) lowers: its label,
/// body kind, leading accumulator slots and post-fold epilogue.
#[derive(Clone, Copy)]
pub(in crate::core_erlang::control_flow) enum FoldOp<'a> {
    /// `collect:` — the mapped values, reversed.
    Collect,
    /// `select:` — the kept values, reversed.
    Select,
    /// `reject:` — the dropped values, reversed.
    Reject,
    /// `count:` — a running count.
    Count,
    /// `flatMap:` — reversed sub-lists, then reversed and appended.
    FlatMap,
    /// `takeWhile:` — `{ResultList, StillTaking}`.
    TakeWhile,
    /// `dropWhile:` — `{ResultList, StillDropping}`.
    DropWhile,
    /// `partition:` — `{MatchList, NoMatchList}`.
    Partition,
    /// `groupBy:` — a map of key to reversed group.
    GroupBy,
    /// `anySatisfy:` — a boolean accumulator, decided by the first match.
    AnySatisfy,
    /// `allSatisfy:` — a boolean accumulator, decided by the first failure.
    AllSatisfy,
    /// `detect:` — `{FoundItem, FoundFlag}`, raising `not_found` on no match.
    Detect,
    /// `detect:ifNone:` — `{FoundItem, FoundFlag}`, then `if_none` on no match.
    DetectIfNone { if_none: &'a Expression },
    /// `inject:into:` (BT-3916: also the synthetic fold under
    /// `eachWithIndex:` / `do:separatedBy:`, whose receiver guard names
    /// `guard`).
    Inject {
        initial: &'a Expression,
        guard: &'a str,
    },
}

/// The post-fold result of a [`FoldOp`]: the `let` chain ending in `" in "`
/// and the variable holding the op's result value.
struct Epilogue {
    bindings: Document<'static>,
    result: Document<'static>,
    /// `detect:ifNone:` only: the found flag its closing `case` tests.
    found_flag: Option<Document<'static>>,
}

impl FoldOp<'_> {
    /// The selector: the key [`SELF_THREADING_FOLDS`] is consulted with.
    pub(super) fn label(&self) -> &'static str {
        match self {
            Self::Collect => "collect:",
            Self::Select => "select:",
            Self::Reject => "reject:",
            Self::Count => "count:",
            Self::FlatMap => "flatMap:",
            Self::TakeWhile => "takeWhile:",
            Self::DropWhile => "dropWhile:",
            Self::Partition => "partition:",
            Self::GroupBy => "groupBy:",
            Self::AnySatisfy => "anySatisfy:",
            Self::AllSatisfy => "allSatisfy:",
            Self::Detect => "detect:",
            Self::DetectIfNone { .. } => "detect:ifNone:",
            Self::Inject { .. } => "inject:into:",
        }
    }

    /// The selector the receiver guard checks.
    fn guard_selector(&self) -> &str {
        match self {
            Self::Inject { guard, .. } => guard,
            _ => self.label(),
        }
    }

    fn list_kind(&self) -> ListOpKind {
        match self {
            Self::TakeWhile
            | Self::DropWhile
            | Self::Partition
            | Self::Detect
            | Self::DetectIfNone { .. } => ListOpKind::TwoSlot,
            _ => ListOpKind::Accumulate,
        }
    }

    fn body_kind(&self, item_var: &str) -> BodyKind {
        let item_var = || item_var.to_string();
        match self {
            Self::Collect | Self::FlatMap => BodyKind::FoldlCollect,
            Self::Select | Self::Reject => BodyKind::FoldlFilter {
                item_var: item_var(),
                negate: matches!(self, Self::Reject),
            },
            Self::Count => BodyKind::FoldlCount,
            Self::TakeWhile => BodyKind::FoldlTakeWhile {
                item_var: item_var(),
            },
            Self::DropWhile => BodyKind::FoldlDropWhile {
                item_var: item_var(),
            },
            Self::Partition => BodyKind::FoldlPartition {
                item_var: item_var(),
            },
            Self::GroupBy => BodyKind::FoldlGroupBy {
                item_var: item_var(),
            },
            Self::AnySatisfy | Self::AllSatisfy => BodyKind::FoldlBoolPredicate {
                is_all: matches!(self, Self::AllSatisfy),
            },
            Self::Detect | Self::DetectIfNone { .. } => BodyKind::FoldlDetect {
                item_var: item_var(),
            },
            Self::Inject { .. } => BodyKind::FoldlInject,
        }
    }

    /// The names the lambda binds the leading accumulator slots to
    /// (`inject:into:` mints its own).
    fn slot_names(&self) -> &'static [&'static str] {
        match self {
            Self::Collect | Self::Select | Self::Reject | Self::FlatMap => &["AccList"],
            Self::Count => &["CountAcc"],
            Self::TakeWhile => &["AccList", "StillTaking"],
            Self::DropWhile => &["AccList", "StillDropping"],
            Self::Partition => &["MatchList", "NoMatchList"],
            Self::GroupBy => &["GroupMap"],
            Self::AnySatisfy | Self::AllSatisfy => &["BoolAcc"],
            Self::Detect | Self::DetectIfNone { .. } => &["FoundItem", "FoundFlag"],
            Self::Inject { .. } => &[],
        }
    }

    /// The seed of each leading slot (`inject:into:` seeds its own).
    fn slot_inits(&self) -> Vec<Document<'static>> {
        let empty_list = || Document::Str("[]");
        match self {
            Self::Collect | Self::Select | Self::Reject | Self::FlatMap => vec![empty_list()],
            Self::Count => vec![leaf::int_lit(0)],
            Self::TakeWhile | Self::DropWhile => vec![empty_list(), leaf::atom("true")],
            Self::Partition => vec![empty_list(), empty_list()],
            Self::GroupBy => vec![Document::Str("~{}~")],
            Self::AnySatisfy => vec![leaf::atom("false")],
            Self::AllSatisfy => vec![leaf::atom("true")],
            Self::Detect | Self::DetectIfNone { .. } => {
                vec![leaf::atom("nil"), leaf::atom("false")]
            }
            Self::Inject { .. } => vec![],
        }
    }

    /// Whether the tuple-mode initial accumulator opens `{ ` (a list or map
    /// seed) rather than `{` (a count, a boolean, `nil` or an `inject:into:`
    /// seed).
    fn pads_tuple_init(&self) -> bool {
        matches!(
            self,
            Self::Collect
                | Self::Select
                | Self::Reject
                | Self::FlatMap
                | Self::TakeWhile
                | Self::DropWhile
                | Self::Partition
                | Self::GroupBy
        )
    }

    /// BT-3841 (BT-3805): the early-exit capability. A search fold stops
    /// running the block once its answer is decided; `lists:foldl` has no
    /// early exit, so the lambda returns the accumulator unchanged for every
    /// later element. `(decided value, decided slot)` when the op has one.
    fn decided(&self) -> Option<(&'static str, &'static str)> {
        match self {
            Self::AnySatisfy => Some(("'true'", "BoolAcc")),
            Self::AllSatisfy => Some(("'false'", "BoolAcc")),
            Self::Detect | Self::DetectIfNone { .. } => Some(("'true'", "FoundFlag")),
            _ => None,
        }
    }

    /// Wraps the lowered fold body in the early-exit guard when the op has one.
    fn guard_decided(&self, acc_state_var: &str, body: Document<'static>) -> Document<'static> {
        match self.decided() {
            Some((value, var)) => skip_when_decided(value, var, acc_state_var, body),
            None => body,
        }
    }

    /// `detect:ifNone:`'s handler block, packed with the search block.
    fn handler(&self) -> Option<&Block> {
        match self {
            Self::DetectIfNone {
                if_none: Expression::Block(handler),
            } => Some(handler),
            _ => None,
        }
    }

    /// The prefix of the temp the fold call binds its result to.
    fn fold_result_prefix(&self) -> &'static str {
        match self {
            Self::Inject { .. } => "temp",
            _ => "FoldResult",
        }
    }

    /// Mints the epilogue's fresh variables (before the map-mode state out).
    fn epilogue_vars(&self, generator: &mut CoreErlangGenerator) -> Vec<String> {
        let names: &[&str] = match self {
            Self::Collect | Self::Select | Self::Reject | Self::TakeWhile | Self::DropWhile => {
                &["RevList", "FinalList"]
            }
            Self::Count => &["CountResult"],
            Self::FlatMap => &["RevList", "OrderedList", "FinalList"],
            Self::Partition => &[
                "RevMatch",
                "RevNoMatch",
                "FinalMatch",
                "FinalNoMatch",
                "PartResult",
            ],
            Self::GroupBy => &["RawMap", "FinalMap"],
            Self::AnySatisfy | Self::AllSatisfy => &["BoolResult"],
            Self::Detect => &["FoundResult", "DetectRecvClass", "DetectNoMatch"],
            Self::DetectIfNone { .. } => &["FoundItem", "FoundFlag"],
            Self::Inject { .. } => &["AccOut"],
        };
        names.iter().map(|n| generator.fresh_temp_var(n)).collect()
    }

    /// The `let` chain turning the fold result into the op's value.
    #[allow(clippy::too_many_lines)]
    /// `str_check_var` is the receiver temp the String-aware repack tests.
    fn epilogue(
        &self,
        generator: &mut CoreErlangGenerator,
        fold_result: &str,
        vars: &[String],
        str_check_var: &str,
    ) -> Epilogue {
        let element = |n: i64, v: &str| {
            docvec![
                " = call 'erlang':'element'(",
                leaf::int_lit(n),
                ", ",
                leaf::var(v.to_string()),
                ") in "
            ]
        };
        let var = |i: usize| leaf::var(vars[i].clone());
        let reversed = |out: usize, from: usize| {
            docvec![
                "let ",
                var(out),
                " = call 'lists':'reverse'(",
                var(from),
                ") in "
            ]
        };
        let first = |out: usize| docvec!["let ", var(out), element(1, fold_result)];
        match self {
            Self::Collect | Self::Select | Self::Reject => {
                let (str_binding, str_result) =
                    generator.generate_list_like_result_binding(str_check_var, &vars[1]);
                Epilogue {
                    found_flag: None,
                    bindings: docvec![first(0), reversed(1, 0), str_binding, " in "],
                    result: leaf::var(str_result),
                }
            }
            Self::TakeWhile | Self::DropWhile => Epilogue {
                found_flag: None,
                bindings: docvec![first(0), reversed(1, 0)],
                result: var(1),
            },
            Self::Detect => Epilogue {
                found_flag: None,
                bindings: bind_detect_found_or_raise_doc(
                    &vars[0],
                    fold_result,
                    str_check_var,
                    &vars[1],
                    vars[2].clone(),
                ),
                result: var(0),
            },
            Self::DetectIfNone { .. } => Epilogue {
                found_flag: Some(var(1)),
                bindings: docvec![first(0), "let ", var(1), element(2, fold_result)],
                result: var(0),
            },
            Self::Count | Self::AnySatisfy | Self::AllSatisfy | Self::Inject { .. } => Epilogue {
                found_flag: None,
                bindings: first(0),
                result: var(0),
            },
            Self::FlatMap => Epilogue {
                found_flag: None,
                bindings: docvec![
                    first(0),
                    reversed(1, 0),
                    "let ",
                    var(2),
                    " = call 'lists':'append'(",
                    var(1),
                    ") in "
                ],
                result: var(2),
            },
            Self::Partition => Epilogue {
                found_flag: None,
                bindings: docvec![
                    first(0),
                    "let ",
                    var(1),
                    element(2, fold_result),
                    reversed(2, 0),
                    reversed(3, 1),
                    "let ",
                    var(4),
                    " = [",
                    var(2),
                    " | [",
                    var(3),
                    " | []]] in "
                ],
                result: var(4),
            },
            Self::GroupBy => Epilogue {
                found_flag: None,
                bindings: docvec![
                    first(0),
                    "let ",
                    var(1),
                    " = call 'beamtalk_list':'reverse_group_values'(",
                    var(0),
                    ") in "
                ],
                result: var(1),
            },
        }
    }
}

/// `{` (`{ ` when `pad`), each slot seed and `tail`, closed with `}`.
fn tuple_init_doc(
    pad: bool,
    seeds: &[Document<'static>],
    tail: Document<'static>,
) -> Document<'static> {
    let mut docs = vec![Document::Str(if pad { "{ " } else { "{" })];
    for seed in seeds {
        docs.push(seed.clone());
        docs.push(Document::Str(", "));
    }
    docs.push(tail);
    docs.push(Document::Str("}"));
    Document::Vec(docs)
}

/// Whether the fold selector `label` threads a value-type `Self` through its
/// accumulator ([`SELF_THREADING_FOLDS`]); every other fold rejects a field
/// write in its body.
pub(super) fn fold_threads_self(label: &str) -> bool {
    SELF_THREADING_FOLDS.contains(&label)
}

impl CoreErlangGenerator {
    /// Step 1: the plan, the loop-convention diagnostic and the discarded-fold
    /// mark. `threads_self` opts the plan into the value-type `Self` slot
    /// ([`fold_threads_self`] decides it for a list fold; a dictionary fold
    /// has no `Self` slot and passes `false`).
    pub(in crate::core_erlang::control_flow) fn begin_fold_plan(
        &mut self,
        body: &Block,
        kind: ListOpKind,
        handler: Option<&Block>,
        threads_self: bool,
        guard_discard: bool,
    ) -> Result<(ThreadingPlan, bool)> {
        let mut plan = ThreadingPlan::new_for_foldl_list_op_with_handler(self, body, kind, handler);
        if threads_self {
            plan = plan.with_value_self_slot(self, body)?;
        }
        self.emit_loop_convention_diagnostic(&plan, body.span);
        let discard = guard_discard && self.take_discarded_fold(body.span);
        Ok((plan, discard))
    }

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
        let (plan, discard) =
            self.begin_fold_plan(body, kind, handler, fold_threads_self(label), guard_discard)?;
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

    fn bind_fold_params(&mut self, body: &Block, params: FoldParams<'_>) {
        match params {
            FoldParams::Item(item) => {
                if let Some(param) = body.parameters.first() {
                    self.bind_var(&param.name, item);
                }
            }
            FoldParams::Pair { first, second } => {
                if let Some(param) = body.parameters.first() {
                    self.bind_var(&param.name, first);
                }
                if let Some(param) = body.parameters.get(1) {
                    self.bind_var(&param.name, second);
                }
            }
        }
    }

    /// Step 3, tuple-accumulator flavour: lowers `body` inside the
    /// guarded-operator marker (BT-4121) and a scope, then checks the marker
    /// out. `index_offset` is the number of leading accumulator slots.
    pub(in crate::core_erlang::control_flow) fn lower_tuple_fold_body<'a>(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
        params: impl Into<FoldParams<'a>>,
        acc_var: &str,
        index_offset: usize,
    ) -> Result<TupleFoldBody> {
        // The marker spans the body lowering (the receiver is method-level
        // setup, already lowered). `prestore_state` is the loop-entry `State`
        // the pre-store captures when the marker asks for the bracket.
        let prestore_state = self.state_var_to_store();
        self.push_guarded_op_loop(body, None);
        self.push_scope();
        self.bind_fold_params(body, params.into());
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
    pub(in crate::core_erlang::control_flow) fn lower_map_fold_body<'a>(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
        params: impl Into<FoldParams<'a>>,
        acc_param_name: &str,
        index_offset: usize,
    ) -> Result<Document<'static>> {
        self.push_scope();
        self.bind_fold_params(body, params.into());
        let lowered = self.generate_foldl_loop_body(body, plan, kind, acc_param_name, index_offset);
        self.pop_scope();
        Ok(lowered?.0)
    }

    /// The one stateful `lists:foldl` lowering the folds [`FoldOp`] names go
    /// through (BT-4807): the driver owns the tuple-accumulator versus
    /// `StateAcc`-map choice, the lambda prologue, the fold call and the
    /// state extraction; [`FoldOp`] supplies the leading accumulator slots,
    /// the body kind and the post-fold epilogue.
    ///
    /// Tuple mode folds `{Slot1, .., SlotK, Var1, .., VarN}`, map mode
    /// `{Slot1, .., SlotK, StateAcc}`. Fresh variables are minted in one
    /// fixed order (the fold result, the epilogue's, then — map mode only —
    /// the state out) so the generated Core Erlang names stay stable.
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang::control_flow) fn generate_fold_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        op: FoldOp<'_>,
    ) -> Result<Document<'static>> {
        let label = op.label();
        let (plan, _) = self.begin_fold_plan(
            body,
            op.list_kind(),
            op.handler(),
            fold_threads_self(label),
            false,
        )?;
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        // `inject:into:` binds its initial value ahead of the lambda.
        let init = match &op {
            FoldOp::Inject { initial, .. } => {
                let init_var = self.fresh_temp_var("temp");
                let init_code = self.expression_doc(initial)?;
                Some((init_var, init_code))
            }
            _ => None,
        };
        let lambda_var = self.fresh_temp_var("temp");
        let acc_state_var = self.fresh_temp_var("AccSt");
        // `inject:into:` mints its item and accumulator parameters per fold:
        // a fixed `Item`/`Acc` would be shadowed by an `inject:into:` nested in
        // this one's body that reads this fold's parameter. Every other fold
        // reuses the block's own item parameter.
        let (item_var, inject_acc_var) = if init.is_some() {
            let item_var = self.fresh_temp_var("Item");
            (item_var, Some(self.fresh_temp_var("Acc")))
        } else {
            let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
            (Self::to_core_erlang_var(item_param), None)
        };
        let slot_names: Vec<String> = match &inject_acc_var {
            Some(acc) => vec![acc.clone()],
            None => op.slot_names().iter().map(|n| (*n).to_string()).collect(),
        };
        let slot_count = slot_names.len();
        let body_kind = op.body_kind(&item_var);
        let params = match &inject_acc_var {
            Some(acc) => FoldParams::Pair {
                first: acc,
                second: &item_var,
            },
            None => FoldParams::Item(&item_var),
        };
        // `let <init> = <code> in ` ahead of the lambda (inject only).
        let init_binding = match &init {
            Some((v, code)) => docvec!["let ", leaf::var(v.clone()), " = ", code.clone(), " in "],
            None => Document::Nil,
        };
        // The seed of each leading slot, in order.
        let slot_inits: Vec<Document<'static>> = match &init {
            Some((v, _)) => vec![leaf::var(v.clone())],
            None => op.slot_inits(),
        };
        // `let Slot = element(i, Acc) in ...` for each leading slot.
        let slot_bindings = Document::Vec(
            slot_names
                .iter()
                .zip(1_i64..)
                .map(|(name, index)| {
                    docvec![
                        "let ",
                        leaf::var(name.clone()),
                        " = call 'erlang':'element'(",
                        leaf::int_lit(index),
                        ", ",
                        leaf::var(acc_state_var.clone()),
                        ") in ",
                    ]
                })
                .collect(),
        );
        let state_index = i64::try_from(slot_count + 1).unwrap_or(i64::MAX);
        // Save the receiver temp for the String-aware result check.
        let str_check_var = list_var.clone();
        let safe_list = super::list_recv_to_safe_list_doc(
            recv_code,
            Some(op.guard_selector()),
            list_var,
            safe_list_var.clone(),
        );

        if plan.use_tuple_acc {
            let vars_doc = plan.current_vars_doc(self); // Before push_scope.
            let init_tuple_doc = tuple_init_doc(op.pads_tuple_init(), &slot_inits, vars_doc);

            let mut docs: Vec<Document<'static>> = vec![safe_list];
            docs.push(docvec![
                init_binding,
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> ",
                slot_bindings,
            ]);
            let lowered = self.lower_tuple_fold_body(
                body,
                &plan,
                &body_kind,
                params,
                &acc_state_var,
                slot_count,
            )?;
            docs.push(op.guard_decided(&acc_state_var, lowered.body_doc));
            docs.insert(0, lowered.prestore);

            let fold_result = self.fresh_temp_var(op.fold_result_prefix());
            let vars = op.epilogue_vars(self);
            let epilogue = op.epilogue(self, &fold_result, &vars, &str_check_var);
            // Extract each updated local var from the tuple past the slots.
            let extract_doc =
                plan.generate_tuple_extract_suffix_doc(&fold_result, slot_count + 1, self);
            // Re-pack into StateAcc for outer method-body `maps:get` extraction.
            let (repack_doc, stateacc) =
                plan.append_repack_stateacc_doc(self, lowered.guarded_op_bracket);
            let tail =
                self.fold_tail_doc(&op, epilogue.found_flag, epilogue.result, &stateacc, None)?;
            docs.push(docvec![
                ThreadingPlan::foldl_call_doc(
                    &lambda_var,
                    init_tuple_doc,
                    &safe_list_var,
                    &fold_result
                ),
                epilogue.bindings,
                extract_doc,
                repack_doc,
                tail,
            ]);
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);
        // BT-3827: a value-type body that writes a field carries `Self` in a
        // `{Acc, Self}` wrapper around the accumulator (only the ops
        // `SELF_THREADING_FOLDS` names; the rest have no slots).
        let slots = plan.fold_family_slots(self);
        let mut docs: Vec<Document<'static>> = vec![pack_doc, safe_list];
        docs.push(docvec![
            init_binding,
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
            slot_bindings,
            "let StateAcc = call 'erlang':'element'(",
            leaf::int_lit(state_index),
            ", ",
            leaf::var(acc_state_var.clone()),
            ") in ",
        ]);
        let body_doc =
            self.lower_map_fold_body(body, &plan, &body_kind, params, &acc_state_var, slot_count)?;
        docs.push(op.guard_decided(&acc_state_var, body_doc));

        let fold_result = self.fresh_temp_var(op.fold_result_prefix());
        let vars = op.epilogue_vars(self);
        let state_out = self.fresh_temp_var("StOut");
        let epilogue = op.epilogue(self, &fold_result, &vars, &str_check_var);

        let init_acc = tuple_init_doc(false, &slot_inits, leaf::var(init_state));
        let init_acc = match &slots {
            Some(s) => s.wrap_init(init_acc, self),
            None => init_acc,
        };
        let fold_pair = slots
            .as_ref()
            .map_or(fold_result.as_str(), FoldFamilySlots::result_var);
        let unwrap = slots
            .as_ref()
            .map_or(Document::Nil, |s| s.unwrap_result(&fold_result));
        let extract_doc = plan.generate_extract_suffix_doc(&state_out, self);
        let tail = self.fold_tail_doc(
            &op,
            epilogue.found_flag,
            epilogue.result,
            &state_out,
            slots.as_ref(),
        )?;
        docs.push(docvec![
            ThreadingPlan::foldl_call_doc(&lambda_var, init_acc, &safe_list_var, fold_pair),
            unwrap,
            epilogue.bindings,
            "let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(",
            leaf::int_lit(state_index),
            ", ",
            leaf::var(fold_result),
            ") in ",
            extract_doc,
            tail,
        ]);
        Ok(Document::Vec(docs))
    }

    /// What closes the fold: `{Result, State}` (with the `Self` slots after the
    /// fold appended in map mode), or `detect:ifNone:`'s closing `case`.
    fn fold_tail_doc(
        &mut self,
        op: &FoldOp<'_>,
        found_flag: Option<Document<'static>>,
        result: Document<'static>,
        state_var: &str,
        slots: Option<&FoldFamilySlots>,
    ) -> Result<Document<'static>> {
        if let (FoldOp::DetectIfNone { if_none }, Some(flag)) = (op, found_flag) {
            return self.if_none_case_doc(if_none, flag, result, state_var);
        }
        let result_open = docvec!["{", result, ", ", leaf::var(state_var.to_string())];
        Ok(match slots {
            Some(s) => s.close_tuple(result_open, self),
            None => docvec![result_open, "}"],
        })
    }

    /// `detect:ifNone:`'s closing `case`: the found item with the state, or
    /// the `ifNone:` arm threaded from `state_var`.
    fn if_none_case_doc(
        &mut self,
        if_none: &Expression,
        found_flag: Document<'static>,
        found_item: Document<'static>,
        state_var: &str,
    ) -> Result<Document<'static>> {
        let if_none_arm = self.generate_if_none_branch_tuple(if_none, state_var)?;
        // An explicit fallback: a `case` without one inside a `try` body
        // (a protected body, a handler) trips OTP's `beam_validator`
        // (`ambiguous_catch_try_state`, see `case_clause_fallback`).
        let no_match_fallback = self.case_clause_fallback("DetectNoMatch");
        Ok(docvec![
            "case ",
            found_flag,
            " of <'true'> when 'true' -> {",
            found_item,
            ", ",
            leaf::var(state_var.to_string()),
            "} <'false'> when 'true' -> ",
            if_none_arm,
            no_match_fallback,
            " end",
        ])
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
        // Widening the list also means the emitter wraps its accumulator with
        // `FoldFamilySlots`; the shared prologue gives every listed label a
        // `Self` slot, so a change here must be a reviewed edit.
        assert_eq!(
            SELF_THREADING_FOLDS,
            &["do:", "collect:", "inject:into:"][..]
        );
    }

    /// Each op's label is the selector, and exactly the ops
    /// `SELF_THREADING_FOLDS` names thread `Self`.
    #[test]
    fn fold_ops_use_their_selector_as_label() {
        let initial = Expression::Identifier(beamtalk_core::ast::Identifier::new(
            "x",
            beamtalk_core::source_analysis::Span::new(0, 1),
        ));
        let ops = [
            (FoldOp::Collect, "collect:", true),
            (FoldOp::Select, "select:", false),
            (FoldOp::Reject, "reject:", false),
            (FoldOp::Count, "count:", false),
            (FoldOp::FlatMap, "flatMap:", false),
            (FoldOp::TakeWhile, "takeWhile:", false),
            (FoldOp::DropWhile, "dropWhile:", false),
            (FoldOp::Partition, "partition:", false),
            (FoldOp::GroupBy, "groupBy:", false),
            (
                FoldOp::Inject {
                    initial: &initial,
                    guard: "eachWithIndex:",
                },
                "inject:into:",
                true,
            ),
        ];
        for (op, label, threads_self) in ops {
            assert_eq!(op.label(), label);
            assert_eq!(fold_threads_self(label), threads_self, "{label}");
        }
    }
}

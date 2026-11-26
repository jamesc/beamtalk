// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5117: the typed accumulator value of a fold lambda
//! ([`ThreadedStmt::FoldAccumulator`](super::ir::ThreadedStmt::FoldAccumulator)).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A fold kind that extends a result list (`select:`/`reject:`/`collect:`,
//! [`FoldResultList`](super::ir::FoldResultList)) builds its accumulator as
//! [`FoldAccValue`] data, so the verifier can walk each leaf arm and count the
//! conses into the result-list slot instead of trusting a declared flag. Emit
//! renders the same value, so the data the verifier reads is the text that is
//! generated. Every other kind keeps [`FoldAccBody::Opaque`].

use super::ir::FoldResultList;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, docvec, join, leaf};

/// The accumulator list a fold lambda binds from `element(FOLD_RESULT_LIST_SLOT, Acc)`;
/// the base every cons into the result-list slot must build on.
pub(in crate::core_erlang) const ACC_LIST: &str = "AccList";

/// `{head…, tail}` — an accumulator tuple whose last slot is the epilogue's
/// trailing state doc (`tail`). The one renderer every fold accumulator tuple uses.
pub(in crate::core_erlang) fn render_tuple(
    head: impl IntoIterator<Item = Document<'static>>,
    tail: &Document<'static>,
) -> Document<'static> {
    docvec![
        "{",
        join(head, &Document::Str(", ")),
        ", ",
        tail.clone(),
        "}"
    ]
}

/// One element of an accumulator tuple's head.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum AccSlot {
    /// `[item | list]` — `item` consed onto the accumulator list `list`
    /// (itself a cons when an element is added twice).
    Cons { item: String, list: Box<AccSlot> },
    /// A bare accumulator name (`AccList`, `MatchList`, …) carried unchanged.
    Name(&'static str),
}

impl AccSlot {
    /// `[item | list]` onto the named accumulator list.
    pub(in crate::core_erlang) fn cons(item: &str, list: &'static str) -> Self {
        Self::Cons {
            item: item.to_string(),
            list: Box::new(Self::Name(list)),
        }
    }

    /// The element consed last onto this slot, if it conses at all.
    fn head_item(&self) -> Option<&str> {
        match self {
            Self::Cons { item, .. } => Some(item),
            Self::Name(_) => None,
        }
    }

    /// The conses stacked on this slot, and the name they are consed onto.
    fn cons_depth(&self) -> (usize, &'static str) {
        match self {
            Self::Cons { list, .. } => {
                let (n, base) = list.cons_depth();
                (n + 1, base)
            }
            Self::Name(name) => (0, name),
        }
    }

    pub(in crate::core_erlang) fn render(&self) -> Document<'static> {
        match self {
            Self::Cons { item, list } => {
                docvec!["[", leaf::var(item.clone()), " | ", list.render(), "]"]
            }
            Self::Name(name) => Document::Str(name),
        }
    }
}

/// One `<'bool'> when 'true' -> body` arm of a predicate `case`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct BoolArm {
    pub(in crate::core_erlang) value: bool,
    pub(in crate::core_erlang) body: FoldAccBody,
}

/// The accumulator expression itself, down to its leaf tuples.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum FoldAccBody {
    /// `{head…, tail}` — `tail` is the repacked locals or the `StateAcc` map.
    Tuple {
        head: Vec<AccSlot>,
        tail: Document<'static>,
    },
    /// `case scrutinee of <'a'> when 'true' -> … <'b'> when 'true' -> … fallback end`.
    BoolCase {
        scrutinee: Document<'static>,
        /// BT-5190: the scrutinee is `not(<block answer>)` (`reject:`), so an
        /// arm taken on `true` is the one taken when the block answers `false`.
        negated: bool,
        arms: [Box<BoolArm>; 2],
        /// The `<V> when 'true' -> error({'case_clause', V})` clause, built
        /// once by the epilogue (it mints the `_PredBad` temp).
        fallback: Document<'static>,
    },
    /// Hand-built text the verifier cannot look inside.
    Opaque(Document<'static>),
}

/// A fold accumulator: optional verbatim `prelude` (the `let` bindings and
/// spacing that precede it) and the typed [`FoldAccBody`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct FoldAccValue {
    pub(in crate::core_erlang) prelude: Option<Document<'static>>,
    pub(in crate::core_erlang) body: FoldAccBody,
}

/// How a leaf arm treats the result-list slot, as the verifier counts it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum LeafConses {
    /// Conses into the result-list slot, and into any other slot.
    Tuple {
        slot_conses: usize,
        stray_slots: Vec<usize>,
        /// The head has no element at the result-list slot.
        slot_missing: bool,
        /// The list the result-list slot builds on (`None` when missing).
        slot_base: Option<&'static str>,
        /// BT-5190: the element consed last onto the result-list slot (the
        /// head of the list it builds), `None` when the slot conses nothing.
        slot_item: Option<String>,
    },
    /// The leaf is hand-built text.
    Opaque,
}

impl FoldAccValue {
    pub(in crate::core_erlang) fn opaque(doc: Document<'static>) -> Self {
        Self {
            prelude: None,
            body: FoldAccBody::Opaque(doc),
        }
    }

    pub(in crate::core_erlang) fn tuple(head: Vec<AccSlot>, tail: Document<'static>) -> Self {
        Self {
            prelude: None,
            body: FoldAccBody::Tuple { head, tail },
        }
    }

    pub(in crate::core_erlang) fn with_prelude(mut self, prelude: Document<'static>) -> Self {
        self.prelude = Some(prelude);
        self
    }

    /// The text emitted for this accumulator.
    pub(in crate::core_erlang) fn render(&self) -> Document<'static> {
        match &self.prelude {
            Some(prelude) => docvec![prelude.clone(), self.body.render()],
            None => self.body.render(),
        }
    }

    /// BT-5190: the result list this accumulator *builds*, read off its own
    /// shape rather than off the fold kind that asked for it, so the
    /// declaration and the kind are two independent witnesses
    /// (`verify_fold_result_list_declared` compares them). A tuple that conses
    /// the result-list slot is a map (`ExactlyOne`); a boolean `case` with one
    /// consing arm is a filter keeping on that arm's boolean (undone by
    /// `negated`). Anything else (no consing arm, both arms consing, hand-built
    /// text) declares nothing. An arm that conses more than once still counts
    /// as consing: the per-leaf count check reports that.
    pub(in crate::core_erlang) fn inferred_result_list(&self) -> Option<FoldResultList> {
        use super::ir::{ConsPerCall, FOLD_RESULT_LIST_SLOT};
        let slot = FOLD_RESULT_LIST_SLOT;
        let conses = |body: &FoldAccBody| match body {
            FoldAccBody::Tuple { head, .. } => head.get(slot - 1).map(|s| s.cons_depth().0 > 0),
            _ => None,
        };
        let cons = match &self.body {
            FoldAccBody::Tuple { .. } => conses(&self.body)?.then_some(ConsPerCall::ExactlyOne)?,
            FoldAccBody::BoolCase { arms, negated, .. } => {
                let [a, b] = arms;
                match (conses(&a.body)?, conses(&b.body)?) {
                    (true, false) => ConsPerCall::Filter {
                        keep_when: a.value != *negated,
                    },
                    (false, true) => ConsPerCall::Filter {
                        keep_when: b.value != *negated,
                    },
                    _ => return None,
                }
            }
            FoldAccBody::Opaque(_) => return None,
        };
        Some(FoldResultList { slot, cons })
    }

    /// The conses each leaf arm makes, with `slot` the 1-based result-list slot.
    pub(in crate::core_erlang) fn leaf_conses(&self, slot: usize) -> Vec<LeafConses> {
        let mut out = Vec::new();
        self.body.collect_leaves(slot, &mut out);
        out
    }
}

pub(in crate::core_erlang) fn bool_arm_clause(
    value: bool,
    body: Document<'static>,
) -> Document<'static> {
    form::clause(
        [leaf::atom(if value { "true" } else { "false" })],
        None,
        body,
        Layout::Flat,
    )
}

/// `case scrutinee of <'a'> when 'true' -> … <'b'> when 'true' -> … fallback end` — the
/// two-boolean-arm predicate `case`, arms in the order given (the generated
/// text depends on it). The one renderer both the typed [`FoldAccBody::BoolCase`]
/// and the opaque predicate epilogues build through.
pub(in crate::core_erlang) fn render_bool_case(
    scrutinee: Document<'static>,
    arms: [(bool, Document<'static>); 2],
    fallback: Document<'static>,
) -> Document<'static> {
    let [(first, first_body), (second, second_body)] = arms;
    form::case_of(
        scrutinee,
        [
            bool_arm_clause(first, first_body),
            bool_arm_clause(second, second_body),
            fallback,
        ],
        Layout::Flat,
    )
}

impl FoldAccBody {
    fn render(&self) -> Document<'static> {
        match self {
            Self::Tuple { head, tail } => render_tuple(head.iter().map(AccSlot::render), tail),
            Self::BoolCase {
                scrutinee,
                arms,
                fallback,
                ..
            } => render_bool_case(
                scrutinee.clone(),
                [
                    (arms[0].value, arms[0].body.render()),
                    (arms[1].value, arms[1].body.render()),
                ],
                fallback.clone(),
            ),
            Self::Opaque(doc) => doc.clone(),
        }
    }

    fn collect_leaves(&self, slot: usize, out: &mut Vec<LeafConses>) {
        match self {
            Self::Tuple { head, .. } => {
                let mut slot_conses = 0;
                let mut slot_missing = true;
                let mut slot_base = None;
                let mut slot_item = None;
                let mut stray_slots = Vec::new();
                for (i, s) in head.iter().enumerate() {
                    let (conses, base) = s.cons_depth();
                    if i + 1 == slot {
                        slot_conses = conses;
                        slot_base = Some(base);
                        slot_item = s.head_item().map(str::to_string);
                        slot_missing = false;
                    } else if conses > 0 {
                        stray_slots.push(i + 1);
                    }
                }
                out.push(LeafConses::Tuple {
                    slot_conses,
                    stray_slots,
                    slot_missing,
                    slot_base,
                    slot_item,
                });
            }
            Self::BoolCase { arms, .. } => {
                for arm in arms {
                    arm.body.collect_leaves(slot, out);
                }
            }
            Self::Opaque(_) => out.push(LeafConses::Opaque),
        }
    }
}

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
    },
    /// The leaf is hand-built text.
    Opaque,
}

impl From<Document<'static>> for FoldAccValue {
    fn from(doc: Document<'static>) -> Self {
        Self::opaque(doc)
    }
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

impl FoldAccBody {
    fn render(&self) -> Document<'static> {
        match self {
            Self::Tuple { head, tail } => render_tuple(head.iter().map(AccSlot::render), tail),
            Self::BoolCase {
                scrutinee,
                arms,
                fallback,
            } => form::case_of(
                scrutinee.clone(),
                [
                    bool_arm_clause(arms[0].value, arms[0].body.render()),
                    bool_arm_clause(arms[1].value, arms[1].body.render()),
                    fallback.clone(),
                ],
                Layout::Flat,
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
                let mut stray_slots = Vec::new();
                for (i, s) in head.iter().enumerate() {
                    let (conses, base) = s.cons_depth();
                    if i + 1 == slot {
                        slot_conses = conses;
                        slot_base = Some(base);
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

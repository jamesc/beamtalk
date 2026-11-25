// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Comments written inside collection literals (BT-3983).
//!
//! **DDD Context:** Source Analysis
//!
//! The four collection literals (`{...}`, `#(...)`, `#[...]`, `#{...}`) have
//! no field for comments, and adding one to four `Expression` variants would
//! touch every construction site. The parser instead records the comments of
//! each literal that has any in a side table, keyed by the literal's span, the
//! way [`PipelineSends`](super::PipelineSends) records `|>`. The table lives on
//! the definition that owns the code ([`MethodDefinition::literal_comments`]
//! for a method body, [`Module::literal_comments`] for everything else), and
//! the unparser installs it for the duration of that definition's unparse.
//!
//! A literal with no comments has no entry. Spans in one definition are
//! unique and survive normalisation and cloning, so the entry still matches
//! after the operand-paren peel and restore.
//!
//! Slots: one [`CommentAttachment`] per element (`items`, in source order; a
//! cons tail `#(a | t)` is the slot after the last element), plus the comments
//! written after the last item and before the closing delimiter (`closing`).
//! An item's `leading` comments are on their own lines before it; its
//! `trailing` comment is the end-of-line comment after the item or its comma.
//!
//! # Gap comments (BT-5183)
//!
//! The table also records comments written *between* the tokens of an
//! expression, where no AST node has a comment field: before a keyword part
//! (`foo: 1 // why` / `bar: 2`), after a cascade `;`, around a binary
//! operator, just inside a parenthesis, between a block's parameters and its
//! `|`, on their own lines before a block's `]`, and after `:=` or `^`. Each gap is keyed by an
//! anchor offset plus a [`GapSlot`] naming which gap of which node it is, so
//! the printer can put the comments back where they were written.
//!
//! # Number spellings (BT-5183)
//!
//! The same per-definition table also records the source spelling of every
//! number literal (`16rFF`, `1e10`, `1_000`), keyed by the literal token's
//! span. The AST holds only the value (`Literal::Integer(255)`), so without
//! this the formatter would rewrite `16rFF` as `255`. Spellings ride on this
//! table rather than a new field because they need exactly its lifecycle:
//! recorded by the parser, owned by the definition, installed by the
//! unparser for that definition's code, and narrowed by
//! [`LiteralComments::within`] for a rebuilt definition.

use std::collections::BTreeMap;

use super::span_table::{entries_within, span_key as key};
use ecow::EcoString;

use super::{Comment, CommentAttachment};
use crate::span::Span;

/// The comments of one collection literal. See the module docs for the slots.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct LiteralCommentSlots {
    /// One attachment per element (then the cons tail, if any).
    pub items: Vec<CommentAttachment>,
    /// Comments after the last item, before the closing delimiter.
    pub closing: Vec<Comment>,
}

impl LiteralCommentSlots {
    /// `true` if no slot holds a comment.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.closing.is_empty()
            && self
                .items
                .iter()
                .all(|c| c.leading.is_empty() && c.trailing.is_none())
    }
}

/// Which gap between an expression's tokens a [`GapComments`] was written in
/// (BT-5183). Each names the node its anchor offset is taken from.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GapSlot {
    /// Before a keyword part of a send: anchored at the keyword's start.
    BeforeKeyword,
    /// Around the `;` before a cascade message: anchored at the message's
    /// start.
    BeforeCascadeMessage,
    /// Around a binary operator: anchored at its argument's start.
    AroundBinaryOperator,
    /// Just after a `(`: anchored at the parenthesised expression's start.
    AfterOpenParen,
    /// Just before a `)`: anchored at the parenthesised expression's end.
    BeforeCloseParen,
    /// Between a block's last parameter and its `|`: anchored at the block's
    /// start.
    BeforeBlockBar,
    /// Before a block's `]`, after its last statement: anchored at the
    /// block's end.
    BeforeBlockClose,
    /// After a `:=` or `^`, before its value: anchored at the value's start.
    BeforeValue,
}

/// The comments written in one gap between two tokens (BT-5183).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GapComments {
    /// Comments on the line of the token before the gap, after it.
    pub same_line: Vec<Comment>,
    /// Comments on their own lines, before the token after the gap.
    pub own_line: Vec<Comment>,
}

impl GapComments {
    /// `true` if the gap holds no comment.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.same_line.is_empty() && self.own_line.is_empty()
    }

    /// Appends `other`'s comments after this gap's: its same-line comments
    /// follow this gap's own-line ones when this gap already broke the line.
    pub fn extend(&mut self, other: Self) {
        if self.own_line.is_empty() {
            self.same_line.extend(other.same_line);
        } else {
            self.own_line.extend(other.same_line);
        }
        self.own_line.extend(other.own_line);
    }
}

/// The comments inside the collection literals of one definition (BT-3983),
/// the comments in the gaps between its expressions' tokens, and the source
/// spellings of its number literals (BT-5183).
///
/// Keyed by span. Ordered by source position, so its `Debug` form is
/// deterministic. The `Debug` form shows only the comments (gap comments in
/// anchor order, without the offsets, which are source positions): a
/// spelling, like
/// a span, records how the source was written, not what it means, so two
/// trees that differ only in layout (the printer may reorder
/// `ifFalse:ifTrue:`) still print alike. `format_source` checks spellings
/// separately, from the lexer's tokens.
#[derive(Clone, Default, PartialEq)]
pub struct LiteralComments {
    literals: BTreeMap<(u32, u32), LiteralCommentSlots>,
    spellings: BTreeMap<(u32, u32), EcoString>,
    /// Keyed `(anchor, anchor)` so [`entries_within`] narrows it like the
    /// span-keyed tables.
    gaps: BTreeMap<(u32, u32), Vec<(GapSlot, GapComments)>>,
}

impl std::fmt::Debug for LiteralComments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(
                self.literals
                    .iter()
                    .map(|(&(start, end), slots)| (Span::new(start, end), slots)),
            )
            .entries(self.gaps.values().flatten().map(|(slot, gap)| (slot, gap)))
            .finish()
    }
}

impl LiteralComments {
    /// `true` if no literal has a comment. Recorded number spellings do not
    /// count: they are not comments.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.literals.is_empty()
    }

    /// The number of literals with comments (number spellings do not count).
    #[must_use]
    pub fn len(&self) -> usize {
        self.literals.len()
    }

    /// Records the comments of the literal spanning `span`. Does nothing if
    /// `slots` holds no comment.
    pub fn record(&mut self, span: Span, slots: LiteralCommentSlots) {
        if !slots.is_empty() {
            self.literals.insert(key(span), slots);
        }
    }

    /// The comments of the literal spanning `span`, if it has any.
    #[must_use]
    pub fn get(&self, span: Span) -> Option<&LiteralCommentSlots> {
        self.literals.get(&key(span))
    }

    /// Records the source spelling of the number literal token at `span`
    /// (BT-5183).
    pub fn record_spelling(&mut self, span: Span, spelling: EcoString) {
        self.spellings.insert(key(span), spelling);
    }

    /// The source spelling of the number literal token at `span`, if one was
    /// recorded.
    #[must_use]
    pub fn spelling(&self, span: Span) -> Option<&EcoString> {
        self.spellings.get(&key(span))
    }

    /// The recorded number spellings that lie inside `span`, in source order.
    #[must_use]
    pub fn spellings_within(&self, span: Span) -> Vec<EcoString> {
        entries_within(&self.spellings, span)
            .into_values()
            .collect()
    }

    /// Records the comments of the gap `slot` anchored at `anchor`. Does
    /// nothing if `gap` holds no comment.
    pub fn record_gap(&mut self, anchor: u32, slot: GapSlot, gap: GapComments) {
        if gap.is_empty() {
            return;
        }
        let entry = self.gaps.entry((anchor, anchor)).or_default();
        match entry.iter_mut().find(|(s, _)| *s == slot) {
            Some((_, existing)) => existing.extend(gap),
            None => entry.push((slot, gap)),
        }
    }

    /// The comments of the gap `slot` anchored at `anchor`, if it has any.
    #[must_use]
    pub fn gap(&self, anchor: u32, slot: GapSlot) -> Option<&GapComments> {
        self.gaps
            .get(&(anchor, anchor))?
            .iter()
            .find(|(s, _)| *s == slot)
            .map(|(_, gap)| gap)
    }

    /// Builds the table of one definition from the parser's records.
    pub fn from_records(
        comments: impl IntoIterator<Item = (Span, LiteralCommentSlots)>,
        spellings: impl IntoIterator<Item = (Span, EcoString)>,
        gaps: impl IntoIterator<Item = (u32, GapSlot, GapComments)>,
    ) -> Self {
        let mut out: Self = comments.into_iter().collect();
        for (span, spelling) in spellings {
            out.record_spelling(span, spelling);
        }
        for (anchor, slot, gap) in gaps {
            out.record_gap(anchor, slot, gap);
        }
        out
    }

    /// The entries whose literals lie inside `span`.
    ///
    /// For a definition rebuilt from part of another one's code, such as a
    /// `ClassBuilder` block method (`#sel => [:self | …]`): the block's
    /// literals were recorded on the enclosing definition, so the rebuilt one
    /// takes the entries inside the block (BT-4708).
    #[must_use]
    pub fn within(&self, span: Span) -> Self {
        Self {
            literals: entries_within(&self.literals, span),
            spellings: entries_within(&self.spellings, span),
            gaps: entries_within(&self.gaps, span),
        }
    }
}

impl FromIterator<(Span, LiteralCommentSlots)> for LiteralComments {
    fn from_iter<I: IntoIterator<Item = (Span, LiteralCommentSlots)>>(iter: I) -> Self {
        let mut out = Self::default();
        for (span, slots) in iter {
            out.record(span, slots);
        }
        out
    }
}

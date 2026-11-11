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

use std::collections::BTreeMap;

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

/// The comments inside the collection literals of one definition (BT-3983).
///
/// Keyed by the literal's span. Ordered by source position, so its `Debug`
/// form is deterministic.
#[derive(Clone, Default, PartialEq)]
pub struct LiteralComments {
    literals: BTreeMap<(u32, u32), LiteralCommentSlots>,
}

impl std::fmt::Debug for LiteralComments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(
                self.literals
                    .iter()
                    .map(|(&(start, end), slots)| (Span::new(start, end), slots)),
            )
            .finish()
    }
}

fn key(span: Span) -> (u32, u32) {
    (span.start(), span.end())
}

impl LiteralComments {
    /// `true` if no literal has a comment.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.literals.is_empty()
    }

    /// The number of literals with comments.
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

    /// The recorded literal spans, in source order.
    pub fn spans(&self) -> impl Iterator<Item = Span> + '_ {
        self.literals
            .keys()
            .map(|&(start, end)| Span::new(start, end))
    }

    /// The entries whose literals lie inside `span`.
    #[must_use]
    pub fn within(&self, span: Span) -> Self {
        Self {
            literals: self
                .literals
                .range((span.start(), span.start())..=(span.end(), span.end()))
                .filter(|&(&(_, end), _)| end <= span.end())
                .map(|(&key, slots)| (key, slots.clone()))
                .collect(),
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

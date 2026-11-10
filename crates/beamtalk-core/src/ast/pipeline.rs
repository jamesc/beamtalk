// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `|>` re-sugaring record (ADR 0145 § AST representation).
//!
//! **DDD Context:** Source Analysis
//!
//! There is no pipeline node in the tree. The parser builds
//! `recv m1 |> m2 |> m3` directly as the left-nested sends it means,
//! `((recv m1) m2) m3`, so analysis, codegen and `ThreadedIr` see ordinary
//! sends. What the tree cannot say is that the author wrote `|>`. The parser
//! therefore records, for each send whose message followed a `|>` token, that
//! send's span. The unparser prints `|>` before each recorded send, so
//! `beamtalk fmt` and a method's reflected source show the pipeline as
//! written.
//!
//! The record lives on the definition that owns the code: on
//! [`MethodDefinition::pipeline_sends`](super::MethodDefinition) for a method
//! body, and on [`Module::pipeline_sends`](super::Module) for everything
//! outside a method (top-level and REPL expressions, state and class-variable
//! initialisers). Every `unparse_method` / `unparse_module` call sees it
//! without a caller passing it along.
//!
//! Sends are keyed by span. A synthesised send gets the hand-nested span
//! (from its receiver's start to its message's end), so spans in one
//! definition are unique, and normalisation and cloning keep them: the record
//! still matches after the operand-paren peel and restore.
//!
//! # Comments between stages
//!
//! A comment written between two stages attaches to the record entry of the
//! send whose `|>` follows it: an end-of-line comment after the previous
//! stage is that entry's `trailing` comment, and comments on their own lines
//! before the `|>` are its `leading` comments. So in
//!
//! ```text
//! total := orders select: [:o | o isPaid]   // paid only
//!   // sum the amounts
//!   |> collect: [:o | o amount]
//!   |> sum
//! ```
//!
//! both comments belong to the `collect:` send's entry: they sit between the
//! `select:` stage and the `|>` that starts `collect:`. An end-of-line
//! comment after a trailing `|>` (`x foo |>  // why`, then the stage on the
//! next line) also belongs to that entry, as its last leading comment, so it
//! is printed before the `|>` on its own line. A comment after the last stage
//! is the statement's own trailing comment, as before.

use std::collections::BTreeMap;

use super::CommentAttachment;
use crate::span::Span;

/// The sends of one definition that were written with `|>` (ADR 0145).
///
/// Keyed by the send's span; each entry holds the comments written between
/// the previous stage and the `|>` (see the module docs). Empty for code with
/// no pipeline. Ordered by source position, so its `Debug` form (which tree
/// comparisons in tests read) is deterministic; that form shows each key as a
/// [`Span`], like every other position in the tree.
#[derive(Clone, Default, PartialEq)]
pub struct PipelineSends {
    sends: BTreeMap<(u32, u32), CommentAttachment>,
}

impl std::fmt::Debug for PipelineSends {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(
                self.sends
                    .iter()
                    .map(|(&(start, end), comments)| (Span::new(start, end), comments)),
            )
            .finish()
    }
}

fn key(span: Span) -> (u32, u32) {
    (span.start(), span.end())
}

impl PipelineSends {
    /// `true` if no send was written with `|>`.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.sends.is_empty()
    }

    /// The number of recorded `|>` sends.
    #[must_use]
    pub fn len(&self) -> usize {
        self.sends.len()
    }

    /// Records that the send spanning `span` was written after a `|>`, with
    /// the comments written between the previous stage and that `|>`.
    pub fn record(&mut self, span: Span, comments: CommentAttachment) {
        self.sends.insert(key(span), comments);
    }

    /// `true` if the send spanning `span` was written after a `|>`.
    #[must_use]
    pub fn contains(&self, span: Span) -> bool {
        self.sends.contains_key(&key(span))
    }

    /// The comments written before the `|>` of the send spanning `span`, or
    /// `None` if that send was not written with `|>`.
    #[must_use]
    pub fn comments(&self, span: Span) -> Option<&CommentAttachment> {
        self.sends.get(&key(span))
    }

    /// The recorded send spans, in source order.
    pub fn spans(&self) -> impl Iterator<Item = Span> + '_ {
        self.sends.keys().map(|&(start, end)| Span::new(start, end))
    }
}

impl FromIterator<(Span, CommentAttachment)> for PipelineSends {
    fn from_iter<I: IntoIterator<Item = (Span, CommentAttachment)>>(iter: I) -> Self {
        Self {
            sends: iter
                .into_iter()
                .map(|(span, comments)| (key(span), comments))
                .collect(),
        }
    }
}

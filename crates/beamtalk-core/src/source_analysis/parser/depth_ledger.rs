// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The depths of the trees already measured by nested parse levels, so each
//! level's depth check walks only the nodes it built itself (BT-5237).
//!
//! The tree-depth bound of BT-5181 is checked on the result of every nesting
//! level. Measured from scratch, each check walked every subtree again once
//! per level it sits inside — O(n × depth) per parse. Instead, each level
//! records the depth it measured here, and the enclosing level's walk takes
//! that depth when it reaches the subtree rather than walking it again.
//!
//! A level's subtrees are recognised by their span and node kind. The parser
//! never backtracks, so the trees answered by the levels directly inside one
//! level cover disjoint, consecutive runs of tokens; and every node built
//! above such a tree adds a token (a selector, operator, bracket or `:=`), so
//! no other node in the enclosing tree has the same span and kind. Two type
//! constructs rearrange a nested level's answer, and the parser records them
//! explicitly: grouping parentheses widen its span ([`DepthLedger::respan_last`])
//! and a grouped union is spliced into the union around it, so a union's
//! members are recorded alongside it ([`DepthLedger::close_with`]).
//!
//! The parser's own unit tests hold every recorded depth equal to a full
//! walk (`Parser::bound_expression_depth`) over every `.bt` file in the
//! repository and generated input; `nesting_guard_tests` hold the visits to
//! a small multiple of the token count.

use std::mem::Discriminant;

use crate::source_analysis::Span;

/// One measured tree: its span, its root's kind, and its depth.
struct Known<T> {
    start: u32,
    end: u32,
    kind: Discriminant<T>,
    depth: usize,
}

/// The depths measured by the nesting levels open around the parser, as one
/// stack: each open level owns the entries from its [`Self::open`] mark up.
pub(super) struct DepthLedger<T> {
    entries: Vec<Known<T>>,
}

impl<T> Default for DepthLedger<T> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<T> DepthLedger<T> {
    /// Opens a level: the entries recorded from here until it is
    /// [`Self::close`]d are the trees of the levels directly inside it.
    pub(super) fn open(&self) -> usize {
        self.entries.len()
    }

    /// The entries of the level opened at `mark`, sorted for [`Frame::depth_of`].
    pub(super) fn frame(&mut self, mark: usize) -> Frame<'_, T> {
        let entries = &mut self.entries[mark..];
        entries.sort_unstable_by_key(|known| (known.start, known.end));
        Frame { entries }
    }

    /// Closes the level opened at `mark`, recording the depth of the tree it
    /// answers (span `span`, root kind `kind`) for the level around it.
    pub(super) fn close(&mut self, mark: usize, span: Span, kind: Discriminant<T>, depth: usize) {
        self.close_with(mark, Vec::new(), span, kind, depth);
    }

    /// [`Self::close`], also recording `parts` — subtrees of the answer that
    /// the level around may find in place of the answer itself (the members
    /// of a union, which a grouped union is spliced into). The answer is
    /// recorded last, so [`Self::respan_last`] finds it.
    pub(super) fn close_with(
        &mut self,
        mark: usize,
        parts: Vec<(Span, Discriminant<T>, usize)>,
        span: Span,
        kind: Discriminant<T>,
        depth: usize,
    ) {
        self.entries.truncate(mark);
        for (span, kind, depth) in parts.into_iter().chain([(span, kind, depth)]) {
            self.entries.push(Known {
                start: span.start(),
                end: span.end(),
                kind,
                depth,
            });
        }
    }

    /// Moves the most recently recorded tree from span `from` to span `to`,
    /// for a caller that widens the span of the tree a nested level answered
    /// (grouping parentheses in a type). Does nothing when that tree was not
    /// recorded — the level reported "nesting is too deep" instead.
    pub(super) fn respan_last(&mut self, from: Span, to: Span) {
        if let Some(last) = self.entries.last_mut()
            && (last.start, last.end) == (from.start(), from.end())
        {
            last.start = to.start();
            last.end = to.end();
        }
    }
}

/// The measured trees of one level, sorted by span.
pub(super) struct Frame<'a, T> {
    entries: &'a [Known<T>],
}

impl<T> Frame<'_, T> {
    /// The depth of the measured tree with this span and root kind, if any.
    pub(super) fn depth_of(&self, span: Span, kind: Discriminant<T>) -> Option<usize> {
        let key = (span.start(), span.end());
        let from = self
            .entries
            .partition_point(|known| (known.start, known.end) < key);
        self.entries[from..]
            .iter()
            .take_while(|known| (known.start, known.end) == key)
            .filter(|known| known.kind == kind)
            .map(|known| known.depth)
            .max()
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Selector-rename span queries — exact byte spans for `renameSelector:to:`.
//!
//! **DDD Context:** Language Service / Compilation (shared leaf, per
//! `docs/development/architecture-principles.md` §6)
//!
//! Backs `Behaviour>>renameSelector:to:` (ADR 0114). Given a
//! specific `self`/`super` message send (or a method's own definition), a
//! whole-method span (`resolve_method_span`) is too coarse to splice — it
//! would let a caller corrupt the rest of the method body — and a plain
//! text/regex search is unsafe for a multi-keyword selector like `at:put:`
//! (arguments between keyword parts can contain arbitrary nested
//! expressions, including sends that coincidentally reuse the same keyword
//! text). These two queries resolve the exact selector-token span(s) via an
//! AST walk instead.
//!
//! # Implementation
//!
//! The implementation lives in [`beamtalk_core::method_source_walker`], the same
//! shared leaf module [`super::all_sends_query`] re-exports from — see that
//! module's own doc for why this lives below both consuming contexts.

pub use beamtalk_core::method_source_walker::{
    SelectorSendSpan, find_definition_selector_spans, find_selector_send_spans,
};

use beamtalk_core::ast::{Expression, ExpressionStatement, Literal, MessageSelector};
use beamtalk_core::ast_walker::{SendSite, walk_sends};
use beamtalk_core::method_source_walker::first_token_span_after;
use beamtalk_core::source_analysis::Span;

/// How a send's receiver is written, as far as a static rename can tell
/// (ADR 0114's `recv_kind`, minus the FFI case which is just `Other` here).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SendReceiver {
    /// The receiver is `self` (parentheses looked through).
    SelfReceiver,
    /// The receiver is a bare `super`.
    SuperReceiver,
    /// Any other receiver expression.
    Other,
}

/// One send of the target selector inside a method body, with the exact
/// selector-token splice spans (one per keyword part).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorSendSite {
    /// How the receiver is written.
    pub receiver: SendReceiver,
    /// Selector-token spans to splice, in source order.
    pub spans: Vec<SelectorSendSpan>,
}

/// Every send of `old_selector` in `body` (a method body or top-level
/// statements; cascade messages included), with its selector-token spans for a rename to `new_selector`.
///
/// The all-receivers counterpart of [`find_selector_send_spans`], which only
/// finds `self`/`super` sends in a method's *source text*. A static rename
/// has the parsed module and the full file `source` already, so it walks the
/// real AST (spans are absolute) and classifies each send by receiver. The
/// caller decides which are confirmed (ADR 0114) and which are candidates.
///
/// A send whose keyword arity differs from `new_selector`'s is skipped.
#[must_use]
pub fn find_selector_send_sites(
    body: &[ExpressionStatement],
    source: &str,
    old_selector: &str,
    new_selector: &str,
) -> Vec<SelectorSendSite> {
    find_selector_send_sites_in(
        body.iter().map(|s| &s.expression),
        source,
        old_selector,
        new_selector,
    )
}

/// [`find_selector_send_sites`] over arbitrary root expressions (state and
/// class-variable defaults are single expressions, not statement lists).
#[must_use]
pub fn find_selector_send_sites_in<'a>(
    roots: impl IntoIterator<Item = &'a Expression>,
    source: &str,
    old_selector: &str,
    new_selector: &str,
) -> Vec<SelectorSendSite> {
    let new_parts: Vec<String> = new_selector
        .split(':')
        .filter(|part| !part.is_empty())
        .map(|part| format!("{part}:"))
        .collect();
    let mut sites = Vec::new();
    for root in roots {
        walk_sends(root, &mut |send| {
            if send.selector.name() != old_selector {
                return;
            }
            let receiver = if send.receiver.is_self() {
                SendReceiver::SelfReceiver
            } else if send.receiver.is_super() {
                SendReceiver::SuperReceiver
            } else {
                SendReceiver::Other
            };
            let spans = match send.selector {
                MessageSelector::Keyword(parts) if parts.len() == new_parts.len() => parts
                    .iter()
                    .zip(&new_parts)
                    .map(|(part, text)| SelectorSendSpan {
                        span: part.span,
                        new_text: text.clone(),
                    })
                    .collect(),
                MessageSelector::Keyword(_) => return,
                MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                    // A later cascade message has no receiver of its own to
                    // anchor on; its span starts at the selector token.
                    let anchor = if send.site == SendSite::CascadeLater {
                        send.span.start()
                    } else {
                        send.receiver.span().end()
                    };
                    let Some(span) = first_token_span_after(source, anchor) else {
                        return;
                    };
                    vec![SelectorSendSpan {
                        span,
                        new_text: new_selector.to_string(),
                    }]
                }
            };
            sites.push(SelectorSendSite { receiver, spans });
        });
    }
    sites
}

/// Spans of every symbol literal `#old_selector` under `roots`.
///
/// A symbol equal to a selector is a dynamic use of it (`perform: #go`,
/// `respondsTo: #go`) or an unrelated atom; either way a static rename cannot
/// prove it names the renamed method, so callers report these as candidates.
/// Only literals whose source text is exactly `#old_selector` are returned,
/// so a quoted or otherwise respelled symbol is never spliced blindly.
#[must_use]
pub fn find_symbol_literal_sites<'a>(
    roots: impl IntoIterator<Item = &'a Expression>,
    source: &str,
    old_selector: &str,
) -> Vec<Span> {
    let spelled = format!("#{old_selector}");
    let mut out = Vec::new();
    for root in roots {
        beamtalk_core::ast_walker::walk_expression(root, &mut |expr| {
            if let Expression::Literal(Literal::Symbol(name), span) = expr
                && name.as_str() == old_selector
                && source.get(span.start() as usize..span.end() as usize) == Some(spelled.as_str())
            {
                out.push(*span);
            }
        });
    }
    out
}

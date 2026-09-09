// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source-level AST walkers for per-method send and class-reference extraction.
//!
//! **DDD Context:** Compilation (shared leaf below both Language Service and
//! Code Generation)
//!
//! This module provides the pure source-analysis functions that walk a single
//! method's source text and collect every message send ([`find_all_sends_in_source`])
//! or every class reference ([`find_all_references_in_source`]). Both functions
//! are needed by two DDD contexts:
//!
//! - **Language Service** (`queries::all_sends_query`, `queries::references_to_query`)
//!   for `SystemNavigation unimplementedSelectors` / `referencesTo:` queries.
//! - **Code Generation** (`codegen::core_erlang::gen_server::methods`)
//!   to bake per-method xref data into `register_class/0` (ADR 0087 Phase 2).
//!
//! Placing both here (below both contexts) lets each import the shared leaf
//! without introducing a codegen→Language-Service dependency
//! (forbidden by `docs/development/architecture-principles.md` §1) or a
//! Language-Service→codegen dependency. See §6 (Shared-Leaf-Module Pattern)
//! of that document for the extraction rationale.
//!
//! ## Parsing strategy
//!
//! `CompiledMethod source` returns a bare method definition (signature + body,
//! no class header), which the parser does not accept at the top level. The
//! input is wrapped in a synthetic class definition before lexing/parsing, and
//! line numbers are translated back to the input's coordinate space by
//! subtracting the prefix line count. Callers treat parse errors as empty
//! results — any sub-trees that parsed successfully still contribute results.

use crate::ast::{
    CascadeMessage, Expression, MessageSelector, MethodDefinition, Pattern, StringSegment,
    TypeAnnotation,
};
use crate::source_analysis::{MethodSide, Span, SpanResolveError, lex_with_eof, parse};

// ---------------------------------------------------------------------------
// selector_span helper (previously in `queries/mod.rs`)
// ---------------------------------------------------------------------------

/// Returns the source span covering a keyword selector's keyword tokens, if any.
///
/// For a keyword selector with at least one part, merges the span of the first
/// and last keyword tokens. Returns `None` for unary and binary selectors.
///
/// Used by `beamtalk-language-service`'s `queries` siblings (`senders_query`,
/// `all_sends_query`, `announce_sites_query`, `ffi_sites_query`) and by the
/// send-walker in this module so the single implementation can be shared
/// without reaching upward into `queries`.
///
/// BT-3361: widened from `pub(crate)` to `pub` — no longer reachable at
/// `pub(crate)` visibility now that `queries` lives in its own crate.
pub fn selector_span(selector: &MessageSelector) -> Option<Span> {
    match selector {
        MessageSelector::Keyword(parts) => {
            let first = parts.first()?.span;
            let last = parts.last()?.span;
            Some(first.merge(last))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Send extraction
// ---------------------------------------------------------------------------

const SENDS_PREFIX_LINES: u32 = 1;
const SENDS_SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticAllSendsScope\n";

/// The syntactic kind of receiver a message send was directed at.
///
/// Used by the typo-finder's exclusions:
/// - `self` / `super` sends are suppressed in classes that define
///   `doesNotUnderstand:` (those classes interpret arbitrary selectors).
/// - `ErlangFfi` sends — `Erlang module …` and `(Erlang module) selector: …` —
///   are Erlang function invocations through the `ErlangModule` DNU bridge, not
///   Beamtalk message sends, so their "selectors" (module names and FFI
///   function names) must never be checked against the Beamtalk defined-set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReceiverKind {
    /// The receiver is the `self` pseudo-variable.
    SelfReceiver,
    /// The receiver is the `super` pseudo-variable.
    SuperReceiver,
    /// The receiver is the `Erlang` FFI bridge (`Erlang foo` or
    /// `(Erlang foo) bar:`). These are Erlang calls, not Beamtalk sends.
    ErlangFfi,
    /// Any other receiver (a literal, identifier, class reference, send, …).
    Other,
}

/// A single message send discovered while walking a method's AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SendHit {
    /// The selector name (without a leading `#`). Keyword selectors are
    /// concatenated, e.g. `at:put:`.
    pub selector: String,
    /// 1-based line number within the input source where the send appears.
    pub line: u32,
    /// The syntactic kind of receiver the message was sent to.
    pub receiver: ReceiverKind,
    /// For [`ReceiverKind::ErlangFfi`] sends, the resolved native (Erlang)
    /// module name the call targets — the `M` in `(Erlang M) fun: …` or the
    /// module-name send `Erlang M`. `None` for every non-FFI send and for an
    /// FFI chain whose module receiver is not a static `Erlang <module>` form.
    /// Backs the reverse "callers of a native module" index (BT-2669).
    pub target_module: Option<String>,
}

/// Find every message send within `method_source`.
///
/// Walks the parsed AST for [`Expression::MessageSend`] and
/// [`Expression::Cascade`] nodes and emits one [`SendHit`] per send, recording
/// the selector name, the 1-based line number (relative to the input), and the
/// receiver kind. A method that sends the same selector multiple times produces
/// one entry per occurrence. Hits follow a stable pre-order walk — an outer
/// send is recorded before the sends in its receiver/argument subtrees, so the
/// order is not strictly left-to-right by source position.
///
/// Returns an empty vector if the source contains no sends or cannot be parsed
/// at all.
#[must_use]
pub fn find_all_sends_in_source(method_source: &str) -> Vec<SendHit> {
    let wrapped = format!("{SENDS_SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut hits = Vec::new();

    crate::ast_walker::for_each_expr_seq(&module, |seq| {
        for stmt in seq {
            collect_sends(&stmt.expression, &wrapped, &mut hits);
        }
    });

    for hit in &mut hits {
        hit.line = hit.line.saturating_sub(SENDS_PREFIX_LINES).max(1);
    }
    hits
}

fn self_or_super_kind(receiver: &Expression) -> Option<ReceiverKind> {
    match receiver {
        Expression::Super(..) => Some(ReceiverKind::SuperReceiver),
        Expression::Identifier(ident) if ident.name == "self" => Some(ReceiverKind::SelfReceiver),
        _ => None,
    }
}

fn receiver_kind(receiver: &Expression, selector: &MessageSelector) -> ReceiverKind {
    if let Some(kind) = self_or_super_kind(receiver) {
        return kind;
    }
    if ffi_target_module(receiver, selector).is_some() {
        return ReceiverKind::ErlangFfi;
    }
    ReceiverKind::Other
}

fn ffi_target_module(receiver: &Expression, selector: &MessageSelector) -> Option<String> {
    if crate::ffi_receiver::is_erlang_class_reference(receiver) {
        if let MessageSelector::Unary(module_name) = selector {
            if !crate::ffi_receiver::is_class_protocol_selector(module_name) {
                return Some(module_name.to_string());
            }
        }
        return None;
    }
    crate::ffi_receiver::erlang_module_of_receiver(receiver).map(ToString::to_string)
}

fn cascade_receiver_kind(receiver: &Expression) -> ReceiverKind {
    if let Expression::MessageSend {
        receiver: inner,
        selector,
        ..
    } = receiver
    {
        return receiver_kind(inner, selector);
    }
    if let Some(kind) = self_or_super_kind(receiver) {
        return kind;
    }
    if crate::ffi_receiver::erlang_module_of_receiver(receiver).is_some() {
        return ReceiverKind::ErlangFfi;
    }
    ReceiverKind::Other
}

fn cascade_shared_ffi_module(receiver: &Expression) -> Option<String> {
    match receiver {
        Expression::MessageSend {
            receiver: inner,
            selector,
            ..
        } => ffi_target_module(inner, selector),
        other => crate::ffi_receiver::erlang_module_of_receiver(other).map(ToString::to_string),
    }
}

fn receiver_rooted_in_error(receiver: &Expression) -> bool {
    match receiver {
        Expression::Error { .. } => true,
        Expression::MessageSend {
            receiver: inner, ..
        }
        | Expression::Cascade {
            receiver: inner, ..
        }
        | Expression::FieldAccess {
            receiver: inner, ..
        } => receiver_rooted_in_error(inner),
        Expression::Parenthesized { expression, .. } => receiver_rooted_in_error(expression),
        _ => false,
    }
}

fn push_send(
    selector: &MessageSelector,
    span: Span,
    receiver: ReceiverKind,
    target_module: Option<String>,
    source: &str,
    hits: &mut Vec<SendHit>,
) {
    hits.push(SendHit {
        selector: selector.name().to_string(),
        line: selector_line(selector, span, source),
        receiver,
        target_module,
    });
}

fn collect_cascade_sends(
    receiver: &Expression,
    messages: &[crate::ast::CascadeMessage],
    source: &str,
    hits: &mut Vec<SendHit>,
) {
    let kind = cascade_receiver_kind(receiver);
    let module = if kind == ReceiverKind::ErlangFfi {
        cascade_shared_ffi_module(receiver)
    } else {
        None
    };
    let error_rooted = receiver_rooted_in_error(receiver);
    collect_sends(receiver, source, hits);
    for msg in messages {
        if !error_rooted {
            push_send(&msg.selector, msg.span, kind, module.clone(), source, hits);
        }
        for arg in &msg.arguments {
            collect_sends(arg, source, hits);
        }
    }
}

fn collect_sends(expr: &Expression, source: &str, hits: &mut Vec<SendHit>) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            if !receiver_rooted_in_error(receiver) {
                let kind = receiver_kind(receiver, selector);
                let module = if kind == ReceiverKind::ErlangFfi {
                    ffi_target_module(receiver, selector)
                } else {
                    None
                };
                push_send(selector, *span, kind, module, source, hits);
            }
            collect_sends(receiver, source, hits);
            for arg in arguments {
                collect_sends(arg, source, hits);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => collect_cascade_sends(receiver, messages, source, hits),
        Expression::Assignment { target, value, .. } => {
            collect_sends(target, source, hits);
            collect_sends(value, source, hits);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_sends(value, source, hits);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_sends(&stmt.expression, source, hits);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_sends(expression, source, hits);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_sends(receiver, source, hits);
        }
        Expression::Match { value, arms, .. } => {
            collect_sends(value, source, hits);
            for arm in arms {
                collect_pattern_sends(&arm.pattern, source, hits);
                if let Some(guard) = &arm.guard {
                    collect_sends(guard, source, hits);
                }
                collect_sends(&arm.body, source, hits);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_sends(inner, source, hits);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_sends(element, source, hits);
            }
            if let Some(tail_expr) = tail {
                collect_sends(tail_expr, source, hits);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_sends(element, source, hits);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_sends(&pair.key, source, hits);
                collect_sends(&pair.value, source, hits);
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

fn collect_pattern_sends(pattern: &Pattern, source: &str, hits: &mut Vec<SendHit>) {
    match pattern {
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_sends(&segment.value, source, hits);
                if let Some(size) = &segment.size {
                    collect_sends(size, source, hits);
                }
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_sends(element, source, hits);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_sends(element, source, hits);
            }
            if let Some(rest_pattern) = rest {
                collect_pattern_sends(rest_pattern, source, hits);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_sends(element, source, hits);
            }
            if let Some(tail_pattern) = tail {
                collect_pattern_sends(tail_pattern, source, hits);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_sends(&pair.value, source, hits);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_selector, inner) in keywords {
                collect_pattern_sends(inner, source, hits);
            }
        }
        Pattern::Wildcard(..)
        | Pattern::Literal(..)
        | Pattern::Variable(..)
        | Pattern::Nil(..)
        | Pattern::Type { .. } => {}
    }
}

fn selector_line(selector: &MessageSelector, fallback: Span, source: &str) -> u32 {
    selector_span(selector)
        .unwrap_or(fallback)
        .line_number(source)
}

// ---------------------------------------------------------------------------
// Selector-send span extraction (ADR 0114, BT-3279)
// ---------------------------------------------------------------------------

/// One rewrite target within a single matched self/super send of the target
/// selector: for a keyword selector, one entry per keyword part (each
/// keyword token can be renamed independently — the multi-site rewrite
/// mechanism in `beamtalk_repl_loader:rewrite_sites/2` already handles
/// multiple spans per file); for unary/binary, one entry covering the bare
/// selector token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorSendSpan {
    /// The exact byte span to splice, in the caller's own coordinate space
    /// (the *unwrapped* `method_source`/definition `source` the caller
    /// passed in — synthetic-prefix/wrapper offsets are already corrected
    /// out before this is returned).
    pub span: Span,
    /// The exact replacement text for this span (a keyword token like
    /// `"at:"`, or the whole unary/binary selector).
    pub new_text: String,
}

/// Splits a plain selector string into its keyword parts, re-appending the
/// trailing colon to each (the inverse of [`MessageSelector::name`]'s
/// concatenation for the `Keyword` case). `"at:put:"` -> `["at:", "put:"]`;
/// a unary/binary selector (no `:`) yields a single-element vec containing
/// the selector unchanged — callers needing the *keyword* shape check
/// `parts.len()` against the matched selector's own `KeywordPart` count and
/// treat a mismatch as "not this shape" per both resolvers' documented
/// skip-don't-panic contract.
fn split_keyword_parts(selector: &str) -> Vec<String> {
    selector
        .split(':')
        .filter(|part| !part.is_empty())
        .map(|part| format!("{part}:"))
        .collect()
}

/// The span of the first real (non-trivia) token starting at or after
/// `pos`, translated back into `source`'s own absolute coordinates.
///
/// Re-lexes `source[pos..]` rather than scanning bytes for whitespace: a
/// `BeamTalk` `//`/`/* */` comment is lexer TRIVIA attached to the token that
/// follows it, not a token of its own (`Token::span()` excludes trivia by
/// construction — see `token.rs`), so this is exact regardless of how much
/// whitespace *or* how many comments sit between `pos` and the real next
/// token. Mirrors `find_definition_selector_spans`'s own modifier-skipping
/// re-lex, generalized to "take the first token" (index 0) instead of
/// "skip N known modifier tokens".
///
/// Returns `None` only if `source[pos..]` has no token at all (e.g. `pos`
/// is at/past EOF) — not expected for a well-formed send (there is always
/// at least the selector token following the receiver), but never panics.
fn first_token_span_after(source: &str, pos: u32) -> Option<Span> {
    let tail = source.get(pos as usize..)?;
    let tail_tokens = lex_with_eof(tail);
    let tok_span = tail_tokens.first()?.span();
    Some(Span::new(pos + tok_span.start(), pos + tok_span.end()))
}

/// Find every self/super-directed send of `old_selector` within
/// `method_source`, returning the splice spans needed to rewrite each
/// occurrence to `new_selector` (ADR 0114 § "`renameSelector:to:` auto-
/// rewrites only `self`/`super` sends", BT-3279).
///
/// `senders_of/1` (`beamtalk_xref.erl`) only carries a *line* number per
/// sending method, not a byte span — and a whole-method span
/// (`resolve_method_span`) is too coarse to splice a single send's selector
/// token(s) without corrupting the rest of the method body. This resolver
/// closes that gap with a small, precisely-scoped AST walk: it is NOT a
/// regex/text search, because a multi-keyword selector like `at:put:` can
/// have arbitrary nested expressions (including sends that coincidentally
/// reuse the same keyword text) between its keyword parts — only the AST
/// tells us where the token boundaries actually are.
///
/// Reuses `find_all_sends_in_source`'s wrap-in-synthetic-class / lex / parse
/// / `ast_walker::for_each_expr_seq` pattern so the same parser entry point
/// backs both resolvers. Unlike that function, no line-number correction is
/// needed (this returns byte spans, not lines) — but the synthetic prefix's
/// byte length must still be subtracted from every returned span so callers
/// see offsets in their own `method_source`'s coordinate space.
///
/// Both selectors MUST be the same [`MessageSelector`] shape (unary/binary/
/// keyword) and the same arity — callers are responsible for that invariant
/// (the Erlang caller only reaches this after confirming the rename target
/// selector already resolves against the class, so a shape/arity mismatch
/// is not expected in practice). A mismatched keyword arity for one matched
/// send yields no spans for *that* occurrence (skipped, not panicked) —
/// a selector rename that changes arity is out of scope for this resolver;
/// it surfaces upstream as "no confirmed site found" rather than a special
/// error.
///
/// Only a send whose receiver is written literally as `self` or `super`
/// counts — a cascade's non-first cascaded message (`self foo: 1; bar: 2`'s
/// `bar: 2`) is not walked as its own `Expression::MessageSend` node and is
/// therefore NOT found by this resolver (only the cascade's first message,
/// folded into `receiver` by the parser, is an ordinary `MessageSend` and IS
/// found). This is a deliberate, documented scope limit — cascaded self/
/// super sends of a renamed selector are rare — rather than a correctness
/// bug: an unresolved occurrence is simply never added to the confirmed
/// rewrite set, the same "accepted completeness gap" category ADR 0114
/// already names for `renameTo:`'s live-patch/string-literal blind spots,
/// never a corruption risk.
///
/// Returns an empty vector if `method_source` contains no matching sends or
/// cannot be parsed at all (mirrors `find_all_sends_in_source`'s contract).
#[must_use]
#[expect(
    clippy::cast_possible_truncation,
    reason = "the synthetic prefix is a short fixed constant, never near u32::MAX"
)]
pub fn find_selector_send_spans(
    method_source: &str,
    old_selector: &str,
    new_selector: &str,
) -> Vec<Vec<SelectorSendSpan>> {
    let wrapped = format!("{SENDS_SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut occurrences: Vec<Vec<SelectorSendSpan>> = Vec::new();

    crate::ast_walker::for_each_expr_seq(&module, |seq| {
        for stmt in seq {
            collect_selector_send_spans(
                &stmt.expression,
                &wrapped,
                old_selector,
                new_selector,
                &mut occurrences,
            );
        }
    });

    let prefix_len = SENDS_SYNTHETIC_PREFIX.len() as u32;
    occurrences
        .into_iter()
        .map(|spans| {
            spans
                .into_iter()
                .map(|hit| SelectorSendSpan {
                    span: Span::new(hit.span.start() - prefix_len, hit.span.end() - prefix_len),
                    new_text: hit.new_text,
                })
                .collect()
        })
        .collect()
}

/// Mirrors [`collect_sends`]'s recursive shape, but only records a match
/// (pushing one whole occurrence's worth of [`SelectorSendSpan`]s) at a
/// direct self/super-receiver `MessageSend` of `old_selector`; every other
/// node is walked purely for recursion, never itself pushed. Pattern-nested
/// expressions (a `Match` arm's constructor-pattern keyword defaults, a
/// binary-pattern segment's size expression) are not walked — self/super
/// sends there are vanishingly rare and, per this function's own doc,
/// missing one is a completeness gap, not a corruption risk.
fn collect_selector_send_spans(
    expr: &Expression,
    source: &str,
    old_selector: &str,
    new_selector: &str,
    out: &mut Vec<Vec<SelectorSendSpan>>,
) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            if !receiver_rooted_in_error(receiver)
                && self_or_super_kind(receiver).is_some()
                && selector.name() == old_selector
            {
                if let Some(spans) =
                    selector_send_spans_for_match(selector, receiver, source, new_selector)
                {
                    out.push(spans);
                }
            }
            collect_selector_send_spans(receiver, source, old_selector, new_selector, out);
            for arg in arguments {
                collect_selector_send_spans(arg, source, old_selector, new_selector, out);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_selector_send_spans(receiver, source, old_selector, new_selector, out);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_selector_send_spans(arg, source, old_selector, new_selector, out);
                }
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_selector_send_spans(target, source, old_selector, new_selector, out);
            collect_selector_send_spans(value, source, old_selector, new_selector, out);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_selector_send_spans(value, source, old_selector, new_selector, out);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_selector_send_spans(
                    &stmt.expression,
                    source,
                    old_selector,
                    new_selector,
                    out,
                );
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_selector_send_spans(expression, source, old_selector, new_selector, out);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_selector_send_spans(receiver, source, old_selector, new_selector, out);
        }
        Expression::Match { value, arms, .. } => {
            collect_selector_send_spans(value, source, old_selector, new_selector, out);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_selector_send_spans(guard, source, old_selector, new_selector, out);
                }
                collect_selector_send_spans(&arm.body, source, old_selector, new_selector, out);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_selector_send_spans(inner, source, old_selector, new_selector, out);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_selector_send_spans(element, source, old_selector, new_selector, out);
            }
            if let Some(tail_expr) = tail {
                collect_selector_send_spans(tail_expr, source, old_selector, new_selector, out);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_selector_send_spans(element, source, old_selector, new_selector, out);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_selector_send_spans(&pair.key, source, old_selector, new_selector, out);
                collect_selector_send_spans(&pair.value, source, old_selector, new_selector, out);
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

/// Builds the per-occurrence rewrite spans for one matched self/super send
/// (already confirmed to have `selector.name() == old_selector`), per this
/// module's shape-specific rules — see [`find_selector_send_spans`]'s doc.
fn selector_send_spans_for_match(
    selector: &MessageSelector,
    receiver: &Expression,
    source: &str,
    new_selector: &str,
) -> Option<Vec<SelectorSendSpan>> {
    match selector {
        MessageSelector::Keyword(parts) => {
            let new_parts = split_keyword_parts(new_selector);
            if new_parts.len() != parts.len() {
                // Arity mismatch: skip this occurrence, never panic (see
                // this function's own doc and `find_selector_send_spans`'s).
                return None;
            }
            Some(
                parts
                    .iter()
                    .zip(new_parts)
                    .map(|(part, new_text)| SelectorSendSpan {
                        span: part.span,
                        new_text,
                    })
                    .collect(),
            )
        }
        MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
            // Neither shape carries its own selector span from the parser
            // (unlike `Keyword`'s per-part spans) — re-lex from just after
            // the receiver and take the first real token. This is exact
            // even when a `//`/`/* */` comment sits between the receiver
            // and the selector (review finding on PR #3529: a prior
            // whitespace-only byte scan swallowed such a comment into the
            // splice span, silently deleting it on rewrite) — see
            // `first_token_span_after`'s own doc for why re-lexing rather
            // than scanning bytes is exact here regardless of trivia.
            let span = first_token_span_after(source, receiver.span().end())?;
            Some(vec![SelectorSendSpan {
                span,
                new_text: new_selector.to_string(),
            }])
        }
    }
}

/// Resolve `class`'s `(old_selector, side)` method DEFINITION's own bare
/// selector-token span(s) within `source`, paired with the replacement text
/// for a rename to `new_selector` (ADR 0114 § `ChangeLog` schema's `sites[0]`
/// note, BT-3279) — the narrow-rewrite counterpart to
/// [`find_selector_send_spans`] for the definition site itself.
///
/// A method's own signature is not a self/super *send* — `MethodDefinition`
/// has no receiver at all — so it needs its own resolver rather than reusing
/// the send-walker above. The definition site MUST be a narrow
/// selector-token splice, never the whole method body: `emit_rewrite_change_
/// entry`/`rewrite_sites` treat a rewrite site's `new_text` as replacing
/// exactly the bytes at `span`, so passing the method's full text here would
/// corrupt its own parameter names/body on rewrite.
///
/// For a keyword selector, returns one [`SelectorSendSpan`] per
/// `KeywordPart` — each keyword token already carries its own exact span
/// from the parser, no text-scanning needed (mirrors
/// [`find_selector_send_spans`]'s keyword case exactly).
///
/// For unary/binary, [`MessageSelector::Unary`]/[`MessageSelector::Binary`]
/// carry only the selector's name, not a span, so this walks forward from
/// the definition's own AST span start (`MethodDefinition::span`, which the
/// parser sets to start at the first modifier token — `class`/`internal`/
/// `sealed` — or the selector token itself when there are no modifiers),
/// skipping exactly as many leading modifier identifier tokens as the
/// resolved definition's own `is_sealed`/`is_internal`/`is_class_method`
/// flags say are present, via a small re-lex of that prefix. This is exact
/// and immune to a doc comment or method body coincidentally repeating the
/// selector's own name as a substring: the doc comment lives entirely
/// before `MethodDefinition::span` (per `resolve_method_span`'s module doc),
/// and the body lives entirely after the selector token this walk stops at
/// — so, unlike `renameTo:`'s `word_occurrence_spans` regex approach (safe
/// there only because a bare class name can't appear as a modifier), no
/// text search of ambiguous scope is needed here at all.
///
/// Returns `Ok(vec![])` — never a panic — when the matched definition's
/// selector and `new_selector` have different keyword arities (mirrors
/// [`find_selector_send_spans`]'s "skip, don't panic" contract for the same
/// case).
///
/// # Errors
///
/// Returns the same [`SpanResolveError`] variants `resolve_method_span`
/// does: `ClassNotFound` when `class` does not appear in `source` at all,
/// `SelectorNotFound` when `class` exists but has no `(old_selector, side)`
/// definition, and `Ambiguous` when more than one definition matches.
pub fn find_definition_selector_spans(
    source: &str,
    class: &str,
    old_selector: &str,
    new_selector: &str,
    side: MethodSide,
) -> Result<Vec<SelectorSendSpan>, SpanResolveError> {
    let tokens = lex_with_eof(source);
    let (module, _diags) = parse(tokens);
    let (class_seen, matches) = crate::source_analysis::method_span::find_matching_definitions(
        &module,
        class,
        old_selector,
        side,
    );

    let method = match matches.len() {
        0 if !class_seen => {
            return Err(SpanResolveError::ClassNotFound {
                class: class.to_string(),
            });
        }
        0 => {
            return Err(SpanResolveError::SelectorNotFound {
                class: class.to_string(),
                selector: old_selector.to_string(),
                side,
            });
        }
        1 => matches[0].method,
        count => {
            return Err(SpanResolveError::Ambiguous {
                class: class.to_string(),
                selector: old_selector.to_string(),
                side,
                count,
            });
        }
    };

    match &method.selector {
        MessageSelector::Keyword(parts) => {
            let new_parts = split_keyword_parts(new_selector);
            if new_parts.len() != parts.len() {
                return Ok(Vec::new());
            }
            Ok(parts
                .iter()
                .zip(new_parts)
                .map(|(part, new_text)| SelectorSendSpan {
                    span: part.span,
                    new_text,
                })
                .collect())
        }
        MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
            let modifier_count = usize::from(method.is_sealed)
                + usize::from(method.is_internal)
                + usize::from(method.is_class_method);
            let start = method.span.start();
            let tail = &source[start as usize..];
            let tail_tokens = lex_with_eof(tail);
            match tail_tokens.get(modifier_count) {
                Some(token) => {
                    let tok_span = token.span();
                    let span = Span::new(start + tok_span.start(), start + tok_span.end());
                    Ok(vec![SelectorSendSpan {
                        span,
                        new_text: new_selector.to_string(),
                    }])
                }
                None => Ok(Vec::new()),
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Receiver-span extraction (BT-3217, ADR 0115 Phase 2)
// ---------------------------------------------------------------------------

/// One send/cascade-message hit discovered by [`collect_receiver_spans`]: the
/// selector name (identical to what [`push_send`] would record in a
/// [`SendHit`] at the same pre-order position — used by the corpus
/// conformance test to assert selector-sequence identity between the two
/// walks) and the receiver's file-absolute [`Span`] (used by codegen's
/// `TypeMap` join).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReceiverSpanHit {
    /// The selector name, exactly as [`SendHit::selector`] would record it.
    pub selector: String,
    /// File-absolute span of the message's receiver expression.
    pub span: Span,
}

/// Walks the original, already-parsed `&MethodDefinition` in the exact same
/// pre-order as [`find_all_sends_in_source`]'s reparsed walk, collecting the
/// **receiver span** of each send/cascade-message hit — file-absolute
/// coordinates, since this walks the real AST directly rather than a
/// re-unparsed synthetic copy.
///
/// Codegen (`gen_server/methods.rs::build_method_xref_entry`) joins this
/// function's output against `find_all_sends_in_source`'s `SendHit` stream
/// **by pre-order ordinal** (index `i` here pairs with `SendHit`s\[i\]) to
/// recover each send's receiver type from the type checker's `TypeMap`
/// (keyed by file-absolute span) — the coordinate-space join the ADR 0115
/// Phase 1 spike (`docs/internal/adr-0115-phase1-spike-findings.md` §1c/§1d)
/// identified as the real blocker, since `find_all_sends_in_source`'s
/// `SendHit`s carry no span into a re-unparsed copy's source.
///
/// **This walk must stay structurally identical to [`collect_sends`]** (same
/// hit count, same order, same cascade-expansion rule) or the ordinal join
/// silently misaligns. `source_analysis::method_span_corpus_tests` asserts
/// this holds — hit-count and selector-sequence identity — over the full
/// stdlib+examples corpus, per this project's no-"keep-in-sync"-comment-
/// without-a-test rule.
///
/// Unlike `find_all_sends_in_source`, this needs no synthetic-class
/// wrapping, re-lex/re-parse, or coordinate translation: the input
/// `MethodDefinition` already carries file-absolute spans. Depends only on
/// `crate::ast`/`crate::source_analysis` — no `semantic_analysis` dependency
/// (this module's leaf position, ADR 0115 Constraint 3, is unchanged).
#[must_use]
pub fn collect_receiver_spans(method: &MethodDefinition) -> Vec<ReceiverSpanHit> {
    let mut hits = Vec::new();
    for stmt in &method.body {
        collect_receiver_spans_expr(&stmt.expression, &mut hits);
    }
    hits
}

/// Mirrors [`collect_sends`] arm-for-arm, pushing a [`ReceiverSpanHit`]
/// instead of a [`SendHit`] at each site `collect_sends` would call
/// `push_send`.
fn collect_receiver_spans_expr(expr: &Expression, hits: &mut Vec<ReceiverSpanHit>) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            if !receiver_rooted_in_error(receiver) {
                hits.push(ReceiverSpanHit {
                    selector: selector.name().to_string(),
                    span: receiver.span(),
                });
            }
            collect_receiver_spans_expr(receiver, hits);
            for arg in arguments {
                collect_receiver_spans_expr(arg, hits);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => collect_cascade_receiver_spans(receiver, messages, hits),
        Expression::Assignment { target, value, .. } => {
            collect_receiver_spans_expr(target, hits);
            collect_receiver_spans_expr(value, hits);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_receiver_spans_expr(value, hits);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_receiver_spans_expr(&stmt.expression, hits);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_receiver_spans_expr(expression, hits);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_receiver_spans_expr(receiver, hits);
        }
        Expression::Match { value, arms, .. } => {
            collect_receiver_spans_expr(value, hits);
            for arm in arms {
                collect_pattern_receiver_spans(&arm.pattern, hits);
                if let Some(guard) = &arm.guard {
                    collect_receiver_spans_expr(guard, hits);
                }
                collect_receiver_spans_expr(&arm.body, hits);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_receiver_spans_expr(inner, hits);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_receiver_spans_expr(element, hits);
            }
            if let Some(tail_expr) = tail {
                collect_receiver_spans_expr(tail_expr, hits);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_receiver_spans_expr(element, hits);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_receiver_spans_expr(&pair.key, hits);
                collect_receiver_spans_expr(&pair.value, hits);
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

/// Mirrors [`collect_cascade_sends`]: one hit per cascade message (the
/// shared receiver's span, computed once), plus a recursive descent into the
/// receiver subtree and each message's arguments.
fn collect_cascade_receiver_spans(
    receiver: &Expression,
    messages: &[CascadeMessage],
    hits: &mut Vec<ReceiverSpanHit>,
) {
    let shared_span = cascade_receiver_span(receiver);
    let error_rooted = receiver_rooted_in_error(receiver);
    collect_receiver_spans_expr(receiver, hits);
    for msg in messages {
        if !error_rooted {
            hits.push(ReceiverSpanHit {
                selector: msg.selector.name().to_string(),
                span: shared_span,
            });
        }
        for arg in &msg.arguments {
            collect_receiver_spans_expr(arg, hits);
        }
    }
}

/// Mirrors [`cascade_receiver_kind`]'s receiver-unwrapping rule: a cascade's
/// `receiver` field holds the *first* cascaded message's full send tree (the
/// parser folds `a foo: 1; bar: 2` into `Cascade { receiver: (a foo: 1),
/// messages: [bar: 2] }`), so the span every cascade message shares is the
/// *inner* receiver of that send (`a`), not the send tree itself.
fn cascade_receiver_span(receiver: &Expression) -> Span {
    if let Expression::MessageSend {
        receiver: inner, ..
    } = receiver
    {
        inner.span()
    } else {
        receiver.span()
    }
}

/// Mirrors [`collect_pattern_sends`] arm-for-arm.
fn collect_pattern_receiver_spans(pattern: &Pattern, hits: &mut Vec<ReceiverSpanHit>) {
    match pattern {
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_receiver_spans(&segment.value, hits);
                if let Some(size) = &segment.size {
                    collect_receiver_spans_expr(size, hits);
                }
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_receiver_spans(element, hits);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_receiver_spans(element, hits);
            }
            if let Some(rest_pattern) = rest {
                collect_pattern_receiver_spans(rest_pattern, hits);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_receiver_spans(element, hits);
            }
            if let Some(tail_pattern) = tail {
                collect_pattern_receiver_spans(tail_pattern, hits);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_receiver_spans(&pair.value, hits);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_selector, inner) in keywords {
                collect_pattern_receiver_spans(inner, hits);
            }
        }
        Pattern::Wildcard(..)
        | Pattern::Literal(..)
        | Pattern::Variable(..)
        | Pattern::Nil(..)
        | Pattern::Type { .. } => {}
    }
}

// ---------------------------------------------------------------------------
// Class-reference extraction
// ---------------------------------------------------------------------------

const REFS_PREFIX_LINES: u32 = 1;
const REFS_SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticReferencesScope\n";

/// A single class reference discovered while walking a method's AST.
///
/// Used by the xref codegen (ADR 0087 Phase 2, BT-2298) to bake per-method
/// `references` rows into `register_class/0`, and re-exported by
/// `beamtalk-language-service`'s `queries::references_to_query` for Language
/// Service use.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceHit {
    /// The referenced class name (as written in source).
    pub class: String,
    /// 1-based line number within the input source where the reference appears.
    pub line: u32,
}

/// Find every class reference within `method_source`.
///
/// Walks the parsed AST for [`Expression::ClassReference`] nodes and the
/// class names mentioned in parameter / return / state type annotations,
/// emitting one [`ReferenceHit`] per mention. A class referenced multiple
/// times produces one entry per occurrence.
///
/// Shares the parsing strategy and coordinate translation of
/// `find_references_to_in_source` — the input is wrapped in a synthetic
/// class header, walked, and line numbers are translated back to input-source
/// space (dropping any match inside the wrapper header).
///
/// Returns an empty vector if the source contains no references or cannot be
/// parsed at all.
#[must_use]
pub fn find_all_references_in_source(method_source: &str) -> Vec<ReferenceHit> {
    let wrapped = format!("{REFS_SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut hits: Vec<(String, u32)> = Vec::new();

    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for param in &method.parameters {
                if let Some(ann) = &param.type_annotation {
                    collect_all_type_refs(ann, &wrapped, &mut hits);
                }
            }
            if let Some(ret) = &method.return_type {
                collect_all_type_refs(ret, &wrapped, &mut hits);
            }
            for stmt in &method.body {
                collect_all_references(&stmt.expression, &wrapped, &mut hits);
            }
        }
    }

    for stmt in &module.expressions {
        collect_all_references(&stmt.expression, &wrapped, &mut hits);
    }
    for smd in &module.method_definitions {
        for param in &smd.method.parameters {
            if let Some(ann) = &param.type_annotation {
                collect_all_type_refs(ann, &wrapped, &mut hits);
            }
        }
        if let Some(ret) = &smd.method.return_type {
            collect_all_type_refs(ret, &wrapped, &mut hits);
        }
        for stmt in &smd.method.body {
            collect_all_references(&stmt.expression, &wrapped, &mut hits);
        }
    }

    hits.into_iter()
        .filter(|&(_, line)| line > REFS_PREFIX_LINES)
        .map(|(class, line)| ReferenceHit {
            class,
            line: line - REFS_PREFIX_LINES,
        })
        .collect()
}

fn collect_all_references(expr: &Expression, source: &str, hits: &mut Vec<(String, u32)>) {
    match expr {
        Expression::ClassReference { name, span, .. } => {
            hits.push((name.name.to_string(), span.line_number(source)));
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_all_references(receiver, source, hits);
            for arg in arguments {
                collect_all_references(arg, source, hits);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_all_references(receiver, source, hits);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_all_references(arg, source, hits);
                }
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_all_references(target, source, hits);
            collect_all_references(value, source, hits);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_all_references(value, source, hits);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_all_references(&stmt.expression, source, hits);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_all_references(expression, source, hits);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_all_references(receiver, source, hits);
        }
        Expression::Match { value, arms, .. } => {
            collect_all_references(value, source, hits);
            for arm in arms {
                collect_all_pattern_refs(&arm.pattern, source, hits);
                if let Some(guard) = &arm.guard {
                    collect_all_references(guard, source, hits);
                }
                collect_all_references(&arm.body, source, hits);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_all_references(inner, source, hits);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_all_references(element, source, hits);
            }
            if let Some(tail_expr) = tail {
                collect_all_references(tail_expr, source, hits);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_all_references(element, source, hits);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_all_references(&pair.key, source, hits);
                collect_all_references(&pair.value, source, hits);
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

fn collect_all_pattern_refs(pattern: &Pattern, source: &str, hits: &mut Vec<(String, u32)>) {
    match pattern {
        Pattern::Constructor {
            class, keywords, ..
        } => {
            hits.push((class.name.to_string(), class.span.line_number(source)));
            for (_selector, inner) in keywords {
                collect_all_pattern_refs(inner, source, hits);
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_all_pattern_refs(element, source, hits);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_all_pattern_refs(element, source, hits);
            }
            if let Some(rest_pattern) = rest {
                collect_all_pattern_refs(rest_pattern, source, hits);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_all_pattern_refs(element, source, hits);
            }
            if let Some(tail_pattern) = tail {
                collect_all_pattern_refs(tail_pattern, source, hits);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_all_pattern_refs(&pair.value, source, hits);
            }
        }
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_all_pattern_refs(&segment.value, source, hits);
                if let Some(size) = &segment.size {
                    collect_all_references(size, source, hits);
                }
            }
        }
        Pattern::Type { class, .. } => {
            hits.push((class.name.to_string(), class.span.line_number(source)));
        }
        Pattern::Wildcard(..) | Pattern::Literal(..) | Pattern::Variable(..) | Pattern::Nil(..) => {
        }
    }
}

fn collect_all_type_refs(annotation: &TypeAnnotation, source: &str, hits: &mut Vec<(String, u32)>) {
    match annotation {
        TypeAnnotation::Simple(id) => {
            hits.push((id.name.to_string(), id.span.line_number(source)));
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            hits.push((base.name.to_string(), base.span.line_number(source)));
            for param in parameters {
                collect_all_type_refs(param, source, hits);
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for ty in types {
                collect_all_type_refs(ty, source, hits);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            collect_all_type_refs(inner, source, hits);
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            collect_all_type_refs(base, source, hits);
            collect_all_type_refs(excluded, source, hits);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            collect_all_type_refs(left, source, hits);
            collect_all_type_refs(right, source, hits);
        }
        TypeAnnotation::ClassOf {
            class_name: class_id,
            ..
        } => {
            hits.push((class_id.name.to_string(), class_id.span.line_number(source)));
        }
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

// ---------------------------------------------------------------------------
// Selector-send / definition span tests (ADR 0114, BT-3279)
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn slice(source: &str, span: Span) -> &str {
        &source[span.start() as usize..span.end() as usize]
    }

    // ---- find_selector_send_spans ----

    #[test]
    fn unary_self_send_is_found() {
        let src = "increment => self bump";
        let occurrences = find_selector_send_spans(src, "bump", "increase");
        assert_eq!(occurrences.len(), 1, "got {occurrences:?}");
        assert_eq!(occurrences[0].len(), 1);
        let hit = &occurrences[0][0];
        assert_eq!(slice(src, hit.span), "bump");
        assert_eq!(hit.new_text, "increase");
    }

    #[test]
    fn block_comment_between_receiver_and_unary_selector_is_not_swallowed() {
        // Review finding on PR #3529: a whitespace-only byte scan for the
        // unary/binary selector's start treated a `/* ... */` block comment
        // (legal BeamTalk lexer trivia between any two tokens on the same
        // line) as part of the splice span, so a rewrite would silently
        // delete the comment along with the old selector text.
        // `first_token_span_after` fixes this by re-lexing rather than
        // scanning bytes. (A `//` line comment can't occur in this position
        // in the first place — verified independently: a bare newline
        // between receiver and selector already splits them into two
        // separate statements before any comment is involved, since
        // BeamTalk newlines are significant statement terminators — so
        // there is no reachable "line comment mid-send" case to cover.)
        let src = "increment => self /* why */ bump";
        let occurrences = find_selector_send_spans(src, "bump", "increase");
        assert_eq!(occurrences.len(), 1, "got {occurrences:?}");
        let hit = &occurrences[0][0];
        assert_eq!(
            slice(src, hit.span),
            "bump",
            "span should be JUST the selector token"
        );
    }

    #[test]
    fn comment_between_receiver_and_binary_selector_is_not_swallowed() {
        let src = "combine: n => self /* why */ + n";
        let occurrences = find_selector_send_spans(src, "+", "plus");
        assert_eq!(occurrences.len(), 1, "got {occurrences:?}");
        let hit = &occurrences[0][0];
        assert_eq!(
            slice(src, hit.span),
            "+",
            "span should be JUST the operator token"
        );
    }

    #[test]
    fn unary_super_send_is_found() {
        let src = "increment => super bump";
        let occurrences = find_selector_send_spans(src, "bump", "increase");
        assert_eq!(occurrences.len(), 1, "got {occurrences:?}");
        let hit = &occurrences[0][0];
        assert_eq!(slice(src, hit.span), "bump");
        assert_eq!(hit.new_text, "increase");
    }

    #[test]
    fn binary_self_send_is_found() {
        let src = "combine: n => self + n";
        let occurrences = find_selector_send_spans(src, "+", "plus");
        assert_eq!(occurrences.len(), 1, "got {occurrences:?}");
        let hit = &occurrences[0][0];
        assert_eq!(slice(src, hit.span), "+");
        assert_eq!(hit.new_text, "plus");
    }

    #[test]
    fn keyword_self_send_with_multiple_parts_is_found() {
        let src = "store: k value: v => self at: k put: v";
        let occurrences = find_selector_send_spans(src, "at:put:", "setAt:to:");
        assert_eq!(occurrences.len(), 1, "got {occurrences:?}");
        let spans = &occurrences[0];
        assert_eq!(spans.len(), 2);
        assert_eq!(slice(src, spans[0].span), "at:");
        assert_eq!(spans[0].new_text, "setAt:");
        assert_eq!(slice(src, spans[1].span), "put:");
        assert_eq!(spans[1].new_text, "to:");
    }

    #[test]
    fn other_receiver_send_is_not_matched() {
        // `anObject increment` — same selector name, but not a self/super
        // send, so this resolver (deliberately) never finds it; the caller's
        // xref-driven cross-hierarchy check is what routes this to
        // `candidate_sites` instead.
        let src = "run: anObject => anObject increment";
        let occurrences = find_selector_send_spans(src, "increment", "bump");
        assert!(occurrences.is_empty(), "got {occurrences:?}");
    }

    #[test]
    fn different_selector_on_self_is_not_matched() {
        let src = "run => self decrement";
        let occurrences = find_selector_send_spans(src, "increment", "bump");
        assert!(occurrences.is_empty(), "got {occurrences:?}");
    }

    #[test]
    fn two_occurrences_of_same_self_send_are_both_found() {
        let src = "run => self bump. self bump";
        let occurrences = find_selector_send_spans(src, "bump", "increase");
        assert_eq!(occurrences.len(), 2, "got {occurrences:?}");
        for occurrence in &occurrences {
            assert_eq!(occurrence.len(), 1);
            assert_eq!(slice(src, occurrence[0].span), "bump");
            assert_eq!(occurrence[0].new_text, "increase");
        }
    }

    #[test]
    fn mismatched_keyword_arity_returns_no_spans_for_that_occurrence() {
        let src = "store: k value: v => self at: k put: v";
        // `setAt:` has one keyword part; `at:put:` has two — arity mismatch,
        // skipped rather than panicking.
        let occurrences = find_selector_send_spans(src, "at:put:", "setAt:");
        assert!(occurrences.is_empty(), "got {occurrences:?}");
    }

    #[test]
    fn unparseable_source_returns_empty() {
        let occurrences = find_selector_send_spans(")@!", "foo", "bar");
        assert!(occurrences.is_empty());
    }

    // ---- find_definition_selector_spans ----

    #[test]
    fn definition_unary_selector_span_is_found() {
        let src = "Object subclass: Counter\n  increment => self.value := self.value + 1\n";
        let spans = find_definition_selector_spans(
            src,
            "Counter",
            "increment",
            "bump",
            MethodSide::Instance,
        )
        .expect("resolves");
        assert_eq!(spans.len(), 1);
        assert_eq!(slice(src, spans[0].span), "increment");
        assert_eq!(spans[0].new_text, "bump");
    }

    #[test]
    fn definition_binary_selector_span_is_found() {
        let src = "Object subclass: Vec\n  + other => self\n";
        let spans = find_definition_selector_spans(src, "Vec", "+", "plus", MethodSide::Instance)
            .expect("resolves");
        assert_eq!(spans.len(), 1);
        assert_eq!(slice(src, spans[0].span), "+");
        assert_eq!(spans[0].new_text, "plus");
    }

    #[test]
    fn definition_keyword_selector_spans_are_found() {
        let src = "Object subclass: D\n  at: k put: v => self\n";
        let spans =
            find_definition_selector_spans(src, "D", "at:put:", "setAt:to:", MethodSide::Instance)
                .expect("resolves");
        assert_eq!(spans.len(), 2);
        assert_eq!(slice(src, spans[0].span), "at:");
        assert_eq!(spans[0].new_text, "setAt:");
        assert_eq!(slice(src, spans[1].span), "put:");
        assert_eq!(spans[1].new_text, "to:");
    }

    #[test]
    fn definition_keyword_selector_span_skips_class_sealed_modifiers() {
        let src = "Object subclass: Counter\n  class sealed new: name => self\n";
        let spans =
            find_definition_selector_spans(src, "Counter", "new:", "make:", MethodSide::Class)
                .expect("resolves");
        assert_eq!(spans.len(), 1);
        assert_eq!(slice(src, spans[0].span), "new:");
        assert_eq!(spans[0].new_text, "make:");
    }

    #[test]
    fn definition_unary_selector_span_skips_class_sealed_modifiers() {
        // Real stdlib shape (`actor.bt`'s `class sealed spawn`): the bare
        // selector token has no dedicated AST span, so this exercises the
        // modifier-skipping re-lex.
        let src = "Object subclass: Counter\n  class sealed reset => self\n";
        let spans =
            find_definition_selector_spans(src, "Counter", "reset", "clear", MethodSide::Class)
                .expect("resolves");
        assert_eq!(spans.len(), 1);
        assert_eq!(slice(src, spans[0].span), "reset");
        assert_eq!(spans[0].new_text, "clear");
    }

    #[test]
    fn definition_selector_not_found_is_structured_error() {
        let src = "Object subclass: Counter\n  increment => self\n";
        let err =
            find_definition_selector_spans(src, "Counter", "missing", "x", MethodSide::Instance)
                .expect_err("missing selector");
        assert!(matches!(err, SpanResolveError::SelectorNotFound { .. }));
    }

    #[test]
    fn definition_class_not_found_is_structured_error() {
        let src = "Object subclass: Counter\n  increment => self\n";
        let err =
            find_definition_selector_spans(src, "NoSuch", "increment", "x", MethodSide::Instance)
                .expect_err("missing class");
        assert!(matches!(err, SpanResolveError::ClassNotFound { .. }));
    }

    #[test]
    fn definition_mismatched_keyword_arity_returns_empty() {
        let src = "Object subclass: D\n  at: k put: v => self\n";
        let spans =
            find_definition_selector_spans(src, "D", "at:put:", "setAt:", MethodSide::Instance)
                .expect("resolves");
        assert!(spans.is_empty(), "got {spans:?}");
    }
}

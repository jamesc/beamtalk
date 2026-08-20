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
use crate::source_analysis::{Span, lex_with_eof, parse};

// ---------------------------------------------------------------------------
// selector_span helper (previously in `queries/mod.rs`)
// ---------------------------------------------------------------------------

/// Returns the source span covering a keyword selector's keyword tokens, if any.
///
/// For a keyword selector with at least one part, merges the span of the first
/// and last keyword tokens. Returns `None` for unary and binary selectors.
///
/// Used by [`crate::queries`] siblings (`senders_query`, `all_sends_query`,
/// `announce_sites_query`, `ffi_sites_query`) and by the send-walker in this
/// module so the single implementation can be shared without reaching upward
/// into `queries`.
pub(crate) fn selector_span(selector: &MessageSelector) -> Option<Span> {
    match selector {
        MessageSelector::Keyword(parts) if !parts.is_empty() => {
            let first = parts.first().unwrap().span;
            let last = parts.last().unwrap().span;
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
/// [`crate::queries::references_to_query`] for Language Service use.
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

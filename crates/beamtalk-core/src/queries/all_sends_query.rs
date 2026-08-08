// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! All-sends query — extract every message send within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation unimplementedSelectors` (BT-2206). Where
//! [`crate::queries::senders_query`] filters by one known selector, this query
//! collects EVERY [`Expression::MessageSend`] and [`Cascade`] message in a
//! single pass — selector name, 1-based line number (relative to the input
//! source), and the syntactic kind of receiver
//! (`self` / `super` / Erlang FFI / other).
//!
//! The single-pass design lets the typo-finder compute
//! `allSentSelectors − allDefinedSelectors` without re-parsing each method
//! once per candidate selector.
//!
//! # Parsing strategy
//!
//! Identical to [`crate::queries::senders_query`]: `CompiledMethod source`
//! returns a bare method definition (signature + body, no class header), which
//! the parser does not accept at the top level. The input is wrapped in a
//! synthetic class definition before lexing/parsing, and line numbers are
//! translated back to the input's coordinate space by subtracting the prefix
//! line count.
//!
//! # Error handling
//!
//! Parse errors are tolerated: any sub-trees that parsed successfully still
//! contribute results. A completely unparseable source returns an empty list.

use super::selector_span;
use crate::ast::{Expression, Pattern, StringSegment};
use crate::source_analysis::{Span, lex_with_eof, parse};

/// Number of newlines in the synthetic class header that wraps the input.
/// Used to translate line numbers from wrapped-source to input-source space.
const PREFIX_LINES: u32 = 1;

/// The synthetic class header used to make a bare method definition parseable.
/// The single newline at the end is counted by [`PREFIX_LINES`].
const SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticAllSendsScope\n";

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
    let wrapped = format!("{SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut hits = Vec::new();

    crate::ast_walker::for_each_expr_seq(&module, |seq| {
        for stmt in seq {
            collect_sends(&stmt.expression, &wrapped, &mut hits);
        }
    });

    // Translate from wrapped-source line numbers back to input-source space.
    for hit in &mut hits {
        hit.line = hit.line.saturating_sub(PREFIX_LINES).max(1);
    }
    hits
}

/// Classify a bare `self`/`super` receiver, if it is one.
///
/// Shared by [`receiver_kind`] and [`cascade_receiver_kind`] so the two
/// classifiers don't each hand-roll the same self/super check — the kind of
/// duplication this PR otherwise exists to eliminate (BT-3079 review nit).
fn self_or_super_kind(receiver: &Expression) -> Option<ReceiverKind> {
    match receiver {
        Expression::Super(..) => Some(ReceiverKind::SuperReceiver),
        Expression::Identifier(ident) if ident.name == "self" => Some(ReceiverKind::SelfReceiver),
        _ => None,
    }
}

/// Classify a receiver expression as `self`, `super`, Erlang FFI, or other,
/// given the selector of the send `receiver` is the receiver of.
///
/// The selector is needed to apply the class-protocol filter to the bare
/// `Erlang <selector>` shape (BT-3079): `Erlang class` / `Erlang new` dispatch
/// to the class protocol, not the FFI bridge, so they must classify as
/// `Other` even though their receiver is the `Erlang` class reference.
fn receiver_kind(receiver: &Expression, selector: &crate::ast::MessageSelector) -> ReceiverKind {
    if let Some(kind) = self_or_super_kind(receiver) {
        return kind;
    }
    if ffi_target_module(receiver, selector).is_some() {
        return ReceiverKind::ErlangFfi;
    }
    ReceiverKind::Other
}

/// Resolve the native (Erlang) module a send is routed to, given the send's
/// own `receiver` and `selector`. `None` when the send is not FFI at all, or
/// when it would be but `selector` is a class-protocol selector (BT-3079):
/// `Erlang class` / `Erlang new` dispatch to the class protocol
/// (`beamtalk_object_class:class_send/3`) instead of the FFI bridge.
///
/// Two shapes carry FFI module information, both centralized in
/// [`crate::ffi_receiver`]:
/// - The module-name send itself (`Erlang lists`): `receiver` is the bare
///   `Erlang` class reference and `selector` (`lists`) is the module name.
/// - A function send (`(Erlang lists) reverse:`): the module is recovered
///   from the receiver chain via
///   [`crate::ffi_receiver::erlang_module_of_receiver`] — `selector` (the
///   *function* name, `reverse:`) plays no role in the module lookup here.
fn ffi_target_module(
    receiver: &Expression,
    selector: &crate::ast::MessageSelector,
) -> Option<String> {
    if crate::ffi_receiver::is_erlang_class_reference(receiver) {
        if let crate::ast::MessageSelector::Unary(module_name) = selector {
            if !crate::ffi_receiver::is_class_protocol_selector(module_name) {
                return Some(module_name.to_string());
            }
        }
        return None;
    }
    crate::ffi_receiver::erlang_module_of_receiver(receiver).map(ToString::to_string)
}

/// Classify the shared receiver of a cascade.
///
/// The parser folds the cascade's first message into the cascade `receiver`
/// field as a [`Expression::MessageSend`] (see `parse_cascade`), so the actual
/// shared receiver is that send's receiver. For any other shape (a cascade with
/// a bare receiver), fall back to classifying the receiver directly.
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
    // Defensive fallback for a cascade with a genuinely bare receiver (see
    // the doc comment above) — no wrapping send means no selector to apply
    // the class-protocol filter with, but that filter only matters for the
    // bare-`Erlang`-class-reference shape, which cannot itself appear here
    // without a wrapping send.
    if crate::ffi_receiver::erlang_module_of_receiver(receiver).is_some() {
        return ReceiverKind::ErlangFfi;
    }
    ReceiverKind::Other
}

/// Resolve the native module shared by an FFI cascade's messages.
///
/// The cascade `receiver` is the folded-in first message (a `MessageSend`); its
/// own receiver is the shared cascade receiver, from which the FFI module is
/// recovered the same way as for a non-cascade send.
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

/// Whether a send's receiver chain is rooted in a parse error.
///
/// Partial parses produce phantom sends: e.g. a bare typed method whose
/// signature collides with the synthetic class grammar (the `native:` selector
/// is consumed as a class backing-module declaration) leaves the return type
/// `Symbol -> Symbol` to parse as `Symbol` (unary on an `Error`) and `->`
/// (binary on that). Such sends are not real and must not be reported. Walking
/// the receiver chain to its base and checking for an [`Expression::Error`]
/// catches them without affecting genuine sends, whose chains bottom out at
/// `self` / `super` / an identifier / a class reference / a literal.
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

/// Record a real (non-phantom) send into `hits`.
///
/// Phantom sends produced by partial parses (receiver rooted in a parse error)
/// are dropped — see [`receiver_rooted_in_error`].
fn push_send(
    selector: &crate::ast::MessageSelector,
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

/// Collect the sends of a cascade (`recv msg1; msg2; …`) into `hits`.
///
/// The parser folds the first cascade message into `receiver` as a full
/// [`Expression::MessageSend`], so the shared cascade receiver is that send's
/// receiver. Recursing into `receiver` collects the first message; the remaining
/// `messages` reuse the same shared receiver kind. An FFI cascade's module is
/// resolved once from the shared receiver and reused for every message.
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

/// Recursively collect every message send into `hits`.
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

/// Recursively collect message sends inside a [`Pattern`].
///
/// Mirrors `senders_query::collect_pattern_send_lines`: patterns carry no
/// message sends today, but the walker traverses container patterns and the
/// binary-segment size expression defensively so loosening the parser later
/// does not silently regress the typo-finder.
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

/// Determine the line number to report for a send. Uses the span of the
/// selector keywords when available (so the line points at the selector token
/// rather than the receiver, for multi-line sends) and falls back to the
/// enclosing message-send span otherwise.
fn selector_line(selector: &crate::ast::MessageSelector, fallback: Span, source: &str) -> u32 {
    selector_span(selector)
        .unwrap_or(fallback)
        .line_number(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn selectors(hits: &[SendHit]) -> Vec<&str> {
        hits.iter().map(|h| h.selector.as_str()).collect()
    }

    #[test]
    fn finds_self_send() {
        let hits = find_all_sends_in_source("greet => self name");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "name");
        assert_eq!(hits[0].line, 1);
        assert_eq!(hits[0].receiver, ReceiverKind::SelfReceiver);
    }

    #[test]
    fn finds_super_send() {
        let hits = find_all_sends_in_source("increment => super increment");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "increment");
        assert_eq!(hits[0].receiver, ReceiverKind::SuperReceiver);
    }

    #[test]
    fn finds_other_receiver_send() {
        let hits = find_all_sends_in_source("describe => Transcript show");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "show");
        assert_eq!(hits[0].receiver, ReceiverKind::Other);
    }

    #[test]
    fn finds_unary_keyword_and_binary_sends() {
        // `self items at: index put: value` — keyword send on self, plus the
        // `index`/`value` identifiers are not sends.
        let hits = find_all_sends_in_source("store: value => self items at: 1 put: value");
        let sels = selectors(&hits);
        // `items` (unary on self) and `at:put:` (keyword on the `items` result).
        assert!(sels.contains(&"items"), "got {sels:?}");
        assert!(sels.contains(&"at:put:"), "got {sels:?}");
        // `items` is sent to self; `at:put:` is sent to the `items` result.
        let items = hits.iter().find(|h| h.selector == "items").unwrap();
        assert_eq!(items.receiver, ReceiverKind::SelfReceiver);
        let atput = hits.iter().find(|h| h.selector == "at:put:").unwrap();
        assert_eq!(atput.receiver, ReceiverKind::Other);
    }

    #[test]
    fn finds_binary_send() {
        let hits = find_all_sends_in_source("double: n => n * 2");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "*");
        assert_eq!(hits[0].receiver, ReceiverKind::Other);
    }

    #[test]
    fn finds_cascade_sends_with_shared_receiver_kind() {
        let src = "report =>\n  self\n    show: \"a\";\n    show: \"b\"";
        let hits = find_all_sends_in_source(src);
        let shows: Vec<&SendHit> = hits.iter().filter(|h| h.selector == "show:").collect();
        assert_eq!(shows.len(), 2, "got {hits:?}");
        // Both cascade messages share the cascade's `self` receiver.
        assert!(
            shows
                .iter()
                .all(|h| h.receiver == ReceiverKind::SelfReceiver)
        );
        assert_eq!(shows[0].line, 3);
        assert_eq!(shows[1].line, 4);
    }

    #[test]
    fn finds_send_inside_nested_block() {
        let src = "shout =>\n  [:x | x asString] value: 42";
        let hits = find_all_sends_in_source(src);
        let sels = selectors(&hits);
        assert!(sels.contains(&"asString"), "got {sels:?}");
        assert!(sels.contains(&"value:"), "got {sels:?}");
        let as_string = hits.iter().find(|h| h.selector == "asString").unwrap();
        assert_eq!(as_string.line, 2);
        assert_eq!(as_string.receiver, ReceiverKind::Other);
    }

    #[test]
    fn collects_multiple_sends_in_source_order() {
        let src = "report =>\n  a printString\n  b printString\n  c printString";
        let hits = find_all_sends_in_source(src);
        let prints: Vec<&SendHit> = hits
            .iter()
            .filter(|h| h.selector == "printString")
            .collect();
        assert_eq!(prints.len(), 3);
        assert_eq!(prints[0].line, 2);
        assert_eq!(prints[1].line, 3);
        assert_eq!(prints[2].line, 4);
    }

    #[test]
    fn finds_send_in_class_method_with_return_type() {
        let src = "default -> SystemNavigation =>\n  self new";
        let hits = find_all_sends_in_source(src);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "new");
        assert_eq!(hits[0].line, 2);
        assert_eq!(hits[0].receiver, ReceiverKind::SelfReceiver);
    }

    #[test]
    fn finds_send_with_leading_doc_comment_and_expect() {
        let src = "/// doc line\ndefault -> SystemNavigation =>\n  @expect dnu\n  self new";
        let hits = find_all_sends_in_source(src);
        let new = hits.iter().find(|h| h.selector == "new");
        assert!(new.is_some(), "expected `new`, got {hits:?}");
    }

    #[test]
    fn unparseable_source_returns_empty() {
        let hits = find_all_sends_in_source(")@!");
        assert!(hits.is_empty(), "got {hits:?}");
    }

    #[test]
    fn empty_body_returns_empty() {
        // A method with no sends in its body produces no hits.
        let hits = find_all_sends_in_source("answer => 42");
        assert!(hits.is_empty(), "got {hits:?}");
    }

    #[test]
    fn erlang_ffi_module_name_is_tagged() {
        // `Erlang beamtalk_interface` — the `beamtalk_interface` send has the
        // `Erlang` class reference as its receiver.
        let hits = find_all_sends_in_source("classes => Erlang beamtalk_interface");
        let hit = hits
            .iter()
            .find(|h| h.selector == "beamtalk_interface")
            .unwrap();
        assert_eq!(hit.receiver, ReceiverKind::ErlangFfi);
        // The module-name send carries the target module too (its own selector).
        assert_eq!(hit.target_module.as_deref(), Some("beamtalk_interface"));
    }

    #[test]
    fn erlang_ffi_chained_call_is_tagged() {
        // `(Erlang beamtalk_interface) allSendsIn: src` — the `allSendsIn:`
        // send's receiver is the `(Erlang beamtalk_interface)` send chain.
        let hits =
            find_all_sends_in_source("run: src => (Erlang beamtalk_interface) allSendsIn: src");
        let chained = hits.iter().find(|h| h.selector == "allSendsIn:").unwrap();
        assert_eq!(chained.receiver, ReceiverKind::ErlangFfi);
        // The function send resolves the target module from its receiver chain.
        assert_eq!(chained.target_module.as_deref(), Some("beamtalk_interface"));
        let module = hits
            .iter()
            .find(|h| h.selector == "beamtalk_interface")
            .unwrap();
        assert_eq!(module.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(module.target_module.as_deref(), Some("beamtalk_interface"));
    }

    #[test]
    fn erlang_ffi_keyword_call_resolves_module() {
        // `(Erlang lists) reverse: xs` — the keyword function send's module is
        // `lists`, recovered from the parenthesised receiver chain.
        let hits = find_all_sends_in_source("rev: xs => (Erlang lists) reverse: xs");
        let call = hits.iter().find(|h| h.selector == "reverse:").unwrap();
        assert_eq!(call.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(call.target_module.as_deref(), Some("lists"));
    }

    #[test]
    fn non_erlang_class_reference_is_other_not_ffi() {
        // A real class-reference receiver (`Integer new`) is `Other`, not FFI.
        let hits = find_all_sends_in_source("make => Integer new");
        let hit = hits.iter().find(|h| h.selector == "new").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        // Non-FFI sends never carry a target module.
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn erlang_class_protocol_selector_is_other_not_ffi() {
        // BT-3079 regression: `Erlang class` must NOT be reported as an FFI
        // call to a module named "class" — it dispatches to the class
        // protocol (metaclass), like `Erlang new`, `Erlang printString`, etc.
        let hits = find_all_sends_in_source("meta => Erlang class");
        let hit = hits.iter().find(|h| h.selector == "class").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn erlang_new_class_protocol_selector_is_other_not_ffi() {
        // Same as above for `new` — the other class-protocol selector most
        // likely to collide with a real Erlang module name.
        let hits = find_all_sends_in_source("make => Erlang new");
        let hit = hits.iter().find(|h| h.selector == "new").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn package_qualified_erlang_is_other_not_ffi() {
        // BT-3079 regression: `json@Erlang lists` names a package-scoped
        // `Erlang` class, not the compiler's built-in FFI bridge, so it must
        // not be tagged `ErlangFfi`.
        let hits = find_all_sends_in_source("go => json@Erlang lists");
        let hit = hits.iter().find(|h| h.selector == "lists").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn erlang_ffi_cascade_reuses_module_across_messages() {
        // BT-3079 regression: `(Erlang lists) reverse: xs; flatten: xs` — the
        // shared cascade receiver resolves to FFI module `lists` once and
        // both cascade messages (including the second, which never sees the
        // receiver syntactically) must carry it.
        let hits = find_all_sends_in_source("rev: xs => (Erlang lists) reverse: xs; flatten: xs");
        let reverse = hits.iter().find(|h| h.selector == "reverse:").unwrap();
        let flatten = hits.iter().find(|h| h.selector == "flatten:").unwrap();
        assert_eq!(reverse.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(reverse.target_module.as_deref(), Some("lists"));
        assert_eq!(flatten.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(flatten.target_module.as_deref(), Some("lists"));
    }

    #[test]
    fn erlang_class_protocol_cascade_is_other_not_ffi() {
        // BT-3079 regression: `Erlang class; foo` — the shared cascade
        // receiver's first message is the class-protocol selector `class`,
        // so neither cascade message may be tagged FFI.
        let hits = find_all_sends_in_source("meta => Erlang class; foo");
        let class_msg = hits.iter().find(|h| h.selector == "class").unwrap();
        let foo_msg = hits.iter().find(|h| h.selector == "foo").unwrap();
        assert_eq!(class_msg.receiver, ReceiverKind::Other);
        assert_eq!(class_msg.target_module, None);
        assert_eq!(foo_msg.receiver, ReceiverKind::Other);
        assert_eq!(foo_msg.target_module, None);
    }

    #[test]
    fn native_typed_signature_phantom_sends_are_skipped() {
        // The `native:` selector collides with the synthetic class grammar
        // (it is consumed as a class backing-module declaration), so the typed
        // return type `Symbol -> Symbol` parses into phantom `Symbol` / `->`
        // sends rooted in a parse error. Those must not be reported.
        let src = "native: m :: Symbol -> Symbol =>\n  self.backingModule := m";
        let hits = find_all_sends_in_source(src);
        let sels = selectors(&hits);
        assert!(
            !sels.contains(&"Symbol"),
            "phantom `Symbol` leaked: {sels:?}"
        );
        assert!(!sels.contains(&"->"), "phantom `->` leaked: {sels:?}");
    }

    #[test]
    fn error_rooted_send_is_skipped_but_clean_body_survives() {
        // A typed signature with a *non-colliding* selector parses cleanly and
        // its body sends are still collected.
        let hits = find_all_sends_in_source("store: m :: Symbol -> Symbol =>\n  self register: m");
        let sels = selectors(&hits);
        assert!(
            sels.contains(&"register:"),
            "clean body send lost: {sels:?}"
        );
        assert!(!sels.contains(&"Symbol"), "phantom leaked: {sels:?}");
    }

    #[test]
    fn perform_argument_symbol_is_not_a_send() {
        // The dynamic selector argument to `perform:` is a symbol literal, not
        // an AST send, so only `perform:` itself is collected — never the
        // symbol it carries. This is what makes the typo-finder's `perform:`
        // exclusion fall out for free.
        let hits = find_all_sends_in_source("run => self perform: #anythingXyzzy");
        let sels = selectors(&hits);
        assert!(sels.contains(&"perform:"), "got {sels:?}");
        assert!(!sels.contains(&"anythingXyzzy"), "got {sels:?}");
    }
}

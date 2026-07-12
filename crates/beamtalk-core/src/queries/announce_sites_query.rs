// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Announce-sites query — extract every `announce:` emission within a single
//! method's source, resolving each argument to its announcement class.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation announcementsSentBy:` (BT-2475) — the static dual of
//! the runtime-ETS `AnnouncementNavigation` (ADR 0093 §7). Where
//! `AnnouncementNavigation` answers "who *listens* for which events" from the
//! live subscription table, this query answers "which `Announcement` subclasses
//! does a class *emit*" by mining the `announce:` call sites out of the AST —
//! no new syntax, no drift.
//!
//! Recognises all three announce selectors: `announce:`, `announceAndWait:`, and
//! `announceAndWait:timeout:`. For each emission site it resolves the *event
//! argument* (the first keyword argument) to the name of the class being
//! instantiated — `announce: (PriceChanged newPrice: 42)` resolves to
//! `PriceChanged`, `announce: PriceChanged new` to `PriceChanged`.
//!
//! # Resolution is syntactic and advisory
//!
//! The argument is resolved by looking at the *immediate receiver* of the event
//! expression: a constructor send (`SomeClass selector: …` or `SomeClass new`)
//! whose receiver is a bare class reference resolves to that class. Anything else
//! — a bare identifier (`announce: someVar`), a literal, a chained send, or a
//! parenthesised non-constructor — is **unresolvable** ([`None`]). This matches
//! the issue's "advisory discoverability, not a sound guarantee" discipline:
//! `Dynamic`-typed and `perform:`-style indirection legitimately escape it, and
//! the caller treats unresolvable sites as a documented miss, never an error.
//!
//! Whether a resolved class name is actually an `Announcement` subclass is
//! decided by the caller (`SystemNavigation`), which has the live class
//! registry; this query only recovers the syntactic class name.
//!
//! # Parsing strategy
//!
//! Identical to [`crate::queries::all_sends_query`]: `CompiledMethod source`
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
const SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticAnnounceSitesScope\n";

/// The announce selectors recognised as emission sites. The event argument is
/// always the first keyword argument; `announceAndWait:timeout:` carries the
/// timeout as a second argument, which is ignored here.
const ANNOUNCE_SELECTORS: [&str; 3] = ["announce:", "announceAndWait:", "announceAndWait:timeout:"];

/// A single `announce:` emission site discovered while walking a method's AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnounceHit {
    /// The announce selector that was sent (`announce:`, `announceAndWait:`, or
    /// `announceAndWait:timeout:`).
    pub selector: String,
    /// 1-based line number within the input source where the emission appears.
    pub line: u32,
    /// The syntactically-resolved announcement class name (without a leading
    /// `#`), or [`None`] when the event argument is unresolvable (a bare
    /// identifier, literal, chained send, …).
    pub announcement_class: Option<String>,
}

/// Find every `announce:` / `announceAndWait:` / `announceAndWait:timeout:`
/// emission within `method_source`.
///
/// Walks the parsed AST for [`Expression::MessageSend`] and
/// [`Expression::Cascade`] messages whose selector is an announce selector and
/// emits one [`AnnounceHit`] per occurrence, recording the announce selector,
/// the 1-based line number (relative to the input), and the resolved
/// announcement class name (or [`None`]). Hits follow a stable pre-order walk.
///
/// Returns an empty vector if the source contains no emissions or cannot be
/// parsed at all.
#[must_use]
pub fn find_announce_sites_in_source(method_source: &str) -> Vec<AnnounceHit> {
    let wrapped = format!("{SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut hits = Vec::new();

    crate::ast_walker::for_each_expr_seq(&module, |seq| {
        for stmt in seq {
            collect_announce_sites(&stmt.expression, &wrapped, &mut hits);
        }
    });

    // Translate from wrapped-source line numbers back to input-source space.
    for hit in &mut hits {
        hit.line = hit.line.saturating_sub(PREFIX_LINES).max(1);
    }
    hits
}

/// Whether `selector_name` is one of the recognised announce selectors.
pub(crate) fn is_announce_selector(selector_name: &str) -> bool {
    ANNOUNCE_SELECTORS.contains(&selector_name)
}

/// Resolve an `announce:` event argument expression to its announcement class
/// name, syntactically.
///
/// Recognises the constructor-call idiom: a [`Expression::MessageSend`] whose
/// immediate receiver is a bare class reference (`PriceChanged newPrice: 42`,
/// `PriceChanged new`) resolves to that class. Parentheses are seen through.
/// Anything else is unresolvable ([`None`]).
fn resolve_announcement_class(arg: &Expression) -> Option<String> {
    match arg {
        Expression::Parenthesized { expression, .. } => resolve_announcement_class(expression),
        Expression::MessageSend { receiver, .. } => class_reference_name(receiver),
        _ => None,
    }
}

/// The name of a bare (non-FFI) class reference, seeing through parentheses.
///
/// `Erlang` is excluded so an `(Erlang module) fn:` chain — an Erlang call, not
/// an announcement constructor — never resolves to a phantom "Erlang" class.
fn class_reference_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::ClassReference { name, package, .. }
            if package.is_none() && name.name != "Erlang" =>
        {
            Some(name.name.to_string())
        }
        Expression::Parenthesized { expression, .. } => class_reference_name(expression),
        _ => None,
    }
}

/// Record an announce emission into `hits`, resolving its event argument.
///
/// `arguments` is the full argument list of the send; the event is the first
/// argument (the timeout of `announceAndWait:timeout:` is the second and is
/// ignored). A keyword send with no arguments cannot be an announce site, but is
/// handled defensively as unresolvable.
fn push_announce(
    selector: &crate::ast::MessageSelector,
    arguments: &[Expression],
    span: Span,
    source: &str,
    hits: &mut Vec<AnnounceHit>,
) {
    let announcement_class = arguments.first().and_then(resolve_announcement_class);
    hits.push(AnnounceHit {
        selector: selector.name().to_string(),
        line: selector_line(selector, span, source),
        announcement_class,
    });
}

/// Recursively collect every announce emission into `hits`.
fn collect_announce_sites(expr: &Expression, source: &str, hits: &mut Vec<AnnounceHit>) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            if is_announce_selector(selector.name().as_str()) {
                push_announce(selector, arguments, *span, source, hits);
            }
            collect_announce_sites(receiver, source, hits);
            for arg in arguments {
                collect_announce_sites(arg, source, hits);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_announce_sites(receiver, source, hits);
            for msg in messages {
                if is_announce_selector(msg.selector.name().as_str()) {
                    push_announce(&msg.selector, &msg.arguments, msg.span, source, hits);
                }
                for arg in &msg.arguments {
                    collect_announce_sites(arg, source, hits);
                }
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_announce_sites(target, source, hits);
            collect_announce_sites(value, source, hits);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_announce_sites(value, source, hits);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_announce_sites(&stmt.expression, source, hits);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_announce_sites(expression, source, hits);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_announce_sites(receiver, source, hits);
        }
        Expression::Match { value, arms, .. } => {
            collect_announce_sites(value, source, hits);
            for arm in arms {
                collect_pattern_announce_sites(&arm.pattern, source, hits);
                if let Some(guard) = &arm.guard {
                    collect_announce_sites(guard, source, hits);
                }
                collect_announce_sites(&arm.body, source, hits);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_announce_sites(inner, source, hits);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_announce_sites(element, source, hits);
            }
            if let Some(tail_expr) = tail {
                collect_announce_sites(tail_expr, source, hits);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_announce_sites(element, source, hits);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_announce_sites(&pair.key, source, hits);
                collect_announce_sites(&pair.value, source, hits);
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

/// Recursively collect announce emissions inside a [`Pattern`].
///
/// Mirrors `all_sends_query::collect_pattern_sends`: patterns carry no message
/// sends today, but the walker traverses container patterns and the
/// binary-segment size expression defensively so loosening the parser later does
/// not silently regress this query.
fn collect_pattern_announce_sites(pattern: &Pattern, source: &str, hits: &mut Vec<AnnounceHit>) {
    match pattern {
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_announce_sites(&segment.value, source, hits);
                if let Some(size) = &segment.size {
                    collect_announce_sites(size, source, hits);
                }
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_announce_sites(element, source, hits);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_announce_sites(element, source, hits);
            }
            if let Some(rest_pattern) = rest {
                collect_pattern_announce_sites(rest_pattern, source, hits);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_announce_sites(element, source, hits);
            }
            if let Some(tail_pattern) = tail {
                collect_pattern_announce_sites(tail_pattern, source, hits);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_announce_sites(&pair.value, source, hits);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_selector, inner) in keywords {
                collect_pattern_announce_sites(inner, source, hits);
            }
        }
        Pattern::Wildcard(..)
        | Pattern::Literal(..)
        | Pattern::Variable(..)
        | Pattern::Nil(..)
        | Pattern::Type { .. } => {}
    }
}

/// Determine the line number to report for an emission. Uses the span of the
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

    #[test]
    fn finds_announce_with_keyword_constructor_argument() {
        // `announce: (PriceChanged newPrice: 42)` resolves to PriceChanged.
        let hits = find_announce_sites_in_source(
            "emit => announcer announce: (PriceChanged newPrice: 42)",
        );
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "announce:");
        assert_eq!(hits[0].line, 1);
        assert_eq!(hits[0].announcement_class.as_deref(), Some("PriceChanged"));
    }

    #[test]
    fn finds_announce_with_unary_new_argument() {
        // `announce: PriceChanged new` resolves to PriceChanged.
        let hits = find_announce_sites_in_source("emit => announcer announce: PriceChanged new");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].announcement_class.as_deref(), Some("PriceChanged"));
    }

    #[test]
    fn recognises_announce_and_wait() {
        let hits = find_announce_sites_in_source(
            "emit => announcer announceAndWait: (OrderPlaced sku: \"x\")",
        );
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "announceAndWait:");
        assert_eq!(hits[0].announcement_class.as_deref(), Some("OrderPlaced"));
    }

    #[test]
    fn recognises_announce_and_wait_timeout_resolving_event_not_timeout() {
        // The event is the first keyword argument; the timeout (second) is
        // ignored.
        let hits = find_announce_sites_in_source(
            "emit => announcer announceAndWait: (OrderPlaced sku: \"x\") timeout: 1000",
        );
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "announceAndWait:timeout:");
        assert_eq!(hits[0].announcement_class.as_deref(), Some("OrderPlaced"));
    }

    #[test]
    fn collects_multiple_distinct_emissions() {
        let src = "emit =>\n  announcer announce: (EventA payload: 1)\n  announcer announce: (EventB payload: 2)";
        let hits = find_announce_sites_in_source(src);
        assert_eq!(hits.len(), 2);
        assert_eq!(hits[0].announcement_class.as_deref(), Some("EventA"));
        assert_eq!(hits[0].line, 2);
        assert_eq!(hits[1].announcement_class.as_deref(), Some("EventB"));
        assert_eq!(hits[1].line, 3);
    }

    #[test]
    fn identifier_argument_is_unresolvable() {
        // `announce: someVar` — the concrete class is unknown by construction.
        let hits = find_announce_sites_in_source("emit: e => announcer announce: e");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "announce:");
        assert_eq!(hits[0].announcement_class, None);
    }

    #[test]
    fn literal_argument_is_unresolvable() {
        let hits = find_announce_sites_in_source("emit => announcer announce: 42");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].announcement_class, None);
    }

    #[test]
    fn chained_send_argument_is_unresolvable() {
        // `announce: (self currentEvent reset)` — the immediate receiver is a
        // send, not a class reference, so the concrete class is unknown.
        let hits =
            find_announce_sites_in_source("emit => announcer announce: (self currentEvent reset)");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].announcement_class, None);
    }

    #[test]
    fn finds_emission_inside_block() {
        let src = "emit =>\n  items do: [:i | announcer announce: (ItemSeen id: i)]";
        let hits = find_announce_sites_in_source(src);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].announcement_class.as_deref(), Some("ItemSeen"));
        assert_eq!(hits[0].line, 2);
    }

    #[test]
    fn finds_emissions_in_cascade() {
        let src = "emit =>\n  announcer\n    announce: (EventA payload: 1);\n    announce: (EventB payload: 2)";
        let hits = find_announce_sites_in_source(src);
        let classes: Vec<Option<&str>> = hits
            .iter()
            .map(|h| h.announcement_class.as_deref())
            .collect();
        assert!(classes.contains(&Some("EventA")), "got {classes:?}");
        assert!(classes.contains(&Some("EventB")), "got {classes:?}");
    }

    #[test]
    fn non_announce_sends_are_ignored() {
        let hits = find_announce_sites_in_source("emit => self doSomething: (EventA payload: 1)");
        assert!(hits.is_empty(), "got {hits:?}");
    }

    #[test]
    fn class_side_emission_is_found() {
        let src = "emit -> Nil =>\n  announcer announce: (EventA payload: 1)";
        let hits = find_announce_sites_in_source(src);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].announcement_class.as_deref(), Some("EventA"));
        assert_eq!(hits[0].line, 2);
    }

    #[test]
    fn erlang_ffi_argument_does_not_resolve_to_phantom_class() {
        // `announce: (Erlang foo bar)` must not resolve to a phantom "Erlang".
        let hits = find_announce_sites_in_source("emit => announcer announce: (Erlang foo bar)");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].announcement_class, None);
    }

    #[test]
    fn no_emissions_returns_empty() {
        let hits = find_announce_sites_in_source("answer => 42");
        assert!(hits.is_empty());
    }

    #[test]
    fn unparseable_source_returns_empty() {
        let hits = find_announce_sites_in_source(")@!");
        assert!(hits.is_empty());
    }
}

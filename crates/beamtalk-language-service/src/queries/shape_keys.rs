// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Map-shape key completion (ADR 0147, BT-4735).
//!
//! Two completion sites offer the keys of a map shape:
//!
//! - **Keyed read.** After `at: #` (or `at: #pa`) on a receiver whose type is a
//!   shape — the first keyword of every shape-aware keyed selector
//!   (`at:`, `at:ifAbsent:`, `at:ifPresent:`, `at:ifPresent:ifAbsent:`,
//!   `at:put:`). Offers every key of the shape.
//! - **Map literal.** At a key position inside `#{ … }` whose expected type is
//!   a shape: the literal is the value of an annotated local
//!   (`o :: Options := #{ … }`), an argument of a send whose parameter is a
//!   shape, or a typed state default. Offers the keys not yet written.
//!
//! Both sites are *incomplete source*: `cfg at: #` and `#{#a => 1, ` do not
//! parse into the send or literal being typed. The cursor is found on tokens,
//! then a placeholder key is spliced in so the file parses and the type pass
//! types it normally; the receiver or the enclosing literal is then found by
//! the placeholder.

use crate::queries::{enrich_hierarchy_with_inferred_returns, type_of_parsed};
use crate::{Completion, CompletionKind};
use beamtalk_core::ast::{Expression, Literal, MapPair, Module};
use beamtalk_core::ast_walker::{for_each_expr_seq, walk_expression};
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use beamtalk_core::semantic_analysis::type_checker::{
    NativeTypeRegistry, TypeMap, is_shape_keyed_selector,
};
use beamtalk_core::semantic_analysis::{
    AliasRegistry, ClassHierarchy, InferredType, ProtocolRegistry,
};
use beamtalk_core::source_analysis::{TokenKind, lex, lex_with_eof, parse};
use ecow::EcoString;
use std::fmt::Write;
use std::sync::Arc;

/// The symbol spliced in at the cursor so the file parses. Not a valid
/// identifier a user would type, and never offered back.
const PLACEHOLDER: &str = "__bt_key_cursor__";

/// Where the cursor sits relative to a shape key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SiteKind {
    /// `recv at: #|` — the key argument of a keyed selector.
    KeyedRead,
    /// `#{ …, |` — a key position inside a map literal.
    Literal,
}

#[derive(Debug, Clone, Copy)]
struct KeySite {
    kind: SiteKind,
    /// Byte offset where the key being typed begins (the `#`), or the cursor
    /// when nothing is typed yet. The spliced placeholder replaces
    /// `start..cursor`.
    start: u32,
}

/// Classifies the cursor as a shape-key site from the tokens before it.
fn find_key_site(source: &str, offset: u32) -> Option<KeySite> {
    let before = source.get(..offset as usize)?;
    let tokens = lex(before);
    let mut prior: &[_] = &tokens;
    let mut start = offset;
    if let Some(last) = tokens.last() {
        // A key being typed: a lone `#`, or `#par` — a symbol ending at the cursor.
        if matches!(last.kind(), TokenKind::Hash | TokenKind::Symbol(_))
            && last.span().end() == offset
        {
            start = last.span().start();
            prior = &tokens[..tokens.len() - 1];
        }
    }
    let typed_something = start != offset;
    let last = prior.last()?;
    if typed_something && matches!(last.kind(), TokenKind::Keyword(k) if k.as_str() == "at:") {
        return Some(KeySite {
            kind: SiteKind::KeyedRead,
            start,
        });
    }
    let at_key_position = match last.kind() {
        TokenKind::MapOpen => true,
        TokenKind::BinarySelector(s) if s.as_str() == "," => innermost_open_is_map(prior),
        _ => false,
    };
    at_key_position.then_some(KeySite {
        kind: SiteKind::Literal,
        start,
    })
}

/// Whether the innermost unclosed bracket in `tokens` is a `#{`.
fn innermost_open_is_map(tokens: &[beamtalk_core::source_analysis::Token]) -> bool {
    let mut stack: Vec<bool> = Vec::new();
    for token in tokens {
        match token.kind() {
            TokenKind::MapOpen => stack.push(true),
            TokenKind::LeftBrace
            | TokenKind::LeftBracket
            | TokenKind::LeftParen
            | TokenKind::ListOpen
            | TokenKind::ArrayOpen => stack.push(false),
            TokenKind::RightBrace | TokenKind::RightBracket | TokenKind::RightParen => {
                stack.pop();
            }
            _ => {}
        }
    }
    stack.last().copied().unwrap_or(false)
}

/// Shape-key completions at `offset`, or `None` when the cursor is not at a
/// shape-key site or the expected type there is not a shape (the caller then
/// completes as usual).
#[allow(clippy::too_many_arguments)] // project context threaded to the type pass
pub(crate) fn compute_shape_key_completions(
    source: &str,
    offset: u32,
    hierarchy: &ClassHierarchy,
    native_types: Option<&Arc<NativeTypeRegistry>>,
    alias_registry: Option<&Arc<AliasRegistry>>,
    protocol_registry: Option<Arc<ProtocolRegistry>>,
) -> Option<Vec<Completion>> {
    let site = find_key_site(source, offset)?;
    let after_cursor = source.get(offset as usize..)?;
    // The cursor may sit inside a symbol (`#po|rt`): the placeholder replaces
    // the whole symbol, not just the part before the cursor.
    let tail_len = after_cursor
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == ':'))
        .unwrap_or(after_cursor.len());
    let rest = if site.start == offset {
        after_cursor
    } else {
        &after_cursor[tail_len..]
    };
    let spliced = match site.kind {
        SiteKind::KeyedRead => format!("#{PLACEHOLDER}"),
        SiteKind::Literal => {
            // A whole pair, so `#{ | }` parses. Nothing more when the key was
            // already followed by its value (`#po| => 1`) or a separator;
            // a separator only when more pairs follow the cursor.
            let next = rest.trim_start();
            if next.starts_with("=>") {
                format!("#{PLACEHOLDER}")
            } else if matches!(next.chars().next(), None | Some('}' | ',')) {
                format!("#{PLACEHOLDER} => nil")
            } else {
                format!("#{PLACEHOLDER} => nil, ")
            }
        }
    };
    let patched = format!("{}{spliced}{rest}", source.get(..site.start as usize)?);
    let (module, _) = parse(lex_with_eof(&patched));

    let protocol_registry =
        crate::queries::protocols_for_module(protocol_registry, &module, hierarchy);
    let aliases = alias_registry
        .cloned()
        .unwrap_or_else(|| Arc::new(AliasRegistry::from_module_declarations(&module)));
    let (enriched, type_map) = enrich_hierarchy_with_inferred_returns(
        &module,
        hierarchy,
        native_types,
        &protocol_registry,
        Some(&aliases),
    );
    let hierarchy = enriched.as_ref().unwrap_or(hierarchy);

    match site.kind {
        SiteKind::KeyedRead => keyed_read_completions(&module, &type_map),
        SiteKind::Literal => literal_completions(&module, &type_map, hierarchy, &aliases),
    }
}

// ── keyed read ──────────────────────────────────────────────────────────

fn is_placeholder_symbol(expr: &Expression) -> bool {
    matches!(expr, Expression::Literal(Literal::Symbol(s), _) if s.as_str() == PLACEHOLDER)
}

fn keyed_read_completions(module: &Module, type_map: &TypeMap) -> Option<Vec<Completion>> {
    let mut receiver_ty: Option<InferredType> = None;
    for_each_expr_seq(module, |body| {
        for stmt in body {
            walk_expression(&stmt.expression, &mut |expr| {
                let Expression::MessageSend {
                    receiver,
                    selector,
                    arguments,
                    ..
                } = expr
                else {
                    return;
                };
                if receiver_ty.is_none()
                    && arguments.first().is_some_and(is_placeholder_symbol)
                    && is_shape_keyed_selector(&selector.name())
                {
                    receiver_ty = type_of_parsed(type_map, receiver).cloned();
                }
            });
        }
    });
    let fields = merged_keys(&receiver_ty?)?;
    Some(fields.iter().map(key_completion).collect())
}

/// One key offered for completion: the value types the shapes in play give
/// it (several only when a union receiver's shapes disagree) and whether it
/// may be absent.
struct KeyInfo {
    key: EcoString,
    values: Vec<InferredType>,
    optional: bool,
}

/// The keys of every shape in `ty` (a shape, or a union holding shapes —
/// `Nil` and other members are skipped). A key several shapes name is listed
/// once, optional if any shape makes it so.
fn merged_keys(ty: &InferredType) -> Option<Vec<KeyInfo>> {
    let members: Vec<&InferredType> = match ty {
        InferredType::Union { members, .. } => members.iter().collect(),
        other => vec![other],
    };
    let mut out: Vec<KeyInfo> = Vec::new();
    let mut any = false;
    for member in members {
        let InferredType::Shape { fields, .. } = member else {
            continue;
        };
        any = true;
        for f in fields {
            match out.iter_mut().find(|o| o.key == f.key) {
                Some(existing) => {
                    if !existing.values.contains(&f.value) {
                        existing.values.push(f.value.clone());
                    }
                    existing.optional |= f.optional;
                }
                None => out.push(KeyInfo {
                    key: f.key.clone(),
                    values: vec![f.value.clone()],
                    optional: f.optional,
                }),
            }
        }
    }
    any.then_some(out)
}

/// The value types of one key, as a union when several shapes disagree.
fn display_values(values: &[InferredType]) -> String {
    values
        .iter()
        .map(|v| {
            v.display_for_diagnostic()
                .unwrap_or_else(|| EcoString::from("Dynamic"))
        })
        .collect::<Vec<_>>()
        .join(" | ")
}

/// A completion for one shape key: the label is the key as written after
/// `#`, the detail its value type and optionality.
fn key_completion(info: &KeyInfo) -> Completion {
    let value = display_values(&info.values);
    let (detail, doc) = if info.optional {
        (
            format!("{value} (optional)"),
            format!("Optional shape key `#{}`: {value}, or absent", info.key),
        )
    } else {
        (
            value.clone(),
            format!("Required shape key `#{}`: {value}", info.key),
        )
    };
    Completion::new(symbol_label(&info.key), CompletionKind::Field)
        .with_detail(detail)
        .with_documentation(doc)
}

/// The text that follows `#` for symbol `name`: bare when it lexes as an
/// identifier or keyword symbol, quoted otherwise.
fn symbol_label(name: &str) -> String {
    let plain = !name.is_empty()
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':')
        && !name.starts_with(|c: char| c.is_ascii_digit());
    if plain {
        name.to_string()
    } else {
        format!("'{name}'")
    }
}

// ── map literal ─────────────────────────────────────────────────────────

fn contains_placeholder_pair(expr: &Expression) -> bool {
    let mut found = false;
    walk_expression(expr, &mut |e| {
        if let Expression::MapLiteral { pairs, .. } = e {
            found |= pairs.iter().any(|p| is_placeholder_symbol(&p.key));
        }
    });
    found
}

fn literal_completions(
    module: &Module,
    type_map: &TypeMap,
    hierarchy: &ClassHierarchy,
    aliases: &AliasRegistry,
) -> Option<Vec<Completion>> {
    let mut site: Option<(Vec<EcoString>, InferredType)> = None;
    let mut consider = |value: &Expression, expected: &InferredType| {
        if site.is_none() && contains_placeholder_pair(value) {
            site = expected_literal(value, expected);
        }
    };

    // Annotated local: `o :: Options := #{ … }`.
    for_each_expr_seq(module, |body| {
        for stmt in body {
            walk_expression(&stmt.expression, &mut |expr| match expr {
                Expression::Assignment {
                    value,
                    type_annotation: Some(annotation),
                    ..
                } => {
                    let expected = aliases.resolve_declared(&DeclaredType::from(annotation));
                    consider(value, &expected);
                }
                Expression::MessageSend {
                    receiver,
                    selector,
                    arguments,
                    ..
                } => {
                    let Some(method) =
                        receiver_method(receiver, &selector.name(), type_map, hierarchy)
                    else {
                        return;
                    };
                    for (arg, param) in arguments.iter().zip(&method) {
                        if let Some(declared) = param {
                            consider(arg, &aliases.resolve_declared(declared));
                        }
                    }
                }
                _ => {}
            });
        }
    });
    // Typed state default: `state: o :: Options = #{ … }`.
    for class in &module.classes {
        for state in &class.state {
            if let (Some(annotation), Some(default)) =
                (&state.type_annotation, &state.default_value)
            {
                let expected = aliases.resolve_declared(&DeclaredType::from(annotation));
                consider(default, &expected);
            }
        }
    }

    let (written, shape) = site?;
    let fields = merged_keys(&shape)?;
    Some(
        fields
            .iter()
            .filter(|f| !written.contains(&f.key))
            .map(key_completion)
            .collect(),
    )
}

/// Declared parameter types of the method `selector` on `receiver`'s type.
fn receiver_method(
    receiver: &Expression,
    selector: &str,
    type_map: &TypeMap,
    hierarchy: &ClassHierarchy,
) -> Option<Vec<Option<DeclaredType>>> {
    let ty = type_of_parsed(type_map, receiver)?;
    let info = match ty {
        InferredType::Meta { class_name, .. } => hierarchy.find_class_method(class_name, selector),
        other => {
            let (class_name, _) = other.instance_class()?;
            hierarchy.find_method(class_name, selector)
        }
    }?;
    Some(info.param_types)
}

/// The literal holding the placeholder inside `value` (descending through
/// nested literals whose expected key type is itself a shape), with the keys
/// already written in it and the shape it is expected to have.
fn expected_literal(
    value: &Expression,
    expected: &InferredType,
) -> Option<(Vec<EcoString>, InferredType)> {
    let Expression::MapLiteral { pairs, .. } = value.unwrap_parens() else {
        return None;
    };
    if pairs.iter().any(|p| is_placeholder_symbol(&p.key)) {
        merged_keys(expected)?;
        return Some((written_keys(pairs), expected.clone()));
    }
    let fields = merged_keys(expected)?;
    pairs.iter().find_map(|pair| {
        let Expression::Literal(Literal::Symbol(key), _) = &pair.key else {
            return None;
        };
        let info = fields.iter().find(|f| f.key == *key)?;
        info.values
            .iter()
            .find_map(|value| expected_literal(&pair.value, value))
    })
}

fn written_keys(pairs: &[MapPair]) -> Vec<EcoString> {
    pairs
        .iter()
        .filter_map(|p| match &p.key {
            Expression::Literal(Literal::Symbol(s), _) if s.as_str() != PLACEHOLDER => {
                Some(s.clone())
            }
            _ => None,
        })
        .collect()
}

// ── hover ───────────────────────────────────────────────────────────────

/// Hover text for the symbol `key` read from, or written in, a value of type
/// `owner`: the key's value type, and that it may be absent when the shape
/// makes it optional. `None` when `owner` holds no shape naming `key`.
///
/// `named_owner` adds "of `<owner>`" — right for a keyed read (`o at: #k`,
/// where `o`'s type is the thing to name) and noise for a literal's own
/// pairs, whose owner is the literal.
pub(crate) fn key_hover_text(key: &str, owner: &InferredType, named_owner: bool) -> Option<String> {
    let info = merged_keys(owner)?.into_iter().find(|k| k.key == key)?;
    let value = display_values(&info.values);
    let mut text = format!("Shape key: `#{key}`");
    if named_owner {
        if let Some(owner) = owner.display_for_diagnostic() {
            let _ = write!(text, " of `{owner}`");
        }
    }
    let _ = write!(text, " — Type: {value}");
    if info.optional {
        text.push_str(" (optional: may be absent)");
    }
    Some(text)
}

/// `key_hover_text` for the key argument of a keyed send, when `selector` is
/// shape-keyed.
pub(crate) fn keyed_send_hover_text(
    selector: &str,
    key_arg: &Expression,
    receiver_ty: &InferredType,
) -> Option<String> {
    if !is_shape_keyed_selector(selector) {
        return None;
    }
    let Expression::Literal(Literal::Symbol(key), _) = key_arg else {
        return None;
    };
    key_hover_text(key, receiver_ty, true)
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type-annotation unparsing.
//! Split from `mod.rs`; see the module docs there.

use super::{
    Document, Literal, ShapeField, TypeAnnotation, break_, concat, docvec, group, join_docs, leaf,
    nest, nil, unparse_literal,
};

// --- Type annotation unparsing ---

pub(super) fn unparse_type_annotation(ty: &TypeAnnotation) -> Document<'static> {
    match ty {
        TypeAnnotation::Simple(id) => leaf::ident(&id.name),
        TypeAnnotation::Singleton { name, .. } => {
            docvec!["#", leaf::ident(name)]
        }
        TypeAnnotation::Union { types, .. } => {
            let type_docs: Vec<Document<'static>> =
                types.iter().map(unparse_type_annotation).collect();
            join_docs(type_docs, " | ")
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            let param_docs: Vec<Document<'static>> =
                parameters.iter().map(unparse_type_annotation).collect();
            let joined = join_docs(param_docs, ", ");
            docvec![leaf::ident(&base.name), "(", joined, ")"]
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            // A union/`FalseOr` inner needs grouping to match `type_name`.
            let needs_parens = matches!(
                inner.as_ref(),
                TypeAnnotation::Union { .. } | TypeAnnotation::FalseOr { .. }
            );
            docvec![unparse_grouped_type(inner, needs_parens), " | False"]
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            // Re-derive grouping parens where re-parsing would
            // otherwise change the AST. The predicate is shared with
            // `TypeAnnotation::type_name` — single source of truth.
            docvec![
                unparse_grouped_type(base, base.needs_parens_in_difference(false)),
                " \\ ",
                unparse_grouped_type(excluded, excluded.needs_parens_in_difference(true))
            ]
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            // Mirror image of `Difference` above; predicate shared with
            // `TypeAnnotation::type_name`.
            docvec![
                unparse_grouped_type(left, left.needs_parens_in_intersection(false)),
                " & ",
                unparse_grouped_type(right, right.needs_parens_in_intersection(true))
            ]
        }
        TypeAnnotation::SelfType { .. } => Document::Str("Self"),
        TypeAnnotation::SelfClass { .. } => Document::Str("Self class"),
        TypeAnnotation::ClassOf { class_name, .. } => {
            docvec![leaf::ident(&class_name.name), " class"]
        }
        TypeAnnotation::Shape { fields, open, .. } => unparse_shape_type(fields, *open),
    }
}

/// Unparses a map-shape type `#{#a => T, ?#b => U, ...}` (ADR 0147).
///
/// Mirrors the map-literal layout: inline when it fits, otherwise one entry
/// per line.
fn unparse_shape_type(fields: &[ShapeField], open: bool) -> Document<'static> {
    let mut entries: Vec<Document<'static>> = fields
        .iter()
        .map(|f| {
            docvec![
                if f.optional { "?" } else { "" },
                unparse_literal(&Literal::Symbol(f.key.clone())),
                " => ",
                unparse_type_annotation(&f.value)
            ]
        })
        .collect();
    if open {
        entries.push(Document::Str("..."));
    }
    if entries.is_empty() {
        return Document::Str("#{}");
    }
    let mut body = Vec::new();
    for (i, entry) in entries.into_iter().enumerate() {
        if i > 0 {
            body.push(Document::Str(","));
            body.push(break_("", " "));
        } else {
            body.push(break_("", ""));
        }
        body.push(entry);
    }
    group(docvec!["#{", nest(2, concat(body)), break_("", ""), "}"])
}

/// Unparses a `\`/`&` operand, wrapping it in grouping parentheses
/// when `parens` is set — i.e. when re-parsing the bare operand
/// would bind differently (see the `Difference`/`Intersection` arms of
/// [`unparse_type_annotation`]).
fn unparse_grouped_type(ty: &TypeAnnotation, parens: bool) -> Document<'static> {
    if parens {
        docvec!["(", unparse_type_annotation(ty), ")"]
    } else {
        unparse_type_annotation(ty)
    }
}

pub(super) fn unparse_type_annotation_opt(ty: Option<&TypeAnnotation>) -> Document<'static> {
    if let Some(t) = ty {
        docvec![" :: ", unparse_type_annotation(t)]
    } else {
        nil()
    }
}

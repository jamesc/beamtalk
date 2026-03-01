// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Hover provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `HoverProvider` from the DDD model.
//! It shows information on hover over symbols, literals, and keywords in the
//! editor. The provider follows LSP terminology and aligns with the ubiquitous
//! language defined in `docs/beamtalk-ddd-model.md`.
//!
//! # Design
//!
//! Hover information shows:
//! - Type signatures (future work - requires type system)
//! - Documentation comments
//! - Value information for constants
//!
//! # Performance
//!
//! Must respond in <50ms for typical file sizes.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: Language Server Protocol hover requests

use crate::ast::{
    ClassDefinition, Expression, Literal, MessageSelector, MethodDefinition, Module, to_module_name,
};
use crate::language_service::{HoverInfo, Position};
use crate::semantic_analysis::type_checker::TypeMap;
use crate::semantic_analysis::{ClassHierarchy, InferredType, infer_types};
use crate::source_analysis::Span;

/// Computes hover information at a given position.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `source` - The source text
/// * `position` - The cursor position
/// * `hierarchy` - The class hierarchy for type context
///
/// # Returns
///
/// Hover information if the position is over a relevant symbol, `None` otherwise.
///
/// # Examples
///
/// ```
/// use beamtalk_core::queries::hover_provider::compute_hover;
/// use beamtalk_core::language_service::Position;
/// use beamtalk_core::semantic_analysis::ClassHierarchy;
/// use beamtalk_core::source_analysis::{lex_with_eof, parse};
///
/// let source = "x := 42";
/// let tokens = lex_with_eof(source);
/// let (module, _) = parse(tokens);
/// let hierarchy = ClassHierarchy::build(&module).0.unwrap();
///
/// let hover = compute_hover(&module, source, Position::new(0, 0), &hierarchy);
/// assert!(hover.is_some());
/// ```
#[must_use]
pub fn compute_hover(
    module: &Module,
    source: &str,
    position: Position,
    hierarchy: &ClassHierarchy,
) -> Option<HoverInfo> {
    let offset = position.to_byte_offset(source)?;
    let offset_val = offset.get();

    // Find the class context at this position for self-type hover
    let class_context = find_hover_class_context(module, offset_val);

    // Run type inference to get types at positions
    let type_map = infer_types(module, hierarchy);

    // Find declaration-level symbols (class names, superclasses, standalone method qualifiers)
    if let Some(hover) = find_hover_in_declarations(module, offset_val, hierarchy) {
        return Some(hover);
    }

    // Find the expression at this position
    for stmt in &module.expressions {
        if let Some(hover) = find_hover_in_expr(
            &stmt.expression,
            offset_val,
            &class_context,
            hierarchy,
            &type_map,
        ) {
            return Some(hover);
        }
    }

    // Also search inside class method bodies
    for class in &module.classes {
        for method in &class.methods {
            if let Some(hover) = find_hover_on_method_signature(method, source, offset_val) {
                return Some(hover);
            }
            for body_stmt in &method.body {
                if let Some(hover) = find_hover_in_expr(
                    &body_stmt.expression,
                    offset_val,
                    &class_context,
                    hierarchy,
                    &type_map,
                ) {
                    return Some(hover);
                }
            }
        }
        for method in &class.class_methods {
            if let Some(hover) = find_hover_on_method_signature(method, source, offset_val) {
                return Some(hover);
            }
            for body_stmt in &method.body {
                if let Some(hover) = find_hover_in_expr(
                    &body_stmt.expression,
                    offset_val,
                    &class_context,
                    hierarchy,
                    &type_map,
                ) {
                    return Some(hover);
                }
            }
        }
    }

    // Also search standalone method definitions (Tonel-style)
    for smd in &module.method_definitions {
        if let Some(hover) = find_hover_on_method_signature(&smd.method, source, offset_val) {
            return Some(hover);
        }
        for body_stmt in &smd.method.body {
            if let Some(hover) = find_hover_in_expr(
                &body_stmt.expression,
                offset_val,
                &class_context,
                hierarchy,
                &type_map,
            ) {
                return Some(hover);
            }
        }
    }

    None
}

/// Finds hover information for declaration-level identifiers that are not expressions.
fn find_hover_in_declarations(
    module: &Module,
    offset: u32,
    hierarchy: &ClassHierarchy,
) -> Option<HoverInfo> {
    for class in &module.classes {
        if offset >= class.name.span.start() && offset < class.name.span.end() {
            return Some(class_reference_hover_info(
                &class.name.name,
                class.name.span,
                hierarchy,
            ));
        }
        if let Some(superclass) = &class.superclass {
            if offset >= superclass.span.start() && offset < superclass.span.end() {
                return Some(class_reference_hover_info(
                    &superclass.name,
                    superclass.span,
                    hierarchy,
                ));
            }
        }
    }

    for smd in &module.method_definitions {
        if offset >= smd.class_name.span.start() && offset < smd.class_name.span.end() {
            return Some(class_reference_hover_info(
                &smd.class_name.name,
                smd.class_name.span,
                hierarchy,
            ));
        }
    }

    None
}

/// Returns hover info when the cursor is over method declaration syntax.
///
/// Includes selector, parameter names, parameter types, and return type annotations.
fn find_hover_on_method_signature(
    method: &MethodDefinition,
    source: &str,
    offset: u32,
) -> Option<HoverInfo> {
    for parameter in &method.parameters {
        let parameter_span = parameter_name_span_in_signature(parameter.name.span, source);
        if offset >= parameter_span.start() && offset < parameter_span.end() {
            let mut hover = HoverInfo::new(
                format!("Parameter: `{}`", parameter.name.name),
                parameter_span,
            );
            if let Some(type_annotation) = &parameter.type_annotation {
                hover = hover.with_documentation(format!(
                    "Type: `{}`",
                    type_annotation_label(type_annotation)
                ));
            }
            return Some(hover);
        }

        if let Some(type_annotation) = &parameter.type_annotation {
            let type_span = type_annotation.span();
            if offset >= type_span.start() && offset < type_span.end() {
                return Some(HoverInfo::new(
                    format!(
                        "Type annotation: `{}`",
                        type_annotation_label(type_annotation)
                    ),
                    type_span,
                ));
            }
        }
    }

    if let Some(return_type) = &method.return_type {
        let return_span = return_type.span();
        if offset >= return_span.start() && offset < return_span.end() {
            return Some(HoverInfo::new(
                format!("Return type: `{}`", type_annotation_label(return_type)),
                return_span,
            ));
        }
    }

    let selector_span = selector_span_in_method_signature(method, source)?;
    if offset >= selector_span.start() && offset < selector_span.end() {
        Some(method_declaration_selector_hover_info(
            method,
            selector_span,
        ))
    } else {
        None
    }
}

/// Creates hover info for a method declaration selector with signature + docs.
fn method_declaration_selector_hover_info(method: &MethodDefinition, span: Span) -> HoverInfo {
    let signature = method_signature(method);
    let mut hover = HoverInfo::new(format!("Method: `{signature}`"), span);

    if let Some(doc_comment) = method
        .doc_comment
        .as_deref()
        .map(str::trim)
        .filter(|doc| !doc.is_empty())
    {
        hover = hover.with_documentation(doc_comment.to_string());
    } else {
        hover = hover.with_documentation(format!(
            "Selector: `{}` (arity: {})",
            method.selector.name(),
            method.selector.arity()
        ));
    }

    hover
}

fn method_signature(method: &MethodDefinition) -> String {
    let base = selector_with_parameters(&method.selector, &method.parameters);
    if let Some(return_type) = &method.return_type {
        format!("{base} -> {}", type_annotation_label(return_type))
    } else {
        base
    }
}

fn selector_with_parameters(
    selector: &MessageSelector,
    parameters: &[crate::ast::ParameterDefinition],
) -> String {
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => {
            let mut signature = op.to_string();
            if let Some(parameter) = parameters.first() {
                signature.push(' ');
                signature.push_str(&parameter_signature(parameter));
            }
            signature
        }
        MessageSelector::Keyword(parts) => {
            let mut fragments = Vec::with_capacity(parts.len());
            for (index, part) in parts.iter().enumerate() {
                let mut fragment = part.keyword.to_string();
                if let Some(parameter) = parameters.get(index) {
                    fragment.push(' ');
                    fragment.push_str(&parameter_signature(parameter));
                }
                fragments.push(fragment);
            }
            fragments.join(" ")
        }
    }
}

fn parameter_signature(parameter: &crate::ast::ParameterDefinition) -> String {
    let mut text = parameter.name.name.to_string();
    if let Some(type_annotation) = &parameter.type_annotation {
        text.push_str(": ");
        text.push_str(&type_annotation_label(type_annotation));
    }
    text
}

/// Extends a parameter-name span to include trailing `:` in signatures like `aBlock:`.
fn parameter_name_span_in_signature(span: Span, source: &str) -> Span {
    let end = span.end() as usize;
    if end < source.len() && source.as_bytes()[end] == b':' {
        Span::new(span.start(), span.end() + 1)
    } else {
        span
    }
}

fn type_annotation_label(type_annotation: &crate::ast::TypeAnnotation) -> String {
    match type_annotation {
        crate::ast::TypeAnnotation::Simple(id) => id.name.to_string(),
        crate::ast::TypeAnnotation::Singleton { name, .. } => format!("#{name}"),
        crate::ast::TypeAnnotation::Union { types, .. } => types
            .iter()
            .map(type_annotation_label)
            .collect::<Vec<_>>()
            .join(" | "),
        crate::ast::TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            let params = parameters
                .iter()
                .map(type_annotation_label)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{params}>", base.name)
        }
        crate::ast::TypeAnnotation::FalseOr { inner, .. } => {
            format!("{}?", type_annotation_label(inner))
        }
    }
}

/// Computes the selector span inside a method signature.
///
/// For keyword selectors, spans are taken directly from `KeywordPart` metadata.
/// For unary/binary selectors, we find the selector lexeme in the method header slice.
fn selector_span_in_method_signature(method: &MethodDefinition, source: &str) -> Option<Span> {
    match &method.selector {
        MessageSelector::Keyword(parts) => {
            let first = parts.first()?;
            let last = parts.last()?;
            Some(first.span.merge(last.span))
        }
        MessageSelector::Unary(name) | MessageSelector::Binary(name) => {
            let method_start = usize::try_from(method.span.start()).ok()?;
            let method_end = usize::try_from(method.span.end()).ok()?;
            if method_start >= source.len()
                || method_end > source.len()
                || method_start >= method_end
            {
                return None;
            }

            // Restrict search to header area up to first body expression, when present.
            let header_end_u32 = method
                .body
                .first()
                .map_or(method.span.end(), |stmt| stmt.expression.span().start());
            let header_end = usize::try_from(header_end_u32).ok()?.min(source.len());
            if method_start >= header_end {
                return None;
            }

            let header = &source[method_start..header_end];
            let selector = name.as_str();
            let rel = header.find(selector)?;
            let start = method_start + rel;
            let end = start + selector.len();

            let start_u32 = u32::try_from(start).ok()?;
            let end_u32 = u32::try_from(end).ok()?;
            Some(Span::new(start_u32, end_u32))
        }
    }
}

/// The hover-relevant class context at a position.
#[derive(Debug, Clone)]
enum HoverClassContext<'a> {
    InstanceMethod(&'a ClassDefinition),
    ClassMethod(&'a ClassDefinition),
    /// Inside a standalone instance method (`Counter >> method => ...`).
    StandaloneInstanceMethod(&'a str),
    /// Inside a standalone class method (`Counter class >> method => ...`).
    StandaloneClassMethod(&'a str),
    TopLevel,
}

/// Find which class/method the offset is inside of.
fn find_hover_class_context(module: &Module, offset: u32) -> HoverClassContext<'_> {
    for class in &module.classes {
        for method in &class.methods {
            if offset >= method.span.start() && offset < method.span.end() {
                return HoverClassContext::InstanceMethod(class);
            }
        }
        for method in &class.class_methods {
            if offset >= method.span.start() && offset < method.span.end() {
                return HoverClassContext::ClassMethod(class);
            }
        }
    }
    for smd in &module.method_definitions {
        if offset >= smd.method.span.start() && offset < smd.method.span.end() {
            if smd.is_class_method {
                return HoverClassContext::StandaloneClassMethod(&smd.class_name.name);
            }
            return HoverClassContext::StandaloneInstanceMethod(&smd.class_name.name);
        }
    }
    HoverClassContext::TopLevel
}

/// Creates hover information for a message selector.
fn selector_hover_info(selector: &MessageSelector, span: Span) -> HoverInfo {
    let (kind, name, arity) = match selector {
        MessageSelector::Unary(name) => ("Unary message", name.as_str(), 0),
        MessageSelector::Binary(op) => ("Binary message", op.as_str(), 1),
        MessageSelector::Keyword(parts) => {
            let name = selector.name();
            return HoverInfo::new(
                format!("Keyword message: `{name}` (arity: {})", parts.len()),
                span,
            )
            .with_documentation("Keyword messages have named arguments ending in ':'");
        }
    };
    HoverInfo::new(format!("{kind}: `{name}` (arity: {arity})"), span)
}

/// Recursively searches for hover information in an expression.
#[expect(clippy::too_many_lines, reason = "match on Expression variants")]
fn find_hover_in_expr(
    expr: &Expression,
    offset: u32,
    context: &HoverClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<HoverInfo> {
    let span = expr.span();
    if offset < span.start() || offset >= span.end() {
        return None;
    }

    match expr {
        Expression::Identifier(ident) => {
            if offset >= ident.span.start() && offset < ident.span.end() {
                // Special handling for `self` - show type context
                if ident.name == "self" {
                    return Some(self_hover_info(ident.span, context));
                }
                // Show inferred type if available
                let type_info = type_map.get(ident.span).and_then(InferredType::as_known);
                let contents = if let Some(ty) = type_info {
                    format!("Identifier: `{}` — Type: {ty}", ident.name)
                } else {
                    format!("Identifier: `{}`", ident.name)
                };
                Some(HoverInfo::new(contents, ident.span))
            } else {
                None
            }
        }
        Expression::ClassReference { name, span } => {
            if offset >= span.start() && offset < span.end() {
                Some(class_reference_hover_info(&name.name, *span, hierarchy))
            } else {
                None
            }
        }
        Expression::Super(span) => {
            if offset >= span.start() && offset < span.end() {
                Some(HoverInfo::new(
                    "Keyword: `super` (superclass method dispatch)".to_string(),
                    *span,
                ))
            } else {
                None
            }
        }
        Expression::Literal(lit, span) => {
            if offset >= span.start() && offset < span.end() {
                let info = match lit {
                    Literal::Integer(n) => format!("Integer: `{n}`"),
                    Literal::Float(f) => format!("Float: `{f}`"),
                    Literal::String(s) => format!("String: `\"{s}\"`"),
                    Literal::Symbol(s) => format!("Symbol: `#{s}`"),
                    Literal::List(_) => "List literal".to_string(),
                    Literal::Character(c) => format!("Character: `${c}`"),
                };
                Some(HoverInfo::new(info, *span))
            } else {
                None
            }
        }
        Expression::Assignment { target, value, .. } => {
            find_hover_in_expr(target, offset, context, hierarchy, type_map)
                .or_else(|| find_hover_in_expr(value, offset, context, hierarchy, type_map))
        }
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            // First check receiver and arguments
            if let Some(hover) = find_hover_in_expr(receiver, offset, context, hierarchy, type_map)
            {
                return Some(hover);
            }
            if let Some(hover) = arguments
                .iter()
                .find_map(|arg| find_hover_in_expr(arg, offset, context, hierarchy, type_map))
            {
                return Some(hover);
            }

            // At this point we know we're not hovering over the receiver or any argument.
            // Compute a more precise selector span based on selector type.
            let receiver_span = receiver.span();
            let in_selector = match selector {
                // For unary and binary messages, the selector appears between the end
                // of the receiver and the start of the first argument (if any), or
                // up to the end of the message send when there are no arguments.
                MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                    let start = receiver_span.end();
                    let end = arguments
                        .first()
                        .map_or_else(|| span.end(), |arg| arg.span().start());
                    offset >= start && offset < end
                }
                // For keyword messages, selector parts are interleaved with arguments.
                // We approximate by treating any position within the message send span
                // (that's not the receiver or an argument) as being over the selector.
                MessageSelector::Keyword(_) => offset >= span.start() && offset < span.end(),
            };

            if in_selector {
                // Compute a span for the selector region
                let selector_span = match selector {
                    MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                        let start = receiver_span.end();
                        let end = arguments
                            .first()
                            .map_or_else(|| span.end(), |arg| arg.span().start());
                        Span::new(start, end)
                    }
                    // For keyword messages, use the full span since parts are interleaved
                    MessageSelector::Keyword(_) => *span,
                };
                if let Some(hover) = resolved_selector_hover_info(
                    receiver,
                    selector,
                    selector_span,
                    context,
                    hierarchy,
                    type_map,
                ) {
                    return Some(hover);
                }
                return Some(selector_hover_info(selector, selector_span));
            }
            None
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|s| find_hover_in_expr(&s.expression, offset, context, hierarchy, type_map)),
        Expression::Return { value, .. } => {
            find_hover_in_expr(value, offset, context, hierarchy, type_map)
        }
        Expression::Parenthesized { expression, .. } => {
            find_hover_in_expr(expression, offset, context, hierarchy, type_map)
        }
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            if offset >= field.span.start() && offset < field.span.end() {
                // Show declared state type if available from the class hierarchy
                let field_type = match context {
                    HoverClassContext::InstanceMethod(class) => {
                        hierarchy.state_field_type(class.name.name.as_str(), field.name.as_str())
                    }
                    HoverClassContext::StandaloneInstanceMethod(name) => {
                        hierarchy.state_field_type(name, field.name.as_str())
                    }
                    _ => None,
                };
                let contents = if let Some(ty) = field_type {
                    format!("Field: `{}` — Type: {ty}", field.name)
                } else {
                    format!("Field: `{}`", field.name)
                };
                Some(HoverInfo::new(contents, field.span))
            } else {
                find_hover_in_expr(receiver, offset, context, hierarchy, type_map)
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            if let Some(hover) = find_hover_in_expr(receiver, offset, context, hierarchy, type_map)
            {
                return Some(hover);
            }
            // Check each cascaded message
            for msg in messages {
                // Check arguments first
                if let Some(hover) = msg
                    .arguments
                    .iter()
                    .find_map(|arg| find_hover_in_expr(arg, offset, context, hierarchy, type_map))
                {
                    return Some(hover);
                }
                // Compute selector span: from message start to first argument (or message end)
                if offset >= msg.span.start() && offset < msg.span.end() {
                    let selector_end = msg
                        .arguments
                        .first()
                        .map_or_else(|| msg.span.end(), |arg| arg.span().start());
                    let selector_span = Span::new(msg.span.start(), selector_end);

                    // Only show hover if we're in the selector region (before first arg)
                    if offset >= msg.span.start() && offset < selector_end {
                        if let Some(hover) = resolved_selector_hover_info(
                            receiver,
                            &msg.selector,
                            selector_span,
                            context,
                            hierarchy,
                            type_map,
                        ) {
                            return Some(hover);
                        }
                        return Some(selector_hover_info(&msg.selector, selector_span));
                    }
                }
            }
            None
        }
        Expression::Match { value, arms, .. } => {
            find_hover_in_expr(value, offset, context, hierarchy, type_map).or_else(|| {
                arms.iter().find_map(|arm| {
                    find_hover_in_expr(&arm.body, offset, context, hierarchy, type_map)
                })
            })
        }
        Expression::MapLiteral { pairs, span } => {
            if offset >= span.start() && offset < span.end() {
                // Check if hovering over a specific key or value
                for pair in pairs {
                    if let Some(hover) =
                        find_hover_in_expr(&pair.key, offset, context, hierarchy, type_map)
                    {
                        return Some(hover);
                    }
                    if let Some(hover) =
                        find_hover_in_expr(&pair.value, offset, context, hierarchy, type_map)
                    {
                        return Some(hover);
                    }
                }
                // Hovering over the map literal itself
                Some(HoverInfo::new(
                    format!("Map literal with {} pairs", pairs.len()),
                    *span,
                ))
            } else {
                None
            }
        }
        Expression::ListLiteral {
            elements,
            tail,
            span,
        } => {
            if offset >= span.start() && offset < span.end() {
                for elem in elements {
                    if let Some(hover) =
                        find_hover_in_expr(elem, offset, context, hierarchy, type_map)
                    {
                        return Some(hover);
                    }
                }
                if let Some(t) = tail {
                    if let Some(hover) = find_hover_in_expr(t, offset, context, hierarchy, type_map)
                    {
                        return Some(hover);
                    }
                }
                Some(HoverInfo::new(
                    format!("List literal with {} elements", elements.len()),
                    *span,
                ))
            } else {
                None
            }
        }
        Expression::ArrayLiteral { elements, span } => {
            if offset >= span.start() && offset < span.end() {
                for elem in elements {
                    if let Some(hover) =
                        find_hover_in_expr(elem, offset, context, hierarchy, type_map)
                    {
                        return Some(hover);
                    }
                }
                Some(HoverInfo::new(
                    format!("Array literal with {} elements", elements.len()),
                    *span,
                ))
            } else {
                None
            }
        }
        Expression::Primitive {
            name,
            is_quoted,
            span,
            ..
        } => {
            if offset >= span.start() && offset < span.end() {
                let display = if *is_quoted {
                    format!("Primitive: `@primitive \"{name}\"`")
                } else {
                    format!("Primitive: `@primitive {name}`")
                };
                Some(HoverInfo::new(display, *span))
            } else {
                None
            }
        }
        Expression::Error { message, span } => {
            if offset >= span.start() && offset < span.end() {
                Some(HoverInfo::new(format!("Error: {message}"), *span))
            } else {
                None
            }
        }
        Expression::StringInterpolation { segments, span } => {
            if offset >= span.start() && offset < span.end() {
                // Check interpolated expressions for hover targets
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(expr) = segment {
                        if let Some(info) =
                            find_hover_in_expr(expr, offset, context, hierarchy, type_map)
                        {
                            return Some(info);
                        }
                    }
                }
                Some(HoverInfo::new("Interpolated string".to_string(), *span))
            } else {
                None
            }
        }
        Expression::ExpectDirective { .. } => None,
    }
}

fn resolved_selector_hover_info(
    receiver: &Expression,
    selector: &MessageSelector,
    span: Span,
    context: &HoverClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<HoverInfo> {
    let selector_name = selector.name();
    let (receiver_class, class_side) =
        resolve_receiver_class(receiver, context, hierarchy, type_map)?;

    let method = if class_side {
        hierarchy.find_class_method(receiver_class.as_str(), selector_name.as_str())?
    } else {
        hierarchy.find_method(receiver_class.as_str(), selector_name.as_str())?
    };

    let dispatch = if class_side {
        "class-side"
    } else {
        "instance-side"
    };
    let typed_sig = method_info_signature(&method);
    let summary = format!("Method: `{typed_sig}` ({dispatch})");

    Some(HoverInfo::new(summary, span).with_documentation(format!(
        "Resolved on `{receiver_class}` (defined in `{}`)",
        method.defined_in
    )))
}

/// Formats a `MethodInfo` from the hierarchy as a typed signature string.
///
/// Uses `param_types` and `return_type` from the hierarchy (populated by BT-669).
/// Example output: `deposit: amount: Integer -> Integer`
fn method_info_signature(method: &crate::semantic_analysis::class_hierarchy::MethodInfo) -> String {
    use std::fmt::Write;
    let selector = &method.selector;
    let base = if selector.contains(':') {
        // Keyword message: interleave keyword parts with param types
        let parts: Vec<&str> = selector.split(':').filter(|s| !s.is_empty()).collect();
        let mut fragments = Vec::with_capacity(parts.len());
        for (i, part) in parts.iter().enumerate() {
            let mut fragment = format!("{part}:");
            if let Some(Some(ty)) = method.param_types.get(i) {
                let _ = write!(fragment, " {ty}");
            }
            fragments.push(fragment);
        }
        fragments.join(" ")
    } else if method.arity == 1 {
        // Binary message
        let mut sig = selector.to_string();
        if let Some(Some(ty)) = method.param_types.first() {
            let _ = write!(sig, " {ty}");
        }
        sig
    } else {
        // Unary message
        selector.to_string()
    };

    if let Some(ref return_type) = method.return_type {
        format!("{base} -> {return_type}")
    } else {
        base
    }
}

fn resolve_receiver_class(
    receiver: &Expression,
    context: &HoverClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<(ecow::EcoString, bool)> {
    match receiver {
        Expression::ClassReference { name, .. } => Some((name.name.clone(), true)),
        Expression::Super(_) => match context {
            HoverClassContext::InstanceMethod(class) => class
                .superclass
                .as_ref()
                .map(|superclass| superclass.name.clone())
                .map(|name| (name, false)),
            HoverClassContext::ClassMethod(class) => class
                .superclass
                .as_ref()
                .map(|superclass| superclass.name.clone())
                .map(|name| (name, true)),
            HoverClassContext::StandaloneInstanceMethod(name)
            | HoverClassContext::StandaloneClassMethod(name) => hierarchy
                .get_class(name)
                .and_then(|info| info.superclass.clone())
                .map(|super_name| {
                    (
                        super_name,
                        matches!(context, HoverClassContext::StandaloneClassMethod(_)),
                    )
                }),
            HoverClassContext::TopLevel => None,
        },
        Expression::Identifier(ident) if ident.name == "self" => match context {
            HoverClassContext::InstanceMethod(class) => Some((class.name.name.clone(), false)),
            HoverClassContext::ClassMethod(class) => Some((class.name.name.clone(), true)),
            HoverClassContext::StandaloneInstanceMethod(name) => {
                Some((ecow::EcoString::from(*name), false))
            }
            HoverClassContext::StandaloneClassMethod(name) => {
                Some((ecow::EcoString::from(*name), true))
            }
            HoverClassContext::TopLevel => None,
        },
        _ => type_map
            .get(receiver.span())
            .and_then(InferredType::as_known)
            .cloned()
            .map(|name| (name, false)),
    }
}

/// Creates hover info for `self` based on the class context.
fn self_hover_info(span: Span, context: &HoverClassContext<'_>) -> HoverInfo {
    match context {
        HoverClassContext::InstanceMethod(class) => {
            HoverInfo::new(format!("self: `{}`", class.name.name), span).with_documentation(
                format!("Reference to the current `{}` instance", class.name.name),
            )
        }
        HoverClassContext::ClassMethod(class) => {
            HoverInfo::new(format!("self: `{} class`", class.name.name), span).with_documentation(
                format!(
                    "Reference to the `{}` class object (class-side method)",
                    class.name.name
                ),
            )
        }
        HoverClassContext::StandaloneInstanceMethod(name) => {
            HoverInfo::new(format!("self: `{name}`"), span)
                .with_documentation(format!("Reference to the current `{name}` instance"))
        }
        HoverClassContext::StandaloneClassMethod(name) => {
            HoverInfo::new(format!("self: `{name} class`"), span).with_documentation(format!(
                "Reference to the `{name}` class object (class-side method)"
            ))
        }
        HoverClassContext::TopLevel => {
            HoverInfo::new("Keyword: `self` (current receiver)".to_string(), span)
        }
    }
}

/// Creates hover info for a class reference with hierarchy information.
fn class_reference_hover_info(
    class_name: &ecow::EcoString,
    span: Span,
    hierarchy: &ClassHierarchy,
) -> HoverInfo {
    let module_name = to_module_name(class_name);
    let mut info = format!("Class: `{class_name}` (module `{module_name}`)");

    if let Some(class_info) = hierarchy.get_class(class_name.as_str()) {
        use std::fmt::Write;

        // Add superclass info
        if let Some(ref superclass) = class_info.superclass {
            let _ = write!(info, "\n\nExtends: `{superclass}`");
        }

        // Add hierarchy chain
        let chain = hierarchy.superclass_chain(class_name.as_str());
        if !chain.is_empty() {
            let chain_str: Vec<&str> = chain.iter().map(ecow::EcoString::as_str).collect();
            let _ = write!(
                info,
                "\n\nHierarchy: {} → {}",
                class_name,
                chain_str.join(" → ")
            );
        }

        // Add state info with types
        if !class_info.state.is_empty() {
            let state_entries: Vec<String> = class_info
                .state
                .iter()
                .map(|s| {
                    if let Some(ty) = class_info.state_types.get(s) {
                        format!("{s}: {ty}")
                    } else {
                        s.to_string()
                    }
                })
                .collect();
            let _ = write!(info, "\n\nState: {}", state_entries.join(", "));
        }

        // Add typed method summary (capped to avoid unwieldy hover popups)
        if !class_info.methods.is_empty() || !class_info.class_methods.is_empty() {
            const MAX_METHODS: usize = 10;
            let _ = write!(info, "\n\n**Methods:**");
            let total_instance = class_info.methods.len();
            for method in class_info.methods.iter().take(MAX_METHODS) {
                let sig = method_info_signature(method);
                let _ = write!(info, "\n- `{sig}`");
            }
            if total_instance > MAX_METHODS {
                let _ = write!(
                    info,
                    "\n- ...and {} more instance methods",
                    total_instance - MAX_METHODS
                );
            }
            let remaining_slots = MAX_METHODS.saturating_sub(total_instance);
            let class_display = if remaining_slots > 0 {
                remaining_slots
            } else {
                2 // Always show at least a couple class-side methods
            };
            let total_class = class_info.class_methods.len();
            for method in class_info.class_methods.iter().take(class_display) {
                let sig = method_info_signature(method);
                let _ = write!(info, "\n- `{sig}` (class)");
            }
            if total_class > class_display {
                let _ = write!(
                    info,
                    "\n- ...and {} more class methods",
                    total_class - class_display
                );
            }
        }
    }

    HoverInfo::new(info, span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::{lex_with_eof, parse};

    /// Parse source and compute hover with a fresh hierarchy.
    fn hover_at(source: &str, position: Position) -> Option<HoverInfo> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        compute_hover(&module, source, position, &hierarchy)
    }

    #[test]
    fn compute_hover_on_identifier() {
        let hover = hover_at("x := 42", Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains('x'));
    }

    #[test]
    fn compute_hover_on_integer_literal() {
        // Position 5 is at "42"
        let hover = hover_at("x := 42", Position::new(0, 5));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("42"));
    }

    #[test]
    fn compute_hover_on_string_literal() {
        // Position 5 is at the string
        let hover = hover_at(r#"x := "hello""#, Position::new(0, 5));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("String"));
    }

    #[test]
    fn compute_hover_outside_expr_returns_none() {
        // Position 100 is way out of bounds
        let hover = hover_at("x := 42", Position::new(10, 0));
        assert!(hover.is_none());
    }

    #[test]
    fn compute_hover_on_binary_message() {
        // Position 7 is at the "+" operator
        let hover = hover_at("x := 3 + 4", Position::new(0, 7));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Binary message") || hover.contents.contains("Method:"),
            "Unexpected hover contents: {}",
            hover.contents
        );
        assert!(hover.contents.contains('+') || hover.contents.contains("arity"));
    }

    #[test]
    fn compute_hover_on_unary_message() {
        // Position 9 is at "size"
        let hover = hover_at("x := obj size", Position::new(0, 9));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Unary message") || hover.contents.contains("Method:"),
            "Unexpected hover contents: {}",
            hover.contents
        );
        assert!(hover.contents.contains("size") || hover.contents.contains("arity"));
    }

    #[test]
    fn hover_on_resolved_keyword_call_site_shows_method_resolution() {
        let source = "Object subclass: Counter\n  increment => 1\n\nCounter new increment";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let selector_offset = source.rfind("increment").unwrap();
        let pos = Position::from_offset(source, selector_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);
        assert!(hover.is_some(), "Should hover call-site selector");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Method:") || hover.contents.contains("Unary message"),
            "Unexpected hover contents: {}",
            hover.contents
        );
    }

    #[test]
    fn compute_hover_on_class_reference() {
        // Position 0 is at "Counter"
        let hover = hover_at("Counter spawn", Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("Class: `Counter`"));
        assert!(hover.contents.contains("module `counter`"));
    }

    #[test]
    fn compute_hover_on_class_reference_camelcase() {
        // Position 0 is at "MyCounterActor"
        let hover = hover_at("MyCounterActor spawn", Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("Class: `MyCounterActor`"));
        // Verify proper CamelCase → snake_case conversion
        assert!(hover.contents.contains("module `my_counter_actor`"));
        // Ensure it's NOT using naive to_lowercase
        assert!(!hover.contents.contains("module `mycounteractor`"));
    }

    #[test]
    fn hover_on_primitive_quoted() {
        use crate::ast::Expression;
        use crate::source_analysis::Span;

        let ctx = HoverClassContext::TopLevel;
        let hierarchy = ClassHierarchy::with_builtins();

        let expr = Expression::Primitive {
            name: "erlang_add".into(),
            is_quoted: true,
            is_intrinsic: false,
            span: Span::new(0, 22),
        };
        let hover = find_hover_in_expr(&expr, 5, &ctx, &hierarchy, &TypeMap::new());
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("`@primitive \"erlang_add\"`"));
    }

    #[test]
    fn hover_on_primitive_unquoted() {
        use crate::ast::Expression;
        use crate::source_analysis::Span;

        let ctx = HoverClassContext::TopLevel;
        let hierarchy = ClassHierarchy::with_builtins();

        let expr = Expression::Primitive {
            name: "size".into(),
            is_quoted: false,
            is_intrinsic: false,
            span: Span::new(0, 15),
        };
        let hover = find_hover_in_expr(&expr, 5, &ctx, &hierarchy, &TypeMap::new());
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("`@primitive size`"));
        assert!(!hover.contents.contains("'size'"));
    }

    #[test]
    fn hover_on_self_in_instance_method() {
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // "self" appears at offset 59 in the method body
        // Find the exact offset of "self" in the source
        let self_offset = source.find("self.count").unwrap();
        let pos = Position::from_offset(source, self_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);
        assert!(hover.is_some(), "Should find hover for self");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("self: `Counter`"),
            "Should show 'self: Counter', got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_self_in_class_method() {
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  class withInitial: n => self new: #{count => n}\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Find "self" in the class method body
        let class_method_self = source.find("self new:").unwrap();
        let pos = Position::from_offset(source, class_method_self).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);
        assert!(
            hover.is_some(),
            "Should find hover for self in class method"
        );
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("self: `Counter class`"),
            "Should show 'self: Counter class', got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_class_reference_shows_hierarchy() {
        // Use "Counter spawn" as top-level expression after class definition
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\nCounter spawn";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Find the position of the top-level "Counter" reference
        let counter_pos = source.rfind("Counter").unwrap();
        let pos = Position::from_offset(source, counter_pos).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);
        assert!(hover.is_some(), "Should find hover for Counter reference");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Class: `Counter`"),
            "Should show class name, got: {}",
            hover.contents
        );
        assert!(
            hover.contents.contains("Extends: `Actor`"),
            "Should show superclass, got: {}",
            hover.contents
        );
        assert!(
            hover.contents.contains("State: count"),
            "Should show state variables, got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_builtin_class_reference_shows_hierarchy() {
        let hover = hover_at("Integer class", Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("Class: `Integer`"));
        assert!(hover.contents.contains("Extends: `Number`"));
    }

    #[test]
    fn hover_on_self_at_top_level() {
        let source = "self doSomething";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let hover = compute_hover(&module, source, Position::new(0, 0), &hierarchy);
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Keyword: `self`"),
            "Top-level self should show generic info, got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_identifier_shows_inferred_type() {
        let source = "x := 42\nx";
        let hover = hover_at(source, Position::new(1, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Type: Integer"),
            "Should show inferred type Integer, got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_untyped_identifier_no_type_annotation() {
        // Variable not yet assigned — should show identifier without type
        let source = "x";
        let hover = hover_at(source, Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Identifier: `x`"),
            "Should show identifier, got: {}",
            hover.contents
        );
        assert!(
            !hover.contents.contains("Type:"),
            "Should NOT show type for unknown identifier, got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_string_variable_shows_type() {
        let source = "x := \"hello\"\nx";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let hover = compute_hover(&module, source, Position::new(1, 0), &hierarchy);
        assert!(hover.is_some(), "Should find hover");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Type: String"),
            "Should show inferred type String, got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_self_in_standalone_instance_method() {
        let source = "Counter >> increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let self_offset = source.find("self.count").unwrap();
        let pos = Position::from_offset(source, self_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);
        assert!(
            hover.is_some(),
            "Should find hover for self in standalone method"
        );
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("self: `Counter`"),
            "Should show 'self: Counter', got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_self_in_standalone_class_method() {
        let source = "Counter class >> withInitial: n => self new: #{count => n}";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let self_offset = source.find("self new:").unwrap();
        let pos = Position::from_offset(source, self_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);
        assert!(
            hover.is_some(),
            "Should find hover for self in standalone class method"
        );
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("self: `Counter class`"),
            "Should show 'self: Counter class', got: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_method_declaration_selector_unary() {
        let source = "Object subclass: Counter\n  state: value = 0\n\n  increment => self.value := self.value + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let selector_offset = source.find("increment =>").unwrap();
        let pos = Position::from_offset(source, selector_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);

        assert!(hover.is_some(), "Should hover method declaration selector");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Method: `increment`")
                || hover.contents.contains("Method: `increment ->"),
            "Unexpected hover contents: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_method_declaration_selector_keyword() {
        let source = "Object subclass: Counter\n  at: index put: value => value";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let selector_offset = source.find("at:").unwrap();
        let pos = Position::from_offset(source, selector_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);

        assert!(hover.is_some(), "Should hover keyword declaration selector");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Method:"),
            "Unexpected hover contents: {}",
            hover.contents
        );
        assert!(
            hover.contents.contains("at: index put: value"),
            "Unexpected hover contents: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_method_declaration_typed_parameter_name_with_colon() {
        let source = "Object subclass: Boolean\n  and: aBlock: Block -> Boolean => self ifTrue: aBlock ifFalse: [false]";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let param_offset = source.find("aBlock:").unwrap();
        let pos = Position::from_offset(source, param_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);

        assert!(hover.is_some(), "Should hover typed parameter name");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Parameter: `aBlock`"),
            "Unexpected hover contents: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_method_declaration_parameter_type_and_return_type() {
        let source = "Object subclass: Boolean\n  and: aBlock: Block -> Boolean => self ifTrue: aBlock ifFalse: [false]";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let param_type_offset = source.find("Block ->").unwrap();
        let param_type_pos = Position::from_offset(source, param_type_offset).unwrap();
        let param_type_hover = compute_hover(&module, source, param_type_pos, &hierarchy);
        assert!(param_type_hover.is_some(), "Should hover parameter type");
        let param_type_hover = param_type_hover.unwrap();
        assert!(
            param_type_hover
                .contents
                .contains("Type annotation: `Block`"),
            "Unexpected parameter type hover: {}",
            param_type_hover.contents
        );

        let return_type_offset = source.find("-> Boolean").unwrap() + 3;
        let return_type_pos = Position::from_offset(source, return_type_offset).unwrap();
        let return_type_hover = compute_hover(&module, source, return_type_pos, &hierarchy);
        assert!(return_type_hover.is_some(), "Should hover return type");
        let return_type_hover = return_type_hover.unwrap();
        assert!(
            return_type_hover
                .contents
                .contains("Return type: `Boolean`"),
            "Unexpected return type hover: {}",
            return_type_hover.contents
        );
    }

    #[test]
    fn hover_on_method_declaration_selector_prefers_doc_comment() {
        let source = "Object subclass: Counter\n  /// Increments the counter by one\n  increment -> Integer => 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let selector_offset = source.find("increment ->").unwrap();
        let pos = Position::from_offset(source, selector_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);

        assert!(hover.is_some(), "Should hover declaration selector");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Method: `increment -> Integer`"),
            "Unexpected hover contents: {}",
            hover.contents
        );
        assert!(
            hover
                .documentation
                .as_deref()
                .unwrap_or_default()
                .contains("Increments the counter by one"),
            "Expected doc comment in hover documentation, got: {:?}",
            hover.documentation
        );
    }

    #[test]
    fn hover_on_superclass_name_in_class_declaration() {
        let source = "Object subclass: Counter\n  increment => 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let object_offset = source.find("Object").unwrap();
        let pos = Position::from_offset(source, object_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);

        assert!(hover.is_some(), "Should hover superclass identifier");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Class: `Object`"),
            "Unexpected hover contents: {}",
            hover.contents
        );
    }

    #[test]
    fn hover_on_class_name_in_class_declaration() {
        let source = "Object subclass: Counter\n  increment => 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let class_offset = source.find("Counter").unwrap();
        let pos = Position::from_offset(source, class_offset).unwrap();
        let hover = compute_hover(&module, source, pos, &hierarchy);

        assert!(hover.is_some(), "Should hover class declaration name");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Class: `Counter`"),
            "Unexpected hover contents: {}",
            hover.contents
        );
    }
}

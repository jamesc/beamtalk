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

use crate::ast::{ClassDefinition, Expression, Literal, MessageSelector, Module};
use crate::codegen::core_erlang::to_module_name;
use crate::language_service::{HoverInfo, Position};
use crate::semantic_analysis::ClassHierarchy;
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
/// let hierarchy = ClassHierarchy::build(&module).0;
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

    // Find the expression at this position
    for expr in &module.expressions {
        if let Some(hover) = find_hover_in_expr(expr, offset_val, &class_context, hierarchy) {
            return Some(hover);
        }
    }

    // Also search inside class method bodies
    for class in &module.classes {
        for method in &class.methods {
            for body_expr in &method.body {
                if let Some(hover) =
                    find_hover_in_expr(body_expr, offset_val, &class_context, hierarchy)
                {
                    return Some(hover);
                }
            }
        }
        for method in &class.class_methods {
            for body_expr in &method.body {
                if let Some(hover) =
                    find_hover_in_expr(body_expr, offset_val, &class_context, hierarchy)
                {
                    return Some(hover);
                }
            }
        }
    }

    None
}

/// The hover-relevant class context at a position.
#[derive(Debug, Clone)]
enum HoverClassContext<'a> {
    InstanceMethod(&'a ClassDefinition),
    ClassMethod(&'a ClassDefinition),
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
                Some(HoverInfo::new(
                    format!("Identifier: `{}`", ident.name),
                    ident.span,
                ))
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
            find_hover_in_expr(target, offset, context, hierarchy)
                .or_else(|| find_hover_in_expr(value, offset, context, hierarchy))
        }
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
        } => {
            // First check receiver and arguments
            if let Some(hover) = find_hover_in_expr(receiver, offset, context, hierarchy) {
                return Some(hover);
            }
            if let Some(hover) = arguments
                .iter()
                .find_map(|arg| find_hover_in_expr(arg, offset, context, hierarchy))
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
                return Some(selector_hover_info(selector, selector_span));
            }
            None
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|expr| find_hover_in_expr(expr, offset, context, hierarchy)),
        Expression::Return { value, .. } => find_hover_in_expr(value, offset, context, hierarchy),
        Expression::Parenthesized { expression, .. } => {
            find_hover_in_expr(expression, offset, context, hierarchy)
        }
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            if offset >= field.span.start() && offset < field.span.end() {
                Some(HoverInfo::new(
                    format!("Field: `{}`", field.name),
                    field.span,
                ))
            } else {
                find_hover_in_expr(receiver, offset, context, hierarchy)
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            if let Some(hover) = find_hover_in_expr(receiver, offset, context, hierarchy) {
                return Some(hover);
            }
            // Check each cascaded message
            for msg in messages {
                // Check arguments first
                if let Some(hover) = msg
                    .arguments
                    .iter()
                    .find_map(|arg| find_hover_in_expr(arg, offset, context, hierarchy))
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
                        return Some(selector_hover_info(&msg.selector, selector_span));
                    }
                }
            }
            None
        }
        Expression::Pipe { value, target, .. } => {
            find_hover_in_expr(value, offset, context, hierarchy)
                .or_else(|| find_hover_in_expr(target, offset, context, hierarchy))
        }
        Expression::Match { value, arms, .. } => {
            find_hover_in_expr(value, offset, context, hierarchy).or_else(|| {
                arms.iter()
                    .find_map(|arm| find_hover_in_expr(&arm.body, offset, context, hierarchy))
            })
        }
        Expression::MapLiteral { pairs, span } => {
            if offset >= span.start() && offset < span.end() {
                // Check if hovering over a specific key or value
                for pair in pairs {
                    if let Some(hover) = find_hover_in_expr(&pair.key, offset, context, hierarchy) {
                        return Some(hover);
                    }
                    if let Some(hover) = find_hover_in_expr(&pair.value, offset, context, hierarchy)
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
                    if let Some(hover) = find_hover_in_expr(elem, offset, context, hierarchy) {
                        return Some(hover);
                    }
                }
                if let Some(t) = tail {
                    if let Some(hover) = find_hover_in_expr(t, offset, context, hierarchy) {
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
        Expression::Primitive {
            name,
            is_quoted,
            span,
        } => {
            if offset >= span.start() && offset < span.end() {
                let display = if *is_quoted {
                    format!("Primitive: `@primitive '{name}'`")
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
                        if let Some(info) = find_hover_in_expr(expr, offset, context, hierarchy) {
                            return Some(info);
                        }
                    }
                }
                Some(HoverInfo::new("Interpolated string".to_string(), *span))
            } else {
                None
            }
        }
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

        // Add state info
        if !class_info.state.is_empty() {
            let state_str: Vec<&str> = class_info
                .state
                .iter()
                .map(ecow::EcoString::as_str)
                .collect();
            let _ = write!(info, "\n\nState: {}", state_str.join(", "));
        }

        // Add method count
        let instance_count = class_info.methods.len();
        let class_count = class_info.class_methods.len();
        if class_count > 0 {
            let _ = write!(
                info,
                "\n\nMethods: {instance_count} instance, {class_count} class-side"
            );
        } else if instance_count > 0 {
            let _ = write!(info, "\n\nMethods: {instance_count}");
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
        let hierarchy = ClassHierarchy::build(&module).0;
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
        assert!(hover.contents.contains("Binary message"));
        assert!(hover.contents.contains('+'));
    }

    #[test]
    fn compute_hover_on_unary_message() {
        // Position 9 is at "size"
        let hover = hover_at("x := obj size", Position::new(0, 9));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("Unary message"));
        assert!(hover.contents.contains("size"));
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
            span: Span::new(0, 22),
        };
        let hover = find_hover_in_expr(&expr, 5, &ctx, &hierarchy);
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("`@primitive 'erlang_add'`"));
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
            span: Span::new(0, 15),
        };
        let hover = find_hover_in_expr(&expr, 5, &ctx, &hierarchy);
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
        let hierarchy = ClassHierarchy::build(&module).0;

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
        let hierarchy = ClassHierarchy::build(&module).0;

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
        let hierarchy = ClassHierarchy::build(&module).0;

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
        let hierarchy = ClassHierarchy::build(&module).0;

        let hover = compute_hover(&module, source, Position::new(0, 0), &hierarchy);
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("Keyword: `self`"),
            "Top-level self should show generic info, got: {}",
            hover.contents
        );
    }
}

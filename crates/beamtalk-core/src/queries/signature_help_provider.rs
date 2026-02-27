// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Signature help provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `SignatureHelpProvider` from the DDD model.
//! It shows parameter information when typing keyword messages, helping users
//! understand the expected arguments and their types.
//!
//! # Design
//!
//! Signature help triggers when the cursor is inside a keyword message send.
//! It resolves the receiver type, looks up the method in the `ClassHierarchy`,
//! and returns parameter names, types, and the return type.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: `textDocument/signatureHelp`

use crate::ast::{ClassDefinition, Expression, MessageSelector, Module};
use crate::language_service::{ParameterInfo, Position, SignatureHelp, SignatureInfo};
use crate::semantic_analysis::class_hierarchy::MethodInfo;
use crate::semantic_analysis::type_checker::TypeMap;
use crate::semantic_analysis::{ClassHierarchy, InferredType, infer_types};
use ecow::EcoString;

/// The class context for resolving receiver types.
#[derive(Debug)]
enum SigHelpClassContext<'a> {
    InstanceMethod(&'a ClassDefinition),
    ClassMethod(&'a ClassDefinition),
    StandaloneInstanceMethod(&'a str),
    StandaloneClassMethod(&'a str),
    TopLevel,
}

/// Computes signature help at a given position.
///
/// Returns signature information if the cursor is inside a keyword message send,
/// including parameter names, types, and the active parameter index.
#[must_use]
pub fn compute_signature_help(
    module: &Module,
    source: &str,
    position: Position,
    hierarchy: &ClassHierarchy,
) -> Option<SignatureHelp> {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    let offset = position.to_offset(source)? as u32;
    let type_map = infer_types(module, hierarchy);

    // Search top-level expressions
    for expr in &module.expressions {
        let context = SigHelpClassContext::TopLevel;
        if let Some(help) = find_signature_in_expr(expr, offset, &context, hierarchy, &type_map) {
            return Some(help);
        }
    }

    // Search class method bodies
    for class in &module.classes {
        for method in &class.methods {
            let context = SigHelpClassContext::InstanceMethod(class);
            for expr in &method.body {
                if let Some(help) =
                    find_signature_in_expr(expr, offset, &context, hierarchy, &type_map)
                {
                    return Some(help);
                }
            }
        }
        for method in &class.class_methods {
            let context = SigHelpClassContext::ClassMethod(class);
            for expr in &method.body {
                if let Some(help) =
                    find_signature_in_expr(expr, offset, &context, hierarchy, &type_map)
                {
                    return Some(help);
                }
            }
        }
    }

    // Search standalone method definitions
    for smd in &module.method_definitions {
        let context = if smd.is_class_method {
            SigHelpClassContext::StandaloneClassMethod(smd.class_name.name.as_str())
        } else {
            SigHelpClassContext::StandaloneInstanceMethod(smd.class_name.name.as_str())
        };
        for expr in &smd.method.body {
            if let Some(help) = find_signature_in_expr(expr, offset, &context, hierarchy, &type_map)
            {
                return Some(help);
            }
        }
    }

    None
}

/// Recursively searches for a keyword message send containing the cursor.
#[expect(clippy::too_many_lines, reason = "match on Expression variants")]
fn find_signature_in_expr(
    expr: &Expression,
    offset: u32,
    context: &SigHelpClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<SignatureHelp> {
    let span = expr.span();
    if offset < span.start() || offset > span.end() {
        return None;
    }

    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
            ..
        } => {
            // Check nested expressions first (inner-most message wins)
            if let Some(help) =
                find_signature_in_expr(receiver, offset, context, hierarchy, type_map)
            {
                return Some(help);
            }
            for arg in arguments {
                if let Some(help) =
                    find_signature_in_expr(arg, offset, context, hierarchy, type_map)
                {
                    return Some(help);
                }
            }

            // Only provide signature help for keyword messages
            if let MessageSelector::Keyword(parts) = selector {
                if offset >= span.start() && offset <= span.end() {
                    // Determine which parameter is active based on cursor position
                    let active_param = determine_active_parameter(parts, arguments, offset);
                    let selector_name = selector.name();

                    // Resolve receiver type and look up method
                    if let Some(method) =
                        resolve_method(receiver, &selector_name, context, hierarchy, type_map)
                    {
                        return Some(build_signature_help(&method, active_param));
                    }
                }
            }
            None
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|e| find_signature_in_expr(e, offset, context, hierarchy, type_map)),
        Expression::Assignment { target, value, .. } => {
            find_signature_in_expr(target, offset, context, hierarchy, type_map)
                .or_else(|| find_signature_in_expr(value, offset, context, hierarchy, type_map))
        }
        Expression::Return { value, .. } => {
            find_signature_in_expr(value, offset, context, hierarchy, type_map)
        }
        Expression::Parenthesized { expression, .. } => {
            find_signature_in_expr(expression, offset, context, hierarchy, type_map)
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            if let Some(help) =
                find_signature_in_expr(receiver, offset, context, hierarchy, type_map)
            {
                return Some(help);
            }
            for msg in messages {
                for arg in &msg.arguments {
                    if let Some(help) =
                        find_signature_in_expr(arg, offset, context, hierarchy, type_map)
                    {
                        return Some(help);
                    }
                }
                if let MessageSelector::Keyword(parts) = &msg.selector {
                    if offset >= msg.span.start() && offset <= msg.span.end() {
                        let active_param =
                            determine_active_parameter(parts, &msg.arguments, offset);
                        let selector_name = msg.selector.name();
                        if let Some(method) =
                            resolve_method(receiver, &selector_name, context, hierarchy, type_map)
                        {
                            return Some(build_signature_help(&method, active_param));
                        }
                    }
                }
            }
            None
        }
        Expression::Match { value, arms, .. } => {
            find_signature_in_expr(value, offset, context, hierarchy, type_map).or_else(|| {
                arms.iter().find_map(|arm| {
                    find_signature_in_expr(&arm.body, offset, context, hierarchy, type_map)
                })
            })
        }
        Expression::FieldAccess { receiver, .. } => {
            find_signature_in_expr(receiver, offset, context, hierarchy, type_map)
        }
        Expression::MapLiteral { pairs, .. } => pairs.iter().find_map(|pair| {
            find_signature_in_expr(&pair.key, offset, context, hierarchy, type_map).or_else(|| {
                find_signature_in_expr(&pair.value, offset, context, hierarchy, type_map)
            })
        }),
        Expression::ListLiteral { elements, tail, .. } => elements
            .iter()
            .find_map(|e| find_signature_in_expr(e, offset, context, hierarchy, type_map))
            .or_else(|| {
                tail.as_ref()
                    .and_then(|t| find_signature_in_expr(t, offset, context, hierarchy, type_map))
            }),
        Expression::StringInterpolation { segments, .. } => segments.iter().find_map(|seg| {
            if let crate::ast::StringSegment::Interpolation(expr) = seg {
                find_signature_in_expr(expr, offset, context, hierarchy, type_map)
            } else {
                None
            }
        }),
        _ => None,
    }
}

/// Determines which parameter is active based on cursor position relative to keyword parts.
fn determine_active_parameter(
    parts: &[crate::ast::KeywordPart],
    arguments: &[Expression],
    offset: u32,
) -> u32 {
    // Walk through keyword parts; the active parameter is the last keyword
    // whose span start is at or before the cursor.
    let mut active = 0u32;
    for (i, part) in parts.iter().enumerate() {
        if offset >= part.span.start() {
            #[expect(
                clippy::cast_possible_truncation,
                reason = "keyword messages won't have more than u32::MAX parts"
            )]
            {
                active = i as u32;
            }
        }
    }
    // If cursor is past the last keyword's colon, ensure we're on the last param
    if let Some(last_arg) = arguments.last() {
        if offset > last_arg.span().end() {
            #[expect(
                clippy::cast_possible_truncation,
                reason = "keyword messages won't have more than u32::MAX parts"
            )]
            {
                active = parts.len().saturating_sub(1) as u32;
            }
        }
    }
    active
}

/// Resolves the receiver type and looks up the method in the hierarchy.
fn resolve_method(
    receiver: &Expression,
    selector_name: &str,
    context: &SigHelpClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<MethodInfo> {
    let (receiver_class, class_side) =
        resolve_receiver_class(receiver, context, hierarchy, type_map)?;

    if class_side {
        hierarchy.find_class_method(receiver_class.as_str(), selector_name)
    } else {
        hierarchy.find_method(receiver_class.as_str(), selector_name)
    }
}

/// Resolves the class of a receiver expression.
fn resolve_receiver_class(
    receiver: &Expression,
    context: &SigHelpClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<(EcoString, bool)> {
    match receiver {
        Expression::ClassReference { name, .. } => Some((name.name.clone(), true)),
        Expression::Super(_) => match context {
            SigHelpClassContext::InstanceMethod(class) => {
                class.superclass.as_ref().map(|s| (s.name.clone(), false))
            }
            SigHelpClassContext::ClassMethod(class) => {
                class.superclass.as_ref().map(|s| (s.name.clone(), true))
            }
            SigHelpClassContext::StandaloneInstanceMethod(name) => hierarchy
                .get_class(name)
                .and_then(|info| info.superclass.clone())
                .map(|super_name| (super_name, false)),
            SigHelpClassContext::StandaloneClassMethod(name) => hierarchy
                .get_class(name)
                .and_then(|info| info.superclass.clone())
                .map(|super_name| (super_name, true)),
            SigHelpClassContext::TopLevel => None,
        },
        Expression::Identifier(ident) if ident.name == "self" => match context {
            SigHelpClassContext::InstanceMethod(class) => Some((class.name.name.clone(), false)),
            SigHelpClassContext::ClassMethod(class) => Some((class.name.name.clone(), true)),
            SigHelpClassContext::StandaloneInstanceMethod(name) => {
                Some((EcoString::from(*name), false))
            }
            SigHelpClassContext::StandaloneClassMethod(name) => {
                Some((EcoString::from(*name), true))
            }
            SigHelpClassContext::TopLevel => None,
        },
        _ => type_map
            .get(receiver.span())
            .and_then(InferredType::as_known)
            .cloned()
            .map(|name| (name, false)),
    }
}

/// Builds a `SignatureHelp` from a resolved `MethodInfo`.
fn build_signature_help(method: &MethodInfo, active_parameter: u32) -> SignatureHelp {
    let selector = &method.selector;

    // Build parameter list from keyword parts and param_types
    let parts: Vec<&str> = selector.split(':').filter(|s| !s.is_empty()).collect();
    let mut parameters = Vec::with_capacity(parts.len());
    let mut label_fragments = Vec::with_capacity(parts.len());

    for (i, part) in parts.iter().enumerate() {
        let param_type = method.param_types.get(i).and_then(|t| t.as_ref());
        let param_label = if let Some(ty) = param_type {
            format!("{part}: {ty}")
        } else {
            format!("{part}:")
        };
        label_fragments.push(param_label.clone());
        parameters.push(ParameterInfo {
            label: EcoString::from(param_label),
            documentation: param_type.map(|ty| EcoString::from(format!("Type: {ty}"))),
        });
    }

    let mut label = label_fragments.join(" ");
    if let Some(ref return_type) = method.return_type {
        use std::fmt::Write;
        let _ = write!(label, " -> {return_type}");
    }

    let documentation = Some(EcoString::from(format!(
        "Defined in `{}`",
        method.defined_in
    )));

    SignatureHelp {
        signatures: vec![SignatureInfo {
            label: EcoString::from(label),
            documentation,
            parameters,
        }],
        active_signature: 0,
        active_parameter,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::{lex_with_eof, parse};

    fn sig_help_at(source: &str, position: Position) -> Option<SignatureHelp> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        compute_signature_help(&module, source, position, &hierarchy)
    }

    #[test]
    fn no_signature_help_on_unary_message() {
        let result = sig_help_at("x size", Position::new(0, 3));
        assert!(result.is_none());
    }

    #[test]
    fn no_signature_help_on_binary_message() {
        let result = sig_help_at("3 + 4", Position::new(0, 3));
        assert!(result.is_none());
    }

    #[test]
    fn signature_help_inside_method_body_has_correct_params() {
        // Beamtalk class definition syntax: methods use => for body
        let source = "Object subclass: Calc\n  add: x to: y => x + y\n  run => self add: 1 to: 2";
        let tokens = lex_with_eof(source);
        let (module, diags) = parse(tokens);
        assert!(diags.is_empty(), "Parse errors: {diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Position in `self add: 1 to: 2` — cursor on `to:` (line 2, col 23)
        // Line 2: "  run => self add: 1 to: 2"
        //          0123456789012345678901234567
        let result = compute_signature_help(&module, source, Position::new(2, 23), &hierarchy);
        let help = result.expect("should find signature help for self add:to:");
        assert_eq!(help.signatures.len(), 1);
        assert_eq!(help.signatures[0].parameters.len(), 2);
        assert!(
            help.signatures[0].label.contains("add:"),
            "label should contain 'add:', got: {}",
            help.signatures[0].label
        );
        assert!(
            help.signatures[0].label.contains("to:"),
            "label should contain 'to:', got: {}",
            help.signatures[0].label
        );
    }

    #[test]
    fn signature_help_active_parameter_tracks_cursor() {
        let source = "Object subclass: Calc\n  add: x to: y => x + y\n  run => self add: 1 to: 2";
        let tokens = lex_with_eof(source);
        let (module, diags) = parse(tokens);
        assert!(diags.is_empty(), "Parse errors: {diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Cursor right after `add:` — should be param 0
        // Line 2: "  run => self add: 1 to: 2"
        //                       ^-- col 19
        let result = compute_signature_help(&module, source, Position::new(2, 19), &hierarchy);
        let help = result.expect("should get signature help at 'add:' position");
        assert_eq!(
            help.active_parameter, 0,
            "cursor after 'add:' should be param 0"
        );

        // Cursor right after `to:` — should be param 1
        // Line 2: "  run => self add: 1 to: 2"
        //                               ^-- col 24
        let result = compute_signature_help(&module, source, Position::new(2, 24), &hierarchy);
        let help = result.expect("should get signature help at 'to:' position");
        assert_eq!(
            help.active_parameter, 1,
            "cursor after 'to:' should be param 1"
        );
    }
}

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

use std::sync::Arc;

use crate::queries::{protocols_for_module, type_of_parsed};
use crate::{ParameterInfo, Position, SignatureHelp, SignatureInfo};
use beamtalk_core::ast::normalize::canonical_operand;
use beamtalk_core::ast::{BinarySegment, ClassDefinition, Expression, MessageSelector, Module};
use beamtalk_core::semantic_analysis::class_hierarchy::MethodInfo;
use beamtalk_core::semantic_analysis::protocol_registry::{
    ProtocolMethodRequirement, ProtocolRegistry,
};
use beamtalk_core::semantic_analysis::type_checker::native_type_registry::NativeTypeRegistry;
use beamtalk_core::semantic_analysis::type_checker::{
    TypeMap, infer_types_and_returns_with_registries,
};
use beamtalk_core::semantic_analysis::{ClassHierarchy, InferredType};
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

/// What a signature-help lookup resolves a receiver and selector against.
struct Lookup<'a> {
    hierarchy: &'a ClassHierarchy,
    type_map: &'a TypeMap,
    native_types: Option<&'a NativeTypeRegistry>,
    protocol_registry: &'a ProtocolRegistry,
}

/// Computes signature help at a given position.
///
/// Returns signature information if the cursor is inside a keyword message send,
/// including parameter names, types, and the active parameter index.
///
/// `protocol_registry` is the file's protocol registry (cross-file protocols
/// plus the module's own), as for completion and hover: a receiver typed
/// `P class` (ADR 0144) shows the signature of `P`'s `class` requirement.
/// With `None`, only the module's own protocols are registered.
#[must_use]
pub fn compute_signature_help(
    module: &Module,
    source: &str,
    position: Position,
    hierarchy: &ClassHierarchy,
    native_types: Option<&NativeTypeRegistry>,
    protocol_registry: Option<Arc<ProtocolRegistry>>,
) -> Option<SignatureHelp> {
    let offset = position.to_byte_offset(source)?.get();
    let protocol_registry = protocols_for_module(protocol_registry, module, hierarchy);
    let (type_map, _) = infer_types_and_returns_with_registries(
        module,
        hierarchy,
        native_types,
        Some(Arc::clone(&protocol_registry)),
        None,
    );
    let lookup = Lookup {
        hierarchy,
        type_map: &type_map,
        native_types,
        protocol_registry: &protocol_registry,
    };

    // Search top-level expressions
    for stmt in &module.expressions {
        let context = SigHelpClassContext::TopLevel;
        if let Some(help) = find_signature_in_expr(&stmt.expression, offset, &context, &lookup) {
            return Some(help);
        }
    }

    // Search class method bodies
    for class in &module.classes {
        for method in &class.methods {
            let context = SigHelpClassContext::InstanceMethod(class);
            for stmt in &method.body {
                if let Some(help) =
                    find_signature_in_expr(&stmt.expression, offset, &context, &lookup)
                {
                    return Some(help);
                }
            }
        }
        for method in &class.class_methods {
            let context = SigHelpClassContext::ClassMethod(class);
            for stmt in &method.body {
                if let Some(help) =
                    find_signature_in_expr(&stmt.expression, offset, &context, &lookup)
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
        for stmt in &smd.method.body {
            if let Some(help) = find_signature_in_expr(&stmt.expression, offset, &context, &lookup)
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
    lookup: &Lookup<'_>,
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
            if let Some(help) = find_signature_in_expr(receiver, offset, context, lookup) {
                return Some(help);
            }
            for arg in arguments {
                if let Some(help) = find_signature_in_expr(arg, offset, context, lookup) {
                    return Some(help);
                }
            }

            // Only provide signature help for keyword messages
            if let MessageSelector::Keyword(parts) = selector {
                if offset >= span.start() && offset <= span.end() {
                    // Determine which parameter is active based on cursor position
                    let active_param = determine_active_parameter(parts, arguments, offset);
                    let selector_name = selector.name();

                    // ADR 0075 Phase 4: FFI signature help for ErlangModule receivers
                    if let Some(help) = resolve_ffi_signature_help(
                        receiver,
                        &selector_name,
                        selector,
                        active_param,
                        lookup.type_map,
                        lookup.native_types,
                    ) {
                        return Some(help);
                    }

                    // Resolve receiver type and look up method
                    if let Some(help) =
                        resolve_signature(receiver, &selector_name, context, lookup, active_param)
                    {
                        return Some(help);
                    }
                }
            }
            None
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|s| find_signature_in_expr(&s.expression, offset, context, lookup)),
        Expression::Assignment { target, value, .. } => {
            find_signature_in_expr(target, offset, context, lookup)
                .or_else(|| find_signature_in_expr(value, offset, context, lookup))
        }
        Expression::Return { value, .. } => find_signature_in_expr(value, offset, context, lookup),
        Expression::Parenthesized { expression, .. } => {
            find_signature_in_expr(expression, offset, context, lookup)
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            if let Some(help) = find_signature_in_expr(receiver, offset, context, lookup) {
                return Some(help);
            }
            for msg in messages {
                for arg in &msg.arguments {
                    if let Some(help) = find_signature_in_expr(arg, offset, context, lookup) {
                        return Some(help);
                    }
                }
                if let MessageSelector::Keyword(parts) = &msg.selector {
                    if offset >= msg.span.start() && offset <= msg.span.end() {
                        let active_param =
                            determine_active_parameter(parts, &msg.arguments, offset);
                        let selector_name = msg.selector.name();
                        if let Some(help) = resolve_signature(
                            receiver,
                            &selector_name,
                            context,
                            lookup,
                            active_param,
                        ) {
                            return Some(help);
                        }
                    }
                }
            }
            None
        }
        Expression::Match { value, arms, .. } => {
            find_signature_in_expr(value, offset, context, lookup).or_else(|| {
                arms.iter()
                    .find_map(|arm| find_signature_in_expr(&arm.body, offset, context, lookup))
            })
        }
        Expression::FieldAccess { receiver, .. } => {
            find_signature_in_expr(receiver, offset, context, lookup)
        }
        Expression::MapLiteral { pairs, .. } => pairs.iter().find_map(|pair| {
            find_signature_in_expr(&pair.key, offset, context, lookup)
                .or_else(|| find_signature_in_expr(&pair.value, offset, context, lookup))
        }),
        Expression::ArrayLiteral { elements, .. } | Expression::TupleLiteral { elements, .. } => {
            elements
                .iter()
                .find_map(|e| find_signature_in_expr(e, offset, context, lookup))
        }
        Expression::ListLiteral { elements, tail, .. } => elements
            .iter()
            .find_map(|e| find_signature_in_expr(e, offset, context, lookup))
            .or_else(|| {
                tail.as_ref()
                    .and_then(|t| find_signature_in_expr(t, offset, context, lookup))
            }),
        Expression::StringInterpolation { segments, .. } => segments.iter().find_map(|seg| {
            if let beamtalk_core::ast::StringSegment::Interpolation(expr) = seg {
                find_signature_in_expr(expr, offset, context, lookup)
            } else {
                None
            }
        }),
        Expression::BinaryConstruction { segments, .. } => segments
            .iter()
            .flat_map(BinarySegment::sub_expressions)
            .find_map(|expr| find_signature_in_expr(expr, offset, context, lookup)),
        _ => None,
    }
}

/// Resolves FFI signature help for `ErlangModule` receivers (ADR 0075 Phase 4).
///
/// When the receiver is typed as `ErlangModule<module_name>`, extracts the Erlang
/// module and function name, looks up the signature in `NativeTypeRegistry`, and
/// builds a `SignatureHelp` with typed parameter info.
fn resolve_ffi_signature_help(
    receiver: &Expression,
    selector_name: &str,
    selector: &MessageSelector,
    active_param: u32,
    type_map: &TypeMap,
    native_types: Option<&NativeTypeRegistry>,
) -> Option<SignatureHelp> {
    use beamtalk_core::unparse::{
        SignatureParam, SignatureRenderOptions, SignatureSelector, render_signature_text,
    };

    // Check if receiver is typed as ErlangModule
    let receiver_ty = type_of_parsed(type_map, receiver)?;
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = receiver_ty
    else {
        return None;
    };
    if class_name != "ErlangModule" {
        return None;
    }

    // Extract module name from type args
    let module_name = match type_args.first() {
        Some(InferredType::Known { class_name, .. }) => class_name.as_str(),
        _ => return None,
    };

    // Extract function name and arity
    let function_name = selector_name.split(':').next().unwrap_or(selector_name);
    let arity = match selector {
        MessageSelector::Keyword(parts) => u8::try_from(parts.len()).unwrap_or(u8::MAX),
        _ => return None, // Signature help only for keyword messages
    };

    let registry = native_types?;
    let sig = registry.lookup(module_name, function_name, arity)?;

    // Build signature help from the FFI signature via the shared
    // signature-text composer: each parameter's keyword/name/type
    // is pre-rendered here (native-type registry types don't have a
    // `TypeAnnotation`, so they render through their own
    // `display_for_diagnostic`, not `unparse::unparse_type_annotation_display`),
    // then composed identically to every other consumer.
    let keywords: Vec<String> = sig
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            if i == 0 {
                format!("{function_name}:")
            } else {
                match &param.keyword {
                    Some(kw) => format!("{kw}:"),
                    None => "with:".to_string(),
                }
            }
        })
        .collect();
    let type_displays: Vec<EcoString> = sig
        .params
        .iter()
        .map(|param| {
            param
                .type_
                .display_for_diagnostic()
                .unwrap_or_else(|| EcoString::from("Dynamic"))
        })
        .collect();
    let sig_params: Vec<SignatureParam<'_>> = sig
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| SignatureParam {
            keyword: &keywords[i],
            name: Some(param.keyword.as_deref().unwrap_or("arg")),
            type_text: Some(type_displays[i].as_str()),
        })
        .collect();

    let parameters: Vec<ParameterInfo> = sig_params
        .iter()
        .zip(&type_displays)
        .map(|(param, type_display)| ParameterInfo {
            label: EcoString::from(param.render()),
            documentation: Some(EcoString::from(format!("Type: {type_display}"))),
        })
        .collect();

    let ret_display = sig
        .return_type
        .display_for_diagnostic()
        .unwrap_or_else(|| EcoString::from("Dynamic"));
    let label = render_signature_text(
        SignatureSelector::Params(&sig_params),
        Some(&ret_display),
        &SignatureRenderOptions::DISPLAY,
    );

    let documentation = Some(EcoString::from(format!(
        "Erlang FFI: `{module_name}:{function_name}/{arity}`"
    )));

    Some(SignatureHelp {
        signatures: vec![SignatureInfo {
            label: EcoString::from(label),
            documentation,
            parameters,
        }],
        active_signature: 0,
        active_parameter: active_param,
    })
}

/// Determines which parameter is active based on cursor position relative to keyword parts.
fn determine_active_parameter(
    parts: &[beamtalk_core::ast::KeywordPart],
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

/// Resolves the receiver type and builds the signature of `selector_name`
/// on it: a method in the hierarchy, or — for a `P class` receiver (ADR 0144)
/// — `P`'s `class` requirement, falling back to the class-object protocol
/// (`Metaclass`'s instance side), the same order completion offers them in.
fn resolve_signature(
    receiver: &Expression,
    selector_name: &str,
    context: &SigHelpClassContext<'_>,
    lookup: &Lookup<'_>,
    active_parameter: u32,
) -> Option<SignatureHelp> {
    let hierarchy = lookup.hierarchy;
    if let Some(InferredType::ProtocolMeta { protocol_name, .. }) =
        type_of_parsed(lookup.type_map, receiver)
    {
        let registry = lookup.protocol_registry;
        if let Some(requirement) = registry
            .get(protocol_name)
            .and_then(|protocol| protocol.find_class_requirement(registry, selector_name))
        {
            return Some(build_requirement_signature_help(
                requirement,
                protocol_name,
                active_parameter,
            ));
        }
        return hierarchy
            .find_method("Metaclass", selector_name)
            .map(|method| build_signature_help(&method, active_parameter));
    }

    let (receiver_class, class_side) =
        resolve_receiver_class(receiver, context, hierarchy, lookup.type_map)?;

    let method = if class_side {
        hierarchy.find_class_method(receiver_class.as_str(), selector_name)
    } else {
        hierarchy.find_method(receiver_class.as_str(), selector_name)
    }?;
    Some(build_signature_help(&method, active_parameter))
}

/// Resolves the class of a receiver expression.
fn resolve_receiver_class(
    receiver: &Expression,
    context: &SigHelpClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<(EcoString, bool)> {
    // The receiver as analysis sees it (BT-4237): `(self) foo:` resolves like
    // `self foo:`, while `(super) foo:` stays an ordinary send.
    match canonical_operand(receiver) {
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
        _ => type_of_parsed(type_map, receiver)
            .and_then(InferredType::as_known)
            .cloned()
            .map(|name| (name, false)),
    }
}

/// Builds signature help from a resolved `ClassHierarchy::MethodInfo`.
/// Like [`crate::queries::hover_provider`]'s resolved-call
/// display, `MethodInfo` has no parameter names — only types — so each
/// parameter renders as `keyword: Type` via the shared
/// [`beamtalk_core::unparse::render_signature_text`] core (`SignatureParam { name:
/// None, .. }`), not the declaration's `keyword name :: Type`.
fn build_signature_help(method: &MethodInfo, active_parameter: u32) -> SignatureHelp {
    // `MethodInfo` types are structured `DeclaredType`s — render
    // once here (`Display` matches `TypeAnnotation::type_name` verbatim)
    // and hand the composer the text it contracts for.
    let param_type_texts: Vec<Option<String>> = method
        .param_types
        .iter()
        .map(|ty| ty.as_ref().map(ToString::to_string))
        .collect();
    signature_help_from_parts(
        &method.selector,
        &param_type_texts,
        method
            .return_type
            .as_ref()
            .map(ToString::to_string)
            .as_deref(),
        &format!("Defined in `{}`", method.defined_in),
        active_parameter,
    )
}

/// Builds signature help from a protocol's `class` requirement, for a send to
/// a `P class` receiver (ADR 0144). Rendered like a hierarchy method, and
/// documented the way completion documents the same requirement.
fn build_requirement_signature_help(
    requirement: &ProtocolMethodRequirement,
    protocol_name: &str,
    active_parameter: u32,
) -> SignatureHelp {
    let param_type_texts: Vec<Option<String>> = requirement
        .param_types
        .iter()
        .map(|ty| ty.as_ref().map(ToString::to_string))
        .collect();
    signature_help_from_parts(
        &requirement.selector,
        &param_type_texts,
        requirement.return_type.as_deref(),
        &format!("class requirement of protocol `{protocol_name}`"),
        active_parameter,
    )
}

/// The shared signature-help composer: one `keyword: Type` parameter per
/// keyword part of `selector`, labelled via
/// [`beamtalk_core::unparse::render_signature_text`].
fn signature_help_from_parts(
    selector: &str,
    param_type_texts: &[Option<String>],
    return_type_text: Option<&str>,
    documentation: &str,
    active_parameter: u32,
) -> SignatureHelp {
    use beamtalk_core::unparse::{
        SignatureParam, SignatureRenderOptions, SignatureSelector, render_signature_text,
    };

    // Build parameter list from keyword parts (re-adding the trailing `:`
    // the split strips) and param_types.
    let keywords: Vec<String> = selector
        .split(':')
        .filter(|s| !s.is_empty())
        .map(|part| format!("{part}:"))
        .collect();
    let sig_params: Vec<SignatureParam<'_>> = keywords
        .iter()
        .enumerate()
        .map(|(i, keyword)| SignatureParam {
            keyword,
            name: None,
            type_text: param_type_texts.get(i).and_then(Option::as_deref),
        })
        .collect();

    let parameters: Vec<ParameterInfo> = sig_params
        .iter()
        .map(|param| ParameterInfo {
            label: EcoString::from(param.render()),
            documentation: param
                .type_text
                .map(|ty| EcoString::from(format!("Type: {ty}"))),
        })
        .collect();

    let label = render_signature_text(
        SignatureSelector::Params(&sig_params),
        return_type_text,
        &SignatureRenderOptions::DISPLAY,
    );

    SignatureHelp {
        signatures: vec![SignatureInfo {
            label: EcoString::from(label),
            documentation: Some(EcoString::from(documentation)),
            parameters,
        }],
        active_signature: 0,
        active_parameter,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::semantic_analysis::ClassHierarchy;
    use beamtalk_core::source_analysis::{lex_with_eof, parse};

    fn sig_help_at(source: &str, position: Position) -> Option<SignatureHelp> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        compute_signature_help(&module, source, position, &hierarchy, None, None)
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
        let result = compute_signature_help(
            &module,
            source,
            Position::new(2, 23),
            &hierarchy,
            None,
            None,
        );
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
        let result = compute_signature_help(
            &module,
            source,
            Position::new(2, 19),
            &hierarchy,
            None,
            None,
        );
        let help = result.expect("should get signature help at 'add:' position");
        assert_eq!(
            help.active_parameter, 0,
            "cursor after 'add:' should be param 0"
        );

        // Cursor right after `to:` — should be param 1
        // Line 2: "  run => self add: 1 to: 2"
        //                               ^-- col 24
        let result = compute_signature_help(
            &module,
            source,
            Position::new(2, 24),
            &hierarchy,
            None,
            None,
        );
        let help = result.expect("should get signature help at 'to:' position");
        assert_eq!(
            help.active_parameter, 1,
            "cursor after 'to:' should be param 1"
        );
    }

    // --- FFI signature help tests (ADR 0075 Phase 4) ---

    #[test]
    fn ffi_signature_help_shows_typed_params() {
        use beamtalk_core::semantic_analysis::InferredType;
        use beamtalk_core::semantic_analysis::type_checker::TypeProvenance;
        use beamtalk_core::semantic_analysis::type_checker::native_type_registry::{
            FunctionSignature, NativeTypeRegistry, ParamType,
        };

        let source = "Erlang lists seq: 1 to: 10";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "lists",
            vec![FunctionSignature {
                name: "seq".to_string(),
                arity: 2,
                params: vec![
                    ParamType {
                        keyword: Some(ecow::EcoString::from("from")),
                        type_: InferredType::known("Integer"),
                    },
                    ParamType {
                        keyword: Some(ecow::EcoString::from("to")),
                        type_: InferredType::known("Integer"),
                    },
                ],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            }],
        );

        // Position inside `seq: 1 to: 10` — cursor on "to:" (line 0, col 20)
        // Source: "Erlang lists seq: 1 to: 10"
        //          0123456789012345678901234567
        let result = compute_signature_help(
            &module,
            source,
            Position::new(0, 20),
            &hierarchy,
            Some(&registry),
            None,
        );
        let help = result.expect("should find FFI signature help");
        assert_eq!(help.signatures.len(), 1);
        assert_eq!(help.signatures[0].parameters.len(), 2);
        assert!(
            help.signatures[0].label.contains("Integer"),
            "FFI sig help should show Integer type. Got: {}",
            help.signatures[0].label
        );
        assert!(
            help.signatures[0].label.contains("List"),
            "FFI sig help should show List return type. Got: {}",
            help.signatures[0].label
        );
    }

    #[test]
    fn ffi_signature_help_without_registry_falls_through() {
        let source = "Erlang lists seq: 1 to: 10";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        // Without registry, signature help should still work (falls through to normal resolution)
        let result = compute_signature_help(
            &module,
            source,
            Position::new(0, 20),
            &hierarchy,
            None,
            None,
        );
        // ErlangModule doesn't have a "seq:to:" method in the hierarchy, so no result
        assert!(
            result.is_none(),
            "Without registry, FFI sig help should not resolve"
        );
    }

    // -----------------------------------------------------------------------
    // ADR 0144: `P class` receivers (BT-4450)
    // -----------------------------------------------------------------------

    const WIRE_SOURCE: &str = "Protocol define: Wire\n  class send: data :: Object to: port :: Integer -> Boolean\n\nObject subclass: Caller\n  run: w :: Wire class =>\n    w send: 1 to: 2\n";

    fn wire_hierarchy(module: &Module) -> ClassHierarchy {
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.register_protocol_classes(module);
        hierarchy
    }

    /// A keyword send to a `P class` receiver shows the signature of `P`'s
    /// `class` requirement, as completion and hover resolve it.
    #[test]
    fn protocol_metatype_signature_help_shows_class_requirement() {
        let (module, _) = parse(lex_with_eof(WIRE_SOURCE));
        let hierarchy = wire_hierarchy(&module);
        // Cursor on the `to:` argument.
        let help = compute_signature_help(
            &module,
            WIRE_SOURCE,
            Position::new(5, 19),
            &hierarchy,
            None,
            None,
        )
        .expect("the class requirement's signature is shown");
        let signature = &help.signatures[0];
        assert_eq!(
            signature.label.as_str(),
            "send: Object to: Integer -> Boolean"
        );
        assert_eq!(
            signature.documentation.as_deref(),
            Some("class requirement of protocol `Wire`")
        );
        assert_eq!(signature.parameters.len(), 2);
        assert_eq!(help.active_parameter, 1);
    }

    /// A protocol known only from the caller's project registry (declared in
    /// another file) still resolves.
    #[test]
    fn protocol_metatype_signature_help_uses_project_registry() {
        let protocol_source =
            "Protocol define: Wire\n  class send: data :: Object to: port :: Integer -> Boolean\n";
        let source = "Object subclass: Caller\n  run: w :: Wire class =>\n    w send: 1 to: 2\n";
        let (protocol_module, _) = parse(lex_with_eof(protocol_source));
        let (module, _) = parse(lex_with_eof(source));
        let hierarchy = wire_hierarchy(&protocol_module);
        let mut registry = ProtocolRegistry::new();
        let _ = registry.seed_and_register_module(&protocol_module, Vec::new(), &hierarchy, None);

        let without = compute_signature_help(
            &module,
            source,
            Position::new(2, 19),
            &hierarchy,
            None,
            None,
        );
        assert!(
            without
                .is_none_or(|help| help.signatures[0].label.as_str()
                    != "send: Object to: Integer -> Boolean"),
            "without the project registry `Wire` is not a known protocol"
        );

        let help = compute_signature_help(
            &module,
            source,
            Position::new(2, 19),
            &hierarchy,
            None,
            Some(Arc::new(registry)),
        )
        .expect("the class requirement's signature is shown");
        assert_eq!(
            help.signatures[0].label.as_str(),
            "send: Object to: Integer -> Boolean"
        );
    }

    /// A selector that is no `class` requirement falls back to the
    /// class-object protocol (`Metaclass`'s instance side), as completion does.
    #[test]
    fn protocol_metatype_signature_help_falls_back_to_class_object_protocol() {
        let source = "Protocol define: Wire\n  class label -> String\n\nObject subclass: Caller\n  run: w :: Wire class =>\n    w respondsTo: #label\n";
        let (module, _) = parse(lex_with_eof(source));
        let hierarchy = wire_hierarchy(&module);
        let help = compute_signature_help(
            &module,
            source,
            Position::new(5, 18),
            &hierarchy,
            None,
            None,
        )
        .expect("the class-object protocol's signature is shown");
        assert!(
            help.signatures[0].label.starts_with("respondsTo:"),
            "got {}",
            help.signatures[0].label
        );
    }
}

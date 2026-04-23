// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test fixtures for `type_checker` per-feature tests.
//!
//! These helpers were previously inlined at the top (and middle) of the
//! monolithic `tests.rs`. They are re-exported into every child test module
//! via `use super::common::*;`.

pub(super) use super::super::*;
pub(super) use crate::ast::{
    Block, CascadeMessage, ClassDefinition, ClassKind, ClassModifiers, CommentAttachment,
    ExpectCategory, Expression, ExpressionStatement, Identifier, KeywordPart, Literal, MatchArm,
    MessageSelector, MethodDefinition, MethodKind, Module, ParameterDefinition, Pattern,
    ProtocolDefinition, ProtocolMethodSignature, StateDeclaration, TypeAnnotation,
};
pub(super) use crate::source_analysis::{DiagnosticCategory, Span};

pub(super) fn span() -> Span {
    Span::new(0, 1)
}

pub(super) fn ident(name: &str) -> Identifier {
    Identifier {
        name: name.into(),
        span: span(),
    }
}

pub(super) fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

pub(super) fn make_module(expressions: Vec<Expression>) -> Module {
    Module::new(
        expressions
            .into_iter()
            .map(ExpressionStatement::bare)
            .collect(),
        span(),
    )
}

pub(super) fn make_module_with_classes(
    expressions: Vec<Expression>,
    classes: Vec<ClassDefinition>,
) -> Module {
    let mut module = Module::new(
        expressions
            .into_iter()
            .map(ExpressionStatement::bare)
            .collect(),
        span(),
    );
    module.classes = classes;
    module
}

pub(super) fn msg_send(
    receiver: Expression,
    selector: MessageSelector,
    args: Vec<Expression>,
) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(receiver),
        selector,
        arguments: args,
        is_cast: false,
        span: span(),
    }
}

pub(super) fn int_lit(n: i64) -> Expression {
    Expression::Literal(Literal::Integer(n), span())
}

pub(super) fn str_lit(s: &str) -> Expression {
    Expression::Literal(Literal::String(s.into()), span())
}

pub(super) fn symbol_lit(s: &str) -> Expression {
    Expression::Literal(Literal::Symbol(s.into()), span())
}

pub(super) fn char_lit(c: char) -> Expression {
    Expression::Literal(Literal::Character(c), span())
}

pub(super) fn var(name: &str) -> Expression {
    Expression::Identifier(ident(name))
}

pub(super) fn class_ref(name: &str) -> Expression {
    Expression::ClassReference {
        name: ident(name),
        package: None,
        span: span(),
    }
}

pub(super) fn assign(name: &str, value: Expression) -> Expression {
    Expression::Assignment {
        target: Box::new(var(name)),
        value: Box::new(value),
        type_annotation: None,
        span: span(),
    }
}

// Helper: lex + parse a source string into a Module.
pub(super) fn parse_source(source: &str) -> Module {
    use crate::source_analysis::{lex_with_eof, parse};
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
}

// Helper: run the full diagnostic pipeline (type check + @expect suppression).
// Suppression is owned by `apply_expect_directives` in diagnostic_provider —
// the type checker itself does NOT suppress.
pub(super) fn run_with_expect(module: &Module, hierarchy: &ClassHierarchy) -> Vec<Diagnostic> {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    let mut diags = checker.take_diagnostics();
    crate::queries::diagnostic_provider::apply_expect_directives(module, &mut diags);
    diags
}

/// Helper: Build an `Erlang <module>` receiver expression.
/// Uses a different span (10,20) from the outer send so type map lookups are distinct.
pub(super) fn erlang_module_recv(module_name: &str) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(class_ref("Erlang")),
        selector: MessageSelector::Unary(module_name.into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(10, 20),
    }
}

pub(super) fn field_access(field_name: &str) -> Expression {
    Expression::FieldAccess {
        receiver: Box::new(var("self")),
        field: ident(field_name),
        span: span(),
    }
}

pub(super) fn field_assign(field_name: &str, value: Expression) -> Expression {
    Expression::Assignment {
        target: Box::new(field_access(field_name)),
        value: Box::new(value),
        type_annotation: None,
        span: span(),
    }
}

/// Helper: build a block expression `[body]`
pub(super) fn block_expr(body: Vec<Expression>) -> Expression {
    Expression::Block(Block::new(
        vec![],
        body.into_iter().map(ExpressionStatement::bare).collect(),
        span(),
    ))
}

/// Helper: build `receiver ifTrue: [block]`
pub(super) fn if_true(receiver: Expression, block: Expression) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![KeywordPart::new("ifTrue:", span())]),
        vec![block],
    )
}

/// Helper: build `receiver ifFalse: [block]`
pub(super) fn if_false(receiver: Expression, block: Expression) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![KeywordPart::new("ifFalse:", span())]),
        vec![block],
    )
}

/// Helper: build `receiver ifTrue: [block1] ifFalse: [block2]`
pub(super) fn if_true_if_false(
    receiver: Expression,
    true_block: Expression,
    false_block: Expression,
) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![
            KeywordPart::new("ifTrue:", span()),
            KeywordPart::new("ifFalse:", span()),
        ]),
        vec![true_block, false_block],
    )
}

/// Helper: build `x isNil` unary expression
pub(super) fn is_nil(var_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Unary("isNil".into()),
        vec![],
    )
}

/// Helper: build `x isError` unary expression (BT-1859)
pub(super) fn is_error(var_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Unary("isError".into()),
        vec![],
    )
}

/// Helper: build `x isOk` unary expression (BT-1859)
pub(super) fn is_ok(var_name: &str) -> Expression {
    msg_send(var(var_name), MessageSelector::Unary("isOk".into()), vec![])
}

/// Helper: build a symbol literal `#name`
pub(super) fn sym_lit(name: &str) -> Expression {
    Expression::Literal(Literal::Symbol(name.into()), span())
}

/// Helper: make a keyword method with typed parameters and a body
pub(super) fn make_keyword_method(
    selector_parts: &[&str],
    params: Vec<(&str, Option<&str>)>,
    body: Vec<Expression>,
) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Keyword(
            selector_parts
                .iter()
                .map(|k| KeywordPart::new(*k, span()))
                .collect(),
        ),
        parameters: params
            .into_iter()
            .map(|(name, ty)| {
                if let Some(t) = ty {
                    ParameterDefinition::with_type(ident(name), TypeAnnotation::Simple(ident(t)))
                } else {
                    ParameterDefinition::new(ident(name))
                }
            })
            .collect(),
        body: body.into_iter().map(ExpressionStatement::bare).collect(),
        return_type: None,
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    }
}

pub(super) fn counter_class_with_typed_state(
    methods: Vec<MethodDefinition>,
    state: Vec<StateDeclaration>,
) -> ClassDefinition {
    ClassDefinition {
        name: ident("Counter"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state,
        methods,
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    }
}

/// Helper to build a hierarchy with an extension method registered.
pub(super) fn hierarchy_with_extension(
    class_name: &str,
    selector: &str,
    arity: usize,
    param_types: Vec<Option<EcoString>>,
    return_type: Option<EcoString>,
) -> ClassHierarchy {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;

    let mut h = ClassHierarchy::with_builtins();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: class_name.into(),
        side: MethodSide::Instance,
        selector: selector.into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("test.bt"),
            span: span(),
            type_info: ExtensionTypeInfo {
                arity,
                param_types,
                return_type,
            },
        }],
    );
    h.register_extensions(&index);
    h
}

pub(super) fn make_class_with_methods(
    name: &str,
    instance_methods: Vec<MethodDefinition>,
) -> ClassDefinition {
    ClassDefinition {
        name: ident(name),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: instance_methods,
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    }
}

pub(super) fn make_method(selector: &str, body: Vec<Expression>) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Unary(selector.into()),
        parameters: vec![],
        body: body.into_iter().map(ExpressionStatement::bare).collect(),
        return_type: None,
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    }
}

pub(super) fn eco_string(s: &str) -> ecow::EcoString {
    ecow::EcoString::from(s)
}

/// Build a `GenResult(T, E)` class in the hierarchy for generic tests.
///
/// Uses `GenResult` instead of `Result` to avoid collision with the
/// builtin `Result` class (which has no `type_params`).
#[allow(clippy::too_many_lines)] // test fixture — length is proportional to ClassInfo fields
pub(super) fn add_generic_result_class(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let result_info = ClassInfo {
        name: eco_string("GenResult"),
        superclass: Some(eco_string("Value")),
        is_sealed: true,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("okValue"), eco_string("errReason")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("okValue"), eco_string("T"));
            m.insert(eco_string("errReason"), eco_string("E"));
            m
        },
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("unwrap"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("error"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("E")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("map:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("GenResult(R, E)")),
                param_types: vec![Some(eco_string("Block(T, R)"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("isOk"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Boolean")),
                param_types: vec![],
                doc: None,
            },
        ],
        class_methods: vec![
            MethodInfo {
                selector: eco_string("ok:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Self")),
                param_types: vec![Some(eco_string("T"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("error:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Self")),
                param_types: vec![Some(eco_string("E"))],
                doc: None,
            },
        ],
        class_variables: vec![],
        type_params: vec![eco_string("T"), eco_string("E")],
        type_param_bounds: vec![None, None],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![result_info]);
}

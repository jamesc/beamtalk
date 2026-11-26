// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test fixtures for `type_checker` per-feature tests.
//!
//! These helpers were previously inlined at the top (and middle) of the
//! monolithic `tests.rs`. They are re-exported into every child test module
//! via `use super::common::*;`.

pub(super) use super::super::type_resolver;
pub(super) use super::super::*;
pub(super) use crate::ast::{
    Block, CascadeMessage, ClassDefinition, ClassKind, ClassModifiers, CommentAttachment,
    ExpectCategory, Expression, ExpressionStatement, Identifier, KeywordPart, Literal, MatchArm,
    MessageSelector, MethodDefinition, Module, ParameterDefinition, Pattern, ProtocolDefinition,
    ProtocolMethodSignature, StateDeclaration, TypeAnnotation,
};
pub(super) use crate::semantic_analysis::class_hierarchy::DeclaredType;
pub(super) use crate::semantic_analysis::class_hierarchy::MethodInfo;
pub(super) use crate::source_analysis::{DiagnosticCategory, Span};

pub(super) use crate::test_helpers::test_support::{
    bare, empty_module, unit_ident as ident, unit_span as span,
};

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

pub(super) use crate::semantic_analysis::test_helpers::parse_and_build;
pub(super) use crate::test_helpers::test_support::{parse_ok, parse_source};

// Helper: run the full diagnostic pipeline (type check + @expect suppression).
// Suppression is owned by `apply_expect_directives` in diagnostic_provider —
// the type checker itself does NOT suppress.
pub(super) fn run_with_expect(module: &Module, hierarchy: &ClassHierarchy) -> Vec<Diagnostic> {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    let mut diags = checker.take_diagnostics();
    crate::compilation::diagnostics_policy::apply_expect_directives(module, &mut diags);
    diags
}

/// Source to diagnostics through the full pipeline: parse (lenient), build the
/// hierarchy, type check, then apply `@expect` suppression (BT-4678).
///
/// Panics if the hierarchy cannot be built. Use [`diags`] to skip suppression.
pub(super) fn diags_with_expect(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0;
    run_with_expect(&module, &hierarchy)
}

/// Source to the type checker's raw diagnostics: parse (lenient), build the
/// hierarchy, type check. No `@expect` suppression (BT-4678).
pub(super) fn diags(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0;
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    checker.take_diagnostics()
}

/// [`diags_with_expect`] for source that must parse without any non-lint
/// diagnostic (see [`parse_and_build`]): the strict-parse counterpart that the
/// full pipeline's `@expect` suppression applies to.
pub(super) fn diags_parse_ok_with_expect(source: &str) -> Vec<Diagnostic> {
    let (module, hierarchy) = parse_and_build(source);
    run_with_expect(&module, &hierarchy)
}

/// The hierarchy of a module holding only `class`. Panics if it cannot be built.
pub(super) fn build_class_hierarchy(class: ClassDefinition) -> ClassHierarchy {
    ClassHierarchy::build(&make_module_with_classes(vec![], vec![class])).0
}

/// Asserts `diags` is empty; `context` (the case name and/or source) names the
/// case in the failure message.
#[track_caller]
pub(super) fn assert_clean_diags(diags: &[Diagnostic], context: &str) {
    assert!(
        diags.is_empty(),
        "expected no diagnostics for:\n{context}\ngot: {diags:#?}"
    );
}

/// Asserts `diags` is not empty; `context` names the case in the failure message.
#[track_caller]
pub(super) fn assert_has_diags(diags: &[Diagnostic], context: &str) {
    assert!(
        !diags.is_empty(),
        "expected a diagnostic for:\n{context}\ngot none"
    );
}

/// Type checks an already-built `module` against `hierarchy` and hands back the
/// checker, for tests that inspect more than its diagnostics (BT-4678).
pub(super) fn check(module: &Module, hierarchy: &ClassHierarchy) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    checker
}

/// Like [`diags`], but the source must parse without any non-lint diagnostic
/// (lints are tolerated; see [`parse_and_build`]).
pub(super) fn diags_parse_ok(source: &str) -> Vec<Diagnostic> {
    let (module, hierarchy) = parse_and_build(source);
    check(&module, &hierarchy).take_diagnostics()
}

/// Inferred method return types and diagnostics for `source`, which must parse
/// without any non-lint diagnostic (see [`parse_and_build`]).
pub(super) fn return_types_and_diags(
    source: &str,
) -> (
    std::collections::HashMap<MethodReturnKey, InferredType>,
    Vec<Diagnostic>,
) {
    let (module, hierarchy) = parse_and_build(source);
    let mut checker = check(&module, &hierarchy);
    let diagnostics = checker.take_diagnostics();
    (checker.take_method_return_types(), diagnostics)
}

/// The `Type`-category diagnostics among `diags`.
pub(super) fn type_category_of(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

/// Type checks `module` with `registry` installed as the native type registry
/// and hands back the checker.
pub(super) fn check_with_registry(
    module: &Module,
    hierarchy: &ClassHierarchy,
    registry: impl Into<std::sync::Arc<NativeTypeRegistry>>,
) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(registry);
    checker.check_module(module, hierarchy);
    checker
}

/// Type checks `module` through `check_module_with_protocols` and hands back
/// the checker.
pub(super) fn check_with_protocols(
    module: &Module,
    hierarchy: &ClassHierarchy,
    protocols: &ProtocolRegistry,
) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(module, hierarchy, protocols);
    checker
}

/// The inferred type of `expr` in an empty environment.
pub(super) fn infer(expr: &Expression, hierarchy: &ClassHierarchy) -> InferredType {
    infer_with_locals(expr, hierarchy, &[])
}

/// The inferred type of `expr` in an environment holding `locals`.
pub(super) fn infer_with_locals(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    locals: &[(&str, InferredType)],
) -> InferredType {
    infer_with_diags(expr, hierarchy, locals).0
}

/// Like [`infer_with_locals`], but also hands back the diagnostics the
/// checker emitted while inferring `expr`.
pub(super) fn infer_with_diags(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    locals: &[(&str, InferredType)],
) -> (InferredType, Vec<Diagnostic>) {
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    for (name, ty) in locals {
        env.set_local(*name, ty.clone());
    }
    let ty = checker.infer_expr(expr, hierarchy, &mut env, false);
    (ty, checker.take_diagnostics())
}

/// A type checker and the environment it infers in, for the tests that run
/// several inference steps against one shared environment (assignments bind
/// locals that later steps read) or inspect the environment afterwards.
///
/// The single-step cases are [`infer`], [`infer_with_locals`] and
/// [`infer_with_diags`].
pub(super) struct InferSession {
    pub(super) checker: TypeChecker,
    pub(super) env: TypeEnv,
}

impl InferSession {
    /// A session with a fresh checker and an empty environment.
    pub(super) fn new() -> Self {
        Self {
            checker: TypeChecker::new(),
            env: TypeEnv::new(),
        }
    }

    /// Infers `expr` in the session's environment.
    pub(super) fn infer(&mut self, expr: &Expression, hierarchy: &ClassHierarchy) -> InferredType {
        self.checker
            .infer_expr(expr, hierarchy, &mut self.env, false)
    }
}

/// Runs `check_argument_types` for an instance-side send of `selector` to
/// `class_name` with no argument expressions or receiver type arguments, and
/// hands back the checker.
pub(super) fn check_args(
    class_name: &str,
    selector: &str,
    arg_types: &[InferredType],
    hierarchy: &ClassHierarchy,
) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &class_name.into(),
        selector,
        arg_types,
        span(),
        hierarchy,
        false,
        None,
        None,
        &[],
    );
    checker
}

/// [`check_args`] with the argument expressions in hand, for the checks that
/// look at the expression itself (class literals).
pub(super) fn check_args_with_exprs(
    class_name: &str,
    selector: &str,
    arg_types: &[InferredType],
    arg_exprs: &[Expression],
    hierarchy: &ClassHierarchy,
) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &class_name.into(),
        selector,
        arg_types,
        span(),
        hierarchy,
        false,
        Some(arg_exprs),
        None,
        &[],
    );
    checker
}

/// Runs `check_return_type` for `method` in `class_name` and hands back the
/// checker.
pub(super) fn check_return(
    method: &MethodDefinition,
    body_type: &InferredType,
    class_name: &str,
    hierarchy: &ClassHierarchy,
) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_return_type(
        method,
        body_type,
        &class_name.into(),
        hierarchy,
        crate::semantic_analysis::type_checker::validation::ReturnSite::Tail,
    );
    checker
}

/// Runs `check_ffi_argument_types` for `module_name:function_name` and hands
/// back the checker.
pub(super) fn check_ffi_args(
    module_name: &str,
    function_name: &str,
    sig: &crate::semantic_analysis::type_checker::native_type_registry::FunctionSignature,
    arg_types: &[InferredType],
    hierarchy: &ClassHierarchy,
) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_ffi_argument_types(
        module_name,
        function_name,
        sig,
        arg_types,
        span(),
        hierarchy,
    );
    checker
}

/// Runs `check_type_param_bounds` for `class_name<type_args>` and hands back
/// the checker.
pub(super) fn check_bounds(
    class_name: &str,
    type_args: &[InferredType],
    hierarchy: &ClassHierarchy,
    protocols: &ProtocolRegistry,
) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(&class_name.into(), type_args, span(), hierarchy, protocols);
    checker
}

/// `refine_class_narrowing` of `info` against `env`.
pub(super) fn refine_narrowing(
    info: super::super::narrowing::info::NarrowingInfo,
    env: &TypeEnv,
    hierarchy: &ClassHierarchy,
) -> super::super::narrowing::info::NarrowingInfo {
    TypeChecker::new().refine_class_narrowing(info, env, hierarchy, span())
}

/// Source to the diagnostics of `check_module_with_protocols` against an empty
/// protocol registry, with the user's classes merged onto the built-ins.
pub(super) fn run_with_protocols(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let mut hierarchy = ClassHierarchy::with_builtins();
    let user_hierarchy = ClassHierarchy::build(&module).0;
    hierarchy.merge(&user_hierarchy);

    let mut checker = check_with_protocols(
        &module,
        &hierarchy,
        &crate::semantic_analysis::protocol_registry::ProtocolRegistry::new(),
    );
    checker.take_diagnostics()
}

/// A hierarchy holding one `typed Object subclass: <class_name>`.
pub(super) fn typed_class_hierarchy(class_name: &str) -> ClassHierarchy {
    let source = format!("typed Object subclass: {class_name}\n");
    let module = parse_source(&source);
    ClassHierarchy::build(&module).0
}

/// The return-type mismatch diagnostics `checker` has collected.
pub(super) fn type_mismatch_diagnostics(checker: &TypeChecker) -> Vec<&Diagnostic> {
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect()
}

/// [`diags_with_expect`] keeping only `Type` diagnostics.
pub(super) fn type_diags(source: &str) -> Vec<Diagnostic> {
    diags_with_expect(source)
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

/// [`diags_with_expect`] keeping only `Type` and `Dnu` diagnostics.
pub(super) fn type_dnu_diags(source: &str) -> Vec<Diagnostic> {
    diags_with_expect(source)
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(DiagnosticCategory::Type | DiagnosticCategory::Dnu)
            )
        })
        .collect()
}

/// Source to the diagnostics of the whole semantic `analyse` pass. The source
/// must parse without any diagnostic (BT-4678).
pub(super) fn analyse_diags(source: &str) -> Vec<Diagnostic> {
    let module = parse_ok(source);
    crate::semantic_analysis::analyse(&module).diagnostics
}

/// Messages of the `Type` and `Dnu` diagnostics from [`analyse_diags`].
pub(super) fn messages(source: &str) -> Vec<String> {
    analyse_diags(source)
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(DiagnosticCategory::Type | DiagnosticCategory::Dnu)
            )
        })
        .map(|d| d.message.to_string())
        .collect()
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

/// Helper: build `x isError` unary expression
pub(super) fn is_error(var_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Unary("isError".into()),
        vec![],
    )
}

/// Helper: build `x isOk` unary expression
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
    MethodDefinition::new(
        MessageSelector::Keyword(
            selector_parts
                .iter()
                .map(|k| KeywordPart::new(*k, span()))
                .collect(),
        ),
        params
            .into_iter()
            .map(|(name, ty)| {
                if let Some(t) = ty {
                    ParameterDefinition::with_type(ident(name), TypeAnnotation::Simple(ident(t)))
                } else {
                    ParameterDefinition::new(ident(name))
                }
            })
            .collect(),
        body.into_iter().map(ExpressionStatement::bare).collect(),
        span(),
    )
}

pub(super) fn counter_class_with_typed_state(
    methods: Vec<MethodDefinition>,
    state: Vec<StateDeclaration>,
) -> ClassDefinition {
    ClassDefinition {
        state,
        methods,
        ..ClassDefinition::test_at("Counter", Some("Object"), ClassKind::Object, span())
    }
}

/// Helper to build a hierarchy where `class_name` additionally has an
/// instance method `selector` with the given signature.
#[allow(clippy::needless_pass_by_value)] // callers build the signature inline
pub(super) fn hierarchy_with_method(
    class_name: &str,
    selector: &str,
    arity: usize,
    param_types: Vec<Option<EcoString>>,
    return_type: Option<EcoString>,
) -> ClassHierarchy {
    use crate::semantic_analysis::class_hierarchy::{DeclaredType, MethodInfo};

    let mut h = ClassHierarchy::with_builtins();
    assert_eq!(arity, param_types.len(), "arity must match the parameters");
    let method = MethodInfo::synthesized(
        selector,
        class_name,
        return_type.as_deref().map(DeclaredType::parse),
        param_types
            .iter()
            .map(|p| p.as_deref().map(DeclaredType::parse))
            .collect(),
        None,
    );
    h.get_class_mut(class_name)
        .expect("class is a builtin")
        .methods
        .push(method);
    h.rebuild_all_indexes();
    h
}

pub(super) fn make_class_with_methods(
    name: &str,
    instance_methods: Vec<MethodDefinition>,
) -> ClassDefinition {
    ClassDefinition {
        methods: instance_methods,
        ..ClassDefinition::test_at(name, Some("Object"), ClassKind::Value, span())
    }
}

pub(super) fn make_method(selector: &str, body: Vec<Expression>) -> MethodDefinition {
    MethodDefinition {
        body: body.into_iter().map(ExpressionStatement::bare).collect(),
        ..MethodDefinition::test_at(selector, span())
    }
}

pub(super) fn eco_string(s: &str) -> ecow::EcoString {
    ecow::EcoString::from(s)
}

/// Helper: build a `MethodInfo` with the given param types and return type.
///
/// Shared by the `inference_method_local_params` and `inference_generics_bt2023`
/// feature modules — both exercise `TypeChecker::infer_method_local_params`
/// against fixture `MethodInfo`s.
pub(super) fn method_info(
    selector: &str,
    param_types: Vec<Option<&str>>,
    return_type: Option<&str>,
) -> MethodInfo {
    let arity = param_types.len();
    MethodInfo {
        selector: selector.into(),
        arity,
        defined_in: "TestClass".into(),
        is_sealed: false,
        is_internal: false,
        spawns_block: false,
        return_type: return_type.map(DeclaredType::parse),
        param_types: param_types
            .into_iter()
            .map(|p| p.map(DeclaredType::parse))
            .collect(),
        doc: None,

        origin: None,
        primitive: None,
        constant_value: None,
        answers_receiver: false,
    }
}

/// An `Object` subclass `name` with one primary instance method `selector`
/// taking a single `param_type` parameter and answering `return_type`.
pub(super) fn class_with_method(
    name: &str,
    selector: &str,
    param_type: &str,
    return_type: &str,
) -> crate::semantic_analysis::class_hierarchy::ClassInfo {
    crate::semantic_analysis::class_hierarchy::ClassInfo {
        methods: vec![MethodInfo {
            defined_in: name.into(),
            ..method_info(selector, vec![Some(param_type)], Some(return_type))
        }],
        ..crate::semantic_analysis::class_hierarchy::ClassInfo::test(name, Some("Object"))
    }
}

/// Build a `GenResult(T, E)` class in the hierarchy for generic tests.
///
/// Uses `GenResult` instead of `Result` to avoid collision with the
/// builtin `Result` class (which has no `type_params`).
#[allow(clippy::too_many_lines)] // test fixture — length is proportional to ClassInfo fields
pub(super) fn add_generic_result_class(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let result_info = ClassInfo {
        is_sealed: true,
        is_value: true,
        state: vec![eco_string("okValue"), eco_string("errReason")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("okValue"), DeclaredType::parse("T"));
            m.insert(eco_string("errReason"), DeclaredType::parse("E"));
            m
        },
        methods: vec![
            MethodInfo {
                selector: eco_string("unwrap"),
                arity: 0,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::parse("T")),
                param_types: vec![],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            },
            MethodInfo {
                selector: eco_string("error"),
                arity: 0,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::parse("E")),
                param_types: vec![],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            },
            MethodInfo {
                selector: eco_string("map:"),
                arity: 1,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::parse("GenResult(R, E)")),
                param_types: vec![Some(DeclaredType::parse("Block(T, R)"))],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            },
            MethodInfo {
                selector: eco_string("isOk"),
                arity: 0,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::parse("Boolean")),
                param_types: vec![],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            },
        ],
        class_methods: vec![
            MethodInfo {
                selector: eco_string("ok:"),
                arity: 1,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::SelfType),
                param_types: vec![Some(DeclaredType::parse("T"))],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            },
            MethodInfo {
                selector: eco_string("error:"),
                arity: 1,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::SelfType),
                param_types: vec![Some(DeclaredType::parse("E"))],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            },
        ],
        type_params: vec![eco_string("T"), eco_string("E")],
        type_param_bounds: vec![None, None],
        ..ClassInfo::test("GenResult", Some("Value"))
    };

    hierarchy.add_from_beam_meta(vec![result_info]);
}

/// Assert the module's `type_map` contains a Union whose members' class names
/// match `expected` (order-insensitive). Returns the matching type on success.
///
/// Uses `filter_map(InferredType::as_known)`, which silently drops any
/// non-`Known` member (e.g. a nested `Union`). `union_of` always flattens
/// nested unions into one flat member list, so a flat `Union` is the only
/// shape this ever needs to match against in practice.
pub(super) fn find_union_in_type_map<'a>(
    type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
    expected: &[&str],
) -> Option<&'a InferredType> {
    type_map.iter().find_map(|(_, ty)| {
        if let InferredType::Union { members, .. } = ty {
            let names: std::collections::BTreeSet<String> = members
                .iter()
                .filter_map(|m| m.as_known().map(std::string::ToString::to_string))
                .collect();
            let want: std::collections::BTreeSet<String> =
                expected.iter().map(|s| (*s).to_string()).collect();
            if names == want {
                return Some(ty);
            }
        }
        None
    })
}

/// Walk the module AST to find the first `MessageSend` whose selector name
/// matches `selector_name`, then look up its inferred type in `type_map`.
/// Checking diagnostics alone isn't enough because `check_return_type` bails
/// on `Union` / `Dynamic` inferred bodies.
///
/// Built on top of the shared [`crate::ast::visitor::Visitor`] trait so that
/// variant coverage is exhaustive (`ArrayLiteral`, `ListLiteral`, `Match`,
/// etc. are all searched). Unlike the default opaque-block visitor, this
/// walker overrides `visit_block` to descend: callers need to find sends
/// anywhere in the AST, including inside nested block bodies.
pub(super) fn find_send_inferred_ty<'a>(
    module: &'a crate::ast::Module,
    type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
    selector_name: &'a str,
) -> Option<&'a InferredType> {
    use crate::ast::visitor::{Visitor, walk_block, walk_expr};
    use crate::ast::{Expression, MessageSelector};

    struct Finder<'a> {
        type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
        selector_name: &'a str,
        found: Option<&'a InferredType>,
    }
    impl<'a> Visitor<'a> for Finder<'a> {
        fn visit_expr(&mut self, e: &'a Expression) {
            if self.found.is_some() {
                return;
            }
            if let Expression::MessageSend { selector, span, .. } = e {
                let this_sel = match selector {
                    MessageSelector::Unary(s) | MessageSelector::Binary(s) => s.to_string(),
                    MessageSelector::Keyword(parts) => {
                        parts.iter().map(|p| p.keyword.as_str()).collect::<String>()
                    }
                };
                if this_sel == self.selector_name {
                    // Match: capture the outermost send's type. Intentionally
                    // do NOT descend into receiver/arguments — tests want the
                    // outermost send, not a matching send buried inside its
                    // arguments.
                    self.found = self.type_map.get(*span);
                    return;
                }
            }
            walk_expr(self, e);
        }
        fn visit_block(&mut self, block: &'a crate::ast::Block) {
            // Opt-in descent: tests want to see into nested blocks.
            walk_block(self, block);
        }
    }

    let mut finder = Finder {
        type_map,
        selector_name,
        found: None,
    };
    for class in &module.classes {
        for method in &class.methods {
            for stmt in &method.body {
                finder.visit_expr(&stmt.expression);
                if finder.found.is_some() {
                    return finder.found;
                }
            }
        }
    }
    finder.found
}

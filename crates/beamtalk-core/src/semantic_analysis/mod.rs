// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis for Beamtalk.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module performs semantic analysis on the AST, including:
//! - Variable scope and lifetime analysis (via `scope` module)
//! - Pattern variable binding in match expressions
//! - Block context determination (control flow, stored, passed)
//! - Capture analysis for blocks
//! - Mutation tracking for captured variables in blocks
//!
//! The analysis produces diagnostics and metadata used by the code generator.

use crate::ast::{Expression, MatchArm, Module};
use crate::source_analysis::{Diagnostic, Span};
use ecow::EcoString;
use std::collections::HashMap;

mod block_analyzer;
pub(crate) mod block_context;
pub mod class_hierarchy;
pub mod error;
pub(crate) mod method_validators;
pub mod module_validator;
pub mod name_resolver;
pub(crate) mod pattern_bindings;
pub mod primitive_validator;
pub(crate) mod scope;
pub(crate) mod string_utils;
pub mod type_checker;
pub(crate) mod validators;

// Property-based tests for semantic analysis (ADR 0011 Phase 2)
#[cfg(test)]
mod property_tests;
#[cfg(test)]
pub mod test_helpers;

pub use class_hierarchy::ClassHierarchy;
pub use error::{SemanticError, SemanticErrorKind};
pub use name_resolver::NameResolver;
pub use pattern_bindings::extract_pattern_bindings;
pub use scope::BindingKind;
pub use type_checker::{InferredType, TypeChecker, TypeMap, infer_types};

/// BT-738: Warn when a user-defined class name shadows a stdlib built-in.
///
/// Must NOT be called for stdlib compilation (`stdlib_mode = true`). Call
/// this alongside `validate_primitives`, guarded by `!options.stdlib_mode`.
pub fn check_stdlib_name_shadowing(
    module: &Module,
    diagnostics: &mut Vec<crate::source_analysis::Diagnostic>,
) {
    validators::check_stdlib_name_shadowing(module, diagnostics);
}

/// Result of semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisResult {
    /// Diagnostics (errors and warnings) from analysis.
    pub diagnostics: Vec<Diagnostic>,

    /// Block metadata indexed by block span.
    pub block_info: HashMap<Span, BlockInfo>,

    /// Static class hierarchy (built-in + user-defined classes).
    pub class_hierarchy: ClassHierarchy,
}

impl AnalysisResult {
    /// Create a new empty analysis result.
    #[must_use]
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            block_info: HashMap::new(),
            class_hierarchy: ClassHierarchy::with_builtins(),
        }
    }
}

impl Default for AnalysisResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Information about a block expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockInfo {
    /// Context in which the block is used.
    pub context: BlockContext,

    /// Variables captured from outer scopes.
    pub captures: Vec<CapturedVar>,

    /// Mutations that occur within the block.
    pub mutations: Vec<Mutation>,
}

/// Context in which a block is used, routing between codegen tiers (ADR 0041).
///
/// - `ControlFlow` → **Tier 1** inline codegen (optimized pack/unpack)
/// - `Stored` / `Passed` / `Other` / `Unknown` → **Tier 2** universal stateful protocol
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockContext {
    /// Block in control flow position → **Tier 1** inline codegen.
    /// Whitelisted selectors (whileTrue:, ifTrue:, do:, etc.) generate optimized
    /// pack/unpack scaffolding directly.
    ControlFlow,

    /// Block stored in a variable or field → **Tier 2** stateful protocol.
    /// Captured variable mutations are threaded through `StateAcc` maps.
    Stored,

    /// Block passed as argument to a message send → **Tier 2** stateful protocol.
    Passed,

    /// Other known context (e.g., immediate evaluation) → **Tier 2** by default.
    Other,

    /// Context could not be determined → **Tier 2** by default.
    Unknown,
}

/// A variable captured from an outer scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapturedVar {
    /// Name of the captured variable.
    pub name: EcoString,

    /// Span where the variable was defined.
    pub defined_at: Span,
}

/// A mutation that occurs within a block or method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mutation {
    /// Kind of mutation.
    pub kind: MutationKind,

    /// Span of the mutation.
    pub span: Span,
}

/// Type of mutation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MutationKind {
    /// Assignment to a local variable.
    LocalVariable { name: EcoString },

    /// Assignment to a captured variable.
    CapturedVariable { name: EcoString },

    /// Assignment to an object field.
    Field { name: EcoString },
}

/// Perform semantic analysis on a module.
///
/// This is the main entry point for semantic analysis. It orchestrates the
/// `NameResolver` and `TypeChecker` domain services to analyze the module AST
/// and returns diagnostics and metadata for code generation.
///
/// **DDD Context:** Semantic Analysis
///
/// This function orchestrates the following domain services:
/// - `NameResolver`: Resolves identifiers to bindings and detects undefined variables
/// - `TypeChecker`: Validates type constraints (currently stub)
/// - `Analyser`: Performs block context analysis, capture tracking, and mutation detection
///
/// # Examples
///
/// ```
/// # use beamtalk_core::semantic_analysis::analyse;
/// # use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
/// let module = Module::new(vec![], Span::default());
/// let result = analyse(&module);
/// assert_eq!(result.diagnostics.len(), 0);
/// ```
pub fn analyse(module: &Module) -> AnalysisResult {
    analyse_full(module, &[], false, false)
}

/// Analyse a module with pre-defined variables (for REPL context).
///
/// Variables passed in `known_vars` are treated as already defined,
/// preventing "Undefined variable" errors for REPL session variables.
///
/// **DDD Context:** Semantic Analysis
///
/// This function orchestrates the `NameResolver`, `TypeChecker`, and block analysis
/// services. Pre-defining known variables is essential for REPL contexts where
/// users build up state incrementally across multiple evaluations.
pub fn analyse_with_known_vars(module: &Module, known_vars: &[&str]) -> AnalysisResult {
    analyse_full(module, known_vars, false, false)
}

/// Analyse a module with compiler options controlling stdlib-specific behaviour.
///
/// When `stdlib_mode` is true, built-in classes are permitted to subclass sealed
/// classes (BT-791). This should only be set when compiling stdlib sources.
pub fn analyse_with_options(module: &Module, options: &crate::CompilerOptions) -> AnalysisResult {
    analyse_full(
        module,
        &[],
        options.stdlib_mode,
        options.skip_module_expression_lint,
    )
}

/// Internal: full analysis with all knobs.
fn analyse_full(
    module: &Module,
    known_vars: &[&str],
    stdlib_mode: bool,
    skip_module_expression_lint: bool,
) -> AnalysisResult {
    let mut result = AnalysisResult::new();

    // Phase 0: Build Class Hierarchy (ADR 0006 Phase 1a)
    let (hierarchy_result, hierarchy_diags) =
        ClassHierarchy::build_with_options(module, stdlib_mode);
    // build_with_options is infallible; propagate any diagnostics it produced
    result.class_hierarchy = hierarchy_result.expect("ClassHierarchy::build is infallible");
    result.diagnostics.extend(hierarchy_diags);

    // Phase 1: Name Resolution
    let mut name_resolver = NameResolver::new();
    name_resolver.define_known_vars(known_vars, module.span);
    name_resolver.resolve_module(module);
    result.diagnostics.extend(name_resolver.take_diagnostics());

    // Extract scope from NameResolver to pass to Analyser
    // This eliminates duplicate scope building - the scope already contains:
    // - Built-in identifiers (true, false, nil)
    // - Known REPL variables
    // - All variable bindings from name resolution
    let scope = name_resolver.into_scope();

    // Phase 2: Type Checking (ADR 0025 Phase 1 — zero-syntax inference)
    let mut type_checker = TypeChecker::new();
    type_checker.check_module(module, &result.class_hierarchy);
    result.diagnostics.extend(type_checker.take_diagnostics());
    let type_map = type_checker.take_type_map();

    // Phase 3: Block Context Analysis (captures, mutations, context determination)
    // Receive scope from NameResolver and TypeMap from TypeChecker
    let mut analyser = Analyser::with_scope(scope, type_map);

    analyser.analyse_module(module);
    result.diagnostics.extend(analyser.result.diagnostics);
    result.block_info = analyser.result.block_info;

    // Phase 4: Abstract instantiation check (BT-105)
    validators::check_abstract_instantiation(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );

    // Phase 5: Class-aware diagnostics (BT-563)
    validators::check_actor_new_usage(module, &result.class_hierarchy, &mut result.diagnostics);
    validators::check_new_field_names(module, &result.class_hierarchy, &mut result.diagnostics);
    validators::check_class_variable_access(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    validators::check_empty_method_bodies(module, &mut result.diagnostics);
    validators::check_value_slot_assignment(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    // BT-919: Reject cast (!) on value types
    validators::check_cast_on_value_type(module, &result.class_hierarchy, &mut result.diagnostics);
    // BT-950: Warn on redundant assignment (x := x)
    validators::check_redundant_assignment(module, &mut result.diagnostics);
    // BT-953: Hint on self capture in collection HOF blocks (deadlock risk)
    validators::check_self_capture_in_actor_block(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    // BT-955: Warn on literal boolean conditions (always true / always false)
    validators::check_literal_boolean_condition(module, &mut result.diagnostics);

    // BT-951/BT-979: Lint on effect-free statements (suppressed during normal compile).
    // Module-level expressions are checked by default; set skip_module_expression_lint
    // to opt out (bootstrap-test compilation uses this).
    validators::check_effect_free_statements(
        module,
        &mut result.diagnostics,
        skip_module_expression_lint,
    );

    // Phase 6: Module-level validation (BT-349)
    let module_diags = module_validator::validate_single_class(module);
    result.diagnostics.extend(module_diags);

    result
}

/// Internal analyser state.
struct Analyser {
    result: AnalysisResult,
    scope: scope::Scope,
    method_validators: method_validators::MethodValidatorRegistry,
    type_map: TypeMap,
}

impl Analyser {
    /// Creates a new analyser with an existing scope from `NameResolver`
    /// and a `TypeMap` from `TypeChecker`.
    ///
    /// This constructor receives the scope built by `NameResolver`, eliminating
    /// duplicate scope construction. The scope already contains:
    /// - Built-in identifiers (true, false, nil)
    /// - Known REPL variables (if any)
    /// - All variable bindings from name resolution
    fn with_scope(scope: scope::Scope, type_map: TypeMap) -> Self {
        Self {
            result: AnalysisResult::new(),
            scope,
            method_validators: method_validators::MethodValidatorRegistry::new(),
            type_map,
        }
    }

    fn analyse_module(&mut self, module: &Module) {
        // Scope is now received from NameResolver, already populated with:
        // - Built-in identifiers (true, false, nil)
        // - Known REPL variables
        // - All variable bindings from name resolution
        // No need to re-define them here.

        // Analyse top-level expressions
        for stmt in &module.expressions {
            self.analyse_expression(&stmt.expression, None);
        }

        // Analyse classes
        for class in &module.classes {
            self.analyse_class(class);
        }
    }

    fn analyse_class(&mut self, class: &crate::ast::ClassDefinition) {
        self.scope.push(); // Enter class scope (depth 1)

        // Define state variables in class scope
        for state in &class.state {
            self.scope
                .define(&state.name.name, state.span, BindingKind::InstanceField);
        }

        // Analyse methods
        for method in &class.methods {
            self.analyse_method(method);
        }

        self.scope.pop(); // Exit class scope
    }

    fn analyse_method(&mut self, method: &crate::ast::MethodDefinition) {
        self.scope.push(); // Enter method scope (depth 2)

        // Define 'self' - implicitly available in all method bodies.
        //
        // Although 'self' is conceptually the receiver *parameter*, we classify it
        // as a `Local` binding rather than `Parameter` for two reasons:
        //   - Consistency with other implicit bindings (true, false, nil), which
        //     are also modeled as locals that are always in scope.
        //   - It maintains a semantic distinction between explicit user-declared
        //     parameters (marked as `Parameter`) and implicit bindings provided
        //     by the language runtime.
        //
        // If future type checking or code generation needs to treat 'self' as a
        // formal parameter, this BindingKind choice can be revisited, but the
        // current behavior is intentional.
        self.scope.define("self", method.span, BindingKind::Local);

        // Define method parameters
        for param in &method.parameters {
            self.scope
                .define(&param.name.name, param.name.span, BindingKind::Parameter);
        }

        // Analyse method body
        for stmt in &method.body {
            self.analyse_expression(&stmt.expression, None);
        }

        self.scope.pop(); // Exit method scope
    }

    #[allow(clippy::too_many_lines)] // one arm per Expression variant
    fn analyse_expression(&mut self, expr: &Expression, parent_context: Option<ExprContext>) {
        #[allow(clippy::enum_glob_use)] // cleaner match arms
        use crate::ast::Expression::*;

        match expr {
            Identifier(_id) => {
                // Binding identifiers to declarations and reporting undefined variables
                // is handled by `NameResolver`. The semantic analyser still maintains
                // its own scope (`self.scope`), but only for capture/mutation analysis
                // and related metadata, not for additional name-resolution diagnostics.
            }

            Assignment { target, value, .. } => {
                // Handle assignment target
                match target.as_ref() {
                    Identifier(id) => {
                        // Define in scope for capture tracking (even though diagnostics are in NameResolver)
                        // Use lookup_immut: assigning to a variable is not a "read"
                        if self.scope.lookup_immut(&id.name).is_none() {
                            self.scope.define(&id.name, id.span, BindingKind::Local);
                        }
                    }
                    _ => {
                        // For field access, analyze the target (especially the receiver)
                        self.analyse_expression(target, None);
                    }
                }
                // Pass Assignment context so blocks know they're being stored
                self.analyse_expression(value, Some(ExprContext::Assignment));
            }

            Block(block) => {
                self.analyse_block(block, parent_context);
            }

            MessageSend {
                receiver,
                selector,
                arguments,
                span,
                ..
            } => {
                self.analyse_expression(receiver, None);

                // Determine context for block arguments
                let selector_str = block_context::selector_to_string(selector);

                // Run method-specific validators (reuse selector_str to avoid extra allocation)
                if let Some(validator) = self.method_validators.get(&selector_str) {
                    let receiver_type = self.type_map.get(receiver.span());
                    let diagnostics = validator.validate(selector, arguments, receiver_type, *span);
                    self.result.diagnostics.extend(diagnostics);
                }

                // Run receiver-based validators
                if let Some(diag) = method_validators::validate_primitive_instantiation(
                    receiver,
                    &selector_str,
                    *span,
                ) {
                    self.result.diagnostics.push(diag);
                }
                if let Some(diag) =
                    method_validators::validate_immutable_mutation(receiver, &selector_str, *span)
                {
                    self.result.diagnostics.push(diag);
                }

                for (i, arg) in arguments.iter().enumerate() {
                    let is_control_flow = block_context::is_control_flow_selector(&selector_str, i);
                    let context = if is_control_flow {
                        Some(ExprContext::ControlFlowArg)
                    } else {
                        Some(ExprContext::MessageArg)
                    };
                    self.analyse_expression(arg, context);
                }
            }

            FieldAccess { receiver, .. } => {
                self.analyse_expression(receiver, None);
            }

            Cascade {
                receiver, messages, ..
            } => {
                self.analyse_expression(receiver, None);
                for msg in messages {
                    // Apply selector-based context detection for cascade messages
                    let selector_str = block_context::selector_to_string(&msg.selector);

                    // Run method-specific validators for cascade messages
                    if let Some(validator) = self.method_validators.get(&selector_str) {
                        let receiver_type = self.type_map.get(receiver.span());
                        let diagnostics = validator.validate(
                            &msg.selector,
                            &msg.arguments,
                            receiver_type,
                            msg.span,
                        );
                        self.result.diagnostics.extend(diagnostics);
                    }

                    // Run receiver-based validators for cascade messages
                    if let Some(diag) = method_validators::validate_primitive_instantiation(
                        receiver,
                        &selector_str,
                        msg.span,
                    ) {
                        self.result.diagnostics.push(diag);
                    }
                    if let Some(diag) = method_validators::validate_immutable_mutation(
                        receiver,
                        &selector_str,
                        msg.span,
                    ) {
                        self.result.diagnostics.push(diag);
                    }

                    for (i, arg) in msg.arguments.iter().enumerate() {
                        let is_control_flow =
                            block_context::is_control_flow_selector(&selector_str, i);
                        let context = if is_control_flow {
                            Some(ExprContext::ControlFlowArg)
                        } else {
                            Some(ExprContext::MessageArg)
                        };
                        self.analyse_expression(arg, context);
                    }
                }
            }

            Return { value, .. } => {
                self.analyse_expression(value, None);
            }

            Parenthesized { expression, .. } => {
                self.analyse_expression(expression, parent_context);
            }

            Match { value, arms, .. } => {
                self.analyse_expression(value, None);
                for arm in arms {
                    self.analyse_match_arm(arm);
                }
            }

            MapLiteral { pairs, .. } => {
                // Analyze key and value expressions in map literals
                for pair in pairs {
                    self.analyse_expression(&pair.key, None);
                    self.analyse_expression(&pair.value, None);
                }
            }

            ListLiteral { elements, tail, .. } => {
                for elem in elements {
                    self.analyse_expression(elem, None);
                }
                if let Some(t) = tail {
                    self.analyse_expression(t, None);
                }
            }

            ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.analyse_expression(elem, None);
                }
            }

            Literal(..)
            | Super(..)
            | Error { .. }
            | ClassReference { .. }
            | Primitive { .. }
            | ExpectDirective { .. } => {
                // No analysis needed
            }

            StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(expr) = segment {
                        self.analyse_expression(expr, None);
                    }
                }
            }
        }
    }

    fn analyse_match_arm(&mut self, arm: &MatchArm) {
        // Create a new scope for this match arm
        self.scope.push();

        // Pattern variable binding is now handled by NameResolver
        // Extract bindings here just to define them in the scope for captures/mutations tracking
        let (bindings, _pattern_diagnostics) =
            crate::semantic_analysis::extract_pattern_bindings(&arm.pattern);
        // Note: diagnostics are already collected by NameResolver, no need to duplicate

        for binding in bindings {
            self.scope
                .define(&binding.name, binding.span, BindingKind::Local);
        }

        // Analyze guard expression (if present) - can see pattern variables
        if let Some(guard) = &arm.guard {
            self.analyse_expression(guard, None);
        }

        // Analyze body expression - can see pattern variables
        self.analyse_expression(&arm.body, None);

        // Exit match arm scope
        self.scope.pop();
    }
}

/// Context in which an expression appears.
#[derive(Debug, Clone, Copy)]
enum ExprContext {
    /// Expression is an argument to a control flow message.
    ControlFlowArg,
    /// Expression is an argument to a regular message.
    MessageArg,
    /// Expression is being assigned to a variable.
    Assignment,
}

#[cfg(test)]
mod tests {
    //! Tests for semantic analysis: analysis pipeline, block info, and error construction.
    use super::test_helpers::test_span;
    use super::*;
    use crate::ast::{
        Block, BlockParameter, ClassDefinition, ClassKind, CommentAttachment, Expression,
        ExpressionStatement, Identifier, Literal, MatchArm, MessageSelector, MethodDefinition,
        Pattern, StateDeclaration, StringSegment,
    };
    use crate::source_analysis::{Severity, Span};

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    #[test]
    fn test_analyse_empty_module() {
        let module = Module::new(vec![], Span::default());
        let result = analyse(&module);

        assert_eq!(result.diagnostics.len(), 0);
        assert_eq!(result.block_info.len(), 0);
    }

    #[test]
    fn test_analysis_result_default() {
        let result = AnalysisResult::default();
        assert_eq!(result.diagnostics.len(), 0);
        assert_eq!(result.block_info.len(), 0);
    }

    #[test]
    fn test_block_context_values() {
        // Ensure all variants are constructible
        let contexts = [
            BlockContext::ControlFlow,
            BlockContext::Stored,
            BlockContext::Passed,
            BlockContext::Other,
            BlockContext::Unknown,
        ];

        assert_eq!(contexts.len(), 5);
    }

    #[test]
    fn test_semantic_error_creation() {
        let error = SemanticError::new(
            SemanticErrorKind::UndefinedVariable { name: "foo".into() },
            Span::default(),
        );

        assert!(matches!(
            error.kind,
            SemanticErrorKind::UndefinedVariable { .. }
        ));
    }

    #[test]
    fn test_block_info_construction() {
        let block_info = BlockInfo {
            context: BlockContext::ControlFlow,
            captures: vec![CapturedVar {
                name: "count".into(),
                defined_at: Span::default(),
            }],
            mutations: vec![Mutation {
                kind: MutationKind::LocalVariable { name: "x".into() },
                span: Span::default(),
            }],
        };

        assert_eq!(block_info.context, BlockContext::ControlFlow);
        assert_eq!(block_info.captures.len(), 1);
        assert_eq!(block_info.mutations.len(), 1);
    }

    #[test]
    fn test_captured_var_construction() {
        let captured = CapturedVar {
            name: "myVar".into(),
            defined_at: Span::default(),
        };

        assert_eq!(captured.name, "myVar");
    }

    #[test]
    fn test_mutation_kinds() {
        let local = Mutation {
            kind: MutationKind::LocalVariable { name: "x".into() },
            span: Span::default(),
        };

        let captured = Mutation {
            kind: MutationKind::CapturedVariable {
                name: "count".into(),
            },
            span: Span::default(),
        };

        let field = Mutation {
            kind: MutationKind::Field { name: "sum".into() },
            span: Span::default(),
        };

        assert!(matches!(local.kind, MutationKind::LocalVariable { .. }));
        assert!(matches!(
            captured.kind,
            MutationKind::CapturedVariable { .. }
        ));
        assert!(matches!(field.kind, MutationKind::Field { .. }));
    }

    #[test]
    fn test_analyse_simple_block() {
        // Create a simple block: [:x | x + 1]
        let block = Block::new(
            vec![BlockParameter::new("x", test_span())],
            vec![bare(Expression::Identifier(Identifier::new(
                "x",
                test_span(),
            )))],
            test_span(),
        );
        let expr = Expression::Block(block);
        let module = Module::new(vec![bare(expr)], test_span());

        let result = analyse(&module);

        // Block should be recorded
        assert_eq!(result.block_info.len(), 1);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn test_analyse_block_with_capture() {
        // Create: count := 0. [:x | count + x]
        let count_def = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            ))),
            value: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(0),
                test_span(),
            )),
            span: test_span(),
        };

        let block = Block::new(
            vec![BlockParameter::new("x", test_span())],
            vec![bare(Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            )))],
            Span::new(10, 20),
        );

        let module = Module::new(
            vec![bare(count_def), bare(Expression::Block(block))],
            test_span(),
        );

        let result = analyse(&module);

        // Block should capture 'count'
        let block_span = Span::new(10, 20);
        let block_info = result.block_info.get(&block_span).unwrap();
        assert_eq!(block_info.captures.len(), 1);
        assert_eq!(block_info.captures[0].name, "count");
    }

    #[test]
    fn test_analyse_block_with_local_mutation() {
        // Create: [:x | temp := x. temp]
        let block = Block::new(
            vec![BlockParameter::new("x", test_span())],
            vec![
                bare(Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new("temp", test_span()))),
                    value: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                    span: test_span(),
                }),
                bare(Expression::Identifier(Identifier::new("temp", test_span()))),
            ],
            Span::new(10, 20),
        );

        let module = Module::new(vec![bare(Expression::Block(block))], test_span());

        let result = analyse(&module);

        // Block should have local mutation
        let block_span = Span::new(10, 20);
        let block_info = result.block_info.get(&block_span).unwrap();
        assert_eq!(block_info.mutations.len(), 1);
        assert!(matches!(
            block_info.mutations[0].kind,
            MutationKind::LocalVariable { .. }
        ));
    }

    #[test]
    fn test_analyse_block_with_captured_mutation() {
        // Create: count := 0. [:x | count := count + x]
        let count_def = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            ))),
            value: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(0),
                test_span(),
            )),
            span: test_span(),
        };

        let block = Block::new(
            vec![BlockParameter::new("x", test_span())],
            vec![bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    test_span(),
                ))),
                value: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                span: test_span(),
            })],
            Span::new(10, 20),
        );

        let module = Module::new(
            vec![bare(count_def), bare(Expression::Block(block))],
            test_span(),
        );

        let result = analyse(&module);

        // Block should have captured mutation
        let block_span = Span::new(10, 20);
        let block_info = result.block_info.get(&block_span).unwrap();
        assert_eq!(block_info.mutations.len(), 1);
        assert!(matches!(
            block_info.mutations[0].kind,
            MutationKind::CapturedVariable { .. }
        ));
    }

    #[test]
    fn test_analyse_control_flow_block_context() {
        // Create: 5 timesRepeat: [x := 1]
        let block = Block::new(
            vec![],
            vec![bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                value: Box::new(Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    test_span(),
                )),
                span: test_span(),
            })],
            Span::new(20, 30),
        );

        let message_send = Expression::MessageSend {
            receiver: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(5),
                test_span(),
            )),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "timesRepeat:",
                test_span(),
            )]),
            arguments: vec![Expression::Block(block)],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(message_send)], test_span());

        let result = analyse(&module);

        // Block should have ControlFlow context
        let block_span = Span::new(20, 30);
        let block_info = result.block_info.get(&block_span).unwrap();
        assert_eq!(block_info.context, BlockContext::ControlFlow);
    }

    // PR Review Comment Tests

    #[test]
    fn test_block_assigned_to_variable_gets_stored_context() {
        // Comment 7: Block assigned to variable should get Stored context
        // Code: myBlock := [:x | x + 1]
        let block = Block::new(
            vec![BlockParameter::new("x", Span::new(15, 16))],
            vec![bare(Expression::Identifier(Identifier::new(
                "x",
                Span::new(19, 20),
            )))],
            Span::new(12, 25),
        );

        let assignment = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "myBlock",
                Span::new(0, 7),
            ))),
            value: Box::new(Expression::Block(block)),
            span: Span::new(0, 25),
        };

        let module = Module::new(vec![bare(assignment)], Span::new(0, 25));
        let result = analyse(&module);

        // Block should have Stored context
        let block_span = Span::new(12, 25);
        let block_info = result.block_info.get(&block_span).unwrap();
        assert_eq!(block_info.context, BlockContext::Stored);
    }

    #[test]
    fn test_block_passed_as_argument_gets_passed_context() {
        // Comment 8: Block passed as non-control-flow argument should get Passed context
        // Code: array at: 1 put: [:x | x + 1]
        let block = Block::new(
            vec![BlockParameter::new("x", Span::new(23, 24))],
            vec![bare(Expression::Identifier(Identifier::new(
                "x",
                Span::new(27, 28),
            )))],
            Span::new(20, 33),
        );

        let message = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "array",
                Span::new(0, 5),
            ))),
            selector: MessageSelector::Keyword(vec![
                crate::ast::KeywordPart::new("at:", Span::new(6, 9)),
                crate::ast::KeywordPart::new("put:", Span::new(12, 16)),
            ]),
            arguments: vec![
                Expression::Literal(crate::ast::Literal::Integer(1), Span::new(10, 11)),
                Expression::Block(block),
            ],
            is_cast: false,
            span: Span::new(0, 33),
        };

        let module = Module::new(vec![bare(message)], Span::new(0, 33));
        let result = analyse(&module);

        // Block should have Passed context
        let block_span = Span::new(20, 33);
        let block_info = result.block_info.get(&block_span).unwrap();
        assert_eq!(block_info.context, BlockContext::Passed);
    }

    #[test]
    fn test_match_arm_analysis_with_simple_pattern() {
        // Test that pattern variables are accessible in the body
        // value match: [x -> x]
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "value",
                test_span(),
            ))),
            arms: vec![MatchArm::new(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("x", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have diagnostic for 'value' only, not 'x'
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: value")
        );
    }

    #[test]
    fn test_match_arm_analysis_with_guard() {
        // Test that pattern variables are accessible in guards
        // value match: [x when x > 0 -> x]
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "value",
                test_span(),
            ))),
            arms: vec![MatchArm::with_guard(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("x", test_span())), // guard uses x
                Expression::Identifier(Identifier::new("x", test_span())), // body uses x
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have diagnostic for 'value' only, not 'x' (used in guard and body)
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: value")
        );
    }

    #[test]
    fn test_match_arm_analysis_with_tuple_pattern() {
        // Test nested patterns: {#ok, value} -> value
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "result",
                test_span(),
            ))),
            arms: vec![MatchArm::new(
                Pattern::Tuple {
                    elements: vec![
                        Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                        Pattern::Variable(Identifier::new("value", test_span())),
                    ],
                    span: test_span(),
                },
                Expression::Identifier(Identifier::new("value", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have diagnostic for 'result' only, not 'value' (pattern-bound)
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: result")
        );
    }

    #[test]
    fn test_match_arm_analysis_multiple_arms() {
        // Test multiple arms with different patterns
        // result match: [
        //   {#ok, value} -> value;
        //   {#error, msg} -> msg
        // ]
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "result",
                test_span(),
            ))),
            arms: vec![
                MatchArm::new(
                    Pattern::Tuple {
                        elements: vec![
                            Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                            Pattern::Variable(Identifier::new("value", test_span())),
                        ],
                        span: test_span(),
                    },
                    Expression::Identifier(Identifier::new("value", test_span())),
                    test_span(),
                ),
                MatchArm::new(
                    Pattern::Tuple {
                        elements: vec![
                            Pattern::Literal(Literal::Symbol("error".into()), test_span()),
                            Pattern::Variable(Identifier::new("msg", test_span())),
                        ],
                        span: test_span(),
                    },
                    Expression::Identifier(Identifier::new("msg", test_span())),
                    test_span(),
                ),
            ],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have diagnostic for 'result' only, not 'value' or 'msg' (pattern-bound)
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: result")
        );
    }

    #[test]
    fn test_match_arm_analysis_with_list_pattern() {
        // Test list patterns: [head | tail] -> head
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new("list", test_span()))),
            arms: vec![MatchArm::new(
                Pattern::List {
                    elements: vec![Pattern::Variable(Identifier::new("head", test_span()))],
                    tail: Some(Box::new(Pattern::Variable(Identifier::new(
                        "tail",
                        test_span(),
                    )))),
                    span: test_span(),
                },
                Expression::Identifier(Identifier::new("head", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have diagnostic for 'list' only, not 'head' or 'tail' (pattern-bound)
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: list")
        );
    }

    #[test]
    fn test_match_arm_scope_isolation() {
        // Test that variables from one arm don't leak to another
        // This test verifies that each arm gets its own scope
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "value",
                test_span(),
            ))),
            arms: vec![
                MatchArm::new(
                    Pattern::Variable(Identifier::new("x", test_span())),
                    Expression::Identifier(Identifier::new("x", test_span())),
                    test_span(),
                ),
                MatchArm::new(
                    Pattern::Variable(Identifier::new("y", test_span())),
                    Expression::Identifier(Identifier::new("y", test_span())),
                    test_span(),
                ),
            ],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have diagnostic for 'value' only, not 'x' or 'y' (pattern-bound in separate arms)
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: value")
        );
    }

    #[test]
    fn test_undefined_variable_in_match_arm_body() {
        // Test that undefined variables produce diagnostics
        // value match: [x -> undefined_var]
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "value",
                test_span(),
            ))),
            arms: vec![MatchArm::new(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("undefined_var", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have 2 diagnostics: value and undefined_var
        assert_eq!(result.diagnostics.len(), 2);
        assert!(
            result.diagnostics[1]
                .message
                .contains("Undefined variable: undefined_var")
        );
    }

    #[test]
    fn test_undefined_variable_in_guard() {
        // Test that undefined variables in guards produce diagnostics
        // value match: [x when undefined_var -> x]
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "value",
                test_span(),
            ))),
            arms: vec![MatchArm::with_guard(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("undefined_var", test_span())),
                Expression::Identifier(Identifier::new("x", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should have diagnostic for undefined_var in guard
        assert_eq!(result.diagnostics.len(), 2); // value and undefined_var
        assert!(
            result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Undefined variable: undefined_var"))
        );
    }

    #[test]
    fn test_pattern_bound_variable_no_error() {
        // Test that pattern-bound variables do NOT produce diagnostics
        // value match: [x -> x]
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "value",
                test_span(),
            ))),
            arms: vec![MatchArm::new(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("x", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should only have diagnostic for 'value', not 'x'
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: value")
        );
    }

    #[test]
    fn test_nested_pattern_variables_accessible() {
        // Test nested tuple pattern variables are accessible
        // result match: [{#ok, {x, y}} -> x]
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new(
                "result",
                test_span(),
            ))),
            arms: vec![MatchArm::new(
                Pattern::Tuple {
                    elements: vec![
                        Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                        Pattern::Tuple {
                            elements: vec![
                                Pattern::Variable(Identifier::new("x", test_span())),
                                Pattern::Variable(Identifier::new("y", test_span())),
                            ],
                            span: test_span(),
                        },
                    ],
                    span: test_span(),
                },
                Expression::Identifier(Identifier::new("x", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let module = Module::new(vec![bare(match_expr)], test_span());
        let result = analyse(&module);

        // Should only error on 'result', not 'x' or 'y'
        assert_eq!(result.diagnostics.len(), 1);
        assert!(
            result.diagnostics[0]
                .message
                .contains("Undefined variable: result")
        );
    }

    #[test]
    fn test_self_available_in_method_bodies() {
        // Test that 'self' is implicitly available in method bodies
        // Simple method that uses self:
        //   getValue => self.value

        use crate::ast::{
            ClassDefinition, ClassKind, CommentAttachment, MessageSelector, MethodDefinition,
            MethodKind, StateDeclaration,
        };

        let get_value_method = MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![bare(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
                field: Identifier::new("value", test_span()),
                span: test_span(),
            })],
            return_type: None,
            is_sealed: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let state_decl = StateDeclaration {
            name: Identifier::new("value", test_span()),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), test_span())),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let class_def = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![state_decl],
            methods: vec![get_value_method],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class_def],
            method_definitions: Vec::new(),
            expressions: vec![],
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
            span: test_span(),
        };
        let result = analyse(&module);

        // Should have NO diagnostics - 'self' should be recognized
        assert_eq!(
            result.diagnostics.len(),
            0,
            "Expected no diagnostics, but got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_field_assignment_in_stored_block_emits_error() {
        // Test: myBlock := [self.sum := 0] should emit error

        let field_assignment = Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
                field: Identifier::new("sum", test_span()),
                span: test_span(),
            }),
            value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
            span: test_span(),
        };

        let block = Expression::Block(crate::ast::Block {
            parameters: vec![],
            body: vec![bare(field_assignment)],
            span: test_span(),
        });

        // Assign block to variable (Stored context)
        let assignment = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "myBlock",
                test_span(),
            ))),
            value: Box::new(block),
            span: test_span(),
        };

        let module = Module::new(vec![bare(assignment)], test_span());
        let result = analyse(&module);

        // Should have at least 1 error diagnostic for field assignment in stored block
        // (may have additional errors for undefined 'self', which is expected)
        assert!(!result.diagnostics.is_empty());
        let has_field_error = result.diagnostics.iter().any(|d| {
            d.message.contains("cannot assign to field 'sum'")
                && d.message.contains("stored closure")
        });
        assert!(
            has_field_error,
            "Expected field assignment error, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_field_assignment_in_passed_block_emits_error() {
        // Test: obj callWith: [:x | self.sum := 0] should emit error
        // "callWith:" is not a control flow selector, so block is Passed
        let field_assignment = Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
                field: Identifier::new("sum", test_span()),
                span: test_span(),
            }),
            value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
            span: test_span(),
        };

        let block = Expression::Block(crate::ast::Block {
            parameters: vec![crate::ast::BlockParameter::new("x", test_span())],
            body: vec![bare(field_assignment)],
            span: test_span(),
        });

        // Pass block to a message send with a non-control-flow selector
        let message_send = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("obj", test_span()))),
            selector: crate::ast::MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "callWith:",
                test_span(),
            )]),
            arguments: vec![block],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(message_send)], test_span());
        let result = analyse(&module);

        // Should have at least 1 error diagnostic for field assignment in passed block
        // (may have additional errors for undefined variables, which is expected)
        assert!(!result.diagnostics.is_empty());
        let has_field_error = result.diagnostics.iter().any(|d| {
            d.message.contains("cannot assign to field 'sum'")
                && d.message.contains("passed closure")
        });
        assert!(
            has_field_error,
            "Expected field assignment error, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_captured_variable_mutation_in_stored_block_no_warning() {
        // BT-856 (ADR 0041 Phase 3): Captured variable mutations in stored blocks are
        // now valid and supported via the Tier 2 stateful block protocol (BT-852).
        // The old warning ("has no effect on outer scope") was incorrect — Tier 2
        // threads state through StateAcc maps so mutations propagate correctly.
        let count_def = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            ))),
            value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
            span: test_span(),
        };

        let count_mutation = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            ))),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    test_span(),
                ))),
                selector: crate::ast::MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
                is_cast: false,
                span: test_span(),
            }),
            span: test_span(),
        };

        let block = Expression::Block(crate::ast::Block {
            parameters: vec![],
            body: vec![bare(count_mutation)],
            span: test_span(),
        });

        // Assign block to variable (Stored context)
        let block_assignment = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "myBlock",
                test_span(),
            ))),
            value: Box::new(block),
            span: test_span(),
        };

        let module = Module::new(vec![bare(count_def), bare(block_assignment)], test_span());
        let result = analyse(&module);

        // Should have NO diagnostic — captured variable mutations in stored blocks are valid
        assert_eq!(
            result.diagnostics.len(),
            0,
            "Unexpected diagnostics: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_field_assignment_in_control_flow_block_no_diagnostic() {
        // Test: 10 timesRepeat: [self.sum := 0] should NOT emit error
        let field_assignment = Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
                field: Identifier::new("sum", test_span()),
                span: test_span(),
            }),
            value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
            span: test_span(),
        };

        let block = Expression::Block(crate::ast::Block {
            parameters: vec![],
            body: vec![bare(field_assignment)],
            span: test_span(),
        });

        // Use in control flow position
        let message_send = Expression::MessageSend {
            receiver: Box::new(Expression::Literal(Literal::Integer(10), test_span())),
            selector: crate::ast::MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "timesRepeat:",
                test_span(),
            )]),
            arguments: vec![block],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(message_send)], test_span());
        let result = analyse(&module);

        // Should have NO diagnostics for field assignment in control flow blocks
        // (may have errors for undefined 'self', but no mutation warnings)
        let has_field_error = result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("cannot assign to field"));
        assert!(
            !has_field_error,
            "Should not have field assignment error for control flow, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_local_variable_mutation_in_stored_block_no_diagnostic() {
        // Test: myBlock := [x := 0. x := x + 1] should NOT emit warning
        // (only captured variable mutations get warnings)
        let x_def = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
            span: test_span(),
        };

        let x_mutation = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                selector: crate::ast::MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
                is_cast: false,
                span: test_span(),
            }),
            span: test_span(),
        };

        let block = Expression::Block(crate::ast::Block {
            parameters: vec![],
            body: vec![bare(x_def), bare(x_mutation)],
            span: test_span(),
        });

        // Assign block to variable (Stored context)
        let block_assignment = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "myBlock",
                test_span(),
            ))),
            value: Box::new(block),
            span: test_span(),
        };

        let module = Module::new(vec![bare(block_assignment)], test_span());
        let result = analyse(&module);

        // Should have NO diagnostics - local variables can be mutated
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn test_analyse_with_known_vars_suppresses_undefined() {
        // Test: x + 1 where 'x' is a known REPL variable
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            selector: crate::ast::MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(expr)], test_span());

        // Without known vars - should report undefined
        let result_without = analyse(&module);
        assert!(
            result_without
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Undefined variable: x")),
            "Should report undefined variable without known vars"
        );

        // With 'x' in known vars - should NOT report undefined
        let result_with = analyse_with_known_vars(&module, &["x"]);
        assert!(
            !result_with
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Undefined variable: x")),
            "Should not report undefined variable when in known_vars, got: {:?}",
            result_with.diagnostics
        );
    }

    #[test]
    fn test_analyse_with_known_vars_handles_reassignment() {
        // Test: x := x + 1 where 'x' is a known REPL variable (reassignment)
        let expr = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                selector: crate::ast::MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
                is_cast: false,
                span: test_span(),
            }),
            span: test_span(),
        };

        let module = Module::new(vec![bare(expr)], test_span());

        // With 'x' known, the RHS reference should not report undefined
        let result = analyse_with_known_vars(&module, &["x"]);
        assert!(
            !result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Undefined variable")),
            "Should not report undefined for known REPL variable reassignment, got: {:?}",
            result.diagnostics
        );
    }

    // --- ClassHierarchy integration tests (BT-279) ---

    #[test]
    fn test_analyse_populates_class_hierarchy() {
        let module = Module::new(vec![], Span::default());
        let result = analyse(&module);

        // Hierarchy should be populated with built-in classes
        assert!(result.class_hierarchy.has_class("ProtoObject"));
        assert!(result.class_hierarchy.has_class("Object"));
        assert!(result.class_hierarchy.has_class("Actor"));
        assert!(result.class_hierarchy.has_class("Integer"));
    }

    #[test]
    fn test_analyse_hierarchy_includes_user_classes() {
        use crate::ast::{
            ClassDefinition, ClassKind, CommentAttachment, MethodDefinition, MethodKind,
            StateDeclaration,
        };

        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("count", test_span()),
                type_annotation: None,
                default_value: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        assert!(result.class_hierarchy.has_class("Counter"));
        assert!(
            result
                .class_hierarchy
                .resolves_selector("Counter", "increment")
        );
        // Inherited from Actor
        assert!(result.class_hierarchy.resolves_selector("Counter", "spawn"));
    }

    #[test]
    fn test_analyse_reports_sealed_class_diagnostic() {
        use crate::ast::{ClassDefinition, ClassKind};

        let class = ClassDefinition {
            name: Identifier::new("MyInt", test_span()),
            superclass: Some(Identifier::new("Integer", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        // Should have sealed class diagnostic
        assert!(
            result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("sealed") && d.message.contains("Integer")),
            "Expected sealed class diagnostic, got: {:?}",
            result.diagnostics
        );
    }

    // --- Method Validator Integration Tests (BT-244) ---

    #[test]
    fn test_responds_to_with_symbol_no_diagnostic() {
        // counter respondsTo: #increment — should be fine
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "counter",
                test_span(),
            ))),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "respondsTo:",
                test_span(),
            )]),
            arguments: vec![Expression::Literal(
                Literal::Symbol("increment".into()),
                test_span(),
            )],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(expr)], test_span());
        let result = analyse_with_known_vars(&module, &["counter"]);

        // No symbol-related diagnostics
        assert!(
            !result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("expects a symbol literal")),
            "Should not report error for symbol literal, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_responds_to_with_identifier_emits_error() {
        // counter respondsTo: increment — should error
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "counter",
                test_span(),
            ))),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "respondsTo:",
                test_span(),
            )]),
            arguments: vec![Expression::Identifier(Identifier::new(
                "increment",
                Span::new(20, 29),
            ))],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(expr)], test_span());
        let result = analyse_with_known_vars(&module, &["counter"]);

        let symbol_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("expects a symbol literal"))
            .collect();

        assert_eq!(
            symbol_errors.len(),
            1,
            "Expected 1 symbol literal error, got: {:?}",
            result.diagnostics
        );
        assert!(
            symbol_errors[0]
                .message
                .contains("expects a symbol literal")
        );
        assert!(
            symbol_errors[0]
                .message
                .contains("checks if an object understands a message")
        );
        assert_eq!(symbol_errors[0].span, Span::new(20, 29));
        // Fix suggestion is in the hint field
        let hint = symbol_errors[0].hint.as_ref().unwrap();
        assert!(hint.contains("#increment"));
    }

    #[test]
    fn test_class_named_with_class_reference_emits_error() {
        // Beamtalk classNamed: Counter — should error
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Beamtalk", test_span()),
                span: test_span(),
            }),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "classNamed:",
                test_span(),
            )]),
            arguments: vec![Expression::ClassReference {
                name: Identifier::new("Counter", Span::new(22, 29)),
                span: Span::new(22, 29),
            }],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(expr)], test_span());
        let result = analyse(&module);

        let symbol_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("expects a symbol literal"))
            .collect();

        assert_eq!(
            symbol_errors.len(),
            1,
            "Expected 1 symbol literal error, got: {:?}",
            result.diagnostics
        );
        let hint = symbol_errors[0].hint.as_ref().unwrap();
        assert!(hint.contains("#Counter"));
        assert!(
            symbol_errors[0]
                .message
                .contains("looks up a class by name")
        );
    }

    #[test]
    fn test_inst_var_at_with_integer_emits_error() {
        // obj fieldAt: 42 — should error
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("obj", test_span()))),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "fieldAt:",
                test_span(),
            )]),
            arguments: vec![Expression::Literal(Literal::Integer(42), Span::new(15, 17))],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(expr)], test_span());
        let result = analyse_with_known_vars(&module, &["obj"]);

        let symbol_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("expects a symbol literal"))
            .collect();

        assert_eq!(
            symbol_errors.len(),
            1,
            "Expected 1 symbol literal error, got: {:?}",
            result.diagnostics
        );
        assert!(
            symbol_errors[0]
                .message
                .contains("accesses a field by name")
        );
    }

    #[test]
    fn test_non_reflection_method_no_validation() {
        // obj someMethod: increment — should NOT trigger validator
        let expr = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("obj", test_span()))),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "someMethod:",
                test_span(),
            )]),
            arguments: vec![Expression::Identifier(Identifier::new(
                "increment",
                test_span(),
            ))],
            is_cast: false,
            span: test_span(),
        };

        let module = Module::new(vec![bare(expr)], test_span());
        let result = analyse_with_known_vars(&module, &["obj"]);

        assert!(
            !result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("expects a symbol literal")),
            "Non-reflection methods should not trigger symbol validation, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_cascade_responds_to_with_identifier_emits_error() {
        // counter respondsTo: increment; size — cascade should also validate
        let cascade = Expression::Cascade {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "counter",
                test_span(),
            ))),
            messages: vec![
                crate::ast::CascadeMessage::new(
                    MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                        "respondsTo:",
                        test_span(),
                    )]),
                    vec![Expression::Identifier(Identifier::new(
                        "increment",
                        Span::new(20, 29),
                    ))],
                    test_span(),
                ),
                crate::ast::CascadeMessage::new(
                    MessageSelector::Unary("size".into()),
                    vec![],
                    test_span(),
                ),
            ],
            span: test_span(),
        };

        let module = Module::new(vec![bare(cascade)], test_span());
        let result = analyse_with_known_vars(&module, &["counter"]);

        let symbol_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("expects a symbol literal"))
            .collect();

        assert_eq!(
            symbol_errors.len(),
            1,
            "Cascade messages should trigger symbol validation, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn test_abstract_class_instantiation_error() {
        use crate::ast::{
            ClassDefinition, ClassKind, CommentAttachment, MethodDefinition, MethodKind,
        };

        // BT-105: abstract class cannot be instantiated
        let class = ClassDefinition {
            name: Identifier::new("Shape", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: true,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("area".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(Literal::Integer(42), test_span()))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        // Top-level expression: Shape spawn
        let spawn_expr = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Shape", test_span()),
                span: test_span(),
            }),
            selector: MessageSelector::Unary("spawn".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![bare(spawn_expr)],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };

        let result = analyse(&module);

        let abstract_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Cannot instantiate abstract class"))
            .collect();

        assert_eq!(
            abstract_errors.len(),
            1,
            "Should detect abstract class instantiation, got: {:?}",
            result.diagnostics
        );
        assert!(abstract_errors[0].message.contains("Shape"));
    }

    // --- Self misuse diagnostic tests (BT-595) ---

    #[test]
    fn test_self_outside_method_gives_specialized_error() {
        // Using self at module top level should give a specialized message
        let self_expr = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
        let module = Module::new(vec![bare(self_expr)], test_span());
        let result = analyse(&module);

        let self_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("self"))
            .collect();
        assert_eq!(self_errors.len(), 1);
        assert!(
            self_errors[0]
                .message
                .contains("self can only be used inside a method body")
        );
        // Should NOT say "Undefined variable: self"
        assert!(!self_errors[0].message.contains("Undefined variable"));
    }

    #[test]
    fn test_self_inside_method_no_error() {
        // self inside a method body should work fine
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("value", test_span()),
                type_annotation: None,
                default_value: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::FieldAccess {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        test_span(),
                    ))),
                    field: Identifier::new("value", test_span()),
                    span: test_span(),
                })],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let self_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("self"))
            .collect();
        assert!(self_errors.is_empty());
    }

    // --- Unused variable warning tests (BT-595) ---

    #[test]
    fn test_unused_variable_in_method_warns() {
        // Method with unused local: getValue => x := 42. self.value
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("value", test_span()),
                type_annotation: None,
                default_value: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "x",
                            Span::new(10, 11),
                        ))),
                        value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                        span: test_span(),
                    }),
                    bare(Expression::FieldAccess {
                        receiver: Box::new(Expression::Identifier(Identifier::new(
                            "self",
                            test_span(),
                        ))),
                        field: Identifier::new("value", test_span()),
                        span: test_span(),
                    }),
                ],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("`x`"));
        assert_eq!(warnings[0].severity, Severity::Warning);
        assert!(warnings[0].hint.is_some());
        assert!(warnings[0].hint.as_ref().unwrap().contains("_x"));
    }

    #[test]
    fn test_used_variable_no_warning() {
        // Method where variable is used: getValue => x := 42. x
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                        value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                        span: test_span(),
                    }),
                    bare(Expression::Identifier(Identifier::new("x", test_span()))),
                ],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_underscore_prefixed_variable_no_warning() {
        // Method with _x := 42 should not warn
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("doSomething".into()),
                parameters: vec![],
                body: vec![bare(Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new("_x", test_span()))),
                    value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                    span: test_span(),
                })],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_unused_parameter_emits_warning() {
        // BT-954: Unused method parameter should warn
        // process: newValue => 0  // Warning: parameter newValue unused
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                    keyword: "setValue:".into(),
                    span: test_span(),
                }]),
                parameters: vec![crate::ast::ParameterDefinition {
                    name: Identifier::new("newValue", test_span()),
                    type_annotation: None,
                }],
                body: vec![bare(Expression::Literal(Literal::Integer(0), test_span()))],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused parameter"))
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("newValue"));
        assert_eq!(warnings[0].severity, Severity::Warning);
    }

    #[test]
    fn test_unused_parameter_underscore_suppresses_warning() {
        // BT-954: Parameter prefixed with _ should not warn
        // process: _newValue => 0  // No warning
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: crate::ast::ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                    keyword: "setValue:".into(),
                    span: test_span(),
                }]),
                parameters: vec![crate::ast::ParameterDefinition {
                    name: Identifier::new("_newValue", test_span()),
                    type_annotation: None,
                }],
                body: vec![bare(Expression::Literal(Literal::Integer(0), test_span()))],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused parameter"))
            .collect();
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_used_parameter_no_warning() {
        // BT-954: Used method parameter should not warn
        // process: x => x  // No warning
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            is_abstract: false,
            class_kind: crate::ast::ClassKind::Actor,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                    keyword: "process:".into(),
                    span: test_span(),
                }]),
                parameters: vec![crate::ast::ParameterDefinition {
                    name: Identifier::new("x", test_span()),
                    type_annotation: None,
                }],
                body: vec![bare(Expression::Identifier(Identifier::new(
                    "x",
                    test_span(),
                )))],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused parameter"))
            .collect();
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_unused_parameter_primitive_body_no_warning() {
        // @primitive body implicitly passes all params to the Erlang primitive —
        // no unused-parameter warning should fire.
        // at: index => @primitive "at:"
        let class = ClassDefinition {
            name: Identifier::new("Array", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                    keyword: "at:".into(),
                    span: test_span(),
                }]),
                parameters: vec![crate::ast::ParameterDefinition {
                    name: Identifier::new("index", test_span()),
                    type_annotation: None,
                }],
                body: vec![bare(Expression::Primitive {
                    name: "at:".into(),
                    is_quoted: true,
                    is_intrinsic: false,
                    span: test_span(),
                })],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);
        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused parameter"))
            .collect();
        assert!(
            warnings.is_empty(),
            "@primitive body: all params are passed to the primitive, expected no warning, got: {warnings:?}"
        );
    }

    #[test]
    fn test_unused_parameter_intrinsic_body_no_warning() {
        // @intrinsic body (unquoted primitive) also passes all params implicitly —
        // no unused-parameter warning should fire.
        // spawnWith: initArgs => @intrinsic actorSpawnWith
        let class = ClassDefinition {
            name: Identifier::new("Actor", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                    keyword: "spawnWith:".into(),
                    span: test_span(),
                }]),
                parameters: vec![crate::ast::ParameterDefinition {
                    name: Identifier::new("initArgs", test_span()),
                    type_annotation: None,
                }],
                body: vec![bare(Expression::Primitive {
                    name: "actorSpawnWith".into(),
                    is_quoted: false,
                    is_intrinsic: false,
                    span: test_span(),
                })],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);
        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused parameter"))
            .collect();
        assert!(
            warnings.is_empty(),
            "@intrinsic body: all params are passed to the primitive, expected no warning, got: {warnings:?}"
        );
    }

    #[test]
    fn test_unused_variable_in_class_method_warns() {
        // Class method with unused local: class create => x := 42. nil
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("create".into()),
                parameters: vec![],
                body: vec![
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "temp",
                            Span::new(10, 14),
                        ))),
                        value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                        span: test_span(),
                    }),
                    bare(Expression::Identifier(Identifier::new("nil", test_span()))),
                ],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("`temp`"));
    }

    #[test]
    fn test_block_parameter_no_unused_warning() {
        // Block parameters should not warn: getValue => [:x | x]
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Block(Block::new(
                    vec![BlockParameter::new("x", test_span())],
                    vec![bare(Expression::Identifier(Identifier::new(
                        "x",
                        test_span(),
                    )))],
                    test_span(),
                )))],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_pattern_variable_no_unused_warning() {
        // Pattern variables in match arms should not warn:
        // getValue => x match: { 1 -> #one }
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Match {
                    value: Box::new(Expression::Literal(Literal::Integer(1), test_span())),
                    arms: vec![MatchArm::new(
                        Pattern::Variable(Identifier::new("result", test_span())),
                        Expression::Identifier(Identifier::new("result", test_span())),
                        test_span(),
                    )],
                    span: test_span(),
                })],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_unused_variable_in_nested_block_warns() {
        // Unused variable declared inside a nested block should warn:
        // getValue => [unused := 42]
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Block(Block::new(
                    vec![],
                    vec![bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "unused",
                            Span::new(10, 16),
                        ))),
                        value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                        span: test_span(),
                    })],
                    test_span(),
                )))],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("`unused`"));
    }

    #[test]
    fn test_variable_used_via_closure_no_warning() {
        // Variable defined at method scope, used inside a block:
        // getValue => x := 1. [x]
        // The block reads x, so no unused warning.
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "x",
                            Span::new(10, 11),
                        ))),
                        value: Box::new(Expression::Literal(Literal::Integer(1), test_span())),
                        span: test_span(),
                    }),
                    bare(Expression::Block(Block::new(
                        vec![],
                        vec![bare(Expression::Identifier(Identifier::new(
                            "x",
                            test_span(),
                        )))],
                        test_span(),
                    ))),
                ],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        // x is used via closure in the block
        assert!(warnings.is_empty());
    }

    // --- Dead code after early return tests (BT-596) ---

    #[test]
    fn test_dead_code_after_return_in_method() {
        // Method: getValue => ^ 42. self doSomething
        let class = ClassDefinition {
            name: Identifier::new("Foo", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![
                    bare(Expression::Return {
                        value: Box::new(Expression::Literal(Literal::Integer(42), Span::new(0, 4))),
                        span: Span::new(0, 4),
                    }),
                    bare(Expression::MessageSend {
                        receiver: Box::new(Expression::Identifier(Identifier::new(
                            "self",
                            Span::new(5, 9),
                        ))),
                        selector: MessageSelector::Unary("doSomething".into()),
                        arguments: vec![],
                        is_cast: false,
                        span: Span::new(5, 20),
                    }),
                ],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let dead_code: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unreachable code"))
            .collect();
        assert_eq!(dead_code.len(), 1);
        assert!(
            dead_code[0]
                .message
                .contains("Unreachable code after early return")
        );
    }

    #[test]
    fn test_no_dead_code_without_return() {
        // Method with no return: getValue => 42
        let class = ClassDefinition {
            name: Identifier::new("Foo", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(Literal::Integer(42), test_span()))],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let dead_code: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unreachable code"))
            .collect();
        assert!(dead_code.is_empty());
    }

    #[test]
    fn test_dead_code_in_block() {
        // Block with return followed by expression: [^ 42. 99]
        let block = Expression::Block(Block {
            parameters: vec![],
            body: vec![
                bare(Expression::Return {
                    value: Box::new(Expression::Literal(Literal::Integer(42), Span::new(0, 4))),
                    span: Span::new(0, 4),
                }),
                bare(Expression::Literal(Literal::Integer(99), Span::new(5, 7))),
            ],
            span: test_span(),
        });

        // Put block inside a method body so it's in scope
        let class = ClassDefinition {
            name: Identifier::new("Foo", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("test".into()),
                parameters: vec![],
                body: vec![bare(block)],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let dead_code: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unreachable code"))
            .collect();
        assert_eq!(dead_code.len(), 1);
    }

    #[test]
    fn test_return_at_end_no_warning() {
        // Return as the last expression — no dead code
        let class = ClassDefinition {
            name: Identifier::new("Foo", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Return {
                    value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                    span: test_span(),
                })],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let dead_code: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unreachable code"))
            .collect();
        assert!(dead_code.is_empty());
    }

    // --- Super outside method tests (BT-596) ---

    #[test]
    fn test_super_outside_method_gives_error() {
        // Using super at module top level should error
        let super_expr = Expression::Super(Span::new(0, 5));
        let module = Module::new(vec![bare(super_expr)], test_span());
        let result = analyse(&module);

        let super_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("super"))
            .collect();
        assert_eq!(super_errors.len(), 1);
        assert!(
            super_errors[0]
                .message
                .contains("super can only be used inside a method body")
        );
        assert_eq!(super_errors[0].severity, Severity::Error);
    }

    #[test]
    fn test_super_inside_method_no_error() {
        // super inside a method body should not emit an error from name resolution
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("reset".into()),
                parameters: vec![],
                body: vec![bare(Expression::MessageSend {
                    receiver: Box::new(Expression::Super(test_span())),
                    selector: MessageSelector::Unary("reset".into()),
                    arguments: vec![],
                    is_cast: false,
                    span: test_span(),
                })],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let super_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("super can only"))
            .collect();
        assert!(super_errors.is_empty());
    }

    #[test]
    fn test_super_in_class_scope_gives_error() {
        // super at class level (not inside method) should error
        // This tests depth 1 (class scope, not method scope)
        let super_expr = Expression::Super(Span::new(0, 5));
        let module = Module::new(vec![bare(super_expr)], test_span());
        let result = analyse(&module);

        let super_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("super can only"))
            .collect();
        assert_eq!(super_errors.len(), 1);
    }

    // --- Variable shadowing tests (BT-596) ---

    #[test]
    fn test_block_param_shadows_outer_variable() {
        // [:x | [:x | x + 1]] — inner x shadows outer x
        let inner_block = Expression::Block(Block {
            parameters: vec![BlockParameter {
                name: "x".into(),
                span: Span::new(10, 11),
            }],
            body: vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "x",
                    Span::new(14, 15),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(18, 19))],
                is_cast: false,
                span: Span::new(14, 19),
            })],
            span: Span::new(8, 20),
        });

        let outer_block = Expression::Block(Block {
            parameters: vec![BlockParameter {
                name: "x".into(),
                span: Span::new(1, 2),
            }],
            body: vec![bare(inner_block)],
            span: Span::new(0, 21),
        });

        // Put in method body
        let class = ClassDefinition {
            name: Identifier::new("Foo", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("test".into()),
                parameters: vec![],
                body: vec![bare(outer_block)],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let shadow_warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("shadows"))
            .collect();
        assert_eq!(shadow_warnings.len(), 1);
        assert!(shadow_warnings[0].message.contains("Variable `x` shadows"));
        assert_eq!(shadow_warnings[0].severity, Severity::Warning);
    }

    #[test]
    fn test_underscore_prefixed_no_shadow_warning() {
        // [:_x | [:_x | _x + 1]] — underscore-prefixed, no warning
        let inner_block = Expression::Block(Block {
            parameters: vec![BlockParameter {
                name: "_x".into(),
                span: Span::new(10, 12),
            }],
            body: vec![bare(Expression::Identifier(Identifier::new(
                "_x",
                Span::new(15, 17),
            )))],
            span: Span::new(8, 18),
        });

        let outer_block = Expression::Block(Block {
            parameters: vec![BlockParameter {
                name: "_x".into(),
                span: Span::new(1, 3),
            }],
            body: vec![bare(inner_block)],
            span: Span::new(0, 19),
        });

        let class = ClassDefinition {
            name: Identifier::new("Foo", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("test".into()),
                parameters: vec![],
                body: vec![bare(outer_block)],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let shadow_warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("shadows"))
            .collect();
        assert!(shadow_warnings.is_empty());
    }

    #[test]
    fn test_no_shadow_warning_different_names() {
        // [:x | [:y | y + 1]] — different names, no shadowing
        let inner_block = Expression::Block(Block {
            parameters: vec![BlockParameter {
                name: "y".into(),
                span: Span::new(10, 11),
            }],
            body: vec![bare(Expression::Identifier(Identifier::new(
                "y",
                Span::new(14, 15),
            )))],
            span: Span::new(8, 16),
        });

        let outer_block = Expression::Block(Block {
            parameters: vec![BlockParameter {
                name: "x".into(),
                span: Span::new(1, 2),
            }],
            body: vec![bare(inner_block)],
            span: Span::new(0, 17),
        });

        let class = ClassDefinition {
            name: Identifier::new("Foo", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("test".into()),
                parameters: vec![],
                body: vec![bare(outer_block)],
                return_type: None,
                is_sealed: false,
                kind: crate::ast::MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let result = analyse(&module);

        let shadow_warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("shadows"))
            .collect();
        assert!(shadow_warnings.is_empty());
    }

    #[test]
    fn test_block_match_pattern_var_not_treated_as_capture() {
        // BT-655: A block containing a match expression where the pattern variable
        // has the same name as an outer variable should NOT treat the pattern
        // variable as a captured variable.
        //
        // Code equivalent:
        //   x := 0
        //   [:val | val match: [x -> x]]
        //
        // The `x` in the match arm pattern and body refers to the pattern-bound
        // variable, not the outer `x`. It should NOT appear in block captures.
        let outer_x = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
            span: test_span(),
        };

        let block_span = Span::new(100, 200);
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new("val", test_span()))),
            arms: vec![MatchArm::new(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("x", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let block = Block::new(
            vec![BlockParameter::new("val", test_span())],
            vec![bare(match_expr)],
            block_span,
        );

        let module = Module::new(
            vec![bare(outer_x), bare(Expression::Block(block))],
            test_span(),
        );

        let result = analyse(&module);

        let block_info = result.block_info.get(&block_span).unwrap();
        // The pattern variable `x` should NOT be in captures,
        // even though an outer variable `x` exists.
        assert!(
            block_info.captures.is_empty(),
            "Pattern variable 'x' should not be treated as a capture, but found: {:?}",
            block_info
                .captures
                .iter()
                .map(|c| &c.name)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_block_match_captures_real_outer_variable() {
        // Counterpart to the above test: when a match arm body references a
        // variable that is NOT a pattern variable, it SHOULD be captured.
        //
        // Code equivalent:
        //   y := 42
        //   [:val | val match: [x -> y]]
        //
        // Here `y` in the match body is genuinely captured from outer scope.
        let outer_y = Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("y", test_span()))),
            value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
            span: test_span(),
        };

        let block_span = Span::new(100, 200);
        let match_expr = Expression::Match {
            value: Box::new(Expression::Identifier(Identifier::new("val", test_span()))),
            arms: vec![MatchArm::new(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("y", test_span())),
                test_span(),
            )],
            span: test_span(),
        };

        let block = Block::new(
            vec![BlockParameter::new("val", test_span())],
            vec![bare(match_expr)],
            block_span,
        );

        let module = Module::new(
            vec![bare(outer_y), bare(Expression::Block(block))],
            test_span(),
        );

        let result = analyse(&module);

        let block_info = result.block_info.get(&block_span).unwrap();
        assert_eq!(
            block_info.captures.len(),
            1,
            "Should capture outer variable 'y'"
        );
        assert_eq!(block_info.captures[0].name, "y");
    }

    // --- BT-656: Validator coverage tests ────────────────────────────────────

    /// Helper: create an abstract class definition for validator tests.
    fn make_abstract_class(name: &str) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: true,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }
    }

    /// Helper: create a `ClassName spawn` message send.
    fn make_spawn_expr(class_name: &str) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new(class_name, test_span()),
                span: test_span(),
            }),
            selector: MessageSelector::Unary("spawn".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        }
    }

    #[test]
    fn test_abstract_instantiation_in_class_method() {
        use crate::ast::MethodKind;

        // BT-656: abstract instantiation inside a class-side method should be detected
        let mut shape = make_abstract_class("Shape");
        shape.class_methods.push(MethodDefinition {
            selector: MessageSelector::Unary("create".into()),
            parameters: vec![],
            body: vec![bare(make_spawn_expr("Shape"))],
            return_type: None,
            is_sealed: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        });

        let module = Module {
            classes: vec![shape],
            method_definitions: Vec::new(),
            expressions: Vec::new(),
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };

        let result = analyse(&module);
        let abstract_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Cannot instantiate abstract class"))
            .collect();

        assert_eq!(
            abstract_errors.len(),
            1,
            "Should detect abstract instantiation in class method, got: {:?}",
            result.diagnostics
        );
        assert!(abstract_errors[0].message.contains("Shape"));
    }

    #[test]
    fn test_abstract_instantiation_in_string_interpolation() {
        // BT-656: abstract instantiation inside string interpolation should be detected
        let shape = make_abstract_class("Shape");

        let interp = Expression::StringInterpolation {
            segments: vec![
                StringSegment::Literal("result: ".into()),
                StringSegment::Interpolation(make_spawn_expr("Shape")),
            ],
            span: test_span(),
        };

        let module = Module {
            classes: vec![shape],
            method_definitions: Vec::new(),
            expressions: vec![bare(interp)],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };

        let result = analyse(&module);
        let abstract_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Cannot instantiate abstract class"))
            .collect();

        assert_eq!(
            abstract_errors.len(),
            1,
            "Should detect abstract instantiation in string interpolation, got: {:?}",
            result.diagnostics
        );
        assert!(abstract_errors[0].message.contains("Shape"));
    }

    #[test]
    fn test_abstract_instantiation_in_standalone_method() {
        use crate::ast::{MethodKind, StandaloneMethodDefinition};

        // BT-656: abstract instantiation inside a standalone method definition should be detected
        let shape = make_abstract_class("Shape");

        let standalone = StandaloneMethodDefinition {
            class_name: Identifier::new("Foo", test_span()),
            is_class_method: false,
            method: MethodDefinition {
                selector: MessageSelector::Unary("build".into()),
                parameters: vec![],
                body: vec![bare(make_spawn_expr("Shape"))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            },
            span: test_span(),
        };

        let module = Module {
            classes: vec![shape],
            method_definitions: vec![standalone],
            expressions: Vec::new(),
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };

        let result = analyse(&module);
        let abstract_errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Cannot instantiate abstract class"))
            .collect();

        assert_eq!(
            abstract_errors.len(),
            1,
            "Should detect abstract instantiation in standalone method, got: {:?}",
            result.diagnostics
        );
        assert!(abstract_errors[0].message.contains("Shape"));
    }

    #[test]
    fn test_actor_new_warning_in_standalone_method() {
        use crate::ast::{MethodKind, StandaloneMethodDefinition};

        // BT-656: actor `new` usage warning inside standalone method definitions
        let counter = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        };

        // Counter new (should warn — use spawn instead)
        let new_expr = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Counter", test_span()),
                span: test_span(),
            }),
            selector: MessageSelector::Unary("new".into()),
            arguments: vec![],
            is_cast: false,
            span: test_span(),
        };

        let standalone = StandaloneMethodDefinition {
            class_name: Identifier::new("Foo", test_span()),
            is_class_method: false,
            method: MethodDefinition {
                selector: MessageSelector::Unary("build".into()),
                parameters: vec![],
                body: vec![bare(new_expr)],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            },
            span: test_span(),
        };

        let module = Module {
            classes: vec![counter],
            method_definitions: vec![standalone],
            expressions: Vec::new(),
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };

        let result = analyse(&module);
        let actor_warnings: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("should use `spawn`"))
            .collect();

        assert_eq!(
            actor_warnings.len(),
            1,
            "Should detect actor new usage in standalone method, got: {:?}",
            result.diagnostics
        );
        assert!(actor_warnings[0].message.contains("Counter"));
    }
}

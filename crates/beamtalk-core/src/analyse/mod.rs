// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis for Beamtalk.
//!
//! This module performs semantic analysis on the AST, including:
//! - Variable scope and lifetime analysis (via `scope` module)
//! - Pattern variable binding in match expressions
//! - Block context determination (control flow, stored, passed)
//! - Capture analysis for blocks
//! - Mutation tracking for captured variables in blocks
//!
//! The analysis produces diagnostics and metadata used by the code generator.

use crate::ast::{Expression, Identifier, MatchArm, Module, Pattern};
use crate::parse::{Diagnostic, Span};
use ecow::EcoString;
use std::collections::HashMap;

pub mod block_context;
pub mod error;
pub mod scope;

pub use error::{SemanticError, SemanticErrorKind};

/// Result of semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisResult {
    /// Diagnostics (errors and warnings) from analysis.
    pub diagnostics: Vec<Diagnostic>,

    /// Block metadata indexed by block span.
    pub block_info: HashMap<Span, BlockInfo>,
}

impl AnalysisResult {
    /// Create a new empty analysis result.
    #[must_use]
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            block_info: HashMap::new(),
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

/// Context in which a block is used.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockContext {
    /// Block used as control flow (if/while condition).
    ControlFlow,

    /// Block stored in a variable or field.
    Stored,

    /// Block passed as argument to a message send.
    Passed,

    /// Other known context (e.g., immediate evaluation).
    Other,

    /// Context could not be determined.
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

/// Extract variable bindings from a pattern.
///
/// Recursively traverses the pattern and collects all variable identifiers
/// that will be bound when the pattern matches.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::analyse::extract_pattern_bindings;
/// # use beamtalk_core::ast::{Pattern, Identifier};
/// # use beamtalk_core::parse::Span;
/// # use ecow::EcoString;
/// let pattern = Pattern::Variable(Identifier::new("x", Span::default()));
/// let bindings = extract_pattern_bindings(&pattern);
/// assert_eq!(bindings.len(), 1);
/// assert_eq!(bindings[0].name, EcoString::from("x"));
/// ```
pub fn extract_pattern_bindings(pattern: &Pattern) -> Vec<Identifier> {
    let mut bindings = Vec::new();
    extract_pattern_bindings_impl(pattern, &mut bindings);
    bindings
}

/// Internal implementation of pattern binding extraction.
///
/// # Note
///
/// This function collects all variable bindings without duplicate detection.
/// Duplicate variables like `{X, X}` will bind `X` twice, with the second
/// overwriting the first span in the scope. Erlang allows this as an equality
/// constraint, but validation is not yet implemented.
///
/// TODO(BT-183): Add duplicate pattern variable detection and validation.
/// Either emit diagnostic for duplicates or implement Erlang-style equality checks.
fn extract_pattern_bindings_impl(pattern: &Pattern, bindings: &mut Vec<Identifier>) {
    match pattern {
        // Variable patterns bind the identifier
        Pattern::Variable(id) => {
            bindings.push(id.clone());
        }

        // Tuple patterns: recursively extract from all elements
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                extract_pattern_bindings_impl(element, bindings);
            }
        }

        // List patterns: recursively extract from elements and tail
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                extract_pattern_bindings_impl(element, bindings);
            }
            if let Some(tail_pattern) = tail {
                extract_pattern_bindings_impl(tail_pattern, bindings);
            }
        }

        // Binary patterns: extract from segment value patterns
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                // Binary segments may have value patterns that bind variables
                extract_pattern_bindings_impl(&segment.value, bindings);
            }
        }

        // Wildcards and literals don't bind variables
        Pattern::Wildcard(_) | Pattern::Literal(_, _) => {}
    }
}

/// Perform semantic analysis on a module.
///
/// This is the main entry point for semantic analysis. It analyzes the module
/// AST and returns diagnostics and metadata for code generation.
///
/// Currently focuses on pattern variable binding in match expressions.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::analyse::analyse;
/// # use beamtalk_core::ast::Module;
/// # use beamtalk_core::parse::Span;
/// let module = Module::new(vec![], Span::default());
/// let result = analyse(&module);
/// assert_eq!(result.diagnostics.len(), 0);
/// ```
pub fn analyse(module: &Module) -> AnalysisResult {
    let mut analyser = Analyser::new();
    analyser.analyse_module(module);
    analyser.result
}

/// Internal analyser state.
struct Analyser {
    result: AnalysisResult,
    scope: scope::Scope,
}

impl Analyser {
    fn new() -> Self {
        Self {
            result: AnalysisResult::new(),
            scope: scope::Scope::new(),
        }
    }

    fn analyse_module(&mut self, module: &Module) {
        // Analyse top-level expressions
        for expr in &module.expressions {
            self.analyse_expression(expr, None);
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
            self.scope.define(&state.name.name, state.span);
        }

        // Analyse methods
        for method in &class.methods {
            self.analyse_method(method);
        }

        self.scope.pop(); // Exit class scope
    }

    fn analyse_method(&mut self, method: &crate::ast::MethodDefinition) {
        self.scope.push(); // Enter method scope (depth 2)

        // Define 'self' - implicitly available in all method bodies
        self.scope.define("self", method.span);

        // Define method parameters
        for param in &method.parameters {
            self.scope.define(&param.name, param.span);
        }

        // Analyse method body
        for expr in &method.body {
            self.analyse_expression(expr, None);
        }

        self.scope.pop(); // Exit method scope
    }

    fn analyse_expression(&mut self, expr: &Expression, parent_context: Option<ExprContext>) {
        #[allow(clippy::enum_glob_use)] // cleaner match arms
        use crate::ast::Expression::*;

        match expr {
            Identifier(id) => {
                // Check if variable is defined in scope
                if self.scope.lookup(&id.name).is_none() {
                    self.result.diagnostics.push(Diagnostic::error(
                        format!("Undefined variable: {}", id.name),
                        id.span,
                    ));
                }
            }

            Assignment { target, value, .. } => {
                // Handle assignment target
                match target.as_ref() {
                    Identifier(id) => {
                        // Only define if not already in an outer scope
                        if self.scope.lookup(&id.name).is_none() {
                            self.scope.define(&id.name, id.span);
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
                span: _,
            } => {
                self.analyse_expression(receiver, None);

                // Determine context for block arguments
                let selector_str = block_context::selector_to_string(selector);
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

            CompoundAssignment { target, value, .. } => {
                self.analyse_expression(target, None);
                self.analyse_expression(value, None);
            }

            Pipe { value, target, .. } => {
                self.analyse_expression(value, None);
                self.analyse_expression(target, None);
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

            Literal(..) | Super(..) | Error { .. } | ClassReference { .. } => {
                // No analysis needed
            }
        }
    }

    fn analyse_block(&mut self, block: &crate::ast::Block, parent_context: Option<ExprContext>) {
        self.scope.push(); // Enter block scope (depth 3+)

        // Determine block context
        let context = match parent_context {
            Some(ExprContext::ControlFlowArg) => BlockContext::ControlFlow,
            Some(ExprContext::MessageArg) => BlockContext::Passed,
            Some(ExprContext::Assignment) => BlockContext::Stored,
            None => BlockContext::Unknown,
        };

        // Define block parameters
        for param in &block.parameters {
            self.scope.define(&param.name, param.span);
        }

        // Track captures and mutations
        let mut captures = Vec::new();
        let mut mutations = Vec::new();

        // Analyse block body
        for expr in &block.body {
            self.collect_captures_and_mutations(expr, &mut captures, &mut mutations);
            self.analyse_expression(expr, None);
        }

        // Store block info
        let block_info = BlockInfo {
            context,
            captures,
            mutations,
        };
        self.result.block_info.insert(block.span, block_info);

        self.scope.pop(); // Exit block scope
    }

    fn analyse_match_arm(&mut self, arm: &MatchArm) {
        // Create a new scope for this match arm
        self.scope.push();

        // Extract and define all pattern variables
        let bindings = extract_pattern_bindings(&arm.pattern);
        for binding in bindings {
            self.scope.define(&binding.name, binding.span);
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

    #[allow(clippy::too_many_lines)] // recursive traversal function
    fn collect_captures_and_mutations(
        &self,
        expr: &Expression,
        captures: &mut Vec<CapturedVar>,
        mutations: &mut Vec<Mutation>,
    ) {
        #[allow(clippy::enum_glob_use)] // cleaner match arms
        use crate::ast::Expression::*;

        match expr {
            Identifier(id) => {
                // Check if this is a captured variable
                if let Some(var_info) = self.scope.lookup(&id.name) {
                    if self.scope.is_captured(&id.name) {
                        // Only add if not already in captures list
                        if !captures.iter().any(|c| c.name == id.name) {
                            captures.push(CapturedVar {
                                name: id.name.clone(),
                                defined_at: var_info.defined_at,
                            });
                        }
                    }
                }
            }

            Assignment {
                target,
                value,
                span,
            } => {
                // Track mutation
                if let Identifier(id) = target.as_ref() {
                    let kind = if self.scope.is_captured(&id.name) {
                        MutationKind::CapturedVariable {
                            name: id.name.clone(),
                        }
                    } else {
                        MutationKind::LocalVariable {
                            name: id.name.clone(),
                        }
                    };
                    mutations.push(Mutation { kind, span: *span });
                } else if let FieldAccess { field, .. } = target.as_ref() {
                    mutations.push(Mutation {
                        kind: MutationKind::Field {
                            name: field.name.clone(),
                        },
                        span: *span,
                    });
                }

                // Recurse into value
                self.collect_captures_and_mutations(value, captures, mutations);
            }

            CompoundAssignment {
                target,
                value,
                span,
                ..
            } => {
                // Track mutation
                if let Identifier(id) = target.as_ref() {
                    let kind = if self.scope.is_captured(&id.name) {
                        MutationKind::CapturedVariable {
                            name: id.name.clone(),
                        }
                    } else {
                        MutationKind::LocalVariable {
                            name: id.name.clone(),
                        }
                    };
                    mutations.push(Mutation { kind, span: *span });
                } else if let FieldAccess { field, .. } = target.as_ref() {
                    mutations.push(Mutation {
                        kind: MutationKind::Field {
                            name: field.name.clone(),
                        },
                        span: *span,
                    });
                }

                // Recurse
                self.collect_captures_and_mutations(target, captures, mutations);
                self.collect_captures_and_mutations(value, captures, mutations);
            }

            Block(_block) => {
                // Do not recurse into nested blocks here.
                // Nested blocks are analyzed separately via `analyse_block`
                // which handles proper scoping and parameter definitions.
            }

            MessageSend {
                receiver,
                arguments,
                ..
            } => {
                self.collect_captures_and_mutations(receiver, captures, mutations);
                for arg in arguments {
                    self.collect_captures_and_mutations(arg, captures, mutations);
                }
            }

            FieldAccess { receiver, .. } => {
                self.collect_captures_and_mutations(receiver, captures, mutations);
            }

            Cascade {
                receiver, messages, ..
            } => {
                self.collect_captures_and_mutations(receiver, captures, mutations);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.collect_captures_and_mutations(arg, captures, mutations);
                    }
                }
            }

            Return { value, .. } => {
                self.collect_captures_and_mutations(value, captures, mutations);
            }

            Parenthesized { expression, .. } => {
                self.collect_captures_and_mutations(expression, captures, mutations);
            }

            Pipe { value, target, .. } => {
                self.collect_captures_and_mutations(value, captures, mutations);
                self.collect_captures_and_mutations(target, captures, mutations);
            }

            Match { value, arms, .. } => {
                self.collect_captures_and_mutations(value, captures, mutations);
                for arm in arms {
                    // Analyze guard if present
                    if let Some(guard) = &arm.guard {
                        self.collect_captures_and_mutations(guard, captures, mutations);
                    }
                    self.collect_captures_and_mutations(&arm.body, captures, mutations);
                }
            }

            MapLiteral { pairs, .. } => {
                // Collect captures and mutations from map literal pairs
                for pair in pairs {
                    self.collect_captures_and_mutations(&pair.key, captures, mutations);
                    self.collect_captures_and_mutations(&pair.value, captures, mutations);
                }
            }

            Literal(..) | Super(..) | Error { .. } | ClassReference { .. } => {
                // No captures or mutations
            }
        }
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
    use super::*;
    use crate::ast::{
        BinarySegment, Block, BlockParameter, Expression, Identifier, Literal, MessageSelector,
    };
    use crate::parse::Span;

    fn test_span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_extract_pattern_bindings_variable() {
        let pattern = Pattern::Variable(Identifier::new("x", test_span()));
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].name, "x");
    }

    #[test]
    fn test_extract_pattern_bindings_wildcard() {
        let pattern = Pattern::Wildcard(test_span());
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 0);
    }

    #[test]
    fn test_extract_pattern_bindings_literal() {
        let pattern = Pattern::Literal(Literal::Integer(42), test_span());
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 0);
    }

    #[test]
    fn test_extract_pattern_bindings_tuple() {
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(Identifier::new("x", test_span())),
                Pattern::Variable(Identifier::new("y", test_span())),
            ],
            span: test_span(),
        };
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "x");
        assert_eq!(bindings[1].name, "y");
    }

    #[test]
    fn test_extract_pattern_bindings_nested_tuple() {
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(Identifier::new("status", test_span())),
                Pattern::Tuple {
                    elements: vec![
                        Pattern::Variable(Identifier::new("x", test_span())),
                        Pattern::Variable(Identifier::new("y", test_span())),
                    ],
                    span: test_span(),
                },
            ],
            span: test_span(),
        };
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 3);
        assert_eq!(bindings[0].name, "status");
        assert_eq!(bindings[1].name, "x");
        assert_eq!(bindings[2].name, "y");
    }

    #[test]
    fn test_extract_pattern_bindings_list() {
        let pattern = Pattern::List {
            elements: vec![
                Pattern::Variable(Identifier::new("head", test_span())),
                Pattern::Variable(Identifier::new("second", test_span())),
            ],
            tail: Some(Box::new(Pattern::Variable(Identifier::new(
                "tail",
                test_span(),
            )))),
            span: test_span(),
        };
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 3);
        assert_eq!(bindings[0].name, "head");
        assert_eq!(bindings[1].name, "second");
        assert_eq!(bindings[2].name, "tail");
    }

    #[test]
    fn test_extract_pattern_bindings_list_no_tail() {
        let pattern = Pattern::List {
            elements: vec![
                Pattern::Variable(Identifier::new("a", test_span())),
                Pattern::Variable(Identifier::new("b", test_span())),
            ],
            tail: None,
            span: test_span(),
        };
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "a");
        assert_eq!(bindings[1].name, "b");
    }

    #[test]
    fn test_extract_pattern_bindings_binary() {
        let pattern = Pattern::Binary {
            segments: vec![
                BinarySegment {
                    value: Pattern::Variable(Identifier::new("version", test_span())),
                    size: None,
                    segment_type: None,
                    signedness: None,
                    endianness: None,
                    unit: None,
                    span: test_span(),
                },
                BinarySegment {
                    value: Pattern::Variable(Identifier::new("data", test_span())),
                    size: None,
                    segment_type: None,
                    signedness: None,
                    endianness: None,
                    unit: None,
                    span: test_span(),
                },
            ],
            span: test_span(),
        };
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "version");
        assert_eq!(bindings[1].name, "data");
    }

    #[test]
    fn test_extract_pattern_bindings_mixed() {
        // Pattern like: {#ok, [first | _], value}
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                Pattern::List {
                    elements: vec![Pattern::Variable(Identifier::new("first", test_span()))],
                    tail: Some(Box::new(Pattern::Wildcard(test_span()))),
                    span: test_span(),
                },
                Pattern::Variable(Identifier::new("value", test_span())),
            ],
            span: test_span(),
        };
        let bindings = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "first");
        assert_eq!(bindings[1].name, "value");
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
            vec![Expression::Identifier(Identifier::new("x", test_span()))],
            test_span(),
        );
        let expr = Expression::Block(block);
        let module = Module::new(vec![expr], test_span());

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
            vec![Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            ))],
            Span::new(10, 20),
        );

        let module = Module::new(vec![count_def, Expression::Block(block)], test_span());

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
                Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new("temp", test_span()))),
                    value: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                    span: test_span(),
                },
                Expression::Identifier(Identifier::new("temp", test_span())),
            ],
            Span::new(10, 20),
        );

        let module = Module::new(vec![Expression::Block(block)], test_span());

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
            vec![Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    test_span(),
                ))),
                value: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                span: test_span(),
            }],
            Span::new(10, 20),
        );

        let module = Module::new(vec![count_def, Expression::Block(block)], test_span());

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
            vec![Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                value: Box::new(Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    test_span(),
                )),
                span: test_span(),
            }],
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
            span: test_span(),
        };

        let module = Module::new(vec![message_send], test_span());

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
            vec![Expression::Identifier(Identifier::new(
                "x",
                Span::new(19, 20),
            ))],
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

        let module = Module::new(vec![assignment], Span::new(0, 25));
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
            vec![Expression::Identifier(Identifier::new(
                "x",
                Span::new(27, 28),
            ))],
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
            span: Span::new(0, 33),
        };

        let module = Module::new(vec![message], Span::new(0, 33));
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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

        let module = Module::new(vec![match_expr], test_span());
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
            ClassDefinition, MessageSelector, MethodDefinition, MethodKind, StateDeclaration,
        };

        let get_value_method = MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
                field: Identifier::new("value", test_span()),
                span: test_span(),
            }],
            return_type: None,
            is_sealed: false,
            kind: MethodKind::Primary,
            span: test_span(),
        };

        let state_decl = StateDeclaration {
            name: Identifier::new("value", test_span()),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), test_span())),
            span: test_span(),
        };

        let class_def = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Identifier::new("Actor", test_span()),
            is_abstract: false,
            is_sealed: false,
            state: vec![state_decl],
            methods: vec![get_value_method],
            span: test_span(),
        };

        let module = Module {
            classes: vec![class_def],
            expressions: vec![],
            leading_comments: vec![],
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
}

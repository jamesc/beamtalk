// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block context analysis — captures, mutations, and scope tracking.
//!
//! **DDD Context:** Semantic Analysis
//!
//! The `Analyser` walks the AST after name resolution and type checking,
//! recording block captures, mutations, and context (control flow, stored, passed).

use crate::ast::{Expression, MatchArm, Module};
use crate::semantic_analysis::{
    AnalysisResult, BindingKind, TypeMap, block_context, method_validators, scope,
};

/// Internal analyser state.
pub(super) struct Analyser {
    pub(super) result: AnalysisResult,
    pub(super) scope: scope::Scope,
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
    pub(super) fn with_scope(scope: scope::Scope, type_map: TypeMap) -> Self {
        Self {
            result: AnalysisResult::new(),
            scope,
            method_validators: method_validators::MethodValidatorRegistry::new(),
            type_map,
        }
    }

    pub(super) fn analyse_module(&mut self, module: &Module) {
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
    pub(super) fn analyse_expression(
        &mut self,
        expr: &Expression,
        parent_context: Option<ExprContext>,
    ) {
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

            DestructureAssignment { pattern, value, .. } => {
                self.analyse_expression(value, None);
                // Define pattern variables in scope for capture tracking
                self.define_pattern_variables_in_scope(pattern);
            }

            Literal(..)
            | Super(..)
            | Error { .. }
            | ClassReference { .. }
            | Primitive { .. }
            | ExpectDirective { .. }
            | Spread { .. } => {
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

    /// Defines variables from a destructuring pattern in the current scope.
    fn define_pattern_variables_in_scope(&mut self, pattern: &crate::ast::Pattern) {
        use crate::ast::Pattern;
        match pattern {
            Pattern::Variable(id) => {
                if self.scope.lookup_immut(&id.name).is_none() {
                    self.scope.define(&id.name, id.span, BindingKind::Local);
                }
            }
            Pattern::Tuple { elements, .. } => {
                for elem in elements {
                    self.define_pattern_variables_in_scope(elem);
                }
            }
            Pattern::Array { elements, rest, .. } => {
                for elem in elements {
                    self.define_pattern_variables_in_scope(elem);
                }
                if let Some(rest_pat) = rest {
                    self.define_pattern_variables_in_scope(rest_pat);
                }
            }
            Pattern::List { elements, tail, .. } => {
                for elem in elements {
                    self.define_pattern_variables_in_scope(elem);
                }
                if let Some(t) = tail {
                    self.define_pattern_variables_in_scope(t);
                }
            }
            Pattern::Map { pairs, .. } => {
                for pair in pairs {
                    self.define_pattern_variables_in_scope(&pair.value);
                }
            }
            Pattern::Constructor { keywords, .. } => {
                for (_, binding) in keywords {
                    self.define_pattern_variables_in_scope(binding);
                }
            }
            Pattern::Binary { .. } | Pattern::Wildcard(_) | Pattern::Literal(_, _) => {}
        }
    }

    fn analyse_match_arm(&mut self, arm: &MatchArm) {
        // Create a new scope for this match arm
        self.scope.push();

        // Pattern variable binding is now handled by NameResolver
        // Extract bindings here just to define them in the scope for captures/mutations tracking
        let (bindings, _pattern_diagnostics) =
            crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
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
pub(super) enum ExprContext {
    /// Expression is an argument to a control flow message.
    ControlFlowArg,
    /// Expression is an argument to a regular message.
    MessageArg,
    /// Expression is being assigned to a variable.
    Assignment,
}

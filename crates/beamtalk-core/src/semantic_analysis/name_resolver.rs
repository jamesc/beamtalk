// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Name resolution for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements the `NameResolver` domain service from the DDD model.
//! The `NameResolver` is responsible for:
//! - Managing scope hierarchy (module → class → method → block)
//! - Defining bindings (variables, parameters, fields)
//! - Looking up identifiers and resolving them to bindings
//! - Detecting undefined variables and emitting diagnostics
//!
//! **References:**
//! - `docs/beamtalk-ddd-model.md` - Semantic Analysis Context

use crate::ast::{Block, ClassDefinition, Expression, MatchArm, MethodDefinition, Module};
use crate::semantic_analysis::scope::{BindingKind, Scope};
use crate::source_analysis::{Diagnostic, Span};

/// Name resolution domain service.
///
/// **DDD Context:** Semantic Analysis - Domain Service
///
/// Resolves identifiers to bindings and manages scope hierarchy. This is a
/// stateful service that owns and manages the `Scope` domain object and
/// accumulates diagnostics during name resolution.
#[derive(Debug)]
pub struct NameResolver {
    scope: Scope,
    diagnostics: Vec<Diagnostic>,
}

impl NameResolver {
    /// Creates a new name resolver.
    #[must_use]
    pub fn new() -> Self {
        Self {
            scope: Scope::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Returns all diagnostics collected during name resolution.
    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Takes ownership of diagnostics, leaving an empty vec.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Returns a reference to the current scope.
    #[must_use]
    pub fn scope(&self) -> &Scope {
        &self.scope
    }

    /// Consumes the name resolver and returns ownership of the scope.
    ///
    /// This allows the scope to be passed to other analysis phases (e.g., Analyser)
    /// without duplicating scope construction.
    #[must_use]
    pub fn into_scope(self) -> Scope {
        self.scope
    }

    /// Resolves a module, defining bindings and detecting undefined variables.
    pub fn resolve_module(&mut self, module: &Module) {
        // Define built-in identifiers that are always available
        // These are special values in Beamtalk (true, false, nil)
        self.scope.define("true", module.span, BindingKind::Local);
        self.scope.define("false", module.span, BindingKind::Local);
        self.scope.define("nil", module.span, BindingKind::Local);

        // Resolve top-level expressions
        for expr in &module.expressions {
            self.resolve_expression(expr);
        }

        // Resolve classes
        for class in &module.classes {
            self.resolve_class(class);
        }

        // Note: No unused variable warnings at module scope (depth 0).
        // Module-level variables include built-in identifiers (true, false, nil)
        // and REPL known vars, which would produce false positives.
    }

    /// Pre-defines known variables (for REPL context).
    ///
    /// Variables passed in `known_vars` are treated as already defined,
    /// preventing "Undefined variable" errors for REPL session variables.
    pub fn define_known_vars(&mut self, known_vars: &[&str], span: Span) {
        for var_name in known_vars {
            self.scope.define(var_name, span, BindingKind::Local);
        }
    }

    /// Resolves a class definition, entering class scope and resolving methods.
    fn resolve_class(&mut self, class: &ClassDefinition) {
        self.scope.push(); // Enter class scope (depth 1)

        // Define state variables in class scope
        for state in &class.state {
            self.scope
                .define(&state.name.name, state.span, BindingKind::InstanceField);
        }

        // Resolve instance methods
        for method in &class.methods {
            self.resolve_method(method);
        }

        // Resolve class methods
        for method in &class.class_methods {
            self.resolve_method(method);
        }

        self.scope.pop(); // Exit class scope
    }

    /// Resolves a method definition, entering method scope and resolving parameters and body.
    fn resolve_method(&mut self, method: &MethodDefinition) {
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

        // Resolve method body, checking for dead code after early return
        self.resolve_body(&method.body);

        // Collect unused variable warnings before exiting scope
        self.collect_unused_warnings();

        self.scope.pop(); // Exit method scope
    }

    /// Resolves an expression, checking for undefined variables and defining new bindings.
    #[allow(clippy::too_many_lines)] // one arm per Expression variant; irreducible
    fn resolve_expression(&mut self, expr: &Expression) {
        #[allow(clippy::enum_glob_use)] // cleaner match arms
        use Expression::*;

        match expr {
            Identifier(id) => {
                // Check if variable is defined in scope
                if self.scope.lookup(&id.name).is_none() {
                    if id.name == "self" {
                        self.diagnostics.push(Diagnostic::error(
                            "self can only be used inside a method body",
                            id.span,
                        ));
                    } else {
                        self.diagnostics.push(Diagnostic::error(
                            format!("Undefined variable: {}", id.name),
                            id.span,
                        ));
                    }
                }
            }

            Assignment { target, value, .. } => {
                // Handle assignment target
                match target.as_ref() {
                    Identifier(id) => {
                        // Only define if not already in an outer scope
                        // Use lookup_immut: assigning to a variable is not a "read"
                        if self.scope.lookup_immut(&id.name).is_none() {
                            self.scope.define(&id.name, id.span, BindingKind::Local);
                        }
                    }
                    _ => {
                        // For field access, analyze the target (especially the receiver)
                        self.resolve_expression(target);
                    }
                }
                self.resolve_expression(value);
            }

            Block(block) => {
                self.resolve_block(block);
            }

            MessageSend {
                receiver,
                arguments,
                ..
            } => {
                self.resolve_expression(receiver);
                for arg in arguments {
                    self.resolve_expression(arg);
                }
            }

            FieldAccess { receiver, .. } => {
                self.resolve_expression(receiver);
            }

            Cascade {
                receiver, messages, ..
            } => {
                self.resolve_expression(receiver);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.resolve_expression(arg);
                    }
                }
            }

            Return { value, .. } => {
                self.resolve_expression(value);
            }

            Parenthesized { expression, .. } => {
                self.resolve_expression(expression);
            }

            Pipe { value, target, .. } => {
                self.resolve_expression(value);
                self.resolve_expression(target);
            }

            Match { value, arms, .. } => {
                self.resolve_expression(value);
                for arm in arms {
                    self.resolve_match_arm(arm);
                }
            }

            MapLiteral { pairs, .. } => {
                // Resolve key and value expressions in map literals
                for pair in pairs {
                    self.resolve_expression(&pair.key);
                    self.resolve_expression(&pair.value);
                }
            }

            ListLiteral { elements, tail, .. } => {
                for elem in elements {
                    self.resolve_expression(elem);
                }
                if let Some(t) = tail {
                    self.resolve_expression(t);
                }
            }

            Super(span) => {
                // super can only be used inside a method body (depth >= 2).
                // Depth 0 = module, 1 = class, 2+ = method/block.
                if self.scope.current_depth() < 2 {
                    self.diagnostics.push(Diagnostic::error(
                        "super can only be used inside a method body",
                        *span,
                    ));
                }
            }

            Literal(..) | Error { .. } | ClassReference { .. } | Primitive { .. } => {
                // No name resolution needed
            }

            StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(expr) = segment {
                        self.resolve_expression(expr);
                    }
                }
            }
        }
    }

    /// Resolves a block expression, entering block scope and resolving parameters and body.
    fn resolve_block(&mut self, block: &Block) {
        self.scope.push(); // Enter block scope (depth 3+)

        // Define block parameters, checking for shadowing
        for param in &block.parameters {
            self.check_shadowing(&param.name, param.span);
            self.scope
                .define(&param.name, param.span, BindingKind::Parameter);
        }

        // Resolve block body, checking for dead code after early return
        self.resolve_body(&block.body);

        // Collect unused variable warnings before exiting scope
        self.collect_unused_warnings();

        self.scope.pop(); // Exit block scope
    }

    /// Resolves a match arm, entering arm scope and defining pattern variables.
    fn resolve_match_arm(&mut self, arm: &MatchArm) {
        // Create a new scope for this match arm
        self.scope.push();

        // Extract and define all pattern variables, collect duplicate diagnostics
        let (bindings, pattern_diagnostics) =
            crate::semantic_analysis::extract_pattern_bindings(&arm.pattern);
        self.diagnostics.extend(pattern_diagnostics);

        for binding in bindings {
            // Pattern variables use Parameter kind — they're introduced by syntax,
            // not by explicit assignment (:=), so they're exempt from unused warnings.
            self.scope
                .define(&binding.name, binding.span, BindingKind::Parameter);
        }

        // Resolve guard expression (if present) - can see pattern variables
        if let Some(guard) = &arm.guard {
            self.resolve_expression(guard);
        }

        // Resolve body expression - can see pattern variables
        self.resolve_expression(&arm.body);

        // Collect unused variable warnings before exiting scope
        self.collect_unused_warnings();

        // Exit match arm scope
        self.scope.pop();
    }

    /// Checks if defining a variable with the given name would shadow an outer variable.
    ///
    /// Emits a warning if the name exists in an outer scope. Variables prefixed
    /// with `_` are exempt (conventional "I know what I'm doing" signal).
    fn check_shadowing(&mut self, name: &str, span: Span) {
        if name.starts_with('_') {
            return;
        }

        // Check if this name exists in an outer scope (different depth)
        if let Some(outer) = self.scope.lookup_immut(name) {
            // Only warn if the existing binding is at a strictly outer scope depth.
            // Same-depth duplicates (e.g., [:x :x |]) are not shadowing.
            if outer.depth < self.scope.current_depth() {
                let outer_span = outer.defined_at;
                let mut diag =
                    Diagnostic::warning(format!("Variable `{name}` shadows outer variable"), span);
                diag.hint = Some(
                    format!(
                        "Outer variable defined at offset {}. Use a different name to avoid confusion",
                        outer_span.start()
                    )
                    .into(),
                );
                self.diagnostics.push(diag);
            }
        }
    }

    /// Resolves a body (method or block), detecting unreachable code after early return.
    ///
    /// Iterates through expressions, resolving each one. If a `Return` expression
    /// is encountered and there are subsequent expressions, emits a warning for the
    /// first unreachable expression only (avoids diagnostic spam).
    fn resolve_body(&mut self, body: &[Expression]) {
        let mut saw_return = false;

        for expr in body {
            if saw_return {
                // First unreachable expression after early return — warn and stop
                let mut diag =
                    Diagnostic::warning("Unreachable code after early return", expr.span());
                diag.hint = Some("Code after an early return (^) will never execute".into());
                self.diagnostics.push(diag);
                // Still resolve the rest for other diagnostics (undefined vars, etc.)
                self.resolve_expression(expr);
                break;
            }

            self.resolve_expression(expr);

            if matches!(expr, Expression::Return { .. }) {
                saw_return = true;
            }
        }
    }

    /// Collects warnings for unused local variables in the current scope.
    ///
    /// A variable is considered unused if:
    /// - It is a `Local` binding (not a parameter or field)
    /// - It was never looked up (referenced) after definition
    /// - Its name does not start with `_` (underscore prefix suppresses the warning)
    /// - It is not the implicit `self` binding
    fn collect_unused_warnings(&mut self) {
        let unused: Vec<_> = self
            .scope
            .unused_locals()
            .iter()
            .map(|b| (b.name.clone(), b.defined_at))
            .collect();
        for (name, span) in unused {
            let mut diag = Diagnostic::warning(format!("Unused variable `{name}`"), span);
            diag.hint =
                Some(format!("If this is intentional, prefix with underscore: _{name}").into());
            self.diagnostics.push(diag);
        }
    }
}

impl Default for NameResolver {
    fn default() -> Self {
        Self::new()
    }
}

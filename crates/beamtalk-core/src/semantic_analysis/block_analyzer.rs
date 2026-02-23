// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block analysis for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module handles block-specific analysis within the `Analyser`:
//! - Block context determination (control flow, stored, passed)
//! - Variable capture tracking
//! - Mutation tracking and diagnostics

use crate::ast::Expression;
use crate::semantic_analysis::{
    BlockContext, BlockInfo, CapturedVar, ExprContext, Mutation, MutationKind,
};
use crate::source_analysis::Diagnostic;

use super::Analyser;

impl Analyser {
    pub(super) fn analyse_block(
        &mut self,
        block: &crate::ast::Block,
        parent_context: Option<ExprContext>,
    ) {
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
            self.scope.define(
                &param.name,
                param.span,
                crate::semantic_analysis::scope::BindingKind::Parameter,
            );
        }

        // Track captures and mutations
        let mut captures = Vec::new();
        let mut mutations = Vec::new();

        // Analyse block body
        for expr in &block.body {
            self.collect_captures_and_mutations(expr, &mut captures, &mut mutations);
            self.analyse_expression(expr, None);
        }

        // Store block info before emitting diagnostics (avoids unnecessary clones)
        let block_info = BlockInfo {
            context,
            captures,
            mutations: mutations.clone(), // Clone only mutations (needed for diagnostics below)
        };
        self.result.block_info.insert(block.span, block_info);

        // Emit diagnostics for mutations based on block context
        for mutation in &mutations {
            match &mutation.kind {
                MutationKind::Field { name } => {
                    // Error: Field assignment in Stored or Passed blocks
                    if matches!(context, BlockContext::Stored | BlockContext::Passed) {
                        let context_str = match context {
                            BlockContext::Stored => "stored",
                            BlockContext::Passed => "passed",
                            _ => "stored or passed",
                        };
                        self.result.diagnostics.push(Diagnostic::error(
                            format!(
                                "cannot assign to field '{name}' inside a {context_str} closure\n\
                                 \n\
                                 = help: field assignments require immediate execution context\n\
                                 = help: use control flow directly: `items do: [:item | self.{name} := value]`"
                            ),
                            mutation.span,
                        ));
                    }
                }
                MutationKind::CapturedVariable { name } => {
                    // Warning: Captured variable mutation in Stored blocks
                    if matches!(context, BlockContext::Stored) {
                        self.result.diagnostics.push(Diagnostic::warning(
                            format!(
                                "assignment to '{name}' has no effect on outer scope\n\
                                 \n\
                                 = help: closures capture variables by value\n\
                                 = help: use control flow directly: `10 timesRepeat: [{name} := {name} + 1]`"
                            ),
                            mutation.span,
                        ));
                    }
                }
                MutationKind::LocalVariable { .. } => {
                    // Local variable mutations are always allowed
                }
            }
        }

        self.scope.pop(); // Exit block scope
    }

    #[allow(clippy::too_many_lines)] // recursive traversal function
    pub(super) fn collect_captures_and_mutations(
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
                let binding_info = self.scope.lookup_immut(&id.name).map(|b| b.defined_at);
                if let Some(defined_at) = binding_info {
                    if self.scope.is_captured(&id.name) {
                        // Only add if not already in captures list
                        if !captures.iter().any(|c| c.name == id.name) {
                            captures.push(CapturedVar {
                                name: id.name.clone(),
                                defined_at,
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
                } else if let FieldAccess {
                    receiver, field, ..
                } = target.as_ref()
                {
                    mutations.push(Mutation {
                        kind: MutationKind::Field {
                            name: field.name.clone(),
                        },
                        span: *span,
                    });
                    // Receiver is evaluated; track captures within it.
                    self.collect_captures_and_mutations(receiver, captures, mutations);
                }

                // Recurse into value
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

            Match { value, arms, .. } => {
                self.collect_captures_and_mutations(value, captures, mutations);
                for arm in arms {
                    // Extract pattern-bound variable names so we can exclude them
                    // from captures. Without this, a pattern variable with the same
                    // name as an outer-scope variable would be incorrectly treated
                    // as a captured variable (BT-655).
                    let (bindings, _) =
                        crate::semantic_analysis::extract_pattern_bindings(&arm.pattern);
                    let pattern_names: std::collections::HashSet<&str> =
                        bindings.iter().map(|b| b.name.as_str()).collect();

                    // Collect this arm's captures separately so we can filter them
                    let mut arm_captures = Vec::new();
                    if let Some(guard) = &arm.guard {
                        self.collect_captures_and_mutations(guard, &mut arm_captures, mutations);
                    }
                    self.collect_captures_and_mutations(&arm.body, &mut arm_captures, mutations);

                    // Only keep captures that aren't pattern-bound variables
                    for cap in arm_captures {
                        if !pattern_names.contains(cap.name.as_str())
                            && !captures.iter().any(|c| c.name == cap.name)
                        {
                            captures.push(cap);
                        }
                    }
                }
            }

            MapLiteral { pairs, .. } => {
                // Collect captures and mutations from map literal pairs
                for pair in pairs {
                    self.collect_captures_and_mutations(&pair.key, captures, mutations);
                    self.collect_captures_and_mutations(&pair.value, captures, mutations);
                }
            }

            ListLiteral { elements, tail, .. } => {
                for elem in elements {
                    self.collect_captures_and_mutations(elem, captures, mutations);
                }
                if let Some(t) = tail {
                    self.collect_captures_and_mutations(t, captures, mutations);
                }
            }

            ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.collect_captures_and_mutations(elem, captures, mutations);
                }
            }

            Literal(..)
            | Super(..)
            | Error { .. }
            | ClassReference { .. }
            | Primitive { .. }
            | ExpectDirective { .. } => {
                // No captures or mutations
            }

            StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(expr) = segment {
                        self.collect_captures_and_mutations(expr, captures, mutations);
                    }
                }
            }
        }
    }
}

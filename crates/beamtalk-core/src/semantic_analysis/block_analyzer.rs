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
        for stmt in &block.body {
            self.collect_captures_and_mutations(&stmt.expression, &mut captures, &mut mutations);
            self.analyse_expression(&stmt.expression, None);
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
                    // BT-1140: Field assignments in Passed blocks are now supported via
                    // Tier 2 state threading — the actor State IS the StateAcc, so
                    // field reads/writes inside the block propagate back to the caller.
                    //
                    // Error: Field assignment in Stored blocks remains unsupported because
                    // the compiler cannot track the Tier 2 type of a block stored in a
                    // variable (see BT-909 for future tracking of block variable types).
                    if matches!(context, BlockContext::Stored) {
                        self.result.diagnostics.push(Diagnostic::error(
                            format!(
                                "cannot assign to field '{name}' inside a stored closure\n\
                                 \n\
                                 = help: field assignments require immediate execution context\n\
                                 = help: use control flow directly: `items do: [:item | self.{name} := value]`"
                            ),
                            mutation.span,
                        ).with_hint("Inline the block at the call site, or extract the field assignment into a separate method"));
                    }
                }
                MutationKind::CapturedVariable { .. } | MutationKind::LocalVariable { .. } => {
                    // BT-856 (ADR 0041 Phase 3): Captured variable mutations in Stored/Passed
                    // blocks are now supported via the Tier 2 stateful block protocol (BT-852).
                    // State is threaded through StateAcc maps so mutations propagate correctly.
                    // No diagnostic needed — this is valid and working behaviour.
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
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
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

            DestructureAssignment { value, .. } => {
                // Walk into value; pattern variables are new bindings, not field mutations
                self.collect_captures_and_mutations(value, captures, mutations);
            }

            Literal(..)
            | Super(..)
            | Error { .. }
            | ClassReference { .. }
            | Primitive { .. }
            | ExpectDirective { .. }
            | Spread { .. } => {
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

#[cfg(test)]
mod tests {
    use crate::semantic_analysis::{BlockContext, analyse};
    use crate::source_analysis::Severity;

    fn parse_module(src: &str) -> crate::ast::Module {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diagnostics) = crate::source_analysis::parse(tokens);
        let parse_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            parse_errors.is_empty(),
            "Test fixture failed to parse cleanly: {parse_errors:?}"
        );
        module
    }

    #[test]
    fn control_flow_block_has_control_flow_context() {
        let src = "Object subclass: Foo\n  bar => true ifTrue: [42]";
        let module = parse_module(src);
        let result = analyse(&module);
        assert!(
            result
                .block_info
                .values()
                .any(|b| b.context == BlockContext::ControlFlow),
            "Expected ControlFlow block context, got: {:?}",
            result
                .block_info
                .values()
                .map(|b| b.context)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn stored_block_has_stored_context() {
        let src = "Object subclass: Foo\n  bar =>\n    f := [42].\n    f";
        let module = parse_module(src);
        let result = analyse(&module);
        assert!(
            result
                .block_info
                .values()
                .any(|b| b.context == BlockContext::Stored),
            "Expected Stored block context, got: {:?}",
            result
                .block_info
                .values()
                .map(|b| b.context)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn outer_variable_captured_in_block() {
        let src = "Object subclass: Foo\n  bar =>\n    x := 42.\n    [:y | x + y]";
        let module = parse_module(src);
        let result = analyse(&module);
        let has_capture = result
            .block_info
            .values()
            .any(|b| b.captures.iter().any(|c| c.name == "x"));
        assert!(
            has_capture,
            "Expected x to be captured in block, captures: {:?}",
            result
                .block_info
                .values()
                .map(|b| &b.captures)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn field_mutation_in_stored_block_produces_error() {
        let src = "Object subclass: Foo\n  state: x = 0\n  bar =>\n    f := [self.x := 1].\n    f";
        let module = parse_module(src);
        let result = analyse(&module);
        assert!(
            result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("cannot assign to field")),
            "Expected field-in-stored-block error, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn block_passed_as_message_arg_has_passed_context() {
        let src = "Object subclass: Foo\n  bar => self doWith: [42]";
        let module = parse_module(src);
        let result = analyse(&module);
        assert!(
            result
                .block_info
                .values()
                .any(|b| b.context == BlockContext::Passed),
            "Expected Passed block context, got: {:?}",
            result
                .block_info
                .values()
                .map(|b| b.context)
                .collect::<Vec<_>>()
        );
    }
}

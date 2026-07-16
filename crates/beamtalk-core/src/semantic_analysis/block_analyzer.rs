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
            Some(ExprContext::Assignment | ExprContext::FieldAssignment) => BlockContext::Stored,
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
                    // BT-2797: Field assignments in blocks stored into an instance
                    // field (`self.field := [block]`) are also now supported —
                    // every `self.field value(:...)` call site runtime-discriminates
                    // Tier 1 vs Tier 2 (generalizing the BT-909 is_function/2
                    // precedent from Erlang FFI interop to Beamtalk-level block
                    // calls), regardless of which method performs the call.
                    //
                    // Error: Field assignment in blocks stored into a *local
                    // variable* remains flagged here — codegen's
                    // `prescan_tier2_local_vars` can prove per-method that a
                    // specific local's later uses are safe, but this pass has no
                    // equivalent whole-body lookahead, so it stays conservative.
                    //
                    // BT-2797 (PR #2899 review fix): the field-stored exemption
                    // is only safe when the block does NOT also capture and
                    // mutate an outer local. `generate_block_stateful` reads a
                    // captured local's `'__local__<var>'` key from the calling
                    // method's `StateAcc` with a fallback to the value closed
                    // over at block-*definition* time — correct for a block
                    // invoked from the same method (BT-856), but wrong here: a
                    // field-stored block can be invoked from a *different*
                    // method whose `StateAcc` never had that key seeded, so the
                    // fallback silently returns the stale definition-time value
                    // forever, and the key then leaks into the actor's
                    // persistent gen_server state once the call site merges the
                    // returned `NewState` back in. Keep flagging the mixed case
                    // so it still hits the same compile-time diagnostic as a
                    // captured-local-only stored block.
                    let has_captured_local_mutations = mutations
                        .iter()
                        .any(|m| matches!(m.kind, MutationKind::CapturedVariable { .. }));
                    let is_field_stored =
                        matches!(parent_context, Some(ExprContext::FieldAssignment))
                            && !has_captured_local_mutations;
                    if matches!(context, BlockContext::Stored) && !is_field_stored {
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
                MutationKind::CapturedVariable { name } => {
                    // BT-856 (ADR 0041 Phase 3): Captured variable mutations in Stored/Passed
                    // blocks are supported via the Tier 2 stateful block protocol (BT-852).
                    // State is threaded through the *calling method's own* StateAcc map, so
                    // mutations propagate correctly back to that same method — this is valid
                    // and working behaviour, and needs no diagnostic in that case.
                    //
                    // BT-2809: that threading mechanism breaks down for a block stored into
                    // an instance field (`self.field := [block]`), because the field may be
                    // invoked from a *different* method than the one that assigned it (the
                    // whole point of BT-2797). `generate_block_stateful`'s captured-var
                    // codegen keys the mutation into `'__local__<var>'` on the *calling*
                    // method's own State — a different method's State was never seeded with
                    // that key, so the runtime fallback silently returns the value the
                    // variable had at block-*definition* time forever, and the stray key
                    // then leaks into the actor's persistent gen_server state once the call
                    // site merges the returned state back in. Unlike a field mutation (BT-2797
                    // made those cross-method-safe via runtime is_function/2 discrimination),
                    // a captured local has no cross-method-visible home to thread through, so
                    // this is flagged the same way a field-write-plus-captured-local mix
                    // already is above (`has_captured_local_mutations`) — just for the
                    // captured-local-only case that arm never reaches.
                    //
                    // Only fire for the captured-local-ONLY case: when a field write
                    // *also* exists in the same block, the Field arm above already emits
                    // its own diagnostic for this exact mix (that's what its
                    // `has_captured_local_mutations` check is for), so skip here to avoid
                    // reporting two separate errors for one closure.
                    let has_field_mutation = mutations
                        .iter()
                        .any(|m| matches!(m.kind, MutationKind::Field { .. }));
                    if matches!(parent_context, Some(ExprContext::FieldAssignment))
                        && !has_field_mutation
                    {
                        self.result.diagnostics.push(Diagnostic::error(
                            format!(
                                "cannot mutate captured variable '{name}' inside a closure stored in a field\n\
                                 \n\
                                 = help: a field-stored block may be invoked from a different method, \
                                   which has no way to see this variable's mutation\n\
                                 = help: use a field instead of a captured local variable: `self.{name} := ...`"
                            ),
                            mutation.span,
                        ).with_hint("Store the mutated value in a field, or extract the block and the variable into the same method"));
                    }
                }
                MutationKind::LocalVariable { .. } => {
                    // A block-local variable (defined AND used only inside the block, not
                    // captured from an outer scope) never escapes the block invocation —
                    // no state threading is needed, so no diagnostic applies in any context.
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
    fn bt2809_captured_local_only_field_stored_block_produces_error() {
        // BT-2809: a block stored in a field whose ONLY mutation is a captured
        // local (no field write) must be flagged — it can be invoked from a
        // different method whose State never seeded the captured variable's
        // '__local__' key, silently returning the stale definition-time value.
        let src = "Actor subclass: Ctr\n  state: total = 0\n  state: callback = nil\n\n  setup =>\n    count := 0\n    self.callback := [:n | count := count + n]\n\n  process: n =>\n    self.callback value: n\n";
        let module = parse_module(src);
        let result = analyse(&module);
        assert!(
            result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("cannot mutate captured variable")),
            "Expected captured-local-in-field-stored-block error, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn bt2809_mixed_field_and_captured_local_in_field_stored_block_produces_one_error() {
        // Code review follow-up: a field-stored block with BOTH a field write and
        // a captured-local mutation must produce exactly ONE diagnostic (the
        // Field arm's "cannot assign to field"), not two — the CapturedVariable
        // arm's own diagnostic is for the captured-local-ONLY case; firing both
        // for the same closure would be confusing, redundant double-reporting.
        let src = "Actor subclass: Ctr\n  state: total = 0\n\n  setup =>\n    count := 0\n    self.callback := [:n | self.total := n. count := count + n]\n";
        let module = parse_module(src);
        let result = analyse(&module);
        assert_eq!(
            result.diagnostics.len(),
            1,
            "Expected exactly one diagnostic for a field-stored block with both a \
             field write and a captured-local mutation, got: {:?}",
            result.diagnostics
        );
        assert!(
            result.diagnostics[0]
                .message
                .contains("cannot assign to field"),
            "Expected the field-write diagnostic to be the one reported, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn captured_local_only_stored_in_local_var_same_method_produces_no_error() {
        // BT-856/BT-2809 sanity check: a block stored in a *local variable* (not
        // a field) with only a captured-local mutation must remain undiagnosed —
        // codegen's prescan_tier2_local_vars proves per-method safety for this
        // shape (BT-2797), and it's the well-established BT-856 pattern.
        let src = "Object subclass: Foo\n  bar =>\n    count := 0\n    blk := [:n | count := count + n]\n    blk value: 5";
        let module = parse_module(src);
        let result = analyse(&module);
        assert!(
            !result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("cannot mutate captured variable")),
            "Did not expect a captured-local error for a locally-stored block, got: {:?}",
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

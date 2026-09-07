// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared state-threading utilities used across loop/fold/conditional codegen:
//! the `__local__` state-map key convention, rebinding threaded locals from a
//! `StateAcc` map, and computing which locals a loop must thread.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-3459: split out of `control_flow/mod.rs`, no logic changes.

use super::super::{CodeGenContext, CoreErlangGenerator, block_analysis};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Expression;

impl CoreErlangGenerator {
    /// BT-598: Returns the state map key for a local variable.
    /// Uses a `__local__` prefix to prevent collision with actor field names.
    pub(in crate::core_erlang) fn local_state_key(var_name: &str) -> String {
        format!("__local__{var_name}")
    }

    /// ADR 0111 Addendum 5 (BT-1213/BT-2355/BT-3173 rebind idiom): rebinds
    /// each of `vars` — a nested control-flow construct's threaded
    /// `__local__` captured vars — from `state_var`, returning one `let V =
    /// maps:get(...) in` `Document` per var and updating each var's own
    /// Core Erlang binding via `bind_var` so later code (in whatever form
    /// the caller assembles) sees the rebound value rather than the stale
    /// pre-statement one.
    ///
    /// Shared leaf helper for two call sites that both need this same
    /// rebind after unpacking a nested construct's `{Result, NewState}`
    /// result, differing only in how they wrap the returned `Document`s:
    /// `conditionals.rs`'s `push_control_flow_threaded_var_rereads` (the
    /// `ThreadedIr`-rendered conditional-arm path, wraps as one
    /// `ThreadedStmt::Statement`) and this module's
    /// `generate_threaded_loop_body_inner` (the foldl loop-body path,
    /// pushes directly onto its `docs` vec).
    pub(super) fn rebind_threaded_vars_from_state(
        &mut self,
        vars: &[String],
        state_var: &str,
    ) -> Vec<Document<'static>> {
        let mut docs = Vec::new();
        for var in vars {
            let core_var = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            docs.push(docvec![
                "let ",
                leaf::var(core_var.clone()),
                " = call 'maps':'get'(",
                leaf::atom(Self::local_state_key(var)),
                ", ",
                leaf::var(state_var.to_string()),
                ") in ",
            ]);
            self.bind_var(var, &core_var);
        }
        docs
    }

    /// BT-598/BT-1053: Compute local variables that need threading through a loop's `StateAcc`.
    ///
    /// For actor methods: returns vars that are both read and written in the block
    /// (excluding block parameters). Reads from an optional condition block are merged.
    ///
    /// For value-type methods (BT-1053): returns vars that are captured from the outer
    /// scope AND written in the block. Using `captured_reads` (not all reads) avoids
    /// threading block-internal temporaries that happen to be read+written within the block.
    ///
    /// Returns empty for REPL mode (handled separately) and other contexts.
    pub(in crate::core_erlang) fn compute_threaded_locals_for_loop(
        &self,
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
    ) -> Vec<String> {
        if self.is_repl_mode() {
            return Vec::new();
        }

        let analysis = block_analysis::analyze_block(body);
        let block_params: std::collections::HashSet<String> =
            body.parameters.iter().map(|p| p.name.to_string()).collect();

        // BT-1329: Include variables captured and mutated by nested list op blocks.
        // `analyze_block` doesn't propagate local_writes from nested (non-conditional) blocks,
        // so variables mutated inside `do:`, `collect:`, `inject:into:`, `select:`, `reject:`
        // blocks are invisible to the outer loop's threaded_locals computation.
        // Scan the body for list op message sends and include their cross-scope mutations.
        let mut list_op_cross_scope_writes = std::collections::HashSet::new();
        for stmt in &body.body {
            Self::collect_list_op_cross_scope_mutations_recursive(
                &stmt.expression,
                &self.semantic_facts,
                &mut list_op_cross_scope_writes,
            );
        }
        // BT-2363: also thread write-only outer locals mutated inside nested counted/list-op
        // loops (those that the read+write `collect_list_op_cross_scope_mutations` misses).
        for stmt in &body.body {
            self.collect_nested_loop_outer_local_writes(
                &stmt.expression,
                &block_params,
                &mut list_op_cross_scope_writes,
            );
        }

        match self.context {
            CodeGenContext::Actor => {
                // BT-1224: Only thread vars captured from the outer scope that are also
                // written in the block. Using `captured_reads` (not `local_reads`) excludes
                // block-internal temporaries that are first defined then read within the block.
                // Using `local_reads` caused unbound_var errors in dispatch/4 because packing
                // code tried to reference unbound Core Erlang variables (e.g. `Y`) that only
                // exist inside the lambda, not in the outer dispatch/4 function.
                let mut all_captured_reads = analysis.captured_reads.clone();
                let mut all_writes = analysis.local_writes.clone();
                if let Some(Expression::Block(cond_block)) = condition {
                    let cond_analysis = block_analysis::analyze_block(cond_block);
                    all_captured_reads = all_captured_reads
                        .union(&cond_analysis.captured_reads)
                        .cloned()
                        .collect();
                    // BT-1224: Also include writes from the condition block so that
                    // variables first written in a condition are included in threading.
                    all_writes = all_writes
                        .union(&cond_analysis.local_writes)
                        .cloned()
                        .collect();
                }
                // BT-1329: Add cross-scope list op mutations to both reads and writes.
                // These vars are both read and written in the nested block, so they need
                // threading through the outer loop.
                all_captured_reads = all_captured_reads
                    .union(&list_op_cross_scope_writes)
                    .cloned()
                    .collect();
                all_writes = all_writes
                    .union(&list_op_cross_scope_writes)
                    .cloned()
                    .collect();
                // BT-1329: Also include outer-scope variables that are written in the loop
                // body but not read (write-only). These variables need their final value
                // to escape the loop via StateAcc. We detect them by checking if the
                // variable already has a binding in the generator's scope (meaning it was
                // defined before the loop in the method body).
                for v in &all_writes {
                    if !block_params.contains(v.as_str())
                        && !all_captured_reads.contains(v)
                        && self.lookup_var(v).is_some()
                    {
                        all_captured_reads.insert(v.clone());
                    }
                }
                all_captured_reads
                    .intersection(&all_writes)
                    .filter(|v| !block_params.contains(*v))
                    .cloned()
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect()
            }
            CodeGenContext::ValueType => {
                // BT-1053: Only thread vars captured from the outer scope that are also
                // written in the block. `captured_reads` excludes block-internal temps.
                let mut captured = analysis.captured_reads.clone();
                let mut writes = analysis.local_writes.clone();
                captured = captured
                    .union(&list_op_cross_scope_writes)
                    .cloned()
                    .collect();
                writes = writes.union(&list_op_cross_scope_writes).cloned().collect();
                // BT-1329: Include outer-scope write-only variables (same as Actor above).
                for v in &writes {
                    if !block_params.contains(v.as_str())
                        && !captured.contains(v)
                        && self.lookup_var(v).is_some()
                    {
                        captured.insert(v.clone());
                    }
                }
                captured
                    .intersection(&writes)
                    .filter(|v| !block_params.contains(*v))
                    .cloned()
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect()
            }
            CodeGenContext::Repl => Vec::new(),
        }
    }
}

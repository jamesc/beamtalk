// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 §6 "One predicate, in `beamtalk-core`" (BT-3746): over the
//! `class_var_program` corpus, every threaded set codegen's
//! `threaded_locals_of` computes in its own scope (`lookup_var`) is the set
//! the `beamtalk-core` diagnostic pass's recognizer
//! (`local_threading_construct` + `construct_outer_local_writes`) gives the
//! same construct in source scope.
//!
//! The source scope here is an independent walk (method parameters, block
//! parameters and first assignments, innermost frame last), so a scope that
//! codegen tracks differently from the source shows up as a disagreement.
//!
//! What it does not check yet: that every Tier 2 argument the §6 pass
//! accepts lowers to a producer and every one it rejects does not. That
//! needs the producer (ADR 0131 phase 2, BT-3749).

use crate::core_erlang::threading_analysis::recorded_sets::{Record, recording};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{Block, Expression, ExpressionStatement};
use beamtalk_core::semantic_analysis::block_facts::{
    construct_outer_local_writes, local_threading_construct,
};
use beamtalk_core::source_analysis::{Span, lex_with_eof, parse};
use beamtalk_core::test_helpers::class_var_program::{Shapes, Spelling, gen_program};
use std::collections::{HashMap, HashSet};

/// The source-scope threaded set of every construct in a method body.
struct SourceSets {
    frames: Vec<HashSet<String>>,
    sets: HashMap<Span, Vec<String>>,
}

impl SourceSets {
    fn method(params: Vec<String>, body: &[ExpressionStatement]) -> HashMap<Span, Vec<String>> {
        let mut walk = SourceSets {
            frames: vec![params.into_iter().collect()],
            sets: HashMap::new(),
        };
        walk.body(body);
        walk.sets
    }

    fn bound(&self, name: &str) -> bool {
        self.frames.iter().any(|f| f.contains(name))
    }

    fn define(&mut self, name: &str) {
        if !self.bound(name) {
            if let Some(frame) = self.frames.last_mut() {
                frame.insert(name.to_string());
            }
        }
    }

    fn body(&mut self, body: &[ExpressionStatement]) {
        for stmt in body {
            self.expr(&stmt.expression);
        }
    }

    fn block(&mut self, block: &Block) {
        self.frames.push(
            block
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect(),
        );
        self.body(&block.body);
        self.frames.pop();
    }

    fn expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Assignment { target, value, .. } => {
                self.expr(value);
                if let Expression::Identifier(id) = target.as_ref() {
                    self.define(&id.name);
                } else {
                    self.expr(target);
                }
            }
            Expression::Block(block) => self.block(block),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                if let Some(construct) = local_threading_construct(expr) {
                    let mut names: Vec<String> =
                        construct_outer_local_writes(&construct, &|n| self.bound(n))
                            .into_iter()
                            .map(|w| w.name.to_string())
                            .collect();
                    names.sort();
                    self.sets.insert(expr.span(), names);
                }
                self.expr(receiver);
                for arg in arguments {
                    self.expr(arg);
                }
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                self.expr(receiver);
                for message in messages {
                    for arg in &message.arguments {
                        self.expr(arg);
                    }
                }
            }
            Expression::Parenthesized { expression, .. } => self.expr(expression),
            Expression::Return { value, .. } => self.expr(value),
            Expression::FieldAccess { receiver, .. } => self.expr(receiver),
            Expression::ArrayLiteral { elements, .. } => {
                for e in elements {
                    self.expr(e);
                }
            }
            _ => {}
        }
    }
}

/// Codegen's records for one class source, checked against the source
/// sets. Returns how many non-empty sets agreed, or `None` when codegen
/// rejected the program.
fn check_class(name: &str, source: &str) -> Result<Option<usize>, String> {
    let (module, diagnostics) = parse(lex_with_eof(source));
    assert!(diagnostics.is_empty(), "{source}: {diagnostics:?}");
    let mut source_sets: HashMap<Span, Vec<String>> = HashMap::new();
    for class in &module.classes {
        for method in class.methods.iter().chain(&class.class_methods) {
            let params = method
                .parameters
                .iter()
                .map(|p| p.name.name.to_string())
                .collect();
            source_sets.extend(SourceSets::method(params, &method.body));
        }
    }
    let (generated, records): (_, Vec<Record>) =
        recording(|| generate_module_with_warnings(&module, CodegenOptions::new(name)));
    if generated.is_err() {
        return Ok(None);
    }
    let mut agreed = 0;
    for (span, codegen_set) in records {
        let codegen_set = codegen_set.unwrap_or_default();
        // Not a construct for the core recognizer either (a Tier 2 `value`
        // call or a non-construct expression): nothing to compare.
        let Some(source_set) = source_sets.get(&span) else {
            if codegen_set.is_empty() {
                continue;
            }
            return Err(format!(
                "codegen threads {codegen_set:?} at {span:?}, which the core recognizer \
                 does not see as a construct\n{source}"
            ));
        };
        if &codegen_set != source_set {
            return Err(format!(
                "at {span:?} codegen threads {codegen_set:?} but the core recognizer \
                 threads {source_set:?}\n{source}"
            ));
        }
        if !codegen_set.is_empty() {
            agreed += 1;
        }
    }
    Ok(Some(agreed))
}

#[test]
fn threaded_locals_of_agrees_with_the_core_recognizer_over_the_class_var_corpus() {
    let mut agreed = 0;
    let mut compiled = 0;
    for seed in 0..96u64 {
        let size = 1 + u32::try_from(seed % 3).unwrap_or(0);
        let program = gen_program(seed, size, Shapes::all());
        for spelling in [Spelling::Open, Spelling::Sealed] {
            for class in program.render(0, spelling) {
                match check_class(&class.name, &class.source) {
                    Ok(Some(n)) => {
                        compiled += 1;
                        agreed += n;
                    }
                    Ok(None) => {}
                    Err(e) => panic!("seed {seed} size {size} {spelling:?}: {e}"),
                }
            }
        }
    }
    // Not vacuous: the corpus (with `local_touch`) writes method locals
    // inside loops, folds and protected blocks.
    assert!(compiled > 50, "only {compiled} classes compiled");
    assert!(
        agreed > 20,
        "only {agreed} non-empty threaded sets compared"
    );
}

#[test]
fn source_sets_walk_sees_assignment_order() {
    // Sanity check of the oracle itself: `t` is bound before the loop, `u`
    // only inside it.
    let src = "Object subclass: P\n  class m => \n    t := 0\n    #(1) do: [:x | t := x. u := x]\n    t\n";
    let (module, _) = parse(lex_with_eof(src));
    let method = &module.classes[0].class_methods[0];
    let sets = SourceSets::method(Vec::new(), &method.body);
    assert_eq!(
        sets.values().collect::<Vec<_>>(),
        vec![&vec!["t".to_string()]]
    );
}

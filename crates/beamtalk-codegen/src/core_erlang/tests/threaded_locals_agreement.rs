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
//! It also checks the set today's lowering packs (`ThreadedLocals::lowered`,
//! the only set that reaches the generated code), per construct: the packing
//! side (the loop, fold, conditional and exception generators) and the
//! unpacking side (the sequencers reading `lowered_threaded_locals_of`) must
//! select the same blocks of the construct and compute the same set. Records
//! are keyed by the construct that owns the blocks (from the source), so a
//! block-selection drift between the two sides is compared, not filed under
//! two keys.
//!
//! `class_var_program` generates class methods only, so it never produces a
//! value-type or actor loop, nor a stateful `whileTrue:` condition (a
//! condition that sends to `self` outside a class method).
//! [`HAND_WRITTEN`] adds those shapes in every method context.
//!
//! What it does not check yet: that every Tier 2 argument the §6 pass
//! accepts lowers to a producer and every one it rejects does not. That
//! needs the producer (ADR 0131 phase 2, BT-3749).

use crate::core_erlang::threading_analysis::recorded_sets::{Side, recording};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{Block, Expression, ExpressionStatement};
use beamtalk_core::semantic_analysis::block_facts::{
    construct_outer_local_writes, local_threading_construct,
};
use beamtalk_core::source_analysis::{Span, lex_with_eof, parse};
use beamtalk_core::test_helpers::class_var_program::{Shapes, Spelling, gen_program};
use std::collections::{HashMap, HashSet};

/// The source-scope threaded set of every construct in a method body, and
/// which construct owns each construct block.
struct SourceSets {
    frames: Vec<HashSet<String>>,
    sets: HashMap<Span, Vec<String>>,
    /// Construct block span -> the span of the send it belongs to.
    owners: HashMap<Span, Span>,
}

impl SourceSets {
    fn method(params: Vec<String>, body: &[ExpressionStatement]) -> Self {
        let mut walk = SourceSets {
            frames: vec![params.into_iter().collect()],
            sets: HashMap::new(),
            owners: HashMap::new(),
        };
        walk.body(body);
        walk
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
                    for block in &construct.blocks {
                        self.owners.insert(block.span, expr.span());
                    }
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

/// One construct's lowered-set record: the blocks selected, the set, and
/// which sides (`true`: packing) computed it.
type ConstructRecord = (Vec<Span>, Vec<String>, HashSet<bool>);

/// How many comparisons one class contributed.
#[derive(Default)]
struct Agreed {
    /// Non-empty `names` sets that matched the core recognizer.
    names: usize,
    /// Constructs whose non-empty lowered set both the packing and the
    /// unpacking side computed, with the same blocks and the same set.
    lowered: usize,
    /// Per construct: how many blocks were selected, and the lowered set.
    selections: Vec<(usize, Vec<String>)>,
}

/// Codegen's records for one class source, checked against the source
/// sets. `None` when codegen rejected the program.
fn check_class(name: &str, source: &str) -> Result<Option<Agreed>, String> {
    let (module, diagnostics) = parse(lex_with_eof(source));
    assert!(diagnostics.is_empty(), "{source}: {diagnostics:?}");
    let mut source_sets: HashMap<Span, Vec<String>> = HashMap::new();
    let mut owners: HashMap<Span, Span> = HashMap::new();
    for class in &module.classes {
        for method in class.methods.iter().chain(&class.class_methods) {
            let params = method
                .parameters
                .iter()
                .map(|p| p.name.name.to_string())
                .collect();
            let walk = SourceSets::method(params, &method.body);
            source_sets.extend(walk.sets);
            owners.extend(walk.owners);
        }
    }
    let (generated, records) =
        recording(|| generate_module_with_warnings(&module, CodegenOptions::new(name)));
    if generated.is_err() {
        return Ok(None);
    }
    let mut agreed = Agreed::default();
    for (span, codegen_set) in records.names {
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
            agreed.names += 1;
        }
    }
    // Per construct: the first record, and which sides recorded it.
    let mut lowered: HashMap<Span, ConstructRecord> = HashMap::new();
    for (side, blocks, set) in records.lowered {
        let Some(construct) = blocks.first().and_then(|b| owners.get(b)).copied() else {
            continue;
        };
        let entry = lowered
            .entry(construct)
            .or_insert_with(|| (blocks.clone(), set.clone(), HashSet::new()));
        if entry.0 != blocks || entry.1 != set {
            return Err(format!(
                "for the construct at {construct:?} codegen selected {:?} packing {:?} once, \
                 and {blocks:?} packing {set:?} on the {side:?} side\n{source}",
                entry.0, entry.1
            ));
        }
        entry.2.insert(side == Side::Pack);
    }
    agreed.selections = lowered
        .values()
        .map(|(blocks, set, _)| (blocks.len(), set.clone()))
        .collect();
    agreed.lowered = lowered
        .values()
        .filter(|(_, set, sides)| !set.is_empty() && sides.len() == 2)
        .count();
    Ok(Some(agreed))
}

#[test]
fn threaded_locals_of_agrees_with_the_core_recognizer_over_the_class_var_corpus() {
    let mut agreed = Agreed::default();
    let mut compiled = 0;
    for seed in 0..96u64 {
        let size = 1 + u32::try_from(seed % 3).unwrap_or(0);
        let program = gen_program(seed, size, Shapes::all());
        for spelling in [Spelling::Open, Spelling::Sealed] {
            for class in program.render(0, spelling) {
                match check_class(&class.name, &class.source) {
                    Ok(Some(n)) => {
                        compiled += 1;
                        agreed.names += n.names;
                        agreed.lowered += n.lowered;
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
        agreed.names > 20,
        "only {} non-empty threaded sets compared",
        agreed.names
    );
    assert!(
        agreed.lowered > 20,
        "only {} lowered sets computed on both the packing and unpacking side",
        agreed.lowered
    );
}

/// Hand-written shapes the generated corpus does not produce: loops,
/// folds, conditionals and protected blocks writing method locals in
/// value-type, actor and class methods, including a stateful `whileTrue:`
/// condition (BT-3746 review: a value-type condition that sends to `self`
/// is packed by the stateful-condition lowering).
const HAND_WRITTEN: &[(&str, &str)] = &[
    (
        "bt@hw_value",
        "Value subclass: HwValue
  check: n => n < 3

  stateful =>
    t := 0
    u := 0
    [
      t := t + 1
      self check: t
    ] whileTrue: [u := u + 1]
    #[t, u]

  plain =>
    t := 0
    u := 0
    [
      t := t + 1
      t < 3
    ] whileTrue: [u := u + 1]
    #[t, u]

  nested =>
    s := 0
    1 to: 3 do: [:i | #(1, 2) do: [:x | s := s + x]]
    s

  cond: f =>
    s := 0
    f ifTrue: [#(1, 2) do: [:x | s := s + x]] ifFalse: [s := 9]
    s

  guarded =>
    s := 0
    [#(1, 2) do: [:x | s := s + x]] ensure: [nil]
    s
",
    ),
    (
        "bt@hw_actor",
        "Actor subclass: HwActor
  check: n => n < 3

  stateful =>
    t := 0
    u := 0
    [
      t := t + 1
      self check: t
    ] whileTrue: [u := u + 1]
    #[t, u]

  nested =>
    s := 0
    1 to: 3 do: [:i | #(1, 2) do: [:x | s := s + x]]
    s

  each =>
    s := 0
    #(4, 5) eachWithIndex: [:x :i | s := s + i]
    s
",
    ),
    (
        "bt@hw_class",
        "Object subclass: HwClass
  class plain =>
    t := 0
    u := 0
    [
      t := t + 1
      t < 3
    ] whileTrue: [u := u + 1]
    #[t, u]

  class nested =>
    s := 0
    1 to: 3 do: [:i | #(1, 2) do: [:x | s := s + x]]
    s
",
    ),
];

#[test]
fn threaded_locals_of_agrees_on_hand_written_shapes_in_every_context() {
    let mut both_sides = 0;
    for (name, source) in HAND_WRITTEN {
        match check_class(name, source) {
            Ok(Some(n)) => {
                both_sides += n.lowered;
                // Every stateful `whileTrue:` (value type and actor) packs its
                // condition's write: two blocks, `t` and `u`.
                if name != &"bt@hw_class" {
                    assert!(
                        n.selections
                            .contains(&(2, vec!["t".to_string(), "u".to_string()])),
                        "{name}: the stateful whileTrue: condition is not packed: {:?}",
                        n.selections
                    );
                }
            }
            Ok(None) => panic!("{name}: codegen rejected the program"),
            Err(e) => panic!("{name}: {e}"),
        }
    }
    // Value-type and class-method loops read their result back through the
    // same `loop_threaded_locals` the generator packs with, so only the
    // actor constructs are seen from both sides here.
    assert!(
        both_sides >= 3,
        "only {both_sides} constructs compared on both sides"
    );
}

#[test]
fn source_sets_walk_sees_assignment_order() {
    // Sanity check of the oracle itself: `t` is bound before the loop, `u`
    // only inside it.
    let src = "Object subclass: P\n  class m => \n    t := 0\n    #(1) do: [:x | t := x. u := x]\n    t\n";
    let (module, _) = parse(lex_with_eof(src));
    let method = &module.classes[0].class_methods[0];
    let sets = SourceSets::method(Vec::new(), &method.body).sets;
    assert_eq!(
        sets.values().collect::<Vec<_>>(),
        vec![&vec!["t".to_string()]]
    );
}

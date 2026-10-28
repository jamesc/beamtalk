// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Send-operand parenthesis normalisation (BT-4059).
//!
//! **DDD Context:** Source Analysis
//!
//! A redundant pair of parentheses around a send operand means nothing: the
//! tree already encodes precedence, so `(e) foo`, `x + (e)` and
//! `x at: (e)` are the same program as `e foo`, `x + e` and `x at: e`. The
//! parser nonetheless keeps the `( … )` as an [`Expression::Parenthesized`]
//! node (the formatter and the LSP need it), and every analysis or codegen
//! check that matched an operand without peeling it sent the parenthesised
//! form down a different, sometimes wrong, path.
//!
//! [`peel_operand_parens`] is the one place that removes those wrappers, so
//! analysis and codegen see a single canonical tree. It strips **every**
//! `Parenthesized` layer from:
//!
//! - a message send's receiver and each of its arguments, and
//! - each cascade message's arguments.
//!
//! A cascade's *shared* receiver is the receiver of its first (folded)
//! message, so it is a send receiver and peeled as such. The cascade's own
//! `receiver` field — that folded first send — is **not** an operand and is
//! left alone: `(a foo); bar` cascades onto the *result* of `a foo`, whereas
//! `a foo; bar` cascades onto `a`, so removing that wrapper would change the
//! program.
//!
//! Two further positions are deliberately untouched:
//!
//! - a `super` receiver: `(super) foo` is an ordinary send to the value of
//!   `super`, not a super send, so a wrapper whose innermost node is `super`
//!   is kept whole;
//! - statements, assignment right-hand sides and every other non-operand
//!   position: the parenthesised-statement rule of BT-3838 reads the
//!   statement-level wrapper (BT-4092 owns that decision).
//!
//! Removing a wrapper never changes a span of the node it wrapped, so
//! diagnostics and type-map entries keyed on the inner node land on the inner
//! span (narrower by the parentheses).
//!
//! # Recording and restoring
//!
//! The parentheses are meaningless to compiled code but not to the person who
//! wrote them: a method's reflected source (`methodSource`, xref, the IDE
//! method editor) is the text as written, which the workspace compares with
//! the file on disk. [`peel_operand_parens_recording`] therefore returns a
//! [`PeeledParens`] — for each peeled operand, keyed by its reflection unit
//! ([`PeelScope`]: the enclosing method, or the module level) and the inner
//! node's span, the wrapper layers removed from it, each with its own span —
//! and [`restore_operand_parens`] puts them back on a clone, so
//! `unparse_method` of the restored method is `unparse_method` of the method
//! as parsed. Peel and restore visit operands through the one shared walk
//! (`visit_operands_mut`), so they agree on every position, the skipped ones
//! included.
//!
//! The scope matters because a flattened module holds protocol provisions
//! spliced in from other files, whose byte offsets can coincide with the
//! class file's: keyed by span alone, a restore could wrap an operand that
//! was never parenthesised. Keyed by (method span, inner span), a collision
//! needs both to coincide across files.
//!
//! The pass runs where analysis and codegen take ownership of a module
//! (`analyse_full` on its own clone; `generate_module_with_warnings` and the
//! REPL codegen entries, recording, on theirs). The formatter, the LSP, the raw
//! parse and the module a driver hands to codegen keep the parenthesised tree.
//! It is idempotent.

use std::collections::HashMap;
use std::collections::hash_map::Entry;

use super::{Expression, MethodDefinition, Module};
use crate::ast_walker::{walk_expression, walk_expression_mut};
use crate::span::Span;

/// The reflection unit a peeled operand belongs to: the method whose body
/// holds it (identified by the method's span), or the module level — module
/// statements (and so `ClassBuilder` block methods outside any method), the
/// REPL's expressions and variable initialisers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PeelScope {
    /// Outside any method.
    Module,
    /// Inside the method with this span.
    Method(Span),
}

impl PeelScope {
    /// The scope of `method`'s body.
    #[must_use]
    pub fn of_method(method: &MethodDefinition) -> Self {
        Self::Method(method.span)
    }
}

/// The operand parentheses a [`peel_operand_parens_recording`] run removed:
/// for each peeled operand, keyed by its [`PeelScope`] and the span of the
/// node left in its place, the `Parenthesized` spans that wrapped it,
/// innermost first.
///
/// [`restore_operand_parens`] re-wraps those operands, so a method's text as
/// written can be recovered from the peeled tree codegen compiles.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PeeledParens {
    layers: HashMap<(PeelScope, Span), Vec<Span>>,
}

impl PeeledParens {
    /// `true` if no operand wrapper was recorded: restoring changes nothing.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.layers.is_empty()
    }

    /// The number of peeled operands recorded.
    #[must_use]
    pub fn len(&self) -> usize {
        self.layers.len()
    }

    /// Adds the operands `other` recorded (a second peel of more of the same
    /// tree, e.g. provisions flattened in after the first). An entry already
    /// recorded is kept, never replaced (see [`Self::record`]).
    pub fn extend(&mut self, other: PeeledParens) {
        for (key, layers) in other.layers {
            self.record(key, layers);
        }
    }

    /// Records `layers` for `key`, never overwriting an existing entry. Within
    /// one scope two operands share an inner span only when they are the same
    /// node (e.g. a protocol's provision and its same-file flattened copy), so
    /// their layers agree; should they ever differ, the first record wins
    /// rather than one operand's wrappers silently replacing another's.
    fn record(&mut self, key: (PeelScope, Span), layers: Vec<Span>) {
        if let Entry::Vacant(slot) = self.layers.entry(key) {
            slot.insert(layers);
        }
    }

    fn layers_of(&self, scope: PeelScope, inner: Span) -> Option<&Vec<Span>> {
        self.layers.get(&(scope, inner))
    }
}

/// Calls `visit` on every send-operand position of `expr`'s tree, in the walk's
/// pre-order: a node's operands are visited before the walk descends into
/// them. The one definition of "operand position" for peel, restore and the
/// `has_*` scans; `$m` is `mut` for the mutable walk.
macro_rules! visit_operands_with {
    ($walk:path, $expr:expr, $visit:expr $(, $m:tt)?) => {
        $walk($expr, &mut |node| match node {
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                $visit(&$($m)? **receiver);
                for argument in arguments {
                    $visit(argument);
                }
            }
            // The cascade's `receiver` is the folded first send, not an
            // operand (its own receiver is reached as that send's operand).
            Expression::Cascade { messages, .. } => {
                for message in messages {
                    for argument in &$($m)? message.arguments {
                        $visit(argument);
                    }
                }
            }
            _ => {}
        })
    };
}

/// The shared-reference operand walk (see [`visit_operands_with`]).
fn visit_operands(expr: &Expression, visit: &mut dyn FnMut(&Expression)) {
    visit_operands_with!(walk_expression, expr, visit);
}

/// The mutable operand walk (see [`visit_operands_with`]) peel and restore
/// share. A visit may replace the operand; the walk then descends into the
/// replacement.
fn visit_operands_mut(expr: &mut Expression, visit: &mut dyn FnMut(&mut Expression)) {
    visit_operands_with!(walk_expression_mut, expr, visit, mut);
}

/// Strips redundant parentheses from every send operand in `module`.
///
/// See the module documentation for exactly which positions are peeled.
/// Idempotent: a second run changes nothing.
pub fn peel_operand_parens(module: &mut Module) {
    let _ = peel_operand_parens_recording(module);
}

/// [`peel_operand_parens`], returning what it removed so
/// [`restore_operand_parens`] can put it back.
pub fn peel_operand_parens_recording(module: &mut Module) -> PeeledParens {
    let mut record = PeeledParens::default();
    for_each_root_mut(module, &mut |scope, root| {
        peel_in_scope(root, scope, &mut record);
    });
    record
}

/// Strips redundant parentheses from every send operand in `expr`'s tree
/// (including nested blocks), leaving `expr` itself as it is, and adds what
/// it removed to `record`.
///
/// The expression-level entry of [`peel_operand_parens_recording`], for
/// callers that hold module-level expressions rather than a module (the
/// REPL): recorded under [`PeelScope::Module`].
pub fn peel_operand_parens_expr_recording(expr: &mut Expression, record: &mut PeeledParens) {
    peel_in_scope(expr, PeelScope::Module, record);
}

fn peel_in_scope(expr: &mut Expression, scope: PeelScope, record: &mut PeeledParens) {
    visit_operands_mut(expr, &mut |operand| peel_operand(operand, scope, record));
}

/// Puts back, in `method`'s body, the operand parentheses `peeled` recorded
/// removing from it (its own [`PeelScope::of_method`] scope): the inverse of
/// the peel on the method's tree, so `unparse_method` of the result is the
/// method's text as written. A no-op when nothing was recorded. Idempotent.
pub fn restore_operand_parens(method: &mut MethodDefinition, peeled: &PeeledParens) {
    restore_operand_parens_in_scope(method, peeled, PeelScope::of_method(method));
}

/// [`restore_operand_parens`] from the entries of `scope` rather than the
/// method's own: for a method built from a block (a `ClassBuilder` block
/// method), whose operands were recorded in the scope the block sits in.
pub fn restore_operand_parens_in_scope(
    method: &mut MethodDefinition,
    peeled: &PeeledParens,
    scope: PeelScope,
) {
    if peeled.is_empty() {
        return;
    }
    for stmt in &mut method.body {
        visit_operands_mut(&mut stmt.expression, &mut |operand| {
            restore_operand(operand, scope, peeled);
        });
    }
}

/// `true` if [`peel_operand_parens`] would change `module`: a cheap scan so
/// callers holding a shared reference can skip cloning a module that is
/// already canonical.
#[must_use]
pub fn has_parenthesised_operand(module: &Module) -> bool {
    let mut found = false;
    for_each_root(module, &mut |_, root| {
        if !found {
            found = expr_has_parenthesised_operand(root);
        }
    });
    found
}

/// `true` if [`peel_operand_parens_expr_recording`] would change `expr`.
#[must_use]
pub fn expr_has_parenthesised_operand(expr: &Expression) -> bool {
    let mut found = false;
    visit_operands(expr, &mut |operand| found = found || is_peelable(operand));
    found
}

/// `true` if `operand` is a `Parenthesized` wrapper the pass removes: any
/// wrapper whose innermost node is not `super`.
fn is_peelable(operand: &Expression) -> bool {
    matches!(operand, Expression::Parenthesized { .. })
        && !matches!(operand.unwrap_parens(), Expression::Super(_))
}

/// A throwaway node to swap into a slot while its content is moved.
fn placeholder() -> Expression {
    Expression::Error {
        message: "".into(),
        span: Span::default(),
    }
}

/// Replaces `operand` by its innermost non-`Parenthesized` node, when
/// [`is_peelable`], recording the removed wrappers' spans in `record` under
/// `scope`.
fn peel_operand(operand: &mut Expression, scope: PeelScope, record: &mut PeeledParens) {
    if !is_peelable(operand) {
        return;
    }
    let mut layers = Vec::new();
    while let Expression::Parenthesized { expression, span } = operand {
        layers.push(*span);
        let inner = std::mem::replace(expression.as_mut(), placeholder());
        *operand = inner;
    }
    // Peeled outermost first; restored innermost first.
    layers.reverse();
    record.record((scope, operand.span()), layers);
}

/// Re-wraps `operand` in the layers recorded for its span in `scope`. A
/// `Parenthesized` operand was never peeled (a recorded node is the innermost
/// one), so it is left alone, which also makes a second restore a no-op.
fn restore_operand(operand: &mut Expression, scope: PeelScope, peeled: &PeeledParens) {
    if matches!(operand, Expression::Parenthesized { .. }) {
        return;
    }
    let Some(layers) = peeled.layers_of(scope, operand.span()) else {
        return;
    };
    for &span in layers {
        let inner = std::mem::replace(operand, placeholder());
        *operand = Expression::Parenthesized {
            expression: Box::new(inner),
            span,
        };
    }
}

/// Calls `f` on every root expression of `module`, with its [`PeelScope`]:
/// module-level statements and every instance- and class-variable initialiser
/// ([`PeelScope::Module`]), and every method body statement (class,
/// class-side, standalone and protocol provisions) under its method.
fn for_each_root(module: &Module, f: &mut dyn FnMut(PeelScope, &Expression)) {
    let method_body = |method: &MethodDefinition, f: &mut dyn FnMut(PeelScope, &Expression)| {
        let scope = PeelScope::of_method(method);
        for stmt in &method.body {
            f(scope, &stmt.expression);
        }
    };
    for stmt in &module.expressions {
        f(PeelScope::Module, &stmt.expression);
    }
    for class in &module.classes {
        for decl in class.state.iter().chain(&class.class_variables) {
            if let Some(default) = &decl.default_value {
                f(PeelScope::Module, default);
            }
        }
        for method in class.methods.iter().chain(&class.class_methods) {
            method_body(method, f);
        }
    }
    for standalone in &module.method_definitions {
        method_body(&standalone.method, f);
    }
    for protocol in &module.protocols {
        for method in &protocol.provided_methods {
            method_body(method, f);
        }
    }
}

/// The mutable counterpart of [`for_each_root`]; the two enumerate the same
/// roots with the same scopes, which
/// `roots_agree_between_shared_and_mutable_walks` asserts.
fn for_each_root_mut(module: &mut Module, f: &mut dyn FnMut(PeelScope, &mut Expression)) {
    let method_body = |method: &mut MethodDefinition,
                       f: &mut dyn FnMut(PeelScope, &mut Expression)| {
        let scope = PeelScope::of_method(method);
        for stmt in &mut method.body {
            f(scope, &mut stmt.expression);
        }
    };
    for stmt in &mut module.expressions {
        f(PeelScope::Module, &mut stmt.expression);
    }
    for class in &mut module.classes {
        for decl in class.state.iter_mut().chain(&mut class.class_variables) {
            if let Some(default) = &mut decl.default_value {
                f(PeelScope::Module, default);
            }
        }
        for method in class.methods.iter_mut().chain(&mut class.class_methods) {
            method_body(method, f);
        }
    }
    for standalone in &mut module.method_definitions {
        method_body(&mut standalone.method, f);
    }
    for protocol in &mut module.protocols {
        for method in &mut protocol.provided_methods {
            method_body(method, f);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_support::parse_bt;
    use crate::unparse::unparse_module;

    fn peeled(source: &str) -> Module {
        let mut module = parse_bt(source);
        peel_operand_parens(&mut module);
        module
    }

    fn first_expr(module: &Module) -> &Expression {
        &module.expressions[0].expression
    }

    #[test]
    fn peels_receivers_and_arguments_of_every_selector_shape() {
        for (src, expected) in [
            ("(self) foo", "self foo"),
            ("((C)) + 1", "C + 1"),
            ("(C) add: (3) to: ((4))", "C add: 3 to: 4"),
            ("(x) + (y)", "x + y"),
            ("#(1, 2) do: ([:k | k])", "#(1, 2) do: [:k | k]"),
        ] {
            let module = peeled(src);
            assert!(!has_parenthesised_operand(&module), "{src}");
            assert_eq!(unparse_module(&module).trim(), expected, "{src}");
        }
    }

    #[test]
    fn peels_cascade_target_and_message_arguments_but_not_the_folded_first_send() {
        let module = peeled("(x) foo: (1); bar: (2)");
        assert_eq!(unparse_module(&module).trim(), "x foo: 1; bar: 2");

        // `(a foo); bar` cascades onto the result of `a foo`, so the wrapper
        // around the folded first send stays.
        let module = peeled("(a foo); bar");
        let Expression::Cascade { receiver, .. } = first_expr(&module) else {
            panic!("expected a cascade");
        };
        assert!(matches!(
            receiver.as_ref(),
            Expression::Parenthesized { .. }
        ));
    }

    #[test]
    fn keeps_a_parenthesised_super_receiver() {
        let module = peeled("Object subclass: A\n  foo => ((super)) foo\n");
        let body = &module.classes[0].methods[0].body;
        let Expression::MessageSend { receiver, .. } = &body[0].expression else {
            panic!("expected a send");
        };
        assert!(matches!(
            receiver.as_ref(),
            Expression::Parenthesized { .. }
        ));
        assert!(!has_parenthesised_operand(&module));
    }

    #[test]
    fn leaves_statements_and_assignment_values_untouched() {
        let module = peeled("(x foo)\ny := (3 + (4))\n^ (z)");
        let [s0, s1, s2] = module.expressions.as_slice() else {
            panic!("expected three statements");
        };
        // A parenthesised statement keeps its wrapper; its own send operands
        // are peeled.
        assert!(matches!(s0.expression, Expression::Parenthesized { .. }));
        let Expression::Assignment { value, .. } = &s1.expression else {
            panic!("expected an assignment");
        };
        let Expression::Parenthesized { expression, .. } = value.as_ref() else {
            panic!("assignment value keeps its wrapper");
        };
        let Expression::MessageSend { arguments, .. } = expression.as_ref() else {
            panic!("expected a send");
        };
        assert!(matches!(arguments[0], Expression::Literal(..)));
        let Expression::Return { value, .. } = &s2.expression else {
            panic!("expected a return");
        };
        assert!(matches!(value.as_ref(), Expression::Parenthesized { .. }));
    }

    #[test]
    fn keeps_the_inner_span() {
        let module = peeled("(x) foo");
        let Expression::MessageSend { receiver, .. } = first_expr(&module) else {
            panic!("expected a send");
        };
        assert_eq!(receiver.span().start(), 1);
        assert_eq!(receiver.span().end(), 2);
    }

    #[test]
    fn reaches_blocks_initialisers_match_arms_and_every_method_kind() {
        let source = "Object subclass: A
  state: s = (1) + (2)
  classState: c = (3) max: (4)
  foo => [:b | (b) + 1] value: 2
  bar => 3 match: [n -> (n) + 1]
  class baz => (self) foo

A >> qux => (self) bar

Protocol define: P
  pro => (self) foo
";
        let mut module = parse_bt(source);
        assert!(has_parenthesised_operand(&module));
        peel_operand_parens(&mut module);
        assert!(!has_parenthesised_operand(&module), "{module:#?}");
    }

    #[test]
    fn is_idempotent() {
        let source = "Object subclass: A
  foo => ((self) bar: ((1) + (2))) baz; qux: ((3))
  bar => (super) foo
";
        let once = peeled(source);
        let mut twice = once.clone();
        peel_operand_parens(&mut twice);
        assert_eq!(once, twice);
    }

    #[test]
    fn has_parenthesised_operand_is_false_for_canonical_modules() {
        assert!(!has_parenthesised_operand(&parse_bt(
            "x foo\n(y bar)\nz := (3)"
        )));
        assert!(has_parenthesised_operand(&parse_bt("x foo: (y)")));
    }

    /// The methods of `source`'s first class, peeled with a record, each
    /// restored from it, next to the raw parse's.
    fn restored_and_raw(source: &str) -> Vec<(MethodDefinition, MethodDefinition)> {
        let raw = parse_bt(source);
        let mut peeled = raw.clone();
        let record = peel_operand_parens_recording(&mut peeled);
        assert!(!has_parenthesised_operand(&peeled));
        peeled.classes[0]
            .methods
            .iter()
            .zip(&raw.classes[0].methods)
            .map(|(method, raw_method)| {
                let mut restored = method.clone();
                restore_operand_parens(&mut restored, &record);
                (restored, raw_method.clone())
            })
            .collect()
    }

    const NESTED_SOURCE: &str = "Object subclass: A
  summed: a => (a * 2) + 1
  m => (self) foo
  n => ((x)) foo: (((1))) bar: [:b | (b) + ((2))]
  c => (x) foo: (1); bar: ((2)); baz
  f => (a foo); bar
  s => ((super)) foo: (3)
  st => y := (x)
";

    #[test]
    fn restore_gives_back_the_tree_as_parsed() {
        for (restored, raw) in restored_and_raw(NESTED_SOURCE) {
            assert_eq!(restored, raw);
        }
    }

    #[test]
    fn restore_is_idempotent() {
        let mut peeled = parse_bt(NESTED_SOURCE);
        let record = peel_operand_parens_recording(&mut peeled);
        assert!(!record.is_empty());
        for method in &peeled.classes[0].methods {
            let mut once = method.clone();
            restore_operand_parens(&mut once, &record);
            let mut twice = once.clone();
            restore_operand_parens(&mut twice, &record);
            assert_eq!(once, twice);
        }
    }

    #[test]
    fn restore_with_an_empty_record_changes_nothing() {
        let mut peeled = parse_bt(NESTED_SOURCE);
        peel_operand_parens(&mut peeled);
        for method in &peeled.classes[0].methods {
            let mut restored = method.clone();
            restore_operand_parens(&mut restored, &PeeledParens::default());
            assert_eq!(&restored, method);
        }
    }

    #[test]
    fn records_one_entry_per_peeled_operand_with_every_layer() {
        let mut module = parse_bt("((x)) foo: (1)");
        let record = peel_operand_parens_recording(&mut module);
        assert_eq!(record.len(), 2);
        // `((x))`: the inner node `x` at 2..3 was wrapped by 1..4, then 0..5.
        assert_eq!(
            record.layers_of(PeelScope::Module, Span::new(2, 3)),
            Some(&vec![Span::new(1, 4), Span::new(0, 5)])
        );
    }

    #[test]
    fn roots_agree_between_shared_and_mutable_walks() {
        let mut module = parse_bt(
            "Object subclass: A\n  state: s = 1\n  classState: c = 2\n  foo => 3\n  class bar => 4\nA >> baz => 5\nProtocol define: P\n  pro => 6\n7",
        );
        let mut shared = Vec::new();
        for_each_root(&module, &mut |scope, e| shared.push((scope, e.clone())));
        let mut mutable = Vec::new();
        for_each_root_mut(&mut module, &mut |scope, e| {
            mutable.push((scope, e.clone()));
        });
        assert_eq!(shared.len(), 7);
        assert_eq!(shared, mutable);
        let module_level = shared
            .iter()
            .filter(|(scope, _)| *scope == PeelScope::Module)
            .count();
        // `s = 1`, `c = 2` and `7`; the four method bodies are scoped to their method.
        assert_eq!(module_level, 3);
    }

    /// A class source and a protocol source (another file) in which the
    /// class method's bare operand `x` and the provision's parenthesised
    /// `(x)` have the same byte span.
    fn colliding_sources() -> (String, String) {
        let protocol = format!(
            "// {}\nProtocol define: P\n  pro: x => (x) + 1\n",
            "=".repeat(60)
        );
        let target = protocol.find("(x)").unwrap() + 1;
        let body = "Object subclass: C\n  foo: x => x + 1\n";
        let base = body.find("=> x").unwrap() + 3;
        let pad = format!("//{}\n", "-".repeat(target - base - 3));
        let class = format!("{pad}{body}");
        assert_eq!(class.find("=> x").unwrap() + 3, target);
        (class, protocol)
    }

    #[test]
    fn restore_is_scoped_to_the_method_it_was_peeled_from() {
        let (class_src, protocol_src) = colliding_sources();
        let mut class = parse_bt(&class_src);
        let mut protocol = parse_bt(&protocol_src);
        // As codegen does for a flattened module: one record for both files.
        let mut record = peel_operand_parens_recording(&mut class);
        record.extend(peel_operand_parens_recording(&mut protocol));
        assert_eq!(record.len(), 1);

        let mut foo = class.classes[0].methods[0].clone();
        restore_operand_parens(&mut foo, &record);
        assert_eq!(crate::unparse::unparse_method(&foo), "foo: x => x + 1");
        let mut pro = protocol.protocols[0].provided_methods[0].clone();
        restore_operand_parens(&mut pro, &record);
        assert_eq!(crate::unparse::unparse_method(&pro), "pro: x => (x) + 1");
    }

    #[test]
    fn recording_never_overwrites_an_entry() {
        let key = (PeelScope::Module, Span::new(5, 6));
        let mut record = PeeledParens::default();
        record.record(key, vec![Span::new(4, 7)]);
        let mut other = PeeledParens::default();
        other.record(key, vec![Span::new(3, 8)]);
        record.extend(other);
        assert_eq!(record.layers_of(key.0, key.1), Some(&vec![Span::new(4, 7)]));
    }
}

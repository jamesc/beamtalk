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
//! The pass runs where analysis and codegen take ownership of a module
//! (`analyse_full`, `lower_module_for_codegen`/`apply_codegen_writebacks`,
//! `generate_module_with_warnings`, the REPL codegen entries). The formatter,
//! the LSP and the raw parse keep the parenthesised tree. It is idempotent.

use super::{Expression, MethodDefinition, Module};
use crate::ast_walker::{walk_expression, walk_expression_mut};

/// Strips redundant parentheses from every send operand in `module`.
///
/// See the module documentation for exactly which positions are peeled.
/// Idempotent: a second run changes nothing.
pub fn peel_operand_parens(module: &mut Module) {
    for_each_root_mut(module, &mut |root| peel_operand_parens_expr(root));
}

/// Strips redundant parentheses from every send operand in `method`'s body:
/// [`peel_operand_parens`] for one method, as
/// [`crate::unparse::canonical_method_source`] needs it.
pub fn peel_method_operand_parens(method: &mut MethodDefinition) {
    for stmt in &mut method.body {
        peel_operand_parens_expr(&mut stmt.expression);
    }
}

/// Strips redundant parentheses from every send operand in `expr`'s tree
/// (including nested blocks), leaving `expr` itself as it is.
///
/// The expression-level entry of [`peel_operand_parens`], for callers that
/// hold an expression rather than a module (the REPL).
pub fn peel_operand_parens_expr(expr: &mut Expression) {
    // Pre-order: the operands of a node are peeled before the walk descends
    // into them, so a peeled operand's own operands are visited next.
    walk_expression_mut(expr, &mut |node| match node {
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            peel_operand(receiver);
            arguments.iter_mut().for_each(peel_operand);
        }
        Expression::Cascade { messages, .. } => {
            for message in messages {
                message.arguments.iter_mut().for_each(peel_operand);
            }
        }
        _ => {}
    });
}

/// `true` if [`peel_operand_parens`] would change `module`: a cheap scan so
/// callers holding a shared reference can skip cloning a module that is
/// already canonical.
#[must_use]
pub fn has_parenthesised_operand(module: &Module) -> bool {
    let mut found = false;
    for_each_root(module, &mut |root| {
        if !found {
            found = expr_has_parenthesised_operand(root);
        }
    });
    found
}

/// `true` if [`peel_operand_parens_expr`] would change `expr`.
#[must_use]
pub fn expr_has_parenthesised_operand(expr: &Expression) -> bool {
    let mut found = false;
    walk_expression(expr, &mut |node| {
        if found {
            return;
        }
        found = match node {
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => is_peelable(receiver) || arguments.iter().any(is_peelable),
            Expression::Cascade { messages, .. } => messages
                .iter()
                .any(|message| message.arguments.iter().any(is_peelable)),
            _ => false,
        };
    });
    found
}

/// `true` if `operand` is a `Parenthesized` wrapper the pass removes: any
/// wrapper whose innermost node is not `super`.
fn is_peelable(operand: &Expression) -> bool {
    matches!(operand, Expression::Parenthesized { .. })
        && !matches!(operand.unwrap_parens(), Expression::Super(_))
}

/// Replaces `operand` by its innermost non-`Parenthesized` node, when
/// [`is_peelable`].
fn peel_operand(operand: &mut Expression) {
    if !is_peelable(operand) {
        return;
    }
    while let Expression::Parenthesized { expression, .. } = operand {
        let inner = std::mem::replace(
            expression.as_mut(),
            Expression::Error {
                message: "".into(),
                span: crate::span::Span::default(),
            },
        );
        *operand = inner;
    }
}

/// Calls `f` on every root expression of `module`: module-level statements,
/// every method body statement (class, class-side, standalone and protocol
/// provisions), and every instance- and class-variable initialiser.
fn for_each_root(module: &Module, f: &mut dyn FnMut(&Expression)) {
    for stmt in &module.expressions {
        f(&stmt.expression);
    }
    for class in &module.classes {
        for decl in class.state.iter().chain(&class.class_variables) {
            if let Some(default) = &decl.default_value {
                f(default);
            }
        }
        for method in class.methods.iter().chain(&class.class_methods) {
            method.body.iter().for_each(|stmt| f(&stmt.expression));
        }
    }
    for standalone in &module.method_definitions {
        standalone
            .method
            .body
            .iter()
            .for_each(|stmt| f(&stmt.expression));
    }
    for protocol in &module.protocols {
        for method in &protocol.provided_methods {
            method.body.iter().for_each(|stmt| f(&stmt.expression));
        }
    }
}

/// The mutable counterpart of [`for_each_root`]; the two enumerate the same
/// roots, which `roots_agree_between_shared_and_mutable_walks` asserts.
fn for_each_root_mut(module: &mut Module, f: &mut dyn FnMut(&mut Expression)) {
    for stmt in &mut module.expressions {
        f(&mut stmt.expression);
    }
    for class in &mut module.classes {
        for decl in class.state.iter_mut().chain(&mut class.class_variables) {
            if let Some(default) = &mut decl.default_value {
                f(default);
            }
        }
        for method in class.methods.iter_mut().chain(&mut class.class_methods) {
            method
                .body
                .iter_mut()
                .for_each(|stmt| f(&mut stmt.expression));
        }
    }
    for standalone in &mut module.method_definitions {
        standalone
            .method
            .body
            .iter_mut()
            .for_each(|stmt| f(&mut stmt.expression));
    }
    for protocol in &mut module.protocols {
        for method in &mut protocol.provided_methods {
            method
                .body
                .iter_mut()
                .for_each(|stmt| f(&mut stmt.expression));
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

    #[test]
    fn roots_agree_between_shared_and_mutable_walks() {
        let mut module = parse_bt(
            "Object subclass: A\n  state: s = 1\n  classState: c = 2\n  foo => 3\n  class bar => 4\nA >> baz => 5\nProtocol define: P\n  pro => 6\n7",
        );
        let mut shared = Vec::new();
        for_each_root(&module, &mut |e| shared.push(e.clone()));
        let mut mutable = Vec::new();
        for_each_root_mut(&mut module, &mut |e| mutable.push(e.clone()));
        assert_eq!(shared.len(), 7);
        assert_eq!(shared, mutable);
    }
}

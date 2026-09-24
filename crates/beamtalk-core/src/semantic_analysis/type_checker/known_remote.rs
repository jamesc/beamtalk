// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Known-remote flow-fact tracking (ADR 0126 §6).
//!
//! There is **no `Remote(T)` type** (ADR 0104's location transparency is
//! kept). Instead the checker tracks a *flow fact*: a local binding is
//! "known-remote" when its initializer is (or transparently unwraps) a
//! `spawnOn:`/`spawnWith:on:`/`spawnAs:on:`/`spawnWith:as:on:`/`named:on:`
//! send, or a `scope: #global` spawn/lookup send. Known-remote receivers
//! upgrade two of ADR 0103's advisory sendability checks
//! (`sendability_validators.rs::check_block_captures`,
//! `validation.rs::check_arg_sendability`) from silent/info to a
//! diagnostic that predicts an actual runtime rejection or failure mode.
//!
//! This module is the **single source of truth** for the classification —
//! [`classify`] is the one place that decides whether an expression's value
//! is known-remote, so the live-`TypeEnv` check (message-argument
//! sendability, during inference) and the span-indexed post-hoc check
//! (block-argument Hint, `sendability_validators.rs`, which runs *after*
//! inference with no live `TypeEnv`) cannot drift apart — both resolve a
//! bare identifier's current binding through the `is_remote_ident` callback
//! and share every other rule.
//!
//! Flow-local only (ADR 0126 §6): does **not** propagate through fields,
//! collections, or method return types — `classify` only ever recurses
//! through the fixed selector lists below, so nothing else can widen it.
//! The runtime encode-time check (ADR 0126 §5.1/§5.4, which inspects
//! *runtime* classes, not declared/inferred types) is authoritative
//! regardless of what this module concludes.

use ecow::EcoString;

use crate::ast::{Block, Expression, Identifier, Literal, MessageSelector};

/// Known-remote provenance for one binding/use.
///
/// Besides the yes/no fact, this carries a best-effort display name for the
/// target `Node` — the argument to the spawn/lookup send's trailing `on:`
/// keyword at the binding site — so a downstream diagnostic can name it
/// (the block-argument Hint's "on `worker@host`" wording, ADR 0126 §6).
/// `node_hint` is `None` when the node argument isn't a simple identifier,
/// or when there is no node argument at all (`scope: #global` is
/// cluster-wide, not node-qualified).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct KnownRemote {
    pub(crate) node_hint: Option<EcoString>,
}

impl KnownRemote {
    fn new(node_hint: Option<EcoString>) -> Self {
        Self { node_hint }
    }
}

/// The direct remote spawn/lookup selectors (ADR 0126 §3's decision table):
/// `spawnOn:`, `spawnAs:on:`, `spawnWith:on:`, `spawnWith:as:on:`,
/// `named:on:`. All five are already declared as builtin `Actor` class
/// methods (`generated_builtins.rs`) and every one carries the target
/// `Node` as its **last** positional argument.
const KNOWN_REMOTE_SELECTORS: &[&str] = &[
    "spawnOn:",
    "spawnWith:on:",
    "spawnAs:on:",
    "spawnWith:as:on:",
    "named:on:",
];

/// Selectors whose result carries through the RECEIVER's known-remote fact
/// unchanged (ADR 0126 §6: "through `unwrap`, `value`, ... — and the
/// checker propagates known-remote through `withTimeout:`"). `unwrap` and
/// `value` are `Result`'s ok-accessors (raise on an error `Result`);
/// `withTimeout:` locally re-types the receiver as a transparent
/// `TimeoutProxy` (ADR 0104 Phase 3 / ADR 0126 §6 "`withTimeout:` hides
/// remoteness [in TYPE]"), but the flow fact itself survives.
const TRANSPARENT_SELECTORS: &[&str] = &["unwrap", "value", "withTimeout:"];

/// The known-remote flow classification of `expr`'s **value**, per ADR
/// 0126 §6 — the single source of truth every consumer calls.
///
/// `is_remote_ident` resolves whether a bare identifier's *current*
/// binding is already known-remote (the recursive base case):
/// * during inference, with a live `TypeEnv`, a caller passes a closure
///   over `TypeEnv::known_remote`;
/// * a later, separate walk over the already-type-checked module (no live
///   `TypeEnv` — `sendability_validators.rs`'s block-argument check) passes
///   a closure over the persisted `TypeChecker::known_remote_spans`, which
///   the inference pass recorded at every identifier *use* site.
#[must_use]
pub(crate) fn classify(
    expr: &Expression,
    is_remote_ident: &impl Fn(&Identifier) -> Option<KnownRemote>,
) -> Option<KnownRemote> {
    match expr.unwrap_parens() {
        Expression::Identifier(id) => is_remote_ident(id),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast: false,
            ..
        } => {
            if is_known_remote_send(selector, arguments) {
                return Some(KnownRemote::new(node_hint_for_send(selector, arguments)));
            }
            let name = selector.name();
            if TRANSPARENT_SELECTORS.contains(&name.as_str()) {
                return classify(receiver, is_remote_ident);
            }
            // `receiver ifOk: [:v | v] ifError: [...]` — the common
            // "unwrap with fallback" idiom. Only the trivial
            // parameter-forwarding ok-block shape is recognised (ADR 0126
            // §6 "through ... `ifOk:ifError:` ok-branches") — a
            // transforming block (`[:v | v foo]`) is intentionally not
            // followed, matching the "flow-local, shallow" contract: the
            // checker does not attempt data-flow analysis *inside* an
            // arbitrary block body.
            if name == "ifOk:ifError:" {
                if let [Expression::Block(ok_block), _] = arguments.as_slice() {
                    if is_trivial_param_forward(ok_block) {
                        return classify(receiver, is_remote_ident);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Whether `selector`/`arguments` is one of the five direct remote
/// spawn/lookup sends, or a keyword send carrying `scope: #global`.
fn is_known_remote_send(selector: &MessageSelector, arguments: &[Expression]) -> bool {
    KNOWN_REMOTE_SELECTORS.contains(&selector.name().as_str())
        || scope_arg_is_global(selector, arguments)
}

/// The best-effort display text for the target `Node` argument of a direct
/// remote spawn/lookup send — the **last** positional argument on every
/// selector in [`KNOWN_REMOTE_SELECTORS`]. `None` for a non-identifier node
/// expression (a literal, another send, …) or for a `scope: #global` send
/// (no node argument at all — cluster-wide, not node-qualified).
fn node_hint_for_send(selector: &MessageSelector, arguments: &[Expression]) -> Option<EcoString> {
    if !KNOWN_REMOTE_SELECTORS.contains(&selector.name().as_str()) {
        return None;
    }
    match arguments.last()?.unwrap_parens() {
        Expression::Identifier(id) => Some(id.name.clone()),
        _ => None,
    }
}

/// Whether a keyword send has a `scope:` keyword part whose argument is the
/// symbol literal `#global` (ADR 0126 §4/§6). `scope: #local` (or any other
/// scope symbol) is not known-remote — matching local sends stay unchanged.
fn scope_arg_is_global(selector: &MessageSelector, arguments: &[Expression]) -> bool {
    let MessageSelector::Keyword(parts) = selector else {
        return false;
    };
    parts
        .iter()
        .position(|part| part.keyword.as_str() == "scope:")
        .is_some_and(|i| arguments.get(i).is_some_and(is_global_symbol))
}

/// Whether `expr` is the bare symbol literal `#global` (parens-tolerant).
fn is_global_symbol(expr: &Expression) -> bool {
    matches!(
        expr.unwrap_parens(),
        Expression::Literal(Literal::Symbol(s), _) if s.as_str() == "global"
    )
}

/// Whether `block` has exactly one parameter and its body is exactly that
/// parameter, bare (`[:v | v]`) — the trivial `ifOk:` forwarding shape.
fn is_trivial_param_forward(block: &Block) -> bool {
    let [param] = block.parameters.as_slice() else {
        return false;
    };
    let [stmt] = block.body.as_slice() else {
        return false;
    };
    matches!(
        stmt.expression.unwrap_parens(),
        Expression::Identifier(id) if id.name == param.name
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BlockParameter, ExpressionStatement, KeywordPart};
    use crate::source_analysis::Span;

    fn stmt(expression: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expression)
    }

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn ident(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name, span()))
    }

    fn symbol(name: &str) -> Expression {
        Expression::Literal(Literal::Symbol(EcoString::from(name)), span())
    }

    fn msg_send(
        receiver: Expression,
        selector: MessageSelector,
        args: Vec<Expression>,
    ) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector,
            arguments: args,
            is_cast: false,
            span: span(),
        }
    }

    fn keyword(parts: &[&str]) -> MessageSelector {
        MessageSelector::Keyword(parts.iter().map(|p| KeywordPart::new(*p, span())).collect())
    }

    /// `is_remote_ident` that always answers "not known-remote" — used by
    /// tests that only exercise the structural (non-identifier-base-case)
    /// rules.
    fn never_remote(_: &Identifier) -> Option<KnownRemote> {
        None
    }

    #[test]
    fn spawn_on_is_known_remote() {
        let expr = msg_send(
            Expression::ClassReference {
                name: Identifier::new("Counter", span()),
                package: None,
                span: span(),
            },
            keyword(&["spawnOn:"]),
            vec![ident("worker")],
        );
        let remote = classify(&expr, &never_remote).expect("spawnOn: must be known-remote");
        assert_eq!(remote.node_hint.as_deref(), Some("worker"));
    }

    #[test]
    fn scope_global_is_known_remote() {
        let expr = msg_send(
            Expression::ClassReference {
                name: Identifier::new("Scheduler", span()),
                package: None,
                span: span(),
            },
            keyword(&["spawnAs:", "scope:"]),
            vec![symbol("scheduler"), symbol("global")],
        );
        let remote = classify(&expr, &never_remote).expect("scope: #global must be known-remote");
        // No node argument at all for a global send.
        assert_eq!(remote.node_hint, None);
    }

    #[test]
    fn scope_local_is_not_known_remote() {
        let expr = msg_send(
            Expression::ClassReference {
                name: Identifier::new("Scheduler", span()),
                package: None,
                span: span(),
            },
            keyword(&["spawnAs:", "scope:"]),
            vec![symbol("scheduler"), symbol("local")],
        );
        assert!(classify(&expr, &never_remote).is_none());
    }

    #[test]
    fn unwrap_propagates_through_receiver() {
        let spawn = msg_send(
            Expression::ClassReference {
                name: Identifier::new("Counter", span()),
                package: None,
                span: span(),
            },
            keyword(&["spawnOn:"]),
            vec![ident("worker")],
        );
        let unwrapped = msg_send(
            spawn,
            MessageSelector::Unary(EcoString::from("unwrap")),
            vec![],
        );
        assert!(classify(&unwrapped, &never_remote).is_some());
    }

    #[test]
    fn with_timeout_propagates_through_receiver() {
        let remote_ident = |id: &Identifier| {
            (id.name == "c").then(|| KnownRemote::new(Some(EcoString::from("worker"))))
        };
        let with_timeout = msg_send(
            ident("c"),
            keyword(&["withTimeout:"]),
            vec![Expression::Literal(Literal::Integer(5000), span())],
        );
        let remote = classify(&with_timeout, &remote_ident).expect("withTimeout: must propagate");
        assert_eq!(remote.node_hint.as_deref(), Some("worker"));
    }

    #[test]
    fn ordinary_message_is_not_known_remote() {
        let send = msg_send(ident("c"), keyword(&["increment:"]), vec![ident("x")]);
        assert!(classify(&send, &never_remote).is_none());
    }

    #[test]
    fn collection_construction_is_not_known_remote() {
        // `Array with: c` — provenance does not flow through a collection.
        let remote_ident = |id: &Identifier| (id.name == "c").then(|| KnownRemote::new(None));
        let send = msg_send(
            Expression::ClassReference {
                name: Identifier::new("Array", span()),
                package: None,
                span: span(),
            },
            keyword(&["with:"]),
            vec![ident("c")],
        );
        assert!(classify(&send, &remote_ident).is_none());
    }

    #[test]
    fn if_ok_if_error_trivial_forward_propagates() {
        let spawn = msg_send(
            Expression::ClassReference {
                name: Identifier::new("Counter", span()),
                package: None,
                span: span(),
            },
            keyword(&["spawnOn:"]),
            vec![ident("worker")],
        );
        let ok_block = Block::new(
            vec![BlockParameter::new("v", span())],
            vec![stmt(ident("v"))],
            span(),
        );
        let err_block = Block::new(
            vec![BlockParameter::new("e", span())],
            vec![stmt(Expression::Literal(Literal::Integer(0), span()))],
            span(),
        );
        let send = msg_send(
            spawn,
            keyword(&["ifOk:", "ifError:"]),
            vec![Expression::Block(ok_block), Expression::Block(err_block)],
        );
        assert!(classify(&send, &never_remote).is_some());
    }

    #[test]
    fn if_ok_if_error_transforming_block_does_not_propagate() {
        let spawn = msg_send(
            Expression::ClassReference {
                name: Identifier::new("Counter", span()),
                package: None,
                span: span(),
            },
            keyword(&["spawnOn:"]),
            vec![ident("worker")],
        );
        // [:v | v foo] — not a bare parameter forward.
        let ok_block = Block::new(
            vec![BlockParameter::new("v", span())],
            vec![stmt(msg_send(
                ident("v"),
                MessageSelector::Unary(EcoString::from("foo")),
                vec![],
            ))],
            span(),
        );
        let err_block = Block::new(vec![BlockParameter::new("e", span())], vec![], span());
        let send = msg_send(
            spawn,
            keyword(&["ifOk:", "ifError:"]),
            vec![Expression::Block(ok_block), Expression::Block(err_block)],
        );
        assert!(classify(&send, &never_remote).is_none());
    }
}

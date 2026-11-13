// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `binary-string-compare` hint (BT-4257).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A `String` is `{beamtalk_string, Bin}` and a raw binary is a `Binary`
//! (BT-4173), so a String and a Binary holding the same bytes are never
//! equal: `==` answers `false`, and a `Dictionary` keyed by Strings or a
//! `Set` of Strings never finds the Binary. Nothing errors — the lookup just
//! comes back absent — so the checker points it out.
//!
//! ## When a value is "provably a raw Binary"
//!
//! `String` is a subclass of `Binary` (ADR 0086), so a value whose static
//! type is `Binary` may hold a `String`, and comparing it with a String may
//! well succeed. The type alone is not evidence. The hint therefore fires
//! only when the Binary side is a **raw-Binary producer** ([`is_raw_binary`]):
//!
//! - a class-side send that always answers a raw binary:
//!   `Binary fromBytes:` / `fromIolist:` / `withAll:` / `serialize:` /
//!   `empty`, and `Digest sha256:` / `sha512:` / `md5:` /
//!   `hmacSha256:key:` / `hmacSha512:key:`;
//! - a `<<...>>` binary construction (ADR 0143);
//! - `concat:` or `part:size:` sent to a raw Binary;
//! - a local whose most recent assignment was one of the above (a flow
//!   fact on [`TypeEnv`](super::TypeEnv), like ADR 0126's known-remote
//!   fact: any later write to the local clears it, and it does not
//!   propagate through fields, parameters, collections or method returns).
//!   A local that any block literal or `match:` arm in the body writes is
//!   never marked: both are inferred in a child environment, and a block may
//!   run at any later point, so their writes cannot clear the fact
//!   ([`block_written_locals`]). Nor is a local trusted at a send whose
//!   later-evaluated argument assigns it: the check runs after every
//!   argument was inferred, but the operand was read before.
//!
//! The String side must have the static type `String` exactly (`String` is
//! sealed). That is a static claim: a parameter typed `String` is not checked
//! at run time, so a Dynamic caller can still pass a Binary — acceptable for
//! a hint. `Dynamic`, unions and anything else
//! stay silent (ADR 0100 Rule 1).
//!
//! The "reverse" collection case — a `Dictionary(Binary, V)` or `Set(Binary)`
//! probed with a String — is never reported: the declared element type
//! `Binary` may hold Strings, so the lookup may legitimately hit.

use std::collections::HashSet;

use ecow::EcoString;

use crate::ast::{Expression, ExpressionStatement, MessageSelector};
use crate::semantic_analysis::block_facts;
use crate::semantic_analysis::type_checker::{InferredType, TypeChecker, TypeEnv};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

/// Class-side selectors on `Binary` that always answer a raw binary.
const BINARY_FACTORY_SELECTORS: &[&str] = &[
    "fromBytes:",
    "fromIolist:",
    "withAll:",
    "serialize:",
    "empty",
];

/// Class-side selectors on `Digest` — each answers the raw digest bytes
/// (`crypto:hash/2`, `crypto:mac/4`).
const DIGEST_SELECTORS: &[&str] = &[
    "sha256:",
    "sha512:",
    "md5:",
    "hmacSha256:key:",
    "hmacSha512:key:",
];

/// Instance selectors that answer a raw binary when sent to a raw binary.
const RAW_PRESERVING_SELECTORS: &[&str] = &["concat:", "part:size:"];

/// `Dictionary` selectors whose first argument is a key.
const DICTIONARY_KEY_SELECTORS: &[&str] = &[
    "at:",
    "at:ifAbsent:",
    "at:put:",
    "includesKey:",
    "removeKey:",
];

/// `Set` selectors whose first argument is an element.
const SET_ELEMENT_SELECTORS: &[&str] = &["includes:", "add:"];

/// Whether `expr`'s value is provably a raw `Binary` — never a `String` —
/// by construction (see the module doc). `is_raw_local` answers the
/// flow fact for a bare local name.
///
/// Read on assignment values (which keep their parentheses) as well as on
/// send operands, hence the peel.
pub(super) fn is_raw_binary(expr: &Expression, is_raw_local: &dyn Fn(&str) -> bool) -> bool {
    match expr.unwrap_parens() {
        Expression::Identifier(ident) => is_raw_local(&ident.name),
        // `<<...>>` always builds bytes, never a String (ADR 0143); the
        // caller's `Binary` type check excludes an unaligned `Bitstring`.
        Expression::BinaryConstruction { .. } => true,
        Expression::MessageSend {
            receiver, selector, ..
        } => {
            let selector_name = selector.name();
            // A send receiver is already peeled (BT-4059).
            match receiver.as_ref() {
                Expression::ClassReference {
                    name,
                    package: None,
                    ..
                } => match name.name.as_str() {
                    "Binary" => BINARY_FACTORY_SELECTORS.contains(&selector_name.as_str()),
                    "Digest" => DIGEST_SELECTORS.contains(&selector_name.as_str()),
                    _ => false,
                },
                other => {
                    RAW_PRESERVING_SELECTORS.contains(&selector_name.as_str())
                        && is_raw_binary(other, is_raw_local)
                }
            }
        }
        _ => false,
    }
}

/// The locals written by any block literal or `match:` arm body in `body`, at
/// any depth — the shared outer-write walks (`block_facts::outer_local_writes`,
/// `block_facts::match_arm_body_writes`) run on each. Both are inferred in a
/// child environment whose writes never reach the flow fact. Treating every
/// name as possibly outer over-approximates, which only ever silences the
/// hint.
pub(super) fn block_written_locals(body: &[ExpressionStatement]) -> HashSet<EcoString> {
    let mut names = HashSet::new();
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |expr| {
            let writes = match expr {
                Expression::Block(block) => block_facts::outer_local_writes(block, &|_| true),
                // An arm that assigns a bound outer local threads the write
                // back (BT-3947), but the checker infers each arm in a child
                // environment that drops it.
                Expression::Match { arms, .. } => {
                    block_facts::match_arm_body_writes(arms, &|_| true)
                }
                _ => return,
            };
            names.extend(writes.into_iter().map(|write| write.name));
        });
    }
    names
}

/// The locals assigned (`x := …`) anywhere in `exprs`, nested blocks
/// included. Only a plain assignment can mark a local raw.
fn assigned_locals(exprs: &[Expression]) -> HashSet<EcoString> {
    let mut names = HashSet::new();
    for expr in exprs {
        crate::ast_walker::walk_expression(expr, &mut |e| {
            if let Expression::Assignment { target, .. } = e
                && let Expression::Identifier(ident) = target.as_ref()
            {
                names.insert(ident.name.clone());
            }
        });
    }
    names
}

/// Whether `ty` is exactly the class named `class`.
fn is_known(ty: &InferredType, class: &str) -> bool {
    matches!(ty, InferredType::Known { class_name, .. } if class_name.as_str() == class)
}

impl TypeChecker {
    /// Emits the `binary-string-compare` hint (BT-4257) for a send that
    /// compares a provably raw `Binary` with a `String` — an equality
    /// operator with one operand of each, or a `Dictionary(String, V)` key
    /// operation / `Set(String)` membership operation given a raw Binary.
    #[allow(clippy::too_many_arguments)] // the send's parts, as the caller holds them
    pub(super) fn check_binary_string_compare(
        &mut self,
        receiver: &Expression,
        receiver_ty: &InferredType,
        selector: &MessageSelector,
        arguments: &[Expression],
        arg_types: &[InferredType],
        span: Span,
        env: &TypeEnv,
    ) {
        let (Some(arg), Some(arg_ty)) = (arguments.first(), arg_types.first()) else {
            return;
        };
        // The receiver is read before every argument, argument 0 before the
        // rest; a later argument that rebinds the local makes the fact (read
        // after all of them) describe the wrong value.
        let written_by_args = assigned_locals(arguments);
        let written_after_arg0 = assigned_locals(arguments.get(1..).unwrap_or_default());
        let is_raw = |expr: &Expression, ty: &InferredType, written_later: &HashSet<EcoString>| {
            is_known(ty, "Binary")
                && is_raw_binary(expr, &|name| {
                    env.is_raw_binary_local(name) && !written_later.contains(name)
                })
        };

        let mismatch = match selector {
            MessageSelector::Binary(op) => {
                crate::source_analysis::equality_operator_is_negated(op).is_some()
                    && ((is_raw(receiver, receiver_ty, &written_by_args)
                        && is_known(arg_ty, "String"))
                        || (is_known(receiver_ty, "String")
                            && is_raw(arg, arg_ty, &written_after_arg0)))
            }
            MessageSelector::Unary(_) => false,
            MessageSelector::Keyword(_) => {
                let InferredType::Known {
                    class_name,
                    type_args,
                    ..
                } = receiver_ty
                else {
                    return;
                };
                let selector_name = selector.name();
                let element_selectors = match class_name.as_str() {
                    "Dictionary" => DICTIONARY_KEY_SELECTORS,
                    "Set" => SET_ELEMENT_SELECTORS,
                    _ => return,
                };
                element_selectors.contains(&selector_name.as_str())
                    && type_args
                        .first()
                        .is_some_and(|key_ty| is_known(key_ty, "String"))
                    && is_raw(arg, arg_ty, &written_after_arg0)
            }
        };
        if mismatch {
            self.diagnostics.push(binary_string_compare_hint(span));
        }
    }
}

/// The `binary-string-compare` hint diagnostic.
fn binary_string_compare_hint(span: Span) -> Diagnostic {
    Diagnostic::hint(
        "a Binary never equals a String, even with the same bytes",
        span,
    )
    .with_hint(
        "convert the Binary first with `asString` (validated, answers a Result) \
         or `asStringUnchecked`",
    )
    .with_category(DiagnosticCategory::BinaryStringCompare)
}

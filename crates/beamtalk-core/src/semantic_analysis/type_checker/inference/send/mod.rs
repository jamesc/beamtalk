// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Message-send type inference.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Entry points for `Expression::MessageSend` inference and the small
//! syntactic-receiver-shape helpers shared across the `send` submodule tree.
//! Receiver-typed dispatch (`Known`/`Meta`/`ClassReference`/`Union` lookup)
//! lives in [`receiver`]; FFI call inference lives in [`ffi`].

pub(in crate::semantic_analysis::type_checker) mod ffi;
pub(in crate::semantic_analysis::type_checker) mod receiver;

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::{InferredType, TypeChecker, TypeEnv};
use crate::source_analysis::Span;

impl TypeChecker {
    /// Infer the type of a message send and validate the selector.
    #[allow(clippy::too_many_arguments)] // hierarchy + env + flag needed for recursive checking
    pub(in crate::semantic_analysis::type_checker) fn infer_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let receiver_ty = self.infer_expr(receiver, hierarchy, env, in_abstract_method);
        self.infer_message_send_with_receiver_ty(
            receiver,
            receiver_ty,
            selector,
            arguments,
            span,
            hierarchy,
            env,
            in_abstract_method,
        )
    }

    /// Infer argument types via the generic block-context path (BT-2158
    /// class-side detection + [`Self::infer_args_with_block_context`]).
    ///
    /// This is the fallback used by [`Self::infer_message_send_with_receiver_ty`]
    /// for any selector that doesn't get bespoke narrowing/block-parameter
    /// treatment above it (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`, `on:do:`,
    /// `ifNil:`/`ifNotNil:` variants, and `notNil and:` — BT-2872). Factored
    /// out so both the plain "no special case matched" branch and the
    /// `notNil and:` branch's own fallback (when the narrowing match
    /// unexpectedly has no argument to narrow) share one implementation.
    #[allow(clippy::too_many_arguments)] // mirrors infer_args_with_block_context's shape
    pub(in crate::semantic_analysis::type_checker) fn infer_generic_send_args(
        &mut self,
        arguments: &[Expression],
        receiver: &Expression,
        receiver_ty: &InferredType,
        selector_name: &str,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        // BT-2158: detect class-side sends so block-param propagation
        // uses `find_class_method` instead of `find_method`. Shares the
        // helper with the downstream `is_class_side_receiver` check below.
        // ADR 0083: a metatype-typed receiver (`Meta{C}`) is also class-side
        // — its block params should resolve against `C`'s class methods.
        let is_class_side = Self::is_class_side_receiver(receiver, env)
            || matches!(receiver_ty, InferredType::Meta { .. });
        self.infer_args_with_block_context(
            arguments,
            receiver_ty,
            selector_name,
            hierarchy,
            env,
            in_abstract_method,
            is_class_side,
        )
    }

    /// Returns true if the expression is `self` (direct identifier reference).
    pub(in crate::semantic_analysis::type_checker) fn is_self_receiver(expr: &Expression) -> bool {
        matches!(expr, Expression::Identifier(ident) if ident.name == "self")
    }

    /// Returns true if `expr` resolves to a class-side receiver — either a
    /// direct `ClassReference` or `self` inside a class method. Unwraps
    /// parentheses so `(HTTPRouter) foo:` and `(self) foo:` are treated
    /// identically to the un-parenthesised forms (BT-2158).
    pub(in crate::semantic_analysis::type_checker) fn is_class_side_receiver(
        expr: &Expression,
        env: &TypeEnv,
    ) -> bool {
        let unwrapped = expr.unwrap_parens();
        matches!(unwrapped, Expression::ClassReference { .. })
            || (env.in_class_method && Self::is_self_receiver(unwrapped))
    }
}
/// Class-protocol selectors that must NOT be intercepted as FFI module lookups.
///
/// These are handled by `beamtalk_object_class:class_send/3` at runtime.
/// BT-3079: delegates to the single shared recognizer in
/// [`crate::ffi_receiver`], which codegen and the semantic-analysis validators
/// also use, to keep this behaviour consistent everywhere (BT-1880).
pub(in crate::semantic_analysis::type_checker) fn is_class_protocol_selector(
    selector: &str,
) -> bool {
    crate::ffi_receiver::is_class_protocol_selector(selector)
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! FFI call type inference.
//!
//! **DDD Context:** Semantic Analysis
//!
//! ADR 0075 — infers the return type of an `Erlang <module> <function>:`
//! call on an `ErlangModule<module_name>` receiver by looking up
//! `(module, function, arity)` in `NativeTypeRegistry`, checks argument
//! types positionally against the declared signature, and emits the
//! keyword-mismatch footgun warning.

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::validation::UnionArgCompat;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{DynamicReason, InferredType, TypeChecker};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

impl TypeChecker {
    /// Infer the return type of an FFI call on an `ErlangModule<module_name>` receiver.
    ///
    /// Extracts the Erlang module name from the `type_args`, the function name from
    /// the first keyword of the selector, and the arity from argument count. Looks
    /// up `(module, function, arity)` in `NativeTypeRegistry` and returns the
    /// declared return type, or `Dynamic` if not found.
    ///
    /// Also emits keyword mismatch warnings when call-site keywords don't match
    /// the registry's declared parameter names (ADR 0075 — footgun prevention).
    ///
    /// **References:** ADR 0075 — Type Checker Integration, Keyword mismatch warning
    #[allow(clippy::too_many_arguments)] // hierarchy needed for Union-arm compatibility checks
    pub(in crate::semantic_analysis::type_checker) fn infer_ffi_call(
        &mut self,
        receiver_type_args: &[InferredType],
        selector: &MessageSelector,
        arguments: &[Expression],
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        // Extract the module name from the receiver's type args.
        // ErlangModule<lists> → module_name = "lists"
        let Some(InferredType::Known {
            class_name: module_name,
            ..
        }) = receiver_type_args.first()
        else {
            return InferredType::Dynamic(DynamicReason::DynamicReceiver); // Dynamic module name
        };

        // Extract the canonical Erlang function name and arity from the selector
        // (delegates to the one canonical erlang_function_name/erlang_arity
        // pair, see extract_ffi_function_info's doc comment). `None` for a binary
        // selector — unreachable today since the call site's own guard already
        // excludes `MessageSelector::Binary` before we get here,
        // but falling back to `UntypedFfi` rather than panicking/asserting keeps
        // that a soft invariant instead of a hard one.
        let Some((function_name, arity)) = Self::extract_ffi_function_info(selector, arguments)
        else {
            return InferredType::Dynamic(DynamicReason::UntypedFfi);
        };

        // Clone the signature to release the borrow on self before emitting diagnostics.
        let sig = self
            .native_type_registry
            .as_ref()
            .and_then(|reg| reg.lookup(module_name, &function_name, arity))
            .cloned();

        let Some(sig) = sig else {
            return InferredType::Dynamic(DynamicReason::UntypedFfi);
        };

        // Emit keyword mismatch warnings (ADR 0075 footgun prevention)
        self.check_ffi_keyword_mismatch(module_name, &function_name, arity, selector, &sig, span);

        // Check argument types positionally against declared params
        self.check_ffi_argument_types(
            module_name,
            &function_name,
            &sig,
            arg_types,
            span,
            hierarchy,
        );

        // Propagate call-site type_args into the FFI return type.
        // Erlang specs with polymorphic types (e.g., `[T] -> [T]` for lists:reverse/1)
        // are registered as bare `List -> List` with no type_args. When the call-site
        // argument carries type_args (e.g., `List(String)`), propagate them to the
        // return type so downstream sends see the element type.
        Self::substitute_ffi_return_type(&sig.return_type, &sig.params, arg_types)
    }

    /// Extracts the Erlang function name and arity from a selector and arguments.
    ///
    /// For keyword selectors like `seq: 1 to: 10`, the function name is the first
    /// keyword ("seq") and the arity is the argument count.
    /// For unary selectors like `reverse`, the function name is the selector and
    /// arity is 0 (nullary Erlang call).
    ///
    /// Delegates to the canonical FFI-naming pair
    /// [`erlang_function_name`](crate::semantic_analysis::validators::erlang_function_name) /
    /// [`erlang_arity`](crate::semantic_analysis::validators::erlang_arity)
    /// rather than re-deriving "first keyword sans colon" with its
    /// own `.split(':')` — that duplicate previously computed a name for
    /// *binary* selectors too (e.g. `"+".split(':').next()` ⇒ `"+"`), which
    /// disagreed with the canonical function (binary ⇒ `None`, no FFI-name
    /// concept) and with `dispatch_codegen`'s `generate_direct_erlang_call`
    /// (binary ⇒ falls through to normal dispatch, never emits a direct FFI
    /// call). Returns `None` for a binary selector, matching both of those.
    pub(in crate::semantic_analysis::type_checker) fn extract_ffi_function_info(
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Option<(String, u8)> {
        let function_name = crate::semantic_analysis::validators::erlang_function_name(selector)?;
        let arity = crate::semantic_analysis::validators::erlang_arity(selector, arguments.len());
        let arity = u8::try_from(arity).unwrap_or(u8::MAX);
        Some((function_name, arity))
    }

    /// Checks argument types against declared parameter types in an FFI signature.
    ///
    /// Types are matched positionally (ADR 0075 — FFI calls are positional).
    pub(in crate::semantic_analysis::type_checker) fn check_ffi_argument_types(
        &mut self,
        module_name: &str,
        function_name: &str,
        sig: &crate::semantic_analysis::type_checker::native_type_registry::FunctionSignature,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        for (i, (param, arg_ty)) in sig.params.iter().zip(arg_types.iter()).enumerate() {
            // Skip Dynamic args — we don't know the type
            if matches!(arg_ty, InferredType::Dynamic(_)) {
                continue;
            }
            // Skip Dynamic param types — anything is accepted
            if matches!(param.type_, InferredType::Dynamic(_)) {
                continue;
            }

            let InferredType::Known {
                class_name: expected,
                ..
            } = &param.type_
            else {
                continue;
            };

            let param_pos = i + 1;
            let fallback_label = format!("parameter {param_pos}");
            let param_label = param.keyword.as_deref().unwrap_or(&fallback_label);
            // Render `UndefinedObject` as `Nil` in user-facing messages.
            let expected_display = InferredType::class_name_for_diagnostic(expected.as_str());

            // Object is the root of the BT class hierarchy — any class is a
            // subtype. This arises when Erlang specs use beamtalk_object()
            // (which maps to Object) and the call site passes a concrete class.
            let expected_is_object =
                WellKnownClass::from_str(expected) == Some(WellKnownClass::Object);

            match arg_ty {
                InferredType::Known {
                    class_name: actual, ..
                } => {
                    // Use the same subtyping predicate the `Union` arm
                    // below already uses, rather than bare name equality — a
                    // lone singleton `#first` is a subtype of a `Symbol` param
                    // (Erlang spec `atom()`) exactly as `#first | #last` is.
                    if expected_is_object || Self::is_type_compatible(actual, expected, hierarchy) {
                        continue;
                    }
                    let actual_display = InferredType::class_name_for_diagnostic(actual.as_str());
                    self.diagnostics.push(Diagnostic::warning(
                        format!(
                            "{module_name}:{function_name}/{arity} {param_label} expects {expected_display}, got {actual_display}",
                            arity = sig.arity,
                        ),
                        span,
                    ).with_hint("Use `@expect type` to suppress if the call is intentional")
                    .with_category(DiagnosticCategory::Type));
                }
                InferredType::Union { members, .. } => {
                    // Every member of the argument's union
                    // is checked against the declared FFI parameter type,
                    // via the same classification `check_argument_types`
                    // uses (`UnionArgCompat`) — not a re-derived copy.
                    if expected_is_object {
                        continue;
                    }
                    let (compat, incompatible) =
                        match Self::classify_union_arg_compat(members, expected, hierarchy) {
                            UnionArgCompat::AllCompatible | UnionArgCompat::Unclassifiable => {
                                continue;
                            }
                            UnionArgCompat::SomeIncompatible {
                                compat,
                                incompatible,
                                ..
                            } => (compat, incompatible),
                        };
                    let union_display = arg_ty
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let base_message = format!(
                        "{module_name}:{function_name}/{arity} {param_label} expects {expected_display}, got {union_display}",
                        arity = sig.arity,
                    );
                    let diag = if compat == 0 {
                        Diagnostic::warning(base_message, span).with_hint(format!(
                            "No member of {union_display} is compatible with {expected_display}"
                        ))
                    } else {
                        let list = incompatible
                            .iter()
                            .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        Diagnostic::hint(base_message, span).with_hint(format!(
                            "Some members of the union are not compatible with {expected_display}: {list}"
                        ))
                    };
                    self.diagnostics
                        .push(diag.with_category(DiagnosticCategory::Type));
                }
                _ => {
                    // Meta/Negation/Intersection/Never argument shapes are not
                    // handled by this check — same conservative skip as before.
                }
            }
        }
    }

    /// Propagate call-site `type_args` into an FFI return type.
    ///
    /// Erlang specs lose type-variable identity during extraction -- a spec like
    /// `-spec reverse([T]) -> [T]` becomes `List -> List` with empty `type_args`
    /// on both sides. When the call-site argument carries concrete `type_args`
    /// (e.g., `List(String)`) and the return type has the same base class as a
    /// parameter, we copy the argument's `type_args` to the return type.
    ///
    /// This is a heuristic: it assumes that when the param and return share a
    /// base class, the return preserves the same `type_args`. To stay sound,
    /// we restrict it to unary functions (single param) — the `[T] -> [T]`
    /// pattern of `lists:reverse/1`, `lists:sort/1`, etc. Multi-arg functions
    /// like `lists:map/2` (`Fun, [A] -> [B]`) would be unsound under this
    /// rule, so we leave their return types alone.
    pub(in crate::semantic_analysis::type_checker) fn substitute_ffi_return_type(
        return_type: &InferredType,
        params: &[crate::semantic_analysis::type_checker::native_type_registry::ParamType],
        arg_types: &[InferredType],
    ) -> InferredType {
        // Only applies to unary functions — see doc comment.
        if params.len() != 1 || arg_types.len() != 1 {
            return return_type.clone();
        }

        // Only applies when the return type is a Known type with no type_args
        let InferredType::Known {
            class_name: ret_class,
            type_args: ret_args,
            provenance,
        } = return_type
        else {
            return return_type.clone();
        };

        // If the return type already has type_args, nothing to propagate
        if !ret_args.is_empty() {
            return return_type.clone();
        }

        let param = &params[0];
        let arg_ty = &arg_types[0];

        let InferredType::Known {
            class_name: param_class,
            ..
        } = &param.type_
        else {
            return return_type.clone();
        };

        if param_class != ret_class {
            return return_type.clone();
        }

        let InferredType::Known {
            type_args: arg_type_args,
            ..
        } = arg_ty
        else {
            return return_type.clone();
        };

        if arg_type_args.is_empty() {
            return return_type.clone();
        }

        InferredType::Known {
            class_name: ret_class.clone(),
            type_args: arg_type_args.clone(),
            provenance: provenance.clone(),
        }
    }

    /// Emits a warning when call-site keywords don't match the registry's declared
    /// parameter names (ADR 0075 — keyword mismatch warning).
    ///
    /// Suppressed for the universal `with:` fallback (ADR 0028 convention).
    pub(in crate::semantic_analysis::type_checker) fn check_ffi_keyword_mismatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arity: u8,
        selector: &MessageSelector,
        sig: &crate::semantic_analysis::type_checker::native_type_registry::FunctionSignature,
        span: Span,
    ) {
        let MessageSelector::Keyword(parts) = selector else {
            return; // Unary/binary — no keyword mismatch possible
        };

        // Compare each keyword (except the first, which IS the function name)
        // against the declared parameter names (starting from index 1).
        for (i, part) in parts.iter().enumerate().skip(1) {
            let call_keyword = part.keyword.trim_end_matches(':');

            // Suppress warning for universal `with:` fallback (ADR 0028)
            if call_keyword == "with" {
                continue;
            }

            // Check against the declared keyword at this position
            if let Some(param) = sig.params.get(i) {
                if let Some(ref declared_keyword) = param.keyword {
                    // Skip generic/non-canonical param names — "arg" is used
                    // by beamtalk_spec_reader for placeholder parameters, and
                    // normalization also lowercases an explicit `Arg` to "arg".
                    if declared_keyword == "arg" {
                        continue;
                    }
                    if call_keyword != declared_keyword.as_str() {
                        let param_pos = i + 1;
                        self.diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "FFI keyword '{call_keyword}:' does not match declaration '{declared_keyword}:' \
                                     for {module_name}:{function_name}/{arity} parameter {param_pos}"
                                ),
                                span,
                            )
                            .with_hint(format!(
                                "FFI calls are positional — keyword names don't affect dispatch. \
                                 Preferred form: {}",
                                sig.display_signature(),
                            ))
                            .with_category(DiagnosticCategory::Type),
                        );
                    }
                }
            }
        }
    }
}

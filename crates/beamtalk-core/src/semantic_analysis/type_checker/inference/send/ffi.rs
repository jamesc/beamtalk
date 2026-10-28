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
//!
//! Also home of the `ffi-nested-string` hint (BT-4243): a `String` nested
//! in a collection argument to a stock Erlang module reaches it tagged,
//! because the FFI boundary unwraps only top-level `String` arguments.

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::validation::UnionArgCompat;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{DynamicReason, InferredType, TypeChecker};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

/// Collection classes whose instances reach Erlang as plain terms (lists,
/// maps, tuples, sets), so a `String` element inside one is visible to — and
/// compared by — the Erlang callee (BT-4243, [`TypeChecker::nests_string`]).
const ERLANG_TERM_COLLECTIONS: &[&str] = &["List", "Dictionary", "Tuple", "Set"];

fn is_erlang_term_collection(class_name: &str) -> bool {
    ERLANG_TERM_COLLECTIONS.contains(&class_name)
}

/// Whether `ty` is a `String`, or a collection (at any depth) or union
/// containing one — the element test behind [`TypeChecker::nests_string`].
fn contains_string(ty: &InferredType) -> bool {
    match ty {
        InferredType::Known { class_name, .. }
            if WellKnownClass::from_str(class_name) == Some(WellKnownClass::String) =>
        {
            true
        }
        InferredType::Union { members, .. } => members.iter().any(contains_string),
        _ => TypeChecker::nests_string(ty),
    }
}

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

        // BT-4243: independent of whether the registry knows the function —
        // the hazard is in the boundary's shallow unwrap, not the signature.
        self.check_ffi_nested_string_args(module_name, &function_name, arity, arguments, arg_types);

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

    /// Emits the `ffi-nested-string` hint (BT-4243) for each argument of an
    /// inline `Erlang <mod> <fn>:` call whose static type nests a `String`
    /// inside a collection (see [`Self::nests_string`]).
    ///
    /// Silent for Beamtalk-owned modules (`beamtalk_*`, `bt@*`), which get
    /// their arguments untouched and own the tagged representation, and for a
    /// top-level `String` argument, which the proxy unwraps.
    fn check_ffi_nested_string_args(
        &mut self,
        module_name: &str,
        function_name: &str,
        arity: u8,
        arguments: &[Expression],
        arg_types: &[InferredType],
    ) {
        if crate::ast::is_beamtalk_erlang_module(module_name) {
            return;
        }
        for (argument, arg_ty) in arguments.iter().zip(arg_types) {
            if Self::nests_string(arg_ty) {
                self.diagnostics.push(Self::ffi_nested_string_hint(
                    "this argument",
                    module_name,
                    function_name,
                    usize::from(arity),
                    argument.span(),
                ));
            }
        }
    }

    /// The `ffi-nested-string` hint (BT-4243) for a `native:` `Object`'s
    /// `self delegate` method (ADR 0101): each declared parameter is passed to
    /// `<backing module>:<leading keyword>` through the same stock-mode
    /// boundary as inline FFI, so a parameter whose declared type nests a
    /// `String` gets the same hint, on the parameter.
    ///
    /// Silent for a Beamtalk-owned backing module. The caller has already
    /// established that the class is not an `Actor`: a `native:` `Actor`
    /// delegates through `gen_server:call` to the module's own callbacks, not
    /// through the stock-mode boundary. An instance
    /// method passes `self` first, so its Erlang arity is one more than the
    /// selector's.
    pub(in crate::semantic_analysis::type_checker) fn check_native_delegate_nested_string_params(
        &mut self,
        backing_module: &str,
        method: &crate::ast::MethodDefinition,
        is_class_method: bool,
    ) {
        if !method.is_self_delegate() || crate::ast::is_beamtalk_erlang_module(backing_module) {
            return;
        }
        let mut env = crate::semantic_analysis::type_checker::TypeEnv::new();
        let mut ctx = crate::semantic_analysis::type_checker::type_resolver::ResolutionContext::new(
            self.alias_registry.as_ref(),
            &mut self.referenced_aliases,
        );
        Self::set_param_types(
            &mut env,
            &method.parameters,
            self.protocol_registry.as_ref(),
            &mut ctx,
        );
        let function_name = method.selector.leading_word().to_string();
        let arity = method.parameters.len() + usize::from(!is_class_method);
        for param in &method.parameters {
            let Some(param_ty) = env.get_local(param.name.name.as_str()) else {
                continue;
            };
            if Self::nests_string(&param_ty) {
                let subject = format!("parameter `{}`", param.name.name);
                self.diagnostics.push(Self::ffi_nested_string_hint(
                    &subject,
                    backing_module,
                    &function_name,
                    arity,
                    param.name.span,
                ));
            }
        }
    }

    /// Builds the `ffi-nested-string` hint diagnostic (BT-4243) — the one
    /// wording shared by the inline-FFI and `native:` delegation sites.
    fn ffi_nested_string_hint(
        subject: &str,
        module_name: &str,
        function_name: &str,
        arity: usize,
        span: Span,
    ) -> Diagnostic {
        Diagnostic::hint(
            format!(
                "Strings nested in {subject} reach {module_name}:{function_name}/{arity} as tagged \
                 Beamtalk Strings; an Erlang comparison against raw binaries won't match"
            ),
            span,
        )
        .with_hint(
            "use the Beamtalk-side operation (`includes:`, `at:`, `detect:`), \
             or pass Symbols/raw binaries",
        )
        .with_category(DiagnosticCategory::FfiNestedString)
    }

    /// Whether `ty` is a collection whose static type contains a `String`
    /// below the top level (BT-4243) — the shapes the FFI boundary's shallow
    /// unwrap does not reach.
    ///
    /// The collections are the ones that reach Erlang as plain terms:
    /// `List(E)`, `Dictionary(K, V)` (keys and values), `Tuple(T1, …, Tn)`
    /// and `Set(E)`. Nesting is followed recursively through those same
    /// collections (`List(List(String))`, `Dictionary(Symbol, Tuple(String,
    /// Integer))` both count), and through unions (`List(String | Nil)`,
    /// `List(String) | Nil`), since one `String` member is evidence enough.
    ///
    /// A bare `String` is *not* nested: the boundary unwraps it. `Dynamic`
    /// or missing element types never count (ADR 0100 Rule 1: no warning
    /// without evidence), and neither do Strings inside other classes
    /// (`Result(String, E)`, user objects) — those are Beamtalk objects, not
    /// collections an Erlang function compares against.
    pub(in crate::semantic_analysis::type_checker) fn nests_string(ty: &InferredType) -> bool {
        match ty {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => is_erlang_term_collection(class_name) && type_args.iter().any(contains_string),
            InferredType::Union { members, .. } => members.iter().any(Self::nests_string),
            _ => false,
        }
    }
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

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Selector-specific block/argument type inference.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Propagates declared or narrowed block-parameter types into a block
//! literal argument for the selectors that need bespoke treatment —
//! `on:do:`, the `ifNil:`/`ifNotNil:` family, `ifTrue:`/`ifFalse:`/
//! `ifTrue:ifFalse:` narrowing (ADR 0068 Phase 1g), and the generic
//! `Block(...)`-typed-parameter push-down (including its `Union`-receiver
//! variant) used by every other selector.

use crate::ast::{Expression, WellKnownSelector};
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::type_checker::narrowing::NarrowingInfo;
use crate::semantic_analysis::type_checker::narrowing::refinement::RefinementLayer;
use crate::semantic_analysis::type_checker::narrowing::visitors::{
    block_has_any_return, block_has_return,
};
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{
    DynamicReason, EnvKey, InferredType, TypeChecker, TypeEnv,
};
use crate::source_analysis::Span;

impl TypeChecker {
    /// BT-2045: Infer argument types for `on:do:` with exception class propagation.
    ///
    /// When the first argument is a class reference (e.g., `Exception`, `Error`),
    /// the handler block's parameter is typed as that class instead of
    /// `Dynamic(UnannotatedParam)`.
    ///
    /// `[...] on: Error do: [:e | e message]` → `e :: Error`
    pub(in crate::semantic_analysis::type_checker) fn infer_args_for_on_do(
        &mut self,
        arguments: &[Expression],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        // on:do: expects exactly 2 arguments: exClass and handler
        if arguments.len() != 2 {
            return arguments
                .iter()
                .map(|arg| self.infer_expr(arg, hierarchy, env, in_abstract_method))
                .collect();
        }

        let ex_class_arg = &arguments[0];
        let handler_arg = &arguments[1];

        // Infer the exception class argument normally
        let ex_class_ty = self.infer_expr(ex_class_arg, hierarchy, env, in_abstract_method);

        // Unwrap parentheses so `on: (Error) do: ([:e | ...])` also gets the
        // contextual block-param typing.
        let ex_class_inner = ex_class_arg.unwrap_parens();
        let handler_inner = handler_arg.unwrap_parens();

        // Extract class name from ClassReference for block param typing
        let exception_class_name = if let Expression::ClassReference { name, .. } = ex_class_inner {
            Some(name.name.clone())
        } else {
            None
        };

        // If handler is a block and we have a class name, type the block param
        let handler_ty = if let (Some(class_name), Expression::Block(block)) =
            (&exception_class_name, handler_inner)
        {
            let param_types = if block.parameters.is_empty() {
                vec![]
            } else {
                // Type the first (and typically only) block parameter as the exception class
                let mut types = vec![InferredType::known(class_name.clone())];
                // Any additional params beyond the first stay Dynamic
                for _ in 1..block.parameters.len() {
                    types.push(InferredType::Dynamic(DynamicReason::UnannotatedParam));
                }
                types
            };
            self.infer_block_with_typed_params(
                block,
                handler_arg.span(),
                &param_types,
                hierarchy,
                env,
                in_abstract_method,
            )
        } else {
            self.infer_expr(handler_arg, hierarchy, env, in_abstract_method)
        };

        vec![ex_class_ty, handler_ty]
    }

    /// BT-2046: Infer argument types for `ifNotNil:` / `ifNil:ifNotNil:` /
    /// `ifNotNil:ifNil:` with non-nil narrowing of the receiver propagated to
    /// the not-nil block's parameter.
    ///
    /// When the receiver is `T | Nil`, the block parameter in
    /// `ifNotNil: [:x | ...]` should be typed `T` (the non-nil branch),
    /// instead of `Dynamic(UnannotatedParam)`. For non-nullable receivers the
    /// parameter is typed as the full receiver type (not a regression from
    /// prior behaviour, which also produced `Dynamic`).
    ///
    /// Nil-branch blocks (`ifNil:`) and blocks with no declared parameter get
    /// the default inference path — solo `ifNil:` (BT-2824) also lands here
    /// (`not_nil_index` is `None` for it) purely to reuse that default path,
    /// which preserves the block's `Block(..., R)` return type.
    pub(in crate::semantic_analysis::type_checker) fn infer_args_for_if_not_nil(
        &mut self,
        selector_name: &str,
        arguments: &[Expression],
        receiver_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        // Compute the non-nil branch type once. `non_nil_type` strips
        // `UndefinedObject` / `Nil` from a union and returns other types
        // unchanged (matches the `isNil ifFalse:` narrowing — BT-2048).
        let non_nil_ty = Self::non_nil_type(receiver_ty);

        // Positions of the `ifNotNil:` block in the argument list per selector.
        let not_nil_index: Option<usize> = match WellKnownSelector::from_name(selector_name) {
            Some(WellKnownSelector::IfNilIfNotNil) => Some(1),
            Some(WellKnownSelector::IfNotNil | WellKnownSelector::IfNotNilIfNil) => Some(0),
            _ => None,
        };

        arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                if Some(i) == not_nil_index {
                    self.infer_if_not_nil_block(
                        arg,
                        receiver_ty,
                        &non_nil_ty,
                        hierarchy,
                        env,
                        in_abstract_method,
                    )
                } else {
                    // Preserve block-context inference for the `ifNil:` arm in
                    // `ifNil:ifNotNil:` / `ifNotNil:ifNil:` — a bare
                    // `infer_expr` would drop the `Block(..., R)` return type,
                    // degrading the whole send on statically-known receivers.
                    let inner = arg.unwrap_parens();
                    if let Expression::Block(block) = inner {
                        self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &[],
                            hierarchy,
                            env,
                            in_abstract_method,
                        )
                    } else {
                        self.infer_expr(arg, hierarchy, env, in_abstract_method)
                    }
                }
            })
            .collect()
    }

    /// Infer the `ifNotNil:` block, typing its first parameter (if any) as the
    /// non-nil branch of the receiver's type.
    ///
    /// Falls back to the normal expression inference path when the argument
    /// isn't a block literal (e.g. `receiver ifNotNil: aSymbol` is legal but
    /// non-local-inferable here).
    pub(in crate::semantic_analysis::type_checker) fn infer_if_not_nil_block(
        &mut self,
        arg: &Expression,
        receiver_ty: &InferredType,
        non_nil_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        // Unwrap parens: `ifNotNil: ([:x | ...])` should narrow the same as
        // the unparenthesised form.
        let inner = arg.unwrap_parens();
        let Expression::Block(block) = inner else {
            return self.infer_expr(arg, hierarchy, env, in_abstract_method);
        };

        // Zero-arity `ifNotNil: [ ... ]` — still call the typed-param helper
        // with an empty param list so the returned `Block(..., R)` type
        // carries the body's return type (consistent with
        // `infer_block_with_narrowing`; relevant for BT-2047).
        let param_types: Vec<InferredType> = if block.parameters.is_empty() {
            vec![]
        } else {
            // If the RECEIVER is nil-only (e.g. receiver is a literal `nil`,
            // `UndefinedObject | Nil`), the block is dead code. Check against
            // the original receiver — `non_nil_type` collapses a nil-only
            // union to `Dynamic(Unknown)`, so checking `non_nil_ty` here would
            // miss the case. Leave the param as `Dynamic(UnannotatedParam)`
            // so unreachable bodies still compile without DNU noise.
            let first_param_ty = if Self::is_nil_only(receiver_ty) {
                InferredType::Dynamic(DynamicReason::UnannotatedParam)
            } else {
                non_nil_ty.clone()
            };
            let mut types = Vec::with_capacity(block.parameters.len());
            types.push(first_param_ty);
            // Any additional params beyond the first stay Dynamic. A
            // well-formed `ifNotNil:` block has 0 or 1 parameter (validated by
            // `validate_if_not_nil_block` in codegen), so this is defensive.
            for _ in 1..block.parameters.len() {
                types.push(InferredType::Dynamic(DynamicReason::UnannotatedParam));
            }
            types
        };

        self.infer_block_with_typed_params(
            block,
            arg.span(),
            &param_types,
            hierarchy,
            env,
            in_abstract_method,
        )
    }

    /// BT-2047: Compute the return type of `ifNil:ifNotNil:` /
    /// `ifNotNil:ifNil:` as the union of both branch bodies' return types.
    ///
    /// `arg_types` must be the pair of `Block(..., R)` types produced by
    /// [`Self::infer_args_for_if_not_nil`]; the last type-arg of each Block is
    /// the branch body's inferred return type. A block literal containing a
    /// non-local return (`^`) exits the enclosing method, so its branch
    /// contributes `Never` to the union regardless of the returned value's
    /// type — matching the semantics noted in the issue's AC #3.
    ///
    /// Returns `None` if either arg isn't a well-formed `Block(...)` (e.g. the
    /// caller passed a symbol or bare value instead of a block literal), so
    /// the caller falls back to the generic method-lookup path for those cases.
    pub(in crate::semantic_analysis::type_checker) fn if_nil_branch_union_ret_ty(
        arguments: &[Expression],
        arg_types: &[InferredType],
    ) -> Option<InferredType> {
        if arg_types.len() < 2 || arguments.len() < 2 {
            return None;
        }
        let branch_ret = |arg: &Expression, ty: &InferredType| -> Option<InferredType> {
            let InferredType::Known {
                class_name,
                type_args,
                ..
            } = ty
            else {
                return None;
            };
            if class_name.as_str() != "Block" {
                return None;
            }
            // Non-local `^` anywhere inside the branch (including nested
            // sub-expressions like `[[^1] value]` or `foo: (^bar)`) exits
            // the method before the expression value is observed — treat
            // the branch as Never so `union_of` skips it.
            if let Expression::Block(block) = arg.unwrap_parens() {
                if block_has_any_return(block) {
                    return Some(InferredType::Never);
                }
            }
            type_args.last().cloned()
        };
        let a = branch_ret(&arguments[0], &arg_types[0])?;
        let b = branch_ret(&arguments[1], &arg_types[1])?;
        Some(InferredType::union_of(&[a, b]))
    }

    /// BT-2824: Compute the return type of a solo `ifNil:` / `ifNotNil:` send
    /// on a `T | Nil` union receiver as the union of the "self" branch
    /// (executed when the nil-check condition doesn't hold) and the block
    /// branch's inferred return type `R`.
    ///
    /// For `ifNil:`, the self branch is the receiver's non-nil type `T`
    /// (`Object>>ifNil:` returns `Self` when not nil) and the block branch is
    /// the nil block's `R`. For `ifNotNil:`, the self branch is `Nil`
    /// (`UndefinedObject>>ifNotNil:` returns `self`) and the block branch is
    /// the not-nil block's `R` (already narrowed to the non-nil receiver type
    /// by `infer_if_not_nil_block`).
    ///
    /// Only fires when `receiver_ty` is actually a `Nil`-containing union — a
    /// plain `Known` receiver (nilable or not) falls through to the
    /// pre-existing dispatch path unchanged, since the "impossible" branch
    /// can't be ruled out generically for those without risking a
    /// false-positive widening (e.g. `NonNilT ifNotNil: [...]` must not gain
    /// a spurious `Nil` member).
    ///
    /// The "self branch" semantics (`Object>>ifNil: -> Self`,
    /// `UndefinedObject>>ifNotNil: -> Nil`) are verified against the actual
    /// resolved stdlib signature rather than assumed, so a future edit to
    /// either method's declared return type in `object.bt` / `undefined_object.bt`
    /// falls back to the generic dispatch path instead of silently going stale.
    ///
    /// Returns `None` when the block argument isn't a well-formed `Block(...)`
    /// type, the receiver doesn't qualify, or the stdlib contract this
    /// function relies on no longer matches, so the caller falls back to the
    /// generic dispatch path for those cases.
    pub(in crate::semantic_analysis::type_checker) fn if_nil_solo_union_ret_ty(
        selector_name: &str,
        receiver_ty: &InferredType,
        arguments: &[Expression],
        arg_types: &[InferredType],
        hierarchy: &ClassHierarchy,
    ) -> Option<InferredType> {
        let InferredType::Union { members, .. } = receiver_ty else {
            return None;
        };
        let has_nil = members.iter().any(|m| {
            m.as_known().is_some_and(|n| {
                WellKnownClass::from_str(n).is_some_and(WellKnownClass::is_nil_class)
            })
        });
        if !has_nil {
            return None;
        }
        let non_nil_ty = Self::non_nil_type(receiver_ty);
        if matches!(non_nil_ty, InferredType::Dynamic(_)) {
            return None;
        }

        let arg = arguments.first()?;
        let ty = arg_types.first()?;
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = ty
        else {
            return None;
        };
        if class_name.as_str() != "Block" {
            return None;
        }
        let block_ret = if let Expression::Block(block) = arg.unwrap_parens() {
            if block_has_any_return(block) {
                InferredType::Never
            } else {
                type_args.last().cloned()?
            }
        } else {
            type_args.last().cloned()?
        };

        let self_branch = match WellKnownSelector::from_name(selector_name) {
            Some(WellKnownSelector::IfNil) => {
                let ret_ty = hierarchy
                    .find_method(
                        WellKnownClass::Object.as_str(),
                        WellKnownSelector::IfNil.as_str(),
                    )?
                    .return_type?;
                if !matches!(ret_ty, DeclaredType::SelfType) {
                    return None;
                }
                non_nil_ty
            }
            Some(WellKnownSelector::IfNotNil) => {
                let ret_ty = hierarchy
                    .find_method(
                        WellKnownClass::UndefinedObject.as_str(),
                        WellKnownSelector::IfNotNil.as_str(),
                    )?
                    .return_type?;
                let is_nil_class = matches!(&ret_ty, DeclaredType::Simple(n) if WellKnownClass::from_str(n).is_some_and(WellKnownClass::is_nil_class));
                if !is_nil_class {
                    return None;
                }
                InferredType::known(WellKnownClass::UndefinedObject.as_str())
            }
            _ => return None,
        };
        Some(InferredType::union_of(&[self_branch, block_ret]))
    }

    /// BT-2868: Compute the return type of a solo `ifTrue:` / `ifFalse:` send
    /// on a receiver whose type is exactly `Boolean` as the union of the
    /// "self" branch (the sibling `True`/`False` case that does not invoke
    /// the block, and thus statically returns `self` typed as `Boolean`) and
    /// the block branch's inferred return type `R`. Mirrors
    /// [`Self::if_nil_solo_union_ret_ty`]'s fix for `ifNil:`/`ifNotNil:`.
    ///
    /// `Boolean>>ifTrue:`/`ifFalse:` deliberately declare no `-> R` (see
    /// `stdlib/src/boolean.bt`): on an unnarrowed `Boolean` receiver the
    /// checker can't statically prove whether `True` or `False` handles the
    /// send, and the sibling override (e.g. `False>>ifTrue:`) never invokes
    /// the block — it returns `self`. Only fires for `class_name ==
    /// "Boolean"` exactly: `True`/`False` both override `ifTrue:`/`ifFalse:`
    /// with concrete declared return types, so a receiver already narrowed
    /// to either resolves through the normal `Some(ret_ty)` path and never
    /// reaches this helper.
    ///
    /// The "self branch is `Boolean`" semantics are verified against the
    /// actual resolved stdlib signature rather than assumed — mirroring
    /// [`Self::if_nil_solo_union_ret_ty`]'s staleness guard — so a future
    /// edit that gives `Boolean>>ifTrue:`/`ifFalse:` a declared return type
    /// falls back to the generic dispatch path instead of silently
    /// double-unioning with an already-resolved `R`.
    ///
    /// Returns `None` when the block argument isn't a well-formed `Block(...)`
    /// type, `class_name` isn't `Boolean`, or the stdlib contract this
    /// function relies on no longer matches, so the caller falls back to the
    /// generic Dynamic classification for those cases.
    ///
    /// Soundness also depends on a second, closed-world assumption that this
    /// function does *not* verify structurally: that `True` and `False` are
    /// the *only* concrete classes that can appear at runtime under a
    /// `Boolean`-typed receiver. That's what makes the "self branch returns
    /// `Boolean`" reasoning above valid — if a third subclass existed and
    /// overrode `ifTrue:`/`ifFalse:` to return something outside `True |
    /// False | R`, the inferred union here would be unsound for it. This is
    /// enforced by `Boolean` being declared `sealed` in `stdlib/src/boolean.bt`
    /// (BT-2886), which closes it to exactly its two existing `sealed`
    /// subclasses, `True` and `False`. Unlike the `ifNil:`/`ifNotNil:`
    /// self-branch check above, there's no `hierarchy.find_method(...)` guard
    /// for this half of the assumption — it relies on the parser/semantic
    /// analysis rejecting any attempt to subclass a `sealed` class.
    pub(in crate::semantic_analysis::type_checker) fn if_true_false_solo_boolean_ret_ty(
        selector_name: &str,
        class_name: &str,
        arguments: &[Expression],
        arg_types: &[InferredType],
        hierarchy: &ClassHierarchy,
    ) -> Option<InferredType> {
        if !matches!(
            WellKnownSelector::from_name(selector_name),
            Some(WellKnownSelector::IfTrue | WellKnownSelector::IfFalse)
        ) {
            return None;
        }
        if WellKnownClass::from_str(class_name) != Some(WellKnownClass::Boolean) {
            return None;
        }
        if hierarchy
            .find_method(WellKnownClass::Boolean.as_str(), selector_name)?
            .return_type
            .is_some()
        {
            return None;
        }

        let arg = arguments.first()?;
        let ty = arg_types.first()?;
        let InferredType::Known {
            class_name: block_class,
            type_args,
            ..
        } = ty
        else {
            return None;
        };
        if block_class.as_str() != "Block" {
            return None;
        }
        // `block_has_any_return` is a conservative over-approximation: it
        // reports `true` whenever a `^` appears anywhere in the block body
        // (nested block literals are opaque to it, so a `^` buried inside
        // one is invisible), not only when the block is guaranteed to always
        // exit the method on every path. Some blocks that may-but-not-always
        // diverge get widened to `Never` here — an accepted imprecision this
        // shares with the mirror function, [`Self::if_nil_solo_union_ret_ty`].
        let block_ret = if let Expression::Block(block) = arg.unwrap_parens() {
            if block_has_any_return(block) {
                InferredType::Never
            } else {
                type_args.last().cloned()?
            }
        } else {
            type_args.last().cloned()?
        };

        Some(InferredType::union_of(&[
            InferredType::known(WellKnownClass::Boolean.as_str()),
            block_ret,
        ]))
    }

    /// Infer argument types for `ifTrue:` / `ifFalse:` / `ifTrue:ifFalse:` with
    /// narrowed type environments for block arguments.
    ///
    /// For `ifTrue:`, the true-block gets the narrowed type.
    /// For `ifFalse:`, the false-block gets the complement (non-nil for nil checks).
    /// For `ifTrue:ifFalse:`, both blocks get their respective narrowings.
    #[allow(clippy::too_many_lines)]
    pub(in crate::semantic_analysis::type_checker) fn infer_args_with_narrowing(
        &mut self,
        arguments: &[Expression],
        selector_name: &str,
        info: &NarrowingInfo,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        let mut arg_types = Vec::new();

        match WellKnownSelector::from_name(selector_name) {
            Some(WellKnownSelector::IfTrue) => {
                // Single argument: narrow in the true branch
                if let Some(arg) = arguments.first() {
                    let ty = self.infer_block_with_narrowing(
                        arg,
                        &info.variable,
                        &info.true_type,
                        hierarchy,
                        env,
                        in_abstract_method,
                    );
                    arg_types.push(ty);
                }
            }
            Some(WellKnownSelector::IfFalse) => {
                // Single argument: narrow in the false branch (complement)
                if let Some(arg) = arguments.first() {
                    if let Some(ref false_ty) = info.false_type {
                        // Explicit false type (e.g., Result isOk/isError — BT-1859;
                        // singleton (in)equality complement — BT-2617; or
                        // class = / isKindOf: nominal-class complement — BT-2744)
                        let ty = self.infer_block_with_narrowing(
                            arg,
                            &info.variable,
                            false_ty,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else if info.is_nil_check {
                        // isNil ifFalse: → variable is non-nil
                        let current_ty = Self::resolve_narrowing_variable_type(
                            &info.variable,
                            env,
                            hierarchy,
                            self.alias_registry.as_ref(),
                        );
                        let non_nil = Self::non_nil_type(&current_ty);
                        let ty = self.infer_block_with_narrowing(
                            arg,
                            &info.variable,
                            &non_nil,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else {
                        // respondsTo: ifFalse: → no useful narrowing. (isKindOf:
                        // now populates `false_type` above — BT-2744; class =:=
                        // still leaves it None, since its false branch can't be
                        // narrowed via subtree exclusion.)
                        //
                        // BT-2868: still preserve the block's own return type
                        // as `Block(..., R)` (mirrors BT-2020's rationale for
                        // the narrowed branches above) — without this, a solo
                        // `respondsTo: ifFalse: [...]` on `Boolean` lost `R`
                        // entirely and `if_true_false_solo_boolean_ret_ty`
                        // couldn't union it with the `Boolean` self-branch.
                        let ty = if let Expression::Block(block) = arg.unwrap_parens() {
                            self.infer_block_with_typed_params(
                                block,
                                arg.span(),
                                &[],
                                hierarchy,
                                env,
                                in_abstract_method,
                            )
                        } else {
                            self.infer_expr(arg, hierarchy, env, in_abstract_method)
                        };
                        arg_types.push(ty);
                    }
                }
            }
            Some(WellKnownSelector::IfTrueIfFalse) => {
                // Two arguments: true block then false block
                if let Some(true_arg) = arguments.first() {
                    let ty = self.infer_block_with_narrowing(
                        true_arg,
                        &info.variable,
                        &info.true_type,
                        hierarchy,
                        env,
                        in_abstract_method,
                    );
                    arg_types.push(ty);
                }
                if let Some(false_arg) = arguments.get(1) {
                    if let Some(ref false_ty) = info.false_type {
                        // Explicit false type (e.g., Result isOk/isError — BT-1859;
                        // singleton (in)equality complement — BT-2617; or
                        // class = / isKindOf: nominal-class complement — BT-2744)
                        let ty = self.infer_block_with_narrowing(
                            false_arg,
                            &info.variable,
                            false_ty,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else if info.is_nil_check {
                        // isNil ifTrue: [...] ifFalse: [block] → non-nil in false block
                        let current_ty = Self::resolve_narrowing_variable_type(
                            &info.variable,
                            env,
                            hierarchy,
                            self.alias_registry.as_ref(),
                        );
                        let non_nil = Self::non_nil_type(&current_ty);
                        let ty = self.infer_block_with_narrowing(
                            false_arg,
                            &info.variable,
                            &non_nil,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else {
                        // respondsTo: ifTrue: [...] ifFalse: [...] — no useful
                        // narrowing for false block. (isKindOf: now populates
                        // `false_type` above — BT-2744; class =:= still leaves
                        // it None, since its false branch can't be narrowed via
                        // subtree exclusion.)
                        let ty = self.infer_expr(false_arg, hierarchy, env, in_abstract_method);
                        arg_types.push(ty);
                    }
                }
                // Handle any remaining arguments (shouldn't happen, but be safe)
                for arg in arguments.iter().skip(2) {
                    arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
                }
            }
            _ => {
                // Fallback: no narrowing
                for arg in arguments {
                    arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
                }
            }
        }

        arg_types
    }

    /// Type-check a block expression (or any expression) with a variable narrowed
    /// to a specific type in a child environment.
    ///
    /// BT-2020: Preserves the block body's inferred return type as a `type_arg`
    /// on the returned `Block(..., R)` type. Without this, `ifTrue:ifFalse:`
    /// return types collapsed to `Dynamic` because `infer_method_local_params`
    /// requires the Block argument to carry its return type before it can unify
    /// the method-local `R` type parameter.
    pub(in crate::semantic_analysis::type_checker) fn infer_block_with_narrowing(
        &mut self,
        arg: &Expression,
        var_key: &EnvKey,
        narrowed_type: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        if let Expression::Block(block) = arg {
            let mut block_env = env.child();
            // BT-2050: narrowing uses the unified refinement API; the layer
            // is block-scoped because the child env is dropped on return.
            block_env.push_refinement(RefinementLayer::block_scope(
                var_key.clone(),
                narrowed_type.clone(),
            ));
            // Block parameters are unannotated in the narrowing context (the
            // selectors we enter here — ifTrue:/ifFalse:/ifTrue:ifFalse: —
            // take zero-arity blocks, but be defensive for any future use).
            let mut block_param_types: Vec<InferredType> =
                Vec::with_capacity(block.parameters.len());
            for param in &block.parameters {
                let param_ty = InferredType::Dynamic(DynamicReason::UnannotatedParam);
                block_env.set_local(param.name.clone(), param_ty.clone());
                block_param_types.push(param_ty);
            }
            let body_ty =
                self.infer_stmts(&block.body, hierarchy, &mut block_env, in_abstract_method);
            // Build `Block(P1, ..., Pn, R)` so downstream generic inference
            // (e.g. `infer_method_local_params` matching `Block(R) -> R`) can
            // recover the block's return type.
            let mut block_type_args = block_param_types;
            block_type_args.push(body_ty);
            let ty = InferredType::Known {
                class_name: "Block".into(),
                type_args: block_type_args,
                provenance: crate::semantic_analysis::type_checker::TypeProvenance::Inferred(
                    arg.span(),
                ),
            };
            self.type_map.insert(arg.span(), ty.clone());
            ty
        } else {
            // Not a block literal — just infer normally
            self.infer_expr(arg, hierarchy, env, in_abstract_method)
        }
    }

    /// Finds the `Block(...)` arm of a declared type (BT-2864), returning its
    /// type parameters (the block's own param types plus trailing return
    /// type — e.g. `[A, B, Boolean]` for `Block(A, B, Boolean)`).
    ///
    /// Matches either a bare `Block(...)` type, or — when the declared type
    /// is a Union — the single `Block(...)` member among its arms (e.g.
    /// `Block(A, B) | Handler | Router`). Without this, only a bare
    /// `Block(...)` param type propagated its expected signature into a
    /// block-literal argument; a Union-typed param fell back to typing every
    /// block param as `Dynamic`, even when exactly one arm was a `Block(...)`
    /// whose signature could unambiguously be used.
    ///
    /// Returns `None` if no arm is a `Block(...)` type, or if more than one
    /// arm is (ambiguous — which signature would apply is unclear, so this
    /// conservatively falls back to the pre-fix Dynamic behaviour rather than
    /// guessing).
    ///
    /// BT-3076: matches structurally on [`DeclaredType`] rather than
    /// re-parsing a rendered string — a `Union`'s members are already the
    /// flattened top-level arms (mirroring `TypeAnnotation::Union`), so no
    /// depth-aware string split is needed to avoid a nested-Union
    /// mis-detection the way the pre-BT-3076 string version required.
    ///
    /// `pub(super)` so `type_checker::tests` can unit-test this directly
    /// rather than only indirectly through diagnostics (a Dynamic receiver
    /// never fires DNU, so a diagnostics-only test can't distinguish a
    /// correctly-typed block param from a Dynamic one for every arm shape).
    pub(in crate::semantic_analysis::type_checker) fn find_block_arm(
        dt: &DeclaredType,
    ) -> Option<&[DeclaredType]> {
        match dt {
            DeclaredType::Generic { base, parameters } if base == "Block" => Some(parameters),
            DeclaredType::Union(members) => {
                let mut found: Option<&[DeclaredType]> = None;
                for member in members {
                    if let DeclaredType::Generic { base, parameters } = member {
                        if base == "Block" {
                            if found.is_some() {
                                return None;
                            }
                            found = Some(parameters);
                        }
                    }
                }
                found
            }
            _ => None,
        }
    }

    /// Infer argument types, propagating block parameter types from the callee
    /// method's signature when the receiver type is known.
    ///
    /// For example, `List(String)>>sort:` declares `Block(E, E, Boolean)`.
    /// With E=String (from receiver type args), block params get typed as String
    /// instead of `Dynamic(UnannotatedParam)`.
    ///
    /// **BT-2042:** When the receiver is Dynamic (or the method can't be resolved),
    /// block arguments still need their parameters typed — otherwise each unannotated
    /// block param defaults to `Dynamic(UnannotatedParam)`, which fires the
    /// "expression inferred as Dynamic in typed class" warning at every use of the
    /// block param. In a `typed` class this forces users to annotate every block
    /// parameter whose upstream iterable happens to be Dynamic, even though the
    /// root cause is the Dynamic receiver, not the block itself. We propagate
    /// `Dynamic(DynamicReceiver)` into block params in the fallback paths so
    /// downstream uses propagate that reason (which is filtered from the warning),
    /// matching how the send result itself is already classified.
    #[allow(clippy::too_many_arguments)] // class-side flag (BT-2158) added to existing 7 args
    #[allow(clippy::too_many_lines)] // two-phase block-arg inference adds necessary branches
    pub(in crate::semantic_analysis::type_checker) fn infer_args_with_block_context(
        &mut self,
        arguments: &[Expression],
        receiver_ty: &InferredType,
        selector_name: &str,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
        is_class_side_send: bool,
    ) -> Vec<InferredType> {
        // ADR 0083: a metatype receiver `Meta{C}` is class-side — normalize it
        // to a bare `Known{C}` so the method lookup below resolves `C`'s
        // class-side methods (the class object is unparameterized, so no type
        // args). The `is_class_side_send` flag is already set by the caller.
        let normalized_receiver: InferredType;
        let receiver_ty = if let InferredType::Meta { class_name, .. } = receiver_ty {
            normalized_receiver = InferredType::known(class_name.clone());
            &normalized_receiver
        } else {
            receiver_ty
        };

        // BT-2868: a `Union`-typed receiver sending a solo `ifTrue:` /
        // `ifFalse:` still needs its (zero-arity) block argument's own
        // return type preserved as `Block(..., R)`, even though there's no
        // single declared method signature to resolve block *param* types
        // from — these selectors' blocks never take params anyway. Without
        // this, `infer_union_message_send`'s `Boolean` member fallback
        // (`if_true_false_solo_boolean_ret_ty`) never sees an `R` to union
        // with `Boolean`, since the generic `Known`-only fast path below is
        // skipped for a `Union` receiver. Mirrors the zero-arg `Block(R)`
        // handling in the `Known`-receiver path further down.
        if matches!(
            WellKnownSelector::from_name(selector_name),
            Some(WellKnownSelector::IfTrue | WellKnownSelector::IfFalse)
        ) {
            if let InferredType::Union { .. } = receiver_ty {
                return arguments
                    .iter()
                    .map(|arg| {
                        if let Expression::Block(block) = arg.unwrap_parens() {
                            self.infer_block_with_typed_params(
                                block,
                                arg.span(),
                                &[],
                                hierarchy,
                                env,
                                in_abstract_method,
                            )
                        } else {
                            self.infer_expr(arg, hierarchy, env, in_abstract_method)
                        }
                    })
                    .collect();
            }
        }

        // BT-3463: a `Union`-typed receiver (e.g. the `V | T` widened result of
        // `Dictionary>>at:ifAbsent:`, BT-3408) still has a resolvable method
        // signature on each of its members — resolve declared `Block(...)`
        // param types per member and merge them, instead of falling through
        // to the `Dynamic`-receiver fallback below (which types every block
        // param `Dynamic(UnannotatedParam)`, a reason the BT-1914 lint does
        // not filter). Handles every argument (block and non-block alike),
        // so it always returns rather than conditionally falling through —
        // falling through here would re-run `infer_expr` on non-block
        // arguments already inferred inside it, double-emitting diagnostics.
        if let InferredType::Union { members, .. } = receiver_ty {
            return self.resolve_union_block_param_types(
                members,
                arguments,
                selector_name,
                hierarchy,
                env,
                in_abstract_method,
                is_class_side_send,
            );
        }

        // Fast path: receiver must be Known to look up method signatures.
        // For non-Known receivers (Dynamic, Never, etc.), we can't resolve block
        // param types from the signature — but we can still propagate a reason-
        // preserving type into the block params so their uses don't re-fire the
        // "Dynamic in typed class" warning (BT-2042).
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = receiver_ty
        else {
            return self.infer_args_with_dynamic_block_params(
                arguments,
                receiver_ty,
                hierarchy,
                env,
                in_abstract_method,
            );
        };

        // Look up the method to get param types. For class-side sends
        // (`ClassName foo:` or `self foo:` inside a class method), look up the
        // class-side method; otherwise the instance method. BT-2158: without
        // this split, class-side block parameters never get their declared
        // types propagated to the call-site block params.
        let method_lookup = if is_class_side_send {
            hierarchy.find_class_method(class_name, selector_name)
        } else {
            hierarchy.find_method(class_name, selector_name)
        };
        let Some(method) = method_lookup else {
            return self.infer_args_with_dynamic_block_params(
                arguments,
                receiver_ty,
                hierarchy,
                env,
                in_abstract_method,
            );
        };

        // Check if any param type is a Block(...) type, or a Union containing
        // one (BT-2864: e.g. `Block(A, B) | Handler | Router`).
        let has_block_param = method.param_types.iter().any(|pt| {
            pt.as_ref()
                .is_some_and(|t| Self::find_block_arm(t).is_some())
        });
        if !has_block_param {
            return arguments
                .iter()
                .map(|arg| self.infer_expr(arg, hierarchy, env, in_abstract_method))
                .collect();
        }

        // Phase 1: Infer non-block arguments to resolve method-local type params
        let mut arg_types: Vec<InferredType> = Vec::with_capacity(arguments.len());
        for arg in arguments {
            if matches!(arg, Expression::Block(_)) {
                // Placeholder — will be re-inferred in Phase 2
                arg_types.push(InferredType::known("Block"));
            } else {
                arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
            }
        }

        // Build substitution maps
        let class_subst = Self::build_inherited_substitution_map(
            hierarchy,
            class_name,
            type_args,
            &method.defined_in,
        );
        let method_subst = Self::infer_method_local_params(
            &method,
            &arg_types,
            &class_subst,
            hierarchy,
            &method.defined_in,
        );

        // Phase 2: Re-infer block arguments with typed params
        for (i, arg) in arguments.iter().enumerate() {
            if let Expression::Block(block) = arg {
                let block_arm = method
                    .param_types
                    .get(i)
                    .and_then(|pt| pt.as_ref())
                    .and_then(Self::find_block_arm);

                if let Some(type_params) = block_arm {
                    // Block(X, Y, Z) → params = [X, Y], return = Z

                    if type_params.len() >= 2 {
                        // All but last are block parameter types, last is return type
                        let block_param_types: Vec<InferredType> = type_params
                            [..type_params.len() - 1]
                            .iter()
                            .map(|p| {
                                Self::resolve_type_param(p, &class_subst, &method_subst, hierarchy)
                            })
                            .collect();

                        arg_types[i] = self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &block_param_types,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                    } else {
                        // BT-2020: Block(R) — zero-arity block with a return type param.
                        // Use `infer_block_with_typed_params` with no param types so the
                        // returned Block type_args preserve the body's return type (R).
                        // Without this, the bare `Block` returned by `infer_expr` gives
                        // `infer_method_local_params` nothing to unify `R` against, and
                        // callers see the method's return type as `Dynamic`.
                        arg_types[i] = self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &[],
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                    }
                } else {
                    // No Block(...) param type for this position
                    arg_types[i] = self.infer_expr(arg, hierarchy, env, in_abstract_method);
                }
            }
        }

        arg_types
    }

    /// BT-3463: resolve declared block-parameter types for each block-typed
    /// argument in a message sent to a `Union`-typed receiver, mirroring the
    /// `Known`-receiver push-down above but merged across the union's members.
    ///
    /// For each block argument position, every union member that is `Known`,
    /// resolves the selector to a method, and declares a `Block(...)`
    /// parameter at that position contributes its (generically-substituted)
    /// block param types — computed the same way as the `Known`-receiver fast
    /// path (`class_subst` + `infer_method_local_params`). Members that don't
    /// resolve (Dynamic, unresolvable, non-responding, `UndefinedObject`/nil)
    /// simply don't contribute, mirroring `infer_union_message_send`'s
    /// treatment of those cases for the return type.
    ///
    /// Non-block arguments are inferred once here (Phase 1, mirroring the
    /// `Known`-receiver path) and reused verbatim in the returned vector —
    /// they don't depend on which union member resolves the block, so
    /// re-inferring them per member (or in the caller) would double-emit any
    /// diagnostics their inference produces. This includes the common case
    /// of a `Union` receiver with no block arguments at all (e.g. a plain
    /// `unionResult includes: x` send): every argument is inferred exactly
    /// once here, in Phase 1, and Phase 2 below is then a no-op for them.
    /// Always returns the final per-argument inferred types — never `None`
    /// — so the caller must not re-infer any argument on top of this.
    ///
    /// `pub(super)` so `type_checker::tests` can unit-test the
    /// class-side/instance-side DNU-override branch directly — a `Union`
    /// receiver combined with `is_class_side_send: true` isn't reachable
    /// through today's other type-checker features (a class-side send's
    /// receiver is always a `ClassReference` or `self`, both of which are
    /// single, non-`Union` types), but the branch's correctness matters on
    /// its own regardless of reachability, matching `find_block_arm`'s
    /// rationale for the same `pub(super)` treatment.
    #[allow(clippy::too_many_arguments)] // mirrors infer_args_with_block_context's arg count
    #[allow(clippy::too_many_lines)] // per-member resolution + merge adds necessary branches
    pub(in crate::semantic_analysis::type_checker) fn resolve_union_block_param_types(
        &mut self,
        members: &[InferredType],
        arguments: &[Expression],
        selector_name: &str,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
        is_class_side_send: bool,
    ) -> Vec<InferredType> {
        // Phase 1 (receiver-independent): infer non-block arguments once so
        // method-local generic params can be resolved from them per member
        // below; block arguments get a placeholder, overwritten in Phase 2.
        let mut arg_types: Vec<InferredType> = Vec::with_capacity(arguments.len());
        for arg in arguments {
            if matches!(arg, Expression::Block(_)) {
                arg_types.push(InferredType::known("Block"));
            } else {
                arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
            }
        }

        // Per block-argument position: the block param type lists
        // contributed by each participating union member.
        let mut per_position: Vec<Vec<Vec<InferredType>>> = vec![Vec::new(); arguments.len()];

        for member in members {
            let InferredType::Known {
                class_name: member_name,
                type_args,
                ..
            } = member
            else {
                continue; // Dynamic / other non-Known member: no method to resolve.
            };
            // BT-2624: a singleton member (`#foo`) resolves through `Symbol`,
            // mirroring `infer_union_message_send`'s member resolution.
            let resolve_name: &str = if member_name.starts_with('#') {
                "Symbol"
            } else {
                member_name.as_str()
            };
            // BT-1857: Nil is expected to be guarded by isNil/notNil checks;
            // it never contributes a block-param resolution here either.
            if WellKnownClass::from_str(resolve_name).is_some_and(WellKnownClass::is_nil_class) {
                continue;
            }
            if !hierarchy.has_class(resolve_name) {
                continue;
            }
            // BT-3463 review: branch the DNU-override check on class-side vs
            // instance-side, mirroring the `find_class_method`/`find_method`
            // branch immediately below — a class-side-only or
            // instance-side-only override must only skip the matching send
            // kind, not both.
            let has_dnu_override = if is_class_side_send {
                hierarchy.has_class_dnu_override(resolve_name)
            } else {
                hierarchy.has_instance_dnu_override(resolve_name)
            };
            if has_dnu_override {
                continue;
            }
            let method_lookup = if is_class_side_send {
                hierarchy.find_class_method(resolve_name, selector_name)
            } else {
                hierarchy.find_method(resolve_name, selector_name)
            };
            let Some(method) = method_lookup else {
                continue;
            };
            let class_subst = Self::build_inherited_substitution_map(
                hierarchy,
                resolve_name,
                type_args,
                &method.defined_in,
            );
            let method_subst = Self::infer_method_local_params(
                &method,
                &arg_types,
                &class_subst,
                hierarchy,
                &method.defined_in,
            );

            for (i, arg) in arguments.iter().enumerate() {
                if !matches!(arg, Expression::Block(_)) {
                    continue;
                }
                let Some(type_params) = method
                    .param_types
                    .get(i)
                    .and_then(|pt| pt.as_ref())
                    .and_then(Self::find_block_arm)
                else {
                    continue;
                };
                let block_param_types: Vec<InferredType> = if type_params.len() >= 2 {
                    type_params[..type_params.len() - 1]
                        .iter()
                        .map(|p| {
                            Self::resolve_type_param(p, &class_subst, &method_subst, hierarchy)
                        })
                        .collect()
                } else {
                    // BT-2020: Block(R) — zero-arity block, no params to resolve.
                    Vec::new()
                };
                per_position[i].push(block_param_types);
            }
        }

        // Phase 2: merge each block argument's per-member contributions and
        // (re-)infer the block body with the merged param types. A position
        // with no usable merge (no contribution, or members disagreeing on
        // arity) falls back to plain `infer_expr`, matching the pre-existing
        // `infer_args_with_dynamic_block_params` behaviour for that argument.
        for (i, arg) in arguments.iter().enumerate() {
            if let Expression::Block(block) = arg {
                match Self::merge_union_block_param_types(&per_position[i]) {
                    Some(param_types) => {
                        arg_types[i] = self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &param_types,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                    }
                    None => {
                        arg_types[i] = self.infer_expr(arg, hierarchy, env, in_abstract_method);
                    }
                }
            }
        }

        arg_types
    }

    /// Merge per-member block-param-type contributions for a single block
    /// argument position (see [`Self::resolve_union_block_param_types`]).
    ///
    /// `None` when no member contributed a param list for this position, or
    /// members disagree on arity (can't merge positionally). Otherwise
    /// `Some(merged)`, one entry per parameter slot: the single concrete type
    /// every contributing member agrees on — a `Dynamic` contribution (e.g.
    /// from BT-3408's `at:ifAbsent: [#()]` widening, whose empty-literal
    /// fallback member resolves to `Dynamic`) never blocks agreement, since
    /// it carries no positive information; only two *concrete* types
    /// disagreeing does. Falls back to `Dynamic(UnannotatedParam)` when no
    /// member contributed a concrete type, or concrete types genuinely
    /// disagree.
    pub(in crate::semantic_analysis::type_checker) fn merge_union_block_param_types(
        contributions: &[Vec<InferredType>],
    ) -> Option<Vec<InferredType>> {
        let arity = contributions.first()?.len();
        if contributions.iter().any(|c| c.len() != arity) {
            return None;
        }
        Some(
            (0..arity)
                .map(|slot| {
                    let mut concrete: Option<&InferredType> = None;
                    let mut disagreement = false;
                    for c in contributions {
                        let ty = &c[slot];
                        if matches!(ty, InferredType::Dynamic(_)) {
                            continue;
                        }
                        match concrete {
                            None => concrete = Some(ty),
                            Some(existing) if existing == ty => {}
                            Some(_) => disagreement = true,
                        }
                    }
                    match concrete {
                        Some(ty) if !disagreement => ty.clone(),
                        _ => InferredType::Dynamic(DynamicReason::UnannotatedParam),
                    }
                })
                .collect(),
        )
    }

    /// Fallback variant of [`Self::infer_args_with_block_context`] used when the
    /// receiver type isn't `Known` (Dynamic/Never/Union/…) or when the selector
    /// can't be resolved on the receiver.
    ///
    /// Walks each argument via `infer_expr`, **except** for block literals: those
    /// are walked with their parameters pre-bound to `Dynamic(DynamicReceiver)`
    /// so that usages inside the block body inherit a "propagated Dynamic" reason
    /// (which is filtered out of the BT-1914 "Dynamic in typed class" warning).
    /// Without this step, each block param would default to
    /// `Dynamic(UnannotatedParam)`, re-firing the warning at every use of the
    /// block param inside a `typed` class — see BT-2042.
    ///
    /// The chosen reason follows the send's result classification at line
    /// `infer_message_send_with_receiver_ty` fallback (see `Dynamic(DynamicReceiver)`
    /// returns): the send's result is already classified that way for the same
    /// root cause, so block params inherit the same provenance for consistency.
    pub(in crate::semantic_analysis::type_checker) fn infer_args_with_dynamic_block_params(
        &mut self,
        arguments: &[Expression],
        receiver_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        // Only propagate DynamicReceiver when the receiver actually is Dynamic.
        // For Never / Union / other shapes, fall back to the plain walk so we
        // don't mask unrelated root causes.
        let propagate_dynamic = matches!(receiver_ty, InferredType::Dynamic(_));

        arguments
            .iter()
            .map(|arg| {
                // Unwrap parens so `foo: ([:x | ...])` gets the same
                // Dynamic(DynamicReceiver) propagation as the unparenthesised
                // `foo: [:x | ...]` form.
                let inner = arg.unwrap_parens();
                if let Expression::Block(block) = inner {
                    if propagate_dynamic {
                        let param_types: Vec<InferredType> = (0..block.parameters.len())
                            .map(|_| InferredType::Dynamic(DynamicReason::DynamicReceiver))
                            .collect();
                        return self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &param_types,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                    }
                }
                self.infer_expr(arg, hierarchy, env, in_abstract_method)
            })
            .collect()
    }

    /// Infer a block expression with typed parameters resolved from the callee
    /// method's signature.
    pub(in crate::semantic_analysis::type_checker) fn infer_block_with_typed_params(
        &mut self,
        block: &crate::ast::Block,
        block_span: Span,
        param_types: &[InferredType],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let mut block_env = env.child();
        for (param, ty) in block.parameters.iter().zip(param_types.iter()) {
            block_env.set_local(param.name.clone(), ty.clone());
        }
        // Extra params beyond resolved types stay Dynamic
        for param in block.parameters.iter().skip(param_types.len()) {
            block_env.set_local(
                param.name.clone(),
                InferredType::Dynamic(DynamicReason::UnannotatedParam),
            );
        }
        let body_ty = self.infer_stmts(&block.body, hierarchy, &mut block_env, in_abstract_method);
        // BT-2866: a block whose body ends in a top-level non-local return
        // (`^expr`) never produces a *local* value — it diverges out of the
        // enclosing method entirely, and `^expr`'s type naturally matches
        // whatever the enclosing method itself declares as its return type
        // (that's what makes `^` type-check). `infer_stmts` reports that
        // expression's type as the block's "body type" regardless, since for
        // an ordinary method body that IS the right answer. But for a block
        // passed to a combinator like `ifOk:ifError:` (`Block(T, R) ...
        // Block(E, R) -> R`), treating a non-local-return branch's body type
        // as a real contribution to `R` pollutes the union with the
        // *enclosing method's* return type — unrelated to what this block's
        // sibling argument (e.g. `ifOk:`'s block) actually produces locally.
        // Override to `Never` (the `union_of` identity element — see
        // `infer_method_local_params`/`merge_method_local_binding`) so only
        // branches that genuinely produce a local value participate in `R`.
        // `block_has_return` (not the deeper `block_has_any_return`) matches
        // `infer_stmts`'s own top-level-only `break` check above exactly —
        // a `^` buried inside a conditional the block might not take
        // shouldn't force the whole block to `Never`.
        let body_ty = if block_has_return(block) {
            InferredType::Never
        } else {
            body_ty
        };
        // Build Block type with resolved param types + inferred return type
        let mut block_type_args: Vec<InferredType> = param_types.to_vec();
        block_type_args.push(body_ty);
        let ty = InferredType::Known {
            class_name: "Block".into(),
            type_args: block_type_args,
            provenance: crate::semantic_analysis::type_checker::TypeProvenance::Inferred(
                block_span,
            ),
        };
        self.type_map.insert(block_span, ty.clone());
        ty
    }

    /// Check whether a type is *only* the nil type (`UndefinedObject` or
    /// its legacy `Nil` alias). Returns `true` for the bare nil type itself,
    /// or a union whose members are all nil. Used by the `ifNotNil:` block-
    /// param narrowing (BT-2046) to avoid typing the param as `UndefinedObject`
    /// when the non-nil branch is dead code.
    pub(in crate::semantic_analysis::type_checker) fn is_nil_only(ty: &InferredType) -> bool {
        match ty {
            InferredType::Known { class_name, .. } => {
                WellKnownClass::from_str(class_name).is_some_and(WellKnownClass::is_nil_class)
            }
            InferredType::Union { members, .. } => members.iter().all(|m| {
                m.as_known().is_some_and(|n| {
                    WellKnownClass::from_str(n).is_some_and(WellKnownClass::is_nil_class)
                })
            }),
            _ => false,
        }
    }
}

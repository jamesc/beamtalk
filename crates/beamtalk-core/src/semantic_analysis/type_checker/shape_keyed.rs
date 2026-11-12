// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4732): shape-aware keyed operations.
//!
//! Only these operations use a map shape's per-key types; every other message
//! sees the shape's `Dictionary(K, V)` view:
//!
//! | Operation | Required key | Optional key | Key not in shape |
//! |---|---|---|---|
//! | `at: #k` | `T` | `T` | closed: warning; open: `Object` |
//! | `at: #k ifAbsent: [b]` | `T` | `T \| B` | closed: warning; open: `Object \| B` |
//! | `at: #k ifPresent: [:v \| …]` | block gets `T` | block gets `T` | closed: warning; open: block gets `Object` |
//! | map pattern `#{#k => v}` | `v :: T` | `v :: T` | closed: warning; open: `v :: Object` |
//! | `at: #k put: v` | shape with `#k => typeof(v)`, required | same | shape gains `#k`, required |
//!
//! The key argument must have a singleton symbol type (`#k`); a union of
//! singletons answers the union of the per-key results; any other key falls
//! back to the `Dictionary` view (this module answers `None`).
//!
//! A union receiver answers the union of each member's result, with non-shape
//! members typed as they are today. The "key not in shape" warning fires only
//! when every member is a closed shape lacking the key.

use ecow::{EcoString, eco_format};

use crate::ast::{Expression, Literal, MapPatternKey, Pattern};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::string_utils::is_symbol_singleton_name;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

use super::{DynamicReason, InferredType, ShapeFieldType, TypeChecker, TypeEnv};

/// The keyed selectors that read or write a shape by key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum KeyedOp {
    At,
    AtIfAbsent,
    AtIfPresent,
    AtIfPresentIfAbsent,
    AtPut,
}

impl KeyedOp {
    pub(super) fn from_selector(selector: &str) -> Option<Self> {
        match selector {
            "at:" => Some(Self::At),
            "at:ifAbsent:" => Some(Self::AtIfAbsent),
            "at:ifPresent:" => Some(Self::AtIfPresent),
            "at:ifPresent:ifAbsent:" => Some(Self::AtIfPresentIfAbsent),
            "at:put:" => Some(Self::AtPut),
            _ => None,
        }
    }

    fn arity(self) -> usize {
        match self {
            Self::At => 1,
            Self::AtIfAbsent | Self::AtIfPresent | Self::AtPut => 2,
            Self::AtIfPresentIfAbsent => 3,
        }
    }

    fn is_if_present(self) -> bool {
        matches!(self, Self::AtIfPresent | Self::AtIfPresentIfAbsent)
    }
}

/// What a shape says about one key.
enum KeyLookup<'a> {
    /// The shape names the key.
    Field(&'a ShapeFieldType),
    /// An open shape that does not name the key: any value may be there.
    OpenUnnamed,
    /// A closed shape that does not name the key.
    ClosedMissing,
}

fn lookup<'a>(shape: &'a InferredType, key: &str) -> Option<KeyLookup<'a>> {
    let InferredType::Shape { fields, open, .. } = shape else {
        return None;
    };
    Some(match fields.iter().find(|f| f.key == key) {
        Some(field) => KeyLookup::Field(field),
        None if *open => KeyLookup::OpenUnnamed,
        None => KeyLookup::ClosedMissing,
    })
}

/// A receiver split into its shape members and its other (non-`Nil`) members.
struct SplitReceiver<'a> {
    shapes: Vec<&'a InferredType>,
    others: Vec<&'a InferredType>,
}

impl<'a> SplitReceiver<'a> {
    /// `None` when no member is a shape. `Nil` members are dropped: a keyed
    /// send to `nil` is the union machinery's `T | Nil` convention (guarded by
    /// `isNil` checks), not a member that widens the result.
    fn of(ty: &'a InferredType) -> Option<Self> {
        let members: Vec<&InferredType> = match ty {
            InferredType::Union { members, .. } => members.iter().collect(),
            other => vec![other],
        };
        let mut shapes = Vec::new();
        let mut others = Vec::new();
        for m in members {
            if matches!(m, InferredType::Shape { .. }) {
                shapes.push(m);
            } else if !m.as_known().is_some_and(|n| {
                super::well_known::WellKnownClass::from_str(n)
                    .is_some_and(super::well_known::WellKnownClass::is_nil_class)
            }) {
                others.push(m);
            }
        }
        (!shapes.is_empty()).then_some(Self { shapes, others })
    }

    /// Every shape is closed and no other member exists: the only receiver
    /// shape for which "key not in shape" can be asserted.
    fn all_closed_shapes(&self) -> bool {
        self.others.is_empty()
            && self
                .shapes
                .iter()
                .all(|s| matches!(s, InferredType::Shape { open: false, .. }))
    }
}

/// The key names of a singleton symbol type (`#k`) or a union of them.
fn singleton_keys(ty: &InferredType) -> Option<Vec<EcoString>> {
    match ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } if type_args.is_empty() && is_symbol_singleton_name(class_name) => {
            Some(vec![EcoString::from(&class_name[1..])])
        }
        InferredType::Union { members, .. } => {
            let mut keys = Vec::new();
            for m in members {
                keys.extend(singleton_keys(m)?);
            }
            (!keys.is_empty()).then_some(keys)
        }
        _ => None,
    }
}

/// The `V` of a plain `Dictionary(K, V)` member.
fn dictionary_value_type(ty: &InferredType) -> Option<InferredType> {
    match ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } if class_name == "Dictionary" && type_args.len() == 2 => Some(type_args[1].clone()),
        _ => None,
    }
}

fn object() -> InferredType {
    InferredType::known("Object")
}

impl TypeChecker {
    /// The key a keyed send's first argument names, if it is syntactically a
    /// symbol literal or a local whose type is a singleton symbol. Pure — used
    /// before the argument is inferred.
    fn syntactic_singleton_keys(arg: &Expression, env: &TypeEnv) -> Option<Vec<EcoString>> {
        match arg.unwrap_parens() {
            Expression::Literal(Literal::Symbol(name), _) => Some(vec![name.clone()]),
            Expression::Identifier(id) => singleton_keys(&env.get_local(&id.name)?),
            _ => None,
        }
    }

    /// The type a shape-aware `at:ifPresent:` / `at:ifPresent:ifAbsent:`
    /// block's parameter gets, or `None` to use the generic (Dictionary view)
    /// inference.
    fn shape_present_param_type(
        receiver_ty: &InferredType,
        keys: &[EcoString],
    ) -> Option<InferredType> {
        let split = SplitReceiver::of(receiver_ty)?;
        let mut parts = Vec::new();
        for key in keys {
            for shape in &split.shapes {
                match lookup(shape, key)? {
                    KeyLookup::Field(f) => parts.push(f.value.clone()),
                    KeyLookup::OpenUnnamed => parts.push(object()),
                    KeyLookup::ClosedMissing => {}
                }
            }
        }
        for other in &split.others {
            parts.push(dictionary_value_type(other)?);
        }
        (!parts.is_empty()).then(|| InferredType::union_of(&parts))
    }

    /// Argument inference for an `ifPresent:` keyed send on a shape receiver:
    /// the presence block's parameter is the key's value type. `None` when the
    /// send is not shape-aware (the caller then uses the generic path, so no
    /// argument is inferred twice).
    pub(super) fn infer_args_for_shape_if_present(
        &mut self,
        selector_name: &str,
        arguments: &[Expression],
        receiver_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Option<Vec<InferredType>> {
        let op = KeyedOp::from_selector(selector_name).filter(|op| op.is_if_present())?;
        if arguments.len() != op.arity() {
            return None;
        }
        let keys = Self::syntactic_singleton_keys(&arguments[0], env)?;
        let param_ty = Self::shape_present_param_type(receiver_ty, &keys)?;
        let mut types = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            let ty = match arg {
                Expression::Block(block) if i > 0 => {
                    let params: &[InferredType] = if i == 1 && block.parameters.len() == 1 {
                        std::slice::from_ref(&param_ty)
                    } else {
                        &[]
                    };
                    self.infer_block_with_typed_params(
                        block,
                        arg.span(),
                        params,
                        hierarchy,
                        env,
                        in_abstract_method,
                    )
                }
                _ => self.infer_expr(arg, hierarchy, env, in_abstract_method),
            };
            types.push(ty);
        }
        Some(types)
    }

    /// The result of a shape-aware keyed send, or `None` when the send is
    /// not shape-aware and the `Dictionary` view must answer it. Emits the
    /// "key not in shape" warning. The `ifPresent:` family only gets the
    /// warning here: its result (`R | Nil`) comes from the generic path.
    pub(super) fn infer_shape_keyed_send(
        &mut self,
        receiver_ty: &InferredType,
        selector_name: &str,
        arguments: &[Expression],
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) -> Option<InferredType> {
        let op = KeyedOp::from_selector(selector_name)?;
        if arguments.len() != op.arity() || arg_types.len() != op.arity() {
            return None;
        }
        let split = SplitReceiver::of(receiver_ty)?;
        let keys = singleton_keys(&arg_types[0])?;

        if op == KeyedOp::AtPut {
            let [key] = keys.as_slice() else {
                return None;
            };
            let mut parts: Vec<InferredType> = split
                .shapes
                .iter()
                .map(|s| Self::shape_with_key(s, key, &arg_types[1]))
                .collect();
            if !split.others.is_empty() {
                let others: Vec<InferredType> = split.others.iter().map(|m| (*m).clone()).collect();
                parts.push(self.infer_union_message_send(
                    &others,
                    selector_name,
                    arguments,
                    arg_types,
                    span,
                    hierarchy,
                ));
            }
            return Some(InferredType::union_of(&parts));
        }

        for key in &keys {
            if split.all_closed_shapes()
                && split
                    .shapes
                    .iter()
                    .all(|s| matches!(lookup(s, key), Some(KeyLookup::ClosedMissing)))
            {
                self.warn_key_not_in_shape(
                    receiver_ty,
                    &split,
                    key,
                    if op == KeyedOp::AtIfAbsent {
                        "The `ifAbsent:` block always runs"
                    } else {
                        "This read can only fail at run time"
                    },
                    span,
                );
            }
        }
        if op.is_if_present() {
            return None;
        }

        let absent_ty = (op == KeyedOp::AtIfAbsent).then(|| {
            Self::block_arg_result_ty(&arguments[1], &arg_types[1])
                .unwrap_or(InferredType::Dynamic(DynamicReason::Unknown))
        });
        let mut parts: Vec<InferredType> = Vec::new();
        for key in &keys {
            for shape in &split.shapes {
                match lookup(shape, key)? {
                    KeyLookup::Field(f) => {
                        parts.push(f.value.clone());
                        if f.optional {
                            parts.extend(absent_ty.clone());
                        }
                    }
                    KeyLookup::OpenUnnamed => {
                        parts.push(object());
                        parts.extend(absent_ty.clone());
                    }
                    // The key can never be there: `at:` fails, the
                    // `ifAbsent:` block always runs.
                    KeyLookup::ClosedMissing => parts.extend(absent_ty.clone()),
                }
            }
        }
        if !split.others.is_empty() {
            let others: Vec<InferredType> = split.others.iter().map(|m| (*m).clone()).collect();
            parts.push(self.infer_union_message_send(
                &others,
                selector_name,
                arguments,
                arg_types,
                span,
                hierarchy,
            ));
        }
        Some(if parts.is_empty() {
            InferredType::Dynamic(DynamicReason::Unknown)
        } else {
            InferredType::union_of(&parts)
        })
    }

    /// `shape` with `key` set to a required field of type `value`.
    fn shape_with_key(shape: &InferredType, key: &EcoString, value: &InferredType) -> InferredType {
        let InferredType::Shape { fields, open, .. } = shape else {
            return shape.clone();
        };
        let mut fields = fields.clone();
        let new = ShapeFieldType {
            key: key.clone(),
            value: value.clone(),
            optional: false,
        };
        match fields.iter_mut().find(|f| f.key == *key) {
            Some(slot) => *slot = new,
            None => fields.push(new),
        }
        InferredType::shape(fields, *open)
    }

    /// The "key not in shape" warning (ADR 0147 § Diagnostic severity).
    fn warn_key_not_in_shape(
        &mut self,
        receiver_ty: &InferredType,
        split: &SplitReceiver<'_>,
        key: &str,
        consequence: &str,
        span: Span,
    ) {
        let shape_display = receiver_ty
            .display_for_diagnostic()
            .unwrap_or_else(|| EcoString::from("the shape"));
        let names: Vec<EcoString> = split
            .shapes
            .iter()
            .filter_map(|s| match s {
                InferredType::Shape { fields, .. } => Some(fields.iter().map(|f| f.key.clone())),
                _ => None,
            })
            .flatten()
            .collect();
        let hint = match Self::closest_key(key, &names) {
            Some(near) => eco_format!("{consequence}. Did you mean `#{near}`?"),
            None => EcoString::from(consequence),
        };
        self.diagnostics.push(
            Diagnostic::warning(
                eco_format!("`#{key}` is not a key of {shape_display}"),
                span,
            )
            .with_hint(hint)
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Types the variables of a top-level map pattern `#{#k => v}` matched
    /// against `scrutinee_ty` (a shape, or a union of shapes and `Nil`):
    /// `v :: T` for a named key, `v :: Object` for an unnamed key of an open
    /// shape. A pattern on a key every closed shape lacks can never match.
    /// Any non-shape member leaves the variables as `bind_pattern_vars` set
    /// them.
    pub(super) fn bind_map_pattern_types(
        &mut self,
        pattern: &Pattern,
        scrutinee_ty: &InferredType,
        env: &mut TypeEnv,
    ) {
        let Pattern::Map { pairs, .. } = pattern else {
            return;
        };
        let Some(split) = SplitReceiver::of(scrutinee_ty) else {
            return;
        };
        if !split.others.is_empty() {
            return;
        }
        for pair in pairs {
            let MapPatternKey::Symbol(key) = &pair.key else {
                continue;
            };
            let mut parts = Vec::new();
            for shape in &split.shapes {
                match lookup(shape, key) {
                    Some(KeyLookup::Field(f)) => parts.push(f.value.clone()),
                    Some(KeyLookup::OpenUnnamed) => parts.push(object()),
                    _ => {}
                }
            }
            if parts.is_empty() {
                self.warn_key_not_in_shape(
                    scrutinee_ty,
                    &split,
                    key,
                    "This pattern can never match",
                    pair.span,
                );
            } else if let Pattern::Variable(id) = &pair.value {
                env.set_local(id.name.clone(), InferredType::union_of(&parts));
            }
        }
    }
}

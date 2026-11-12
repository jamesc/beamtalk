// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference for literal expressions.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Covers `Expression::Literal`, `MapLiteral`, `ListLiteral`, `ArrayLiteral`,
//! `TupleLiteral`, `StringInterpolation`, `BinaryConstruction`, and the literal-index `Tuple at:` shortcut — the
//! leaves of `infer_expr`'s dispatch that construct a type from a literal's
//! own shape rather than from message-send/receiver lookup.

use crate::ast::{
    BinarySegment, BinarySegmentType, BinarySegmentValue, Expression, Literal, MapPair,
    StringSegment, WellKnownSelector,
};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{
    DynamicReason, InferredType, ShapeFieldType, TypeChecker, TypeEnv,
};
use ecow::{EcoString, eco_format};

impl TypeChecker {
    /// Joins the inferred element types of a collection literal into a single
    /// element type.
    ///
    /// Delegates to [`InferredType::union_of`], which collapses a homogeneous
    /// literal to its single element type (`#[1, 2, 3]` → `Integer`), joins a
    /// heterogeneous one into a union (`#[1, "a"]` → `Integer | String`),
    /// degrades to `Dynamic` if any element is itself `Dynamic`, and — for an
    /// empty literal (no elements) — returns `Dynamic`, so `#[]` infers
    /// `Array(Dynamic)`.
    pub(in crate::semantic_analysis::type_checker) fn join_element_types(
        elements: &[InferredType],
    ) -> InferredType {
        InferredType::union_of(elements)
    }

    /// Extracts the element type contributed by a list literal `tail` (cons).
    ///
    /// A BEAM cons tail must be a list, so only a known `List(T)` contributes
    /// its element type `T` to the literal's element join. Any other tail
    /// carries no usable element information and widens the element to
    /// `Dynamic` — including an `Array` (a distinct, tuple-backed type that
    /// would form an *improper* list in cons position) and a bare, unannotated
    /// `List`.
    pub(in crate::semantic_analysis::type_checker) fn tail_element_type(
        tail_ty: &InferredType,
    ) -> InferredType {
        match tail_ty {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } if class_name == "List" && !type_args.is_empty() => type_args[0].clone(),
            _ => InferredType::Dynamic(DynamicReason::Unknown),
        }
    }

    /// Reports whether a list-literal cons tail is a *known, non-`List`* type —
    /// i.e. one that would form an improper BEAM list at runtime.
    ///
    /// A cons tail (`[1 | tail]`) must evaluate to a proper list. A known type
    /// whose base class is not `List` (most notably `Array`, which is
    /// tuple-backed) would instead build an improper list. We surface this as a
    /// diagnostic so the user sees a likely runtime bug rather than the silent
    /// degradation to `List(Dynamic)` that [`tail_element_type`] performs.
    ///
    /// Returns `None` (no diagnostic) for `List` tails — proper by construction —
    /// and for `Dynamic`/`Union`/`Meta`/`Never`, which are too uncertain to flag
    /// without risking noise. When a diagnostic is warranted, returns the
    /// offending type's user-facing display name.
    pub(in crate::semantic_analysis::type_checker) fn improper_cons_tail_display(
        tail_ty: &InferredType,
    ) -> Option<EcoString> {
        match tail_ty {
            InferredType::Known { class_name, .. } if class_name != "List" => Some(
                tail_ty
                    .display_for_diagnostic()
                    .unwrap_or_else(|| class_name.clone()),
            ),
            _ => None,
        }
    }

    /// ADR 0075 amendment: infer the element type of
    /// `aTuple at: <literal int>` from a known `Tuple(T1, …, Tn)` type.
    ///
    /// Returns `Some(element_type)` only when **all** of the following hold:
    /// - the receiver class is `Tuple` and the selector is `at:`
    /// - the receiver carries positional element types (`type_args` non-empty)
    /// - the single argument is an integer literal (allowing parentheses)
    /// - the 1-based literal index is within `1..=type_args.len()`
    ///
    /// Any other shape returns `None`, so the caller falls through to normal
    /// dispatch (which yields `Dynamic` for the untyped `Tuple at:` primitive).
    /// This guarantees no false-positive type/DNU warnings for non-literal or
    /// out-of-range indices.
    pub(in crate::semantic_analysis::type_checker) fn infer_literal_index_tuple_at(
        class_name: &str,
        selector_name: &str,
        type_args: &[InferredType],
        arguments: &[Expression],
    ) -> Option<InferredType> {
        if WellKnownClass::from_str(class_name) != Some(WellKnownClass::Tuple)
            || WellKnownSelector::from_name(selector_name) != Some(WellKnownSelector::At)
        {
            return None;
        }
        if type_args.is_empty() || arguments.len() != 1 {
            return None;
        }
        let index = match &arguments[0] {
            Expression::Literal(Literal::Integer(n), _) => *n,
            _ => return None,
        };
        // 1-based index must be in range. `index <= 0` and `index > len` both
        // fall through to Dynamic rather than warning.
        let idx = usize::try_from(index).ok()?;
        if idx == 0 || idx > type_args.len() {
            return None;
        }
        Some(type_args[idx - 1].clone())
    }

    /// Infer the type of a literal value.
    pub(in crate::semantic_analysis::type_checker) fn infer_literal(lit: &Literal) -> InferredType {
        match lit {
            Literal::Integer(_) => InferredType::known("Integer"),
            Literal::Float(_) => InferredType::known("Float"),
            Literal::String(_) => InferredType::known("String"),
            Literal::Symbol(name) => InferredType::known(eco_format!("#{name}")),
            Literal::Character(_) => InferredType::known("Character"),
            Literal::List(_) => InferredType::known("List"),
        }
    }

    /// Infer a map literal `#{...}` — the `Expression::MapLiteral` arm of
    /// `infer_expr`'s dispatch.
    ///
    /// A non-empty literal whose keys are all distinct symbol literals infers
    /// a *closed shape* with every key required and each
    /// value's own inferred type, so nested literals infer nested shapes
    /// (ADR 0147 § Map literals infer shapes). Any other literal — a computed
    /// key, a non-symbol key, a mix, or a repeated key — infers
    /// `Dictionary(K, V)` with `K`/`V` the joins of the key/value types. The
    /// empty `#{}` is deliberately one of those: it is the seed of the
    /// `inject: #{} into: [...]` / `d := d at: k put: v` idiom, whose result
    /// is a filled `Dictionary`, never the empty record, so it keeps the
    /// gradual `Dictionary(Dynamic, Dynamic)`. The
    /// shape's [`dictionary_view`](InferredType::dictionary_view) equals that
    /// same `Dictionary(K, V)`, so sends that are not shape-aware see no
    /// difference.
    pub(in crate::semantic_analysis::type_checker) fn infer_map_literal(
        &mut self,
        pairs: &[MapPair],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        // Infer key/value in interleaved source order (k1, v1, k2, v2, …)
        // so any narrowing an expression applies to the shared `env`
        // propagates — the keys and
        // values share one env (no per-pair `env.child()`), so order is
        // observable.
        let mut key_types: Vec<InferredType> = Vec::with_capacity(pairs.len());
        let mut value_types: Vec<InferredType> = Vec::with_capacity(pairs.len());
        for pair in pairs {
            key_types.push(self.infer_expr(&pair.key, hierarchy, env, in_abstract_method));
            value_types.push(self.infer_expr(&pair.value, hierarchy, env, in_abstract_method));
        }
        if let Some(shape) = Self::symbol_keyed_shape(pairs, &value_types) {
            return shape;
        }
        let key_ty = Self::join_element_types(&key_types);
        let value_ty = Self::join_element_types(&value_types);
        InferredType::known_with_args("Dictionary", vec![key_ty, value_ty])
    }

    /// The closed shape of a map literal whose keys are all distinct symbol
    /// literals, or `None` for any other literal (and for `#{}`) (which stays a
    /// `Dictionary(K, V)`). A repeated key is declined rather than collapsed
    /// so the shape's `Dictionary` view keeps equalling the joined type.
    fn symbol_keyed_shape(pairs: &[MapPair], value_types: &[InferredType]) -> Option<InferredType> {
        if pairs.is_empty() {
            return None;
        }
        let mut fields: Vec<ShapeFieldType> = Vec::with_capacity(pairs.len());
        for (pair, value) in pairs.iter().zip(value_types) {
            let Expression::Literal(Literal::Symbol(name), _) = &pair.key else {
                return None;
            };
            if fields.iter().any(|f| f.key == *name) {
                return None;
            }
            fields.push(ShapeFieldType {
                key: name.clone(),
                value: value.clone(),
                optional: false,
            });
        }
        Some(InferredType::shape(fields, false))
    }

    /// Infer a list literal `#(...)` (optionally with a cons `tail`) as
    /// `List(E)` — the `Expression::ListLiteral` arm of `infer_expr`'s
    /// dispatch.
    pub(in crate::semantic_analysis::type_checker) fn infer_list_literal(
        &mut self,
        elements: &[Expression],
        tail: Option<&Expression>,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let mut element_types: Vec<InferredType> = elements
            .iter()
            .map(|elem| self.infer_expr(elem, hierarchy, env, in_abstract_method))
            .collect();
        if let Some(t) = tail {
            let tail_ty = self.infer_expr(t, hierarchy, env, in_abstract_method);
            // A cons tail must be a proper list. A known
            // non-`List` tail (e.g. `Array`, tuple-backed) builds an
            // improper list at runtime; flag it instead of silently
            // widening the element type to `Dynamic`. The fact
            // (this pure check) stays here; `validation.rs` renders it.
            if let Some(tail_display) = Self::improper_cons_tail_display(&tail_ty) {
                self.emit_improper_cons_tail(&tail_display, t.span());
            }
            element_types.push(Self::tail_element_type(&tail_ty));
        }
        let element_ty = Self::join_element_types(&element_types);
        InferredType::known_with_args("List", vec![element_ty])
    }

    /// Infer an array literal `#[...]` as `Array(E)` — the
    /// `Expression::ArrayLiteral` arm of `infer_expr`'s dispatch.
    pub(in crate::semantic_analysis::type_checker) fn infer_array_literal(
        &mut self,
        elements: &[Expression],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let element_types: Vec<InferredType> = elements
            .iter()
            .map(|elem| self.infer_expr(elem, hierarchy, env, in_abstract_method))
            .collect();
        let element_ty = Self::join_element_types(&element_types);
        InferredType::known_with_args("Array", vec![element_ty])
    }

    /// Infer a tuple literal `{e1, …, en}` as the positional
    /// `Tuple(T1, …, Tn)` type (ADR 0075, ADR 0136) — the
    /// `Expression::TupleLiteral` arm of `infer_expr`'s dispatch.
    ///
    /// Each element keeps its own inferred type (no join), so
    /// `{#a, 1} at: 2` infers `Integer` via
    /// [`Self::infer_literal_index_tuple_at`]. The empty tuple `{}` infers as
    /// bare `Tuple`.
    pub(in crate::semantic_analysis::type_checker) fn infer_tuple_literal(
        &mut self,
        elements: &[Expression],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let element_types: Vec<InferredType> = elements
            .iter()
            .map(|elem| self.infer_expr(elem, hierarchy, env, in_abstract_method))
            .collect();
        if element_types.is_empty() {
            InferredType::known(WellKnownClass::Tuple.as_str())
        } else {
            InferredType::known_with_args(WellKnownClass::Tuple.as_str(), element_types)
        }
    }

    /// Infer a string interpolation expression as `String`, type-checking
    /// each interpolated sub-expression along the way — the
    /// `Expression::StringInterpolation` arm of `infer_expr`'s dispatch.
    pub(in crate::semantic_analysis::type_checker) fn infer_string_interpolation(
        &mut self,
        segments: &[StringSegment],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        for seg in segments {
            if let crate::ast::StringSegment::Interpolation(inner_expr) = seg {
                self.infer_expr(inner_expr, hierarchy, env, in_abstract_method);
            }
        }
        InferredType::known("String")
    }

    /// Infers a binary construction (`<<v:8, p/binary>>`, ADR 0143).
    ///
    /// Every segment value and size is inferred, so sends inside a
    /// parenthesised value are still checked. The result class follows the
    /// bit alignment (ADR 0143 § Result class, [`binary_construction_type`]).
    pub(in crate::semantic_analysis::type_checker) fn infer_binary_construction(
        &mut self,
        segments: &[BinarySegment<Expression>],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        for inner_expr in segments.iter().flat_map(BinarySegment::sub_expressions) {
            self.infer_expr(inner_expr, hierarchy, env, in_abstract_method);
        }
        binary_construction_type(segments)
    }
}

/// The class a binary construction builds (ADR 0143 § Result class): a
/// `Binary` when its total bit size is a multiple of 8, a `Bitstring`
/// otherwise, and `Bitstring | Binary` (what `Bitstring>>concat:` answers)
/// when a segment leaves the alignment unknown.
///
/// A segment's bits are known from a literal size × unit and the default
/// `integer` size; a bare string literal and `utf*` segments are whole
/// bytes, and so is a `float` segment of any size (it is 16, 32 or 64 bits
/// or the construction fails). Any other segment (a variable size, a
/// sizeless `binary` or `bits` value) is a whole number of its units, so it
/// keeps the alignment exactly when its unit is a multiple of 8 (`binary`
/// defaults to 8, `bits` to 1).
pub(in crate::semantic_analysis::type_checker) fn binary_construction_type(
    segments: &[BinarySegment<Expression>],
) -> InferredType {
    let mut bits_mod_8: u64 = 0;
    for seg in segments {
        let utf = matches!(
            seg.effective_type(),
            BinarySegmentType::Utf8 | BinarySegmentType::Utf16 | BinarySegmentType::Utf32
        );
        let bare_string = seg.segment_type.is_none()
            && matches!(seg.value.binary_literal(), Some((Literal::String(_), _)));
        // A `float` segment builds 16, 32 or 64 bits or fails at run time
        // (`badarg`), whatever its size and unit, so when the construction
        // answers at all the segment is whole bytes.
        let float = seg.effective_type() == BinarySegmentType::Float;
        let segment_bits = if utf || bare_string || float {
            Some(0)
        } else if let Some(bits) = seg.literal_bit_size() {
            Some(bits % 8)
        } else {
            seg.effective_unit().filter(|unit| unit % 8 == 0).map(|_| 0)
        };
        match segment_bits {
            Some(bits) => bits_mod_8 = (bits_mod_8 + bits) % 8,
            None => {
                return InferredType::union_of(&[
                    InferredType::known("Bitstring"),
                    InferredType::known("Binary"),
                ]);
            }
        }
    }
    if bits_mod_8 == 0 {
        InferredType::known("Binary")
    } else {
        InferredType::known("Bitstring")
    }
}

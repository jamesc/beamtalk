// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type definitions for the type checker — `TypeProvenance`, `InferredType`, and helpers.
//!
//! **DDD Context:** Semantic Analysis

use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::class_hierarchy::DeclaredType;
use crate::semantic_analysis::protocol_registry::{ProtocolRegistry, ReturnTypeMismatch};
use crate::source_analysis::Span;
use ecow::{EcoString, eco_format};

use super::well_known::WellKnownClass;

/// Why a type could not be determined — enables hover provenance,
/// coverage detail, and diagnostic messages.
///
/// **References:** ADR 0077 Section 1
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynamicReason {
    /// Parameter has no type annotation.
    UnannotatedParam,
    /// Method has no return type annotation and body could not be inferred.
    UnannotatedReturn,
    /// Receiver is Dynamic, so message send result is Dynamic.
    DynamicReceiver,
    /// Control flow produces incompatible types (pre-union-narrowing fallback).
    AmbiguousControlFlow,
    /// Erlang FFI call with no spec in the native type registry.
    UntypedFfi,
    /// Erlang FFI call has a spec but the return type is `any()`/`term()`.
    ///
    /// Distinguished from `UntypedFfi` because the spec EXISTS — the function
    /// simply has a broad return type.  Not actionable in typed classes.
    DynamicSpec,
    /// The type annotation literally is the `Dynamic` keyword —
    /// e.g. `Result(Dynamic, Error)`'s first type-arg, or a field/param
    /// explicitly declared `:: Dynamic`.
    ///
    /// Distinguished from every other reason (all of which mean "we don't
    /// have enough information to give a concrete type") because this one
    /// means the *opposite*: the author explicitly promised the value can be
    /// anything. `merge_method_local_binding` and `join_branch_arms` treat
    /// this as authoritative — it must survive being unified with any other
    /// binding, including a concrete one from a sibling argument position or
    /// branch arm — where every other `DynamicReason` instead loses to a
    /// concrete binding observed elsewhere.
    ExplicitDynamic,
    /// A branch join (e.g. `ifTrue:ifFalse:`'s shared `R`) where at least
    /// one arm is `Dynamic` and another is concrete (BT-4207).
    ///
    /// The join is soundly `Dynamic` — collapsing it to the concrete arm
    /// (`Dynamic | Nil` ⇒ `Nil`) would make later sends report spurious
    /// DNUs. It is not a root cause: the `Dynamic` arm already produced
    /// its own diagnostic (if any) at its source, so this reason does not
    /// re-warn "Dynamic in typed class" on the enclosing expression.
    DynamicBranch,
    /// A send whose selector the receiver's *visible* method surface does not
    /// define (BT-4526) — e.g. `HPACK validateHeader:value:` after the method
    /// was removed. The receiver is well-typed; it is the selector that is
    /// unresolved.
    ///
    /// Only produced where the checker could not prove a does-not-understand
    /// (the receiver's surface is `Open` — e.g. the pre-WS3 dependency
    /// guard, ADR 0100 Rule 2 — so an unseen extension might still define
    /// it). Where the surface is `ClosedComplete` the DNU diagnostic is the
    /// root cause and the send's result is `DynamicReceiver` instead, so the
    /// "Dynamic in typed class" warning does not repeat it.
    UnresolvedSelector,
    /// Fallback — no specific reason available.
    Unknown,
}

impl DynamicReason {
    /// Returns a human-readable description of why the type is Dynamic,
    /// or `None` for `Unknown` (no useful context to show).
    #[must_use]
    pub fn description(self) -> Option<&'static str> {
        match self {
            Self::UnannotatedParam => Some("unannotated parameter"),
            Self::UnannotatedReturn => Some("unannotated return"),
            Self::DynamicReceiver => Some("dynamic receiver"),
            Self::AmbiguousControlFlow => Some("ambiguous control flow"),
            Self::UntypedFfi => Some("untyped FFI"),
            Self::DynamicSpec => Some("FFI spec is Dynamic"),
            Self::ExplicitDynamic => Some("explicitly declared Dynamic"),
            Self::DynamicBranch => Some("dynamic branch"),
            Self::UnresolvedSelector => Some("unresolved selector"),
            Self::Unknown => None,
        }
    }
}

/// Tracks where a type came from — enables precise error messages
/// and determines how far inference should propagate.
///
/// **References:** ADR 0068 Challenge 3
///
/// Not `Copy` — [`Aliased`](Self::Aliased) carries an `EcoString` payload, so
/// call sites need an explicit `.clone()`. Cloning is cheap (`EcoString` is
/// reference-counted).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeProvenance {
    /// User wrote `:: Type` at this location.
    Declared(Span),
    /// Compiler inferred from expression at this location.
    Inferred(Span),
    /// Derived from a generic substitution at this location.
    Substituted(Span),
    /// Auto-extracted from `.beam` abstract code (ADR 0075).
    ///
    /// Types with this provenance come from Erlang `-spec` attributes read at
    /// build time. The span is always `Span::default()` (no source location in
    /// the Beamtalk codebase) — diagnostics show "from <module>.beam -spec".
    Extracted,
    /// The type is the eager expansion of a named alias reference (ADR 0108)
    /// — attached *only* at the exact point of expansion in
    /// [`resolve_type_annotation`](super::resolve_type_annotation), so the
    /// tagged value is by construction structurally identical to alias
    /// `name`'s expansion (the ADR's "display through normalisation, scoped
    /// honestly" rule).
    ///
    /// `span` is the annotation *reference* site (where `name` was written),
    /// not the alias's own `type Name = ...` declaration. **Not currently
    /// consumed by anything** (only `name` drives display) and **not
    /// reliable across repeated references within one annotation**:
    /// `resolve_type_annotation`'s per-call `memo` is keyed by alias name
    /// alone, so a second reference to the same alias in one annotation
    /// (`A | A`, `Result(A, A)`) reuses the *first* reference's cached,
    /// already-tagged value — including its `span` — rather than expanding
    /// fresh. A future consumer (e.g. find-references) needing a
    /// precise per-site span will need to record it at the `memo.get` hit
    /// path too, not just at first expansion.
    ///
    /// This tag does **not** survive [`InferredType::difference`],
    /// [`InferredType::intersect`], or [`InferredType::union_of`] — those
    /// normalising operators construct fresh provenance for their result, so
    /// a narrowing residual (e.g. the false branch of `policy =:= #temporary`)
    /// renders the structural type only, never a stale `(from RestartStrategy)`
    /// breadcrumb. `union_of`'s "best provenance" inheritance heuristic
    /// deliberately excludes this variant (see its doc) so a freshly-built,
    /// differently-shaped union can never silently inherit a member's alias
    /// identity either.
    Aliased { name: EcoString, span: Span },
}

impl TypeProvenance {
    /// Degrades [`TypeProvenance::Aliased`] to a spanless [`TypeProvenance::Inferred`];
    /// a no-op for every other variant.
    ///
    /// Callers that construct a `Negation` (via [`InferredType::make_negation`])
    /// from a provenance captured off an *existing* negation — same-base
    /// flattening in [`InferredType::difference`], and [`InferredType::absorb_negations`]'s
    /// "first negation wins" carry-over — must call this first. The captured
    /// provenance describes the *input* negation, but the widened `excluded`
    /// set makes the *result* structurally different from whatever alias the
    /// input happened to be tagged with, so carrying the tag forward
    /// unchanged would violate `Aliased`'s own documented invariant (this tag
    /// does not survive `difference`).
    fn strip_alias(self) -> Self {
        match self {
            Self::Aliased { .. } => Self::Inferred(Span::default()),
            other => other,
        }
    }
}

/// Controls how [`InferredType`] renders class names when converted to a
/// display string.
///
/// See [`InferredType::display_name`], [`InferredType::display_annotation`],
/// and [`InferredType::display_for_diagnostic`].
#[derive(Debug, Clone, Copy)]
struct DisplayOptions {
    /// When `true`, `Known("UndefinedObject")` renders as `"Nil"`. When
    /// `false`, the canonical `UndefinedObject` name is used.
    nil_as_source_name: bool,
    /// When `true`, `Dynamic(reason)` renders as `"Dynamic (description)"` if
    /// the reason has a description. When `false`, always renders as `"Dynamic"`.
    show_dynamic_reason: bool,
    /// When `true`, an anonymous shape with more than
    /// [`SHAPE_DISPLAY_MAX_FIELDS`] fields renders a prefix plus `… +N more`
    /// (diagnostics and hover). Only the source-friendly mode sets it: the
    /// canonical and bare forms must stay complete — they feed assignability
    /// lookups and paste-back annotation strings.
    truncate_shapes: bool,
}

/// How many fields of an anonymous shape diagnostics and hover list before
/// eliding the rest.
const SHAPE_DISPLAY_MAX_FIELDS: usize = 4;

impl DisplayOptions {
    const CANONICAL: Self = Self {
        nil_as_source_name: false,
        show_dynamic_reason: true,
        truncate_shapes: false,
    };
    const SOURCE_FRIENDLY: Self = Self {
        nil_as_source_name: true,
        show_dynamic_reason: true,
        truncate_shapes: true,
    };
    /// Produces a valid Beamtalk annotation string: no nil remapping, no
    /// dynamic-reason parenthetical. Used by [`InferredType::display_annotation`].
    const BARE: Self = Self {
        nil_as_source_name: false,
        show_dynamic_reason: false,
        truncate_shapes: false,
    };
}

/// Inferred type for an expression or variable.
///
/// **Equality semantics:** Two types are equal if they represent the same type,
/// regardless of provenance. This ensures `HashMap` lookups in `TypeMap` and test
/// assertions work correctly even when the same type is inferred at different spans.
///
/// **References:** ADR 0068 Phase 1
#[derive(Debug, Clone, Eq)]
pub enum InferredType {
    /// A known concrete class type (e.g., "Integer", "Counter").
    Known {
        class_name: EcoString,
        /// Type arguments for generic types (empty for non-generic types).
        type_args: Vec<InferredType>,
        /// Where this type came from.
        provenance: TypeProvenance,
    },
    /// A union of known types (e.g., `String | UndefinedObject`).
    ///
    /// Members are full `InferredType` values, preserving generic type args
    /// (e.g., `Result(Integer, String) | nil`).  Equality is order-independent.
    /// An empty member list is impossible — construction always requires ≥2 members.
    Union {
        members: Vec<InferredType>,
        provenance: TypeProvenance,
    },
    /// The metatype of a class — the type of the class object `C class`
    /// (a.k.a. "metatype-of-`C`").
    ///
    /// **Name-only, deliberately not parameterized.** Per ADR 0068 the class
    /// object is unparameterized ("there's no `Result(Integer, Error)` class
    /// object"), so `Meta` carries a class *name* only (`Meta{List}`, never
    /// `Meta{List(E)}`). This makes parameterized metatypes structurally
    /// unrepresentable — the 0068 rule is enforced by the type, not by
    /// discipline. Instance type arguments are recovered at the *call site*
    /// via ADR 0068's class-method inference (e.g. `List withAll: aList(Integer)`
    /// infers the element type from the argument, not the class object).
    ///
    /// Subtyping: `Meta{C} <: Class <: Behaviour <: Object` (see
    /// `validation.rs`), so a metatype value still satisfies `:: Class` /
    /// `:: Behaviour` parameters.
    ///
    /// A *dedicated variant* (rather than an `is_meta` flag on [`Known`]) is
    /// chosen so the relevant match arms are compiler-visible: an
    /// `if let Known { .. }` simply falls through to "unknown" rather than
    /// silently treating a metatype as the instance type.
    ///
    /// Named `Meta` (not `Metaclass`) to avoid clashing with the tower's
    /// `Metaclass` *class*.
    ///
    /// **References:** ADR 0083 (Metaclass-Aware Type Inference), ADR 0068,
    /// ADR 0036.
    ///
    /// [`Known`]: InferredType::Known
    Meta {
        class_name: EcoString,
        /// Where this metatype came from.
        provenance: TypeProvenance,
    },
    /// The protocol metatype `P class` for a protocol `P` — the class object
    /// of *any* class that conforms to `P` (ADR 0144).
    ///
    /// `C class <: P class` iff `C` conforms to `P`, judged by the one
    /// structural conformance check
    /// ([`ProtocolRegistry::check_conformance`]). Sends resolve against `P`'s
    /// `class` requirements first, then the metaclass tower every class object
    /// answers (`Class` → `Behaviour` → `Object`), exactly as for a [`Meta`].
    ///
    /// A *sibling variant* rather than a marker on [`Meta`] because a
    /// protocol *literal* (`Transport`, the protocol object) still infers as
    /// `Meta{Transport}`; a separate variant keeps the two meanings apart and
    /// makes every `Meta` match arm decide explicitly whether it applies.
    /// Name-only, like [`Meta`] (ADR 0068): a generic protocol's metatype
    /// carries no type arguments.
    ///
    /// [`Meta`]: InferredType::Meta
    /// [`ProtocolRegistry::check_conformance`]: crate::semantic_analysis::protocol_registry::ProtocolRegistry::check_conformance
    ProtocolMeta {
        protocol_name: EcoString,
        /// Where this protocol metatype came from.
        provenance: TypeProvenance,
    },
    /// Type cannot be determined — skip all checking.
    ///
    /// The [`DynamicReason`] explains *why* the type could not be determined,
    /// enabling hover provenance, coverage detail, and diagnostic messages.
    /// The `PartialEq` impl ignores the reason — all `Dynamic` values are equal.
    Dynamic(DynamicReason),
    /// Bottom type — the type of expressions that never produce a value
    /// (e.g., `self error:`, `Exception signal:`).
    ///
    /// `Never` is the identity element for union: `T | Never = T`.
    /// It is a subtype of every type (bottom of the lattice).
    Never,
    /// A negation type `base \ excluded` — the values of `base` that are *not*
    /// values of `excluded` (ADR 0102 §1, §5).
    ///
    /// Produced by [`difference`](InferredType::difference) when a proper
    /// subtype is subtracted that cannot be expressed by dropping a union
    /// member. Two flavours, distinguished by what `excluded` holds:
    /// - **Singleton-excluded** — canonically `Symbol \ #foo` (all symbols
    ///   except `#foo`); `base` is always exactly `Symbol` (see
    ///   [`is_symbol_base`](Self::is_symbol_base)).
    /// - **Nominal-excluded** (ADR 0102 §5) — e.g. `Object \ Number`;
    ///   `base` is any nominal class and `excluded` a single strict nominal
    ///   subclass of it (generalising `excluded` to a union of classes is out
    ///   of scope — see §5's scope note).
    ///
    /// **Normal form.** For the singleton-excluded flavour, `excluded` is
    /// always a singleton (`Known("#foo")`) or a normalised union of
    /// singletons; for the nominal-excluded flavour it is a single nominal
    /// class name. Either way it is never another `Negation` and never a
    /// `Union` containing a `Negation`. Nested singleton negation is flattened
    /// at construction (`(Symbol \ #a) \ #b = Symbol \ (#a | #b)`), so a
    /// `Negation` never appears inside its own `excluded` or `base`.
    ///
    /// **Equality** is order-independent in `excluded` (it delegates to the
    /// order-independent `Union` equality), so `Symbol \ (#a | #b)` equals
    /// `Symbol \ (#b | #a)`.
    Negation {
        /// The type being narrowed (`Symbol` for the singleton-excluded
        /// flavour; any nominal class for the nominal-excluded flavour).
        base: Box<InferredType>,
        /// The removed values — a singleton, a normalised union of
        /// singletons, or (nominal-excluded flavour) a single nominal class.
        excluded: Box<InferredType>,
        /// Where this negation came from.
        provenance: TypeProvenance,
    },
    /// An irreducible intersection type `A ∩ B ∩ …` (ADR 0102 §1/§3).
    ///
    /// Produced by [`intersect`](InferredType::intersect) only for the one
    /// case that cannot collapse to an existing variant: **class ∩
    /// protocol** (e.g. `Collection(Object) & Comparable`, ADR 0068 §Protocol
    /// Composition). Class ∩ class always reduces via the nominal hierarchy
    /// (to the subclass, or `Never` for hierarchy-unrelated sealed classes),
    /// so a stored `Intersection` never contains two classes related by
    /// inheritance — every member is either a protocol, or a class that does
    /// not (as far as the checker can prove) reduce against its co-members.
    ///
    /// **Normal form.** `members` has at least two entries, is **flattened**
    /// (a nested `Intersection` is never a member — constructing one merges
    /// the nested members into the parent), and is **deduplicated** by
    /// structural equality. **Equality is order-independent** (mirrors
    /// [`Union`](Self::Union)).
    Intersection {
        /// The intersected member types (≥2, flattened, deduplicated).
        members: Vec<InferredType>,
        /// Where this intersection came from.
        provenance: TypeProvenance,
    },
    /// A map-shape type (ADR 0147): a structural type for a dictionary with
    /// known symbol keys, each with its own value type.
    ///
    /// Denotes a set of maps: a required field means the key is present with
    /// a value in `T`; an optional field means a value in `T` or absent; a
    /// closed shape says every unnamed key is absent, an open shape says
    /// nothing about unnamed keys.
    ///
    /// **Equality ignores field order** (`#{#a => A, #b => B}` equals
    /// `#{#b => B, #a => A}`); the stored order is the source order and
    /// drives display only. A *dedicated variant* (like [`Meta`]) so every
    /// match arm must decide explicitly what a shape means — none falls
    /// through to `Dynamic`.
    ///
    /// Sends other than the shape-aware keyed operations see the shape as
    /// its [`dictionary_view`](InferredType::dictionary_view).
    ///
    /// **Algebra.** Unions keep shape members separate (shapes are never
    /// merged); intersection and difference are stored with ADR 0102's
    /// existing forms and not simplified fieldwise.
    ///
    /// [`Meta`]: InferredType::Meta
    Shape {
        /// The named fields, in source order. Keys are unique.
        fields: Vec<ShapeFieldType>,
        /// `true` when other keys may be present.
        open: bool,
        /// Where this shape came from (`Aliased` when it is the expansion of
        /// a `type` alias, ADR 0108).
        provenance: TypeProvenance,
    },
}

/// One field of an [`InferredType::Shape`] (ADR 0147).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeFieldType {
    /// The symbol key name, without the leading `#`.
    pub key: EcoString,
    /// The value type (for an optional field: the type when present).
    pub value: InferredType,
    /// `true` for `?#key => T` — the key may be absent.
    pub optional: bool,
}

impl PartialEq for InferredType {
    /// Compares types structurally, ignoring provenance.
    ///
    /// `Known("Integer", [], Inferred(0..1))` == `Known("Integer", [], Declared(5..10))`
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Known {
                    class_name: a,
                    type_args: ta,
                    ..
                },
                Self::Known {
                    class_name: b,
                    type_args: tb,
                    ..
                },
            ) => a == b && ta == tb,
            (Self::Union { members: a, .. }, Self::Union { members: b, .. }) => {
                a.len() == b.len() && a.iter().all(|m| b.contains(m))
            }
            (Self::Meta { class_name: a, .. }, Self::Meta { class_name: b, .. })
            | (
                Self::ProtocolMeta {
                    protocol_name: a, ..
                },
                Self::ProtocolMeta {
                    protocol_name: b, ..
                },
            ) => a == b,
            (
                Self::Negation {
                    base: base_a,
                    excluded: excluded_a,
                    ..
                },
                Self::Negation {
                    base: base_b,
                    excluded: excluded_b,
                    ..
                },
            ) => base_a == base_b && excluded_a == excluded_b,
            (Self::Intersection { members: a, .. }, Self::Intersection { members: b, .. }) => {
                a.len() == b.len() && a.iter().all(|m| b.contains(m))
            }
            (
                Self::Shape {
                    fields: fa,
                    open: oa,
                    ..
                },
                Self::Shape {
                    fields: fb,
                    open: ob,
                    ..
                },
            ) => {
                // Order-independent: keys are unique, so equal length plus
                // every field of `a` found identically in `b` is equality.
                oa == ob && fa.len() == fb.len() && fa.iter().all(|f| fb.iter().any(|g| f == g))
            }
            (Self::Dynamic(_), Self::Dynamic(_)) | (Self::Never, Self::Never) => true,
            _ => false,
        }
    }
}

impl InferredType {
    /// Creates a `Known` type with no type arguments and `Inferred` provenance.
    ///
    /// This is the most common construction path — used for literals, message
    /// sends, and all inference sites where we don't yet track provenance
    /// precisely. As Phase 2+ rolls out, callers will switch to explicit
    /// provenance where appropriate.
    #[must_use]
    pub fn known(class_name: impl Into<EcoString>) -> Self {
        Self::Known {
            class_name: class_name.into(),
            type_args: vec![],
            provenance: TypeProvenance::Inferred(Span::default()),
        }
    }

    /// Creates a known generic type `class_name(type_args...)` with `Inferred`
    /// provenance — e.g. `Array(Integer)` or `Dictionary(Integer, String)`.
    ///
    /// For non-generic types prefer [`known`](Self::known). Passing an empty
    /// `type_args` is equivalent to `known`.
    #[must_use]
    pub fn known_with_args(class_name: impl Into<EcoString>, type_args: Vec<Self>) -> Self {
        Self::Known {
            class_name: class_name.into(),
            type_args,
            provenance: TypeProvenance::Inferred(Span::default()),
        }
    }

    /// Creates a [`Meta`](InferredType::Meta) metatype with `Inferred`
    /// provenance — the type of the class object `class_name class`.
    ///
    /// Name-only, per ADR 0083 / ADR 0068 (the class object is unparameterized).
    /// Any type-argument suffix on `class_name` is stripped to enforce this
    /// invariant — `meta("List(E)")` yields `Meta{List}`, not `Meta{List(E)}`.
    #[must_use]
    pub fn meta(class_name: impl Into<EcoString>) -> Self {
        let name: EcoString = class_name.into();
        // Strip any `(...)` type-argument suffix so the stored name is the
        // bare class name (the class object carries no type args).
        let base = match name.find('(') {
            Some(idx) => EcoString::from(&name[..idx]),
            None => name,
        };
        Self::Meta {
            class_name: base,
            provenance: TypeProvenance::Inferred(Span::default()),
        }
    }

    /// Returns the class name of a [`Meta`](InferredType::Meta) metatype
    /// (`Meta{C}` → `Some("C")`), or `None` for any other variant.
    ///
    /// Use this to route a metatype-typed receiver to class-side method lookup
    /// via `find_class_method(C, …)`.
    #[must_use]
    pub fn as_meta(&self) -> Option<&EcoString> {
        match self {
            Self::Meta { class_name, .. } => Some(class_name),
            _ => None,
        }
    }

    /// Creates a [`ProtocolMeta`](InferredType::ProtocolMeta) protocol
    /// metatype `protocol_name class` with `Inferred` provenance (ADR 0144).
    ///
    /// Name-only like [`meta`](Self::meta): any `(...)` type-argument suffix
    /// is stripped.
    #[must_use]
    pub fn protocol_meta(protocol_name: impl Into<EcoString>) -> Self {
        let name: EcoString = protocol_name.into();
        let base = match name.find('(') {
            Some(idx) => EcoString::from(&name[..idx]),
            None => name,
        };
        Self::ProtocolMeta {
            protocol_name: base,
            provenance: TypeProvenance::Inferred(Span::default()),
        }
    }

    /// Returns the protocol name of a
    /// [`ProtocolMeta`](InferredType::ProtocolMeta) (`P class` → `Some("P")`),
    /// or `None` for any other variant.
    #[must_use]
    pub fn as_protocol_meta(&self) -> Option<&EcoString> {
        match self {
            Self::ProtocolMeta { protocol_name, .. } => Some(protocol_name),
            _ => None,
        }
    }

    /// Creates a [`Shape`](InferredType::Shape) with `Inferred` provenance.
    #[must_use]
    pub fn shape(fields: Vec<ShapeFieldType>, open: bool) -> Self {
        Self::Shape {
            fields,
            open,
            provenance: TypeProvenance::Inferred(Span::default()),
        }
    }

    /// The `Dictionary(K, V)` a [`Shape`](InferredType::Shape) behaves as for
    /// every message that is not a shape-aware keyed operation (ADR 0147
    /// § "Every other message"): `K` is the join of the key types (each key
    /// is the singleton `#k`) and `V` the join of the value types; both are
    /// `Object` when the shape is open. `None` for any other variant.
    ///
    /// An empty closed shape has no keys or values, so it views as
    /// `Dictionary(Never, Never)`.
    #[must_use]
    pub fn dictionary_view(&self) -> Option<Self> {
        let Self::Shape { fields, open, .. } = self else {
            return None;
        };
        let (key_ty, value_ty) = if *open {
            (Self::known("Object"), Self::known("Object"))
        } else if fields.is_empty() {
            (Self::Never, Self::Never)
        } else {
            let keys: Vec<Self> = fields
                .iter()
                .map(|f| Self::known(eco_format!("#{}", f.key)))
                .collect();
            let values: Vec<Self> = fields.iter().map(|f| f.value.clone()).collect();
            (Self::union_of(&keys), Self::union_of(&values))
        };
        Some(Self::known_with_args("Dictionary", vec![key_ty, value_ty]))
    }

    /// Creates a `Union` from simple class names with `Inferred` provenance.
    ///
    /// Convenience for the common case of `String | nil` style unions where
    /// members don't carry generic type args.  Resolves type keywords
    /// (`nil` → `UndefinedObject`, `true` → `True`, `false` → `False`)
    /// and deduplicates via `union_of`.
    #[must_use]
    pub fn simple_union(names: &[&str]) -> Self {
        let members: Vec<Self> = names
            .iter()
            .map(|name| match *name {
                "nil" => Self::known(WellKnownClass::UndefinedObject.as_str()),
                "true" => Self::known("True"),
                "false" => Self::known("False"),
                other => Self::known(other),
            })
            .collect();
        Self::union_of(&members)
    }

    /// Returns the class name if this is a known single type.
    ///
    /// Returns `None` for `Dynamic` and `Union` variants. LSP providers that
    /// need a display string for any variant should use [`display_name`] instead.
    #[must_use]
    pub fn as_known(&self) -> Option<&EcoString> {
        match self {
            Self::Known { class_name, .. } => Some(class_name),
            // A metatype is *not* its instance class — `Meta{C}.as_known()`
            // must be `None` so callers don't mistake the class object for an
            // instance of `C`. Use [`as_meta`](Self::as_meta) instead.
            Self::Meta { .. }
            | Self::ProtocolMeta { .. }
            | Self::Dynamic(_)
            | Self::Union { .. }
            | Self::Never
            // A negation (`Symbol \ #foo`) is not a single known class — callers
            // must handle it structurally, not as a bare class name.
            | Self::Negation { .. }
            // An intersection (`A & B`) is not a single known class either —
            // callers must handle it structurally (ADR 0102 §1).
            | Self::Intersection { .. }
            // A shape is a structural map type, not a nominal class; callers
            // that want the `Dictionary` it behaves as use
            // [`dictionary_view`](Self::dictionary_view).
            | Self::Shape { .. } => None,
        }
    }

    /// Returns the [`TypeProvenance`] attached to this type, or `None` for the
    /// provenance-free variants ([`Dynamic`](Self::Dynamic) and
    /// [`Never`](Self::Never), which carry no source location).
    // Consumed by the narrowing rules in ADR 0102's epic.
    #[allow(dead_code)]
    #[must_use]
    pub(crate) fn provenance(&self) -> Option<TypeProvenance> {
        match self {
            Self::Known { provenance, .. }
            | Self::Union { provenance, .. }
            | Self::Meta { provenance, .. }
            | Self::ProtocolMeta { provenance, .. }
            | Self::Negation { provenance, .. }
            | Self::Intersection { provenance, .. }
            | Self::Shape { provenance, .. } => Some(provenance.clone()),
            Self::Dynamic(_) | Self::Never => None,
        }
    }

    /// Returns the alias display name if this type's top-level provenance is
    /// [`TypeProvenance::Aliased`] (ADR 0108) — i.e. this exact value
    /// is the eager expansion of a named alias reference, structurally
    /// identical to that alias's declared expansion by construction (see
    /// `Aliased`'s doc).
    ///
    /// Scoped to `type_checker` (not merely `pub(crate)`): the "structurally
    /// identical" guarantee only holds because [`tag_alias_expansion`]'s only
    /// caller is `type_resolver.rs`'s own alias-expansion point — a caller
    /// outside this module has no way to uphold that invariant.
    ///
    /// [`tag_alias_expansion`]: Self::tag_alias_expansion
    #[must_use]
    pub(in crate::semantic_analysis::type_checker) fn alias_display_name(
        &self,
    ) -> Option<&EcoString> {
        match self.provenance() {
            Some(TypeProvenance::Aliased { .. }) => match self {
                Self::Known {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                }
                | Self::Union {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                }
                | Self::Meta {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                }
                | Self::ProtocolMeta {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                }
                | Self::Negation {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                }
                | Self::Intersection {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                }
                | Self::Shape {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                } => Some(name),
                _ => None,
            },
            _ => None,
        }
    }

    /// Rewrites this type's top-level provenance to
    /// [`TypeProvenance::Aliased`] (ADR 0108) — the single
    /// construction path for that provenance variant, called exactly once,
    /// at the point [`resolve_type_annotation`](super::resolve_type_annotation)
    /// finishes eagerly expanding a `Simple` annotation that names a
    /// registered alias. A no-op for [`Dynamic`](Self::Dynamic) and
    /// [`Never`](Self::Never), which carry no provenance slot.
    ///
    /// Callers **must** only call this with `self` already equal to the
    /// exact expansion `name` resolves to — the display layer trusts the tag
    /// unconditionally (see `alias_display_name`'s doc), so tagging an
    /// unrelated value would silently produce a false `Name (…)` diagnostic.
    /// Kept out of `pub(crate)` for this reason: `type_resolver.rs` is the
    /// only call site able to uphold the invariant.
    #[must_use]
    pub(in crate::semantic_analysis::type_checker) fn tag_alias_expansion(
        self,
        name: EcoString,
        span: Span,
    ) -> Self {
        let aliased = TypeProvenance::Aliased { name, span };
        match self {
            Self::Known {
                class_name,
                type_args,
                ..
            } => Self::Known {
                class_name,
                type_args,
                provenance: aliased,
            },
            Self::Union { members, .. } => Self::Union {
                members,
                provenance: aliased,
            },
            Self::Meta { class_name, .. } => Self::Meta {
                class_name,
                provenance: aliased,
            },
            Self::ProtocolMeta { protocol_name, .. } => Self::ProtocolMeta {
                protocol_name,
                provenance: aliased,
            },
            Self::Negation { base, excluded, .. } => Self::Negation {
                base,
                excluded,
                provenance: aliased,
            },
            Self::Intersection { members, .. } => Self::Intersection {
                members,
                provenance: aliased,
            },
            Self::Shape { fields, open, .. } => Self::Shape {
                fields,
                open,
                provenance: aliased,
            },
            other @ (Self::Dynamic(_) | Self::Never) => other,
        }
    }

    /// Returns a human-readable display name for this type, using canonical
    /// class-hierarchy names.
    ///
    /// - `Known("Integer", [])` → `"Integer"`
    /// - `Known("UndefinedObject", [])` → `"UndefinedObject"`
    /// - `Known("Result", [Known("Integer"), Known("String")])` → `"Result(Integer, String)"`
    /// - `Union([Known("String"), Known("UndefinedObject")])` → `"String | UndefinedObject"`
    /// - `Dynamic(Unknown)` → `"Dynamic"`
    /// - `Dynamic(UnannotatedParam)` → `"Dynamic (unannotated parameter)"`
    ///
    /// This is the "internal" display — it surfaces the canonical
    /// `UndefinedObject` name. For user-facing diagnostics, hover, signature
    /// help, and code actions, prefer [`display_for_diagnostic`](Self::display_for_diagnostic),
    /// which renders the source-sympathetic `Nil` spelling instead.
    #[must_use]
    pub fn display_name(&self) -> EcoString {
        self.display_with_options(DisplayOptions::CANONICAL)
    }

    /// Returns the type as a valid Beamtalk annotation string — the form a
    /// user writes in source code.
    ///
    /// - `Known("Integer", [])` → `"Integer"`
    /// - `Known("UndefinedObject", [])` → `"UndefinedObject"` (no `Nil` remapping)
    /// - `Dynamic(_)` → `"Dynamic"` (provenance description is always omitted)
    /// - `Union([Known("A"), Known("B")])` → `"A | B"`
    /// - `Negation { base, excluded: Union }` → `"base \\ (excluded)"`
    /// - `Intersection { members }` → `"A & B"`
    ///
    /// Unlike [`display_name`](Self::display_name), the `Dynamic` variant never
    /// includes a provenance description parenthetical. Use this wherever you
    /// need a string that could be pasted back into a type annotation or compared
    /// against another annotation string for assignability.
    #[must_use]
    pub fn display_annotation(&self) -> EcoString {
        self.display_with_options(DisplayOptions::BARE)
    }

    /// Maps a raw class-name string to its user-facing diagnostic spelling.
    ///
    /// Rewrites every occurrence of `"UndefinedObject"` → `"Nil"` as a whole
    /// identifier, so this scrubs both bare class names (`"UndefinedObject"`)
    /// and nested annotations (`"Array(UndefinedObject)"`,
    /// `"Foo | UndefinedObject"`). Identifiers that merely *contain*
    /// `UndefinedObject` as a substring (e.g. `"MyUndefinedObjectWrapper"`)
    /// are left alone. Use this when you have an `EcoString`/`&str` that may
    /// be either a bare class name or a full annotation string rather than a
    /// structured [`InferredType`].
    ///
    #[must_use]
    pub fn class_name_for_diagnostic(name: &str) -> EcoString {
        const TARGET: &str = "UndefinedObject";
        if !name.contains(TARGET) {
            return EcoString::from(name);
        }
        let is_ident_char = |c: char| c.is_ascii_alphanumeric() || c == '_';
        let mut out = String::with_capacity(name.len());
        let mut cursor = 0;
        while let Some(rel) = name[cursor..].find(TARGET) {
            let start = cursor + rel;
            let end = start + TARGET.len();
            out.push_str(&name[cursor..start]);
            // Use the original `name` for both boundary checks — the cursor
            // walks forward, but the char immediately before `start` must be
            // inspected against full context, not the (possibly empty) slice
            // we copied over this iteration.
            let before_ok = name[..start]
                .chars()
                .next_back()
                .is_none_or(|c| !is_ident_char(c));
            let after_ok = name[end..].chars().next().is_none_or(|c| !is_ident_char(c));
            if before_ok && after_ok {
                out.push_str("Nil");
            } else {
                out.push_str(TARGET);
            }
            cursor = end;
        }
        out.push_str(&name[cursor..]);
        EcoString::from(out)
    }

    /// Returns a human-readable display name for this type, using the
    /// source-sympathetic names users type in their code.
    ///
    /// Identical to [`display_name`](Self::display_name) except that
    /// `Known("UndefinedObject")` renders as `"Nil"`. Users write `:: Foo | Nil`
    /// in source and never see the canonical `UndefinedObject` spelling
    /// anywhere else, so diagnostics must never echo `UndefinedObject` back at them.
    ///
    /// Use this for any user-facing string: diagnostic messages, hover
    /// contents and signature help labels. (Not for text inserted into source,
    /// such as code actions: this form truncates long map shapes — use
    /// [`display_annotation`](Self::display_annotation) there.) Keep
    /// [`display_name`](Self::display_name) for internal bookkeeping where the
    /// canonical name is required (e.g., `is_assignable_to` lookups).
    ///
    /// **ADR 0108:** when this type's top-level provenance is
    /// [`TypeProvenance::Aliased`], the rendered string is prefixed with the
    /// alias name — `RestartStrategy (#temporary | #transient | #permanent)`
    /// rather than just the bare expansion — because this value *is* (by
    /// construction, see `Aliased`'s doc) structurally identical to that
    /// alias's declared expansion. Narrowing residuals and freshly-built
    /// unions never carry this tag (see `Aliased` and `union_of`'s docs), so
    /// they render the plain structural form with no alias breadcrumb, which
    /// is the ADR's intentional v1 scoping, not a gap.
    ///
    /// **References:** ADR 0108
    #[must_use]
    pub fn display_for_diagnostic(&self) -> Option<EcoString> {
        let structural = self.display_with_options(DisplayOptions::SOURCE_FRIENDLY);
        if let Some(alias_name) = self.alias_display_name() {
            // ADR 0147: a shape alias is a field list too long to repeat in
            // every diagnostic, so it prints as the alias name alone
            // (`WebSocketOptions`, not the expansion); `:help` shows the
            // expansion.
            if matches!(self, Self::Shape { .. }) {
                return Some(alias_name.clone());
            }
            return Some(eco_format!("{alias_name} ({structural})"));
        }
        Some(structural)
    }

    fn display_with_options(&self, opts: DisplayOptions) -> EcoString {
        match self {
            Self::Known {
                class_name,
                type_args,
                ..
            } => {
                let rendered_name: EcoString = if opts.nil_as_source_name {
                    Self::class_name_for_diagnostic(class_name.as_str())
                } else {
                    class_name.clone()
                };
                if type_args.is_empty() {
                    rendered_name
                } else {
                    let args: Vec<String> = type_args
                        .iter()
                        .map(|a| a.display_with_options(opts).to_string())
                        .collect();
                    EcoString::from(format!("{}({})", rendered_name, args.join(", ")))
                }
            }
            Self::Union { members, .. } => {
                let mut result = EcoString::new();
                for (i, m) in members.iter().enumerate() {
                    if i > 0 {
                        result.push_str(" | ");
                    }
                    result.push_str(&m.display_with_options(opts));
                }
                result
            }
            Self::Meta { class_name, .. } => {
                // Render as the source spelling `C class` (ADR 0083). Scrub
                // `UndefinedObject` → `Nil` in the source-friendly mode for
                // consistency with the other arms.
                let rendered_name: EcoString = if opts.nil_as_source_name {
                    Self::class_name_for_diagnostic(class_name.as_str())
                } else {
                    class_name.clone()
                };
                EcoString::from(format!("{rendered_name} class"))
            }
            // ADR 0144: a protocol metatype renders as its source spelling.
            Self::ProtocolMeta { protocol_name, .. } => {
                EcoString::from(format!("{protocol_name} class"))
            }
            Self::Dynamic(reason) => {
                if opts.show_dynamic_reason {
                    if let Some(desc) = reason.description() {
                        return EcoString::from(format!("Dynamic ({desc})"));
                    }
                }
                EcoString::from("Dynamic")
            }
            Self::Never => EcoString::from("Never"),
            Self::Negation { base, excluded, .. } => {
                let base_str = base.display_with_options(opts);
                let excluded_str = excluded.display_with_options(opts);
                // Parenthesise a union of removed singletons so the `\` binds
                // clearly, e.g. `Symbol \ (#a | #b)`. A lone singleton needs no
                // parens: `Symbol \ #foo`.
                if matches!(excluded.as_ref(), Self::Union { .. }) {
                    EcoString::from(format!("{base_str} \\ ({excluded_str})"))
                } else {
                    EcoString::from(format!("{base_str} \\ {excluded_str}"))
                }
            }
            Self::Shape { fields, open, .. } => {
                let shown = if opts.truncate_shapes {
                    fields.len().min(SHAPE_DISPLAY_MAX_FIELDS)
                } else {
                    fields.len()
                };
                crate::ast::render_shape(
                    fields[..shown]
                        .iter()
                        .map(|f| (f.optional, &f.key, f.value.display_with_options(opts))),
                    *open,
                    fields.len() - shown,
                )
            }
            Self::Intersection { members, .. } => {
                let mut result = EcoString::new();
                for (i, m) in members.iter().enumerate() {
                    if i > 0 {
                        result.push_str(" & ");
                    }
                    // `&` binds tighter than `|`; a union member is only
                    // reachable via explicit grouping, so parenthesise to
                    // preserve meaning.
                    if matches!(m, Self::Union { .. }) {
                        result.push('(');
                        result.push_str(&m.display_with_options(opts));
                        result.push(')');
                    } else {
                        result.push_str(&m.display_with_options(opts));
                    }
                }
                result
            }
        }
    }

    /// Builds a union type, simplifying when possible.
    ///
    /// - If all resolved members are the same type, returns that type.
    /// - If any member is `Dynamic`, returns `Dynamic` (can't validate).
    /// - Otherwise returns `Union { members, .. }` with deduplication.
    ///
    /// Provenance is derived from the first input that carries a non-default
    /// provenance (Declared or Substituted win over Inferred).
    ///
    /// A member's [`TypeProvenance::Aliased`] tag never wins this
    /// selection (see [`provenance_wins_union_default`](Self::provenance_wins_union_default)) —
    /// a freshly-built union is a *different* type from any one member's
    /// alias expansion, so it must never silently claim that member's alias
    /// identity as its own display name. `best_provenance` itself is
    /// therefore never `Aliased`, which is also why the single-member
    /// collapse case below (which checks the *member's own* provenance, not
    /// `best_provenance`) needs no corresponding change: `Aliased` already
    /// fails its `matches!(.., Inferred(_))` guard today, so an aliased
    /// singleton member is already never overwritten.
    ///
    /// **Order-dependent edge case:**
    /// when two *structurally-equal* members disagree only in provenance
    /// (e.g. `union_of([Port_aliased, Integer_plain])` where `type Port =
    /// Integer`), dedup (`flat.contains`, which is provenance-blind) keeps
    /// whichever operand was pushed first and drops the other — so the
    /// surviving member's display depends on input order. This holds
    /// for `Declared` vs. `Inferred` too; `Aliased` carries
    /// the same characteristic, not a new one.
    pub(crate) fn union_of(members: &[Self]) -> Self {
        let mut flat: Vec<InferredType> = Vec::new();
        let mut best_provenance = TypeProvenance::Inferred(Span::default());
        for m in members {
            match m {
                Self::Known { provenance, .. }
                | Self::Meta { provenance, .. }
                | Self::ProtocolMeta { provenance, .. }
                | Self::Negation { provenance, .. }
                // A `Shape` is opaque to `union_of` too: shape members stay
                // separate and are never merged into a wider shape (ADR 0147
                // § Shapes in the type algebra); duplicates collapse by
                // order-independent structural equality.
                | Self::Shape { provenance, .. }
                // `Intersection` is opaque to `union_of` (ADR 0102 §1) — treated
                // like any other opaque member: kept as-is and deduplicated by
                // structural equality, with no absorption/flattening law of its
                // own (unlike `Negation`, which `union_of` actively simplifies).
                | Self::Intersection { provenance, .. } => {
                    if matches!(best_provenance, TypeProvenance::Inferred(_))
                        && Self::provenance_wins_union_default(provenance)
                    {
                        best_provenance = provenance.clone();
                    }
                    if !flat.contains(m) {
                        flat.push(m.clone());
                    }
                }
                Self::Union {
                    members: inner,
                    provenance,
                } => {
                    if matches!(best_provenance, TypeProvenance::Inferred(_))
                        && Self::provenance_wins_union_default(provenance)
                    {
                        best_provenance = provenance.clone();
                    }
                    for inner_m in inner {
                        if let Self::Known { provenance: p, .. } = inner_m {
                            if matches!(best_provenance, TypeProvenance::Inferred(_))
                                && Self::provenance_wins_union_default(p)
                            {
                                best_provenance = p.clone();
                            }
                        }
                        if !flat.contains(inner_m) {
                            flat.push(inner_m.clone());
                        }
                    }
                }
                Self::Dynamic(reason) => return Self::Dynamic(*reason),
                Self::Never => { /* identity element — skip */ }
            }
        }
        // Apply the negation absorption law (`(Symbol \ #foo) | #foo ⇒ Symbol`),
        // drop singletons subsumed by a `Symbol`-based negation, and collapse
        // bare singletons under a bare `Symbol` (`#a | Symbol ⇒ Symbol` — this
        // fires with or without a negation present, keeping one normal form
        // per set so `intersect` stays commutative).
        Self::absorb_negations(&mut flat);
        match flat.len() {
            0 if members.iter().all(|m| matches!(m, Self::Never)) && !members.is_empty() => {
                Self::Never
            }
            0 => Self::Dynamic(DynamicReason::Unknown),
            1 => match flat.into_iter().next().unwrap() {
                Self::Known {
                    class_name,
                    type_args,
                    provenance,
                } if matches!(provenance, TypeProvenance::Inferred(_))
                    && !matches!(best_provenance, TypeProvenance::Inferred(_)) =>
                {
                    Self::Known {
                        class_name,
                        type_args,
                        provenance: best_provenance,
                    }
                }
                only => only,
            },
            _ => Self::Union {
                members: flat,
                provenance: best_provenance,
            },
        }
    }

    /// Whether a member's provenance is eligible to become a freshly-built
    /// union's own "best provenance" (see [`union_of`](Self::union_of)).
    ///
    /// `Inferred` never wins (it's the no-information default). `Aliased`
    /// (ADR 0108) never wins either, even though it's informative —
    /// letting it win would mean a union like `A | Integer` (where alias `A`
    /// resolves to some `InferredType` tagged `Aliased("A", _)`) could
    /// silently render as `A (...)`, even though `A | Integer` is not
    /// structurally identical to `A`'s own expansion. Only a member's alias
    /// tag on *itself* is ever display-accurate; a container built from it is
    /// a different type and must never inherit that identity.
    fn provenance_wins_union_default(p: &TypeProvenance) -> bool {
        !matches!(
            p,
            TypeProvenance::Inferred(_) | TypeProvenance::Aliased { .. }
        )
    }

    /// Returns `true` if `name` is a symbol singleton (`#foo`) — a subtype of
    /// `Symbol` in the type checker's singleton convention.
    fn is_symbol_singleton(name: &str) -> bool {
        crate::semantic_analysis::string_utils::is_symbol_singleton_name(name)
    }

    /// Returns `true` if `ty` is the base of a well-formed **singleton-excluded**
    /// [`Negation`] — i.e. `Known("Symbol")`. `Symbol` is the sole supertype of
    /// singletons the type checker models, so it is the only base for which
    /// singleton subtraction and absorption are sound.
    ///
    /// Distinct from the **nominal-excluded** `Negation` flavour (ADR 0102 §5,
    /// e.g. `Object \ Number`), whose base is any nominal class — see
    /// the nominal-class arm of [`difference`](Self::difference). The two
    /// flavours are distinguished structurally by whether `excluded` is a
    /// singleton/union-of-singletons or a nominal class name; this predicate
    /// only ever needs to ask "is this the Symbol-singleton flavour".
    ///
    /// [`Negation`]: InferredType::Negation
    fn is_symbol_base(ty: &Self) -> bool {
        matches!(ty, Self::Known { class_name, .. } if class_name == "Symbol")
    }

    /// Returns `true` if every symbol singleton (`#foo`) is a member of `ty`'s
    /// value set — i.e. `ty` is `Symbol` itself, or (with a hierarchy) a
    /// nominal *supertype* of `Symbol` such as `ProtoObject`.
    ///
    /// Used by the symbol-singleton arms of [`intersect`](Self::intersect) so
    /// `ProtoObject ∩ #foo` reduces to `#foo` instead of falling through to
    /// the nominal arm (where `is_nominal_subtype(h, "#foo", "ProtoObject")`
    /// is `false` — singletons are never hierarchy entries) and yielding a
    /// spurious `Never`. `Object` is already absorbed by the top-identity
    /// arms, so in practice the hierarchy case only fires for supertypes
    /// *above* `Object` (`ProtoObject` in the builtin hierarchy). Without a
    /// hierarchy this is exactly [`is_symbol_base`](Self::is_symbol_base) —
    /// the previous, conservative behaviour.
    ///
    /// Note this deliberately does **not** widen the *singleton-excluded*
    /// [`Negation`] well-formedness: `is_symbol_base` (exactly `Symbol`)
    /// remains the only valid base for subtracting a *singleton*, so
    /// `difference(ProtoObject, #foo)` stays a no-op rather than fabricating a
    /// `ProtoObject \ #foo` complement — `ProtoObject` is not itself a
    /// hierarchy entry above which singleton subtraction was specified. This
    /// is unrelated to the *nominal-excluded* flavour (ADR 0102 §5),
    /// which admits any nominal class as `base` when the excluded value is
    /// itself a nominal class, not a singleton.
    ///
    /// [`Negation`]: InferredType::Negation
    fn admits_symbol_singletons(ty: &Self, hierarchy: Option<&ClassHierarchy>) -> bool {
        match ty {
            Self::Known { class_name, .. } => {
                class_name == "Symbol"
                    || hierarchy.is_some_and(|h| Self::is_nominal_subtype(h, "Symbol", class_name))
            }
            _ => false,
        }
    }

    /// Builds a canonical [`Negation`](Self::Negation) — the sole construction
    /// path, so every `Negation` in the system carries a **canonically ordered**
    /// `excluded` set (ADR 0102 §1: members sorted ascending by `class_name`).
    ///
    /// Order-independent `PartialEq` already treats `Symbol \ (#a | #b)` and
    /// `Symbol \ (#b | #a)` as equal; canonical ordering additionally makes the
    /// stored form *deterministic* across serialisation round-trips and
    /// independent implementations, so `Union` dedup and diagnostic output are
    /// stable.
    fn make_negation(base: Self, excluded: Self, provenance: TypeProvenance) -> Self {
        Self::Negation {
            base: Box::new(base),
            excluded: Box::new(Self::canonical_excluded(excluded)),
            provenance,
        }
    }

    /// Sorts a `Negation`'s `excluded` union members ascending by `class_name`
    /// (ADR 0102 §1). A lone singleton (or any non-`Union`) is already canonical.
    fn canonical_excluded(excluded: Self) -> Self {
        match excluded {
            Self::Union {
                mut members,
                provenance,
            } => {
                members.sort_by(|x, y| Self::excluded_sort_key(x).cmp(&Self::excluded_sort_key(y)));
                Self::Union {
                    members,
                    provenance,
                }
            }
            other => other,
        }
    }

    /// Sort key for a canonical `excluded` member — the singleton's
    /// `class_name` (e.g. `#foo`). Non-`Known` members fall back to their
    /// display name so ordering is still total.
    fn excluded_sort_key(member: &Self) -> EcoString {
        match member {
            Self::Known { class_name, .. } => class_name.clone(),
            other => other.display_name(),
        }
    }

    /// Flattens `excluded` (a singleton or a normalised union of singletons)
    /// into its constituent members.
    fn excluded_members(excluded: &Self) -> Vec<Self> {
        match excluded {
            Self::Union { members, .. } => members.clone(),
            other => vec![other.clone()],
        }
    }

    /// Applies the full negation absorption-law set (ADR 0102 §1) in place.
    ///
    /// All laws below hold because a symbol singleton is a subtype of `Symbol`
    /// (the only supertype-of-singleton relationship modelled here); other
    /// members are left untouched.
    ///
    /// - **Bare-base subsumption:** `(Symbol \ E) | Symbol = Symbol` — a bare
    ///   `Symbol` swallows every symbol-based negation and singleton. This law
    ///   applies **even with no `Negation` present** (`#a | Symbol = Symbol`):
    ///   the singleton is a subtype of `Symbol`, so the collapsed form admits
    ///   exactly the same values. Gating it on negation presence gave the same
    ///   set two normal forms depending on evaluation order, breaking
    ///   `intersect` commutativity (a Windows CI counterexample surfaced this:
    ///   `intersect(#a | Symbol, (Symbol \ #a) | Object)` vs the swapped
    ///   order).
    /// - **Full / partial singleton absorption:** a bare singleton `#s` is
    ///   redundant beside a `Symbol`-based negation, so it is dropped, and when
    ///   it appears in the negation's `excluded` set it is added back by removing
    ///   it from `excluded` (`(Symbol \ (#a | #b)) | #a = Symbol \ #b`). When
    ///   `excluded` empties, the negation collapses to bare `Symbol`.
    /// - **Same-base complement union:** `(Symbol \ E1) | (Symbol \ E2) =
    ///   Symbol \ (E1 ∩ E2)`, so logically-equal negations never escape dedup and
    ///   unions do not grow across repeated narrowing/widening passes.
    fn absorb_negations(flat: &mut Vec<Self>) {
        let has_symbol_negation = flat
            .iter()
            .any(|m| matches!(m, Self::Negation { base, .. } if Self::is_symbol_base(base)));
        // Bare-singleton-under-bare-`Symbol` subsumption fires without any
        // negation too (see doc comment) — same pass, one code path.
        let has_bare_symbol_and_singleton = flat.iter().any(Self::is_symbol_base)
            && flat.iter().any(
                |m| matches!(m, Self::Known { class_name, .. } if Self::is_symbol_singleton(class_name)),
            );
        if !has_symbol_negation && !has_bare_symbol_and_singleton {
            return;
        }

        // Partition members into the symbol-ish parts (which collapse into a
        // single result) and everything else (untouched).
        let mut bare_symbol: Option<Self> = None;
        let mut neg_excludeds: Vec<Self> = Vec::new();
        let mut neg_provenance: Option<TypeProvenance> = None;
        let mut bare_singletons: Vec<EcoString> = Vec::new();
        let mut others: Vec<Self> = Vec::new();

        for m in flat.drain(..) {
            match m {
                Self::Negation {
                    base,
                    excluded,
                    provenance,
                } if Self::is_symbol_base(&base) => {
                    // The merged negation's `excluded` set is the union of
                    // every absorbed negation's own set, so it's structurally
                    // different from whichever single input this provenance
                    // was captured from — strip a stale `Aliased` tag for the
                    // same reason `difference`'s same-base flattening arm
                    // does (see `TypeProvenance::strip_alias`'s doc).
                    neg_provenance.get_or_insert(provenance.strip_alias());
                    neg_excludeds.push(*excluded);
                }
                Self::Known { ref class_name, .. } if class_name == "Symbol" => {
                    bare_symbol.get_or_insert(m);
                }
                Self::Known { ref class_name, .. } if Self::is_symbol_singleton(class_name) => {
                    bare_singletons.push(class_name.clone());
                }
                other => others.push(other),
            }
        }

        let provenance = neg_provenance.unwrap_or(TypeProvenance::Inferred(Span::default()));

        let symbol_part = if let Some(sym) = bare_symbol {
            // Bare-base subsumption: `Symbol` swallows the negations/singletons.
            sym
        } else {
            // Same-base complement union: merge every negation's `excluded` via
            // intersection (`E1 ∩ E2 ∩ …`).
            let mut merged = neg_excludeds[0].clone();
            for e in &neg_excludeds[1..] {
                // Structural singleton merge only — no hierarchy or protocol
                // registry needed (and neither is threaded through `union_of`).
                merged = Self::intersect(&merged, e, provenance.clone(), None, None);
            }
            // Add back any excluded singleton that also appears bare (absorption).
            let remaining: Vec<Self> = if matches!(merged, Self::Never) {
                Vec::new()
            } else {
                Self::excluded_members(&merged)
                    .into_iter()
                    .filter(|e| match e {
                        Self::Known { class_name, .. } => !bare_singletons.contains(class_name),
                        _ => true,
                    })
                    .collect()
            };
            if remaining.is_empty() {
                // `Symbol \ {} = Symbol`.
                Self::known("Symbol")
            } else {
                Self::make_negation(
                    Self::known("Symbol"),
                    Self::union_of(&remaining),
                    provenance,
                )
            }
        };

        // Reassemble (symbol part first), deduplicating the untouched members.
        let mut deduped: Vec<Self> = Vec::with_capacity(others.len() + 1);
        deduped.push(symbol_part);
        for m in others {
            if !deduped.contains(&m) {
                deduped.push(m);
            }
        }
        *flat = deduped;
    }

    /// `shape ∩ Known(class_name)` (ADR 0147): a map shape is a `Dictionary`,
    /// so a test against `Dictionary` or one of its superclasses keeps the
    /// shape; any other class is disjoint.
    fn intersect_shape_with_class(
        shape: &Self,
        class_name: &str,
        hierarchy: Option<&ClassHierarchy>,
    ) -> Self {
        let is_dictionary = class_name == "Dictionary"
            || hierarchy.is_some_and(|h| Self::is_nominal_subtype(h, "Dictionary", class_name));
        if is_dictionary {
            shape.clone()
        } else {
            Self::Never
        }
    }

    /// Normalising **intersection** `A ∩ B` (ADR 0102 §1).
    ///
    /// Symbol-singleton membership IS modelled — a singleton `#foo` is a subtype
    /// of `Symbol`, so `Symbol ∩ #foo = #foo`. The general **nominal-class base
    /// case** is also modelled *when a [`ClassHierarchy`] is supplied*
    /// (`hierarchy = Some(_)`): `intersect(A, B) = B` when `B <: A`
    /// (e.g. `Number ∩ Integer = Integer`), `= A` when `A <: B`, and `= Never`
    /// when `A` and `B` are hierarchy-unrelated (single inheritance ⇒ unrelated
    /// classes are disjoint, so `Integer ∩ String = Never`). Only nominal
    /// *difference* (`Object \ Number`) stays out of scope (see
    /// [`difference`](Self::difference)).
    ///
    /// The `hierarchy` argument is **optional** so the internal structural
    /// callers (`union_of`'s complement-merge in
    /// [`absorb_negations`](Self::absorb_negations)) can pass `None` — they only
    /// merge singletons, which never need the hierarchy. When `None`, two
    /// distinct non-symbol `Known` classes fall through to `Never` (the previous
    /// behaviour). Narrowing call sites pass `Some(hierarchy)` so class narrowing
    /// reduces correctly.
    ///
    /// The `protocol_registry` argument is likewise **optional**
    /// (ADR 0102 §1/§3): supplying it lets `intersect` recognise a
    /// protocol name and route class ∩ protocol / protocol ∩ protocol to the
    /// stored [`Intersection`](Self::Intersection) instead of falling through
    /// to the disjoint default. `None` preserves the plain structural
    /// behaviour (two distinct-named `Known` types with no proven relation
    /// are `Never`).
    ///
    /// Rules:
    /// - **Dynamic first:** `intersect(Dynamic, P) = P` (Dynamic acts as the top
    ///   type for intersection — *not* union's "Dynamic absorbs" rule). Checked
    ///   before identity/Object so `intersect(Dynamic, Object) = Object`.
    /// - `intersect(T, T) = T`; `intersect(T, Never) = Never`;
    ///   `intersect(T, Object) = T`.
    /// - **Intersect through a complement (both orders):**
    ///   `intersect(Negation{B, E}, P) = difference(intersect(B, P), E)` and
    ///   symmetrically `intersect(P, Negation{B, E}) =
    ///   difference(intersect(P, B), E)` — a `Negation` never reaches the
    ///   disjoint default (needed for chained narrowing / Phase 2 `\` syntax).
    /// - **Symbol-singleton membership:** `intersect(Symbol, #foo) = #foo`.
    /// - **Class ∩ protocol / protocol ∩ protocol** (`protocol_registry =
    ///   Some`, ADR 0102 §1/§3): when the two distinct-named `Known` operands
    ///   include at least one protocol name, the result is the normalised,
    ///   irreducible [`Intersection`](Self::Intersection) — e.g.
    ///   `Collection(Object) ∩ Comparable`. Checked before the nominal-class
    ///   base case because a protocol is never an entry in the class
    ///   hierarchy's subtype lattice, and protocols/classes share one
    ///   namespace (collisions are compile errors), so `has_protocol(name)`
    ///   alone identifies a protocol without consulting the hierarchy.
    /// - **Nominal-class base case** (`hierarchy = Some`): `B <: A ⇒ B`,
    ///   `A <: B ⇒ A`, unrelated ⇒ `Never`.
    /// - **LHS-union distribution:** `intersect(T1 | … | Tn, P) =
    ///   (T1 ∩ P) | … | (Tn ∩ P)`, e.g. `intersect(Integer | #infinity,
    ///   #infinity)` normalises to the bare singleton `#infinity`.
    /// - **RHS-union fold:** `intersect(T, A | B) = (T ∩ A) | (T ∩ B)`.
    /// - Generics compare exactly, including `type_args`.
    ///
    /// The result is re-normalised through [`union_of`](Self::union_of).
    #[allow(clippy::too_many_lines)] // one arm per type-pair rule; splitting hides the ordering
    pub(crate) fn intersect(
        a: &Self,
        b: &Self,
        provenance: TypeProvenance,
        hierarchy: Option<&ClassHierarchy>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> Self {
        match (a, b) {
            // Dynamic FIRST (before identity/Object): `intersect(Dynamic, P) = P`
            // refines the unknown, so `intersect(Dynamic, Object) = Object`.
            (Self::Dynamic(_), _) => b.clone(),
            (_, Self::Dynamic(_)) => a.clone(),
            // Never annihilates.
            (Self::Never, _) | (_, Self::Never) => Self::Never,
            // Object (top) is the identity: `T ∩ Object = T`.
            (_, other) if Self::is_object(other) => a.clone(),
            (other, _) if Self::is_object(other) => b.clone(),
            // Intersect through a complement (both orders): `(B \ E) ∩ P =
            // (B ∩ P) \ E`. A `Negation` must never fall through to the disjoint
            // default (the `(Known, Negation)` case fires in Phase 2's `\`
            // syntax; missing it silently loses narrowing).
            (Self::Negation { base, excluded, .. }, _) => Self::difference(
                &Self::intersect(base, b, provenance.clone(), hierarchy, protocol_registry),
                excluded,
                provenance,
                hierarchy,
            ),
            (_, Self::Negation { base, excluded, .. }) => Self::difference(
                &Self::intersect(a, base, provenance.clone(), hierarchy, protocol_registry),
                excluded,
                provenance,
                hierarchy,
            ),
            // LHS-union distribution.
            (Self::Union { members, .. }, _) => {
                let parts: Vec<Self> = members
                    .iter()
                    .map(|m| {
                        Self::intersect(m, b, provenance.clone(), hierarchy, protocol_registry)
                    })
                    .collect();
                Self::union_of(&parts)
            }
            // RHS-union fold.
            (_, Self::Union { members, .. }) => {
                let parts: Vec<Self> = members
                    .iter()
                    .map(|m| {
                        Self::intersect(a, m, provenance.clone(), hierarchy, protocol_registry)
                    })
                    .collect();
                Self::union_of(&parts)
            }
            // Intersect through an existing stored Intersection (both
            // orders) — flatten rather than falling through to the disjoint
            // default (ADR 0102 §1/§3). Needed so a 3+-way `&`
            // chain (`A & B & C`, parsed left-associatively as `(A & B) &
            // C`) reduces through repeated pairwise `intersect` calls to a
            // single flat, deduplicated `Intersection` instead of silently
            // losing a member. Checked after the `Union` arms above so
            // `intersect(Intersection{..}, T1 | T2)` still distributes over
            // the union first — by this point neither operand is a `Union`
            // (those arms already matched), `Dynamic`/`Never`/`Object`
            // (matched even earlier), or a `Negation` (matched above too).
            (Self::Intersection { members, .. }, _) => {
                // Re-check pairwise disjointness before appending: without
                // this, `Printable & Integer & String` would flatten to
                // `Intersection{Integer, Printable, String}` instead of
                // reducing to `Never` (`Integer ∩ String = Never`), violating
                // the stored-intersection invariant.
                for m in members {
                    let pair =
                        Self::intersect(m, b, provenance.clone(), hierarchy, protocol_registry);
                    if matches!(pair, Self::Never) {
                        return Self::Never;
                    }
                }
                let mut merged = members.clone();
                merged.push(b.clone());
                Self::normalize_intersection(merged, provenance)
            }
            (_, Self::Intersection { members, .. }) => {
                for m in members {
                    let pair =
                        Self::intersect(a, m, provenance.clone(), hierarchy, protocol_registry);
                    if matches!(pair, Self::Never) {
                        return Self::Never;
                    }
                }
                let mut merged = members.clone();
                merged.push(a.clone());
                Self::normalize_intersection(merged, provenance)
            }
            // ADR 0147: a map shape is a `Dictionary`, so an `isKindOf:` /
            // type-pattern test against `Dictionary` (or one of its
            // superclasses) keeps the shape; any other class is disjoint.
            (Self::Shape { .. }, Self::Known { class_name, .. }) => {
                Self::intersect_shape_with_class(a, class_name, hierarchy)
            }
            (Self::Known { class_name, .. }, Self::Shape { .. }) => {
                Self::intersect_shape_with_class(b, class_name, hierarchy)
            }
            // Symbol-singleton membership: `#foo <: Symbol`, so the narrower
            // singleton is kept in both orders. Checked before the general
            // nominal case because singletons are not entries in the hierarchy
            // — the nominal arm's `is_nominal_subtype(h, "#foo", …)` would
            // always answer `false`. With a hierarchy, the same rule extends
            // to every nominal *supertype* of `Symbol` (e.g. `ProtoObject`):
            // `ProtoObject ∩ #foo = #foo`, matching the
            // pre-ADR-0102 hierarchy walk. (`Object` never reaches here — the
            // top-identity arms above already handled it.)
            (Self::Known { .. }, Self::Known { class_name: s, .. })
                if Self::admits_symbol_singletons(a, hierarchy) && Self::is_symbol_singleton(s) =>
            {
                b.clone()
            }
            (Self::Known { class_name: s, .. }, Self::Known { .. })
                if Self::admits_symbol_singletons(b, hierarchy) && Self::is_symbol_singleton(s) =>
            {
                a.clone()
            }
            // `T ∩ T = T` (exact structural equality, generics included).
            _ if a == b => a.clone(),
            // A bare generic class name denotes every instantiation of the
            // class (a type pattern or `isKindOf:` cannot name type args), so
            // it is the identity on any instantiation of the same class:
            // `Result(String, Error) ∩ Result = Result(String, Error)`, in
            // both orders (BT-4510: the `r :: Result ->` arm of a match on a
            // `Result(String, Error)` was typed `Never`).
            _ if Self::bare_instantiation_of(b, a) => a.clone(),
            _ if Self::bare_instantiation_of(a, b) => b.clone(),
            // Class ∩ protocol / protocol ∩ protocol (ADR 0102 §1/§3):
            // the irreducible intersection. Must be checked before the nominal
            // arm below — a protocol name is never a hierarchy entry, so
            // without this arm it would silently fall through to `Never`.
            (Self::Known { class_name: an, .. }, Self::Known { class_name: bn, .. })
                if an != bn
                    && protocol_registry
                        .is_some_and(|r| r.has_protocol(an) || r.has_protocol(bn)) =>
            {
                Self::normalize_intersection(vec![a.clone(), b.clone()], provenance)
            }
            // Nominal-class base case (ADR 0102 §1): with a hierarchy, two
            // distinct-named `Known` classes relate via subtyping — the subclass
            // wins (`Number ∩ Integer = Integer`), and hierarchy-unrelated
            // classes are disjoint (`Integer ∩ String = Never`). Without a
            // hierarchy, or when the names match but generics differ (invariant
            // args ⇒ disjoint), fall through to `Never` — the previous
            // structural-only behaviour.
            (Self::Known { class_name: an, .. }, Self::Known { class_name: bn, .. }) => {
                match hierarchy {
                    Some(h) if an != bn => {
                        if Self::is_nominal_subtype(h, bn, an) {
                            b.clone()
                        } else if Self::is_nominal_subtype(h, an, bn) {
                            a.clone()
                        } else {
                            Self::Never
                        }
                    }
                    _ => Self::Never,
                }
            }
            // ADR 0144: `P class & Q class` (or `C class & P class`) is the
            // class object of a class conforming to both — irreducible, like
            // protocol ∩ protocol above, never disjoint.
            (Self::ProtocolMeta { .. }, Self::ProtocolMeta { .. } | Self::Meta { .. })
            | (Self::Meta { .. }, Self::ProtocolMeta { .. }) => {
                Self::normalize_intersection(vec![a.clone(), b.clone()], provenance)
            }
            // Distinct concrete types with no structural relationship (including
            // distinct singletons, `#foo ∩ #bar`) are disjoint.
            _ => Self::Never,
        }
    }

    /// Builds a normalised [`Intersection`](Self::Intersection) — the sole
    /// construction path (ADR 0102 §1), analogous to
    /// [`make_negation`](Self::make_negation).
    ///
    /// - **Flattens** nested `Intersection` members into the parent (an
    ///   `Intersection` never contains another `Intersection`).
    /// - **Deduplicates** by structural equality.
    /// - **Degenerate case:** if flattening/dedup leaves a single member, it
    ///   is returned bare rather than wrapped (mirrors `union_of`'s
    ///   single-member collapse).
    /// - Sorted by display name so independent constructions of the same
    ///   logical intersection produce the same stored order (equality is
    ///   already order-independent; this makes serialisation/output
    ///   deterministic too, mirroring `Negation`'s canonical `excluded`
    ///   ordering).
    fn normalize_intersection(raw: Vec<Self>, provenance: TypeProvenance) -> Self {
        let mut flat: Vec<Self> = Vec::with_capacity(raw.len());
        for m in raw {
            match m {
                Self::Intersection { members, .. } => {
                    for inner in members {
                        if !flat.contains(&inner) {
                            flat.push(inner);
                        }
                    }
                }
                other => {
                    if !flat.contains(&other) {
                        flat.push(other);
                    }
                }
            }
        }
        // A bare generic member is subsumed by an instantiation of the same
        // class (`Result & Result(String, Error)` is `Result(String,
        // Error)`), so it is dropped rather than kept beside it (BT-4510).
        let snapshot = flat.clone();
        flat.retain(|m| !snapshot.iter().any(|o| Self::bare_instantiation_of(m, o)));
        flat.sort_by_key(Self::display_name);
        match flat.len() {
            // Unreachable from `intersect`'s call site (which always passes two
            // distinct members), kept for a total/defensive standalone helper.
            0 => Self::Never,
            1 => flat.into_iter().next().unwrap_or(Self::Never),
            _ => Self::Intersection {
                members: flat,
                provenance,
            },
        }
    }

    /// Returns `true` if `sub` is `sup` or a (transitive) subclass of `sup` in
    /// the nominal class hierarchy. Symbol singletons (`#foo`) are not entries in
    /// the hierarchy, so this returns `false` for them (their membership is
    /// handled by the explicit `Symbol`-singleton arms in
    /// [`intersect`](Self::intersect)).
    ///
    /// The reflexive `sub == sup` short-circuit is unreachable from `intersect`
    /// (its match arm guards on differing names) but is load-bearing for the
    /// `:: Class` argument guard in `check_argument_types_impl` (BT-4599), where
    /// an argument typed exactly `Class` must match.
    pub(crate) fn is_nominal_subtype(hierarchy: &ClassHierarchy, sub: &str, sup: &str) -> bool {
        sub == sup || hierarchy.superclass_chain(sub).iter().any(|c| c == sup)
    }

    /// Normalising **difference** `A \ B` (ADR 0102 §1, §5).
    ///
    /// Rules:
    /// - Boundary: `difference(Never, P) = Never`; `difference(T, Never) = T`.
    /// - **Dynamic asymmetry:** `difference(Dynamic, P) = Dynamic` and
    ///   `difference(T, Dynamic) = T` (Dynamic is opaque — *not* union's
    ///   "Dynamic absorbs" rule).
    /// - **RHS-union fold:** `difference(T, A | B) =
    ///   difference(difference(T, A), B)`.
    /// - **LHS-union distribution:** `difference(T1 | … | Tn, P) =
    ///   (T1 \ P) | … | (Tn \ P)`, which drops any member exactly equal to `P`.
    /// - `difference(Symbol, #foo) = Negation{Symbol, #foo}`; the removed set is
    ///   an [`InferredType`] (a singleton or normalised union of singletons),
    ///   so `difference(Symbol, #a | #b) = Negation{Symbol, #a | #b}`.
    /// - **Same-base flattening:** `difference(Negation{Symbol, E}, #bar) =
    ///   Negation{Symbol, union_of(E, #bar)}` — nested negation never escapes
    ///   normal form.
    /// - **Nominal-class base case** (ADR 0102 §5, `hierarchy = Some`):
    ///   for two distinct-named, non-singleton nominal classes, `difference(A,
    ///   B)` ⇒ `Negation{A, B}` when `B` is a strict subclass of `A` (the
    ///   class-hierarchy analogue of `Symbol \ #foo`); ⇒ `Never` when `A <: B`
    ///   (every instance of `A` is also a `B`); ⇒ `A` unchanged when
    ///   hierarchy-unrelated (nothing to remove). Without a hierarchy, or when
    ///   either operand is a singleton (never a hierarchy entry — that is the
    ///   dedicated `Symbol \ #foo` arm above), falls through to the structural
    ///   default. `excluded` stays a single class (never a union of classes —
    ///   out of scope per ADR 0102 §5 scope note).
    /// - `Meta` and `Dynamic` are opaque to `Negation`.
    /// - Generics compare exactly, including `type_args`.
    pub(crate) fn difference(
        a: &Self,
        b: &Self,
        provenance: TypeProvenance,
        hierarchy: Option<&ClassHierarchy>,
    ) -> Self {
        match (a, b) {
            // `T \ Dynamic = T` and `T \ Never = T` (subtracting these removes
            // nothing), plus `Dynamic \ P = Dynamic` — here `a` *is* `Dynamic`,
            // so `a.clone()` is correct. That last case is the asymmetry vs
            // union's "Dynamic absorbs" rule (ADR 0102 §1).
            (Self::Dynamic(_), _) | (_, Self::Dynamic(_) | Self::Never) => a.clone(),
            // Boundary: `Never \ P = Never`.
            (Self::Never, _) => Self::Never,
            // RHS-union fold: subtract each removed member in turn.
            (_, Self::Union { members, .. }) => members.iter().fold(a.clone(), |acc, m| {
                Self::difference(&acc, m, provenance.clone(), hierarchy)
            }),
            // LHS-union distribution: `(A | B) \ P = (A \ P) | (B \ P)`.
            (Self::Union { members, .. }, _) => {
                let parts: Vec<Self> = members
                    .iter()
                    .map(|m| Self::difference(m, b, provenance.clone(), hierarchy))
                    .collect();
                Self::union_of(&parts)
            }
            // Same-base flattening: fold the newly removed singleton into the
            // existing negation. Only well-formed `Symbol`-based negations and
            // singleton subtractions apply.
            (
                Self::Negation {
                    base,
                    excluded,
                    provenance: neg_prov,
                },
                Self::Known { class_name, .. },
            ) if Self::is_symbol_base(base) && Self::is_symbol_singleton(class_name) => {
                Self::make_negation(
                    (**base).clone(),
                    Self::union_of(&[(**excluded).clone(), b.clone()]),
                    neg_prov.clone().strip_alias(),
                )
            }
            // `Symbol \ #foo = Negation{Symbol, #foo}`.
            (
                Self::Known { class_name, .. },
                Self::Known {
                    class_name: sym, ..
                },
            ) if class_name == "Symbol" && Self::is_symbol_singleton(sym) => {
                Self::make_negation(a.clone(), b.clone(), provenance)
            }
            // `T \ T = Never` (exact structural equality, generics included).
            _ if a == b => Self::Never,
            // Subtracting a bare generic class name removes every
            // instantiation of it: `Result(String, Error) \ Result = Never`
            // (the residual after an unguarded `r :: Result ->` arm, BT-4510).
            // The converse (`Result \ Result(String, Error)`) is not
            // representable and falls through to the no-op below.
            _ if Self::bare_instantiation_of(b, a) => Self::Never,
            // Nominal-class base case (ADR 0102 §5): with a hierarchy,
            // two distinct-named, non-singleton nominal classes relate via
            // subtyping — a proper subclass subtraction is irreducible
            // (`Negation{A, B}`), a supertype/self subtraction removes
            // everything (`Never`, though `A == B` is already caught above),
            // and hierarchy-unrelated classes are disjoint (no-op). Singletons
            // are never hierarchy entries, so they are explicitly excluded —
            // they are handled by the dedicated `Symbol \ #foo` arm above.
            (Self::Known { class_name: an, .. }, Self::Known { class_name: bn, .. })
                if an != bn
                    && !Self::is_symbol_singleton(an)
                    && !Self::is_symbol_singleton(bn)
                    && hierarchy.is_some() =>
            {
                let h = hierarchy.unwrap_or_else(|| unreachable!("checked by guard above"));
                if Self::is_nominal_subtype(h, bn, an) {
                    // `B <: A` (proper, since `an != bn`).
                    Self::make_negation(a.clone(), b.clone(), provenance)
                } else if Self::is_nominal_subtype(h, an, bn) {
                    // `A <: B` — every instance of `A` is also a `B`.
                    Self::Never
                } else {
                    // Hierarchy-unrelated — disjoint, nothing to remove.
                    a.clone()
                }
            }
            // Nothing structurally removable — no-op (no hierarchy supplied,
            // or hierarchy-unrelated nominal classes; `Meta`/`Dynamic` are
            // opaque to `Negation`).
            _ => a.clone(),
        }
    }

    /// Whether `bare` is a generic class named without type arguments and
    /// `other` is a parameterized instantiation of the same class
    /// (`Result` and `Result(String, Error)`): the bare name admits every
    /// instantiation, so it is a supertype of `other` (BT-4510).
    fn bare_instantiation_of(bare: &Self, other: &Self) -> bool {
        matches!(
            (bare, other),
            (
                Self::Known { class_name: bn, type_args: bargs, .. },
                Self::Known { class_name: on, type_args: oargs, .. },
            ) if bn == on && bargs.is_empty() && !oargs.is_empty()
        )
    }

    /// Returns `true` if `ty` is the `Object` root class — the top type for
    /// [`intersect`](Self::intersect) (`T ∩ Object = T`).
    fn is_object(ty: &Self) -> bool {
        matches!(ty, Self::Known { class_name, type_args, .. }
            if class_name == "Object" && type_args.is_empty())
    }
}

/// Returns `true` if the type name looks like an unresolved generic type parameter
/// (e.g., `V`, `K`, `T`, `R`, `E`).
///
/// Generic type parameters are single uppercase letters that come from class
/// type parameter lists. When a generic method like `Dictionary at:ifAbsent:`
/// returns `V`, the type checker reports `V` as the variable's type. This
/// helper identifies such types so diagnostics can provide better context.
pub(in crate::semantic_analysis) fn is_generic_type_param(name: &str) -> bool {
    let bytes = name.as_bytes();
    bytes.len() == 1 && bytes[0].is_ascii_uppercase()
}

/// `true` when `name` is a bare type-variable placeholder (`E`, `T`, `V`):
/// shaped like a generic type param ([`is_generic_type_param`]) and *not* a
/// real class in the hierarchy. A user class that happens to be named with a
/// single uppercase letter is a class, judged nominally like any other. The
/// one rule every "is this an unresolved type variable?" check shares
/// (BT-4233).
pub(in crate::semantic_analysis) fn is_type_var_placeholder(
    name: &str,
    hierarchy: &ClassHierarchy,
) -> bool {
    is_generic_type_param(name) && !hierarchy.has_class(name)
}

// ── Diagnostic facts ──────────────────────────────────────────────────────
//
// A shared leaf between `inference/` and `validation.rs` (this module is a
// dependency of both already, via `InferredType`/`DynamicReason`): each
// struct below is a plain data record of something an `inference/` site
// *detected*, with no `Diagnostic`/message-formatting logic of its own.
// `inference/` constructs one and hands it to the matching `validation.rs`
// `emit_*` method, which does the actual rendering (message text, hint,
// category, severity) and pushes it. Splitting detection from rendering
// this way means inference never
// constructs `Diagnostic`s itself for these four sites. (The cons-tail site
// already had this split, via `InferredType::improper_cons_tail_display`'s
// existing `Option<EcoString>` — that one carries no new struct here, only
// its `validation.rs` renderer moved.)

/// A class-object type as the slot/argument assignability check sees it
/// (ADR 0083, ADR 0144): the class object of one named class (`Meta{C}`) or
/// of some class conforming to a protocol (`ProtocolMeta{P}`). Both render
/// as `<name> class`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::semantic_analysis::type_checker) enum ClassObjectType<'a> {
    /// `C class` — `Meta{C}`.
    Class(&'a EcoString),
    /// `P class` — `ProtocolMeta{P}`.
    Protocol(&'a EcoString),
}

impl<'a> ClassObjectType<'a> {
    /// The class-object view of `ty`, or `None` when it is not a class object.
    pub(in crate::semantic_analysis::type_checker) fn of(ty: &'a InferredType) -> Option<Self> {
        match ty {
            InferredType::Meta { class_name, .. } => Some(Self::Class(class_name)),
            InferredType::ProtocolMeta { protocol_name, .. } => Some(Self::Protocol(protocol_name)),
            _ => None,
        }
    }
}

impl std::fmt::Display for ClassObjectType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Class(name) | Self::Protocol(name) => write!(f, "{name} class"),
        }
    }
}

/// An instance-method override whose declared return type does not refine
/// the one it inherits. Found by `validation.rs::override_return_widening`,
/// rendered by `check_override_return_compatibility`, and consulted by
/// `ProtocolRegistry::conformance_return_type_mismatches` (BT-4175).
pub(crate) struct OverrideReturnWidening {
    pub(crate) child_rt: DeclaredType,
    pub(crate) parent_rt: DeclaredType,
    pub(crate) parent_class: EcoString,
}

/// A declared-vs-inferred type mismatch on an assignment's type annotation
/// (`x :: Declared := value`). Detected in `inference/assignment.rs`,
/// rendered by `validation.rs::emit_assignment_type_mismatch`.
pub(in crate::semantic_analysis::type_checker) struct AssignmentTypeMismatch {
    pub(in crate::semantic_analysis::type_checker) declared_display: EcoString,
    pub(in crate::semantic_analysis::type_checker) inferred_display: EcoString,
    /// ADR 0144: when a class object `C class` misses a declared `P class`,
    /// the protocol `P` and the selectors `C` is missing — rendered as
    /// `C class does not conform to P class` instead of the plain mismatch.
    pub(in crate::semantic_analysis::type_checker) nonconformance: Option<MetatypeNonconformance>,
}

/// ADR 0144: why a class object `C class` is not a declared `P class` — the
/// selectors `C` is missing, and (BT-4490) the requirements it answers with
/// an incompatible declared return type, the same two findings
/// `ProtocolInfo::subsumes_conformance_to` judges for `P class <: Q class`.
pub(in crate::semantic_analysis::type_checker) struct MetatypeNonconformance {
    pub(in crate::semantic_analysis::type_checker) protocol: EcoString,
    /// As `ProtocolRegistry::check_conformance` reports them; class-side
    /// ones are prefixed `class `.
    pub(in crate::semantic_analysis::type_checker) missing: Vec<EcoString>,
    pub(in crate::semantic_analysis::type_checker) return_mismatches: Vec<ReturnTypeMismatch>,
}

/// An attempt to mutate another object's field (`other.field := value`,
/// rather than `self.field := value`) — objects cannot mutate another
/// object's state (Value types are immutable; actors only mutate their own
/// state via `self.x :=`). Detected in `inference/assignment.rs`, rendered
/// by `validation.rs::emit_cross_object_field_mutation`.
pub(in crate::semantic_analysis::type_checker) struct CrossObjectFieldMutation {
    pub(in crate::semantic_analysis::type_checker) receiver_name: EcoString,
    pub(in crate::semantic_analysis::type_checker) field_name: EcoString,
}

/// An expression inside a `typed` class's method body inferred as
/// `Dynamic` for a root-cause reason (not a propagated `DynamicReceiver`, an
/// inert `Unknown`, or an author-acknowledged `ExplicitDynamic`). Detected
/// in `inference/mod.rs`, rendered by
/// `validation.rs::emit_dynamic_in_typed_class`.
pub(in crate::semantic_analysis::type_checker) struct DynamicInTypedClass {
    pub(in crate::semantic_analysis::type_checker) class_name: EcoString,
    pub(in crate::semantic_analysis::type_checker) description: &'static str,
    /// For [`DynamicReason::UnresolvedSelector`], the unresolved selector, so
    /// the diagnostic names the real problem rather than suggesting a type
    /// annotation (BT-4526). `None` for every other reason.
    pub(in crate::semantic_analysis::type_checker) unresolved_selector: Option<EcoString>,
}

#[cfg(test)]
mod display_tests {
    //! `display_name` uses the canonical `UndefinedObject`
    //! class-hierarchy spelling; `display_for_diagnostic` substitutes the
    //! source-sympathetic `Nil` spelling for user-facing messages.

    use super::*;

    #[test]
    fn display_name_keeps_canonical_undefined_object() {
        let ty = InferredType::known("UndefinedObject");
        assert_eq!(ty.display_name(), "UndefinedObject");
    }

    #[test]
    fn display_for_diagnostic_renders_undefined_object_as_nil() {
        let ty = InferredType::known("UndefinedObject");
        assert_eq!(ty.display_for_diagnostic().unwrap(), "Nil");
    }

    #[test]
    fn display_for_diagnostic_renders_union_members_as_nil() {
        let ty = InferredType::simple_union(&["Integer", "UndefinedObject"]);
        let rendered = ty.display_for_diagnostic().unwrap();
        assert!(
            rendered.contains("Nil"),
            "union should render `Nil` not `UndefinedObject`, got: {rendered}"
        );
        assert!(
            !rendered.contains("UndefinedObject"),
            "union must not leak canonical name, got: {rendered}"
        );
    }

    #[test]
    fn display_name_union_keeps_canonical_undefined_object() {
        let ty = InferredType::simple_union(&["Integer", "UndefinedObject"]);
        let rendered = ty.display_name();
        assert!(
            rendered.contains("UndefinedObject"),
            "display_name must keep canonical spelling for internal use, got: {rendered}"
        );
    }

    #[test]
    fn display_for_diagnostic_rewrites_nested_generic_nil() {
        // `Result(Integer, UndefinedObject)` should display as
        // `Result(Integer, Nil)` in user-facing messages.
        let ty = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("UndefinedObject"),
            ],
            provenance: TypeProvenance::Inferred(Span::default()),
        };
        let rendered = ty.display_for_diagnostic().unwrap();
        assert_eq!(rendered, "Result(Integer, Nil)");
    }

    #[test]
    fn display_for_diagnostic_leaves_non_nil_names_untouched() {
        let ty = InferredType::known("Integer");
        assert_eq!(ty.display_for_diagnostic().unwrap(), "Integer");

        let ty2 = InferredType::known("MyCustomClass");
        assert_eq!(ty2.display_for_diagnostic().unwrap(), "MyCustomClass");
    }

    #[test]
    fn class_name_for_diagnostic_maps_undefined_object_and_nil_alias() {
        assert_eq!(
            InferredType::class_name_for_diagnostic("UndefinedObject"),
            "Nil"
        );
        // The legacy `Nil` spelling is already source-sympathetic; round-trip.
        assert_eq!(InferredType::class_name_for_diagnostic("Nil"), "Nil");
        // Non-nil names pass through.
        assert_eq!(
            InferredType::class_name_for_diagnostic("Integer"),
            "Integer"
        );
        assert_eq!(
            InferredType::class_name_for_diagnostic("MyCustomClass"),
            "MyCustomClass"
        );
    }

    #[test]
    fn class_name_for_diagnostic_scrubs_nested_undefined_object() {
        // Nested generic — annotation strings like `Array(UndefinedObject)`
        // must also get scrubbed even though they aren't bare class names.
        assert_eq!(
            InferredType::class_name_for_diagnostic("Array(UndefinedObject)"),
            "Array(Nil)"
        );
        // Union spelling.
        assert_eq!(
            InferredType::class_name_for_diagnostic("Foo | UndefinedObject"),
            "Foo | Nil"
        );
        // Deeply nested.
        assert_eq!(
            InferredType::class_name_for_diagnostic("Result(Integer, UndefinedObject)"),
            "Result(Integer, Nil)"
        );
    }

    // ── ADR 0108: alias display-name provenance ───────────────────────────────

    #[test]
    fn tag_alias_expansion_prefixes_display_for_diagnostic() {
        let expansion = InferredType::simple_union(&["#temporary", "#transient", "#permanent"]);
        let tagged = expansion.tag_alias_expansion("RestartStrategy".into(), Span::new(0, 1));
        assert_eq!(
            tagged.display_for_diagnostic().unwrap(),
            "RestartStrategy (#temporary | #transient | #permanent)"
        );
    }

    #[test]
    fn tag_alias_expansion_is_provenance_only_equality_unaffected() {
        // Structural `PartialEq` ignores provenance (module doc) — tagging
        // an alias name must not change what a type is equal to.
        let expansion = InferredType::known("Integer");
        let tagged = expansion
            .clone()
            .tag_alias_expansion("Port".into(), Span::new(0, 1));
        assert_eq!(expansion, tagged);
    }

    #[test]
    fn tag_alias_expansion_is_noop_for_dynamic_and_never() {
        let dynamic = InferredType::Dynamic(DynamicReason::Unknown);
        assert_eq!(
            dynamic
                .clone()
                .tag_alias_expansion("X".into(), Span::new(0, 1)),
            dynamic
        );
        assert_eq!(
            InferredType::Never.tag_alias_expansion("X".into(), Span::new(0, 1)),
            InferredType::Never
        );
    }

    #[test]
    fn tag_alias_expansion_prefixes_meta_negation_and_intersection() {
        // The hover/diagnostic tests only
        // exercised `Union`/`Known` end-to-end. This pins the other three
        // provenance-bearing variants `tag_alias_expansion` also supports —
        // an alias RHS can legally be any `TypeAnnotation` (ADR 0108
        // Semantics), including ones that resolve to `Meta`, `Negation`
        // (`Symbol \ #foo`), or `Intersection` (`P1 & P2`).
        let meta =
            InferredType::meta("List").tag_alias_expansion("ListClass".into(), Span::new(0, 1));
        assert_eq!(
            meta.display_for_diagnostic().unwrap(),
            "ListClass (List class)"
        );

        let negation = InferredType::Negation {
            base: Box::new(InferredType::known("Symbol")),
            excluded: Box::new(InferredType::known("#internal")),
            provenance: TypeProvenance::Inferred(Span::default()),
        }
        .tag_alias_expansion("PublicTag".into(), Span::new(0, 1));
        assert_eq!(
            negation.display_for_diagnostic().unwrap(),
            "PublicTag (Symbol \\ #internal)"
        );

        let intersection = InferredType::Intersection {
            members: vec![
                InferredType::known("Printable"),
                InferredType::known("Comparable"),
            ],
            provenance: TypeProvenance::Inferred(Span::default()),
        }
        .tag_alias_expansion("Sortable".into(), Span::new(0, 1));
        assert_eq!(
            intersection.display_for_diagnostic().unwrap(),
            "Sortable (Printable & Comparable)"
        );
    }

    #[test]
    fn alias_display_name_absent_without_tagging() {
        let ty = InferredType::simple_union(&["temporary", "transient"]);
        assert_eq!(ty.alias_display_name(), None);
        assert_eq!(
            ty.display_for_diagnostic().unwrap(),
            "temporary | transient"
        );
    }

    #[test]
    fn union_of_never_inherits_a_members_alias_tag() {
        // `A | Integer` (a *fresh* union built from an alias-tagged
        // member `A` and an unrelated `Integer`) must render structurally —
        // it is not the same type as `A`'s own expansion, so it must never
        // silently claim `A`'s alias identity as its own display name (see
        // `union_of`'s `provenance_wins_union_default` doc).
        let aliased_member =
            InferredType::known("String").tag_alias_expansion("MyAlias".into(), Span::new(0, 1));
        let built = InferredType::union_of(&[aliased_member, InferredType::known("Integer")]);
        assert_eq!(built.alias_display_name(), None);
        let rendered = built.display_for_diagnostic().unwrap();
        assert!(
            !rendered.contains("MyAlias"),
            "freshly-built union must not inherit a member's alias name, got: {rendered}"
        );
        assert_eq!(rendered, "String | Integer");
    }

    #[test]
    fn difference_strips_alias_tag_from_the_residual() {
        // Narrowing residual must render structurally, never the alias name
        // (ADR 0108 Decision — "Display through normalisation, scoped
        // honestly"). Mirrors the hover-level pin in
        // `queries::hover_provider`'s test module.
        let aliased = InferredType::simple_union(&["#temporary", "#transient", "#permanent"])
            .tag_alias_expansion("RestartStrategy".into(), Span::new(0, 1));
        let residual = InferredType::difference(
            &aliased,
            &InferredType::known("#temporary"),
            TypeProvenance::Inferred(Span::default()),
            None,
        );
        assert_eq!(residual.alias_display_name(), None);
        let rendered = residual.display_for_diagnostic().unwrap();
        assert!(
            !rendered.contains("RestartStrategy"),
            "difference residual must not carry the alias name, got: {rendered}"
        );
        assert_eq!(rendered, "#transient | #permanent");
    }

    #[test]
    fn difference_strips_alias_tag_from_the_residual_negation_arm() {
        // Same as `difference_strips_alias_tag_from_the_residual`, but for
        // the same-base-flattening Negation arm rather than the Union arm —
        // regression test: `type PublicTag =
        // Symbol \ #internal`, then narrowing further (`Symbol \ #internal \
        // #other`) produced a residual that still displayed as `"PublicTag
        // (...)"` even though it's structurally different from the alias's
        // own definition.
        let base_negation = InferredType::difference(
            &InferredType::known("Symbol"),
            &InferredType::known("#internal"),
            TypeProvenance::Inferred(Span::default()),
            None,
        );
        let aliased = base_negation.tag_alias_expansion("PublicTag".into(), Span::new(0, 1));
        let residual = InferredType::difference(
            &aliased,
            &InferredType::known("#other"),
            TypeProvenance::Inferred(Span::default()),
            None,
        );
        assert_eq!(residual.alias_display_name(), None);
        let rendered = residual.display_for_diagnostic().unwrap();
        assert!(
            !rendered.contains("PublicTag"),
            "negation-arm difference residual must not carry the alias name, got: {rendered}"
        );
        assert_eq!(rendered, "Symbol \\ (#internal | #other)");
    }

    #[test]
    fn class_name_for_diagnostic_preserves_substring_matches() {
        // `UndefinedObject` as part of a longer identifier (user-defined
        // wrapper class) must NOT be scrubbed — whole-identifier match only.
        assert_eq!(
            InferredType::class_name_for_diagnostic("MyUndefinedObjectWrapper"),
            "MyUndefinedObjectWrapper"
        );
        assert_eq!(
            InferredType::class_name_for_diagnostic("UndefinedObjectFactory"),
            "UndefinedObjectFactory"
        );
        // Adjacent repeats — both sides are identifier characters, neither
        // occurrence forms a whole identifier, so both must be preserved.
        assert_eq!(
            InferredType::class_name_for_diagnostic("UndefinedObjectUndefinedObject"),
            "UndefinedObjectUndefinedObject"
        );
    }

    #[test]
    fn debug_still_shows_canonical_undefined_object() {
        // `Debug` derive uses the raw class_name — must stay canonical for
        // unambiguous test-failure / log output.
        let ty = InferredType::known("UndefinedObject");
        let debug_out = format!("{ty:?}");
        assert!(
            debug_out.contains("UndefinedObject"),
            "Debug output must keep canonical name, got: {debug_out}"
        );
    }

    // ── ADR 0147: `InferredType::Shape` ────────────────────────────────────

    fn field(key: &str, value: InferredType, optional: bool) -> ShapeFieldType {
        ShapeFieldType {
            key: key.into(),
            value,
            optional,
        }
    }

    fn int() -> InferredType {
        InferredType::known("Integer")
    }

    fn string() -> InferredType {
        InferredType::known("String")
    }

    #[test]
    fn shape_equality_ignores_field_order_and_provenance() {
        let ab = InferredType::shape(
            vec![field("a", int(), false), field("b", string(), true)],
            false,
        );
        let ba = InferredType::Shape {
            fields: vec![field("b", string(), true), field("a", int(), false)],
            open: false,
            provenance: TypeProvenance::Declared(Span::new(3, 9)),
        };
        assert_eq!(ab, ba);
    }

    #[test]
    fn shape_equality_distinguishes_optionality_openness_and_types() {
        let base = InferredType::shape(vec![field("a", int(), false)], false);
        assert_ne!(
            base,
            InferredType::shape(vec![field("a", int(), true)], false)
        );
        assert_ne!(
            base,
            InferredType::shape(vec![field("a", int(), false)], true)
        );
        assert_ne!(
            base,
            InferredType::shape(vec![field("a", string(), false)], false)
        );
        assert_ne!(
            base,
            InferredType::shape(vec![field("b", int(), false)], false)
        );
        assert_ne!(
            base,
            InferredType::shape(
                vec![field("a", int(), false), field("b", int(), false)],
                false
            )
        );
        assert_ne!(base, InferredType::known("Dictionary"));
    }

    #[test]
    fn shape_displays_in_source_syntax() {
        let ty = InferredType::shape(
            vec![
                field("name", string(), false),
                field("age", int(), true),
                field("first name", InferredType::known("UndefinedObject"), false),
            ],
            true,
        );
        assert_eq!(
            ty.display_name(),
            "#{#name => String, ?#age => Integer, #'first name' => UndefinedObject, ...}"
        );
        // Diagnostics use the source spelling `Nil`.
        assert_eq!(
            ty.display_for_diagnostic().unwrap(),
            "#{#name => String, ?#age => Integer, #'first name' => Nil, ...}"
        );
        assert_eq!(InferredType::shape(vec![], false).display_name(), "#{}");
        assert_eq!(InferredType::shape(vec![], true).display_name(), "#{...}");
    }

    #[test]
    fn shape_display_nests() {
        let inner = InferredType::shape(vec![field("x", int(), false)], false);
        let outer = InferredType::shape(
            vec![
                field("p", inner.clone(), false),
                field(
                    "xs",
                    InferredType::known_with_args("List", vec![inner]),
                    true,
                ),
            ],
            false,
        );
        assert_eq!(
            outer.display_name(),
            "#{#p => #{#x => Integer}, ?#xs => List(#{#x => Integer})}"
        );
    }

    #[test]
    fn long_anonymous_shape_is_truncated_for_diagnostics_only() {
        let fields: Vec<ShapeFieldType> = (0..7)
            .map(|i| field(&format!("k{i}"), int(), false))
            .collect();
        let ty = InferredType::shape(fields, true);
        assert_eq!(
            ty.display_for_diagnostic().unwrap(),
            "#{#k0 => Integer, #k1 => Integer, #k2 => Integer, #k3 => Integer, … +3 more, ...}"
        );
        // The canonical and annotation forms stay complete (they feed
        // assignability lookups and paste-back).
        assert!(ty.display_name().contains("#k6 => Integer"));
        assert!(ty.display_annotation().contains("#k6 => Integer"));
        assert!(!ty.display_annotation().contains('…'));
    }

    #[test]
    fn aliased_shape_displays_as_the_alias_name() {
        let ty = InferredType::shape(
            vec![field("a", int(), false), field("b", int(), false)],
            false,
        )
        .tag_alias_expansion("Point".into(), Span::default());
        assert_eq!(
            ty.alias_display_name().map(EcoString::as_str),
            Some("Point")
        );
        assert_eq!(ty.display_for_diagnostic().unwrap(), "Point");
        // The structural forms are unchanged by the tag.
        assert_eq!(ty.display_name(), "#{#a => Integer, #b => Integer}");
    }

    #[test]
    fn shape_has_no_single_class_name() {
        let ty = InferredType::shape(vec![field("a", int(), false)], false);
        assert_eq!(ty.as_known(), None);
        assert_eq!(ty.as_meta(), None);
        assert!(ty.provenance().is_some());
    }

    #[test]
    fn dictionary_view_joins_key_and_value_types() {
        let ty = InferredType::shape(
            vec![field("a", int(), false), field("b", string(), true)],
            false,
        );
        assert_eq!(
            ty.dictionary_view().unwrap(),
            InferredType::known_with_args(
                "Dictionary",
                vec![
                    InferredType::union_of(&[InferredType::known("#a"), InferredType::known("#b")]),
                    InferredType::union_of(&[int(), string()]),
                ],
            )
        );
    }

    #[test]
    fn dictionary_view_of_a_single_field_shape_is_not_a_union() {
        let ty = InferredType::shape(vec![field("a", int(), false)], false);
        assert_eq!(
            ty.dictionary_view().unwrap(),
            InferredType::known_with_args("Dictionary", vec![InferredType::known("#a"), int()])
        );
    }

    #[test]
    fn dictionary_view_of_an_open_shape_is_object_object() {
        let ty = InferredType::shape(vec![field("a", int(), false)], true);
        let object = InferredType::known("Object");
        assert_eq!(
            ty.dictionary_view().unwrap(),
            InferredType::known_with_args("Dictionary", vec![object.clone(), object])
        );
        let open_empty = InferredType::shape(vec![], true);
        assert_eq!(
            open_empty.dictionary_view().unwrap().display_name(),
            "Dictionary(Object, Object)"
        );
    }

    #[test]
    fn dictionary_view_of_the_empty_closed_shape_is_never_never() {
        assert_eq!(
            InferredType::shape(vec![], false)
                .dictionary_view()
                .unwrap()
                .display_name(),
            "Dictionary(Never, Never)"
        );
    }

    #[test]
    fn dictionary_view_is_none_for_every_other_variant() {
        assert!(int().dictionary_view().is_none());
        assert!(InferredType::Never.dictionary_view().is_none());
        assert!(InferredType::meta("Foo").dictionary_view().is_none());
    }

    #[test]
    fn union_keeps_shape_members_separate_and_dedups_by_equality() {
        let a = InferredType::shape(vec![field("a", int(), false)], false);
        let b = InferredType::shape(vec![field("b", int(), false)], false);
        let union = InferredType::union_of(&[a.clone(), b.clone()]);
        let InferredType::Union { members, .. } = &union else {
            panic!("expected a union, got {union:?}");
        };
        assert_eq!(members, &vec![a.clone(), b]);

        // A reordered duplicate collapses.
        let a_again = InferredType::shape(vec![field("a", int(), false)], false);
        assert_eq!(InferredType::union_of(&[a.clone(), a_again]), a);

        // `S | Nil` keeps the shape intact.
        let with_nil = InferredType::union_of(&[a.clone(), InferredType::known("UndefinedObject")]);
        assert_eq!(
            with_nil.display_name(),
            "#{#a => Integer} | UndefinedObject"
        );
    }
}

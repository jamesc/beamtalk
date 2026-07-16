// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type definitions for the type checker — `TypeProvenance`, `InferredType`, and helpers.
//!
//! **DDD Context:** Semantic Analysis

use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
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
    /// The type annotation literally is the `Dynamic` keyword (BT-2865) —
    /// e.g. `Result(Dynamic, Error)`'s first type-arg, or a field/param
    /// explicitly declared `:: Dynamic`.
    ///
    /// Distinguished from every other reason (all of which mean "we don't
    /// have enough information to give a concrete type") because this one
    /// means the *opposite*: the author explicitly promised the value can be
    /// anything. `merge_method_local_binding` treats this as authoritative —
    /// it must survive being unified with any other binding, including a
    /// concrete one from a sibling argument position — where every other
    /// `DynamicReason` instead loses to a concrete binding observed
    /// elsewhere (BT-2039).
    ExplicitDynamic,
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
            Self::Unknown => None,
        }
    }
}

/// Tracks where a type came from — enables precise error messages
/// and determines how far inference should propagate.
///
/// **References:** ADR 0068 Challenge 3
///
/// Not `Copy` (BT-2897 added an `EcoString` payload to [`Aliased`](Self::Aliased))
/// — call sites that previously relied on an implicit copy now need an
/// explicit `.clone()`. Cloning is cheap (`EcoString` is reference-counted).
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
    /// The type is the eager expansion of a named alias reference (ADR 0108,
    /// BT-2897) — attached *only* at the exact point of expansion in
    /// [`resolve_type_annotation`](super::resolve_type_annotation), so the
    /// tagged value is by construction structurally identical to alias
    /// `name`'s expansion (the ADR's "display through normalisation, scoped
    /// honestly" rule).
    ///
    /// `span` is the annotation *reference* site (where `name` was written),
    /// not the alias's own `type Name = ...` declaration.
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

/// Controls how [`InferredType`] renders class names when converted to a
/// display string.
///
/// See [`InferredType::display_name`] and
/// [`InferredType::display_for_diagnostic`].
#[derive(Debug, Clone, Copy)]
struct DisplayOptions {
    /// When `true`, `Known("UndefinedObject")` renders as `"Nil"`. When
    /// `false`, the canonical `UndefinedObject` name is used.
    nil_as_source_name: bool,
}

impl DisplayOptions {
    const CANONICAL: Self = Self {
        nil_as_source_name: false,
    };
    const SOURCE_FRIENDLY: Self = Self {
        nil_as_source_name: true,
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
    /// - **Nominal-excluded** (ADR 0102 §5, BT-2744) — e.g. `Object \ Number`;
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
            (Self::Meta { class_name: a, .. }, Self::Meta { class_name: b, .. }) => a == b,
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
            | Self::Dynamic(_)
            | Self::Union { .. }
            | Self::Never
            // A negation (`Symbol \ #foo`) is not a single known class — callers
            // must handle it structurally, not as a bare class name.
            | Self::Negation { .. }
            // An intersection (`A & B`) is not a single known class either —
            // callers must handle it structurally (ADR 0102 §1).
            | Self::Intersection { .. } => None,
        }
    }

    /// Returns the [`TypeProvenance`] attached to this type, or `None` for the
    /// provenance-free variants ([`Dynamic`](Self::Dynamic) and
    /// [`Never`](Self::Never), which carry no source location).
    // Consumed by the narrowing rules landing later in ADR 0102's epic (BT-2738).
    #[allow(dead_code)]
    #[must_use]
    pub(crate) fn provenance(&self) -> Option<TypeProvenance> {
        match self {
            Self::Known { provenance, .. }
            | Self::Union { provenance, .. }
            | Self::Meta { provenance, .. }
            | Self::Negation { provenance, .. }
            | Self::Intersection { provenance, .. } => Some(provenance.clone()),
            Self::Dynamic(_) | Self::Never => None,
        }
    }

    /// Returns the alias display name if this type's top-level provenance is
    /// [`TypeProvenance::Aliased`] (ADR 0108, BT-2897) — i.e. this exact value
    /// is the eager expansion of a named alias reference, structurally
    /// identical to that alias's declared expansion by construction (see
    /// `Aliased`'s doc).
    #[must_use]
    pub(crate) fn alias_display_name(&self) -> Option<&EcoString> {
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
                | Self::Negation {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                }
                | Self::Intersection {
                    provenance: TypeProvenance::Aliased { name, .. },
                    ..
                } => Some(name),
                _ => None,
            },
            _ => None,
        }
    }

    /// Rewrites this type's top-level provenance to
    /// [`TypeProvenance::Aliased`] (ADR 0108, BT-2897) — the single
    /// construction path for that provenance variant, called exactly once,
    /// at the point [`resolve_type_annotation`](super::resolve_type_annotation)
    /// finishes eagerly expanding a `Simple` annotation that names a
    /// registered alias. A no-op for [`Dynamic`](Self::Dynamic) and
    /// [`Never`](Self::Never), which carry no provenance slot.
    #[must_use]
    pub(crate) fn tag_alias_expansion(self, name: EcoString, span: Span) -> Self {
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
            Self::Negation { base, excluded, .. } => Self::Negation {
                base,
                excluded,
                provenance: aliased,
            },
            Self::Intersection { members, .. } => Self::Intersection {
                members,
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
    /// **References:** BT-2066
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
    /// anywhere else, so diagnostics echoing `UndefinedObject` back at them
    /// were jarring and triggered BT-2066.
    ///
    /// Use this for any user-facing string: diagnostic messages, hover
    /// contents, signature help labels, and code-action inserts. Keep
    /// [`display_name`](Self::display_name) for internal bookkeeping where the
    /// canonical name is required (e.g., `is_assignable_to` lookups).
    ///
    /// **BT-2897 / ADR 0108:** when this type's top-level provenance is
    /// [`TypeProvenance::Aliased`], the rendered string is prefixed with the
    /// alias name — `RestartStrategy (#temporary | #transient | #permanent)`
    /// rather than just the bare expansion — because this value *is* (by
    /// construction, see `Aliased`'s doc) structurally identical to that
    /// alias's declared expansion. Narrowing residuals and freshly-built
    /// unions never carry this tag (see `Aliased` and `union_of`'s docs), so
    /// they render the plain structural form with no alias breadcrumb, which
    /// is the ADR's intentional v1 scoping, not a gap.
    ///
    /// **References:** BT-2066, ADR 0108 (BT-2897)
    #[must_use]
    pub fn display_for_diagnostic(&self) -> Option<EcoString> {
        let structural = self.display_with_options(DisplayOptions::SOURCE_FRIENDLY);
        if let Some(alias_name) = self.alias_display_name() {
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
            Self::Dynamic(reason) => {
                if let Some(desc) = reason.description() {
                    EcoString::from(format!("Dynamic ({desc})"))
                } else {
                    EcoString::from("Dynamic")
                }
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
    /// **BT-2897:** a member's [`TypeProvenance::Aliased`] tag never wins this
    /// selection (see [`provenance_wins_union_default`](Self::provenance_wins_union_default)) —
    /// a freshly-built union is a *different* type from any one member's
    /// alias expansion, so it must never silently claim that member's alias
    /// identity as its own display name. `best_provenance` itself is
    /// therefore never `Aliased`, which is also why the single-member
    /// collapse case below (which checks the *member's own* provenance, not
    /// `best_provenance`) needs no corresponding change: `Aliased` already
    /// fails its `matches!(.., Inferred(_))` guard today, so an aliased
    /// singleton member is already never overwritten.
    pub(crate) fn union_of(members: &[Self]) -> Self {
        let mut flat: Vec<InferredType> = Vec::new();
        let mut best_provenance = TypeProvenance::Inferred(Span::default());
        for m in members {
            match m {
                Self::Known { provenance, .. }
                | Self::Meta { provenance, .. }
                | Self::Negation { provenance, .. }
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
        // per set so `intersect` stays commutative; BT-2741).
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
    /// (BT-2897, ADR 0108) never wins either, even though it's informative —
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
        name.starts_with('#')
    }

    /// Returns `true` if `ty` is the base of a well-formed **singleton-excluded**
    /// [`Negation`] — i.e. `Known("Symbol")`. `Symbol` is the sole supertype of
    /// singletons the type checker models, so it is the only base for which
    /// singleton subtraction and absorption are sound.
    ///
    /// Distinct from the **nominal-excluded** `Negation` flavour (ADR 0102 §5,
    /// BT-2744, e.g. `Object \ Number`), whose base is any nominal class — see
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
    /// nominal *supertype* of `Symbol` such as `ProtoObject` (BT-2764).
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
    /// is unrelated to the *nominal-excluded* flavour (ADR 0102 §5, BT-2744),
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
    ///   `intersect` commutativity (BT-2741 Windows CI counterexample:
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
                    neg_provenance.get_or_insert(provenance);
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
    /// (ADR 0102 §1/§3, BT-2743): supplying it lets `intersect` recognise a
    /// protocol name and route class ∩ protocol / protocol ∩ protocol to the
    /// stored [`Intersection`](Self::Intersection) instead of falling through
    /// to the disjoint default. `None` preserves the pre-BT-2743 structural
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
            // default (ADR 0102 §1/§3, BT-2743). Needed so a 3+-way `&`
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
            // Symbol-singleton membership: `#foo <: Symbol`, so the narrower
            // singleton is kept in both orders. Checked before the general
            // nominal case because singletons are not entries in the hierarchy
            // — the nominal arm's `is_nominal_subtype(h, "#foo", …)` would
            // always answer `false`. With a hierarchy, the same rule extends
            // to every nominal *supertype* of `Symbol` (e.g. `ProtoObject`,
            // BT-2764): `ProtoObject ∩ #foo = #foo`, matching the
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
            // Class ∩ protocol / protocol ∩ protocol (ADR 0102 §1/§3, BT-2743):
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
    /// (its match arm guards on differing names) but kept so the predicate is
    /// correct standalone for future callers.
    fn is_nominal_subtype(hierarchy: &ClassHierarchy, sub: &str, sup: &str) -> bool {
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
    /// - **Nominal-class base case** (ADR 0102 §5, BT-2744, `hierarchy = Some`):
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
                    neg_prov.clone(),
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
            // Nominal-class base case (ADR 0102 §5, BT-2744): with a hierarchy,
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
/// helper identifies such types so diagnostics can provide better context
/// (BT-1588).
pub(in crate::semantic_analysis) fn is_generic_type_param(name: &str) -> bool {
    let bytes = name.as_bytes();
    bytes.len() == 1 && bytes[0].is_ascii_uppercase()
}

#[cfg(test)]
mod display_tests {
    //! BT-2066: `display_name` uses the canonical `UndefinedObject`
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

    // ── BT-2897 / ADR 0108: alias display-name provenance ──────────────────

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
        // BT-2897: `A | Integer` (a *fresh* union built from an alias-tagged
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
}

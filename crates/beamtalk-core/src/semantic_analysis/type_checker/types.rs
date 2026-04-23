// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type definitions for the type checker — `TypeProvenance`, `InferredType`, and helpers.
//!
//! **DDD Context:** Semantic Analysis

use crate::source_analysis::Span;
use ecow::EcoString;

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
            Self::Unknown => None,
        }
    }
}

/// Tracks where a type came from — enables precise error messages
/// and determines how far inference should propagate.
///
/// **References:** ADR 0068 Challenge 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            Self::Dynamic(_) | Self::Union { .. } | Self::Never => None,
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
    pub fn display_name(&self) -> Option<EcoString> {
        Some(self.display_with_options(DisplayOptions::CANONICAL))
    }

    /// Maps a raw class-name string to its user-facing diagnostic spelling.
    ///
    /// Today this rewrites `"UndefinedObject"` → `"Nil"` and leaves all other
    /// names untouched. Use this when you have a bare `EcoString`/`&str`
    /// class name (e.g., a union-member list passed to a diagnostic) rather
    /// than a full `InferredType`.
    ///
    /// **References:** BT-2066
    #[must_use]
    pub fn class_name_for_diagnostic(name: &str) -> EcoString {
        if WellKnownClass::from_str(name).is_some_and(WellKnownClass::is_nil_class) {
            EcoString::from("Nil")
        } else {
            EcoString::from(name)
        }
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
    /// **References:** BT-2066
    #[must_use]
    pub fn display_for_diagnostic(&self) -> Option<EcoString> {
        Some(self.display_with_options(DisplayOptions::SOURCE_FRIENDLY))
    }

    fn display_with_options(&self, opts: DisplayOptions) -> EcoString {
        match self {
            Self::Known {
                class_name,
                type_args,
                ..
            } => {
                let rendered_name: EcoString = if opts.nil_as_source_name
                    && WellKnownClass::from_str(class_name.as_str())
                        .is_some_and(WellKnownClass::is_nil_class)
                {
                    EcoString::from("Nil")
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
            Self::Dynamic(reason) => {
                if let Some(desc) = reason.description() {
                    EcoString::from(format!("Dynamic ({desc})"))
                } else {
                    EcoString::from("Dynamic")
                }
            }
            Self::Never => EcoString::from("Never"),
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
    pub(crate) fn union_of(members: &[Self]) -> Self {
        let mut flat: Vec<InferredType> = Vec::new();
        let mut best_provenance = TypeProvenance::Inferred(Span::default());
        for m in members {
            match m {
                Self::Known { provenance, .. } => {
                    if matches!(best_provenance, TypeProvenance::Inferred(_))
                        && !matches!(provenance, TypeProvenance::Inferred(_))
                    {
                        best_provenance = *provenance;
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
                        && !matches!(provenance, TypeProvenance::Inferred(_))
                    {
                        best_provenance = *provenance;
                    }
                    for inner_m in inner {
                        if let Self::Known { provenance: p, .. } = inner_m {
                            if matches!(best_provenance, TypeProvenance::Inferred(_))
                                && !matches!(p, TypeProvenance::Inferred(_))
                            {
                                best_provenance = *p;
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
        assert_eq!(ty.display_name().unwrap(), "UndefinedObject");
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
        let rendered = ty.display_name().unwrap();
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

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! A span-free, structured representation of a declared type (BT-3076).
//!
//! **DDD Context:** Semantic Analysis — Value Object
//!
//! [`MethodInfo::return_type`](super::MethodInfo::return_type) and
//! `param_types` now store this type directly (BT-3076 stage 3), rather than
//! the flattened `EcoString` (see [`crate::ast::TypeAnnotation::type_name`])
//! they stored before. Before this module existed, every consumer that
//! needed structure back out of one of those strings had hand-rolled its own
//! parser — `TypeChecker::resolve_type_string` (BT-3075) was the canonical,
//! merged one, since deleted (BT-3076 stage 3c / BT-3080) now that every
//! call site resolves a `DeclaredType` directly via
//! [`resolve_declared_type`](crate::semantic_analysis::type_checker::type_resolver::resolve_declared_type).
//! `DeclaredType` gives that structure a proper value type: a span-free
//! mirror of [`TypeAnnotation`] that can be built directly from an AST
//! annotation ([`From<&TypeAnnotation>`](#impl-From<%26TypeAnnotation>-for-DeclaredType)),
//! parsed back out of a stored string ([`DeclaredType::parse`] — still used
//! for legacy artifacts and a handful of string-keyed helpers not worth
//! restructuring), or partially recovered from an already-resolved
//! [`InferredType`] ([`DeclaredType::from_inferred`]).
//!
//! Lives in the `class_hierarchy` layer (not `type_checker`) because
//! `MethodInfo` — the field this type is stored in — lives here, and
//! `type_checker` already depends downward on `class_hierarchy`, never the
//! reverse.

use std::fmt;

use ecow::EcoString;

use crate::ast::TypeAnnotation;
use crate::semantic_analysis::string_utils::{split_generic_base, split_top_level};
use crate::semantic_analysis::type_checker::InferredType;

/// A span-free, structured type signature — the value-object counterpart to
/// [`TypeAnnotation`], with every [`crate::source_analysis::Span`] and
/// [`crate::ast::Identifier`] stripped down to a bare [`EcoString`].
///
/// Mirrors every `TypeAnnotation` variant. See the module docs for how a
/// value is produced (`From<&TypeAnnotation>`, [`DeclaredType::parse`],
/// [`DeclaredType::from_inferred`]) and displayed ([`Display`](fmt::Display),
/// byte-identical to [`TypeAnnotation::type_name`]).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum DeclaredType {
    /// A simple named type (e.g., `Integer`, `String`, `Counter`). Also the
    /// degrade-to fallback for any string [`DeclaredType::parse`] cannot
    /// otherwise make sense of — see that method's doc.
    Simple(EcoString),
    /// A union type (e.g., `Integer | String`).
    Union(Vec<DeclaredType>),
    /// A singleton/literal type (e.g., `#north`, stored without the `#`).
    Singleton(EcoString),
    /// A generic type (e.g., `Collection(Integer)`, `Result(T, E)`).
    Generic {
        /// The base type name.
        base: EcoString,
        /// Type parameters.
        parameters: Vec<DeclaredType>,
    },
    /// A false-or type (Option/Maybe pattern), e.g. `Integer | False`.
    FalseOr(Box<DeclaredType>),
    /// A difference/negation type (e.g., `Symbol \ #foo`) — ADR 0102 §1.
    Difference {
        /// The base type being narrowed.
        base: Box<DeclaredType>,
        /// The type removed from `base`.
        excluded: Box<DeclaredType>,
    },
    /// An intersection type (e.g., `Collection(Object) & Comparable`) — ADR
    /// 0068 §Protocol Composition, ADR 0102 §1/§3.
    Intersection {
        /// The left-hand operand.
        left: Box<DeclaredType>,
        /// The right-hand operand.
        right: Box<DeclaredType>,
    },
    /// The `Self` return type.
    SelfType,
    /// The `Self class` metatype.
    SelfClass,
    /// The `<ClassName> class` metatype, carrying the named class.
    ClassOf(EcoString),
}

impl DeclaredType {
    /// Parse a legacy flattened type-signature string into a structured
    /// `DeclaredType`.
    ///
    /// [`MethodInfo::return_type`](super::MethodInfo::return_type) /
    /// `param_types` and `ClassHierarchy::state_field_type` are structured
    /// `DeclaredType`s directly since BT-3076 stage 3 — built via
    /// `DeclaredType::from(&TypeAnnotation)` at the AST boundary, never
    /// stringified and reparsed. This parser instead serves the boundaries
    /// that still only have a flat string in hand: a pre-BT-3076 compiled
    /// `.beam` artifact's ETF metadata (`beamtalk-compiler-port`'s
    /// `term_to_declared_type`), and a handful of string-keyed helpers
    /// elsewhere in the type checker not worth restructuring (documented at
    /// their own call sites).
    ///
    /// Grammar mirrors the deleted `TypeChecker::resolve_type_string`'s
    /// parsing (BT-3075, removed by BT-3076 stage 3c): split on top-level
    /// `|` (respecting parenthesis nesting) for unions, then
    /// `Base(Arg1, Arg2)` for generics (args themselves split on top-level
    /// `,`, respecting nesting), with `#name` recognised as a
    /// [`DeclaredType::Singleton`]. This is **parse only** — no keyword
    /// resolution: `Never`, `Dynamic`, `nil`, `true`, `false` all come back
    /// as plain `Simple`/`Singleton` names, exactly as written. That
    /// normalisation is the resolver's job, not the parser's (see
    /// `resolve_declared_type` in `type_resolver`).
    ///
    /// The self-type shapes rendered flat by [`TypeAnnotation::type_name`]
    /// (and by codegen's `MetaTypeRepr::Atom` fallback for method-signature
    /// metadata) are recognised structurally: bare `Self` →
    /// [`DeclaredType::SelfType`], `Self class` → [`DeclaredType::SelfClass`],
    /// and `<Name> class` (identifier followed by the single word `class`) →
    /// [`DeclaredType::ClassOf`]. This keeps a `-> Self` return type that
    /// crosses a string boundary (ETF compiler-port metadata, extension-method
    /// type info) behaving like its AST-built counterpart instead of decaying
    /// to an opaque nominal name.
    ///
    /// Never panics. This grammar has no representation for `\`
    /// (difference) or `&` (intersection) — strings in those shapes (which
    /// *can* occur in a legacy artifact, since a pre-BT-3076
    /// `MethodInfo::return_type` was populated via
    /// [`TypeAnnotation::type_name`], which does render them) degrade to an
    /// opaque `Simple(whole_string)` — an unparsed string becomes a nominal
    /// class name. Malformed/unbalanced input (e.g. `"Array(Integer"`, no
    /// closing paren) degrades the same way.
    #[must_use]
    pub fn parse(s: &str) -> DeclaredType {
        let trimmed = s.trim();

        // Split on top-level `|` first (respecting parens), matching
        // `resolve_type_string`'s union-first ordering.
        let union_parts = split_top_level(trimmed, '|');
        if union_parts.len() > 1 {
            return DeclaredType::Union(union_parts.into_iter().map(DeclaredType::parse).collect());
        }

        // Generic: `Base(Arg1, Arg2)`, nesting-aware.
        let (base_str, args) = split_generic_base(trimmed);
        if let Some(inner) = args {
            let parameters = split_top_level(inner, ',')
                .into_iter()
                .map(DeclaredType::parse)
                .collect();
            return DeclaredType::Generic {
                base: base_str.trim().into(),
                parameters,
            };
        }

        // Singleton: `#name`.
        if let Some(rest) = trimmed.strip_prefix('#') {
            if !rest.is_empty() {
                return DeclaredType::Singleton(rest.into());
            }
        }

        // Self-type shapes rendered flat by `TypeAnnotation::type_name` /
        // codegen's `MetaTypeRepr::Atom` fallback. `Self class` must win
        // over the generic `<Name> class` suffix strip below, or it would
        // come back as `ClassOf("Self")`.
        if trimmed == "Self" {
            return DeclaredType::SelfType;
        }
        if trimmed == "Self class" {
            return DeclaredType::SelfClass;
        }
        if let Some(base) = trimmed.strip_suffix(" class")
            && !base.is_empty()
            && base.chars().all(|c| c.is_alphanumeric() || c == '_')
        {
            return DeclaredType::ClassOf(base.into());
        }

        // Fallback: opaque nominal name, including for constructs this
        // grammar doesn't parse (`\`, `&`) and malformed input.
        DeclaredType::Simple(trimmed.into())
    }

    /// Partially convert an already-resolved [`InferredType`] back into a
    /// `DeclaredType`, for writeback-style call sites that only have an
    /// `InferredType` in hand.
    ///
    /// Matches the current writeback filter in
    /// [`ClassHierarchy::apply_inferred_return_types`](super::ClassHierarchy::apply_inferred_return_types)
    /// (only `Known` and `Never` are ever written back today):
    /// - [`InferredType::Known`] → [`DeclaredType::Simple`] (no type args) or
    ///   [`DeclaredType::Generic`] (with type args, converted via
    ///   [`Self::from_inferred_nested`]; `None` if any argument is
    ///   unconvertible).
    /// - [`InferredType::Never`] → `Simple("Never")`.
    /// - [`InferredType::Union`] → `Union` of converted members (also via
    ///   `from_inferred_nested`), `None` if any member is unconvertible.
    /// - Everything else (`Dynamic`, `Meta`, `Negation`, `Intersection`) →
    ///   `None` — these don't have a single canonical declared-type spelling
    ///   the writeback path should commit to. Note the top-level/nested
    ///   asymmetry for `Dynamic`: a bare `Dynamic` return is *not* written
    ///   back (same as the pre-BT-3076 `Known | Never` filter), but a
    ///   `Dynamic` nested inside a `Known`/`Union` converts to
    ///   `Simple("Dynamic")` so partially-inferred generics like
    ///   `List(Dynamic)` still write back — matching the old
    ///   `display_name()` string path, which rendered exactly
    ///   `"List(Dynamic)"` (BT-3101). Lossy cases that remain: a nested
    ///   `Meta`/`Negation`/`Intersection` still aborts the whole conversion.
    #[must_use]
    pub fn from_inferred(ty: &InferredType) -> Option<DeclaredType> {
        match ty {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } if type_args.is_empty() => Some(DeclaredType::Simple(class_name.clone())),
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                let parameters = type_args
                    .iter()
                    .map(DeclaredType::from_inferred_nested)
                    .collect::<Option<Vec<_>>>()?;
                Some(DeclaredType::Generic {
                    base: class_name.clone(),
                    parameters,
                })
            }
            InferredType::Never => Some(DeclaredType::Simple("Never".into())),
            InferredType::Union { members, .. } => {
                let converted = members
                    .iter()
                    .map(DeclaredType::from_inferred_nested)
                    .collect::<Option<Vec<_>>>()?;
                Some(DeclaredType::Union(converted))
            }
            _ => None,
        }
    }

    /// [`Self::from_inferred`] for *nested* positions (generic type args,
    /// union members), where `Dynamic` additionally converts to
    /// `Simple("Dynamic")` — the resolver normalises that name back to
    /// `Dynamic`, so the round-trip is faithful. Kept out of `from_inferred`
    /// itself so a bare top-level `Dynamic` return type still skips
    /// writeback entirely (see its doc, BT-3101).
    fn from_inferred_nested(ty: &InferredType) -> Option<DeclaredType> {
        match ty {
            InferredType::Dynamic(_) => Some(DeclaredType::Simple("Dynamic".into())),
            other => DeclaredType::from_inferred(other),
        }
    }

    /// Compact constructor for [`DeclaredType::Simple`] — takes anything
    /// convertible to an [`EcoString`] so call sites can write
    /// `DeclaredType::simple("Integer")` instead of
    /// `DeclaredType::Simple("Integer".into())`.
    #[must_use]
    pub fn simple(name: impl Into<EcoString>) -> DeclaredType {
        DeclaredType::Simple(name.into())
    }

    /// Compact constructor for [`DeclaredType::Singleton`] (stored without
    /// the leading `#`, matching the variant's own convention).
    #[must_use]
    pub fn singleton(name: impl Into<EcoString>) -> DeclaredType {
        DeclaredType::Singleton(name.into())
    }

    /// Compact constructor for [`DeclaredType::Generic`].
    #[must_use]
    pub fn generic(base: impl Into<EcoString>, parameters: Vec<DeclaredType>) -> DeclaredType {
        DeclaredType::Generic {
            base: base.into(),
            parameters,
        }
    }

    /// Compact constructor for [`DeclaredType::Union`].
    #[must_use]
    pub fn union(members: Vec<DeclaredType>) -> DeclaredType {
        DeclaredType::Union(members)
    }

    /// `true` when `self`, used as an operand of `\`, must be parenthesised
    /// for the printed form to re-parse to the same value — mirrors
    /// [`TypeAnnotation::needs_parens_in_difference`].
    fn needs_parens_in_difference(&self, is_excluded: bool) -> bool {
        match self {
            Self::Union(_) | Self::FalseOr(_) | Self::Intersection { .. } => true,
            Self::Difference { .. } => is_excluded,
            _ => false,
        }
    }

    /// `true` when `self`, used as an operand of `&`, must be parenthesised
    /// — mirrors [`TypeAnnotation::needs_parens_in_intersection`].
    fn needs_parens_in_intersection(&self, is_right: bool) -> bool {
        match self {
            Self::Union(_) | Self::FalseOr(_) | Self::Difference { .. } => true,
            Self::Intersection { .. } => is_right,
            _ => false,
        }
    }
}

/// Byte-identical to [`TypeAnnotation::type_name`] — see that method for the
/// per-variant rendering rules this mirrors. `DeclaredType` is a separate,
/// span-free type (see the module docs), so this can't literally call
/// `TypeAnnotation::type_name` — keep them in sync by hand, matching what
/// `TypeAnnotation` itself does across `type_name` / `needs_parens_in_*`.
/// This invariant (and parity with the third independent renderer,
/// `unparse::unparse_type_annotation_display`) is enforced by the
/// `assert_display_parity` fixture tests below (BT-3089) — not just this
/// comment, per this repo's "No duplicate implementations" rule.
impl fmt::Display for DeclaredType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Simple(name) => write!(f, "{name}"),
            Self::Singleton(name) => write!(f, "#{name}"),
            Self::Union(types) => {
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{ty}")?;
                }
                Ok(())
            }
            Self::Generic { base, parameters } => {
                write!(f, "{base}(")?;
                for (i, ty) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ")")
            }
            Self::FalseOr(inner) => match inner.as_ref() {
                Self::Union(_) | Self::FalseOr(_) => write!(f, "({inner}) | False"),
                _ => write!(f, "{inner} | False"),
            },
            Self::Difference { base, excluded } => {
                if base.needs_parens_in_difference(false) {
                    write!(f, "({base})")?;
                } else {
                    write!(f, "{base}")?;
                }
                write!(f, " \\ ")?;
                if excluded.needs_parens_in_difference(true) {
                    write!(f, "({excluded})")
                } else {
                    write!(f, "{excluded}")
                }
            }
            Self::Intersection { left, right } => {
                if left.needs_parens_in_intersection(false) {
                    write!(f, "({left})")?;
                } else {
                    write!(f, "{left}")?;
                }
                write!(f, " & ")?;
                if right.needs_parens_in_intersection(true) {
                    write!(f, "({right})")
                } else {
                    write!(f, "{right}")
                }
            }
            Self::SelfType => write!(f, "Self"),
            Self::SelfClass => write!(f, "Self class"),
            Self::ClassOf(name) => write!(f, "{name} class"),
        }
    }
}

impl From<&TypeAnnotation> for DeclaredType {
    /// Span-strip conversion: every `TypeAnnotation` variant maps to its
    /// `DeclaredType` counterpart 1:1, dropping spans and unwrapping
    /// `Identifier`s down to their bare `EcoString` name.
    fn from(ann: &TypeAnnotation) -> Self {
        match ann {
            TypeAnnotation::Simple(id) => DeclaredType::Simple(id.name.clone()),
            TypeAnnotation::Union { types, .. } => {
                DeclaredType::Union(types.iter().map(DeclaredType::from).collect())
            }
            TypeAnnotation::Singleton { name, .. } => DeclaredType::Singleton(name.clone()),
            TypeAnnotation::Generic {
                base, parameters, ..
            } => DeclaredType::Generic {
                base: base.name.clone(),
                parameters: parameters.iter().map(DeclaredType::from).collect(),
            },
            TypeAnnotation::FalseOr { inner, .. } => {
                DeclaredType::FalseOr(Box::new(DeclaredType::from(inner.as_ref())))
            }
            TypeAnnotation::Difference { base, excluded, .. } => DeclaredType::Difference {
                base: Box::new(DeclaredType::from(base.as_ref())),
                excluded: Box::new(DeclaredType::from(excluded.as_ref())),
            },
            TypeAnnotation::Intersection { left, right, .. } => DeclaredType::Intersection {
                left: Box::new(DeclaredType::from(left.as_ref())),
                right: Box::new(DeclaredType::from(right.as_ref())),
            },
            TypeAnnotation::SelfType { .. } => DeclaredType::SelfType,
            TypeAnnotation::SelfClass { .. } => DeclaredType::SelfClass,
            TypeAnnotation::ClassOf { class_name, .. } => {
                DeclaredType::ClassOf(class_name.name.clone())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Identifier;
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

    // ---- parse ∘ to_string round-trips ----

    #[test]
    fn round_trip_simple() {
        assert_round_trip("Integer");
    }

    #[test]
    fn round_trip_generic_two_params() {
        assert_round_trip("Result(T, E)");
    }

    #[test]
    fn round_trip_nested_generic() {
        assert_round_trip("List(Result(Integer, String))");
    }

    #[test]
    fn round_trip_union() {
        assert_round_trip("String | UndefinedObject");
    }

    #[test]
    fn round_trip_union_with_singleton() {
        assert_round_trip("Integer | #infinity");
    }

    #[test]
    fn round_trip_bare_singleton() {
        assert_round_trip("#north");
    }

    #[test]
    fn round_trip_union_nested_in_generic() {
        // `Result(Integer | String, Error)` — union inside a generic
        // argument; the top-level split must not fire on the inner `|`.
        assert_round_trip("Result(Integer | String, Error)");
    }

    #[test]
    fn round_trip_union_of_generics() {
        assert_round_trip("List(Integer) | Dictionary(String, Integer)");
    }

    #[test]
    fn round_trip_false_or_rendering() {
        // `DeclaredType::FalseOr` can't be produced by `parse` (the string
        // grammar doesn't distinguish it from a plain `Union`), but its
        // `Display` form must still match `TypeAnnotation::type_name`'s
        // exactly — pinned directly against a `TypeAnnotation` built via the
        // real parser in `display_parity_false_or` below. Here we only check
        // that re-parsing the *displayed* text is stable (round-trips to the
        // same text), since `Simple`/`Union` are what parsing that text
        // actually yields.
        let false_or = DeclaredType::FalseOr(Box::new(DeclaredType::Simple("Integer".into())));
        let text = false_or.to_string();
        assert_eq!(text, "Integer | False");
        let reparsed = DeclaredType::parse(&text);
        assert_eq!(reparsed.to_string(), text);
    }

    #[test]
    fn round_trip_difference_rendering() {
        let diff = DeclaredType::Difference {
            base: Box::new(DeclaredType::Simple("Symbol".into())),
            excluded: Box::new(DeclaredType::Singleton("foo".into())),
        };
        let text = diff.to_string();
        assert_eq!(text, "Symbol \\ #foo");
        // The `\` grammar isn't understood by `parse`, so this degrades to
        // an opaque `Simple` — but the *text* stays stable across the trip.
        let reparsed = DeclaredType::parse(&text);
        assert_eq!(reparsed, DeclaredType::Simple(text.clone().into()));
        assert_eq!(reparsed.to_string(), text);
    }

    fn assert_round_trip(s: &str) {
        let parsed = DeclaredType::parse(s);
        assert_eq!(
            parsed.to_string(),
            s,
            "round-trip mismatch for {s:?}: parsed as {parsed:?}"
        );
    }

    // ---- parse: malformed input never panics, degrades to Simple ----

    #[test]
    fn parse_unterminated_generic_degrades_to_simple() {
        let result = DeclaredType::parse("Array(Integer");
        assert_eq!(result, DeclaredType::Simple("Array(Integer".into()));
    }

    #[test]
    fn parse_empty_string_degrades_to_simple_empty() {
        let result = DeclaredType::parse("");
        assert_eq!(result, DeclaredType::Simple("".into()));
    }

    #[test]
    fn parse_bare_hash_degrades_to_simple() {
        // `#` alone has no singleton name to strip — falls back to Simple("#").
        let result = DeclaredType::parse("#");
        assert_eq!(result, DeclaredType::Simple("#".into()));
    }

    // ---- parse: self-type shapes come back structurally ----

    #[test]
    fn parse_bare_self_returns_self_type() {
        let result = DeclaredType::parse("Self");
        assert_eq!(result, DeclaredType::SelfType);
        assert_eq!(result.to_string(), "Self");
    }

    #[test]
    fn parse_self_class_returns_self_class() {
        // Must win over the `<Name> class` suffix rule — `ClassOf("Self")`
        // would be wrong.
        let result = DeclaredType::parse("Self class");
        assert_eq!(result, DeclaredType::SelfClass);
        assert_eq!(result.to_string(), "Self class");
    }

    #[test]
    fn parse_class_of_returns_class_of() {
        let result = DeclaredType::parse("Actor class");
        assert_eq!(result, DeclaredType::ClassOf("Actor".into()));
        assert_eq!(result.to_string(), "Actor class");
    }

    #[test]
    fn parse_self_inside_union_and_generic() {
        // Leaf recognition composes with the union/generic grammar.
        assert_eq!(
            DeclaredType::parse("Self | Nil"),
            DeclaredType::union(vec![DeclaredType::SelfType, DeclaredType::simple("Nil")])
        );
        assert_eq!(
            DeclaredType::parse("Result(Self, Error)"),
            DeclaredType::generic(
                "Result",
                vec![DeclaredType::SelfType, DeclaredType::simple("Error")]
            )
        );
    }

    #[test]
    fn parse_non_identifier_class_suffix_degrades_to_simple() {
        // The base before ` class` must be a bare identifier — anything else
        // (spaces, operators) keeps the whole string as an opaque Simple.
        let result = DeclaredType::parse("A | B class");
        assert_eq!(
            result,
            DeclaredType::union(vec![
                DeclaredType::simple("A"),
                DeclaredType::ClassOf("B".into())
            ])
        );
        let opaque = DeclaredType::parse("not a name class");
        assert_eq!(opaque, DeclaredType::Simple("not a name class".into()));
    }

    // ---- Display parity with TypeAnnotation::type_name() ----

    /// Parses `src` (a full class definition with one method) and returns
    /// that method's return-type annotation.
    fn parse_return_type(src: &str) -> TypeAnnotation {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diags) = crate::source_analysis::parse(tokens);
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == crate::source_analysis::Severity::Error)
            .collect();
        assert!(errors.is_empty(), "parse failed for {src:?}: {errors:?}");
        module.classes[0].methods[0]
            .return_type
            .clone()
            .unwrap_or_else(|| panic!("no return type annotation parsed from {src:?}"))
    }

    /// Enforces the "byte-identical, keep in sync by hand" claim this
    /// module's own doc comments make about `DeclaredType::Display` vs
    /// `TypeAnnotation::type_name` — and additionally against the third
    /// independent renderer, `unparse::unparse_type_annotation_display`
    /// (BT-3089; per this repo's "No duplicate implementations" rule, a
    /// "keep in sync" comment needs an enforcing test, not just a comment).
    ///
    /// All three are legitimately separate *implementations* — they operate
    /// on different types (`TypeAnnotation` carries spans/`Identifier`s;
    /// `DeclaredType` is a span-free structural mirror parsed back out of a
    /// stored string; `unparse_type_annotation_display` builds a `Document`
    /// through the same pretty-printer used for full source unparsing) — so
    /// collapsing them into one function isn't a safe mechanical change.
    /// What they share is a rendering *contract* (same text for the same
    /// type), which this test pins directly instead of trusting the doc
    /// comment.
    fn assert_display_parity(src: &str) {
        let ann = parse_return_type(src);
        let declared = DeclaredType::from(&ann);
        let expected = ann.type_name();
        assert_eq!(
            declared.to_string(),
            expected,
            "DeclaredType::Display parity mismatch for {src:?}"
        );
        assert_eq!(
            crate::unparse::unparse_type_annotation_display(&ann),
            expected,
            "unparse_type_annotation_display parity mismatch for {src:?}"
        );
    }

    #[test]
    fn display_parity_simple() {
        assert_display_parity("Object subclass: Foo\n  bar -> Integer => 1\n");
    }

    #[test]
    fn display_parity_generic() {
        assert_display_parity("Object subclass: Foo\n  bar -> Result(Integer, String) => 1\n");
    }

    #[test]
    fn display_parity_union() {
        assert_display_parity("Object subclass: Foo\n  bar -> Integer | String => 1\n");
    }

    #[test]
    fn display_parity_singleton_union() {
        assert_display_parity("Object subclass: Foo\n  bar -> Integer | #infinity => 1\n");
    }

    #[test]
    fn display_parity_false_or() {
        assert_display_parity("Object subclass: Foo\n  bar -> Integer | False => 1\n");
    }

    #[test]
    fn display_parity_difference() {
        assert_display_parity("Object subclass: Foo\n  bar -> Symbol \\ #foo => 1\n");
    }

    #[test]
    fn display_parity_intersection() {
        assert_display_parity(
            "Protocol define: Comparable\n\nObject subclass: Foo\n  bar -> Foo & Comparable => self\n",
        );
    }

    #[test]
    fn display_parity_self_type() {
        assert_display_parity("Object subclass: Foo\n  bar -> Self => self\n");
    }

    #[test]
    fn display_parity_self_class() {
        assert_display_parity("Object subclass: Foo\n  class -> Self class => self class\n");
    }

    #[test]
    fn display_parity_class_of() {
        assert_display_parity("Object subclass: Foo\n  actorClass -> Foo class => self class\n");
    }

    #[test]
    fn display_parity_nested_generic() {
        assert_display_parity(
            "Object subclass: Foo\n  bar -> List(Result(Integer, String)) => 1\n",
        );
    }

    // ---- From<&TypeAnnotation> structural conversion ----

    #[test]
    fn from_type_annotation_simple() {
        let ann = TypeAnnotation::Simple(ident("Integer"));
        assert_eq!(
            DeclaredType::from(&ann),
            DeclaredType::Simple("Integer".into())
        );
    }

    #[test]
    fn from_type_annotation_singleton_strips_hash() {
        let ann = TypeAnnotation::Singleton {
            name: "north".into(),
            span: span(),
        };
        assert_eq!(
            DeclaredType::from(&ann),
            DeclaredType::Singleton("north".into())
        );
    }

    #[test]
    fn from_type_annotation_generic_preserves_structure() {
        let ann = TypeAnnotation::Generic {
            base: ident("Result"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("String")),
            ],
            span: span(),
        };
        assert_eq!(
            DeclaredType::from(&ann),
            DeclaredType::Generic {
                base: "Result".into(),
                parameters: vec![
                    DeclaredType::Simple("Integer".into()),
                    DeclaredType::Simple("String".into()),
                ],
            }
        );
    }

    #[test]
    fn from_type_annotation_self_variants() {
        assert_eq!(
            DeclaredType::from(&TypeAnnotation::SelfType { span: span() }),
            DeclaredType::SelfType
        );
        assert_eq!(
            DeclaredType::from(&TypeAnnotation::SelfClass { span: span() }),
            DeclaredType::SelfClass
        );
        assert_eq!(
            DeclaredType::from(&TypeAnnotation::ClassOf {
                class_name: ident("Actor"),
                span: span(),
            }),
            DeclaredType::ClassOf("Actor".into())
        );
    }

    // ---- from_inferred ----

    #[test]
    fn from_inferred_known_no_args() {
        let ty = InferredType::known("Integer");
        assert_eq!(
            DeclaredType::from_inferred(&ty),
            Some(DeclaredType::Simple("Integer".into()))
        );
    }

    #[test]
    fn from_inferred_known_with_args() {
        use crate::semantic_analysis::type_checker::TypeProvenance;
        let ty = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Inferred(span()),
        };
        assert_eq!(
            DeclaredType::from_inferred(&ty),
            Some(DeclaredType::Generic {
                base: "List".into(),
                parameters: vec![DeclaredType::Simple("String".into())],
            })
        );
    }

    #[test]
    fn from_inferred_never() {
        assert_eq!(
            DeclaredType::from_inferred(&InferredType::Never),
            Some(DeclaredType::Simple("Never".into()))
        );
    }

    #[test]
    fn from_inferred_union_of_convertible_members() {
        use crate::semantic_analysis::type_checker::TypeProvenance;
        let ty = InferredType::Union {
            members: vec![InferredType::known("Integer"), InferredType::Never],
            provenance: TypeProvenance::Inferred(span()),
        };
        let result = DeclaredType::from_inferred(&ty).expect("expected Some");
        let DeclaredType::Union(members) = result else {
            panic!("expected Union");
        };
        assert_eq!(members.len(), 2);
        assert!(members.contains(&DeclaredType::Simple("Integer".into())));
        assert!(members.contains(&DeclaredType::Simple("Never".into())));
    }

    #[test]
    fn from_inferred_dynamic_and_meta_are_none() {
        use crate::semantic_analysis::type_checker::{DynamicReason, TypeProvenance};
        assert_eq!(
            DeclaredType::from_inferred(&InferredType::Dynamic(DynamicReason::Unknown)),
            None
        );
        assert_eq!(
            DeclaredType::from_inferred(&InferredType::Meta {
                class_name: "Foo".into(),
                provenance: TypeProvenance::Inferred(span()),
            }),
            None
        );
    }

    #[test]
    fn from_inferred_nested_dynamic_arg_writes_back_as_dynamic_name() {
        // BT-3101: `List(Dynamic)` must still write back — the pre-BT-3076
        // string path always produced `"List(Dynamic)"`; skipping the whole
        // conversion was a precision regression. The nested `Dynamic`
        // becomes `Simple("Dynamic")`, which the resolver normalises back
        // to the real `Dynamic` variant (BT-2865).
        use crate::semantic_analysis::type_checker::{DynamicReason, TypeProvenance};
        let ty = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::Dynamic(DynamicReason::Unknown)],
            provenance: TypeProvenance::Inferred(span()),
        };
        assert_eq!(
            DeclaredType::from_inferred(&ty),
            Some(DeclaredType::Generic {
                base: "List".into(),
                parameters: vec![DeclaredType::Simple("Dynamic".into())],
            })
        );
    }

    #[test]
    fn from_inferred_deeply_nested_dynamic_arg() {
        use crate::semantic_analysis::type_checker::{DynamicReason, TypeProvenance};
        let inner = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::Dynamic(DynamicReason::Unknown)],
            provenance: TypeProvenance::Inferred(span()),
        };
        let ty = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![inner, InferredType::known("Error")],
            provenance: TypeProvenance::Inferred(span()),
        };
        assert_eq!(
            DeclaredType::from_inferred(&ty).map(|dt| dt.to_string()),
            Some("Result(List(Dynamic), Error)".to_string())
        );
    }

    #[test]
    fn from_inferred_union_with_dynamic_member() {
        use crate::semantic_analysis::type_checker::{DynamicReason, TypeProvenance};
        let ty = InferredType::Union {
            members: vec![
                InferredType::known("Integer"),
                InferredType::Dynamic(DynamicReason::Unknown),
            ],
            provenance: TypeProvenance::Inferred(span()),
        };
        let result = DeclaredType::from_inferred(&ty).expect("expected Some");
        assert_eq!(result.to_string(), "Integer | Dynamic");
    }

    #[test]
    fn from_inferred_nested_meta_still_aborts() {
        // The nested-`Dynamic` carve-out (BT-3101) is deliberately narrow:
        // a nested `Meta` still has no declared-type spelling the writeback
        // should commit to, so the whole conversion aborts as before.
        use crate::semantic_analysis::type_checker::TypeProvenance;
        let ty = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::Meta {
                class_name: "Foo".into(),
                provenance: TypeProvenance::Inferred(span()),
            }],
            provenance: TypeProvenance::Inferred(span()),
        };
        assert_eq!(DeclaredType::from_inferred(&ty), None);
    }
}

/// Property-based tests for type-string fidelity (BT-3100): does a
/// [`DeclaredType`] survive the round trip through its own textual form
/// (`Display`) and back (`parse`)?
///
/// `DeclaredType::parse`'s grammar (see its doc comment) is strictly weaker
/// than `Display`'s output range — it has no representation for `\`
/// (difference), `&` (intersection), or `FalseOr`, so `parse` can't always
/// invert `Display` on the first try. What it *can* do, and what these
/// properties pin, is reach a stable fixed point: re-displaying what
/// `parse` recovers, then re-parsing that, always lands on exactly the same
/// structured value and text it started converging toward — the round trip
/// never drifts, loses a branch, or panics, no matter how deeply
/// union/intersection/difference/`FalseOr`/generic shapes are nested
/// (BT-2760 grouping-paren shapes included).
///
/// One concrete non-idempotence this uncovered and pins deliberately: an
/// intersection/difference whose right/excluded operand needs
/// parenthesising (e.g. `A & (B | C)`) is misread by `parse` on the first
/// pass as a single-argument `Generic` whose "base name" is the literal
/// text `"A &"` — `split_generic_base` finds the first `(` in the whole
/// string and doesn't know `&`/`\` aren't part of a class name. Displaying
/// that `Generic` back out drops the space before the paren (`"A &(B | C)"`),
/// but from there it's stable: reparsing that already-collapsed text
/// reproduces the same `Generic` byte-for-byte forever after. Filed as a
/// follow-up (not fixed here — out of scope per this issue's "Out of
/// Scope" section, which restricts BT-3100 to adding the properties, not
/// reworking `parse`'s grammar).
#[cfg(test)]
mod property_tests {
    use super::*;
    use crate::test_helpers::test_support::arb_declared_type;
    use proptest::prelude::*;

    fn proptest_config() -> ProptestConfig {
        crate::test_helpers::test_support::proptest_config_default()
    }

    proptest! {
        #![proptest_config(proptest_config())]

        /// `parse ∘ Display` never panics on any generated shape, including
        /// the ones (`\`, `&`, `FalseOr`) the parser can't fully represent.
        #[test]
        fn parse_display_never_panics(dt in arb_declared_type()) {
            let text = dt.to_string();
            let _ = DeclaredType::parse(&text);
        }

        /// `parse ∘ Display` reaches a canonical fixed point within one
        /// extra round trip: parsing the text `dt` displays, then
        /// re-displaying and re-parsing *that*, always lands on the same
        /// structured value and text — it never keeps drifting.
        #[test]
        fn parse_display_reaches_fixed_point(dt in arb_declared_type()) {
            let text1 = dt.to_string();
            let recovered1 = DeclaredType::parse(&text1);
            let text2 = recovered1.to_string();
            let recovered2 = DeclaredType::parse(&text2);
            let text3 = recovered2.to_string();

            prop_assert_eq!(
                &recovered1, &recovered2,
                "parse(Display(x)) did not stabilise for {:?}: \
                 first pass = {:?} ({:?}), second pass = {:?} ({:?})",
                dt, recovered1, text2, recovered2, text3,
            );
            prop_assert_eq!(
                &text2, &text3,
                "Display text kept drifting for {:?}: {:?} -> {:?}",
                dt, text2, text3,
            );
        }

        /// Once at the fixed point, `parse ∘ Display` is a true identity —
        /// applying it again changes nothing further.
        #[test]
        fn parse_display_idempotent_from_fixed_point(dt in arb_declared_type()) {
            let stable = DeclaredType::parse(&DeclaredType::parse(&dt.to_string()).to_string());
            let text = stable.to_string();
            let reparsed = DeclaredType::parse(&text);
            prop_assert_eq!(&reparsed, &stable);
            prop_assert_eq!(reparsed.to_string(), text);
        }
    }
}

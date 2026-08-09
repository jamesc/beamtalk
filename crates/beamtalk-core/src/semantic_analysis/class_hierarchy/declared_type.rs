// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! A span-free, structured representation of a declared type (BT-3076).
//!
//! **DDD Context:** Semantic Analysis — Value Object
//!
//! [`MethodInfo::return_type`](super::MethodInfo::return_type) and
//! `param_types` store type signatures as flattened `EcoString`s (see
//! [`crate::ast::TypeAnnotation::type_name`]). Every consumer that needs
//! structure back out of one of those strings parses through this type —
//! `TypeChecker::resolve_type_string` (BT-3075's canonical, merged
//! hand-rolled parser) is deleted (BT-3080); every former call site now goes
//! through [`DeclaredType::parse`] +
//! [`resolve_declared_type`](crate::semantic_analysis::type_checker::resolve_type_annotation)-family
//! resolution instead. `DeclaredType` gives that structure a proper value
//! type: a span-free mirror of [`TypeAnnotation`] that can be built directly
//! from an AST annotation ([`From<&TypeAnnotation>`](#impl-From<%26TypeAnnotation>-for-DeclaredType)),
//! parsed back out of a stored string ([`DeclaredType::parse`]), or partially
//! recovered from an already-resolved [`InferredType`]
//! ([`DeclaredType::from_inferred`]).
//!
//! The canonical annotation resolver
//! ([`resolve_type_annotation`](crate::semantic_analysis::type_checker::resolve_type_annotation))
//! is built on this type (BT-3076); `MethodInfo`'s string-typed fields
//! themselves remain a possible future migration.
//!
//! Lives in the `class_hierarchy` layer (not `type_checker`) because
//! `MethodInfo` — the eventual home for a structured field — lives here, and
//! `type_checker` already depends downward on `class_hierarchy`, never the
//! reverse.

use std::fmt;

use ecow::EcoString;

use crate::ast::TypeAnnotation;
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
    /// Parse a stored type-signature string (a `MethodInfo::return_type` /
    /// `param_types` entry, or a `ClassHierarchy::state_field_type` value)
    /// into a structured `DeclaredType`.
    ///
    /// Grammar mirrors the deleted `TypeChecker::resolve_type_string`'s
    /// parsing (BT-3075, folded into this type BT-3080): split on top-level
    /// `|` (respecting parenthesis nesting) for unions, then `Base(Arg1,
    /// Arg2)` for generics (args themselves split on top-level `,`,
    /// respecting nesting), with `#name` recognised as a
    /// [`DeclaredType::Singleton`]. This is **parse only** — no keyword
    /// resolution: `Never`, `Dynamic`, `nil`, `true`, `false` all come back
    /// as plain `Simple`/`Singleton` names, exactly as written. That
    /// normalisation is the resolver's job, not the parser's (see
    /// `resolve_declared_type` in `type_resolver`).
    ///
    /// Never panics. This grammar has no representation for `\`
    /// (difference), `&` (intersection), bare `Self`, `Self class`, or
    /// `<Name> class` — strings in exactly those shapes (which *can* occur,
    /// since [`MethodInfo::return_type`](super::MethodInfo::return_type) is
    /// populated via [`TypeAnnotation::type_name`], which does render them)
    /// degrade to an opaque `Simple(whole_string)` (an unparsed string
    /// becomes a nominal class name). Malformed/unbalanced input (e.g.
    /// `"Array(Integer"`, no closing paren) degrades the same way.
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

        // Fallback: opaque nominal name, including for constructs this
        // grammar doesn't parse (`\`, `&`, `Self`, `Self class`, `<Name>
        // class`) and malformed input.
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
    ///   [`DeclaredType::Generic`] (with type args, recursively converted;
    ///   `None` if any argument is unconvertible).
    /// - [`InferredType::Never`] → `Simple("Never")`.
    /// - [`InferredType::Union`] → `Union` of converted members, `None` if
    ///   any member is unconvertible.
    /// - Everything else (`Dynamic`, `Meta`, `Negation`, `Intersection`) →
    ///   `None` — these don't have a single canonical declared-type spelling
    ///   the writeback path should commit to.
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
                    .map(DeclaredType::from_inferred)
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
                    .map(DeclaredType::from_inferred)
                    .collect::<Option<Vec<_>>>()?;
                Some(DeclaredType::Union(converted))
            }
            _ => None,
        }
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
/// per-variant rendering rules this mirrors (the single source of truth for
/// both is documented there; keep them in sync by hand, matching what
/// `TypeAnnotation` itself does across `type_name` / `needs_parens_in_*`).
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

/// Split a type-parameter/argument string on `,` while respecting
/// parenthesis nesting — e.g. `"Outer(Inner(A, B), C), D"` splits into
/// `["Outer(Inner(A, B), C)", "D"]`, not four pieces.
///
/// Shared implementation for [`split_top_level`]'s `,` case; kept as one
/// function (parameterised on the separator char) since the nesting-aware
/// scan is otherwise identical for `|` and `,`.
fn split_top_level(s: &str, sep: char) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            c if c == sep && depth == 0 => {
                result.push(s[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    let last = s[start..].trim();
    if !last.is_empty() {
        result.push(last);
    }
    result
}

/// Split a stored type-name string into `(base, args_slice)`.
///
/// Given `"Array(Integer)"` returns `("Array", Some("Integer"))`. Given
/// `"Integer"` (no parentheses) returns `("Integer", None)`. Given
/// `"Array(Integer)extra"` (not terminated by `)`) falls back to
/// `("Array(Integer)extra", None)` — the caller treats the string as opaque.
///
/// Mirrors `type_resolver::split_generic_base` — duplicated here (rather
/// than imported) because `class_hierarchy` sits below `type_checker` in the
/// dependency graph and must not reach up into it.
fn split_generic_base(type_name: &str) -> (&str, Option<&str>) {
    match type_name.split_once('(') {
        Some((base, rest)) if rest.ends_with(')') => {
            let args = &rest[..rest.len() - 1];
            (base, Some(args))
        }
        _ => (type_name, None),
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

    #[test]
    fn parse_self_class_degrades_to_simple() {
        let result = DeclaredType::parse("Self class");
        assert_eq!(result, DeclaredType::Simple("Self class".into()));
        assert_eq!(result.to_string(), "Self class");
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

    fn assert_display_parity(src: &str) {
        let ann = parse_return_type(src);
        let declared = DeclaredType::from(&ann);
        assert_eq!(
            declared.to_string(),
            ann.type_name(),
            "Display parity mismatch for {src:?}"
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
}

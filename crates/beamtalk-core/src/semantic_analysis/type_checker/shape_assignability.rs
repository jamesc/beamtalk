// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4730): assignability for map shapes (`#{#a => Integer, ...}`).
//!
//! The string-level compatibility helpers in `validation.rs` know no `#{...}`
//! class, so every check that can see a shape — on either side, at any depth —
//! is routed through [`TypeChecker::shape_assignable`] instead. It works on
//! structured [`InferredType`]s and, when the answer is "no", says *why* as a
//! [`ShapeMismatch`] naming the offending key.
//!
//! The rules (ADR 0147 § Subtyping; dictionaries are immutable, so shapes are
//! covariant in their value types):
//!
//! - **Shape to shape** — `S <: T` iff every required key of `T` is required
//!   in `S` with an assignable value; every optional key of `T` that `S` has
//!   has an assignable value (and if `S` lacks it, `S` is closed); every key
//!   of `S` that `T` does not name needs `T` open; an open `S` needs `T` open.
//! - **Shape to `Dictionary(K, V)`** — the shape's
//!   [`dictionary_view`](InferredType::dictionary_view) must be assignable
//!   (closed: its key/value unions; open: `Object`/`Object`). Bare
//!   `Dictionary` accepts any shape.
//! - **`Dictionary` to shape** — the gradual generics rule: bare
//!   `Dictionary`, or one with a `Dynamic` argument, is accepted silently; a
//!   fully typed `Dictionary(K, V)` is not.

use ecow::{EcoString, eco_format};

use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::string_utils::edit_distance;

use super::{InferredType, TypeChecker, TypeStringContext, type_resolver};

/// Why a value is not assignable to a shape-bearing type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ShapeMismatch {
    /// The expected shape requires `key`; the actual type does not (always)
    /// provide it.
    MissingKey { key: EcoString },
    /// The expected shape is optional in `key` and the actual shape is open,
    /// so it may hold `key` with any type.
    OpenMayHoldKey { key: EcoString },
    /// The actual shape names `key`, which the closed expected shape does not.
    UnexpectedKey {
        key: EcoString,
        suggestion: Option<EcoString>,
    },
    /// The actual shape is open, the expected one is closed.
    OpenIntoClosed,
    /// The value type of `key` is not assignable; `inner` says why when it is
    /// itself a shape mismatch.
    ValueType {
        key: EcoString,
        actual: EcoString,
        expected: EcoString,
        inner: Option<Box<ShapeMismatch>>,
    },
    /// A fully typed `Dictionary(K, V)` is not assignable to a shape.
    TypedDictionary { actual: EcoString },
    /// Anything else (a non-`Dictionary` against a shape, or a shape against
    /// an unrelated class, or inside a union/generic).
    Incompatible,
}

impl ShapeMismatch {
    /// A one-line explanation naming the offending key, for a diagnostic hint.
    pub(super) fn describe(&self) -> EcoString {
        match self {
            Self::MissingKey { key } => eco_format!("missing required key `#{key}`"),
            Self::OpenMayHoldKey { key } => eco_format!(
                "an open shape may hold `#{key}` with any type, but the expected shape types it"
            ),
            Self::UnexpectedKey { key, suggestion } => {
                if let Some(s) = suggestion {
                    eco_format!(
                        "key `#{key}` is not named by the closed expected shape — did you mean `#{s}`?"
                    )
                } else {
                    eco_format!("key `#{key}` is not named by the closed expected shape")
                }
            }
            Self::OpenIntoClosed => {
                EcoString::from("an open shape (`...`) is not assignable to a closed shape")
            }
            Self::ValueType {
                key,
                actual,
                expected,
                inner,
            } => {
                if let Some(inner) = inner {
                    eco_format!("in key `#{key}`: {}", inner.describe())
                } else {
                    eco_format!("key `#{key}` expects {expected}, got {actual}")
                }
            }
            Self::TypedDictionary { actual } => eco_format!(
                "{actual} is not assignable to a shape (only bare `Dictionary` or a `Dynamic` \
                 argument is); annotate a local to assert it"
            ),
            Self::Incompatible => EcoString::from("the types are not compatible"),
        }
    }
}

impl InferredType {
    /// `true` when a map shape (ADR 0147) appears anywhere in this type.
    pub(super) fn contains_shape(&self) -> bool {
        match self {
            Self::Shape { .. } => true,
            Self::Known { type_args, .. } => type_args.iter().any(Self::contains_shape),
            Self::Union { members, .. } | Self::Intersection { members, .. } => {
                members.iter().any(Self::contains_shape)
            }
            Self::Negation { base, excluded, .. } => {
                base.contains_shape() || excluded.contains_shape()
            }
            Self::Meta { .. } | Self::ProtocolMeta { .. } | Self::Dynamic(_) | Self::Never => false,
        }
    }
}

fn is_dictionary(name: &str) -> bool {
    name == "Dictionary"
}

impl TypeChecker {
    /// Is `actual` assignable to `expected`, where a shape appears in either?
    ///
    /// `Ok(())` when it is — also (conservatively) for anything this function
    /// cannot judge, such as `Dynamic`, class objects, or intersections.
    pub(super) fn shape_assignable(
        actual: &InferredType,
        expected: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> Result<(), ShapeMismatch> {
        match (actual, expected) {
            (InferredType::Dynamic(_) | InferredType::Never, _) | (_, InferredType::Dynamic(_)) => {
                Ok(())
            }
            (_, InferredType::Never) => Err(ShapeMismatch::Incompatible),
            // Every member of a union actual must fit.
            (InferredType::Union { members, .. }, _) => members
                .iter()
                .try_for_each(|m| Self::shape_assignable(m, expected, hierarchy)),
            // Some member of a union expected must accept it. A single
            // candidate shape explains itself; otherwise there is nothing
            // better to say than "incompatible".
            (_, InferredType::Union { members, .. }) => {
                let mut first_err = None;
                let mut shape_members = 0;
                for member in members {
                    match Self::shape_assignable(actual, member, hierarchy) {
                        Ok(()) => return Ok(()),
                        Err(e) => {
                            if matches!(member, InferredType::Shape { .. }) {
                                shape_members += 1;
                            }
                            first_err.get_or_insert(e);
                        }
                    }
                }
                match (shape_members, first_err) {
                    (1, Some(e)) if matches!(actual, InferredType::Shape { .. }) => Err(e),
                    _ => Err(ShapeMismatch::Incompatible),
                }
            }
            (
                InferredType::Shape {
                    fields: afields,
                    open: aopen,
                    ..
                },
                InferredType::Shape {
                    fields: efields,
                    open: eopen,
                    ..
                },
            ) => Self::shape_to_shape(afields, *aopen, efields, *eopen, hierarchy),
            // Shape to a class: the shape behaves as its `Dictionary(K, V)`.
            (InferredType::Shape { .. }, InferredType::Known { .. }) => {
                let view = actual
                    .dictionary_view()
                    .ok_or(ShapeMismatch::Incompatible)?;
                Self::known_assignable(&view, expected, hierarchy)
            }
            (
                InferredType::Known { .. },
                InferredType::Shape { .. } | InferredType::Known { .. },
            ) => Self::known_assignable(actual, expected, hierarchy),
            // A shape against a class object / protocol metatype is a
            // mismatch; the reverse is judged by the callers' metatype paths.
            (InferredType::Shape { .. }, _) | (_, InferredType::Shape { .. }) => {
                Err(ShapeMismatch::Incompatible)
            }
            // Intersections, negations, metatypes: not judged here.
            _ => Ok(()),
        }
    }

    fn shape_to_shape(
        afields: &[super::ShapeFieldType],
        aopen: bool,
        efields: &[super::ShapeFieldType],
        eopen: bool,
        hierarchy: &ClassHierarchy,
    ) -> Result<(), ShapeMismatch> {
        for ef in efields {
            match afields.iter().find(|f| f.key == ef.key) {
                Some(af) => {
                    if af.optional && !ef.optional {
                        return Err(ShapeMismatch::MissingKey {
                            key: ef.key.clone(),
                        });
                    }
                    if let Err(inner) = Self::shape_assignable(&af.value, &ef.value, hierarchy) {
                        return Err(ShapeMismatch::ValueType {
                            key: ef.key.clone(),
                            actual: af.value.display_name(),
                            expected: ef.value.display_name(),
                            inner: (!matches!(inner, ShapeMismatch::Incompatible))
                                .then(|| Box::new(inner)),
                        });
                    }
                }
                None if !ef.optional => {
                    return Err(ShapeMismatch::MissingKey {
                        key: ef.key.clone(),
                    });
                }
                None if aopen => {
                    return Err(ShapeMismatch::OpenMayHoldKey {
                        key: ef.key.clone(),
                    });
                }
                None => {}
            }
        }
        if !eopen {
            if let Some(extra) = afields
                .iter()
                .find(|af| !efields.iter().any(|ef| ef.key == af.key))
            {
                let names: Vec<EcoString> = efields.iter().map(|f| f.key.clone()).collect();
                return Err(ShapeMismatch::UnexpectedKey {
                    key: extra.key.clone(),
                    suggestion: Self::closest_key(&extra.key, &names),
                });
            }
            if aopen {
                return Err(ShapeMismatch::OpenIntoClosed);
            }
        }
        Ok(())
    }

    /// Nearest of `keys` to `target` by edit distance (a "did you mean").
    fn closest_key(target: &str, keys: &[EcoString]) -> Option<EcoString> {
        keys.iter()
            .map(|k| (k, edit_distance(target, k.as_str())))
            .filter(|(_, d)| *d > 0 && *d <= 3 && *d < target.len() / 2 + 1)
            .min_by_key(|(_, d)| *d)
            .map(|(k, _)| k.clone())
    }

    /// `Known` vs `Known` with a shape somewhere inside (a type argument), or
    /// `Dictionary` against a shape.
    fn known_assignable(
        actual: &InferredType,
        expected: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> Result<(), ShapeMismatch> {
        let (
            InferredType::Known {
                class_name: aname,
                type_args: aargs,
                ..
            },
            expected_known,
        ) = (actual, expected)
        else {
            return Ok(());
        };
        if super::is_type_var_placeholder(aname, hierarchy) {
            return Ok(());
        }
        // Against a shape: only a `Dictionary` (gradually) can stand in.
        if matches!(expected, InferredType::Shape { .. }) {
            if !Self::is_type_compatible(aname, &"Dictionary".into(), hierarchy)
                || (!is_dictionary(aname) && hierarchy.has_class(aname))
                    && !hierarchy
                        .superclass_chain(aname)
                        .iter()
                        .any(|c| is_dictionary(c))
            {
                return Err(ShapeMismatch::Incompatible);
            }
            // Interim (until map literals infer shapes, ADR 0147 § Map
            // literals infer shapes): a symbol-literal map still infers
            // `Dictionary(#k1 | #k2, V)`. Its keys are singletons, which no
            // annotation writes, so it is the literal and is accepted like a
            // bare `Dictionary` rather than rejected as "fully typed".
            let literal_keys = aargs.first().is_some_and(Self::is_singleton_key_type);
            let gradual = aargs.is_empty()
                || literal_keys
                || aargs.iter().any(|a| {
                    matches!(a, InferredType::Dynamic(_))
                        || a.as_known()
                            .is_some_and(|n| super::is_type_var_placeholder(n, hierarchy))
                });
            return if gradual {
                Ok(())
            } else {
                Err(ShapeMismatch::TypedDictionary {
                    actual: actual.display_name(),
                })
            };
        }
        let InferredType::Known {
            class_name: ename,
            type_args: eargs,
            ..
        } = expected_known
        else {
            return Ok(());
        };
        if !Self::is_type_compatible(aname, ename, hierarchy) {
            return Err(ShapeMismatch::Incompatible);
        }
        // Same arity on both sides: judge each argument (covariantly — the
        // shape's `dictionary_view` and immutable collections). A bare side is
        // gradual.
        if aargs.is_empty() || eargs.is_empty() || aargs.len() != eargs.len() {
            return Ok(());
        }
        aargs
            .iter()
            .zip(eargs.iter())
            .try_for_each(|(a, e)| Self::arg_assignable(a, e, hierarchy))
    }

    /// `true` for `#k` or a union of `#k`s — the key type of a symbol-literal
    /// map's current `Dictionary(K, V)` inference.
    fn is_singleton_key_type(ty: &InferredType) -> bool {
        match ty {
            InferredType::Known { class_name, .. } => class_name.starts_with('#'),
            InferredType::Union { members, .. } => members.iter().all(Self::is_singleton_key_type),
            _ => false,
        }
    }

    /// One type-argument slot. Without a shape on either side this defers to
    /// the existing nominal rule so non-shape behaviour is unchanged.
    fn arg_assignable(
        actual: &InferredType,
        expected: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> Result<(), ShapeMismatch> {
        if actual.contains_shape() || expected.contains_shape() {
            return Self::shape_assignable(actual, expected, hierarchy);
        }
        if Self::type_args_compatible(expected, actual, hierarchy) {
            Ok(())
        } else {
            Err(ShapeMismatch::Incompatible)
        }
    }

    /// Resolve a declared type (aliases expanded, `subst` applied) to a
    /// structured [`InferredType`], recording the aliases it names for
    /// hot-reload re-checking.
    pub(super) fn resolve_declared_recording_aliases(
        &mut self,
        declared: &crate::semantic_analysis::class_hierarchy::DeclaredType,
        subst: &type_resolver::SubstitutionMap,
    ) -> InferredType {
        let (resolved, deps) = type_resolver::resolve_declared_type_with_alias_deps(
            declared,
            subst,
            self.protocol_registry.as_deref(),
            self.alias_registry.as_deref(),
            TypeStringContext::Substitution,
        );
        self.referenced_aliases.extend(deps);
        resolved
    }

    /// The warning for a value of type `actual` flowing into a slot of type
    /// `expected` where `mismatch` says why not. `subject` completes
    /// "`subject` expects X, got Y" for the caller's context.
    pub(super) fn shape_mismatch_diagnostic(
        headline: String,
        mismatch: &ShapeMismatch,
        expected: &InferredType,
        span: crate::source_analysis::Span,
    ) -> crate::source_analysis::Diagnostic {
        use crate::source_analysis::{Diagnostic, DiagnosticCategory};
        let expected_display = expected
            .display_for_diagnostic()
            .unwrap_or_else(|| EcoString::from("Dynamic"));
        let hint = match mismatch {
            ShapeMismatch::Incompatible => format!("Expected {expected_display}"),
            other => format!("{} (expected {expected_display})", other.describe()),
        };
        Diagnostic::warning(headline, span)
            .with_hint(hint)
            .with_category(DiagnosticCategory::Type)
    }

    /// The shared slot check for field defaults, field assignments and
    /// `spawnWith:` values. When a shape appears on either side, judges
    /// `actual` against `expected` structurally, pushes the warning
    /// (`headline` gets the two display strings) and returns `true`; returns
    /// `false` — nothing judged — when no shape is involved.
    pub(super) fn check_shape_slot(
        &mut self,
        actual: &InferredType,
        expected: &InferredType,
        hierarchy: &ClassHierarchy,
        span: crate::source_analysis::Span,
        headline: impl FnOnce(&str, &str) -> String,
    ) -> bool {
        if !(expected.contains_shape() || actual.contains_shape()) {
            return false;
        }
        if let Err(mismatch) = Self::shape_assignable(actual, expected, hierarchy) {
            let show = |t: &InferredType| {
                t.display_for_diagnostic()
                    .unwrap_or_else(|| EcoString::from("Dynamic"))
            };
            let diag = Self::shape_mismatch_diagnostic(
                headline(&show(actual), &show(expected)),
                &mismatch,
                expected,
                span,
            );
            self.diagnostics.push(diag);
        }
        true
    }
}

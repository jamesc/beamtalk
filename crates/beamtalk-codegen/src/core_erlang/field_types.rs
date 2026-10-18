// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-field type facts for the operator fast-path classifiers (BT-3892,
//! BT-3925).
//!
//! **DDD Context:** Code Generation
//!
//! `receiver_is_statically_numeric` / `receiver_is_statically_comparable`
//! (`operators.rs`) keep `self.<field> + x` / `self.<field> < x` on the bare
//! Erlang BIF only when the field is *known* to hold a primitive. This module
//! owns what "known" means for a field, so the decision is made once, where
//! the field-type map is built, rather than special-cased at the operator.
//!
//! The rule is the declared type, and nothing else:
//!
//! * A **declared** field records its full [`DeclaredType`]. A simple type is
//!   bare when it is in the operator's primitive set. For **arithmetic** a
//!   union is bare when *every non-`Nil` member* is (so `:: Integer | Nil`
//!   stays bare while `:: Point | Nil` is guarded): the arithmetic BIF already
//!   raises `badarith` on `nil`. For **comparison** a `Nil` member makes the
//!   union guarded: the comparison BIF would silently term-order `nil`
//!   (`nil < 5` → `false`), while the runtime guard dispatches it to a
//!   `does_not_understand` (BT-3924). See [`BareOperator`]. Every other shape
//!   (generic, singleton, false-or, …) is an object type and is guarded.
//! * An **untyped** field — `state:`, `field:` or `classState:`, of any class
//!   kind, whatever its default — is always guarded
//!   ([`FieldTypeFact::Untyped`]). A field's writers are not all in its class
//!   source: `spawnWith:`, a Value's keyword constructor, `fieldAt:put:`, a
//!   subclass assigning an inherited field, an inherited class method running
//!   over a redeclared `classState:` (ADR 0130) or a class-side extension can
//!   each store an object in a field whose default is `0` (BT-3925). The
//!   guard is a type test on the hot path and measured no slower than the
//!   bare BIF on the Actor counter benchmark
//!   (`runtime/perf/self_send_bench`, `SsbMain counters`), so `:: Integer`
//!   (or `Float`/`Number`) is the way to state a field is numeric.
//! * A field with **no fact** (an inherited untyped field, an extension
//!   target's untyped field, a name codegen knows nothing about) is guarded
//!   too — the guard is always correct, merely not free.

use std::collections::{HashMap, HashSet};

use beamtalk_core::ast::{Expression, MethodDefinition, Pattern, StateDeclaration};
use beamtalk_core::ast_walker;
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use beamtalk_core::semantic_analysis::extract_pattern_bindings;

/// Builtin types on which the arithmetic BIFs (`+ - * /`) are correct.
pub(in crate::core_erlang) const NUMERIC_TYPES: &[&str] = &["Integer", "Float", "Number"];

/// Builtin types with a primitive total order, on which the comparison BIFs
/// (`< > <= >=`) are correct: the numeric types plus `Character` and `String`
/// (both define `< <=` as `@primitive`).
pub(in crate::core_erlang) const COMPARABLE_TYPES: &[&str] =
    &["Integer", "Float", "Number", "Character", "String"];

/// What codegen knows about one field's runtime type.
#[derive(Debug, Clone, PartialEq)]
pub(in crate::core_erlang) enum FieldTypeFact {
    /// The field carries an explicit `::` annotation.
    Declared(DeclaredType),
    /// No annotation: the field may hold anything, so it is always guarded.
    Untyped,
}

/// The operator family a fast-path decision is for: it fixes which builtin
/// types the bare BIF is correct on, and whether a `Nil` union member may
/// share the bare path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum BareOperator {
    /// `+ - * /` — bare on [`NUMERIC_TYPES`]; the BIF raises `badarith` on
    /// `nil`, so a `Nil` union member does not force the guard.
    Arithmetic,
    /// `< > <= >=` — bare on [`COMPARABLE_TYPES`]; the BIF is a total term
    /// order that would silently answer `nil < 5` → `false`, so a `Nil`
    /// union member forces the runtime guard, which dispatches `nil` to a
    /// `does_not_understand` (BT-3924).
    Comparison,
}

impl BareOperator {
    /// Builtin types on which this operator's BIF is correct.
    pub(in crate::core_erlang) fn primitive_set(self) -> &'static [&'static str] {
        match self {
            Self::Arithmetic => NUMERIC_TYPES,
            Self::Comparison => COMPARABLE_TYPES,
        }
    }

    /// Whether a `nil` reaching this operator's bare BIF raises (so a `Nil`
    /// union member may stay on the bare path).
    fn bif_raises_on_nil(self) -> bool {
        matches!(self, Self::Arithmetic)
    }
}

impl FieldTypeFact {
    /// Whether a `self.<field>` read with this fact may skip the runtime
    /// operator guard for `op`.
    pub(in crate::core_erlang) fn is_bare(&self, op: BareOperator) -> bool {
        match self {
            Self::Declared(ty) => declared_type_is_bare(ty, op),
            Self::Untyped => false,
        }
    }
}

/// A declared type is bare when it is a simple type in `op`'s primitive set,
/// or — only for an operator whose BIF raises on `nil` — a union with at least
/// one non-`Nil` member whose non-`Nil` members all are.
fn declared_type_is_bare(ty: &DeclaredType, op: BareOperator) -> bool {
    match ty {
        DeclaredType::Simple(name) => op.primitive_set().contains(&name.as_str()),
        DeclaredType::Union(members) => {
            if !op.bif_raises_on_nil() && members.iter().any(is_nil_type) {
                return false;
            }
            let mut non_nil = members.iter().filter(|m| !is_nil_type(m)).peekable();
            non_nil.peek().is_some() && non_nil.all(|m| declared_type_is_bare(m, op))
        }
        _ => false,
    }
}

fn is_nil_type(ty: &DeclaredType) -> bool {
    matches!(ty, DeclaredType::Simple(name) if name == "Nil" || name == "UndefinedObject")
}

/// Classifies every field in `decls` (`state:`/`field:` or `classState:`) by
/// its declaration alone: an annotated field records its declared type, an
/// untyped one is [`FieldTypeFact::Untyped`] (BT-3925).
pub(in crate::core_erlang) fn classify_fields(
    decls: &[StateDeclaration],
) -> HashMap<String, FieldTypeFact> {
    decls
        .iter()
        .map(|d| {
            let fact = d
                .type_annotation
                .as_ref()
                .map_or(FieldTypeFact::Untyped, |ann| {
                    FieldTypeFact::Declared(DeclaredType::from(ann))
                });
            (d.name.name.to_string(), fact)
        })
        .collect()
}

/// The `(name, simple type name)` of each of `method`'s parameters whose
/// `Simple` type annotation may be trusted throughout the body (BT-3921).
///
/// A typed parameter is evidence only while its name means the parameter: a
/// block parameter, local or pattern variable of the same name (shadowing is
/// a warning, not an error) may hold anything, and neither the field
/// classifier's walk nor the operator fast-path classifiers
/// (`receiver_is_statically_numeric` / `receiver_is_statically_comparable`,
/// which match an identifier against the method's parameters by name) track
/// scopes. So a parameter whose name the body rebinds anywhere is left out —
/// its operators keep the runtime guard, which is always correct. Parameters
/// with no annotation or a non-`Simple` one are left out too.
pub(in crate::core_erlang) fn trusted_param_types(method: &MethodDefinition) -> Vec<(&str, &str)> {
    let rebound = names_rebound_in(method);
    method
        .parameters
        .iter()
        .filter(|p| !rebound.contains(p.name.name.as_str()))
        .filter_map(|p| match p.type_annotation.as_ref() {
            Some(beamtalk_core::ast::TypeAnnotation::Simple(id)) => {
                Some((p.name.name.as_str(), id.name.as_str()))
            }
            _ => None,
        })
        .collect()
}

/// Every name `method`'s body binds: block parameters, plain assignment
/// targets, and destructuring / `match:` pattern variables.
/// Used by [`trusted_param_types`] to drop a typed parameter whose name the
/// body rebinds (BT-3921).
fn names_rebound_in(method: &MethodDefinition) -> HashSet<String> {
    let mut names = HashSet::new();
    let add_pattern = |names: &mut HashSet<String>, pattern: &Pattern| {
        let (bindings, _) = extract_pattern_bindings(pattern);
        names.extend(bindings.into_iter().map(|id| id.name.to_string()));
    };
    for stmt in &method.body {
        ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
            Expression::Block(block) => {
                names.extend(block.parameters.iter().map(|p| p.name.to_string()));
            }
            Expression::Assignment { target, .. } => {
                if let Expression::Identifier(id) = target.unwrap_parens() {
                    names.insert(id.name.to_string());
                }
            }
            Expression::DestructureAssignment { pattern, .. } => add_pattern(&mut names, pattern),
            Expression::Match { arms, .. } => {
                for arm in arms {
                    add_pattern(&mut names, &arm.pattern);
                }
            }
            _ => {}
        });
    }
    names
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_class(src: &str) -> beamtalk_core::ast::ClassDefinition {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diags.is_empty(), "parse errors: {diags:?}");
        module.classes.into_iter().next().expect("one class")
    }

    /// Parses `src` (one class) and classifies its instance fields.
    fn instance_facts(src: &str) -> HashMap<String, FieldTypeFact> {
        classify_fields(&parse_class(src).state)
    }

    fn class_side_facts(src: &str) -> HashMap<String, FieldTypeFact> {
        classify_fields(&parse_class(src).class_variables)
    }

    fn numeric(facts: &HashMap<String, FieldTypeFact>, f: &str) -> bool {
        facts[f].is_bare(BareOperator::Arithmetic)
    }

    fn comparable(facts: &HashMap<String, FieldTypeFact>, f: &str) -> bool {
        facts[f].is_bare(BareOperator::Comparison)
    }

    #[test]
    fn untyped_fields_are_never_trusted_whatever_their_default_or_writers() {
        // BT-3925: `spawnWith:`, a keyword constructor, a subclass or an
        // extension can store an object in any of these, so even the
        // all-numeric Actor counter is guarded.
        for src in [
            "Actor subclass: C\n  state: count = 0\n  \
             bump => self.count := self.count + 1\n",
            "sealed Actor subclass: C\n  state: count = 0\n  \
             bump => self.count := self.count + 1\n",
            "Value subclass: C\n  field: count = 0\n  shift: q => self.count + q\n",
            "Object subclass: C\n  field: count = 0.5\n",
            "TestCase subclass: C\n  field: count = nil\n",
            "Actor subclass: C\n  state: count\n",
        ] {
            let facts = instance_facts(src);
            assert_eq!(facts["count"], FieldTypeFact::Untyped, "{src}");
            assert!(!numeric(&facts, "count") && !comparable(&facts, "count"));
        }
    }

    #[test]
    fn untyped_class_variables_are_never_trusted_even_in_a_sealed_class() {
        for src in [
            "Object subclass: H\n  classState: hits = 0\n  \
             class bump => self.hits := self.hits + 1\n",
            "sealed Object subclass: H\n  classState: hits = 0\n  \
             class bump => self.hits := self.hits + 1\n",
        ] {
            let facts = class_side_facts(src);
            assert_eq!(facts["hits"], FieldTypeFact::Untyped, "{src}");
            assert!(!numeric(&facts, "hits"));
        }
    }

    #[test]
    fn numeric_annotation_is_the_fast_path() {
        let facts = instance_facts(
            "Actor subclass: C\n  state: count :: Integer = 0\n  \
             state: total :: Float = 0.5\n  state: n :: Number = 0\n",
        );
        for f in ["count", "total", "n"] {
            assert!(numeric(&facts, f) && comparable(&facts, f), "{f}");
        }
        let class_side =
            class_side_facts("Object subclass: H\n  classState: hits :: Integer = 0\n");
        assert!(numeric(&class_side, "hits"));
    }

    #[test]
    fn names_rebound_in_collects_block_params_locals_and_patterns() {
        let class = parse_class(
            "Actor subclass: H\n  \
             add: n :: Integer in: xs from: t =>\n    \
             xs do: [:b | b]\n    tmp := 1\n    {p, _} := t\n    n\n",
        );
        let names = names_rebound_in(&class.methods[0]);
        for name in ["b", "tmp", "p"] {
            assert!(names.contains(name), "{name} missing from {names:?}");
        }
        assert!(!names.contains("n") && !names.contains("xs"), "{names:?}");
    }

    #[test]
    fn union_typed_fields_are_recorded_and_bare_only_when_all_non_nil_members_are() {
        let facts = instance_facts(
            "Value subclass: H\n  field: tp :: Point | Nil = nil\n  \
             field: n :: Integer | Nil = nil\n  field: nf :: Integer | Float\n  \
             field: s :: String | Nil = nil\n  field: mix :: Integer | Point\n",
        );
        assert!(matches!(
            facts["tp"],
            FieldTypeFact::Declared(DeclaredType::Union(_))
        ));
        assert!(!numeric(&facts, "tp") && !comparable(&facts, "tp"));
        // A `Nil` member keeps arithmetic bare (the BIF `badarith`s on nil) but
        // guards comparison (the BIF would silently term-order nil, BT-3924).
        assert!(numeric(&facts, "n") && !comparable(&facts, "n"));
        assert!(numeric(&facts, "nf") && comparable(&facts, "nf"));
        assert!(!numeric(&facts, "s") && !comparable(&facts, "s"));
        assert!(!numeric(&facts, "mix") && !comparable(&facts, "mix"));
    }

    #[test]
    fn plain_typed_fields_follow_their_declared_type() {
        let facts = instance_facts(
            "Value subclass: H\n  field: ok :: Point = nil\n  field: c :: Integer = 0\n  \
             field: ch :: Character = $a\n",
        );
        assert!(!numeric(&facts, "ok") && !comparable(&facts, "ok"));
        assert!(numeric(&facts, "c") && comparable(&facts, "c"));
        assert!(!numeric(&facts, "ch") && comparable(&facts, "ch"));
    }

    #[test]
    fn nil_only_union_is_not_bare() {
        let ty = DeclaredType::union(vec![DeclaredType::simple("Nil")]);
        assert!(!FieldTypeFact::Declared(ty).is_bare(BareOperator::Arithmetic));
    }
}

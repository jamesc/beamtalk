// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-field type facts for the operator fast-path classifiers (BT-3892).
//!
//! **DDD Context:** Code Generation
//!
//! `receiver_is_statically_numeric` / `receiver_is_statically_comparable`
//! (`operators.rs`) keep `self.<field> + x` / `self.<field> < x` on the bare
//! Erlang BIF only when the field is *known* to hold a primitive. This module
//! owns what "known" means for a field, so the decision is made once, where
//! the field-type map is built, rather than special-cased at the operator:
//!
//! * A **declared** field records its full [`DeclaredType`]. A simple type is
//!   bare when it is in the operator's primitive set; a union is bare only when
//!   *every non-`Nil` member* is (so `:: Integer | Nil` stays bare while
//!   `:: Point | Nil` is guarded). `nil` is not an object, so the runtime
//!   guard would send it to the bare BIF anyway. Every other shape (generic,
//!   singleton, false-or, …) is an object type and is guarded.
//! * An **untyped** field of an **Actor** (`state:`) or a `classState:`
//!   variable of a **sealed** class is trusted as numeric only on positive
//!   evidence gathered from the class's own source
//!   ([`FieldTypeFact::UntypedNumeric`]): its default is a numeric literal
//!   *and* every in-class `self.f := e` writes a
//!   numeric-evidenced `e` (a numeric literal, a numeric-typed parameter,
//!   another numeric-evidenced field, or `+ - * / %` over two
//!   numeric-evidenced operands). This keeps the `self.count := self.count + 1`
//!   counter pattern bare while `state: p = nil` (later set to a `Point`)
//!   routes through the runtime guard and dispatches. Untyped fields of every
//!   other class kind (Value, Object, `TestCase`, …) are never trusted: their
//!   usual writer is a keyword constructor (`H origin: aPoint`) sent from
//!   outside the class, which the class source cannot show, so a numeric
//!   default says nothing about the value a read sees.
//! * A field with **no fact** (an inherited untyped field, an extension
//!   target's untyped field, a name codegen knows nothing about) is guarded —
//!   the guard is always correct, merely not free.
//!
//! A `classState:` variable of a class that is not sealed is never trusted:
//! an inherited class method runs over a subclass's redeclared variable
//! (ADR 0130), with a default and writers this class cannot see.
//!
//! Open-world writers the class source cannot show — `spawnWith:`,
//! `fieldAt:put:`, a subclass assigning an inherited untyped `state:` field,
//! or a class-side extension of a sealed class writing a `classState:`
//! variable — are not tracked (BT-3925): the literal default is taken as the
//! field's declared shape. An Actor's `state:` is deliberately *not* gated on
//! `sealed` the way `classState:` is: that would guard the
//! `self.count := self.count + 1` counter in every open Actor, and the
//! subclass-write case shares BT-3925's open-world gap with `spawnWith:`, so
//! it is decided there together.

use std::collections::{HashMap, HashSet};

use beamtalk_core::ast::{
    Expression, Literal, MessageSelector, MethodDefinition, Pattern, StateDeclaration,
};
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

/// Binary operators whose result is a number when both operands are numbers
/// (the operators `generate_binary_op` lowers to an arithmetic BIF).
const NUMERIC_RESULT_OPERATORS: &[&str] = &["+", "-", "*", "/", "%"];

/// What codegen knows about one field's runtime type.
#[derive(Debug, Clone, PartialEq)]
pub(in crate::core_erlang) enum FieldTypeFact {
    /// The field carries an explicit `::` annotation.
    Declared(DeclaredType),
    /// Untyped, but its default and every in-class assignment are numeric.
    UntypedNumeric,
    /// Untyped with no numeric evidence (e.g. `field: p = nil`).
    Untyped,
}

impl FieldTypeFact {
    /// Whether a `self.<field>` read with this fact may skip the runtime
    /// operator guard for an operator correct on exactly `primitive_set`.
    pub(in crate::core_erlang) fn is_bare(&self, primitive_set: &[&str]) -> bool {
        match self {
            Self::Declared(ty) => declared_type_is_bare(ty, primitive_set),
            Self::UntypedNumeric => NUMERIC_TYPES.iter().all(|t| primitive_set.contains(t)),
            Self::Untyped => false,
        }
    }
}

/// A declared type is bare when it is a simple type in `primitive_set`, or a
/// union with at least one non-`Nil` member whose non-`Nil` members all are.
fn declared_type_is_bare(ty: &DeclaredType, primitive_set: &[&str]) -> bool {
    match ty {
        DeclaredType::Simple(name) => primitive_set.contains(&name.as_str()),
        DeclaredType::Union(members) => {
            let mut non_nil = members.iter().filter(|m| !is_nil_type(m)).peekable();
            non_nil.peek().is_some() && non_nil.all(|m| declared_type_is_bare(m, primitive_set))
        }
        _ => false,
    }
}

fn is_nil_type(ty: &DeclaredType) -> bool {
    matches!(ty, DeclaredType::Simple(name) if name == "Nil" || name == "UndefinedObject")
}

/// Whether untyped fields may be trusted as numeric from in-class evidence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum UntypedTrust {
    /// The class's own writes are the fields' normal writers (an Actor's
    /// `state:`, a sealed class's `classState:`).
    FromClassSource,
    /// The fields' usual writer is outside the class (a Value's keyword
    /// constructor), so untyped fields are never trusted.
    Never,
}

/// Classifies every field in `decls` given the method bodies that may assign
/// it via `self.<field> :=` (the instance methods for `state:`/`field:`, the
/// class methods for `classState:`).
pub(in crate::core_erlang) fn classify_fields(
    decls: &[StateDeclaration],
    writers: &[&MethodDefinition],
    trust: UntypedTrust,
) -> HashMap<String, FieldTypeFact> {
    // Optimistic start: every untyped field with a numeric-literal default is
    // a candidate; a pass drops each candidate assigned a non-evidenced value
    // until nothing changes (candidates may justify each other, e.g.
    // `self.total := self.total + self.step`).
    let mut candidates: HashSet<&str> = decls
        .iter()
        .filter(|_| trust == UntypedTrust::FromClassSource)
        .filter(|d| d.type_annotation.is_none())
        .filter(|d| d.default_value.as_ref().is_some_and(is_numeric_literal))
        .map(|d| d.name.name.as_str())
        .collect();
    loop {
        let mut rejected: Vec<String> = Vec::new();
        for method in writers {
            // A typed parameter is evidence only while its name means the
            // parameter: a block parameter, local or pattern variable of the
            // same name (shadowing is a warning, not an error) may hold
            // anything, and the walk below does not track scopes.
            let rebound = names_rebound_in(method);
            let numeric_params: HashSet<&str> = method
                .parameters
                .iter()
                .filter(|p| {
                    matches!(
                        p.type_annotation.as_ref(),
                        Some(beamtalk_core::ast::TypeAnnotation::Simple(id))
                            if NUMERIC_TYPES.contains(&id.name.as_str())
                    )
                })
                .map(|p| p.name.name.as_str())
                .filter(|name| !rebound.contains(*name))
                .collect();
            for stmt in &method.body {
                ast_walker::walk_expression(&stmt.expression, &mut |e| {
                    if let Some((field, value)) = field_write(e)
                        && candidates.contains(field)
                        && !is_numeric_evidenced(value, &candidates, &numeric_params)
                    {
                        rejected.push(field.to_string());
                    }
                });
            }
        }
        if rejected.is_empty() {
            break;
        }
        for field in &rejected {
            candidates.remove(field.as_str());
        }
    }

    decls
        .iter()
        .map(|d| {
            let fact = match d.type_annotation.as_ref() {
                Some(ann) => FieldTypeFact::Declared(DeclaredType::from(ann)),
                None if candidates.contains(d.name.name.as_str()) => FieldTypeFact::UntypedNumeric,
                None => FieldTypeFact::Untyped,
            };
            (d.name.name.to_string(), fact)
        })
        .collect()
}

/// Every name `method`'s body binds: block parameters, plain assignment
/// targets, and destructuring / `match:` pattern variables.
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

fn is_numeric_literal(expr: &Expression) -> bool {
    matches!(
        expr.unwrap_parens(),
        Expression::Literal(Literal::Integer(_) | Literal::Float(_), _)
    )
}

/// The field and written value when `expr` is a `self.f := e` write.
fn field_write(expr: &Expression) -> Option<(&str, &Expression)> {
    match expr {
        Expression::Assignment { target, value, .. } => {
            self_field_name(target).map(|field| (field, value.as_ref()))
        }
        _ => None,
    }
}

fn is_self(expr: &Expression) -> bool {
    matches!(expr.unwrap_parens(), Expression::Identifier(id) if id.name == "self")
}

/// The field name of a `self.<field>` expression.
fn self_field_name(expr: &Expression) -> Option<&str> {
    match expr.unwrap_parens() {
        Expression::FieldAccess {
            receiver, field, ..
        } if is_self(receiver) => Some(field.name.as_str()),
        _ => None,
    }
}

/// Whether `expr` is syntactically certain to evaluate to a number, given the
/// fields currently trusted as numeric and the method's numeric parameters.
fn is_numeric_evidenced(
    expr: &Expression,
    numeric_fields: &HashSet<&str>,
    numeric_params: &HashSet<&str>,
) -> bool {
    let expr = expr.unwrap_parens();
    if is_numeric_literal(expr) {
        return true;
    }
    if let Some(field) = self_field_name(expr) {
        return numeric_fields.contains(field);
    }
    match expr {
        Expression::Identifier(id) => numeric_params.contains(id.name.as_str()),
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            ..
        } => {
            NUMERIC_RESULT_OPERATORS.contains(&op.as_str())
                && arguments.len() == 1
                && is_numeric_evidenced(receiver, numeric_fields, numeric_params)
                && is_numeric_evidenced(&arguments[0], numeric_fields, numeric_params)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parses `src` (one class) and classifies its instance fields, trusting
    /// in-class evidence only for an `Actor` subclass (as the generator does).
    fn instance_facts(src: &str) -> HashMap<String, FieldTypeFact> {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diags.is_empty(), "parse errors: {diags:?}");
        let class = module.classes.first().expect("one class");
        let writers: Vec<&MethodDefinition> = class.methods.iter().collect();
        let trust = if class.superclass.as_ref().is_some_and(|s| s.name == "Actor") {
            UntypedTrust::FromClassSource
        } else {
            UntypedTrust::Never
        };
        classify_fields(&class.state, &writers, trust)
    }

    fn class_side_facts(src: &str) -> HashMap<String, FieldTypeFact> {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diags.is_empty(), "parse errors: {diags:?}");
        let class = module.classes.first().expect("one class");
        let writers: Vec<&MethodDefinition> = class.class_methods.iter().collect();
        classify_fields(
            &class.class_variables,
            &writers,
            UntypedTrust::FromClassSource,
        )
    }

    fn numeric(facts: &HashMap<String, FieldTypeFact>, f: &str) -> bool {
        facts[f].is_bare(NUMERIC_TYPES)
    }

    fn comparable(facts: &HashMap<String, FieldTypeFact>, f: &str) -> bool {
        facts[f].is_bare(COMPARABLE_TYPES)
    }

    #[test]
    fn untyped_counter_with_numeric_writes_stays_bare() {
        let facts = instance_facts(
            "Actor subclass: C\n  state: count = 0\n  state: total = 0.5\n  \
             bump => self.count := self.count + 1\n  \
             reset => self.count := 0\n  \
             add: n :: Integer => self.total := (self.total + n) * 2\n",
        );
        assert_eq!(facts["count"], FieldTypeFact::UntypedNumeric);
        assert!(numeric(&facts, "count") && comparable(&facts, "count"));
        assert!(numeric(&facts, "total") && comparable(&facts, "total"));
    }

    #[test]
    fn untyped_nil_default_is_guarded() {
        let facts = instance_facts("Value subclass: H\n  field: p = nil\n  field: q\n");
        assert_eq!(facts["p"], FieldTypeFact::Untyped);
        assert_eq!(facts["q"], FieldTypeFact::Untyped);
        assert!(!numeric(&facts, "p") && !comparable(&facts, "p"));
        assert!(!numeric(&facts, "q") && !comparable(&facts, "q"));
    }

    #[test]
    fn untyped_numeric_default_with_object_write_is_guarded() {
        let facts =
            instance_facts("Actor subclass: H\n  state: p = 0\n  reset => self.p := Point new\n");
        assert_eq!(facts["p"], FieldTypeFact::Untyped);
        assert!(!numeric(&facts, "p") && !comparable(&facts, "p"));
    }

    #[test]
    fn non_actor_untyped_numeric_default_is_never_trusted() {
        // A Value's usual writer is its keyword constructor, sent from outside
        // the class (`H origin: aPoint`), so the `0` default proves nothing.
        for src in [
            "Value subclass: H\n  field: origin = 0\n  shift: q => self.origin + q\n",
            "Object subclass: H\n  field: origin = 0\n  bump => self.origin := self.origin + 1\n",
            "TestCase subclass: H\n  field: origin = 0\n",
        ] {
            let facts = instance_facts(src);
            assert_eq!(facts["origin"], FieldTypeFact::Untyped, "{src}");
            assert!(!numeric(&facts, "origin") && !comparable(&facts, "origin"));
        }
    }

    #[test]
    fn untyped_write_inside_a_block_is_seen() {
        let facts = instance_facts(
            "Actor subclass: H\n  state: p = 0\n  run: xs => xs do: [:x | self.p := x]\n",
        );
        assert_eq!(facts["p"], FieldTypeFact::Untyped);
    }

    #[test]
    fn untyped_write_of_untyped_param_is_guarded() {
        // `self.total + n` with `n` unknown could coerce to a non-number
        // (`plusFromNumber:`), so it is not numeric evidence.
        let facts = instance_facts(
            "Actor subclass: H\n  state: total = 0\n  add: n => self.total := self.total + n\n",
        );
        assert_eq!(facts["total"], FieldTypeFact::Untyped);
    }

    #[test]
    fn typed_param_name_rebound_in_the_body_is_not_evidence() {
        // The block's `n` is a collection element, not the `:: Integer`
        // parameter it shadows.
        let shadowed = instance_facts(
            "Actor subclass: H\n  state: total = 0\n  \
             add: n :: Integer in: xs => xs do: [:n | self.total := self.total + n]\n",
        );
        assert_eq!(shadowed["total"], FieldTypeFact::Untyped);
        let pattern = instance_facts(
            "Actor subclass: H\n  state: total = 0\n  \
             add: n :: Integer from: t => {n, _} := t. self.total := self.total + n\n",
        );
        assert_eq!(pattern["total"], FieldTypeFact::Untyped);
    }

    #[test]
    fn untyped_fields_justify_each_other_until_one_falls() {
        let ok = instance_facts(
            "Actor subclass: H\n  state: a = 0\n  state: b = 1\n  \
             step => self.a := self.a + self.b\n  step2 => self.b := self.a * 2\n",
        );
        assert!(numeric(&ok, "a") && numeric(&ok, "b"));
        let bad = instance_facts(
            "Actor subclass: H\n  state: a = 0\n  state: b = 1\n  \
             step => self.a := self.a + self.b\n  poison => self.b := #x\n",
        );
        assert!(!numeric(&bad, "a") && !numeric(&bad, "b"));
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
        assert!(numeric(&facts, "n") && comparable(&facts, "n"));
        assert!(numeric(&facts, "nf") && comparable(&facts, "nf"));
        assert!(!numeric(&facts, "s") && comparable(&facts, "s"));
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
        assert!(!FieldTypeFact::Declared(ty).is_bare(NUMERIC_TYPES));
    }

    #[test]
    fn class_variables_are_classified_from_class_methods() {
        let facts = class_side_facts(
            "Object subclass: H\n  classState: hits = 0\n  classState: last = 0\n  \
             class bump => self.hits := self.hits + 1\n  \
             class remember: x => self.last := x\n",
        );
        assert!(numeric(&facts, "hits"));
        assert!(!numeric(&facts, "last"));
    }
}

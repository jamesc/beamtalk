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
//!   `:: Point | Nil` is guarded). Every other shape (generic, singleton,
//!   false-or, …) is an object type and is guarded.
//! * An **untyped** field is trusted as numeric only on positive evidence
//!   gathered from the class's own source ([`FieldTypeFact::UntypedNumeric`]):
//!   its default is a numeric literal *and* every in-class write —
//!   `self.f := e`, or a Value's copy-setter `self withF: e` — writes a
//!   numeric-evidenced `e` (a numeric literal, a numeric-typed parameter,
//!   another numeric-evidenced field, or `+ - * / %` over two
//!   numeric-evidenced operands). This keeps the `self.count := self.count + 1`
//!   counter pattern bare while `field: p = nil` (later set to a `Point`)
//!   routes through the runtime guard and dispatches.
//! * A field with **no fact** (an inherited untyped field, an extension
//!   target's untyped field, a name codegen knows nothing about) is guarded —
//!   the guard is always correct, merely not free.
//!
//! Open-world writers the class cannot see — keyword constructors and
//! `with*:` sent to a Value from outside, `spawnWith:` on an Actor,
//! `fieldAt:put:`, or a subclass assigning an inherited untyped field — are
//! not tracked: the literal default is taken as the untyped field's declared
//! shape.

use std::collections::{HashMap, HashSet};

use beamtalk_core::ast::{
    Expression, Literal, MessageSelector, MethodDefinition, StateDeclaration,
};
use beamtalk_core::ast_walker;
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use beamtalk_core::synthetic_selectors::with_star_selector;

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

/// Classifies every field in `decls` given the method bodies that may assign
/// it via `self.<field> :=` (the instance methods for `state:`/`field:`, the
/// class methods for `classState:`).
pub(in crate::core_erlang) fn classify_fields(
    decls: &[StateDeclaration],
    writers: &[&MethodDefinition],
) -> HashMap<String, FieldTypeFact> {
    // Optimistic start: every untyped field with a numeric-literal default is
    // a candidate; a pass drops each candidate assigned a non-evidenced value
    // until nothing changes (candidates may justify each other, e.g.
    // `self.total := self.total + self.step`).
    let mut candidates: HashSet<&str> = decls
        .iter()
        .filter(|d| d.type_annotation.is_none())
        .filter(|d| d.default_value.as_ref().is_some_and(is_numeric_literal))
        .map(|d| d.name.name.as_str())
        .collect();
    // `self withF: e` is an in-class write of `f` on a Value (ADR 0042
    // copy-setter), keyed by its synthesized selector.
    let with_setters: HashMap<String, &str> = decls
        .iter()
        .map(|d| (with_star_selector(&d.name.name), d.name.name.as_str()))
        .collect();

    loop {
        let mut rejected: Vec<String> = Vec::new();
        for method in writers {
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
                .collect();
            for stmt in &method.body {
                ast_walker::walk_expression(&stmt.expression, &mut |e| {
                    if let Some((field, value)) = field_write(e, &with_setters)
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

fn is_numeric_literal(expr: &Expression) -> bool {
    matches!(
        expr.unwrap_parens(),
        Expression::Literal(Literal::Integer(_) | Literal::Float(_), _)
    )
}

/// The field and written value when `expr` writes one of the class's own
/// fields: `self.f := e`, or a Value's copy-setter `self withF: e`.
fn field_write<'e>(
    expr: &'e Expression,
    with_setters: &HashMap<String, &'e str>,
) -> Option<(&'e str, &'e Expression)> {
    match expr {
        Expression::Assignment { target, value, .. } => {
            self_field_name(target).map(|field| (field, value.as_ref()))
        }
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } if parts.len() == 1 && arguments.len() == 1 && is_self(receiver) => with_setters
            .get(parts[0].keyword.as_str())
            .map(|field| (*field, &arguments[0])),
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

    /// Parses `src` (one class) and classifies its instance fields.
    fn instance_facts(src: &str) -> HashMap<String, FieldTypeFact> {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diags.is_empty(), "parse errors: {diags:?}");
        let class = module.classes.first().expect("one class");
        let writers: Vec<&MethodDefinition> = class.methods.iter().collect();
        classify_fields(&class.state, &writers)
    }

    fn class_side_facts(src: &str) -> HashMap<String, FieldTypeFact> {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diags.is_empty(), "parse errors: {diags:?}");
        let class = module.classes.first().expect("one class");
        let writers: Vec<&MethodDefinition> = class.class_methods.iter().collect();
        classify_fields(&class.class_variables, &writers)
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
            instance_facts("Object subclass: H\n  field: p = 0\n  setUp => self.p := Point new\n");
        assert_eq!(facts["p"], FieldTypeFact::Untyped);
        assert!(!numeric(&facts, "p") && !comparable(&facts, "p"));
    }

    #[test]
    fn value_copy_setter_on_self_is_an_in_class_write() {
        let facts = instance_facts(
            "TestCase subclass: T\n  field: origin = 0\n  field: n = 0\n  \
             setUp => (self withOrigin: Point new) withN: 3\n",
        );
        assert_eq!(facts["origin"], FieldTypeFact::Untyped);
        assert_eq!(facts["n"], FieldTypeFact::UntypedNumeric);
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

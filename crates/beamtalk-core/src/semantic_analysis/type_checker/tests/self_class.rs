// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Metaclass-aware type inference — Slice 1 (ADR 0083 / BT-2255).
//!
//! Covers `InferredType::Meta` representation, `Self class` / `X class`
//! resolution, type-driven class-side routing, `new`/`basicNew` on a metatype,
//! and the safety/negative cases that the precision increase must NOT regress
//! (abstract-class guard, genuinely-unknown reflection class objects, tower
//! subtyping, and no false-positive diagnostics on class values flowing through
//! variables).

use super::super::*;
use super::common::*;
use ecow::EcoString;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Lex + parse + build a hierarchy for a full source string.
fn parse_and_build(source: &str) -> (Module, ClassHierarchy) {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    (module, hierarchy)
}

/// Type-check `source` and return every does-not-understand (DNU) diagnostic.
fn dnu_diags(source: &str) -> Vec<EcoString> {
    let (module, hierarchy) = parse_and_build(source);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .map(|d| d.message.clone())
        .collect()
}

/// Infer the type of a bare expression with `self` bound to the metatype of
/// `class_name` (i.e. `self` is a *class object*). Mirrors a class-method body.
fn infer_with_self_meta(
    expr: &Expression,
    class_name: &str,
    hierarchy: &ClassHierarchy,
) -> InferredType {
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::meta(class_name));
    checker.infer_expr(expr, hierarchy, &mut env, false)
}

/// Infer the type of `recv class <selector>` style send where `recv` is a local
/// of inferred type `recv_ty`.
fn infer_send_on_local(
    local: &str,
    local_ty: InferredType,
    send: &Expression,
    hierarchy: &ClassHierarchy,
) -> InferredType {
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(local, local_ty);
    checker.infer_expr(send, hierarchy, &mut env, false)
}

// ---------------------------------------------------------------------------
// Representation: Meta variant basics
// ---------------------------------------------------------------------------

#[test]
fn meta_variant_is_name_only_and_displays_as_class() {
    // ADR 0068: the class object is unparameterized. `meta` ignores any type
    // args — there is structurally no place to put them.
    let m = InferredType::meta("List");
    assert_eq!(m.as_meta().map(EcoString::as_str), Some("List"));
    // `as_known` must be None — a metatype is NOT its instance class.
    assert!(m.as_known().is_none());
    assert_eq!(m.display_name().unwrap(), "List class");
}

#[test]
fn meta_equality_is_by_class_name_only() {
    assert_eq!(InferredType::meta("Counter"), InferredType::meta("Counter"));
    assert_ne!(InferredType::meta("Counter"), InferredType::meta("List"));
    // A metatype is never equal to its instance type.
    assert_ne!(
        InferredType::meta("Counter"),
        InferredType::known("Counter")
    );
}

// ---------------------------------------------------------------------------
// POSITIVE: annotation resolution (`Self class` / `X class`)
// ---------------------------------------------------------------------------

#[test]
fn class_of_annotation_resolves_to_metatype() {
    // `X class` (TypeAnnotation::ClassOf) resolves to Meta{X}.
    let ann = TypeAnnotation::ClassOf {
        class_name: ident("Counter"),
        span: span(),
    };
    let ty = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(ty.as_meta().map(EcoString::as_str), Some("Counter"));
}

#[test]
fn obj_class_returns_metatype_of_receiver() {
    // `obj class` flows through `Object>>class -> Self class`, which resolves to
    // the metatype of the *static receiver* class.
    let hierarchy = ClassHierarchy::with_builtins();
    let send = msg_send(int_lit(1), MessageSelector::Unary("class".into()), vec![]);
    let ty = infer_send_on_local("ignored", InferredType::known("Integer"), &send, &hierarchy);
    assert_eq!(
        ty.as_meta().map(EcoString::as_str),
        Some("Integer"),
        "`1 class` should be Meta{{Integer}}; got {ty:?}"
    );
}

#[test]
fn self_class_in_class_method_is_metatype_of_class() {
    // Inside a class method `self` is the class object; `self class` (well,
    // `self` already being a metatype) routing keeps it class-side.
    let hierarchy = ClassHierarchy::with_builtins();
    // `self name` where self :: Meta{Integer} resolves the class-side `name`
    // (lives on Behaviour) — returns Symbol, no DNU.
    let send = msg_send(var("self"), MessageSelector::Unary("name".into()), vec![]);
    let ty = infer_with_self_meta(&send, "Integer", &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Symbol"),
        "`self name` on a class object resolves Behaviour>>name -> Symbol; got {ty:?}"
    );
}

// ---------------------------------------------------------------------------
// POSITIVE: `aConcreteInstance class new` → the instance class
// ---------------------------------------------------------------------------

#[test]
fn instance_class_new_infers_the_instance_class() {
    // `aPoint class new` — `aPoint class` is Meta{Point}, `new` on a concrete
    // class metatype yields an instance of Point.
    let hierarchy = ClassHierarchy::with_builtins();
    // 1 class new  — Integer is concrete.
    let class_send = msg_send(int_lit(1), MessageSelector::Unary("class".into()), vec![]);
    let new_send = msg_send(class_send, MessageSelector::Unary("new".into()), vec![]);
    let ty = infer_send_on_local(
        "ignored",
        InferredType::known("Integer"),
        &new_send,
        &hierarchy,
    );
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "`1 class new` should infer an Integer instance; got {ty:?}"
    );
}

#[test]
fn metatype_basic_new_infers_the_instance_class() {
    // `basicNew` on a concrete-class metatype also yields an instance.
    let hierarchy = ClassHierarchy::with_builtins();
    let send = msg_send(
        var("self"),
        MessageSelector::Unary("basicNew".into()),
        vec![],
    );
    let ty = infer_with_self_meta(&send, "Integer", &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "`self basicNew` on Meta{{Integer}} should infer Integer; got {ty:?}"
    );
}

// ---------------------------------------------------------------------------
// POSITIVE: `obj class <selector>` resolves class-side
// ---------------------------------------------------------------------------

#[test]
fn class_value_through_variable_resolves_class_side() {
    // A class value stored in a local (typed Meta{Integer}) resolves a
    // class-side selector — `cls name` reads Behaviour>>name.
    let hierarchy = ClassHierarchy::with_builtins();
    let send = msg_send(var("cls"), MessageSelector::Unary("name".into()), vec![]);
    let ty = infer_send_on_local("cls", InferredType::meta("Integer"), &send, &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Symbol"),
        "class value `cls name` resolves Behaviour>>name -> Symbol; got {ty:?}"
    );
}

#[test]
fn binary_compare_on_class_value_is_boolean_not_dnu() {
    // `cls = Foo` / `x class = Integer` — binary comparisons on a class object
    // must NOT route to class-side DNU lookup (that broke `x class = Integer`
    // narrowing). They are universal value comparisons → Boolean.
    let hierarchy = ClassHierarchy::with_builtins();
    let send = msg_send(
        var("cls"),
        MessageSelector::Binary("=".into()),
        vec![class_ref("Integer")],
    );
    let ty = infer_send_on_local("cls", InferredType::meta("Integer"), &send, &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Boolean"),
        "`cls = Integer` on a metatype should be Boolean; got {ty:?}"
    );
}

// ---------------------------------------------------------------------------
// NEGATIVE / safety: abstract-class `new` must NOT become a concrete instance
// ---------------------------------------------------------------------------

#[test]
fn abstract_class_metatype_new_is_not_a_concrete_instance() {
    // `Collection` is abstract. `new` on Meta{Collection} must NOT bless a
    // concrete Collection instance — it falls back to Dynamic.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        hierarchy.is_abstract("Collection"),
        "precondition: Collection must be abstract"
    );
    let send = msg_send(var("self"), MessageSelector::Unary("new".into()), vec![]);
    let ty = infer_with_self_meta(&send, "Collection", &hierarchy);
    assert!(
        matches!(ty, InferredType::Dynamic(_)),
        "abstract `Collection` new must fall back to Dynamic, NOT Known(Collection); got {ty:?}"
    );
}

#[test]
fn abstract_behaviour_metatype_new_is_not_a_concrete_instance() {
    // `Behaviour` is the abstract root of the metaclass tower. A genuinely
    // unknown reflection class object whose metatype is Meta{Behaviour} must
    // not yield a concrete Behaviour instance from `new`.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        hierarchy.is_abstract("Behaviour"),
        "precondition: Behaviour must be abstract"
    );
    let send = msg_send(var("self"), MessageSelector::Unary("new".into()), vec![]);
    let ty = infer_with_self_meta(&send, "Behaviour", &hierarchy);
    assert!(
        matches!(ty, InferredType::Dynamic(_)),
        "abstract `Behaviour` new must fall back to Dynamic; got {ty:?}"
    );
}

#[test]
fn concrete_class_metatype_new_still_works_after_abstract_guard() {
    // The abstract guard must not over-fire: a concrete class metatype `new`
    // still yields an instance.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(!hierarchy.is_abstract("Integer"));
    let send = msg_send(var("self"), MessageSelector::Unary("new".into()), vec![]);
    let ty = infer_with_self_meta(&send, "Integer", &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "concrete `Integer` new still infers an Integer instance; got {ty:?}"
    );
}

// ---------------------------------------------------------------------------
// NEGATIVE / safety: genuinely-unknown reflection class object stays
// Object/Dynamic for `new` (instance type is statically unknown).
// ---------------------------------------------------------------------------

#[test]
fn behaviour_typed_reflection_value_new_stays_dynamic() {
    // The ADR reflection example: `allClasses first :: Behaviour`. `cls` is an
    // *instance* of Behaviour (a class object whose instance type is unknown).
    // `cls name` resolves class-side (Behaviour>>name), but `cls new` must stay
    // Dynamic/Object — the instance type is statically unknown.
    let hierarchy = ClassHierarchy::with_builtins();

    // Class-side resolution works: `cls name` -> Symbol (Behaviour>>name).
    let name_send = msg_send(var("cls"), MessageSelector::Unary("name".into()), vec![]);
    let name_ty = infer_send_on_local(
        "cls",
        InferredType::known("Behaviour"),
        &name_send,
        &hierarchy,
    );
    assert_eq!(
        name_ty.as_known().map(EcoString::as_str),
        Some("Symbol"),
        "`cls name` on a Behaviour value resolves class-side; got {name_ty:?}"
    );

    // Instance precision does NOT: `cls new` stays Dynamic (not a concrete type).
    let new_send = msg_send(var("cls"), MessageSelector::Unary("new".into()), vec![]);
    let new_ty = infer_send_on_local(
        "cls",
        InferredType::known("Behaviour"),
        &new_send,
        &hierarchy,
    );
    assert!(
        matches!(new_ty, InferredType::Dynamic(_)),
        "`cls new` on a genuinely-unknown Behaviour value must stay Dynamic; got {new_ty:?}"
    );
}

// ---------------------------------------------------------------------------
// NEGATIVE / safety: Meta{C} satisfies `:: Class` / `:: Behaviour` parameters
// ---------------------------------------------------------------------------

#[test]
fn metatype_satisfies_class_and_behaviour_parameters() {
    // A method taking `:: Behaviour` accepts a class value. Passing `x class`
    // (Meta) must not warn — Meta{C} <: Class <: Behaviour.
    let source = "
typed Object subclass: Registry
  register: cls :: Behaviour -> Object => cls
  use -> Object =>
    self register: 1 class
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "passing a class value to a `:: Behaviour` param must not warn; got {diags:?}"
    );

    // And the full type-check produces no Type-category warnings for the arg.
    let (module, hierarchy) = parse_and_build(source);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects Behaviour"))
        .map(|d| d.message.clone())
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "Meta{{Integer}} must satisfy a `:: Behaviour` parameter; got {arg_warnings:?}"
    );
}

#[test]
fn metatype_rejected_for_unrelated_concrete_parameter() {
    // The precision must be real: a class object does NOT satisfy `:: String`.
    // Passing `1 class` (Meta{Integer}) to a `:: String` param must warn, and
    // the diagnostic should name the metatype `Integer class`.
    let source = "
typed Object subclass: NeedsString
  take: s :: String -> Object => s
  use -> Object =>
    self take: 1 class
";
    let (module, hierarchy) = parse_and_build(source);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let mismatches: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects String"))
        .map(|d| d.message.clone())
        .collect();
    assert_eq!(
        mismatches.len(),
        1,
        "a class object must NOT satisfy a `:: String` param; got {mismatches:?}"
    );
    assert!(
        mismatches[0].contains("Integer class"),
        "diagnostic should name the metatype `Integer class`; got {:?}",
        mismatches[0]
    );
}

// ---------------------------------------------------------------------------
// NEGATIVE / safety: no false positives for class values flowing through
// variables — full-pipeline check, no DNU.
// ---------------------------------------------------------------------------

#[test]
fn class_value_flow_through_variable_no_false_dnu() {
    // A class value assigned to a local, then sent a class-side selector —
    // must type-check clean (no DNU). `name` lives on Behaviour.
    let source = "
typed Object subclass: Inspector
  describe -> Symbol =>
    cls := 1 class
    cls name
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "class value flowing through a variable must not produce a DNU; got {diags:?}"
    );
}

#[test]
fn species_withall_pattern_resolves_class_side_no_dnu() {
    // The exact `Collection>>collect:` shape (the stdlib change this ADR
    // enables): `species -> Self class`, plus a `class withAll:` constructor
    // returning `Self`. `self species withAll: result` must resolve the
    // class-side `withAll:` (no DNU) and type as the receiver's collection.
    let source = "
typed Object subclass: Bagly(E)
  class withAll: items :: List(E) -> Self => @primitive \"withAll:\"
  species -> Self class => self class
  build: items :: List(E) -> Self =>
    self species withAll: items
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "`self species withAll:` (species -> Self class) must resolve class-side \
         with NO @expect dnu; got {diags:?}"
    );

    // The `build:` body must satisfy its `-> Self` return — no return-type
    // mismatch warning (the species `withAll: -> Self` resolves to the class).
    let (module, hierarchy) = parse_and_build(source);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let ret_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type") && d.message.contains("'build:'"))
        .map(|d| d.message.clone())
        .collect();
    assert!(
        ret_warnings.is_empty(),
        "species `withAll:` body must satisfy `build: -> Self`; got {ret_warnings:?}"
    );
}

#[test]
fn species_pattern_style_resolves_class_side_no_dnu() {
    // The species pattern shape: a method returns `Self class`, and a caller
    // sends a class-side selector to it. `withAll:` lives class-side on
    // Collection subclasses; here we use the builtin `List class withAll:`
    // reached via a `Self class`-returning method.
    let source = "
typed Object subclass: SpeciesUser
  species -> Self class => self class
  build -> Object =>
    self species name
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "`self species name` (species returns Self class) must resolve class-side; got {diags:?}"
    );

    // And `self species` is declared `-> Self class` (a metatype, not Object).
    let (module, _hierarchy) = parse_and_build(source);
    let species_method = module.classes[0]
        .methods
        .iter()
        .find(|m| m.selector.name() == "species")
        .expect("species method");
    // The declared return annotation is `Self class`.
    assert!(
        matches!(
            species_method.return_type,
            Some(TypeAnnotation::SelfClass { .. })
        ),
        "species should declare `-> Self class`"
    );
}

// ---------------------------------------------------------------------------
// Implicit class-side `new` on concrete Object subclasses (subsumes the
// `SystemNavigation default` `@expect dnu`).
// ---------------------------------------------------------------------------

#[test]
fn implicit_new_on_sealed_object_subclass_no_dnu() {
    // A `sealed typed Object subclass` with no explicit class `new` — `self new`
    // in a class method must NOT warn (runtime supplies the implicit
    // instantiation path). This is the `SystemNavigation default` shape.
    let source = "
sealed typed Object subclass: Nav
  class default -> Nav => self new
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "implicit `new` on a sealed Object subclass must not warn; got {diags:?}"
    );
}

#[test]
fn implicit_new_on_object_subclass_via_class_literal_no_dnu() {
    // The same implicit-`new` suppression also applies to a class-literal
    // receiver `Nav new`.
    let source = "
sealed typed Object subclass: Nav
  class default -> Nav => self new

Nav new
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "`Nav new` (implicit constructor on Object subclass) must not warn; got {diags:?}"
    );
}

#[test]
fn new_on_actor_subclass_still_warns() {
    // Actors are spawned, not `new`'d — the implicit-`new` suppression must NOT
    // leak to Actor subclasses (drift-prevention pin).
    let source = "
Actor subclass: Worker
  state: v = 0
  go => self.v

Worker new
";
    let diags = dnu_diags(source);
    assert!(
        !diags.is_empty(),
        "`Worker new` on an Actor subclass must still warn (use spawn); got no diagnostics"
    );
}

// ---------------------------------------------------------------------------
// NEGATIVE / safety: `X class | Nil` state field (the SupervisionSpec shape,
// BT-2034) must not produce false DNU when its class-side selectors are sent.
// ---------------------------------------------------------------------------

#[test]
fn class_or_nil_state_field_no_false_dnu() {
    // Mirrors `SupervisionSpec>>actorClass :: Actor class | Nil`. Reading a
    // class-side selector off the field (after the runtime guarantees non-nil)
    // must not warn. The union member is now a metatype rather than Dynamic;
    // the union-send path treats it conservatively (no DNU).
    let source = "
typed Object subclass: Spec
  field: actorClass :: Actor class | Nil = nil
  describe -> Object =>
    self.actorClass isNil
      ifTrue: [#none]
      ifFalse: [self.actorClass name]
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "`Actor class | Nil` field's class-side send must not produce a false DNU; got {diags:?}"
    );
}

#[test]
fn class_or_nil_state_field_default_nil_no_type_warning() {
    // The `= nil` default on a `Actor class | Nil` field must remain valid —
    // `Meta{Actor} | Nil` includes Nil, so the default does not warn.
    let source = "
typed Object subclass: Spec
  field: actorClass :: Actor class | Nil = nil
  go -> Object => self.actorClass
";
    let (module, hierarchy) = parse_and_build(source);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declared as") || d.message.contains("Type mismatch"))
        .map(|d| d.message.clone())
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`= nil` default on `Actor class | Nil` field must not warn; got {type_warnings:?}"
    );
}

// ---------------------------------------------------------------------------
// Display / diagnostics rendering
// ---------------------------------------------------------------------------

#[test]
fn metatype_renders_as_source_spelling_in_diagnostics() {
    let m = InferredType::meta("Counter");
    assert_eq!(m.display_for_diagnostic().unwrap(), "Counter class");
    // nil metatype scrubs to `Nil class` in the source-friendly mode.
    let nil_meta = InferredType::meta("UndefinedObject");
    assert_eq!(nil_meta.display_for_diagnostic().unwrap(), "Nil class");
    // canonical (internal) keeps UndefinedObject.
    assert_eq!(nil_meta.display_name().unwrap(), "UndefinedObject class");
}

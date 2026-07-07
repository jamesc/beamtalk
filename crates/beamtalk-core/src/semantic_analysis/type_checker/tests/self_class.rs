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
    assert_eq!(m.display_name(), "List class");
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
    // `cls =:= Foo` / `x class =:= Integer` — binary comparisons on a class
    // object must NOT route to class-side DNU lookup (that broke
    // `x class =:= Integer` narrowing). They are universal value comparisons
    // → Boolean.
    let hierarchy = ClassHierarchy::with_builtins();
    let send = msg_send(
        var("cls"),
        MessageSelector::Binary("=:=".into()),
        vec![class_ref("Integer")],
    );
    let ty = infer_send_on_local("cls", InferredType::meta("Integer"), &send, &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Boolean"),
        "`cls =:= Integer` on a metatype should be Boolean; got {ty:?}"
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
    assert_eq!(nil_meta.display_name(), "UndefinedObject class");
}

// ---------------------------------------------------------------------------
// Regression: code-review findings on the metaclass-aware inference PR
// (BT-2255 / ADR 0083). See inference.rs `is_equality_comparison_op`,
// `class_object_tower_return`, `InferredType::meta`, and the union-send path.
// ---------------------------------------------------------------------------

/// Like `infer_send_on_local` but also returns the does-not-understand
/// diagnostics emitted during inference, so a test can assert that a send is
/// (or is NOT) silently DNU-suppressed.
fn infer_send_on_local_with_dnu(
    local: &str,
    local_ty: InferredType,
    send: &Expression,
    hierarchy: &ClassHierarchy,
) -> (InferredType, Vec<EcoString>) {
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(local, local_ty);
    let ty = checker.infer_expr(send, hierarchy, &mut env, false);
    let dnus = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .map(|d| d.message.clone())
        .collect();
    (ty, dnus)
}

#[test]
fn non_comparison_binary_on_class_value_is_not_boolean_and_not_dnu_suppressed() {
    // Finding 1: the binary fast-path must only fire for equality/identity
    // comparisons. `SomeClass + 1` on a `Meta{Integer}` receiver is NOT a
    // comparison — it must NOT type as Boolean, and it must NOT be silently
    // suppressed (it should fall through to normal class-side lookup and DNU,
    // since `Integer class` has no `+`).
    let hierarchy = ClassHierarchy::with_builtins();
    let send = msg_send(
        var("cls"),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    );
    let (ty, dnus) =
        infer_send_on_local_with_dnu("cls", InferredType::meta("Integer"), &send, &hierarchy);
    assert_ne!(
        ty.as_known().map(EcoString::as_str),
        Some("Boolean"),
        "`cls + 1` on a metatype must NOT infer Boolean (non-comparison binary); got {ty:?}"
    );
    assert!(
        !dnus.is_empty(),
        "`cls + 1` on `Integer class` must surface a DNU (the fast-path must not \
         swallow non-comparison binaries); got no DNU, type {ty:?}"
    );
}

#[test]
fn equality_binary_on_class_value_still_boolean() {
    // Finding 1 (companion): an equality send on a `Meta{C}` receiver still
    // infers Boolean via the (now-restricted) fast-path. Covers every
    // equality/identity operator in the canonical set.
    let hierarchy = ClassHierarchy::with_builtins();
    for op in ["==", "=:=", "/=", "=/="] {
        let send = msg_send(
            var("cls"),
            MessageSelector::Binary(op.into()),
            vec![class_ref("Float")],
        );
        let (ty, dnus) =
            infer_send_on_local_with_dnu("cls", InferredType::meta("Integer"), &send, &hierarchy);
        assert_eq!(
            ty.as_known().map(EcoString::as_str),
            Some("Boolean"),
            "`cls {op} Float` on a metatype should be Boolean; got {ty:?}"
        );
        assert!(
            dnus.is_empty(),
            "equality send `cls {op} Float` must not DNU; got {dnus:?}"
        );
    }
}

#[test]
fn meta_constructor_strips_type_argument_suffix() {
    // Finding 2: `InferredType::meta` is documented name-only. A name carrying
    // a `(...)` type-argument suffix must be stripped — `meta("Foo(Bar)")` is
    // `Meta{Foo}`, never `Meta{Foo(Bar)}`.
    let m = InferredType::meta("Foo(Bar)");
    assert_eq!(
        m.as_meta().map(EcoString::as_str),
        Some("Foo"),
        "`meta(\"Foo(Bar)\")` must strip the type-arg suffix to Meta{{Foo}}; got {m:?}"
    );
    // Multi-arg / nested suffixes strip at the first paren too.
    let nested = InferredType::meta("List(Pair(K, V))");
    assert_eq!(nested.as_meta().map(EcoString::as_str), Some("List"));
    // A plain name is unchanged.
    assert_eq!(
        InferredType::meta("Counter")
            .as_meta()
            .map(EcoString::as_str),
        Some("Counter")
    );
}

#[test]
fn tower_self_returning_method_preserves_metatype() {
    // Finding 3: a `Self`-returning tower identity method on a `Meta{C}`
    // receiver must preserve `Meta{C}` rather than collapsing to Dynamic.
    // `Object>>yourself -> Self`; `aClass yourself` is still the class object,
    // so it stays `Meta{Integer}` (and `aClass yourself new` keeps working).
    let hierarchy = ClassHierarchy::with_builtins();
    let send = msg_send(
        var("cls"),
        MessageSelector::Unary("yourself".into()),
        vec![],
    );
    let ty = infer_send_on_local("cls", InferredType::meta("Integer"), &send, &hierarchy);
    assert_eq!(
        ty.as_meta().map(EcoString::as_str),
        Some("Integer"),
        "`cls yourself` (tower Self-return) on Meta{{Integer}} must stay Meta{{Integer}}; got {ty:?}"
    );

    // And the metatype must remain chainable class-side: `cls yourself new`
    // resolves an Integer instance, proving Dynamic was not leaked.
    let chained = msg_send(send, MessageSelector::Unary("new".into()), vec![]);
    let chained_ty =
        infer_send_on_local("cls", InferredType::meta("Integer"), &chained, &hierarchy);
    assert_eq!(
        chained_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "`cls yourself new` must infer an Integer instance (Meta preserved); got {chained_ty:?}"
    );
}

#[test]
fn union_member_x_class_return_infers_metatype() {
    // Finding 4: a union-member method that explicitly returns `X class` must
    // resolve to `Meta{X}`, mirroring the non-union path. Without the fix the
    // ` class` suffix leaked through as `Known("Integer class")`.
    //
    // Two user classes both declare `metaOf -> Integer class` so the receiver
    // stays a genuine `Union` (a single-member union collapses).
    let source = "
typed Object subclass: Alpha
  metaOf -> Integer class => Integer
typed Object subclass: Beta
  metaOf -> Integer class => Integer
";
    let (_module, hierarchy) = parse_and_build(source);
    let union =
        InferredType::union_of(&[InferredType::known("Alpha"), InferredType::known("Beta")]);
    assert!(
        matches!(union, InferredType::Union { .. }),
        "precondition: receiver must be a Union; got {union:?}"
    );
    let send = msg_send(var("u"), MessageSelector::Unary("metaOf".into()), vec![]);
    let ty = infer_send_on_local("u", union, &send, &hierarchy);
    assert_eq!(
        ty.as_meta().map(EcoString::as_str),
        Some("Integer"),
        "union-member method returning `Integer class` must infer Meta{{Integer}}, \
         not Known(\"Integer class\"); got {ty:?}"
    );
}

// ---------------------------------------------------------------------------
// BT-2260: a *bare class literal* `Foo` infers `Meta{Foo}` so an unannotated
// class value routes class-side wherever it flows — through a variable, a
// collection, or as a class/behaviour argument — not just when used
// syntactically as a direct receiver (`Foo new`).
// ---------------------------------------------------------------------------

#[test]
fn bare_class_literal_infers_metatype() {
    // The core flip: `Expression::ClassReference` is the class *object*, typed
    // `Meta{C}` — NOT an instance of `C` (`Known(C)`).
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&class_ref("Integer"), &hierarchy, &mut env, false);
    assert_eq!(
        ty.as_meta().map(EcoString::as_str),
        Some("Integer"),
        "a bare class literal `Integer` must infer Meta{{Integer}}; got {ty:?}"
    );
    assert!(
        ty.as_known().is_none(),
        "a class literal is the class object, not an instance — `as_known` must be None; got {ty:?}"
    );
}

#[test]
fn class_literal_through_variable_then_new_infers_instance() {
    // AC: `klass := Foo. klass new` → an instance of `Foo`. The literal `Foo` is
    // now Meta{Foo}; storing it in a local keeps the metatype, so `klass new`
    // routes through the type-driven class-side path and yields a `Foo` instance.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // klass := Integer
    let assign_expr = assign("klass", class_ref("Integer"));
    let assigned_ty = checker.infer_expr(&assign_expr, &hierarchy, &mut env, false);
    assert_eq!(
        assigned_ty.as_meta().map(EcoString::as_str),
        Some("Integer"),
        "`klass := Integer` must bind klass to Meta{{Integer}}; got {assigned_ty:?}"
    );
    // klass new
    let new_send = msg_send(var("klass"), MessageSelector::Unary("new".into()), vec![]);
    let new_ty = checker.infer_expr(&new_send, &hierarchy, &mut env, false);
    assert_eq!(
        new_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "`klass new` (klass = a class literal) must infer an Integer instance; got {new_ty:?}"
    );
}

#[test]
fn class_literal_through_variable_resolves_class_selector() {
    // AC: `klass <classSelector>` resolves class-side when `klass` came from a
    // bare class literal. `name` lives on Behaviour (class-side) -> Symbol.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let assign_expr = assign("klass", class_ref("Integer"));
    checker.infer_expr(&assign_expr, &hierarchy, &mut env, false);
    let name_send = msg_send(var("klass"), MessageSelector::Unary("name".into()), vec![]);
    let ty = checker.infer_expr(&name_send, &hierarchy, &mut env, false);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Symbol"),
        "`klass name` (klass = a class literal) resolves Behaviour>>name -> Symbol; got {ty:?}"
    );
}

#[test]
fn class_literal_in_collection_routes_class_side_no_false_dnu() {
    // AC: class values in a collection route class-side. `#(Integer, String)` is
    // an untyped Array, so each element is still type-checked as Meta{C}; sending
    // a class-side selector to a class value pulled from the collection must not
    // produce a false DNU.
    let source = "
typed Object subclass: Registry
  describe -> Object =>
    #(Integer, String) do: [:c | c new]
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "class values in a collection (`#(Integer, String) do: [:c | c new]`) must not \
         produce a false DNU; got {diags:?}"
    );
}

#[test]
fn bare_class_literal_satisfies_class_and_behaviour_parameters() {
    // AC: a class literal passed to a `:: Class` / `:: Behaviour` parameter is
    // accepted. The bare literal is Meta{C}, and Meta{C} <: Class <: Behaviour.
    let source = "
typed Object subclass: Registry
  register: cls :: Behaviour -> Object => cls
  registerClass: cls :: Class -> Object => cls
  use -> Object =>
    self register: Integer
    self registerClass: String
";
    let diags = dnu_diags(source);
    assert!(
        diags.is_empty(),
        "passing bare class literals to `:: Behaviour` / `:: Class` params must not DNU; got {diags:?}"
    );
    let (module, hierarchy) = parse_and_build(source);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects Behaviour") || d.message.contains("expects Class"))
        .map(|d| d.message.clone())
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "bare class literals must satisfy `:: Behaviour` / `:: Class` params; got {arg_warnings:?}"
    );
}

#[test]
fn syntactic_class_literal_new_still_works() {
    // REGRESSION: the syntactic `Foo new` direct-literal path must keep working
    // (it dispatches via the `Expression::ClassReference` receiver arm, not the
    // type-driven Meta path). `Integer new` infers an Integer instance.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let new_send = msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("new".into()),
        vec![],
    );
    let ty = checker.infer_expr(&new_send, &hierarchy, &mut env, false);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "syntactic `Integer new` must still infer an Integer instance; got {ty:?}"
    );
}

#[test]
fn class_eq_literal_narrowing_not_regressed() {
    // REGRESSION (critical): `x class =:= Foo` narrowing must still narrow `x`
    // to a `Foo` instance in the true branch, even though `Foo` is now
    // Meta{Foo} and the `=:= Foo` comparison receiver `x class` is Meta{X}.
    // The narrowing rule is AST-driven (class_eq.rs), so it builds Known(Foo)
    // from the literal node — and the `Meta{X} =:= ...` comparison must stay
    // Boolean (no DNU on the guard).
    //
    // Built as AST (mirrors the pinned narrowing tests) so the guard
    // `x class =:= Integer` is exercised.
    let hierarchy = ClassHierarchy::with_builtins();
    // process: x :: Object =>
    //   (x class =:= Integer) ifTrue: [x isEven] ifFalse: [x printString]
    // `isEven` is an Integer-only selector — it must resolve in the true branch
    // (x narrowed to Integer), confirming the literal-eq narrowing still fires.
    let guard = msg_send(
        msg_send(var("x"), MessageSelector::Unary("class".into()), vec![]),
        MessageSelector::Binary("=:=".into()),
        vec![class_ref("Integer")],
    );
    let process_method = make_keyword_method(
        &["process:"],
        vec![("x", Some("Object"))],
        vec![if_true_if_false(
            guard,
            block_expr(vec![msg_send(
                var("x"),
                MessageSelector::Unary("isEven".into()),
                vec![],
            )]),
            block_expr(vec![msg_send(
                var("x"),
                MessageSelector::Unary("printString".into()),
                vec![],
            )]),
        )],
    );
    let class = ClassDefinition::new(
        ident("Probe"),
        ident("Object"),
        vec![],
        vec![process_method],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .map(|d| d.message.clone())
        .collect();
    assert!(
        dnu.is_empty(),
        "`x class =:= Integer` literal-eq narrowing must still narrow x to Integer \
         (so `x isEven` resolves) and the guard must not DNU; got {dnu:?}"
    );
}

// ---------------------------------------------------------------------------
// BT-2256 (ADR 0083 Slice 2): class-side `Self`-return precision for *concrete*
// class-literal / concrete-metatype receivers. A class-side constructor sent to
// a concrete class literal infers *that* class's instance type (`Set withAll: →
// Set`, `List withAll: → List`, `Array withAll: → Array`), composing the element
// type from the argument where statically known (`Set withAll: aList(Integer) →
// Set(Integer)`). Abstract receivers (`Collection`) keep Slice 1 behaviour.
// ---------------------------------------------------------------------------

/// Build a `#(...)` array literal of integer elements.
fn int_array(ns: &[i64]) -> Expression {
    Expression::ArrayLiteral {
        elements: ns.iter().map(|n| int_lit(*n)).collect(),
        span: span(),
    }
}

/// Build a keyword send `recv kw: arg`.
fn kw1_send(receiver: Expression, kw: &str, arg: Expression) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![KeywordPart::new(kw, span())]),
        vec![arg],
    )
}

/// A `List(elem)`-typed local value (the typed-argument shape for composition).
fn list_of(elem: &str) -> InferredType {
    InferredType::Known {
        class_name: "List".into(),
        type_args: vec![InferredType::known(elem)],
        provenance: super::super::TypeProvenance::Inferred(span()),
    }
}

#[test]
fn concrete_collection_withall_infers_concrete_class() {
    // AC: `Set withAll:` → Set, `List withAll:` → List, `Array withAll:` → Array.
    // Each concrete collection's class-side `withAll:` constructor, sent to the
    // bare class literal, infers the *concrete* receiver class — never the
    // abstract `Collection`.
    let hierarchy = ClassHierarchy::with_builtins();
    for cls in ["Set", "List", "Array", "Bag"] {
        let send = kw1_send(class_ref(cls), "withAll:", int_array(&[1, 2, 3]));
        let ty = infer_send_on_local("ignored", InferredType::known("Object"), &send, &hierarchy);
        assert_eq!(
            ty.as_known().map(EcoString::as_str),
            Some(cls),
            "`{cls} withAll: #(1,2,3)` must infer {cls}, not Collection; got {ty:?}"
        );
    }
}

#[test]
fn concrete_collection_withall_on_metatype_infers_concrete_class() {
    // AC: same precision when the receiver is a *metatype* of a concrete class
    // (a class value flowing through a variable), not only a bare literal.
    // `(Set class)`-typed value `withAll:` → Set.
    let hierarchy = ClassHierarchy::with_builtins();
    for cls in ["Set", "List", "Array"] {
        let send = kw1_send(var("cls"), "withAll:", int_array(&[1, 2]));
        let ty = infer_send_on_local("cls", InferredType::meta(cls), &send, &hierarchy);
        assert_eq!(
            ty.as_known().map(EcoString::as_str),
            Some(cls),
            "`(Meta{{{cls}}}) withAll:` must infer {cls}; got {ty:?}"
        );
    }
}

#[test]
fn abstract_collection_withall_stays_abstract_no_concrete_inference() {
    // AC: an *abstract* receiver keeps Slice 1 behaviour — `Self` resolves to the
    // abstract class itself (Collection), NOT some false concrete subclass. The
    // result must be the abstract class name (so downstream `Self`-typed chaining
    // type-checks), never a concrete collection.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(hierarchy.is_abstract("Collection"));
    let send = kw1_send(class_ref("Collection"), "withAll:", int_array(&[1]));
    let ty = infer_send_on_local("ignored", InferredType::known("Object"), &send, &hierarchy);
    // Collection>>withAll: is declared `-> Self`; on the abstract Collection
    // literal it resolves to Collection — never a concrete species.
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Collection"),
        "abstract `Collection withAll:` must stay Collection (Slice 1 behaviour), \
         not over-resolve to a concrete species; got {ty:?}"
    );
    assert!(
        !matches!(
            ty.as_known().map(EcoString::as_str),
            Some("Set" | "List" | "Array" | "Bag")
        ),
        "abstract receiver must never infer a concrete collection; got {ty:?}"
    );
}

#[test]
fn concrete_withall_composes_known_element_type() {
    // AC: composes with ADR 0068 element-type inference — `Set withAll:
    // aList(Integer)` infers `Set(Integer)` where the element type is statically
    // known. The element flows from the `List(E)`-shaped parameter, recovered by
    // class-side nested unification (BT-2256).
    let hierarchy = ClassHierarchy::with_builtins();
    for cls in ["Set", "List", "Array"] {
        let send = kw1_send(class_ref(cls), "withAll:", var("items"));
        let ty = infer_send_on_local("items", list_of("Integer"), &send, &hierarchy);
        let known = ty.as_known();
        assert_eq!(
            known.map(EcoString::as_str),
            Some(cls),
            "`{cls} withAll: items(List(Integer))` must infer {cls}; got {ty:?}"
        );
        // The element type composes to Integer (not erased to Dynamic).
        let elem = match &ty {
            InferredType::Known { type_args, .. } => type_args.first(),
            _ => None,
        };
        assert_eq!(
            elem.and_then(InferredType::as_known).map(EcoString::as_str),
            Some("Integer"),
            "`{cls} withAll: items(List(Integer))` must compose to {cls}(Integer); got {ty:?}"
        );
    }
}

#[test]
fn inherited_class_side_self_return_resolves_to_concrete_subclass() {
    // The genuine Slice 2 `-> Self` mechanism: a concrete subclass that
    // *inherits* (does not override) a class-side `-> Self` constructor from an
    // abstract parent. Sent to the concrete class literal, `Self` resolves to the
    // *concrete* subclass — not the abstract definition site.
    let source = "
typed Object subclass: AbstractMaker(E)
  class make: items :: List(E) -> Self => @primitive \"make:\"

typed AbstractMaker subclass: ConcreteMaker(E)
";
    let (_module, hierarchy) = parse_and_build(source);
    let send = kw1_send(class_ref("ConcreteMaker"), "make:", var("items"));
    let ty = infer_send_on_local("items", list_of("Integer"), &send, &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("ConcreteMaker"),
        "inherited class-side `-> Self` on a concrete subclass must resolve Self to \
         the concrete subclass (ConcreteMaker), not the abstract parent; got {ty:?}"
    );
    // And it composes the inherited generic element type from the argument.
    let elem = match &ty {
        InferredType::Known { type_args, .. } => type_args.first(),
        _ => None,
    };
    assert_eq!(
        elem.and_then(InferredType::as_known).map(EcoString::as_str),
        Some("Integer"),
        "inherited `-> Self` must compose ConcreteMaker(Integer) from List(Integer); got {ty:?}"
    );
}

#[test]
fn class_side_self_return_on_defining_class_literal_resolves_to_itself() {
    // Slice 1 boundary (no regression): sending a `-> Self` class-side method to
    // the *defining* class literal resolves Self to that class itself — not a
    // subclass, and not Dynamic. (The parent here is a plain base class, not
    // marked `abstract`; this pins the receiver=defining-class case.)
    let source = "
typed Object subclass: BaseMaker(E)
  class make: items :: List(E) -> Self => @primitive \"make:\"

typed BaseMaker subclass: ConcreteMaker(E)
";
    let (_module, hierarchy) = parse_and_build(source);
    let send = kw1_send(class_ref("BaseMaker"), "make:", var("items"));
    let ty = infer_send_on_local("items", list_of("Integer"), &send, &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("BaseMaker"),
        "class-side `-> Self` on the defining class literal resolves to that class; got {ty:?}"
    );
}

#[test]
fn concrete_withall_unknown_element_type_does_not_overclaim() {
    // Safety: when the element type is NOT statically known (untyped array
    // literal `#(1, 2)`), composition must not invent a concrete element — the
    // class is still concrete (`Set`) but the element stays Dynamic. This pins
    // that the nested-unification fix only fires on a typed, base-matching arg.
    let hierarchy = ClassHierarchy::with_builtins();
    let send = kw1_send(class_ref("Set"), "withAll:", int_array(&[1, 2]));
    let ty = infer_send_on_local("ignored", InferredType::known("Object"), &send, &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Set"),
        "`Set withAll: #(1,2)` is still a Set; got {ty:?}"
    );
    let elem = match &ty {
        InferredType::Known { type_args, .. } => type_args.first().cloned(),
        _ => None,
    };
    assert!(
        matches!(elem, Some(InferredType::Dynamic(_))),
        "untyped `#(1,2)` arg must leave the element type Dynamic, not over-claim; got {elem:?}"
    );
}

#[test]
fn nested_class_param_composition_recurses() {
    // The nested-unification fix recurses into deeper generic shapes. A
    // class-side `from: :: List(Pair(K, V)) -> Self` on `Maply(K, V)` must
    // recover BOTH K and V from a `List(Pair(Symbol, Integer))` argument.
    let source = "
typed Object subclass: Pairly(A, B)
typed Object subclass: Maply(K, V)
  class from: entries :: List(Pairly(K, V)) -> Self => @primitive \"from:\"
";
    let (_module, hierarchy) = parse_and_build(source);
    let arg_ty = InferredType::Known {
        class_name: "List".into(),
        type_args: vec![InferredType::Known {
            class_name: "Pairly".into(),
            type_args: vec![
                InferredType::known("Symbol"),
                InferredType::known("Integer"),
            ],
            provenance: super::super::TypeProvenance::Inferred(span()),
        }],
        provenance: super::super::TypeProvenance::Inferred(span()),
    };
    let send = kw1_send(class_ref("Maply"), "from:", var("entries"));
    let ty = infer_send_on_local("entries", arg_ty, &send, &hierarchy);
    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Maply"),
        "`Maply from:` must infer Maply; got {ty:?}"
    );
    let args = match &ty {
        InferredType::Known { type_args, .. } => type_args.clone(),
        _ => vec![],
    };
    let names: Vec<Option<&str>> = args
        .iter()
        .map(|a| a.as_known().map(EcoString::as_str))
        .collect();
    assert_eq!(
        names,
        vec![Some("Symbol"), Some("Integer")],
        "nested unification must recover both K=Symbol and V=Integer; got {ty:?}"
    );
}

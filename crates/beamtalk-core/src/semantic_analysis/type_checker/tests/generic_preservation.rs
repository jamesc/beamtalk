// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generic return `type_args` preservation on class- and instance-method assignment (BT-2018, BT-2019).

use super::super::*;
use super::common::*;

// ---- BT-2018: Generic return type args preserved on class-method assignment ----
//
// When a class method declares a parameterised return type like
// `Result(List(String), Error)`, callers that bind the result to a local
// must see the full nested type — not a flattened `Known("Result(...)")`.
// Pre-fix the call site dropped the inner generics, which made downstream
// `result unwrap` resolve to `Dynamic` and cascaded into block-parameter
// inference (every `[:f | ...]` after that became Dynamic).
//
// These tests cover the three reproducer variants from the issue plus a
// chained-assignment case and an instance-method case.

/// BT-2018 (a): explicit annotation on the LHS already worked pre-fix —
/// keep it as a baseline so a regression here is loud.
#[test]
fn class_method_return_with_explicit_annotation_preserves_type_args() {
    let source = "
typed Object subclass: Box
  class wrap: v :: List(String) -> Result(List(String), Error) =>
    Result ok: v

typed Object subclass: Driver
  probe -> Nil =>
    r :: Result(List(String), Error) := Box wrap: #()
    r
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe")
        .expect("probe method");
    // Last expression is `r` — its type should be Result(List(String), Error).
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Driver"));
    let _ = checker.infer_expr(&probe.body[0].expression, &hierarchy, &mut env, false);
    let last_stmt = probe.body.last().expect("probe body non-empty");
    let r_ty = checker.infer_expr(&last_stmt.expression, &hierarchy, &mut env, false);
    assert_full_result_list_string_error(&r_ty);
}

/// BT-2018 (b): the bug — RHS is a class-method send returning the same
/// concrete generic. Pre-fix, `r` lost its inner `type_args` and
/// `r unwrap` collapsed to `Dynamic`.
#[test]
fn class_method_return_preserves_nested_type_args_on_assignment() {
    let source = "
typed Object subclass: Box
  class wrap: v :: List(String) -> Result(List(String), Error) =>
    Result ok: v

typed Object subclass: Driver
  probe -> Nil =>
    r := Box wrap: #()
    r
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe")
        .expect("probe method");
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Driver"));
    let _ = checker.infer_expr(&probe.body[0].expression, &hierarchy, &mut env, false);
    let r_ty = checker.infer_expr(
        &probe.body.last().expect("probe body non-empty").expression,
        &hierarchy,
        &mut env,
        false,
    );
    assert_full_result_list_string_error(&r_ty);
}

/// BT-2018 chained: `a := ClassMethod`, `b := a instanceMethod` — type args
/// on `a` must flow through to `b` so `b` resolves to the concrete type.
#[test]
fn class_method_return_type_args_flow_through_chained_assignment() {
    let source = "
typed Object subclass: Box
  class wrap: v :: List(String) -> Result(List(String), Error) =>
    Result ok: v

typed Object subclass: Driver
  probe -> Nil =>
    r := Box wrap: #()
    files := r unwrap
    files frobnicate
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Pre-fix: `files` was Dynamic, so `frobnicate` was silently accepted.
    // Post-fix: `files` is `List(String)` and `List` does not understand
    // `frobnicate`, so we expect a DNU warning. (We assert *some* DNU
    // warning naming `frobnicate` rather than pinning the exact wording —
    // the receiver class hint format may evolve.)
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand") && d.message.contains("frobnicate"))
        .collect();
    assert!(
        !dnu.is_empty(),
        "expected `frobnicate` DNU after chained assignment from class-method \
         result; diagnostics: {:?}",
        checker.diagnostics()
    );
}

// BT-2018 instance-method case via chained assignment: the chained-flow
// test above (`class_method_return_type_args_flow_through_chained_assignment`)
// already exercises the instance-method substitution path: after
// `r := Box wrap: #()` gives `r :: Result(List(String), Error)`, the
// `r unwrap` send is an instance-method call whose return type `T`
// substitutes through to `List(String)`. The DNU on `files frobnicate`
// proves the instance-method substitution preserved the generics.
// (Pure instance-method *concrete* return-type preservation — e.g.
// `-> List(String)` on a non-generic receiver — is the sibling BT-2019,
// not in scope here.)

/// Helper: assert the type is the fully-nested
/// `Known("Result", [Known("List", [Known("String")]), Known("Error")])`.
fn assert_full_result_list_string_error(ty: &InferredType) {
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = ty
    else {
        panic!("expected Known(Result, ...), got {ty:?}");
    };
    assert_eq!(
        class_name.as_str(),
        "Result",
        "outer class should be Result, got {class_name}"
    );
    assert_eq!(
        type_args.len(),
        2,
        "Result should have 2 type args, got {type_args:?}"
    );
    let InferredType::Known {
        class_name: inner_name,
        type_args: inner_args,
        ..
    } = &type_args[0]
    else {
        panic!(
            "expected first type_arg to be Known(List, [String]), got {:?}",
            type_args[0]
        );
    };
    assert_eq!(
        inner_name.as_str(),
        "List",
        "first type_arg should be List, got {inner_name}"
    );
    assert_eq!(
        inner_args.len(),
        1,
        "List should have 1 type_arg, got {inner_args:?}"
    );
    assert_eq!(
        inner_args[0].as_known().map(EcoString::as_str),
        Some("String"),
        "List inner type_arg should be String, got {:?}",
        inner_args[0]
    );
    assert_eq!(
        type_args[1].as_known().map(EcoString::as_str),
        Some("Error"),
        "second type_arg should be Error, got {:?}",
        type_args[1]
    );
}

// ---- BT-2019: Concrete parametric return types preserve type_args on
// instance-method sends from non-generic receivers ----
//
// When a non-generic class declares an instance or class method returning a
// concrete parametric type such as `-> List(String)` or `-> List(MyThing)`,
// the call-site result must preserve the inner type arguments so downstream
// generic resolution (`first`, `do:`, etc.) sees the element type.
//
// Pre-fix the path in `inference.rs` that handled instance-method return
// types stripped the `(...)` portion off, so callers saw `Known("List", [])`
// instead of `Known("List", [Known("MyThing")])`. Every downstream send then
// fell back to Object/Dynamic.

/// BT-2019 (b): instance method on a non-generic receiver returning a
/// concrete parametric type. The call-site result must preserve the
/// inner element type so `events first` resolves to `MyThing`, not bare
/// `Object`/`Dynamic`.
#[test]
fn instance_method_concrete_parametric_return_preserves_type_args() {
    let source = "
typed Value subclass: MyThing
  field: v :: Integer = 0

typed Object subclass: Store
  readEvents -> List(MyThing) =>
    #()

typed Object subclass: Driver
  probe: store :: Store -> Nil =>
    events := store readEvents
    events
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe:")
        .expect("probe: method");

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Driver"));
    env.set_local("store", InferredType::known("Store"));
    // Walk the body so each binding is recorded in env.
    for stmt in &probe.body {
        let _ = checker.infer_expr(&stmt.expression, &hierarchy, &mut env, false);
    }
    let events_ty = checker.infer_expr(
        &probe.body.last().expect("probe body non-empty").expression,
        &hierarchy,
        &mut env,
        false,
    );
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = events_ty
    else {
        panic!("expected Known(List, [MyThing]), got {events_ty:?}");
    };
    assert_eq!(class_name.as_str(), "List");
    assert_eq!(
        type_args.len(),
        1,
        "List should preserve its element type arg, got {type_args:?}"
    );
    assert_eq!(
        type_args[0].as_known().map(EcoString::as_str),
        Some("MyThing"),
        "List element type should be MyThing, got {:?}",
        type_args[0]
    );
}

/// BT-2019: end-to-end DNU check. After the fix, sending a non-existent
/// selector to the element of a `List(MyThing)` returned by an instance
/// method must produce a "`MyThing` does not understand 'xyzzyNonsense'"
/// warning. Pre-fix the warning was silently lost (element type was
/// Dynamic).
#[test]
fn instance_method_concrete_parametric_return_drives_element_dnu() {
    let source = "
typed Value subclass: MyThing
  field: v :: Integer = 0

typed Object subclass: Store
  readEvents -> List(MyThing) =>
    #()

typed Object subclass: Driver
  probe: store :: Store -> Nil =>
    events := store readEvents
    first := events first
    first xyzzyNonsense
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("does not understand") && d.message.contains("xyzzyNonsense")
        })
        .collect();
    assert!(
        !dnu.is_empty(),
        "expected `xyzzyNonsense` DNU on MyThing element from List(MyThing) \
         instance-method return; diagnostics: {:?}",
        checker.diagnostics()
    );
}

/// BT-2019: class-method case. `ExduraSupervisor class >> children -> List(Actor)`
/// — the call-site result of `Sup children` must be `List(Actor)`, not
/// bare `List`.
#[test]
fn class_method_concrete_parametric_return_preserves_type_args() {
    let source = "
typed Object subclass: Actor
  field: name :: String = \"\"

typed Object subclass: Sup
  class children -> List(Actor) =>
    #()

typed Object subclass: Driver
  probe -> Nil =>
    kids := Sup children
    kids
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe")
        .expect("probe method");

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Driver"));
    for stmt in &probe.body {
        let _ = checker.infer_expr(&stmt.expression, &hierarchy, &mut env, false);
    }
    let kids_ty = checker.infer_expr(
        &probe.body.last().expect("probe body non-empty").expression,
        &hierarchy,
        &mut env,
        false,
    );
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = kids_ty
    else {
        panic!("expected Known(List, [Actor]), got {kids_ty:?}");
    };
    assert_eq!(class_name.as_str(), "List");
    assert_eq!(
        type_args.len(),
        1,
        "List should preserve its element type arg"
    );
    assert_eq!(
        type_args[0].as_known().map(EcoString::as_str),
        Some("Actor")
    );
}

/// BT-2019: Dictionary(K, V) — two type args must be preserved.
#[test]
fn instance_method_dictionary_return_preserves_both_type_args() {
    let source = "
typed Object subclass: Store
  readMap -> Dictionary(String, Integer) =>
    Dictionary new

typed Object subclass: Driver
  probe: store :: Store -> Nil =>
    m := store readMap
    m
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe:")
        .expect("probe: method");

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Driver"));
    env.set_local("store", InferredType::known("Store"));
    for stmt in &probe.body {
        let _ = checker.infer_expr(&stmt.expression, &hierarchy, &mut env, false);
    }
    let m_ty = checker.infer_expr(
        &probe.body.last().expect("probe body non-empty").expression,
        &hierarchy,
        &mut env,
        false,
    );
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = m_ty
    else {
        panic!("expected Known(Dictionary, [String, Integer]), got {m_ty:?}");
    };
    assert_eq!(class_name.as_str(), "Dictionary");
    assert_eq!(type_args.len(), 2);
    assert_eq!(
        type_args[0].as_known().map(EcoString::as_str),
        Some("String")
    );
    assert_eq!(
        type_args[1].as_known().map(EcoString::as_str),
        Some("Integer")
    );
}

/// BT-2019: Set(T) — single-arg generic, instance-method return.
#[test]
fn instance_method_set_return_preserves_type_arg() {
    let source = "
typed Object subclass: Store
  readSet -> Set(String) =>
    Set new

typed Object subclass: Driver
  probe: store :: Store -> Nil =>
    s := store readSet
    s
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe:")
        .expect("probe: method");

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Driver"));
    env.set_local("store", InferredType::known("Store"));
    for stmt in &probe.body {
        let _ = checker.infer_expr(&stmt.expression, &hierarchy, &mut env, false);
    }
    let s_ty = checker.infer_expr(
        &probe.body.last().expect("probe body non-empty").expression,
        &hierarchy,
        &mut env,
        false,
    );
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = s_ty
    else {
        panic!("expected Known(Set, [String]), got {s_ty:?}");
    };
    assert_eq!(class_name.as_str(), "Set");
    assert_eq!(type_args.len(), 1);
    assert_eq!(
        type_args[0].as_known().map(EcoString::as_str),
        Some("String")
    );
}

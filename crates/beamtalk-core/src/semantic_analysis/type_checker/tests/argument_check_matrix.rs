// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! a selector-kind x `InferredType`-shape coverage matrix for
//! argument-type checking.
//!
//! Six independent bugs shared the same shape: a specific validation
//! path (binary sends, cascades, FFI calls, nested generic args, class-body
//! defaults) assumed a "broader"/"more specific" check elsewhere covered
//! `Union`/`Dynamic` argument shapes, when in fact that other check only
//! handled `InferredType::Known`. Each fix landed with its own narrow
//! regression test (see `bt2843_binary_arg_union_check.rs`,
//! `bt2871_cascade_binary_arg_check.rs`, `bt2847_nested_union_type_args.rs`,
//! the continuation-message tests in `arg_return_checking.rs`, and the Union
//! tests in `ffi.rs`), but nothing forced the *same* coverage matrix onto
//! every message-send shape uniformly — which is exactly why these gaps went
//! unnoticed for as long as they did.
//!
//! This module is that matrix. It parameterizes over:
//!
//! - **Send shape**: `Unary`, `Binary`, `Keyword`, `Cascade-first`,
//!   `Cascade-continuation` (keyword and binary selectors on an instance
//!   receiver are genuinely different code paths from the class-ref and
//!   self-in-class-method receivers, since the continuation loop's three
//!   dispatch branches are independent), and `FFI call`.
//! - **Argument inferred-type shape**: `Known-compatible`,
//!   `Known-incompatible`, `Union-all-compatible`,
//!   `Union-with-one-incompatible-member`, and `Dynamic`.
//!
//! Expected outcomes are derived from the *declared contract* (a parameter
//! typed `String` must never silently accept an incompatible argument), not
//! from today's behavior — every "incompatible" cell must produce exactly
//! one `Type`-category diagnostic containing `"expects"`, and every
//! "compatible"/`Dynamic` cell must produce none. This is additive coverage;
//! the existing narrower regression tests are left in place.
//!
//! Verified with an empirical revert-and-check: temporarily undoing the
//! cascade-continuation-binary fix in `inference.rs` made exactly the two
//! cells that claim to cover it fail, confirming this matrix has teeth rather
//! than just re-asserting the status quo. The cascade-continuation loop's
//! three dispatch branches (class-ref, self-in-class-method, and
//! instance/binary "else" — `inference.rs`'s `for msg in messages` loop,
//! ~lines 716-818) are each covered by their own row of cells.

use super::common::*;

/// Shared fixture: a `typed Value` class with one method per selector kind
/// under test. `takeStr:` / `++` declare a `String` parameter (used for the
/// `Known-compatible`, `Known-incompatible`, `Dynamic`, and
/// `Union-with-one-incompatible-member` cells); `takeObj:` / `+` declare an
/// `Object` parameter (used for the `Union-all-compatible` cells, since a
/// `String | Symbol` union is only uniformly compatible against a supertype
/// wide enough to accept every member). `second:` / `secondObj:` are used as
/// the *continuation* message in cascade tests, distinct from the first
/// message so the two code paths (`infer_message_send_with_receiver_ty` vs.
/// the cascade continuation loop) can be exercised independently.
///
/// `classTakeStr:` / `classTakeObj:` / `classSecond:` / `classSecondObj:`
/// mirror the four instance-side methods above but are declared `class`-side,
/// for the cascade-continuation-class-ref and
/// cascade-continuation-self-in-class-method cells — the two dispatch
/// branches in the continuation loop (`inference.rs`'s `for msg in messages`,
/// ~lines 735-754 and ~755-775) the instance-receiver cells above never
/// exercise.
fn fixture(extra_class_method: &str) -> String {
    format!(
        r"typed Value subclass: Thing
  ++ other :: String -> String => other
  + other :: Object -> Object => other
  takeStr: aString :: String -> String => aString
  takeObj: anObject :: Object -> Object => anObject
  second: aString :: String -> String => aString
  secondObj: anObject :: Object -> Object => anObject
  class classTakeStr: aString :: String -> String => aString
  class classTakeObj: anObject :: Object -> Object => anObject
  class classSecond: aString :: String -> String => aString
  class classSecondObj: anObject :: Object -> Object => anObject

{extra_class_method}
"
    )
}

/// Argument-type mismatch diagnostics — the `Type`-category diagnostics
/// `check_argument_types`/`check_binary_operand_types`/
/// `check_ffi_argument_types` push, all of which share the
/// `"Argument N of '<selector>' ... expects <T>, got <U>"` (or, for
/// `check_binary_operand_types`'s bespoke arithmetic/concat wording,
/// `"... expects a numeric/String argument, got <U>"`) message shape.
fn expects_diags(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type) && d.message.contains("expects"))
        .collect()
}

fn assert_incompatible(diags: &[Diagnostic], cell: &str) {
    let hits = expects_diags(diags);
    assert_eq!(
        hits.len(),
        1,
        "[{cell}] expected exactly one argument-type diagnostic, got: {diags:#?}"
    );
}

fn assert_compatible(diags: &[Diagnostic], cell: &str) {
    let hits = expects_diags(diags);
    assert!(
        hits.is_empty(),
        "[{cell}] expected no argument-type diagnostic, got: {hits:#?}"
    );
}

// ---------------------------------------------------------------------------
// Unary — a unary selector has no argument position at all, so
// `check_argument_types` early-returns via `method.param_types.is_empty()`
// before it ever inspects an argument shape. This is the matrix's trivial
// "control" row: no matter what an (artificial, since real unary sends can't
// carry one) argument shape would be, a 0-param method must never produce an
// argument-type diagnostic. Exercised directly against `check_argument_types`
// since real Beamtalk syntax has no way to attach an argument to a unary
// send.
// ---------------------------------------------------------------------------

fn unary_hierarchy() -> ClassHierarchy {
    let module = parse_source("typed Value subclass: Thing\n  probe -> String => \"x\"\n");
    ClassHierarchy::build(&module).0
}

fn assert_unary_cell(cell: &str, arg_ty: InferredType) {
    let hierarchy = unary_hierarchy();
    let checker = check_args("Thing", "probe", &[arg_ty], &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "[{cell}] a 0-param (unary) method must never produce an argument-type \
         diagnostic, got: {:?}",
        checker.diagnostics()
    );
}

/// The unary row: `(cell, argument shape)`.
fn unary_cells() -> Vec<(&'static str, InferredType)> {
    vec![
        ("unary/known-compatible", InferredType::known("String")),
        ("unary/known-incompatible", InferredType::known("Integer")),
        (
            "unary/union-all-compatible",
            InferredType::simple_union(&["String", "Symbol"]),
        ),
        (
            "unary/union-incompatible-member",
            InferredType::simple_union(&["String", "Nil"]),
        ),
        (
            "unary/dynamic",
            InferredType::Dynamic(DynamicReason::Unknown),
        ),
    ]
}

#[test]
fn matrix_unary_cells() {
    for (cell, arg_ty) in unary_cells() {
        assert_unary_cell(cell, arg_ty);
    }
}

// ---------------------------------------------------------------------------
// Binary — `c ++ <arg>` / `c + <arg>` (non-cascade).
// ---------------------------------------------------------------------------

/// How an argument-type check must come out for a cell.
#[derive(Clone, Copy, Debug)]
enum Expect {
    /// No argument-type diagnostic.
    Compatible,
    /// Exactly one argument-type diagnostic.
    Incompatible,
}

fn assert_expect(diags: &[Diagnostic], cell: &str, expect: Expect) {
    match expect {
        Expect::Compatible => assert_compatible(diags, cell),
        Expect::Incompatible => assert_incompatible(diags, cell),
    }
}

/// The source-based cells: `(cell, class-side method for the fixture, outcome)`.
/// Each row's failure names the cell (`<send shape>/<argument shape>`).
const SOURCE_CELLS: &[(&str, &str, Expect)] = &[
    (
        "binary/known-compatible",
        "  class run -> Nil =>\n    c := self new\n    c ++ \"ok\"\n    nil",
        Expect::Compatible,
    ),
    (
        "binary/known-incompatible",
        "  class run -> Nil =>\n    c := self new\n    c ++ 42\n    nil",
        Expect::Incompatible,
    ),
    (
        "binary/union-all-compatible",
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c + y\n    nil",
        Expect::Compatible,
    ),
    (
        "binary/union-incompatible-member",
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c ++ y\n    nil",
        Expect::Incompatible,
    ),
    (
        "binary/dynamic",
        "  class run -> Nil =>\n    c := self new\n    c ++ undeclaredVar\n    nil",
        Expect::Compatible,
    ),
    // ---------------------------------------------------------------------------
    // Keyword — `c takeStr: <arg>` / `c takeObj: <arg>` (non-cascade). This is the
    // original coverage (the baseline every other shape is measured against).
    // ---------------------------------------------------------------------------
    (
        "keyword/known-compatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"ok\"\n    nil",
        Expect::Compatible,
    ),
    (
        "keyword/known-incompatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: 42\n    nil",
        Expect::Incompatible,
    ),
    (
        "keyword/union-all-compatible",
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeObj: y\n    nil",
        Expect::Compatible,
    ),
    (
        "keyword/union-incompatible-member",
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: y\n    nil",
        Expect::Incompatible,
    ),
    (
        "keyword/dynamic",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: undeclaredVar\n    nil",
        Expect::Compatible,
    ),
    // ---------------------------------------------------------------------------
    // Cascade-first — the first message of a cascade goes through the exact same
    // `infer_message_send_with_receiver_ty` path as a non-cascade send, just
    // reached via cascade syntax. A harmless `second: "y"` continuation keeps the
    // cascade well-formed without affecting the cell under test.
    // ---------------------------------------------------------------------------
    (
        "cascade-first/known-compatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"ok\"; second: \"y\"\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-first/known-incompatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: 42; second: \"y\"\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-first/union-all-compatible",
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeObj: y; secondObj: \"y\"\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-first/union-incompatible-member",
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: y; second: \"y\"\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-first/dynamic",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: undeclaredVar; second: \"y\"\n    nil",
        Expect::Compatible,
    ),
    // ---------------------------------------------------------------------------
    // Cascade-continuation (keyword) — a well-typed first message (`takeStr:
    // "seed"`) followed by the `second:` continuation under test — the cascade
    // loop's `for msg in messages` runs `check_argument_types` for continuation
    // messages too, not just the first message.
    // ---------------------------------------------------------------------------
    (
        "cascade-continuation-keyword/known-compatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: \"ok\"\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-keyword/known-incompatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: 42\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-keyword/union-all-compatible",
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeObj: \"seed\"; secondObj: y\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-keyword/union-incompatible-member",
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: y\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-keyword/dynamic",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: undeclaredVar\n    nil",
        Expect::Compatible,
    ),
    // ---------------------------------------------------------------------------
    // Cascade-continuation (binary) — same shape as above, but the continuation
    // message is a *binary* selector (`++` / `+`). `check_binary_operand_types` is
    // only ever invoked from `infer_message_send_with_receiver_ty` (the
    // first-message path), never from the cascade continuation loop, so a binary
    // continuation's argument needs its own fallback coverage distinct from the
    // keyword-continuation coverage above.
    // ---------------------------------------------------------------------------
    (
        "cascade-continuation-binary/known-compatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ \"ok\"\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-binary/known-incompatible",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ 42\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-binary/union-all-compatible",
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; + y\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-binary/union-incompatible-member",
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ y\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-binary/dynamic",
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ undeclaredVar\n    nil",
        Expect::Compatible,
    ),
    // ---------------------------------------------------------------------------
    // Cascade-continuation (class-ref) — `Thing classTakeStr: ...;
    // classSecond: ...`, where the cascade *target* itself parses as an
    // `Expression::ClassReference` (any capitalized identifier — see
    // `parse_identifier_or_field_access`). This exercises the `is_class_ref`
    // branch of the continuation loop (`inference.rs` ~lines 735-754), which
    // resolves `class_name` straight from the `ClassReference` AST node rather
    // than from `dispatch_ty`, and is reached *before* the `InferredType::Known`
    // match arm the instance-receiver cells above go through — a distinct branch
    // the matrix previously left uncovered.
    // ---------------------------------------------------------------------------
    (
        "cascade-continuation-class-ref/known-compatible",
        "  class run -> Nil =>\n    Thing classTakeStr: \"seed\"; classSecond: \"ok\"\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-class-ref/known-incompatible",
        "  class run -> Nil =>\n    Thing classTakeStr: \"seed\"; classSecond: 42\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-class-ref/union-all-compatible",
        "  class run: y :: String | Symbol -> Nil =>\n    Thing classTakeObj: \"seed\"; classSecondObj: y\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-class-ref/union-incompatible-member",
        "  class run: y :: String | Nil -> Nil =>\n    Thing classTakeStr: \"seed\"; classSecond: y\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-class-ref/dynamic",
        "  class run -> Nil =>\n    Thing classTakeStr: \"seed\"; classSecond: undeclaredVar\n    nil",
        Expect::Compatible,
    ),
    // ---------------------------------------------------------------------------
    // Cascade-continuation (self-in-class-method) — `self classTakeStr:
    // ...; classSecond: ...`, sent from inside `Thing`'s own `class run` method.
    // `self` there is bound to `InferredType::Known { class_name: "Thing", .. }`
    // (see `receiver_type_for_class`, set for `class.class_methods` bodies)
    // *and* `env.in_class_method` is `true`, so this exercises the
    // `env.in_class_method && ....is_self()` branch of the
    // continuation loop (`inference.rs` ~lines 755-775) — the one other branch
    // besides the class-ref branch above and the instance/binary "else" branch
    // the earlier cells cover. Unlike the class-ref branch, this one additionally
    // gates the check on `!in_abstract_method`.
    // ---------------------------------------------------------------------------
    (
        "cascade-continuation-self-class-method/known-compatible",
        "  class run -> Nil =>\n    self classTakeStr: \"seed\"; classSecond: \"ok\"\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-self-class-method/known-incompatible",
        "  class run -> Nil =>\n    self classTakeStr: \"seed\"; classSecond: 42\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-self-class-method/union-all-compatible",
        "  class run: y :: String | Symbol -> Nil =>\n    self classTakeObj: \"seed\"; classSecondObj: y\n    nil",
        Expect::Compatible,
    ),
    (
        "cascade-continuation-self-class-method/union-incompatible-member",
        "  class run: y :: String | Nil -> Nil =>\n    self classTakeStr: \"seed\"; classSecond: y\n    nil",
        Expect::Incompatible,
    ),
    (
        "cascade-continuation-self-class-method/dynamic",
        "  class run -> Nil =>\n    self classTakeStr: \"seed\"; classSecond: undeclaredVar\n    nil",
        Expect::Compatible,
    ),
];

#[test]
fn matrix_source_cells() {
    for &(cell, extra_class_method, expect) in SOURCE_CELLS {
        let src = fixture(extra_class_method);
        assert_expect(&diags_with_expect(&src), cell, expect);
    }
}

/// Supplementary to the five `binary/*` cells of `SOURCE_CELLS`: those route
/// through a custom `Thing`-defined `++`/`+`, so `check_binary_operand_types`
/// never has bespoke logic for them (`binary_operand_check_ran` stays
/// `false`) and every cell exercises only the generic `check_argument_types`
/// fallback. The crux of the shared `check_argument_types` fallback is the *interaction* between the two
/// checks on a receiver `check_binary_operand_types` DOES have bespoke logic
/// for (`String`'s `++`) — the fallback must not fire a *duplicate*
/// diagnostic alongside the bespoke one. Exercise that directly here so the
/// matrix doesn't just gesture at `bt2843_binary_arg_union_check.rs` for it.
#[test]
fn matrix_binary_builtin_receiver_known_incompatible_no_duplicate() {
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("++".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let checker = check(&module, &hierarchy);
    assert_incompatible(
        checker.diagnostics(),
        "binary/builtin-receiver-known-incompatible-no-duplicate",
    );
}

// ---------------------------------------------------------------------------
// FFI call — `check_ffi_argument_types` directly, mirroring the Union
// tests in `ffi.rs`. A `String`-typed param covers `Known-compatible`,
// `Known-incompatible`, `Union-with-one-incompatible-member`, and `Dynamic`.
//
// `Union-all-compatible` deliberately does *not* reuse the `Object`-typed
// param the non-FFI rows use below: `check_ffi_argument_types`'s `Union` arm
// has an `expected_is_object` early-exit *before* it ever calls
// `classify_union_members` (unlike `check_argument_types`'s Union arm, which
// has no such shortcut), so an `Object`-typed param here would never
// exercise the union-classification logic at all — every member is
// trivially "compatible" with `Object` without a single `is_type_compatible`
// call. A `Number`-typed param with an `Integer | Float` union routes
// through the real classification path (both members share `Number` as a
// direct superclass, per `generated_builtins.rs`), so this cell actually
// exercises the FFI Union handling instead of vacuously passing.
// ---------------------------------------------------------------------------

fn ffi_sig(param_type: InferredType) -> FunctionSignature {
    FunctionSignature {
        name: "fun".to_string(),
        arity: 1,
        params: vec![ParamType {
            keyword: Some(eco_string("arg")),
            type_: param_type,
        }],
        return_type: InferredType::Dynamic(DynamicReason::DynamicSpec),
        provenance: TypeProvenance::Extracted,
        line: None,
    }
}

fn run_ffi(param_type: InferredType, arg_type: InferredType) -> Vec<Diagnostic> {
    let sig = ffi_sig(param_type);
    let hierarchy = ClassHierarchy::with_builtins();
    let checker = check_ffi_args("mymod", "fun", &sig, &[arg_type], &hierarchy);
    checker.diagnostics().to_vec()
}

/// The FFI row: `(cell, declared parameter type, argument type, outcome)`.
fn ffi_cells() -> Vec<(&'static str, InferredType, InferredType, Expect)> {
    vec![
        (
            "ffi/known-compatible",
            InferredType::known("String"),
            InferredType::known("String"),
            Expect::Compatible,
        ),
        (
            "ffi/known-incompatible",
            InferredType::known("String"),
            InferredType::known("Integer"),
            Expect::Incompatible,
        ),
        (
            "ffi/union-all-compatible",
            InferredType::known("Number"),
            InferredType::simple_union(&["Integer", "Float"]),
            Expect::Compatible,
        ),
        (
            "ffi/union-incompatible-member",
            InferredType::known("String"),
            InferredType::simple_union(&["String", "Integer"]),
            Expect::Incompatible,
        ),
        (
            "ffi/dynamic",
            InferredType::known("String"),
            InferredType::Dynamic(DynamicReason::Unknown),
            Expect::Compatible,
        ),
    ]
}

#[test]
fn matrix_ffi_cells() {
    for (cell, param_type, arg_type, expect) in ffi_cells() {
        assert_expect(&run_ffi(param_type, arg_type), cell, expect);
    }
}
